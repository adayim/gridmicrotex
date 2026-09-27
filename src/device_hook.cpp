// Base-graphics math interception.
//
// Arming is done through a registered graphics system rather than
// setHook("before.plot.new"): that hook lives in the graphics::plot.new
// *closure*, so display-list replay (replayPlot, dev.copy2pdf, RStudio's
// Export, knitr snapshot paths) goes through C_plot_new and never fires
// it. A device armed only at plot.new would render math on screen and
// literal "$...$" in the exported file. GE_InitState fires for every
// device the engine creates, including a dev.copy target.

#include <Rcpp.h>          // must precede the R headers pulled in below
#include "gm_base_device.h"

#include <cstddef>
#include <cstdio>
#include <cstring>

namespace {

const int  GM_MAX_DEV = 64;          // R's own device table limit
GmSavedDev g_saved[GM_MAX_DEV];
int        g_n_saved   = 0;
int        g_system_id = -1;
bool       g_enabled   = false;
SEXP       g_layout_fn = nullptr;    // .gm_base_layout, preserved

// One flag for the whole interceptor. Laying a label out measures its
// \text{} runs, which calls back into strwidth() on this same device and
// re-enters us -- verified nesting depth 4 without this. Falling through
// to the original callback is also what keeps the inner measurement
// correct, since that is exactly what the measurer wants to know.
bool g_in_hook = false;

GmSavedDev *find_saved(pDevDesc dd) {
    for (int i = 0; i < g_n_saved; i++)
        if (g_saved[i].dd == dd) return &g_saved[i];
    return nullptr;
}

// Cheap pre-filter. Deliberately a *superset* of what counts as math:
// the authoritative decision is .gm_base_is_math() in R, built on
// .scan_math_spans(). Keeping the rule in one place is why the gate here
// only asks "could this possibly contain a delimiter?".
bool maybe_math(const char *s) {
    if (!s) return false;
    for (const char *p = s; *p; p++) {
        if (*p == '$') return true;
        if (*p == '\\') {
            char c = p[1];
            if (c == '(' || c == '[') return true;
            if (c == 'b' && std::strncmp(p, "\\begin{", 7) == 0) return true;
        }
    }
    return false;
}

// ------------------------------------------------------- the R call

// Plain data only: an interrupt unwinds straight through layout_for() and
// the callback above it, and a longjmp runs no destructors.
struct LayoutReq {
    const char *str;
    double      fontsize;
    char        col[16];
    const char *family;
};

SEXP do_layout(void *data) {
    LayoutReq *rq = (LayoutReq *) data;
    SEXP s   = PROTECT(Rf_mkString(rq->str));
    SEXP fs  = PROTECT(Rf_ScalarReal(rq->fontsize));
    SEXP cl  = PROTECT(Rf_mkString(rq->col));
    SEXP fam = PROTECT(Rf_mkString(rq->family));
    SEXP call = PROTECT(Rf_lang5(g_layout_fn, s, fs, cl, fam));
    SEXP res  = Rf_eval(call, R_GlobalEnv);
    UNPROTECT(5);
    return res;
}

void leave_hook(void *, Rboolean) { g_in_hook = false; }

// Colour for the R side, from the caller's context -- always opaque.
//
// Alpha deliberately does not travel through the layout: .parse_from_gp()
// hands it to MicroTeX as #AARRGGBB and the records come back #RRGGBBAA,
// and mixing the two up drew a 20%-opacity label either fully opaque or
// not at all. The emitter applies the caller's alpha itself.
void gc_col_hex(const pGEcontext gc, char *out, size_t n) {
    unsigned int c = (unsigned int) gc->col;
    std::snprintf(out, n, "#%02X%02X%02X", R_RED(c), R_GREEN(c), R_BLUE(c));
}

// Lay out `str`, or return R_NilValue to mean "draw it literally". The
// result is unprotected.
//
// Nothing is caught here. .gm_base_layout() turns every failure it can
// recover from into NULL itself, so what reaches this frame is a jump
// that has to get out: an interrupt, or a time limit. R_ToplevelExec
// caught those too, and since layout is most of the time a math-heavy
// plot spends drawing, Ctrl-C rarely stopped one. R_UnwindProtect only
// resets the flag on the way through; R_tryCatch would add an R-level
// tryCatch() to every call, which is most of the cost of a label that
// merely contains a `$`.
SEXP layout_for(const char *str, const pGEcontext gc) {
    if (!g_enabled || !g_layout_fn || g_in_hook) return R_NilValue;
    if (!maybe_math(str)) return R_NilValue;

    LayoutReq rq;
    rq.str      = str;
    rq.fontsize = gc->ps * gc->cex;
    gc_col_hex(gc, rq.col, sizeof rq.col);
    rq.family   = gc->fontfamily;        // char[], never null

    SEXP cont = PROTECT(R_MakeUnwindCont());
    g_in_hook = true;                    // leave_hook() clears it, jump or not
    SEXP res = R_UnwindProtect(do_layout, &rq, leave_hook, nullptr, cont);
    UNPROTECT(1);
    return res;
}

// list(layout, width, height, depth, ascent), metrics in bigpts.
double layout_width_bp(SEXP lay)  { return gm_num_at(VECTOR_ELT(lay, 1), 0); }
double layout_ascent_bp(SEXP lay) { return gm_num_at(VECTOR_ELT(lay, 4), 0); }

// ------------------------------------------------------ the callbacks

using TextFn  = void   (*)(double, double, const char *, double, double,
                           const pGEcontext, pDevDesc);
using WidthFn = double (*)(const char *, const pGEcontext, pDevDesc);

// One body each for text/textUTF8 and strWidth/strWidthUTF8; `orig` names
// the saved callback a literal label goes to.
//
// find_saved() must be repeated after layout_for(): it evaluates R, which
// can disarm a device, and disarm() fills the freed slot from the end of
// g_saved -- so a GmSavedDev* taken before the call can point at a
// different device by the time it returns.
void hook_text(TextFn GmSavedDev::*orig, double x, double y, const char *str,
               double rot, double hadj, const pGEcontext gc, pDevDesc dd) {
    if (!find_saved(dd)) return;
    SEXP lay = PROTECT(layout_for(str, gc));
    const GmSavedDev *sv = find_saved(dd);
    if (sv && lay != R_NilValue) {
        gm_emit_layout(sv, VECTOR_ELT(lay, 0), x, y, rot, hadj,
                       layout_width_bp(lay), layout_ascent_bp(lay), gc, dd);
    } else if (sv && sv->*orig) {
        (sv->*orig)(x, y, str, rot, hadj, gc, dd);
    }
    UNPROTECT(1);
}

// strWidth and text must agree, or GEText centres the literal string
// using the math width.
double hook_width(WidthFn GmSavedDev::*orig, const char *str,
                  const pGEcontext gc, pDevDesc dd) {
    if (!find_saved(dd)) return 0.0;
    SEXP lay = PROTECT(layout_for(str, gc));
    const GmSavedDev *sv = find_saved(dd);
    double w = 0.0;
    if (sv && lay != R_NilValue) {
        w = layout_width_bp(lay);
        if (dd->ipr[0] > 0) w /= 72.0 * dd->ipr[0];
    } else if (sv && sv->*orig) {
        w = (sv->*orig)(str, gc, dd);
    }
    UNPROTECT(1);
    return w;
}

void hooked_text(double x, double y, const char *str, double rot,
                 double hadj, const pGEcontext gc, pDevDesc dd) {
    hook_text(&GmSavedDev::text, x, y, str, rot, hadj, gc, dd);
}

void hooked_textUTF8(double x, double y, const char *str, double rot,
                     double hadj, const pGEcontext gc, pDevDesc dd) {
    hook_text(&GmSavedDev::textUTF8, x, y, str, rot, hadj, gc, dd);
}

double hooked_strWidth(const char *str, const pGEcontext gc, pDevDesc dd) {
    return hook_width(&GmSavedDev::strWidth, str, gc, dd);
}

double hooked_strWidthUTF8(const char *str, const pGEcontext gc, pDevDesc dd) {
    return hook_width(&GmSavedDev::strWidthUTF8, str, gc, dd);
}

// ------------------------------------------------------ arm / disarm

void arm(pDevDesc dd) {
    if (!dd || find_saved(dd) || g_n_saved >= GM_MAX_DEV) return;
    // Never save our own pointers as "the original": a double-arm would
    // make the restore a no-op and the next label recurse forever.
    if (dd->text == hooked_text || dd->textUTF8 == hooked_textUTF8) return;

    GmSavedDev *sv = &g_saved[g_n_saved++];
    sv->dd           = dd;
    sv->text         = dd->text;
    sv->textUTF8     = dd->textUTF8;
    sv->strWidth     = dd->strWidth;
    sv->strWidthUTF8 = dd->strWidthUTF8;

    dd->text     = hooked_text;
    dd->strWidth = hooked_strWidth;
    if (dd->textUTF8)     dd->textUTF8     = hooked_textUTF8;
    if (dd->strWidthUTF8) dd->strWidthUTF8 = hooked_strWidthUTF8;
}

// Drop the registry entry without touching the device. Only for a device
// the engine is destroying, where dd is about to be freed.
void forget(pDevDesc dd) {
    for (int i = 0; i < g_n_saved; i++)
        if (g_saved[i].dd == dd) { g_saved[i] = g_saved[--g_n_saved]; return; }
}

// Restore a device's callbacks. Returns false when another package has
// wrapped ours since we installed them: restoring would then clobber
// their wrapper, so we leave everything alone AND keep the registry
// entry. Dropping the entry while dd still points at hooked_text would
// make find_saved() fail on the next label and silently draw nothing.
bool disarm(pDevDesc dd) {
    for (int i = 0; i < g_n_saved; i++) {
        if (g_saved[i].dd != dd) continue;
        bool ours = dd->text == hooked_text &&
                    dd->strWidth == hooked_strWidth &&
                    (!dd->textUTF8     || dd->textUTF8     == hooked_textUTF8) &&
                    (!dd->strWidthUTF8 || dd->strWidthUTF8 == hooked_strWidthUTF8);
        if (!ours) return false;
        dd->text     = g_saved[i].text;
        dd->strWidth = g_saved[i].strWidth;
        if (dd->textUTF8)     dd->textUTF8     = g_saved[i].textUTF8;
        if (dd->strWidthUTF8) dd->strWidthUTF8 = g_saved[i].strWidthUTF8;
        g_saved[i] = g_saved[--g_n_saved];
        return true;
    }
    return true;   // not armed
}

// Backwards, because disarm() fills the freed slot from the end.
void disarm_all() {
    for (int i = g_n_saved - 1; i >= 0; i--) disarm(g_saved[i].dd);
}

// Every event returns the same named placeholder. We keep no per-device
// state, but the return value cannot simply be R_NilValue:
//
//  - GE_InitState reads R_NilValue as "initialisation failed";
//  - recordPlot() stores one element per registered graphics system, and
//    grDevices:::restoreRecordedPlot() then does, with no NULL guard,
//      for (i in seq_along(plot)[-1])
//          library(attr(plot[[i]], "pkgName"), character.only = TRUE)
//    so an element that is NULL, or that carries no "pkgName", becomes
//    library(character(0)) -- "'package' must be of length 1". That
//    kills replayPlot(), dev.copy(), and every knitr chunk.
SEXP gm_system_cb(GEevent event, pGEDevDesc gd, SEXP) {
    if (gd) {
        if (event == GE_InitState) { if (g_enabled) arm(gd->dev); }
        // The engine is about to free dd, so drop the entry without
        // writing back into it.
        else if (event == GE_FinaliseState) forget(gd->dev);
    }
    SEXP state = PROTECT(Rf_ScalarInteger(1));
    Rf_setAttrib(state, Rf_install("pkgName"), Rf_mkString("gridmicrotex"));
    UNPROTECT(1);
    return state;
}

// GEunregisterSystem() sends GE_FinaliseState to every open device, not
// only to one being destroyed. Unregistering while a wrapped device still
// has its entry forgot that entry, and once the wrapper handed our
// callbacks back every label on the device drew nothing. So the system
// stays registered until nothing is left to track -- which is also what
// lets a later destruction drop the entry.
void release_system() {
    if (g_system_id >= 0 && g_n_saved == 0) {
        GEunregisterSystem(g_system_id);
        g_system_id = -1;
    }
}

}  // namespace

// ----------------------------------------------------------- exports

// [[Rcpp::export]]
void gm_base_set_enabled(bool on, SEXP layout_fn) {
    if (on) {
        if (g_layout_fn) R_ReleaseObject(g_layout_fn);
        g_layout_fn = layout_fn;
        R_PreserveObject(g_layout_fn);
        if (g_system_id < 0) GEregisterSystem(gm_system_cb, &g_system_id);
        g_enabled = true;
        // Devices opened before now never saw GE_InitState. Slot 0 is
        // the null device and the table has GM_MAX_DEV entries, so the
        // last valid index is GM_MAX_DEV - 1.
        for (int i = 1; i < GM_MAX_DEV; i++) {
            pGEDevDesc gd = GEgetDevice(i);
            if (gd && gd->dev) arm(gd->dev);
        }
    } else {
        g_enabled = false;
        disarm_all();
        release_system();
        if (g_layout_fn) { R_ReleaseObject(g_layout_fn); g_layout_fn = nullptr; }
    }
}

// Restore every device before the DLL is unmapped. Returns how many could
// not be, because another package has wrapped them since: those still call
// into this DLL, so .onUnload must leave it loaded.
// [[Rcpp::export]]
int gm_base_teardown() {
    gm_base_set_enabled(false, R_NilValue);
    return g_n_saved;
}

// For R_unload_gridmicrotex() (init.cpp), which runs as the DLL is
// unloaded by any route. The DLL goes whatever happens here, so the
// graphics system, whose callback lives in it, goes too. A device another
// package has wrapped cannot be restored, and crashes once that package
// hands our callbacks back: .onUnload avoids that by keeping the DLL, but
// pkgload::unload() comes straight here, and nothing can.
void gm_base_unload() {
    gm_base_set_enabled(false, R_NilValue);
    if (g_system_id >= 0) {
        GEunregisterSystem(g_system_id);
        g_system_id = -1;
    }
}

// For the task callback .gm_base_set() leaves behind when switching off
// strands a wrapped device: unregister the graphics system once the last
// such device has closed. TRUE while there is still something to wait
// for, which keeps the callback.
// [[Rcpp::export]]
bool gm_base_release_pending() {
    if (g_enabled) return false;         // switched back on: nothing to release
    release_system();
    return g_system_id >= 0;
}

// [[Rcpp::export]]
int gm_base_armed_count() { return g_n_saved; }
