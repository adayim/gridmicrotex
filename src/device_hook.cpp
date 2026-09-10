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

#include <cstdio>
#include <cstring>
#include <string>

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

struct LayoutReq {
    std::string str;
    double      fontsize;
    std::string col;
    std::string family;
    SEXP        result;   // preserved on success, nullptr otherwise
};

void do_layout(void *data) {
    LayoutReq *rq = (LayoutReq *) data;
    SEXP s   = PROTECT(Rf_mkString(rq->str.c_str()));
    SEXP fs  = PROTECT(Rf_ScalarReal(rq->fontsize));
    SEXP cl  = PROTECT(Rf_mkString(rq->col.c_str()));
    SEXP fam = PROTECT(Rf_mkString(rq->family.c_str()));
    SEXP call = PROTECT(Rf_lang5(g_layout_fn, s, fs, cl, fam));
    SEXP res  = PROTECT(Rf_eval(call, R_GlobalEnv));
    if (res != R_NilValue) { R_PreserveObject(res); rq->result = res; }
    UNPROTECT(6);
}

// Colour for the R side, from the caller's context -- always opaque.
//
// Alpha deliberately does not travel through the layout. A translucent
// colour reaches MicroTeX as 9-char hex, and 9-char hex is ambiguous:
// .parse_from_gp() writes #AARRGGBB for the engine while the records
// come back #RRGGBBAA, so whichever order the emitter assumes is wrong
// half the time -- reading them the wrong way round drew a 20%-opacity
// label as either fully opaque or fully invisible. The emitter applies
// the caller's alpha itself, where the byte order is not in doubt.
std::string gc_col_hex(const pGEcontext gc) {
    char buf[16];
    unsigned int c = (unsigned int) gc->col;
    std::snprintf(buf, sizeof buf, "#%02X%02X%02X",
                  R_RED(c), R_GREEN(c), R_BLUE(c));
    return std::string(buf);
}

// Lay out `str`, or return nullptr to mean "draw it literally".
SEXP layout_for(const char *str, const pGEcontext gc) {
    if (!g_enabled || !g_layout_fn || g_in_hook) return nullptr;
    if (!maybe_math(str)) return nullptr;

    LayoutReq rq;
    rq.str      = str;                      // copy: engine memory is not ours
    rq.fontsize = gc->ps * gc->cex;
    rq.col      = gc_col_hex(gc);
    rq.family   = gc->fontfamily[0] ? gc->fontfamily : "";  // char[], never null
    rq.result   = nullptr;

    g_in_hook = true;
    R_ToplevelExec(do_layout, &rq);         // failure -> result stays null
    g_in_hook = false;
    return rq.result;
}

// The metrics originate as MicroTeX ints; .gm_base_layout() coerces them,
// but accept either type so a change on the R side cannot silently make
// every width zero -- which reads as "no horizontal adjustment" and
// left-aligns every centred label.
double num_elt(SEXP lay, int i) {
    SEXP v = VECTOR_ELT(lay, i);
    if (!Rf_xlength(v)) return 0.0;
    if (TYPEOF(v) == REALSXP) return REAL(v)[0];
    if (TYPEOF(v) == INTSXP)  return (double) INTEGER(v)[0];
    return 0.0;
}
double layout_width_bp(SEXP lay)  { return num_elt(lay, 1); }  // list(layout, width, ...)
double layout_ascent_bp(SEXP lay) { return num_elt(lay, 4); }

// ------------------------------------------------------ the callbacks

// find_saved() must be repeated after layout_for(): it evaluates R, which
// can disarm a device, and disarm() fills the freed slot from the end of
// g_saved -- so a GmSavedDev* taken before the call can point at a
// different device by the time it returns.
void hooked_text(double x, double y, const char *str, double rot,
                 double hadj, const pGEcontext gc, pDevDesc dd) {
    if (!find_saved(dd)) return;
    SEXP lay = layout_for(str, gc);
    GmSavedDev *sv = find_saved(dd);
    if (!sv) { if (lay) R_ReleaseObject(lay); return; }
    if (!lay) { if (sv->text) sv->text(x, y, str, rot, hadj, gc, dd); return; }
    gm_emit_layout(sv, VECTOR_ELT(lay, 0), x, y, rot, hadj,
                   layout_width_bp(lay), layout_ascent_bp(lay), gc, dd);
    R_ReleaseObject(lay);
}

void hooked_textUTF8(double x, double y, const char *str, double rot,
                     double hadj, const pGEcontext gc, pDevDesc dd) {
    if (!find_saved(dd)) return;
    SEXP lay = layout_for(str, gc);
    GmSavedDev *sv = find_saved(dd);
    if (!sv) { if (lay) R_ReleaseObject(lay); return; }
    if (!lay) {
        if (sv->textUTF8) sv->textUTF8(x, y, str, rot, hadj, gc, dd);
        return;
    }
    gm_emit_layout(sv, VECTOR_ELT(lay, 0), x, y, rot, hadj,
                   layout_width_bp(lay), layout_ascent_bp(lay), gc, dd);
    R_ReleaseObject(lay);
}

// strWidth and text must agree, or GEText centres the literal string
// using the math width. Both go through layout_for(), and the layout
// cache makes the second call a lookup rather than a re-parse.
double hooked_strWidth(const char *str, const pGEcontext gc, pDevDesc dd) {
    if (!find_saved(dd)) return 0.0;
    SEXP lay = layout_for(str, gc);
    GmSavedDev *sv = find_saved(dd);
    if (!sv) { if (lay) R_ReleaseObject(lay); return 0.0; }
    if (!lay) return sv->strWidth ? sv->strWidth(str, gc, dd) : 0.0;
    double w = layout_width_bp(lay);
    R_ReleaseObject(lay);
    return (dd->ipr[0] > 0) ? w / (72.0 * dd->ipr[0]) : w;
}

double hooked_strWidthUTF8(const char *str, const pGEcontext gc, pDevDesc dd) {
    if (!find_saved(dd)) return 0.0;
    SEXP lay = layout_for(str, gc);
    GmSavedDev *sv = find_saved(dd);
    if (!sv) { if (lay) R_ReleaseObject(lay); return 0.0; }
    if (!lay) return sv->strWidthUTF8 ? sv->strWidthUTF8(str, gc, dd) : 0.0;
    double w = layout_width_bp(lay);
    R_ReleaseObject(lay);
    return (dd->ipr[0] > 0) ? w / (72.0 * dd->ipr[0]) : w;
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
int disarm_all() {
    for (int i = g_n_saved - 1; i >= 0; i--) disarm(g_saved[i].dd);
    return g_n_saved;
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
        // The engine is about to free dd, so drop the entry without
        // writing back into it.
        if (event == GE_InitState) { if (g_enabled) arm(gd->dev); }
        else if (event == GE_FinaliseState) forget(gd->dev);
    }
    SEXP state = PROTECT(Rf_ScalarInteger(1));
    Rf_setAttrib(state, Rf_install("pkgName"), Rf_mkString("gridmicrotex"));
    UNPROTECT(1);
    return state;
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
        if (g_system_id >= 0) { GEunregisterSystem(g_system_id); g_system_id = -1; }
        if (g_layout_fn) { R_ReleaseObject(g_layout_fn); g_layout_fn = nullptr; }
    }
}

// Restore every device before the DLL is unmapped. Without this,
// .onUnload's library.dynam.unload() leaves armed devices pointing into
// freed address space and the next plot jumps to nowhere.
// [[Rcpp::export]]
void gm_base_teardown() { gm_base_set_enabled(false, R_NilValue); }

// [[Rcpp::export]]
int gm_base_armed_count() { return g_n_saved; }
