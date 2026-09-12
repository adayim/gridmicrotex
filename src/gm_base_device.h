#ifndef GM_BASE_DEVICE_H
#define GM_BASE_DEVICE_H

// Base-graphics interception: shared types between the callback hook
// (device_hook.cpp) and the record emitter (device_emit.cpp).
//
// Everything here runs *inside* a device callback. It must not allocate
// R memory outside the guarded R_ToplevelExec call, must not signal, and
// must not re-enter the graphics engine -- drawing happens only through
// the original callbacks captured in GmSavedDev.

// Without this, Rinternals.h defines length() as a macro, which collides
// with std::codecvt::length() the moment <string> is included.
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif

#include <R.h>
#include <Rinternals.h>
#include <R_ext/GraphicsEngine.h>
#include <R_ext/GraphicsDevice.h>

// The original callbacks for one armed device, plus the identity checks
// that keep us from restoring into the wrong pDevDesc after R recycles a
// device number.
typedef struct {
    pDevDesc dd;                       // identity, not ownership
    void   (*text)(double, double, const char *, double, double,
                   const pGEcontext, pDevDesc);
    void   (*textUTF8)(double, double, const char *, double, double,
                       const pGEcontext, pDevDesc);
    double (*strWidth)(const char *, const pGEcontext, pDevDesc);
    double (*strWidthUTF8)(const char *, const pGEcontext, pDevDesc);
} GmSavedDev;

// Draw one laid-out label through `sv`'s original callbacks.
//
// `layout` is the data frame from .gm_base_layout(); `x`/`y` are the
// device coordinates of the label's baseline origin as the engine gave
// them, `rot` the engine's rotation in degrees ccw, `hadj` its horizontal
// adjustment, `width_bp`/`ascent_bp` the label metrics in bigpts.
// `gc` is the caller's context and is never mutated.
void gm_emit_layout(const GmSavedDev *sv, SEXP layout,
                    double x, double y, double rot, double hadj,
                    double width_bp, double ascent_bp,
                    const pGEcontext gc, pDevDesc dd);

#endif  // GM_BASE_DEVICE_H
