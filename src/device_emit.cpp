// Emit a MicroTeX layout through a device's own primitives.
//
// This mirrors build_latex_children() in R/grid-builder.R, which does the
// same job for grid. The two must agree; tests/testthat/test-base-graphics.R
// renders the same expressions through both and compares. Where a rule
// looks arbitrary here it is quoted from that file, with its line.

#include "gm_base_device.h"

#include <cmath>
#include <cstring>
#include <string>
#include <vector>

namespace {

// ---------------------------------------------------------------- utils

SEXP df_col(SEXP df, const char *name) {
    SEXP nms = Rf_getAttrib(df, R_NamesSymbol);
    if (nms == R_NilValue) return R_NilValue;
    for (R_xlen_t i = 0; i < Rf_xlength(nms); i++) {
        if (std::strcmp(CHAR(STRING_ELT(nms, i)), name) == 0)
            return VECTOR_ELT(df, i);
    }
    return R_NilValue;
}

const char *str_at(SEXP col, R_xlen_t i) {
    if (col == R_NilValue || TYPEOF(col) != STRSXP || i >= Rf_xlength(col))
        return nullptr;
    SEXP s = STRING_ELT(col, i);
    return (s == NA_STRING) ? nullptr : CHAR(s);
}

// Records are "#RRGGBB", or "#RRGGBBAA" when translucent: the order
// color_to_hex() in parse_latex.cpp writes. Only the colour going *in*,
// from .parse_from_gp(), is MicroTeX's #AARRGGBB. Parsed by hand because
// R_GE_str2col() signals on a malformed string, and this runs inside a
// device callback.
rcolor parse_color(const char *s, rcolor dflt) {
    if (!s || s[0] != '#') return dflt;
    size_t n = std::strlen(s);
    auto hex2 = [&](size_t at) -> int {
        auto d = [](char c) -> int {
            if (c >= '0' && c <= '9') return c - '0';
            if (c >= 'a' && c <= 'f') return c - 'a' + 10;
            if (c >= 'A' && c <= 'F') return c - 'A' + 10;
            return -1;
        };
        int hi = d(s[at]), lo = d(s[at + 1]);
        return (hi < 0 || lo < 0) ? -1 : hi * 16 + lo;
    };
    int r, g, b, a = 255;
    if (n == 7) { r = hex2(1); g = hex2(3); b = hex2(5); }
    else if (n == 9) { r = hex2(1); g = hex2(3); b = hex2(5); a = hex2(7); }
    else return dflt;
    if (r < 0 || g < 0 || b < 0 || a < 0) return dflt;
    return R_RGBA(r, g, b, a);
}

// Maps layout space (x right, y *down* from the top of the bbox) onto
// device space, applying hadj and the engine's rotation.
struct Frame {
    double ox, oy;      // device coords of the baseline anchor
    double sx, sy;      // device length units per bigpt
    double yd;          // device Y delta per unit of visual "down"
    double c, s;        // cos/sin of the engine rotation
    double hshift_bp;   // hadj * width, subtracted along the text axis
    double ascent_bp;   // layout y of the baseline

    void map(double lx, double ly, double *X, double *Y) const {
        double vr = (lx - hshift_bp) * sx;     // visual right
        double vd = (ly - ascent_bp) * sy;     // visual down
        // Rotate ccw by the engine angle. With "down" positive, a ccw
        // turn is the transpose of the usual matrix: check rot=90 sends
        // (1,0) to visual-up.
        double vr2 =  vr * c + vd * s;
        double vd2 = -vr * s + vd * c;
        *X = ox + vr2;
        *Y = oy + vd2 * yd;
    }
};

// A fresh context for our own primitives. Never mutate the caller's --
// the engine reuses it after we return.
void base_gc(R_GE_gcontext *out, const pGEcontext in) {
    *out = *in;
    out->lty     = LTY_SOLID;   // par(lty=2) must not dash a fraction bar
    out->lend    = GE_BUTT_CAP;
    out->ljoin   = GE_ROUND_JOIN;
    out->lmitre  = 10.0;
    out->patternFill = R_NilValue;
}

// A quad, filled or stroked. Always a polygon rather than dd->rect,
// which is axis-aligned and so cannot express a rotated fraction bar or
// a boxed formula in a ylab.
//
// The fill/stroke split is not cosmetic: R/grid-builder.R:70-115 draws
// "fill_rect"/"fill_roundrect" with fill=colour,col=NA and
// "rect"/"roundrect" with col=colour,fill=NA,lwd. Filling the latter
// turns \boxed{} and \fbox{} into solid blocks.
void quad(const Frame &f, double x0, double y0, double x1, double y1,
          rcolor col, bool filled, double lwd_bp,
          const pGEcontext gc, pDevDesc dd) {
    double X[4], Y[4];
    f.map(x0, y0, &X[0], &Y[0]);
    f.map(x1, y0, &X[1], &Y[1]);
    f.map(x1, y1, &X[2], &Y[2]);
    f.map(x0, y1, &X[3], &Y[3]);
    R_GE_gcontext g; base_gc(&g, gc);
    if (filled) {
        g.fill = col;
        g.col  = R_TRANWHITE;
    } else {
        g.fill = R_TRANWHITE;
        g.col  = col;
        g.lwd  = lwd_bp * 96.0 / 72.0;   // R/grid-builder.R:24
    }
    if (dd->polygon) dd->polygon(4, X, Y, &g, dd);
}

// ------------------------------------------------------------ records

void emit_path(const Frame &f, SEXP rec, rcolor col,
               const pGEcontext gc, pDevDesc dd) {
    if (rec == R_NilValue || TYPEOF(rec) != VECSXP) return;
    SEXP cmds   = df_col(rec, "cmd");
    SEXP coords = df_col(rec, "coords");
    if (cmds == R_NilValue || coords == R_NilValue) return;
    if (TYPEOF(coords) != REALSXP) return;

    SEXP dim = Rf_getAttrib(coords, R_DimSymbol);
    if (dim == R_NilValue || Rf_length(dim) != 2) return;
    int nr = INTEGER(dim)[0], nc = INTEGER(dim)[1];
    if (nc < 6) return;
    const double *C = REAL(coords);
    auto at = [&](int row, int cl) { return C[row + cl * nr]; };

    std::vector<double> xs, ys;
    std::vector<int> nper;
    size_t sub_start = 0;
    // Flattening happens in layout space, so the curve is subdivided the
    // same way regardless of device scale or rotation -- matching what
    // cubic_bezier()/quad_bezier() do before grid applies its viewport.
    double lastx = 0.0, lasty = 0.0;
    bool   have_last = false;

    auto flush = [&]() {
        size_t n = xs.size() - sub_start;
        if (n >= 3) nper.push_back((int) n);
        else { xs.resize(sub_start); ys.resize(sub_start); }
        sub_start = xs.size();
    };
    auto push = [&](double lx, double ly) {
        double X, Y; f.map(lx, ly, &X, &Y);
        xs.push_back(X); ys.push_back(Y);
        lastx = lx; lasty = ly; have_last = true;
    };

    for (int j = 0; j < nr && j < Rf_xlength(cmds); j++) {
        const char *cm = str_at(cmds, j);
        if (!cm) continue;
        // n=16 cubic / n=12 quadratic, endpoints included, first point
        // dropped because it is the current point: R/grid-builder.R:302-318.
        if (cm[0] == 'M') { flush(); push(at(j, 0), at(j, 1)); }
        else if (cm[0] == 'L') { push(at(j, 0), at(j, 1)); }
        else if (cm[0] == 'C') {
            if (!have_last || xs.size() == sub_start) continue;
            double x0 = lastx, y0 = lasty;
            for (int k = 1; k < 16; k++) {
                double t2 = (double) k / 15.0, mt = 1.0 - t2;
                double bx = mt*mt*mt*x0 + 3*mt*mt*t2*at(j,0)
                          + 3*mt*t2*t2*at(j,2) + t2*t2*t2*at(j,4);
                double by = mt*mt*mt*y0 + 3*mt*mt*t2*at(j,1)
                          + 3*mt*t2*t2*at(j,3) + t2*t2*t2*at(j,5);
                push(bx, by);
            }
        }
        else if (cm[0] == 'Q') {
            if (!have_last || xs.size() == sub_start) continue;
            double x0 = lastx, y0 = lasty;
            for (int k = 1; k < 12; k++) {
                double t2 = (double) k / 11.0, mt = 1.0 - t2;
                double bx = mt*mt*x0 + 2*mt*t2*at(j,0) + t2*t2*at(j,2);
                double by = mt*mt*y0 + 2*mt*t2*at(j,1) + t2*t2*at(j,3);
                push(bx, by);
            }
        }
        else if (cm[0] == 'Z') { flush(); have_last = false; }
    }
    flush();
    if (nper.empty()) return;

    R_GE_gcontext g; base_gc(&g, gc);
    g.fill = col;
    g.col  = R_TRANWHITE;
    // Glyph outlines have counters (o, a, 8, A). evenodd + one subpath
    // per contour is what R/grid-builder.R:290-296 uses; dd->polygon has
    // no notion of holes, so a multi-contour glyph needs dd->path.
    if (dd->path) {
        dd->path(xs.data(), ys.data(), (int) nper.size(), nper.data(),
                 FALSE /* evenodd */, &g, dd);
    } else if (nper.size() == 1 && dd->polygon) {
        dd->polygon(nper[0], xs.data(), ys.data(), &g, dd);
    }
}

}  // namespace

// --------------------------------------------------------------- entry

void gm_emit_layout(const GmSavedDev *sv, SEXP layout,
                    double x, double y, double rot, double hadj,
                    double width_bp, double ascent_bp,
                    const pGEcontext gc, pDevDesc dd) {
    if (layout == R_NilValue || TYPEOF(layout) != VECSXP) return;

    SEXP c_type = df_col(layout, "type");
    if (c_type == R_NilValue) return;
    R_xlen_t n = Rf_xlength(c_type);

    SEXP c_x    = df_col(layout, "x");      SEXP c_y   = df_col(layout, "y");
    SEXP c_x2   = df_col(layout, "x2");     SEXP c_y2  = df_col(layout, "y2");
    SEXP c_w    = df_col(layout, "width");  SEXP c_h   = df_col(layout, "height");
    SEXP c_lwd  = df_col(layout, "lwd");    SEXP c_col = df_col(layout, "color");
    SEXP c_txt  = df_col(layout, "text");   SEXP c_rot = df_col(layout, "rotation");
    SEXP c_fs   = df_col(layout, "font_size");
    SEXP c_path = df_col(layout, "path");
    SEXP c_style = df_col(layout, "font_style");
    SEXP c_fam   = df_col(layout, "font_family");

    Frame f;
    f.ox = x; f.oy = y;
    f.sx = (dd->ipr[0] > 0) ? 1.0 / (72.0 * dd->ipr[0]) : 1.0;
    f.sy = (dd->ipr[1] > 0) ? 1.0 / (72.0 * dd->ipr[1]) : 1.0;
    f.yd = (dd->bottom > dd->top) ? 1.0 : -1.0;
    double t = rot * M_PI / 180.0;
    f.c = std::cos(t); f.s = std::sin(t);
    // GEText pre-adjusts x itself when the device cannot (canHAdj == 0),
    // using the width we returned from strWidth. Only shift ourselves
    // when the device was expected to do it.
    f.hshift_bp = (dd->canHAdj > 0) ? hadj * width_bp : 0.0;
    f.ascent_bp = ascent_bp;

    rcolor fg = gc->col;
    // Record colours are opaque by construction (see gc_col_hex), so the
    // caller's alpha is applied here and composed with any per-record
    // alpha a \textcolor may have set.
    int gc_alpha = R_ALPHA(gc->col);

    for (R_xlen_t i = 0; i < n; i++) {
        const char *ty = str_at(c_type, i);
        if (!ty) continue;
        rcolor col = parse_color(str_at(c_col, i), fg);
        if (gc_alpha < 255) {
            col = R_RGBA(R_RED(col), R_GREEN(col), R_BLUE(col),
                         R_ALPHA(col) * gc_alpha / 255);
        }

        if (std::strcmp(ty, "path") == 0) {
            SEXP rec = (c_path != R_NilValue && TYPEOF(c_path) == VECSXP)
                       ? VECTOR_ELT(c_path, i) : R_NilValue;
            emit_path(f, rec, col, gc, dd);

        } else if (std::strcmp(ty, "line") == 0) {
            double x0 = gm_num_at(c_x, i),  y0 = gm_num_at(c_y, i);
            double x1 = gm_num_at(c_x2, i), y1 = gm_num_at(c_y2, i);
            double lwd_bp = gm_num_at(c_lwd, i, 1.0);
            if (std::fabs(y0 - y1) < 0.001) {
                // R/grid-builder.R:48-59: a horizontal rule is drawn as a
                // filled rect of height lwd, centred on y -- not stroked.
                quad(f, x0, y0 - lwd_bp / 2.0, x1, y0 + lwd_bp / 2.0,
                     col, true, lwd_bp, gc, dd);
            } else {
                double X0, Y0, X1, Y1;
                f.map(x0, y0, &X0, &Y0);
                f.map(x1, y1, &X1, &Y1);
                R_GE_gcontext g; base_gc(&g, gc);
                g.col = col;
                g.lwd = lwd_bp * 96.0 / 72.0;   // R/grid-builder.R:24
                if (dd->line) dd->line(X0, Y0, X1, Y1, &g, dd);
            }

        } else if (std::strcmp(ty, "fill_rect") == 0 ||
                   std::strcmp(ty, "rect") == 0 ||
                   std::strcmp(ty, "fill_roundrect") == 0 ||
                   std::strcmp(ty, "roundrect") == 0) {
            // Corners are lost on the round variants; grid rounds them
            // with r = min(rx, ry). Documented, not fixed: dd has no
            // rounded primitive and a rotated one would need a path.
            bool filled = (ty[0] == 'f');   // fill_rect / fill_roundrect
            double x0 = gm_num_at(c_x, i), y0 = gm_num_at(c_y, i);
            double w  = gm_num_at(c_w, i), h  = gm_num_at(c_h, i);
            quad(f, x0, y0, x0 + w, y0 + h, col, filled,
                 gm_num_at(c_lwd, i, 1.0), gc, dd);

        } else if (std::strcmp(ty, "text") == 0) {
            const char *s = str_at(c_txt, i);
            if (!s || !*s) continue;
            double X, Y;
            f.map(gm_num_at(c_x, i), gm_num_at(c_y, i), &X, &Y);
            R_GE_gcontext g; base_gc(&g, gc);
            g.col = col;
            double fs = gm_num_at(c_fs, i, gc->ps * gc->cex);
            g.cex = 1.0;
            g.ps  = fs;
            // Face and family come from the record, not from the caller.
            // MicroTeX measured this run in the face its own \textbf /
            // \textit / \textsf implied; inheriting par(font.main = 2)
            // instead would bold prose that was measured plain, and the
            // drawn text would then overrun the width we reported.
            // Mirrors .resolve_text_face()/.resolve_text_family(),
            // R/grid-builder.R:328-369.
            int style = (int) gm_num_at(c_style, i, 0.0);
            int bold = (style & 2) != 0, ital = (style & 4) != 0;
            g.fontface = bold && ital ? 4 : bold ? 2 : ital ? 3 : 1;
            const char *fam = str_at(c_fam, i);
            if (fam && *fam && std::strcmp(fam, "gridmicrotex.default") != 0) {
                std::snprintf(g.fontfamily, sizeof g.fontfamily, "%s", fam);
            } else if (!fam || !*fam) {
                if (style & 128)      std::snprintf(g.fontfamily, sizeof g.fontfamily, "mono");
                else if (style & 64)  std::snprintf(g.fontfamily, sizeof g.fontfamily, "sans");
            }
            // MicroTeX rotation is ccw in a y-down frame, i.e. visually
            // clockwise, hence the sign flip -- the same reason
            // R/grid-builder.R:136 negates it for grid.
            double trot = rot - gm_num_at(c_rot, i, 0.0);
            // Sub-runs are positioned absolutely; each is left-aligned.
            if (dd->hasTextUTF8 && sv->textUTF8)
                sv->textUTF8(X, Y, s, trot, 0.0, &g, dd);
            else if (sv->text)
                sv->text(X, Y, s, trot, 0.0, &g, dd);
        }
        // "glyph" cannot occur: the layout is parsed with use_path=TRUE.
        // "image" is not supported in base labels; see ?latex_options.
    }
}
