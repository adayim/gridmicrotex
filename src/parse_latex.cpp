#include <Rcpp.h>
#include <memory>
#include "microtex.h"
#include "graphic/graphic.h"
#include "graphic_recorder.h"
#include "macro/macro.h"

using namespace microtex;
using namespace Rcpp;

// RAII guard: restores the global render-mode flag on scope exit
struct RenderModeGuard {
    ~RenderModeGuard() { MicroTeX::setRenderGlyphUsePath(true); }
};

// Helper: convert ARGB color to R hex string "#RRGGBB" or "#RRGGBBAA"
static std::string color_to_hex(color c) {
    char buf[10];
    int a = color_a(c);
    int r = color_r(c);
    int g = color_g(c);
    int b = color_b(c);
    if (a == 255) {
        snprintf(buf, sizeof(buf), "#%02X%02X%02X", r, g, b);
    } else {
        snprintf(buf, sizeof(buf), "#%02X%02X%02X%02X", r, g, b, a);
    }
    return std::string(buf);
}

// Map a user-facing style name to MicroTeX's TexStyle enum.
// Empty string means "do not override; let the parser decide".
static OverrideTeXStyle resolve_tex_style(const std::string& s) {
    if (s.empty()) return {false, TexStyle::text};
    if (s == "display")      return {true, TexStyle::display};
    if (s == "text")         return {true, TexStyle::text};
    if (s == "script")       return {true, TexStyle::script};
    if (s == "scriptscript") return {true, TexStyle::scriptScript};
    Rcpp::stop("tex_style must be one of \"\", \"display\", \"text\", \"script\", \"scriptscript\".");
}

// [[Rcpp::export]]
Rcpp::List parse_latex_cpp(std::string tex,
                           float text_size = 20.0,
                           float line_space = 10.0,
                           std::string fg_color = "#000000",
                           float max_width = 0,
                           std::string math_font = "",
                           std::string main_font = "",
                           bool use_path = true,
                           std::string tex_style = "") {

    if (!MicroTeX::isInited()) {
        Rcpp::stop("MicroTeX is not initialized. Call microtex_init() first.");
    }

    // Each parse starts from a clean slate of user-defined macros so that
    // (a) \newcommand/\def in one R call doesn't leak into the next and
    // (b) the typeface mode's automatic path-fallback parse doesn't fail
    // with "Command already exists!" when re-processing the same input.
    NewCommandMacro::clearUserMacros();

    // Toggle glyph rendering mode (guard restores default on any exit)
    RenderModeGuard render_guard;
    MicroTeX::setRenderGlyphUsePath(use_path);

    // Decode foreground color
    color fg = decodeColor(fg_color);

    // Parse LaTeX. Forward MicroTeX exceptions verbatim so users see the
    // original parser message (with offending token / position info)
    // instead of a generic failure.
    std::unique_ptr<Render> render;
    try {
        render.reset(MicroTeX::parse(
            tex,
            max_width,
            text_size,
            line_space,
            fg,
            true,                                   // fillWidth
            resolve_tex_style(tex_style),           // overrideTeXStyle
            math_font,                              // mathFontName
            main_font                               // mainFontFamily
        ));
    } catch (const std::exception& e) {
        Rcpp::stop(std::string("LaTeX parse error: ") + e.what());
    }

    if (!render) {
        Rcpp::stop("Failed to parse LaTeX expression.");
    }

    // Get dimensions
    int width = render->getWidth();
    int height = render->getHeight();
    int depth = render->getDepth();
    float baseline = render->getBaseline();
    bool is_split = render->isSplit();

    // Draw into our recorder
    Graphics2D_Recorder recorder;
    render->draw(recorder, 0, 0);

    // Convert records to R data structures. MARK records are pulled out
    // into a separate marks list so the main layout data frame stays
    // ink-only — most expressions have zero marks, and downstream code
    // shouldn't have to filter them out of every row scan.
    const auto& all_records = recorder.records();
    std::vector<const DrawRecord*> records;
    records.reserve(all_records.size());
    std::vector<const DrawRecord*> mark_records;
    for (const auto& r : all_records) {
        if (r.type == DrawRecord::MARK) {
            mark_records.push_back(&r);
        } else {
            records.push_back(&r);
        }
    }
    int n = static_cast<int>(records.size());

    // Columns for the layout data.frame
    CharacterVector type_col(n);
    NumericVector x_col(n), y_col(n);
    IntegerVector glyph_col(n);
    NumericVector font_size_col(n);
    CharacterVector color_col(n);
    NumericVector x2_col(n), y2_col(n);
    NumericVector w_col(n), h_col(n);
    NumericVector rx_col(n), ry_col(n);
    NumericVector lwd_col(n);
    CharacterVector text_col(n);
    IntegerVector font_style_col(n);
    NumericVector rotation_col(n);
    IntegerVector codepoint_col(n);
    CharacterVector font_file_col(n);

    // Path data stored separately as a list
    Rcpp::List path_list(n);

    for (int i = 0; i < n; i++) {
        const auto& rec = *records[i];

        // rx/ry are only meaningful for round-rect records; default NA.
        rx_col[i] = NA_REAL;
        ry_col[i] = NA_REAL;
        // rotation is only populated for TEXT records today; default 0 (deg).
        rotation_col[i] = 0;

        switch (rec.type) {
            case DrawRecord::GLYPH:
                type_col[i] = "glyph";
                x_col[i] = rec.x;
                y_col[i] = rec.y;
                glyph_col[i] = rec.glyph_id;
                font_size_col[i] = rec.font_size;
                color_col[i] = color_to_hex(rec.col);
                x2_col[i] = NA_REAL;
                y2_col[i] = NA_REAL;
                w_col[i] = NA_REAL;
                h_col[i] = NA_REAL;
                lwd_col[i] = NA_REAL;
                text_col[i] = NA_STRING;
                codepoint_col[i] = NA_INTEGER;
                if (!rec.font_file.empty()) {
                    font_file_col[i] = rec.font_file;
                } else {
                    font_file_col[i] = NA_STRING;
                }
                font_style_col[i] = NA_INTEGER;
                path_list[i] = R_NilValue;
                break;
            case DrawRecord::TEXT:
                type_col[i] = "text";
                x_col[i] = rec.x;
                y_col[i] = rec.y;
                glyph_col[i] = NA_INTEGER;
                font_size_col[i] = rec.font_size;
                color_col[i] = color_to_hex(rec.col);
                x2_col[i] = NA_REAL;
                y2_col[i] = NA_REAL;
                w_col[i] = NA_REAL;
                h_col[i] = NA_REAL;
                lwd_col[i] = NA_REAL;
                text_col[i] = rec.text;
                font_style_col[i] = rec.font_style;
                // radians → degrees (grid textGrob rot= expects degrees, ccw)
                rotation_col[i] = rec.rotation * (180.0 / 3.14159265358979323846);
                codepoint_col[i] = NA_INTEGER;
                font_file_col[i] = NA_STRING;
                path_list[i] = R_NilValue;
                break;
            case DrawRecord::LINE:
                type_col[i] = "line";
                x_col[i] = rec.x1;
                y_col[i] = rec.y1;
                glyph_col[i] = NA_INTEGER;
                font_size_col[i] = NA_REAL;
                color_col[i] = color_to_hex(rec.col);
                x2_col[i] = rec.x2;
                y2_col[i] = rec.y2;
                w_col[i] = NA_REAL;
                h_col[i] = NA_REAL;
                lwd_col[i] = rec.line_width;
                text_col[i] = NA_STRING;
                font_style_col[i] = NA_INTEGER;
                codepoint_col[i] = NA_INTEGER;
                font_file_col[i] = NA_STRING;
                path_list[i] = R_NilValue;
                break;
            case DrawRecord::FILL_RECT:
                type_col[i] = "fill_rect";
                x_col[i] = rec.x;
                y_col[i] = rec.y;
                glyph_col[i] = NA_INTEGER;
                font_size_col[i] = NA_REAL;
                color_col[i] = color_to_hex(rec.col);
                x2_col[i] = NA_REAL;
                y2_col[i] = NA_REAL;
                w_col[i] = rec.width;
                h_col[i] = rec.height;
                lwd_col[i] = NA_REAL;
                text_col[i] = NA_STRING;
                font_style_col[i] = NA_INTEGER;
                codepoint_col[i] = NA_INTEGER;
                font_file_col[i] = NA_STRING;
                path_list[i] = R_NilValue;
                break;
            case DrawRecord::RECT:
                type_col[i] = "rect";
                x_col[i] = rec.x;
                y_col[i] = rec.y;
                glyph_col[i] = NA_INTEGER;
                font_size_col[i] = NA_REAL;
                color_col[i] = color_to_hex(rec.col);
                x2_col[i] = NA_REAL;
                y2_col[i] = NA_REAL;
                w_col[i] = rec.width;
                h_col[i] = rec.height;
                lwd_col[i] = rec.line_width;
                text_col[i] = NA_STRING;
                font_style_col[i] = NA_INTEGER;
                codepoint_col[i] = NA_INTEGER;
                font_file_col[i] = NA_STRING;
                path_list[i] = R_NilValue;
                break;
            case DrawRecord::PATH: {
                type_col[i] = "path";
                x_col[i] = NA_REAL;
                y_col[i] = NA_REAL;
                glyph_col[i] = rec.path_glyph_id >= 0 ? rec.path_glyph_id : NA_INTEGER;
                font_size_col[i] = NA_REAL;
                color_col[i] = color_to_hex(rec.col);
                x2_col[i] = NA_REAL;
                y2_col[i] = NA_REAL;
                w_col[i] = NA_REAL;
                h_col[i] = NA_REAL;
                lwd_col[i] = NA_REAL;

                // Encode path segments
                int nseg = static_cast<int>(rec.path_segments.size());
                CharacterVector seg_cmd(nseg);
                NumericMatrix seg_coords(nseg, 6);
                for (int j = 0; j < nseg; j++) {
                    const auto& seg = rec.path_segments[j];
                    switch (seg.cmd) {
                        case DrawRecord::PathSegment::MOVE:    seg_cmd[j] = "M"; break;
                        case DrawRecord::PathSegment::LINE_TO: seg_cmd[j] = "L"; break;
                        case DrawRecord::PathSegment::CUBIC:   seg_cmd[j] = "C"; break;
                        case DrawRecord::PathSegment::QUAD:    seg_cmd[j] = "Q"; break;
                        case DrawRecord::PathSegment::CLOSE:   seg_cmd[j] = "Z"; break;
                    }
                    for (int k = 0; k < 6; k++) {
                        seg_coords(j, k) = seg.coords[k];
                    }
                }
                path_list[i] = Rcpp::List::create(
                    Named("cmd") = seg_cmd,
                    Named("coords") = seg_coords
                );
                text_col[i] = NA_STRING;
                font_style_col[i] = NA_INTEGER;
                codepoint_col[i] = rec.path_codepoint > 0
                    ? static_cast<int>(rec.path_codepoint) : NA_INTEGER;
                font_file_col[i] = NA_STRING;
                break;
            }
            case DrawRecord::ROUND_RECT:
            case DrawRecord::FILL_ROUND_RECT:
                type_col[i] = (rec.type == DrawRecord::FILL_ROUND_RECT)
                    ? "fill_roundrect" : "roundrect";
                x_col[i] = rec.x;
                y_col[i] = rec.y;
                glyph_col[i] = NA_INTEGER;
                font_size_col[i] = NA_REAL;
                color_col[i] = color_to_hex(rec.col);
                x2_col[i] = NA_REAL;
                y2_col[i] = NA_REAL;
                w_col[i] = rec.width;
                h_col[i] = rec.height;
                rx_col[i] = rec.rx;
                ry_col[i] = rec.ry;
                lwd_col[i] = (rec.type == DrawRecord::ROUND_RECT)
                    ? rec.line_width : NA_REAL;
                text_col[i] = NA_STRING;
                font_style_col[i] = NA_INTEGER;
                codepoint_col[i] = NA_INTEGER;
                font_file_col[i] = NA_STRING;
                path_list[i] = R_NilValue;
                break;
            default:
                // Unknown record type — emit a minimal placeholder.
                type_col[i] = "unknown";
                x_col[i] = NA_REAL;
                y_col[i] = NA_REAL;
                glyph_col[i] = NA_INTEGER;
                font_size_col[i] = NA_REAL;
                color_col[i] = color_to_hex(rec.col);
                x2_col[i] = NA_REAL;
                y2_col[i] = NA_REAL;
                w_col[i] = NA_REAL;
                h_col[i] = NA_REAL;
                lwd_col[i] = NA_REAL;
                text_col[i] = NA_STRING;
                font_style_col[i] = NA_INTEGER;
                codepoint_col[i] = NA_INTEGER;
                font_file_col[i] = NA_STRING;
                path_list[i] = R_NilValue;
                break;
        }
    }

    Rcpp::List result = Rcpp::List::create(
        Named("type") = type_col,
        Named("x") = x_col,
        Named("y") = y_col,
        Named("glyph") = glyph_col,
        Named("font_size") = font_size_col,
        Named("color") = color_col,
        Named("x2") = x2_col,
        Named("y2") = y2_col,
        Named("width") = w_col,
        Named("height") = h_col,
        Named("rx") = rx_col,
        Named("ry") = ry_col,
        Named("lwd") = lwd_col,
        Named("text") = text_col,
        Named("font_style") = font_style_col,
        Named("rotation") = rotation_col,
        Named("path") = path_list,
        Named("codepoint") = codepoint_col,
        Named("font_file") = font_file_col
    );
    result.attr("class") = "data.frame";
    if (n > 0) {
        result.attr("row.names") = Rcpp::seq(1, n);
    } else {
        result.attr("row.names") = Rcpp::IntegerVector::create();
    }

    // Attach bounding box as attributes
    result.attr("bbox_width") = width;
    result.attr("bbox_height") = height;
    result.attr("bbox_depth") = depth;
    result.attr("bbox_baseline") = baseline;
    result.attr("bbox_is_split") = is_split;

    // Marks (\mark{name}) — stored as a small data.frame attribute keyed by
    // name, with x/y in the same world coords as the bounding box (so the
    // R-side gTree can place them in bigpts from the bbox top-left).
    int m = static_cast<int>(mark_records.size());
    CharacterVector mark_name_col(m);
    NumericVector mark_x_col(m), mark_y_col(m);
    for (int i = 0; i < m; i++) {
        mark_name_col[i] = mark_records[i]->mark_name;
        mark_x_col[i] = mark_records[i]->x;
        mark_y_col[i] = mark_records[i]->y;
    }
    Rcpp::List marks_df = Rcpp::List::create(
        Named("name") = mark_name_col,
        Named("x") = mark_x_col,
        Named("y") = mark_y_col
    );
    marks_df.attr("class") = "data.frame";
    if (m > 0) {
        marks_df.attr("row.names") = Rcpp::seq(1, m);
    } else {
        marks_df.attr("row.names") = Rcpp::IntegerVector::create();
    }
    result.attr("marks") = marks_df;

    return result;
}
