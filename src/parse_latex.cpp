#include <Rcpp.h>
#include "microtex.h"
#include "graphic/graphic.h"
#include "graphic_recorder.h"

using namespace microtex;
using namespace Rcpp;

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

// Helper: encode a Unicode codepoint as UTF-8
static std::string codepoint_to_utf8(uint32_t cp) {
    std::string s;
    if (cp < 0x80) {
        s += static_cast<char>(cp);
    } else if (cp < 0x800) {
        s += static_cast<char>(0xC0 | (cp >> 6));
        s += static_cast<char>(0x80 | (cp & 0x3F));
    } else if (cp < 0x10000) {
        s += static_cast<char>(0xE0 | (cp >> 12));
        s += static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
        s += static_cast<char>(0x80 | (cp & 0x3F));
    } else if (cp < 0x110000) {
        s += static_cast<char>(0xF0 | (cp >> 18));
        s += static_cast<char>(0x80 | ((cp >> 12) & 0x3F));
        s += static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
        s += static_cast<char>(0x80 | (cp & 0x3F));
    }
    return s;
}

// [[Rcpp::export]]
Rcpp::List parse_latex_cpp(std::string tex,
                           float text_size = 20.0,
                           float line_space = 10.0,
                           std::string fg_color = "#000000",
                           float max_width = 0,
                           std::string math_font = "",
                           std::string main_font = "",
                           bool use_path = true) {

    if (!MicroTeX::isInited()) {
        Rcpp::stop("MicroTeX is not initialized. Call microtex_init() first.");
    }

    // Toggle glyph rendering mode
    MicroTeX::setRenderGlyphUsePath(use_path);

    // Decode foreground color
    color fg = decodeColor(fg_color);

    // Parse LaTeX
    Render* render = MicroTeX::parse(
        tex,
        max_width,
        text_size,
        line_space,
        fg,
        true,                               // fillWidth
        {false, TexStyle::text},             // overrideTeXStyle
        math_font,                           // mathFontName
        main_font                            // mainFontFamily
    );

    if (!render) {
        Rcpp::stop("Failed to parse LaTeX expression.");
    }

    // Get dimensions
    int width = render->getWidth();
    int height = render->getHeight();
    int depth = render->getDepth();
    float baseline = render->getBaseline();

    // Draw into our recorder
    Graphics2D_Recorder recorder;
    render->draw(recorder, 0, 0);

    // Clean up render
    delete render;

    // Convert records to R data structures
    const auto& records = recorder.records();
    int n = static_cast<int>(records.size());

    // Columns for the layout data.frame
    CharacterVector type_col(n);
    NumericVector x_col(n), y_col(n);
    IntegerVector glyph_col(n);
    NumericVector font_size_col(n);
    CharacterVector color_col(n);
    NumericVector x2_col(n), y2_col(n);
    NumericVector w_col(n), h_col(n);
    NumericVector lwd_col(n);
    CharacterVector text_col(n);
    IntegerVector font_style_col(n);
    IntegerVector codepoint_col(n);
    CharacterVector font_file_col(n);

    // Path data stored separately as a list
    Rcpp::List path_list(n);

    for (int i = 0; i < n; i++) {
        const auto& rec = records[i];

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
                if (rec.codepoint > 0) {
                    text_col[i] = codepoint_to_utf8(rec.codepoint);
                    codepoint_col[i] = static_cast<int>(rec.codepoint);
                } else {
                    text_col[i] = NA_STRING;
                    codepoint_col[i] = NA_INTEGER;
                }
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
                glyph_col[i] = NA_INTEGER;
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
                codepoint_col[i] = NA_INTEGER;
                font_file_col[i] = NA_STRING;
                break;
            }
            default:
                // ROUND_RECT, FILL_ROUND_RECT — treat as rect for now
                type_col[i] = (rec.type == DrawRecord::FILL_ROUND_RECT) ? "fill_rect" : "rect";
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
        }
    }

    // Restore default path mode
    MicroTeX::setRenderGlyphUsePath(true);

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
        Named("lwd") = lwd_col,
        Named("text") = text_col,
        Named("font_style") = font_style_col,
        Named("path") = path_list,
        Named("codepoint") = codepoint_col,
        Named("font_file") = font_file_col
    );
    result.attr("class") = "data.frame";
    result.attr("row.names") = Rcpp::seq(1, n);

    // Attach bounding box as attributes
    result.attr("bbox_width") = width;
    result.attr("bbox_height") = height;
    result.attr("bbox_depth") = depth;
    result.attr("bbox_baseline") = baseline;

    return result;
}
