#pragma once

#include "graphic/graphic.h"
#include "utils/utils.h"

#include <vector>
#include <string>
#include <array>
#include <cmath>

namespace microtex {

// Platform Font that carries the font file path for typeface rendering
class Font_R : public Font {
public:
    std::string fontFile;
    Font_R() = default;
    explicit Font_R(const std::string& file) : fontFile(file) {}
    bool operator==(const Font& f) const override {
        auto* other = dynamic_cast<const Font_R*>(&f);
        return other && other->fontFile == fontFile;
    }
};

// One recorded draw operation
struct DrawRecord {
    enum Type { GLYPH, LINE, RECT, FILL_RECT, ROUND_RECT, FILL_ROUND_RECT, PATH, TEXT };
    Type type = GLYPH;

    // For GLYPH
    float x = 0, y = 0;
    u16 glyph_id = 0;
    float font_size = 0;
    color col = black;
    std::string font_file;      // OTF font file path (for typeface mode)

    // For TEXT (non-math text from \text{}, etc.)
    std::string text;
    int font_style = 0;  // FontStyle cast to int
    float rotation = 0;  // radians, extracted from current transform (ccw)

    // For LINE / RECT / FILL_RECT / ROUND_RECT / FILL_ROUND_RECT
    float x1 = 0, y1 = 0, x2 = 0, y2 = 0;
    float width = 0, height = 0;
    float rx = 0, ry = 0;
    float line_width = 1.0f;

    // For PATH: stored as a list of segments
    struct PathSegment {
        enum Cmd { MOVE, LINE_TO, CUBIC, QUAD, CLOSE };
        Cmd cmd = MOVE;
        float coords[6] = {};
    };
    std::vector<PathSegment> path_segments;

    // For PATH: glyph identification (set via setPathGlyphInfo before drawing)
    i32 path_glyph_id = -1;
    c32 path_codepoint = 0;
};

// A Graphics2D that records all draw operations
class Graphics2D_Recorder : public Graphics2D {
public:
    Graphics2D_Recorder();
    ~Graphics2D_Recorder() override = default;

    // --- Graphics2D interface ---
    void setColor(color c) override;
    color getColor() const override;
    void setStroke(const Stroke& s) override;
    const Stroke& getStroke() const override;
    void setStrokeWidth(float w) override;
    void setDash(const std::vector<float>& dash) override;
    std::vector<float> getDash() override;

    sptr<Font> getFont() const override;
    void setFont(const sptr<Font>& font) override;
    float getFontSize() const override;
    void setFontSize(float size) override;

    void translate(float dx, float dy) override;
    void scale(float sx, float sy) override;
    void rotate(float angle) override;
    void rotate(float angle, float px, float py) override;
    void reset() override;
    float sx() const override;
    float sy() const override;

    void drawGlyph(u16 glyph, float x, float y) override;

    bool beginPath(i32 id) override;
    void moveTo(float x, float y) override;
    void lineTo(float x, float y) override;
    void cubicTo(float x1, float y1, float x2, float y2, float x3, float y3) override;
    void quadTo(float x1, float y1, float x2, float y2) override;
    void closePath() override;
    void fillPath(i32 id) override;
    void setPathGlyphInfo(i32 glyphId, c32 codepoint) override;

    void drawLine(float x1, float y1, float x2, float y2) override;
    void drawRect(float x, float y, float w, float h) override;
    void fillRect(float x, float y, float w, float h) override;
    void drawRoundRect(float x, float y, float w, float h, float rx, float ry) override;
    void fillRoundRect(float x, float y, float w, float h, float rx, float ry) override;

    // --- Text run (called by TextLayout_R) ---
    void drawTextRun(const std::string& text, float x, float y, int fontStyle, float fontSize);

    // --- Extraction ---
    const std::vector<DrawRecord>& records() const { return _records; }
    void clear();

private:
    std::vector<DrawRecord> _records;
    color _currentColor = black;
    Stroke _currentStroke;
    sptr<Font> _currentFont;
    float _currentFontSize = 10.f;

    // Affine transform: [a, b, c, d, tx, ty]
    // Maps (x,y) -> (a*x + c*y + tx, b*x + d*y + ty)
    struct Transform {
        float a = 1, b = 0, c = 0, d = 1, tx = 0, ty = 0;
    };
    Transform _transform;

    // For building paths
    std::vector<DrawRecord::PathSegment> _currentPath;

    // Glyph info for the next path record
    i32 _pendingPathGlyphId = -1;
    c32 _pendingPathCodepoint = 0;

    // Apply current transform to a point
    void transformPoint(float& x, float& y) const;
};

// Defined in init.cpp. Returns the pre-transform width (in local coords, at
// the given fontSize) of a text run, using the measurement cache populated
// by TextLayout_R::getBounds; falls back to a codepoint-based heuristic.
float measure_cached_text_width(const std::string& text, int fontStyle, float fontSize);

// Defined in init.cpp. Returns the advance width (in world units at the given
// fontSize) of a single glyph in the font identified by fontFile. Used to
// shift the draw anchor under a horizontal flip. Returns 0 when the font or
// glyph is not found.
float measure_glyph_advance(const std::string& fontFile, u16 glyphId, float fontSize);

}  // namespace microtex
