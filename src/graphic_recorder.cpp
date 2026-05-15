#include "graphic_recorder.h"

namespace microtex {

Graphics2D_Recorder::Graphics2D_Recorder() = default;

void Graphics2D_Recorder::setColor(color c) { _currentColor = c; }
color Graphics2D_Recorder::getColor() const { return _currentColor; }
void Graphics2D_Recorder::setStroke(const Stroke& s) { _currentStroke = s; }
const Stroke& Graphics2D_Recorder::getStroke() const { return _currentStroke; }
void Graphics2D_Recorder::setStrokeWidth(float w) { _currentStroke.lineWidth = w; }
void Graphics2D_Recorder::setDash(const std::vector<float>& dash) { /* ignored */ }
std::vector<float> Graphics2D_Recorder::getDash() { return {}; }

sptr<Font> Graphics2D_Recorder::getFont() const { return _currentFont; }
void Graphics2D_Recorder::setFont(const sptr<Font>& font) { _currentFont = font; }
float Graphics2D_Recorder::getFontSize() const { return _currentFontSize; }
void Graphics2D_Recorder::setFontSize(float size) { _currentFontSize = size; }

void Graphics2D_Recorder::translate(float dx, float dy) {
    _transform.tx += _transform.a * dx + _transform.c * dy;
    _transform.ty += _transform.b * dx + _transform.d * dy;
}

void Graphics2D_Recorder::scale(float sx, float sy) {
    _transform.a *= sx;
    _transform.b *= sx;
    _transform.c *= sy;
    _transform.d *= sy;
}

void Graphics2D_Recorder::rotate(float angle) {
    float cosA = std::cos(angle);
    float sinA = std::sin(angle);
    float a = _transform.a, b = _transform.b;
    float c = _transform.c, d = _transform.d;
    _transform.a = a * cosA + c * sinA;
    _transform.b = b * cosA + d * sinA;
    _transform.c = -a * sinA + c * cosA;
    _transform.d = -b * sinA + d * cosA;
}

void Graphics2D_Recorder::rotate(float angle, float px, float py) {
    translate(px, py);
    rotate(angle);
    translate(-px, -py);
}

void Graphics2D_Recorder::reset() {
    _transform = Transform{};
}

float Graphics2D_Recorder::sx() const {
    return std::sqrt(_transform.a * _transform.a + _transform.b * _transform.b);
}

float Graphics2D_Recorder::sy() const {
    return std::sqrt(_transform.c * _transform.c + _transform.d * _transform.d);
}

void Graphics2D_Recorder::transformPoint(float& x, float& y) const {
    float nx = _transform.a * x + _transform.c * y + _transform.tx;
    float ny = _transform.b * x + _transform.d * y + _transform.ty;
    x = nx;
    y = ny;
}

void Graphics2D_Recorder::recordMark(const std::string& name, float x, float y) {
    DrawRecord rec;
    rec.type = DrawRecord::MARK;
    transformPoint(x, y);
    rec.x = x;
    rec.y = y;
    rec.mark_name = name;
    _records.push_back(std::move(rec));
}

void Graphics2D_Recorder::drawGlyph(u16 glyph, float x, float y) {
    auto* fr = dynamic_cast<Font_R*>(_currentFont.get());
    // Under a horizontal flip (\reflectbox), mirror the same fix as for
    // drawTextRun: shift the local x by the glyph's advance width so that
    // transformPoint lands on the visible LEFT edge of the glyph box. Without
    // this the anchor is at the post-transform RIGHT edge and glyphGrob
    // (which draws to the right of its anchor) produces overlap.
    if (_transform.a < 0 && fr && !fr->fontFile.empty()) {
        x += measure_glyph_advance(fr->fontFile, glyph, _currentFontSize);
    }
    DrawRecord rec;
    rec.type = DrawRecord::GLYPH;
    transformPoint(x, y);
    rec.x = x;
    rec.y = y;
    rec.glyph_id = glyph;
    rec.font_size = _currentFontSize * this->sx();
    rec.col = _currentColor;

    // Capture the font file path for glyphGrob rendering
    if (fr && !fr->fontFile.empty()) {
        rec.font_file = fr->fontFile;
    }

    _records.push_back(std::move(rec));
}

bool Graphics2D_Recorder::beginPath(i32 id) {
    _currentPath.clear();
    return false;
}

void Graphics2D_Recorder::moveTo(float x, float y) {
    transformPoint(x, y);
    DrawRecord::PathSegment seg;
    seg.cmd = DrawRecord::PathSegment::MOVE;
    seg.coords[0] = x;
    seg.coords[1] = y;
    _currentPath.push_back(seg);
}

void Graphics2D_Recorder::lineTo(float x, float y) {
    transformPoint(x, y);
    DrawRecord::PathSegment seg;
    seg.cmd = DrawRecord::PathSegment::LINE_TO;
    seg.coords[0] = x;
    seg.coords[1] = y;
    _currentPath.push_back(seg);
}

void Graphics2D_Recorder::cubicTo(float x1, float y1, float x2, float y2, float x3, float y3) {
    transformPoint(x1, y1);
    transformPoint(x2, y2);
    transformPoint(x3, y3);
    DrawRecord::PathSegment seg;
    seg.cmd = DrawRecord::PathSegment::CUBIC;
    seg.coords[0] = x1; seg.coords[1] = y1;
    seg.coords[2] = x2; seg.coords[3] = y2;
    seg.coords[4] = x3; seg.coords[5] = y3;
    _currentPath.push_back(seg);
}

void Graphics2D_Recorder::quadTo(float x1, float y1, float x2, float y2) {
    transformPoint(x1, y1);
    transformPoint(x2, y2);
    DrawRecord::PathSegment seg;
    seg.cmd = DrawRecord::PathSegment::QUAD;
    seg.coords[0] = x1; seg.coords[1] = y1;
    seg.coords[2] = x2; seg.coords[3] = y2;
    _currentPath.push_back(seg);
}

void Graphics2D_Recorder::closePath() {
    DrawRecord::PathSegment seg;
    seg.cmd = DrawRecord::PathSegment::CLOSE;
    _currentPath.push_back(seg);
}

void Graphics2D_Recorder::fillPath(i32 id) {
    DrawRecord rec;
    rec.type = DrawRecord::PATH;
    rec.col = _currentColor;
    rec.path_segments = std::move(_currentPath);
    rec.path_glyph_id = _pendingPathGlyphId;
    rec.path_codepoint = _pendingPathCodepoint;
    _currentPath.clear();
    _pendingPathGlyphId = -1;
    _pendingPathCodepoint = 0;
    _records.push_back(std::move(rec));
}

void Graphics2D_Recorder::setPathGlyphInfo(i32 glyphId, c32 codepoint) {
    _pendingPathGlyphId = glyphId;
    _pendingPathCodepoint = codepoint;
}

void Graphics2D_Recorder::drawLine(float x1, float y1, float x2, float y2) {
    transformPoint(x1, y1);
    transformPoint(x2, y2);
    DrawRecord rec;
    rec.type = DrawRecord::LINE;
    rec.x1 = x1; rec.y1 = y1;
    rec.x2 = x2; rec.y2 = y2;
    // Average sx and sy so asymmetric scales (rare in MicroTeX) give a
    // reasonable thickness instead of following only the x-scale.
    rec.line_width = _currentStroke.lineWidth * 0.5f * (this->sx() + this->sy());
    rec.col = _currentColor;
    _records.push_back(std::move(rec));
}

// Test for rotation/shear in the current transform. When true, an
// axis-aligned rect in local coords becomes a tilted quadrilateral in world
// coords — grid's rectGrob has no rotation parameter, so we emit the four
// transformed corners as LINE records (stroked) or a PATH (filled) instead.
static bool transform_has_rotation(float b, float c) {
    constexpr float eps = 1e-6f;
    return std::abs(b) > eps || std::abs(c) > eps;
}

void Graphics2D_Recorder::drawRect(float x, float y, float w, float h) {
    if (transform_has_rotation(_transform.b, _transform.c)) {
        float cx[4] = {x, x + w, x + w, x};
        float cy[4] = {y, y, y + h, y + h};
        for (int i = 0; i < 4; ++i) transformPoint(cx[i], cy[i]);
        float lw = _currentStroke.lineWidth * 0.5f * (this->sx() + this->sy());
        for (int i = 0; i < 4; ++i) {
            int j = (i + 1) & 3;
            DrawRecord rec;
            rec.type = DrawRecord::LINE;
            rec.x1 = cx[i]; rec.y1 = cy[i];
            rec.x2 = cx[j]; rec.y2 = cy[j];
            rec.line_width = lw;
            rec.col = _currentColor;
            _records.push_back(std::move(rec));
        }
        return;
    }
    float sx = this->sx(), sy = this->sy();
    transformPoint(x, y);
    DrawRecord rec;
    rec.type = DrawRecord::RECT;
    rec.x = x; rec.y = y;
    rec.width = w * sx;
    rec.height = h * sy;
    rec.line_width = _currentStroke.lineWidth * 0.5f * (sx + sy);
    rec.col = _currentColor;
    _records.push_back(std::move(rec));
}

void Graphics2D_Recorder::fillRect(float x, float y, float w, float h) {
    if (transform_has_rotation(_transform.b, _transform.c)) {
        float cx[4] = {x, x + w, x + w, x};
        float cy[4] = {y, y, y + h, y + h};
        for (int i = 0; i < 4; ++i) transformPoint(cx[i], cy[i]);
        DrawRecord rec;
        rec.type = DrawRecord::PATH;
        rec.col = _currentColor;
        for (int i = 0; i < 4; ++i) {
            DrawRecord::PathSegment seg;
            seg.cmd = (i == 0) ? DrawRecord::PathSegment::MOVE
                               : DrawRecord::PathSegment::LINE_TO;
            seg.coords[0] = cx[i];
            seg.coords[1] = cy[i];
            rec.path_segments.push_back(seg);
        }
        DrawRecord::PathSegment close;
        close.cmd = DrawRecord::PathSegment::CLOSE;
        rec.path_segments.push_back(close);
        _records.push_back(std::move(rec));
        return;
    }
    float sx = this->sx(), sy = this->sy();
    transformPoint(x, y);
    DrawRecord rec;
    rec.type = DrawRecord::FILL_RECT;
    rec.x = x; rec.y = y;
    rec.width = w * sx;
    rec.height = h * sy;
    rec.col = _currentColor;
    _records.push_back(std::move(rec));
}

void Graphics2D_Recorder::drawRoundRect(float x, float y, float w, float h, float rx, float ry) {
    if (transform_has_rotation(_transform.b, _transform.c)) {
        // Under rotation, drop the rounding and emit a stroked quadrilateral.
        // Rounded corners under arbitrary rotation would require elliptical
        // arcs in rotated space, which grid can't render directly.
        drawRect(x, y, w, h);
        return;
    }
    float ssx = this->sx(), ssy = this->sy();
    transformPoint(x, y);
    DrawRecord rec;
    rec.type = DrawRecord::ROUND_RECT;
    rec.x = x; rec.y = y;
    rec.width = w * ssx;
    rec.height = h * ssy;
    rec.rx = rx * ssx;
    rec.ry = ry * ssy;
    rec.line_width = _currentStroke.lineWidth * 0.5f * (ssx + ssy);
    rec.col = _currentColor;
    _records.push_back(std::move(rec));
}

void Graphics2D_Recorder::fillRoundRect(float x, float y, float w, float h, float rx, float ry) {
    if (transform_has_rotation(_transform.b, _transform.c)) {
        // Same compromise as drawRoundRect: drop the corners under rotation.
        fillRect(x, y, w, h);
        return;
    }
    float ssx = this->sx(), ssy = this->sy();
    transformPoint(x, y);
    DrawRecord rec;
    rec.type = DrawRecord::FILL_ROUND_RECT;
    rec.x = x; rec.y = y;
    rec.width = w * ssx;
    rec.height = h * ssy;
    rec.rx = rx * ssx;
    rec.ry = ry * ssy;
    rec.col = _currentColor;
    _records.push_back(std::move(rec));
}

void Graphics2D_Recorder::drawTextRun(const std::string& text, float x, float y,
                                       int fontStyle, float fontSize) {
    // Under a horizontal flip (e.g. \reflectbox), transformPoint(x, y) maps
    // the local LEFT edge of the glyph box to what becomes the visible RIGHT
    // edge in post-transform coords. textGrob with hjust=0 would then draw
    // rightward from that right edge, overlapping adjacent glyphs. Shift to
    // the local RIGHT edge before transforming so the recorded x is the
    // visible LEFT edge, i.e. where drawing should actually start.
    if (_transform.a < 0) {
        x += measure_cached_text_width(text, fontStyle, fontSize);
    }
    // Extract rotation separately from any horizontal flip: pure reflect
    // gives 0 (the flip is already handled by the x shift above), pure
    // rotate gives atan2(b, a). When a<0 we flip the x column before
    // atan2 to cancel the reflect component.
    float rot = (_transform.a < 0)
        ? std::atan2(-_transform.b, -_transform.a)
        : std::atan2(_transform.b, _transform.a);
    transformPoint(x, y);
    float s = this->sx();
    DrawRecord rec;
    rec.type = DrawRecord::TEXT;
    rec.x = x;
    rec.y = y;
    rec.text = text;
    rec.font_style = fontStyle;
    rec.font_size = fontSize * s;
    rec.rotation = rot;
    rec.col = _currentColor;
    _records.push_back(std::move(rec));
}

void Graphics2D_Recorder::clear() {
    _records.clear();
}

}  // namespace microtex
