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

void Graphics2D_Recorder::drawGlyph(u16 glyph, float x, float y) {
    DrawRecord rec;
    rec.type = DrawRecord::GLYPH;
    transformPoint(x, y);
    rec.x = x;
    rec.y = y;
    rec.glyph_id = glyph;
    rec.font_size = _currentFontSize * this->sx();
    rec.col = _currentColor;

    // Capture the font file path for glyphGrob rendering
    auto* fr = dynamic_cast<Font_R*>(_currentFont.get());
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
    rec.line_width = _currentStroke.lineWidth * this->sx();
    rec.col = _currentColor;
    _records.push_back(std::move(rec));
}

void Graphics2D_Recorder::drawRect(float x, float y, float w, float h) {
    float sx = this->sx(), sy = this->sy();
    transformPoint(x, y);
    DrawRecord rec;
    rec.type = DrawRecord::RECT;
    rec.x = x; rec.y = y;
    rec.width = w * sx;
    rec.height = h * sy;
    rec.line_width = _currentStroke.lineWidth * sx;
    rec.col = _currentColor;
    _records.push_back(std::move(rec));
}

void Graphics2D_Recorder::fillRect(float x, float y, float w, float h) {
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
    float ssx = this->sx(), ssy = this->sy();
    transformPoint(x, y);
    DrawRecord rec;
    rec.type = DrawRecord::ROUND_RECT;
    rec.x = x; rec.y = y;
    rec.width = w * ssx;
    rec.height = h * ssy;
    rec.rx = rx * ssx;
    rec.ry = ry * ssy;
    rec.line_width = _currentStroke.lineWidth * ssx;
    rec.col = _currentColor;
    _records.push_back(std::move(rec));
}

void Graphics2D_Recorder::fillRoundRect(float x, float y, float w, float h, float rx, float ry) {
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
    transformPoint(x, y);
    float s = this->sx();
    DrawRecord rec;
    rec.type = DrawRecord::TEXT;
    rec.x = x;
    rec.y = y;
    rec.text = text;
    rec.font_style = fontStyle;
    rec.font_size = fontSize * s;
    rec.col = _currentColor;
    _records.push_back(std::move(rec));
}

void Graphics2D_Recorder::clear() {
    _records.clear();
}

}  // namespace microtex
