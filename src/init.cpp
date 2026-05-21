#include <Rcpp.h>
#include "microtex.h"
#include "graphic/graphic.h"
#include "graphic_recorder.h"
#include "mark_atom.h"
#include "unimath/font_src.h"
#include "unimath/uni_font.h"
#include "otf/otf.h"
#include "otf/glyph.h"

#include <array>
#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

// Defined in otf_math_reader.cpp — synthesises a CLM v6 byte blob from
// a font file using FreeType + our OT MATH parser.
std::vector<std::uint8_t> a3_build_clm_bytes(const std::string& path, int index);

using namespace microtex;

// --- Global R callback for text measurement ---
// When set, TextLayout_R::getBounds() calls this R function to get
// accurate font metrics from R's graphics system instead of heuristics.
// Use NULL (not R_NilValue) for static init to avoid DLL load ordering issues.
static SEXP g_text_measure_fn = nullptr;

// Per-measurer cache of (src, style) -> (width, ascent, height) ratios
// relative to font size. Cleared whenever the measurer is swapped so stale
// metrics from a different gp$fontfamily/fontface never leak across calls.
static std::unordered_map<std::string, std::array<float, 3>> g_text_measure_cache;

static bool has_text_measurer() {
    return g_text_measure_fn != nullptr && g_text_measure_fn != R_NilValue;
}

// [[Rcpp::export]]
void register_text_measurer(SEXP fn) {
    if (has_text_measurer()) {
        R_ReleaseObject(g_text_measure_fn);
    }
    g_text_measure_fn = fn;
    R_PreserveObject(g_text_measure_fn);
    g_text_measure_cache.clear();
}

// [[Rcpp::export]]
void clear_text_measurer() {
    if (has_text_measurer()) {
        R_ReleaseObject(g_text_measure_fn);
    }
    g_text_measure_fn = nullptr;
    g_text_measure_cache.clear();
}


// --- UTF-8 helpers for text metrics ---

// Decode one UTF-8 codepoint, advance ptr. Returns 0 on end/error.
static u32 next_codepoint(const char*& p, const char* end) {
    if (p >= end) return 0;
    u32 cp;
    unsigned char c = static_cast<unsigned char>(*p);
    if (c < 0x80) {
        cp = c; p += 1;
    } else if (c < 0xE0) {
        if (p + 1 >= end) { p = end; return 0; }
        cp = (c & 0x1F) << 6 | (p[1] & 0x3F); p += 2;
    } else if (c < 0xF0) {
        if (p + 2 >= end) { p = end; return 0; }
        cp = (c & 0x0F) << 12 | (p[1] & 0x3F) << 6 | (p[2] & 0x3F); p += 3;
    } else {
        if (p + 3 >= end) { p = end; return 0; }
        cp = (c & 0x07) << 18 | (p[1] & 0x3F) << 12 | (p[2] & 0x3F) << 6 | (p[3] & 0x3F);
        p += 4;
    }
    return cp;
}

// Heuristic: is this codepoint a fullwidth/CJK character?
static bool is_fullwidth(u32 cp) {
    return (cp >= 0x3000 && cp <= 0x303F)   // CJK punctuation
        || (cp >= 0x3040 && cp <= 0x309F)   // Hiragana
        || (cp >= 0x30A0 && cp <= 0x30FF)   // Katakana
        || (cp >= 0x4E00 && cp <= 0x9FFF)   // CJK Unified Ideographs
        || (cp >= 0xF900 && cp <= 0xFAFF)   // CJK Compatibility Ideographs
        || (cp >= 0xFF00 && cp <= 0xFFEF)   // Fullwidth forms
        || (cp >= 0x20000 && cp <= 0x2FA1F) // CJK Extension B+ and Compatibility Supplement
        || (cp >= 0xAC00 && cp <= 0xD7AF);  // Hangul Syllables
}

namespace microtex {

// Cache of fontFile → FontContext id. Built lazily on demand — FontContext
// assigns sequential ids on addFont(), so a linear scan of getFont(i) finds
// a match; we memoize so subsequent glyphs hit the cache directly.
static std::unordered_map<std::string, i32> g_font_id_cache;

static sptr<const OtfFont> lookup_otf_font_by_file(const std::string& fontFile) {
    if (fontFile.empty()) return nullptr;
    auto it = g_font_id_cache.find(fontFile);
    if (it != g_font_id_cache.end()) {
        return FontContext::getFont(it->second);
    }
    for (i32 id = 0; ; ++id) {
        auto f = FontContext::getFont(id);
        if (!f) break;
        if (f->fontFile == fontFile) {
            g_font_id_cache[fontFile] = id;
            return f;
        }
    }
    return nullptr;
}

// Shared with graphic_recorder.cpp: glyph advance width (in world units at
// fontSize) used to compensate for horizontal flips on drawGlyph.
float measure_glyph_advance(const std::string& fontFile, u16 glyphId, float fontSize) {
    auto f = lookup_otf_font_by_file(fontFile);
    if (!f) return 0.f;
    const auto& o = f->otf();
    const Glyph* g = o.glyph(glyphId);
    if (!g) return 0.f;
    u16 em = o.em();
    if (em == 0) return 0.f;
    return g->metrics().width() * fontSize / static_cast<float>(em);
}

// Shared with graphic_recorder.cpp: width lookup used to compensate for
// horizontal flips (e.g. \reflectbox) at drawTextRun time.
float measure_cached_text_width(const std::string& text, int fontStyle, float fontSize) {
    if (has_text_measurer()) {
        std::string key;
        key.reserve(text.size() + 3);
        key.push_back(static_cast<char>(fontStyle & 0xFF));
        key.push_back('\x01');
        key.append(text);
        auto it = g_text_measure_cache.find(key);
        if (it != g_text_measure_cache.end()) {
            return it->second[0] * fontSize;
        }
    }
    float total = 0.f;
    const char* p = text.c_str();
    const char* end = p + text.size();
    while (p < end) {
        u32 cp = next_codepoint(p, end);
        if (cp == 0) break;
        total += is_fullwidth(cp) ? fontSize : fontSize * 0.55f;
    }
    return total;
}

}  // namespace microtex

// --- TextLayout implementation that emits TEXT records ---

class TextLayout_R : public TextLayout {
public:
    TextLayout_R(const std::string& src, FontStyle style, float size)
        : _src(src), _style(style), _size(size) {}

    void getBounds(Rect& bounds) override {
        // Try R callback for accurate font metrics
        if (has_text_measurer()) {
            // Cache key: style prefix + '\x01' separator + src. The separator
            // can't appear in UTF-8 text so two different (src, style) pairs
            // cannot collide.
            std::string key;
            key.reserve(_src.size() + 3);
            key.push_back(static_cast<char>(static_cast<int>(_style) & 0xFF));
            key.push_back('\x01');
            key.append(_src);

            auto it = g_text_measure_cache.find(key);
            if (it != g_text_measure_cache.end()) {
                bounds.x = 0;
                bounds.w = it->second[0] * _size;
                bounds.y = -it->second[1] * _size;
                bounds.h = it->second[2] * _size;
                return;
            }

            try {
                Rcpp::Function fn(g_text_measure_fn);
                Rcpp::NumericVector result = fn(_src, static_cast<int>(_style));
                if (result.size() >= 3) {
                    // result = c(width_ratio, ascent_ratio, height_ratio)
                    // Ratios are relative to font size, multiply by _size
                    float wr = static_cast<float>(result[0]);
                    float ar = static_cast<float>(result[1]);
                    float hr = static_cast<float>(result[2]);
                    g_text_measure_cache.emplace(std::move(key),
                                                 std::array<float, 3>{wr, ar, hr});
                    bounds.x = 0;
                    bounds.w = wr * _size;
                    bounds.y = -ar * _size;
                    bounds.h = hr * _size;
                    return;
                }
            } catch (...) {
                // Fall through to heuristic on any error
            }
        }

        // Heuristic fallback: estimate width by iterating Unicode codepoints.
        // CJK/fullwidth chars get ~1.0 em, others ~0.55 em.
        float totalWidth = 0.f;
        const char* p = _src.c_str();
        const char* end = p + _src.size();
        while (p < end) {
            u32 cp = next_codepoint(p, end);
            if (cp == 0) break;
            totalWidth += is_fullwidth(cp) ? _size * 1.0f : _size * 0.55f;
        }
        bounds.x = 0;
        bounds.y = -_size * 0.8f;   // ascent
        bounds.w = totalWidth;
        bounds.h = _size;
    }

    void draw(Graphics2D& g2, float x, float y) override {
        auto* recorder = dynamic_cast<Graphics2D_Recorder*>(&g2);
        if (recorder) {
            recorder->drawTextRun(_src, x, y, static_cast<int>(_style), _size);
        }
    }

private:
    std::string _src;
    FontStyle _style;
    float _size;
};

// --- Platform factory ---

class PlatformFactory_R : public PlatformFactory {
public:
    sptr<Font> createFont(const std::string& file) override {
        return sptrOf<Font_R>(file);
    }

    sptr<TextLayout> createTextLayout(const std::string& src, FontStyle style, float size) override {
        return sptrOf<TextLayout_R>(src, style, size);
    }
};

// --- State ---

static bool s_initialized = false;

// [[Rcpp::export]]
void microtex_init(std::string clm_path, std::string otf_path) {
    if (s_initialized) return;

    PlatformFactory::registerFactory("r", std::make_unique<PlatformFactory_R>());
    PlatformFactory::activate("r");

    FontSrcFile fontSrc(clm_path, otf_path);
    try {
        MicroTeX::init(fontSrc);
    } catch (const std::exception& e) {
        Rcpp::stop(std::string("MicroTeX::init failed: ") + e.what());
    }

    // Default to path rendering (universal compatibility)
    if (MicroTeX::hasGlyphPathRender()) {
        MicroTeX::setRenderGlyphUsePath(true);
    }

    register_mark_macro();
    s_initialized = true;
}

// [[Rcpp::export]]
void microtex_init_from_otf(std::string otf_path, int index = 0) {
    if (s_initialized) return;

    PlatformFactory::registerFactory("r", std::make_unique<PlatformFactory_R>());
    PlatformFactory::activate("r");

    std::vector<std::uint8_t> clm;
    try {
        clm = a3_build_clm_bytes(otf_path, index);
    } catch (const std::exception& e) {
        Rcpp::stop(std::string("Failed to read font '") + otf_path + "': " + e.what());
    }

    FontSrcData fontSrc(clm.size(), clm.data(), otf_path);
    try {
        MicroTeX::init(fontSrc);
    } catch (const std::exception& e) {
        Rcpp::stop(std::string("MicroTeX::init failed: ") + e.what());
    }

    if (MicroTeX::hasGlyphPathRender()) {
        MicroTeX::setRenderGlyphUsePath(true);
    }

    register_mark_macro();
    s_initialized = true;
}

// [[Rcpp::export]]
void microtex_add_font(std::string clm_path, std::string otf_path) {
    if (!s_initialized) {
        Rcpp::stop("MicroTeX is not initialized.");
    }

    FontSrcFile fontSrc(clm_path, otf_path);
    try {
        auto meta = MicroTeX::addFont(fontSrc);
        if (!meta.isValid()) {
            Rcpp::warning("Failed to load font from: " + clm_path);
        }
    } catch (const std::exception& e) {
        Rcpp::warning(std::string("Font load failed: ") + e.what());
    }
}

// [[Rcpp::export]]
std::vector<std::string> microtex_math_font_names() {
    if (!s_initialized) return {};
    return MicroTeX::mathFontNames();
}

// [[Rcpp::export]]
bool microtex_set_default_math_font(std::string name) {
    if (!s_initialized) return false;
    return MicroTeX::setDefaultMathFont(name);
}

// [[Rcpp::export]]
void microtex_release() {
    if (!s_initialized) return;
    MicroTeX::release();
    microtex::g_font_id_cache.clear();
    // release() rebuilds the macro registry on the next init(); drop the
    // \mark guard so register_mark_macro() runs again.
    microtex::reset_mark_macro();
    s_initialized = false;
}

// [[Rcpp::export]]
bool microtex_is_inited() {
    return s_initialized && MicroTeX::isInited();
}

// [[Rcpp::export]]
std::string microtex_version() {
    return MicroTeX::version();
}

// [[Rcpp::export]]
bool microtex_set_default_main_font(std::string family) {
    if (!s_initialized) return false;
    return MicroTeX::setDefaultMainFont(family);
}

// [[Rcpp::export]]
std::vector<std::string> microtex_main_font_families() {
    if (!s_initialized) return {};
    return MicroTeX::mainFontFamilies();
}
