#include <Rcpp.h>
#include "microtex.h"
#include "graphic/graphic.h"
#include "graphic_recorder.h"
#include "unimath/font_src.h"
#include "unimath/uni_font.h"
#include "otf/otf.h"
#include "utils/utils.h"

#include <unordered_map>
#include <string>

using namespace microtex;

// --- Reverse cmap: glyph_id → Unicode codepoint, keyed by font file ---
// Built at font load time by reading the CLM binary cmap section directly.
static std::unordered_map<std::string,
                          std::unordered_map<u16, u32>> g_reverse_cmap;

// Read the cmap from a CLM file and build glyph→unicode reverse map.
// CLM binary format: 'c','l','m', u16 major, u8 minor, then readMeta:
//   null-terminated name, null-terminated family,
//   u8 isMathFont, u16 style, u16 em, u16 xHeight, u16 ascent, u16 descent,
//   u16 count, then count × (u32 unicode, u16 glyphId)
static void build_reverse_cmap_from_clm(const std::string& font_file,
                                         const std::string& clm_path) {
    FILE* f = fopen(clm_path.c_str(), "rb");
    if (!f) return;

    auto read_u8 = [&]() -> uint8_t {
        uint8_t v = 0;
        if (fread(&v, 1, 1, f) != 1) return 0;
        return v;
    };
    auto read_u16 = [&]() -> uint16_t {
        uint8_t buf[2];
        if (fread(buf, 1, 2, f) != 2) return 0;
        return (static_cast<uint16_t>(buf[0]) << 8) | buf[1];
    };
    auto read_u32 = [&]() -> uint32_t {
        uint8_t buf[4];
        if (fread(buf, 1, 4, f) != 4) return 0;
        return (static_cast<uint32_t>(buf[0]) << 24) |
               (static_cast<uint32_t>(buf[1]) << 16) |
               (static_cast<uint32_t>(buf[2]) << 8) |
               buf[3];
    };
    auto skip_string = [&]() {
        // Read null-terminated string
        int ch;
        do { ch = fgetc(f); } while (ch != 0 && ch != EOF);
    };

    // Header: 'c','l','m', u16 ver, u8 minor
    read_u8(); read_u8(); read_u8(); // magic
    read_u16();  // major version
    read_u8();   // minor version

    // Meta: name, family (null-terminated strings)
    skip_string(); // name
    skip_string(); // family

    // u8 isMathFont, u16 style, u16 em, u16 xHeight, u16 ascent, u16 descent
    read_u8();   // isMathFont
    read_u16();  // style
    read_u16();  // em
    read_u16();  // xHeight
    read_u16();  // ascent
    read_u16();  // descent

    // cmap: u16 count, then count × (u32 unicode, u16 glyphId)
    uint16_t count = read_u16();
    auto& rmap = g_reverse_cmap[font_file];
    for (uint16_t i = 0; i < count; i++) {
        uint32_t unicode = read_u32();
        uint16_t glyph = read_u16();
        // Keep first (lowest) codepoint for each glyph
        if (rmap.find(glyph) == rmap.end()) {
            rmap[glyph] = unicode;
        }
    }

    fclose(f);
}

// External linkage — also used by graphic_recorder.cpp
u32 reverse_cmap_lookup(const std::string& font_file, u16 glyph_id) {
    auto it = g_reverse_cmap.find(font_file);
    if (it == g_reverse_cmap.end()) return 0;
    auto git = it->second.find(glyph_id);
    if (git == it->second.end()) return 0;
    return git->second;
}

// Supplement the reverse cmap by walking math glyph variants (ssty, vert, horiz).
// Many glyphs used at script/scriptscript sizes are variants of base glyphs and
// have no direct cmap entries.  This maps them back to their base unicode.
[[maybe_unused]] static void supplement_reverse_cmap_from_otf(const std::string& font_file) {
    auto it = g_reverse_cmap.find(font_file);
    if (it == g_reverse_cmap.end()) return;

    auto& rmap = it->second;

    // Find the OtfFont matching this font_file by scanning all loaded fonts
    const Otf* otf_ptr = nullptr;
    for (i32 id = 0; ; id++) {
        auto otfFont = FontContext::getFont(id);
        if (!otfFont) break;
        if (otfFont->fontFile == font_file) {
            otf_ptr = &otfFont->otf();
            break;
        }
    }
    if (!otf_ptr) return;

    const Otf& otf = *otf_ptr;

    // Snapshot current base glyph→unicode pairs (we'll modify rmap in-place)
    std::vector<std::pair<u16, u32>> bases(rmap.begin(), rmap.end());

    // For each base glyph with a cmap entry, walk its math variants
    for (auto& [base_glyph, base_unicode] : bases) {
        const Glyph* g = otf.glyph(base_glyph);
        if (!g) continue;

        auto add_variants = [&](const Variants& vars) {
            for (u16 j = 0; j < vars.count(); j++) {
                u16 var_glyph = vars[j];
                if (rmap.find(var_glyph) == rmap.end()) {
                    rmap[var_glyph] = base_unicode;
                }
            }
        };

        add_variants(g->math().scriptsVariants());
        add_variants(g->math().verticalVariants());
        add_variants(g->math().horizontalVariants());
    }
}

// --- Global R callback for text measurement ---
// When set, TextLayout_R::getBounds() calls this R function to get
// accurate font metrics from R's graphics system instead of heuristics.
// Use NULL (not R_NilValue) for static init to avoid DLL load ordering issues.
static SEXP g_text_measure_fn = nullptr;

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
}

// [[Rcpp::export]]
void clear_text_measurer() {
    if (has_text_measurer()) {
        R_ReleaseObject(g_text_measure_fn);
    }
    g_text_measure_fn = nullptr;
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

// --- TextLayout implementation that emits TEXT records ---

class TextLayout_R : public TextLayout {
public:
    TextLayout_R(const std::string& src, FontStyle style, float size)
        : _src(src), _style(style), _size(size) {}

    void getBounds(Rect& bounds) override {
        // Try R callback for accurate font metrics
        if (has_text_measurer()) {
            try {
                Rcpp::Function fn(g_text_measure_fn);
                Rcpp::NumericVector result = fn(_src, static_cast<int>(_style));
                if (result.size() >= 3) {
                    // result = c(width_ratio, ascent_ratio, height_ratio)
                    // Ratios are relative to font size, multiply by _size
                    bounds.x = 0;
                    bounds.w = static_cast<float>(result[0]) * _size;
                    bounds.y = -static_cast<float>(result[1]) * _size;
                    bounds.h = static_cast<float>(result[2]) * _size;
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

    // Build direct glyph->Unicode reverse cmap for typeface mode.
    // Keep this map strict (no variant backfilling): stretchy/script-size
    // glyph variants do not correspond to unique Unicode codepoints.
    // These are handled by an R-side path fallback pass.
    build_reverse_cmap_from_clm(otf_path, clm_path);

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
        } else {
            // Build direct reverse cmap for this font too.
            build_reverse_cmap_from_clm(otf_path, clm_path);
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
    g_reverse_cmap.clear();
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
void microtex_set_render_path(bool use_path) {
    if (s_initialized) {
        MicroTeX::setRenderGlyphUsePath(use_path);
    }
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
