#pragma once

// FontFamilyAtom: carries an arbitrary font family name from LaTeX out to
// the layout data frame, so a run of \text{} can be drawn in a named font.
//
// MicroTeX has no channel for this. Everything a text run carries travels
// in FontStyle, a u16 bitfield whose defined bits stop at tt = 0x80, so it
// can say "monospace" but never "Georgia" -- and \textrm reports the same
// bit as a plain \text{}, so it cannot say "serif" either.
//
// Rather than fork the vendored engine to widen the interface, the family
// rides that same channel as an *index* into a per-parse registry, packed
// into the unused high byte (0xFF00). TextLayout_R turns the index back
// into a name and puts it on the DrawRecord. Nothing in the text path
// normalises a FontStyle it does not recognise: Env::addFontStyle and
// removeFontStyle are unmasked bitwise | and & ~, and findClosestStyle --
// the one function that would -- is only ever applied to *math* styles.

#include <string>
#include <vector>

#include "atom/atom.h"
#include "graphic/font_style.h"

namespace microtex {

// The high byte of FontStyle, where the family index lives.
constexpr u16 kFamilyMask = 0xFF00;
constexpr int kFamilyShift = 8;
constexpr std::size_t kMaxFamilies = 255;

// Guard the packing assumption. If upstream ever moves a style bit into
// the high byte this stops compiling rather than silently mis-rendering.
// (FontStyle::invalid is 0xffff, but a *text* style never takes that
// value: it starts at FontStyle::none and only add/removeTextFontStyle
// touch it.)
constexpr u16 kAllKnownStyleBits =
    static_cast<u16>(FontStyle::rm) | static_cast<u16>(FontStyle::bf) |
    static_cast<u16>(FontStyle::it) | static_cast<u16>(FontStyle::cal) |
    static_cast<u16>(FontStyle::frak) | static_cast<u16>(FontStyle::bb) |
    static_cast<u16>(FontStyle::sf) | static_cast<u16>(FontStyle::tt);
static_assert((kAllKnownStyleBits & kFamilyMask) == 0,
              "A FontStyle bit moved into 0xFF00, which the font family "
              "index packs into. Pick a different mask.");

// --- Per-parse family registry ---------------------------------------
//
// Cleared at the top of every parse_latex_cpp() call. Indices are 1-based;
// 0 means "no family, use the caller's default".

// Register `name`, returning its index. Repeated names reuse their index.
// Returns 0 once kMaxFamilies is exhausted, degrading to the default font
// rather than wrapping round onto someone else's family.
int register_font_family(const std::string& name);

// Name for an index, or "" for 0 / out of range.
const std::string& font_family_name(int index);

// Family carried by a FontStyle, or "" if it carries none.
const std::string& font_family_of_style(int font_style);

void clear_font_families();

// --- Atom ------------------------------------------------------------

class FontFamilyAtom : public Atom {
public:
    FontFamilyAtom(int index, sptr<Atom> atom)
        : _index(index), _atom(std::move(atom)) {}

    sptr<Box> createBox(Env& env) override;

private:
    int _index;
    sptr<Atom> _atom;
};

// Register the \gmfontfamily{name}{content} macro. Internal: emitted by
// R/markdown.R for CSS font-family, not part of the documented dialect.
// Idempotent within one MicroTeX init lifetime.
void register_font_family_macro();

// Clear the registration guard, for microtex_release(), which rebuilds
// MicroTeX's macro registry from scratch.
void reset_font_family_macro();

}  // namespace microtex
