#ifndef MICROTEX_SPLIT_H
#define MICROTEX_SPLIT_H

#include <stack>

#include "box/box.h"
#include "box/box_group.h"
#include "core/glue.h"

namespace microtex {

#ifdef HAVE_LOG

void printBox(const sptr<Box>& box);

#endif  // HAVE_LOG

class BoxSplitter {
public:
  struct Position {
    int _index;
    sptr<HBox> _box;

    Position(int index, const sptr<HBox>& box) : _index(index), _box(box) {}
  };

private:
  /**
   * Guard against unbounded recursion when descending a nested box tree.
   * Real formulas nest a handful of levels deep; anything beyond this is
   * pathological input and is left unsplit rather than risking the stack.
   */
  static constexpr int MAX_SPLIT_DEPTH = 32;

  static float canBreak(std::stack<Position>& stack, const sptr<HBox>& hbox, float width);

  static int getBreakPosition(const sptr<HBox>& hb, int index);

  static std::pair<bool, sptr<Box>> split(const sptr<HBox>& hb, float width, float lineSpace);

  /**
   * Stretch a line's interword glue so it fills `width` exactly.
   *
   * Only glue reached through a chain of HBoxes is touched, so spaces
   * buried inside a fraction or a matrix cell keep the width their own
   * layout gave them. Returns false when the line has no glue to give,
   * or is already at or past the target, in which case it is left alone.
   */
  static bool justifyLine(const sptr<Box>& line, float width);

  /**
   * Split every over-wide row of a vertical box. Added so that content
   * carrying an explicit line break -- `\\`, array/gather/align, and the
   * itemize/enumerate environments, which all produce a VBox at the top
   * level -- still honours the requested width. Without it the splitter
   * would decline such a box wholesale and the text would simply overflow.
   */
  static std::pair<bool, sptr<Box>> split(
    const sptr<VBox>& vb,
    float width,
    float lineSpace,
    int depth
  );

  static std::pair<bool, sptr<Box>> splitDispatch(
    const sptr<Box>& box,
    float width,
    float lineSpace,
    int depth
  );

public:
  /**
   * Stretch interword glue so every line but the last of a wrapped
   * paragraph fills the measure exactly. Off by default: ragged right is
   * what R's own text drawing does, and justification without
   * hyphenation opens ugly gaps in a narrow column.
   *
   * A static toggle, matching how the engine already handles layout
   * modes (RowAtom::_breakEverywhere, MicroTeX::setRenderGlyphUsePath).
   * Set it under a scope guard so it cannot leak into the next parse.
   */
  static bool _justify;

  static std::pair<bool, sptr<Box>> split(const sptr<Box>& box, float width, float lineSpace);
};

}  // namespace microtex

#endif  // MICROTEX_SPLIT_H
