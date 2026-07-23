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
  static std::pair<bool, sptr<Box>> split(const sptr<Box>& box, float width, float lineSpace);
};

}  // namespace microtex

#endif  // MICROTEX_SPLIT_H
