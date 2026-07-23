#include "core/split.h"

#include "box/box_group.h"
// glue.h only forward-declares GlueBox; justifyLine() needs the
// definition to reach _stretch and _width.
#include "box/box_single.h"
#include "utils/log.h"

using namespace std;
using namespace microtex;

#ifdef HAVE_LOG

static void printBox(const sptr<Box>& b, int dep, vector<bool>& lines, int max = 0) {
  logv("%-4d", dep);
  if (lines.size() < dep + 1) lines.resize(dep + 1, false);

  for (int i = 0; i < dep - 1; i++) {
    logv(lines[i] ? "    " : " │  ");
  }

  if (dep > 0) {
    logv(lines[dep - 1] ? " └──" : " ├──");
  }

  if (b == nullptr) {
    logv(ANSI_COLOR_RED " NULL\n" ANSI_RESET);
    return;
  }

  const vector<sptr<Box>>& children = b->descendants();
  const auto size = children.size();
  const auto& name = b->name();
  const char* fmt = (size > 0 ? ANSI_COLOR_CYAN " %-*s " ANSI_RESET : " %-*s ");
  logv(fmt, size > 0 ? name.size() : max, name.c_str());
  // show metrics and additional info
  logv(
    "[%g, (%g + %g) = %g, %g] %s\n",
    b->_width,
    b->_height,
    b->_depth,
    b->vlen(),
    b->_shift,
    b->toString().c_str()
  );
  if (size == 0) return;

  size_t limit = 0;
  for (const auto& x : children) {
    limit = std::max(limit, x->name().size());
  }

  for (size_t i = 0; i < size; i++) {
    lines[dep] = i == size - 1;
    printBox(children[i], dep + 1, lines, limit);
  }
}

void microtex::printBox(const sptr<Box>& box) {
  vector<bool> lines;
  ::printBox(box, 0, lines, box->name().size());
  logv("\n");
}

#endif  // HAVE_LOG

bool BoxSplitter::_justify = false;

// Flatten a line into its boxes, left to right, descending only through
// HBoxes. Stopping at any other box type keeps justification to the
// spaces between words on the line: the spaces inside a fraction or a
// matrix cell belong to that construct's own layout, and stretching them
// would pull it apart.
static void collectLineLeaves(const sptr<Box>& b, std::vector<sptr<Box>>& out) {
  auto h = std::dynamic_pointer_cast<HBox>(b);
  if (h == nullptr) {
    out.push_back(b);
    return;
  }
  for (const auto& child : h->_children) collectLineLeaves(child, out);
}

// Re-sum HBox widths after their glue has been widened. HBox::_width is
// maintained as the sum of its children (see HBox::recalculate), so the
// same restricted descent that found the glue can put the widths right.
static float recomputeHBoxWidth(const sptr<Box>& b) {
  auto h = std::dynamic_pointer_cast<HBox>(b);
  if (h == nullptr) return b->_width;
  float w = 0;
  for (const auto& child : h->_children) w += recomputeHBoxWidth(child);
  h->_width = w;
  return w;
}

bool BoxSplitter::justifyLine(const sptr<Box>& line, float width) {
  if (width <= 0 || line == nullptr) return false;

  std::vector<sptr<Box>> leaves;
  collectLineLeaves(line, leaves);

  // A line ends at its last piece of ink. The break leaves the space it
  // broke at sitting on the end of the line, and TeX drops that space
  // rather than stretching it -- keeping it would push the visible text
  // short of the margin by the width of a stretched space, so the line
  // would measure right but still look ragged.
  int last = -1;
  for (int i = static_cast<int>(leaves.size()) - 1; i >= 0; i--) {
    if (!leaves[i]->isSpace()) {
      last = i;
      break;
    }
  }
  if (last < 0) return false;  // nothing but spaces

  float trailing = 0;
  std::vector<sptr<GlueBox>> glue;
  for (int i = 0; i < static_cast<int>(leaves.size()); i++) {
    auto g = std::dynamic_pointer_cast<GlueBox>(leaves[i]);
    if (g == nullptr) continue;
    if (i > last) {
      trailing += g->_width;
      g->_width = 0;
    } else {
      glue.push_back(g);
    }
  }

  // Never squeeze: shrinking is what makes justified text look cramped,
  // and a line already at or past the measure has nothing to give.
  const float slack = width - (line->_width - trailing);
  if (slack <= PREC || glue.empty()) {
    if (trailing > 0) recomputeHBoxWidth(line);
    return trailing > 0;
  }

  float total = 0;
  for (const auto& g : glue) total += g->_stretch;
  if (total <= PREC) {
    if (trailing > 0) recomputeHBoxWidth(line);
    return trailing > 0;
  }

  for (const auto& g : glue) {
    g->_width += slack * (g->_stretch / total);
  }
  recomputeHBoxWidth(line);
  return true;
}

std::pair<bool, sptr<Box>> BoxSplitter::splitDispatch(
  const sptr<Box>& b,
  float width,
  float lineSpace,
  int depth
) {
  if (depth > MAX_SPLIT_DEPTH) return {false, b};
  auto h = dynamic_pointer_cast<HBox>(b);
  if (h != nullptr) return split(h, width, lineSpace);
  auto v = dynamic_pointer_cast<VBox>(b);
  if (v != nullptr) return split(v, width, lineSpace, depth);
  return {false, b};
}

std::pair<bool, sptr<Box>> BoxSplitter::split(
  const sptr<VBox>& vb,
  float width,
  float lineSpace,
  int depth
) {
  if (width <= 0) return {false, vb};

  // Cheap rejection: if every row already fits there is nothing to do,
  // and the box is returned untouched so unsplit content keeps its
  // original object identity and metrics exactly.
  bool needsSplit = false;
  for (const auto& child : vb->_children) {
    if (child->_width > width) {
      needsSplit = true;
      break;
    }
  }
  if (!needsSplit) return {false, vb};

  // A VBox is positioned by its height/depth split, not just its total
  // extent, and callers such as MatrixAtom::createBox overwrite both to
  // centre the box on the math axis after building it. Recover that
  // offset now so it can be reapplied to the taller box below --
  // rebuilding without it silently shifts the baseline of every
  // multi-row formula.
  const float oldTotal = vb->_height + vb->_depth;
  const float axis = vb->_height - oldTotal / 2;

  // NOTE ON REACH: this splits rows that are plain HBoxes, which covers
  // content separated by `\\` at the top level. It deliberately does NOT
  // reach inside matrix/array cells, and so does not wrap long items in
  // itemize/enumerate/align/gather/tabular.
  //
  // Those rows hold WrapperBoxes (see MatrixAtom::createBox), whose
  // _height/_depth are the *row's* metrics, precomputed by
  // recalculateLine() before the VBox exists; MatrixAtom then overwrites
  // the row HBox metrics too. Breaking a cell would therefore have to
  // re-derive every row height and push the new metrics back up through
  // the WrapperBox and row box -- a change inside the matrix layout
  // itself, affecting every table, matrix, cases and align in the
  // package. Callers that need prose to wrap inside list items should
  // stack the items themselves and give each one its own max_width.
  auto out = sptrOf<VBox>();
  bool splitted = false;
  for (const auto& child : vb->_children) {
    if (child->_width > width) {
      auto [childSplit, newChild] = splitDispatch(child, width, lineSpace, depth + 1);
      if (childSplit) splitted = true;
      out->add(newChild);
    } else {
      // Interline struts and rows that already fit pass through as-is.
      out->add(child);
    }
  }

  // Nothing actually broke -- a row can be wider than the limit yet have
  // no legal break position. Keep the original box rather than an
  // equivalent copy.
  if (!splitted) return {false, vb};

  const float newTotal = out->_height + out->_depth;
  out->_height = newTotal / 2 + axis;
  out->_depth = newTotal / 2 - axis;

  return {true, std::static_pointer_cast<Box>(out)};
}

std::pair<bool, sptr<Box>> BoxSplitter::split(const sptr<Box>& b, float width, float lineSpace) {
  auto [splitted, box] = splitDispatch(b, width, lineSpace, 0);
#ifdef HAVE_LOG
  if (box != b) {
    logv("[BEFORE SPLIT]:\n");
    printBox(b);
    logv("[AFTER SPLIT]:\n");
    printBox(box);
  } else {
    logv("[BOX TREE]:\n");
    printBox(box);
  }
#endif
  return {splitted, box};
}

std::pair<bool, sptr<Box>> BoxSplitter::split(const sptr<HBox>& hb, float width, float lineSpace) {
  if (width == 0 || hb->_width <= width) return {false, hb};

  auto* vbox = new VBox();
  sptr<HBox> first, second;
  stack<Position> positions;
  sptr<HBox> hbox = hb;
  bool splitted = false;

  while (hbox->_width > width && canBreak(positions, hbox, width) != hbox->_width) {
    Position pos = positions.top();
    positions.pop();
    auto hboxes = pos._box->split(pos._index - 1);
    first = hboxes.first;
    second = hboxes.second;
    while (!positions.empty()) {
      pos = positions.top();
      positions.pop();
      hboxes = pos._box->splitRemove(pos._index);
      hboxes.first->add(first);
      hboxes.second->add(0, second);
      first = hboxes.first;
      second = hboxes.second;
    }
    // `first` is a completed line and something follows it, so it is
    // never the last line of the paragraph -- the one case TeX leaves
    // ragged. The trailing `second` added after this loop is that line,
    // and is deliberately left alone.
    if (_justify) justifyLine(first, width);
    vbox->add(first, lineSpace);
    splitted = true;
    hbox = second;
  }

  if (second != nullptr) {
    vbox->add(second, lineSpace);
    return {splitted, sptr<Box>(vbox)};
  }

  return {splitted, hbox};
}

float BoxSplitter::canBreak(stack<Position>& s, const sptr<HBox>& hbox, const float width) {
  const vector<sptr<Box>>& children = hbox->_children;
  const int count = children.size();
  // Cumulative width
  auto* cumWidth = new float[count + 1]();
  cumWidth[0] = 0;
  for (int i = 0; i < count; i++) {
    auto box = children[i];
    cumWidth[i + 1] = cumWidth[i] + box->_width;
    if (cumWidth[i + 1] <= width) continue;
    int pos = getBreakPosition(hbox, i);
    auto h = dynamic_pointer_cast<HBox>(box);
    if (h != nullptr) {
      stack<Position> sub;
      float w = canBreak(sub, h, width - cumWidth[i]);
      if (w != box->_width && (cumWidth[i] + w <= width || pos == -1)) {
        s.push(Position(i - 1, hbox));
        // add to stack
        vector<Position> p;
        while (!sub.empty()) {
          p.push_back(sub.top());
          sub.pop();
        }
        for (auto it = p.rbegin(); it != p.rend(); it++) s.push(*it);
        // release cum-width
        float x = cumWidth[i] + w;
        delete[] cumWidth;
        return x;
      }
    }

    if (pos != -1) {
      s.push(Position(pos, hbox));
      float x = cumWidth[pos];
      delete[] cumWidth;
      return x;
    }
  }

  delete[] cumWidth;
  return hbox->_width;
}

int BoxSplitter::getBreakPosition(const sptr<HBox>& hb, int i) {
  if (hb->_breakPositions.empty()) return -1;

  if (hb->_breakPositions.size() == 1 && hb->_breakPositions[0] <= i) {
    return hb->_breakPositions[0];
  }

  size_t pos = 0;
  for (; pos < hb->_breakPositions.size(); pos++) {
    if (hb->_breakPositions[pos] > i) {
      if (pos == 0) return -1;
      return hb->_breakPositions[pos - 1];
    }
  }

  return hb->_breakPositions[pos - 1];
}
