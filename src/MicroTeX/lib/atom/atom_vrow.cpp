#include "atom/atom_vrow.h"

#include "atom/atom_row.h"
#include "box/box_group.h"
#include "box/box_single.h"
#include "core/split.h"
#include "env/env.h"

using namespace microtex;
using namespace std;

VRowAtom::VRowAtom() {
  _addInterline = false;
  _valign = Alignment::center;
  _halign = Alignment::none;
  _raise = sptrOf<SpaceAtom>(UnitType::ex, 0.f, 0.f, 0.f);
}

VRowAtom::VRowAtom(const sptr<Atom>& base) {
  _addInterline = false;
  _valign = Alignment::center;
  _halign = Alignment::none;
  _raise = sptrOf<SpaceAtom>(UnitType::ex, 0.f, 0.f, 0.f);
  if (base != nullptr) {
    auto* a = dynamic_cast<VRowAtom*>(base.get());
    if (a != nullptr) {
      _elements.insert(_elements.end(), a->_elements.begin(), a->_elements.end());
    } else {
      _elements.push_back(base);
    }
  }
}

void VRowAtom::setRaise(UnitType unit, float r) {
  _raise = sptrOf<SpaceAtom>(unit, r, 0.f, 0.f);
}

void VRowAtom::prepend(const sptr<Atom>& el) {
  if (el != nullptr) _elements.insert(_elements.begin(), el);
}

void VRowAtom::append(const sptr<Atom>& el) {
  if (el != nullptr) _elements.push_back(el);
}

void VRowAtom::addGapAfterLast(const Dimen& gap) {
  if (!_elements.empty()) _gaps[_elements.size() - 1] = gap;
}

namespace {

/** The rows of an alignment display, found inside the boxes it is wrapped in: TeX sets such a display as a vertical list,
 *  so the space above it is measured to its first row's baseline and below it from its last row's. */
const VBox* rowsOf(const sptr<Box>& box) {
  if (const auto* v = dynamic_cast<const VBox*>(box.get())) return v->_children.empty() ? nullptr : v;
  if (const auto* d = dynamic_cast<const DecorBox*>(box.get())) return rowsOf(d->_base);
  if (const auto* h = dynamic_cast<const HBox*>(box.get())) {
    const VBox* found = nullptr;
    for (const auto& child : h->_children) {
      if (child->_height + child->_depth <= 0.f) continue;
      if (found != nullptr) return nullptr;
      found = rowsOf(child);
      if (found == nullptr) return nullptr;
    }
    return found;
  }
  return nullptr;
}

/** The width of the last line `box` makes when it is broken to `width`: what TeX calls predisplaysize, less the 2em. */
float lastLineWidth(const sptr<Box>& box, float width, float baselineSkip) {
  if (width == POS_INF || box->_width <= width) return box->_width;
  const auto [split, lines] = BoxSplitter::split(box, width, baselineSkip);
  const auto vb = std::dynamic_pointer_cast<VBox>(lines);
  if (!split || vb == nullptr || vb->_children.empty()) return box->_width;
  return vb->_children.back()->_width;
}

}  // namespace

sptr<Box> VRowAtom::createBox(Env& env) {
  auto vb = sptrOf<VBox>();
  // Interline space is what makes these a label's lines (getAsVRow()).
  vb->_rows = _addInterline;
  // The space between two lines: TeX's interline glue (Env::interline()), from the depth of the one to the height of the next.
  const auto lineSpace = [&](const sptr<Box>& above, const sptr<Box>& below) {
    return sptrOf<StrutBox>(0.f, env.interline(above->_depth, below->_height), 0.f, 0.f);
  };
  // `\\[len]`: extra space below an element that has one.
  const auto gapAfter = [&](size_t i) {
    const auto it = _gaps.find(i);
    if (it != _gaps.end()) vb->add(sptrOf<StrutBox>(0.f, Units::fsize(it->second, env), 0.f, 0.f));
  };

  if (_halign != Alignment::none) {
    float maxWidth = F_MIN;
    vector<sptr<Box>> boxes;
    const size_t size = _elements.size();
    // find the width of the widest box
    for (auto& atom : _elements) {
      auto box = atom->createBox(env);
      boxes.push_back(box);
      maxWidth = std::max(maxWidth, box->_width);
    }
    // align the boxes and add it to the vertical box
    for (int i = 0; i < size; i++) {
      auto box = boxes[i];
      auto hb = sptrOf<HBox>(box, maxWidth, _halign);
      vb->add(hb);
      if (i < size - 1) gapAfter(i);
      if (_addInterline && i < size - 1) vb->add(lineSpace(box, boxes[i + 1]));
    }
  } else {
    // convert atoms to boxes and add to the vertical box
    const size_t size = _elements.size();
    vector<sptr<Box>> boxes;
    float widest = 0;
    for (auto& atom : _elements) {
      boxes.push_back(atom->createBox(env));
      widest = std::max(widest, boxes.back()->_width);
    }
    // TeX's display skips (\abovedisplayskip and \belowdisplayskip, 1em at 10pt) are long when the line before the
    // display reaches past where the equation starts, 2em of room counted, and short (0 above, 0.6em below) when it
    // does not -- or when no text of the paragraph stands before the display. One choice for the display's two skips.
    const float measure = env.textWidth() == POS_INF ? widest : env.textWidth();
    const auto equation = [&](size_t i) -> const DisplayAtom* {
      const auto* d = dynamic_cast<DisplayAtom*>(_elements[i].get());
      return d != nullptr && d->isEquation() ? d : nullptr;
    };
    vector<int> shortSkips(size, -1);
    const auto isShort = [&](size_t d) {
      if (shortSkips[d] < 0) {
        const DisplayAtom* eq = equation(d);
        bool shortSkip = true;
        if (eq != nullptr && eq->interrupts() && d > 0) {
          const float pre = lastLineWidth(boxes[d - 1], measure, env.baselineSkip());
          const float left =
            eq->alignment() == Alignment::center && !eq->fullWidth() ? (measure - eq->innerWidth()) / 2 : 0.f;
          shortSkip = left > pre + 2 * env.em();
        }
        shortSkips[d] = shortSkip ? 1 : 0;
      }
      return shortSkips[d] == 1;
    };
    for (size_t i = 0; i < size; i++) {
      // A display, or a centred or right-aligned line, with no text width
      // to align in is aligned on the widest line.
      const auto* display = dynamic_cast<DisplayAtom*>(_elements[i].get());
      if (env.textWidth() == POS_INF && display != nullptr) {
        boxes[i] = sptrOf<HBox>(boxes[i], widest, display->alignment());
      }
      vb->add(boxes[i]);
      if (i < size - 1) gapAfter(i);
      if (_addInterline && i < size - 1) {
        // An alignment display is measured from its last row below and to its first row above.
        float depth = boxes[i]->_depth, height = boxes[i + 1]->_height;
        if (const auto* d = dynamic_cast<DisplayAtom*>(_elements[i].get()); d != nullptr && d->fullWidth()) {
          if (const VBox* rows = rowsOf(boxes[i])) depth = rows->_children.back()->_depth;
        }
        if (const auto* d = dynamic_cast<DisplayAtom*>(_elements[i + 1].get()); d != nullptr && d->fullWidth()) {
          if (const VBox* rows = rowsOf(boxes[i + 1])) height = rows->_children.front()->_height;
        }
        vb->add(sptrOf<StrutBox>(0.f, env.interline(depth, height), 0.f, 0.f));
      }
      float skip = 0.f;
      if (i < size - 1 && equation(i + 1) != nullptr) skip += isShort(i + 1) ? 0.f : env.em();
      if (i < size - 1 && equation(i) != nullptr) skip += isShort(i) ? 0.6f * env.em() : env.em();
      if (skip > 0.f) vb->add(sptrOf<StrutBox>(0.f, skip, 0.f, 0.f));
    }
  }

  vb->_shift = -_raise->createBox(env)->_width;
  if (_valign == Alignment::top) {
    float t = vb->size() == 0 ? 0 : vb->_children.front()->_height;
    vb->_height = t;
    vb->_depth = vb->_depth + vb->_height - t;
  } else if (_valign == Alignment::center) {
    const float axis = env.axisHeight();
    const float h = vb->_height + vb->_depth;
    vb->_height = h / 2 + axis;
    vb->_depth = h / 2 - axis;
  } else {
    float t = vb->size() == 0 ? 0 : vb->_children.back()->_depth;
    vb->_height = vb->_depth + vb->_height - t;
    vb->_depth = t;
  }
  return vb;
}

sptr<Box> DisplayAtom::createBox(Env& env) {
  auto box = _base->createBox(env);
  const float width = env.textWidth();
  _innerWidth = std::min(box->_width, width);
  if (width == POS_INF) return box;
  if (box->_width <= width) return sptrOf<HBox>(box, width, _align);
  // Too wide for the line: broken as any line is, and each line it makes
  // aligned -- not justified, as LaTeX's \centering leaves them.
  const bool justify = BoxSplitter::_justify;
  BoxSplitter::_justify = false;
  const auto [split, lines] = BoxSplitter::split(box, width, env.baselineSkip());
  BoxSplitter::_justify = justify;
  const auto vb = std::dynamic_pointer_cast<VBox>(lines);
  if (!split || vb == nullptr) return box;
  auto out = sptrOf<VBox>();
  out->_lines = true;
  for (const auto& line : vb->_children) {
    if (std::dynamic_pointer_cast<HBox>(line) != nullptr) {
      out->add(sptrOf<HBox>(line, width, _align));
    } else {
      out->add(line);  // the space between lines
    }
  }
  return out;
}

namespace {

// The baseline of the last line in `b`, from its top: a paragraph broken
// into lines is a column whose own baseline is its first line.
float lastBaseline(const sptr<Box>& b) {
  const auto v = std::dynamic_pointer_cast<VBox>(b);
  if (v == nullptr || !v->_lines || v->_children.empty()) return b->_height;
  const auto& last = v->_children.back();
  return v->_height + v->_depth - (last->_height + last->_depth) + lastBaseline(last);
}

}  // namespace

sptr<Box> MinipageAtom::createBox(Env& env) {
  if (_body == nullptr) return sptrOf<StrutBox>(0.f, 0.f, 0.f, 0.f);
  const float width = Units::fsize(_width, env);
  const bool measured = width > 0 && width != POS_INF;
  sptr<Box> box;
  {
    // The breaker needs word-level runs, as a p{} cell does: folded into
    // one phrase a paragraph could not be broken.
    const MergeTextGuard guard(true);
    box = env.withTextWidth(measured ? width : env.textWidth(),
                            [&](Env& e) { return _body->createBox(e); });
  }
  if (measured) box = BoxSplitter::split(box, width, env.baselineSkip()).second;

  // A column of lines, whose baseline can be put where the position says.
  auto vb = std::dynamic_pointer_cast<VBox>(box);
  if (vb == nullptr || vb->_children.empty()) {
    vb = sptrOf<VBox>();
    vb->add(box);
  }
  // Measured from its top: the body's own height, and where its first and
  // last baselines are in it -- the last paragraph's last line, when the
  // breaker broke it.
  const float body = vb->_height + vb->_depth;
  const float first = vb->_children.front()->_height;
  const auto& back = vb->_children.back();
  const float last = body - (back->_height + back->_depth) + lastBaseline(back);
  // A height of its own, taller than the body: the room goes where the
  // inner position puts the body (below it for `t`, above for `b`).
  float total = body;
  float above = 0;
  const float height = Units::fsize(_height, env);
  if (_height.isValid() && height > body) {
    const float room = height - body;
    above = _inner == 't' ? 0.f : _inner == 'b' ? room : room / 2;
    total = height;
  }
  const float baseline = _position == 't'   ? above + first
                         : _position == 'b' ? above + last
                                            : total / 2 + env.axisHeight();
  if (above > 0) {
    // Drawn from its top down: a strut there moves the body down by it.
    vb->add(0, sptrOf<StrutBox>(0.f, above, 0.f, 0.f));
  }
  vb->_height = baseline;
  vb->_depth = total - baseline;
  if (!measured) return vb;
  return sptrOf<HBox>(vb, width, Alignment::left);
}

sptr<Box> HangingAtom::createBox(Env& env) {
  auto whole = _whole->createBox(env);
  const float width = env.textWidth();
  if (width == POS_INF || whole->_width <= width) return whole;
  auto lead = _lead->createBox(env);
  const float rest = width - lead->_width;
  if (rest <= 0) return whole;
  const auto [split, lines] = BoxSplitter::split(_rest->createBox(env), rest, env.baselineSkip());
  if (!split) return whole;
  // The title's lines are a column whose baseline is their first line.
  auto row = sptrOf<HBox>();
  row->add(lead);
  row->add(lines);
  return row;
}
