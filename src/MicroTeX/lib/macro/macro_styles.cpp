#include "macro/macro_styles.h"

#include "atom/atom_basic.h"
#include "microtex.h"
#include "utils/utf.h"

namespace microtex {

TexStyle texStyleOf(const std::string& str) {
  TexStyle style = TexStyle::text;
  if (str == "displaystyle") {
    style = TexStyle::display;
  } else if (str == "textstyle") {
    style = TexStyle::text;
  } else if (str == "scriptstyle") {
    style = TexStyle::script;
  } else if (str == "scriptscriptstyle") {
    style = TexStyle::scriptScript;
  }
  return style;
}

cmdmacro(everymath) {
  if (args.text(1).empty()) {
    MicroTeX::overrideTexStyle(false);
  } else {
    TexStyle style = texStyleOf(args.text(1).substr(1));
    MicroTeX::overrideTexStyle(true, style);
  }
  return nullptr;
}

macro(texstyle) {
  auto g = Formula(tp, tp.getOverArgument(), false)._root;
  g = g == nullptr ? sptrOf<EmptyAtom>() : g;
  TexStyle style = texStyleOf(args[0]);
  return sptrOf<StyleAtom>(style, g);
}

cmdmacro(atexstyle) {
  auto g = args.formula(1);
  return sptrOf<AStyleAtom>(args.text(0), g);
}

}  // namespace microtex
