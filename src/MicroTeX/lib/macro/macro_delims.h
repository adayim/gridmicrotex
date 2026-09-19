#ifndef MICROTEX_MACRO_DELIMS_H
#define MICROTEX_MACRO_DELIMS_H

#include "atom/atom.h"
#include "atom/atom_basic.h"
#include "atom/atom_delim.h"
#include "atom/atom_fence.h"
#include "atom/atom_misc.h"
#include "atom/atom_root.h"
#include "macro/macro_decl.h"
#include "utils/string_utils.h"
#include "utils/utf.h"

namespace microtex {

inline cmdmacro(overdelim) {
  const auto& name = args.text(0);
  const auto& base = args.formula(1);
  return sptrOf<OverUnderDelimiter>(base, name, true);
}

inline cmdmacro(underdelim) {
  const auto& name = args.text(0);
  const auto& base = args.formula(1);
  return sptrOf<OverUnderDelimiter>(base, name, false);
}

cmdmacro(xarrow);

inline cmdmacro(overline) {
  return sptrOf<OverUnderBar>(args.formula(1), true);
}

inline cmdmacro(underline) {
  return sptrOf<OverUnderBar>(args.formula(1), false);
}

inline macro(Braket) {
  std::string str(args[1]);
  replaceAll(str, "\\|", "\\middle\\vert ");
  return Formula(tp, "\\left\\langle " + str + "\\right\\rangle")._root;
}

inline macro(Set) {
  std::string str(args[1]);
  replaceFirst(str, "\\|", "\\middle\\vert ");
  return Formula(tp, "\\left\\{" + str + "\\right\\}")._root;
}

inline macro(leftparenthesis) {
  std::string grp = tp.getGroup("\\(", "\\)");
  return sptrOf<MathAtom>(Formula(tp, grp, false)._root, TexStyle::text);
}

inline macro(leftbracket) {
  std::string grp = tp.getGroup("\\[", "\\]");
  return sptrOf<MathAtom>(Formula(tp, grp, false)._root, TexStyle::display);
}

inline cmdmacro(middle) {
  return sptrOf<MiddleAtom>(args.text(1));
}

inline cmdmacro(sqrt) {
  if (args.text(2).empty()) return sptrOf<NthRoot>(args.formula(1), nullptr);
  return sptrOf<NthRoot>(args.formula(1), args.formula(2));
}

macro(left);

}  // namespace microtex

#endif  // MICROTEX_MACRO_DELIMS_H
