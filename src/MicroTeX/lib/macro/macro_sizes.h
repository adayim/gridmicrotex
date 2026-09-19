#ifndef MICROTEX_MACRO_SIZES_H
#define MICROTEX_MACRO_SIZES_H

#include "atom/atom_basic.h"
#include "atom/atom_char.h"
#include "atom/atom_misc.h"
#include "macro/macro_decl.h"
#include "utils/string_utils.h"

namespace microtex {

inline cmdmacro(declaremathsizes) {
  float a, b, c, d;
  valueOf(args.text(1), a), valueOf(args.text(2), b), valueOf(args.text(3), c), valueOf(args.text(4), d);
  // TODO setMathSizes(a, b, c, d);
  return nullptr;
}

inline cmdmacro(magnification) {
  float x;
  valueOf(args.text(1), x);
  // TODO setMagnification(x);
  return nullptr;
}

inline macro(sizes) {
  float f = 1;
  if (args[0] == "tiny")
    f = 0.5f;
  else if (args[0] == "scriptsize")
    f = 0.7f;
  else if (args[0] == "footnotesize")
    f = 0.8f;
  else if (args[0] == "small")
    f = 0.9f;
  else if (args[0] == "normalsize")
    f = 1.f;
  else if (args[0] == "large")
    f = 1.2f;
  else if (args[0] == "Large")
    f = 1.4f;
  else if (args[0] == "LARGE")
    f = 1.8f;
  else if (args[0] == "huge")
    f = 2.f;
  else if (args[0] == "Huge")
    f = 2.5f;

  auto a = Formula(tp, tp.getOverArgument(), false, tp.isMathMode())._root;
  a = a == nullptr ? sptrOf<EmptyAtom>() : a;
  return sptrOf<ScaleAtom>(a, f);
}

inline sptr<Atom> _big(CommandArgs& args, int size, AtomType type = AtomType::none) {
  auto a = args.formula(1);
  auto s = std::dynamic_pointer_cast<SymbolAtom>(a);
  if (s == nullptr) return a;
  auto t = sptrOf<BigSymbolAtom>(s, size);
  if (type != AtomType::none) t->_type = type;
  return t;
}

inline cmdmacro(big) {
  return _big(args, 1);
}

inline cmdmacro(Big) {
  return _big(args, 2);
}

inline cmdmacro(bigg) {
  return _big(args, 3);
}

inline cmdmacro(Bigg) {
  return _big(args, 4);
}

inline cmdmacro(bigl) {
  return _big(args, 1, AtomType::opening);
}

inline cmdmacro(Bigl) {
  return _big(args, 2, AtomType::opening);
}

inline cmdmacro(biggl) {
  return _big(args, 3, AtomType::opening);
}

inline cmdmacro(Biggl) {
  return _big(args, 4, AtomType::opening);
}

inline cmdmacro(bigr) {
  return _big(args, 1, AtomType::closing);
}

inline cmdmacro(Bigr) {
  return _big(args, 2, AtomType::closing);
}

inline cmdmacro(biggr) {
  return _big(args, 3, AtomType::closing);
}

inline cmdmacro(Biggr) {
  return _big(args, 4, AtomType::closing);
}

}  // namespace microtex

#endif  // MICROTEX_MACRO_SIZES_H
