#include "macro/macro_frac.h"

#include "atom/atom_basic.h"
#include "atom/atom_fence.h"
#include "utils/exceptions.h"
#include "utils/string_utils.h"
#include "utils/utf.h"

using namespace std;

namespace microtex {

cmdmacro(binom) {
  const auto num = args.formula(1);
  const auto den = args.formula(2);
  if (num == nullptr || den == nullptr)
    throw ex_parse("Both binomial coefficients must be not empty!");
  auto f = sptrOf<FracAtom>(num, den, false);
  return sptrOf<FencedAtom>(f, "lparen", "rparen");
}

sptr<Atom> _choose(
  const std::string& left,
  const std::string& right,
  Parser& tp,
  std::vector<std::string>& args
) {
  auto num = tp.popFormulaAtom();
  auto den = Formula(tp, tp.getOverArgument(), false)._root;
  if (num == nullptr || den == nullptr)
    throw ex_parse("Both numerator and denominator of choose can't be empty!");
  auto f = sptrOf<FracAtom>(num, den, false);
  return sptrOf<FencedAtom>(f, left, right);
}

cmdmacro(frac) {
  const auto num = args.formula(1);
  const auto den = args.formula(2);
  if (num == nullptr || den == nullptr)
    throw ex_parse("Both numerator and denominator of a fraction can't be empty!");
  return sptrOf<FracAtom>(num, den, true);
}

macro(above) {
  auto num = tp.popFormulaAtom();
  const auto& thick = tp.getDimen();
  auto den = Formula(tp, tp.getOverArgument(), false)._root;
  if (num == nullptr || den == nullptr) {
    throw ex_parse("Both numerator and denominator of a fraction can't be empty!");
  }
  return sptrOf<FracAtom>(num, den, true, thick);
}

macro(atop) {
  auto num = tp.popFormulaAtom();
  auto den = Formula(tp, tp.getOverArgument(), false)._root;
  if (num == nullptr || den == nullptr)
    throw ex_parse("Both numerator and denominator of a fraction can't be empty!");
  return sptrOf<FracAtom>(num, den, false);
}

macro(over) {
  auto num = tp.popFormulaAtom();
  auto den = Formula(tp, tp.getOverArgument(), false)._root;
  if (num == nullptr || den == nullptr)
    throw ex_parse("Both numerator and denominator of a fraction can't be empty!");
  return sptrOf<FracAtom>(num, den, true);
}

sptr<Atom> _frac_with_delims(Parser& tp, Args& args, bool rule, bool hasLength) {
  auto num = tp.popFormulaAtom();
  const auto& l = hasLength ? tp.getDimen() : Dimen();
  auto den = Formula(tp, tp.getOverArgument(), false)._root;

  if (num == nullptr || den == nullptr)
    throw ex_parse("Both numerator and denominator of a fraction can't be empty!");

  auto f = (hasLength ? sptrOf<FracAtom>(num, den, rule, l) : sptrOf<FracAtom>(num, den, rule));
  return sptrOf<FencedAtom>(f, args[1], args[2]);
}

macro(overwithdelims) {
  return _frac_with_delims(tp, args, true, false);
}

macro(atopwithdelims) {
  return _frac_with_delims(tp, args, false, false);
}

macro(abovewithdelims) {
  return _frac_with_delims(tp, args, true, true);
}

cmdmacro(cfrac) {
  Alignment numAlign = Alignment::center;
  if (args.text(3) == "r") {
    numAlign = Alignment::right;
  } else if (args.text(3) == "l") {
    numAlign = Alignment::left;
  }
  const auto num = args.formula(1);
  const auto denom = args.formula(2);
  if (num == nullptr || denom == nullptr)
    throw ex_parse("Both numerator and denominator of a fraction can't be empty!");
  const auto n = sptrOf<StyleAtom>(TexStyle::display, num);
  const auto d = sptrOf<StyleAtom>(TexStyle::display, denom);
  const auto f = sptrOf<FracAtom>(n, d, numAlign, Alignment::center);
  return sptrOf<StyleAtom>(TexStyle::display, f);
}

cmdmacro(genfrac) {
  bool rule = true;
  Dimen thickness;
  if (args.text(3).empty()) {
    rule = false;
  } else {
    thickness = Units::getDimen(args.text(3));
  }

  int style = 0;
  if (!args.text(4).empty()) valueOf(args.text(4), style);

  const auto num = args.formula(5);
  const auto den = args.formula(6);
  if (num == nullptr || den == nullptr) {
    throw ex_parse("Both numerator and denominator of a fraction can't be empty!");
  }

  auto fa = sptrOf<FracAtom>(num, den, rule, thickness);
  auto ra = sptrOf<RowAtom>();
  const auto texStyle = static_cast<TexStyle>(style * 2);
  auto f = sptrOf<FencedAtom>(fa, args.text(1), args.text(2));
  ra->add(sptrOf<StyleAtom>(texStyle, f));

  return ra;
}

}  // namespace microtex
