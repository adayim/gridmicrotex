#include "macro/macro_fonts.h"

#include "microtex.h"
#include "utils/exceptions.h"
#include "utils/string_utils.h"

namespace microtex {

macro(intertext) {
  if (!tp.isArrayMode()) throw ex_parse("Command \\intertext must used in array environment!");

  std::string str(args[1]);
  replaceAll(str, "^{\\prime}", "\'");
  replaceAll(str, "^{\\prime\\prime}", "\'\'");

  auto a = Formula(tp, str, false, false)._root;
  sptr<Atom> ra = sptrOf<FontStyleAtom>(FontStyle::rm, false, a);
  ra->_type = AtomType::interText;
  tp.addAtom(ra);
  tp.addRow();

  return nullptr;
}

cmdmacro(addfont) {
  if (MicroTeX::isPrivilegedEnvironment())
    MicroTeX::addFont(FontSrcFile(args.text(1), args.text(2)));
  else
    throw ex_unprivileged("\\addfont may only be called in privileged environments");
  return nullptr;
}

cmdmacro(mathversion) {
  auto mathStyle = MathStyle::TeX;
  const auto& options = parseOption(args.text(2));
  const auto it = options.find("math-style");
  if (it != options.end()) {
    const auto& value = it->second;
    if (value == "TeX") {
      mathStyle = MathStyle::TeX;
    } else if (value == "ISO") {
      mathStyle = MathStyle::ISO;
    } else if (value == "French") {
      mathStyle = MathStyle::French;
    } else if (value == "upright") {
      mathStyle = MathStyle::upright;
    }
  }
  MicroTeX::setDefaultMathFont(args.text(1));
  return sptrOf<MathFontAtom>(mathStyle, args.text(1));
}

}  // namespace microtex
