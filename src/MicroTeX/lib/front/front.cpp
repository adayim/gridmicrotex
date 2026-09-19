#include "front/front.h"

#include "core/formula.h"
#include "front/diagnostics.h"
#include "front/expander.h"
#include "front/parser.h"
#include "front/spec.h"
#include "macro/macro.h"
#include "unimath/uni_symbol.h"

namespace microtex::front {

namespace {

FrontEnd& current() {
  static FrontEnd which = FrontEnd::expander;
  return which;
}

}  // namespace

FrontEnd frontEnd() {
  return current();
}

void setFrontEnd(FrontEnd which) {
  current() = which;
}

std::string prepareForLegacyParser(const std::string& latex) {
  if (current() == FrontEnd::legacy) return latex;
  ExpanderOptions opts;
  // What the old parser defines: its commands, and the ones it defines in
  // LaTeX (\dfrac, \operatorname, ...), which live under the same names.
  opts.isBuiltinCommand = [](const std::string& name) {
    return MacroInfo::get(name) != nullptr || NewCommandMacro::isMacro(name);
  };
  opts.isBuiltinEnvironment = [](const std::string& name) {
    return NewCommandMacro::isMacro(name + "@env");
  };
  Diagnostics diags;
  return Expander(latex, std::move(opts), diags).expandToText();
}

Ast parseLatex(const std::string& latex, Mode mode, Diagnostics& diagnostics) {
  ExpanderOptions eo;
  eo.prelude = true;
  eo.lex.blankLineIsPar = false;
  eo.isBuiltinCommand = [](const std::string& name) { return findCommand(name) != nullptr; };
  eo.isBuiltinEnvironment = [](const std::string& name) {
    return findEnvironment(name) != nullptr;
  };
  Expander expander(latex, std::move(eo), diagnostics);
  ParserOptions po;
  po.startMode = mode;
  po.isKnownName = [](const std::string& name) {
    return Symbol::get(name.c_str()) != nullptr || Formula::isPredefined(name);
  };
  Ast ast;
  Parser(expander, ast, diagnostics, std::move(po)).parse();
  return ast;
}

}  // namespace microtex::front
