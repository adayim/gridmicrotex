#include "front/front.h"

#include "core/formula.h"
#include "front/diagnostics.h"
#include "front/expander.h"
#include "front/hooks.h"
#include "front/lower.h"
#include "front/parser.h"
#include "front/spec.h"
#include "macro/macro.h"
#include "unimath/uni_symbol.h"
#include "utils/exceptions.h"

namespace microtex::front {

Ast parseLatex(const std::string& latex, Mode mode, Diagnostics& diagnostics, bool lineBreaks,
               bool paragraphs) {
  ExpanderOptions eo;
  eo.prelude = true;
  eo.recover = true;
  eo.lex.blankLineIsPar = paragraphs;
  eo.isBuiltinCommand = [](const std::string& name) { return findCommand(name) != nullptr; };
  eo.isBuiltinEnvironment = [](const std::string& name) {
    return findEnvironment(name) != nullptr;
  };
  Expander expander(latex, std::move(eo), diagnostics);
  ParserOptions po;
  po.startMode = mode;
  po.lineEndsBreak = lineBreaks;
  po.parBreaks = paragraphs;
  po.isKnownName = [](const std::string& name) {
    return Symbol::get(name.c_str()) != nullptr || Formula::isPredefined(name);
  };
  Ast ast;
  Parser(expander, ast, diagnostics, std::move(po)).parse();
  return ast;
}

namespace {

Diagnostics& lastStore() {
  static Diagnostics last;
  return last;
}

}  // namespace

void buildModern(const std::string& latex, InputMode mode, Formula& formula) {
  lastStore() = Diagnostics();
  Diagnostics diags;
  const bool mixed = mode == InputMode::mixed;
  const bool document = mode == InputMode::document;
  // Both prose modes start in text and are laid out as rows; they differ in
  // what breaks a row -- a line end in mixed, a blank line in a document.
  const bool prose = mixed || document;
  const Ast ast =
    parseLatex(latex, prose ? Mode::text : Mode::math, diags, mixed, document);
  // A capacity ran out: what was read is not what the input means.
  if (const Diagnostic* e = diags.firstError()) throw ex_parse(e->message);
  lowerInto(ast, formula, diags, prose, document);
  lastStore() = std::move(diags);
}

const Diagnostics& lastDiagnostics() {
  return lastStore();
}

namespace {

ImageResolver& resolverStore() {
  static ImageResolver resolver;
  return resolver;
}

}  // namespace

void setImageResolver(ImageResolver resolver) {
  resolverStore() = std::move(resolver);
}

const ImageResolver& imageResolver() {
  return resolverStore();
}

}  // namespace microtex::front
