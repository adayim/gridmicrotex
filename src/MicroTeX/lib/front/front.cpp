#include "front/front.h"

#include "front/diagnostics.h"
#include "front/expander.h"
#include "macro/macro.h"

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

}  // namespace microtex::front
