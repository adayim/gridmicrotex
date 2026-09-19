#ifndef GRIDMICROTEX_FRONT_EXPANDER_H
#define GRIDMICROTEX_FRONT_EXPANDER_H

#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "front/diagnostics.h"
#include "front/lexer.h"
#include "front/token.h"

namespace microtex::front {

/** One piece of a macro's replacement text: literal source text, or a
 *  parameter (1..9) to be replaced by that argument's text. */
struct BodyPiece {
  std::string text;
  int param = 0;
};

/**
 * A user macro or environment. The replacement text is split into pieces
 * when it is defined, so an expansion substitutes every `#n` in one pass
 * and never scans an argument's own text for parameters.
 */
struct MacroDef {
  int nparams = 0;
  /** \newcommand[n][default]: the first parameter is optional. */
  bool hasOptional = false;
  std::string optionalDefault;
  /** \def's parameter text: delimiters[0] must follow the name, and
   *  delimiters[i] ends parameter i. Empty means undelimited. */
  std::vector<std::vector<Token>> delimiters;
  std::vector<BodyPiece> body;
  /** For an environment, the code at \end. */
  std::vector<BodyPiece> endBody;
  /** \let to something that is not a user macro: the expansion is that one
   *  token, handed on as it is and never expanded again. That is what
   *  keeps `\let\oldfrac\frac \renewcommand{\frac}...{\oldfrac...}` from
   *  looping. */
  bool alias = false;
};

struct ExpanderOptions {
  /** Is `name` a command the parser itself defines, such as \frac?
   *  \newcommand refuses it and \renewcommand accepts it. */
  std::function<bool(const std::string&)> isBuiltinCommand;
  /** The same for environments. */
  std::function<bool(const std::string&)> isBuiltinEnvironment;
  LexOptions lex;
  /** TeX stops runaway recursion with its capacity limits; these are ours. */
  std::size_t maxExpansions = 10000;
  std::size_t maxExpandedBytes = std::size_t{16} << 20;
};

/**
 * Expands user macros at the token level.
 *
 * Handles \newcommand, \renewcommand, \providecommand and
 * \DeclareMathOperator (each also starred), \def and \gdef with delimited
 * parameters, \let, \newenvironment and \renewenvironment, the macros
 * defined with define_macro(), and a few starred forms of built-ins. Macro
 * arguments are read as TeX reads them: one token or one braced group,
 * never a byte.
 *
 * Built-in commands are passed through. Until the new parser exists
 * (plan Stage 3), the result goes back to the old parser as text: text
 * with no user macros in it comes back byte for byte, whitespace and
 * comments included, so the old parser sees exactly what it saw before.
 *
 * Errors the old parser raised for the same input (redefining an existing
 * command with \newcommand, runaway recursion) are still thrown as
 * ex_parse with the same messages. Other problems are recorded in the
 * diagnostics and expansion carries on.
 */
class Expander {
public:
  Expander(std::string input, ExpanderOptions options, Diagnostics& diagnostics);
  ~Expander();
  Expander(const Expander&) = delete;
  Expander& operator=(const Expander&) = delete;

  /** The whole input with every user macro expanded and every definition
   *  removed. */
  std::string expandToText();

private:
  struct Impl;
  std::unique_ptr<Impl> _impl;
};

/**
 * Macros that outlive a parse: those made with define_macro(). Zero
 * parameters; a parse sees them all, and a \renewcommand in a label shadows
 * one for that label only. The generation changes on every edit, so a
 * cache of layouts can tell when its entries went stale.
 */
void setPersistentMacro(const std::string& name, const std::string& body);
bool removePersistentMacro(const std::string& name);
void clearPersistentMacros();
const std::vector<std::pair<std::string, std::string>>& persistentMacros();
std::uint64_t persistentMacroGeneration();

}  // namespace microtex::front

#endif
