#ifndef MICROTEX_MACRO_ARGS_H
#define MICROTEX_MACRO_ARGS_H

#include <string>
#include <vector>

#include "atom/atom.h"
#include "macro/macro.h"

namespace microtex {

class ArrayFormula;
class Parser;

/**
 * How a command handler reads its arguments, whichever front end called
 * it. Indices are the old parser's: 0 is the command's name, 1 to argc its
 * mandatory arguments, its optional ones after those.
 *
 * The old parser hands over the arguments' text and parses each again
 * when asked. The new front end has read them already, and hands over what
 * it built from them (front/lower.cpp), so an argument is read once, with
 * the rest of the input, and its problems are reported where they are.
 */
class CommandArgs {
public:
  virtual ~CommandArgs() = default;

  /** Argument i's source text; "" when an optional one is absent. */
  virtual const std::string& text(std::size_t i) const = 0;

  /** Argument i as a formula, read in math or text mode: what a handler
   *  built with `Formula(tp, args[i], false, math)._root`. nullptr when it
   *  is empty. `preprocess` is the old parser's, for the calls that asked
   *  for it; the new front end has nothing left to preprocess. */
  virtual sptr<Atom> formula(std::size_t i, bool math = true, bool preprocess = false) = 0;

  /** Argument i as the body of an alignment, its rows and cells in a
   *  formula of its own, which the caller finishes (checkDimensions()). */
  virtual sptr<ArrayFormula> alignment(std::size_t i) = 0;

  /** Whether the command itself was read in math mode. */
  virtual bool isMathMode() const = 0;

  /** The old parser's partial flag: an unknown command is drawn, not an
   *  error. Always so for the new front end. */
  virtual bool isPartial() const = 0;
};

typedef sptr<Atom> (*CommandDelegate)(CommandArgs& args);

/** A command whose handler reads its arguments through CommandArgs, so the
 *  same handler serves the old parser and the new front end. */
class CommandMacro : public MacroInfo {
private:
  CommandDelegate _delegate;

public:
  no_copy_assign(CommandMacro);

  CommandMacro(int argc, int posOpts, CommandDelegate delegate)
      : MacroInfo(argc, posOpts), _delegate(delegate) {}

  CommandMacro(int argc, CommandDelegate delegate) : MacroInfo(argc), _delegate(delegate) {}

  /** From the old parser. */
  sptr<Atom> invoke(Parser& tp, std::vector<std::string>& args) override;

  /** From the new front end. */
  sptr<Atom> call(CommandArgs& args) { return _delegate(args); }
};

}  // namespace microtex

/** A handler written against CommandArgs (compare `macro` in macro_decl.h);
 *  its arguments are `args`, as they were before. */
#define cmdmacro(name) sptr<Atom> macro_##name(CommandArgs& args)

#endif  // MICROTEX_MACRO_ARGS_H
