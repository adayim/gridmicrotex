#ifndef GRIDMICROTEX_FRONT_LOWER_H
#define GRIDMICROTEX_FRONT_LOWER_H

#include "core/formula.h"
#include "front/ast.h"
#include "front/diagnostics.h"

namespace microtex::front {

/**
 * Builds the engine's atoms from a syntax tree, into `formula`.
 *
 * The tree is replayed onto the engine's own Formula in the order the old
 * parser added things to it, so what depended on that order -- the line
 * break marks Formula::add() inserts, the "previous atom" that scripts and
 * \limits take, the braces an environment was wrapped in -- comes out the
 * same. Structure the parser owns (groups, scripts, \over, declarations,
 * \left...\right, line breaks) is built here; every other command is built
 * by its existing handler, given the source text of its arguments exactly
 * as it used to receive it.
 *
 * An error a handler raises is recorded as a diagnostic at the command, and
 * the command is drawn as its name in red, as an unknown one is.
 */
void lowerInto(const Ast& ast, Formula& formula, Diagnostics& diagnostics);

}  // namespace microtex::front

#endif
