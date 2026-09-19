#ifndef GRIDMICROTEX_FRONT_FRONT_H
#define GRIDMICROTEX_FRONT_FRONT_H

#include <cstdint>
#include <string>

#include "front/ast.h"
#include "front/diagnostics.h"
#include "front/input_mode.h"

namespace microtex {
class Formula;
}

namespace microtex::front {

/**
 * Which front end turns LaTeX source into atoms. Both old and new stay
 * available while the new one is built, so any layout can be compared
 * between them (tools/frontend-diff).
 *
 *   legacy    MicroTeX's own parser, macros included.
 *   expander  The new token-level macro expander, handing its result to
 *             the old parser as text.
 *   modern    The new front end throughout: expander, parser, lowering.
 */
enum class FrontEnd : std::uint8_t { legacy, expander, modern };

FrontEnd frontEnd();

void setFrontEnd(FrontEnd which);

/**
 * `latex` with user macros expanded, ready for the old parser. With the
 * legacy front end, `latex` unchanged.
 */
std::string prepareForLegacyParser(const std::string& latex);

/**
 * The syntax tree of `latex` read by the new front end: the prelude, user
 * macros and define_macro() macros expanded, then parsed from `mode`.
 * `lineBreaks`: a line end in text is a line break (mixed mode). Problems
 * go to `diagnostics`; errors the expander still raises (a redefinition,
 * runaway recursion) are thrown as ex_parse.
 */
Ast parseLatex(const std::string& latex, Mode mode, Diagnostics& diagnostics,
               bool lineBreaks = false);

/**
 * The atoms of `latex`, read by the new front end as `mode` says, into
 * `formula`. The problems found replace lastDiagnostics().
 */
void buildModern(const std::string& latex, InputMode mode, Formula& formula);

/** The diagnostics of the last buildModern(), for the host to report. */
const Diagnostics& lastDiagnostics();

}  // namespace microtex::front

#endif
