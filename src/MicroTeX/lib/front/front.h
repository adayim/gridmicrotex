#ifndef GRIDMICROTEX_FRONT_FRONT_H
#define GRIDMICROTEX_FRONT_FRONT_H

#include <cstdint>
#include <string>

#include "front/ast.h"
#include "front/diagnostics.h"

namespace microtex::front {

/**
 * Which front end turns LaTeX source into atoms. Both old and new stay
 * available while the new one is built, so any layout can be compared
 * between them (tools/frontend-diff).
 *
 *   legacy    MicroTeX's own parser, macros included.
 *   expander  The new token-level macro expander, handing its result to
 *             the old parser as text.
 */
enum class FrontEnd : std::uint8_t { legacy, expander };

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
 * Problems go to `diagnostics`; errors the expander still raises (a
 * redefinition, runaway recursion) are thrown as ex_parse.
 */
Ast parseLatex(const std::string& latex, Mode mode, Diagnostics& diagnostics);

}  // namespace microtex::front

#endif
