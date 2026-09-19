#ifndef GRIDMICROTEX_FRONT_FRONT_H
#define GRIDMICROTEX_FRONT_FRONT_H

#include <cstdint>
#include <string>

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

}  // namespace microtex::front

#endif
