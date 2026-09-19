#ifndef GRIDMICROTEX_FRONT_INPUT_MODE_H
#define GRIDMICROTEX_FRONT_INPUT_MODE_H

#include <cstdint>

namespace microtex {

/**
 * How an input is read.
 *
 *   math   A formula: the input is math from the start.
 *   mixed  A plot label: prose, with math in `$...$`, `\(...\)`, `$$...$$`
 *          and `\[...\]`. TeX's rules throughout, but for one: a line end
 *          in the prose is a line break, as R's "\n" is in a label.
 */
enum class InputMode : std::uint8_t { math, mixed };

}  // namespace microtex

#endif
