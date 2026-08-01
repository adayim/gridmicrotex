#ifndef GRIDMICROTEX_HYPHENATOR_H
#define GRIDMICROTEX_HYPHENATOR_H

#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

#include "utils/types.h"

namespace gridmicrotex {

/**
 * Liang's hyphenation algorithm, the one TeX itself uses.
 *
 * Patterns are supplied from R, which reads them out of a bundled
 * `hyph-*.tex` file -- the same format TeX loads. Nothing here is
 * language-specific; a different language is a different pattern set.
 *
 * Everything is static because MicroTeX reaches this from
 * RowAtom::createBox, which has no place to carry an instance. The data
 * is loaded once and read-only thereafter.
 */
class Hyphenator {
public:
  /**
   * @param patterns   TeX patterns, e.g. "hy3ph": letters interleaved
   *                   with the digits that score each position.
   * @param exceptions explicitly hyphenated words, e.g. "as-so-ciate".
   * @param leftMin    characters that must precede the first break.
   * @param rightMin   characters that must follow the last break.
   */
  static void load(
    const std::vector<std::string>& patterns,
    const std::vector<std::string>& exceptions,
    int leftMin,
    int rightMin
  );

  static void clear();

  static bool ready();

  /**
   * Offsets, in characters from the start of `word`, at which it may be
   * broken -- an offset of 2 means "after the second character". Empty
   * when the word is too short, holds anything but letters, or simply
   * has no legal break.
   */
  static std::vector<int> points(const std::vector<microtex::c32>& word);

private:
  static std::unordered_map<std::string, std::vector<std::uint8_t>> _patterns;
  static std::unordered_map<std::string, std::vector<int>> _exceptions;
  static std::size_t _maxPatternLen;
  static int _leftMin;
  static int _rightMin;
};

}  // namespace gridmicrotex

#endif  // GRIDMICROTEX_HYPHENATOR_H
