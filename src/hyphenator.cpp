#include "hyphenator.h"

#include <algorithm>

#include "utils/utf.h"

using namespace gridmicrotex;
using microtex::c32;

std::unordered_map<std::string, std::vector<std::uint8_t>> Hyphenator::_patterns;
std::unordered_map<std::string, std::vector<int>> Hyphenator::_exceptions;
std::size_t Hyphenator::_maxPatternLen = 0;
int Hyphenator::_leftMin = 2;
int Hyphenator::_rightMin = 3;

namespace {

// Length in bytes of the UTF-8 sequence starting with `c`.
std::size_t seqLen(unsigned char c) {
  if (c >= 0xF0) return 4;
  if (c >= 0xE0) return 3;
  if (c >= 0xC0) return 2;
  return 1;
}

c32 toLower(c32 c) {
  return (c >= 'A' && c <= 'Z') ? c + 32 : c;
}

bool isAsciiDigit(c32 c) {
  return c >= '0' && c <= '9';
}

std::string utf8Of(const std::vector<c32>& cs, std::size_t from, std::size_t to) {
  std::string s;
  for (std::size_t i = from; i < to; i++) microtex::appendToUtf8(s, cs[i]);
  return s;
}

// "hy3ph" -> letters "hyph", digits {0,0,3,0,0}. The digit vector is one
// longer than the letters: entry k scores the gap before letter k.
void splitPattern(
  const std::string& p,
  std::string& letters,
  std::vector<std::uint8_t>& digits,
  std::size_t& letterCount
) {
  letters.clear();
  digits.assign(1, 0);
  letterCount = 0;
  for (std::size_t i = 0; i < p.size();) {
    const unsigned char c = static_cast<unsigned char>(p[i]);
    if (c >= '0' && c <= '9') {
      digits.back() = static_cast<std::uint8_t>(c - '0');
      i++;
    } else {
      const std::size_t n = seqLen(c);
      letters.append(p, i, n);
      i += n;
      letterCount++;
      digits.push_back(0);
    }
  }
}

// "as-so-ciate" -> word "associate", offsets {2, 4}.
void splitException(const std::string& e, std::string& word, std::vector<int>& offs) {
  word.clear();
  offs.clear();
  int n = 0;
  for (std::size_t i = 0; i < e.size();) {
    const unsigned char c = static_cast<unsigned char>(e[i]);
    if (c == '-') {
      offs.push_back(n);
      i++;
      continue;
    }
    const std::size_t len = seqLen(c);
    word.append(e, i, len);
    i += len;
    n++;
  }
}

}  // namespace

void Hyphenator::load(
  const std::vector<std::string>& patterns,
  const std::vector<std::string>& exceptions,
  int leftMin,
  int rightMin
) {
  clear();
  _leftMin = std::max(1, leftMin);
  _rightMin = std::max(1, rightMin);

  std::string letters;
  std::vector<std::uint8_t> digits;
  std::size_t letterCount = 0;
  for (const auto& p : patterns) {
    splitPattern(p, letters, digits, letterCount);
    if (letters.empty()) continue;
    _maxPatternLen = std::max(_maxPatternLen, letterCount);
    _patterns[letters] = digits;
  }

  std::string word;
  std::vector<int> offs;
  for (const auto& e : exceptions) {
    splitException(e, word, offs);
    if (!word.empty()) _exceptions[word] = offs;
  }
}

void Hyphenator::clear() {
  _patterns.clear();
  _exceptions.clear();
  _maxPatternLen = 0;
}

bool Hyphenator::ready() {
  return !_patterns.empty();
}

std::vector<int> Hyphenator::points(const std::vector<c32>& word) {
  std::vector<int> out;
  const int n = static_cast<int>(word.size());
  if (!ready() || n < _leftMin + _rightMin) return out;

  std::vector<c32> lw;
  lw.reserve(word.size());
  for (const auto c : word) {
    // A word with a digit in it is not hyphenated, as in TeX: the
    // patterns describe letters, and scoring "covid19" from "covid"
    // would break it somewhere nobody asked for.
    if (isAsciiDigit(c)) return out;
    lw.push_back(toLower(c));
  }

  // An explicit exception wins outright.
  const auto ex = _exceptions.find(utf8Of(lw, 0, lw.size()));
  if (ex != _exceptions.end()) {
    for (const int p : ex->second) {
      if (p >= _leftMin && n - p >= _rightMin) out.push_back(p);
    }
    return out;
  }

  // Liang: wrap the word in '.', score every substring that matches a
  // pattern, keeping the highest digit seen at each gap, then break
  // where the score is odd.
  std::vector<c32> w;
  w.reserve(lw.size() + 2);
  w.push_back('.');
  w.insert(w.end(), lw.begin(), lw.end());
  w.push_back('.');

  const int m = static_cast<int>(w.size());
  std::vector<std::uint8_t> score(static_cast<std::size_t>(m) + 1, 0);
  for (int i = 0; i < m; i++) {
    std::string key;
    const int stop = std::min<int>(m, i + static_cast<int>(_maxPatternLen));
    for (int j = i; j < stop; j++) {
      microtex::appendToUtf8(key, w[j]);
      const auto it = _patterns.find(key);
      if (it == _patterns.end()) continue;
      const auto& d = it->second;
      for (std::size_t k = 0; k < d.size(); k++) {
        const std::size_t p = static_cast<std::size_t>(i) + k;
        if (p < score.size() && d[k] > score[p]) score[p] = d[k];
      }
    }
  }

  for (int i = 0; i < n; i++) {
    // score[i + 2] governs the gap after word[i]: one for the leading
    // '.', one because entry k scores the gap *before* letter k.
    if (score[static_cast<std::size_t>(i) + 2] % 2 == 1) {
      const int off = i + 1;
      if (off >= _leftMin && n - off >= _rightMin) out.push_back(off);
    }
  }
  return out;
}
