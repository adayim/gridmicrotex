// The LaTeX front end (src/MicroTeX/lib/front/) as R needs it beyond a parse:
// the macros define_macro() makes, and the environments whose body is math.
// Its lexer, expander and parser are tested in the MicroTeX fork
// (github.com/adayim/MicroTeX, test/), which runs them against the engine
// directly.

#include <Rcpp.h>

#include <algorithm>
#include <string>
#include <vector>

#include "front/expander.h"
#include "front/spec.h"

using namespace microtex::front;

// The macros made with define_macro(), which outlive a parse.
// [[Rcpp::export]]
void persistent_macro_set_cpp(std::string name, std::string body) {
  setPersistentMacro(name, body);
}

// [[Rcpp::export]]
bool persistent_macro_remove_cpp(std::string name) {
  return removePersistentMacro(name);
}

// [[Rcpp::export]]
void persistent_macro_clear_cpp() {
  clearPersistentMacros();
}

// [[Rcpp::export]]
Rcpp::CharacterVector persistent_macro_list_cpp() {
  const auto& table = persistentMacros();
  Rcpp::CharacterVector bodies(table.size());
  Rcpp::CharacterVector names(table.size());
  for (std::size_t i = 0; i < table.size(); i++) {
    names[static_cast<R_xlen_t>(i)] = Rcpp::String(table[i].first, CE_UTF8);
    bodies[static_cast<R_xlen_t>(i)] = Rcpp::String(table[i].second, CE_UTF8);
  }
  bodies.attr("names") = names;
  return bodies;
}

// Changes whenever a persistent macro does; part of the layout cache key.
// [[Rcpp::export]]
double persistent_macro_generation_cpp() {
  return static_cast<double>(persistentMacroGeneration());
}

// Every environment the new front end knows whose body is math: those it
// builds and those its prelude defines, less the ones that only wrap content
// (`document`, `table`, `figure`), whose body is prose. R's scan for math
// spans in mixed input reads this, and masks what it finds from CommonMark.
// [[Rcpp::export]]
std::vector<std::string> math_env_names_cpp() {
  std::vector<std::string> names;
  // A minipage's body is paragraphs, not math.
  for (const std::string& n : environmentNames()) {
    const EnvSpec* spec = findEnvironment(n);
    if (spec == nullptr || spec->body != EnvBody::text) names.push_back(n);
  }
  const std::vector<std::string> wrappers = preludeTransparentEnvironmentNames();
  for (const std::string& n : preludeEnvironmentNames()) {
    if (std::find(wrappers.begin(), wrappers.end(), n) == wrappers.end()) names.push_back(n);
  }
  std::sort(names.begin(), names.end());
  return names;
}
