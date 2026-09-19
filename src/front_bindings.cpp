// Debug views of the new LaTeX front end (src/MicroTeX/lib/front/), for
// tests and for looking at what the lexer, expander and parser make of an
// input. Internal: not exported from the package namespace.

#include <Rcpp.h>

#include <algorithm>
#include <string>
#include <tuple>
#include <vector>

#include "front/ast.h"
#include "front/diagnostics.h"
#include "front/expander.h"
#include "front/front.h"
#include "front/lexer.h"
#include "front/spec.h"
#include "macro/macro.h"

using namespace microtex::front;

namespace {

const char* kind_name(TokKind k) {
  switch (k) {
    case TokKind::controlWord: return "control_word";
    case TokKind::controlSymbol: return "control_symbol";
    case TokKind::character: return "char";
    case TokKind::space: return "space";
    case TokKind::par: return "par";
    case TokKind::end: return "end";
  }
  return "?";
}

Rcpp::DataFrame diagnostics_frame(const Diagnostics& d) {
  const auto& items = d.items();
  const R_xlen_t n = static_cast<R_xlen_t>(items.size());
  Rcpp::CharacterVector severity(n), message(n);
  Rcpp::IntegerVector line(n), col(n), offset(n), length(n);
  for (R_xlen_t i = 0; i < n; i++) {
    const auto& x = items[static_cast<std::size_t>(i)];
    severity[i] = x.severity == Severity::error ? "error" : "warning";
    message[i] = Rcpp::String(x.message, CE_UTF8);
    line[i] = static_cast<int>(x.span.line);
    col[i] = static_cast<int>(x.span.col);
    offset[i] = static_cast<int>(x.span.offset);
    length[i] = static_cast<int>(x.span.length);
  }
  return Rcpp::DataFrame::create(
    Rcpp::Named("severity") = severity, Rcpp::Named("line") = line,
    Rcpp::Named("col") = col, Rcpp::Named("offset") = offset,
    Rcpp::Named("length") = length, Rcpp::Named("message") = message,
    Rcpp::Named("stringsAsFactors") = false);
}

}  // namespace

// The token stream for `tex`, one row per token, ending with the `end`
// token. `cat` is the TeX category code of a character token (NA
// otherwise); `line_ends` counts the line ends just before a token. The
// problems found are attached as the "diagnostics" attribute.
// [[Rcpp::export]]
Rcpp::DataFrame lex_latex_cpp(std::string tex, bool blank_line_is_par = false) {
  Diagnostics diags;
  LexOptions opts;
  opts.blankLineIsPar = blank_line_is_par;
  CatcodeTable catcodes;
  Lexer lexer(tex, opts, diags, catcodes);

  std::vector<Token> toks;
  while (true) {
    toks.push_back(lexer.next());
    if (toks.back().kind == TokKind::end) break;
  }

  const R_xlen_t n = static_cast<R_xlen_t>(toks.size());
  Rcpp::CharacterVector kind(n), text(n);
  Rcpp::IntegerVector cat(n), cp(n), offset(n), length(n), line(n), col(n), line_ends(n);
  for (R_xlen_t i = 0; i < n; i++) {
    const Token& t = toks[static_cast<std::size_t>(i)];
    kind[i] = kind_name(t.kind);
    text[i] = Rcpp::String(t.text, CE_UTF8);
    const bool isChar = t.kind == TokKind::character;
    cat[i] = isChar ? static_cast<int>(t.cat) : NA_INTEGER;
    cp[i] = isChar ? static_cast<int>(t.cp) : NA_INTEGER;
    offset[i] = static_cast<int>(t.span.offset);
    length[i] = static_cast<int>(t.span.length);
    line[i] = static_cast<int>(t.span.line);
    col[i] = static_cast<int>(t.span.col);
    line_ends[i] = t.lineEnds;
  }
  Rcpp::DataFrame out = Rcpp::DataFrame::create(
    Rcpp::Named("kind") = kind, Rcpp::Named("text") = text,
    Rcpp::Named("cat") = cat, Rcpp::Named("cp") = cp,
    Rcpp::Named("line") = line, Rcpp::Named("col") = col,
    Rcpp::Named("offset") = offset, Rcpp::Named("length") = length,
    Rcpp::Named("line_ends") = line_ends,
    Rcpp::Named("stringsAsFactors") = false);
  out.attr("diagnostics") = diagnostics_frame(diags);
  return out;
}

// `tex` with its user macros expanded, as the old parser receives it, with
// the problems found attached as the "diagnostics" attribute. Errors the old
// parser also raised (redefinition, runaway recursion) are R errors.
// [[Rcpp::export]]
Rcpp::CharacterVector expand_latex_cpp(std::string tex) {
  ExpanderOptions opts;
  opts.isBuiltinCommand = [](const std::string& name) {
    return microtex::MacroInfo::get(name) != nullptr || microtex::NewCommandMacro::isMacro(name);
  };
  opts.isBuiltinEnvironment = [](const std::string& name) {
    return microtex::NewCommandMacro::isMacro(name + "@env");
  };
  Diagnostics diags;
  std::string out;
  try {
    out = Expander(std::move(tex), std::move(opts), diags).expandToText();
  } catch (const std::exception& e) {
    Rcpp::stop(std::string("LaTeX parse error: ") + e.what());
  }
  Rcpp::CharacterVector res = Rcpp::CharacterVector::create(Rcpp::String(out, CE_UTF8));
  res.attr("diagnostics") = diagnostics_frame(diags);
  return res;
}

// Switch the front end: "legacy" (MicroTeX's parser alone), "expander" (the
// new expander before the old parser) or "modern" (the new front end).
// Returns the previous one, so a caller can put it back.
// [[Rcpp::export]]
std::string set_frontend_cpp(std::string which) {
  const FrontEnd now = frontEnd();
  const std::string previous = now == FrontEnd::legacy     ? "legacy"
                               : now == FrontEnd::expander ? "expander"
                                                           : "modern";
  if (which == "legacy") {
    setFrontEnd(FrontEnd::legacy);
  } else if (which == "expander") {
    setFrontEnd(FrontEnd::expander);
  } else if (which == "modern") {
    setFrontEnd(FrontEnd::modern);
  } else {
    Rcpp::stop("Unknown front end: " + which);
  }
  return previous;
}

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

// The syntax tree of `tex` read by the new front end, one row per node in
// document order (depth first), with the problems found attached as the
// "diagnostics" attribute. `mode` is "math" or "text".
// [[Rcpp::export]]
Rcpp::DataFrame parse_ast_cpp(std::string tex, std::string mode = "math") {
  Diagnostics diags;
  Ast ast;
  try {
    ast = parseLatex(tex, mode == "text" ? Mode::text : Mode::math, diags);
  } catch (const std::exception& e) {
    Rcpp::stop(std::string("LaTeX parse error: ") + e.what());
  }

  std::vector<NodeId> order, parents;
  std::vector<int> depths;
  // Iterative, so a deep tree cannot exhaust the stack here either.
  std::vector<std::tuple<NodeId, NodeId, int>> stack;
  if (ast.root != kNoNode) stack.emplace_back(ast.root, kNoNode, 0);
  while (!stack.empty()) {
    const auto [id, parent, depth] = stack.back();
    stack.pop_back();
    order.push_back(id);
    parents.push_back(parent);
    depths.push_back(depth);
    const std::uint32_t n = ast.childCount(id);
    for (std::uint32_t i = n; i > 0; i--) stack.emplace_back(ast.child(id, i - 1), id, depth + 1);
  }

  const R_xlen_t n = static_cast<R_xlen_t>(order.size());
  Rcpp::IntegerVector idv(n), parentv(n), depthv(n), aux(n), line(n), col(n);
  Rcpp::CharacterVector kind(n), modev(n), text(n), raw(n);
  Rcpp::LogicalVector flag(n), star(n);
  for (R_xlen_t i = 0; i < n; i++) {
    const Node& x = ast.node(order[static_cast<std::size_t>(i)]);
    idv[i] = static_cast<int>(order[static_cast<std::size_t>(i)]);
    parentv[i] = parents[static_cast<std::size_t>(i)] == kNoNode
                   ? NA_INTEGER
                   : static_cast<int>(parents[static_cast<std::size_t>(i)]);
    depthv[i] = depths[static_cast<std::size_t>(i)];
    kind[i] = nodeKindName(x.kind);
    modev[i] = x.mode == Mode::math ? "math" : "text";
    text[i] = Rcpp::String(x.text, CE_UTF8);
    raw[i] = Rcpp::String(x.raw, CE_UTF8);
    flag[i] = x.flag;
    star[i] = x.star;
    aux[i] = x.aux;
    line[i] = static_cast<int>(x.span.line);
    col[i] = static_cast<int>(x.span.col);
  }
  Rcpp::DataFrame out = Rcpp::DataFrame::create(
    Rcpp::Named("id") = idv, Rcpp::Named("parent") = parentv, Rcpp::Named("depth") = depthv,
    Rcpp::Named("kind") = kind, Rcpp::Named("mode") = modev, Rcpp::Named("text") = text,
    Rcpp::Named("raw") = raw, Rcpp::Named("flag") = flag, Rcpp::Named("star") = star,
    Rcpp::Named("aux") = aux, Rcpp::Named("line") = line, Rcpp::Named("col") = col,
    Rcpp::Named("stringsAsFactors") = false);
  out.attr("diagnostics") = diagnostics_frame(diags);
  return out;
}

// The new front end's command and environment names, and the engine's own
// registry, for checking one against the other.
// [[Rcpp::export]]
Rcpp::List command_tables_cpp() {
  return Rcpp::List::create(
    Rcpp::Named("spec_commands") = commandNames(),
    Rcpp::Named("spec_environments") = environmentNames(),
    Rcpp::Named("engine_commands") = microtex::MacroInfo::names(),
    Rcpp::Named("prelude_commands") = preludeCommandNames(),
    Rcpp::Named("prelude_environments") = preludeEnvironmentNames());
}

// Every environment the new front end knows: those it builds and those its
// prelude defines. R's scan for math spans in mixed input reads this.
// [[Rcpp::export]]
std::vector<std::string> math_env_names_cpp() {
  std::vector<std::string> names = environmentNames();
  for (const std::string& n : preludeEnvironmentNames()) names.push_back(n);
  std::sort(names.begin(), names.end());
  return names;
}
