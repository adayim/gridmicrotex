// Debug views of the new LaTeX front end (src/MicroTeX/lib/front/), for
// tests and for looking at what the lexer, expander and parser make of an
// input. Internal: not exported from the package namespace.

#include <Rcpp.h>

#include <string>
#include <vector>

#include "front/diagnostics.h"
#include "front/lexer.h"

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
  Lexer lexer(std::move(tex), opts, diags);

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
