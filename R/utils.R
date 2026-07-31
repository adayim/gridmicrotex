# Authoritative list: every environment MicroTeX registers in
# src/MicroTeX/lib/macro/macro_def.cpp env(...) calls, plus the
# starred variants (amsmath's no-equation-numbering forms) that users
# commonly type and that MicroTeX accepts without separate registration.
# Hand-synced to the C++ registrations -- a gap here corrupts mixed-mode
# input, so update both together.
.MATH_ENVS <- c(
  # array family
  "array", "tabular", "tabular*",
  # matrix family
  "matrix", "smallmatrix",
  "pmatrix", "bmatrix", "Bmatrix", "vmatrix", "Vmatrix",
  # equation / display
  "equation", "equation*", "math", "displaymath",
  # amsmath alignments
  "align", "align*", "flalign", "flalign*",
  "alignat", "alignat*", "aligned",
  "alignedat", "alignedat*",
  "eqnarray", "eqnarray*",
  "multline", "multline*",
  # paragraph-like
  "gather", "gather*", "gathered",
  "split", "cases", "rcases",
  # list environments
  "itemize", "enumerate"
)

# Find \end{env} matching \begin{env}, honoring nesting of the same env.
# `from` is the index just past the opening \begin{env}; the return value
# is the index of the backslash that starts \end{env}, or NA_integer_.
.find_env_close <- function(chars, n, from, env) {
  open_tag  <- paste0("\\begin{", env, "}")
  close_tag <- paste0("\\end{",   env, "}")
  ol <- nchar(open_tag); cl <- nchar(close_tag)
  depth <- 1L; k <- from
  while (k <= n) {
    if (chars[k] == "\\") {
      if (k + cl - 1L <= n &&
          paste(chars[k:(k + cl - 1L)], collapse = "") == close_tag) {
        depth <- depth - 1L
        if (depth == 0L) return(k)
        k <- k + cl; next
      }
      if (k + ol - 1L <= n &&
          paste(chars[k:(k + ol - 1L)], collapse = "") == open_tag) {
        depth <- depth + 1L
        k <- k + ol; next
      }
      k <- k + 2L; next   # skip any other \x escape
    }
    k <- k + 1L
  }
  NA_integer_
}

# Single source of truth for "where is the math in this string".
#
# Walks `tex` once and returns its maximal math regions: `$...$`,
# `$$...$$`, `\(...\)`, `\[...\]`, and `\begin{env}...\end{env}` for
# every environment in .MATH_ENVS. Any character not covered by a
# returned span is prose.
#
# Both latex_wrap() (which wraps the prose in \text{}) and
# .md_mask_math() (which hides the math from the markdown parser) are
# built on this, so the two can never disagree about what counts as math.
#
# Returns a list of records:
#   outer_start, outer_end  full span, delimiters included
#   inner_start, inner_end  content between the delimiters. For "env"
#                           spans this equals the outer range, because
#                           the environment is passed through verbatim.
#                           inner_end < inner_start means empty content.
#   display                 TRUE for $$...$$ and \[...\]
#   kind                    "dollar1"|"dollar2"|"paren"|"bracket"|"env"
#   closed                  FALSE when the delimiter ran off the end
.scan_math_spans <- function(tex) {
  chars <- strsplit(tex, "", fixed = TRUE)[[1]]
  n <- length(chars)
  spans <- list()
  i <- 1L

  add_span <- function(outer_start, outer_end, inner_start, inner_end,
                       display, kind, closed) {
    spans[[length(spans) + 1L]] <<- list(
      outer_start = outer_start, outer_end = outer_end,
      inner_start = inner_start, inner_end = inner_end,
      display = display, kind = kind, closed = closed
    )
  }

  # Consume a delimited math run starting at `from` (first content
  # character). `close_fn` reports the length of the closing delimiter at
  # a position, or 0L when it does not close there.
  scan_to_close <- function(from, close_fn) {
    k <- from
    while (k <= n) {
      # An escaped delimiter never closes the run.
      if (chars[k] == "\\" && k < n && chars[k + 1L] %in% c("$", "\\")) {
        k <- k + 2L; next
      }
      cl <- close_fn(k)
      if (cl > 0L) return(c(k, cl))
      k <- k + 1L
    }
    c(NA_integer_, 0L)
  }

  while (i <= n) {
    ch <- chars[i]
    c2 <- if (i < n) chars[i + 1L] else ""

    # Escaped literal in prose -- never opens math.
    if (ch == "\\" && c2 %in% c("$", "\\", "{", "}", "%", "#", "&", "_")) {
      i <- i + 2L; next
    }

    # \[ ... \]  (display)
    if (ch == "\\" && c2 == "[") {
      r <- scan_to_close(i + 2L, function(k) {
        if (chars[k] == "\\" && k < n && chars[k + 1L] == "]") 2L else 0L
      })
      if (is.na(r[1])) {
        add_span(i, n, i + 2L, n, TRUE, "bracket", FALSE); break
      }
      add_span(i, r[1] + r[2] - 1L, i + 2L, r[1] - 1L, TRUE, "bracket", TRUE)
      i <- r[1] + r[2]; next
    }

    # \( ... \)  (inline)
    if (ch == "\\" && c2 == "(") {
      r <- scan_to_close(i + 2L, function(k) {
        if (chars[k] == "\\" && k < n && chars[k + 1L] == ")") 2L else 0L
      })
      if (is.na(r[1])) {
        add_span(i, n, i + 2L, n, FALSE, "paren", FALSE); break
      }
      add_span(i, r[1] + r[2] - 1L, i + 2L, r[1] - 1L, FALSE, "paren", TRUE)
      i <- r[1] + r[2]; next
    }

    # \begin{env} ... \end{env} for known math environments
    if (ch == "\\" && i + 5L <= n &&
        paste(chars[i:(i + 5L)], collapse = "") == "\\begin") {
      rest <- paste(chars[i:min(i + 64L, n)], collapse = "")
      m <- regmatches(rest, regexec("^\\\\begin\\{([^}]+)\\}", rest))[[1]]
      if (length(m) == 2 && m[2] %in% .MATH_ENVS) {
        env <- m[2]
        start_inner <- i + nchar(m[1])
        j <- .find_env_close(chars, n, start_inner, env)
        if (!is.na(j)) {
          close_len <- nchar(paste0("\\end{", env, "}"))
          e <- j + close_len - 1L
          # Environments pass through verbatim: inner == outer.
          add_span(i, e, i, e, FALSE, "env", TRUE)
          i <- e + 1L; next
        }
      }
    }

    # $$ ... $$  (display)
    if (ch == "$" && c2 == "$") {
      r <- scan_to_close(i + 2L, function(k) {
        if (chars[k] == "$" && k < n && chars[k + 1L] == "$") 2L else 0L
      })
      if (is.na(r[1])) {
        add_span(i, n, i + 2L, n, TRUE, "dollar2", FALSE); break
      }
      add_span(i, r[1] + r[2] - 1L, i + 2L, r[1] - 1L, TRUE, "dollar2", TRUE)
      i <- r[1] + r[2]; next
    }

    # $ ... $  (inline)
    if (ch == "$") {
      r <- scan_to_close(i + 1L, function(k) if (chars[k] == "$") 1L else 0L)
      if (is.na(r[1])) {
        add_span(i, n, i + 1L, n, FALSE, "dollar1", FALSE); break
      }
      add_span(i, r[1], i + 1L, r[1] - 1L, FALSE, "dollar1", TRUE)
      i <- r[1] + 1L; next
    }

    i <- i + 1L
  }

  spans
}

#' Wrap standard text for math-first LaTeX renderers
#'
#' @description
#' Parses character strings to safely isolate standard natural language from
#' LaTeX math environments. Standard text is wrapped in `\text{}` blocks, while
#' equations, display math, and specific LaTeX environments are preserved verbatim.
#' This is heavily optimized for passing mixed-content strings (like plot titles
#' or axis labels) to pure-math typesetting engines like MicroTex. The conversion
#' is not perfect, but it should handle most common cases without user intervention.
#'
#' @param tex `character`. The string or vector of strings to be processed.
#' @param input_mode `character`. A length-one character vector dictating the
#'   parsing strategy. If `"mixed"` (default), the string is tokenized and text
#'   is wrapped. If `"math"`, the parser is bypassed and the string is returned
#'   unmodified, assuming the user has provided a pure math equation.
#'
#' @details
#' `latex_wrap()` operates as a state-machine tokenizer to ensure that valid LaTeX
#' math is not corrupted by the text-wrapping process. It features:
#' * **Delimiter Preservation**: Standard inline (`$`, `\(`) and block (`$$`, `\[`)
#'   math delimiters are recognized and preserved.
#' * **Environment Tracking**: Complex nested environments (e.g., `\begin{matrix}`)
#'   are safely extracted and bypassed.
#' * **Newline Conversion**: R newline characters (`\n`) occurring outside of math
#'   environments are automatically converted to LaTeX line breaks (`\\`) inside
#'   the `\text{}` wrapper.
#' * **Literal Escapes**: Escaped LaTeX literals (e.g., `\$`, `\%`, `\#`) are
#'   safely passed into the `\text{}` block without triggering math modes. The
#'   escape character for `\$` is automatically resolved for MicroTex compatibility.
#'
#' @return A `character` vector of the same length as `tex`, formatted for
#'   math-mode LaTeX rendering.
#'
#' @export
#'
#' @examples
#' # "mixed" mode (default) safely wraps text and preserves inline math
#' latex_wrap(r"(The equation \(E=mc^2\) is famous)")
#'
#' # "mixed" mode handles user-escaped characters seamlessly
#' latex_wrap(r"(Cost: \$100 for $x$ items)")
#'
#' # "mixed" mode converts R newlines to stacked text blocks
#' latex_wrap(r"(Line 1\nLine 2)")
#'
#' # "math" mode returns the string completely unmodified
#' latex_wrap(r"(\frac{\alpha}{\beta})", input_mode = "math")

latex_wrap <- function(tex, input_mode = c("mixed", "math")) {
  input_mode <- match.arg(input_mode)
  if (anyNA(tex)) {
    stop("`tex` must not contain NA values.", call. = FALSE)
  }
  if (input_mode == "math") return(tex)
  # The scanner below operates on a single string; recurse so the
  # documented "vector in, vector of the same length out" contract holds.
  if (length(tex) != 1L) {
    return(vapply(tex, latex_wrap, character(1),
                  input_mode = input_mode, USE.NAMES = FALSE))
  }
  if (!nzchar(tex)) return(tex)

  # Math regions come from the shared scanner; everything between them is
  # prose. See .scan_math_spans() for the delimiter rules.
  spans <- .scan_math_spans(tex)
  nch <- nchar(tex)
  out <- character(0)
  pos <- 1L
  unclosed <- FALSE

  # A break -- a newline, or a literal `\\` -- that tops or tails a run of
  # prose is separating that prose from the math beside it, so it belongs
  # at the formula level. Left inside the \text{} it only adds an empty
  # line to the text box, and the box and the math still sit side by side
  # in the enclosing row: that is why "Title\n$x^2$" came out on one line,
  # and why a pasted \caption landed to the left of its table rather than
  # above it. Interior newlines keep their old meaning.
  brk <- "(?:[ \t\r]*(?:\n|\\\\\\\\)[ \t\r]*)+"

  emit_text <- function(s) {
    if (!nzchar(s)) return()
    lead <- regmatches(s, regexpr(paste0("^", brk), s, perl = TRUE))
    lead <- if (length(lead)) lead else ""
    rest <- substring(s, nchar(lead) + 1L)
    trail <- regmatches(rest, regexpr(paste0(brk, "$"), rest, perl = TRUE))
    trail <- if (length(trail)) trail else ""
    core <- substr(rest, 1L, nchar(rest) - nchar(trail))

    # A run of them collapses to one, the way consecutive blank lines make
    # a single paragraph break in LaTeX. Stripping document wrappers
    # leaves blank lines behind, and each would otherwise be an empty row.
    if (nzchar(lead)) out <<- c(out, "\\\\")
    if (nzchar(core)) {
      out <<- c(out, paste0("\\text{",
                            gsub("\n", " \\\\\\\\", core, perl = TRUE), "}"))
    }
    if (nzchar(trail)) out <<- c(out, "\\\\")
  }

  for (sp in spans) {
    if (sp$outer_start > pos) {
      emit_text(substr(tex, pos, sp$outer_start - 1L))
    }
    if (identical(sp$kind, "env")) {
      # Environments pass through verbatim, delimiters included.
      out <- c(out, substr(tex, sp$outer_start, sp$outer_end))
    } else if (sp$inner_end >= sp$inner_start) {
      inner <- substr(tex, sp$inner_start, sp$inner_end)
      if (nzchar(inner)) {
        out <- c(out,
                 if (sp$display) paste0("\\displaystyle ", inner) else inner)
      }
    }
    if (!sp$closed) unclosed <- TRUE
    pos <- sp$outer_end + 1L
  }
  if (pos <= nch) emit_text(substr(tex, pos, nch))

  if (unclosed) {
    warning("Unclosed math delimiter detected and auto-closed at end of string.")
  }

  # A break at either end has nothing to separate -- it would only add a
  # blank first or last row.
  while (length(out) && identical(out[1L], "\\\\")) out <- out[-1L]
  while (length(out) && identical(out[length(out)], "\\\\")) {
    out <- out[-length(out)]
  }

  paste(out, collapse = "")   # <-- no space
}

# Find the position of the matching `}` for an opening `{` immediately
# before `start`. Returns the 1-based index of the close brace or
# NA_integer_ when unbalanced. Backslash-escaped braces (`\{`, `\}`) are
# treated as literal and skipped.
.find_close_brace <- function(s, start) {
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  n <- length(chars)
  depth <- 1L
  i <- start
  while (i <= n) {
    ch <- chars[i]
    if (identical(ch, "\\") && i < n) { i <- i + 2L; next }
    if (identical(ch, "{")) {
      depth <- depth + 1L
    } else if (identical(ch, "}")) {
      depth <- depth - 1L
      if (depth == 0L) return(i)
    }
    i <- i + 1L
  }
  NA_integer_
}

# Replace every occurrence of `\command[opt]?(paren)?{...}` with
# `replacement`. Brace nesting is balanced via .find_close_brace.
# When `keep_inner = TRUE`, the matched braced content is re-attached
# after the replacement (so `\emph{X}` → `\textit{X}` via
# replacement = "\\textit", keep_inner = TRUE). When FALSE (default),
# the whole construct is dropped or substituted with a literal.
.replace_command_braced <- function(tex, command, replacement = "",
                                    keep_inner = FALSE) {
  pat <- paste0("\\\\", command, "(?:\\[[^]]*\\])?(?:\\([^)]*\\))?\\{")
  repeat {
    m <- regexpr(pat, tex, perl = TRUE)
    if (m == -1L) return(tex)
    brace_open <- m + attr(m, "match.length") - 1L   # index of `{`
    close <- .find_close_brace(tex, brace_open + 1L)
    if (is.na(close)) return(tex)
    if (keep_inner) {
      inner <- substr(tex, brace_open + 1L, close - 1L)
      rep <- paste0(replacement, "{", inner, "}")
    } else {
      rep <- replacement
    }
    tex <- paste0(substr(tex, 1L, m - 1L),
                  rep,
                  substr(tex, close + 1L, nchar(tex)))
  }
}

# Rewrite `\url{X}` and `\href{U}{X}` to styled text. There is no link in a
# grob, so only the appearance is reproduced -- and it follows LaTeX's
# convention rather than HTML's: hyperref with `colorlinks=true` (which is
# what almost every document sets) colours the text and does not underline
# it, and the `url` package sets a URL in monospace. The markdown side uses
# the HTML convention instead; see the `a` rule in .md_default_rules().
#
# A URL is verbatim in real LaTeX, so its LaTeX-special characters are
# escaped here -- an unescaped `_` in `example.com/a_b` would otherwise open
# a subscript.
.MD_LINK_COLOR <- "#0969DA"

.replace_links <- function(tex) {
  esc <- function(s) gsub("([{}$&#_%~^])", "\\\\\\1", s)

  # \href{destination}{text}: drop the destination, style the text. The
  # text is *not* escaped -- unlike a URL it is ordinary LaTeX and may
  # legitimately contain markup.
  repeat {
    m <- regexpr("\\\\href\\{", tex, perl = TRUE)
    if (m == -1L) break
    o1 <- m + attr(m, "match.length") - 1L
    c1 <- .find_close_brace(tex, o1 + 1L)
    if (is.na(c1)) break
    # The second group must follow immediately for this to be an \href.
    rest <- substr(tex, c1 + 1L, nchar(tex))
    if (!startsWith(rest, "{")) break
    c2 <- .find_close_brace(tex, c1 + 2L)
    if (is.na(c2)) break
    txt <- substr(tex, c1 + 2L, c2 - 1L)
    tex <- paste0(substr(tex, 1L, m - 1L),
                  "\\textcolor{", .MD_LINK_COLOR, "}{", txt, "}",
                  substr(tex, c2 + 1L, nchar(tex)))
  }

  # \url{X}: monospace and coloured, with X escaped.
  repeat {
    m <- regexpr("\\\\url\\{", tex, perl = TRUE)
    if (m == -1L) break
    o1 <- m + attr(m, "match.length") - 1L
    c1 <- .find_close_brace(tex, o1 + 1L)
    if (is.na(c1)) break
    txt <- esc(substr(tex, o1 + 1L, c1 - 1L))
    tex <- paste0(substr(tex, 1L, m - 1L),
                  "\\textcolor{", .MD_LINK_COLOR, "}{\\texttt{", txt, "}}",
                  substr(tex, c1 + 1L, nchar(tex)))
  }
  tex
}

# Rewrite `\caption[opt]?{X}` → `\text{X}\\` inline at source position.
# Position-preserving: where the caption is written in the source is where
# it renders, separated from neighbouring content by a line break.
.replace_caption <- function(tex) {
  pat <- "\\\\caption(?:\\[[^]]*\\])?\\{"
  repeat {
    m <- regexpr(pat, tex, perl = TRUE)
    if (m == -1L) return(tex)
    brace_open <- m + attr(m, "match.length") - 1L
    close <- .find_close_brace(tex, brace_open + 1L)
    if (is.na(close)) return(tex)
    inner <- substr(tex, brace_open + 1L, close - 1L)
    tex <- paste0(substr(tex, 1L, m - 1L),
                  "\\text{", inner, "}\\\\",
                  substr(tex, close + 1L, nchar(tex)))
  }
}

# Strip / rewrite LaTeX document-layer wrappers that MicroTeX doesn't
# model, so raw `print.xtable()`, `knitr::kable()`, and similar
# `tabular`-bearing strings render directly. See `?latex_grob` (section
# "LaTeX document-level wrappers") for the user-facing list.
#
# Strip-list contract: every command here MUST be absent from MicroTeX's
# command tables. Verified against src/MicroTeX/lib/macro/macro_def.cpp
# and lib/core/formula_def.cpp; in particular `\multirow` and the
# `\tiny`..`\Huge` size family ARE valid MicroTeX macros and must
# not be added.
.strip_document_wrappers <- function(tex) {
  if (!nzchar(tex)) return(tex)

  # 0. Links, before the comment stripper: a `%` inside a URL is a real
  # character, and .replace_links() escapes it to `\%`, which step 1 then
  # leaves alone. Stripping comments first would eat the rest of the URL.
  tex <- .replace_links(tex)

  # 1. `%`-to-EOL comments, preserving `\%`.
  tex <- gsub("(?<!\\\\)%[^\n]*\n?", "", tex, perl = TRUE)

  # 2. Preamble + document boundary.
  tex <- .replace_command_braced(tex, "documentclass")
  tex <- .replace_command_braced(tex, "usepackage")
  tex <- gsub("\\\\(?:begin|end)\\{document\\}", "", tex, perl = TRUE)

  # 3. Float environments: keep the contents.
  tex <- gsub("\\\\begin\\{(?:table|figure)\\*?\\}(\\[[^]]*\\])?",
              "", tex, perl = TRUE)
  tex <- gsub("\\\\end\\{(?:table|figure)\\*?\\}", "", tex, perl = TRUE)

  # 3a. Strip the trailing `*` on env names. In amsmath, `align*` means
  # "no equation numbering" — MicroTeX doesn't number equations, so the
  # star is semantically a no-op. Normalising here lets the engine
  # recognise the env and enter array mode (without it, `&` errors out).
  tex <- gsub("\\\\(begin|end)\\{([A-Za-z]+)\\*\\}",
              "\\\\\\1{\\2}", tex, perl = TRUE)

  # 4. Title-page metadata (no body output in LaTeX either).
  tex <- gsub("\\\\maketitle\\b", "", tex, perl = TRUE)
  tex <- .replace_command_braced(tex, "title")
  tex <- .replace_command_braced(tex, "author")

  # 5. Cross-reference metadata.
  tex <- .replace_command_braced(tex, "label")

  # 6. Layout / alignment scope declarations + content-free declarations
  # that have no analog in a fixed-size grob.
  tex <- gsub("\\\\centering\\b", "", tex, perl = TRUE)
  tex <- gsub("\\\\(?:raggedright|raggedleft|flushleft|flushright)\\b",
              "", tex, perl = TRUE)
  tex <- gsub("\\\\(?:noindent|relax)\\b", "", tex, perl = TRUE)

  # Spacing primitives: map to MicroTeX's \vspace / \quad. Values are
  # em-relative so they scale with the grob's fontsize. The ratios
  # follow LaTeX's small/med/big proportion (1:2:4) rescaled so that
  # \bigskip is one full line at the current size. \hfill and \vfill
  # are rubber lengths in LaTeX (which expand to fill the surrounding
  # glue); a fixed-size grob has nothing to fill, so we substitute a
  # static 1em horizontal / 1em vertical gap.
  tex <- gsub("\\\\smallskip\\b", "\\\\vspace{0.25em}", tex, perl = TRUE)
  tex <- gsub("\\\\medskip\\b",   "\\\\vspace{0.5em}",  tex, perl = TRUE)
  tex <- gsub("\\\\bigskip\\b",   "\\\\vspace{1em}",    tex, perl = TRUE)
  tex <- gsub("\\\\hfill\\b",     "\\\\quad",           tex, perl = TRUE)
  tex <- gsub("\\\\vfill\\b",     "\\\\vspace{1em}",    tex, perl = TRUE)

  # 7. Body-text aliases that map cleanly onto MicroTeX-supported commands.
  tex <- .replace_command_braced(tex, "emph",       "\\textit", keep_inner = TRUE)
  tex <- .replace_command_braced(tex, "textnormal", "\\text",   keep_inner = TRUE)
  tex <- gsub("\\\\(?:par|newline)\\b", "\\\\\\\\", tex, perl = TRUE)

  # 8. Booktabs rules. Top/bottom map to \thickhline (a thicker rule
  # added in MicroTeX C++ alongside this layer); middle rules stay
  # \hline. `\cmidrule[trim]?(parenarg)?{a-b}` keeps its column range
  # via \cline{a-b}, which is also new in C++.
  tex <- gsub("\\\\(?:toprule|bottomrule)\\b",
              "\\\\thickhline", tex, perl = TRUE)
  tex <- gsub("\\\\midrule\\b", "\\\\hline", tex, perl = TRUE)
  tex <- .replace_command_braced(tex, "cmidrule", "\\cline", keep_inner = TRUE)

  # 9. Caption: extract content as inline `\text{...}\\`.
  tex <- .replace_caption(tex)

  tex
}
