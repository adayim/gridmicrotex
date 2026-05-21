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
  # The state-machine below operates on a single string; recurse so the
  # documented "vector in, vector of the same length out" contract holds.
  if (length(tex) != 1L) {
    return(vapply(tex, latex_wrap, character(1),
                  input_mode = input_mode, USE.NAMES = FALSE))
  }
  if (!nzchar(tex)) return(tex)

  # Authoritative list: every environment MicroTeX registers in
  # src/MicroTeX/lib/macro/macro_def.cpp env(...) calls, plus the
  # starred variants (amsmath's no-equation-numbering forms) that users
  # commonly type and that MicroTeX accepts without separate registration.
  MATH_ENVS <- c(
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

  chars <- strsplit(tex, "", fixed = TRUE)[[1]]
  n <- length(chars)

  TEXT <- 1L; DOLLAR_INLINE <- 2L; DOLLAR_BLOCK <- 3L
  PAREN <- 4L; BRACKET <- 5L

  state <- TEXT
  buf <- character(0)
  out <- character(0)
  i <- 1L

  flush_text <- function() {
    if (length(buf)) {
      s <- paste(buf, collapse = "")
      s <- gsub("\n", " \\\\\\\\", s, perl = TRUE)   # was " \\\\\\\\ "
      out <<- c(out, paste0("\\text{", s, "}"))
      buf <<- character(0)
    }
  }
  flush_math <- function(display = FALSE) {
    if (length(buf)) {
      s <- paste(buf, collapse = "")
      out <<- c(out, if (display) paste0("\\displaystyle ", s) else s)
      buf <<- character(0)
    }
  }

  # Find \end{env} matching \begin{env}, honoring nesting of the same env.
  find_env_close <- function(from, env) {
    open_tag  <- paste0("\\begin{",  env, "}")
    close_tag <- paste0("\\end{",    env, "}")
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

  while (i <= n) {
    ch <- chars[i]
    c2 <- if (i < n) chars[i + 1L] else ""

    if (state == TEXT) {
      # Escaped literal in text
      if (ch == "\\" && c2 %in% c("$", "\\", "{", "}", "%", "#", "&", "_")) {
        buf <- c(buf, ch, c2); i <- i + 2L; next
      }
      # \[ ... \]
      if (ch == "\\" && c2 == "[") { flush_text(); state <- BRACKET;       i <- i + 2L; next }
      # \( ... \)
      if (ch == "\\" && c2 == "(") { flush_text(); state <- PAREN;         i <- i + 2L; next }

      # \begin{env}
      if (ch == "\\" && i + 5L <= n &&
          paste(chars[i:(i + 5L)], collapse = "") == "\\begin") {
        rest <- paste(chars[i:min(i + 64L, n)], collapse = "")
        m <- regmatches(rest, regexec("^\\\\begin\\{([^}]+)\\}", rest))[[1]]
        if (length(m) == 2 && m[2] %in% MATH_ENVS) {
          env <- m[2]
          start_inner <- i + nchar(m[1])
          j <- find_env_close(start_inner, env)
          if (!is.na(j)) {
            flush_text()
            close_len <- nchar(paste0("\\end{", env, "}"))
            out <- c(out, paste(chars[i:(j + close_len - 1L)], collapse = ""))
            i <- j + close_len; next
          }
        }
      }

      # $$ ... $$
      if (ch == "$" && c2 == "$") { flush_text(); state <- DOLLAR_BLOCK;  i <- i + 2L; next }
      # $ ... $
      if (ch == "$")              { flush_text(); state <- DOLLAR_INLINE; i <- i + 1L; next }

      buf <- c(buf, ch); i <- i + 1L; next
    }

    # --- math states ---
    if (state == DOLLAR_INLINE) {
      if (ch == "\\" && c2 %in% c("$", "\\")) { buf <- c(buf, ch, c2); i <- i + 2L; next }
      if (ch == "$") { flush_math(FALSE); state <- TEXT; i <- i + 1L; next }
      buf <- c(buf, ch); i <- i + 1L; next
    }
    if (state == DOLLAR_BLOCK) {
      if (ch == "\\" && c2 %in% c("$", "\\")) { buf <- c(buf, ch, c2); i <- i + 2L; next }
      if (ch == "$" && c2 == "$") { flush_math(TRUE); state <- TEXT; i <- i + 2L; next }
      buf <- c(buf, ch); i <- i + 1L; next
    }
    if (state == PAREN) {
      if (ch == "\\" && c2 == "\\") { buf <- c(buf, ch, c2); i <- i + 2L; next }
      if (ch == "\\" && c2 == ")")  { flush_math(FALSE); state <- TEXT; i <- i + 2L; next }
      buf <- c(buf, ch); i <- i + 1L; next
    }
    if (state == BRACKET) {
      if (ch == "\\" && c2 == "\\") { buf <- c(buf, ch, c2); i <- i + 2L; next }
      if (ch == "\\" && c2 == "]")  { flush_math(TRUE); state <- TEXT; i <- i + 2L; next }
      buf <- c(buf, ch); i <- i + 1L; next
    }
  }

  if (state == TEXT) {
    flush_text()
  } else {
    display <- state %in% c(DOLLAR_BLOCK, BRACKET)
    flush_math(display)
    warning("Unclosed math delimiter detected and auto-closed at end of string.")
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
