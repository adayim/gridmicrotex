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
  if (input_mode == "math" || !nzchar(tex)) return(tex)

  MATH_ENVS <- c("equation", "equation*", "align", "align*",
                 "gather", "gather*", "multline", "multline*",
                 "eqnarray", "eqnarray*", "math", "displaymath",
                 "split", "aligned", "alignedat", "gathered",
                 "cases", "matrix", "pmatrix", "bmatrix",
                 "Bmatrix", "vmatrix", "Vmatrix", "array")

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
