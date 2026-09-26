# Every environment the engine knows, and its starred form (amsmath's
# no-numbering variants; the star changes nothing here). Read from the C++
# front end's own tables -- the environments it builds and those its
# prelude defines in LaTeX -- so this can no longer fall out of step with
# them. Asked once, on first use: the DLL is not loaded when this file is.
.math_envs <- local({
  envs <- NULL
  function() {
    if (is.null(envs)) {
      names <- math_env_names_cpp()
      envs <<- c(names, paste0(names, "*"))
    }
    envs
  }
})

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
# every environment in .math_envs(). Any character not covered by a
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
      if (length(m) == 2 && m[2] %in% .math_envs()) {
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
#' Turns a label that mixes text and math into a formula: the text is
#' wrapped in `\text{}` blocks, while equations, display math and math
#' environments are kept verbatim. [latex_grob()] does not need this, as it
#' reads such a label itself (`input_mode = "mixed"`); it is for handing a
#' label to something that takes only math, such as
#' `latex_grob(input_mode = "math")` or another math renderer. The
#' conversion is not perfect, but it should handle most common cases
#' without user intervention.
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

  # A break is a newline or a literal `\\`, and every one of them belongs
  # at the formula level rather than inside the \text{}.
  #
  # A break kept inside the text made a multi-line *text box*, and a box is
  # one item in the enclosing row: anything after it was then set beside
  # the whole box, vertically centred between its lines, instead of on the
  # line it was written on. That is why "Title\n$x^2$" came out on one
  # line, and why a pasted \caption landed to the left of its table.
  #
  # For prose with nothing beside it the two forms lay out identically, so
  # splitting always is free.
  brk <- "(?:[ \t\r]*(?:\n|\\\\\\\\)[ \t\r]*)+"

  emit_text <- function(s) {
    if (!nzchar(s)) return()
    parts <- .split_prose(s, brk)
    # An empty segment at either end means the chunk began or ended with a
    # break: a separator between neighbours, not a line of its own. A run
    # of breaks collapses to one (the `+` in `brk`), the way consecutive
    # blank lines make a single paragraph break in LaTeX -- stripping
    # document wrappers leaves blank lines behind, and each would
    # otherwise be an empty row.
    lead  <- length(parts) > 0L && !nzchar(parts[1L])
    trail <- length(parts) > 1L && !nzchar(parts[length(parts)])
    parts <- parts[nzchar(parts)]
    # Nothing but breaks: a separator between its neighbours, not text.
    if (length(parts) == 0L) {
      out <<- c(out, "\\\\")
      return()
    }
    if (lead) out <<- c(out, "\\\\")
    for (i in seq_along(parts)) {
      if (i > 1L) out <<- c(out, "\\\\")
      out <<- c(out, paste0("\\text{", .escape_prose_amp(parts[i]), "}"))
    }
    if (trail) out <<- c(out, "\\\\")
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

# An `&` in prose is a literal ampersand -- "R&D", "Treatment & Control".
# MicroTeX reads it as an alignment tab even inside \text{}, and drops
# everything after it, so the label lost its second half with no error.
# Escape only the ones the user did not escape themselves.
#
# Prose is the only place this is safe, and it is the only place it is
# applied: math spans and `\begin{tabular}` environments are passed
# through verbatim by .scan_math_spans(), so a real alignment tab never
# reaches here. (The markdown path escapes `&` too, in .md_escape_tex();
# mixed-mode LaTeX was the one input that did not.)
.escape_prose_amp <- function(s) {
  gsub("(?<!\\\\)&", "\\\\&", s, perl = TRUE)
}

# Split a prose chunk at its line breaks, keeping every segment's braces
# balanced.
#
# Each segment becomes its own `\text{}`, so a break inside a group --
# `\textbf{one\\two}` -- used to leave the group open at the end of one
# segment and unopened at the start of the next. The braces balanced out
# across the whole string, so nothing errored; the second line just came
# out unstyled, and any text after the group escaped the \text{} wrapper.
#
# Every group still open at a break is therefore closed before it and
# re-opened after it, which is what LaTeX itself does across a line break
# inside a text command. `\textbf{one\\two}` becomes
# `\text{\textbf{one}}` + `\\` + `\text{\textbf{two}}`.
#
# Returns the segment bodies, without the `\text{}` wrapper. An empty
# first or last element means the chunk began or ended with a break.
.split_prose <- function(s, brk) {
  m <- gregexpr(brk, s, perl = TRUE)[[1]]
  starts <- if (m[1L] == -1L) integer(0) else as.integer(m)
  lens <- if (length(starts)) attr(m, "match.length") else integer(0)

  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  n <- length(chars)
  # Segments are cut out of `s` by position rather than accumulated a
  # character at a time: growing a vector per character made this
  # quadratic, which showed at a few thousand characters.
  segs <- character(0)
  opens <- character(0)   # stack of opener strings, e.g. "\\textbf{"
  reopen <- ""            # openers inherited from the previous segment
  seg_start <- 1L

  flush <- function(upto) {
    body <- if (upto >= seg_start) substr(s, seg_start, upto) else ""
    segs <<- c(segs, paste0(reopen, body, strrep("}", length(opens))))
    reopen <<- paste(opens, collapse = "")
  }

  i <- 1L
  while (i <= n) {
    k <- match(i, starts)
    if (!is.na(k)) {
      flush(i - 1L)
      i <- i + lens[k]
      seg_start <- i
      next
    }
    ch <- chars[i]
    # An escaped literal (`\{`, `\}`, `\&`, ...) is two characters and
    # never opens or closes a group.
    if (ch == "\\" && i < n) {
      i <- i + 2L
      next
    }
    if (ch == "{") {
      # Re-opening needs the command the brace belongs to, not a bare
      # `{`: the group is `\textbf{`. Arguments already given are part of
      # it, both optional (`\parbox[t]{`) and mandatory
      # (`\textcolor{red}{`) -- matching only the command name reopened
      # `\textcolor{red}{two}` as a plain `{two}` and dropped the colour.
      # A command name and its arguments are short, so only the run just
      # before the brace is examined.
      sofar <- substr(s, max(1L, i - 64L), i - 1L)
      cmd <- regmatches(
        sofar,
        regexpr("\\\\[A-Za-z]+(?:\\[[^]]*\\]|\\{[^{}]*\\})*$", sofar))
      opens <- c(opens, paste0(if (length(cmd)) cmd else "", "{"))
    } else if (ch == "}" && length(opens)) {
      opens <- opens[-length(opens)]
    }
    i <- i + 1L
  }
  flush(n)
  segs
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

# The colour of a link. LaTeX's \url and \href are drawn in it by the C++
# front end (link() in front/lower.cpp), following hyperref's colorlinks
# convention; markdown's `a` rule uses it too, with the HTML convention of
# an underline as well.
.MD_LINK_COLOR <- "#0969DA"
