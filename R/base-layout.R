# Deciding whether a base-graphics label is math, and laying it out.
#
# The C interceptor only pre-filters: it forwards anything with no
# delimiter byte at all. Everything past that is decided here, on top of
# .scan_math_spans(), so the gate and latex_wrap() can never disagree
# about what counts as math.

# A `$...$` pair is ambiguous in a plot label: "Cost $5-$10" and
# "Price $1,000 to $5,000" are balanced, so a pairing test alone accepts
# them and renders "5-" and "1,000 to " as math, silently. Require the
# span to look like math as well.
#
# Explicit delimiters (\(...\), \[...\], \begin{env}) are never
# ambiguous, so they skip the heuristic.
#
# This gate runs on every label containing a `$` that a device draws, twice
# or more per label, so it works on code points: on Windows each grepl() or
# trimws() costs tens of microseconds or more.
.gm_span_is_mathish <- function(inner) {
  cps <- utf8ToInt(inner)
  if (anyNA(cps)) return(FALSE)
  cps <- cps[!cps %in% .GM_SPACE]
  if (!length(cps)) return(FALSE)
  # A control sequence, superscript, subscript or group is decisive.
  if (any(cps %in% c(92L, 94L, 95L, 123L, 125L))) return(TRUE)   # \ ^ _ { }
  # A lone variable ("$x$", "$n$") is the other common legitimate form.
  length(cps) == 1L && (cps %in% 65:90 || cps %in% 97:122)
}

# The code points trimws() treats as white space.
.GM_SPACE <- c(9L, 10L, 13L, 32L)

# TRUE when span `s` of `str` opens with R's `$` accessor rather than a
# math delimiter. R writes `$` for column access in the labels it makes up
# itself: hist(df$price_usd - df$tax_usd) is titled "Histogram of
# df$price_usd - df$tax_usd", a closed pair around "price_usd - df". An
# accessor sits between a name -- or `)`, `]`, a closing backtick -- and
# the start of the next name. LaTeX glued to a word does not: what follows
# the `$` in CO$_2$, R$^2$ or 5$\times$ starts no R name.
.gm_span_is_accessor <- function(s, str) {
  if (s$kind != "dollar1") return(FALSE)
  at <- s$outer_start
  .gm_name_char(substr(str, at - 1L, at - 1L), start = FALSE) &&
    .gm_name_char(substr(str, at + 1L, at + 1L), start = TRUE)
}

# Whether the single character `ch` ("" at either end of the label) can
# end an R name as a label shows it, or with `start`, begin one. ASCII --
# nearly every label -- is decided on the code point; only other scripts
# pay for a \p{} regex.
.gm_name_char <- function(ch, start) {
  cp <- utf8ToInt(ch)
  if (length(cp) != 1L || is.na(cp)) return(FALSE)
  if (cp >= 128L) {
    return(grepl(if (start) "^\\p{L}$" else "^[\\p{L}\\p{N}]$", ch,
                 perl = TRUE))
  }
  alpha <- (cp >= 65L && cp <= 90L) || (cp >= 97L && cp <= 122L)
  if (start) return(alpha || cp %in% c(46L, 96L))                  # . `
  alpha || (cp >= 48L && cp <= 57L) ||
    cp %in% c(41L, 46L, 93L, 95L, 96L)                              # ) . ] _ `
}

# TRUE when `str` should be rendered by MicroTeX rather than drawn as
# literal device text. Conservative by construction: anything it is not
# sure about is left to the original callback, byte for byte.
.gm_base_is_math <- function(str) {
  # UTF-8, so utf8ToInt() and the \p{} classes see any script.
  str <- enc2utf8(str)
  if (!nzchar(str)) return(FALSE)
  spans <- tryCatch(.scan_math_spans(str), error = function(e) NULL)
  if (is.null(spans) || !length(spans)) return(FALSE)

  # A single unclosed delimiter means the string was never math -- it is
  # "Revenue ($)" or "Sales in $m". latex_wrap() would auto-close it and
  # corrupt the label.
  if (any(!vapply(spans, `[[`, logical(1), "closed"))) return(FALSE)

  if (any(vapply(spans, .gm_span_is_accessor, logical(1), str = str))) {
    return(FALSE)
  }

  # A label that is a single formula is much less likely to be a price, so
  # its content need not look like math: "$y = 2x + 1$", "$f(x)$". It must
  # still be shaped the way pandoc requires of `$` math -- no space just
  # inside either delimiter -- and hold more than a number, or "$5 to $",
  # "$ - $" and "$1,000$" would all qualify.
  if (length(spans) == 1L && spans[[1]]$kind %in% c("dollar1", "dollar2")) {
    s <- spans[[1]]
    outside <- utf8ToInt(paste0(substr(str, 1L, s$outer_start - 1L),
                                substr(str, s$outer_end + 1L, nchar(str))))
    inner <- utf8ToInt(substr(str, s$inner_start, s$inner_end))
    n <- length(inner)
    if (all(outside %in% .GM_SPACE) && n > 0L && !anyNA(inner) &&
        !inner[1L] %in% .GM_SPACE && !inner[n] %in% .GM_SPACE &&
        !all(inner %in% c(.GM_SPACE, 44L, 46L, 48:57))) {        # , . 0-9
      return(TRUE)
    }
  }

  # Every span must qualify, not merely one of them. latex_wrap() has no
  # way to be told "treat span 3 as math and leave span 1 alone", so one
  # good span would drag the rest in: "Budget $1,000 to $5,000 with $x$
  # shown" would set the currency as math.
  all(vapply(spans, function(s) {
    if (s$inner_end < s$inner_start) return(FALSE)   # empty content
    if (!s$kind %in% c("dollar1", "dollar2")) return(TRUE)
    .gm_span_is_mathish(substr(str, s$inner_start, s$inner_end))
  }, logical(1)))
}

# Lay out one base-graphics label.
#
# Called from the intercepted device callbacks, so it must not draw. This
# is the one place a failure is caught -- the C caller lets every jump
# through -- and a label that cannot be laid out becomes "draw the literal
# string" rather than an error escaping into a half-finished plot. An
# interrupt is never caught and a time limit is signalled again, so both
# still stop the plot.
#
# `fontsize` is gc$ps * gc$cex in points, `col` an R colour, `fontfamily`
# the device's family (may be ""). Face is not carried: `.parse_from_gp()`
# ignores gp$fontface by design (R/latex-grob.R:534), so prose in a bold
# base title renders unbolded -- a documented limitation.
#
# Returns NULL when the label is not math or could not be laid out, else
# a list of the layout data frame and its metrics in bigpts.
.gm_base_layout <- function(str, fontsize, col = "#000000", fontfamily = "") {
  tryCatch(
    .gm_base_layout_impl(str, fontsize, col, fontfamily),
    error = function(e) if (.gm_time_limit_error(e)) stop(e) else NULL
  )
}

# setTimeLimit() stops with a plain simpleError, so it can only be told
# apart by its message -- in whichever language R itself used.
.gm_time_limit_error <- function(e) {
  limits <- gettext(c("reached elapsed time limit", "reached CPU time limit",
                      "reached session elapsed time limit",
                      "reached session CPU time limit"), domain = "R")
  conditionMessage(e) %in% limits
}

.gm_base_layout_impl <- function(str, fontsize, col, fontfamily) {
  if (!.gm_base_is_math(str)) return(NULL)

  gp <- grid::gpar(fontsize = fontsize, col = col)
  if (nzchar(fontfamily)) gp$fontfamily <- fontfamily

  # Honour the document-wide defaults a user would expect to apply here
  # too. `render_mode` is deliberately not among them: base labels are
  # always drawn as outlines, because a device callback has no way to
  # resolve a math font. `input_mode` is likewise fixed at "mixed" --
  # "math" would set the prose in a plot title as italic variables.
  # Muffle warnings rather than abandoning the layout: an unreadable
  # \includegraphics only warns here and still leaves a good layout, and
  # treating that as failure drew the raw LaTeX source instead. Muffling
  # also keeps the callback independent of options(warn = 2), which would
  # otherwise turn every such label into literal text. A warning cannot
  # usefully reach the user from inside a draw callback anyway.
  #
  # An unreadable image is not an error here (.images_lenient()): base
  # labels never draw images, and an error would be caught below and turn
  # the whole label into literal text.
  parsed <- withCallingHandlers(
    .images_lenient(.parse_from_gp(
      tex = str, gp = gp, math_font = .opt("math_font") %||% "",
      max_width = 0, tex_style = .opt("tex_style") %||% "",
      render_mode = "path", input_mode = "mixed",
      justify = FALSE, line_break = "greedy"
    )),
    warning = function(w) invokeRestart("muffleWarning")
  )

  layout <- parsed$layout
  h <- as.numeric(attr(layout, "bbox_height"))
  # as.numeric() is load-bearing, not defensive: MicroTeX's getWidth()
  # and getHeight() return int, so these attributes arrive as integers
  # and the C emitter reads them as doubles.
  list(
    layout   = layout,
    width    = as.numeric(attr(layout, "bbox_width")),
    height   = h,
    depth    = as.numeric(attr(layout, "bbox_depth")),
    # Distance from the top of the box down to the baseline. The C
    # emitter is handed a baseline y by the engine and works downward
    # from the box top, which is the mirror of what R/latex-grob.R:291
    # needs for grid.
    ascent   = h * attr(layout, "bbox_baseline")
  )
}
