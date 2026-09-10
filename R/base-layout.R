# Deciding whether a base-graphics label is math, and laying it out.
#
# The C interceptor only pre-filters: it forwards anything with no
# delimiter byte at all. Everything past that is decided here, on top of
# .scan_math_spans(), so the gate and latex_wrap() can never disagree
# about what counts as math -- the failure mode CLAUDE.md records for
# MATH_ENVS.

# A `$...$` pair is ambiguous in a plot label: "Cost $5-$10" and
# "Price $1,000 to $5,000" are balanced, so a pairing test alone accepts
# them and renders "5-" and "1,000 to " as math, silently. Require the
# span to look like math as well.
#
# Explicit delimiters (\(...\), \[...\], \begin{env}) are never
# ambiguous, so they skip the heuristic.
.gm_span_is_mathish <- function(inner) {
  if (!nzchar(trimws(inner))) return(FALSE)
  # A control sequence, superscript, subscript or group is decisive.
  if (grepl("[\\\\^_{}]", inner)) return(TRUE)
  # A lone variable ("$x$", "$n$") is the other common legitimate form.
  grepl("^[A-Za-z]$", trimws(inner))
}

# TRUE when `str` should be rendered by MicroTeX rather than drawn as
# literal device text. Conservative by construction: anything it is not
# sure about is left to the original callback, byte for byte.
.gm_base_is_math <- function(str) {
  if (!nzchar(str)) return(FALSE)
  spans <- tryCatch(.scan_math_spans(str), error = function(e) NULL)
  if (is.null(spans) || !length(spans)) return(FALSE)

  # A single unclosed delimiter means the string was never math -- it is
  # "Revenue ($)" or "Sales in $m". latex_wrap() would auto-close it and
  # corrupt the label.
  if (any(!vapply(spans, `[[`, logical(1), "closed"))) return(FALSE)

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
# Called from the intercepted device callbacks, so it must not draw and
# must not signal: a failure here has to become "draw the literal
# string", never an error escaping into a half-finished plot.
#
# `fontsize` is gc$ps * gc$cex in points, `col` an R colour, `fontfamily`
# the device's family (may be ""). Face is not carried: `.parse_from_gp()`
# ignores gp$fontface by design (R/latex-grob.R:534), so prose in a bold
# base title renders unbolded -- a documented limitation.
#
# Returns NULL when the label is not math or could not be laid out, else
# a list of the layout data frame and its metrics in bigpts.
.gm_base_layout <- function(str, fontsize, col = "#000000", fontfamily = "") {
  if (!.gm_base_is_math(str)) return(NULL)

  gp <- grid::gpar(fontsize = fontsize, col = col)
  if (nzchar(fontfamily)) gp$fontfamily <- fontfamily

  # Honour the document-wide defaults a user would expect to apply here
  # too. `render_mode` is deliberately not among them: base labels are
  # always drawn as outlines, because a device callback has no way to
  # resolve a math font. `input_mode` is likewise fixed at "mixed" --
  # "math" would set the prose in a plot title as italic variables.
  # Muffle warnings rather than abandoning the layout: an unreadable
  # \includegraphics only warns and still leaves a good layout, and
  # treating that as failure drew the raw LaTeX source instead. Muffling
  # also keeps the callback independent of options(warn = 2), which would
  # otherwise turn every such label into literal text. A warning cannot
  # usefully reach the user from inside a draw callback anyway.
  parsed <- tryCatch(
    withCallingHandlers(
      .parse_from_gp(
        tex = str, gp = gp, math_font = .opt("math_font") %||% "",
        max_width = 0, tex_style = .opt("tex_style") %||% "",
        render_mode = "path", input_mode = "mixed",
        justify = FALSE, line_break = "greedy"
      ),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) NULL
  )
  if (is.null(parsed)) return(NULL)

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
