# Null-coalescing operator. Defined here so it's available to every R
# file in the package (R loads files alphabetically by default, and
# `o` sorts ahead of the files that use it).
`%||%` <- function(x, y) if (is.null(x)) y else x

.latex_options <- new.env(parent = emptyenv())
.latex_options$values <- list(
  math_font   = NULL,
  render_mode = NULL,
  tex_style   = NULL,
  input_mode  = NULL,
  justify     = NULL,
  line_break  = NULL,
  markdown_style = NULL,
  device_math   = NULL
)

# Validate the `justify` argument. Kept here so latex_grob(),
# latex_dims() and latex_options() all reject the same things.
.check_justify <- function(justify) {
  if (!is.logical(justify) || length(justify) != 1L || is.na(justify)) {
    stop("`justify` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Set or query package-wide LaTeX rendering defaults
#'
#' A single entry point for project-wide defaults used by
#' \code{\link{latex_grob}}, \code{\link{grid.latex}},
#' \code{\link{latex_dims}}, and \code{\link{latex_tree}}. Options set
#' here are applied only when the corresponding argument is \emph{not}
#' supplied at the call site, so explicit arguments always win.
#'
#' Calling \code{latex_options()} with no arguments returns the current
#' settings (a list whose \code{NULL} entries mean "use the built-in
#' default"). Supply one or more named arguments to update them.
#'
#' Font size and line spacing are controlled via \code{gp} parameters
#' (\code{fontsize}, \code{cex}, \code{lineheight}) at the grob level;
#' see \code{\link{latex_grob}}.
#'
#' @param math_font Math font name or alias (see
#'   \code{\link{available_math_fonts}}).
#' @param render_mode Either \code{"typeface"} or \code{"path"}.
#' @param tex_style TeX style override. One of \code{""} (let the parser
#'   decide), \code{"display"}, \code{"text"}, \code{"script"}, or
#'   \code{"scriptscript"}. \code{"display"} forces large operators with
#'   limits placed over/under, useful for inline labels that should still
#'   look like display equations.
#' @param input_mode How the input string is interpreted before being
#'   handed to MicroTeX. \code{"mixed"} (default) wraps the string in
#'   \code{\\text{...}} so it reads as ordinary text, with \code{$...$}
#'   (and \code{\\(...\\)}) opening math mode: the document-level
#'   LaTeX convention. Useful when consuming labels from other packages
#'   that mix prose and math without explicit \code{\\text{}} markers.
#'   \code{"math"} treats the whole string as math: the classic
#'   MicroTeX behaviour, where letters render as math italics and
#'   unwrapped prose looks wrong.
#' @param justify Logical. When \code{TRUE}, wrapped text is stretched at
#'   its interword spaces so every line but the last fills
#'   \code{max_width} exactly. Has no effect without \code{max_width},
#'   since it acts on the lines the wrapper produces. \code{FALSE}
#'   (default) leaves the right edge ragged, matching R's own text
#'   drawing. In a narrow column, justifying alone opens noticeably wide
#'   word spaces; mark the words that may break with \code{\\-}.
#' @param line_break How lines are chosen when wrapping.
#'   \code{"greedy"} (default) fills each line as far as it will go and
#'   never reconsiders. \code{"optimal"} chooses the breaks together so
#'   the paragraph as a whole reads best, in the spirit of Knuth-Plass:
#'   pulling one word down early can improve every later line, which a
#'   greedy pass cannot see. Requires \code{max_width}, and costs a
#'   little more layout time.
#' @param markdown_style Default style for \code{\link{markdown_grob}} and
#'   \code{\link{markdown_box_grob}}: a \code{\link{markdown_style}}
#'   object, CSS text, or a path to a \code{.css} file.
#' @param device_math Logical. When \code{TRUE}, text drawn to the
#'   graphics device is rendered with MicroTeX wherever it contains math.
#'   The motivating case is \emph{base} graphics, which has no other route
#'   to LaTeX: \code{plot(main=)}, \code{xlab}, \code{ylab},
#'   \code{\link[graphics]{text}}, \code{\link[graphics]{mtext}},
#'   \code{\link[graphics]{legend}}, and anything built on them such as
#'   \code{hist()} or a package's own \code{plot} method.
#'
#'   Interception happens at the device, so it is \strong{not} limited to
#'   base graphics: text drawn by \pkg{grid}, \pkg{ggplot2} and
#'   \pkg{lattice} is affected too. \code{grid.text("$x^2$")} renders math
#'   while this is on. Grid users normally want \code{\link{latex_grob}}
#'   or \code{\link{element_latex}} instead, which give the same result
#'   without a session-wide switch.
#'
#'   The convention is the one \code{\link{latex_wrap}} already uses:
#'   \code{$...$}, \code{$$...$$}, \code{\\(...\\)}, \code{\\[...\\]},
#'   with \code{\\$} a literal dollar sign. A label is intercepted only
#'   when \emph{every} delimiter in it is closed and the content looks
#'   like math, so \code{"Revenue ($)"}, \code{"Cost $5-$10"} and
#'   \code{"Budget $1,000 to $5,000"} are passed through untouched.
#'   Anything that cannot be laid out is drawn as plain text rather than
#'   raising an error.
#'
#'   \strong{Height is the one thing that cannot be corrected.} R computes
#'   text height from the font, never from the string, and a graphics
#'   device has no string-height entry point to intercept. A tall formula
#'   can therefore overflow a \code{legend()} box or the space
#'   \code{par("mar")} reserved for it. Widths \emph{are} correct. Reserve
#'   the room yourself with \code{\link{latex_dims}}:
#'
#'   \preformatted{
#'   h  <- latex_dims("$\\\\frac{a}{b}$",
#'                    gp = grid::gpar(fontsize = par("ps")))$height
#'   bp <- grid::convertHeight(h, "bigpts", TRUE)
#'   # par(mar) counts lines of par("cin"), not grid's "lines".
#'   need <- ceiling(bp / (par("cin")[2] * 72 * par("mex")))
#'   par(mar = c(5, need + 1, 4, 2))
#'   }
#'
#'   Other limitations: math is drawn as vector outlines, so unlike
#'   \code{\link{latex_grob}} it is not selectable in a PDF; prose inside a
#'   bold label is not bolded, because face comes from \code{\\textbf}
#'   rather than from the device; \code{\\includegraphics} is left out; and
#'   rounded box corners are drawn square.
#'
#'   \code{expression()} labels are untouched: R lays plotmath out inside
#'   the graphics engine, so a device never sees them.
#' @return Invisibly returns the previous settings (a list). With no
#'   arguments, returns the current settings visibly.
#' @seealso \code{\link{available_math_fonts}}, \code{\link{latex_grob}}
#' @export
#'
#' @examples
#' \donttest{
#'   latex_options(math_font = "stix", render_mode = "typeface")
#'   grid.latex("\\sum_{i=1}^{n} i^{2}", gp = grid::gpar(fontsize = 14))
#'   reset_latex_options()
#'
#'   # Math in base graphics, with no other change to the plotting code.
#'   # on.exit() rather than a trailing call: R CMD check runs examples on
#'   # a shared device, which must not be left intercepted if this errors.
#'   local({
#'     on.exit(latex_options(device_math = FALSE), add = TRUE)
#'     latex_options(device_math = TRUE)
#'     plot(1:10, (1:10)^2,
#'          main = "Slope $\\hat{\\beta}_1 = \\sum_{i=1}^{n} x_i^2$",
#'          ylab = "$y^2$")
#'   })
#' }
latex_options <- function(math_font = NULL, render_mode = NULL,
                          tex_style = NULL, input_mode = NULL,
                          justify = NULL, line_break = NULL,
                          markdown_style = NULL, device_math = NULL) {
  if (nargs() == 0L) {
    return(as.list(.latex_options$values))
  }

  old <- as.list(.latex_options$values)

  if (!is.null(math_font)) {
    stopifnot(is.character(math_font), length(math_font) == 1L)
    .set_math_font(math_font)
    .latex_options$values$math_font <- math_font
  }
  if (!is.null(render_mode)) {
    render_mode <- match.arg(render_mode, c("typeface", "path"))
    .latex_options$values$render_mode <- render_mode
  }
  if (!is.null(tex_style)) {
    .check_tex_style(tex_style)
    .latex_options$values$tex_style <- tex_style
  }
  if (!is.null(input_mode)) {
    input_mode <- match.arg(input_mode, c("math", "mixed"))
    .latex_options$values$input_mode <- input_mode
  }
  if (!is.null(justify)) {
    .check_justify(justify)
    .latex_options$values$justify <- justify
  }
  if (!is.null(line_break)) {
    line_break <- match.arg(line_break, c("greedy", "optimal"))
    .latex_options$values$line_break <- line_break
  }
  if (!is.null(markdown_style)) {
    # Coerce here rather than at each use, so a bad value is rejected by
    # the call that set it.
    .latex_options$values$markdown_style <- .md_as_style(markdown_style)
  }
  if (!is.null(device_math)) {
    if (!is.logical(device_math) || length(device_math) != 1L || is.na(device_math)) {
      stop("`device_math` must be TRUE or FALSE.", call. = FALSE)
    }
    # Setting this one has an effect on open devices, in the same way
    # `math_font` switches the engine font rather than only recording a
    # preference. Flip the devices before recording, so a failure in the
    # C layer leaves the option reading FALSE rather than lying.
    .gm_base_set(device_math)
    .latex_options$values$device_math <- device_math
  }
  invisible(old)
}

#' @rdname latex_options
#'
#' @export
reset_latex_options <- function() {
  # Clearing the value is not enough: armed devices would stay hooked
  # while the option read FALSE, and stale callbacks would outlive the
  # option that installed them.
  .gm_base_set(FALSE)
  .latex_options$values <- list(
    math_font   = NULL,
    render_mode = NULL,
    tex_style   = NULL,
    input_mode  = NULL,
    justify     = NULL,
    line_break  = NULL,
    markdown_style = NULL,
    device_math   = NULL
  )
  invisible(NULL)
}

# Internal: resolve an argument against latex_options().
.opt <- function(name) {
  .latex_options$values[[name]]
}

# Resolve named formal args of the calling function against
# latex_options(): any arg the caller did not supply explicitly is
# replaced (in the caller's frame) by .opt(<name>) when that option is
# set. Used by latex_grob(), latex_dims(), latex_tree() so the
# missing-arg-then-fallback pattern lives in one place.
.apply_opts <- function(...) {
  env <- parent.frame()
  for (n in c(...)) {
    if (eval(call("missing", as.name(n)), env)) {
      opt <- .opt(n)
      if (!is.null(opt)) assign(n, opt, envir = env)
    }
  }
}

# Internal: validate a tex_style value. Use exact matching (not match.arg)
# because "" is one of the valid choices and partial matching against an
# empty string is ambiguous.
.tex_style_choices <- c("", "display", "text", "script", "scriptscript")
.check_tex_style <- function(x) {
  stopifnot(is.character(x), length(x) == 1L)
  if (!x %in% .tex_style_choices) {
    stop(
      "tex_style must be one of: ",
      paste(sprintf("'%s'", .tex_style_choices), collapse = ", "),
      call. = FALSE
    )
  }
  x
}
