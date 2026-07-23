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
  justify     = NULL
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
#' (\code{fontsize}, \code{cex}, \code{lineheight}) at the grob level
#' --- see \code{\link{latex_grob}}.
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
#'   (and \code{\\(...\\)}) opening math mode --- the document-level
#'   LaTeX convention. Useful when consuming labels from other packages
#'   that mix prose and math without explicit \code{\\text{}} markers.
#'   \code{"math"} treats the whole string as math --- the classic
#'   MicroTeX behaviour, where letters render as math italics and
#'   unwrapped prose looks wrong.
#' @param justify Logical. When \code{TRUE}, wrapped text is stretched at
#'   its interword spaces so every line but the last fills
#'   \code{max_width} exactly. Has no effect without \code{max_width},
#'   since it acts on the lines the wrapper produces. \code{FALSE}
#'   (default) leaves the right edge ragged, matching R's own text
#'   drawing. Note that gridmicrotex does not hyphenate, so justifying a
#'   narrow column opens noticeably wide word spaces.
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
#' }
latex_options <- function(math_font = NULL, render_mode = NULL,
                          tex_style = NULL, input_mode = NULL,
                          justify = NULL) {
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
  invisible(old)
}

#' @rdname latex_options
#'
#' @export
reset_latex_options <- function() {
  .latex_options$values <- list(
    math_font   = NULL,
    render_mode = NULL,
    tex_style   = NULL,
    input_mode  = NULL,
    justify     = NULL
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
