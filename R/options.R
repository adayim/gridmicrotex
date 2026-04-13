.latex_options <- new.env(parent = emptyenv())
.latex_options$values <- list(
  math_font   = NULL,
  render_mode = NULL
)

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
latex_options <- function(math_font = NULL, render_mode = NULL) {
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
  invisible(old)
}

#' @rdname latex_options
#'
#' @export
reset_latex_options <- function() {
  .latex_options$values <- list(
    math_font   = NULL,
    render_mode = NULL
  )
  invisible(NULL)
}

# Internal: resolve an argument against latex_options().
.opt <- function(name) {
  .latex_options$values[[name]]
}
