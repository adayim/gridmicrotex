#' The layout of a LaTeX expression
#'
#' Returns what a formula is drawn from: one row per glyph, line, rectangle
#' or run of text, with its position. Useful for checking alignment or
#' building your own grobs.
#'
#' @inheritParams latex_grob
#' @return A list of class `"latex_tree"`:
#'   * `records`: a data frame with one row per drawn element, with
#'     columns such as `type`, `x`, `y`, `glyph`, `font_size`, `color` and
#'     `text`.
#'   * `bbox`: `width`, `height`, `depth` and `baseline`, in big points.
#'   * `tex`: the input.
#'   * `render_mode`: the render mode used.
#' @seealso \code{\link{latex_grob}}, \code{\link{latex_dims}}
#' @export
#'
#' @examples
#' \donttest{
#'   tree <- latex_tree("\\frac{a}{b}")
#'   print(tree)
#'   head(tree$records)
#' }
latex_tree <- function(tex, math_font = "", max_width = 0,
                       tex_style = "",
                       input_mode = c("mixed", "math", "document"),
                       render_mode = c("typeface", "path"),
                       gp = grid::gpar()) {
  .apply_opts("math_font", "render_mode", "tex_style", "input_mode")
  render_mode <- match.arg(render_mode)
  input_mode <- match.arg(input_mode)

  parsed <- .parse_from_gp(
    tex = tex, gp = gp, math_font = math_font, max_width = max_width,
    tex_style = tex_style, render_mode = render_mode,
    input_mode = input_mode
  )
  layout <- parsed$layout

  bbox <- c(
    width    = as.numeric(attr(layout, "bbox_width")),
    height   = as.numeric(attr(layout, "bbox_height")),
    depth    = as.numeric(attr(layout, "bbox_depth")),
    baseline = as.numeric(attr(layout, "bbox_baseline"))
  )

  structure(
    list(
      records = layout,
      bbox = bbox,
      tex = parsed$tex,
      render_mode = parsed$render_mode
    ),
    class = "latex_tree"
  )
}

#' @export
print.latex_tree <- function(x, ...) {
  cat("<latex_tree>\n")
  cat("  tex:         ", x$tex, "\n", sep = "")
  cat("  render_mode: ", x$render_mode, "\n", sep = "")
  cat(sprintf(
    "  bbox:        width=%.2f  height=%.2f  depth=%.2f  baseline=%.2f (bigpts)\n",
    x$bbox[["width"]], x$bbox[["height"]],
    x$bbox[["depth"]], x$bbox[["baseline"]]
  ))
  n <- nrow(x$records)
  cat("  records:     ", n, "\n", sep = "")
  if (n > 0) {
    tbl <- table(x$records$type)
    for (nm in names(tbl)) {
      cat(sprintf("    %-10s %d\n", nm, tbl[[nm]]))
    }
  }
  invisible(x)
}
