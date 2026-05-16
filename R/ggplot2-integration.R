# ggplot2 integration: geom_latex() and element_latex()
# All ggplot2-dependent code is guarded with requireNamespace() checks
# so the core package works without ggplot2 installed.
#
# ggplot2 >= 4.0 uses S7 for theme elements, so element_latex is defined
# as an S7 subclass of ggplot2::element_text. Both S7 and ggplot2 are
# soft dependencies — everything is created dynamically in .onLoad_ggplot2().

# --------------------------------------------------------------------------
# geom_latex() — A ggplot2 geom for rendering LaTeX labels
# --------------------------------------------------------------------------

#' A ggplot2 geom for LaTeX math labels
#'
#' Renders LaTeX math expressions as native grid grobs within a ggplot2 plot.
#' Each label is parsed and laid out by MicroTeX, producing resolution-independent
#' vector output.
#'
#' @section Aesthetics:
#' \code{geom_latex()} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \strong{\code{label}} — LaTeX math string
#'   \item \code{size} — font size in points (default: 11)
#'   \item \code{colour} — text colour (default: \code{"black"})
#'   \item \code{angle} — rotation angle in degrees (default: 0)
#'   \item \code{hjust} — horizontal justification, 0–1 (default: 0.5)
#'   \item \code{vjust} — vertical justification, 0–1 (default: 0.5)
#'   \item \code{alpha} — transparency (default: 1)
#' }
#'
#' @inheritParams ggplot2::layer
#' @inheritParams latex_grob
#' @param fontsize Default font size in points. Overridden by the \code{size}
#'   aesthetic if mapped.
#' @param math_font Name of the math font to use (e.g., \code{"stix"}).
#' @param lineheight Multi-line height multiplier (default 1.2), matching
#'   \code{grid::gpar()} semantics.
#' @param max_width Maximum width in big points for automatic line
#'   wrapping (default: 0, no wrapping).
#' @param na.rm If \code{FALSE}, the default, missing values are removed with
#'   a warning. If \code{TRUE}, missing values are silently removed.
#' @param ... Other arguments passed to \code{\link[ggplot2]{layer}}.
#'
#' @return A ggplot2 layer.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   df <- data.frame(
#'     x = 1:3, y = 1:3,
#'     eq = c("x^2", "\\frac{a}{b}", "\\sum_{i=1}^n x_i")
#'   )
#'   ggplot(df, aes(x, y, label = eq)) + geom_latex()
#'
#'   # Use annotate() for single annotations (no legend, no data frame needed)
#'   ggplot(mtcars, aes(wt, mpg)) + geom_point() +
#'     annotate("latex", x = 4, y = 30,
#'              label = r"($\hat{y} = \beta_0 + \beta_1 x$)")
#' }
#' }
geom_latex <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", ...,
                       fontsize = 11, math_font = "",
                       lineheight = 1.2, max_width = 0,
                       input_mode = c("mixed", "math"),
                       render_mode = c("typeface", "path"),
                       na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE) {
  .apply_opts("math_font", "render_mode", "input_mode")
  render_mode <- match.arg(render_mode)
  input_mode <- match.arg(input_mode)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for geom_latex(). ",
         "Please install it with install.packages('ggplot2').",
         call. = FALSE)
  }

  ggplot2::layer(
    geom = GeomLatex,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fontsize = fontsize,
      math_font = math_font,
      lineheight = lineheight,
      max_width = max_width,
      input_mode = input_mode,
      render_mode = render_mode,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname geom_latex
#' @format NULL
#' @usage NULL
#' @export
GeomLatex <- NULL

# --------------------------------------------------------------------------
# element_latex() — A custom ggplot2 theme element (S7)
# --------------------------------------------------------------------------

#' A ggplot2 theme element for LaTeX text
#'
#' Use this as a theme element for axis titles, axis labels, plot titles,
#' or any other text element in a ggplot2 theme. The text string is parsed
#' as LaTeX math and rendered via MicroTeX.
#'
#' Dollar signs (\code{$...$}) in the label text are stripped automatically
#' so that both \code{"\\frac{a}{b}"} and \code{"$\\frac{a}{b}$"} work.
#'
#' This element is an S7 subclass of \code{ggplot2::element_text}, so it
#' inherits all standard text properties (size, colour, hjust, etc.) from
#' the theme and supports \code{merge_element()} correctly.
#'
#' @inheritParams latex_grob
#' @param fontsize Convenience alias for \code{size}; when supplied,
#'   it is forwarded to \code{ggplot2::element_text()} as the text size
#'   in points. If \code{NULL} (default), the theme's inherited size is
#'   used.
#' @param lineheight Multi-line height multiplier (default 1.2), matching
#'   \code{grid::gpar()} semantics.
#' @param ... Additional arguments passed to \code{ggplot2::element_text()}
#'   (e.g., \code{size}, \code{colour}, \code{hjust}).
#'
#' @return An S7 object of class \code{element_latex}, inheriting from
#'   \code{ggplot2::element_text}.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(mtcars, aes(wt, mpg)) + geom_point() +
#'     labs(x = "$\\beta_1 \\cdot x + \\beta_0$") +
#'     theme(axis.title.x = element_latex())
#' }
#' }
element_latex <- function(math_font = "", fontsize = NULL,
                         lineheight = 1.2, max_width = 0,
                         input_mode = c("mixed", "math"),
                         render_mode = c("typeface", "path"), ...) {
  .apply_opts("math_font", "render_mode", "input_mode")
  render_mode <- match.arg(render_mode)
  input_mode <- match.arg(input_mode)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for element_latex(). ",
         "Please install it with install.packages('ggplot2').",
         call. = FALSE)
  }
  # Map the convenience 'fontsize' alias to 'size' (element_text param)
  dots <- list(...)
  if (!is.null(fontsize)) {
    dots$size <- fontsize
  }
  obj <- do.call(.element_latex_class, c(list(math_font = math_font, lineheight = lineheight, max_width = max_width, input_mode = input_mode, render_mode = render_mode), dots))
  # ggplot2's element_text constructor injects legacy "element_text" and
  # "element" S3 strings into the class vector; S7 inheritance loses them,
  # which makes combine_elements treat us as an unrelated sibling and drop
  # us when resolving inherited theme entries (e.g. axis.title.y inheriting
  # from a user-set axis.title). Re-inject them so subclass detection works.
  class(obj) <- union(
    union(c("gridmicrotex::element_latex", "ggplot2::element_text", "element_text"), class(obj)),
    "element"
  )
  obj
}

# Placeholder — replaced in .onLoad_ggplot2() with the real S7 constructor
.element_latex_class <- NULL

# --------------------------------------------------------------------------
# .onLoad_ggplot2() — called from .onLoad in zzz.R
# --------------------------------------------------------------------------
.onLoad_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return()

  # --- GeomLatex ggproto object ---
  GeomLatex <<- ggplot2::ggproto("GeomLatex", ggplot2::Geom,

    required_aes = c("x", "y", "label"),

    default_aes = ggplot2::aes(
      size = 11,
      colour = "black",
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      alpha = 1
    ),

    draw_key = ggplot2::draw_key_text,

    draw_panel = function(data, panel_params, coord, fontsize = 11,
                          math_font = "", lineheight = 1.2, max_width = 0,
                          input_mode = "mixed",
                          render_mode = "typeface",
                          na.rm = FALSE) {
      coords <- coord$transform(data, panel_params)

      grobs <- lapply(seq_len(nrow(coords)), function(i) {
        row <- coords[i, ]

        if (is.na(row$label) || !nzchar(row$label)) return(grid::nullGrob())

        fs <- row$size
        col <- if (!is.null(row$alpha) && row$alpha < 1) {
          grDevices::adjustcolor(row$colour, alpha.f = row$alpha)
        } else {
          row$colour
        }

        latex_grob(
          tex = row$label,
          x = grid::unit(row$x, "npc"),
          y = grid::unit(row$y, "npc"),
          hjust = row$hjust,
          vjust = row$vjust,
          rot = row$angle %||% 0,
          math_font = math_font,
          max_width = max_width,
          input_mode = input_mode,
          render_mode = render_mode,
          gp = grid::gpar(col = col, fontsize = fs, lineheight = lineheight)
        )
      })

      do.call(grid::gList, grobs)
    }
  )

  # --- element_latex S7 class ---
  .element_latex_class <<- S7::new_class(
    "element_latex",
    parent = ggplot2::element_text,
    properties = list(
      math_font = S7::new_property(S7::class_character, default = ""),
      lineheight = S7::new_property(S7::class_numeric, default = 1.2),
      max_width = S7::new_property(S7::class_numeric, default = 0),
      input_mode = S7::new_property(S7::class_character, default = "mixed"),
      render_mode = S7::new_property(S7::class_character, default = "typeface")
    )
  )

  # --- element_grob S3 method for element_latex ---
  # element_grob is still an S3 generic in ggplot2, so we register via S3.
  # S7 objects dispatch correctly on S3 generics thanks to their class vector.
  registerS3method(
    "element_grob", "gridmicrotex::element_latex",
    .element_grob_latex,
    envir = asNamespace("ggplot2")
  )

}

# --------------------------------------------------------------------------
# Internal helpers
# --------------------------------------------------------------------------

# element_grob method implementation for element_latex.
# Defined outside .onLoad_ggplot2 so it can be referenced by name.
# Signature mirrors ggplot2:::element_grob.element_text so callers (axes,
# guides) can override element properties per-call.
.element_grob_latex <- function(element, label = "", x = NULL, y = NULL,
                                family = NULL, face = NULL, colour = NULL,
                                size = NULL, hjust = NULL, vjust = NULL,
                                angle = NULL, lineheight = NULL,
                                margin = NULL, margin_x = FALSE,
                                margin_y = FALSE, ...) {
  if (is.null(label) || (length(label) == 1L && (!nzchar(label) || is.na(label)))) {
    return(grid::nullGrob())
  }

  # Per-call overrides win, then element values, then a final fallback.
  fontsize    <- size       %||% element@size       %||% 11
  colour      <- colour     %||% element@colour     %||% "black"
  hjust       <- hjust      %||% element@hjust      %||% 0.5
  vjust       <- vjust      %||% element@vjust      %||% 0.5
  angle       <- angle      %||% element@angle      %||% 0
  family      <- family     %||% element@family
  face        <- face       %||% element@face
  lineheight  <- lineheight %||% element@lineheight %||% 1.2
  math_font   <- element@math_font   %||% ""
  max_width   <- element@max_width   %||% 0
  input_mode  <- element@input_mode  %||% "mixed"
  render_mode <- element@render_mode %||% "typeface"

  gp <- grid::gpar(col = colour, fontsize = fontsize, lineheight = lineheight)
  if (!is.null(family) && nzchar(family)) gp$fontfamily <- family
  if (!is.null(face)   && nzchar(face))   gp$fontface   <- face

  # In math mode, strip enclosing $...$ that users add by analogy with
  # plotmath-style labels. In text mode, $ toggles math sub-spans, so
  # leave them intact.
  strip_dollars <- function(s) {
    if (input_mode == "mixed") s else gsub("^\\$|\\$$", "", s)
  }

  # When x/y aren't supplied, anchor at the rotation-adjusted just so the
  # rotated text lands where ggplot2:::titleGrob would put it (e.g. for
  # axis.title.y with angle=90, vjust=1: anchor at left-middle, text reads
  # bottom-to-top across the cell).
  default_just <- .rotate_just(angle, hjust, vjust)

  if (length(label) == 1L) {
    label <- strip_dollars(label)
    if (is.null(x)) x <- grid::unit(default_just$hjust, "npc")
    if (is.null(y)) y <- grid::unit(default_just$vjust, "npc")
    return(latex_grob(
      tex = label, x = x, y = y,
      hjust = hjust, vjust = vjust, rot = angle,
      math_font = math_font, max_width = max_width,
      input_mode = input_mode, render_mode = render_mode,
      gp = gp
    ))
  }

  # Multiple labels (axis tick labels) — render a gTree of grobs.
  label <- strip_dollars(label)
  n <- length(label)
  if (is.null(x)) x <- grid::unit(rep(default_just$hjust, n), "npc")
  if (is.null(y)) y <- grid::unit(rep(default_just$vjust, n), "npc")

  grobs <- grid::gList()
  for (i in seq_len(n)) {
    lab <- label[i]
    if (is.na(lab) || !nzchar(lab)) next
    grobs <- grid::gList(grobs, latex_grob(
      tex = lab, x = .pick_unit(x, i), y = .pick_unit(y, i),
      hjust = hjust, vjust = vjust, rot = angle,
      math_font = math_font, max_width = max_width,
      input_mode = input_mode, render_mode = render_mode,
      gp = gp,
      name = paste0("ticklabel.", i)
    ))
  }
  grid::gTree(children = grobs, name = "axis.latex.labels")
}

# ggplot2 passes per-tick positions for one axis and a scalar unit for the
# orthogonal one (e.g. axis.text.x: x is length-n, y is length-1). Recycle
# the scalar so `unit[i]` doesn't error past length 1.
.pick_unit <- function(u, i) {
  if (inherits(u, "unit")) {
    if (length(u) == 1L) u else u[i]
  } else {
    grid::unit(u[((i - 1L) %% length(u)) + 1L], "npc")
  }
}

# Mirror of ggplot2:::rotate_just. Picks the anchor point so that a
# rotated grob with raw (hjust, vjust) lands inside the cell rather than
# overflowing one of its edges.
.rotate_just <- function(angle, hjust, vjust) {
  angle <- (angle %||% 0) %% 360
  if (angle < 90) {
    list(hjust = hjust,     vjust = vjust)
  } else if (angle < 180) {
    list(hjust = 1 - vjust, vjust = hjust)
  } else if (angle < 270) {
    list(hjust = 1 - hjust, vjust = 1 - vjust)
  } else {
    list(hjust = vjust,     vjust = 1 - hjust)
  }
}
