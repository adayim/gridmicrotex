#' Create a grid grob from a LaTeX expression
#'
#' Parses a LaTeX math expression and returns a grid grob object
#' that renders the formula using native grid graphics primitives.
#' The grob supports standard grid queries such as \code{grobWidth()},
#' \code{grobHeight()}, \code{grobX()}, and \code{grobY()}.
#'
#' @param tex Character string of LaTeX math code.
#' @param x,y Position in grid coordinates.
#' @param default.units Units for x, y if given as numeric.
#' @param hjust,vjust Horizontal/vertical justification (0-1).
#' @param fontsize Base font size in points (default: 20).
#' @param rot Rotation angle in degrees, counter-clockwise (default: 0).
#'   Matches the \code{rot} parameter of \code{\link[grid]{textGrob}}.
#' @param math_font Name of the math font to use (e.g., \code{"xits"}).
#'   Use \code{""} (default) for the default Latin Modern Math font.
#'   See \code{\link{available_math_fonts}} for loaded fonts.
#' @param line_space Numeric inter-line spacing in big points for
#'   multi-line formulas (e.g., \code{\\\\}, \code{\\begin\{array\}}).
#'   Default is \code{10}.
#' @param max_width Numeric maximum width in big points for automatic
#'   line wrapping.  Use \code{0} (default) for no wrapping.
#' @param render_mode Character string: \code{"typeface"} (default) renders
#'   glyphs as native text using the math font, producing
#'   selectable/accessible text in PDF and SVG output.
#'   Requires the math font to be installed on the system.
#'   Falls back to path mode automatically on devices that do not support
#'   font embedding (e.g., the base \code{pdf()} device).
#'   \code{"path"} renders math symbols as filled vector paths (works on
#'   all devices but text is not selectable in PDF/SVG).
#'   For PDF output with embedded/selectable text, prefer
#'   \code{\link[grDevices]{cairo_pdf}}.
#' @param name Optional grob name.
#' @param gp Graphical parameters (see \code{\link[grid]{gpar}}).
#'   Use \code{gpar(col = "red")} to set the default foreground color
#'   for the formula. Individual elements can still be overridden with
#'   \code{\\textcolor\{\}} in the LaTeX string.
#'
#'   Font-related parameters (\code{fontfamily}, \code{fontface}) apply
#'   only to non-math text rendered via \code{\\text\{\}} and
#'   \code{\\mbox\{\}}, not to math symbols (which always use the
#'   selected math font). For standard Latin characters, MicroTeX uses
#'   its own internal font metrics for layout. For characters not in the
#'   math font, R's font metrics are used automatically via a callback.
#'   Packages like \pkg{showtext} can be used to make additional fonts
#'   available to R.
#'
#' @return A \code{grid} grob of class \code{"latexgrob"}.
#' @seealso \code{\link{grid.latex}}, \code{\link{latex_dims}},
#'   \code{\link{geom_latex}}, \code{\link{available_math_fonts}}
#' @export
#'
#' @examples
#' \donttest{
#'   g <- latex_grob("\\frac{a}{b}", fontsize = 30)
#'   grid::grid.draw(g)
#'
#'   # Red formula
#'   grid::grid.draw(latex_grob("x^{2}", gp = grid::gpar(col = "red")))
#'
#'   # Rotated formula
#'   grid::grid.draw(latex_grob("x^{2} + y^{2}", fontsize = 24, rot = 45))
#' }
latex_grob <- function(tex,
                       x = grid::unit(0.5, "npc"),
                       y = grid::unit(0.5, "npc"),
                       default.units = "npc",
                       hjust = 0.5,
                       vjust = 0.5,
                       rot = 0,
                       fontsize = 20,
                       math_font = "",
                       line_space = 10,
                       max_width = 0,
                       render_mode = c("typeface", "path"),
                       name = NULL,
                       gp = grid::gpar()) {

  render_mode <- match.arg(render_mode)

  # Convert numeric x/y to units
  if (is.numeric(x)) x <- grid::unit(x, default.units)
  if (is.numeric(y)) y <- grid::unit(y, default.units)

  # Extract foreground color from gp, default to black
  fg_color <- if (!is.null(gp$col)) {
    grDevices::rgb(t(grDevices::col2rgb(gp$col)), maxColorValue = 255)
  } else {
    "#000000"
  }

  # Resolve font name alias
  math_font <- resolve_math_font(math_font)

  # Extract font settings from gp for text (non-math) grobs
  text_gp <- grid::gpar()
  if (!is.null(gp$fontfamily)) text_gp$fontfamily <- gp$fontfamily
  if (!is.null(gp$fontface))   text_gp$fontface <- gp$fontface

  # Register text measurement callback for accurate \text{} layout
  measurer <- .make_text_measurer(text_gp)
  register_text_measurer(measurer)
  on.exit(clear_text_measurer(), add = TRUE)

  # Parse via C++
  layout <- parse_latex_cpp(
    tex = tex,
    text_size = fontsize,
    line_space = line_space,
    fg_color = fg_color,
    max_width = max_width,
    math_font = math_font,
    use_path = (render_mode == "path")
  )

  # Keep a full path-mode layout in typeface mode so we can
  # safely fall back on devices that cannot render glyphs (e.g. pdf(),
  # windows(), quartz()).
  path_layout <- NULL
  if (render_mode == "typeface") {
    path_layout <- parse_latex_cpp(
      tex = tex,
      text_size = fontsize,
      line_space = line_space,
      fg_color = fg_color,
      max_width = max_width,
      math_font = math_font,
      use_path = TRUE
    )
  }

  # Extract bounding box
  bbox_w <- attr(layout, "bbox_width")
  bbox_h <- attr(layout, "bbox_height")
  bbox_d <- attr(layout, "bbox_depth")
  bbox_bl <- attr(layout, "bbox_baseline")

  # bbox_h from MicroTeX's getHeight() is already ascent + descent
  total_h <- bbox_h

  grid::gTree(
    tex = tex,
    layout_df = layout,
    bbox_w = bbox_w,
    bbox_h = bbox_h,
    bbox_d = bbox_d,
    bbox_bl = bbox_bl,
    total_h = total_h,
    fontsize = fontsize,
    hjust = hjust,
    vjust = vjust,
    text_gp = text_gp,
    render_mode = render_mode,
    path_layout_df = path_layout,
    cl = "latexgrob",
    name = name,
    gp = gp,
    vp = grid::viewport(
      x = x, y = y,
      width = grid::unit(bbox_w, "bigpts"),
      height = grid::unit(total_h, "bigpts"),
      just = c(hjust, vjust),
      angle = rot
    )
  )
}


# Check whether the current graphics device supports rendering glyphGrob
# objects via the dev->glyph() graphics engine interface (R >= 4.3).
# Uses dev.capabilities()$glyphs when a device is open; returns TRUE
# when no device is open (layout-only / measurement context).
.device_supports_typeface_glyphs <- function() {
  cur <- grDevices::dev.cur()
  if (cur == 1L) return(TRUE)  # null device (no drawing)

  caps <- grDevices::dev.capabilities()
  isTRUE(caps[["glyphs"]])
}

#' @method makeContent latexgrob
#' @export
makeContent.latexgrob <- function(x) {
  render_mode <- x$render_mode %||% "typeface"
  layout_df <- x$layout_df

  if (identical(render_mode, "typeface") && !.device_supports_typeface_glyphs()) {
    if (!is.null(x$path_layout_df)) {
      layout_df <- x$path_layout_df
      render_mode <- "path"
      warning(
        paste0(
          "Current graphics device does not support typeface glyph rendering; ",
          "falling back to path mode. Use ragg::agg_png(), svglite::svglite(), ",
          "or grDevices::cairo_pdf() for selectable math text."
        ),
        call. = FALSE
      )
    }
  }

  children <- build_latex_children(
    layout_df, x$total_h,
    depth = x$bbox_d %||% 0,
    text_gp = x$text_gp,
    render_mode = render_mode
  )
  grid::setChildren(x, children)
}

#' @method widthDetails latexgrob
#' @export
widthDetails.latexgrob <- function(x) {
  grid::unit(x$bbox_w, "bigpts")
}

#' @method heightDetails latexgrob
#' @export
heightDetails.latexgrob <- function(x) {
  grid::unit(x$total_h, "bigpts")
}

#' @method xDetails latexgrob
#' @export
xDetails.latexgrob <- function(x, theta) {
  gx <- grid::convertX(x$vp$x, "native", valueOnly = TRUE)
  w <- grid::convertWidth(grid::unit(x$bbox_w, "bigpts"), "native", valueOnly = TRUE)
  hjust <- x$hjust
  left <- gx - hjust * w
  right <- left + w
  theta_deg <- (theta %% 360)
  if (theta_deg >= 90 && theta_deg <= 270) {
    grid::unit(left, "native")
  } else {
    grid::unit(right, "native")
  }
}

#' @method yDetails latexgrob
#' @export
yDetails.latexgrob <- function(x, theta) {
  gy <- grid::convertY(x$vp$y, "native", valueOnly = TRUE)
  h <- grid::convertHeight(grid::unit(x$total_h, "bigpts"), "native", valueOnly = TRUE)
  vjust <- x$vjust
  bottom <- gy - vjust * h
  top <- bottom + h
  theta_deg <- (theta %% 360)
  if (theta_deg > 0 && theta_deg < 180) {
    grid::unit(top, "native")
  } else {
    grid::unit(bottom, "native")
  }
}


#' Draw LaTeX directly to the current device
#'
#' @param tex Character string of LaTeX math code.
#' @param ... Additional arguments passed to \code{\link{latex_grob}}.
#' @return Invisibly returns the grob.
#' @seealso \code{\link{latex_grob}}, \code{\link{latex_dims}}
#' @export
#'
#' @examples
#' \donttest{
#'   grid.latex("x^{2} + y^{2} = z^{2}")
#' }
grid.latex <- function(tex, ...) {
  g <- latex_grob(tex, ...)
  grid::grid.draw(g)
  invisible(g)
}

#' Get dimensions of a LaTeX expression
#'
#' @inheritParams latex_grob
#' @return A list with \code{width}, \code{height}, \code{depth}, and
#'   \code{baseline} as grid unit objects.
#' @export
#'
#' @examples
#' latex_dims("\\frac{a}{b}")
latex_dims <- function(tex, fontsize = 20, math_font = "",
                       line_space = 10, max_width = 0,
                       render_mode = c("typeface", "path")) {
  render_mode <- match.arg(render_mode)
  math_font <- resolve_math_font(math_font)
  measurer <- .make_text_measurer(grid::gpar())
  register_text_measurer(measurer)
  on.exit(clear_text_measurer(), add = TRUE)

  layout <- parse_latex_cpp(tex, text_size = fontsize,
                            line_space = line_space,
                            max_width = max_width,
                            math_font = math_font,
                            use_path = (render_mode == "path"))
  list(
    width    = grid::unit(attr(layout, "bbox_width"), "bigpts"),
    height   = grid::unit(attr(layout, "bbox_height"), "bigpts"),
    depth    = grid::unit(attr(layout, "bbox_depth"), "bigpts"),
    baseline = attr(layout, "bbox_baseline")
  )
}


# On some Windows locale/font combinations, stringWidth() can error for CJK text.
# Keep layout flowing with a simple width fallback instead of failing hard.
.measure_text_bigpts <- function(text) {
  out <- tryCatch(
    grid::convertWidth(grid::stringWidth(text), "bigpts", valueOnly = TRUE),
    error = function(e) {
      w <- tryCatch(base::nchar(text, type = "width"), error = function(...) NA_real_)
      if (is.na(w)) {
        w <- base::nchar(text, type = "chars")
      }
      as.numeric(w) * 6
    }
  )
  as.numeric(out)
}


#' Create a text measurement closure for MicroTeX layout
#'
#' Returns a function that measures text using R's grid graphics system.
#' The closure is called from C++ during \code{parse_latex_cpp()} to get
#' accurate font metrics for \code{\\text\{\}} blocks.
#'
#' @param text_gp A \code{\link[grid]{gpar}} object with font settings
#'   (\code{fontfamily}, \code{fontface}) to use for measurement.
#' @return A function taking \code{(text, font_style)} that returns
#'   \code{c(width_ratio, ascent_ratio, height_ratio)} where ratios
#'   are relative to the font size.
#' @keywords internal
.make_text_measurer <- function(text_gp) {
  ref_size <- 72  # reference size in points for measurement precision

  # Cache the R version check
  has_ascent_fn <- getRversion() >= "4.4.0"

  function(text, font_style) {
    face <- .resolve_text_face(as.integer(font_style))

    gp <- grid::gpar(fontsize = ref_size, fontface = face)
    if (!is.null(text_gp$fontfamily)) {
      gp$fontfamily <- text_gp$fontfamily
    }

    # Ensure a graphics device is available for measurement
    needs_dev <- grDevices::dev.cur() == 1L
    if (needs_dev) {
      grDevices::pdf(NULL)
    }

    # Push temporary viewport with our font settings
    grid::pushViewport(grid::viewport(gp = gp))
    # Pop viewport before closing device (order matters)
    on.exit({
      grid::popViewport()
      if (needs_dev) grDevices::dev.off()
    }, add = TRUE)

    # Measure width in bigpts
    w <- .measure_text_bigpts(text)

    # Measure ascent and descent (with Windows/CJK locale fallback)
    ad <- tryCatch({
      if (has_ascent_fn) {
        asc <- grid::convertHeight(
          get("stringAscent", envir = asNamespace("grid"))(text),
          "bigpts", valueOnly = TRUE
        )
        desc <- grid::convertHeight(
          get("stringDescent", envir = asNamespace("grid"))(text),
          "bigpts", valueOnly = TRUE
        )
      } else {
        h <- grid::convertHeight(grid::stringHeight(text), "bigpts", valueOnly = TRUE)
        asc <- h * 0.8
        desc <- h - asc
      }
      c(asc, desc)
    }, error = function(e) {
      # Approximate: 80% of font size for ascent, 20% for descent
      c(ref_size * 0.8, ref_size * 0.2)
    })
    asc  <- ad[1]
    desc <- ad[2]

    # Return ratios relative to font size
    c(w / ref_size, asc / ref_size, (asc + desc) / ref_size)
  }
}
