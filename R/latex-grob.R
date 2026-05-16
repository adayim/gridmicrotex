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
#' @param hjust,vjust Horizontal/vertical justification. Accepts the
#'   usual numeric values in `[0, 1]`. As a convenience, `hjust` also
#'   accepts the strings `"left"`/`"bbleft"`, `"center"`/`"centre"`/
#'   `"middle"`/`"bbcentre"`, and `"right"`/`"bbright"`; `vjust` accepts
#'   `"bottom"`, `"center"`/`"centre"`/`"middle"`, `"top"`, and
#'   `"baseline"`. `"baseline"` aligns the formula's math baseline with
#'   the anchor point — handy for placing a formula in flowing text.
#' @param rot Rotation angle in degrees, counter-clockwise (default: 0).
#'   Matches the \code{rot} parameter of \code{\link[grid]{textGrob}}.
#' @param math_font Name of the math font to use (e.g., \code{"stix"}).
#'   Use \code{""} (default) for Lete Sans Math, which pairs with R's
#'   default sans-serif text font.
#'   See \code{\link{available_math_fonts}} for loaded fonts.
#' @param max_width Numeric maximum width in big points for automatic
#'   line wrapping.  Use \code{0} (default) for no wrapping.
#' @param tex_style Character: TeX style override. One of \code{""}
#'   (default; let the parser decide), \code{"display"}, \code{"text"},
#'   \code{"script"}, or \code{"scriptscript"}. See
#'   \code{\link{latex_grob}} for the semantics of each value.
#' @param input_mode How \code{tex} is interpreted before being parsed.
#'   \code{"mixed"} wraps the input in \code{\\text{...}} so the string
#'   reads as ordinary text and \code{$...$} (or \code{\\(...\\)}) opens
#'   math mode, matching document-level LaTeX semantics. Useful for labels
#'   that arrive from external sources mixing prose and math without explicit
#'   \code{\\text{}} markers. \code{"math"} (default) is the standard
#'   MicroTeX behaviour --- the whole string is treated as math, so unwrapped
#'   prose renders as spaced math italics. The default can be changed globally via
#'   \code{\link{latex_options}(input_mode = "mixed")}. See \code{\link{latex_wrap}}
#'  for details on the wrapping process.
#' @param render_mode Character string: \code{"typeface"} (default) renders
#'   glyphs as native text using the math font, producing
#'   selectable/accessible text in PDF and SVG output.
#'   Bundled math fonts and any registered via \code{\link{load_font}}
#'   are read directly from their OTF files --- no system-wide font
#'   install is required.
#'   Falls back to path mode automatically on devices that lack the
#'   R \eqn{\geq} 4.3 glyph engine (e.g., the base \code{pdf()} device).
#'   For selectable PDF output, prefer \code{\link[grDevices]{cairo_pdf}}.
#'   \code{"path"} renders math symbols as filled vector paths (works on
#'   all devices but text is not selectable in PDF/SVG).
#' @param debug Logical; if \code{TRUE}, draws diagnostic overlays on the
#'   grob --- the full bounding box (dashed gray), the baseline (solid
#'   red), the depth line (dashed gray), and a small dot at each
#'   MicroTeX draw record's origin. Useful for checking positioning and
#'   diagnosing vertical alignment.
#' @param name Optional grob name.
#' @param gp Graphical parameters (see \code{\link[grid]{gpar}}).
#'   Common entries: \code{col} (formula foreground), \code{fontfamily}
#'   / \code{fontface} (text font), \code{fontsize} / \code{cex}
#'   (formula size), and \code{lineheight} (multi-line spacing). See
#'   \code{\link{latex_grob}} for how each of these flows through
#'   MicroTeX.
#'
#' @details
#' ## Controlling TeX style with `tex_style`
#'
#' `tex_style` selects the size-and-spacing regime MicroTeX applies to
#' the whole expression. It changes the *style* (display vs. text), not
#' the font size --- size is always set via `gp$fontsize` / `gp$cex`;
#' style-dependent shrinking (for `"script"` and `"scriptscript"`) is
#' applied on top of that size.
#'
#' - `""` (default): let the parser choose based on the delimiters in
#'   `tex`. Inline delimiters (single `$`, or `\(...\)`) produce
#'   `"text"` style; display delimiters (double `$$`, or `\[...\]`)
#'   produce `"display"` style. If the string has no delimiters,
#'   MicroTeX defaults to `"text"` style.
#' - `"display"`: force display style. Large operators (`\sum`,
#'   `\int`, `\prod`) render at their full size, limits are placed
#'   above/below rather than as subscripts/superscripts, and fractions
#'   use full-size numerators and denominators. Useful when you want a
#'   display-style equation inline in a label, legend, or
#'   `element_latex()` title.
#' - `"text"`: force text (inline) style. Big operators shrink to their
#'   inline size and limits attach as scripts. The right choice for
#'   formulas embedded in a line of prose.
#' - `"script"`: force script style --- the size normally used for
#'   first-level subscripts and superscripts. Produces a smaller,
#'   tighter layout; mainly useful for callouts or sub-labels where a
#'   compact equation is wanted.
#' - `"scriptscript"`: force scriptscript style --- the smallest style,
#'   used by TeX for doubly-nested scripts. Rarely needed on its own;
#'   primarily for very dense annotations.
#'
#' `tex_style` applies to the entire expression. To override the style
#' of a sub-expression from within `tex`, use the inline TeX commands
#' `\displaystyle`, `\textstyle`, `\scriptstyle`, or
#' `\scriptscriptstyle`.
#'
#' ## Graphical parameters (`gp`)
#'
#' - `col`: default foreground color for the formula. Individual
#'   elements can still be overridden with an inline `\textcolor`
#'   command in the LaTeX string.
#' - `fontfamily` / `fontface`: control the appearance of text inside
#'   `\text` and `\mbox` blocks. For example, `gpar(fontfamily =
#'   "serif")` renders `\text` content in R's serif family. Any font
#'   available to R's graphics system works --- base families
#'   (`"sans"`, `"serif"`, `"mono"`) as well as fonts registered via
#'   \pkg{showtext} or \pkg{systemfonts}. Math symbols always use the
#'   selected math font (see `math_font`).
#'
#'   `fontfamily` *also* drives MicroTeX's layout metrics for non-math
#'   text: the matching system font is resolved via \pkg{systemfonts},
#'   a minimal metrics file is generated on first use and cached under
#'   `tools::R_user_dir("gridmicrotex", "cache")`, so MicroTeX's
#'   spacing of `\text` blocks stays in sync with what \pkg{grid}
#'   actually draws. When `fontfamily` is unset, the R default
#'   (`"sans"`) is used. No manual font loading is required for text
#'   fonts; [load_font()] remains only for adding custom **math**
#'   fonts.
#' - `fontsize` / `cex`: formula size is `fontsize * cex` big points
#'   (default 20 * 1). Both math and text scale together. The effective
#'   size is baked into the parsed layout, so downstream viewports that
#'   inherit `cex` will not re-scale the grob (matching `textGrob`
#'   semantics when `gp` is set explicitly).
#' - `lineheight`: controls multi-line spacing (default 1.2). The
#'   inter-line gap is `(lineheight - 1) * fontsize` big points.
#'
#' @return A \code{grid} grob of class \code{"latexgrob"}.
#' @seealso \code{\link{grid.latex}}, \code{\link{latex_dims}},
#'   \code{\link{geom_latex}}, \code{\link{available_math_fonts}},
#' \code{\link{latex_wrap}}, \code{\link{latex_options}}
#' @export
#'
#' @examples
#' \donttest{
#'   g <- latex_grob(r"($\fcolorbox{red}{yellow}{\frac{a}{b}}$)",
#'                   x = grid::unit(0.3, "npc"),
#'                   y = grid::unit(0.3, "npc"),
#'                   gp = grid::gpar(fontsize = 30))
#'   grid::grid.draw(g)
#'   # Red formula
#'   grid::grid.draw(latex_grob("$x^{2}$",
#'                              x = grid::unit(0.3, "npc"),
#'                              y = grid::unit(0.8, "npc"),
#'                              gp = grid::gpar(col = "red")))
#'
#'                              # Rotated formula
#'   grid::grid.draw(latex_grob(r"($\colorbox{BurntOrange}{x^{2}} + y^{2}$)",
#'                              x = grid::unit(0.6, "npc"),
#'                              y = grid::unit(0.3, "npc"),
#'                              gp = grid::gpar(fontsize = 24),
#'                              rot = 45))
#'
#'   grid.latex(r"($\textcolor{red}{x^{2}} + y^{2} = z^{2}$)",
#'              x = grid::unit(0.6, "npc"),
#'              y = grid::unit(0.8, "npc"),)
#' }
latex_grob <- function(tex,
                       x = grid::unit(0.5, "npc"),
                       y = grid::unit(0.5, "npc"),
                       default.units = "npc",
                       hjust = 0.5,
                       vjust = 0.5,
                       rot = 0,
                       math_font = "",
                       max_width = 0,
                       tex_style = "",
                       input_mode = c("mixed", "math"),
                       render_mode = c("typeface", "path"),
                       debug = FALSE,
                       name = NULL,
                       gp = grid::gpar()) {

  .apply_opts("math_font", "render_mode", "tex_style", "input_mode")
  render_mode <- match.arg(render_mode)
  input_mode <- match.arg(input_mode)

  parsed <- .parse_from_gp(
    tex = tex, gp = gp, math_font = math_font, max_width = max_width,
    tex_style = tex_style, render_mode = render_mode,
    input_mode = input_mode,
    with_path_fallback = TRUE
  )

  # Convert numeric x/y to units
  if (is.numeric(x)) x <- grid::unit(x, default.units)
  if (is.numeric(y)) y <- grid::unit(y, default.units)

  layout <- parsed$layout
  bbox_w <- attr(layout, "bbox_width")
  bbox_h <- attr(layout, "bbox_height")
  bbox_d <- attr(layout, "bbox_depth")
  # Baseline as bigpts from the bottom edge of the bounding box —
  # exposed read-only on the gTree for advanced grob-to-grob alignment.
  bbox_bl_bp <- bbox_h * (1 - attr(layout, "bbox_baseline"))
  is_split <- isTRUE(attr(layout, "bbox_is_split"))

  just <- .resolve_just(hjust, vjust, bbox_bl_bp = bbox_bl_bp, bbox_h = bbox_h)
  marks <- .extract_marks(layout, bbox_h = bbox_h)

  grid::gTree(
    tex = parsed$tex,
    layout_df = layout,
    bbox_w = bbox_w,
    bbox_h = bbox_h,
    bbox_d = bbox_d,
    bbox_bl_bp = bbox_bl_bp,
    is_split = is_split,
    marks = marks,
    fontsize = parsed$fontsize,
    hjust = just$hjust,
    vjust = just$vjust,
    hjust_input = hjust,
    vjust_input = vjust,
    # Input parameters kept on the grob so editGrob() can re-parse when
    # any of them change. Resolved/baked values live in the parsed fields
    # above; these fields hold the user-facing inputs.
    math_font = math_font,
    max_width = max_width,
    tex_style = tex_style,
    input_mode = input_mode,
    text_gp = parsed$text_gp,
    render_mode = parsed$render_mode,
    path_layout_df = parsed$path_layout,
    debug = isTRUE(debug),
    cl = "latexgrob",
    name = name,
    gp = parsed$gp,
    vp = grid::viewport(
      x = x, y = y,
      width = grid::unit(bbox_w, "bigpts"),
      height = grid::unit(bbox_h, "bigpts"),
      just = c(just$hjust, just$vjust),
      angle = rot
    )
  )
}

# Extract \mark{name} anchors from a parsed layout. MicroTeX's y axis is
# top-down; flip to grid's bottom-up so the values can be added directly
# to a bigpts-from-bbox-bottom-left reference like the children grobs use.
# Returns a data.frame with columns (name, x, y) in bigpts, or NULL when
# no marks were emitted.
.extract_marks <- function(layout, bbox_h) {
  m <- attr(layout, "marks")
  if (is.null(m) || nrow(m) == 0L) return(NULL)
  data.frame(
    name = as.character(m$name),
    x    = as.numeric(m$x),
    y    = bbox_h - as.numeric(m$y),
    stringsAsFactors = FALSE
  )
}

#' Look up a named anchor inside a LaTeX grob
#'
#' Resolves a \code{\\mark\{name\}} that was placed inside the LaTeX
#' source to a pair of \pkg{grid} units in the grob's parent viewport.
#' The returned units already account for the grob's viewport position
#' and \code{hjust}/\code{vjust}, so you can pass them directly to grid
#' drawing functions to anchor other graphics on parts of the formula.
#'
#' @param grob A \code{latexgrob} returned by \code{\link{latex_grob}}.
#' @param name The mark name (the argument to \code{\\mark\{...\}}).
#' @return A list with elements \code{x} and \code{y}, each a
#'   \code{\link[grid]{unit}}. Mark coordinates are evaluated in the
#'   grob's parent viewport.
#' @seealso \code{\link{latex_grob}}
#' @export
#'
#' @examples
#' \donttest{
#'   g <- latex_grob(r"($a\mark{eq}^2 = b + c^2$)",
#'                   x = grid::unit(0.5, "npc"),
#'                   y = grid::unit(0.5, "npc"))
#'   grid::grid.newpage(); grid::grid.draw(g)
#'   mk <- grobMark(g, "eq")
#'   grid::grid.points(mk$x, mk$y, pch = 19,
#'                     gp = grid::gpar(col = "red"))
#' }
grobMark <- function(grob, name) {
  if (!inherits(grob, "latexgrob")) {
    stop("grob must be a latexgrob (returned by latex_grob()).", call. = FALSE)
  }
  marks <- grob$marks
  if (is.null(marks) || nrow(marks) == 0L) {
    stop("This grob has no marks. Place \\mark{name} inside the LaTeX source.",
         call. = FALSE)
  }
  idx <- match(name, marks$name)
  if (is.na(idx)) {
    stop(
      "Mark '", name, "' not found. Available: ",
      paste(sprintf("'%s'", marks$name), collapse = ", "),
      call. = FALSE
    )
  }
  vp <- grob$vp
  bbox_w <- grob$bbox_w
  bbox_h <- grob$bbox_h
  # bbox bottom-left in the parent viewport, expressed as a unit
  # expression so it resolves lazily at draw time.
  left   <- vp$x - grid::unit(grob$hjust * bbox_w, "bigpts")
  bottom <- vp$y - grid::unit(grob$vjust * bbox_h, "bigpts")
  list(
    x = left   + grid::unit(marks$x[idx], "bigpts"),
    y = bottom + grid::unit(marks$y[idx], "bigpts")
  )
}

# Translate string-valued hjust/vjust into the [0,1] viewport just values
# grid expects. Numeric inputs pass through unchanged. "baseline" (vjust
# only) places the formula's math baseline at the anchor point — using
# bbox_bl_bp / bbox_h, the same baseline that grobs query via
# `ascentDetails()`/`descentDetails()`.
.resolve_just <- function(hjust, vjust, bbox_bl_bp, bbox_h) {
  hj <- .resolve_hjust(hjust)
  vj <- .resolve_vjust(vjust, bbox_bl_bp = bbox_bl_bp, bbox_h = bbox_h)
  list(hjust = hj, vjust = vj)
}

.hjust_strings <- c(
  left     = 0,
  bbleft   = 0,
  center   = 0.5,
  centre   = 0.5,
  middle   = 0.5,
  bbcentre = 0.5,
  right    = 1,
  bbright  = 1
)

.resolve_hjust <- function(hjust) {
  if (is.numeric(hjust)) return(hjust)
  if (!is.character(hjust) || length(hjust) != 1L) {
    stop("hjust must be a numeric or a single string.", call. = FALSE)
  }
  v <- .hjust_strings[hjust]
  if (is.na(v)) {
    stop(
      "hjust must be numeric or one of: ",
      paste(sprintf("'%s'", names(.hjust_strings)), collapse = ", "),
      call. = FALSE
    )
  }
  unname(v)
}

.resolve_vjust <- function(vjust, bbox_bl_bp, bbox_h) {
  if (is.numeric(vjust)) return(vjust)
  if (!is.character(vjust) || length(vjust) != 1L) {
    stop("vjust must be a numeric or a single string.", call. = FALSE)
  }
  switch(
    vjust,
    bottom = 0,
    center = ,
    centre = ,
    middle = 0.5,
    top = 1,
    baseline = if (bbox_h > 0) bbox_bl_bp / bbox_h else 0.5,
    stop(
      "vjust must be numeric or one of: 'bottom', 'center'/'centre'/'middle', ",
      "'top', 'baseline'.",
      call. = FALSE
    )
  )
}

# Shared parse pipeline used by latex_grob(), latex_dims(), latex_tree().
# Resolves fontsize/cex/lineheight/fontfamily/fontface/col out of `gp`,
# runs MicroTeX parse via the cache, optionally also runs a path-mode
# parse for device-fallback. Returns the layout and the stripped-down
# `gp` safe to attach to child grobs (fontsize/cex/lineheight removed
# so they don't re-scale at draw time).
.parse_from_gp <- function(tex, gp, math_font, max_width, tex_style,
                           render_mode, input_mode = "mixed",
                           with_path_fallback = FALSE) {
  .ensure_bundled_fonts_registered()
  .check_tex_style(tex_style)
  input_mode <- match.arg(input_mode, c("math", "mixed"))
  if (max_width < 0) stop("max_width must be non-negative.", call. = FALSE)

  tex <- .expand_macros(tex)
  # The user-facing `tex` stays as the macro-expanded source so that
  # editDetails() can re-parse without doubling up the \text{} wrap.
  # `parse_input` is the actual string handed to the MicroTeX parser.
  parse_input <- latex_wrap(tex, input_mode = input_mode)
  math_font <- resolve_math_font(math_font)

  fg_color <- if (!is.null(gp$col)) {
    grDevices::rgb(t(grDevices::col2rgb(gp$col)), maxColorValue = 255)
  } else {
    "#000000"
  }

  # Grid semantics: gp$fontsize is in points, gp$cex multiplies it,
  # gp$lineheight is total-line-height multiplier. Bake these into the
  # parse call (layout depends on them), then strip from gp so they
  # don't re-apply at draw time.
  fontsize <- gp$fontsize %||% 20
  if (!is.null(gp$cex)) fontsize <- fontsize * gp$cex
  line_space <- .line_space_from_lineheight(gp$lineheight, fontsize)
  gp$fontsize <- NULL
  gp$cex <- NULL
  gp$lineheight <- NULL

  text_gp <- grid::gpar()
  if (!is.null(gp$fontfamily)) text_gp$fontfamily <- gp$fontfamily
  if (!is.null(gp$fontface))   text_gp$fontface <- gp$fontface

  main_font <- .resolve_text_font(text_gp$fontfamily %||% "sans")

  measurer <- .make_text_measurer(text_gp)
  register_text_measurer(measurer)
  on.exit(clear_text_measurer(), add = TRUE)

  layout <- .parse_latex_cached(
    tex = parse_input, text_size = fontsize, line_space = line_space,
    fg_color = fg_color, max_width = max_width, math_font = math_font,
    main_font = main_font, use_path = (render_mode == "path"),
    tex_style = tex_style
  )

  path_layout <- NULL
  if (with_path_fallback && render_mode == "typeface") {
    path_layout <- .parse_latex_cached(
      tex = parse_input, text_size = fontsize, line_space = line_space,
      fg_color = fg_color, max_width = max_width, math_font = math_font,
      main_font = main_font, use_path = TRUE, tex_style = tex_style
    )
  }

  list(
    tex = tex,
    layout = layout,
    path_layout = path_layout,
    fontsize = fontsize,
    fg_color = fg_color,
    text_gp = text_gp,
    gp = gp,
    render_mode = render_mode
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
    layout_df, x$bbox_h,
    depth = x$bbox_d %||% 0,
    text_gp = x$text_gp,
    render_mode = render_mode
  )

  if (isTRUE(x$debug)) {
    children <- .add_debug_overlay(
      children, layout_df,
      total_h = x$bbox_h,
      bbox_w = x$bbox_w,
      depth = x$bbox_d %||% 0
    )
  }

  grid::setChildren(x, children)
}

# Fields whose values feed .parse_from_gp(); editing any of them forces a
# re-parse so the layout/bbox/text metrics stay in sync with the inputs.
.latex_parse_fields <- c("tex", "math_font", "max_width", "tex_style",
                         "input_mode", "render_mode", "gp")

#' @method editDetails latexgrob
#' @export
editDetails.latexgrob <- function(x, specs) {
  if (length(specs) == 0L) return(x)

  parse_changed <- any(.latex_parse_fields %in% names(specs))
  just_changed  <- any(c("hjust", "vjust") %in% names(specs))

  if (parse_changed) {
    parsed <- .parse_from_gp(
      tex = x$tex, gp = x$gp, math_font = x$math_font,
      max_width = x$max_width, tex_style = x$tex_style,
      input_mode = x$input_mode %||% "math",
      render_mode = x$render_mode, with_path_fallback = TRUE
    )
    layout <- parsed$layout
    x$tex            <- parsed$tex
    x$layout_df      <- layout
    x$bbox_w         <- attr(layout, "bbox_width")
    x$bbox_h         <- attr(layout, "bbox_height")
    x$bbox_d         <- attr(layout, "bbox_depth")
    x$bbox_bl_bp     <- x$bbox_h * (1 - attr(layout, "bbox_baseline"))
    x$is_split       <- isTRUE(attr(layout, "bbox_is_split"))
    x$marks          <- .extract_marks(layout, bbox_h = x$bbox_h)
    x$fontsize       <- parsed$fontsize
    x$text_gp        <- parsed$text_gp
    x$render_mode    <- parsed$render_mode
    x$path_layout_df <- parsed$path_layout
    x$gp             <- parsed$gp
  }

  # When the user edits hjust/vjust, the spec value is the new raw input
  # (potentially a string like "baseline"). Stash it so a later parse-only
  # edit can re-resolve correctly against the new bbox.
  if ("hjust" %in% names(specs)) x$hjust_input <- specs$hjust
  if ("vjust" %in% names(specs)) x$vjust_input <- specs$vjust

  if ((parse_changed || just_changed) && !is.null(x$vp)) {
    just <- .resolve_just(
      x$hjust_input %||% x$hjust,
      x$vjust_input %||% x$vjust,
      bbox_bl_bp = x$bbox_bl_bp, bbox_h = x$bbox_h
    )
    x$hjust <- just$hjust
    x$vjust <- just$vjust
    old_vp <- x$vp
    x$vp <- grid::viewport(
      x = old_vp$x, y = old_vp$y,
      width  = grid::unit(x$bbox_w, "bigpts"),
      height = grid::unit(x$bbox_h, "bigpts"),
      just   = c(just$hjust, just$vjust),
      angle  = old_vp$angle
    )
  }

  x
}

.add_debug_overlay <- function(children, layout_df, total_h, bbox_w, depth) {
  bbox <- grid::rectGrob(
    x = grid::unit(0, "bigpts"),
    y = grid::unit(0, "bigpts"),
    width = grid::unit(bbox_w, "bigpts"),
    height = grid::unit(total_h, "bigpts"),
    just = c("left", "bottom"),
    gp = grid::gpar(col = "gray60", fill = NA, lty = "dashed", lwd = 0.5),
    name = "debug.bbox"
  )

  baseline_y <- depth
  baseline <- grid::segmentsGrob(
    x0 = grid::unit(0, "bigpts"),
    y0 = grid::unit(baseline_y, "bigpts"),
    x1 = grid::unit(bbox_w, "bigpts"),
    y1 = grid::unit(baseline_y, "bigpts"),
    gp = grid::gpar(col = "red", lwd = 0.75),
    name = "debug.baseline"
  )

  depth_line <- grid::segmentsGrob(
    x0 = grid::unit(0, "bigpts"),
    y0 = grid::unit(0, "bigpts"),
    x1 = grid::unit(bbox_w, "bigpts"),
    y1 = grid::unit(0, "bigpts"),
    gp = grid::gpar(col = "gray60", lwd = 0.5, lty = "dashed"),
    name = "debug.depth"
  )

  overlays <- grid::gList(bbox, depth_line, baseline)

  n <- nrow(layout_df)
  if (!is.null(n) && n > 0) {
    ox <- layout_df$x
    oy <- total_h - layout_df$y
    keep <- is.finite(ox) & is.finite(oy)
    if (any(keep)) {
      dots <- grid::pointsGrob(
        x = grid::unit(ox[keep], "bigpts"),
        y = grid::unit(oy[keep], "bigpts"),
        pch = 20,
        size = grid::unit(0.6, "mm"),
        gp = grid::gpar(col = "blue"),
        name = "debug.origins"
      )
      overlays <- grid::gList(overlays, dots)
    }
  }

  grid::gList(children, overlays)
}

#' @method widthDetails latexgrob
#' @export
widthDetails.latexgrob <- function(x) {
  grid::unit(x$bbox_w, "bigpts")
}

#' @method heightDetails latexgrob
#' @export
heightDetails.latexgrob <- function(x) {
  grid::unit(x$bbox_h, "bigpts")
}

#' @method ascentDetails latexgrob
#' @export
ascentDetails.latexgrob <- function(x) {
  grid::unit(x$bbox_h - x$bbox_d, "bigpts")
}

#' @method descentDetails latexgrob
#' @export
descentDetails.latexgrob <- function(x) {
  grid::unit(x$bbox_d, "bigpts")
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
  h <- grid::convertHeight(grid::unit(x$bbox_h, "bigpts"), "native", valueOnly = TRUE)
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
#' A convenience wrapper that creates a \code{\link{latex_grob}} and
#' immediately draws it on the current device via
#' \code{\link[grid]{grid.draw}}.
#'
#' @param tex Character string of LaTeX math code.
#' @param ... Additional arguments passed to \code{\link{latex_grob}}.
#' @return Invisibly returns the grob.
#' @rdname latex_grob
#' @export
#'
grid.latex <- function(tex, ...) {
  g <- latex_grob(tex, ...)
  grid::grid.draw(g)
  invisible(g)
}

#' Get dimensions of a LaTeX expression
#'
#' @inheritParams latex_grob
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{width}, \code{height}, \code{depth}: grid unit objects
#'     in big points. \code{height} is total height (ascent + descent).
#'   \item \code{baseline}: grid unit object giving the baseline position
#'     measured in big points from the \emph{bottom} of the bounding box.
#'     Equivalent to \code{height - depth} for single-line formulas. Useful
#'     for aligning a formula's baseline with surrounding text.
#'   \item \code{is_split}: logical; \code{TRUE} if the formula was wrapped
#'     across multiple lines (only possible when \code{max_width > 0}).
#' }
#' @export
#'
#' @examples
#' latex_dims("\\frac{a}{b}")
latex_dims <- function(tex, math_font = "", max_width = 0,
                       tex_style = "",
                       input_mode = c("mixed", "math"),
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
  bbox_h <- attr(layout, "bbox_height")
  bbox_bl_frac <- attr(layout, "bbox_baseline")
  list(
    width    = grid::unit(attr(layout, "bbox_width"), "bigpts"),
    height   = grid::unit(bbox_h, "bigpts"),
    depth    = grid::unit(attr(layout, "bbox_depth"), "bigpts"),
    baseline = grid::unit(bbox_h * (1 - bbox_bl_frac), "bigpts"),
    is_split = isTRUE(attr(layout, "bbox_is_split"))
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

  # Per-closure cache keyed on (font_style, text). Lifetime = one parse
  # (the closure is created fresh per parse in latex_grob/latex_dims), so
  # graphics state can't drift between calls. Hits avoid a push/pop
  # viewport + grid::convertHeight round trip per repeated span.
  cache <- new.env(parent = emptyenv())

  function(text, font_style) {
    key <- paste0(as.integer(font_style), "\x1f", text)
    hit <- cache[[key]]
    if (!is.null(hit)) return(hit)

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

    result <- c(w / ref_size, asc / ref_size, (asc + desc) / ref_size)
    cache[[key]] <- result
    result
  }
}
