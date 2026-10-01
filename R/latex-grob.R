#' Create a grid grob from LaTeX
#'
#' `latex_grob()` returns a grob that draws LaTeX: a formula, a label
#' mixing text and math, or a document body. `grid.latex()` draws it
#' straight away. The grob works with `grobWidth()`, `grobHeight()`,
#' `grobX()` and `grobY()`.
#'
#' @param tex LaTeX, as a character string.
#' @param x,y Position.
#' @param default.units Units for `x` and `y` when they are numbers.
#' @param hjust,vjust Justification: a number in `[0, 1]`, or a name.
#'   `hjust` takes `"left"`, `"center"` and `"right"` (also `"centre"`,
#'   `"middle"`, `"bbleft"`, `"bbcentre"`, `"bbright"`). `vjust` takes
#'   `"bottom"`, `"center"`, `"top"` and `"baseline"`, which puts the
#'   formula's baseline on `y`.
#' @param rot Rotation in degrees, counter-clockwise.
#' @param math_font Math font: `"lete"` (Lete Sans Math, the default),
#'   `"stix"` (STIX Two Math), or one added with [load_math_font()]. See
#'   [available_math_fonts()].
#' @param max_width Width in big points (1/72 inch) at which lines wrap.
#'   `0`, the default, does not wrap.
#' @param tex_style Force a TeX style: `"display"`, `"text"`, `"script"`
#'   or `"scriptscript"`. `""`, the default, follows the delimiters. See
#'   Details.
#' @param input_mode How `tex` is read:
#'   * `"mixed"` (default): text, with math between `$...$` or `\(...\)`.
#'     A newline starts a new line.
#'   * `"math"`: everything is math; write text in `\text{}`.
#'   * `"document"`: a LaTeX document body, with paragraphs, numbered
#'     headings and displayed equations. Use it with `max_width`.
#' @param render_mode `"typeface"` (default) draws glyphs as text, which
#'   can be selected in PDF and SVG output. It needs a device such as
#'   ragg, svglite or [grDevices::cairo_pdf()], and falls back to
#'   `"path"` on others, such as `pdf()`. `"path"` draws glyphs as
#'   outlines, which works on every device.
#' @param justify If `TRUE`, wrapped lines are stretched to fill
#'   `max_width`, except the last. Needs `max_width`.
#' @param line_break `"greedy"` (default) fills one line at a time.
#'   `"optimal"` chooses the breaks for the whole paragraph. Needs
#'   `max_width`.
#' @param debug If `TRUE`, draws the bounding box, the baseline (red) and
#'   a dot at the origin of each glyph.
#' @param name Grob name.
#' @param gp Graphical parameters from [grid::gpar()]: `col`,
#'   `fontfamily`, `fontsize`, `cex` and `lineheight`. See Details.
#'
#' @details
#' ## Style
#'
#' `$...$` sets a formula in text style, as in a paragraph; `$$...$$` and
#' `\[...\]` set it in display style, with larger operators and limits
#' above and below. A label with no delimiters is set in text style.
#' `tex_style` forces one style on the whole formula; `"script"` and
#' `"scriptscript"` are the smaller styles of sub- and superscripts. For
#' part of a formula, use `\displaystyle`, `\textstyle`, `\scriptstyle`
#' or `\scriptscriptstyle`.
#'
#' ## Graphical parameters
#'
#' * `col`: the colour. `\textcolor` overrides it.
#' * `fontfamily`: the font of text, such as `"serif"` or the name of any
#'   installed font. Math uses `math_font`. Bold and italic come from
#'   `\textbf{}` and `\textit{}`, not from `fontface`.
#' * `fontsize`, `cex`: the size is `fontsize * cex` points (default 20).
#' * `lineheight`: line spacing (default 1.2).
#'
#' ## Errors
#'
#' Invalid LaTeX is drawn as well as it can be, with one warning listing
#' each problem by line and column. An unknown command is drawn in red. A
#' macro that expands without end, or nesting deeper than 400 levels, is
#' an error.
#'
#' ## Pasted LaTeX
#'
#' LaTeX from a document can be pasted as it is: the output of
#' `knitr::kable()` or `xtable`, a `table` float, or a paper's body with
#' `input_mode = "document"`. The preamble, `\maketitle` and `\label` draw
#' nothing, and `\caption` is drawn where it is written. `\ref` and
#' `\pageref` draw `??`, `\eqref` draws `(??)` and `\cite` draws `[?]`,
#' with a warning. A footnote is set where it is written, and equations
#' are not numbered.
#'
#' Not supported: `\tag`, `\verb`, `\textsc`, switches such as `\bfseries`
#' and `\itshape` (use `\textbf{}` and `\textit{}`, or `\bf` and `\it`),
#' the `description` list, theorem environments and TikZ.
#'
#' ## Images
#'
#' `\includegraphics[options]{file}` draws a local PNG, JPEG or SVG file.
#' Each format needs a suggested package: png, jpeg, or rsvg and
#' grImport2 for SVG. `width`, `height` and `scale` size the image,
#' `keepaspectratio` fits it inside both, and `angle` rotates it;
#' `\textwidth` means `max_width`. `trim` and `clip` are ignored with a
#' warning. The extension may be left off, and `\graphicspath{{dir/}}`
#' adds a folder to search.
#'
#' An SVG stays sharp at any size; a PNG or JPEG warns when it is shown
#' below 150 dpi. PDF and EPS files are not supported. A file that cannot
#' be drawn is an error, except with `input_mode = "document"`, where it
#' warns and draws the file name instead.
#'
#' ## Parallel code
#'
#' Do not draw in forked processes, such as `parallel::mclapply()` or
#' `future::plan(multicore)`. Use `future::plan(multisession)` or a socket
#' cluster instead.
#'
#' @return `latex_grob()` returns a grob of class `"latexgrob"`;
#'   `grid.latex()` draws it and returns it invisibly.
#' @seealso [latex_dims()], [latex_options()], [markdown_grob()],
#'   [geom_latex()]
#' @export
#'
#' @examples
#' \donttest{
#'   grid::grid.newpage()
#'   grid.latex(r"($x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}$)",
#'              y = 0.8, gp = grid::gpar(fontsize = 24))
#'
#'   # Colour, and a rotated grob
#'   g <- latex_grob(r"($\colorbox{BurntOrange}{x^{2}} + y^{2}$)",
#'                   x = 0.3, y = 0.4, rot = 45)
#'   grid::grid.draw(g)
#'   grid.latex(r"($\textcolor{red}{x^{2}} + y^{2} = z^{2}$)",
#'              x = 0.7, y = 0.4, gp = grid::gpar(col = "grey30"))
#'
#'   # A document body, wrapped at 250 points
#'   grid::grid.newpage()
#'   doc <- r"(\section{Results}
#' The fitted line is
#' \[ \hat{y} = \beta_0 + \beta_1 x, \]
#' and its slope, $\beta_1$, is positive.)"
#'   grid.latex(doc, input_mode = "document", max_width = 250,
#'              x = 0.05, y = 0.95, hjust = 0, vjust = 1,
#'              gp = grid::gpar(fontsize = 12))
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
                       input_mode = c("mixed", "math", "document"),
                       render_mode = c("typeface", "path"),
                       justify = FALSE,
                       line_break = c("greedy", "optimal"),
                       debug = FALSE,
                       name = NULL,
                       gp = grid::gpar()) {

  # Whether typeface was actually asked for, rather than inherited as the
  # default. Captured before .apply_opts(), which assigns the option value
  # into `render_mode` and would make missing() FALSE from then on.
  render_mode_explicit <- !missing(render_mode) || !is.null(.opt("render_mode"))

  .apply_opts("math_font", "render_mode", "tex_style", "input_mode",
              "justify", "line_break")
  render_mode <- match.arg(render_mode)
  input_mode <- match.arg(input_mode)
  line_break <- match.arg(line_break)
  .check_justify(justify)

  parsed <- .parse_from_gp(
    tex = tex, gp = gp, math_font = math_font, max_width = max_width,
    tex_style = tex_style, render_mode = render_mode,
    input_mode = input_mode,
    with_path_fallback = TRUE, justify = justify, line_break = line_break
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
    justify = justify,
    line_break = line_break,
    text_gp = parsed$text_gp,
    render_mode = parsed$render_mode,
    render_mode_explicit = render_mode_explicit,
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

#' Position of a named point in a LaTeX grob
#'
#' Returns the position of a `\mark{name}` written in the LaTeX, as grid
#' units that can be passed straight to other grid functions, for example
#' to point an arrow at part of a formula.
#'
#' @param grob A grob from [latex_grob()].
#' @param name The name given to `\mark{}`.
#' @return A list with `x` and `y`, each a [grid::unit()]. Rotation
#'   (`rot`) is not taken into account.
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

.single_just <- function(x, arg) {
  if (length(x) != 1L || !is.finite(x)) {
    stop(arg, " must be a numeric or a single string.", call. = FALSE)
  }
  x
}

.resolve_hjust <- function(hjust) {
  if (is.numeric(hjust)) return(.single_just(hjust, "hjust"))
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
  if (is.numeric(vjust)) return(.single_just(vjust, "vjust"))
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
# Resolves fontsize/cex/lineheight/fontfamily/col out of `gp`,
# runs MicroTeX parse via the cache, optionally also runs a path-mode
# parse for device-fallback. Returns the layout and the stripped-down
# `gp` safe to attach to child grobs (fontsize/cex/lineheight removed
# so they don't re-scale at draw time).
.parse_from_gp <- function(tex, gp, math_font, max_width, tex_style,
                           render_mode, input_mode = "mixed",
                           with_path_fallback = FALSE, justify = FALSE,
                           line_break = "greedy") {
  # Patterns are registered on first use, not at load.
  .ensure_bundled_fonts_registered()
  .check_tex_style(tex_style)
  input_mode <- match.arg(input_mode, c("math", "mixed", "document"))
  if (!is.numeric(max_width) || length(max_width) != 1L || is.na(max_width) ||
      max_width < 0) {
    stop("max_width must be a single non-negative number.", call. = FALSE)
  }

  # Font size is needed before anything else now, because `em`/`ex` in an
  # \includegraphics option resolve against it.
  fontsize <- gp$fontsize %||% 20
  if (!is.null(gp$cex)) fontsize <- fontsize * gp$cex

  # The parser reads UTF-8 bytes; a Latin-1 string would reach it as
  # invalid ones.
  tex <- enc2utf8(tex)
  math_font <- resolve_math_font(math_font)

  fg_color <- if (!is.null(gp$col)) {
    # parse_latex_cpp takes a single colour; use the first if a vector
    # slipped through gpar().
    rgba <- grDevices::col2rgb(gp$col[[1]], alpha = TRUE)[, 1]
    if (rgba[["alpha"]] >= 255L) {
      sprintf("#%02X%02X%02X", rgba[["red"]], rgba[["green"]], rgba[["blue"]])
    } else {
      # MicroTeX's decodeColor() reads 9-char hex as #AARRGGBB, not the
      # #RRGGBBAA that grDevices::rgb() would emit.
      sprintf("#%02X%02X%02X%02X", rgba[["alpha"]],
              rgba[["red"]], rgba[["green"]], rgba[["blue"]])
    }
  } else {
    "#000000"
  }

  # Grid semantics: gp$fontsize is in points, gp$cex multiplies it,
  # gp$lineheight is total-line-height multiplier. Bake these into the
  # parse call (layout depends on them), then strip from gp so they
  # don't re-apply at draw time. `fontsize` itself is resolved at the top
  # of this function, since the image resolver needs it.
  line_space <- .line_space_from_lineheight(gp$lineheight, fontsize)
  gp$fontsize <- NULL
  gp$cex <- NULL
  gp$lineheight <- NULL

  # Only fontfamily matters for \text{} blocks: bold/italic runs come from
  # the LaTeX source (\textbf, \textit, ...) as per-record font_style, so a
  # gpar()-level fontface is not consulted.
  text_gp <- grid::gpar()
  if (!is.null(gp$fontfamily)) text_gp$fontfamily <- gp$fontfamily

  main_font <- .resolve_text_font(text_gp$fontfamily %||% "sans")

  measurer <- .make_text_measurer(text_gp)
  register_text_measurer(measurer)
  on.exit(clear_text_measurer(), add = TRUE)

  text_family <- text_gp$fontfamily %||% ""

  layout <- .parse_latex_cached(
    tex = tex, text_size = fontsize, line_space = line_space,
    fg_color = fg_color, max_width = max_width, math_font = math_font,
    main_font = main_font, use_path = (render_mode == "path"),
    tex_style = tex_style, text_family = text_family, justify = justify,
    optimal_break = identical(line_break, "optimal"), input_mode = input_mode
  )
  # Read off the layout rather than the parse, so a cached one says it too.
  .warn_diagnostics(attr(layout, "diagnostics"))

  path_layout <- NULL
  if (with_path_fallback && render_mode == "typeface") {
    path_layout <- .parse_latex_cached(
      tex = tex, text_size = fontsize, line_space = line_space,
      fg_color = fg_color, max_width = max_width, math_font = math_font,
      main_font = main_font, use_path = TRUE, tex_style = tex_style,
      text_family = text_family, justify = justify,
      optimal_break = identical(line_break, "optimal"), input_mode = input_mode
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

# What the parser recovered from -- it drew the rest -- as one warning,
# each problem at its line:col in the string the parser was given.
.warn_diagnostics <- function(d) {
  if (is.null(d) || !NROW(d)) return(invisible(NULL))
  d <- d[order(d$line, d$col), , drop = FALSE]
  lines <- sprintf("%d:%d: %s", d$line, d$col, d$message)
  dropped <- attr(d, "dropped") %||% 0
  if (dropped > 0) lines <- c(lines, sprintf("... and %d more", as.integer(dropped)))
  warning(
    "LaTeX input: ",
    if (length(lines) == 1L) lines else paste0("\n  ", lines, collapse = ""),
    call. = FALSE
  )
}


# Check whether the current graphics device can set glyphGrob objects as
# text via the dev->glyph() graphics engine interface (R >= 4.3). Uses
# dev.capabilities()$glyphs when a device is open; returns TRUE when no
# device is open (layout-only / measurement context).
#
# Base pdf() and postscript() are refused by name. pdf() does report
# glyphs, but it names the font in the file without embedding it, so the
# math is garbled in any viewer that lacks the font -- nearly all of them,
# as the math fonts are bundled rather than installed ("FFN(x)" read as
# "DDL&..."). Outlines draw right everywhere; cairo_pdf() embeds.
.device_supports_typeface_glyphs <- function() {
  cur <- grDevices::dev.cur()
  if (cur == 1L) return(TRUE)  # null device (no drawing)
  if (names(cur) %in% c("pdf", "postscript")) return(FALSE)

  caps <- grDevices::dev.capabilities()
  isTRUE(caps[["glyphs"]])
}

# Devices already told about the typeface fallback, keyed by device number
# and name. Whether glyphs can be set is a property of the *device*, not of
# the grob, but makeContent() runs per grob and on every redraw -- warning
# there flooded a figure holding several labels with the same message.
.typeface_noted <- new.env(parent = emptyenv())

.clear_typeface_noted <- function() {
  rm(list = ls(.typeface_noted), envir = .typeface_noted)
  invisible(NULL)
}

# Tell the user at most once per device. Returns TRUE if it did.
.note_typeface_fallback_once <- function(explicit) {
  # Only tell someone who asked for typeface and did not get it. Reporting
  # the default would reach every user on every unsupported device, and
  # they never expressed a preference. This also needs no list of which
  # devices are vector -- there is no capability to test for that.
  if (!isTRUE(explicit)) return(invisible(FALSE))
  cur <- grDevices::dev.cur()
  key <- paste0(cur, ":", names(cur))
  if (!is.null(.typeface_noted[[key]])) return(invisible(FALSE))
  .typeface_noted[[key]] <- TRUE
  # A message, not a warning: nothing is wrong and the package has already
  # done the sensible thing. A warning would also be escalated to an error
  # under options(warn = 2), which some CI setups use.
  message(
    "Current graphics device cannot set the math font as text; falling ",
    "back to path mode, so math is drawn as outlines rather than text. ",
    "On a vector device -- svglite::svglite() or grDevices::cairo_pdf() ",
    "-- it stays selectable."
  )
  invisible(TRUE)
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
      .note_typeface_fallback_once(isTRUE(x$render_mode_explicit))
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
                         "input_mode", "render_mode", "justify",
                         "line_break", "gp")

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
      render_mode = x$render_mode, with_path_fallback = TRUE,
      justify = isTRUE(x$justify),
      line_break = x$line_break %||% "greedy"
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

# grid resolves grobx/groby units by pushing this grob's viewport, calling
# xDetails() AND yDetails() with the same theta, and transforming the
# resulting (x, y) location back through the viewport (position, just,
# rotation). The formula's bounding box *is* the viewport, so the boundary
# point is the boundary of the unit square in the pushed context — exactly
# what grid's own rect method computes (ray from the centre at angle theta,
# intersected with the edges). Delegating keeps us identical to a
# rectGrob drawn in the same viewport; note this means theta is measured
# in the formula's own frame, so for rot != 0 it rotates with the grob.

#' @method xDetails latexgrob
#' @export
xDetails.latexgrob <- function(x, theta) {
  grid::xDetails(grid::rectGrob(), theta)
}

#' @method yDetails latexgrob
#' @export
yDetails.latexgrob <- function(x, theta) {
  grid::yDetails(grid::rectGrob(), theta)
}


#' @param ... Arguments passed to `latex_grob()`.
#' @rdname latex_grob
#' @export
#'
grid.latex <- function(tex, ...) {
  g <- latex_grob(tex, ...)
  grid::grid.draw(g)
  invisible(g)
}

#' Size of a LaTeX expression
#'
#' @inheritParams latex_grob
#' @return A list of grid units in big points:
#'   * `width`, `height`: the size of the bounding box.
#'   * `depth`: how far it extends below the baseline.
#'   * `baseline`: the height of the baseline above the bottom.
#'
#'   And `is_split`: `TRUE` if the text was wrapped over several lines.
#' @export
#'
#' @examples
#' latex_dims(r"($\frac{a}{b}$)")
latex_dims <- function(tex, math_font = "", max_width = 0,
                       tex_style = "",
                       input_mode = c("mixed", "math", "document"),
                       render_mode = c("typeface", "path"),
                       justify = FALSE,
                       line_break = c("greedy", "optimal"),
                       gp = grid::gpar()) {
  .apply_opts("math_font", "render_mode", "tex_style", "input_mode",
              "justify", "line_break")
  render_mode <- match.arg(render_mode)
  input_mode <- match.arg(input_mode)
  line_break <- match.arg(line_break)
  .check_justify(justify)

  parsed <- .parse_from_gp(
    tex = tex, gp = gp, math_font = math_font, max_width = max_width,
    tex_style = tex_style, render_mode = render_mode,
    input_mode = input_mode, justify = justify, line_break = line_break
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


# The measurer's grob, carrying the measurement gp and, per call, the label.
#
# grid evaluates a grob's size with the device locked (R >= 4.6), and an
# error that unwinds out of that evaluation leaves the lock on: R's
# eval_with_gd() sets the hook that lifts it before begincontext(), which
# clears it. Every later dev.off() of the device then warns "Killing locked
# device". pdf() raises such errors for text outside its encoding under
# R CMD check --as-cran (_R_CHECK_MBCS_CONVERSION_FAILURE_), so a textGrob
# measured in tryCatch() left 15 of them in a check's test log. These
# methods ask the device through string units instead, which grid
# evaluates in C, and catch a failure before it leaves grid's evaluation:
# what the device cannot measure reads as NA.
.measure_extent <- function(size, convert, label) {
  grid::unit(tryCatch(convert(size(label), "bigpts", valueOnly = TRUE),
                      error = function(e) NA_real_), "bigpts")
}

#' @method widthDetails gridmicrotex_measure
#' @export
widthDetails.gridmicrotex_measure <- function(x) {
  .measure_extent(grid::stringWidth, grid::convertWidth, x$label)
}

#' @method heightDetails gridmicrotex_measure
#' @export
heightDetails.gridmicrotex_measure <- function(x) {
  .measure_extent(grid::stringHeight, grid::convertHeight, x$label)
}

#' @method ascentDetails gridmicrotex_measure
#' @export
ascentDetails.gridmicrotex_measure <- function(x) {
  .measure_extent(grid::stringAscent, grid::convertHeight, x$label)
}

#' @method descentDetails gridmicrotex_measure
#' @export
descentDetails.gridmicrotex_measure <- function(x) {
  .measure_extent(grid::stringDescent, grid::convertHeight, x$label)
}

# The whole string's width, or where the device cannot measure it (pdf()
# for CJK text on some Windows locales, or outside its encoding under
# --as-cran), a simple estimate that keeps the layout flowing. `tg` is the
# measurer's grob; `em` is the font size the measurement runs at (the
# measurer's ref_size).
.measure_text_bigpts <- function(tg, text, em = 72) {
  out <- grid::convertWidth(grid::grobWidth(tg), "bigpts", valueOnly = TRUE)
  if (!is.na(out)) return(as.numeric(out))
  w <- tryCatch(base::nchar(text, type = "width"), error = function(...) NA_real_)
  if (is.na(w)) {
    w <- base::nchar(text, type = "chars")
  }
  # Half an em per terminal width cell: narrow chars ~0.5 em, CJK
  # (2 cells) ~1 em — matching the C++ heuristic in src/init.cpp.
  as.numeric(w) * 0.5 * em
}


#' Create a text measurement closure for MicroTeX layout
#'
#' Returns a function that measures text using R's grid graphics system.
#' The closure is called from C++ during \code{parse_latex_cpp()} to get
#' accurate font metrics for \code{\\text\{\}} blocks.
#'
#' @param text_gp A \code{\link[grid]{gpar}} object whose
#'   \code{fontfamily} is used for measurement. The face comes from
#'   MicroTeX's per-run \code{font_style}, not from \code{text_gp}.
#' @return A function taking \code{(text, font_style)} that returns
#'   \code{c(width_ratio, ascent_ratio, height_ratio)} where ratios
#'   are relative to the font size.
#' @noRd
.make_text_measurer <- function(text_gp) {
  ref_size <- 72  # reference size in points for measurement precision

  # Cache the R version check
  has_ascent_fn <- getRversion() >= "4.4.0"

  # Per-closure cache keyed on (font_style, font_family, text). Lifetime =
  # one parse (the closure is created fresh per parse in
  # latex_grob/latex_dims), so graphics state can't drift between calls.
  # Hits avoid a textGrob construction + grid::convertHeight round trip per
  # repeated span.
  cache <- new.env(parent = emptyenv())

  # A layout measures each distinct word of its prose once (the engine
  # caches the rest), so a paper's vocabulary is some two thousand calls
  # and each one's cost is what a long document waits on. What does not
  # depend on the word is kept per font (style x family): the gpar, a grob
  # to carry it, and each character's ascent and descent.
  fonts <- new.env(parent = emptyenv())
  font_for <- function(style, family) {
    fkey <- paste0(style, "\x1f", family)
    f <- fonts[[fkey]]
    if (!is.null(f)) return(f)
    gp <- grid::gpar(fontsize = ref_size, fontface = .resolve_text_face(style))
    fam <- .resolve_text_family(style, text_gp$fontfamily, family)
    if (!is.null(fam)) {
      gp$fontfamily <- fam
    }
    f <- new.env(parent = emptyenv())
    # Carry the font settings on a throwaway grob rather than pushing a
    # viewport. pushViewport() writes to the device's display list (6
    # records per push/pop), which makes a caller's device look like it
    # holds a plot: knitr then snapshots that page as a spurious blank
    # figure before the real plot's grid.newpage(). grob* queries below
    # only read metrics, through the size methods of its class
    # (widthDetails.gridmicrotex_measure). Its label is set per call.
    f$grob <- grid::grob(label = "", gp = gp, cl = "gridmicrotex_measure")
    f$chars <- new.env(parent = emptyenv())
    fonts[[fkey]] <- f
    f
  }
  if (has_ascent_fn) {
    grob_ascent <- get("grobAscent", envir = asNamespace("grid"))
    grob_descent <- get("grobDescent", envir = asNamespace("grid"))
  }
  # A one-line string's ascent and descent are the largest of its
  # characters' -- the rule R's own GEStrMetric() applies -- so each
  # character is measured once per font and the word's are read off.
  # NULL when the device cannot answer, and the caller measures the whole
  # string instead.
  char_extent <- function(f, text) {
    # By code point, not strsplit(): the engine hands over its text marked
    # "unknown", and on Windows strsplit() then cut a character outside the
    # BMP (an emoji, a mathematical script letter) into an extra, empty
    # piece.
    cps <- utf8ToInt(text)
    if (anyNA(cps)) return(NULL)
    cps <- unique(cps)
    if (!length(cps)) return(c(0, 0))
    keys <- as.character(cps)
    ext <- matrix(NA_real_, 2L, length(cps))
    for (i in seq_along(cps)) {
      ad <- f$chars[[keys[i]]]
      if (is.null(ad)) {
        tg <- f$grob
        tg$label <- intToUtf8(cps[i])
        ad <- c(
          grid::convertHeight(grob_ascent(tg), "bigpts", valueOnly = TRUE),
          grid::convertHeight(grob_descent(tg), "bigpts", valueOnly = TRUE)
        )
        if (anyNA(ad)) return(NULL)
        f$chars[[keys[i]]] <- ad
      }
      ext[, i] <- ad
    }
    c(max(ext[1, ]), max(ext[2, ]))
  }

  measure <- function(text, font_style, font_family) {
    key <- paste0(as.integer(font_style), "\x1f", font_family, "\x1f", text)
    # The key becomes a variable name, which R caps at 10000 bytes, so a
    # long run is measured every time rather than cached.
    cacheable <- nchar(key, type = "bytes") <= 2048L
    hit <- if (cacheable) cache[[key]]
    if (!is.null(hit)) return(hit)

    # MicroTeX probes C-style escapes while tokenising \text{} content
    # (e.g. the leading "\f" of \frac arrives here as a form feed before
    # the parser backtracks to the real command). Control characters have
    # no metrics on some devices (grid warns, e.g. cp1252 pdf()) and the
    # probe never reaches the final layout — answer without touching the
    # device.
    if (grepl("^[[:cntrl:]]*$", text)) {
      result <- c(0, 0.8, 1)
      if (cacheable) cache[[key]] <- result
      return(result)
    }

    # Ensure a graphics device is available for measurement. A parse opens
    # one for its whole length (.parse_latex_cached()), so this is a
    # safety net.
    needs_dev <- grDevices::dev.cur() == 1L
    if (needs_dev) {
      grDevices::pdf(NULL)
      on.exit(grDevices::dev.off(), add = TRUE)
    }

    f <- font_for(as.integer(font_style), font_family)
    tg <- f$grob
    tg$label <- text

    # Measuring is per *character*, so a device that cannot resolve the
    # family -- base pdf() has no named families, only what pdfFonts()
    # declares -- would warn dozens of times for one label. Stay quiet
    # here: the same device warns again when the text is actually drawn,
    # which is the once-per-run, user-actionable copy of the message.
    # The width is the whole string's, which kerning makes other than
    # the sum of its characters'.
    w <- suppressWarnings(.measure_text_bigpts(tg, text, em = ref_size))

    # Measure ascent and descent
    ad <- suppressWarnings(if (has_ascent_fn) {
      # Characters, except in a string of several lines, where
      # GEStrMetric() reads the first line's ascent and the last's
      # descent instead.
      per_char <- if (!grepl("\n", text, fixed = TRUE)) char_extent(f, text)
      per_char %||% c(
        grid::convertHeight(grob_ascent(tg), "bigpts", valueOnly = TRUE),
        grid::convertHeight(grob_descent(tg), "bigpts", valueOnly = TRUE)
      )
    } else {
      h <- grid::convertHeight(grid::grobHeight(tg), "bigpts", valueOnly = TRUE)
      asc <- h * 0.8
      c(asc, h - asc)
    })
    # Where the device cannot measure, approximate: 80% of the font size
    # for ascent, 20% for descent.
    if (anyNA(ad)) ad <- c(ref_size * 0.8, ref_size * 0.2)
    asc  <- ad[1]
    desc <- ad[2]

    result <- c(w / ref_size, asc / ref_size, (asc + desc) / ref_size)
    if (cacheable) cache[[key]] <- result
    result
  }

  # `font_family` is the family named by a \gmfontfamily span, passed in by
  # TextLayout_R. Defaulted, so the two-argument calls that predate it --
  # including register_text_measurer() users -- keep working.
  #
  # An error becomes an empty result, which TextLayout_R::getBounds()
  # answers with its own width estimate. It has to be caught here: Rcpp
  # hands an R error to C++ as the same jump as an interrupt, and
  # getBounds() lets jumps through so that Ctrl-C stops the layout.
  function(text, font_style, font_family = "") {
    tryCatch(measure(text, font_style, font_family),
             error = function(e) numeric(0))
  }
}
