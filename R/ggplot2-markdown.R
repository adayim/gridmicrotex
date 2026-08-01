# ggplot2 integration for markdown: geom_markdown() and element_markdown().
#
# These mirror geom_latex() / element_latex() in ggplot2-integration.R and
# reuse the same machinery -- the S7 subclass-of-element_text pattern, the
# legacy class re-injection, .rotate_just() and .pick_unit(). Only the grob
# constructor differs: markdown_grob() instead of latex_grob().
#
# Two differences from the LaTeX versions are deliberate:
#
#   input_mode is absent. Markdown decides for itself what is prose and
#   what is math; the walker emits \text{}-wrapped LaTeX either way.
#
#   Enclosing $...$ is never stripped. element_latex() strips it in math
#   mode, where users add it by analogy with plotmath. In markdown a `$`
#   pair *is* the math delimiter, so removing it would destroy the label.

# --------------------------------------------------------------------------
# geom_markdown()
# --------------------------------------------------------------------------

#' A ggplot2 geom for markdown labels
#'
#' Renders markdown labels -- \code{**bold**}, \code{*italic*},
#' \code{`code`}, \code{~~strike~~} and \code{$math$} -- as native grid
#' grobs inside a plot. The markdown is converted to LaTeX and laid out by
#' MicroTeX, so the output is resolution-independent vector graphics.
#'
#' @section Aesthetics:
#' \code{geom_markdown()} understands the following aesthetics (required
#' aesthetics are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \strong{\code{label}} --- markdown string
#'   \item \code{size} --- font size in points (default: 11)
#'   \item \code{colour} --- text colour (default: \code{"black"})
#'   \item \code{angle} --- rotation angle in degrees (default: 0)
#'   \item \code{hjust} --- horizontal justification, 0-1 (default: 0.5)
#'   \item \code{vjust} --- vertical justification, 0-1 (default: 0.5)
#'   \item \code{alpha} --- transparency (default: 1)
#' }
#'
#' @inheritParams ggplot2::layer
#' @inheritParams markdown_grob
#' @param fontsize Default font size in points. Overridden by the
#'   \code{size} aesthetic if mapped.
#' @param math_font Name of the math font to use (e.g. \code{"stix"}).
#' @param lineheight Multi-line height multiplier (default 1.2), matching
#'   \code{grid::gpar()} semantics.
#' @param max_width Maximum width in big points for automatic line
#'   wrapping (default: 0, no wrapping).
#' @param render_mode \code{"typeface"} (default) or \code{"path"}; see
#'   \code{\link{latex_grob}}.
#' @param justify Logical; justify wrapped lines. Requires
#'   \code{max_width}. See \code{\link{latex_grob}}.
#' @param style A \code{\link{markdown_style}} object, CSS text, or the
#'   path to a \code{.css} file, applied to every label this layer draws.
#'   \code{NULL} falls back to
#'   \code{\link{latex_options}(markdown_style = )}. Only the properties
#'   that compile to LaTeX apply here --- a label has no block layout, so
#'   margins and padding are ignored. See \code{\link{md_style}}.
#' @param na.rm If \code{FALSE}, the default, missing values are removed
#'   with a warning. If \code{TRUE}, they are removed silently.
#' @param ... Other arguments passed to \code{\link[ggplot2]{layer}}.
#'
#' @return A ggplot2 layer.
#' @seealso \code{\link{markdown_grob}}, \code{\link{geom_latex}}
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   df <- data.frame(
#'     x = 1:3, y = 1:3,
#'     lab = c("**bold**", "*slope* $\\beta_1$", "`code` and $x^2$")
#'   )
#'   ggplot(df, aes(x, y, label = lab)) + geom_markdown()
#' }
#' }
geom_markdown <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", ...,
                          fontsize = 11, math_font = "",
                          lineheight = 1.2, max_width = 0,
                          render_mode = c("typeface", "path"),
                          justify = FALSE, style = NULL,
                          na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE) {
  .apply_opts("math_font", "render_mode", "justify")
  render_mode <- match.arg(render_mode)
  .check_justify(justify)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for geom_markdown(). ",
         "Please install it with install.packages('ggplot2').",
         call. = FALSE)
  }

  ggplot2::layer(
    geom = GeomMarkdown,
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
      render_mode = render_mode,
      justify = justify,
      style = style,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_markdown
#' @format NULL
#' @usage NULL
#' @export
GeomMarkdown <- NULL

# --------------------------------------------------------------------------
# element_markdown()
# --------------------------------------------------------------------------

#' A ggplot2 theme element for markdown text
#'
#' Use as a theme element for axis titles, axis labels, plot titles or any
#' other text element. The label is parsed as markdown --- including
#' \code{$...$} math --- and rendered via MicroTeX.
#'
#' @details
#' This is an S7 subclass of \code{ggplot2::element_text}, so it inherits
#' the standard text properties (size, colour, hjust, ...) from the theme
#' and merges correctly with inherited theme entries.
#'
#' @section Block labels:
#' A label with real block structure --- a heading, a list, a table, a
#' rule, or more than one paragraph --- is laid out by
#' \code{\link{markdown_box_grob}} rather than flattened into one run, so
#' list markers, indents and block spacing survive. That makes a title
#' like this work:
#'
#' \preformatted{labs(title = "## Findings\\n\\n- slope $\\\\beta_1$\\n- *p* < 0.001")}
#'
#' The box's own background, border, padding and corner radius come from
#' the stylesheet's \code{body} rule --- \code{style = "body \{ background:
#' grey95; padding: 8px \}"} --- not from arguments here. See
#' \code{\link{markdown_style}}.
#'
#' Three details follow from how ggplot2 measures theme elements:
#'
#' \itemize{
#'   \item \strong{Wrapping is opt-in.} Without \code{width} the label is
#'     sized to its content, because ggplot2 asks an element for its
#'     height before placing it, when a relative width would resolve
#'     against the whole device rather than the element's cell.
#'   \item \strong{Axis tick labels are never laid out as blocks},
#'     whatever they contain, for the same reason.
#'   \item \strong{A rotated label is never laid out as blocks.} The box
#'     cannot rotate, so a label with both blocks and a non-zero
#'     \code{angle} keeps the angle, is rendered as a single run, and
#'     warns. A \code{width} given with an angle becomes the run's
#'     wrapping measure instead.
#' }
#'
#' \code{math_font}, \code{render_mode} and \code{justify} are not
#' forwarded to the box; a block label takes those from
#' \code{\link{latex_options}}.
#'
#' Note that \pkg{ggtext} also exports a function called
#' \code{element_markdown()}. If both packages are attached, the one loaded
#' later wins; call \code{gridmicrotex::element_markdown()} explicitly to
#' be unambiguous. The two are not interchangeable --- ggtext renders
#' HTML/CSS and images, this renders LaTeX math.
#'
#' @param math_font Name of the math font to use (e.g. \code{"stix"}).
#' @param fontsize Convenience alias for \code{size}; forwarded to
#'   \code{ggplot2::element_text()} as the text size in points.
#'   \code{NULL} (default) uses the theme's inherited size.
#' @param lineheight Multi-line height multiplier (default 1.2).
#' @param max_width Maximum width in big points for automatic line
#'   wrapping (default: 0, no wrapping).
#' @param render_mode \code{"typeface"} (default) or \code{"path"}.
#' @param justify Logical; justify wrapped lines. Requires
#'   \code{max_width}.
#' @param style A \code{\link{markdown_style}} object, CSS text, or the
#'   path to a \code{.css} file, applied to labels drawn through this
#'   theme element. \code{NA}, the default, means unset --- the global
#'   \code{\link{latex_options}(markdown_style = )} applies instead. Only
#'   the properties that compile to LaTeX have an effect, unless the label
#'   is laid out as blocks (see \emph{Block labels}). See
#'   \code{\link{md_style}}.
#' @param width Wrapping measure for the label, as a
#'   \code{\link[grid]{unit}}. \code{NA}, the default, means unset: the
#'   label is sized to its content and does not wrap.
#'   \code{unit(1, "npc")} is the useful value for a plot title, whose
#'   cell really is the full plot width. See \emph{Block labels}.
#' @param ... Additional arguments passed to
#'   \code{ggplot2::element_text()} (e.g. \code{colour}, \code{hjust}).
#'
#' @return An S7 object of class \code{element_markdown}, inheriting from
#'   \code{ggplot2::element_text}.
#' @seealso \code{\link{markdown_grob}}, \code{\link{element_latex}}
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(mtcars, aes(wt, mpg)) + geom_point() +
#'     labs(x = "**weight** in $10^3$ lbs") +
#'     theme(axis.title.x = gridmicrotex::element_markdown())
#' }
#' }
element_markdown <- function(math_font = "", fontsize = NULL,
                             lineheight = 1.2, max_width = 0,
                             render_mode = c("typeface", "path"),
                             justify = FALSE, style = NA, width = NA, ...) {
  .apply_opts("math_font", "render_mode", "justify")
  render_mode <- match.arg(render_mode)
  .check_justify(justify)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for element_markdown(). ",
         "Please install it with install.packages('ggplot2').",
         call. = FALSE)
  }
  dots <- list(...)
  if (!is.null(fontsize)) dots$size <- fontsize

  obj <- do.call(.element_markdown_class, c(
    list(math_font = math_font, lineheight = lineheight,
         max_width = max_width, render_mode = render_mode,
         justify = justify, style = style, width = width),
    dots
  ))
  # ggplot2's element_text constructor injects legacy "element_text" and
  # "element" S3 strings into the class vector; S7 inheritance loses them,
  # which makes combine_elements treat us as an unrelated sibling and drop
  # us when resolving inherited theme entries (e.g. axis.title.y inheriting
  # from a user-set axis.title). Re-inject them so subclass detection works.
  class(obj) <- union(
    union(
      c("gridmicrotex::element_markdown", "ggplot2::element_text",
        "element_text"),
      class(obj)
    ),
    "element"
  )
  obj
}

# Placeholder -- replaced in .onLoad_ggplot2_markdown() with the real
# S7 constructor.
.element_markdown_class <- NULL

# --------------------------------------------------------------------------
# .onLoad_ggplot2_markdown() -- called from .onLoad_ggplot2()
# --------------------------------------------------------------------------
.onLoad_ggplot2_markdown <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return()

  GeomMarkdown <<- ggplot2::ggproto("GeomMarkdown", ggplot2::Geom,

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

    # default_aes is applied after setup_data, so a missing `size` column
    # here means the aesthetic was not mapped -- fill it from the fontsize
    # parameter. (In draw_panel the column is always populated, so the
    # parameter could never win there.)
    setup_data = function(data, params) {
      if (is.null(data$size)) {
        data$size <- params$fontsize %||% 11
      }
      data
    },

    draw_panel = function(data, panel_params, coord, fontsize = 11,
                          math_font = "", lineheight = 1.2, max_width = 0,
                          render_mode = "typeface", justify = FALSE,
                          style = NULL, na.rm = FALSE) {
      coords <- coord$transform(data, panel_params)

      grobs <- lapply(seq_len(nrow(coords)), function(i) {
        row <- coords[i, ]

        if (is.na(row$label) || !nzchar(row$label)) return(grid::nullGrob())

        col <- if (!is.null(row$alpha) && row$alpha < 1) {
          grDevices::adjustcolor(row$colour, alpha.f = row$alpha)
        } else {
          row$colour
        }

        markdown_grob(
          md = row$label,
          x = grid::unit(row$x, "npc"),
          y = grid::unit(row$y, "npc"),
          hjust = row$hjust,
          vjust = row$vjust,
          rot = row$angle %||% 0,
          math_font = math_font,
          max_width = max_width,
          render_mode = render_mode,
          justify = justify,
          style = style,
          gp = grid::gpar(col = col, fontsize = row$size,
                          lineheight = lineheight)
        )
      })

      do.call(grid::gList, grobs)
    }
  )

  .element_markdown_class <<- S7::new_class(
    "element_markdown",
    parent = ggplot2::element_text,
    properties = list(
      math_font = S7::new_property(S7::class_character, default = ""),
      lineheight = S7::new_property(S7::class_numeric, default = 1.2),
      max_width = S7::new_property(S7::class_numeric, default = 0),
      render_mode = S7::new_property(S7::class_character,
                                     default = "typeface"),
      justify = S7::new_property(S7::class_logical, default = FALSE),
      # A grid unit, or NA for "unset" -- see the note on `style` below
      # for why no property here may default to NULL.
      width = S7::new_property(S7::class_any, default = NA),
      # A markdown_style object, CSS text or a .css path -- whatever
      # markdown_grob(style =) takes, hence class_any.
      #
      # The default is NA, not NULL: ggplot2's theme merge reads an unset
      # property from the parent element, and a plain element_text has no
      # @style, so a NULL default fails with "Can't find property
      # <ggplot2::element_text>@style" as soon as the element is inherited
      # into. Every other property here has a non-NULL typed default for
      # the same reason. .element_grob_markdown() maps NA back to NULL,
      # which is what means "fall back to latex_options(markdown_style=)".
      style = S7::new_property(S7::class_any, default = NA)
    )
  )

  # element_grob is still an S3 generic in ggplot2, so register via S3.
  # S7 objects dispatch correctly thanks to their class vector.
  registerS3method(
    "element_grob", "gridmicrotex::element_markdown",
    .element_grob_markdown,
    envir = asNamespace("ggplot2")
  )
}

# element_grob method for element_markdown. Signature mirrors
# ggplot2:::element_grob.element_text so axes and guides can override
# element properties per call.
.element_grob_markdown <- function(element, label = "", x = NULL, y = NULL,
                                   family = NULL, face = NULL, colour = NULL,
                                   size = NULL, hjust = NULL, vjust = NULL,
                                   angle = NULL, lineheight = NULL,
                                   margin = NULL, margin_x = FALSE,
                                   margin_y = FALSE, ...) {
  if (is.null(label) ||
      (length(label) == 1L && (!nzchar(label) || is.na(label)))) {
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
  render_mode <- element@render_mode %||% "typeface"
  justify     <- element@justify     %||% FALSE
  # NA is the element's "unset" (see the property definition); markdown_grob
  # wants NULL for that, so it can fall back to the global option.
  unset <- function(v) is.atomic(v) && length(v) == 1L && is.na(v)
  style       <- element@style
  if (unset(style)) style <- NULL
  width       <- element@width
  if (unset(width)) width <- NULL
  margin      <- margin %||% element@margin

  gp <- grid::gpar(col = colour, fontsize = fontsize, lineheight = lineheight)
  if (!is.null(family) && nzchar(family)) gp$fontfamily <- family
  if (!is.null(face)   && nzchar(face))   gp$fontface   <- face

  # No $-stripping here, unlike element_latex(): in markdown a `$` pair is
  # the math delimiter, so removing it would destroy the label.

  # When x/y aren't supplied, anchor at the rotation-adjusted just so the
  # rotated text lands where ggplot2:::titleGrob would put it (e.g. for
  # axis.title.y with angle = 90, vjust = 1).
  default_just <- .rotate_just(angle, hjust, vjust)

  if (length(label) == 1L) {
    # A label with real block structure -- a heading, a list, a table, more
    # than one paragraph -- cannot be laid out as a single run: the run
    # path flattens it and the list markers and indents are lost. Fall
    # through to the box, which is the only thing here that does blocks.
    #
    # Tick labels never take this path: the multi-label branch below is
    # left alone deliberately, because a box sized in npc would measure
    # against the whole device (ggplot2 has not placed the element in its
    # cell when it asks for the height) and every tick would claim it.
    promote <- .md_needs_blocks(label)

    if (promote && angle %% 360 != 0) {
      # The box has no rotation, so one of the two has to give. Keeping
      # the angle and flattening the blocks is much the smaller loss: a
      # rotated axis title laid out horizontally would cross the panel.
      warning("A rotated `element_markdown()` label cannot use block ",
              "layout; rendering it as a single run. Set `angle = 0` to ",
              "lay the blocks out.", call. = FALSE)
      promote <- FALSE
    }
    if (!is.null(width) && angle %% 360 != 0) {
      # Still honour the requested measure, as the run path's own.
      max_width <- grid::convertWidth(width, "bigpts", valueOnly = TRUE)
      width <- NULL
    }

    if (promote || !is.null(width)) {
      return(markdown_box_grob(
        md = label,
        x = x %||% grid::unit(hjust, "npc"),
        y = y %||% grid::unit(vjust, "npc"),
        # NULL sizes the box to its content, so nothing wraps and the
        # reported height does not depend on a width that is not yet known.
        width = width,
        hjust = hjust, vjust = vjust,
        # hjust positions a content-sized box; halign is what aligns the
        # blocks when the box spans its cell. Both are wanted.
        halign = hjust,
        margin = if (grid::is.unit(margin) && length(margin) == 4L) margin,
        style = style, gp = gp
      ))
    }

    if (is.null(x)) x <- grid::unit(default_just$hjust, "npc")
    if (is.null(y)) y <- grid::unit(default_just$vjust, "npc")
    return(markdown_grob(
      md = label, x = x, y = y,
      hjust = hjust, vjust = vjust, rot = angle,
      math_font = math_font, max_width = max_width,
      render_mode = render_mode, justify = justify, style = style,
      gp = gp
    ))
  }

  # Multiple labels (axis tick labels) -- render a gTree of grobs.
  n <- length(label)
  if (is.null(x)) x <- grid::unit(rep(default_just$hjust, n), "npc")
  if (is.null(y)) y <- grid::unit(rep(default_just$vjust, n), "npc")

  grobs <- grid::gList()
  for (i in seq_len(n)) {
    lab <- label[i]
    if (is.na(lab) || !nzchar(lab)) next
    grobs <- grid::gList(grobs, markdown_grob(
      md = lab, x = .pick_unit(x, i), y = .pick_unit(y, i),
      hjust = hjust, vjust = vjust, rot = angle,
      math_font = math_font, max_width = max_width,
      render_mode = render_mode, justify = justify, style = style,
      gp = gp,
      name = paste0("ticklabel.", i)
    ))
  }
  # Same class as the LaTeX tick labels, so the width/height methods that
  # let ggplot2 reserve room for the axis strip apply here too.
  grid::gTree(children = grobs, name = "axis.markdown.labels",
              cl = "gridmicrotex_labels")
}
