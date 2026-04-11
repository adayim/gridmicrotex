#' Build grid children from a MicroTeX layout data.frame
#'
#' Converts each row of the layout data.frame into the appropriate
#' grid grob (pathGrob, segmentsGrob, rectGrob, textGrob, glyphGrob).
#'
#' @param layout_df Data.frame returned by \code{parse_latex_cpp()}.
#' @param total_h Total height of the formula (height + depth) in bigpts.
#' @param depth Depth below the baseline in bigpts (default 0).
#' @param text_gp Optional \code{\link[grid]{gpar}} for text grobs
#'   (from \code{\\text\{\}} blocks). Controls fontfamily and fontface.
#' @param render_mode Character string: \code{"path"} or \code{"typeface"}.
#'   In typeface mode, glyph records are rendered via \code{glyphGrob}.
#' @return A \code{grid::gList} of child grobs.
#' @keywords internal
build_latex_children <- function(layout_df, total_h, depth = 0,
                                 text_gp = NULL,
                                 render_mode = "typeface") {
  children <- grid::gList()
  n <- nrow(layout_df)
  if (n == 0) return(children)

  # MicroTeX line widths are in bigpts (1/72 inch) but R's lwd unit is
  # 1/96 inch, so scale by 96/72 = 4/3 to get correct stroke widths.
  lwd_scale <- 96 / 72

  # Collect glyph records for batched glyphGrob creation (typeface mode)
  glyph_ids <- integer(0)
  glyph_x <- numeric(0)
  glyph_y <- numeric(0)
  glyph_sizes <- numeric(0)
  glyph_cols <- character(0)
  glyph_fonts <- character(0)

  for (i in seq_len(n)) {
    row <- layout_df[i, ]

    switch(row$type,
      "path" = {
        path_data <- layout_df$path[[i]]
        if (!is.null(path_data)) {
          grob <- build_path_grob(path_data, row$color, i, total_h)
          if (!is.null(grob)) {
            children <- grid::gList(children, grob)
          }
        }
      },
      "line" = {
        # MicroTeX draws all TeX rules (fraction bars, \hline, vertical
        # borders) as horizontal strokes via drawLine with large stroke
        # width.  Render horizontal lines as filled rectGrobs so that
        # dimensions stay in bigpts and avoid lwd-unit issues.
        if (abs(row$y - row$y2) < 0.001) {
          # Horizontal line → filled rect (covers hline, vline rules,
          # fraction bars, overlines, underlines)
          lw <- row$lwd
          children <- grid::gList(children, grid::rectGrob(
            x = grid::unit(row$x, "bigpts"),
            y = grid::unit(total_h - row$y, "bigpts"),
            width = grid::unit(row$x2 - row$x, "bigpts"),
            height = grid::unit(lw, "bigpts"),
            just = c("left", "centre"),
            gp = grid::gpar(fill = row$color, col = NA),
            name = paste0("rule.", i)
          ))
        } else {
          # Non-horizontal line (e.g. \cancel diagonals)
          children <- grid::gList(children, grid::segmentsGrob(
            x0 = grid::unit(row$x, "bigpts"),
            y0 = grid::unit(total_h - row$y, "bigpts"),
            x1 = grid::unit(row$x2, "bigpts"),
            y1 = grid::unit(total_h - row$y2, "bigpts"),
            gp = grid::gpar(
              col = row$color,
              lwd = row$lwd * lwd_scale,
              lineend = "butt"
            ),
            name = paste0("line.", i)
          ))
        }
      },
      "fill_rect" = {
        children <- grid::gList(children, grid::rectGrob(
          x = grid::unit(row$x, "bigpts"),
          y = grid::unit(total_h - row$y, "bigpts"),
          width = grid::unit(row$width, "bigpts"),
          height = grid::unit(row$height, "bigpts"),
          just = c("left", "top"),
          gp = grid::gpar(fill = row$color, col = NA),
          name = paste0("fillrect.", i)
        ))
      },
      "rect" = {
        children <- grid::gList(children, grid::rectGrob(
          x = grid::unit(row$x, "bigpts"),
          y = grid::unit(total_h - row$y, "bigpts"),
          width = grid::unit(row$width, "bigpts"),
          height = grid::unit(row$height, "bigpts"),
          just = c("left", "top"),
          gp = grid::gpar(
            col = row$color,
            fill = NA,
            lwd = row$lwd * lwd_scale
          ),
          name = paste0("rect.", i)
        ))
      },
      "text" = {
        # Non-math text from \text{} — render as textGrob
        txt <- row$text
        if (!is.na(txt) && nzchar(txt)) {
          # Resolve font face from MicroTeX FontStyle flags
          face <- .resolve_text_face(row$font_style)
          # Build gpar: use text_gp font settings, override color and size
          tgp <- grid::gpar(
            fontsize = row$font_size,
            col = row$color,
            fontface = face
          )
          if (!is.null(text_gp$fontfamily)) {
            tgp$fontfamily <- text_gp$fontfamily
          }
          children <- grid::gList(children, grid::textGrob(
            label = txt,
            x = grid::unit(row$x, "bigpts"),
            y = grid::unit(total_h - row$y, "bigpts"),
            just = c("left", "bottom"),
            gp = tgp,
            name = paste0("text.", i)
          ))
        }
      },
      "glyph" = {
        # Typeface mode: collect glyph data for batched glyphGrob.
        if (render_mode == "typeface") {
          font_file <- row$font_file
          glyph_id <- row$glyph
          if (!is.na(font_file) && nzchar(font_file) &&
              !is.na(glyph_id)) {
            glyph_ids <- c(glyph_ids, glyph_id)
            glyph_x <- c(glyph_x, row$x)
            glyph_y <- c(glyph_y, total_h - row$y)
            glyph_sizes <- c(glyph_sizes, row$font_size)
            glyph_cols <- c(glyph_cols, row$color)
            glyph_fonts <- c(glyph_fonts, font_file)
          }
        }
        # In path mode, glyphs are rendered as path records (no action needed)
      }
    )
  }

  # Build a single batched glyphGrob for all typeface-mode glyphs
  if (length(glyph_ids) > 0) {
    glyph_grob <- .build_glyph_grob(
      glyph_ids, glyph_x, glyph_y, glyph_sizes,
      glyph_cols, glyph_fonts, depth = depth
    )
    if (!is.null(glyph_grob)) {
      children <- grid::gList(children, glyph_grob)
    }
  }

  children
}

#' Convert path segments to a grid pathGrob
#'
#' @param path_data List with \code{cmd} and \code{coords} elements.
#' @param col Fill color.
#' @param idx Index for naming.
#' @param total_h Total height for y-axis flipping.
#'
#' @return A \code{grid::pathGrob} or \code{NULL}.
#' @keywords internal
build_path_grob <- function(path_data, col, idx, total_h) {
  cmds <- path_data$cmd
  coords <- path_data$coords
  n <- length(cmds)

  if (n == 0) return(NULL)

  all_x <- numeric(0)
  all_y <- numeric(0)
  all_id <- integer(0)
  current_id <- 1L
  sub_x <- numeric(0)
  sub_y <- numeric(0)

  flush_subpath <- function() {
    if (length(sub_x) > 2) {
      all_x <<- c(all_x, sub_x)
      all_y <<- c(all_y, sub_y)
      all_id <<- c(all_id, rep(current_id, length(sub_x)))
      current_id <<- current_id + 1L
    }
    sub_x <<- numeric(0)
    sub_y <<- numeric(0)
  }

  for (j in seq_len(n)) {
    cmd <- cmds[j]
    cr <- coords[j, ]

    switch(cmd,
      "M" = {
        flush_subpath()
        sub_x <- cr[1]
        sub_y <- total_h - cr[2]
      },
      "L" = {
        sub_x <- c(sub_x, cr[1])
        sub_y <- c(sub_y, total_h - cr[2])
      },
      "C" = {
        if (length(sub_x) == 0) next
        p0x <- sub_x[length(sub_x)]
        p0y <- sub_y[length(sub_y)]
        pts <- cubic_bezier(
          p0x, p0y,
          cr[1], total_h - cr[2],
          cr[3], total_h - cr[4],
          cr[5], total_h - cr[6]
        )
        sub_x <- c(sub_x, pts$x[-1])
        sub_y <- c(sub_y, pts$y[-1])
      },
      "Q" = {
        if (length(sub_x) == 0) next
        p0x <- sub_x[length(sub_x)]
        p0y <- sub_y[length(sub_y)]
        pts <- quad_bezier(
          p0x, p0y,
          cr[1], total_h - cr[2],
          cr[3], total_h - cr[4]
        )
        sub_x <- c(sub_x, pts$x[-1])
        sub_y <- c(sub_y, pts$y[-1])
      },
      "Z" = {
        flush_subpath()
      }
    )
  }
  flush_subpath()

  if (length(all_x) == 0) return(NULL)

  grid::pathGrob(
    x = grid::unit(all_x, "bigpts"),
    y = grid::unit(all_y, "bigpts"),
    id = all_id,
    rule = "evenodd",
    gp = grid::gpar(fill = col, col = NA),
    name = paste0("path.", idx)
  )
}

#' Approximate a cubic bezier curve with line segments
#' @keywords internal
cubic_bezier <- function(x0, y0, x1, y1, x2, y2, x3, y3, n = 16) {
  t <- seq(0, 1, length.out = n)
  mt <- 1 - t
  x <- mt^3 * x0 + 3 * mt^2 * t * x1 + 3 * mt * t^2 * x2 + t^3 * x3
  y <- mt^3 * y0 + 3 * mt^2 * t * y1 + 3 * mt * t^2 * y2 + t^3 * y3
  list(x = x, y = y)
}

#' Approximate a quadratic bezier curve with line segments
#' @keywords internal
quad_bezier <- function(x0, y0, x1, y1, x2, y2, n = 12) {
  t <- seq(0, 1, length.out = n)
  mt <- 1 - t
  x <- mt^2 * x0 + 2 * mt * t * x1 + t^2 * x2
  y <- mt^2 * y0 + 2 * mt * t * y1 + t^2 * y2
  list(x = x, y = y)
}

#' Resolve MicroTeX FontStyle bitmask to R font face
#'
#' MicroTeX FontStyle: rm=1, bf=2, it=4, etc. (bitmask).
#'
#' @param style Integer font style bitmask.
#' @return Character: \code{"plain"}, \code{"bold"}, \code{"italic"},
#'   or \code{"bold.italic"}.
#' @keywords internal
.resolve_text_face <- function(style) {
  if (is.na(style)) return("plain")
  is_bold   <- bitwAnd(style, 2L) != 0L
  is_italic <- bitwAnd(style, 4L) != 0L
  if (is_bold && is_italic) "bold.italic"
  else if (is_bold) "bold"
  else if (is_italic) "italic"
  else "plain"
}

# Cache of font file -> glyphFont objects
.glyph_font_cache <- new.env(parent = emptyenv())

#' Get or create a glyphFont object for a font file
#'
#' Caches \code{grDevices::glyphFont()} objects keyed by file path
#' so that repeated calls for the same font file reuse the same object.
#'
#' @param font_file Absolute path to the OTF/TTF font file.
#' @return A \code{glyphFont} object.
#' @keywords internal
.get_glyph_font <- function(font_file) {
  cached <- .glyph_font_cache[[font_file]]
  if (!is.null(cached)) return(cached)

  gf <- grDevices::glyphFont(
    file = font_file,
    index = 0L,
    family = tools::file_path_sans_ext(basename(font_file)),
    weight = 400,
    style = "normal"
  )
  .glyph_font_cache[[font_file]] <- gf
  gf
}

#' Build a single batched glyphGrob from collected glyph data
#'
#' Creates a \code{grid::glyphGrob} containing all math glyphs,
#' using \code{grDevices::glyphInfo()} to describe glyph IDs, positions,
#' sizes, colors, and fonts. Each unique font file becomes an entry
#' in the \code{glyphFontList}.
#'
#' @param ids Integer vector of glyph IDs.
#' @param x,y Numeric vectors of glyph positions (already y-flipped, in bigpts).
#' @param sizes Numeric vector of font sizes.
#' @param cols Character vector of colors.
#' @param font_files Character vector of font file paths.
#' @param depth Depth below the baseline in bigpts (default 0).
#' @return A \code{grid::glyphGrob} or \code{NULL}.
#' @keywords internal
.build_glyph_grob <- function(ids, x, y, sizes, cols, font_files, depth = 0) {
  n <- length(ids)
  if (n == 0) return(NULL)

  # Build font list from unique font files
  unique_fonts <- unique(font_files)
  font_list_args <- lapply(unique_fonts, .get_glyph_font)
  font_list <- do.call(grDevices::glyphFontList, font_list_args)

  # Map each glyph to its font index (1-based)
  font_idx <- match(font_files, unique_fonts)

  # Glyph positions are in bigpts within the formula's coordinate system.
  # Set anchors at origin so positions map 1:1 to viewport coordinates.
  w <- if (n > 1) max(x) - min(x) else max(sizes)
  h <- if (n > 1) max(y) - min(y) else max(sizes)

  # Anchor points for positioning (see Paul Murrell, 2023,
  # "Rendering Typeset Glyphs in R Graphics").
  # "bottom" = 0 (the lowest glyph y-offset), "baseline" = depth
  # above the bottom (the TeX baseline), "centre" half-way up.
  baseline_y <- depth  # depth is distance from baseline to bottom
  centre_y <- h / 2

  info <- grDevices::glyphInfo(
    id = as.integer(ids),
    x = x,
    y = y,
    font = as.integer(font_idx),
    size = sizes,
    fontList = font_list,
    width = w,
    height = h,
    hAnchor = grDevices::glyphAnchor(0, label = "left"),
    vAnchor = grDevices::glyphAnchor(
      c(0, baseline_y, centre_y),
      label = c("bottom", "baseline", "centre")
    ),
    col = cols
  )

  grid::glyphGrob(
    info,
    x = grid::unit(0, "bigpts"),
    y = grid::unit(0, "bigpts"),
    hjust = "left",
    vjust = "bottom",
    name = "glyphs"
  )
}
