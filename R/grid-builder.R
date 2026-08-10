#' Build grid children from a MicroTeX layout data.frame
#'
#' Converts each row of the layout data.frame into the appropriate
#' grid grob (pathGrob, segmentsGrob, rectGrob, textGrob, glyphGrob).
#'
#' @param layout_df Data.frame returned by \code{parse_latex_cpp()}.
#' @param total_h Total height of the formula (height + depth) in bigpts.
#' @param depth Depth below the baseline in bigpts (default 0).
#' @param text_gp Optional \code{\link[grid]{gpar}} for text grobs
#'   (from \code{\\text\{\}} blocks). Controls fontfamily; the face comes
#'   from each record's MicroTeX \code{font_style}.
#' @param render_mode Character string: \code{"path"} or \code{"typeface"}.
#'   In typeface mode, glyph records are rendered via \code{glyphGrob}.
#' @return A \code{grid::gList} of child grobs.
#' @noRd
build_latex_children <- function(layout_df, total_h, depth = 0,
                                 text_gp = NULL,
                                 render_mode = "typeface") {
  n <- nrow(layout_df)
  if (n == 0) return(grid::gList())

  # MicroTeX line widths are in bigpts (1/72 inch) but R's lwd unit is
  # 1/96 inch, so scale by 96/72 = 4/3 to get correct stroke widths.
  lwd_scale <- 96 / 72

  # Pre-extract columns. data.frame row indexing (`layout_df[i, ]`)
  # is much slower than column access (`layout_df$x[i]`).
  type   <- layout_df$type
  x      <- layout_df$x;      y      <- layout_df$y
  x2     <- layout_df$x2;     y2     <- layout_df$y2
  width  <- layout_df$width;  height <- layout_df$height
  rx     <- layout_df$rx;     ry     <- layout_df$ry
  color  <- layout_df$color;  lwd    <- layout_df$lwd

  # Pre-allocated parts slot. parts[i] holds the grob for layout row i
  # (NULL for empty/skipped rows and for glyph rows, which are batched
  # below). Reserve one extra slot at the end for the batched glyphGrob
  # so the final assembly is one do.call instead of an O(n) gList walk.
  parts <- vector("list", n + 1L)

  for (i in seq_len(n)) {
    parts[[i]] <- switch(type[i],
      "path" = {
        path_data <- layout_df$path[[i]]
        if (is.null(path_data)) NULL
        else build_path_grob(path_data, color[i], i, total_h)
      },
      "line" = if (abs(y[i] - y2[i]) < 0.001) {
        # Horizontal rule (fraction bars, \hline, vertical borders) →
        # filled rect; keeps dimensions in bigpts and avoids lwd issues.
        grid::rectGrob(
          x = grid::unit(x[i], "bigpts"),
          y = grid::unit(total_h - y[i], "bigpts"),
          width = grid::unit(x2[i] - x[i], "bigpts"),
          height = grid::unit(lwd[i], "bigpts"),
          just = c("left", "centre"),
          gp = grid::gpar(fill = color[i], col = NA),
          name = paste0("rule.", i)
        )
      } else {
        grid::segmentsGrob(
          x0 = grid::unit(x[i], "bigpts"),
          y0 = grid::unit(total_h - y[i], "bigpts"),
          x1 = grid::unit(x2[i], "bigpts"),
          y1 = grid::unit(total_h - y2[i], "bigpts"),
          gp = grid::gpar(col = color[i], lwd = lwd[i] * lwd_scale, lineend = "butt"),
          name = paste0("line.", i)
        )
      },
      "fill_rect" = grid::rectGrob(
        x = grid::unit(x[i], "bigpts"),
        y = grid::unit(total_h - y[i], "bigpts"),
        width = grid::unit(width[i], "bigpts"),
        height = grid::unit(height[i], "bigpts"),
        just = c("left", "top"),
        gp = grid::gpar(fill = color[i], col = NA),
        name = paste0("fillrect.", i)
      ),
      "rect" = grid::rectGrob(
        x = grid::unit(x[i], "bigpts"),
        y = grid::unit(total_h - y[i], "bigpts"),
        width = grid::unit(width[i], "bigpts"),
        height = grid::unit(height[i], "bigpts"),
        just = c("left", "top"),
        gp = grid::gpar(col = color[i], fill = NA, lwd = lwd[i] * lwd_scale),
        name = paste0("rect.", i)
      ),
      "fill_roundrect" = {
        # MicroTeX supplies rx/ry separately; grid only takes one radius,
        # so use the smaller axis to avoid clipping outside the bounds.
        r <- min(rx[i], ry[i], na.rm = TRUE)
        grid::roundrectGrob(
          x = grid::unit(x[i], "bigpts"),
          y = grid::unit(total_h - y[i], "bigpts"),
          width = grid::unit(width[i], "bigpts"),
          height = grid::unit(height[i], "bigpts"),
          r = grid::unit(r, "bigpts"),
          just = c("left", "top"),
          gp = grid::gpar(fill = color[i], col = NA),
          name = paste0("fillroundrect.", i)
        )
      },
      "roundrect" = {
        r <- min(rx[i], ry[i], na.rm = TRUE)
        grid::roundrectGrob(
          x = grid::unit(x[i], "bigpts"),
          y = grid::unit(total_h - y[i], "bigpts"),
          width = grid::unit(width[i], "bigpts"),
          height = grid::unit(height[i], "bigpts"),
          r = grid::unit(r, "bigpts"),
          just = c("left", "top"),
          gp = grid::gpar(col = color[i], fill = NA, lwd = lwd[i] * lwd_scale),
          name = paste0("roundrect.", i)
        )
      },
      "text" = {
        txt <- layout_df$text[i]
        if (is.na(txt) || !nzchar(txt)) NULL
        else {
          tgp <- grid::gpar(
            fontsize = layout_df$font_size[i],
            col = color[i],
            fontface = .resolve_text_face(layout_df$font_style[i])
          )
          fam <- .resolve_text_family(layout_df$font_style[i],
                                      text_gp$fontfamily,
                                      layout_df$font_family[i])
          if (!is.null(fam)) tgp$fontfamily <- fam
          grid::textGrob(
            label = txt,
            x = grid::unit(x[i], "bigpts"),
            y = grid::unit(total_h - y[i], "bigpts"),
            just = c("left", "bottom"),
            # y is flipped to grid's y-up (total_h - y), so rotation sign
            # flips too: MicroTeX y-down ccw → grid y-up ccw.
            rot = -layout_df$rotation[i],
            gp = tgp,
            name = paste0("text.", i)
          )
        }
      },
      "image" = {
        # The reference is opaque to the engine; decode it here and let
        # .image_grob() decide what kind of grob the file deserves -- a
        # true-vector pictureGrob for an SVG, a raster otherwise. The
        # record carries its top-left corner, so it flips like fill_rect.
        p <- .image_ref_decode(layout_df$image_ref[i])
        # y is flipped to grid's y-up, so the rotation sign flips with it,
        # exactly as for text. A rotated record carries its centre rather
        # than its top-left corner; .image_grob() reads x/y that way when
        # angle is non-zero.
        g <- if (is.null(p)) NULL
        else .image_grob(p, width[i], height[i],
                         x = grid::unit(x[i], "bigpts"),
                         y = grid::unit(total_h - y[i], "bigpts"),
                         name = paste0("image.", i),
                         angle = -layout_df$rotation[i])
        # The box was reserved at parse time, when the file did read. If it
        # no longer does -- deleted since, or a reader that parsed the header
        # and then choked on the body -- say so rather than leaving a gap
        # nothing in the output accounts for.
        if (is.null(g) && !is.null(p)) {
          .image_warn_once(paste0("draw\x1f", .image_stamp(p)),
                           paste0("Cannot draw '", p,
                                  "' now, though it was readable when the ",
                                  "layout was measured; leaving it blank."))
        }
        g
      },
      NULL  # glyph rows handled in bulk below; everything else → NULL
    )
  }

  # --- Glyphs: collected in bulk and rendered as a single batched
  # glyphGrob at the end of the gList (matching original behavior, so
  # math symbols overlay rules/fills). Path mode renders glyphs as
  # path records and emits no "glyph" rows. ---
  if (render_mode == "typeface") {
    g <- which(type == "glyph")
    if (length(g) > 0L) {
      ff  <- layout_df$font_file[g]
      gid <- layout_df$glyph[g]
      keep <- !is.na(ff) & nzchar(ff) & !is.na(gid)
      if (any(keep)) {
        k <- g[keep]
        parts[[n + 1L]] <- .build_glyph_grob(
          ids        = layout_df$glyph[k],
          x          = layout_df$x[k],
          y          = total_h - layout_df$y[k],
          sizes      = layout_df$font_size[k],
          cols       = layout_df$color[k],
          font_files = layout_df$font_file[k],
          depth      = depth
        )
      }
    }
  }

  # Images go behind everything else. Glyphs are batched into the final
  # slot, so leaving images in record order would put them *under* the text
  # in typeface mode and *over* it in path mode -- the same input would
  # z-order differently in the two render modes.
  img <- which(type == "image")
  if (length(img)) parts <- c(parts[img], parts[-img])
  do.call(grid::gList, Filter(Negate(is.null), parts))
}

#' Convert path segments to a grid pathGrob
#'
#' @param path_data List with \code{cmd} and \code{coords} elements.
#' @param col Fill color.
#' @param idx Index for naming.
#' @param total_h Total height for y-axis flipping.
#'
#' @return A \code{grid::pathGrob} or \code{NULL}.
#' @noRd
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
#' @noRd
cubic_bezier <- function(x0, y0, x1, y1, x2, y2, x3, y3, n = 16) {
  t <- seq(0, 1, length.out = n)
  mt <- 1 - t
  x <- mt^3 * x0 + 3 * mt^2 * t * x1 + 3 * mt * t^2 * x2 + t^3 * x3
  y <- mt^3 * y0 + 3 * mt^2 * t * y1 + 3 * mt * t^2 * y2 + t^3 * y3
  list(x = x, y = y)
}

#' Approximate a quadratic bezier curve with line segments
#' @noRd
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
#' @noRd
.resolve_text_face <- function(style) {
  if (is.na(style)) return("plain")
  is_bold   <- bitwAnd(style, 2L) != 0L
  is_italic <- bitwAnd(style, 4L) != 0L
  if (is_bold && is_italic) "bold.italic"
  else if (is_bold) "bold"
  else if (is_italic) "italic"
  else "plain"
}

# Which font family a text record should be drawn in. The single place
# the precedence lives, because the renderer and the *measurer* both ask:
# measuring in one font and drawing in another puts the glyphs where they
# do not fit.
#
#   \gmfontfamily  >  \texttt / \textsf  >  gp$fontfamily
#
# Local beats global. `family` is the name carried by the record, from a
# CSS font-family; it can say "Georgia", which the font_style bits never
# can. Those bits still cover \texttt (128) and \textsf (64), which is how
# markdown's `code` and <code> get monospace -- without this they reported
# monospace but drew in the body font.
#
# There is deliberately no serif case for the bits: \textrm and a plain
# \text{} both report bit 1, so mapping it would restyle every ordinary
# run. font-family: serif goes through `family` instead -- and so does
# \textrm, which names the reserved family below rather than relying on
# its (indistinguishable) style bit. See src/font_family_atom.h.
.GM_DEFAULT_FAMILY <- "gridmicrotex.default"

.resolve_text_family <- function(style, default = NULL, family = NULL) {
  if (!is.null(family) && !is.na(family) && nzchar(family)) {
    # \textrm: back to the caller's font, overriding any enclosing
    # \textsf / \texttt rather than inheriting it.
    if (identical(family, .GM_DEFAULT_FAMILY)) return(default)
    return(family)
  }
  if (is.na(style)) return(default)
  if (bitwAnd(style, 128L) != 0L) return("mono")
  if (bitwAnd(style, 64L) != 0L) return("sans")
  default
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
#' @noRd
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
#' @noRd
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
