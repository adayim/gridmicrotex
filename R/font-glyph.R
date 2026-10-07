# Text drawn as glyphs, for the fonts that were loaded (load_font(), a font
# option, a document's \setmainfont ...).
#
# A text record whose family is in the font registry -- so its files are known
# -- is not left to the graphics device, which can only draw a font it can find
# by name: base pdf() knows its Type 1 families and nothing else, cairo only
# installed fonts. It is
#   measured by shaping it with HarfBuzz (kerning and ligatures kept), so a
#     label measures the same on every device;
#   drawn, in "typeface" mode, as glyphs in the same glyphGrob as the math;
#   drawn, in "path" mode, on devices without glyphs, and when rotated, as
#     outlines from the font file.
#
# What stays on the device, as before: families not in the registry ("sans",
# "serif", an installed family that was never loaded), runs with
# right-to-left characters (the order they are shaped in stays the device's, so
# the bidi work is untouched), and runs with a glyph the font lacks, which the
# device may find in another font.

# Shaping is done through textshaping::shape_text(), not systemfonts::
# shape_string(): the latter only reads a font's legacy "kern" table, which
# most current fonts (New Computer Modern, the bundled Lete and STIX used as
# text, most of Calibri) leave empty in favour of the OpenType GPOS table --
# shape_string() answered no kerning at all for them. shape_text() is
# HarfBuzz and reads GPOS, and forms ligatures the same way.
#
# shape_text() also does font *substitution*: given a character `face`'s own
# file has no glyph for, it fills in a glyph from whatever font the system
# would fall back to, instead of answering a "notdef" glyph the way
# shape_string() does -- so a per-glyph font_path check (below) stands in for
# the old index-0 check to find a run shape_text() quietly reached outside
# `face`. Skipping that check would draw the substitute glyph ID's outline
# from `face`'s own file (glyph_outline() is never told about the
# substitution), which is a different, unrelated glyph.

# Shaping is done at this size and scaled: shape_text() rounds the offsets
# it returns to the size it is given, so at the size of the text they are
# whole points.
.shape_size <- 1000

# Whether `path` (as shape_text() reports it) is the same file as
# `registered` (as normalizePath() left it when the font was registered,
# fonts.R:.resolve_font_spec()). Windows paths are case-insensitive and
# shape_text() does not promise the same slash direction, so both sides are
# normalized and compared case-insensitively; two distinct registered fonts
# differing only by case on a case-sensitive filesystem is not a real case.
.same_font_file <- function(path, registered) {
  tolower(normalizePath(path, winslash = "/", mustWork = FALSE)) ==
    tolower(registered)
}

# --- Which font a record is in -----------------------------------------------

# The registered face (list(path, index)) for family `family` in the style
# `style` (MicroTeX's bit mask: bold 2, italic 4), or NULL when the family is
# not a registered font. A face not registered is the plain one, once told.
.registered_face <- function(family, style) {
  if (is.null(family) || is.na(family) || !nzchar(family)) return(NULL)
  name <- .font_lookup(family)
  if (is.null(name)) return(NULL)
  entry <- .font_registry$fonts[[name]]
  style <- if (is.na(style)) 0L else as.integer(style)
  bold <- bitwAnd(style, 2L) != 0L
  italic <- bitwAnd(style, 4L) != 0L
  key <- if (bold && italic) "bolditalic" else if (bold) "bold"
         else if (italic) "italic" else "plain"
  face <- entry$faces[[key]]
  if (is.null(face)) {
    .warn_face_once(name, key)
    face <- entry$faces$plain
  }
  face
}

.face_warned <- new.env(parent = emptyenv())

.warn_face_once <- function(name, key) {
  id <- paste(name, key)
  if (!is.null(.face_warned[[id]])) return(invisible())
  .face_warned[[id]] <- TRUE
  warning("Font '", name, "' has no ", key, " face, so the regular one is used. ",
          "Give it with load_font(", if (key == "bolditalic") "bolditalic" else key,
          " = ).", call. = FALSE)
  invisible()
}

# Whether the run can be drawn from the font's own glyphs: it has no
# right-to-left character (Hebrew, Arabic, Syriac, Thaana, N'Ko, and the
# presentation forms and marks), and no control character.
.glyph_text_ok <- function(text) {
  nzchar(text) &&
    !grepl("[[:cntrl:]]", text) &&
    !grepl("[\u0590-\u08FF\uFB1D-\uFDFF\uFE70-\uFEFF\u200F\u202B\u202E\u2067]", text)
}

# --- Shaping ------------------------------------------------------------------

.shape_cache <- new.env(parent = emptyenv())
.shape_cache_n <- new.env(parent = emptyenv())
.shape_cache_n$n <- 0L

# The glyphs of `text` in `face`, at a font size of 1: list(ids, x, y, advance),
# the x and y offsets of each glyph from the start of the run and the run's
# advance; NULL when the font has no glyph for some character.
.shape_run <- function(text, face) {
  key <- paste(face$path, face$index, text, sep = "\x1f")
  cacheable <- nchar(key, type = "bytes") <= 2048L
  if (cacheable && !is.null(hit <- .shape_cache[[key]])) {
    return(if (identical(hit, FALSE)) NULL else hit)
  }
  shaped <- tryCatch(
    textshaping::shape_text(text, path = face$path, index = face$index,
                            size = .shape_size),
    error = function(e) NULL
  )
  out <- NULL
  if (!is.null(shaped)) {
    s <- shaped$shape
    if (nrow(s) && !anyNA(s$index) && all(s$index > 0L) &&
        all(s$font_index == face$index) &&
        all(vapply(s$font_path, .same_font_file, logical(1), face$path))) {
      baseline <- shaped$metrics$pen_y[[1L]]
      out <- list(ids = as.integer(s$index), x = s$x_offset / .shape_size,
                  y = (s$y_offset - baseline) / .shape_size,
                  advance = shaped$metrics$pen_x[[1L]] / .shape_size)
    }
  }
  if (cacheable) {
    if (.shape_cache_n$n >= 5000L) {
      rm(list = ls(.shape_cache), envir = .shape_cache)
      .shape_cache_n$n <- 0L
    }
    .shape_cache[[key]] <- if (is.null(out)) FALSE else out
    .shape_cache_n$n <- .shape_cache_n$n + 1L
  }
  out
}

# --- Measuring -----------------------------------------------------------------

.extent_cache <- new.env(parent = emptyenv())

# The ascent and descent of each glyph, at a font size of 1: the ink above and
# below the baseline, a 2 x n matrix. Cached by font and glyph.
.glyph_extents <- function(face, ids) {
  font <- paste(face$path, face$index, sep = "\x1f")
  cached <- .extent_cache[[font]]
  if (is.null(cached)) {
    cached <- new.env(parent = emptyenv())
    .extent_cache[[font]] <- cached
  }
  keys <- as.character(ids)
  new_ids <- unique(ids[!vapply(keys, exists, logical(1), envir = cached,
                                inherits = FALSE)])
  if (length(new_ids)) {
    outline <- systemfonts::glyph_outline(new_ids, path = face$path,
                                          index = face$index, size = .shape_size)
    for (i in seq_along(new_ids)) {
      y <- outline$y[outline$glyph == i]
      assign(as.character(new_ids[i]),
             if (length(y)) c(max(max(y), 0), max(-min(y), 0)) / .shape_size
             else c(0, 0),
             envir = cached)
    }
  }
  vapply(keys, function(k) get(k, envir = cached), numeric(2), USE.NAMES = FALSE)
}

# What the text measurer answers for a run of a registered font:
# c(width, ascent, height), as ratios of the font size. NULL when the run is
# left to the device.
.measure_registered <- function(text, face) {
  if (!.glyph_text_ok(text)) return(NULL)
  shaped <- .shape_run(text, face)
  if (is.null(shaped)) return(NULL)
  ext <- .glyph_extents(face, shaped$ids)
  ascent <- max(ext[1L, ])
  descent <- max(ext[2L, ])
  c(shaped$advance, ascent, ascent + descent)
}

# --- Drawing -------------------------------------------------------------------

# The outlines of a shaped run as one pathGrob, at font size `size`, its
# start at (x0, y0) bigpts and turned `rot` degrees counter-clockwise about it.
# NULL for a run with no ink (spaces).
.glyph_path_grob <- function(shaped, face, size, x0, y0, rot, col, name) {
  outline <- systemfonts::glyph_outline(shaped$ids, path = face$path,
                                        index = face$index, size = size)
  if (!nrow(outline)) return(NULL)
  px <- outline$x + shaped$x[outline$glyph] * size
  py <- outline$y + shaped$y[outline$glyph] * size
  if (rot != 0) {
    th <- rot * pi / 180
    rx <- px * cos(th) - py * sin(th)
    ry <- px * sin(th) + py * cos(th)
    px <- rx
    py <- ry
  }
  key <- paste(outline$glyph, outline$contour)
  grid::pathGrob(
    x = grid::unit(x0 + px, "bigpts"),
    y = grid::unit(y0 + py, "bigpts"),
    id = match(key, unique(key)),
    rule = "winding",
    gp = grid::gpar(fill = col, col = NA),
    name = name
  )
}
