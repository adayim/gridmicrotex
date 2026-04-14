# Writer for a minimal CLM v6 file (minor=1, no glyph paths). Produces
# a .clm1 suitable for registering a system text font as `main_font`.
# The output exactly mirrors CLMReader's read-order in
# src/MicroTeX/lib/otf/clm.cpp so MicroTeX loads it without
# modification. Intentionally omitted:
#   - Class kernings (count = 0)
#   - Ligatures (empty root node; liga = -1)
#   - Math consts (only read when isMathFont = 1)
#   - Per-glyph kern records, math table, glyph paths
# Per-glyph metrics are approximated: width from hmtx, height = font
# ascent, depth = font descent, xMin = 0. This is sufficient for
# positioning non-math text runs; MicroTeX's R text-measurement callback
# provides exact string metrics for \text{} blocks.

# Must match src/MicroTeX/lib/otf/otfconfig.h:CLM_VER_MAJOR.
.CLM_VER_MAJOR <- 6L

# Write a minimal text-font CLM to `out_path`. `meta` must be the list
# returned by .parse_otf_metrics(). Returns out_path invisibly.
.write_text_clm <- function(meta, out_path) {
  con <- file(out_path, "wb")
  on.exit(close(con), add = TRUE)

  # --- Header: "clm" + u16 major + u8 minor(=1, no glyph paths) ---
  writeBin(charToRaw("clm"), con)
  .wu16(con, .CLM_VER_MAJOR)
  .wu8(con, 1L)

  # --- Meta ---
  .wstr0(con, meta$name %||% "")
  .wstr0(con, meta$family %||% "")
  .wu8(con, 0L)                     # isMathFont = false
  .wu16(con, 0L)                    # style (FontStyle::none)
  .wu16(con, meta$em)
  .wu16(con, max(0L, meta$x_height))
  .wu16(con, max(0L, meta$ascent))
  .wu16(con, max(0L, meta$descent))

  # Unicode → glyph id map. CLMReader expects u16 count; clamp for safety.
  cmap <- meta$cmap
  # Drop entries whose glyph id is out of range for safety.
  keep <- cmap$glyph_id >= 0L & cmap$glyph_id < meta$num_glyphs
  cmap <- cmap[keep, , drop = FALSE]
  n_cmap <- min(nrow(cmap), 65535L)
  if (nrow(cmap) > n_cmap) cmap <- cmap[seq_len(n_cmap), , drop = FALSE]
  .wu16(con, n_cmap)
  if (n_cmap > 0L) {
    # Write as one bulk buffer for speed.
    raw_buf <- .pack_cmap(cmap$unicode, cmap$glyph_id)
    writeBin(raw_buf, con)
  }

  # --- ClassKernings: none ---
  .wu16(con, 0L)

  # --- Ligatures: empty root (glyph=0, liga=-1, childCount=0) ---
  .wu16(con, 0L)
  .wi32(con, -1L)
  .wu16(con, 0L)

  # --- MathConsts: omitted because isMathFont = 0 ---

  # --- Glyphs: one 10-byte record per glyph id in [0, num_glyphs) ---
  n_g <- meta$num_glyphs
  .wu16(con, n_g)
  if (n_g > 0L) {
    asc  <- max(0L, meta$ascent)
    desc <- max(0L, meta$descent)
    raw_buf <- .pack_glyphs(meta$advances, asc, desc, n_g)
    writeBin(raw_buf, con)
  }

  invisible(out_path)
}

# --- Bulk packers (big-endian raw buffers) ---

# Pack cmap entries as (u32 unicode + u16 glyph) repeated n times.
.pack_cmap <- function(unicodes, glyph_ids) {
  n <- length(unicodes)
  buf <- raw(n * 6L)
  for (i in seq_len(n)) {
    u <- unicodes[i]
    g <- glyph_ids[i]
    base <- (i - 1L) * 6L
    buf[base + 1L] <- as.raw(bitwAnd(bitwShiftR(u, 24L), 0xFFL))
    buf[base + 2L] <- as.raw(bitwAnd(bitwShiftR(u, 16L), 0xFFL))
    buf[base + 3L] <- as.raw(bitwAnd(bitwShiftR(u,  8L), 0xFFL))
    buf[base + 4L] <- as.raw(bitwAnd(u,                0xFFL))
    buf[base + 5L] <- as.raw(bitwAnd(bitwShiftR(g,  8L), 0xFFL))
    buf[base + 6L] <- as.raw(bitwAnd(g,                0xFFL))
  }
  buf
}

# Pack glyph records: (i16 width, i16 height, i16 depth, i16 xMin, u16 kernCount=0).
.pack_glyphs <- function(advances, ascent, descent, n) {
  buf <- raw(n * 10L)
  # Pre-compute ascent/descent bytes (same for every glyph in this minimal form).
  h_hi <- as.raw(bitwAnd(bitwShiftR(ascent,  8L), 0xFFL))
  h_lo <- as.raw(bitwAnd(ascent, 0xFFL))
  d_hi <- as.raw(bitwAnd(bitwShiftR(descent, 8L), 0xFFL))
  d_lo <- as.raw(bitwAnd(descent, 0xFFL))
  zero <- as.raw(0L)
  for (i in seq_len(n)) {
    w <- if (i <= length(advances)) advances[i] else 0L
    if (w < 0L) w <- 0L
    if (w > 0xFFFFL) w <- 0xFFFFL
    w_hi <- as.raw(bitwAnd(bitwShiftR(w, 8L), 0xFFL))
    w_lo <- as.raw(bitwAnd(w, 0xFFL))
    base <- (i - 1L) * 10L
    buf[base + 1L] <- w_hi      # width  hi
    buf[base + 2L] <- w_lo      # width  lo
    buf[base + 3L] <- h_hi      # height hi
    buf[base + 4L] <- h_lo      # height lo
    buf[base + 5L] <- d_hi      # depth  hi
    buf[base + 6L] <- d_lo      # depth  lo
    buf[base + 7L] <- zero      # xMin   hi (0)
    buf[base + 8L] <- zero      # xMin   lo (0)
    buf[base + 9L] <- zero      # kernCount hi (0)
    buf[base + 10L] <- zero     # kernCount lo (0)
  }
  buf
}

# --- Primitive writers (big-endian) ---
.wu8 <- function(con, v) {
  writeBin(as.raw(bitwAnd(as.integer(v), 0xFFL)), con)
}
.wu16 <- function(con, v) {
  v <- as.integer(v)
  writeBin(as.raw(c(
    bitwAnd(bitwShiftR(v, 8L), 0xFFL),
    bitwAnd(v, 0xFFL)
  )), con)
}
.wi32 <- function(con, v) {
  # R integer is already 32-bit; writeBin with endian="big" works.
  writeBin(as.integer(v), con, size = 4L, endian = "big")
}
# Null-terminated string matching CLMReader::readString() (reads until 0x00).
.wstr0 <- function(con, s) {
  # Write as Latin-1 bytes; PostScript names are ASCII by spec. If the
  # family contains non-ASCII we coerce via iconv; unmappable → '?'.
  b <- tryCatch(
    charToRaw(enc2utf8(s)),
    error = function(e) charToRaw("Unknown")
  )
  writeBin(b, con)
  writeBin(as.raw(0L), con)
}
