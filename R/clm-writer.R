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
  if (n == 0L) return(buf)
  u <- as.integer(unicodes)
  g <- as.integer(glyph_ids)
  # Vectorized subset-assign: one call per byte position across all records.
  buf[seq.int(1L, by = 6L, length.out = n)] <- as.raw(bitwAnd(bitwShiftR(u, 24L), 0xFFL))
  buf[seq.int(2L, by = 6L, length.out = n)] <- as.raw(bitwAnd(bitwShiftR(u, 16L), 0xFFL))
  buf[seq.int(3L, by = 6L, length.out = n)] <- as.raw(bitwAnd(bitwShiftR(u,  8L), 0xFFL))
  buf[seq.int(4L, by = 6L, length.out = n)] <- as.raw(bitwAnd(u, 0xFFL))
  buf[seq.int(5L, by = 6L, length.out = n)] <- as.raw(bitwAnd(bitwShiftR(g, 8L), 0xFFL))
  buf[seq.int(6L, by = 6L, length.out = n)] <- as.raw(bitwAnd(g, 0xFFL))
  buf
}

# Pack glyph records: (i16 width, i16 height, i16 depth, i16 xMin, u16 kernCount=0).
.pack_glyphs <- function(advances, ascent, descent, n) {
  buf <- raw(n * 10L)
  if (n == 0L) return(buf)
  # Pad / truncate advances to n and clamp to u16 range.
  w <- if (length(advances) >= n) advances[seq_len(n)] else c(advances, integer(n - length(advances)))
  w <- pmin.int(pmax.int(as.integer(w), 0L), 0xFFFFL)
  # Width: vectorized across records. Height/depth: same for every glyph,
  # so a single scalar broadcast suffices for each of the two bytes.
  buf[seq.int(1L, by = 10L, length.out = n)] <- as.raw(bitwAnd(bitwShiftR(w, 8L), 0xFFL))
  buf[seq.int(2L, by = 10L, length.out = n)] <- as.raw(bitwAnd(w, 0xFFL))
  buf[seq.int(3L, by = 10L, length.out = n)] <- as.raw(bitwAnd(bitwShiftR(ascent,  8L), 0xFFL))
  buf[seq.int(4L, by = 10L, length.out = n)] <- as.raw(bitwAnd(ascent, 0xFFL))
  buf[seq.int(5L, by = 10L, length.out = n)] <- as.raw(bitwAnd(bitwShiftR(descent, 8L), 0xFFL))
  buf[seq.int(6L, by = 10L, length.out = n)] <- as.raw(bitwAnd(descent, 0xFFL))
  # Positions 7..10 (xMin, kernCount) stay zero from raw() init.
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
