# TrueType Collection (.ttc) → single-face sfnt extractor.
#
# systemfonts::match_fonts() on macOS can return a TTC (e.g. Helvetica.ttc)
# plus a face index. The C++ A3 font loader and the grid glyph renderer
# both want a single-face .otf/.ttf path; this helper splits out the
# requested face into a standalone sfnt in the cache dir on first use
# and returns the new path. Subsequent calls short-circuit to the cached
# file so the split cost is paid once per (ttc, mtime, size, index).
#
# All OpenType values are big-endian.

# Detect a TrueType Collection by its 'ttcf' magic. Cheap — reads 4 bytes.
.is_ttc_file <- function(path) {
  if (!file.exists(path)) return(FALSE)
  magic <- tryCatch(readBin(path, what = "raw", n = 4L), error = function(e) raw(0))
  length(magic) == 4L && identical(magic, charToRaw("ttcf"))
}

# Extract face `index` from a TrueType Collection into a freestanding
# single-face sfnt, written to the cache dir, and return the new path.
# Result is cached across sessions keyed on (path, mtime, size, index).
# The output is a valid single-face OTF/TTF: sfnt header + rewritten
# table directory (new local offsets) + each selected table's bytes
# copied verbatim.
.extract_ttc_face <- function(ttc_path, index = 0L) {
  if (!file.exists(ttc_path)) {
    stop("TTC file not found: ", ttc_path, call. = FALSE)
  }
  index <- as.integer(index)
  if (is.na(index) || index < 0L) index <- 0L

  cache_dir <- .ttc_cache_dir()
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  key <- .ttc_face_cache_key(ttc_path, index)
  base <- tools::file_path_sans_ext(basename(ttc_path))
  out_path <- file.path(cache_dir, paste0(base, "-face", index, "-", key, ".otf"))
  if (file.exists(out_path) && file.info(out_path)$size > 0L) {
    return(out_path)
  }

  bytes <- readBin(ttc_path, what = "raw", n = file.info(ttc_path)$size)
  if (length(bytes) < 16L || !identical(bytes[1:4], charToRaw("ttcf"))) {
    stop("Not a TrueType Collection: ", ttc_path, call. = FALSE)
  }

  # TTC header: tag(4) + version(4) + numFonts(u32) + offsets[numFonts](u32).
  num_fonts <- .ru32(bytes, 9L)
  if (index >= num_fonts) {
    stop("TTC face index ", index, " out of range (collection has ",
         num_fonts, " face", if (num_fonts == 1) "" else "s", "): ",
         ttc_path, call. = FALSE)
  }
  face_off <- as.integer(.ru32(bytes, 13L + index * 4L)) + 1L  # R 1-based

  # sfnt header at face_off: sfntVersion(4), numTables(u16), then three u16
  # search fields we recompute.
  sfnt_version <- bytes[face_off:(face_off + 3L)]
  num_tables <- .ru16(bytes, face_off + 4L)
  if (num_tables == 0L) {
    stop("TTC face ", index, " has no tables: ", ttc_path, call. = FALSE)
  }

  records <- vector("list", num_tables)
  rec_pos <- face_off + 12L
  for (i in seq_len(num_tables)) {
    records[[i]] <- list(
      tag      = bytes[rec_pos:(rec_pos + 3L)],
      checksum = bytes[(rec_pos + 4L):(rec_pos + 7L)],
      offset   = .ru32(bytes, rec_pos + 8L),
      length   = as.integer(.ru32(bytes, rec_pos + 12L))
    )
    rec_pos <- rec_pos + 16L
  }

  # Lay out the new file: sfnt header (12) + table directory (numTables*16)
  # + table bodies, each padded to a 4-byte boundary.
  pad4 <- function(n) (4L - (n %% 4L)) %% 4L
  header_size <- 12L + num_tables * 16L
  body_offsets <- integer(num_tables)
  cursor <- header_size
  for (i in seq_len(num_tables)) {
    body_offsets[i] <- cursor
    cursor <- cursor + records[[i]]$length + pad4(records[[i]]$length)
  }
  total_size <- cursor

  out <- raw(total_size)
  out[1:4] <- sfnt_version
  out[5:6] <- .w_u16(num_tables)
  es <- as.integer(floor(log2(num_tables)))
  sr <- bitwShiftL(1L, es) * 16L
  rs <- num_tables * 16L - sr
  out[7:8]   <- .w_u16(sr)
  out[9:10]  <- .w_u16(es)
  out[11:12] <- .w_u16(rs)

  dir_pos <- 13L
  for (i in seq_len(num_tables)) {
    rec <- records[[i]]
    off <- body_offsets[i]
    len <- rec$length

    out[dir_pos:(dir_pos + 3L)]         <- rec$tag
    out[(dir_pos + 4L):(dir_pos + 7L)]  <- rec$checksum
    out[(dir_pos + 8L):(dir_pos + 11L)] <- .w_u32(off)
    out[(dir_pos + 12L):(dir_pos + 15L)] <- .w_u32(len)
    dir_pos <- dir_pos + 16L

    if (len > 0L) {
      src_from <- as.integer(rec$offset) + 1L
      out[(off + 1L):(off + len)] <- bytes[src_from:(src_from + len - 1L)]
    }
  }

  tmp <- paste0(out_path, ".tmp")
  writeBin(out, tmp)
  file.rename(tmp, out_path)
  out_path
}

.ttc_face_cache_key <- function(ttc_path, index) {
  info <- file.info(ttc_path)
  hash_input <- paste(
    normalizePath(ttc_path, winslash = "/", mustWork = FALSE),
    sprintf("%.0f", as.numeric(info$mtime)),
    sprintf("%.0f", as.numeric(info$size)),
    as.integer(index),
    "ttcface-v1",
    sep = "|"
  )
  substr(.short_hash(hash_input), 1L, 12L)
}

.ttc_cache_dir <- function() {
  tools::R_user_dir("gridmicrotex", which = "cache")
}

# --- Low-level big-endian readers/writers used only by the TTC splitter. ---
.ru16 <- function(b, off) {
  bitwShiftL(as.integer(b[off]), 8L) + as.integer(b[off + 1L])
}
.ru32 <- function(b, off) {
  # R integers are 32-bit signed; use double to avoid overflow.
  (as.double(b[off])       * 16777216) +
  (as.double(b[off + 1L])  * 65536) +
  (as.double(b[off + 2L])  * 256) +
   as.double(b[off + 3L])
}
.w_u16 <- function(n) {
  n <- as.integer(n)
  as.raw(c(bitwAnd(bitwShiftR(n, 8L), 255L), bitwAnd(n, 255L)))
}
.w_u32 <- function(n) {
  n <- as.double(n)
  as.raw(c(
    as.integer(floor(n / 16777216) %% 256),
    as.integer(floor(n / 65536)    %% 256),
    as.integer(floor(n / 256)      %% 256),
    as.integer(n %% 256)
  ))
}

# Small platform-independent hash without pulling in a new dep.
.short_hash <- function(s) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeBin(charToRaw(s), tmp)
  unname(tools::md5sum(tmp))
}
