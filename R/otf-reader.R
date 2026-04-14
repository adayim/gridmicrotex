# Minimal OTF/TTF parser for the metrics MicroTeX needs when using a
# system text font as `main_font`. We extract just enough to synthesize a
# pathless CLM: em size, vertical metrics, per-glyph advance widths, and
# the Unicode → glyph id mapping. OpenType MATH / glyf / CFF are ignored.
#
# All OpenType values are big-endian. TrueType Collections (.ttc) are
# refused: MicroTeX takes a single OTF path, so selecting a face from a
# collection would require cutting a new OTF out — out of scope for v1.

# Read OTF metrics and cmap for a single font file.
# Returns a list with:
#   name, family        — PostScript names (character(1))
#   em                  — unitsPerEm (integer)
#   ascent, descent     — font-wide, from hhea (integer; descent is positive)
#   x_height            — sxHeight from OS/2, 0 if absent
#   num_glyphs          — total glyph count
#   advances            — integer vector of length num_glyphs (advance widths)
#   cmap                — data.frame(unicode, glyph_id), deduplicated
.parse_otf_metrics <- function(otf_path) {
  if (!file.exists(otf_path)) {
    stop("Font file not found: ", otf_path, call. = FALSE)
  }

  bytes <- readBin(otf_path, what = "raw", n = file.info(otf_path)$size)

  # sfnt header + table directory
  if (length(bytes) < 12L) stop("Not a valid OTF/TTF file: ", otf_path, call. = FALSE)

  # Reject TrueType Collections — MicroTeX takes a single-face file.
  if (identical(bytes[1:4], charToRaw("ttcf"))) {
    stop(
      "TrueType Collection (.ttc) files are not supported as text fonts.\n",
      "Extract a single face (.otf/.ttf) or use a different family.\n",
      "File: ", otf_path,
      call. = FALSE
    )
  }

  num_tables <- .ru16(bytes, 5L)

  tables <- list()
  pos <- 13L  # after 12-byte sfnt header
  for (i in seq_len(num_tables)) {
    tag    <- .raw_to_ascii(bytes[pos:(pos + 3L)])
    offset <- .ru32(bytes, pos + 8L)
    length_ <- .ru32(bytes, pos + 12L)
    tables[[tag]] <- list(offset = offset + 1L, length = length_)  # +1 for R 1-based
    pos <- pos + 16L
  }

  need <- c("head", "hhea", "maxp", "hmtx", "cmap", "name")
  missing <- setdiff(need, names(tables))
  if (length(missing) > 0L) {
    stop("OTF is missing required tables: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  # head table → unitsPerEm at offset 18
  em <- .ru16(bytes, tables$head$offset + 18L)

  # hhea → ascender (i16) at 4, descender (i16) at 6, numOfLongHorMetrics (u16) at 34
  ascent  <- .ri16(bytes, tables$hhea$offset + 4L)
  descent <- -.ri16(bytes, tables$hhea$offset + 6L)  # spec is negative; we store positive
  num_h_metrics <- .ru16(bytes, tables$hhea$offset + 34L)

  # maxp → numGlyphs (u16) at offset 4
  num_glyphs <- .ru16(bytes, tables$maxp$offset + 4L)

  # OS/2 → sxHeight (i16) at 86 (table version >= 2 only)
  x_height <- 0L
  if (!is.null(tables[["OS/2"]])) {
    os2_off <- tables[["OS/2"]]$offset
    os2_ver <- .ru16(bytes, os2_off)
    if (os2_ver >= 2L && tables[["OS/2"]]$length >= 96L) {
      x_height <- .ri16(bytes, os2_off + 86L)
      if (x_height < 0L) x_height <- 0L
    }
  }

  # hmtx → numOfLongHorMetrics pairs of (u16 advance, i16 lsb), then
  # (num_glyphs - numOfLongHorMetrics) trailing i16 lsb values. Glyphs
  # beyond numOfLongHorMetrics inherit the last advance width.
  advances <- integer(num_glyphs)
  hmtx_off <- tables$hmtx$offset
  for (i in seq_len(num_h_metrics)) {
    advances[i] <- .ru16(bytes, hmtx_off + (i - 1L) * 4L)
  }
  if (num_glyphs > num_h_metrics && num_h_metrics > 0L) {
    advances[(num_h_metrics + 1L):num_glyphs] <- advances[num_h_metrics]
  }

  # cmap → pick the best subtable
  cmap <- .parse_cmap(bytes, tables$cmap$offset)

  # name → PostScript name (id=6) and family (id=1)
  nm <- .parse_name(bytes, tables$name$offset)

  list(
    name      = nm$post_name,
    family    = nm$family,
    em        = as.integer(em),
    ascent    = as.integer(ascent),
    descent   = as.integer(descent),
    x_height  = as.integer(x_height),
    num_glyphs = as.integer(num_glyphs),
    advances  = as.integer(advances),
    cmap      = cmap
  )
}

# Pick a cmap subtable and return data.frame(unicode, glyph_id). Prefers a
# full-Unicode (format 12) table over BMP-only (format 4).
.parse_cmap <- function(bytes, cmap_off) {
  num_subtables <- .ru16(bytes, cmap_off + 2L)
  # Enumerate subtables: (platformID, encodingID, offset-within-cmap)
  subs <- vector("list", num_subtables)
  for (i in seq_len(num_subtables)) {
    rec_off <- cmap_off + 4L + (i - 1L) * 8L
    pid <- .ru16(bytes, rec_off)
    eid <- .ru16(bytes, rec_off + 2L)
    sub_off <- cmap_off + .ru32(bytes, rec_off + 4L)
    fmt <- .ru16(bytes, sub_off)
    subs[[i]] <- list(pid = pid, eid = eid, offset = sub_off, format = fmt)
  }

  # Priority: format 12 full Unicode (3/10 or 0/4) → format 4 BMP (3/1 or 0/3)
  pick <- function(pred) {
    for (s in subs) if (pred(s)) return(s)
    NULL
  }
  chosen <-
    pick(function(s) s$format == 12L &&
           ((s$pid == 3L && s$eid == 10L) || (s$pid == 0L && s$eid %in% c(4L, 6L)))) %||%
    pick(function(s) s$format == 12L) %||%
    pick(function(s) s$format == 4L &&
           ((s$pid == 3L && s$eid == 1L) || (s$pid == 0L))) %||%
    pick(function(s) s$format == 4L)

  if (is.null(chosen)) {
    stop("No supported cmap subtable (need format 4 or 12) in OTF.", call. = FALSE)
  }

  if (chosen$format == 12L) {
    .parse_cmap12(bytes, chosen$offset)
  } else {
    .parse_cmap4(bytes, chosen$offset)
  }
}

.parse_cmap4 <- function(bytes, off) {
  seg_count_x2 <- .ru16(bytes, off + 6L)
  seg_count <- seg_count_x2 %/% 2L
  # end_code[], reservedPad(u16), start_code[], id_delta[] (i16), id_range_offset[]
  end_off   <- off + 14L
  start_off <- end_off + seg_count_x2 + 2L
  delta_off <- start_off + seg_count_x2
  range_off <- delta_off + seg_count_x2

  unicodes <- integer(0)
  glyphs   <- integer(0)
  for (i in seq_len(seg_count)) {
    end_code   <- .ru16(bytes, end_off   + (i - 1L) * 2L)
    start_code <- .ru16(bytes, start_off + (i - 1L) * 2L)
    id_delta   <- .ri16(bytes, delta_off + (i - 1L) * 2L)
    id_range   <- .ru16(bytes, range_off + (i - 1L) * 2L)
    if (end_code == 0xFFFFL && start_code == 0xFFFFL) next  # sentinel
    cps <- start_code:end_code
    if (id_range == 0L) {
      gids <- (cps + id_delta) %% 65536L
    } else {
      # Indirect: glyph id via id_range table
      gids <- integer(length(cps))
      for (k in seq_along(cps)) {
        rec_off_i <- range_off + (i - 1L) * 2L
        # offset = id_range + cps_offset * 2 + rec_off_i
        glyph_off <- rec_off_i + id_range + (cps[k] - start_code) * 2L
        g <- .ru16(bytes, glyph_off)
        gids[k] <- if (g == 0L) 0L else (g + id_delta) %% 65536L
      }
    }
    keep <- gids != 0L & cps != 0L
    unicodes <- c(unicodes, cps[keep])
    glyphs   <- c(glyphs, gids[keep])
  }
  .dedupe_cmap(unicodes, glyphs)
}

.parse_cmap12 <- function(bytes, off) {
  num_groups <- .ru32(bytes, off + 12L)
  unicodes <- integer(0)
  glyphs   <- integer(0)
  group_off <- off + 16L
  for (i in seq_len(num_groups)) {
    rec <- group_off + (i - 1L) * 12L
    start_char <- .ru32(bytes, rec)
    end_char   <- .ru32(bytes, rec + 4L)
    start_gid  <- .ru32(bytes, rec + 8L)
    cps  <- start_char:end_char
    gids <- start_gid + seq_len(length(cps)) - 1L
    unicodes <- c(unicodes, cps)
    glyphs   <- c(glyphs, gids)
  }
  .dedupe_cmap(unicodes, glyphs)
}

.dedupe_cmap <- function(unicodes, glyphs) {
  if (length(unicodes) == 0L) {
    return(data.frame(unicode = integer(0), glyph_id = integer(0)))
  }
  ord <- order(unicodes)
  unicodes <- unicodes[ord]
  glyphs   <- glyphs[ord]
  dup <- duplicated(unicodes)
  data.frame(
    unicode  = as.integer(unicodes[!dup]),
    glyph_id = as.integer(glyphs[!dup])
  )
}

# name table → PostScript name (id=6) and family (id=1). Prefer
# (platformID=3, encodingID=1, languageID=0x0409) records, which are
# UTF-16BE ASCII-ish; fall back to platformID=1 records (MacRoman).
.parse_name <- function(bytes, off) {
  count <- .ru16(bytes, off + 2L)
  # stringOffset is relative to the start of the name table; add `off`
  # (already 1-based) to get an R offset.
  storage_r <- off + .ru16(bytes, off + 4L)

  best <- list("1" = NULL, "6" = NULL)
  for (i in seq_len(count)) {
    rec <- off + 6L + (i - 1L) * 12L
    pid <- .ru16(bytes, rec)
    eid <- .ru16(bytes, rec + 2L)
    lid <- .ru16(bytes, rec + 4L)
    nid <- .ru16(bytes, rec + 6L)
    len <- .ru16(bytes, rec + 8L)
    noff <- .ru16(bytes, rec + 10L)
    key <- as.character(nid)
    if (!(nid %in% c(1L, 6L))) next

    score <- 0L
    if (pid == 3L && eid == 1L && lid == 0x0409L) score <- 3L  # Windows English
    else if (pid == 3L && eid == 1L) score <- 2L
    else if (pid == 0L) score <- 2L  # Unicode
    else if (pid == 1L && eid == 0L) score <- 1L  # MacRoman
    else next

    s <- .decode_name_bytes(bytes, storage_r + noff, len, pid, eid)
    prev <- best[[key]]
    if (is.null(prev) || prev$score < score) {
      best[[key]] <- list(score = score, value = s)
    }
  }

  post_name <- if (!is.null(best[["6"]])) best[["6"]]$value else ""
  family    <- if (!is.null(best[["1"]])) best[["1"]]$value else post_name
  list(post_name = post_name, family = family)
}

.decode_name_bytes <- function(bytes, start_r, len, pid, eid) {
  if (len == 0L) return("")
  raw <- bytes[start_r:(start_r + len - 1L)]
  if (pid == 3L || pid == 0L) {
    # UTF-16BE. For ASCII names (all high bytes zero) decode fast.
    evens <- raw[seq(1L, length(raw), by = 2L)]
    odds  <- raw[seq(2L, length(raw), by = 2L)]
    if (all(evens == as.raw(0L))) {
      return(rawToChar(odds))
    }
    # Mixed: use iconv via stringToRaw roundabout — keep it simple and
    # concatenate codepoints, replacing non-ASCII with '?'. Good enough
    # for PostScript names, which are ASCII by spec.
    out <- vapply(seq_along(odds), function(k) {
      if (evens[k] == as.raw(0L)) rawToChar(odds[k]) else "?"
    }, character(1))
    paste0(out, collapse = "")
  } else {
    rawToChar(raw)
  }
}

# Safe byte-array → ASCII without choking on 0x00 (which rawToChar treats
# as an error). OpenType table tags are always 4 printable ASCII bytes in
# practice, but the sfnt scaler type itself (bytes 1..4) contains 0x00.
.raw_to_ascii <- function(raw_bytes) {
  ints <- as.integer(raw_bytes)
  chars <- ifelse(ints >= 32L & ints <= 126L, rawToChar(as.raw(ints), multiple = TRUE), ".")
  paste0(chars, collapse = "")
}

# --- Low-level big-endian readers. Offsets are R 1-based positions. ---
.ru16 <- function(b, off) {
  bitwShiftL(as.integer(b[off]), 8L) + as.integer(b[off + 1L])
}
.ri16 <- function(b, off) {
  v <- .ru16(b, off)
  if (v >= 0x8000L) v - 0x10000L else v
}
.ru32 <- function(b, off) {
  # R integers are 32-bit signed; use double to avoid overflow.
  (as.double(b[off])       * 16777216) +
  (as.double(b[off + 1L])  * 65536) +
  (as.double(b[off + 2L])  * 256) +
   as.double(b[off + 3L])
}
