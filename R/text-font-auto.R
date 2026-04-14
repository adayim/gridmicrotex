# Automatic text-font registration for MicroTeX's `main_font`.
#
# When a user passes gp = gpar(fontfamily = "..."), we want MicroTeX to
# lay out non-math text using the same font that R will draw with. That
# requires the OTF plus a .clm metrics file. We look up the OTF via the
# systemfonts package, generate a minimal text-only CLM on first use,
# cache it under tools::R_user_dir(), and register the pair with
# MicroTeX. On subsequent calls we reuse the cached CLM.
#
# If systemfonts is unavailable or the font can't be resolved we fall
# back to an empty main_font; MicroTeX then uses the math font's text
# fallback. Layout and rendering may then drift slightly, but nothing
# errors.

# Bump this when .write_text_clm's output layout changes. The cache key
# includes it so stale files are ignored automatically.
.TEXT_CLM_WRITER_VER <- 1L

# In-process registry of families we've already fed to MicroTeX, keyed
# by the family name we resolved. Prevents redundant addFont() calls.
.text_font_registered <- new.env(parent = emptyenv())

# Lightweight in-process cache of family-string → resolved family name
# so that repeated grobs in the same session avoid the systemfonts +
# file-stat round trip. Cleared with .clear_text_font_cache().
.text_font_lookup <- new.env(parent = emptyenv())

# Resolve an R fontfamily string to a MicroTeX main_font name, loading
# a generated CLM on demand. Returns "" if anything goes wrong (caller
# should treat "" as "use default"). Never throws.
.resolve_text_font <- function(family) {
  if (!is.character(family) || length(family) != 1L || !nzchar(family)) {
    family <- "sans"
  }

  cached <- .text_font_lookup[[family]]
  if (!is.null(cached)) return(cached)

  resolved <- tryCatch(
    .do_resolve_text_font(family),
    error = function(e) {
      warning(
        "Could not prepare text font for '", family, "': ", conditionMessage(e),
        "\nFalling back to MicroTeX's built-in metrics.",
        call. = FALSE
      )
      ""
    }
  )
  .text_font_lookup[[family]] <- resolved
  resolved
}

.do_resolve_text_font <- function(family) {
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    # systemfonts is a soft dep (via ragg/svglite); advise but don't error.
    warning(
      "Package 'systemfonts' is not installed; cannot auto-resolve text font '",
      family, "'. Install it for matching text metrics.",
      call. = FALSE
    )
    return("")
  }

  # match_fonts() always returns *something* (a system fallback) so we
  # don't need an extra existence check. `index` identifies the face
  # within a TrueType Collection (.ttc); for a single-face file it's 0.
  match <- systemfonts::match_fonts(family)
  source_path <- match$path
  if (!is.character(source_path) || !nzchar(source_path) || !file.exists(source_path)) {
    return("")
  }
  face_index <- if (is.numeric(match$index)) as.integer(match$index) else 0L
  if (is.na(face_index) || face_index < 0L) face_index <- 0L

  # TTCs (e.g. macOS Helvetica.ttc) hold multiple faces in one file;
  # MicroTeX takes a single-face sfnt, so extract the requested face
  # into a freestanding OTF the first time we see it.
  otf_path <- if (.is_ttc_file(source_path)) {
    .extract_ttc_face(source_path, face_index)
  } else {
    source_path
  }

  # Already registered? Re-use the family name we recorded then.
  cached_fam <- .text_font_registered[[otf_path]]
  if (!is.null(cached_fam)) return(cached_fam)

  # Cache key is tied to the original source + face index so that the
  # same ttc face always resolves to the same CLM across sessions.
  clm_key <- .text_clm_cache_key(source_path, face_index)
  clm_path <- .ensure_text_clm(otf_path, clm_key)
  if (is.null(clm_path)) return("")

  # Parse the OTF just enough to learn the family name MicroTeX will
  # index it under. Could be read from the CLM instead, but re-parsing
  # is fast and avoids duplicating that logic.
  meta <- .parse_otf_metrics(otf_path)
  fam_name <- if (nzchar(meta$family)) meta$family else meta$name
  if (!nzchar(fam_name)) return("")

  microtex_add_font(clm_path, otf_path)
  .text_font_registered[[otf_path]] <- fam_name
  fam_name
}

# Generate or locate a cached .clm1 for otf_path. `key` overrides the
# cache key — useful when `otf_path` is an intermediate file (e.g. a
# face extracted from a .ttc) and we want the CLM keyed to the original
# source. Returns the clm path, or NULL on failure.
.ensure_text_clm <- function(otf_path, key = NULL) {
  cache_dir <- .text_clm_cache_dir()
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (is.null(key)) key <- .text_clm_cache_key(otf_path)
  clm_path <- file.path(cache_dir, paste0(key, ".clm1"))

  if (file.exists(clm_path) &&
      file.info(clm_path)$size > 0L) {
    return(clm_path)
  }

  meta <- .parse_otf_metrics(otf_path)
  # Write to a temp file first, then rename — avoids leaving a
  # half-written CLM on disk if R is interrupted.
  tmp <- paste0(clm_path, ".tmp")
  .write_text_clm(meta, tmp)
  file.rename(tmp, clm_path)
  clm_path
}

# Cache key: short hash of (path, mtime, size, face_index, writer-ver,
# clm-ver). mtime is included so a reinstalled or updated font produces
# a fresh CLM. face_index distinguishes faces within a .ttc so the same
# collection with different faces doesn't collide.
.text_clm_cache_key <- function(otf_path, face_index = 0L) {
  info <- file.info(otf_path)
  mtime <- as.numeric(info$mtime)
  size  <- as.numeric(info$size)
  base <- tools::file_path_sans_ext(basename(otf_path))
  hash_input <- paste(
    normalizePath(otf_path, winslash = "/", mustWork = FALSE),
    sprintf("%.0f", mtime), sprintf("%.0f", size),
    as.integer(face_index),
    .TEXT_CLM_WRITER_VER, .CLM_VER_MAJOR,
    sep = "|"
  )
  paste0(base, "-", substr(.short_hash(hash_input), 1L, 12L))
}

.text_clm_cache_dir <- function() {
  tools::R_user_dir("gridmicrotex", which = "cache")
}

# Small platform-independent hash without pulling in a new dep.
.short_hash <- function(s) {
  # Use tools::md5sum on a temp file if available; otherwise a simple
  # polynomial hash of bytes. md5sum works on file paths only, so we
  # write the string to a temp file.
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeBin(charToRaw(s), tmp)
  unname(tools::md5sum(tmp))
}

# For tests and user-facing "clear everything" helpers.
.clear_text_font_cache <- function() {
  rm(list = ls(.text_font_lookup), envir = .text_font_lookup)
  rm(list = ls(.text_font_registered), envir = .text_font_registered)
  invisible(NULL)
}
