# Automatic text-font registration for MicroTeX's `main_font`.
#
# When a user passes gp = gpar(fontfamily = "..."), we want MicroTeX to
# lay out non-math text using the same font that R will draw with. We
# look up the OTF via the systemfonts package and hand it to the C++
# A3 path (microtex_add_font_from_otf), which parses metrics in-memory
# and registers the font with MicroTeX in one step.
#
# TrueType Collections (.ttc) are split into a single-face sfnt on
# first use so systemfonts' glyph-rendering path can also resolve the
# font_file for drawing — see R/ttc-splitter.R::.extract_ttc_face.
#
# If the font can't be resolved we fall back to an empty main_font;
# MicroTeX then uses the math font's text fallback. Layout and rendering
# may then drift slightly, but nothing errors.

# In-process registry of OTFs we've already fed to MicroTeX, keyed by
# the on-disk path. Value is the family name MicroTeX registered under.
.text_font_registered <- new.env(parent = emptyenv())

# Lightweight in-process cache of family-string → resolved family name
# so that repeated grobs in the same session avoid the systemfonts +
# file-stat round trip. Cleared with .clear_text_font_cache().
.text_font_lookup <- new.env(parent = emptyenv())

# Resolve an R fontfamily string to a MicroTeX main_font name, loading
# the OTF on demand. Returns "" if anything goes wrong (caller should
# treat "" as "use default"). Never throws.
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

  # C++ parses the OTF, synthesises CLM bytes in memory, and registers
  # with MicroTeX in one call. Empty string on failure.
  fam_name <- microtex_add_font_from_otf(otf_path, 0L)
  if (!is.character(fam_name) || !nzchar(fam_name)) return("")

  # If systemfonts handed us a font that happens to carry an OT MATH
  # table (some Linux CI runners alias "sans" to such a font), MicroTeX
  # registers it as a math font, not a main font — it would never be
  # found via the main-font lookup. Treat this as "no matching text
  # font" so the caller falls back to MicroTeX's default text metrics.
  if (!fam_name %in% microtex_main_font_families()) return("")

  .text_font_registered[[otf_path]] <- fam_name
  fam_name
}

# For tests and user-facing "clear everything" helpers.
.clear_text_font_cache <- function() {
  rm(list = ls(.text_font_lookup), envir = .text_font_lookup)
  rm(list = ls(.text_font_registered), envir = .text_font_registered)
  invisible(NULL)
}
