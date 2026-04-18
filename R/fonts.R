# Font name aliases for convenience
.font_aliases <- c(
  "latinmodern" = "LatinModernMath-Regular",
  "lm"          = "LatinModernMath-Regular",
  "stix"        = "STIX Two Math",
  "stix2"       = "STIX Two Math",
  "lete"        = "Lete Sans Math",
  "letesans"    = "Lete Sans Math",
  "dejavu"      = "TeXGyreDejaVuMath-Regular",
  "texgyre"     = "TeXGyreDejaVuMath-Regular"
)

#' Resolve a math font name
#'
#' Translates short aliases (e.g., \code{"stix"}, \code{"lm"}) to the
#' full MicroTeX font name. Validates that the font is loaded.
#'
#' @param name Font name or alias. Empty string uses the default font.
#' @return The resolved font name.
#' @keywords internal
resolve_math_font <- function(name) {
  if (is.null(name) || !nzchar(name)) return("")

  # Check aliases first
  lower <- tolower(name)
  if (lower %in% names(.font_aliases)) {
    return(.font_aliases[[lower]])
  }

  # Check if it matches a loaded font (case-insensitive). This also
  # covers the exact-match case.
  loaded <- microtex_math_font_names()
  idx <- match(tolower(name), tolower(loaded))
  if (!is.na(idx)) {
    return(loaded[idx])
  }

  stop(
    "Math font '", name, "' not found. Available fonts: ",
    paste(loaded, collapse = ", "),
    "\nAliases: ", paste(names(.font_aliases), collapse = ", "),
    call. = FALSE
  )
}

#' List available math fonts
#'
#' Returns the names of all math fonts currently loaded by MicroTeX.
#' These names can be passed to the \code{math_font} parameter of
#' \code{\link{latex_grob}} and \code{\link{grid.latex}}.
#'
#' @section Font pairing:
#' The bundled math fonts have different styles. For a consistent look,
#' pair them with a matching \code{fontfamily} in \code{gp}:
#'
#' \tabular{lll}{
#'   \strong{Math font}     \tab \strong{Style}  \tab \strong{Suggested text font} \cr
#'   Lete Sans Math (\code{"lete"}, default) \tab Sans-serif \tab \code{"sans"} \cr
#'   TeX Gyre DejaVu Math (\code{"dejavu"}) \tab Sans-serif \tab \code{"sans"} \cr
#'   Latin Modern Math (\code{"lm"}) \tab Serif  \tab \code{"serif"} \cr
#'   STIX Two Math (\code{"stix"})   \tab Serif  \tab \code{"serif"} \cr
#' }
#'
#' @return A character vector of math font names.
#' @export
#'
#' @examples
#' available_math_fonts()
available_math_fonts <- function() {
  microtex_math_font_names()
}

# Internal: set the default math font used by MicroTeX.
# Public entry point is `latex_options(math_font = ...)`.
.set_math_font <- function(name) {
  if (!microtex_is_inited()) {
    stop("MicroTeX is not initialized.", call. = FALSE)
  }

  if (is.null(name) || !nzchar(name)) {
    stop(
      "Please provide a math font name. Use available_math_fonts() to list choices.",
      call. = FALSE
    )
  }

  resolved <- resolve_math_font(name)
  ok <- microtex_set_default_math_font(resolved)
  if (!ok) {
    stop(
      "Failed to set math font '", resolved, "'. Available fonts: ",
      paste(available_math_fonts(), collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Load a font file into MicroTeX
#'
#' Loads an OTF/TTF font into MicroTeX's internal font registry. The font
#' can then be used as a math font via the \code{math_font} parameter of
#' \code{\link{latex_grob}}.
#'
#' For standard usage, supply only \code{otf_path}. If a matching
#' \code{.clm2} file is present next to the OTF, it is used automatically.
#' Otherwise you must generate one first (see \emph{Generating a CLM
#' metrics file} below).
#'
#' @section Text fonts:
#' Text inside \code{\\text\{\}} is rendered using R's standard
#' text-rendering system. Control the font with
#' \code{gp = gpar(fontfamily = "...")} in \code{\link{latex_grob}} ---
#' no font loading required. This function is only needed for adding
#' custom \strong{math} fonts.
#'
#' @section Generating a CLM metrics file:
#' MicroTeX reads glyph metrics, the OpenType MATH table, and glyph
#' outlines from a binary companion file (\code{.clm2}) rather than from
#' the OTF directly. A converter script is bundled with the package at
#' \code{system.file("otf2clm.py", package = "gridmicrotex")}. It requires
#' FontForge's embedded Python (\code{ffpython}), since the standard
#' Python does not ship the \code{fontforge} module.
#'
#' Typical usage from a shell, for a single font:
#' \preformatted{
#' ffpython otf2clm.py --single path/to/font.otf true out_dir
#' }
#'
#' Or to convert every OTF in a directory:
#' \preformatted{
#' ffpython otf2clm.py --batch in_dir true out_dir
#' }
#'
#' The \code{true} argument tells the converter to embed glyph outlines
#' (required for \code{render_mode = "path"}); pass \code{false} to
#' generate a smaller \code{.clm1} file suitable only for
#' \code{render_mode = "typeface"}. Run the script with no arguments to
#' see the full usage including font-style flags.
#'
#' Place the generated \code{.clm2} file next to the OTF (same stem) so
#' that \code{load_font()} finds it automatically, or pass it explicitly
#' via \code{clm_path}.
#'
#' @param otf_path Path to the OTF/TTF font file.
#' @param clm_path Optional path to the companion \code{.clm2} (or
#'   legacy \code{.clm1}) metrics file. If \code{NULL} (the default),
#'   \code{load_font()} searches for a file with the same stem as
#'   \code{otf_path}.
#' @return Invisibly returns \code{NULL}.
#' @seealso \code{\link{available_math_fonts}}, \code{\link{latex_options}},
#'   \code{\link{latex_grob}}
#' @export
#'
#' @examples
#' \donttest{
#'   # Load a custom math font from OTF (with a sibling .clm2 file):
#'   # load_font("path/to/font.otf")
#'
#'   # Locate the bundled CLM converter:
#'   # system.file("otf2clm.py", package = "gridmicrotex")
#' }
load_font <- function(otf_path, clm_path = NULL) {
  if (!file.exists(otf_path)) {
    stop("Font file not found: ", otf_path, call. = FALSE)
  }

  # Reject TrueType Collections — MicroTeX::addFont expects a single face.
  header <- tryCatch(
    readBin(otf_path, what = "raw", n = 4L),
    error = function(e) raw()
  )
  if (length(header) == 4L && identical(header, charToRaw("ttcf"))) {
    stop(
      "TrueType Collection (.ttc) files are not supported.\n",
      "Extract a single face (.otf/.ttf) and pass that instead.\n",
      "File: ", otf_path,
      call. = FALSE
    )
  }

  if (is.null(clm_path)) {
    clm_path <- .find_clm(otf_path)
  }

  if (!file.exists(clm_path)) {
    stop("CLM metrics file not found: ", clm_path, call. = FALSE)
  }

  microtex_add_font(clm_path, otf_path)
  invisible(NULL)
}

#' Download an extra bundled math font on demand
#'
#' Downloads a math font and its companion \code{.clm2} metrics file from
#' the \code{gridmicrotex} GitHub release and registers them with MicroTeX
#' so they become available for rendering. Downloaded files are cached
#' under \code{tools::R_user_dir("gridmicrotex", "cache")/fonts} and
#' re-registered automatically in subsequent R sessions, so a given font
#' only needs to be downloaded once per user.
#'
#' This function exists to keep the installed package small. Only the
#' default \code{"lete"} math font is shipped inside the package; the
#' larger fonts (Latin Modern Math, STIX Two Math, TeX Gyre DejaVu Math)
#' are fetched on demand.
#'
#' @param name Font alias. One of \code{"lm"} / \code{"latinmodern"},
#'   \code{"stix"} / \code{"stix2"}, or \code{"dejavu"} / \code{"texgyre"}.
#' @param overwrite If \code{TRUE}, re-download even if cached files are
#'   already present and valid. Defaults to \code{FALSE}.
#' @param quiet If \code{TRUE}, suppress \code{download.file()} progress
#'   messages. Defaults to \code{FALSE} in interactive sessions and
#'   \code{TRUE} otherwise.
#' @return Invisibly, a named character vector with the cached
#'   \code{otf} and \code{clm} paths.
#' @seealso \code{\link{load_font}}, \code{\link{available_math_fonts}},
#'   \code{\link{check_fonts}}
#' @export
#'
#' @examples
#' \dontrun{
#'   download_math_font("stix")
#'   latex_options(math_font = "stix")
#' }
download_math_font <- function(name,
                               overwrite = FALSE,
                               quiet     = !interactive()) {
  entry <- .registry_lookup(name)
  cache <- .font_cache_dir(create = TRUE)

  otf_dest <- file.path(cache, entry$otf$file)
  clm_dest <- file.path(cache, entry$clm$file)

  .ensure_asset(entry$otf, otf_dest, overwrite = overwrite, quiet = quiet)
  .ensure_asset(entry$clm, clm_dest, overwrite = overwrite, quiet = quiet)

  microtex_add_font(clm_dest, otf_dest)
  invisible(c(otf = otf_dest, clm = clm_dest))
}

# Registry lookup: accepts canonical key or any alias.
.registry_lookup <- function(name) {
  if (is.null(name) || !nzchar(name)) {
    stop("Please supply a font alias (e.g. \"stix\", \"lm\", \"dejavu\").",
         call. = FALSE)
  }
  lower <- tolower(name)
  for (entry in .font_registry) {
    if (lower %in% entry$aliases) return(entry)
  }
  keys <- vapply(.font_registry, function(x) x$key, character(1))
  stop("Unknown downloadable math font '", name, "'. Available: ",
       paste(keys, collapse = ", "), ".", call. = FALSE)
}

# Cache directory for user-downloaded fonts.
.font_cache_dir <- function(create = FALSE) {
  path <- file.path(tools::R_user_dir("gridmicrotex", "cache"), "fonts")
  if (create) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

# Ensure a single asset (OTF or CLM) is present at `dest` with matching
# SHA-256. Downloads if missing, overwritten, or corrupt.
.ensure_asset <- function(asset, dest, overwrite, quiet) {
  if (!overwrite && file.exists(dest) &&
      identical(unname(tools::sha256sum(dest)), asset$sha256)) {
    return(invisible(dest))
  }

  tmp <- paste0(dest, ".part")
  on.exit(unlink(tmp), add = TRUE)

  status <- tryCatch(
    utils::download.file(asset$url, tmp, mode = "wb", quiet = quiet),
    error = function(e) {
      stop("Failed to download ", asset$file, " from ", asset$url,
           "\n  ", conditionMessage(e), call. = FALSE)
    }
  )
  if (!identical(status, 0L)) {
    stop("download.file() returned non-zero status (", status,
         ") for ", asset$url, call. = FALSE)
  }

  got <- unname(tools::sha256sum(tmp))
  if (!identical(got, asset$sha256)) {
    stop("SHA-256 mismatch for ", asset$file, "\n",
         "  expected: ", asset$sha256, "\n",
         "  got:      ", got, "\n",
         "The downloaded file may be corrupt or tampered with. Delete ",
         tmp, " and try again.", call. = FALSE)
  }

  if (!file.rename(tmp, dest)) {
    file.copy(tmp, dest, overwrite = TRUE)
  }
  invisible(dest)
}

# Scan the cache dir and re-register any previously-downloaded fonts.
# Called from .onLoad so returning users don't need to call
# download_math_font() every session. Silent on failure -- a startup
# error would be user-hostile.
.register_cached_fonts <- function() {
  if (!exists(".font_registry", envir = asNamespace("gridmicrotex"),
              inherits = FALSE)) return(invisible())

  cache <- .font_cache_dir(create = FALSE)
  if (!dir.exists(cache)) return(invisible())

  loaded <- microtex_math_font_names()
  for (entry in .font_registry) {
    if (entry$display_name %in% loaded) next

    otf <- file.path(cache, entry$otf$file)
    clm <- file.path(cache, entry$clm$file)
    if (!file.exists(otf) || !file.exists(clm)) next

    try(microtex_add_font(clm, otf), silent = TRUE)
  }
  invisible()
}

# Search for a matching CLM file for a given OTF path. Looks next to
# the OTF first, then in the package's bundled fonts dir; tries .clm2
# (current format) before .clm1 (legacy, no glyph paths).
.find_clm <- function(otf_path) {
  stem      <- tools::file_path_sans_ext(otf_path)
  base_stem <- tools::file_path_sans_ext(basename(otf_path))

  for (ext in c(".clm2", ".clm1")) {
    sibling <- paste0(stem, ext)
    if (file.exists(sibling)) return(sibling)

    bundled <- system.file("fonts", paste0(base_stem, ext), package = "gridmicrotex")
    if (nzchar(bundled)) return(bundled)
  }

  stop(
    "Could not find a CLM metrics file for: ", basename(otf_path), "\n",
    "Searched:\n",
    "  - ", paste0(stem, ".clm2"), "\n",
    "  - Package bundled fonts\n",
    "Provide a CLM file via the clm_path argument, or place a .clm2 file\n",
    "next to the OTF with the same filename stem.\n",
    "CLM files can be generated with the otf2clm utility from the MicroTeX\n",
    "project: https://github.com/NanoMichael/MicroTeX",
    call. = FALSE
  )
}

#' Check font status
#'
#' Reports which math fonts are loaded and available for rendering.
#' Shows the MicroTeX version, loaded math fonts, and whether bundled
#' font files are present.
#'
#' @return Invisibly returns the character vector of available font names.
#' @export
#'
#' @examples
#' check_fonts()
check_fonts <- function() {
  if (!microtex_is_inited()) {
    message("MicroTeX is not initialized.")
    return(invisible(character(0)))
  }

  fonts <- microtex_math_font_names()
  message("MicroTeX version: ", microtex_version())
  message("Loaded math fonts (", length(fonts), "):")
  for (f in fonts) {
    message("  - ", f)
  }

  # Check bundled default font files exist
  pkg <- "gridmicrotex"
  bundled <- list(
    "Lete Sans Math" = c("LeteSansMath.clm2", "LeteSansMath.otf")
  )
  message("Bundled font files:")
  for (nm in names(bundled)) {
    files <- bundled[[nm]]
    ok <- all(nchar(system.file("fonts", files, package = pkg)) > 0)
    message("  - ", nm, ": ", if (ok) "found" else "MISSING")
  }

  cache <- .font_cache_dir(create = FALSE)
  message("Downloadable math fonts (download_math_font()):")
  for (entry in .font_registry) {
    otf <- file.path(cache, entry$otf$file)
    clm <- file.path(cache, entry$clm$file)
    status <- if (file.exists(otf) && file.exists(clm)) "cached" else "not downloaded"
    message("  - ", entry$display_name, " (", entry$key, "): ", status)
  }
  if (dir.exists(cache)) message("Cache dir: ", cache)

  invisible(fonts)
}
