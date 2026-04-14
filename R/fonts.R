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

  # Check if it matches a loaded font (case-insensitive)
  loaded <- microtex_math_font_names()
  idx <- match(tolower(name), tolower(loaded))
  if (!is.na(idx)) {
    return(loaded[idx])
  }

  # Exact match already?
  if (name %in% loaded) {
    return(name)
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

# Search for a matching CLM file for a given OTF path
.find_clm <- function(otf_path) {
  # 1. Same directory, same stem
  stem <- tools::file_path_sans_ext(otf_path)
  candidate <- paste0(stem, ".clm2")
  if (file.exists(candidate)) return(candidate)

  # Also try .clm1 extension
  candidate1 <- paste0(stem, ".clm1")
  if (file.exists(candidate1)) return(candidate1)

  # 2. Check bundled fonts in the package
  otf_base <- basename(otf_path)
  clm_base <- paste0(tools::file_path_sans_ext(otf_base), ".clm2")
  pkg_candidate <- system.file("fonts", clm_base, package = "gridmicrotex")
  if (nzchar(pkg_candidate)) return(pkg_candidate)

  clm_base1 <- paste0(tools::file_path_sans_ext(otf_base), ".clm1")
  pkg_candidate1 <- system.file("fonts", clm_base1, package = "gridmicrotex")
  if (nzchar(pkg_candidate1)) return(pkg_candidate1)

  stop(
    "Could not find a CLM metrics file for: ", otf_base, "\n",
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

  # Check bundled font files exist
  pkg <- "gridmicrotex"
  bundled <- list(
    "Latin Modern Math" = c("latinmodern-math.clm2", "latinmodern-math.otf"),
    "STIX Two Math" = c("STIXTwoMath-Regular.clm2", "STIXTwoMath-Regular.otf"),
    "Lete Sans Math" = c("LeteSansMath.clm2", "LeteSansMath.otf"),
    "TeX Gyre DejaVu Math" = c("texgyredejavu-math.clm2", "texgyredejavu-math.otf")
  )
  message("Bundled font files:")
  for (nm in names(bundled)) {
    files <- bundled[[nm]]
    ok <- all(nchar(system.file("fonts", files, package = pkg)) > 0)
    message("  - ", nm, ": ", if (ok) "found" else "MISSING")
  }

  invisible(fonts)
}
