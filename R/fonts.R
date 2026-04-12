# Font name aliases for convenience
.font_aliases <- c(
  "latinmodern" = "LatinModernMath-Regular",
  "lm"          = "LatinModernMath-Regular",
  "xits"        = "XITS Math",
  "dejavu"      = "TeXGyreDejaVuMath-Regular",
  "texgyre"     = "TeXGyreDejaVuMath-Regular",
  "garamond"    = "Garamond-Math"
)

#' Resolve a math font name
#'
#' Translates short aliases (e.g., \code{"xits"}, \code{"lm"}) to the
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
#'   Latin Modern Math (\code{"lm"}) \tab Serif  \tab \code{"serif"} \cr
#'   XITS Math (\code{"xits"})       \tab Serif  \tab \code{"serif"} \cr
#'   Garamond Math (\code{"garamond"}) \tab Serif  \tab \code{"serif"} \cr
#'   TeX Gyre DejaVu Math (\code{"dejavu"}) \tab Sans-serif \tab \code{"sans"} \cr
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

#' Set the default math font used by MicroTeX
#'
#' Selects the default math font for formula layout. For most users,
#' this is the recommended way to switch fonts: choose one of the
#' fonts returned by \code{\link{available_math_fonts}}.
#'
#' This avoids dealing with external OTF/CLM files in normal usage.
#' Use \code{\link{load_font}} only when you need to add a custom font
#' that is not already loaded.
#'
#' @param name Math font name or alias (e.g., \code{"lm"}, \code{"xits"}).
#' @return Invisibly returns \code{TRUE} on success.
#' @seealso \code{\link{available_math_fonts}}, \code{\link{latex_grob}},
#'   \code{\link{load_font}}
#' @export
#'
#' @examples
#' \donttest{
#'   # Switch default math font to XITS Math (if loaded)
#'   set_math_font("xits")
#' }
set_math_font <- function(name) {
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
#' For standard usage, supply only \code{otf_path}. The package handles
#' the remaining font loading details internally.
#'
#' @section Text fonts:
#' Text inside \code{\\text\{\}} is rendered using R's standard
#' text-rendering system. Control the font with
#' \code{gp = gpar(fontfamily = "...")} in \code{\link{latex_grob}} ---
#' no font loading required. This function is only needed for adding
#' custom \strong{math} fonts.
#'
#' @param otf_path Path to the OTF/TTF font file.
#' @param clm_path Optional legacy metrics path. Usually leave as
#'   \code{NULL}.
#' @return Invisibly returns \code{NULL}.
#' @seealso \code{\link{available_math_fonts}}, \code{\link{set_math_font}},
#'   \code{\link{latex_grob}}
#' @export
#'
#' @examples
#' \donttest{
#'   # Load a custom math font from OTF:
#'   # load_font("path/to/font.otf")
#' }
load_font <- function(otf_path, clm_path = NULL) {
  if (!file.exists(otf_path)) {
    stop("Font file not found: ", otf_path, call. = FALSE)
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
    "XITS Math" = c("XITSMath-Regular.clm2", "XITSMath-Regular.otf"),
    "Garamond Math" = c("Garamond-Math.clm2", "Garamond-Math.otf"),
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
