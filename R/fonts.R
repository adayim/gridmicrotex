# Font name aliases for convenience
.font_aliases <- c(
  "stix"     = "STIX Two Math",
  "stix2"    = "STIX Two Math",
  "lete"     = "Lete Sans Math",
  "letesans" = "Lete Sans Math"
)

#' Resolve a math font name
#'
#' Translates short aliases (e.g., \code{"stix"}, \code{"lete"}) to the
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
#'   STIX Two Math (\code{"stix"})   \tab Serif  \tab \code{"serif"} \cr
#' }
#' Additional math fonts can be loaded with \code{\link{load_font}}.
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

#' Load a math font from an OTF file
#'
#' Loads an OTF/TTF math font into MicroTeX's internal font registry. The
#' font's OpenType MATH table is parsed directly in C++ and the required
#' metrics are synthesised on the fly. You can download free math fonts like
#' Latin Modern Math (default math fonts in LaTeX) and load it with
#' \code{load_font()} to use it for math rendering.
#'
#' The font is also registered with the \pkg{systemfonts} package so it
#' can be selected for surrounding plot text via
#' \code{gp = gpar(fontfamily = "...")} without being installed
#' system-wide.
#'
#' @section Text fonts:
#' This function is only for \strong{math} fonts (fonts with an
#' OpenType MATH table). Plain text fonts used inside \code{\\text\{\}}
#' blocks are resolved automatically by \pkg{systemfonts} from the
#' \code{gp$fontfamily} argument --- no \code{load_font()} call required.
#'
#' @param otf_path Path to the OTF/TTF font file.
#' @return Invisibly returns \code{NULL}.
#' @seealso \code{\link{available_math_fonts}}, \code{\link{latex_options}},
#'   \code{\link{latex_grob}}
#' @export
#'
#' @examples
#' \donttest{
#'   # Load a math font from a local OTF file. Here we point at the
#'   # bundled STIX font so the example is self-contained and loaded.
#'   # You don't need to load the bundled fonts to use them — they're registered
#'   # with systemfonts on first render — but this shows how to load a custom font.
#'   # in practice you would pass the path to any OTF with an OpenType MATH table.
#'   otf <- system.file("fonts", "STIXTwoMath-Regular.otf",
#'                      package = "gridmicrotex")
#'   load_font(otf)
#'   available_math_fonts()
#' }
load_font <- function(otf_path) {
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

  display <- microtex_add_font_from_otf(otf_path, 0L)
  if (!nzchar(display)) {
    stop(
      "Could not read OpenType MATH table from: ", basename(otf_path), "\n",
      "The font may not be a math font, or may have an unsupported MATH ",
      "table layout.",
      call. = FALSE
    )
  }

  # Make the font selectable via gp = gpar(fontfamily = <name>).
  .register_font_with_systemfonts(otf_path, display)

  invisible(NULL)
}

# Register a math font with systemfonts so gp$fontfamily = <name> (or any
# alias) resolves to `otf_path` for grid text drawing. Silent on failure
# — systemfonts is a soft dep and missing it is not a blocker.
.register_font_with_systemfonts <- function(otf_path, display_name,
                                            aliases = character(0)) {
  if (!requireNamespace("systemfonts", quietly = TRUE)) return(invisible())
  names <- unique(c(display_name, aliases))
  for (nm in names) {
    try(
      systemfonts::register_font(name = nm, plain = otf_path),
      silent = TRUE
    )
  }
  invisible()
}

# Session-level flag for the bundled-font systemfonts registration. Kept
# out of .onLoad so we don't touch Core Text at namespace-load time on
# macOS — older SDKs print "XType: Using static font registry." to stderr,
# which R CMD check captures and flags across many check phases. Running
# this lazily on first render keeps the check log clean.
.fonts_state <- new.env(parent = emptyenv())
.fonts_state$registered <- FALSE

.ensure_bundled_fonts_registered <- function() {
  if (isTRUE(.fonts_state$registered)) return(invisible())
  .fonts_state$registered <- TRUE  # set first so a failure isn't retried each call

  lete <- system.file("fonts", "LeteSansMath.otf", package = "gridmicrotex")
  if (nzchar(lete)) {
    .register_font_with_systemfonts(lete, "Lete Sans Math", aliases = "lete")
  }

  stix <- system.file("fonts", "STIXTwoMath-Regular.otf", package = "gridmicrotex")
  if (nzchar(stix)) {
    .register_font_with_systemfonts(stix, "STIX Two Math", aliases = "stix")
  }

  invisible()
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

  pkg <- "gridmicrotex"
  message("Bundled font files:")
  for (file in c("LeteSansMath.otf", "STIXTwoMath-Regular.otf")) {
    p <- system.file("fonts", file, package = pkg)
    message("  - ", file, ": ", if (nzchar(p)) "found" else "MISSING")
  }

  invisible(fonts)
}
