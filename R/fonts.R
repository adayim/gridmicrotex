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
#' @noRd
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
#' Returns the math fonts that can be passed to `math_font`.
#'
#' @section Font pairing:
#' For a consistent look, pair each math font with a matching
#' `fontfamily` in `gp`:
#'
#' \tabular{lll}{
#'   \strong{Math font}     \tab \strong{Style}  \tab \strong{Suggested text font} \cr
#'   Lete Sans Math (\code{"lete"}, default) \tab Sans-serif \tab \code{"sans"} \cr
#'   STIX Two Math (\code{"stix"})   \tab Serif  \tab \code{"serif"} \cr
#' }
#' Additional math fonts can be loaded with \code{\link{load_math_font}}.
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

# Internal: put MicroTeX back on the math font .onLoad() started it with.
.reset_math_font <- function() {
  if (microtex_is_inited()) {
    microtex_set_default_math_font(resolve_math_font("lete"))
  }
  invisible()
}

#' Load a math font from an OTF file
#'
#' Adds an OpenType math font, such as Latin Modern Math, for use as
#' `math_font`. The font can then also be used as a `fontfamily` for
#' plot text.
#'
#' Only math fonts need loading. For text, set `gp$fontfamily` to any
#' installed font.
#'
#' @param otf_path Path to an OTF or TTF math font.
#' @return `NULL`, invisibly.
#' @seealso \code{\link{available_math_fonts}}, \code{\link{check_math_fonts}},
#'   \code{\link{latex_options}}, \code{\link{latex_grob}}
#' @export
#'
#' @examples
#' \donttest{
#'   # The bundled STIX font stands in for your own math font here
#'   otf <- system.file("fonts", "STIXTwoMath-Regular.otf",
#'                      package = "gridmicrotex")
#'   load_math_font(otf)
#'   available_math_fonts()
#' }
load_math_font <- function(otf_path) {
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
# — registration is best-effort; failing only means gp$fontfamily won't
# resolve to the bundled OTF.
.register_font_with_systemfonts <- function(otf_path, display_name,
                                            aliases = character(0)) {
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

#' Check math font status
#'
#' Prints which math fonts are available and whether the bundled font
#' files are present. Text fonts are not covered; use
#' `systemfonts::match_fonts()` to see what a font family resolves to.
#'
#' @return The names of the available math fonts, invisibly.
#' @seealso \code{\link{available_math_fonts}}, \code{\link{load_math_font}}
#' @export
#'
#' @examples
#' check_math_fonts()
check_math_fonts <- function() {
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
