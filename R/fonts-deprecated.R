#' Superseded font functions
#'
#' `load_math_font()`, `available_math_fonts()` and `check_math_fonts()` came
#' before [load_font()] and [available_fonts()], which replace them. They keep
#' working as they did: [load_font()] also loads a math font, and
#' [available_fonts()] lists the math fonts (`math` is `TRUE`).
#'
#' @param otf_path Path to an OTF or TTF math font.
#' @return `load_math_font()` returns `NULL`, invisibly. `available_math_fonts()`
#'   returns a character vector of math font names. `check_math_fonts()`
#'   prints the math fonts that are loaded and whether the bundled font files
#'   are present, and returns their names, invisibly.
#' @seealso [load_font()], [available_fonts()]
#' @keywords internal
#' @name fonts-deprecated
#' @export
#'
#' @examples
#' available_math_fonts()
available_math_fonts <- function() {
  .math_font_names()
}

#' @rdname fonts-deprecated
#' @export
load_math_font <- function(otf_path) {
  if (!file.exists(otf_path)) {
    stop("Font file not found: ", otf_path, call. = FALSE)
  }
  name <- load_font(otf_path)
  if (!isTRUE(.font_registry$fonts[[name]]$math)) {
    stop(
      "Could not read OpenType MATH table from: ", basename(otf_path), "\n",
      "The font may not be a math font, or may have an unsupported MATH ",
      "table layout.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' @rdname fonts-deprecated
#' @export
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
