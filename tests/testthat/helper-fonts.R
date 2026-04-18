# Skip tests that need a downloadable math font not currently registered
# with MicroTeX. Keeps CRAN and fresh-install runs clean without forcing
# every CI job to pre-download the optional fonts.
skip_if_math_font_unavailable <- function(name) {
  nm <- gridmicrotex:::.registry_lookup(name)$display_name
  if (!(nm %in% gridmicrotex::available_math_fonts())) {
    testthat::skip(paste0(
      nm, " is not registered - run download_math_font(\"", name, "\") to enable."
    ))
  }
}
