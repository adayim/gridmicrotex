.onLoad <- function(libname, pkgname) {
  # Initialize MicroTeX with Lete Sans Math as the default font --- it pairs
  # naturally with R's sans-serif default for plot text.
  clm_path <- system.file("fonts", "LeteSansMath.clm2", package = pkgname)
  otf_path <- system.file("fonts", "LeteSansMath.otf", package = pkgname)

  if (nchar(clm_path) == 0 || nchar(otf_path) == 0) {
    warning("gridmicrotex: Lete Sans Math font files not found, LaTeX rendering will not work")
    return()
  }

  microtex_init(clm_path, otf_path)

  # Initialize ggplot2 integration if ggplot2 is available
  .onLoad_ggplot2()

  # Pick up any extra math fonts the user fetched previously via
  # download_math_font(). Latin Modern, STIX, and TeX Gyre DejaVu are
  # no longer bundled; users install them on demand.
  .register_cached_fonts()
}

.onAttach <- function(libname, pkgname) {
  # Warn only if the bundled default (Lete) is missing -- the other
  # math fonts are fetched on demand via download_math_font() and are
  # expected to be absent on first run.
  if (!"Lete Sans Math" %in% microtex_math_font_names()) {
    packageStartupMessage(
      "gridmicrotex: bundled default math font (Lete Sans Math) failed ",
      "to register. Run check_fonts() for details."
    )
  }
}

.onUnload <- function(libpath) {
  microtex_release()
}
