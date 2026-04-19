.onLoad <- function(libname, pkgname) {
  # Force-load systemfonts here: our C++ OTF parser calls its
  # get_cached_face via R_GetCCallable at init time, and that lookup
  # only succeeds once systemfonts' DLL init has run. Relying on the
  # Imports declaration alone isn't sufficient on all R versions.
  requireNamespace("systemfonts", quietly = TRUE)

  # Initialize MicroTeX with Lete Sans Math as the default font --- it pairs
  # naturally with R's sans-serif default for plot text. The CLM metrics
  # are synthesised in C++ from the OTF's OpenType MATH table at load
  # time; no companion .clm2 file is shipped.
  otf_path <- system.file("fonts", "LeteSansMath.otf", package = pkgname)

  if (nchar(otf_path) == 0) {
    warning("gridmicrotex: Lete Sans Math font file not found, LaTeX rendering will not work")
    return()
  }

  microtex_init_from_otf(otf_path)

  # Register Lete with systemfonts so gp = gpar(fontfamily = "Lete Sans Math")
  # (and the "lete" alias) resolves to our bundled OTF without the user
  # having to install the font system-wide.
  .register_font_with_systemfonts(otf_path, "Lete Sans Math", aliases = "lete")

  # Register the bundled STIX Two Math font the same way.
  stix_path <- system.file("fonts", "STIXTwoMath-Regular.otf", package = pkgname)
  if (nzchar(stix_path)) {
    display <- microtex_add_font_from_otf(stix_path, 0L)
    if (nzchar(display)) {
      .register_font_with_systemfonts(stix_path, display, aliases = "stix")
    }
  }

  # Initialize ggplot2 integration if ggplot2 is available
  .onLoad_ggplot2()
}

.onAttach <- function(libname, pkgname) {
  # Warn only if the bundled default (Lete) is missing.
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
