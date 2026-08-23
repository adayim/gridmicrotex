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

  # Add the bundled STIX Two Math font to MicroTeX's registry. The
  # systemfonts registration (so gp$fontfamily = "lete"/"stix" resolves
  # to the bundled OTF) is deferred to first render — see
  # .ensure_bundled_fonts_registered() in R/fonts.R for why.
  stix_path <- system.file("fonts", "STIXTwoMath-Regular.otf", package = pkgname)
  if (nzchar(stix_path)) {
    microtex_add_font_from_otf(stix_path, 0L)
  }

  # Initialize ggplot2 integration if ggplot2 is available
  .onLoad_ggplot2()
}

.onAttach <- function(libname, pkgname) {
  # Warn only if the bundled default (Lete) is missing.
  if (!"Lete Sans Math" %in% microtex_math_font_names()) {
    packageStartupMessage(
      "gridmicrotex: bundled default math font (Lete Sans Math) failed ",
      "to register. Run check_math_fonts() for details."
    )
  }
}

.onUnload <- function(libpath) {
  microtex_release()
  # Hand the shared object back as well. R does not do this for us, and
  # R_unload_gridmicrotex() (src/init.cpp) -- which frees the macro
  # registries and releases the preserved text-measurer callback -- runs
  # from nowhere else.
  library.dynam.unload("gridmicrotex", libpath)
}
