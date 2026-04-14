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

  # Load additional bundled math fonts
  lm_clm <- system.file("fonts", "latinmodern-math.clm2", package = pkgname)
  lm_otf <- system.file("fonts", "latinmodern-math.otf", package = pkgname)
  if (nchar(lm_clm) > 0 && nchar(lm_otf) > 0) {
    microtex_add_font(lm_clm, lm_otf)
  }

  stix_clm <- system.file("fonts", "STIXTwoMath-Regular.clm2", package = pkgname)
  stix_otf <- system.file("fonts", "STIXTwoMath-Regular.otf", package = pkgname)
  if (nchar(stix_clm) > 0 && nchar(stix_otf) > 0) {
    microtex_add_font(stix_clm, stix_otf)
  }

  dv_clm <- system.file("fonts", "texgyredejavu-math.clm2", package = pkgname)
  dv_otf <- system.file("fonts", "texgyredejavu-math.otf", package = pkgname)
  if (nchar(dv_clm) > 0 && nchar(dv_otf) > 0) {
    microtex_add_font(dv_clm, dv_otf)
  }
}

.onUnload <- function(libpath) {
  microtex_release()
}
