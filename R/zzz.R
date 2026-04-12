.onLoad <- function(libname, pkgname) {
  # Initialize MicroTeX with Latin Modern Math as the default font
  clm_path <- system.file("fonts", "latinmodern-math.clm2", package = pkgname)
  otf_path <- system.file("fonts", "latinmodern-math.otf", package = pkgname)

  if (nchar(clm_path) == 0 || nchar(otf_path) == 0) {
    warning("gridmicrotex: Latin Modern Math font files not found, LaTeX rendering will not work")
    return()
  }

  microtex_init(clm_path, otf_path)

  # Initialize ggplot2 integration if ggplot2 is available
  .onLoad_ggplot2()

  # Load additional bundled math fonts
  xits_clm <- system.file("fonts", "XITSMath-Regular.clm2", package = pkgname)
  xits_otf <- system.file("fonts", "XITSMath-Regular.otf", package = pkgname)
  if (nchar(xits_clm) > 0 && nchar(xits_otf) > 0) {
    microtex_add_font(xits_clm, xits_otf)
  }

  dv_clm <- system.file("fonts", "texgyredejavu-math.clm2", package = pkgname)
  dv_otf <- system.file("fonts", "texgyredejavu-math.otf", package = pkgname)
  if (nchar(dv_clm) > 0 && nchar(dv_otf) > 0) {
    microtex_add_font(dv_clm, dv_otf)
  }

  gm_clm <- system.file("fonts", "Garamond-Math.clm2", package = pkgname)
  gm_otf <- system.file("fonts", "Garamond-Math.otf", package = pkgname)
  if (nchar(gm_clm) > 0 && nchar(gm_otf) > 0) {
    microtex_add_font(gm_clm, gm_otf)
  }
}

.onUnload <- function(libpath) {
  microtex_release()
}
