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
  additional <- list(
    "Latin Modern Math"    = c("latinmodern-math.clm2",       "latinmodern-math.otf"),
    "STIX Two Math"        = c("STIXTwoMath-Regular.clm2",    "STIXTwoMath-Regular.otf"),
    "TeX Gyre DejaVu Math" = c("texgyredejavu-math.clm2",     "texgyredejavu-math.otf")
  )
  for (nm in names(additional)) {
    files <- additional[[nm]]
    f_clm <- system.file("fonts", files[1], package = pkgname)
    f_otf <- system.file("fonts", files[2], package = pkgname)
    if (nchar(f_clm) > 0 && nchar(f_otf) > 0) {
      microtex_add_font(f_clm, f_otf)
    }
  }

}

.onAttach <- function(libname, pkgname) {
  # Any bundled font that didn't register at load time gets a startup
  # message so users notice a broken install instead of silently missing
  # aliases (e.g. `math_font = "stix"`).
  loaded <- microtex_math_font_names()
  expected <- c(
    "Lete Sans Math"       = "Lete Sans Math",
    "Latin Modern Math"    = "LatinModernMath-Regular",
    "STIX Two Math"        = "STIX Two Math",
    "TeX Gyre DejaVu Math" = "TeXGyreDejaVuMath-Regular"
  )
  missing_names <- names(expected)[!expected %in% loaded]
  if (length(missing_names) > 0) {
    packageStartupMessage(
      "gridmicrotex: bundled math font(s) failed to register: ",
      paste(missing_names, collapse = ", "),
      ". Run check_fonts() for details."
    )
  }
}

.onUnload <- function(libpath) {
  microtex_release()
}
