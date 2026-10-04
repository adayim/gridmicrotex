# Shared by the font tests (test-fonts-named.R, test-fonts-commands.R). The
# bundled fonts stand in for "a font file" so that no test needs a file of its
# own.

stix <- function() {
  f <- system.file("fonts", "STIXTwoMath-Regular.otf", package = "gridmicrotex")
  skip_if_not(nzchar(f))
  f
}
lete <- function() {
  f <- system.file("fonts", "LeteSansMath.otf", package = "gridmicrotex")
  skip_if_not(nzchar(f))
  f
}

# The family each text run of a label is drawn in, named by its text.
drawn_families <- function(g) {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  kids <- grid::makeContent(g)$children
  txt <- Filter(function(k) inherits(k, "text"), unclass(kids))
  stats::setNames(
    vapply(txt, function(k) if (is.null(k$gp$fontfamily)) NA_character_ else k$gp$fontfamily, character(1)),
    vapply(txt, function(k) trimws(as.character(k$label)), character(1))
  )
}

# The family each text record of a label's layout carries, by its text: NA
# for a run that names none, which is then drawn in the body font or its role's
# default.
record_families <- function(tex, ...) {
  d <- latex_grob(tex, ...)$layout_df
  d <- d[d$type == "text" & nzchar(trimws(d$text)), ]
  stats::setNames(d$font_family, trimws(d$text))
}

# The font files the glyph records of a label are drawn from.
glyph_files <- function(tex, ...) {
  d <- latex_grob(tex, ...)$layout_df
  unique(basename(d$font_file[d$type == "glyph" & nzchar(d$font_file)]))
}

# A font file with no MATH table -- this machine's "sans" -- loaded as
# `name`; the test is skipped where there is none.
load_text_font <- function(name = "Text Only Probe") {
  file <- systemfonts::match_fonts("sans")$path
  skip_if(!nzchar(file) || !file.exists(file), "no installed font to load")
  loaded <- tryCatch(load_font(file, name = name), error = function(e) NULL)
  skip_if(is.null(loaded), "this machine's sans font cannot be loaded")
  skip_if(isTRUE(available_fonts()$math[available_fonts()$name == name]),
          "this machine's sans font has a math table")
  name
}

same_file <- function(a, b) {
  identical(normalizePath(a, winslash = "/"), normalizePath(b, winslash = "/"))
}
