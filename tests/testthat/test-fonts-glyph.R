# Text in a font that was loaded is measured by shaping it and drawn from the
# font's own glyphs, so it is the same on every device (R/font-glyph.R). What
# is not a loaded font stays with the device.

kinds <- function(g) {
  kids <- grid::makeContent(g)$children
  unname(vapply(unclass(kids), function(k) class(k)[1L], character(1)))
}

cairo_png <- function() grDevices::png(tempfile(fileext = ".png"), type = "cairo")

skip_if_no_glyphs <- function() {
  cairo_png()
  on.exit(grDevices::dev.off(), add = TRUE, after = FALSE)
  skip_if_not(isTRUE(grDevices::dev.capabilities()[["glyphs"]]),
              "this device cannot draw glyphs")
}

label <- function(tex, family = "Glyph Probe", ...) {
  latex_grob(tex, gp = grid::gpar(fontfamily = family, fontsize = 20), ...)
}

test_that("a loaded font's text is outlines on pdf() and glyphs where a device has them", {
  load_font(stix(), name = "Glyph Probe")
  g <- label("\\text{Hello} AV")

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  on_pdf <- kinds(g)
  expect_false("text" %in% on_pdf)
  expect_true("pathgrob" %in% on_pdf)
  dev.off()
  on.exit()

  skip_if_no_glyphs()
  cairo_png()
  on.exit(dev.off(), add = TRUE)
  expect_equal(kinds(g), "glyphgrob")
})

test_that("a label measures the same on every device", {
  skip_if_no_glyphs()
  load_font(stix(), name = "Glyph Probe")
  width <- function() {
    latex_cache_clear()
    grid::convertWidth(
      latex_dims("\\text{Hello world} AV fi", gp = grid::gpar(fontfamily = "Glyph Probe",
                                                              fontsize = 20))$width,
      "bigpts", valueOnly = TRUE)
  }
  pdf(NULL)
  on_pdf <- width()
  dev.off()
  cairo_png()
  on_cairo <- width()
  dev.off()
  expect_equal(on_pdf, on_cairo)
  expect_gt(on_pdf, 0)
})

test_that("glyphs and outlines put the text in the same place", {
  skip_if_no_glyphs()
  skip_if_not_installed("png")
  load_font(stix(), name = "Glyph Probe")
  ink <- function(mode) {
    f <- tempfile(fileext = ".png")
    grDevices::png(f, type = "cairo", width = 400, height = 100, res = 144)
    grid::grid.newpage()
    grid.latex("\\text{Hello} AV $x^2$", gp = grid::gpar(fontfamily = "Glyph Probe",
                                                          fontsize = 24),
               render_mode = mode)
    grDevices::dev.off()
    dark <- apply(png::readPNG(f)[, , 1:3, drop = FALSE], c(1, 2), min) < 0.5
    c(range(which(rowSums(dark) > 0)), range(which(colSums(dark) > 0)))
  }
  # The same ink to within the pixel an edge may gain or lose.
  expect_lte(max(abs(ink("typeface") - ink("path"))), 1)
})

test_that("a rotated run is drawn as outlines", {
  skip_if_no_glyphs()
  load_font(stix(), name = "Glyph Probe")
  cairo_png()
  on.exit(dev.off(), add = TRUE)
  expect_equal(kinds(label("\\rotatebox{90}{\\text{ab}}", render_mode = "typeface")),
               "pathgrob")
})

test_that("bold and italic take the registered face, or the regular one with a word", {
  skip_if_no_glyphs()
  load_font(stix(), name = "Glyph Probe Faces", bold = lete())
  fonts_of <- function(tex) {
    kid <- grid::makeContent(label(tex, "Glyph Probe Faces",
                                   render_mode = "typeface"))$children[[1L]]
    vapply(kid$glyphInfo$fonts, function(f) basename(f$file), character(1))
  }
  cairo_png()
  on.exit(dev.off(), add = TRUE)
  expect_equal(fonts_of("\\text{Hello}"), "STIXTwoMath-Regular.otf")
  expect_equal(fonts_of("\\textbf{Hello}"), "LeteSansMath.otf")

  # No italic file given: the regular face, and one warning.
  rm(list = ls(gridmicrotex:::.face_warned), envir = gridmicrotex:::.face_warned)
  expect_warning(f <- fonts_of("\\textit{Hello}"), "has no italic face")
  expect_equal(f, "STIXTwoMath-Regular.otf")
  expect_no_warning(fonts_of("\\textit{Hello}"))
})

test_that("what is not a loaded font stays with the device", {
  load_font(stix(), name = "Glyph Probe")
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  # An installed or generic family.
  expect_equal(kinds(label("\\text{Hello}", "serif")), "text")
  # Right-to-left text, whose order is the device's.
  expect_true("text" %in% kinds(label("\\text{abc אב}")))
  # A character the font has no glyph for, which the device may find elsewhere.
  expect_true("text" %in% kinds(label("\\text{a 中 b}")))
})

test_that("the shaping a run gets keeps its kerning, and a missing glyph is no shape", {
  load_font(stix(), name = "Glyph Probe")
  face <- gridmicrotex:::.registered_face("Glyph Probe", 0L)
  shaped <- gridmicrotex:::.shape_run("AV", face)
  alone <- gridmicrotex:::.shape_run("A", face)$advance +
    gridmicrotex:::.shape_run("V", face)$advance
  expect_lt(shaped$advance, alone)
  expect_null(gridmicrotex:::.shape_run("a中b", face))
  expect_null(gridmicrotex:::.measure_registered("a中b", face))
  m <- gridmicrotex:::.measure_registered("Ag", face)
  expect_gt(m[[1L]], 0)
  expect_gt(m[[3L]], m[[2L]])  # the g has a descender
})
