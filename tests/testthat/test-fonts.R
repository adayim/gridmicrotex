test_that("bundled default math font (Lete) loads and resolves", {
  fonts <- available_math_fonts()
  expect_true("Lete Sans Math" %in% fonts)

  expect_equal(gridmicrotex:::resolve_math_font("lete"), "Lete Sans Math")
  expect_equal(gridmicrotex:::resolve_math_font("letesans"), "Lete Sans Math")
  expect_error(gridmicrotex:::resolve_math_font("nonexistent"), "not found")

  g <- latex_grob("\\frac{a}{b}", math_font = "lete",
                  gp = grid::gpar(fontsize = 20))
  expect_s3_class(g, "latexgrob")
})

test_that("downloadable math fonts resolve aliases and render when cached", {
  skip_if_math_font_unavailable("stix")
  skip_if_math_font_unavailable("lm")
  skip_if_math_font_unavailable("dejavu")

  fonts <- available_math_fonts()
  expect_true("STIX Two Math" %in% fonts)
  expect_true("LatinModernMath-Regular" %in% fonts)
  expect_true("TeXGyreDejaVuMath-Regular" %in% fonts)

  expect_equal(gridmicrotex:::resolve_math_font("stix"), "STIX Two Math")
  expect_equal(gridmicrotex:::resolve_math_font("LM"), "LatinModernMath-Regular")
  expect_equal(gridmicrotex:::resolve_math_font("dejavu"), "TeXGyreDejaVuMath-Regular")
  expect_equal(gridmicrotex:::resolve_math_font("texgyre"), "TeXGyreDejaVuMath-Regular")

  old <- latex_options(math_font = "stix")
  expect_equal(latex_options()$math_font, "stix")
  do.call(latex_options, old)

  g <- latex_grob("\\frac{a}{b}", math_font = "stix",
                  gp = grid::gpar(fontsize = 20))
  expect_s3_class(g, "latexgrob")
})

test_that("download_math_font rejects unknown aliases", {
  expect_error(download_math_font("nope"), "Unknown downloadable math font")
  expect_error(download_math_font(""), "supply a font alias")
})
