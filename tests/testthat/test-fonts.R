test_that("math fonts load, resolve aliases, and render", {
  # available_math_fonts lists bundled fonts
  fonts <- available_math_fonts()
  expect_true("LatinModernMath-Regular" %in% fonts)
  expect_true("STIX Two Math" %in% fonts)
  expect_true("Lete Sans Math" %in% fonts)
  expect_true("TeXGyreDejaVuMath-Regular" %in% fonts)

  # resolve_math_font aliases (case-insensitive)
  expect_equal(gridmicrotex:::resolve_math_font("stix"), "STIX Two Math")
  expect_equal(gridmicrotex:::resolve_math_font("lete"), "Lete Sans Math")
  expect_equal(gridmicrotex:::resolve_math_font("dejavu"), "TeXGyreDejaVuMath-Regular")
  expect_equal(gridmicrotex:::resolve_math_font("texgyre"), "TeXGyreDejaVuMath-Regular")
  expect_equal(gridmicrotex:::resolve_math_font("LM"), "LatinModernMath-Regular")
  expect_error(gridmicrotex:::resolve_math_font("nonexistent"), "not found")

  # set_math_font with alias + error cases
  expect_true(set_math_font("stix"))
  expect_error(set_math_font(""), "provide a math font")

  # Rendering with a specific math font
  g <- latex_grob("\\frac{a}{b}", fontsize = 20, math_font = "stix")
  expect_s3_class(g, "latexgrob")
})
