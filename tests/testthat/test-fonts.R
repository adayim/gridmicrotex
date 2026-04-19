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

test_that("bundled STIX math font loads, resolves aliases, and renders", {
  fonts <- available_math_fonts()
  expect_true("STIX Two Math" %in% fonts)

  expect_equal(gridmicrotex:::resolve_math_font("stix"), "STIX Two Math")
  expect_equal(gridmicrotex:::resolve_math_font("stix2"), "STIX Two Math")

  old <- latex_options(math_font = "stix")
  expect_equal(latex_options()$math_font, "stix")
  do.call(latex_options, old)

  g <- latex_grob("\\frac{a}{b}", math_font = "stix",
                  gp = grid::gpar(fontsize = 20))
  expect_s3_class(g, "latexgrob")
})
