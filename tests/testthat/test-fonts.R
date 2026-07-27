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

  # Switching the math font has to be undone explicitly, and neither
  # do.call(latex_options, old) nor reset_latex_options() is enough:
  #
  #  * latex_options() ignores NULL arguments -- that is how it separates
  #    "leave this alone" from "set it" -- so replaying an all-NULL `old`
  #    (the state when nothing has been set yet) restores nothing.
  #  * latex_options(math_font=) also calls .set_math_font(), which sets
  #    MicroTeX's default font in C++. reset_latex_options() only clears
  #    the R-side list, leaving the engine still on STIX.
  #
  # Left unrestored, every later test file renders in the wrong font --
  # which is exactly how the visual snapshots came to be recorded against
  # STIX rather than the default Lete.
  latex_options(math_font = "stix")
  expect_equal(latex_options()$math_font, "stix")

  latex_options(math_font = "lete")  # restores the engine default
  reset_latex_options()              # and clears the R-side record
  expect_null(latex_options()$math_font)

  g <- latex_grob("\\frac{a}{b}", math_font = "stix",
                  gp = grid::gpar(fontsize = 20))
  expect_s3_class(g, "latexgrob")
})
