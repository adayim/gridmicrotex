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

  # Switching the math font also switches the engine's default, so it must
  # be undone, or every later test file renders in STIX.
  old <- latex_options(math_font = "stix")
  expect_equal(latex_options()$math_font, "stix")
  do.call(latex_options, old)
  expect_null(latex_options()$math_font)

  g <- latex_grob("\\frac{a}{b}", math_font = "stix",
                  gp = grid::gpar(fontsize = 20))
  expect_s3_class(g, "latexgrob")
})

test_that("restoring or resetting options puts the engine font back", {
  pdf(NULL)
  on.exit({
    latex_options(math_font = "lete")
    reset_latex_options()
    dev.off()
  }, add = TRUE)
  width <- function() {
    latex_cache_clear()
    grid::convertWidth(latex_dims("$\\sum_{i=1}^n x_i$")$width, "bigpts", TRUE)
  }
  reset_latex_options()
  lete <- width()

  # Replaying the settings latex_options() returned restores them, the
  # unset (NULL) ones included.
  old <- latex_options(math_font = "stix")
  expect_false(isTRUE(all.equal(width(), lete)))
  do.call(latex_options, old)
  expect_null(latex_options()$math_font)
  expect_equal(width(), lete)

  # reset_latex_options() resets the engine's font too.
  latex_options(math_font = "stix")
  reset_latex_options()
  expect_equal(width(), lete)
})
