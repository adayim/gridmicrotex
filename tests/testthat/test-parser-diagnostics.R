# Malformed input is drawn as far as it can be, and each problem is one line
# of a single warning, at its line:col. What is a problem, and what is drawn
# in spite of it, is tested with the engine, in the MicroTeX fork
# (test/test_diagnostics.cpp); these check how R reports them. Only running
# out of capacity is an error (see test-latex-grob.R).

test_that("one warning lists every problem, in order, at its line:col", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  w <- character()
  withCallingHandlers(
    latex_grob("a\n\\foo b\n\\bar", input_mode = "math"),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") }
  )
  expect_length(w, 1L)
  expect_equal(
    strsplit(w, "\n", fixed = TRUE)[[1]],
    c("LaTeX input: ",
      "  2:1: unknown command \\foo: drawn as its name",
      "  3:5: missing argument for \\bar: an empty one is used")
  )
})

test_that("a cached layout warns again", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  latex_cache_clear(); on.exit(latex_cache_clear(), add = TRUE)
  expect_warning(latex_grob("x^{2", input_mode = "math"), "1:3: missing } inserted")
  expect_warning(latex_grob("x^{2", input_mode = "math"), "1:3: missing } inserted")
})

test_that("valid input does not warn", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_warning(latex_grob(
    "\\begin{pmatrix} a & b \\\\ c & d \\end{pmatrix} + \\left( \\frac12 \\right)",
    input_mode = "math"
  ))
})
