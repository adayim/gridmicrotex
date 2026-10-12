# input_mode = "document": a LaTeX document body, by TeX's rules. How the
# engine sets one is tested with the engine, in the MicroTeX fork
# (test/test_document_mode.cpp); these check the R side.

test_that("latex_options and the grob functions accept the document mode", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  on.exit(reset_latex_options(), add = TRUE)
  expect_silent(latex_options(input_mode = "document"))
  expect_identical(latex_dims("one\ntwo")$height, latex_dims("one two")$height)
  reset_latex_options()
  expect_error(latex_options(input_mode = "paragraph"), "should be one of")
})

test_that("a minipage's body is paragraphs, so markdown does not take it for math", {
  expect_false("minipage" %in% .math_envs())
})

test_that("a document's text width follows the grob's gpar fontsize", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # A paragraph's indent is 1.5em of the size the label is set in.
  for (fs in c(8, 12, 20)) {
    gp <- grid::gpar(fontsize = fs)
    indent <- latex_dims("\\kern1.5em", gp = gp)$width
    t <- latex_tree("one\n\ntwo", input_mode = "document", gp = gp)
    expect_equal(t$records$x[t$records$type == "text"], c(indent, indent),
                 tolerance = 1e-5, info = fs)
  }
})
