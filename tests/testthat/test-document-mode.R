# input_mode = "document": a LaTeX document body, by TeX's rules with no
# exception. A line end is a space, a blank line starts a paragraph, and a
# paragraph indents its first line.

runs <- function(tex, mode = "document", ...) {
  t <- latex_tree(tex, input_mode = mode, ...)
  r <- t$records[t$records$type == "text", c("text", "x", "y")]
  r[order(r$y, r$x), ]
}

test_that("a line end is a space, as TeX reads it", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # One line, and one run of text, so the device shapes "one two" whole.
  d <- runs("one\ntwo")
  expect_identical(nrow(d), 1L)
  expect_identical(d$text, "one two")
  # The same input in a label is two lines: mixed mode's one departure
  # from TeX.
  m <- runs("one\ntwo", mode = "mixed")
  expect_identical(m$text, c("one", "two"))
  expect_false(m$y[1] == m$y[2])
})

test_that("a blank line starts a paragraph, however many there are", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  one <- runs("one\n\ntwo")
  expect_identical(nrow(one), 2L)
  expect_false(one$y[1] == one$y[2])
  # Two blank lines are one paragraph break, not two.
  expect_identical(runs("one\n\n\n\ntwo")$y, one$y)
  # \par is the same break spelled out.
  expect_identical(runs("one\\par two")$y, one$y)
})

test_that("a paragraph indents its first line by TeX's \\parindent", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # 1.5em, which is TeX's 15pt at a 10pt font, and follows the font size.
  for (fs in c(8, 12, 20)) {
    gp <- grid::gpar(fontsize = fs)
    indent <- latex_dims("\\kern1.5em", gp = gp)$width
    d <- runs("one\n\ntwo", gp = gp)
    # Every paragraph is indented, the first one included.
    expect_equal(d$x, c(indent, indent), tolerance = 1e-5, info = fs)
  }
  # A label is not a document: no indent there.
  expect_identical(runs("one\ntwo", mode = "mixed")$x, c(0, 0))
})

test_that("math and environments are read in a document as anywhere else", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(nrow(runs("Total $x^2$ here")), 2L)  # "Total " and " here"
  d <- latex_tree("a\n\n$$x$$\n\nb", input_mode = "document")
  expect_true(any(d$records$type == "text"))
  # A display formula is its own row, between the paragraphs.
  ys <- sort(unique(d$records$y))
  expect_identical(length(ys), 3L)
})

test_that("latex_options and the grob functions accept the document mode", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  on.exit(reset_latex_options(), add = TRUE)
  expect_silent(latex_options(input_mode = "document"))
  expect_identical(latex_dims("one\ntwo")$height, latex_dims("one two")$height)
  reset_latex_options()
  expect_error(latex_options(input_mode = "paragraph"), "should be one of")
})
