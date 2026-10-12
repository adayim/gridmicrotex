# Mixed mode (a plot label: prose, math in `$...$`, a line end as a line break)
# is tested with the engine, in the MicroTeX fork (test/test_labels.cpp).
# This checks latex_wrap(), which wraps a label for math mode, against it;
# test-latex-wrap.R covers latex_wrap() itself.

records <- function(tex, mode = "mixed") {
  latex_tree(tex, input_mode = mode, render_mode = "typeface")$records
}

test_that("a label lays out as its wrapped form did", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  for (s in c("Hello $x^2$ world", "Title\nSubtitle", "a\\\\b", "a \\\\ b",
              "\\textbf{one\\\\two} $x$", "\\textbf{one\ntwo}", "$$\\sum_i x_i$$",
              "Cost: \\$100 for $x$ items",
              "\\textcolor{red}{a\nb} c", "{a\\\\b}", "\n\nTitle\n\n", "a\n\n\nb",
              "$a\\\\b$ c", "\\begin{matrix}a&b\\end{matrix} after",
              "A \\(x\\) B", "A \\[x\\] B")) {
    expect_identical(records(s), suppressWarnings(records(latex_wrap(s), "math")), info = s)
  }
})

test_that("a list is not wrapped in \\text{}", {
  expect_false(any(grepl("\\\\text\\{", latex_wrap("\\begin{itemize}\\item a\\end{itemize}"))))
})
