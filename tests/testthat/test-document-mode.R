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

test_that("a heading is numbered as LaTeX numbers it", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  d <- runs("\\section{A}\n\n\\subsection{B}\n\n\\subsubsection{C}")
  expect_identical(d$text, c("1", "A", "1.1", "B", "1.1.1", "C"))
  # A counter carries on, and a lower one restarts under it.
  d <- runs("\\section{A}\n\n\\subsection{B}\n\n\\section{C}\n\n\\subsection{D}")
  expect_identical(d$text[d$text %in% c("1", "1.1", "2", "2.1")], c("1", "1.1", "2", "2.1"))
  # A starred heading has no number and does not advance the counter, and
  # \paragraph is below article's secnumdepth, so it has none either.
  expect_identical(runs("\\section*{A}\n\n\\section{B}")$text, c("A", "1", "B"))
  # The space after the run-in heading is the one written after its `}`.
  expect_identical(runs("\\paragraph{P} text")$text, c("P", " text"))
})

test_that("a heading is bold, sized and on a line of its own", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  t <- latex_tree("\\section{A}\n\n\\subsection{B}\n\n\\subsubsection{C}\n\nbody",
                  input_mode = "document")
  r <- t$records[t$records$type == "text", ]
  body <- r$font_size[r$text == "body"]
  # LaTeX's own sizes: \Large, \large, then the body size.
  expect_equal(r$font_size[r$text == "A"] / body, 1.4, tolerance = 1e-6)
  expect_equal(r$font_size[r$text == "B"] / body, 1.2, tolerance = 1e-6)
  expect_equal(r$font_size[r$text == "C"] / body, 1.0, tolerance = 1e-6)
  # Each heading has a row to itself, and is never indented: its row opens
  # with the number, flush left.
  expect_identical(length(unique(r$y)), 4L)
  expect_identical(r$x[r$text == "1"], 0)
  # \paragraph is the exception: LaTeX runs it into its paragraph.
  p <- runs("\\paragraph{P} text")
  expect_identical(p$y[1], p$y[2])
})

test_that("the paragraph after a heading is not indented, as in LaTeX", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  indent <- as.numeric(latex_dims("\\kern1.5em")$width)
  # However it is separated from the heading -- a line end or a blank line
  # -- the first paragraph is flush, and the one after it is indented.
  for (tex in c("\\section{H}\nfirst\n\nsecond", "\\section{H}\n\nfirst\n\nsecond")) {
    d <- runs(tex)
    expect_identical(d$x[d$text == "first"], 0, info = tex)
    expect_equal(d$x[d$text == "second"], indent, tolerance = 1e-5, info = tex)
  }
  # \noindent says so for any paragraph.
  d <- runs("one\n\n\\noindent two")
  expect_equal(d$x, c(indent, 0), tolerance = 1e-5)
})

test_that("latex_options and the grob functions accept the document mode", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  on.exit(reset_latex_options(), add = TRUE)
  expect_silent(latex_options(input_mode = "document"))
  expect_identical(latex_dims("one\ntwo")$height, latex_dims("one two")$height)
  reset_latex_options()
  expect_error(latex_options(input_mode = "paragraph"), "should be one of")
})
