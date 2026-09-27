# Mixed mode is read by the parser itself: prose, math in `$...$` and its
# kin, and a line end in the prose as a line break. These pin down what
# that reading does; test-latex-wrap.R covers latex_wrap(), which markdown
# still uses.

records <- function(tex, mode = "mixed") {
  latex_tree(tex, input_mode = mode, render_mode = "typeface")$records
}

texts <- function(tex) {
  r <- records(tex)
  r[r$type == "text", ]
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
  # Except a tabular's cells, which are text in a label, as in LaTeX, where
  # the wrapped form kept them math.
  s <- "x\n\\begin{tabular}{l}p q\\end{tabular}"
  expect_identical(texts(s)$text, c("x", "p q"))
})

test_that("a span of math is a list of its own, as in TeX", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # The wrapped form set it in the prose's list: a sign opening it was
  # binary, and \over took the prose as its numerator.
  x_of <- function(tex) { r <- records(tex); r$x[r$type == "glyph"] }
  expect_equal(diff(x_of("The value $-x$ here")), diff(x_of("$-x$")), tolerance = 1e-4)
  r <- records("Ratio: $a \\over b$ end")
  expect_identical(r$font_size[r$type == "text"], c(20, 20))
  # And a line end in the prose ends \over's denominator there.
  expect_identical(texts("a \\over b\nnext line")$text, c("a ", "next line"))
})

test_that("a line end in the prose breaks the line, even after a command", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  r <- texts("Price \\euro\nnext")
  expect_equal(r$text[2], "next")
  expect_gt(r$y[2], r$y[1])
  # A comment takes its line end with it, as in TeX.
  expect_equal(texts("a% note\nb")$text, "ab")
  # In math a line end is a space, as in TeX.
  expect_length(unique(records("$a\nb$")$y), 1)
})

test_that("a break inside a group ends the line, and the group goes on", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  r <- texts("\\textbf{one\ntwo} three")
  expect_equal(r$text, c("one", "two", " three"))
  expect_equal(r$font_style[1:2], r$font_style[c(1, 1)])
  expect_false(r$font_style[3] == r$font_style[1])
  expect_equal(length(unique(r$y)), 2)
  # \color runs to the end of its group, across the break, as in TeX.
  r <- texts("{\\color{red} a\nb} c")
  expect_equal(r$color, c("#FF0000", "#FF0000", "#000000"))
})

test_that("breaks at either end are none, and a run of them is one", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_length(unique(texts("\n\nTitle\n\n")$y), 1)
  expect_length(unique(texts("a\n\n\nb")$y), 2)
  expect_length(unique(texts("a \\\\ \\\\ b")$y), 2)
  # A line with nothing drawn on it is no line.
  one <- latex_dims("$x$")
  defined <- latex_dims("\\newcommand{\\foo}{x}\n$\\foo$")
  expect_equal(defined$height, one$height)
})

test_that("\\\\[len] adds its space below the line, in both modes", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  for (mode in c("mixed", "math")) {
    plain <- records("a\\\\b", mode)$y
    gap <- records("a\\\\[10pt]b", mode)$y
    expect_equal(diff(gap) - diff(plain), 9.96, tolerance = 0.01, info = mode)
  }
})

test_that("problems are reported where they are in the label", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_warning(latex_dims("a $x$ \\nosuch b"), "1:7: unknown command \\\\nosuch")
  expect_warning(latex_dims("Revenue ($)"), "1:10: missing \\$ inserted")
  expect_warning(latex_dims("\\textit{a $b$ c}\n\\bad"), "2:1: unknown command \\\\bad")
})

test_that("_ ^ # & in prose are drawn and warned about, as TeX refuses them", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_warning(r <- texts("x^2"), "1:2: \\^ outside math is drawn as a character")
  expect_equal(r$text, "x^2")
  expect_warning(texts("a_b"), "1:2: _ outside math")
  expect_warning(texts("# Title"), "1:1: macro parameter #")
  # Escaped, they are characters of the text, with nothing to warn about.
  expect_no_warning(r <- texts("a \\& b \\% c \\$ d \\# e \\_ f"))
  expect_equal(r$text, "a & b % c $ d # e _ f")
})

test_that("~ at the start of a line is a space that stays", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  r <- texts("a\n~b")
  expect_equal(r$text[2], " b")
})

test_that("a number or a dimension takes one optional space, as in TeX", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  text_of <- function(tex) {
    t <- latex_tree(tex, input_mode = "mixed")
    t$records[t$records$type == "text", c("text", "x", "y")]
  }
  # The space ends the dimension and goes with it; a second one is a space.
  expect_identical(text_of("\\kern100bp word")[, c("text", "x")],
                   data.frame(text = "word", x = 100))
  expect_identical(text_of("\\kern100bp\\ word")$text, " word")
  # In a label, a line end there still breaks the line.
  r <- text_of("a\\kern10bp\nb")
  expect_identical(r$text, c("a", "b"))
  expect_false(r$y[1] == r$y[2])
})
