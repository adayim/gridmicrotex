# Fixed-width, wrapping table columns: the `p{len}` column type added to
# the vendored MicroTeX (atom_matrix.cpp). Before it, the spec parser knew
# only `l r c | @ * >` and `p{}` was a hard parse error, so a wide table
# could only overflow.

test_that("\\cline past the last column stays inside the table", {
  # \cline{2-2} on a one-column table read past the column widths (found by
  # the engine fuzzer); it clamps to the last column, as its end already did.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  t <- latex_tree("\\begin{array}{l}a\\\\ \\cline{2-2} b\\end{array}", input_mode = "math")
  rule <- t$records[t$records$type == "line", ]
  expect_equal(nrow(rule), 1L)
  expect_lte(rule$x2, t$bbox[["width"]] + 0.01)
})

test_that("an empty \\multirow and numbers past any range draw, not crash", {
  # All found by the engine fuzzer. An empty \multirow cell was a null atom
  # that crashed R when laid out; a number past an int's range was
  # converted as it was, which is undefined (\cline, \multirow's count).
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  cells <- function(tex) {
    r <- suppressWarnings(latex_tree(tex, input_mode = "math"))$records
    r$glyph[r$type == "glyph"]
  }
  expect_length(cells("\\begin{array}{ll}\\multirow{2}{*}{} & b \\\\ c & d\\end{array}"), 3L)
  expect_length(cells("\\begin{array}{ll}\\multirow{99999999999}{*}{x} & b \\\\ c & d\\end{array}"), 4L)
  t <- latex_tree("\\begin{array}{ll} a & b \\\\ \\cline{1e400-2} c & d\\end{array}", input_mode = "math")
  expect_equal(sum(t$records$type == "line"), 1)
})

test_that("p{} wraps a cell to a fixed measure", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  long <- "\\text{the quick brown fox jumps over the lazy dog again and again}"
  dims <- function(spec) {
    d <- latex_dims(paste0("\\begin{tabular}{", spec, "}", long,
                           "\\end{tabular}"),
                    input_mode = "math", gp = grid::gpar(fontsize = 16))
    c(w = as.numeric(d$width), h = as.numeric(d$height))
  }

  free <- dims("l")
  fixed <- dims("p{3cm}")
  # Narrower, and taller because it now occupies several lines.
  expect_lt(fixed[["w"]], free[["w"]] / 2)
  expect_gt(fixed[["h"]], free[["h"]] * 2)

  # The width tracks the request rather than the content.
  w2 <- dims("p{2cm}")[["w"]]
  w4 <- dims("p{4cm}")[["w"]]
  w6 <- dims("p{6cm}")[["w"]]
  expect_lt(w2, w4)
  expect_lt(w4, w6)
  # Equal steps in the request give equal steps in the result.
  expect_equal(w4 - w2, w6 - w4, tolerance = 2)

  # m{} and b{} differ from p{} only in vertical alignment, which the row
  # builder already handles, so they parse to the same measure.
  expect_equal(dims("m{3cm}")[["w"]], dims("p{3cm}")[["w"]])
  expect_equal(dims("b{3cm}")[["w"]], dims("p{3cm}")[["w"]])
})

test_that("a malformed p{} width leaves the column content-sized", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  ref <- as.numeric(latex_dims(
    "\\begin{tabular}{l}\\text{a}\\end{tabular}",
    input_mode = "math", gp = grid::gpar(fontsize = 16))$width)
  for (spec in c("p{}", "p{nonsense}", "p", "p{0pt}")) {
    w <- as.numeric(latex_dims(
      paste0("\\begin{tabular}{", spec, "}\\text{a}\\end{tabular}"),
      input_mode = "math", gp = grid::gpar(fontsize = 16))$width)
    expect_equal(w, ref, info = spec)
  }
})

test_that("p{} does not disturb ordinary tables or leak between parses", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  plain <- function() as.numeric(latex_dims(
    "\\begin{tabular}{|c|c|}\\hline \\text{a}&\\text{b}\\\\\\hline\\end{tabular}",
    input_mode = "math", gp = grid::gpar(fontsize = 16))$width)

  before <- plain()
  invisible(latex_dims(
    "\\begin{tabular}{p{3cm}}\\text{wrap me please}\\end{tabular}",
    input_mode = "math", gp = grid::gpar(fontsize = 16)))
  # A fixed width is per-column state; it must not survive into the next
  # parse the way a stray static would.
  expect_equal(plain(), before)
})

test_that("a tabular met in text has text cells, as in LaTeX", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  cells <- function(tex, ...) {
    r <- latex_tree(tex, ...)$records
    r$text[r$type == "text"]
  }
  tab <- "\\begin{tabular}{lr} Term & Estimate \\\\ \\hline Intercept & $\\beta_1$ \\end{tabular}"
  # A cell's words are one run of text, the spaces at either end of it
  # dropped; its math is math, between $...$.
  expect_identical(cells(tab), c("Term", "Estimate", "Intercept"))
  expect_true(any(latex_tree(tab)$records$type == "glyph"))
  expect_identical(cells(tab, input_mode = "document"), c("Term", "Estimate", "Intercept"))
  # What \multicolumn spans is a cell too.
  expect_identical(cells("\\begin{tabular}{ll}\\multicolumn{2}{c}{Both columns}\\end{tabular}"),
                   "Both columns")
  # Met in math, a tabular is an array, as it always was.
  expect_length(cells("\\begin{tabular}{l}ab\\end{tabular}", input_mode = "math"), 0L)
})

test_that("a \\multirow reaching past the table's edge ends there", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Its span was written past the row heights (a crash).
  glyphs <- function(tex) sum(latex_tree(tex, input_mode = "math")$records$type == "glyph")
  expect_equal(glyphs("\\begin{array}{cc} a & b \\\\ \\multirow{3}{*}{\\frac{x}{y}} & c \\end{array}"), 5)
  expect_equal(glyphs("\\begin{array}{cc} \\multirow{-3}{*}{\\frac{x}{y}} & c \\end{array}"), 3)
})

test_that("a column type defined in terms of itself, or a count past any table, ends", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_warning(latex_tree("\\newcolumntype{Y}{Y}\\begin{array}{Y}a\\end{array}", input_mode = "math"),
                 "expands without end")
  expect_warning(latex_tree("\\begin{array}{*{9999999999}{c}}a\\end{array}", input_mode = "math"),
                 "expands without end")
  t <- latex_tree("\\begin{array}{cc}\\hdotsfor{10000000}\\\\a&b\\end{array}", input_mode = "math")
  expect_lt(nrow(t$records), 1e5)
})

test_that("\\multicolumn{1} spans its own column only", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  r <- latex_tree("\\begin{tabular}{ll}\\multicolumn{1}{c}{Head} & Two \\\\ aaaa & bbbb\\end{tabular}")$records
  expect_equal(r$x[r$text %in% "Two"], r$x[r$text %in% "bbbb"])
})

test_that("a tabular's [t|b|c] is its position, and tabular* drops its width", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(latex_tree("\\begin{tabular}[t]{ll} a & b \\end{tabular}")$records,
                   latex_tree("\\begin{tabular}{ll} a & b \\end{tabular}")$records)
  expect_no_warning(t <- latex_tree("\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}ll}a&b\\end{tabular*}"))
  expect_identical(t$records$text[t$records$type == "text"], c("a", "b"))
})

# The release / re-init half of this lives in test-zz-release-cycle.R:
# tearing MicroTeX down mid-suite strands the font registry that
# test-text-font-auto.R depends on.

