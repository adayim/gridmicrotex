# Fixed-width, wrapping table columns: the `p{len}` column type added to
# the vendored MicroTeX (atom_matrix.cpp). Before it, the spec parser knew
# only `l r c | @ * >` and `p{}` was a hard parse error, so a wide table
# could only overflow.

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

# The release / re-init half of this lives in test-zz-release-cycle.R:
# tearing MicroTeX down mid-suite strands the font registry that
# test-text-font-auto.R depends on.

