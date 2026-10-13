# Line breaking -- wrapping, justification, optimal breaking -- is tested with
# the engine, in the MicroTeX fork (test/test_split.cpp). These check the R
# side: the arguments and options, the cache, and that a parse leaves no
# setting behind for the next.

just_para <- paste0(
  "\\text{",
  paste(rep("The quick brown fox jumps over the lazy dog.", 4), collapse = " "),
  "}"
)

test_that("the justify flag does not leak into later parses", {
  # It is a global on the splitter.
  before <- as.numeric(latex_dims(just_para, max_width = 300)$width)
  invisible(latex_dims(just_para, max_width = 300, justify = TRUE))
  after <- as.numeric(latex_dims(just_para, max_width = 300)$width)
  expect_equal(after, before, tolerance = 1e-6)
  expect_lt(after, 300)  # still ragged
})

test_that("justified and ragged layouts are cached separately", {
  latex_cache_clear()
  r <- as.numeric(latex_dims(just_para, max_width = 300, justify = FALSE)$width)
  j <- as.numeric(latex_dims(just_para, max_width = 300, justify = TRUE)$width)
  expect_lt(r, j)
  expect_equal(j, 300, tolerance = 1e-6)
})

test_that("justify is validated and settable as an option", {
  expect_error(latex_dims("x", justify = "yes"), "TRUE or FALSE")
  expect_error(latex_dims("x", justify = NA), "TRUE or FALSE")
  expect_error(latex_dims("x", justify = c(TRUE, FALSE)), "TRUE or FALSE")

  latex_options(justify = TRUE)
  expect_true(latex_options()$justify)
  expect_equal(as.numeric(latex_dims(just_para, max_width = 300)$width), 300,
               tolerance = 1e-6)
  reset_latex_options()
  expect_null(latex_options()$justify)
})

test_that("line_break is validated, settable and does not leak", {
  expect_error(latex_dims("x", line_break = "nope"))

  before <- as.numeric(latex_dims(just_para, max_width = 300)$width)
  invisible(latex_dims(just_para, max_width = 300, line_break = "optimal"))
  expect_equal(as.numeric(latex_dims(just_para, max_width = 300)$width),
               before, tolerance = 1e-6)

  latex_options(line_break = "optimal")
  expect_equal(latex_options()$line_break, "optimal")
  reset_latex_options()
  expect_null(latex_options()$line_break)
})

test_that("greedy and optimal layouts are cached separately", {
  latex_cache_clear()
  a <- latex_dims(just_para, max_width = 300, line_break = "greedy")
  b <- latex_dims(just_para, max_width = 300, line_break = "optimal")
  expect_false(identical(a$is_split, NA))
  expect_equal(latex_cache_info()$size, 2L)
})
