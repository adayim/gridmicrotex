test_that("geom_latex() creates a ggplot layer", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(x = 1:3, y = 1:3,
                   eq = c("x^2", "\\frac{a}{b}", "\\sum_{i=1}^n x_i"))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, label = eq)) + geom_latex()
  expect_s3_class(p, "gg")
  # Should build without error
  built <- ggplot2::ggplot_build(p)
  expect_true(nrow(built$data[[1]]) == 3)
})

test_that("geom_latex() renders without error", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(x = 1:3, y = 1:3,
                   eq = c("x^2", "\\frac{a}{b}", "\\sum_{i=1}^n x_i"))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, label = eq)) + geom_latex()
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_error(ggplot2::ggsave(tmp, p, width = 6, height = 4, dpi = 72))
})

test_that("geom_latex() accepts fontsize parameter", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(x = 1, y = 1, eq = "x^2")
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, label = eq)) +
    geom_latex(fontsize = 14)
  # fontsize is a layer param, default size aesthetic is unchanged
  built <- ggplot2::ggplot_build(p)
  expect_equal(nrow(built$data[[1]]), 1)
})

test_that("element_latex() creates an S7 element", {
  skip_if_not_installed("ggplot2")

  el <- element_latex()
  expect_true(inherits(el, "ggplot2::element_text"))
  expect_true(inherits(el, "S7_object"))
})

test_that("element_latex() renders axis title without error", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "$\\beta_1 \\cdot x + \\beta_0$") +
    ggplot2::theme(axis.title.x = element_latex())
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_error(ggplot2::ggsave(tmp, p, width = 6, height = 4, dpi = 72))
})

test_that("element_latex() merges with parent theme element", {
  skip_if_not_installed("ggplot2")

  el <- element_latex(size = 14)
  parent <- ggplot2::element_text(colour = "blue", size = 10)
  merged <- ggplot2::merge_element(el, parent)
  # Our size should win
  expect_equal(merged@size, 14)
  # Parent colour should be inherited
  expect_equal(merged@colour, "blue")
})

test_that("geom_latex() with annotation data works", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    geom_latex(
      data = data.frame(x = 4, y = 30, label = "\\hat{y} = \\beta x"),
      ggplot2::aes(x, y, label = label),
      fontsize = 12
    )
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_error(ggplot2::ggsave(tmp, p, width = 6, height = 4, dpi = 72))
})
