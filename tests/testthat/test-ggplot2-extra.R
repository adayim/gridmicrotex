test_that("annotate_latex() creates a ggplot2 layer", {
  skip_if_not_installed("ggplot2")

  layer <- annotate_latex(x = 2, y = 3, label = "x^2 + y^2", fontsize = 12)
  expect_true(inherits(layer, "LayerInstance") || inherits(layer, "Layer"))
})

test_that("annotate_latex() renders on a plot without error", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    annotate_latex(x = 4, y = 30, label = "\\hat{y} = \\beta_0 + \\beta_1 x",
                   fontsize = 12)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_error(ggplot2::ggsave(tmp, p, width = 6, height = 4, dpi = 72))
})

test_that("annotate_latex() colour parameter works", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    annotate_latex(x = 4, y = 30, label = "x^2", colour = "red")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_error(ggplot2::ggsave(tmp, p, width = 6, height = 4, dpi = 72))
})

test_that(".element_grob_latex() returns nullGrob for NULL label", {
  skip_if_not_installed("ggplot2")

  result <- gridmicrotex:::.element_grob_latex(element_latex(), label = NULL)
  expect_s3_class(result, "grob")
  expect_true(inherits(result, "null"))
})

test_that(".element_grob_latex() returns nullGrob for empty string label", {
  skip_if_not_installed("ggplot2")

  result <- gridmicrotex:::.element_grob_latex(element_latex(), label = "")
  expect_s3_class(result, "grob")
  expect_true(inherits(result, "null"))
})

test_that(".element_grob_latex() handles single label correctly", {
  skip_if_not_installed("ggplot2")

  el <- element_latex()
  result <- gridmicrotex:::.element_grob_latex(el, label = "x^2")
  expect_s3_class(result, "latexgrob")
})

test_that(".element_grob_latex() strips dollar signs from label", {
  skip_if_not_installed("ggplot2")

  el <- element_latex()
  # Dollar signs should be stripped: "$x^2$" → "x^2"
  result_dollars <- gridmicrotex:::.element_grob_latex(el, label = "$x^2$")
  result_plain   <- gridmicrotex:::.element_grob_latex(el, label = "x^2")
  # Both should render valid grobs with the same dimensions
  expect_s3_class(result_dollars, "latexgrob")
  expect_equal(result_dollars$bbox_w, result_plain$bbox_w, tolerance = 1e-3)
})

test_that(".element_grob_latex() handles multiple labels (axis ticks)", {
  skip_if_not_installed("ggplot2")

  el <- element_latex()
  labels <- c("x_1", "x_2", "x_3")
  result <- gridmicrotex:::.element_grob_latex(el, label = labels)
  expect_s3_class(result, "gTree")
  expect_true(length(result$children) > 0)
})

test_that(".element_grob_latex() skips NA/empty labels in multiple-label case", {
  skip_if_not_installed("ggplot2")

  el <- element_latex()
  labels <- c("x_1", NA, "", "x_4")
  result <- gridmicrotex:::.element_grob_latex(el, label = labels)
  expect_s3_class(result, "gTree")
  # Only x_1 and x_4 are non-NA/non-empty; so 2 children
  expect_equal(length(result$children), 2)
})

test_that("geom_latex() with render_mode = 'path' renders without error", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(x = 1:2, y = 1:2, eq = c("x^2", "\\frac{a}{b}"))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, label = eq)) +
    geom_latex(render_mode = "path")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_error(ggplot2::ggsave(tmp, p, width = 6, height = 4, dpi = 72))
})

test_that("geom_latex() with NA label produces no error", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(x = 1:2, y = 1:2, eq = c("x^2", NA))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, label = eq)) +
    geom_latex(render_mode = "path", na.rm = TRUE)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_error(ggplot2::ggsave(tmp, p, width = 6, height = 4, dpi = 72))
})

test_that("geom_latex() with math_font parameter works", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(x = 1, y = 1, eq = "x^2")
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, label = eq)) +
    geom_latex(math_font = "stix", render_mode = "path")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_error(ggplot2::ggsave(tmp, p, width = 4, height = 3, dpi = 72))
})

test_that("element_latex() with fontsize parameter sets size", {
  skip_if_not_installed("ggplot2")

  el <- element_latex(fontsize = 18)
  expect_equal(el@size, 18)
})

test_that("element_latex() with render_mode = 'path' works", {
  skip_if_not_installed("ggplot2")

  el <- element_latex(render_mode = "path")
  expect_equal(el@render_mode, "path")
})

test_that("element_latex() renders axis labels without error", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0("$", x, "^2$")
    ) +
    ggplot2::theme(axis.text.x = element_latex())
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_error(ggplot2::ggsave(tmp, p, width = 6, height = 4, dpi = 72))
})
