# --- geom_latex() ---

test_that("geom_latex creates layers and renders with various options", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(x = 1:3, y = 1:3,
                   eq = c("x^2", "\\frac{a}{b}", "\\sum_{i=1}^n x_i"))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, label = eq)) + geom_latex()
  expect_s3_class(p, "gg")

  # Renders without error (also covers fontsize, path mode, math_font)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_error(ggplot2::ggsave(tmp, p, width = 6, height = 4, dpi = 72))

  # NA label handling
  df_na <- data.frame(x = 1:2, y = 1:2, eq = c("x^2", NA))
  p_na <- ggplot2::ggplot(df_na, ggplot2::aes(x, y, label = eq)) +
    geom_latex(render_mode = "path", na.rm = TRUE)
  expect_no_error(ggplot2::ggsave(tmp, p_na, width = 6, height = 4, dpi = 72))
})

# --- annotate('latex') ---

test_that("annotate('latex') renders on a plot", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::annotate("latex", x = 4, y = 30,
                      label = "\\hat{y} = \\beta_0 + \\beta_1 x",
                      size = 12, colour = "red")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_error(ggplot2::ggsave(tmp, p, width = 6, height = 4, dpi = 72))
})

# --- element_latex() ---

test_that("element_latex works as theme element", {
  skip_if_not_installed("ggplot2")

  el <- element_latex(fontsize = 18, render_mode = "path")
  expect_true(inherits(el, "S7_object"))
  expect_equal(el@render_mode, "path")

  # Merge with parent
  merged <- ggplot2::merge_element(el, ggplot2::element_text(colour = "blue"))
  expect_equal(merged@colour, "blue")

  # Renders axis title + labels
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "$\\beta_1 \\cdot x + \\beta_0$") +
    ggplot2::scale_x_continuous(labels = function(x) paste0("$", x, "^2$")) +
    ggplot2::theme(axis.title.x = element_latex(),
                   axis.text.x = element_latex())
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_error(ggplot2::ggsave(tmp, p, width = 6, height = 4, dpi = 72))
})

# --- .element_grob_latex() internals ---

test_that(".element_grob_latex handles edge cases and multiple labels", {
  skip_if_not_installed("ggplot2")

  # NULL/empty → nullGrob
  expect_true(inherits(
    gridmicrotex:::.element_grob_latex(element_latex(), label = NULL), "null"))
  expect_true(inherits(
    gridmicrotex:::.element_grob_latex(element_latex(), label = ""), "null"))

  # Single label + dollar stripping
  result <- gridmicrotex:::.element_grob_latex(element_latex(), label = "$x^2$")
  expect_s3_class(result, "latexgrob")

  # Multiple labels with NA/empty skipped
  result_multi <- gridmicrotex:::.element_grob_latex(
    element_latex(), label = c("x_1", NA, "", "x_4"))
  expect_equal(length(result_multi$children), 2)
})
