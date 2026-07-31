# geom_latex(), annotate("latex") and element_latex() are adapters: they
# hand ggplot2 a latex_grob(). So the thing worth asserting is that a grob
# of ours, carrying what the caller asked for, actually lands in the built
# plot. Saving a png and checking it did not error says nothing -- it
# passes just as happily when the layer draws nothing at all.

skip_if_not_installed("ggplot2")

# The grobs a layer contributes to the panel.
layer_grobs <- function(p, i = 1L) ggplot2::layer_grob(p, i)[[1]]

# --- geom_latex() ---

test_that("geom_latex contributes one rendered grob per row", {
  df <- data.frame(x = 1:3, y = 1:3,
                   eq = c("x^2", "\\frac{a}{b}", "\\sum_{i=1}^n x_i"))
  gs <- layer_grobs(ggplot2::ggplot(df, ggplot2::aes(x, y, label = eq)) +
                      geom_latex())
  expect_length(gs, 3L)
  expect_true(all(vapply(gs, inherits, logical(1), "latexgrob")))
  # Each one has something to draw. A layer that silently produced empty
  # grobs would still be a gList of the right length.
  expect_true(all(vapply(gs, function(g) nrow(g$layout_df) > 0, logical(1))))
})

test_that("geom_latex passes colour and size down to the grob", {
  df <- data.frame(x = 1, y = 1, eq = "x^2")
  base <- ggplot2::ggplot(df, ggplot2::aes(x, y, label = eq))

  red <- layer_grobs(base + geom_latex(colour = "red"))[[1]]
  expect_equal(red$gp$col, "red")

  big   <- layer_grobs(base + geom_latex(fontsize = 20))[[1]]
  small <- layer_grobs(base + geom_latex(fontsize = 8))[[1]]
  expect_equal(unique(big$layout_df$font_size), 20)
  expect_equal(unique(small$layout_df$font_size), 8)
  expect_gt(big$bbox_w, small$bbox_w)
})

test_that("geom_latex draws nothing for an empty or missing label", {
  # Without the guard in draw_panel these render the literal "NA", or an
  # empty formula that still reserves space.
  df <- data.frame(x = 1:2, y = 1:2, eq = c("x^2", ""))
  gs <- layer_grobs(ggplot2::ggplot(df, ggplot2::aes(x, y, label = eq)) +
                      geom_latex())
  expect_s3_class(gs[[1]], "latexgrob")
  expect_s3_class(gs[[2]], "null")

  # na.rm drops the row before it ever reaches us.
  df_na <- data.frame(x = 1:2, y = 1:2, eq = c("x^2", NA))
  gs_na <- layer_grobs(ggplot2::ggplot(df_na, ggplot2::aes(x, y, label = eq)) +
                         geom_latex(na.rm = TRUE))
  expect_length(gs_na, 1L)
})

test_that("geom_latex fontsize parameter sets size when the aesthetic is unmapped", {
  df <- data.frame(x = 1, y = 1, eq = "x^2")

  # fontsize parameter fills the size column
  p20 <- ggplot2::ggplot(df, ggplot2::aes(x, y, label = eq)) +
    geom_latex(fontsize = 20)
  expect_equal(ggplot2::ggplot_build(p20)$data[[1]]$size, 20)

  # a mapped size aesthetic wins over the parameter
  df$s <- 30
  p_mapped <- ggplot2::ggplot(df, ggplot2::aes(x, y, label = eq, size = s)) +
    geom_latex(fontsize = 20) +
    ggplot2::scale_size_identity()
  expect_equal(ggplot2::ggplot_build(p_mapped)$data[[1]]$size, 30)
})

# --- annotate('latex') ---

test_that("annotate('latex') adds a rendered layer of its own", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::annotate("latex", x = 4, y = 30,
                      label = "\\hat{y} = \\beta_0 + \\beta_1 x",
                      size = 12, colour = "red")
  # Layer 2 is the annotation; the geom has to be reachable by name for
  # annotate() to find it at all.
  g <- layer_grobs(p, 2L)[[1]]
  expect_s3_class(g, "latexgrob")
  expect_gt(nrow(g$layout_df), 0)
  expect_equal(g$gp$col, "red")
})

# --- element_latex() ---

test_that("element_latex is an element_text subclass that merges", {
  el <- element_latex(fontsize = 18, render_mode = "path")
  expect_equal(el@render_mode, "path")
  # `fontsize` is our alias for element_text's `size`; it is not a
  # property of its own, so it has to land there or it is silently lost.
  expect_equal(el@size, 18)

  # Properties the caller did not set come from the parent element.
  merged <- ggplot2::merge_element(el, ggplot2::element_text(colour = "blue"))
  expect_equal(merged@colour, "blue")
  expect_equal(merged@render_mode, "path")
})

test_that("element_latex installs our grob as the axis title", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "$\\beta_1 \\cdot x + \\beta_0$") +
    ggplot2::theme(axis.title.x = element_latex())
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))

  g <- gt$grobs[[which(gt$layout$name == "xlab-b")]]
  expect_s3_class(g, "latexgrob")
  # Set as math, not as the literal dollar-delimited string.
  expect_true("glyph" %in% g$layout_df$type)
})

# Tick labels go through element_grob() with a vector label; that path and
# its reported size are covered in test-ggplot2-markdown.R, which runs it
# for element_latex() as well as element_markdown().

# --- .element_grob_latex() internals ---

test_that(".element_grob_latex handles edge cases and multiple labels", {
  # NULL/empty → nullGrob
  expect_s3_class(
    gridmicrotex:::.element_grob_latex(element_latex(), label = NULL), "null")
  expect_s3_class(
    gridmicrotex:::.element_grob_latex(element_latex(), label = ""), "null")

  # Single label + dollar stripping
  result <- gridmicrotex:::.element_grob_latex(element_latex(), label = "$x^2$")
  expect_s3_class(result, "latexgrob")

  # Multiple labels with NA/empty skipped
  result_multi <- gridmicrotex:::.element_grob_latex(
    element_latex(), label = c("x_1", NA, "", "x_4"))
  expect_equal(length(result_multi$children), 2)
})
