test_that("latex_grob returns a latexgrob for a fraction", {
  g <- latex_grob("\\frac{a}{b}")
  expect_s3_class(g, "latexgrob")
  expect_s3_class(g, "gTree")
  # Children are built lazily by makeContent, so check stored layout
  expect_true(nrow(g$layout_df) > 0)
})

test_that("latex_grob returns a latexgrob for superscript", {
  g <- latex_grob("x^{2}")
  expect_s3_class(g, "latexgrob")
  expect_true(nrow(g$layout_df) > 0)
})

test_that("latex_grob returns a latexgrob for complex expression", {
  g <- latex_grob("\\frac{x^{2}+1}{\\sqrt{y}}")
  expect_s3_class(g, "latexgrob")
  expect_true(nrow(g$layout_df) > 0)
})

test_that("parse_latex_cpp returns a data.frame with expected structure", {
  layout <- gridmicrotex:::parse_latex_cpp("\\frac{a}{b}")
  expect_s3_class(layout, "data.frame")
  expect_true(nrow(layout) > 0)
  expect_true(all(c("type", "x", "y", "color", "path") %in% names(layout)))
  expect_true(!is.null(attr(layout, "bbox_width")))
  expect_true(!is.null(attr(layout, "bbox_height")))
  expect_true(attr(layout, "bbox_width") > 0)
  expect_true(attr(layout, "bbox_height") > 0)
})

test_that("latex_dims returns positive dimensions", {
  dims <- latex_dims("\\frac{a}{b}")
  expect_true(is.list(dims))
  expect_true(grid::convertWidth(dims$width, "points", valueOnly = TRUE) > 0)
  expect_true(grid::convertHeight(dims$height, "points", valueOnly = TRUE) > 0)
})

test_that("microtex_is_inited returns TRUE after package load", {
  expect_true(gridmicrotex:::microtex_is_inited())
})

test_that("microtex_version returns a non-empty string", {
  v <- gridmicrotex:::microtex_version()
  expect_true(is.character(v))
  expect_true(nchar(v) > 0)
})

test_that("grobWidth and grobHeight return positive values", {
  g <- latex_grob("\\frac{a}{b}", fontsize = 30)
  w <- grid::convertWidth(grid::grobWidth(g), "bigpts", valueOnly = TRUE)
  h <- grid::convertHeight(grid::grobHeight(g), "bigpts", valueOnly = TRUE)
  expect_true(w > 0)
  expect_true(h > 0)
})

test_that("makeContent builds children from stored layout", {
  g <- latex_grob("\\frac{a}{b}", fontsize = 30, render_mode = "path")
  g2 <- grid::makeContent(g)
  expect_true(length(g2$children) > 0)
})

test_that("larger fontsize produces larger dimensions", {
  g_small <- latex_grob("x^{2}", fontsize = 10)
  g_large <- latex_grob("x^{2}", fontsize = 40)
  w_small <- grid::convertWidth(grid::grobWidth(g_small), "bigpts", valueOnly = TRUE)
  w_large <- grid::convertWidth(grid::grobWidth(g_large), "bigpts", valueOnly = TRUE)
  expect_true(w_large > w_small)
})
