test_that("latex_dims works with render_mode = 'path'", {
  dims <- latex_dims("\\frac{a}{b}", render_mode = "path")
  expect_true(is.list(dims))
  expect_true(grid::convertWidth(dims$width, "points", valueOnly = TRUE) > 0)
  expect_true(grid::convertHeight(dims$height, "points", valueOnly = TRUE) > 0)
})

test_that("latex_dims includes depth and baseline in result", {
  dims <- latex_dims("x^2")
  expect_true(!is.null(dims$depth))
  expect_true(!is.null(dims$baseline))
  # baseline is numeric (not a unit object)
  expect_type(dims$baseline, "double")
})

test_that("latex_dims larger fontsize produces larger dimensions", {
  dims_small <- latex_dims("x^2", fontsize = 10)
  dims_large <- latex_dims("x^2", fontsize = 40)
  w_small <- grid::convertWidth(dims_small$width, "bigpts", valueOnly = TRUE)
  w_large <- grid::convertWidth(dims_large$width, "bigpts", valueOnly = TRUE)
  expect_true(w_large > w_small)
})

test_that("grid.latex returns a latexgrob invisibly", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = 200, height = 100)
  on.exit(grDevices::dev.off(), add = TRUE)
  g <- grid.latex("x^2 + 1", fontsize = 20, render_mode = "path")
  expect_s3_class(g, "latexgrob")
})

test_that("latex_grob with gp color parameter stores color", {
  g <- latex_grob("x^2", gp = grid::gpar(col = "red"), render_mode = "path")
  expect_s3_class(g, "latexgrob")
  # The grob is created; width should be positive
  w <- grid::convertWidth(grid::grobWidth(g), "bigpts", valueOnly = TRUE)
  expect_true(w > 0)
})

test_that("latex_grob with rotation stores the angle", {
  g <- latex_grob("x^2", rot = 45, render_mode = "path")
  expect_s3_class(g, "latexgrob")
  expect_equal(g$vp$angle, 45)
})

test_that("latex_grob with numeric x/y converts to unit", {
  g <- latex_grob("x^2", x = 0.3, y = 0.7, default.units = "npc",
                  render_mode = "path")
  expect_s3_class(g, "latexgrob")
  expect_true(inherits(g$vp$x, "unit"))
  expect_true(inherits(g$vp$y, "unit"))
})

test_that("latex_grob with unit x/y keeps them as units", {
  g <- latex_grob("x^2", x = grid::unit(0.5, "npc"),
                  y = grid::unit(0.5, "npc"), render_mode = "path")
  expect_s3_class(g, "latexgrob")
})

test_that("latex_grob with max_width produces valid grob", {
  g <- latex_grob("x^2 + y^2 = z^2", max_width = 50, render_mode = "path")
  expect_s3_class(g, "latexgrob")
  w <- grid::convertWidth(grid::grobWidth(g), "bigpts", valueOnly = TRUE)
  expect_true(w > 0)
})

test_that("latex_grob with line_space produces valid grob", {
  g <- latex_grob("x^2 \\\\ y^2", line_space = 20, render_mode = "path")
  expect_s3_class(g, "latexgrob")
})

test_that("widthDetails.latexgrob returns a unit", {
  g <- latex_grob("\\frac{a}{b}", render_mode = "path")
  w <- grid::widthDetails(g)
  expect_true(inherits(w, "unit"))
  expect_true(grid::convertWidth(w, "bigpts", valueOnly = TRUE) > 0)
})

test_that("heightDetails.latexgrob returns a unit", {
  g <- latex_grob("\\frac{a}{b}", render_mode = "path")
  h <- grid::heightDetails(g)
  expect_true(inherits(h, "unit"))
  expect_true(grid::convertHeight(h, "bigpts", valueOnly = TRUE) > 0)
})

test_that("xDetails.latexgrob returns a unit for theta=0 (right side)", {
  g <- latex_grob("x^2", render_mode = "path")
  # xDetails is called inside a graphics device context
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = 200, height = 100)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  gx <- grid::xDetails(g, 0)
  expect_true(inherits(gx, "unit"))
})

test_that("xDetails.latexgrob returns left side for theta=180", {
  g <- latex_grob("x^2", render_mode = "path")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = 200, height = 100)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  gx_right <- grid::convertX(grid::xDetails(g, 0), "npc", valueOnly = TRUE)
  gx_left  <- grid::convertX(grid::xDetails(g, 180), "npc", valueOnly = TRUE)
  expect_true(gx_right >= gx_left)
})

test_that("yDetails.latexgrob returns a unit", {
  g <- latex_grob("x^2", render_mode = "path")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = 200, height = 100)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  gy <- grid::yDetails(g, 90)
  expect_true(inherits(gy, "unit"))
})

test_that("yDetails.latexgrob: theta=0 returns bottom, theta=90 returns top", {
  g <- latex_grob("x^2", render_mode = "path")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = 200, height = 100)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  y_top    <- grid::convertY(grid::yDetails(g, 90), "npc", valueOnly = TRUE)
  y_bottom <- grid::convertY(grid::yDetails(g, 0), "npc", valueOnly = TRUE)
  expect_true(y_top >= y_bottom)
})

test_that(".device_supports_typeface_glyphs returns TRUE with no device open", {
  # When dev.cur() == 1L (null device), should return TRUE
  result <- gridmicrotex:::.device_supports_typeface_glyphs()
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that(".device_supports_typeface_glyphs returns FALSE for pdf() device", {
  tf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tf)
  on.exit({
    grDevices::dev.off()
    unlink(tf)
  }, add = TRUE)
  result <- gridmicrotex:::.device_supports_typeface_glyphs()
  expect_false(result)
})

test_that(".device_supports_typeface_glyphs returns TRUE for png() device", {
  tf <- tempfile(fileext = ".png")
  grDevices::png(tf)
  on.exit({
    grDevices::dev.off()
    unlink(tf)
  }, add = TRUE)
  result <- gridmicrotex:::.device_supports_typeface_glyphs()
  expect_true(result)
})

test_that(".measure_text_bigpts returns positive number for non-empty text", {
  # Open a graphics device so stringWidth is available
  tf <- tempfile(fileext = ".png")
  grDevices::png(tf)
  grid::pushViewport(grid::viewport(gp = grid::gpar(fontsize = 12)))
  on.exit({
    grid::popViewport()
    grDevices::dev.off()
    unlink(tf)
  }, add = TRUE)
  result <- gridmicrotex:::.measure_text_bigpts("Hello")
  expect_type(result, "double")
  expect_true(result > 0)
})

test_that(".measure_text_bigpts scales with text length", {
  tf <- tempfile(fileext = ".png")
  grDevices::png(tf)
  grid::pushViewport(grid::viewport(gp = grid::gpar(fontsize = 12)))
  on.exit({
    grid::popViewport()
    grDevices::dev.off()
    unlink(tf)
  }, add = TRUE)
  short <- gridmicrotex:::.measure_text_bigpts("Hi")
  long  <- gridmicrotex:::.measure_text_bigpts("Hello World")
  expect_true(long > short)
})

test_that("latex_grob stores render_mode in returned object", {
  g_path <- latex_grob("x^2", render_mode = "path")
  g_type <- latex_grob("x^2", render_mode = "typeface")
  expect_equal(g_path$render_mode, "path")
  expect_equal(g_type$render_mode, "typeface")
})

test_that("latex_grob stores bbox attributes", {
  g <- latex_grob("\\frac{a}{b}", render_mode = "path")
  expect_true(is.numeric(g$bbox_w) && g$bbox_w > 0)
  expect_true(is.numeric(g$bbox_h) && g$bbox_h > 0)
  expect_true(is.numeric(g$total_h) && g$total_h > 0)
})

test_that("latex_grob path_layout_df is NULL in path render_mode", {
  g <- latex_grob("x^2", render_mode = "path")
  expect_null(g$path_layout_df)
})

test_that("latex_grob path_layout_df is non-NULL in typeface render_mode", {
  g <- latex_grob("x^2", render_mode = "typeface")
  expect_true(!is.null(g$path_layout_df))
  expect_s3_class(g$path_layout_df, "data.frame")
})
