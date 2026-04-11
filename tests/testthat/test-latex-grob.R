# --- latex_grob creation and structure ---

test_that("latex_grob creates valid grob and returns correct dimensions", {
  g <- latex_grob("\\frac{x^{2}+1}{\\sqrt{y}}")
  expect_s3_class(g, "latexgrob")
  expect_true(nrow(g$layout_df) > 0)
  expect_true(g$bbox_w > 0)

  # render_mode stored correctly
  g_path <- latex_grob("x^2", render_mode = "path")
  g_type <- latex_grob("x^2", render_mode = "typeface")
  expect_equal(g_path$render_mode, "path")
  expect_null(g_path$path_layout_df)
  expect_s3_class(g_type$path_layout_df, "data.frame")

  # latex_dims with fontsize scaling
  dims <- latex_dims("\\frac{a}{b}", render_mode = "path")
  expect_true(grid::convertWidth(dims$width, "points", valueOnly = TRUE) > 0)
  w_small <- grid::convertWidth(latex_dims("x^2", fontsize = 10)$width,
                                "bigpts", valueOnly = TRUE)
  w_large <- grid::convertWidth(latex_dims("x^2", fontsize = 40)$width,
                                "bigpts", valueOnly = TRUE)
  expect_true(w_large > w_small)
})

# --- latex_grob parameters ---

test_that("latex_grob parameters work correctly", {
  # Rotation
  expect_equal(latex_grob("x^2", rot = 45)$vp$angle, 45)

  # max_width
  g_mw <- latex_grob("x^2 + y^2 = z^2", max_width = 50, render_mode = "path")
  expect_true(grid::convertWidth(grid::grobWidth(g_mw), "bigpts", valueOnly = TRUE) > 0)

  # makeContent builds children
  g_mc <- grid::makeContent(latex_grob("\\frac{a}{b}", fontsize = 30, render_mode = "path"))
  expect_true(length(g_mc$children) > 0)

  # width/height details positive
  g <- latex_grob("\\frac{a}{b}", render_mode = "path")
  expect_true(grid::convertWidth(grid::widthDetails(g), "bigpts", valueOnly = TRUE) > 0)
  expect_true(grid::convertHeight(grid::heightDetails(g), "bigpts", valueOnly = TRUE) > 0)
})

# --- device support and typeface rendering ---

test_that("device support detection and typeface fallback work", {
  # pdf reports glyphs=TRUE
  tf_pdf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tf_pdf)
  expect_true(gridmicrotex:::.device_supports_typeface_glyphs())

  # PDF: renders without fallback warning
  expect_no_warning({
    g <- latex_grob("\\frac{a}{b}", render_mode = "typeface", fontsize = 20)
    grid::grid.newpage()
    grid::grid.draw(g)
  })
  grDevices::dev.off()
  unlink(tf_pdf)

  # Postscript: falls back with warning
  tf_ps <- tempfile(fileext = ".ps")
  grDevices::postscript(tf_ps)
  on.exit({ grDevices::dev.off(); unlink(tf_ps) }, add = TRUE)
  expect_false(gridmicrotex:::.device_supports_typeface_glyphs())
  expect_warning(
    expect_no_error({
      g <- latex_grob("\\frac{a}{b}", render_mode = "typeface", fontsize = 20)
      grid::grid.newpage()
      grid::grid.draw(g)
    }),
    "falling back to path mode"
  )
})
