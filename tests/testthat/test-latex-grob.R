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
  w_small <- grid::convertWidth(latex_dims("x^2", gp = grid::gpar(fontsize = 10))$width,
                                "bigpts", valueOnly = TRUE)
  w_large <- grid::convertWidth(latex_dims("x^2", gp = grid::gpar(fontsize = 40))$width,
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
  g_mc <- grid::makeContent(latex_grob("\\frac{a}{b}", render_mode = "path"))
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
    g <- latex_grob("\\frac{a}{b}", render_mode = "typeface", gp = grid::gpar(fontsize = 20))
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
      g <- latex_grob("\\frac{a}{b}", render_mode = "typeface", gp = grid::gpar(fontsize = 20))
      grid::grid.newpage()
      grid::grid.draw(g)
    }),
    "falling back to path mode"
  )
})

# --- edge cases: empty and invalid input ---

test_that("latex_grob handles empty input as a zero-size grob", {
  g <- latex_grob("")
  expect_s3_class(g, "latexgrob")
  expect_equal(g$bbox_w, 0)
  expect_equal(g$bbox_h, 0)
  expect_equal(nrow(g$layout_df), 0L)
})

test_that("latex_grob handles invalid LaTeX commands gracefully", {
  # MicroTeX silently ignores unknown commands; verify it doesn't crash
  g <- latex_grob("\\notavalidcommand{x}")
  expect_s3_class(g, "latexgrob")
})

# --- editGrob: re-parse on parse-affecting fields ---

test_that("editGrob re-parses when tex changes", {
  g <- latex_grob("x", render_mode = "path")
  g2 <- grid::editGrob(g, tex = "x^{2} + y^{2} + z^{2}")
  expect_equal(g2$tex, "x^{2} + y^{2} + z^{2}")
  expect_true(g2$bbox_w > g$bbox_w)
  expect_true(nrow(g2$layout_df) > nrow(g$layout_df))
  # viewport width/height tracked bbox
  expect_equal(
    as.numeric(g2$vp$width),
    as.numeric(grid::unit(g2$bbox_w, "bigpts"))
  )
})

test_that("editGrob re-parses when gp (fontsize) changes", {
  g20 <- latex_grob("x^2", render_mode = "path", gp = grid::gpar(fontsize = 20))
  g40 <- grid::editGrob(g20, gp = grid::gpar(fontsize = 40))
  expect_equal(g40$fontsize, 40)
  # 2x font -> ~2x bbox
  expect_equal(g40$bbox_w / g20$bbox_w, 2, tolerance = 0.05)
  # gp is re-stripped after parse (fontsize not carried on the gTree)
  expect_null(g40$gp$fontsize)
})

test_that("editGrob re-parses when math_font / tex_style / render_mode change", {
  g <- latex_grob("\\frac{a}{b}", render_mode = "path", math_font = "lete")
  g2 <- grid::editGrob(g, math_font = "stix")
  expect_false(identical(g$layout_df, g2$layout_df))

  g3 <- latex_grob("\\sum_{i=1}^{n} i", render_mode = "path", tex_style = "text")
  g4 <- grid::editGrob(g3, tex_style = "display")
  expect_true(g4$bbox_h > g3$bbox_h)  # display makes \sum taller

  g5 <- latex_grob("x^2", render_mode = "path")
  g6 <- grid::editGrob(g5, render_mode = "typeface")
  expect_equal(g6$render_mode, "typeface")
  expect_s3_class(g6$path_layout_df, "data.frame")  # fallback layout generated
})

test_that("editGrob on non-parse fields does not re-parse", {
  g <- latex_grob("\\frac{a}{b}", render_mode = "path")
  orig_layout <- g$layout_df
  g2 <- grid::editGrob(g, debug = TRUE)
  expect_true(isTRUE(g2$debug))
  expect_identical(g2$layout_df, orig_layout)
})

test_that("ascentDetails + descentDetails sum to heightDetails", {
  g <- latex_grob("\\frac{a}{b}", render_mode = "path", gp = grid::gpar(fontsize = 24))
  asc  <- grid::convertHeight(grid::ascentDetails(g),  "bigpts", valueOnly = TRUE)
  desc <- grid::convertHeight(grid::descentDetails(g), "bigpts", valueOnly = TRUE)
  h    <- grid::convertHeight(grid::heightDetails(g),  "bigpts", valueOnly = TRUE)
  expect_equal(asc + desc, h, tolerance = 1e-6)
  expect_true(asc > 0)
  expect_true(desc >= 0)
  # Descent matches the bbox_d field exposed for grob-to-grob alignment
  expect_equal(desc, g$bbox_d, tolerance = 1e-6)
})

test_that("editGrob keeps viewport just in sync with hjust/vjust", {
  g <- latex_grob("x", render_mode = "path", hjust = 0.5, vjust = 0.5)
  g2 <- grid::editGrob(g, hjust = 0, vjust = 1)
  expect_equal(g2$hjust, 0)
  expect_equal(g2$vjust, 1)
  expect_equal(as.numeric(g2$vp$valid.just), c(0, 1))
})

test_that("latex_dims respects math_font parameter", {
  expr <- "$\\int_0^1 f(x)\\,dx + x + y$"
  dims_lete <- latex_dims(expr, math_font = "lete", gp = grid::gpar(fontsize = 20))
  dims_stix <- latex_dims(expr, math_font = "stix", gp = grid::gpar(fontsize = 20))
  w_lete <- grid::convertWidth(dims_lete$width, "bigpts", valueOnly = TRUE)
  w_stix <- grid::convertWidth(dims_stix$width, "bigpts", valueOnly = TRUE)
  expect_true(w_lete > 0)
  expect_true(w_stix > 0)
  expect_false(w_lete == w_stix)
})

# --- \def command ---

test_that("\\def defines a zero-argument macro and renders identically to \\newcommand", {
  # \def\mymacroA{x^2} should produce the same layout as \newcommand{\mymacroB}{x^2}
  layout_nc  <- parse_latex_cpp("\\newcommand{\\mymacroB}{x^2} \\mymacroB", text_size = 20)
  layout_def <- parse_latex_cpp("\\def\\mymacroA{x^2} \\mymacroA",           text_size = 20)
  expect_equal(nrow(layout_def), nrow(layout_nc))
})

test_that("\\def silently overwrites an existing macro", {
  # First define, then redefine with \def — no error should be thrown
  expect_no_error(
    parse_latex_cpp("\\def\\myoverwrite{x} \\def\\myoverwrite{y} \\myoverwrite", text_size = 20)
  )
})

test_that("\\def with invalid control sequence name throws a parse error", {
  expect_error(
    parse_latex_cpp("\\def{notacontrolseq}{body}", text_size = 20)
  )
})

test_that("\\def with sequential #1..#N parameters expands like \\newcommand[N]", {
  # Single-arg form: \def\sq#1{#1^2} should match \newcommand{\sq}[1]{#1^2}
  layout_nc1  <- parse_latex_cpp("\\newcommand{\\sqB}[1]{#1^2} \\sqB{a}", text_size = 20)
  layout_def1 <- parse_latex_cpp("\\def\\sqA#1{#1^2} \\sqA{a}",           text_size = 20)
  expect_equal(nrow(layout_def1), nrow(layout_nc1))

  # Two-arg form: \def\pair#1#2{#1+#2}
  layout_nc2  <- parse_latex_cpp("\\newcommand{\\pairB}[2]{#1+#2} \\pairB{a}{b}", text_size = 20)
  layout_def2 <- parse_latex_cpp("\\def\\pairA#1#2{#1+#2} \\pairA{a}{b}",         text_size = 20)
  expect_equal(nrow(layout_def2), nrow(layout_nc2))
})

test_that("\\def rejects non-sequential or malformed parameter patterns", {
  # Out-of-order parameters: #2 before #1
  expect_error(
    parse_latex_cpp("\\def\\bad#2#1{#1#2}", text_size = 20)
  )
  # Skipping a parameter: #1 then #3
  expect_error(
    parse_latex_cpp("\\def\\skip#1#3{#1#3}", text_size = 20)
  )
  # '#' not followed by a digit
  expect_error(
    parse_latex_cpp("\\def\\noarg#x{x}", text_size = 20)
  )
})
