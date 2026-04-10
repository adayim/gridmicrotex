test_that("typeface fallback replaces missing glyph rows", {
  tex <- "\\sqrt{\\frac{\\pi}{a^{2n+1}}}"
  layout <- parse_latex_cpp(tex, text_size = 20, use_path = FALSE)
  path_layout <- parse_latex_cpp(tex, text_size = 20, use_path = TRUE)

  glyph_rows <- which(layout$type == "glyph")
  skip_if(length(glyph_rows) == 0, "No glyph rows available for fallback test")

  # Force one glyph row to look unmappable so fallback path replacement runs.
  target <- glyph_rows[1]
  layout$text[target] <- NA_character_
  layout$codepoint[target] <- NA_integer_

  out <- gridmicrotex:::.add_typeface_path_fallback(
    layout = layout,
    path_layout = path_layout
  )

  expect_identical(out$type[target], "path")
  expect_true(!is.null(out$path[[target]]))
})


test_that("typeface fallback is a no-op when all glyphs are mapped", {
  tex <- "a+b+c"
  layout <- parse_latex_cpp(tex, text_size = 20, use_path = FALSE)
  path_layout <- parse_latex_cpp(tex, text_size = 20, use_path = TRUE)

  out <- gridmicrotex:::.add_typeface_path_fallback(
    layout = layout,
    path_layout = path_layout
  )

  expect_identical(out$type, layout$type)
  expect_identical(out$text, layout$text)
})


test_that("typeface fallback replaces glyph rows missing font_file", {
  tex <- "\\forall\\varepsilon\\in\\mathbb{R}_+^*"
  layout <- parse_latex_cpp(tex, text_size = 20, use_path = FALSE)
  path_layout <- parse_latex_cpp(tex, text_size = 20, use_path = TRUE)

  mapped <- which(layout$type == "glyph" & !is.na(layout$text) & nzchar(layout$text))
  skip_if(length(mapped) == 0, "No mapped glyph rows available for fallback test")

  target <- mapped[1]
  layout$font_file[target] <- NA_character_

  out <- gridmicrotex:::.add_typeface_path_fallback(
    layout = layout,
    path_layout = path_layout
  )

  expect_identical(out$type[target], "path")
  expect_true(!is.null(out$path[[target]]))
})


test_that("typeface mode falls back on pdf device", {
  tf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tf)
  on.exit({
    grDevices::dev.off()
    unlink(tf)
  }, add = TRUE)

  expect_warning(
    expect_no_error({
      g <- latex_grob("x = \\frac{-b \\pm \\sqrt{b^{2} - 4ac}}{2a}",
                      render_mode = "typeface", fontsize = 20)
      grid::grid.newpage()
      grid::grid.draw(g)
    }),
    "falling back to path mode"
  )
})


test_that("typeface mode stays active on cairo_pdf", {
  skip_if_not(capabilities("cairo"), "Cairo graphics not available")
  skip_on_os("mac") # cairo_pdf + typeface rendering causes segfault on macOS ARM64

  tf <- tempfile(fileext = ".pdf")
  grDevices::cairo_pdf(tf)
  on.exit({
    grDevices::dev.off()
    unlink(tf)
  }, add = TRUE)

  expect_no_warning({
    g <- latex_grob("x = \\frac{-b \\pm \\sqrt{b^{2} - 4ac}}{2a}",
                    render_mode = "typeface", fontsize = 20)
    grid::grid.newpage()
    grid::grid.draw(g)
  })
})
