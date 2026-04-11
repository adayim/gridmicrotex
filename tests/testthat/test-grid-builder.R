# ---- bezier curves ----

test_that("bezier curves produce correct output", {
  # cubic: return type, endpoints, default n
  cubic <- gridmicrotex:::cubic_bezier(0, 0, 1, 2, 2, 2, 3, 0)
  expect_named(cubic, c("x", "y"))
  expect_equal(cubic$x[1], 0)
  expect_equal(cubic$x[length(cubic$x)], 3)
  expect_length(cubic$x, 16)

  # quad: return type, endpoints, default n
  quad <- gridmicrotex:::quad_bezier(1, 2, 3, 4, 5, 6)
  expect_equal(quad$x[1], 1)
  expect_equal(quad$x[length(quad$x)], 5)
  expect_length(quad$x, 12)
})

# ---- build_path_grob ----

test_that("build_path_grob handles empty, simple, and complex paths", {
  # Empty → NULL
  pd_empty <- list(cmd = character(0),
                   coords = matrix(numeric(0), nrow = 0, ncol = 6))
  expect_null(gridmicrotex:::build_path_grob(pd_empty, "#000000", 1, 100))

  # Triangle → pathgrob with y-flip
  pd_tri <- list(
    cmd    = c("M", "L", "L", "Z"),
    coords = matrix(c(
      0, 10, 0, 0, 0, 0,
      10, 20, 0, 0, 0, 0,
      5,  5, 0, 0, 0, 0,
      0,  0, 0, 0, 0, 0
    ), nrow = 4, ncol = 6, byrow = TRUE)
  )
  tri_grob <- gridmicrotex:::build_path_grob(pd_tri, "#FF0000", 1, 50)
  expect_s3_class(tri_grob, "pathgrob")

  # Multiple subpaths
  pd_multi <- list(
    cmd    = c("M", "L", "L", "Z", "M", "L", "L", "Z"),
    coords = matrix(c(
      0, 0, 0, 0, 0, 0,  10, 0, 0, 0, 0, 0,  5, 10, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0,
      20, 0, 0, 0, 0, 0,  30, 0, 0, 0, 0, 0,  25, 10, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0
    ), nrow = 8, ncol = 6, byrow = TRUE)
  )
  multi_grob <- gridmicrotex:::build_path_grob(pd_multi, "#000000", 1, 100)
  expect_true(max(multi_grob$id) == 2)
})

# ---- build_latex_children ----

test_that("build_latex_children processes all layout types", {
  # Empty layout
  empty_df <- data.frame(
    type = character(0), x = numeric(0), y = numeric(0),
    stringsAsFactors = FALSE
  )
  empty_df$path <- list()
  expect_length(gridmicrotex:::build_latex_children(empty_df, 100), 0)

  # Full layout with path + line + fill_rect from a real expression
  layout <- gridmicrotex:::parse_latex_cpp(
    "\\begin{array}{|c|} \\hline \\frac{a}{\\cancel{b}} \\\\  \\hline \\end{array}",
    use_path = TRUE
  )
  children <- gridmicrotex:::build_latex_children(
    layout, attr(layout, "bbox_height"), render_mode = "path"
  )
  expect_true(length(children) > 0)

  # Glyph rows in typeface mode
  layout2 <- gridmicrotex:::parse_latex_cpp("a+b", use_path = FALSE)
  glyph_rows <- layout2[layout2$type == "glyph" &
                          !is.na(layout2$font_file) & nzchar(layout2$font_file), ]
  skip_if(nrow(glyph_rows) == 0, "No glyph rows with font_file in layout")
  children2 <- gridmicrotex:::build_latex_children(
    glyph_rows, attr(layout2, "bbox_height"), render_mode = "typeface"
  )
  expect_true(length(children2) > 0)

  # NA text rows are skipped
  na_row <- layout[1, ]
  na_row$type <- "text"
  na_row$text <- NA_character_
  expect_length(gridmicrotex:::build_latex_children(na_row, 100, render_mode = "path"), 0)
})
