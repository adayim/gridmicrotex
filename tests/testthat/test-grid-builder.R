# Helper: make a minimal path_data list for build_path_grob
.make_path_data <- function(cmds, coords_matrix) {
  list(cmd = cmds, coords = coords_matrix)
}

# Helper: make a minimal layout_df row
.make_layout_row <- function(type = "path", x = 5, y = 5, x2 = 10, y2 = 5,
                              width = 10, height = 10, lwd = 1,
                              color = "#000000", text = NA_character_,
                              font_size = 12, font_style = 0L,
                              codepoint = NA_integer_, font_file = NA_character_,
                              glyph = NA_integer_, path_data = list(NULL)) {
  df <- data.frame(
    type = type, x = x, y = y, x2 = x2, y2 = y2,
    width = width, height = height, lwd = lwd,
    color = color, text = text, font_size = font_size,
    font_style = font_style, codepoint = codepoint,
    font_file = font_file, glyph = glyph,
    stringsAsFactors = FALSE
  )
  df$path <- path_data
  df
}

# ---- cubic_bezier ----

test_that("cubic_bezier returns list with x and y components", {
  result <- gridmicrotex:::cubic_bezier(0, 0, 1, 1, 2, 1, 3, 0)
  expect_type(result, "list")
  expect_named(result, c("x", "y"))
})

test_that("cubic_bezier starts at p0 and ends at p3", {
  result <- gridmicrotex:::cubic_bezier(0, 0, 1, 2, 2, 2, 3, 0)
  expect_equal(result$x[1], 0)
  expect_equal(result$y[1], 0)
  expect_equal(result$x[length(result$x)], 3)
  expect_equal(result$y[length(result$y)], 0)
})

test_that("cubic_bezier default n=16 returns 16 points", {
  result <- gridmicrotex:::cubic_bezier(0, 0, 1, 1, 2, 1, 3, 0)
  expect_length(result$x, 16)
  expect_length(result$y, 16)
})

test_that("cubic_bezier respects custom n parameter", {
  result <- gridmicrotex:::cubic_bezier(0, 0, 1, 1, 2, 1, 3, 0, n = 5)
  expect_length(result$x, 5)
  expect_length(result$y, 5)
})

test_that("cubic_bezier with straight line returns nearly collinear points", {
  result <- gridmicrotex:::cubic_bezier(0, 0, 1, 0, 2, 0, 3, 0)
  expect_true(all(abs(result$y) < 1e-10))
  expect_true(all(result$x >= 0 & result$x <= 3))
})

# ---- quad_bezier ----

test_that("quad_bezier returns list with x and y components", {
  result <- gridmicrotex:::quad_bezier(0, 0, 1, 2, 2, 0)
  expect_type(result, "list")
  expect_named(result, c("x", "y"))
})

test_that("quad_bezier starts at p0 and ends at p2", {
  result <- gridmicrotex:::quad_bezier(1, 2, 3, 4, 5, 6)
  expect_equal(result$x[1], 1)
  expect_equal(result$y[1], 2)
  expect_equal(result$x[length(result$x)], 5)
  expect_equal(result$y[length(result$y)], 6)
})

test_that("quad_bezier default n=12 returns 12 points", {
  result <- gridmicrotex:::quad_bezier(0, 0, 1, 2, 2, 0)
  expect_length(result$x, 12)
  expect_length(result$y, 12)
})

test_that("quad_bezier with straight line returns nearly collinear points", {
  result <- gridmicrotex:::quad_bezier(0, 0, 1, 0, 2, 0)
  expect_true(all(abs(result$y) < 1e-10))
})

# ---- build_path_grob ----

test_that("build_path_grob returns NULL for empty commands", {
  path_data <- list(cmd = character(0),
                    coords = matrix(numeric(0), nrow = 0, ncol = 6))
  result <- gridmicrotex:::build_path_grob(path_data, "#000000", 1, 100)
  expect_null(result)
})

test_that("build_path_grob returns NULL when all subpaths have <= 2 points", {
  # M + Z only → 1-point subpath, flushed and discarded
  path_data <- list(
    cmd = c("M", "Z"),
    coords = matrix(c(0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0), nrow = 2, ncol = 6, byrow = TRUE)
  )
  result <- gridmicrotex:::build_path_grob(path_data, "#000000", 1, 100)
  expect_null(result)
})

test_that("build_path_grob returns a pathGrob for a triangle", {
  path_data <- list(
    cmd    = c("M",  "L",   "L",    "Z"),
    coords = matrix(c(
      0,  0,  0, 0, 0, 0,
      10, 0,  0, 0, 0, 0,
      5,  10, 0, 0, 0, 0,
      0,  0,  0, 0, 0, 0
    ), nrow = 4, ncol = 6, byrow = TRUE)
  )
  result <- gridmicrotex:::build_path_grob(path_data, "#FF0000", 1, 100)
  expect_s3_class(result, "pathgrob")
})

test_that("build_path_grob applies y-axis flip via total_h", {
  path_data <- list(
    cmd    = c("M",  "L",   "L",    "Z"),
    coords = matrix(c(
      0, 10, 0, 0, 0, 0,
      10, 20, 0, 0, 0, 0,
      5,  5, 0, 0, 0, 0,
      0,  0, 0, 0, 0, 0
    ), nrow = 4, ncol = 6, byrow = TRUE)
  )
  total_h <- 50
  result <- gridmicrotex:::build_path_grob(path_data, "#000000", 1, total_h)
  expect_s3_class(result, "pathgrob")
  # Y coordinates should be total_h - original_y
  ys <- grid::convertHeight(result$y, "bigpts", valueOnly = TRUE)
  expect_true(all(ys %in% c(total_h - 10, total_h - 20, total_h - 5)))
})

test_that("build_path_grob handles cubic bezier (C command)", {
  path_data <- list(
    cmd    = c("M",  "C",              "Z"),
    coords = matrix(c(
      0,  0,  0,  0, 0, 0,
      10, 0, 10, 10, 0, 10,
      0,  0,  0,  0, 0, 0
    ), nrow = 3, ncol = 6, byrow = TRUE)
  )
  result <- gridmicrotex:::build_path_grob(path_data, "#000000", 1, 100)
  expect_s3_class(result, "pathgrob")
})

test_that("build_path_grob handles quadratic bezier (Q command)", {
  path_data <- list(
    cmd    = c("M",  "Q",      "Z"),
    coords = matrix(c(
      0,  0, 0, 0, 0, 0,
      5, 10, 10, 0, 0, 0,
      0,  0, 0, 0, 0, 0
    ), nrow = 3, ncol = 6, byrow = TRUE)
  )
  result <- gridmicrotex:::build_path_grob(path_data, "#000000", 1, 100)
  expect_s3_class(result, "pathgrob")
})

test_that("build_path_grob handles multiple subpaths (multiple M commands)", {
  path_data <- list(
    cmd    = c("M", "L", "L", "Z", "M", "L", "L", "Z"),
    coords = matrix(c(
      0,  0, 0, 0, 0, 0,
      10, 0, 0, 0, 0, 0,
      5, 10, 0, 0, 0, 0,
      0,  0, 0, 0, 0, 0,
      20, 0, 0, 0, 0, 0,
      30, 0, 0, 0, 0, 0,
      25,10, 0, 0, 0, 0,
      0,  0, 0, 0, 0, 0
    ), nrow = 8, ncol = 6, byrow = TRUE)
  )
  result <- gridmicrotex:::build_path_grob(path_data, "#000000", 1, 100)
  expect_s3_class(result, "pathgrob")
  # Two subpaths → id should have values 1 and 2
  expect_true(max(result$id) == 2)
})

test_that("build_path_grob skips C command when no current point", {
  # C command at index 1 with no preceding M — should be skipped (next)
  path_data <- list(
    cmd    = c("C",  "M",  "L",   "L",   "Z"),
    coords = matrix(c(
      10, 0, 10, 10, 0, 10,
       0, 0,  0,  0, 0,  0,
      10, 0,  0,  0, 0,  0,
       5,10,  0,  0, 0,  0,
       0, 0,  0,  0, 0,  0
    ), nrow = 5, ncol = 6, byrow = TRUE)
  )
  # Should not error; the leading C is skipped, then M/L/L/Z form a triangle
  result <- gridmicrotex:::build_path_grob(path_data, "#000000", 1, 100)
  expect_s3_class(result, "pathgrob")
})

# ---- build_latex_children ----

test_that("build_latex_children returns empty gList for empty layout", {
  empty_df <- data.frame(
    type = character(0), x = numeric(0), y = numeric(0),
    stringsAsFactors = FALSE
  )
  empty_df$path <- list()
  result <- gridmicrotex:::build_latex_children(empty_df, 100)
  expect_s3_class(result, "gList")
  expect_length(result, 0)
})

test_that("build_latex_children handles 'path' type rows from real layout", {
  layout <- gridmicrotex:::parse_latex_cpp("x^2", use_path = TRUE)
  path_rows <- layout[layout$type == "path", ]
  skip_if(nrow(path_rows) == 0, "No path rows in layout")
  children <- gridmicrotex:::build_latex_children(
    path_rows, attr(layout, "bbox_height"), render_mode = "path"
  )
  expect_s3_class(children, "gList")
  expect_true(length(children) > 0)
})

test_that("build_latex_children handles 'line' type — horizontal rule", {
  layout <- gridmicrotex:::parse_latex_cpp("\\frac{a}{b}", use_path = TRUE)
  line_rows <- layout[layout$type == "line", ]
  skip_if(nrow(line_rows) == 0, "No line rows in layout")
  children <- gridmicrotex:::build_latex_children(
    line_rows, attr(layout, "bbox_height"), render_mode = "path"
  )
  expect_s3_class(children, "gList")
  expect_true(length(children) > 0)
})

test_that("build_latex_children handles non-horizontal line type", {
  # \cancel produces diagonal lines
  layout <- gridmicrotex:::parse_latex_cpp(
    "\\cancel{x}", use_path = TRUE
  )
  line_rows <- layout[layout$type == "line" & abs(layout$y - layout$y2) >= 0.001, ]
  skip_if(nrow(line_rows) == 0, "No non-horizontal line rows")
  children <- gridmicrotex:::build_latex_children(
    line_rows, attr(layout, "bbox_height"), render_mode = "path"
  )
  expect_s3_class(children, "gList")
  expect_true(length(children) > 0)
})

test_that("build_latex_children handles 'fill_rect' type", {
  # Arrays produce fill_rect rows for borders/backgrounds
  layout <- gridmicrotex:::parse_latex_cpp(
    "\\begin{array}{|c|} \\hline a \\\\ \\hline \\end{array}",
    use_path = TRUE
  )
  fill_rows <- layout[layout$type == "fill_rect", ]
  skip_if(nrow(fill_rows) == 0, "No fill_rect rows in layout")
  children <- gridmicrotex:::build_latex_children(
    fill_rows, attr(layout, "bbox_height"), render_mode = "path"
  )
  expect_s3_class(children, "gList")
  expect_true(length(children) > 0)
})

test_that("build_latex_children handles 'text' type rows", {
  layout <- gridmicrotex:::parse_latex_cpp(
    "\\text{\u4F60\u597D}", use_path = TRUE
  )
  text_rows <- layout[layout$type == "text", ]
  skip_if(nrow(text_rows) == 0, "No text rows in layout")
  children <- gridmicrotex:::build_latex_children(
    text_rows, attr(layout, "bbox_height"), text_gp = grid::gpar(),
    render_mode = "path"
  )
  expect_s3_class(children, "gList")
  expect_true(length(children) > 0)
})

test_that("build_latex_children skips text rows with NA or empty text", {
  layout <- gridmicrotex:::parse_latex_cpp("a", use_path = TRUE)
  # Inject a text row with NA text
  na_row <- layout[1, ]
  na_row$type <- "text"
  na_row$text <- NA_character_
  children <- gridmicrotex:::build_latex_children(
    na_row, attr(layout, "bbox_height"), render_mode = "path"
  )
  expect_s3_class(children, "gList")
  expect_length(children, 0)
})

test_that("build_latex_children with glyph type in path mode produces no children", {
  layout <- gridmicrotex:::parse_latex_cpp("a+b", use_path = FALSE)
  glyph_rows <- layout[layout$type == "glyph", ]
  skip_if(nrow(glyph_rows) == 0, "No glyph rows in layout")
  # In path mode, glyph rows are ignored
  children <- gridmicrotex:::build_latex_children(
    glyph_rows, attr(layout, "bbox_height"), render_mode = "path"
  )
  expect_s3_class(children, "gList")
  expect_length(children, 0)
})

test_that("build_latex_children with glyph type in typeface mode renders textGrobs", {
  layout <- gridmicrotex:::parse_latex_cpp("a+b", use_path = FALSE)
  glyph_rows <- layout[layout$type == "glyph" &
                         !is.na(layout$text) & nzchar(layout$text), ]
  skip_if(nrow(glyph_rows) == 0, "No mappable glyph rows in layout")
  children <- gridmicrotex:::build_latex_children(
    glyph_rows, attr(layout, "bbox_height"), render_mode = "typeface"
  )
  expect_s3_class(children, "gList")
  expect_true(length(children) > 0)
})

# ---- .resolve_glyph_font_family ----

test_that(".resolve_glyph_font_family returns a string for non-existent font file", {
  result <- gridmicrotex:::.resolve_glyph_font_family("/nonexistent/path/SomeFont.otf")
  expect_type(result, "character")
  expect_true(nzchar(result))
})

test_that(".resolve_glyph_font_family derives name from filename for missing file", {
  result <- gridmicrotex:::.resolve_glyph_font_family("/no/such/dir/MyFont-Regular.otf")
  expect_equal(result, "MyFont-Regular")
})

test_that(".resolve_glyph_font_family uses cache on repeated calls", {
  path <- "/tmp/fake_font_cache_test.otf"
  # Call twice; should return same result (cached)
  r1 <- gridmicrotex:::.resolve_glyph_font_family(path)
  r2 <- gridmicrotex:::.resolve_glyph_font_family(path)
  expect_equal(r1, r2)
})
