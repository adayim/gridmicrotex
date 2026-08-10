# --- text/CJK parsing ---

test_that("text and CJK parsing produces correct layout records", {
  # Latin \text{} now renders via text records (not math font paths)
  layout_latin <- parse_latex_cpp("\\text{Hello}", text_size = 20)
  expect_true("text" %in% layout_latin$type)
  expect_false("path" %in% layout_latin$type)

  # CJK uses text records; mixed math+CJK produces both types
  layout_mix <- parse_latex_cpp("x^2 + \\text{\u4F60\u597D}", text_size = 20)
  expect_true("text" %in% layout_mix$type)
  expect_true("path" %in% layout_mix$type)
  text_rows <- layout_mix[layout_mix$type == "text", ]
  expect_false(is.na(text_rows$font_size[1]))
})

# --- latex_grob with CJK ---

test_that("latex_grob with CJK text renders and stores fontfamily", {
  g <- latex_grob("x + \\text{\u4F60\u597D}",
                  gp = grid::gpar(fontfamily = "sans", fontsize = 20))
  expect_s3_class(g, "latexgrob")
  expect_equal(g$text_gp$fontfamily, "sans")
})

# --- text rotation / reflection ---

test_that("rotatebox populates rotation column on text records", {
  layout <- parse_latex_cpp("\\rotatebox{30}{\\mbox{Ab}}", text_size = 20)
  expect_true("rotation" %in% names(layout))
  txt <- layout[layout$type == "text", ]
  expect_gt(nrow(txt), 0L)
  # MicroTeX rotates CCW in a y-down canvas, so \rotatebox{30} stores -30
  # in the data frame (R flips the sign when building the grob).
  expect_true(all(abs(txt$rotation - -30) < 1e-3))
})

test_that("reflectbox leaves rotation at 0", {
  layout <- parse_latex_cpp("\\reflectbox{\\mbox{Ab}}", text_size = 20)
  txt <- layout[layout$type == "text", ]
  expect_gt(nrow(txt), 0L)
  expect_true(all(abs(txt$rotation) < 1e-3))
})

test_that("rotation past a quarter turn is not read as a reflection", {
  # The mirror test used to be `a < 0`, and `a` is cos(theta): true of a
  # \reflectbox, but equally true of every rotation past 90 degrees. Those
  # angles came out 180 degrees away from what was asked -- a vertical
  # label upside down -- and took a reflection's x-shift with them. The
  # sign of the determinant is the test that separates the two.
  for (deg in c(30, 89, 90, 91, 135, 180, 270, -90)) {
    layout <- parse_latex_cpp(sprintf("\\rotatebox{%d}{\\mbox{Ab}}", deg),
                              text_size = 20)
    txt <- layout[layout$type == "text", ]
    expect_gt(nrow(txt), 0L)
    # Stored CCW in a y-down canvas, i.e. negated, and only up to a turn --
    # so compare in [0, 360), where -180 and 180 are the same rotation.
    wrapped <- function(a) a %% 360
    expect_equal(wrapped(txt$rotation[1]), wrapped(-deg),
                 tolerance = 1e-3, info = as.character(deg))
  }
})

test_that("plain mbox leaves rotation at 0", {
  layout <- parse_latex_cpp("\\mbox{Ab}", text_size = 20)
  txt <- layout[layout$type == "text", ]
  expect_gt(nrow(txt), 0L)
  expect_true(all(abs(txt$rotation) < 1e-3))
})

# --- rect under rotation ---

test_that("rotated fbox emits 4 line records (not a single rect)", {
  plain <- parse_latex_cpp("\\fbox{A}", text_size = 20, use_path = FALSE)
  rot   <- parse_latex_cpp("\\rotatebox{30}{\\fbox{A}}",
                           text_size = 20, use_path = FALSE)
  # Plain fbox contains rect record(s); rotated version has none.
  expect_true(any(plain$type == "rect"))
  expect_false(any(rot$type == "rect"))
  # Rotated version gets extra line records (the 4 box edges).
  plain_lines <- sum(plain$type == "line")
  rot_lines   <- sum(rot$type == "line")
  expect_equal(rot_lines, plain_lines + 4L)
})

test_that("rotated colorbox emits a filled path (not a fill_rect)", {
  plain <- parse_latex_cpp("\\colorbox{yellow}{A}",
                           text_size = 20, use_path = FALSE)
  rot   <- parse_latex_cpp("\\rotatebox{30}{\\colorbox{yellow}{A}}",
                           text_size = 20, use_path = FALSE)
  expect_true(any(plain$type == "fill_rect"))
  expect_false(any(rot$type == "fill_rect"))
  expect_true(any(rot$type == "path"))
})
