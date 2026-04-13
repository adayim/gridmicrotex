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
