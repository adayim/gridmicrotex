test_that("\\text{} with CJK characters produces TEXT records", {
  layout <- parse_latex_cpp("\\text{\u4F60\u597D}", text_size = 20)
  text_rows <- layout[layout$type == "text", ]
  expect_equal(nrow(text_rows), 1)
  expect_equal(text_rows$text, "\u4F60\u597D")
  expect_false(is.na(text_rows$font_size))
})

test_that("mixed math + CJK produces both path and text records", {
  layout <- parse_latex_cpp("x^2 + \\text{\u4F60\u597D}", text_size = 20)
  expect_true("path" %in% layout$type)
  expect_true("text" %in% layout$type)
})

test_that("Japanese text produces TEXT records", {
  layout <- parse_latex_cpp("\\text{\u3053\u3093\u306B\u3061\u306F}", text_size = 20)
  text_rows <- layout[layout$type == "text", ]
  expect_equal(nrow(text_rows), 1)
  expect_equal(text_rows$text, "\u3053\u3093\u306B\u3061\u306F")
})

test_that("Korean text produces TEXT records", {
  layout <- parse_latex_cpp("\\text{\uD55C\uAD6D\uC5B4}", text_size = 20)
  text_rows <- layout[layout$type == "text", ]
  expect_equal(nrow(text_rows), 1)
})

test_that("Latin \\text{} renders via math font paths (not TEXT records)", {
  # Latin chars are in the math font, so they render as paths

  layout <- parse_latex_cpp("\\text{Hello}", text_size = 20)
  expect_false("text" %in% layout$type)
  expect_true("path" %in% layout$type)
})

test_that("latex_grob with gp fontfamily stores text_gp", {
  g <- latex_grob("x + \\text{\u4F60\u597D}",
                  gp = grid::gpar(fontfamily = "sans"))
  expect_equal(g$text_gp$fontfamily, "sans")
})

test_that("CJK text renders without error", {
  if (.Platform$OS.type == "windows") {
    expect_no_error({
      grob <- latex_grob("x^2 + \\text{\u4F60\u597D\u4E16\u754C}", fontsize = 20)
    })
  } else {
    expect_silent({
      grob <- latex_grob("x^2 + \\text{\u4F60\u597D\u4E16\u754C}", fontsize = 20)
    })
  }
  expect_s3_class(grob, "latexgrob")
})

test_that("mixed CJK and math renders without error", {
  if (.Platform$OS.type == "windows") {
    expect_no_error({
      grob <- latex_grob("\\text{\u5982\u679C } x > 0 \\text{ \u5219 } y = x^2",
                         fontsize = 20)
    })
  } else {
    expect_silent({
      grob <- latex_grob("\\text{\u5982\u679C } x > 0 \\text{ \u5219 } y = x^2",
                         fontsize = 20)
    })
  }
  expect_s3_class(grob, "latexgrob")
})
