test_that("\\text{} command renders upright text", {
  layout <- gridmicrotex:::parse_latex_cpp("\\text{if } x > 0")
  expect_true(nrow(layout) > 0)
})

test_that("mixed math and text renders", {
  layout <- gridmicrotex:::parse_latex_cpp("f(x) = \\text{max}(0, x)")
  expect_true(nrow(layout) > 0)
})

test_that("text command produces valid gTree", {
  g <- latex_grob("\\text{if } x > 0", fontsize = 20, render_mode = "path")
  g2 <- grid::makeContent(g)
  expect_true(length(g2$children) > 0)
})
