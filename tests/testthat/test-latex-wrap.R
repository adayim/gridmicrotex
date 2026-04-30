
test_that("latex_wrap converts correctly", {
  expect_equal(latex_wrap("It is $x^2$"),
               "\\text{It is }x^2")
  expect_equal(latex_wrap("\\(E=mc^2\\) is famous"),
               "E=mc^2\\text{ is famous}")
  expect_equal(latex_wrap("plain text"),
               "\\text{plain text}")
  expect_equal(latex_wrap("cost \\$5 for $x$"),
               "\\text{cost \\$5 for }x")
  expect_equal(latex_wrap("Line 1\nLine 2"),
               "\\text{Line 1 \\\\Line 2}")
})

