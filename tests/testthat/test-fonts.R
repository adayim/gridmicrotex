test_that("available_math_fonts returns loaded fonts", {
  fonts <- available_math_fonts()
  expect_true(is.character(fonts))
  expect_true(length(fonts) >= 1)
  expect_true("LatinModernMath-Regular" %in% fonts)
})

test_that("STIX Two Math is loaded", {
  fonts <- available_math_fonts()
  expect_true("STIX Two Math" %in% fonts)
})

test_that("font alias 'stix' resolves correctly", {
  resolved <- gridmicrotex:::resolve_math_font("stix")
  expect_equal(resolved, "STIX Two Math")
})

test_that("font alias 'lm' resolves correctly", {
  resolved <- gridmicrotex:::resolve_math_font("lm")
  expect_equal(resolved, "LatinModernMath-Regular")
})

test_that("empty font name resolves to default", {
  resolved <- gridmicrotex:::resolve_math_font("")
  expect_equal(resolved, "")
})

test_that("invalid font name throws error", {
  expect_error(
    gridmicrotex:::resolve_math_font("nonexistent"),
    "not found"
  )
})

test_that("rendering with STIX Two Math works", {
  g <- latex_grob("\\frac{a}{b}", fontsize = 20, math_font = "stix")
  expect_s3_class(g, "latexgrob")
  g2 <- grid::makeContent(g)
  expect_true(length(g2$children) > 0)
})

test_that("rendering with Latin Modern works via alias", {
  g <- latex_grob("\\frac{a}{b}", fontsize = 20, math_font = "lm")
  expect_s3_class(g, "latexgrob")
})

test_that("check_fonts runs without error", {
  expect_message(check_fonts(), "Loaded math fonts")
})
