test_that("text measurer creates, measures, and handles styles", {
  measurer <- gridmicrotex:::.make_text_measurer(grid::gpar())
  expect_type(measurer, "closure")

  result <- measurer("Hello", 0L)
  expect_length(result, 3)
  expect_true(all(result > 0))

  # Width scales with text length
  expect_true(measurer("Hello World", 0L)[1] > measurer("Hi", 0L)[1])

  # Bold text wider than plain
  expect_true(measurer("Hello", 2L)[1] >= measurer("Hello", 0L)[1])

  # .resolve_text_face maps style codes
  expect_equal(gridmicrotex:::.resolve_text_face(0L), "plain")
  expect_equal(gridmicrotex:::.resolve_text_face(2L), "bold")
  expect_equal(gridmicrotex:::.resolve_text_face(6L), "bold.italic")
  expect_equal(gridmicrotex:::.resolve_text_face(NA_integer_), "plain")
})

test_that("register/clear measurer lifecycle and integration", {
  m <- gridmicrotex:::.make_text_measurer(grid::gpar())
  expect_silent(register_text_measurer(m))
  expect_silent(clear_text_measurer())

  # Double-register replaces previous without error
  m2 <- gridmicrotex:::.make_text_measurer(grid::gpar(fontfamily = "mono"))
  register_text_measurer(m)
  expect_silent(register_text_measurer(m2))
  clear_text_measurer()

  # CJK layout uses measurer for dimensions
  if (.Platform$OS.type == "windows") {
    expect_no_error(dims <- latex_dims("\\text{\u4F60\u597D\u4E16\u754C}", gp = grid::gpar(fontsize = 20)))
  } else {
    expect_silent(dims <- latex_dims("\\text{\u4F60\u597D\u4E16\u754C}", gp = grid::gpar(fontsize = 20)))
  }
  expect_true(grid::convertWidth(dims$width, "bigpts", valueOnly = TRUE) > 0)
})
