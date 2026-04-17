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

test_that("measurer cache returns identical values to a fresh measurement", {
  # Within one closure, repeat calls hit the cache; they must equal the
  # first (un-cached) call bit-for-bit, and must also match a separate
  # fresh closure's first (un-cached) call.
  txt <- "The quick brown fox jumps over the lazy dog"
  m1 <- gridmicrotex:::.make_text_measurer(grid::gpar())
  first  <- m1(txt, 0L)
  second <- m1(txt, 0L)  # cache hit
  expect_identical(first, second)

  m2 <- gridmicrotex:::.make_text_measurer(grid::gpar())
  fresh  <- m2(txt, 0L)  # un-cached, separate closure
  expect_identical(first, fresh)

  # Different font_style must not collide with a cached entry.
  bold_cached <- m1(txt, 2L)  # first time for style=2 on m1
  bold_fresh  <- m2(txt, 2L)
  expect_identical(bold_cached, bold_fresh)
  expect_false(identical(first, bold_cached))
})
