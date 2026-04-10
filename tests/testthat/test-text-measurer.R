test_that(".make_text_measurer returns a function", {
  measurer <- gridmicrotex:::.make_text_measurer(grid::gpar())
  expect_type(measurer, "closure")
})

test_that("text measurer returns 3-element numeric vector", {
  measurer <- gridmicrotex:::.make_text_measurer(grid::gpar())
  result <- measurer("Hello", 0L)
  expect_type(result, "double")
  expect_length(result, 3)
  # All values should be positive for non-empty text

  expect_true(all(result > 0))
})

test_that("text measurer width scales with text length", {
  measurer <- gridmicrotex:::.make_text_measurer(grid::gpar())
  short <- measurer("Hi", 0L)
  long <- measurer("Hello World", 0L)
  # Longer text should have greater width ratio
  expect_true(long[1] > short[1])
})

test_that("text measurer respects fontfamily", {
  measurer_sans <- gridmicrotex:::.make_text_measurer(
    grid::gpar(fontfamily = "sans")
  )
  measurer_mono <- gridmicrotex:::.make_text_measurer(
    grid::gpar(fontfamily = "mono")
  )
  sans_result <- measurer_sans("Hello World", 0L)
  mono_result <- measurer_mono("Hello World", 0L)
  # Different fonts should generally produce different widths
  # (sans vs mono have notably different metrics)
  expect_false(isTRUE(all.equal(sans_result[1], mono_result[1])))
})

test_that("text measurer handles bold style", {
  measurer <- gridmicrotex:::.make_text_measurer(grid::gpar())
  # FontStyle: rm=1, bf=2, it=4
  plain <- measurer("Hello", 0L)
  bold <- measurer("Hello", 2L)
  # Bold text is typically wider
  expect_true(bold[1] >= plain[1])
})

test_that("text measurer handles italic style", {
  measurer <- gridmicrotex:::.make_text_measurer(grid::gpar())
  result <- measurer("Hello", 4L)
  expect_length(result, 3)
  expect_true(all(result > 0))
})

test_that("text measurer handles bold-italic style", {
  measurer <- gridmicrotex:::.make_text_measurer(grid::gpar())
  result <- measurer("Hello", 6L)  # bf=2 | it=4
  expect_length(result, 3)
  expect_true(all(result > 0))
})

test_that("register/clear text measurer round-trip", {
  measurer <- gridmicrotex:::.make_text_measurer(grid::gpar())
  # Should not error
  expect_silent(register_text_measurer(measurer))
  expect_silent(clear_text_measurer())
})

test_that("clear_text_measurer is safe to call when none registered", {
  # Calling clear when nothing is registered should not error
  expect_silent(clear_text_measurer())
})

test_that("register_text_measurer replaces previous measurer", {
  m1 <- gridmicrotex:::.make_text_measurer(grid::gpar(fontfamily = "sans"))
  m2 <- gridmicrotex:::.make_text_measurer(grid::gpar(fontfamily = "mono"))
  # Registering twice should not error (first is released)
  expect_silent(register_text_measurer(m1))
  expect_silent(register_text_measurer(m2))
  expect_silent(clear_text_measurer())
})

test_that("text measurer affects CJK text layout dimensions", {
  # CJK text goes through \text{} path and triggers the measurer callback.
  # With the measurer, dimensions should come from R's font metrics
  # rather than the heuristic (0.55em per char).
  if (.Platform$OS.type == "windows") {
    expect_no_error({
      dims <- latex_dims("\\text{\u4F60\u597D\u4E16\u754C}", fontsize = 20)
    })
  } else {
    expect_silent({
      dims <- latex_dims("\\text{\u4F60\u597D\u4E16\u754C}", fontsize = 20)
    })
  }
  w <- grid::convertWidth(dims$width, "bigpts", valueOnly = TRUE)
  h <- grid::convertHeight(dims$height, "bigpts", valueOnly = TRUE)
  expect_true(w > 0)
  expect_true(h > 0)
})

test_that("latex_grob with fontfamily uses measurer for layout", {
  # When fontfamily is set, the measurer uses R's metrics for that font.
  # This should produce a valid grob with positive dimensions.
  g <- latex_grob("x + \\text{\u4F60\u597D}",
                  fontsize = 20,
                  gp = grid::gpar(fontfamily = "sans"))
  expect_s3_class(g, "latexgrob")
  w <- grid::convertWidth(grid::grobWidth(g), "bigpts", valueOnly = TRUE)
  expect_true(w > 0)
})

test_that(".resolve_text_face handles all style values", {
  expect_equal(gridmicrotex:::.resolve_text_face(0L), "plain")
  expect_equal(gridmicrotex:::.resolve_text_face(1L), "plain")   # rm
  expect_equal(gridmicrotex:::.resolve_text_face(2L), "bold")    # bf
  expect_equal(gridmicrotex:::.resolve_text_face(4L), "italic")  # it
  expect_equal(gridmicrotex:::.resolve_text_face(6L), "bold.italic")  # bf|it
  expect_equal(gridmicrotex:::.resolve_text_face(NA_integer_), "plain")
})

test_that("text measurer works without active graphics device", {
  # The measurer should open a temporary PDF device if none is active.
  # This test is meaningful primarily when no device is open (e.g., in CI).
  measurer <- gridmicrotex:::.make_text_measurer(grid::gpar())
  result <- measurer("Test", 0L)
  expect_length(result, 3)
  expect_true(all(result > 0))
})

test_that("text measurer returns ratios relative to font size", {
  measurer <- gridmicrotex:::.make_text_measurer(grid::gpar())
  result <- measurer("M", 0L)
  # Width ratio for a single character should be reasonable (0.3 - 1.5)
  expect_true(result[1] > 0.3 && result[1] < 1.5)
  # Ascent ratio should be positive and less than 1.5
  expect_true(result[2] > 0 && result[2] < 1.5)
  # Height ratio should be >= ascent ratio
  expect_true(result[3] >= result[2])
})

test_that("latex_dims uses measurer (not heuristic) for text", {
  # With the measurer, different fontfamily in gpar would change metrics.
  # Since latex_dims uses default gpar, just verify it runs and
  # produces valid results with the measurer active.
  dims <- latex_dims("\\text{Hello World}", fontsize = 20)
  w <- grid::convertWidth(dims$width, "bigpts", valueOnly = TRUE)
  h <- grid::convertHeight(dims$height, "bigpts", valueOnly = TRUE)
  expect_true(w > 0)
  expect_true(h > 0)
})
