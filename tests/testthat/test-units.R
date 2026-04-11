test_that("unit conversions are correct, vectorized, and invertible", {
  # Forward conversions
  expect_equal(gridmicrotex:::tex_pt_to_bigpt(1), 72 / 72.27, tolerance = 1e-6)
  expect_equal(gridmicrotex:::tex_pt_to_bigpt(0), 0)

  # Vectorized
  result <- gridmicrotex:::tex_pt_to_bigpt(c(72.27, 144.54))
  expect_equal(result, c(72, 144), tolerance = 1e-4)

  # Inverse round-trip
  expect_equal(
    gridmicrotex:::bigpt_to_tex_pt(gridmicrotex:::tex_pt_to_bigpt(100)),
    100, tolerance = 1e-9
  )
})

test_that("tex_pt_to_bigpt scales proportionally", {
  expect_equal(
    gridmicrotex:::tex_pt_to_bigpt(20),
    2 * gridmicrotex:::tex_pt_to_bigpt(10),
    tolerance = 1e-9
  )
})
