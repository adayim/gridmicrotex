test_that("tex_pt_to_bigpt converts TeX points to big points", {
  # 1 TeX point = 72/72.27 big points ≈ 0.99627 bigpts
  result <- gridmicrotex:::tex_pt_to_bigpt(1)
  expect_equal(result, 72 / 72.27, tolerance = 1e-6)
})

test_that("bigpt_to_tex_pt converts big points to TeX points", {
  result <- gridmicrotex:::bigpt_to_tex_pt(1)
  expect_equal(result, 72.27 / 72, tolerance = 1e-6)
})

test_that("tex_pt_to_bigpt and bigpt_to_tex_pt are inverse operations", {
  original <- 100
  expect_equal(
    gridmicrotex:::bigpt_to_tex_pt(gridmicrotex:::tex_pt_to_bigpt(original)),
    original,
    tolerance = 1e-9
  )
  expect_equal(
    gridmicrotex:::tex_pt_to_bigpt(gridmicrotex:::bigpt_to_tex_pt(original)),
    original,
    tolerance = 1e-9
  )
})

test_that("tex_pt_to_bigpt returns 0 for 0 input", {
  expect_equal(gridmicrotex:::tex_pt_to_bigpt(0), 0)
})

test_that("bigpt_to_tex_pt returns 0 for 0 input", {
  expect_equal(gridmicrotex:::bigpt_to_tex_pt(0), 0)
})

test_that("tex_pt_to_bigpt works on numeric vectors", {
  input <- c(72.27, 144.54)
  result <- gridmicrotex:::tex_pt_to_bigpt(input)
  expect_length(result, 2)
  expect_equal(result[1], 72, tolerance = 1e-4)
  expect_equal(result[2], 144, tolerance = 1e-4)
})

test_that("bigpt_to_tex_pt scales proportionally", {
  expect_equal(
    gridmicrotex:::bigpt_to_tex_pt(20),
    2 * gridmicrotex:::bigpt_to_tex_pt(10),
    tolerance = 1e-9
  )
})

test_that("tex_pt_to_bigpt scales proportionally", {
  expect_equal(
    gridmicrotex:::tex_pt_to_bigpt(20),
    2 * gridmicrotex:::tex_pt_to_bigpt(10),
    tolerance = 1e-9
  )
})
