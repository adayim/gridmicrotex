test_that("resolve_font_family returns 'serif' for known TeX font names", {
  expect_equal(gridmicrotex:::resolve_font_family("cmr"), "serif")
  expect_equal(gridmicrotex:::resolve_font_family("cmmi"), "serif")
  expect_equal(gridmicrotex:::resolve_font_family("cmsy"), "serif")
  expect_equal(gridmicrotex:::resolve_font_family("cmex"), "serif")
  expect_equal(gridmicrotex:::resolve_font_family("lmmath"), "serif")
})

test_that("resolve_font_family falls back to 'serif' for unknown names", {
  expect_equal(gridmicrotex:::resolve_font_family("unknown_font"), "serif")
  expect_equal(gridmicrotex:::resolve_font_family(""), "serif")
  expect_equal(gridmicrotex:::resolve_font_family("arial"), "serif")
})

test_that("resolve_font_face returns correct face for style 0 (plain)", {
  expect_equal(gridmicrotex:::resolve_font_face(0), "plain")
})

test_that("resolve_font_face returns correct face for style 1 (bold)", {
  expect_equal(gridmicrotex:::resolve_font_face(1), "bold")
})

test_that("resolve_font_face returns correct face for style 2 (italic)", {
  expect_equal(gridmicrotex:::resolve_font_face(2), "italic")
})

test_that("resolve_font_face returns correct face for style 3 (bold.italic)", {
  expect_equal(gridmicrotex:::resolve_font_face(3), "bold.italic")
})

test_that("resolve_font_face falls back to 'plain' for unknown style", {
  expect_equal(gridmicrotex:::resolve_font_face(99), "plain")
  expect_equal(gridmicrotex:::resolve_font_face(-1), "plain")
})
