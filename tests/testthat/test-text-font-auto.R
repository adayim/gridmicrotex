# Auto text-font resolution: gp$fontfamily drives MicroTeX's main_font
# so layout metrics match what grid will draw.

test_that(".resolve_text_font registers the system font for a family", {
  gridmicrotex:::.clear_text_font_cache()
  fam <- gridmicrotex:::.resolve_text_font("sans")
  # Some CI images (notably minimal Ubuntu runners) resolve fontconfig's
  # "sans" alias to a font that happens to carry an OT MATH table, in
  # which case MicroTeX registers it as a *math* font and .resolve_text_font
  # returns "" so the caller falls back to default text metrics. Skip
  # the registry check there; the invariant we care about is: if we DID
  # resolve a text font, it's discoverable via main_font_families().
  skip_if(!nzchar(fam), "No non-math system font resolvable for 'sans' here.")
  expect_true(fam %in% microtex_main_font_families())
})

test_that("second resolve of the same family hits the in-process cache", {
  gridmicrotex:::.clear_text_font_cache()
  gridmicrotex:::.resolve_text_font("sans")
  t <- system.time(gridmicrotex:::.resolve_text_font("sans"))["elapsed"]
  expect_lt(t, 0.05)
})

test_that("unknown family falls back without erroring (systemfonts returns default)", {
  expect_silent(fam <- gridmicrotex:::.resolve_text_font("TotallyNotAFontFamily123"))
  # systemfonts picks a fallback; we should still end up with *some* registered family.
  expect_true(is.character(fam))
})

test_that("latex_grob renders with gp$fontfamily and registers main_font", {
  g1 <- latex_grob("\\text{Hello} $x^2$",
                   gp = grid::gpar(fontsize = 16, fontfamily = "sans"))
  expect_s3_class(g1, "latexgrob")
  expect_gt(g1$bbox_w, 0)

  # Passing a different family should also work
  g2 <- latex_grob("\\text{Hello} $x^2$",
                   gp = grid::gpar(fontsize = 16, fontfamily = "serif"))
  expect_s3_class(g2, "latexgrob")
})

test_that("latex_dims accepts a fontfamily via gp and returns finite dims", {
  d <- latex_dims("\\text{Width test}", gp = grid::gpar(fontsize = 12, fontfamily = "sans"))
  expect_true(is.list(d))
  expect_true(is.finite(as.numeric(d$width)))
  expect_gt(as.numeric(d$width), 0)
})
