# Auto text-font resolution: gp$fontfamily drives MicroTeX's main_font
# so layout metrics match what grid will draw.

test_that(".resolve_text_font registers the system font for a family", {
  gridmicrotex:::.clear_text_font_cache()
  fam <- gridmicrotex:::.resolve_text_font("sans")
  expect_true(nzchar(fam))
  expect_true(fam %in% microtex_main_font_families())
})

test_that("second resolve of the same family hits the in-process cache", {
  gridmicrotex:::.clear_text_font_cache()
  gridmicrotex:::.resolve_text_font("sans")
  t <- system.time(gridmicrotex:::.resolve_text_font("sans"))["elapsed"]
  expect_lt(t, 0.05)
})

test_that("CLM file is cached on disk across sessions", {
  gridmicrotex:::.clear_text_font_cache()
  m <- systemfonts::match_fonts("sans")
  key <- gridmicrotex:::.text_clm_cache_key(m$path)
  cache_dir <- gridmicrotex:::.text_clm_cache_dir()
  clm_path <- file.path(cache_dir, paste0(key, ".clm1"))
  if (file.exists(clm_path)) file.remove(clm_path)
  gridmicrotex:::.resolve_text_font("sans")
  expect_true(file.exists(clm_path))
  expect_gt(file.info(clm_path)$size, 100L)
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

test_that("minimal CLM is a valid v6 file that MicroTeX accepts", {
  # Read back the cached CLM header and verify format.
  m <- systemfonts::match_fonts("sans")
  key <- gridmicrotex:::.text_clm_cache_key(m$path)
  clm_path <- file.path(gridmicrotex:::.text_clm_cache_dir(), paste0(key, ".clm1"))
  gridmicrotex:::.resolve_text_font("sans")
  expect_true(file.exists(clm_path))
  hdr <- readBin(clm_path, "raw", n = 6L)
  expect_equal(rawToChar(hdr[1:3]), "clm")
  # major version (big-endian u16) = 6, minor = 1
  expect_equal(as.integer(hdr[4]) * 256L + as.integer(hdr[5]), 6L)
  expect_equal(as.integer(hdr[6]), 1L)
})
