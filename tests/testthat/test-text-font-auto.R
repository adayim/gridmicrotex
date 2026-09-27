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

test_that("unknown family falls back to a registered font, or to no font", {
  expect_silent(fam <- gridmicrotex:::.resolve_text_font("TotallyNotAFontFamily123"))
  expect_length(fam, 1L)
  # Only two answers are usable: a family MicroTeX has actually registered,
  # or "" meaning "fall back to the default text metrics". Anything else is
  # a name the layout engine will not find, and metrics silently go wrong.
  if (nzchar(fam)) {
    expect_true(fam %in% microtex_main_font_families())
  } else {
    expect_identical(fam, "")
  }
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

test_that("a TrueType Collection that points past its own end is rejected", {
  cache <- file.path(tempdir(), "gm-ttc-cache")
  old <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
  Sys.setenv(R_USER_CACHE_DIR = cache)
  on.exit({
    if (is.na(old)) Sys.unsetenv("R_USER_CACHE_DIR")
    else Sys.setenv(R_USER_CACHE_DIR = old)
    unlink(cache, recursive = TRUE)
  }, add = TRUE)
  u32 <- gridmicrotex:::.w_u32
  u16 <- gridmicrotex:::.w_u16
  ttc <- tempfile(fileext = ".ttc")

  # 44 bytes: one face whose one table claims to be 1 MiB long. R pads an
  # out-of-range raw index with zero bytes rather than failing, so this
  # wrote a 1 MiB cache file of zeros.
  writeBin(c(charToRaw("ttcf"), as.raw(c(0, 1, 0, 0)), u32(1), u32(16),
             as.raw(c(0, 1, 0, 0)), u16(1), as.raw(rep(0, 6)),
             charToRaw("abcd"), as.raw(rep(0, 4)), u32(44), u32(1048576)),
           ttc)
  expect_error(gridmicrotex:::.extract_ttc_face(ttc, 0L), "Malformed")
  expect_length(list.files(cache, recursive = TRUE), 0L)

  # A face that starts past the end of the file.
  writeBin(c(charToRaw("ttcf"), as.raw(c(0, 1, 0, 0)), u32(1), u32(4096)), ttc)
  expect_error(gridmicrotex:::.extract_ttc_face(ttc, 0L), "Malformed")
})
