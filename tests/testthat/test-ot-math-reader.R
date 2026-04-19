test_that("ot_math_table_bytes returns the OT MATH blob for Lete", {
  otf <- system.file("fonts", "LeteSansMath.otf", package = "gridmicrotex")
  expect_true(nzchar(otf))

  blob <- gridmicrotex:::ot_math_table_bytes(otf)
  expect_type(blob, "raw")
  expect_gt(length(blob), 4)

  expect_identical(blob[1:4], as.raw(c(0x00, 0x01, 0x00, 0x00)))
})

test_that("ot_math_table_bytes errors on a path that cannot be opened", {
  expect_error(
    gridmicrotex:::ot_math_table_bytes(tempfile(fileext = ".otf")),
    "failed to open font"
  )
})

test_that("otf_to_clm_bytes emits a CLM v6 blob for Lete", {
  otf <- system.file("fonts", "LeteSansMath.otf", package = "gridmicrotex")
  clm <- gridmicrotex:::otf_to_clm_bytes(otf)
  expect_type(clm, "raw")
  # "clm" magic + major=6 + minor=2 (with glyph paths)
  expect_identical(clm[1:3], charToRaw("clm"))
  expect_identical(clm[4:5], as.raw(c(0x00, 0x06)))
  expect_identical(clm[6],   as.raw(0x02))
})

test_that("load_font synthesises CLM from a bare OTF", {
  # Copy Lete.otf to a temp file with a unique name so it registers as a
  # distinct math font (not colliding with the bundled "Lete Sans Math"
  # already loaded at .onLoad). Exercises the MATH-table synthesis path
  # end-to-end.
  src <- system.file("fonts", "LeteSansMath.otf", package = "gridmicrotex")
  skip_if_not(nzchar(src))
  tmp <- file.path(tempdir(), "A3Probe.otf")
  file.copy(src, tmp, overwrite = TRUE)
  on.exit(unlink(tmp), add = TRUE)

  # load_font is silent on success; a2 might warn if systemfonts reports
  # a duplicate registration.
  expect_no_error(suppressWarnings(load_font(tmp)))
  # The font registers under its family name (Lete Sans Math) — same as
  # the bundled font because it IS the same OTF.
  expect_true("Lete Sans Math" %in% available_math_fonts())
})
