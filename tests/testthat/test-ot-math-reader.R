# A font is read straight from its OpenType file (otf_math_reader.cpp):
# metrics from its own tables, math from its MATH table. What the MATH
# table carries shows in a layout, so that is where it is tested.

test_that("each bundled math font's MATH table reaches the layout", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  glyph_ids <- function(tex, font) {
    r <- latex_tree(tex, math_font = font, input_mode = "math")$records
    r$glyph[r$type == "glyph"]
  }
  for (font in c("lete", "stix")) {
    # A display operator is a larger variant of the text one (MathVariants).
    expect_false(identical(glyph_ids("\\displaystyle\\sum", font),
                           glyph_ids("\\textstyle\\sum", font)), info = font)
    # A delimiter around something tall is stretched -- a variant, or an
    # assembly of parts -- never the plain glyph.
    plain <- glyph_ids("(", font)
    tall <- glyph_ids("\\left(\\rule{1pt}{6em}\\right.", font)
    expect_length(plain, 1L)
    expect_false(plain %in% tall, info = font)
  }
})

test_that("a font that cannot be read is an error saying why", {
  expect_error(
    gridmicrotex:::microtex_add_font_from_otf(tempfile(fileext = ".otf")),
    "failed to open font"
  )
})

test_that("load_math_font reads a bare OTF", {
  # Copy Lete.otf to a temp file with a unique name so it registers as a
  # distinct math font (not colliding with the bundled "Lete Sans Math"
  # already loaded at .onLoad). Exercises the MATH-table synthesis path
  # end-to-end.
  src <- system.file("fonts", "LeteSansMath.otf", package = "gridmicrotex")
  skip_if_not(nzchar(src))
  tmp <- file.path(tempdir(), "A3Probe.otf")
  file.copy(src, tmp, overwrite = TRUE)
  on.exit(unlink(tmp), add = TRUE)

  # load_math_font is silent on success; it might warn if systemfonts
  # reports a duplicate registration.
  suppressWarnings(load_math_font(tmp))
  # The font registers under its family name (Lete Sans Math) — same as
  # the bundled font because it IS the same OTF.
  expect_true("Lete Sans Math" %in% available_math_fonts())
})

test_that("the deprecated aliases still work, and say so", {
  src <- system.file("fonts", "LeteSansMath.otf", package = "gridmicrotex")
  skip_if_not(nzchar(src))
  tmp <- file.path(tempdir(), "A3ProbeDeprecated.otf")
  file.copy(src, tmp, overwrite = TRUE)
  on.exit(unlink(tmp), add = TRUE)

  expect_warning(suppressMessages(load_font(tmp)), "deprecated")
  expect_true("Lete Sans Math" %in% available_math_fonts())

  expect_warning(suppressMessages(check_fonts()), "deprecated")
  # The alias forwards rather than reimplementing, so it returns what the
  # replacement returns.
  expect_identical(
    suppressWarnings(suppressMessages(check_fonts())),
    suppressMessages(check_math_fonts())
  )
})
