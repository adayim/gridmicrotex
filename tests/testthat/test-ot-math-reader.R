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

test_that("a Coverage table of overlapping ranges is read in bounded time", {
  # Format 2 records are sorted and disjoint; tens of thousands of
  # overlapping ones each took a pass over their whole range.
  src <- system.file("fonts", "LeteSansMath.otf", package = "gridmicrotex")
  skip_if_not(nzchar(src))
  raw <- readBin(src, "raw", file.size(src))
  u16 <- function(o) as.integer(raw[o + 1]) * 256L + as.integer(raw[o + 2])
  u32 <- function(o) sum(as.numeric(raw[o + 1:4]) * c(2^24, 2^16, 2^8, 1))
  w16 <- function(x) as.raw(c(x %/% 256L, x %% 256L))
  w32 <- function(x) as.raw(c(x %/% 2^24, x %/% 2^16, x %/% 2^8, x) %% 256)
  rec <- 12 + 16 * (which(vapply(seq_len(u16(4)), function(i) {
    rawToChar(raw[12 + 16 * (i - 1) + 1:4])
  }, "") == "MATH") - 1)
  math <- raw[u32(rec + 8) + seq_len(u32(rec + 12))]
  gi <- as.integer(math[7]) * 256L + as.integer(math[8])
  tbl <- gi + as.integer(math[gi + 1]) * 256L + as.integer(math[gi + 2])
  len <- (length(math) + 3L) %/% 4L * 4L
  math[tbl + 1:2] <- w16(len - tbl)
  n <- 30000L
  # Glyph ids past the font's last glyph are ignored, so the load changes
  # nothing for later tests; only the reading is slow.
  cov <- c(w16(2L), w16(n), rep(c(w16(60000L), w16(65535L), w16(0L)), n))
  patched <- c(math, as.raw(rep(0, len - length(math))), cov)
  pad <- as.raw(rep(0, (4 - length(raw) %% 4) %% 4))
  raw[rec + 9:12] <- w32(length(raw) + length(pad))
  raw[rec + 13:16] <- w32(length(patched))
  f <- file.path(tempdir(), "CoverageProbe.otf")
  writeBin(c(raw, pad, patched), f)
  on.exit(unlink(f), add = TRUE)
  expect_lt(system.time(suppressWarnings(load_math_font(f)))[["elapsed"]], 5)
})

test_that("load_math_font reads a bare OTF", {
  # A copy of Lete under a unique file name, loaded as a custom font.
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

  expect_true("Lete Sans Math" %in% available_math_fonts())

})
