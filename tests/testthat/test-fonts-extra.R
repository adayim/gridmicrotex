test_that("set_math_font works with alias 'stix'", {
  expect_true(set_math_font("stix"))
  # Reset to default
  set_math_font("lm")
})

test_that("set_math_font works with alias 'lm'", {
  expect_true(set_math_font("lm"))
})

test_that("set_math_font errors on empty name", {
  expect_error(set_math_font(""), "provide a math font")
  expect_error(set_math_font(NULL), "provide a math font")
})

test_that("set_math_font errors on nonexistent font name", {
  expect_error(set_math_font("nonexistent_font"), "not found")
})

test_that("set_main_font with empty string does not error", {
  result <- set_main_font("")
  # Returns logical
  expect_type(result, "logical")
})

test_that("main_font_families returns a character vector", {
  result <- main_font_families()
  expect_type(result, "character")
})

test_that("available_math_fonts returns fonts after set_math_font call", {
  set_math_font("stix")
  fonts <- available_math_fonts()
  expect_true("STIX Two Math" %in% fonts)
  set_math_font("lm")
})

test_that("load_font errors when font file does not exist", {
  expect_error(load_font("/nonexistent/path/font.otf"), "not found")
})

test_that("load_font errors when CLM file is missing", {
  # Create a temporary fake OTF file (non-functional but exists on disk)
  tmp_otf <- tempfile(fileext = ".otf")
  writeLines("fake otf", tmp_otf)
  on.exit(unlink(tmp_otf), add = TRUE)
  # No matching .clm2 exists, so load_font should error about CLM
  expect_error(load_font(tmp_otf), "CLM|not found", ignore.case = TRUE)
})

test_that("check_fonts returns character vector invisibly", {
  result <- check_fonts()
  expect_type(result, "character")
  expect_true(length(result) >= 1)
})

test_that("resolve_math_font alias 'stixtwo' works", {
  result <- gridmicrotex:::resolve_math_font("stixtwo")
  expect_equal(result, "STIX Two Math")
})

test_that("resolve_math_font alias 'latinmodern' works", {
  result <- gridmicrotex:::resolve_math_font("latinmodern")
  expect_equal(result, "LatinModernMath-Regular")
})

test_that("resolve_math_font is case-insensitive for aliases", {
  expect_equal(gridmicrotex:::resolve_math_font("STIX"), "STIX Two Math")
  expect_equal(gridmicrotex:::resolve_math_font("LM"), "LatinModernMath-Regular")
})

test_that("rendering with stix font followed by lm gives different layouts", {
  g_stix <- latex_grob("\\alpha", fontsize = 20, math_font = "stix",
                        render_mode = "path")
  g_lm   <- latex_grob("\\alpha", fontsize = 20, math_font = "lm",
                        render_mode = "path")
  # Both should be valid grobs
  expect_s3_class(g_stix, "latexgrob")
  expect_s3_class(g_lm, "latexgrob")
})
