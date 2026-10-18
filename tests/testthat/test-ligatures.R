# TeX's text ligatures are tested with the engine, in the MicroTeX fork
# (test/test_text.cpp). Markdown is not read as TeX's text, which is R's.

test_that("markdown keeps its hyphens and quotes, as CommonMark does", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  g <- markdown_grob("use `--verbose` here")
  expect_true("--verbose" %in% g$layout_df$text)
  # Markdown prose is not read as TeX's ligatures.
  txt <- paste(markdown_grob("run with --verbose, don't `x` it's ``q''")$layout_df$text,
               collapse = "")
  expect_match(txt, "--verbose, don't", fixed = TRUE)
  expect_false(grepl("[–—‘’“”]", txt))
})
