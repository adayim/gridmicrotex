# inst/supported/examples.tsv is the list of what the package draws, which
# build.R sets as a document. That each row is drawn as it says (an "ok" row
# with no warning and a "no" row with one) is tested with the engine, in the
# MicroTeX fork (test/test_supported.cpp); these check the list and the
# document built from it.

examples <- function() {
  path <- system.file("supported/examples.tsv", package = "gridmicrotex")
  skip_if(!nzchar(path), "the examples are not installed")
  utils::read.delim(path, quote = "", comment.char = "", stringsAsFactors = FALSE,
                    encoding = "UTF-8")
}


test_that("the sections have rows", {
  ex <- examples()
  sections <- unique(sub(" / .*", "", ex$section))
  for (s in c("Accents", "Delimiters", "Environments", "Operators", "Relations", "Symbols and Punctuation",
              "Further LaTeX", "Extensions")) {
    expect_true(s %in% sections, info = s)
  }
  expect_false(anyNA(ex$source))
  expect_true(all(ex$kind %in% c("ok", "nolatex", "no")))
})

test_that("the document is built: a LaTeX file, and a PDF with a page number on every page", {
  skip_on_cran()
  skip_if_not_installed("grid")
  path <- system.file("supported/build.R", package = "gridmicrotex")
  skip_if(!nzchar(path), "the script is not installed")
  env <- new.env()
  sys.source(path, env)
  dir <- tempfile("supported")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  # pdf(): its page objects can be counted, and it needs no installed font (cairo_pdf is the default).
  # A few examples are text beyond pdf()'s encoding (an em dash, \OE, ...), which it warns of and
  # leaves out; R CMD check turns that warning into an error, and the page count is what is tested.
  strict <- Sys.getenv("_R_CHECK_MBCS_CONVERSION_FAILURE_", unset = NA)
  Sys.unsetenv("_R_CHECK_MBCS_CONVERSION_FAILURE_")
  if (!is.na(strict)) {
    on.exit(Sys.setenv("_R_CHECK_MBCS_CONVERSION_FAILURE_" = strict), add = TRUE)
  }
  made <- suppressWarnings(env$build_supported(dir, device = "pdf"))
  expect_true(all(file.exists(made)))
  tex <- readLines(made[["tex"]], encoding = "UTF-8")
  pages <- sum(tex == "\\newpage") + 1L
  expect_gt(pages, 5L)
  expect_true(any(tex == "\\begin{document}") && any(tex == "\\end{document}"))
  # Every example is in it, in its table.
  ex <- examples()
  expect_gt(sum(grepl("\\\\verb", tex)), 0.9 * sum(ex$kind != "no"))
  # Three examples to a row where they fit, and only ASCII, which LaTeX reads as it is.
  expect_gt(sum(grepl("\\hspace{14pt}}l@{\\hspace{4pt}}l@{\\hspace{14pt}}l@{\\hspace{4pt}}l@{}", tex, fixed = TRUE)), 5L)
  expect_false(any(grepl("[^ -~]", tex)))
  # The PDF has as many pages as the file.
  bytes <- readBin(made[["pdf"]], "raw", file.size(made[["pdf"]]))
  bytes[bytes == as.raw(0L)] <- as.raw(32L)
  pdf_text <- rawToChar(bytes)
  found <- gregexpr("/Type /Page ", pdf_text, fixed = TRUE, useBytes = TRUE)[[1]]
  expect_equal(sum(found > 0L), pages)
})
