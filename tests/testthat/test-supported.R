# inst/supported/examples.tsv is the list of what the package draws, which
# build.R sets as a document: so each row is a claim, and checked here. An
# "ok" row is drawn with no warning, and LaTeX sets it too (the .tex compiles
# with pdflatex); "nolatex" is drawn with no warning and has no LaTeX
# equivalent; a "no" row is one the package does not draw, and still warns of
# it -- when one is supported, its row moves up.

examples <- function() {
  path <- system.file("supported/examples.tsv", package = "gridmicrotex")
  skip_if(!nzchar(path), "the examples are not installed")
  utils::read.delim(path, quote = "", comment.char = "", stringsAsFactors = FALSE,
                    encoding = "UTF-8")
}

warnings_of <- function(tex) {
  out <- character(0)
  withCallingHandlers(
    latex_tree(tex, input_mode = "math"),
    warning = function(x) {
      out <<- c(out, conditionMessage(x))
      invokeRestart("muffleWarning")
    })
  out
}

test_that("every example listed as supported is drawn with no warning", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  ex <- examples()
  ok <- ex[ex$kind %in% c("ok", "nolatex"), ]
  expect_gt(nrow(ok), 900L)
  bad <- vapply(ok$source, function(s) length(warnings_of(s)) > 0L, NA)
  expect_identical(ok$source[bad], character(0))
})

test_that("an example listed as not supported still warns", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  ex <- examples()
  no <- ex[ex$kind == "no", ]
  # None is listed now: every function of KaTeX's two lists is drawn.
  drawn <- vapply(no$source, function(s) length(warnings_of(s)) == 0L, NA)
  expect_identical(no$source[drawn], character(0))
})

test_that("the sections are KaTeX's, and have rows", {
  ex <- examples()
  sections <- unique(sub(" / .*", "", ex$section))
  for (s in c("Accents", "Delimiters", "Environments", "Operators", "Relations", "Symbols and Punctuation",
              "Beyond KaTeX", "Extensions")) {
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
