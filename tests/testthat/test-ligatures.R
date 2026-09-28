# TeX's text ligatures, and the commands a single grob cannot carry out.

drawn <- function(tex, mode = "document") {
  t <- suppressWarnings(latex_tree(tex, input_mode = mode))
  r <- t$records
  paste(r$text[r$type == "text" & !is.na(r$text)], collapse = "|")
}

test_that("a run of hyphens is TeX's dash", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(drawn("a--b"), "a\u2013b")        # en dash
  expect_identical(drawn("a---b"), "a\u2014b")       # em dash
  # Four is the em dash and a hyphen, as TeX reads them left to right.
  expect_identical(drawn("a----b"), "a\u2014-b")
  # One is a hyphen, and spaced ones are not a ligature at all.
  expect_identical(drawn("a-b"), "a-b")
  expect_identical(drawn("a - b"), "a - b")
  expect_identical(drawn("1--5 and 10---20"), "1\u20135 and 10\u201420")
})

test_that("quotes are the curly ones, as TeX's text fonts set them", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(drawn("``q''"), "\u201cq\u201d")
  # A single one too: ` and ' are TeX's opening and closing single quotes,
  # and ' its apostrophe.
  expect_identical(drawn("don't"), "don\u2019t")
  expect_identical(drawn("`single'"), "\u2018single\u2019")
  # Three are the double quote, then the single one.
  expect_identical(drawn("'''"), "\u201d\u2019")
})

test_that("ligatures are a text thing, and not for a typewriter font", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # In math `''` is a double prime and `-` an operator; nothing ligates.
  expect_identical(drawn("$a-b$", "math"), "")
  # Inside \text{} in a formula they do, because that is text.
  expect_identical(drawn("\\text{a--b ``q''}", "math"), "a\u2013b \u201cq\u201d")
  # A typewriter font has no ligatures in TeX, so code keeps its hyphens.
  expect_identical(drawn("\\texttt{--verbose}"), "--verbose")
  expect_identical(drawn("\\texttt{a--b ``q''}"), "a--b ``q''")
  # \tt is a declaration, but its body is typewriter all the same.
  expect_identical(drawn("{\\tt a--b} c--d"), "a--b| c–d")
  # A URL is verbatim, whatever is in it.
  expect_identical(drawn("\\url{http://a--b.com}"), "http://a--b.com")
})

test_that("markdown keeps its hyphens and quotes, as CommonMark does", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  g <- markdown_grob("use `--verbose` here")
  expect_true("--verbose" %in% g$layout_df$text)
  # Markdown prose is not read as TeX's ligatures.
  txt <- paste(markdown_grob("run with --verbose, don't `x` it's ``q''")$layout_df$text,
               collapse = "")
  expect_match(txt, "--verbose, don't", fixed = TRUE)
  expect_false(grepl("[\u2013\u2014\u2018\u2019\u201c\u201d]", txt))
})

test_that("a reference, citation or footnote warns and draws what LaTeX draws", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # LaTeX draws a bold ?? for a reference it cannot resolve, and a bold ?
  # in brackets for a citation.
  bold <- function(tex) {
    r <- suppressWarnings(latex_tree(tex, input_mode = "document"))$records
    r$text[r$type == "text" & bitwAnd(r$font_style, 2L) != 0L]
  }
  expect_warning(latex_dims("see \\ref{fig:a}", input_mode = "document"),
                 "reference `fig:a' is undefined")
  expect_identical(drawn("see \\ref{fig:a} there"), "see |??| there")
  expect_identical(bold("see \\ref{fig:a} there"), "??")
  expect_identical(drawn("eq \\eqref{eq:1}"), "eq |(|??|)")
  expect_identical(drawn("page \\pageref{p}"), "page |??")
  expect_warning(latex_dims("\\cite{smith_2020}", input_mode = "document"),
                 "citation `smith_2020' is undefined: drawn as \\[\\?\\]")
  expect_identical(drawn("as in \\cite{smith_2020} here"), "as in |[|?|]| here")
  expect_identical(bold("as in \\cite{smith_2020} here"), "?")
  # One ? for each key, and the note after them, as LaTeX prints it.
  expect_identical(drawn("\\cite{a, b}"), "[|?|, |?|]")
  expect_identical(drawn("\\cite[p. 5]{key}"), "[|?|, |p. 5|]")
  # There is no page for a footnote, so its text is set where it stands;
  # its optional number has no note to number.
  expect_warning(latex_dims("text\\footnote{a note} on", input_mode = "document"),
                 "no page to put a note on")
  expect_identical(drawn("text\\footnote{a note} on"), "text|a note| on")
  expect_identical(drawn("text\\footnote[2]{a note} on"), "text|a note| on")
})
