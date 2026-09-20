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

test_that("doubled quotes are the curly ones", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(drawn("``q''"), "\u201cq\u201d")
  # A single one is left alone: TeX's font maps it, we do not.
  expect_identical(drawn("don't"), "don't")
  expect_identical(drawn("`single'"), "`single'")
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
  # A URL is verbatim, whatever is in it.
  expect_identical(drawn("\\url{http://a--b.com}"), "http://a--b.com")
})

test_that("a markdown code span keeps its hyphens, and its prose does not", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  g <- markdown_grob("use `--verbose` here")
  expect_true("--verbose" %in% g$layout_df$text)
  g <- markdown_grob("a -- b")
  expect_true(any(grepl("\u2013", g$layout_df$text, fixed = TRUE)))
})

test_that("a reference, citation or footnote warns and draws what LaTeX draws", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # LaTeX draws ?? for a reference it cannot resolve, and the key in
  # brackets for a citation. None of them is an unknown command any more.
  expect_warning(latex_dims("see \\ref{fig:a}", input_mode = "document"),
                 "reference `fig:a' is undefined")
  expect_identical(drawn("see \\ref{fig:a} there"), "see |??| there")
  expect_identical(drawn("eq \\eqref{eq:1}"), "eq |(??)")
  expect_identical(drawn("page \\pageref{p}"), "page |??")
  # The key is set as written: a `_` or `:` in one is not LaTeX here.
  expect_warning(latex_dims("\\cite{smith_2020}", input_mode = "document"),
                 "citation `smith_2020' is undefined")
  expect_identical(drawn("as in \\cite{smith_2020} here"), "as in |[smith_2020]| here")
  # Its optional note has nothing to be a note on.
  expect_identical(drawn("\\cite[p. 5]{key}"), "[key]")
  # There is no page for a footnote, so its text is set where it stands.
  expect_warning(latex_dims("text\\footnote{a note} on", input_mode = "document"),
                 "no page to put a note on")
  expect_identical(drawn("text\\footnote{a note} on"), "text|a note| on")
})
