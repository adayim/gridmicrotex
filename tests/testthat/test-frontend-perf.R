# The front end has to carry a whole document, not just a label. These are
# timings, so they are generous and never run on CRAN; the shape of the
# curve is what matters, not the constant.

skip_on_cran()

section <- function(i) paste0(
  "\\section{Results for group ", i, "}\n\n",
  paste(rep(paste0("We fitted the model and found that the slope $\\beta_", i,
                   "$ was larger than expected, with $p < 0.001$ across all of ",
                   "the replicates examined in this part of the study."), 4),
        collapse = " "),
  "\n\n$$ \\sum_{k=1}^{n} x_k^2 = \\frac{a+b}{c} $$\n\n",
  "\\begin{itemize}\\item first point \\item second point\\end{itemize}\n\n",
  paste(rep("A further paragraph of prose, with an em dash --- and a quote ``like this''.", 3),
        collapse = " "), "\n\n")

document <- function(n) paste(vapply(seq_len(n), section, ""), collapse = "")

# The best of a few runs: a slow one says the machine was busy, not that
# the parser is slow.
timing <- function(tex, runs = 3) {
  min(replicate(runs, {
    latex_cache_clear()
    t0 <- Sys.time()
    suppressWarnings(latex_dims(tex, input_mode = "document", max_width = 500))
    as.numeric(Sys.time() - t0, units = "secs")
  }))
}

test_that("a 40 KB document is laid out in well under a second", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- document(40)
  expect_gt(nchar(tex), 38 * 1024)
  # The target is 0.3 s; 1 s leaves room for a loaded CI machine.
  expect_lt(timing(tex), 1)
})

test_that("four times the document costs far less than four times the time", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  small <- timing(document(10))
  large <- timing(document(40))
  # Linear would be 4x; the plan allows 5x. Anything quadratic is 16x.
  expect_lt(large / small, 5)
})

test_that("a book-length body is within the expansion caps", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # 10,000 expansions was too tight: this reached it at about 16,000.
  para <- paste(rep("We found \\emph{clear} evidence for the effect here.", 8),
                collapse = " ")
  tex <- paste(rep(para, 2000), collapse = "\n\n")
  expect_gt(nchar(tex), 800 * 1024)
  expect_silent(suppressWarnings(latex_dims(tex, input_mode = "document", max_width = 500)))
})

test_that("a macro defined in terms of itself is still stopped, and quickly", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # The count cap is the net for a recursion that expands to nothing, so
  # raising it must not make this one slow.
  for (tex in c("\\def\\a{\\a}\\a", "\\def\\c{\\c\\c}\\c")) {
    t0 <- Sys.time()
    expect_error(latex_dims(tex, input_mode = "document"), "Too many macro expansions",
                 label = tex)
    expect_lt(as.numeric(Sys.time() - t0, units = "secs"), 5)
  }
})
