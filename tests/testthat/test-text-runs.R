# Consecutive text characters are drawn as one word-level record, not one
# per letter. See RowAtom::processTextRun in
# src/MicroTeX/lib/atom/atom_row.cpp.

texts <- function(tex, ...) {
  r <- latex_tree(tex, input_mode = "math", ...)$records
  r$text[r$type == "text"]
}

test_that("a word is one record, not one per character", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_equal(texts("\\text{Hello}"), "Hello")
  # Punctuation belongs to the word, as it does when R draws the string.
  expect_equal(texts("\\text{Hello, world!}"), c("Hello,", "world!"))
})

test_that("kerning is applied, because the device sees a whole word", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Laid out per character the width is exactly the sum of the advances,
  # which for a kern-heavy string is visibly loose -- AVATAR came out
  # 80.00 against R's 72.22, about 11% too wide.
  gp <- grid::gpar(fontsize = 20, fontfamily = "sans")
  for (s in c("AVATAR", "Wave", "To Vary")) {
    w <- as.numeric(latex_dims(paste0("\\text{", s, "}"),
                               input_mode = "math", gp = gp)$width)
    grid::pushViewport(grid::viewport(gp = gp))
    kerned <- grid::convertWidth(grid::stringWidth(s), "bigpts",
                                 valueOnly = TRUE)
    grid::popViewport()
    # Within a big point: latex_dims() reports whole big points.
    expect_lt(abs(w - kerned), 1)
  }
})

test_that("the output holds the word, so a viewer can find it", {
  skip_if_not_installed("svglite")
  f <- tempfile(fileext = ".svg")
  on.exit(unlink(f), add = TRUE)
  svglite::svglite(f, width = 5, height = 1)
  grid.latex("\\text{Hello world}", input_mode = "math",
             gp = grid::gpar(fontsize = 20))
  dev.off()
  svg <- paste(readLines(f, warn = FALSE), collapse = "\n")
  # One element per word, and the word is searchable. Per character it
  # was ten elements and no element held more than one letter.
  expect_equal(length(gregexpr("<text", svg, fixed = TRUE)[[1]]), 2L)
  expect_true(grepl("Hello", svg, fixed = TRUE))
})

test_that("math is laid out per glyph, untouched", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Math positions every glyph itself; merging there would destroy the
  # layout. These counts must not move.
  expect_equal(nrow(latex_tree("$\\frac{a}{b}$")$records), 3L)
  expect_equal(nrow(latex_tree("$\\sum_{i=1}^n x_i^2$")$records), 8L)
  expect_equal(nrow(latex_tree("$\\alpha\\beta\\gamma$")$records), 3L)
})

test_that("a run stops at anything that is not a plain text character", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Each of these is load-bearing. A font scope must not be merged
  # across, or the styling is lost; math must stay separate; and a break
  # mark must survive, because that is what line breaking -- and
  # hyphenation -- breaks at.
  expect_equal(texts("\\textbf{bold}\\text{plain}"), c("bold", "plain"))
  expect_equal(texts("\\textsf{ab\\textrm{cd}ef}"), c("ab", "cd", "ef"))
  expect_equal(texts("\\text{before $x$ after}"), c("before", "after"))
  expect_equal(texts("\\text{hy\\-phen\\-ation}"), c("hy", "phen", "ation"))
})

test_that("digits stay separate so a long number can still break", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # RowAtom::createBox adds a break position after every digit, which is
  # the only way a long unspaced number can wrap. Merging digits into the
  # run would remove those. The cost is that numbers are still laid out
  # per glyph.
  expect_equal(texts("\\text{ab12cd}"), c("ab", "1", "2", "cd"))
})

test_that("wrapping still happens, and still respects the measure", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  prose <- paste0("\\text{", paste(rep("The quick brown fox jumps over the lazy dog.", 3),
                                   collapse = " "), "}")
  for (mw in c(150, 250, 400)) {
    d <- latex_dims(prose, max_width = mw, input_mode = "math")
    expect_lte(as.numeric(d$width), mw)
    expect_gt(as.numeric(d$height), 18)  # it did wrap
  }
})
