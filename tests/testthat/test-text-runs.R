# Text is drawn as runs, not one record per letter. How far a run
# stretches depends on whether anything will wrap:
#
#   no width set  -> the whole phrase, spaces included, is one run, so the
#                    device can order it (bidi) and kern across spaces
#   width set     -> one run per word, because the spaces are where the
#                    line breaks
#
# See RowAtom::processTextRun in src/MicroTeX/lib/atom/atom_row.cpp.

texts <- function(tex, ...) {
  r <- latex_tree(tex, input_mode = "math", ...)$records
  r$text[r$type == "text"]
}
# latex_tree() takes no max_width-dependent options, so the wrapped cases
# go through latex_grob().
wrapped <- function(tex, mw, ...) {
  d <- latex_grob(tex, input_mode = "math", max_width = mw, ...)$layout_df
  d$text[d$type == "text"]
}

test_that("an unwrapped phrase is a single record", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_equal(texts("\\text{Hello}"), "Hello")
  expect_equal(texts("\\text{Hello, world!}"), "Hello, world!")
  # Digits come along too: the break position after each one only matters
  # when a long number might have to wrap.
  expect_equal(texts("\\text{page 42 of 99}"), "page 42 of 99")
})

test_that("a wrapped phrase keeps one run per word", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # This is the guard that matters: the spaces have to stay separate
  # boxes, because they are what the splitter breaks at.
  expect_equal(wrapped("\\text{one two three}", 40), c("one", "two", "three"))
  expect_equal(wrapped("\\text{ab12cd}", 40), c("ab", "1", "2", "cd"))
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
  # The whole phrase in one element, so a viewer can search for it. Per
  # character this was ten elements holding one letter each, and even the
  # space was missing, so no search could match anything.
  expect_equal(length(gregexpr("<text", svg, fixed = TRUE)[[1]]), 1L)
  expect_true(grepl("Hello world", svg, fixed = TRUE))
})

test_that("math is laid out per glyph, untouched", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Math positions every glyph itself; merging there would destroy the
  # layout. These counts must not move.
  expect_equal(nrow(latex_tree("$\\frac{a}{b}$")$records), 3L)
  expect_equal(nrow(latex_tree("$\\sum_{i=1}^n x_i^2$")$records), 8L)
  expect_equal(nrow(latex_tree("$\\alpha\\beta\\gamma$")$records), 3L)
})

test_that("a run stops at anything that is not plain text", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Each of these is load-bearing. A font scope must not be merged
  # across, or the styling is lost; math and a measured skip keep their
  # own boxes. Only the ordinary word space is folded in.
  expect_equal(texts("\\textbf{bold}\\text{plain}"), c("bold", "plain"))
  expect_equal(texts("\\textsf{ab\\textrm{cd}ef}"), c("ab", "cd", "ef"))
  expect_equal(texts("\\text{before $x$ after}"), c("before ", " after"))
  expect_length(texts("\\text{a \\quad b}"), 2L)
})

test_that("an unwrapped right-to-left phrase comes out in the right order", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # One run means the device orders it, and the device implements the
  # bidirectional algorithm that MicroTeX does not. Positioned per word
  # it came out reversed.
  # Written as \u escapes so the file itself stays ASCII, which is what
  # R CMD check wants of package sources.
  ar <- paste("\u0645\u0631\u062d\u0628\u0627", "\u0628\u0643")
  expect_equal(texts(paste0("\\text{", ar, "}")), ar)
  # Mixed scripts too -- the device resolves the whole line at once.
  mixed <- paste("\u0645\u0631\u062d\u0628\u0627", "abc", "\u0628\u0643")
  expect_equal(texts(paste0("\\text{", mixed, "}")), mixed)
})

test_that("a wrapped right-to-left paragraph is reordered per line", {
  skip_if_not(microtex_bidi_available(),
              "built without fribidi; wrapped RTL keeps logical order")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Lines are broken in logical order and only then put into visual
  # order, which is what the Unicode algorithm requires. Reading each
  # line right to left must give the source order back.
  words <- c("\u0645\u0631\u062d\u0628\u0627", "\u0628\u0643",
             "\u0641\u064a", "\u0627\u0644\u0639\u0627\u0644\u0645")
  src <- rep(words, 3)
  ar <- paste(src, collapse = " ")
  d <- latex_grob(paste0("\\text{", ar, "}"), input_mode = "math",
                  max_width = 120, gp = grid::gpar(fontsize = 16))$layout_df
  d <- d[d$type == "text", ]
  expect_gt(length(unique(round(d$y, 1))), 1L)   # it really wrapped
  got <- unlist(lapply(sort(unique(round(d$y, 1))), function(yy) {
    s <- d[abs(round(d$y, 1) - yy) < 0.01, ]
    s$text[order(-s$x)]                          # right to left
  }))
  expect_equal(got, src)
})

test_that("an explicit right-to-left mark counts as right-to-left", {
  skip_if_not(microtex_bidi_available(), "built without fribidi")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # U+200F carries no script of its own, so a scan for Hebrew/Arabic
  # letters missed it -- yet it is exactly how a right-to-left run is
  # marked when the characters themselves are direction-neutral.
  rlm <- "\u200f"
  words <- c("(a)", "(b)", "(c)")
  order_of <- function(tex) {
    d <- latex_grob(tex, input_mode = "math", max_width = 60,
                    gp = grid::gpar(fontsize = 14))$layout_df
    d <- d[d$type == "text", ]
    d$text[order(-d$x)]                       # right to left
  }
  # With the mark the line reads right to left, so the source order comes
  # back when read from the right. Without it, it does not.
  marked <- order_of(paste0("\\text{", rlm, paste(words, collapse = " "), "}"))
  expect_equal(length(marked), 3L)
  expect_match(marked[1], "a", fixed = TRUE)
  expect_equal(order_of(paste0("\\text{", paste(words, collapse = " "), "}"))[1],
               "(c)")
})

test_that("a hyphenated line is still reordered", {
  skip_if_not(microtex_bidi_available(), "built without fribidi")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # The hyphen is appended to the finished line after the levels were
  # assigned. Appending the box alone left one more child than levels,
  # and bidi_reorder does nothing when the two disagree -- so the whole
  # line silently stayed in logical order, but only when it hyphenated.
  ar <- "\u0645\u0631\u062d\u0628\u0627"
  tex <- paste0("\\text{", ar, " internationalization \u0628\u0643}")
  first_line <- function(...) {
    d <- latex_grob(tex, input_mode = "math", max_width = 130,
                    gp = grid::gpar(fontsize = 14), ...)$layout_df
    d <- d[d$type == "text", ]
    d <- d[abs(round(d$y, 1) - min(round(d$y, 1))) < 0.01, ]
    d$text[order(-d$x)]
  }
  hy <- first_line(hyphenate = TRUE)
  expect_true("-" %in% hy)              # it really did hyphenate
  # Base direction is right-to-left, so the Arabic word is rightmost and
  # the Latin run sits to its left. Unreordered it comes out the other
  # way round, with the Arabic first.
  expect_equal(hy[1], ar)
  expect_equal(first_line(hyphenate = FALSE)[1], ar)
})

test_that("a p{} cell wraps even when the formula as a whole does not", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # A fixed-width column is broken to its own measure whatever the global
  # width is, so its cells must keep word runs. Folding them into one
  # phrase left the cell unbreakable and the column silently sized to
  # content -- caught only because the p{} tests measure the result.
  cell <- "\\text{a fairly long cell that ought to wrap inside its column}"
  mk <- function(spec) paste0("\\begin{tabular}{", spec,
                              "}\\hline ", cell, " & \\text{b}\\\\ \\hline\\end{tabular}")
  free <- as.numeric(latex_dims(mk("ll"), input_mode = "math")$width)
  fixed <- as.numeric(latex_dims(mk("p{3cm}l"), input_mode = "math")$width)
  expect_lt(fixed, free / 2)
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
