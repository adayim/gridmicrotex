# Breaking words across lines. `\-` is TeX's discretionary hyphen: it
# offers a break, and draws a hyphen if that break is taken.
#
# See HyphenMarkAtom (src/MicroTeX/lib/atom/atom_char.h) and
# BoxSplitter::getBreakPosition (src/MicroTeX/lib/core/split.cpp).

# latex_grob(), not latex_tree(): the tree function takes neither
# `justify` nor `hyphenate`, so it cannot show a shaped paragraph.
frags <- function(tex, ...) {
  df <- latex_grob(tex, input_mode = "math", ...)$layout_df
  df$text[df$type == "text"]
}
wide <- function(tex, ...) {
  as.numeric(latex_dims(tex, input_mode = "math", ...)$width)
}

test_that("an unused discretionary costs nothing", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # It must be free when the break is not taken, or every word carrying
  # one would be wider than the same word without.
  expect_equal(wide("\\text{hy\\-phen\\-ation}"), wide("\\text{hyphenation}"))
})

test_that("a taken discretionary draws its hyphen", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Before this, `\-` broke the word and drew nothing, which is worse
  # than not breaking: the reader sees "hy" and "phenation" as two words.
  #
  # Unwrapped, the marks are inert -- there is no line to break -- so the
  # word merges into one run like any other text and no hyphen appears.
  expect_equal(frags("\\text{hy\\-phen\\-ation}"), "hyphenation")
  # Given a measure, each taken break draws its hyphen.
  expect_equal(frags("\\text{hy\\-phen\\-ation}", max_width = 60),
               c("hy", "-", "phen", "-", "ation"))
})

test_that("the break is chosen with the hyphen's width paid for", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # The hyphen is appended to the line *after* the break is chosen, so
  # unless canBreak() accounts for it every hyphenated line overruns the
  # measure by a hyphen. Any measure at least as wide as the widest
  # fragment-plus-hyphen must produce no overrun.
  tex <- "\\text{hy\\-phen\\-ation hy\\-phen\\-ation hy\\-phen\\-ation}"
  widest <- max(wide("\\text{ation}"), wide("\\text{phen-}"))
  for (mw in c(60, 70, 90, 120, 160)) {
    expect_gte(mw, widest)          # the premise of this check
    expect_lte(wide(tex, max_width = mw), mw)
  }
})

test_that("a measure too narrow for any fragment still breaks", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # When nothing fits even with the hyphen paid for, an overfull line is
  # unavoidable -- TeX has the same failure mode. What must not happen is
  # giving up on breaking altogether and leaving the whole word out.
  tex <- "\\text{hy\\-phen\\-ation}"
  narrow <- wide(tex, max_width = 40)
  expect_lt(narrow, wide(tex))    # it did break
  expect_equal(frags(tex, max_width = 40),
               c("hy", "-", "phen", "-", "ation"))
})

test_that("hyphenated lines still justify to the measure", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # The hyphen has to join the line before justifyLine() stretches its
  # glue, or the stretch pushes the line past the measure.
  tex <- "\\text{hy\\-phen\\-ation hy\\-phen\\-ation hy\\-phen\\-ation}"
  for (mw in c(90, 120)) {
    expect_lte(wide(tex, max_width = mw, justify = TRUE), mw + 1e-6)
  }
})

# --- automatic hyphenation ------------------------------------------------

test_that("the bundled patterns parse cleanly", {
  p <- .hyph_read(system.file("hyph", "hyph-en-us.tex",
                              package = "gridmicrotex"))
  expect_gt(length(p$patterns), 4000)
  expect_gt(length(p$exceptions), 5)
  # Braces or comment markers left in a token mean the block extraction
  # picked up the delimiters, and every such token is a dud pattern.
  expect_false(any(grepl("[{}%]", c(p$patterns, p$exceptions))))
})

test_that("Liang's algorithm reproduces TeX's own answers", {
  .ensure_hyphenation()
  show <- function(w) {
    p <- microtex_hyphenate_word(w)
    if (!length(p)) return(w)
    paste(substring(w, c(1, p + 1), c(p, nchar(w))), collapse = "-")
  }
  expect_equal(show("hyphenation"), "hy-phen-ation")
  expect_equal(show("algorithm"), "al-go-rithm")
  expect_equal(show("representation"), "rep-re-sen-ta-tion")
  # From the file's \hyphenation{} block, which must override the patterns.
  expect_equal(show("associate"), "as-so-ciate")
  # \lefthyphenmin 2 / \righthyphenmin 3: too short to break at all.
  expect_equal(show("cat"), "cat")
  # The pattern file's header documents this one as its own known bug
  # ("de-mo-c-rat instead of dem-o-crat"). Matching it exactly is the
  # point: we reproduce TeX, warts and all.
  expect_equal(show("democrat"), "de-mo-c-rat")
})

test_that("hyphenate = FALSE changes nothing at all", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # The pass sits in RowAtom::createBox, which every formula goes through,
  # so "off" has to mean untouched rather than merely cheap.
  tex <- "\\text{hyphenation algorithm representation}"
  off <- frags(tex, max_width = 120)
  expect_identical(off, frags(tex, max_width = 120, hyphenate = FALSE))

  # And the flag must not leak into the next parse -- it is a static on
  # RowAtom, guarded per parse like BoxSplitter::_justify.
  invisible(latex_dims(tex, input_mode = "math", max_width = 120,
                       hyphenate = TRUE))
  expect_identical(off, frags(tex, max_width = 120))
})

test_that("hyphenation makes a long word fit a narrow measure", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # A word longer than the measure has nowhere to break without it.
  tex <- "\\text{Extraordinary hyphenation demonstrates typographical}"
  expect_gt(wide(tex, max_width = 110), 110)
  expect_lte(wide(tex, max_width = 110, hyphenate = TRUE), 110)
})

test_that("hyphenation does not change the unwrapped width", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Splitting a word into syllable runs must not move anything when no
  # break is taken, or turning the option on would reflow every label.
  for (w in c("hyphenation", "Wave", "typography", "representation")) {
    tex <- paste0("\\text{", w, "}")
    expect_equal(wide(tex), wide(tex, hyphenate = TRUE))
  }
})

test_that("math is untouched when hyphenation is on", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  n <- function(...) nrow(latex_grob("$\\sum_{i=1}^n x_i^2$", ...)$layout_df)
  expect_equal(n(), n(hyphenate = TRUE))
})

test_that("an explicit discretionary suppresses the automatic pass", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # TeX's rule: a word the author has hyphenated by hand keeps exactly
  # the breaks they asked for and gains none of its own.
  expect_equal(frags("\\text{hyphenation}", max_width = 60, hyphenate = TRUE),
               c("hy", "-", "phen", "-", "ation"))
  expect_equal(frags("\\text{hyphen\\-ation}", max_width = 60,
                     hyphenate = TRUE),
               c("hyphen", "-", "ation"))
})

test_that("latex_options(hyphenate=) sets and resets the default", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  on.exit(reset_latex_options(), add = TRUE)
  plain <- frags("\\text{hyphenation}", max_width = 60)
  latex_options(hyphenate = TRUE)
  expect_gt(length(frags("\\text{hyphenation}", max_width = 60)),
            length(plain))
  reset_latex_options()
  expect_equal(frags("\\text{hyphenation}", max_width = 60), plain)
})

test_that("hyphenate rejects a non-flag", {
  expect_error(latex_dims("x", hyphenate = "yes"), "must be TRUE or FALSE")
  expect_error(latex_options(hyphenate = NA), "must be TRUE or FALSE")
})

test_that("a plain break is unaffected", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Breaks at spaces record no box and must keep exactly the position
  # they always had -- the width accounting is skipped for them.
  prose <- paste(rep("The quick brown fox jumps over the lazy dog.", 3),
                 collapse = " ")
  tex <- paste0("\\text{", prose, "}")
  for (mw in c(150, 250, 400)) {
    expect_lte(wide(tex, max_width = mw), mw)
  }
  expect_false(any(frags(tex, max_width = 250) == "-"))
})
