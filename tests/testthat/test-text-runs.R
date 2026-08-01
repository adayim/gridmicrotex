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

# Reading a laid-out line from the right, across every line in order.
# Right to left is the direction the source is written in, so for a
# right-to-left paragraph this must give the source words back.
right_to_left <- function(tex, mw = 110, fn = latex_grob, ...) {
  d <- fn(tex, max_width = mw, gp = grid::gpar(fontsize = 14), ...)$layout_df
  d <- d[d$type == "text" & !is.na(d$text), ]
  unlist(lapply(sort(unique(round(d$y, 1))), function(yy) {
    s <- d[abs(round(d$y, 1) - yy) < 0.01, ]
    s$text[order(-s$x)]
  }))
}

test_that("right-to-left text is ordered across font groups, not only within one", {
  skip_if_not(microtex_bidi_available(), "built without fribidi")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # A row used to collect only the characters that were its *direct*
  # children, so text living in sibling groups -- which is what every
  # emphasis compiles to -- left the row with an empty string, no levels
  # and no reordering at all. See RowAtom::collectBidiText.
  a <- c("\u0645\u0631\u062d\u0628\u0627", "\u0628\u0643",
         "\u0641\u064a", "\u0627\u0644\u0639\u0627\u0644\u0645")
  txt <- function(s) paste0("\\text{", s, "}")

  # Two plain groups, and an emphasised group between two plain ones.
  expect_equal(
    right_to_left(paste0(txt(paste(a[1], a[2], "")), txt(paste(a[3], a[4]))),
                  input_mode = "math"),
    a)
  expect_equal(
    right_to_left(paste0(txt(paste(a[1], a[2], "")), "\\textbf{", txt(a[3]), "}",
                         txt(paste0(" ", a[4]))),
                  input_mode = "math"),
    a)
  # A colour and a named family are wrappers of their own.
  expect_equal(
    right_to_left(paste0(txt(paste0(a[1], " ")), "\\textcolor{red}{", txt(a[2]), "}",
                         txt(paste0(" ", a[3]))),
                  input_mode = "math"),
    a[1:3])
  expect_equal(
    right_to_left(paste0(txt(paste0(a[1], " ")), "\\gmfontfamily{serif}{", txt(a[2]), "}",
                         txt(paste0(" ", a[3]))),
                  input_mode = "math"),
    a[1:3])

  # Still correct where it already was: one group, and emphasis nested
  # inside a group rather than beside it.
  expect_equal(right_to_left(txt(paste(a, collapse = " ")), input_mode = "math"), a)
  expect_equal(
    right_to_left(txt(paste0(a[1], " \\textbf{", a[2], "} ", a[3])),
                  input_mode = "math"),
    a[1:3])
})

test_that("a left-to-right run inside right-to-left keeps its own direction", {
  skip_if_not(microtex_bidi_available(), "built without fribidi")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  a1 <- "\u0645\u0631\u062d\u0628\u0627"; a2 <- "\u0628\u0643"
  txt <- function(s) paste0("\\text{", s, "}")

  # A Latin word in its own group sits between the two Arabic words.
  expect_equal(
    right_to_left(paste0(txt(paste0(a1, " ")), "\\textbf{", txt("Latin"), "}",
                         txt(paste0(" ", a2))),
                  input_mode = "math"),
    c(a1, "Latin", a2))

  # Digits are a left-to-right run inside the line: each digit is its own
  # record (they carry break positions), so read from the right they come
  # back reversed -- which is exactly what "drawn left to right" means
  # inside a right-to-left line.
  got <- right_to_left(paste0(txt(paste0(a1, " ")), txt("2026"),
                              txt(paste0(" ", a2))), input_mode = "math")
  expect_equal(got[1], a1)                       # Arabic still rightmost
  expect_equal(got[length(got)], a2)
  expect_equal(rev(got[2:(length(got) - 1)]), c("2", "0", "2", "6"))
})

test_that("a left-to-right group that breaks across lines stays in the flow", {
  skip_if_not(microtex_bidi_available(), "built without fribidi")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # A group with no right-to-left text of its own resolves no levels, so
  # when the splitter breaks inside it the piece was given level 0. Rule
  # L2 reverses runs at or above the lowest *odd* level, so a 0 among 1s
  # split the reversal in two and scrambled the line. It has to inherit
  # the level of the neighbour it is placed against instead.
  u <- c("\u062e\u06c7\u0634", "\u0643\u06d5\u0644\u062f\u0649\u06ad\u0649\u0632",
         "\u0628\u06c7", "\u062f\u06c7\u0646\u064a\u0627\u063a\u0627")
  latin <- c("alpha", "beta", "gamma", "delta", "epsilon", "zeta")
  tex <- paste0("\\text{", u[1], " ", u[2], " }",
                "\\textbf{\\text{", paste(latin, collapse = " "), "}}",
                "\\text{ ", u[3], " ", u[4], "}")
  d <- latex_grob(tex, input_mode = "math", max_width = 130,
                  gp = grid::gpar(fontsize = 14))$layout_df
  d <- d[d$type == "text" & !is.na(d$text), ]
  lines <- sort(unique(round(d$y, 1)))
  expect_gt(length(lines), 1L)                    # it really wrapped

  by_line <- function(f) {
    unlist(lapply(lines, function(yy) f(d[abs(round(d$y, 1) - yy) < 0.01, ])))
  }
  # Right-to-left text reads from the right; the Latin run reads from the
  # left. Both must come back in source order.
  got_u <- by_line(function(s) s$text[order(-s$x)])
  expect_equal(got_u[got_u %in% u], u)
  got_l <- by_line(function(s) s$text[order(s$x)])
  expect_equal(got_l[got_l %in% latin], latin)
})

test_that("right-to-left markdown is ordered through its emphasis", {
  skip_if_not(microtex_bidi_available(), "built without fribidi")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Markdown compiles emphasis to sibling groups, so this is the case the
  # gap actually bit: bold, italic and code all lost the ordering.
  a <- c("\u0645\u0631\u062d\u0628\u0627", "\u0628\u0643",
         "\u0641\u064a", "\u0627\u0644\u0639\u0627\u0644\u0645")
  expect_equal(
    right_to_left(paste(a[1], a[2], paste0("**", a[3], "**"), a[4]),
                  fn = markdown_grob),
    a)
  expect_equal(
    right_to_left(paste(a[1], paste0("*", a[2], "*"), paste0("`", a[3], "`"), a[4]),
                  fn = markdown_grob),
    a)
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

test_that("a line ending at a drawn hyphen is still reordered", {
  skip_if_not(microtex_bidi_available(), "built without fribidi")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # The hyphen `\-` draws is appended to the finished line, after the
  # levels were assigned. Appending the box alone left one more child than
  # levels, and bidi_reorder does nothing when the two disagree -- so the
  # whole line silently stayed in logical order, but only when a break
  # that draws something was taken.
  ar <- "\u0645\u0631\u062d\u0628\u0627"
  first_line <- function(tex) {
    d <- latex_grob(tex, input_mode = "math", max_width = 130,
                    gp = grid::gpar(fontsize = 14))$layout_df
    d <- d[d$type == "text", ]
    d <- d[abs(round(d$y, 1) - min(round(d$y, 1))) < 0.01, ]
    d$text[order(-d$x)]
  }
  marked <- paste0("\\text{", ar, " inter\\-nation\\-alization \u0628\u0643}")
  plain <- paste0("\\text{", ar, " internationalization \u0628\u0643}")

  hy <- first_line(marked)
  expect_true("-" %in% hy)              # the break really was taken
  # Base direction is right-to-left, so the Arabic word is rightmost and
  # the Latin run sits to its left. Unreordered it comes out the other
  # way round, with the Arabic first.
  expect_equal(hy[1], ar)
  expect_equal(first_line(plain)[1], ar)
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

test_that("right-to-left runs are ordered without a max_width", {
  skip_if_not(microtex_bidi_available(), "built without fribidi")
  pdf(NULL); on.exit(dev.off(), add = TRUE)

  # Reordering used to live only on the line-splitting path, which runs
  # only when max_width > 0. With the default max_width = 0 a right-to-left
  # label came out in logical order the moment it held a second child --
  # any emphasis, colour or font change -- because the single merged run
  # that the backend orders for us was no longer the whole line.
  # \u escapes so the source stays ASCII, as elsewhere in this file.
  heb <- "\u05e9\u05dc\u05d5\u05dd \u05e2\u05d5\u05dc\u05dd"
  order_of <- function(tex, mw = 0) {
    d <- latex_grob(tex, input_mode = "math", max_width = mw,
                    gp = grid::gpar(fontsize = 14))$layout_df
    d <- d[d$type == "text" & !is.na(d$text), ]
    d$text[order(d$x)]
  }

  # Base direction is right-to-left (first strong character is Hebrew), so
  # the Latin run belongs to the left of the Hebrew, not the right.
  for (wrap in list(c("\\textbf{", "}"), c("\\textcolor{red}{", "}"),
                    c("\\gmfontfamily{serif}{", "}"))) {
    tex <- paste0("\\text{", heb, " }", wrap[1], "\\text{alpha}", wrap[2])
    got <- order_of(tex)
    expect_equal(length(got), 2L, info = wrap[1])
    expect_equal(got[[1]], "alpha", info = wrap[1])
    expect_match(got[[2]], "^\u05e9", info = wrap[1])
  }

  # Setting a width must not change the reading order, only where it breaks.
  expect_equal(order_of(paste0("\\text{", heb, " }\\textbf{\\text{alpha}}"))[[1]],
               "alpha")
  expect_equal(order_of(paste0("\\text{", heb, " }\\textbf{\\text{alpha}}"), 400)[[1]],
               "alpha")

  # The merged run survives: the Hebrew is still one record, so the text
  # stays selectable and kerned rather than being split into words.
  expect_equal(length(order_of(paste0("\\text{", heb, "}"))), 1L)

  # A left-to-right formula must be untouched by any of this.
  expect_equal(order_of("\\text{alpha }\\textbf{\\text{beta}}"),
               c("alpha ", "beta"))
})
