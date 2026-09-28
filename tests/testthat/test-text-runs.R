# Text is drawn as runs: with no width set, a whole phrase is one run (so
# the device orders it and kerns across spaces); with a width set, one run
# per word, since the spaces are where lines break. See
# RowAtom::processTextRun in src/MicroTeX/lib/atom/atom_row.cpp.

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
  expect_equal(texts("\\text{page 42 of 99}"), "page 42 of 99")
})

test_that("a wrapped phrase keeps one run per word", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_equal(wrapped("\\text{one two three}", 40), c("one", "two", "three"))
  expect_equal(wrapped("\\text{ab12cd}", 40), c("ab", "1", "2", "cd"))
})

test_that("kerning is applied, because the device sees a whole word", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
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
  # The whole phrase in one element, so a viewer can search for it.
  expect_equal(length(gregexpr("<text", svg, fixed = TRUE)[[1]]), 1L)
  expect_true(grepl("Hello world", svg, fixed = TRUE))
})

test_that("math is laid out per glyph, untouched", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_equal(nrow(latex_tree("$\\frac{a}{b}$")$records), 3L)
  expect_equal(nrow(latex_tree("$\\sum_{i=1}^n x_i^2$")$records), 8L)
  expect_equal(nrow(latex_tree("$\\alpha\\beta\\gamma$")$records), 3L)
})

test_that("a run stops at anything that is not plain text", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # A font change, math or a measured skip ends a run; a space does not.
  expect_equal(texts("\\textbf{bold}\\text{plain}"), c("bold", "plain"))
  expect_equal(texts("\\textsf{ab\\textrm{cd}ef}"), c("ab", "cd", "ef"))
  expect_equal(texts("\\text{before $x$ after}"), c("before ", " after"))
  expect_length(texts("\\text{a \\quad b}"), 2L)
})

test_that("an unwrapped right-to-left phrase comes out in the right order", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # One run, so the device orders it. \u escapes keep the file ASCII.
  ar <- paste("مرحبا", "بك")
  expect_equal(texts(paste0("\\text{", ar, "}")), ar)
  # Mixed scripts too -- the device resolves the whole line at once.
  mixed <- paste("مرحبا", "abc", "بك")
  expect_equal(texts(paste0("\\text{", mixed, "}")), mixed)
})

test_that("a wrapped right-to-left paragraph is reordered per line", {
  skip_if_not(microtex_bidi_available(),
              "built without fribidi; wrapped RTL keeps logical order")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Reading each line right to left gives the source order back.
  words <- c("مرحبا", "بك",
             "في", "العالم")
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

# The words of each line read from the right, across the lines in order.
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
  # See RowAtom::collectBidiText.
  a <- c("مرحبا", "بك",
         "في", "العالم")
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

  # One group, and emphasis nested inside a group.
  expect_equal(right_to_left(txt(paste(a, collapse = " ")), input_mode = "math"), a)
  expect_equal(
    right_to_left(txt(paste0(a[1], " \\textbf{", a[2], "} ", a[3])),
                  input_mode = "math"),
    a[1:3])
})

test_that("a left-to-right run inside right-to-left keeps its own direction", {
  skip_if_not(microtex_bidi_available(), "built without fribidi")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  a1 <- "مرحبا"; a2 <- "بك"
  txt <- function(s) paste0("\\text{", s, "}")

  # A Latin word in its own group sits between the two Arabic words.
  expect_equal(
    right_to_left(paste0(txt(paste0(a1, " ")), "\\textbf{", txt("Latin"), "}",
                         txt(paste0(" ", a2))),
                  input_mode = "math"),
    c(a1, "Latin", a2))

  # Each digit is its own record; read from the right they come back
  # reversed, i.e. drawn left to right.
  got <- right_to_left(paste0(txt(paste0(a1, " ")), txt("2026"),
                              txt(paste0(" ", a2))), input_mode = "math")
  expect_equal(got[1], a1)                       # Arabic still rightmost
  expect_equal(got[length(got)], a2)
  expect_equal(rev(got[2:(length(got) - 1)]), c("2", "0", "2", "6"))
})

test_that("a left-to-right group that breaks across lines stays in the flow", {
  skip_if_not(microtex_bidi_available(), "built without fribidi")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # A broken piece of an all-Latin group takes its neighbour's level.
  u <- c("خۇش", "كەلدىڭىز",
         "بۇ", "دۇنياغا")
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
  # Right-to-left text reads from the right, Latin from the left.
  got_u <- by_line(function(s) s$text[order(-s$x)])
  expect_equal(got_u[got_u %in% u], u)
  got_l <- by_line(function(s) s$text[order(s$x)])
  expect_equal(got_l[got_l %in% latin], latin)
})

test_that("right-to-left markdown is ordered through its emphasis", {
  skip_if_not(microtex_bidi_available(), "built without fribidi")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  a <- c("مرحبا", "بك",
         "في", "العالم")
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
  # U+200F has no script of its own.
  rlm <- "‏"
  words <- c("(a)", "(b)", "(c)")
  order_of <- function(tex) {
    d <- latex_grob(tex, input_mode = "math", max_width = 60,
                    gp = grid::gpar(fontsize = 14))$layout_df
    d <- d[d$type == "text", ]
    d$text[order(-d$x)]                       # right to left
  }
  # With the mark the line reads right to left; without it, it does not.
  marked <- order_of(paste0("\\text{", rlm, paste(words, collapse = " "), "}"))
  expect_equal(length(marked), 3L)
  expect_match(marked[1], "a", fixed = TRUE)
  expect_equal(order_of(paste0("\\text{", paste(words, collapse = " "), "}"))[1],
               "(c)")
})

test_that("a line ending at a drawn hyphen is still reordered", {
  skip_if_not(microtex_bidi_available(), "built without fribidi")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  ar <- "مرحبا"
  first_line <- function(tex) {
    d <- latex_grob(tex, input_mode = "math", max_width = 130,
                    gp = grid::gpar(fontsize = 14))$layout_df
    d <- d[d$type == "text", ]
    d <- d[abs(round(d$y, 1) - min(round(d$y, 1))) < 0.01, ]
    d$text[order(-d$x)]
  }
  marked <- paste0("\\text{", ar, " inter\\-nation\\-alization بك}")
  plain <- paste0("\\text{", ar, " internationalization بك}")

  hy <- first_line(marked)
  expect_true("-" %in% hy)              # the break really was taken
  # The Arabic word is rightmost.
  expect_equal(hy[1], ar)
  expect_equal(first_line(plain)[1], ar)
})

test_that("a p{} cell wraps even when the formula as a whole does not", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # A p{} cell is broken to its own width, so it keeps word runs.
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

  heb <- "שלום עולם"
  order_of <- function(tex, mw = 0) {
    d <- latex_grob(tex, input_mode = "math", max_width = mw,
                    gp = grid::gpar(fontsize = 14))$layout_df
    d <- d[d$type == "text" & !is.na(d$text), ]
    d$text[order(d$x)]
  }

  # The base direction is right-to-left, so the Latin run goes on the left.
  for (wrap in list(c("\\textbf{", "}"), c("\\textcolor{red}{", "}"),
                    c("\\gmfontfamily{serif}{", "}"))) {
    tex <- paste0("\\text{", heb, " }", wrap[1], "\\text{alpha}", wrap[2])
    got <- order_of(tex)
    expect_equal(length(got), 2L, info = wrap[1])
    expect_equal(got[[1]], "alpha", info = wrap[1])
    expect_match(got[[2]], "^ש", info = wrap[1])
  }

  # Setting a width must not change the reading order, only where it breaks.
  expect_equal(order_of(paste0("\\text{", heb, " }\\textbf{\\text{alpha}}"))[[1]],
               "alpha")
  expect_equal(order_of(paste0("\\text{", heb, " }\\textbf{\\text{alpha}}"), 400)[[1]],
               "alpha")

  # The Hebrew is still one record.
  expect_equal(length(order_of(paste0("\\text{", heb, "}"))), 1L)

  # A left-to-right formula must be untouched by any of this.
  expect_equal(order_of("\\text{alpha }\\textbf{\\text{beta}}"),
               c("alpha ", "beta"))
})
