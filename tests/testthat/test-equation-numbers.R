# Equation numbers, \tag, \label and \ref, as LaTeX's amsmath sets them: a
# numbered display has its number at the right margin, in upright text, and
# a label records the number it was written next to.

rec <- function(tex, mode = "document", w = 300) {
  latex_tree(tex, input_mode = mode, max_width = w, render_mode = "typeface")
}

# The lines of text of a layout, top to bottom, each its text records left
# to right with the spaces between them dropped; math is glyphs, not text.
lines_of <- function(tex, mode = "document", w = 300) {
  r <- suppressWarnings(rec(tex, mode, w))$records
  r <- r[r$type == "text", ]
  if (!nrow(r)) return(character(0))
  r <- r[order(round(r$y), r$x), ]
  y <- round(r$y)
  unname(vapply(split(r$text, factor(y, levels = sort(unique(y)))), paste,
                "", collapse = ""))
}

# The warnings a parse raises.
warns <- function(tex, mode = "document", w = 300) {
  out <- character(0)
  withCallingHandlers(
    rec(tex, mode, w),
    warning = function(x) {
      out <<- c(out, conditionMessage(x))
      invokeRestart("muffleWarning")
    })
  out
}

# What a string of text is as wide as, in bp.
width_of <- function(s) {
  grid::convertWidth(latex_dims(s, input_mode = "mixed")$width, "bigpts",
                     valueOnly = TRUE)
}

test_that("an equation is numbered at the right margin, in upright text", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- r"(\begin{equation} a = b \end{equation})"
  expect_identical(lines_of(tex), "(1)")
  r <- rec(tex)$records
  tag <- r[r$type == "text", ]
  expect_equal(max(tag$x) + width_of(")"), 300, tolerance = 0.5)
  # Starred, it is not numbered.
  expect_identical(lines_of(r"(\begin{equation*} a = b \end{equation*})"),
                   character(0))
})

test_that("each row of an align is numbered, and \\notag leaves one out", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- r"(\begin{align} a &= b \\ c &= d \notag \\ e &= f \end{align})"
  expect_identical(lines_of(tex), c("(1)", "(2)"))
  expect_identical(lines_of(sub("notag", "nonumber", tex, fixed = TRUE)),
                   c("(1)", "(2)"))
  expect_identical(lines_of(r"(\begin{align*} a &= b \\ c &= d \end{align*})"),
                   character(0))
  # A trailing \\ does not start a numbered row.
  expect_identical(lines_of(r"(\begin{align} a &= b \\ c &= d \\ \end{align})"),
                   c("(1)", "(2)"))
})

test_that("gather numbers its rows and multline its last line only", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(lines_of(r"(\begin{gather} a \\ b \\ c \end{gather})"),
                   c("(1)", "(2)", "(3)"))
  tex <- r"(\begin{multline} a \\ b \\ c \end{multline})"
  expect_identical(lines_of(tex), "(1)")
  # On the last of its three lines.
  r <- rec(tex)$records
  expect_equal(r$y[r$type == "text"][1], max(r$y[r$type == "glyph"]), tolerance = 1)
})

test_that("eqnarray is numbered by rows, too", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(lines_of(r"(\begin{eqnarray} a &=& b \\ c &=& d \end{eqnarray})"),
                   c("(1)", "(2)"))
  expect_identical(lines_of(r"(\begin{eqnarray*} a &=& b \end{eqnarray*})"),
                   character(0))
})

test_that("\\tag sets the number, \\tag* sets it without parentheses", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(lines_of(r"(\begin{equation} a = b \tag{A} \end{equation})"),
                   "(A)")
  expect_identical(lines_of(r"(\begin{equation} a = b \tag*{A} \end{equation})"),
                   "A")
  # Also in a starred environment, which has no number of its own.
  expect_identical(lines_of(r"(\begin{align*} a &= b \tag{3.1} \end{align*})"),
                   "(3.1)")
  # A tagged row does not use up a number.
  expect_identical(
    lines_of(r"(\begin{align} a &= b \tag{*} \\ c &= d \end{align})"),
    c("(*)", "(1)"))
  # In a display of its own.
  expect_identical(lines_of(r"($$ a = b \tag{7} $$)"), "(7)")
  expect_identical(lines_of(r"(\[ a = b \tag*{7} \])"), "7")
})

test_that("the count goes on from one display to the next", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- paste0(r"(\begin{equation} a \end{equation})", "\n\nText\n\n",
                r"(\begin{align} b \\ c \end{align})")
  expect_identical(lines_of(tex), c("(1)", "Text", "(2)", "(3)"))
  expect_identical(
    lines_of(paste0(r"(\setcounter{equation}{4})",
                    r"(\begin{equation} a \end{equation})")),
    "(5)")
})

test_that("\\ref and \\eqref name what a label was written next to, forward or back", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  back <- paste0(r"(\begin{equation}\label{e1} a = b \end{equation})",
                 "\n\nSee ", r"(\eqref{e1} and \ref{e1}.)")
  expect_identical(warns(back), character(0))
  expect_identical(lines_of(back), c("(1)", "See(1)and1."))
  fwd <- paste0("See ", r"(\eqref{e2})", "\n\n",
                r"(\begin{align} a &= b \\ c &= d \label{e2} \end{align})")
  expect_identical(warns(fwd), character(0))
  expect_identical(lines_of(fwd), c("See(2)", "(1)", "(2)"))
  # A label inside the split of an equation is the equation's.
  spl <- paste0(r"(\begin{equation}\begin{split} a &= b \label{s} \end{split}\end{equation})",
                "\n\n", r"(\ref{s})")
  expect_identical(warns(spl), character(0))
  expect_identical(lines_of(spl), c("(1)", "1"))
  # A tag is what its label names.
  tg <- paste0(r"(\begin{equation} a \tag{A.1}\label{t} \end{equation} \ref{t})")
  expect_identical(lines_of(tg), c("(A.1)", "A.1"))
})

test_that("an undefined reference still warns and draws ??", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_match(warns(r"(See \ref{nope}.)"), "reference `nope' is undefined",
               all = FALSE)
  expect_identical(lines_of(r"(See \ref{nope}.)"), "See??.")
  expect_match(warns(r"(\eqref{nope})"), "drawn as (??)", fixed = TRUE, all = FALSE)
  # \pageref has no page.
  expect_match(warns(r"(\pageref{x})"), "undefined", all = FALSE)
  # One that is defined later is not undefined.
  expect_identical(warns(r"(\ref{a} \section{S}\label{a})"), character(0))
})

test_that("a label after a heading names its number", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- paste0(r"(\section{A}\label{a})", "\n\n", r"(\section{B}\label{b})",
                "\n\n", r"(See \ref{a}, \ref{b}.)")
  expect_identical(warns(tex), character(0))
  expect_identical(lines_of(tex), c("1A", "2B", "See1,2."))
})

test_that("with no measure the number follows the display after a gap", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  r <- rec(r"(\begin{equation} a = b \end{equation})", w = 0)
  tag <- r$records[r$records$type == "text", ]
  g <- r$records[r$records$type == "glyph", ]
  expect_gt(min(tag$x), max(g$x))
  expect_equal(max(tag$x) + width_of(")"), r$bbox[["width"]], tolerance = 0.5)
})

test_that("a label's displays are numbered too, as TeX's rule is the same everywhere", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(lines_of(r"(Before \begin{align} a \\ b \end{align})", "mixed"),
                   c("Before", "(1)", "(2)"))
  expect_identical(lines_of(r"(\begin{align*} a \\ b \end{align*})", "mixed"),
                   character(0))
})

# --- a markdown box lays its blocks out one at a time ----------------------

# The text of each block of a box, in order: one string per block.
box_texts <- function(md, width = 300) {
  g <- markdown_box_grob(md, width = grid::unit(width, "bigpts"))
  lay <- gridmicrotex:::.md_box_layout(g)
  unlist(lapply(lay$items, function(it) {
    df <- it$grob$layout_df
    if (is.null(df)) return(NULL)
    df <- df[df$type == "text", ]
    if (!nrow(df)) return(NULL)
    paste(df$text[order(round(df$y), df$x)], collapse = "")
  }))
}

test_that("a markdown box numbers its displays as one document's", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  md <- paste(r"($$\begin{equation} a \end{equation}$$)",
              r"($$\begin{align} b \\ c \end{align}$$)", sep = "\n\n")
  expect_identical(box_texts(md), c("(1)", "(2)(3)"))
  # The same block on its own starts at 1 again: a layout cached for one
  # starting number must not answer for another.
  expect_identical(box_texts(r"($$\begin{equation} b \end{equation}$$)"), "(1)")
  expect_identical(box_texts(md), c("(1)", "(2)(3)"))
  # Starred, a display has none to count.
  md <- paste(r"($$\begin{equation*} a \end{equation*}$$)",
              r"($$\begin{equation} b \end{equation}$$)", sep = "\n\n")
  expect_identical(box_texts(md), "(1)")
})

test_that("a reference in a markdown box names a label in another block", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  md <- paste(r"(See $\eqref{e}$ and $\ref{e}$.)",
              r"($$\begin{equation}\label{x} a \end{equation}$$)",
              r"($$\begin{equation}\label{e} b \end{equation}$$)", sep = "\n\n")
  tx <- box_texts(md)
  expect_false(any(grepl("??", tx, fixed = TRUE)))
  expect_match(tx[1], "(2)", fixed = TRUE)
  expect_match(tx[1], "2.", fixed = TRUE)
  expect_identical(tx[-1], c("(1)", "(2)"))
  # And one that is not there is ??.
  expect_match(suppressWarnings(box_texts(r"(See $\ref{nope}$.)")), "??", fixed = TRUE)
})
