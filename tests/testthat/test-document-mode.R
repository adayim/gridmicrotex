# input_mode = "document": a LaTeX document body, by TeX's rules with no
# exception. A line end is a space, a blank line starts a paragraph, and a
# paragraph indents its first line.

runs <- function(tex, mode = "document", ...) {
  t <- latex_tree(tex, input_mode = mode, ...)
  r <- t$records[t$records$type == "text", c("text", "x", "y")]
  r[order(r$y, r$x), ]
}

test_that("a line end is a space, as TeX reads it", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # One line, and one run of text, so the device shapes "one two" whole.
  d <- runs("one\ntwo")
  expect_identical(nrow(d), 1L)
  expect_identical(d$text, "one two")
  # The same input in a label is two lines: mixed mode's one departure
  # from TeX.
  m <- runs("one\ntwo", mode = "mixed")
  expect_identical(m$text, c("one", "two"))
  expect_false(m$y[1] == m$y[2])
})

test_that("a paragraph neither starts nor ends with a space, as in TeX", {
  # Between paragraphs TeX is in vertical mode, where a space is nothing,
  # and \par drops the space a paragraph ends with.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(runs("\nText.\n"), runs("Text."))
  expect_identical(runs("  Text.  "), runs("Text."))
})

test_that("a blank line starts a paragraph, however many there are", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  one <- runs("one\n\ntwo")
  expect_identical(nrow(one), 2L)
  expect_false(one$y[1] == one$y[2])
  # Two blank lines are one paragraph break, not two.
  expect_identical(runs("one\n\n\n\ntwo")$y, one$y)
  # \par is the same break spelled out.
  expect_identical(runs("one\\par two")$y, one$y)
})

test_that("a paragraph indents its first line by TeX's \\parindent", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # 1.5em, which is TeX's 15pt at a 10pt font, and follows the font size.
  for (fs in c(8, 12, 20)) {
    gp <- grid::gpar(fontsize = fs)
    indent <- latex_dims("\\kern1.5em", gp = gp)$width
    d <- runs("one\n\ntwo", gp = gp)
    # Every paragraph is indented, the first one included.
    expect_equal(d$x, c(indent, indent), tolerance = 1e-5, info = fs)
  }
  # A label is not a document: no indent there.
  expect_identical(runs("one\ntwo", mode = "mixed")$x, c(0, 0))
})

test_that("math and environments are read in a document as anywhere else", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(nrow(runs("Total $x^2$ here")), 2L)  # "Total " and " here"
  d <- latex_tree("a\n\n$$x$$\n\nb", input_mode = "document")
  expect_true(any(d$records$type == "text"))
  # A display formula is its own row, between the paragraphs.
  ys <- sort(unique(d$records$y))
  expect_identical(length(ys), 3L)
})

test_that("a display is centred on a line of its own, as in LaTeX", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  at <- function(tex, ...) {
    t <- latex_tree(tex, input_mode = "document", ...)
    min(t$records$x[t$records$type == "glyph"])
  }
  width <- function(...) as.numeric(latex_dims(...)$width)
  tex <- "Before\n\\[ x^2 + y^2 \\]\nafter."
  # Centred in the text width, which the grob then fills: a text width 100
  # wider moves it 50 to the right. (Dimensions are whole bp, positions are
  # not, so the rest compares positions.)
  expect_equal(at(tex, max_width = 400) - at(tex, max_width = 300), 50, tolerance = 1e-6)
  expect_equal(width(tex, input_mode = "document", max_width = 300), 300)
  dw <- width(r"(\displaystyle x^2 + y^2)", input_mode = "math")
  expect_lt(abs(at(tex, max_width = 300) - (300 - dw) / 2), 1)
  # With no text width, on the widest line: a widest line 100 longer moves
  # it 50 too.
  expect_equal(at(paste0("\\kern100bp ", tex)) - at(tex), 50, tolerance = 1e-6)
  # Each display LaTeX has sits between the text around it, starred or not.
  for (d in c(r"(\[x\])", r"($$x$$)", r"(\begin{equation}x\end{equation})",
              r"(\begin{equation*}x\end{equation*})", r"(\begin{align*}x\end{align*})",
              r"(\begin{gather}x\end{gather})", r"(\begin{displaymath}x\end{displaymath})")) {
    t <- latex_tree(paste0("Before ", d, " after."), input_mode = "document")
    y <- tapply(t$records$y, t$records$type, max)
    txt <- t$records$y[t$records$type == "text"]
    expect_identical(length(unique(txt)), 2L, info = d)
    expect_true(y[["glyph"]] > min(txt) && y[["glyph"]] < max(txt), info = d)
  }
  # In a label, display math stays in its line.
  expect_identical(length(unique(runs("a $$x$$ b", mode = "mixed")$y)), 1L)
})

test_that("a display interrupts its paragraph without ending it", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  indent <- as.numeric(latex_dims("\\kern1.5em")$width)
  # What follows the display goes on unindented, as in TeX ...
  d <- runs("Before\n\\[ x \\]\nafter.")
  expect_equal(d$x, c(indent, 0), tolerance = 1e-5)
  # ... unless a blank line starts a new paragraph.
  d <- runs("Before\n\\[ x \\]\n\nNew.")
  expect_equal(d$x, c(indent, indent), tolerance = 1e-5)
})

test_that("a display environment is in display style, as \\[...\\] is", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # A fraction in display style keeps its numerator at the body size; in
  # text style it would be at script size.
  for (d in c("\\[\\frac{a}{b}\\]", "\\begin{equation}\\frac{a}{b}\\end{equation}",
              "\\begin{align*}\\frac{a}{b}\\end{align*}")) {
    r <- latex_tree(paste("x", d), input_mode = "document")$records
    expect_identical(unique(r$font_size[!is.na(r$font_size)]), 20, info = d)
  }
})

test_that("a list or a float is set apart from its paragraph", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # A list is a block, as in LaTeX: the text before it ends its line, and
  # the text after it goes on unindented, as after a display.
  d <- runs("Before\n\\begin{itemize}\\item one\\end{itemize}\nafter.")
  expect_identical(d$text, c("Before", "one", "after."))
  expect_length(unique(d$y), 3L)
  expect_identical(d$x[d$text == "after."], 0)
  # A float is set where it is written, on lines of its own.
  d <- runs(paste0("Before \\begin{table}\\caption{Cap}",
                   "\\begin{tabular}{l}cell\\end{tabular}\\end{table} after."))
  expect_identical(d$text, c("Before", "Cap", "cell", "after."))
  expect_length(unique(d$y), 4L)
  # In a label, as before, a list is part of the line: it follows "Before".
  d <- runs("Before \\begin{itemize}\\item one\\end{itemize}", mode = "mixed")
  expect_gt(d$x[d$text == "one"], as.numeric(latex_dims("Before", input_mode = "mixed")$width))
})

test_that("center and \\centering centre a document's lines", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  x_of <- function(tex, word, ...) {
    d <- runs(tex, ...)
    d$x[d$text == word]
  }
  # Centred in the text width: 100 more of it moves a line 50 to the right.
  # The text after the block is flush again.
  for (tex in c("\\begin{center}Short\\end{center}\nafter",
                "\\begin{figure}\\centering Short\\end{figure}\nafter")) {
    expect_equal(x_of(tex, "Short", max_width = 400) - x_of(tex, "Short", max_width = 300),
                 50, tolerance = 1e-6, info = tex)
    expect_identical(x_of(tex, "after", max_width = 300), 0, info = tex)
  }
  # A float without \centering is flush left, as LaTeX sets it.
  expect_identical(x_of("\\begin{figure}Short\\end{figure}", "Short", max_width = 300), 0)
  # A label is not a document: \centering does nothing there, as before.
  expect_identical(runs("\\centering Short", mode = "mixed")$x, 0)
})

test_that("a heading reads LaTeX's optional short title and a spaced star", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # The short title is for a table of contents a grob has not got.
  expect_identical(runs("\\section[Intro]{Introduction}\nText.")$text,
                   c("1", "Introduction", "Text."))
  # LaTeX finds the star past a space, as it finds it after a newline.
  expect_identical(runs("\\section *{A}\nText")$text, c("A", "Text"))
})

test_that("\\paragraph starts a paragraph of its own, flush left, and runs in", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  d <- runs("Para one.\n\n\\paragraph{Run} Text after.")
  expect_identical(d$x[d$text == "Run"], 0)
  expect_identical(d$y[d$text == "Run"], d$y[grepl("Text after", d$text)])
  expect_gt(d$y[d$text == "Run"], d$y[d$text == "Para one."])
})

test_that("a caption is a line of its own in a document", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # After the figure, as LaTeX's usual order writes it, it goes below it.
  d <- runs("\\begin{figure}\\centering x\\caption{Cap}\\end{figure}")
  expect_gt(d$y[d$text == "Cap"], d$y[d$text == "x"])
})

test_that("a group's paragraphs and headings are a document's, its declarations kept", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  indent <- as.numeric(latex_dims("\\kern1.5em")$width)
  size <- function(d, word) {
    t <- latex_tree(d, input_mode = "document")$records
    t$font_size[t$type == "text" & t$text == word]
  }
  # \small lasts to the end of its group, across a paragraph break, and the
  # new paragraph is indented.
  tex <- "Intro.\n\n{\\small first para\n\nsecond para}"
  expect_identical(size(tex, "second para"), size(tex, "first para"))
  expect_lt(size(tex, "second para"), size(tex, "Intro."))
  expect_equal(runs(tex)$x[runs(tex)$text == "second para"], indent, tolerance = 1e-5)
  # A heading inside a group is still a heading on a line of its own.
  d <- runs("{\\color{red}\\section{A}\nText one.}")
  expect_false(d$y[d$text == "A"] == d$y[d$text == "Text one."])
  # \small at the top reaches the text after a heading, and adds no line.
  tex <- "\\small\n\\section{A}\nText"
  expect_identical(runs(tex)$y[1], runs("\\section{A}\nText")$y[1])
  expect_lt(size(tex, "Text"), 20)
  # \centering ends with its group: `{\centering Title\par}` centres the
  # title, and what follows is not centred.
  d <- runs("{\\centering Title\\par}\nBody", max_width = 300)
  expect_gt(d$x[d$text == "Title"], 50)
  expect_equal(d$x[d$text == "Body"], indent, tolerance = 1e-5)
})

test_that("a centred paragraph broken at max_width is centred line by line", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  words <- paste(strrep(letters[1:12], 3), collapse = " ")
  at <- function(w) {
    d <- runs(paste0("\\begin{center}", words, "\\end{center}"), max_width = w)
    tapply(d$x, d$y, min)
  }
  # Several lines, none of them flush left, as the splitter left them.
  expect_gt(length(at(80)), 2L)
  expect_true(all(at(80) > 0))
})

test_that("a heading is numbered as LaTeX numbers it", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  d <- runs("\\section{A}\n\n\\subsection{B}\n\n\\subsubsection{C}")
  expect_identical(d$text, c("1", "A", "1.1", "B", "1.1.1", "C"))
  # A counter carries on, and a lower one restarts under it.
  d <- runs("\\section{A}\n\n\\subsection{B}\n\n\\section{C}\n\n\\subsection{D}")
  expect_identical(d$text[d$text %in% c("1", "1.1", "2", "2.1")], c("1", "1.1", "2", "2.1"))
  # A starred heading has no number and does not advance the counter, and
  # \paragraph is below article's secnumdepth, so it has none either.
  expect_identical(runs("\\section*{A}\n\n\\section{B}")$text, c("A", "1", "B"))
  # The space after the run-in heading is the one written after its `}`.
  expect_identical(runs("\\paragraph{P} text")$text, c("P", " text"))
})

test_that("a heading is bold, sized and on a line of its own", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  t <- latex_tree("\\section{A}\n\n\\subsection{B}\n\n\\subsubsection{C}\n\nbody",
                  input_mode = "document")
  r <- t$records[t$records$type == "text", ]
  body <- r$font_size[r$text == "body"]
  # LaTeX's own sizes: \Large, \large, then the body size.
  expect_equal(r$font_size[r$text == "A"] / body, 1.4, tolerance = 1e-6)
  expect_equal(r$font_size[r$text == "B"] / body, 1.2, tolerance = 1e-6)
  expect_equal(r$font_size[r$text == "C"] / body, 1.0, tolerance = 1e-6)
  # Each heading has a row to itself, and is never indented: its row opens
  # with the number, flush left.
  expect_identical(length(unique(r$y)), 4L)
  expect_identical(r$x[r$text == "1"], 0)
  # \paragraph is the exception: LaTeX runs it into its paragraph.
  p <- runs("\\paragraph{P} text")
  expect_identical(p$y[1], p$y[2])
})

test_that("the paragraph after a heading is not indented, as in LaTeX", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  indent <- as.numeric(latex_dims("\\kern1.5em")$width)
  # However it is separated from the heading -- a line end or a blank line
  # -- the first paragraph is flush, and the one after it is indented.
  for (tex in c("\\section{H}\nfirst\n\nsecond", "\\section{H}\n\nfirst\n\nsecond")) {
    d <- runs(tex)
    expect_identical(d$x[d$text == "first"], 0, info = tex)
    expect_equal(d$x[d$text == "second"], indent, tolerance = 1e-5, info = tex)
  }
  # \noindent says so for any paragraph.
  d <- runs("one\n\n\\noindent two")
  expect_equal(d$x, c(indent, 0), tolerance = 1e-5)
})

test_that("latex_options and the grob functions accept the document mode", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  on.exit(reset_latex_options(), add = TRUE)
  expect_silent(latex_options(input_mode = "document"))
  expect_identical(latex_dims("one\ntwo")$height, latex_dims("one two")$height)
  reset_latex_options()
  expect_error(latex_options(input_mode = "paragraph"), "should be one of")
})

test_that("text under \\large, \\color or \\textcolor wraps at max_width", {
  # A size or a colour is one box around its text, which the line breaker
  # could not enter: a paper's \small abstract ran off the page.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  long <- paste(rep("word", 40), collapse = " ")
  for (mode in c("document", "mixed")) {
    for (tex in c(sprintf("\\large %s", long), sprintf("\\color{red} %s", long),
                  sprintf("\\textcolor{red}{%s}", long), sprintf("A {\\small %s}", long))) {
      t <- latex_tree(tex, input_mode = mode, max_width = 200)
      r <- t$records[t$records$type == "text", ]
      expect_lte(t$bbox[["width"]], 200, label = paste(mode, tex))
      expect_gt(length(unique(round(r$y))), 5L, label = paste(mode, tex))
    }
    # What the box did it still does, to every piece: the colour...
    r <- latex_tree(sprintf("\\color{red} %s", long), input_mode = mode,
                    max_width = 200)$records
    expect_true(all(r$color[r$type == "text"] == "#FF0000"), info = mode)
    # ...and the size, word for word as \large sets a line that fits.
    big <- latex_tree(sprintf("\\large %s", long), input_mode = mode, max_width = 200)$records
    one <- latex_tree("\\large word word", input_mode = mode)$records
    expect_identical(unique(big$font_size), unique(one$font_size), info = mode)
  }
  # A line that fits is never opened, so any measure it fits in sets it the
  # same (the harness holds it to the layouts drawn before).
  fits <- "\\large a few {\\color{blue} words} here"
  expect_identical(latex_tree(fits, input_mode = "document", max_width = 1000)$records,
                   latex_tree(fits, input_mode = "document", max_width = 5000)$records)
})

test_that("a line breaks at the last break before it overflows, even a level down", {
  # The break at the space that ends one run of text was not seen when the
  # next run overflowed, so the line ran on to a later break, past the width.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  width <- function(tex, mode, w) latex_tree(tex, input_mode = mode, max_width = w)$bbox[["width"]]
  expect_lte(width("\\text{aaa }\\textbf{bbb}\\text{ ccc ddd eee fff ggg hhh}", "math", 60), 60)
  expect_lte(width("Slope $\\hat{\\beta}_1 = \\sum_{i=1}^{n} x_i^2$", "mixed", 150), 150)
  expect_lte(width("Hello \\textbf{big bold} world, and more words", "mixed", 100), 100)
})

test_that("ulem's \\uline and \\sout wrap with their text, their rules straight", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  long <- "words that run on long enough to need several lines in a narrow box"
  for (cmd in c("uline", "sout")) {
    t <- latex_tree(sprintf("A \\%s{%s} end", cmd, long), input_mode = "mixed", max_width = 150)
    r <- t$records
    expect_lte(t$bbox[["width"]], 150, label = cmd)
    rules <- r[r$type == "line", ]
    txt <- r[r$type == "text", ]
    # One height per line, and no rule past the last word of a line: the
    # space it broke at is left bare, as ulem leaves it.
    for (y in unique(round(txt$y))) {
      line <- txt[round(txt$y) == y, ]
      at <- rules[abs(rules$y - line$y[1]) < 12, ]
      if (nrow(at) == 0L) next
      expect_identical(length(unique(round(at$y, 2))), 1L, label = cmd)
    }
    expect_gt(length(unique(round(txt$y))), 2L)
  }
  # \underline is LaTeX's box: it is not broken, whatever the width.
  t <- latex_tree(sprintf("\\underline{%s}", long), input_mode = "mixed", max_width = 150)
  expect_identical(length(unique(round(t$records$y[t$records$type == "text"]))), 1L)
  # Nor is a fraction.
  t <- latex_tree(sprintf("\\frac{\\text{%s}}{2}", long), input_mode = "math", max_width = 150)
  expect_identical(length(unique(round(t$records$y[t$records$type == "text"]))), 1L)
})

test_that("relsize's \\textscale and \\relscale set a size that wraps", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  sizes <- function(tex) unique(latex_tree(tex, input_mode = "mixed")$records$font_size)
  expect_setequal(sizes("a \\textscale{1.5}{b}"), c(20, 30))
  expect_setequal(sizes("a {\\relscale{0.5} b}"), c(20, 10))
  long <- paste(rep("word", 20), collapse = " ")
  t <- latex_tree(sprintf("A \\textscale{1.5}{%s}", long), input_mode = "mixed", max_width = 150)
  expect_lte(t$bbox[["width"]], 150)
  expect_warning(latex_grob("\\relscale{big} x"), "not a positive number")
})

test_that("\\raggedleft, flushright and flushleft align a document's lines", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  long <- "A paragraph long enough to wrap onto several lines at this width here"
  # Where each line starts. A line put to the right starts twice as far in
  # as the same line centred: all its room is before it, not half.
  lefts <- function(tex) {
    r <- latex_tree(tex, input_mode = "document", max_width = 200)$records
    r <- r[r$type == "text", ]
    unname(tapply(r$x, round(r$y), min))
  }
  centred <- lefts(paste("\\centering", long))
  expect_gt(length(centred), 1L)
  expect_gt(max(centred), 5)
  for (tex in c(paste("\\raggedleft", long), sprintf("\\begin{flushright}%s\\end{flushright}", long))) {
    expect_equal(lefts(tex), 2 * centred, tolerance = 0.02, label = tex)
  }
  expect_equal(lefts(sprintf("\\begin{flushleft}%s\\end{flushleft}", long)), 0 * centred)
  # A label is placed by hjust: nothing aligns its lines.
  expect_identical(latex_tree(paste("\\raggedleft", long), input_mode = "mixed")$records,
                   latex_tree(long, input_mode = "mixed")$records)
})

test_that("X columns share what the table needs, and >{} aligns a column's lines", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tab <- paste("\\begin{tabular}{lXX} 1 & short & a long description of the first thing",
               "measured in the study \\\\ \\end{tabular}")
  r <- latex_tree(tab, input_mode = "mixed", max_width = 250)$records
  r <- r[r$type == "text", ]
  # The short column keeps its width; the long one takes the rest.
  expect_lt(min(r$x[r$y > min(r$y)]), 120)
  expect_lte(max(r$x), 250)
  centred <- "\\begin{tabular}{>{\\centering\\arraybackslash}p{100pt}} a cell of words that wraps \\\\ \\end{tabular}"
  right <- "\\begin{tabular}{>{\\raggedleft\\arraybackslash}p{100pt}} a cell of words that wraps \\\\ \\end{tabular}"
  plain <- "\\begin{tabular}{p{100pt}} a cell of words that wraps \\\\ \\end{tabular}"
  starts <- function(tex) {
    r <- latex_tree(tex, input_mode = "mixed")$records
    r <- r[r$type == "text", ]
    tapply(r$x, round(r$y), min)
  }
  expect_true(all(starts(centred) > starts(plain)))
  expect_true(all(starts(right) > starts(centred)))
})

test_that("\\text, \\mbox and \\textsuperscript keep the style around them", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  styles <- function(tex, mode = "mixed") {
    r <- latex_tree(tex, input_mode = mode)$records
    r <- r[r$type == "text", ]
    setNames(r$font_style, trimws(r$text))
  }
  s <- styles("\\textbf{a $\\text{b}$ \\mbox{c} H\\textsuperscript{2}}")
  expect_true(all(bitwAnd(s, 2L) != 0L))
  # Nor does a style nested in the same style take it away after it.
  s <- styles("\\textbf{a \\textbf{b} c}")
  expect_true(bitwAnd(s[["c"]], 2L) != 0L)
  # \underline and \phantom of text are text, in its font.
  r <- latex_tree("\\textbf{\\underline{Hg}}", input_mode = "mixed")$records
  expect_identical(r$type[r$type != "line"], "text")
  expect_equal(latex_dims("a\\phantom{Hg}b")$width, latex_dims("aHgb")$width)
})

test_that("a definition in math between two dollars is inline math", {
  # The expander takes the \newcommand away; TeX saw it between the `$`,
  # which makes this `$`, not `$$`.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_silent(latex_grob("$\\newcommand{\\R}{\\mathbb{R}}$ $\\R$", input_mode = "document"))
  expect_silent(latex_grob("$$x$$ and \\[y\\]", input_mode = "document"))
})

test_that("a box stays whole in a broken line, and keeps a height set by hand", {
  # Only a declaration's size or colour is broken with its text:
  # \scalebox and \colorbox are boxes in LaTeX, and a line breaks around
  # them, not inside.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  long <- paste(rep("abc", 20), collapse = " ")
  lines <- function(tex) length(unique(round(runs(tex, max_width = 200)$y)))
  for (box in c("\\scalebox{0.9}{%s}", "\\colorbox{yellow}{%s}")) {
    expect_identical(lines(paste("Lead", sprintf(box, long), "tail")), 3L, label = box)
  }
  expect_gt(lines(paste("Lead \\large", long, "tail")), 3L)
  # A row the breaker rebuilds, to reach a size or colour in it, keeps
  # the height \smash or \raisebox gave it.
  words <- paste(rep("word", 30), collapse = " ")
  height <- function(tall) {
    latex_tree(paste("Lead", words, tall, words), input_mode = "document", max_width = 200,
               render_mode = "path")$bbox[["height"]]
  }
  flat <- height("")
  for (tall in c("\\smash{\\Huge Tall}", "\\smash{\\textcolor{red}{\\Huge Tall}}",
                 "\\raisebox{0pt}[0pt][0pt]{\\Huge Tall}",
                 "\\smash{\\rule{1pt}{40pt}\\textcolor{red}{a b}}")) {
    expect_equal(height(tall), flat, label = tall)
  }
  expect_gt(height("{\\Huge Tall}"), flat)
})

# --- what a pasted paper uses (Stage 9d, from rendering arXiv 1706.03762) ---

test_that("\\em is italic, as \\emph is", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Italic, and set where \emph sets it (\emph's \textit also records the
  # roman bit, which draws nothing different).
  em <- latex_tree("a {\\em b} c", input_mode = "document")$records
  emph <- latex_tree("a \\emph{b} c", input_mode = "document")$records
  expect_true(bitwAnd(em$font_style[em$text == "b"], 4L) != 0)
  expect_identical(em[, c("text", "x", "y")], emph[, c("text", "x", "y")])
})

test_that("natbib's citations draw what natbib draws for an unknown key", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  drawn <- function(tex) {
    r <- suppressWarnings(latex_tree(tex, input_mode = "document"))$records
    paste(r$text[r$type == "text"], collapse = "")
  }
  expect_identical(drawn("\\citep{a}"), drawn("\\cite{a}"))
  expect_identical(drawn("\\citep[p.~5]{a}"), drawn("\\cite[p.~5]{a}"))
  expect_identical(drawn("\\citep[see][ch.~2]{a,b}"), "[see ?, ?, ch. 2]")
  expect_identical(drawn("\\citet{a}"), "(author?) [?]")
  expect_identical(drawn("\\citealp{a}"), "?")
  expect_warning(latex_grob("\\citet{k1}", input_mode = "document"),
                 "citation `k1' is undefined: drawn as (author?) [?]", fixed = TRUE)
})

test_that("\\specialrule is a rule of the thickness it names", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  r <- latex_tree("\\begin{tabular}{l}a \\\\ \\specialrule{3pt}{0pt}{0pt} b\\end{tabular}",
                  input_mode = "document")$records
  expect_identical(r$text[r$type == "text"], c("a", "b"))  # its arguments are read
  expect_equal(r$lwd[r$type == "line"], 3 * 72 / 72.27, tolerance = 1e-4)
})

test_that("\\boldmath makes the math of its group bold, and only the math", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  glyphs <- function(tex) {
    r <- latex_tree(tex, input_mode = "document")$records
    r$glyph[r$type == "glyph"]
  }
  expect_false(identical(glyphs("{\\boldmath $x + 2$}"), glyphs("$x + 2$")))
  # It ends with its group.
  expect_identical(glyphs("{\\boldmath $x$} $x$")[2], glyphs("$x$"))
  words <- function(tex) latex_tree(tex, input_mode = "document")$records$font_style
  expect_identical(words("{\\boldmath words}"), words("words"))
})

test_that("an abstract has article's heading, and a bibliography its [n] labels", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  a <- runs("\\begin{abstract}Body text.\\end{abstract}")
  expect_identical(a$text, c("Abstract", "Body text."))
  expect_gt(a$x[1], a$x[2])  # centred over the text
  bib <- paste("\\begin{thebibliography}{9}",
               "\\bibitem{one} A. Author. \\newblock {\\em Title}, 2016.",
               "\\bibitem{two} B. Author.", "\\end{thebibliography}", sep = "\n")
  b <- runs(bib)
  expect_identical(b$text[1], "References")
  expect_false(any(grepl("one|two", b$text)))  # the keys point at nothing
  labels <- function(tex) {
    r <- latex_tree(tex, input_mode = "document")$records
    r$glyph[r$type == "glyph"]
  }
  expect_identical(labels(bib),
                   labels("\\begin{enumerate}[{[}\\arabic*{]}]\\item a \\item b\\end{enumerate}"))
})

test_that("a minipage sets its body to its width, placed as LaTeX places it", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  long <- paste(rep("word", 12), collapse = " ")
  text <- function(tex, ...) {
    r <- latex_tree(tex, input_mode = "document", ...)$records
    r[r$type == "text", ]
  }
  for (pos in c("t", "c", "b")) {
    r <- text(sprintf("A \\begin{minipage}[%s]{100pt}%s\\end{minipage} B", pos, long))
    words <- r[r$text == "word", ]
    lines <- sort(unique(round(words$y, 2)))
    expect_gt(length(lines), 3L, label = pos)                    # it wraps...
    expect_lte(max(words$x) - min(words$x), 100, label = pos)     # ...inside its width
    expect_identical(unique(round(words$x[words$y == min(words$y)], 2))[1],
                     round(min(words$x), 2), label = pos)          # with no indent
    a <- round(r$y[r$text == "A "], 2)
    expected <- switch(pos, t = lines[1], b = lines[length(lines)])
    if (pos == "c") {
      expect_gt(a, lines[1], label = pos)
      expect_lt(a, lines[length(lines)], label = pos)
    } else {
      expect_equal(a, expected, label = pos)
    }
  }
  # \centering centres each line in the minipage's width, not the page's.
  r <- text("\\begin{minipage}{200pt}\\centering ab\\end{minipage}")
  plain <- text("\\begin{minipage}{200pt}ab\\end{minipage}")
  expect_gt(r$x, plain$x + 50)
  # 0.5\textwidth is half of max_width.
  two <- text("\\begin{minipage}{0.5\\textwidth}\\centering L\\end{minipage}%\n\\begin{minipage}{0.5\\textwidth}\\centering R\\end{minipage}",
              max_width = 300)
  expect_equal(two$x[two$text == "R"] - two$x[two$text == "L"], 150, tolerance = 5)
  # Its body is paragraphs, so markdown does not take it for math.
  expect_false("minipage" %in% .math_envs())
})

test_that("a minipage's body is paragraphs, as the text around it is", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # A blank line starts a paragraph in it, as it does outside one.
  inside <- runs("\\begin{minipage}{200pt}First para.\n\nSecond para.\\end{minipage}")
  expect_identical(length(unique(inside$y)), 2L)
  # The line ends around its body -- the usual way to write one -- are
  # nothing, and do not push its lines off their place.
  expect_identical(runs("\\begin{minipage}{200pt}\n  Text  \n\\end{minipage}"),
                   runs("\\begin{minipage}{200pt}Text\\end{minipage}"))
  expect_identical(runs("\\begin{minipage}{200pt}\\centering\n  Text  \n\\end{minipage}"),
                   runs("\\begin{minipage}{200pt}\\centering Text\\end{minipage}"))
  # [b] meets the last line of all, when the breaker broke the paragraph
  # (or a centred line) that ends it.
  r <- runs(paste("A \\begin{minipage}[b]{100pt}\\centering Short\\\\",
                  paste(rep("word", 10), collapse = " "), "\\end{minipage} B"))
  expect_gt(length(unique(r$y[r$text == "word"])), 2L)
  expect_equal(r$y[r$text == "A "], max(r$y[r$text == "word"]))
  expect_equal(r$y[r$text == " B"], max(r$y[r$text == "word"]))
})

test_that("a heading that wraps hangs its title from the number, as LaTeX does", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  title <- "A rather long section title that has to wrap"
  r <- runs(sprintf("\\section{%s}", title), max_width = 200)
  number <- r[r$text == "1", ]
  words <- r[r$text != "1", ]
  expect_gt(length(unique(round(words$y))), 2L)
  # Every line of the title starts where its first line does.
  starts <- tapply(words$x, round(words$y), min)
  expect_identical(length(unique(round(starts, 2))), 1L)
  expect_gt(min(starts), number$x)
  # One that fits is set as it always was, on one line.
  expect_identical(runs("\\section{Short}", max_width = 200),
                   runs("\\section{Short}", max_width = 5000))
})

test_that("a minipage taller than its body puts the room where it is told", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  at <- function(inner) {
    r <- runs(sprintf("A \\begin{minipage}[t][80pt][%s]{60pt}one\\end{minipage} B", inner))
    c(a = r$y[r$text == "A "], one = r$y[r$text == "one"])
  }
  top <- at("t"); mid <- at("c"); bottom <- at("b")
  # [t] meets the body's first line; the room goes below it (t), around
  # it (c) or above it (b), so the body moves down in that order.
  expect_equal(top[["a"]], top[["one"]])
  expect_lt(top[["one"]], mid[["one"]])
  expect_lt(mid[["one"]], bottom[["one"]])
  expect_equal(bottom[["one"]] - top[["one"]], 2 * (mid[["one"]] - top[["one"]]), tolerance = 0.5)
})

test_that("\\textwidth and \\linewidth are the text width, as in LaTeX", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  w <- function(tex, ...) as.numeric(latex_dims(tex, input_mode = "math", ...)$width)
  expect_equal(w("\\rule{0.5\\textwidth}{1pt}", max_width = 200), 100, tolerance = 0.01)
  expect_equal(w("\\rule{\\linewidth}{1pt}", max_width = 200), 200, tolerance = 0.01)
  # With no page to measure, article's 345pt.
  expect_equal(w("\\rule{\\columnwidth}{1pt}"), 345 * 72 / 72.27, tolerance = 0.01)
})
