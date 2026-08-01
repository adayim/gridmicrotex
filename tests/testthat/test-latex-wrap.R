
test_that("latex_wrap converts correctly", {
  expect_equal(latex_wrap("It is $x^2$"),
               "\\text{It is }x^2")
  expect_equal(latex_wrap("\\(E=mc^2\\) is famous"),
               "E=mc^2\\text{ is famous}")
  expect_equal(latex_wrap("plain text"),
               "\\text{plain text}")
  expect_equal(latex_wrap("cost \\$5 for $x$"),
               "\\text{cost \\$5 for }x")
  expect_equal(latex_wrap("Line 1\nLine 2"),
               "\\text{Line 1}\\\\\\text{Line 2}")
})

test_that("latex_wrap is vectorized over its input", {
  # Documented contract: vector in, vector of the same length out.
  expect_equal(
    latex_wrap(c("plain text", "It is $x^2$", "")),
    c("\\text{plain text}", "\\text{It is }x^2", "")
  )
  # math mode bypasses every element unmodified
  expect_equal(
    latex_wrap(c("\\frac{a}{b}", "x^2"), input_mode = "math"),
    c("\\frac{a}{b}", "x^2")
  )
  expect_equal(latex_wrap(character(0)), character(0))
})

test_that("latex_wrap rejects NA input", {
  expect_error(latex_wrap(NA_character_), "must not contain NA")
  expect_error(latex_wrap(c("a", NA, "b")), "must not contain NA")
  expect_error(latex_wrap(NA_character_, input_mode = "math"),
               "must not contain NA")
})

test_that(".find_close_brace balances nested and escaped braces", {
  fcb <- gridmicrotex:::.find_close_brace
  expect_equal(fcb("a}b", 1L), 2L)
  expect_equal(fcb("a{b}c}d", 1L), 6L)         # nested {} skipped
  expect_equal(fcb("a\\}b}c", 1L), 5L)         # escaped \} skipped
  expect_true(is.na(fcb("a{b}c", 1L)))         # unbalanced
})

test_that(".strip_document_wrappers handles comments and floats", {
  strip <- gridmicrotex:::.strip_document_wrappers

  expect_equal(strip("% leading comment\n$x^2$"), "$x^2$")
  expect_equal(strip("$50\\% + x^2$"), "$50\\% + x^2$")

  src <- "\\begin{table}[ht]\n\\centering\n\\begin{tabular}{rr} a & b \\\\ \\end{tabular}\n\\end{table}"
  out <- strip(src)
  expect_false(grepl("\\\\begin\\{table",  out))
  expect_false(grepl("\\\\end\\{table",    out))
  expect_false(grepl("\\\\centering",      out))
  expect_true( grepl("\\\\begin\\{tabular\\}", out))

  # figure floats + starred variants
  expect_false(grepl("\\\\begin\\{figure",
                     strip("\\begin{figure*}[ht]x\\end{figure*}")))
  expect_false(grepl("\\\\begin\\{table",
                     strip("\\begin{table*}[ht]x\\end{table*}")))
})

test_that(".strip_document_wrappers removes preamble and title metadata", {
  strip <- gridmicrotex:::.strip_document_wrappers

  src <- "\\documentclass[12pt]{article}\\usepackage[utf8]{inputenc}\\usepackage{amsmath}\\begin{document}x^2\\end{document}"
  out <- strip(src)
  expect_false(grepl("documentclass", out))
  expect_false(grepl("usepackage",    out))
  expect_false(grepl("begin\\{document",  out))
  expect_false(grepl("end\\{document",    out))
  expect_true( grepl("x\\^2", out))

  out2 <- strip("\\maketitle\\title{Foo}\\author{Bar}\\label{eq:1}content")
  expect_equal(out2, "content")

  # alignment scope declarations
  for (cmd in c("centering", "raggedright", "raggedleft",
                "flushleft", "flushright")) {
    expect_false(grepl(cmd, strip(paste0("\\", cmd, " x"))))
  }
})

test_that("booktabs rules alias correctly", {
  strip <- gridmicrotex:::.strip_document_wrappers

  # top/bottom rules render thicker; middle stays normal
  expect_equal(strip("\\toprule a\\midrule b\\bottomrule"),
               "\\thickhline a\\hline b\\thickhline")

  # \cmidrule keeps its column range via \cline (no longer lossy)
  expect_equal(strip("\\cmidrule{2-3}"),          "\\cline{2-3}")
  expect_equal(strip("\\cmidrule(lr){2-3}"),      "\\cline{2-3}")
  expect_equal(strip("\\cmidrule[2pt]{2-3}"),     "\\cline{2-3}")
  expect_equal(strip("\\cmidrule[2pt](lr){2-3}"), "\\cline{2-3}")
})

test_that("Tier 1 aliases rewrite to MicroTeX-supported commands", {
  strip <- gridmicrotex:::.strip_document_wrappers

  expect_equal(strip("\\emph{stress}"),       "\\textit{stress}")
  expect_equal(strip("\\textnormal{plain}"),  "\\text{plain}")
  expect_equal(strip("a\\par b"),             "a\\\\ b")
  expect_equal(strip("a\\newline b"),         "a\\\\ b")

  # nested braces in argument
  expect_equal(strip("\\emph{a $x_{i}$ b}"),
               "\\textit{a $x_{i}$ b}")
})

test_that("content-free declarations are stripped", {
  strip <- gridmicrotex:::.strip_document_wrappers
  for (cmd in c("noindent", "relax")) {
    expect_false(grepl(cmd, strip(paste0("\\", cmd, " x"))))
  }
})

test_that("skip and fill commands map to em-relative \\vspace / \\quad", {
  strip <- gridmicrotex:::.strip_document_wrappers

  expect_equal(strip("a\\smallskip b"), "a\\vspace{0.25em} b")
  expect_equal(strip("a\\medskip b"),   "a\\vspace{0.5em} b")
  expect_equal(strip("a\\bigskip b"),   "a\\vspace{1em} b")
  expect_equal(strip("a\\hfill b"),     "a\\quad b")
  expect_equal(strip("a\\vfill b"),     "a\\vspace{1em} b")

  # mapped commands render end-to-end (MicroTeX has \vspace and \quad)
  d <- latex_dims("a\\bigskip b")
  expect_gt(as.numeric(d$width),  0)
  expect_gt(as.numeric(d$height), 0)
})

test_that("\\caption{X} is extracted as inline \\text{X}\\\\", {
  strip <- gridmicrotex:::.strip_document_wrappers

  expect_equal(strip("\\caption{Hello}"),     "\\text{Hello}\\\\")
  expect_equal(strip("a \\caption{Hi} b"),    "a \\text{Hi}\\\\ b")
  expect_equal(strip("\\caption[short]{Long}"), "\\text{Long}\\\\")

  # nested braces in caption content (e.g. inline subscripts)
  expect_equal(strip("\\caption{Foo $x_{i}$}"),
               "\\text{Foo $x_{i}$}\\\\")
})

test_that("every break lands at the formula level, not inside the text", {
  # A break kept inside the \text{} makes a multi-line text *box*, and a
  # box is one item in the enclosing row -- so anything after it was set
  # beside the whole box rather than on the line it was written on.
  expect_equal(latex_wrap("Title\n$x^2$"), "\\text{Title}\\\\x^2")
  expect_equal(latex_wrap("Caption\n\\begin{array}{l}a\\end{array}"),
               "\\text{Caption}\\\\\\begin{array}{l}a\\end{array}")
  # Interior breaks too, which is what puts trailing math on the right
  # line instead of centring it between the two.
  expect_equal(latex_wrap("Line 1\nLine 2"), "\\text{Line 1}\\\\\\text{Line 2}")
  expect_equal(latex_wrap("a\nb $x$"), "\\text{a}\\\\\\text{b }x")
  # A literal \\ is a break like any other.
  expect_equal(latex_wrap("a\\\\b"), "\\text{a}\\\\\\text{b}")
  # Nothing but breaks between two math spans is a separator, not text.
  expect_equal(latex_wrap("$a$\n$b$"), "a\\\\b")
  # A break with nothing on one side of it would only add a blank first
  # or last row.
  expect_equal(latex_wrap("\n\nTitle\n"), "\\text{Title}")
  # A run collapses to one, the way consecutive blank lines do in LaTeX.
  expect_equal(latex_wrap("a\n\n\n$x$"), "\\text{a}\\\\x")
  # No break, no change.
  expect_equal(latex_wrap("It is $x^2$"), "\\text{It is }x^2")
  expect_equal(latex_wrap(" "), "\\text{ }")
})

test_that("splitting a prose run does not move it", {
  # The split is only free if the two forms lay out identically.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  xy <- function(tex) {
    d <- latex_grob(tex, input_mode = "math",
                    gp = grid::gpar(fontsize = 14))$layout_df
    d[, c("x", "y")]
  }
  expect_equal(xy("\\text{line one}\\\\\\text{line two}"),
               xy("\\text{line one \\\\line two}"))
})

test_that("a pasted caption sits above its table, not beside it", {
  # End to end, because the regression was invisible in the wrapped string
  # alone: \caption becomes \text{X}\\, and in mixed mode that trailing
  # break used to be swallowed into the surrounding \text{}, leaving the
  # caption as a box to the LEFT of the tabular.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  snippet <- paste(
    "\\begin{table}[ht]", "\\centering", "\\caption{Model coefficients}",
    "\\begin{tabular}{lr}", "\\toprule", "Term & Estimate \\\\",
    "\\midrule", "Intercept & 2.14 \\\\", "\\bottomrule",
    "\\end{tabular}", "\\end{table}", sep = "\n")

  d <- latex_grob(snippet, input_mode = "mixed",
                  gp = grid::gpar(fontsize = 11))$layout_df
  cap <- d[!is.na(d$text) & grepl("Model", d$text), ]
  rules <- d[d$type == "line", ]
  expect_equal(nrow(cap), 1L)
  # Above: the caption's baseline is higher up the page than the top rule
  # (y grows downward), and it starts at the same left edge.
  expect_lt(cap$y, min(rules$y))
  expect_equal(cap$x, min(rules$x))
})

test_that("strip preserves legitimate MicroTeX math commands", {
  strip <- gridmicrotex:::.strip_document_wrappers

  # Size commands (mac(0, macro_sizes, ...)) and \multirow stay intact.
  passthrough <- c(
    "\\sum_{i=1}^n x_i",
    "\\int_0^\\infty e^{-x} dx",
    "\\alpha + \\beta",
    "\\frac{a}{b}",
    "\\small \\text{tiny note}",
    "\\Large \\text{title-ish}",
    "\\multirow{2}{*}{a}",
    "\\hline\\hline"
  )
  for (s in passthrough) expect_equal(strip(s), s)
})

test_that("latex_grob renders raw xtable output end-to-end", {
  src <- paste(
    "% latex table generated by xtable",
    "\\begin{table}[ht]",
    "\\centering",
    "\\begin{tabular}{rrr} \\hline a & b & c \\\\ \\hline 1 & 2 & 3 \\\\ \\hline \\end{tabular}",
    "\\caption{Cox fit}",
    "\\label{tab:cox}",
    "\\end{table}",
    sep = "\n"
  )
  d <- latex_dims(src)
  expect_gt(as.numeric(d$width),  0)
  expect_gt(as.numeric(d$height), 0)
})

test_that("latex_grob renders kable booktabs-style tabular", {
  src <- "\\begin{tabular}{lr}\\toprule x & y \\\\ \\midrule 1 & 2 \\\\ 3 & 4 \\\\ \\bottomrule \\end{tabular}"
  d <- latex_dims(src)
  expect_gt(as.numeric(d$width),  0)
  expect_gt(as.numeric(d$height), 0)
})

test_that("starred env variants are normalized to non-starred", {
  strip <- gridmicrotex:::.strip_document_wrappers
  expect_equal(strip("\\begin{align*}a&=b\\end{align*}"),
               "\\begin{align}a&=b\\end{align}")
  expect_equal(strip("\\begin{alignat*}{2}a&=b\\end{alignat*}"),
               "\\begin{alignat}{2}a&=b\\end{alignat}")

  # End-to-end: starred align/eqnarray previously errored on '&' because
  # MicroTeX didn't recognise the env and never entered array mode.
  alignment_cases <- list(
    "\\begin{align*} a &= b \\end{align*}",
    "\\begin{eqnarray*} a & = & b \\end{eqnarray*}",
    "\\begin{flalign*} a &= b \\end{flalign*}",
    "\\begin{alignat*}{2} a & = b & c & = d \\end{alignat*}",
    "\\begin{alignedat*}{2} a & = b & c & = d \\end{alignedat*}"
  )
  for (src in alignment_cases) {
    d <- latex_dims(src)
    expect_gt(as.numeric(d$width), 0)
  }
})

test_that("latex_wrap passes every MicroTeX-registered env verbatim", {
  # These are the environments registered in
  # src/MicroTeX/lib/macro/macro_def.cpp; mixed mode must not wrap them
  # in \text{} or split them at $-delimiters.
  envs <- c("array", "tabular", "matrix", "smallmatrix",
            "pmatrix", "bmatrix", "Bmatrix", "vmatrix", "Vmatrix",
            "eqnarray", "align", "flalign", "alignat", "aligned",
            "alignedat", "multline", "cases", "rcases", "split",
            "gather", "gathered", "math", "displaymath", "equation")
  for (e in envs) {
    src <- sprintf("\\begin{%s}a & b\\end{%s}", e, e)
    out <- latex_wrap(src, input_mode = "mixed")
    expect_false(grepl("\\\\text\\{", out),
                 info = sprintf("env '%s' got wrapped in \\text{}", e))
    expect_equal(out, src, info = sprintf("env '%s' was modified", e))
  }
})

test_that("tabular is treated as a math environment in mixed mode", {
  # Regression: `$...$` inside cells used to chop the tabular into text
  # chunks because latex_wrap didn't know `tabular` was an array env.
  src <- "\\begin{tabular}{rr}a & Pr($>$F) \\\\ 1 & 2 \\end{tabular}"
  out <- latex_wrap(src, input_mode = "mixed")
  # The whole tabular block should pass through verbatim, not be wrapped
  # in \text{} or split at the $ toggles.
  expect_false(grepl("\\\\text\\{", out))
  expect_equal(out, src)

  # End-to-end: an xtable-style table with $-delimited math in a header
  # renders without collapsing into a single column.
  d <- latex_dims(src)
  expect_gt(as.numeric(d$width),  50)
  expect_gt(as.numeric(d$height), 0)
})

test_that("MicroTeX renders \\thickhline and \\cline natively", {
  d1 <- latex_dims("\\begin{array}{cc}\\thickhline a & b \\\\ \\thickhline\\end{array}")
  expect_gt(as.numeric(d1$width),  0)
  expect_gt(as.numeric(d1$height), 0)

  d2 <- latex_dims("\\begin{array}{cccc}a & b & c & d \\\\ \\cline{2-3} 1 & 2 & 3 & 4\\end{array}")
  expect_gt(as.numeric(d2$width),  0)
  expect_gt(as.numeric(d2$height), 0)

  # single-column form: \cline{2} == \cline{2-2}
  d3 <- latex_dims("\\begin{array}{cc}a & b \\\\ \\cline{2}\\end{array}")
  expect_gt(as.numeric(d3$width), 0)
})

test_that("\\cmidrule renders end-to-end via \\cline", {
  src <- "\\begin{tabular}{lrr}\\toprule x & y & z \\\\ \\cmidrule(lr){2-3} 1 & 2 & 3 \\\\ \\bottomrule \\end{tabular}"
  d <- latex_dims(src)
  expect_gt(as.numeric(d$width),  0)
  expect_gt(as.numeric(d$height), 0)
})

test_that("strip emits no messages — all transforms are silent", {
  msgs <- testthat::capture_messages(
    latex_dims("\\caption{X}\\usepackage{amsmath}\\toprule x \\bottomrule")
  )
  expect_equal(length(msgs), 0L)
})

test_that("links are styled, following LaTeX's convention not HTML's", {
  strip <- gridmicrotex:::.strip_document_wrappers
  col <- gridmicrotex:::.MD_LINK_COLOR

  # hyperref with colorlinks=true colours a link and does not underline it;
  # the `url` package sets a URL in monospace. The markdown side uses the
  # HTML convention (blue AND underlined) instead -- see the `a` rule.
  expect_equal(strip("\\href{https://ex.org}{the text}"),
               paste0("\\textcolor{", col, "}{the text}"))
  expect_equal(strip("\\url{https://ex.org}"),
               paste0("\\textcolor{", col, "}{\\texttt{https://ex.org}}"))

  # A URL is verbatim in real LaTeX, so its specials are escaped: an
  # unescaped _ would open a subscript.
  expect_equal(strip("\\url{a.io/x_y}"),
               paste0("\\textcolor{", col, "}{\\texttt{a.io/x\\_y}}"))

  # Links are rewritten before the comment stripper, so a % inside a URL
  # survives as \% instead of eating the rest of the line.
  expect_equal(strip("\\url{a.io/x%20y} tail"),
               paste0("\\textcolor{", col, "}{\\texttt{a.io/x\\%20y}} tail"))

  # \href text is ordinary LaTeX and keeps its markup.
  expect_equal(strip("\\href{u}{\\textbf{b}}"),
               paste0("\\textcolor{", col, "}{\\textbf{b}}"))

  # Malformed input is left alone rather than half-rewritten.
  expect_equal(strip("\\href{only-one-group}"), "\\href{only-one-group}")
})

test_that("an ampersand in prose is a literal, not an alignment tab", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # MicroTeX reads a bare `&` as an alignment tab even inside \text{} and
  # drops everything after it, so "Treatment & Control" rendered as
  # "Treatment " -- half the label gone, with no error. `&` cannot mean
  # alignment in prose: math spans and tabular environments are passed
  # through verbatim by the scanner and never reach the prose escaper.
  drawn <- function(s) {
    d <- latex_grob(s, gp = grid::gpar(fontsize = 14))$layout_df
    paste(d$text[d$type == "text" & !is.na(d$text)], collapse = "")
  }
  expect_equal(drawn("Treatment & Control"), "Treatment  Control")
  expect_equal(drawn("Cats & Dogs & Mice"), "Cats  Dogs  Mice")

  # An ampersand the user already escaped must not be escaped twice.
  expect_equal(latex_wrap("a \\& b"), "\\text{a \\& b}")
  expect_equal(latex_wrap("a & b"), "\\text{a \\& b}")

  # Verbatim regions keep their real alignment tabs.
  expect_equal(latex_wrap("\\begin{tabular}{ll}a&b\\end{tabular}"),
               "\\begin{tabular}{ll}a&b\\end{tabular}")
  expect_equal(latex_wrap("$\\begin{matrix}a&b\\end{matrix}$"),
               "\\begin{matrix}a&b\\end{matrix}")
})

test_that("a line break inside a group re-opens the group on the next line", {
  # Each segment becomes its own \text{}, so a break inside \textbf{}
  # used to leave the group open at the end of one segment and unopened
  # at the start of the next: the second line came out unstyled and any
  # text after the group escaped the \text{} wrapper entirely.
  expect_equal(latex_wrap("Hello \\textbf{big\nbold} world"),
               "\\text{Hello \\textbf{big}}\\\\\\text{\\textbf{bold} world}")

  # The re-opener carries the command's arguments, not just its name --
  # matching only `\textcolor` reopened a plain `{two}` and lost the colour.
  expect_equal(latex_wrap("\\textcolor{red}{one\ntwo}"),
               "\\text{\\textcolor{red}{one}}\\\\\\text{\\textcolor{red}{two}}")

  # Nesting re-opens every level, outermost first.
  expect_equal(latex_wrap("a \\textbf{\\textit{p\nq}} b"),
               "\\text{a \\textbf{\\textit{p}}}\\\\\\text{\\textbf{\\textit{q}} b}")

  # A literal \\ inside the group behaves the same as a newline.
  expect_equal(latex_wrap("A \\textbf{x \\\\ y} B"),
               "\\text{A \\textbf{x}}\\\\\\text{\\textbf{y} B}")

  # Escaped braces are literals and must not move the group depth.
  expect_equal(latex_wrap("a \\{b\nc\\} d"),
               "\\text{a \\{b}\\\\\\text{c\\} d}")

  # Breaks outside any group are unchanged.
  expect_equal(latex_wrap("plain\ntext"), "\\text{plain}\\\\\\text{text}")
})

test_that("the styling really survives the break, not just the source", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  face <- function(s) {
    d <- latex_grob(s, gp = grid::gpar(fontsize = 14))$layout_df
    d <- d[d$type == "text" & !is.na(d$text), ]
    stats::setNames(d$font_style, d$text)
  }
  # FontStyle is a bitmask: 1 = rm, +2 = bold, +4 = italic.
  f <- face("Hello \\textbf{big\nbold} world")
  expect_equal(unname(f[["big"]]), 3L)
  expect_equal(unname(f[["bold"]]), 3L)   # was 1L: the emphasis was lost
  expect_equal(unname(f[["Hello "]]), 1L)

  f <- face("a \\textbf{\\textit{p\nq}} b")
  expect_equal(unname(f[["p"]]), 7L)
  expect_equal(unname(f[["q"]]), 7L)

  col <- function(s) {
    d <- latex_grob(s, gp = grid::gpar(fontsize = 14))$layout_df
    d <- d[d$type == "text" & !is.na(d$text), ]
    stats::setNames(d$color, d$text)
  }
  cc <- col("\\textcolor{red}{one\ntwo}")
  expect_equal(unname(cc[["one"]]), "#FF0000")
  expect_equal(unname(cc[["two"]]), "#FF0000")
})

test_that("tabular* renders instead of erroring on its width argument", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # `tabular*` takes a target width before the column spec. Stripping only
  # the star left the width where the alignment belonged, and MicroTeX
  # rejected it outright with "Invalid alignment in array environment".
  strip <- gridmicrotex:::.strip_document_wrappers
  expect_equal(strip("\\begin{tabular*}{\\textwidth}{lcr}a\\end{tabular*}"),
               "\\begin{tabular}{lcr}a\\end{tabular}")

  starred <- latex_dims("\\begin{tabular*}{\\textwidth}{lcr}a&b&c\\end{tabular*}")
  plain <- latex_dims("\\begin{tabular}{lcr}a&b&c\\end{tabular}")
  expect_equal(as.numeric(starred$width), as.numeric(plain$width))
})
