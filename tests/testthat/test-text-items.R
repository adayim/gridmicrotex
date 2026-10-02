# The core-LaTeX text items a pasted document uses: \verb and verbatim,
# description lists, small caps, and theorems with their proofs.

rec <- function(tex, mode = "document", w = 300) {
  latex_tree(tex, input_mode = mode, max_width = w, render_mode = "typeface")
}

ns <- function(x) gsub(" ", "", x, fixed = TRUE)

# The lines of text of a layout, top to bottom, each its text records left
# to right. A space is a gap between records, not a record, so the lines
# have none; `ns()` takes them out of what a test expects.
lines_of <- function(tex, mode = "document", w = 300) {
  r <- suppressWarnings(rec(tex, mode, w))$records
  r <- r[r$type == "text", ]
  if (!nrow(r)) return(character(0))
  r <- r[order(round(r$y), r$x), ]
  y <- round(r$y)
  unname(vapply(split(r$text, factor(y, levels = sort(unique(y)))), paste,
                "", collapse = ""))
}

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

text_records <- function(tex, mode = "document", w = 300) {
  r <- suppressWarnings(rec(tex, mode, w))$records
  r[r$type == "text", ]
}

# font_style: bit 1 bold, bit 2 italic, bit 128 the typewriter face.
is_mono <- function(r) bitwAnd(r$font_style, 128L) != 0L
is_bold <- function(r) bitwAnd(r$font_style, 2L) != 0L

test_that("\\verb sets its text as written, in the typewriter face", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- r"(see \verb|a_b & 50% \x{y}| here)"
  expect_identical(warns(tex), character(0))
  r <- text_records(tex)
  v <- r[is_mono(r), ]
  expect_identical(paste(v$text, collapse = ""), ns(r"(a_b & 50% \x{y})"))
  expect_false(any(is_mono(r[!r$text %in% v$text, ])))
  # Any character can delimit it, a brace included.
  expect_identical(lines_of(r"(\verb+a|b+)"), "a|b")
  expect_identical(lines_of(r"(\verb{a}b{)"), "a}b")
})

test_that("\\verb* shows its spaces", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(lines_of(r"(\verb*|a b|)"), "a\u2423b")
})

test_that("\\verb works in a label", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(lines_of(r"(call \verb|f(x)| now)", "mixed"),
                   ns("call f(x) now"))
})

test_that("a \\verb that does not end on its line warns and takes the line", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- "\\verb|abc\nnext line"
  expect_match(warns(tex), "verb", all = FALSE)
  expect_true(any(grepl("nextline", lines_of(tex))))
})

test_that("verbatim keeps every line, its indentation and its specials", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- "\\begin{verbatim}\nx <- 1 # 50%\n  f(x_1) & {y}\n\\end{verbatim}\nafter"
  expect_identical(warns(tex), character(0))
  ls <- lines_of(tex)
  expect_identical(ls[1:2], ns(c("x <- 1 # 50%", "f(x_1) & {y}")))
  expect_identical(ls[3], "after")
  r <- text_records(tex)
  expect_true(all(is_mono(r[r$text != "after", ])))
  # The second line starts to the right of the first: its two spaces.
  first <- min(r$x[round(r$y) == min(round(r$y))])
  second <- min(r$x[round(r$y) == sort(unique(round(r$y)))[2]])
  expect_gt(second, first)
})

test_that("verbatim reads a backslash command as text and ends at its own \\end", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- "\\begin{verbatim}\n\\begin{itemize} \\end{itemize}\n\\end{verbatim}"
  expect_identical(lines_of(tex), ns(r"(\begin{itemize} \end{itemize})"))
  expect_identical(warns(tex), character(0))
})

test_that("verbatim* shows spaces, and an unclosed verbatim warns", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(lines_of("\\begin{verbatim*}\na b\n\\end{verbatim*}"),
                   "a\u2423b")
  expect_match(warns("\\begin{verbatim}\nabc"), "verbatim", all = FALSE)
})

test_that("a whole document finds its body around a verbatim \\end{document}", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- paste("\\documentclass{article}\\begin{document}",
               "\\begin{verbatim}", "\\end{document}", "\\end{verbatim}",
               "kept \\end{document} dropped", sep = "\n")
  expect_identical(lines_of(tex), c("\\end{document}", "kept"))
})

test_that("description sets a bold label hanging before each item", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- "\\begin{description}\\item[Alpha] first\\item[Beta] second\\end{description}"
  expect_identical(warns(tex), character(0))
  r <- text_records(tex)
  lab <- r[r$text %in% c("Alpha", "Beta"), ]
  expect_equal(nrow(lab), 2L)
  expect_true(all(is_bold(lab)))
  # The two labels share a left edge, and their texts another.
  expect_equal(lab$x[1], lab$x[2])
  txt <- r[r$text %in% c("first", "second"), ]
  expect_equal(txt$x[1], txt$x[2])
  expect_gt(txt$x[1], lab$x[1] + 10)
  expect_equal(lines_of(tex), c("Alphafirst", "Betasecond"))
})

test_that("an \\item[label] is the marker of an itemize or enumerate item", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- "\\begin{enumerate}\\item[(a)] one\\item next\\end{enumerate}"
  r <- suppressWarnings(rec(tex))$records
  mark <- r[r$type == "glyph", ]
  first <- mark[round(mark$y) == min(round(mark$y)), ]
  second <- mark[round(mark$y) == max(round(mark$y)), ]
  # "(a)" for the labelled item; the next one is the first counted, "1.",
  # as in a list with no label at all.
  expect_equal(nrow(first), 3L)
  plain <- suppressWarnings(
    rec(r"(\begin{enumerate}\item next\end{enumerate})"))$records
  plain <- plain[plain$type == "glyph", ]
  expect_identical(second$glyph[order(second$x)], plain$glyph[order(plain$x)])
  # No bracket is drawn, and a label in itemize is the item's marker.
  tex2 <- "\\begin{itemize}\\item[$\\star$] one\\item two\\end{itemize}"
  g <- suppressWarnings(rec(tex2))$records
  g <- g[g$type == "glyph", ]
  expect_equal(nrow(g), 2L)
})

test_that("\\textsc draws capitals, the lowercase ones smaller", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  r <- text_records("\\textsc{Hello World}", "mixed")
  expect_identical(paste(r$text, collapse = ""), "HELLOWORLD")
  expect_equal(sort(unique(round(r$font_size))), c(16, 20))
  # H and W are the big ones.
  expect_equal(round(r$font_size[r$text == "H"]), 20)
  expect_equal(round(r$font_size[r$text == "ELLO"]), 16)
  # The declaration does the same, to the end of its group.
  r2 <- text_records("{\\scshape Hello} plain", "mixed")
  expect_identical(paste(r2$text, collapse = ""), "HELLOplain")
  expect_equal(round(r2$font_size[r2$text == "plain"]), 20)
})

thm <- "\\newtheorem{thm}{Theorem}\n"

test_that("a theorem is numbered, titled in bold and set in italics", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- paste0(thm, "\\begin{thm}It holds.\\end{thm}\n",
                "\\begin{thm}And again.\\end{thm}")
  expect_identical(warns(tex), character(0))
  expect_identical(lines_of(tex), ns(c("Theorem 1.It holds.", "Theorem 2.And again.")))
  r <- text_records(tex)
  title <- r[grepl("^Theorem", r$text), ]
  expect_true(all(is_bold(title)))
  body <- r[grepl("holds", r$text), ]
  expect_false(is_bold(body))
  expect_true(bitwAnd(body$font_style, 4L) != 0L)
})

test_that("a theorem takes a note, and \\newtheorem* is unnumbered", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- paste0(thm, "\\newtheorem*{rem}{Remark}\n",
                "\\begin{thm}[Euler]Body.\\end{thm}\n\\begin{rem}Plain.\\end{rem}")
  expect_identical(lines_of(tex), ns(c("Theorem 1 (Euler).Body.", "Remark.Plain.")))
})

test_that("theorems share a counter or restart within a section", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  shared <- paste0(thm, "\\newtheorem{lem}[thm]{Lemma}\n",
                   "\\begin{thm}A\\end{thm}\\begin{lem}B\\end{lem}")
  expect_identical(lines_of(shared), ns(c("Theorem 1.A", "Lemma 2.B")))
  within <- paste0("\\newtheorem{thm}{Theorem}[section]\n",
                   "\\section{One}\\begin{thm}A\\end{thm}\\begin{thm}B\\end{thm}\n",
                   "\\section{Two}\\begin{thm}C\\end{thm}")
  ls <- lines_of(within)
  expect_true(all(ns(c("Theorem 1.1.A", "Theorem 1.2.B", "Theorem 2.1.C")) %in% ls))
})

test_that("a theorem can be labelled and referred to", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- paste0(thm, "\\begin{thm}\\label{t:a}A\\end{thm}\nBy Theorem~\\ref{t:a} and then.")
  expect_identical(warns(tex), character(0))
  expect_true(ns("By Theorem 1 and then.") %in% lines_of(tex))
})

test_that("a proof ends with a box, and an environment never defined warns", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- "\\begin{proof}Trivial.\\end{proof}"
  ls <- lines_of(tex)
  expect_match(ls[1], "^Proof\\.Trivial\\.")
  r <- suppressWarnings(rec(tex))$records
  box <- r[r$type == "glyph", ]
  expect_equal(nrow(box), 1L)
  expect_gt(box$x, max(r$x[r$type == "text"]))
  expect_identical(warns(tex), character(0))
  body <- r[which(r$text == "Trivial."), ]
  expect_true(bitwAnd(body$font_style, 4L) == 0L)  # upright
  expect_match(warns("\\begin{nothm}x\\end{nothm}"), "nothm", all = FALSE)
})

test_that("a proof wraps to the text width as any paragraph does", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  long <- paste(rep("some words that need room", 8), collapse = " ")
  r <- text_records(paste0("\\begin{proof}", long, "\\end{proof}"), w = 200)
  expect_gt(length(unique(round(r$y))), 1L)
  expect_lte(max(r$x), 200 + 30)
})

test_that("a font switch lasts across a paragraph break", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  r <- text_records("x {\\itshape one\\par two} three")
  expect_true(all(bitwAnd(r$font_style[r$text %in% c("one", "two")], 4L) != 0L))
  expect_true(all(bitwAnd(r$font_style[r$text %in% c("x", "three")], 4L) == 0L))
})
