# The LaTeX that R's table packages write -- knitr::kable(), kableExtra, xtable,
# gt and tinytable -- and the commands behind it. The fixtures were written
# once by those packages (tests/testthat/fixtures/tables/), so they hold
# what they really emit.

rec <- function(tex, w = 300, size = 10, mode = "document") {
  latex_tree(tex, input_mode = mode, max_width = w, render_mode = "typeface",
             gp = grid::gpar(fontsize = size))
}

warns <- function(tex, ...) {
  out <- character(0)
  withCallingHandlers(
    rec(tex, ...),
    warning = function(x) {
      out <<- c(out, conditionMessage(x))
      invokeRestart("muffleWarning")
    })
  out
}

# The horizontal extent of the rules of a layout.
rule_span <- function(tex, ...) {
  r <- rec(tex, ...)$records
  l <- r[r$type == "line", ]
  range(c(l$x, l$x2))
}

# The text of a layout, line by line, top to bottom.
lines_of <- function(tex, ...) {
  r <- rec(tex, ...)$records
  r <- r[r$type == "text", ]
  r <- r[order(round(r$y), r$x), ]
  y <- round(r$y)
  unname(vapply(split(r$text, factor(y, levels = sort(unique(y)))), paste, "",
                collapse = ""))
}

fixture_dir <- function() testthat::test_path("fixtures", "tables")

test_that("every table the packages write is read without a warning", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  files <- list.files(fixture_dir(), pattern = "\\.tex$", full.names = TRUE)
  expect_gt(length(files), 25L)
  for (f in files) {
    tex <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_identical(warns(tex), character(0), info = basename(f))
    r <- rec(tex)
    # Something is drawn, and not much wider than the measure it was given
    # (a table at the start of a paragraph is indented, as in LaTeX).
    expect_gt(nrow(r$records), 5L, label = basename(f))
    expect_lte(r$bbox[["width"]], 300 + 20, label = basename(f))
  }
})

test_that("a table's cells are where its rows and columns put them", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # kable(): three rows of four cells, a header, and rules.
  tex <- paste(readLines(file.path(fixture_dir(), "kable-booktabs.tex"), warn = FALSE),
               collapse = "\n")
  r <- rec(tex)$records
  tx <- r[r$type == "text", ]
  expect_true(all(c("Mazda", "Datsun", "Hornet") %in% tx$text))
  # Rules above and below the header and below the body.
  expect_equal(sum(r$type == "line"), 3L)
})

test_that("\\addlinespace leaves the space it is given, and half an em without", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  h <- function(sep) {
    rec(paste0(r"(\begin{tabular}{l}a\\)", sep, r"( b\end{tabular})"), w = 0, size = 20)$bbox[["height"]]
  }
  expect_equal(h(r"(\addlinespace[10pt])") - h(""), 10, tolerance = 1.5)
  expect_equal(h(r"(\addlinespace)") - h(""), 10, tolerance = 1.5)
  expect_identical(warns(r"(\begin{tabular}{l}a\\\addlinespace b\end{tabular})"),
                   character(0))
})

test_that("\\fontsize sets the size, over 10pt, to the end of its group", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  fs <- function(tex) unique(rec(tex, w = 0, size = 10, mode = "mixed")$records$font_size)
  expect_equal(fs(r"({\fontsize{20}{24}\selectfont a})"), 20)
  expect_equal(fs(r"(a{\fontsize{20}{24}\selectfont b}c)"), c(10, 20))
  # kableExtra's group, without braces.
  expect_equal(fs(r"(a\begingroup\fontsize{20}{24}\selectfont b\endgroup c)"), c(10, 20))
  # gt writes the size as a length.
  expect_equal(fs(r"({\fontsize{20.0pt}{24.0pt}\selectfont a})"), 20)
})

test_that("a table given the line width stays that wide inside a size declaration", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- r"(\fontsize{20}{24}\selectfont\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lr}\hline a & b\\\hline\end{tabular*})"
  expect_equal(diff(rule_span(tex, w = 300)), 300, tolerance = 0.5)
  tex <- r"({\large\begin{tabularx}{\linewidth}{XX}\hline a & b\\\hline\end{tabularx}})"
  expect_equal(diff(rule_span(tex, w = 300)), 300, tolerance = 0.5)
})

test_that("tabular* spreads its columns across its width, tabularx its X columns", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  star <- r"(\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lr}\hline a & b\\\hline\end{tabular*})"
  expect_equal(diff(rule_span(star, w = 300)), 300, tolerance = 0.5)
  # Without a measure it is as wide as it is.
  natural <- r"(\begin{tabular}{lr}\hline a & b\\\hline\end{tabular})"
  expect_equal(diff(rule_span(star, w = 0)), diff(rule_span(natural, w = 0)),
               tolerance = 0.5)
  x <- r"(\begin{tabularx}{\linewidth}{lX}\hline a & b\\\hline\end{tabularx})"
  expect_equal(diff(rule_span(x, w = 300)), 300, tolerance = 0.5)
  # An X column is as wide as its share, and what is in it is set at the right.
  r <- rec(paste0(r"(\begin{tabularx}{\linewidth}{>{\raggedleft\arraybackslash}X}\hline a\\\hline)",
                  r"(\end{tabularx})"))$records
  expect_gt(r$x[r$type == "text"], 250)
})

test_that("a longtable sets its head and foot once, and its caption above", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- r"(\begin{longtable}{lr}
\caption{Cap}\\
\hline A & B \endfirsthead
\hline C & D \endhead
\hline E & F \endfoot
\hline G & H \endlastfoot
1 & 2 \\ 3 & 4 \\
\end{longtable})"
  expect_identical(lines_of(tex), c("Cap", "AB", "12", "34", "GH"))
  # No first head, no last foot: the head and the foot.
  tex <- r"(\begin{longtable}{lr}\hline C & D \endhead\hline E & F \endfoot 1 & 2 \\ 3 & 4 \\\end{longtable})"
  expect_identical(lines_of(tex), c("CD", "12", "34", "EF"))
  expect_identical(warns(tex), character(0))
  # The caption is centred across the columns.
  r <- rec(r"(\begin{longtable}{lr}\caption{A wide caption here}\\ a & b\end{longtable})")$records
  cap <- r[r$type == "text" & r$y == min(r$y[r$type == "text"]), ]
  expect_lt(min(cap$x), min(r$x[r$type == "text" & r$y > min(r$y)]) + 1)
})

test_that("a longtable whose columns are repeated with a group missing is recovered from", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # *{n}{cols} without its groups once threw a std::out_of_range from the
  # column count, and the table was drawn as its name with that for a reason.
  for (spec in c("*", "*{3}", "*{3}{c", "*{99999}{*{99999}{*{99999}{*{99999}{c}}}}")) {
    tex <- paste0(r"(\begin{longtable})", "{", spec, r"(}\caption{Cap}\\ a \end{longtable})")
    expect_false(any(grepl("basic_string|substr|out_of_range", warns(tex))), info = spec)
  }
  # With one group missing it is a table of one column, and it is drawn.
  for (spec in c("*", "*{3}")) {
    tex <- paste0(r"(\begin{longtable})", "{", spec, r"(}\caption{Cap}\\ a \end{longtable})")
    expect_true("Cap" %in% lines_of(tex), info = spec)
  }
})

test_that("\\makecell, \\hhline and the font switches are read", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(lines_of(r"(\begin{tabular}{l}\makecell{a\\b}\end{tabular})"),
                   c("a", "b"))
  expect_identical(warns(r"(\begin{tabular}{ll}a & b\\\hhline{--} c & d\end{tabular})"),
                   character(0))
  # The weight and slant it is set in; the roman bit \textbf adds is no part of it.
  style <- function(tex) {
    s <- rec(tex, mode = "mixed")$records$font_style
    sort(unique(bitwAnd(s, bitwNot(1L))))
  }
  expect_identical(style(r"({\bfseries a})"), style(r"(\textbf{a})"))
  expect_identical(style(r"({\itshape a})"), style(r"(\textit{a})"))
  expect_identical(style(r"({\ttfamily a})"), style(r"(\texttt{a})"))
})

test_that("a column's font switch and suffix apply to its cells", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  fonts <- function(spec) {
    r <- rec(paste0(r"(\begin{tabular}{)", spec, r"(}a & b\end{tabular})"))$records
    r$font_style[r$type == "text"]
  }
  expect_identical(length(unique(fonts("lr"))), 1L)
  bold <- fonts(r"(>{\bfseries}lr)")
  expect_identical(length(unique(bold)), 2L)
  expect_identical(bold[1] != bold[2], TRUE)
  # <{...} follows each cell of its column.
  with <- rec(r"(\begin{tabular}{l<{x}}a\end{tabular})", w = 0)
  expect_gt(with$bbox[["width"]],
            rec(r"(\begin{tabular}{l}a\end{tabular})", w = 0)$bbox[["width"]])
  expect_identical(warns(r"(\begin{tabular}{l!{|}r}a & b\end{tabular})"), character(0))
})

test_that("a colour defined with capitals is found by the name it was given", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  r <- rec(r"(\definecolor{cFFFF00}{HTML}{FFFF00}\colorbox{cFFFF00}{x})",
           mode = "mixed")$records
  expect_true("#FFFF00" %in% r$color)
})

test_that("\\caption* is a caption, with no star", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(lines_of(r"(\begin{table}\caption*{Cap}\begin{tabular}{l}a\end{tabular}\end{table})"),
                   c("Cap", "a"))
})

# --- tabularray, as tinytable writes it -------------------------------------

tblr <- function(spec, body, outer = "") {
  paste0(r"(\begin{tblr})", if (nzchar(outer)) paste0("[", outer, "]"),
         "{", spec, "}\n", body, r"(\end{tblr})")
}

test_that("a tblr is a table of the columns its spec names", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- tblr("colspec={Q[]Q[r]}", r"(a & b \\ c & d \\)")
  expect_identical(warns(tex), character(0))
  expect_identical(lines_of(tex), c("ab", "cd"))
  r <- rec(tex)$records
  tx <- r[r$type == "text", ]
  # The second column is right-aligned: its cells end together.
  ends <- tx$x + vapply(tx$text, function(s) {
    grid::convertWidth(latex_dims(s, input_mode = "mixed",
                                  gp = grid::gpar(fontsize = 10))$width, "bigpts", TRUE)
  }, 0)
  expect_equal(unname(ends[tx$text == "b"]), unname(ends[tx$text == "d"]), tolerance = 0.5)
})

test_that("a tblr's hlines and vlines are its rules", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  body <- r"(a & b \\ c & d \\)"
  lines <- function(spec) {
    r <- rec(tblr(paste0("colspec={Q[]Q[]},", spec), body))$records
    r[r$type == "line", ]
  }
  expect_identical(nrow(lines("")), 0L)
  # Above the first row, below the first, and below the last.
  expect_identical(nrow(lines("hline{1,2,3}={0.05em}")), 3L)
  expect_identical(nrow(lines("hlines={}")), 3L)
  # One across two columns only.
  one <- lines("hline{2}={1}{solid}")
  expect_identical(nrow(one), 1L)
  full <- lines("hline{2}={solid}")
  expect_lt(one$x2 - one$x, full$x2 - full$x)
  # Vertical rules between and around the columns.
  expect_identical(nrow(lines("vlines={}")), 3L * 2L)
  # Thickness is taken from the rule.
  thin <- lines("hline{1}={0.01em}")$lwd
  thick <- lines("hline{1}={0.2em}")$lwd
  expect_gt(thick, thin)
})

test_that("a tblr's cells take the colours, fonts, alignment and spans it sets", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- tblr(paste0("colspec={Q[]Q[]Q[]},",
                     "cell{1}{2}={c=2}{halign=c},",
                     "row{2}={}{font=\\bfseries, bg=red},",
                     "column{1}={}{fg=blue}"),
              r"(Head & Span & \\ x & y & z \\)")
  expect_identical(warns(tex), character(0))
  r <- rec(tex)$records
  # The spanned cell sits across the last two columns, centred.
  tx <- r[r$type == "text", ]
  expect_equal(tx$x[tx$text == "Span"] + 8, tx$x[tx$text == "y"] + 3, tolerance = 15)
  # The second row is bold, on red.
  expect_gt(length(unique(r$font_style)), 1L)
  expect_true(any(r$type == "fill_rect"))
  # A first column in blue.
  expect_true("#0000FF" %in% r$color)
})

test_that("a talltblr has its caption above it, and what is not understood is said", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- paste0(r"(\begin{talltblr}[caption={Cars}]{colspec={Q[]Q[]}})", "\na & b \\\\\n",
                r"(\end{talltblr})")
  expect_identical(lines_of(tex)[1], "Cars")
  w <- warns(tblr("colspec={Q[]}, rowsep=3pt, cell{1}{1}={r=2}{}", r"(a \\ b \\)"))
  expect_match(w, "rowsep", all = FALSE)
  expect_match(w, "row span", all = FALSE)
})
