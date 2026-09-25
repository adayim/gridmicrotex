# LaTeX's document commands, which a single grob has no use for, are read by
# the front end itself: dropped, or drawn as their nearest equivalent. Each
# input must draw exactly what its equivalent draws. (They were rewritten in
# R before the parse until 0.2.0.)

layout_of <- function(tex, mode = "mixed") {
  t <- latex_tree(tex, input_mode = mode, render_mode = "path")
  list(records = t$records, bbox = t$bbox)
}

expect_draws_as <- function(input, equivalent, mode = "mixed") {
  expect_identical(layout_of(input, mode), layout_of(equivalent, mode), info = input)
}

test_that("the preamble, title metadata and alignment declarations draw nothing", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_draws_as(paste0("\\documentclass[12pt]{article}\\usepackage[utf8]{inputenc}",
                         "\\usepackage{amsmath}\\begin{document}$x^2$\\end{document}"),
                  "$x^2$")
  expect_draws_as("\\maketitle\\title{Foo}\\author{Bar}\\label{eq:1}content", "content")
  expect_draws_as("\\bibliographystyle{plain}content", "content")
})

test_that("a whole file's preamble is read for its definitions and not drawn", {
  # LaTeX draws nothing before \begin{document}; a paper's preamble is full
  # of package settings a grob cannot honour, which must neither be drawn
  # nor warned about. Its definitions still hold, and what follows
  # \end{document} is ignored, as in LaTeX.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  file <- paste(
    "\\documentclass{article}",
    "\\PassOptionsToPackage{numbers, compress}{natbib}",
    "\\RequirePackage[final]{nips}\\hypersetup{colorlinks=true}",
    "stray preamble text \\unknowncommand{x}",
    "\\newcommand{\\R}{\\mathbb{R}}",
    "\\definecolor{brand}{RGB}{200,30,30}",
    "\\begin{document}",
    "Body $\\R$ in \\textcolor{brand}{colour}.",
    "\\end{document}",
    "Notes after the end.", sep = "\n")
  for (mode in c("document", "mixed")) {
    expect_silent(g <- latex_grob(file, input_mode = mode))
    body <- latex_grob(paste0("\\newcommand{\\R}{\\mathbb{R}}\\definecolor{brand}{RGB}{200,30,30}",
                              "Body $\\R$ in \\textcolor{brand}{colour}."),
                       input_mode = mode)
    expect_identical(g$layout_df$text, body$layout_df$text, info = mode)
    expect_identical(g$layout_df$color, body$layout_df$color, info = mode)
    # The preamble's \definecolor took effect (xcolor's RGB model, 0-255).
    expect_identical(g$layout_df$color[g$layout_df$text %in% "colour"], "#C81E1E", info = mode)
  }
  # xcolor's HTML model too.
  r <- latex_tree("\\definecolor{c}{HTML}{C81E1E}\\textcolor{c}{x}")$records
  expect_identical(r$color[r$text %in% "x"], "#C81E1E")
  # Without \begin{document} nothing is taken for a preamble.
  expect_warning(latex_grob("\\unknowncommand{x} text", input_mode = "document"),
                 "unknown command")
  expect_draws_as("\\graphicspath{{figs/}}\\DeclareGraphicsExtensions{.png}x", "x")
  expect_draws_as("% leading comment\n$x^2$", "$x^2$")
  for (cmd in c("centering", "raggedright", "raggedleft", "flushleft", "flushright",
                "noindent", "relax")) {
    expect_draws_as(paste0("\\", cmd, " x"), "x")
  }
})

test_that("nothing begun in the preamble reaches into the body", {
  # A declaration runs to the end of its group, an environment or a list to
  # its \end, \over takes the rest of its list, and a macro reads its
  # arguments wherever they are: begun in the preamble, none may take the
  # body into what is read only for its definitions.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  file <- function(pre) {
    paste0("\\documentclass{article}\n", pre,
           "\n\\begin{document}Hello \\emph{world}\\end{document}")
  }
  for (mode in c("document", "mixed")) {
    plain <- layout_of(file(""), mode)
    expect_identical(plain$records$text[plain$records$type == "text"], c("Hello ", "world"))
    for (pre in c("\\large", "\\color{red}", "\\bfseries", "\\begin{foo}", "a \\over b",
                  "\\begin{itemize}\\item x", "\\newcommand{\\two}[2]{#1#2}\\two{x}",
                  "\\def\\upto#1.{#1}\\upto abc", "\\title")) {
      expect_silent(got <- layout_of(file(pre), mode))
      expect_identical(got, plain, info = paste(mode, pre))
    }
  }
})

test_that("nothing after \\end{document} is read", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  body <- "\\documentclass{article}\n\\begin{document}Hello\\end{document}\n"
  plain <- layout_of(body, "document")
  # Not taken into the body (\over), not a capacity error (a runaway), not
  # warned about.
  for (post in c("a \\over b", "\\def\\a{\\a}\\a", "} { $ \\unknown", "\\begin{itemize}")) {
    expect_silent(got <- layout_of(paste0(body, post), "document"))
    expect_identical(got, plain, info = post)
  }
})

test_that("floats keep their contents", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_draws_as(paste("\\begin{table}[ht]", "\\centering",
                        "\\begin{tabular}{rr} a & b \\\\ \\end{tabular}", "\\end{table}",
                        sep = "\n"),
                  "\\begin{tabular}{rr} a & b \\\\ \\end{tabular}")
  expect_draws_as("\\begin{figure*}[ht]x\\end{figure*}", "x")
  expect_draws_as("\\begin{table*}[ht]x\\end{table*}", "x")
})

test_that("booktabs rules are drawn as the engine's rules", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_draws_as(
    "\\begin{tabular}{lr}\\toprule x & y \\\\ \\midrule 1 & 2 \\\\ \\bottomrule\\end{tabular}",
    "\\begin{tabular}{lr}\\thickhline x & y \\\\ \\hline 1 & 2 \\\\ \\thickhline\\end{tabular}")
  # \cmidrule keeps its column range; its width and trims have nothing to
  # act on.
  for (rule in c("\\cmidrule{2-3}", "\\cmidrule(lr){2-3}", "\\cmidrule[2pt]{2-3}",
                 "\\cmidrule[2pt](lr){2-3}")) {
    expect_draws_as(
      sprintf("\\begin{tabular}{lrr}x & y & z \\\\ %s 1 & 2 & 3\\end{tabular}", rule),
      "\\begin{tabular}{lrr}x & y & z \\\\ \\cline{2-3} 1 & 2 & 3\\end{tabular}")
  }
})

test_that("text commands a grob has no equivalent for are drawn as the nearest", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_draws_as("\\emph{stress}", "\\textit{stress}")
  expect_draws_as("\\emph{a $x_{i}$ b}", "\\textit{a $x_{i}$ b}")
  expect_draws_as("\\textnormal{plain}", "\\text{plain}")
  expect_draws_as("a\\par b", "a\\\\b")
  expect_draws_as("a\\newline b", "a\\\\b")
  # Fixed space: a grob has no glue to fill. A space after a command word
  # is dropped, as in TeX.
  expect_draws_as("a\\smallskip b", "a\\vspace{0.25em}b")
  expect_draws_as("a\\medskip b", "a\\vspace{0.5em}b")
  expect_draws_as("a\\bigskip b", "a\\vspace{1em}b")
  expect_draws_as("a\\vfill b", "a\\vspace{1em}b")
  expect_draws_as("a\\hfill b", "a\\quad b")
})

test_that("a caption is a line of text where it is written", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_draws_as("\\caption{Hello}", "\\text{Hello}\\\\")
  expect_draws_as("a \\caption{Hi} b", "a \\text{Hi}\\\\ b")
  expect_draws_as("\\caption[short]{Long}", "\\text{Long}\\\\")
  expect_draws_as("\\caption{Foo $x_{i}$}", "\\text{Foo $x_{i}$}\\\\")
})

test_that("links look like LaTeX's, and a URL is drawn as written", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  col <- gridmicrotex:::.MD_LINK_COLOR
  # hyperref with colorlinks colours a link and does not underline it; the
  # url package sets a URL in monospace.
  expect_draws_as("\\href{https://ex.org}{the text}",
                  sprintf("\\textcolor{%s}{the text}", col))
  expect_draws_as("\\href{u}{\\textbf{b}}", sprintf("\\textcolor{%s}{\\textbf{b}}", col))
  expect_draws_as("\\url{https://ex.org}",
                  sprintf("\\textcolor{%s}{\\texttt{https://ex.org}}", col))
  # A URL's specials are characters: `_` opens no subscript, `%` no comment,
  # `#` and `~` are themselves.
  expect_draws_as("\\url{a.io/x_y}", sprintf("\\textcolor{%s}{\\texttt{a.io/x\\_y}}", col))
  expect_draws_as("\\url{a.io/x%20y} tail",
                  sprintf("\\textcolor{%s}{\\texttt{a.io/x\\%%20y}} tail", col))
  expect_draws_as("\\url{a.io/~u#top}",
                  sprintf("\\textcolor{%s}{\\texttt{a.io/\\char126{}u\\#top}}", col))
  expect_draws_as("\\href{a.io/%7E#x}{t} tail", sprintf("\\textcolor{%s}{t} tail", col))
  expect_warning(latex_dims("\\href{only-one-group}"), "missing argument for \\\\href")
  # Nothing in a URL is a command: `\b` is a macro elsewhere.
  r <- latex_tree("\\url{a\\b{c}d}")$records
  expect_equal(r$text, "a\\b{c}d")
  # An \href's text is in the mode around it, and a line break in it ends
  # the line of a label with the rest still a link.
  expect_draws_as("$\\href{u}{x^2}$", sprintf("$\\textcolor{%s}{x^2}$", col))
  r <- latex_tree("\\href{u}{a\nb}")$records
  expect_equal(r$color, c(col, col))
  expect_length(unique(r$y), 2)
})

test_that("a starred environment is its plain form", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_draws_as("\\begin{align*}a&=b\\end{align*}", "\\begin{align}a&=b\\end{align}")
  expect_draws_as("\\begin{alignat*}{1}a&=b\\end{alignat*}",
                  "\\begin{alignat}{1}a&=b\\end{alignat}")
  expect_draws_as("\\begin{equation*}x\\end{equation*}", "\\begin{equation}x\\end{equation}")
  # tabular* takes a width before its column spec; a grob is as wide as
  # its content.
  expect_draws_as("\\begin{tabular*}{\\textwidth}{lcr}a&b&c\\end{tabular*}",
                  "\\begin{tabular}{lcr}a&b&c\\end{tabular}")
})
