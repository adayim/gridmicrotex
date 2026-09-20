# The parser of the new front end, checked through its syntax tree.
#
# `tree()` writes a tree compactly:
#   [a b]      a list          {a b}   a group        \cmd(x y)  a command
#   <a b>      a parsed arg    'text'  a kept-as-text argument   -  an absent one
#   S<order>(base;sub;sup)     scripts, with the order its parts came in
#   \over:(num;den)            an infix command
#   \bf{args|body}             a declaration
#   LR(left;body;...;right)    \left ... \right
#   env:name(args rows)        row:end(cells)   $(...) / $$(...)  math in text
#   ~ a tie, _ a space

ast <- function(tex, mode = "math") gridmicrotex:::parse_ast_cpp(tex, mode)

tree <- function(tex, mode = "math") {
  d <- ast(tex, mode)
  kids <- split(seq_len(nrow(d)), factor(d$parent, levels = d$id))
  render <- function(i) {
    ch <- kids[[as.character(d$id[i])]]
    sub <- vapply(ch, render, "")
    inner <- function(j) {  # a list's items, without its brackets
      k <- kids[[as.character(d$id[j])]]
      paste(vapply(k, render, ""), collapse = " ")
    }
    switch(d$kind[i],
      list = paste0("[", paste(sub, collapse = " "), "]"),
      group = paste0("{", inner(ch[1]), "}"),
      char = d$text[i],
      space = c("_", "~")[d$aux[i] + 1L],
      command = paste0("\\", d$text[i], if (d$flag[i]) "?",
                       if (length(sub)) paste0("(", paste(sub, collapse = " "), ")")),
      argument = if (!d$flag[i]) "-" else if (length(ch)) paste0("<", inner(ch[1]), ">")
                 else paste0("'", d$raw[i], "'"),
      scripts = paste0("S", d$text[i], "(", paste(sub, collapse = ";"), ")"),
      infix = paste0("\\", d$text[i], ":(", paste(sub, collapse = ";"), ")"),
      declaration = paste0("\\", d$text[i], "{",
                           paste(sub[-length(sub)], collapse = " "), "|",
                           sub[length(sub)], "}"),
      leftright = paste0("LR(", paste(sub, collapse = ";"), ")"),
      environment = paste0("env:", d$text[i], "(", paste(sub, collapse = " "), ")"),
      row = paste0("row:", d$text[i], "(", paste(sub, collapse = " "), ")"),
      cell = sub,
      math = paste0(if (d$flag[i]) "$$(" else "$(", inner(ch[1]), ")"),
      paste0("?", d$kind[i]))
  }
  render(1L)
}

diags <- function(tex, mode = "math") attr(ast(tex, mode), "diagnostics")

test_that("the command table and the prelude cover every command the engine defines", {
  t <- gridmicrotex:::command_tables_cpp()
  # Definition commands are the expander's; `name@env` / `name@@env` are the
  # old parser's internal environment builders.
  expander <- c("newcommand", "renewcommand", "providecommand", "def",
                "newenvironment", "renewenvironment", "DeclareMathOperator")
  engine <- t$engine_commands[!grepl("@@?env$", t$engine_commands)]
  expect_identical(sort(setdiff(engine, c(t$spec_commands, t$prelude_commands, expander))),
                   character(0))
  # Every environment the old parser knew is in the spec or the prelude.
  envs <- sub("@@?env$", "", grep("@@?env$", t$engine_commands, value = TRUE))
  expect_identical(sort(setdiff(envs, c(t$spec_environments, t$prelude_environments))),
                   character(0))
  # Read by the parser, not registered as commands in the old one.
  expect_identical(sort(setdiff(t$spec_commands, t$engine_commands)),
                   c("begin", "cite", "cmidrule", "end", "ensuremath", "eqref", "footnote",
                     "graphicspath", "href", "noindent", "pageref", "par", "paragraph", "ref",
                     "right", "section", "subsection", "subsubsection", "url"))
})

test_that("the math environments R scans for come from the C++ tables", {
  # The list that used to be kept by hand, in step with macro_def.cpp.
  old <- c("array", "tabular", "tabular*", "matrix", "smallmatrix", "pmatrix",
           "bmatrix", "Bmatrix", "vmatrix", "Vmatrix", "equation", "equation*",
           "math", "displaymath", "align", "align*", "flalign", "flalign*",
           "alignat", "alignat*", "aligned", "alignedat", "alignedat*",
           "eqnarray", "eqnarray*", "multline", "multline*", "gather", "gather*",
           "gathered", "split", "cases", "rcases", "itemize", "enumerate")
  expect_identical(setdiff(old, gridmicrotex:::.math_envs()), character(0))
  # But an environment that only wraps content is not math: its body is
  # prose. R masks a math span from CommonMark, so counting `document` as
  # one hid a whole markdown document from the parser that reads it.
  expect_identical(
    intersect(gridmicrotex:::.math_envs(),
              c("document", "table", "table*", "figure", "figure*")),
    character(0))
})

test_that("an argument without braces is one character, or one command with its arguments", {
  expect_identical(tree("\\frac12"), "[\\frac(<1> <2>)]")
  expect_identical(tree("\\frac \u03b1\u03b2"), "[\\frac(<\u03b1> <\u03b2>)]")
  expect_identical(tree("\\sqrt\\frac12"), "[\\sqrt(- <\\frac(<1> <2>)>)]")
  # the recorded source is what the old command handlers received
  a <- ast("\\frac 1 {x^2}")
  expect_identical(a$raw[a$kind == "argument"], c("1", "x^2"))
})

test_that("optional arguments nest brackets and keep their source", {
  expect_identical(tree("\\sqrt[3]{x}"), "[\\sqrt(<3> <x>)]")
  a <- ast("\\sqrt[\\left[x\\right]]{y}")
  expect_identical(a$raw[a$kind == "argument"][1], "\\left[x\\right]")
})

test_that("scripts attach to the item before them and remember their order", {
  expect_identical(tree("x^2"), "[S^(x;[];[2])]")
  expect_identical(tree("^2"), "[S^([];[];[2])]")
  expect_identical(tree("x^\\frac12"), "[S^(x;[];[\\frac(<1> <2>)])]")
  expect_identical(tree("f_1'"), "[S_'(f;[1];[])]")
  expect_identical(tree("f'_1"), "[S'_(f;[1];[])]")
  expect_identical(ast("f''")$aux[2], 2L)
  # a double superscript reads as {a^b}^c, with a warning
  expect_identical(tree("a^b^c"), "[S^(S^(a;[];[b]);[];[c])]")
  expect_match(diags("a^b^c")$message, "double superscript")
})

test_that("an infix command divides its list", {
  expect_identical(tree("a \\over b + c"), "[\\over:([a];[b + c])]")
  expect_identical(tree("{a \\over b} + c"), "[{\\over:([a];[b])} + c]")
  expect_identical(tree("\\frac{a}{b} \\over c"), "[\\over:([\\frac(<a> <b>)];[c])]")
})

test_that("\\bf stops at \\\\ and \\color runs to the end of the group", {
  expect_identical(tree("\\bf a \\\\ b"), "[\\bf{|[a]} \\\\(-) b]")
  expect_identical(tree("\\color{red} a \\\\ b"), "[\\color{'red'|[a \\\\(-) b]}]")
  expect_identical(tree("{\\bf a} b"), "[{\\bf{|[a]}} b]")
})

test_that("\\left, \\middle and \\right delimit a group", {
  expect_identical(tree("\\left( x \\middle| y \\right)"), "[LR('(';[x];'|';[y];')')]")
  expect_match(diags("\\left( x")$message, "missing \\\\right")
  expect_match(diags("x \\right)")$message, "\\\\right without \\\\left")
})

test_that("an alignment is rows of cells; a rule ends its row", {
  expect_identical(tree("\\begin{matrix} a & b \\\\ c & d \\end{matrix}"),
                   "[env:matrix(row:\\([a] [b]) row:([c] [d]))]")
  expect_identical(tree("\\begin{array}{cc} \\hline a & b \\end{array}"),
                   "[env:array('cc' row:hline([\\hline]) row:([a] [b]))]")
  a <- ast("\\begin{aligned} a \\\\[4pt] b \\end{aligned}")
  expect_identical(a$raw[a$kind == "row"], c("4pt", ""))
  a <- ast("\\begin{matrix} a & b \\end{matrix}")
  expect_identical(a$raw[a$kind == "environment"], " a & b ")
})

test_that("the prelude defines the engine's LaTeX-written environments and commands", {
  expect_identical(tree("\\begin{pmatrix}a\\end{pmatrix}"),
                   "[{LR('(';[env:matrix(row:([a]))];')')}]")
  expect_identical(tree("\\dfrac12"), "[\\genfrac('' '' '1' '' <1> <2>)]")
})

test_that("itemize keeps its body as text for its builder", {
  a <- ast("\\begin{itemize} \\item a \\begin{itemize}\\item b\\end{itemize} \\end{itemize}")
  env <- a[a$kind == "environment", ]
  expect_identical(env$raw, " \\item a \\begin{itemize}\\item b\\end{itemize} ")
})

test_that("modes switch at \\text and at $", {
  expect_identical(tree("\\text{a $b$ c}"), "[\\text(<a _ $(b) _ c>)]")
  a <- ast("\\text{a $b$}")
  expect_identical(unique(a$mode[a$kind == "char"]), c("text", "math"))
})

test_that("a value TeX reads without braces is kept as text", {
  expect_identical(tree("\\kern-.4ex x"), "[\\kern('-.4ex') x]")
  expect_identical(tree("\\char\"41"), "[\\char('\"41')]")
})

test_that("a file name keeps %, # and _ as characters", {
  a <- ast("\\includegraphics[width=1in]{a%b_c#1.png}")
  # The second option group is the older [llx,lly][urx,ury] spelling.
  expect_identical(a$raw[a$kind == "argument"], c("width=1in", "", "a%b_c#1.png"))
})

test_that("text drops the space after a control word, as TeX does", {
  expect_identical(tree("\\text{\\alpha b}", "math"), "[\\text(<\\alpha b>)]")
  # A control symbol keeps it, and so does a word ended by a group.
  expect_identical(tree("\\text{\\% b}", "math"), "[\\text(<\\% _ b>)]")
  expect_identical(tree("\\text{\\LaTeX{} b}", "math"), "[\\text(<\\LaTeX {} _ b>)]")
})

test_that("problems are reported where they are, and parsing goes on", {
  d <- diags("\\foo x")
  expect_identical(d$message, "unknown command \\foo: drawn as its name")
  expect_identical(tree("\\foo x"), "[\\foo? x]")
  d <- diags("x\n\\frac{1}{2")
  expect_identical(d$message, "missing } inserted")
  expect_identical(c(d$line, d$col), c(2L, 9L))
  expect_identical(tree("\\frac{1}{2"), "[\\frac(<1> <2>)]")
  expect_match(diags("a } b")$message, "extra \\} ignored")
  expect_match(diags("a & b")$message, "& outside an alignment")
})

test_that("a scripts node keeps the position of its own operator", {
  # The span was taken by reference from a peeked token, which the first
  # read inside the scripts pops; a later put-back refilled that slot, so
  # the node ended up with some other token's position.
  s <- function(tex) { d <- ast(tex); d[d$kind == "scripts", c("line", "col")] }
  expect_identical(unlist(s("x^2"), use.names = FALSE), c(1L, 2L))
  expect_identical(unlist(s("f'"), use.names = FALSE), c(1L, 2L))
  expect_identical(unlist(s("ab\ncd_{x}"), use.names = FALSE), c(2L, 3L))
})

test_that("the tree is a tree: every node has one parent", {
  for (tex in c("^2", "_1^2", "x'", "\\limits", "a \\over b", "\\sum\\limits_i^n x_i",
                "\\left( x", "\\begin{matrix} a & b \\\\ \\hline c \\end{matrix}",
                "\\sqrt[3]{x}^2", "\\text{a $b^c$}", "\\bf a \\\\ b \\color{red} c",
                "\\frac{1}{2", "a^b^c_d_e", "\\begin{pmatrix}1\\end{pmatrix}'")) {
    d <- ast(tex)
    expect_false(anyDuplicated(d$id) > 0, info = tex)
  }
})

test_that("deep nesting is an error, not a crash", {
  deep <- paste0(strrep("{", 2000), "x", strrep("}", 2000))
  expect_true(any(grepl("nested too deeply", diags(deep)$message)))
  expect_identical(nrow(diags(paste0(strrep("{", 50), "x", strrep("}", 50)))), 0L)
})
