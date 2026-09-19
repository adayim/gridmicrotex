# Each input must draw exactly what its spelled-out form draws: the same
# records, positions and bounding box. This is how the parser is checked
# against LaTeX's own reading of an input, rather than against a snapshot of
# whatever it drew before.

layout_of <- function(tex) {
  t <- latex_tree(tex, input_mode = "math", render_mode = "path")
  list(records = t$records, bbox = t$bbox)
}

expect_same_layout <- function(pairs) {
  for (input in names(pairs)) {
    expect_identical(layout_of(input), layout_of(pairs[[input]]), info = input)
  }
}

test_that("an argument without braces is one token", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_same_layout(c(
    "\\frac12"          = "\\frac{1}{2}",
    "\\frac\\alpha\\beta" = "\\frac{\\alpha}{\\beta}",
    "\\frac 1 {x^2}"    = "\\frac{1}{x^2}",
    "x^\\frac12"        = "x^{\\frac{1}{2}}",
    "\\sqrt[n]x"        = "\\sqrt[n]{x}",
    "\\mathbb R^n"      = "\\mathbb{R}^n",
    "\\vec x_1"         = "\\vec{x}_1",
    "\\sqrt\\frac12"    = "\\sqrt{\\frac{1}{2}}",
    "\\binom nk"        = "\\binom{n}{k}",
    "\\textcolor{red}a" = "\\textcolor{red}{a}",
    "\\tfrac12"         = "\\tfrac{1}{2}",
    "\\mathbf 1"        = "\\mathbf{1}",
    # One character, not one byte of it.
    "\\frac αβ"         = "\\frac{α}{β}",
    "\\sqrt α"          = "\\sqrt{α}",
    "\\hat é"           = "\\hat{é}",
    "\\overline ω"      = "\\overline{ω}",
    "\\mathrm é"        = "\\mathrm{é}"
  ))
})

test_that("an infix command takes its whole group", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_same_layout(c(
    "{a \\over b} + {c \\over d}" = "{\\frac{a}{b}} + {\\frac{c}{d}}",
    "\\frac{a}{b} \\over c"       = "\\frac{\\frac{a}{b}}{c}",
    "x \\over y + z"              = "\\frac{x}{y + z}"
  ))
})

test_that("a declaration runs to the end of its group", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_same_layout(c(
    "{\\color{red} a} + b"          = "\\textcolor{red}{a} + b",
    "{a {\\color{red} b} c} d"      = "{a {\\textcolor{red}{b}} c} d",
    "\\mathbf{a\\color{red}b}c"     = "\\mathbf{a\\textcolor{red}{b}}c",
    "{\\bf a} b"                    = "{\\mathbf{a}} b",
    "{\\bf a} {b}"                  = "{\\mathbf{a}} {b}",
    # A group after a declaration's group is a group of its own.
    "{\\color{red} a} + {b}"        = "\\textcolor{red}{a} + {b}",
    "{\\color{blue} a} {\\color{red} b}" = "\\textcolor{blue}{a} \\textcolor{red}{b}",
    "{a \\\\ b} {c}"                = "{a \\\\ b}{c}",
    "\\bf a \\\\ b \\color{red} c"  = "\\bf a \\\\ b {\\color{red} c}"
  ))
})

test_that("\\middle takes a delimiter by name, as \\left and \\right do", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # It was handed the text "\vert" and failed at layout: only a single
  # character worked.
  expect_same_layout(c(
    "\\left\\{ x \\middle\\vert x > 0 \\right\\}" = "\\left\\{ x \\middle| x > 0 \\right\\}",
    "\\left\\langle a \\middle\\vert b \\right\\rangle" = "\\left\\langle a \\middle| b \\right\\rangle"
  ))
})

test_that("\\cal and \\frak switch the math alphabet for the rest of the group", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # TeX's old declarations; their argument was lost.
  expect_same_layout(c(
    "{\\cal x} y"          = "\\mathcal{x} y",
    "{\\frak x} y"         = "\\mathfrak{x} y",
    "\\oldstylenums{123}"  = "\\text{123}"
  ))
})

test_that("\\| is \\Vert, as in LaTeX", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # The engine's symbol table had it as a single bar.
  expect_same_layout(c(
    "\\|x\\|"                  = "\\Vert x\\Vert",
    "\\left\\| x \\right\\|"   = "\\left\\Vert x \\right\\Vert"
  ))
})

test_that("a prime is the superscript \\prime, as in TeX", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_same_layout(c(
    "f'(x)"     = "f^{\\prime}(x)",
    "f''"       = "f^{\\prime\\prime}",
    "f'^2"      = "f^{\\prime 2}",
    "f'^{ab}"   = "f^{\\prime ab}",
    "f'_1"      = "f^{\\prime}_1",
    "f_1'"      = "f_1^{\\prime}"
  ))
})

test_that("scripts attach the same in either order", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_same_layout(c(
    "a^{b}_{c}"      = "a_{c}^{b}",
    "\\sum_{i=1}^{n}" = "\\sum^{n}_{i=1}"
  ))
})

test_that("a macro expands to what it stands for", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_same_layout(c(
    "\\newcommand{\\half}{\\frac12} x^\\half"            = "x^{\\frac{1}{2}}",
    "\\newcommand{\\sq}[1]{#1^2} \\sq{y} + \\sq z"       = "y^2 + z^2",
    "\\newcommand{\\p}[2][x]{#1_#2} \\p{1} + \\p[y]{2}"  = "x_1 + y_2",
    "\\newcommand{\\twice}[1]{#1#1} \\twice{\\alpha}"    = "\\alpha\\alpha",
    "\\newcommand{\\bmat}{\\begin{bmatrix}} \\bmat a & b \\end{bmatrix}" =
      "\\begin{bmatrix} a & b \\end{bmatrix}",
    "\\def\\foo#1#2{#2#1} \\foo ab"                     = "ba"
  ))
})

test_that("a starred command is the command's starred form, not a `*` argument", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_same_layout(c(
    "\\operatorname*{argmax}_x f" = "\\mathop{\\mathrm{argmax}}\\limits_x f",
    "\\newcommand*{\\f}{x}\\f"     = "x",
    "\\hspace*{1em}a"             = "\\hspace{1em}a",
    "\\DeclareMathOperator*{\\am}{am}\\am_x f" = "\\mathop{\\mathrm{am}}\\limits_x f",
    "\\begin{aligned} a \\\\* b \\end{aligned}" = "\\begin{aligned} a \\\\ b \\end{aligned}"
  ))
})

test_that("macro arguments are substituted once and may be delimited", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_same_layout(c(
    "\\newcommand{\\A}[2]{#1,#2} \\A{\\#2}{z}" = "\\#2,z",
    "\\def\\foo#1.{[#1]} \\foo abc."           = "[abc]",
    "\\newcommand{\\sq}[1]{#1^2}\\sq α"   = "α^2",
    "\\let\\oldfrac\\frac\\renewcommand{\\frac}[2]{\\oldfrac{#2}{#1}}\\frac{a}{b}" =
      "\\frac{b}{a}"
  ))
})

test_that("text arguments keep their words, braces and escapes", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_same_layout(c(
    "\\text{a {b} c}" = "\\text{a b c}",
    "x^\\text{a b}"   = "x^{\\text{a b}}"
  ))
})
