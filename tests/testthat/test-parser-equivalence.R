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
    "\\mathbf 1"        = "\\mathbf{1}"
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
    "{\\bf a} {b}"                  = "{\\mathbf{a}} {b}"
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

test_that("text arguments keep their words, braces and escapes", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_same_layout(c(
    "\\text{a {b} c}" = "\\text{a b c}",
    "x^\\text{a b}"   = "x^{\\text{a b}}"
  ))
})
