# The small packages every label starts with (bbm, nicefrac, centernot,
# mleftright, esdiff, relsize, and \ohm and \textmu): each command draws as the
# spelled-out form it stands for.

layout <- function(tex) {
  t <- latex_tree(tex, input_mode = "math", render_mode = "path")
  list(t$records, t$bbox)
}

warns <- function(tex) {
  w <- character(0)
  withCallingHandlers(
    latex_tree(tex, input_mode = "math"),
    warning = function(x) {
      w <<- c(w, conditionMessage(x))
      invokeRestart("muffleWarning")
    })
  w
}

expect_stands_for <- function(pairs) {
  for (input in names(pairs)) {
    expect_identical(layout(input), layout(pairs[[input]]), info = input)
  }
}

# esdiff sets a derivative in display size wherever it stands.
chosen <- function(x) {
  sprintf("\\mathchoice{%s}{\\displaystyle{%s}}{\\scriptstyle{%s}}{\\scriptstyle{%s}}", x, x, x, x)
}

test_that("bbm, nicefrac, centernot and mleftright draw as what they stand for", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_stands_for(c(
    "\\mathbbm{1}"                 = "\\mathbb{1}",
    "\\nicefrac{1}{2}"             = "\\sfrac{1}{2}",
    "\\nicefrac[\\mathbf]{1}{2}"   = "\\sfrac{1}{2}",
    "a \\centernot\\implies b"     = "a \\not\\implies b",
    "\\mleft( x \\mright)"         = "\\left( x \\right)"
  ))
})

test_that("esdiff's derivatives draw as the fractions they stand for", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_stands_for(c(
    "\\diff{f}{x}"      = chosen("\\frac{\\mathrm{d} f}{\\mathrm{d} x}"),
    "\\diff[2]{f}{x}"   = chosen("\\frac{\\mathrm{d}^{2} f}{\\mathrm{d} x^{2}}"),
    "\\diff*{f}{x}{a}"  = chosen("\\left(\\frac{\\mathrm{d} f}{\\mathrm{d} x}\\right)_{\\mkern-7mu a}"),
    "\\diffp{f}{x}"     = chosen("\\frac{\\partial f}{\\partial x}"),
    "\\diffp[2]{f}{x}"  = chosen("\\frac{\\partial^{2} f}{\\partial x^{2}}"),
    "\\diffp{f}{xy}"    = chosen("\\frac{\\partial^{2} f}{\\partial x\\partial y}"),
    "\\diffp{f}{xyz}"   = chosen("\\frac{\\partial^{3} f}{\\partial x\\partial y\\partial z}"),
    "\\diffp*{f}{x}{a}" = chosen("\\left(\\frac{\\partial f}{\\partial x}\\right)_{\\mkern-7mu a}")
  ))
  # A group after a derivative is not taken for its argument.
  expect_identical(layout("\\diff{f}{x}{y}"), layout(paste0(chosen("\\frac{\\mathrm{d} f}{\\mathrm{d} x}"), "{y}")))
})

test_that("relsize sets the rest of the group a step larger or smaller", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_stands_for(c(
    "a{\\larger b}"        = "a{\\relscale{1.2} b}",
    "a{\\smaller[2] b}"    = "a{\\relscale{0.69444} b}",
    "a{\\relsize{3} b}"    = "a{\\relscale{1.728} b}",
    "a{\\relsize{-1} b}"   = "a{\\relscale{0.83333} b}",
    "a\\textlarger{b}"     = "a{\\relscale{1.2} b}"
  ))
})

test_that("\\ohm and \\textmu are the symbols they name", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_stands_for(c("\\ohm" = "\\Omega", "\\textmu" = "µ"))
})

test_that("a name from these packages is free to define until the label asks for the package", {
  expect_identical(warns("\\newcommand{\\diff}{x}\\diff"), character(0))
  expect_identical(warns("\\newcommand{\\nicefrac}[2]{x}"), character(0))
  expect_match(warns("\\usepackage{esdiff}\\newcommand{\\diff}{x}"), "already exists", all = FALSE)
})
