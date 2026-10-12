# physics.sty (inst/tex) is a package a label asks for: `\usepackage{physics}` or
# latex_options(packages = "physics"). Each command must draw as what it
# stands for, the spelled-out form.

layout <- function(tex) {
  t <- latex_tree(tex, input_mode = "math", render_mode = "path")
  list(t$records, t$bbox)
}

phys <- function(tex) layout(paste0("\\usepackage{physics}", tex))

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
    expect_identical(phys(input), layout(pairs[[input]]), info = input)
  }
}

test_that("physics reads on top of the format without a problem, and is not loaded until asked for", {
  expect_identical(tex_package_check_cpp("physics"), character(0))
  expect_true("physics" %in% gridmicrotex:::.bundled$packages)
  # Not one of the packages every label starts with.
  expect_match(warns("\\abs{x}"), "unknown command", all = FALSE)
})

test_that("physics' automatic brackets draw as the brackets they stand for", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_stands_for(c(
    "\\qty(\\frac ab)"      = "\\left(\\frac ab\\right)",
    "\\qty[x]"              = "\\left[x\\right]",
    "\\qty{x}"              = "\\left\\lbrace x\\right\\rbrace",
    "\\qty|x|"              = "\\left\\lvert x\\right\\rvert",
    "\\qty\\big(x)"         = "\\bigl(x\\bigr)",
    "\\qty\\Bigg[x]"        = "\\Biggl[x\\Biggr]",
    "\\pqty{x}"             = "\\left(x\\right)",
    "\\bqty{x}"             = "\\left[x\\right]",
    "\\Bqty{x}"             = "\\left\\lbrace x\\right\\rbrace",
    "\\vqty{x}"             = "\\left\\lvert x\\right\\rvert",
    "\\pqty\\bigg{x}"       = "\\biggl(x\\biggr)",
    "\\abs{x}"              = "\\left\\lvert x\\right\\rvert",
    "\\norm{x}"             = "\\left\\lVert x\\right\\rVert",
    "\\order{x^2}"          = "\\mathcal{O}\\mathclose{\\left(x^2\\right)}",
    "\\comm{A}{B}"          = "\\left[A,B\\right]",
    "\\acomm{A}{B}"         = "\\left\\lbrace A,B\\right\\rbrace",
    "\\pb{f}{g}"            = "\\left\\lbrace f,g\\right\\rbrace"
  ))
})

test_that("physics' vector notation, gradient and friends draw as what they stand for", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_stands_for(c(
    "\\vb{x}"               = "\\mathbf{x}",
    "\\vb*{x}"              = "\\boldsymbol{x}",
    "\\va{x}"               = "\\vec{\\mathbf{x}}",
    "\\vu{x}"               = "\\mathbf{\\hat{x}}",
    "a\\vdot b"             = "a\\boldsymbol\\cdot b",
    "a\\cross b"            = "a\\boldsymbol\\times b",
    "\\grad{f}"             = "\\boldsymbol\\nabla f",
    "\\div{F}"              = "\\boldsymbol\\nabla\\boldsymbol\\cdot F",
    "\\curl{F}"             = "\\boldsymbol\\nabla\\boldsymbol\\times F",
    "\\laplacian{f}"        = "\\nabla^2 f",
    "\\grad(f)"             = "\\boldsymbol\\nabla\\mathclose{\\left(f\\right)}"
  ))
})

test_that("physics' derivatives draw as the fractions and differentials they stand for", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_stands_for(c(
    "\\dd{x}"               = "\\mathinner{\\mathrm{d}x}",
    "\\dd"                  = "\\mathrm{d}",
    "\\dv{f}{x}"            = "\\frac{\\mathrm{d} f}{\\mathrm{d} x}",
    "\\dv[2]{f}{x}"         = "\\frac{\\mathrm{d}^{2} f}{\\mathrm{d} x^{2}}",
    "\\dv{x}"               = "\\frac{\\mathrm{d}}{\\mathrm{d} x}",
    "\\pdv{f}{x}"           = "\\frac{\\partial f}{\\partial x}",
    "\\pdv{f}{x}{y}"        = "\\frac{\\partial^2 f}{\\partial x\\partial y}",
    "\\var{F}"              = "\\mathinner{\\delta F}",
    "\\fdv{F}{g}"           = "\\frac{\\delta F}{\\delta g}"
  ))
})

test_that("physics' bra-ket notation draws as the brackets and bars it stands for", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_stands_for(c(
    "\\ket{a}"              = "\\left\\lvert a\\right\\rangle",
    "\\bra{a}"              = "\\left\\langle a\\right\\rvert",
    "\\braket{a}{b}"        = "\\left\\langle a\\middle\\vert b\\right\\rangle",
    "\\braket{a}"           = "\\left\\langle a\\middle\\vert a\\right\\rangle",
    "\\bra{a}\\ket{b}"      = "\\left\\langle a\\middle\\vert b\\right\\rangle",
    "\\op{a}{b}"            = "\\left\\lvert a\\middle\\rangle\\!\\middle\\langle b\\right\\rvert",
    "\\expval{A}"           = "\\left\\langle A\\right\\rangle",
    "\\expval{A}{\\psi}"    = "\\vphantom{A}\\left\\langle\\psi\\middle\\vert\\smash{A}\\middle\\vert\\psi\\right\\rangle",
    "\\mel{n}{H}{m}"        = "\\vphantom{H}\\left\\langle n\\middle\\vert\\smash{H}\\middle\\vert m\\right\\rangle",
    "\\vev{A}"              = "\\vphantom{A}\\left\\langle 0\\middle\\vert\\smash{A}\\middle\\vert 0\\right\\rangle"
  ))
})

test_that("physics' matrices, operators and quick text draw as what they stand for", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_stands_for(c(
    "\\mqty(a & b \\\\ c & d)" = "\\mathord{\\begin{pmatrix}a & b \\\\ c & d\\end{pmatrix}}",
    "\\mqty[a & b \\\\ c & d]" = "\\mathord{\\begin{bmatrix}a & b \\\\ c & d\\end{bmatrix}}",
    "\\mqty{a & b \\\\ c & d}" = "\\mathord{\\begin{matrix}a & b \\\\ c & d\\end{matrix}}",
    "\\mdet{a & b \\\\ c & d}" = "\\begin{vmatrix}a & b \\\\ c & d\\end{vmatrix}",
    "\\tr A"                = "\\operatorname{tr} A",
    "\\tr(A)"               = "\\operatorname{tr}\\mathclose{\\left(A\\right)}",
    "\\erf x"               = "\\operatorname{erf} x",
    "\\qq{and}"             = "\\quad\\text{and}\\quad",
    "\\qq*{and}"            = "\\text{and}\\quad",
    "x\\qc y"               = "x,\\quad y",
    "\\qif"                 = "\\quad\\text{if}\\quad"
  ))
})

test_that("loading physics takes over \\qty, \\bra and \\div from siunitx, braket and the division sign", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_false(identical(phys("\\qty{1}{m}"), layout("\\qty{1}{m}")))
  expect_false(identical(phys("a\\div b"), layout("a\\div b")))
  # What the package replaces is the division sign, still there as \divisionsymbol.
  expect_identical(phys("a\\divisionsymbol b"), layout("a\\div b"))
})

test_that("a name physics defines is no longer free to define once it is loaded", {
  expect_identical(warns("\\newcommand{\\abs}[1]{x}"), character(0))
  expect_match(warns("\\usepackage{physics}\\newcommand{\\abs}[1]{x}"), "already exists", all = FALSE)
})

test_that("latex_options(packages = 'physics') loads it before every label", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  old <- latex_options(packages = "physics")
  on.exit(do.call(latex_options, old), add = TRUE)
  expect_identical(layout("\\abs{x}"), layout("\\left\\lvert x\\right\\rvert"))
})
