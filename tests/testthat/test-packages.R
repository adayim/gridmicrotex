# The engine's own commands are LaTeX files (inst/tex): the format, which every
# input starts with, and packages, which it loads or a document asks for with
# \usepackage. The loader's mechanics are tested with the engine, in the
# MicroTeX fork (test/test_expander.cpp); these check the R side and what a
# label sees.

layout <- function(tex, mode = "math") {
  t <- latex_tree(tex, input_mode = mode, render_mode = "path")
  list(t$records, t$bbox)
}

warns <- function(tex, mode = "math") {
  w <- character(0)
  withCallingHandlers(
    latex_tree(tex, input_mode = mode),
    warning = function(x) {
      w <<- c(w, conditionMessage(x))
      invokeRestart("muffleWarning")
    })
  w
}

test_that("the format and the packages every input starts with read without a problem", {
  # A name defined twice, or a command the expander could not read.
  expect_identical(gridmicrotex:::.tex_prelude_problems(), character(0))
})

test_that("the files in inst/tex are all registered with the engine", {
  files <- sub("[.]sty$", "", list.files(system.file("tex", package = "gridmicrotex"),
                                         pattern = "[.]sty$"))
  expect_gt(length(files), 0L)
  expect_setequal(gridmicrotex:::tex_package_names_cpp(), files)
})

test_that("\\usepackage of a package we have or have not changes nothing a label draws", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(layout("\\usepackage{geometry}x"), layout("x"))
  expect_identical(layout("\\usepackage[utf8]{inputenc}\\usepackage{katex}x"), layout("x"))
  expect_identical(layout("\\usepackage{amsmath,nosuchpackage}\\dfrac{a}{b}"),
                   layout("\\dfrac{a}{b}"))
})

test_that("\\usepackage reads a registered package when it is asked for, and not before", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  gridmicrotex:::tex_package_register_cpp("rprobe", "\\newcommand{\\fromrprobe}{P}")
  expect_identical(layout("\\usepackage{rprobe}\\fromrprobe"), layout("P"))
  expect_identical(layout("\\RequirePackage{rprobe}\\fromrprobe"), layout("P"))
  expect_match(warns("\\fromrprobe"), "unknown command", all = FALSE)
})

test_that("a name from a package the input did not ask for is its own to define", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # LaTeX has no \dfrac without amsmath, no proof without amsthm, and KaTeX's
  # \R is no LaTeX at all.
  expect_identical(warns("\\newcommand{\\dfrac}[2]{Q}\\dfrac{a}{b}"), character(0))
  expect_identical(layout("\\newcommand{\\dfrac}[2]{Q}\\dfrac{a}{b}"), layout("Q"))
  expect_identical(warns("\\newenvironment{proof}{}{}"), character(0))
  expect_identical(warns("\\newcommand{\\R}{Q}\\R"), character(0))
  expect_identical(layout("\\newcommand{\\R}{Q}\\R"), layout("Q"))
})

test_that("a name is taken once the input asks for its package, and always when it is LaTeX's", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_match(warns("\\usepackage{amsmath}\\newcommand{\\dfrac}[2]{Q}"),
               "already exists", all = FALSE)
  expect_match(warns("\\usepackage{amsthm}\\newenvironment{proof}{}{}"),
               "already exists", all = FALSE)
  # A name of the format's own, which no package gave.
  expect_match(warns("\\newcommand{\\hfill}{Q}"), "already exists", all = FALSE)
  expect_match(warns("\\newcommand{\\ldots}{Q}"), "already exists", all = FALSE)
  expect_match(warns("\\newcommand{\\frac}{Q}"), "already exists", all = FALSE)
})
