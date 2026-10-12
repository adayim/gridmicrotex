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
  expect_setequal(gridmicrotex:::.bundled$packages, files)
  expect_true(all(files %in% gridmicrotex:::tex_package_names_cpp()))
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

# --- load_sty() and the `packages` option ---------------------------------

sty_file <- function(lines, name = "probe.sty") {
  dir <- tempfile("sty")
  dir.create(dir)
  path <- file.path(dir, name)
  writeLines(lines, path)
  path
}

test_that("load_sty() makes a file a package that \\usepackage loads, named by its file", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  path <- sty_file(c("\\newcommand{\\fromuser}{U}",
                     "\\newcommand{\\my@a}{A}", "\\newcommand{\\usea}{\\my@a}"),
                   "userprobe.sty")
  expect_identical(load_sty(path), "userprobe")
  expect_identical(layout("\\usepackage{userprobe}\\fromuser"), layout("U"))
  expect_identical(layout("\\usepackage{userprobe}\\usea"), layout("A"))
  # Under another name if asked, and not before it is asked for.
  expect_identical(load_sty(path, "userother"), "userother")
  expect_identical(layout("\\usepackage{userother}\\fromuser"), layout("U"))
  expect_false(identical(suppressWarnings(layout("\\fromuser")), layout("U")))
})

test_that("load_sty() refuses a missing file, a bad name and the name of a package we ship", {
  path <- sty_file("\\newcommand{\\x}{1}")
  expect_error(load_sty(file.path(tempdir(), "no-such-file.sty")), "existing")
  expect_error(load_sty(path, "amsmath"), "comes with gridmicrotex")
  expect_error(load_sty(path, "two words"), "package name")
  expect_error(load_sty(path, c("a", "b")), "package name")
})

test_that("load_sty() reports a problem in the file at its line and column", {
  path <- sty_file(c("% my macros", "", "\\newcommand{\\frac}{x}", "\\newcommand{\\okprobe}{1}"),
                   "badpkg.sty")
  expect_warning(load_sty(path), "3:1: .newcommand: Command frac already exists")
  # The rest of the file still loads, and a label that loads it is told.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  without_diagnostics <- function(x) {
    attr(x[[1]], "diagnostics") <- NULL
    x
  }
  expect_identical(
    without_diagnostics(suppressWarnings(layout("\\usepackage{badpkg}\\okprobe"))),
    without_diagnostics(layout("1")))
  expect_match(warns("\\usepackage{badpkg}\\okprobe"), "package badpkg line 3:1:", all = FALSE)
})

test_that("a package file edited after load_sty() is read again when something is drawn", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  path <- sty_file("\\newcommand{\\fromedit}{1}", "editprobe.sty")
  load_sty(path)
  expect_identical(layout("\\usepackage{editprobe}\\fromedit"), layout("1"))
  writeLines("\\newcommand{\\fromedit}{22}", path)
  expect_identical(layout("\\usepackage{editprobe}\\fromedit"), layout("22"))
})

test_that("latex_options(packages =) loads packages before every label, and resets", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  path <- sty_file("\\newcommand{\\fromopt}{O}", "optprobe.sty")
  without <- suppressWarnings(layout("\\fromopt"))
  old <- latex_options(packages = path)
  on.exit(do.call(latex_options, old), add = TRUE)
  # A file is loaded, and what is recorded is the package's name.
  expect_identical(latex_options()$packages, "optprobe")
  expect_identical(layout("\\fromopt"), layout("O"))
  latex_options(packages = NULL)
  expect_identical(suppressWarnings(layout("\\fromopt")), without)
  latex_options(packages = "optprobe")
  expect_identical(layout("\\fromopt"), layout("O"))
  reset_latex_options()
  expect_null(latex_options()$packages)
})

test_that("latex_options(packages =) rejects a name that is no package and changes nothing", {
  old <- latex_options(packages = "amsmath")
  on.exit(do.call(latex_options, old), add = TRUE)
  expect_error(latex_options(packages = "nosuchpackage"), "no package")
  expect_error(latex_options(packages = NA_character_), "package names")
  expect_identical(latex_options()$packages, "amsmath")
})

test_that("a package written with conditionals, \\csname and options works as written", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  path <- sty_file(c(
    "\\NeedsTeXFormat{LaTeX2e}",
    "\\ProvidesPackage{condprobe}[2026/10/11 a probe]",
    "\\newif\\ifcompact",
    "\\DeclareOption{compact}{\\compacttrue}",
    "\\ProcessOptions\\relax",
    "\\newcommand{\\gap}{\\ifcompact\\,\\else\\quad\\fi}",
    "\\@namedef{unit@m}{\\mathrm{m}}",
    "\\newcommand{\\unit}[1]{\\@ifundefined{unit@#1}{\\mathrm{#1}}{\\@nameuse{unit@#1}}}"),
    "condprobe.sty")
  expect_silent(load_sty(path))
  expect_identical(layout("\\usepackage{condprobe}a\\gap b"), layout("a\\quad b"))
  expect_identical(layout("\\usepackage[compact]{condprobe}a\\gap b"), layout("a\\,b"))
  expect_identical(layout("\\usepackage{condprobe}\\unit{m}"), layout("\\mathrm{m}"))
  expect_identical(layout("\\usepackage{condprobe}\\unit{kg}"), layout("\\mathrm{kg}"))
})

test_that("a name a start package defines is no longer free to define", {
  path <- sty_file("\\newcommand{\\takenprobe}{T}", "takenprobe.sty")
  load_sty(path)
  old <- latex_options(packages = "takenprobe")
  on.exit(do.call(latex_options, old), add = TRUE)
  expect_match(warns("\\newcommand{\\takenprobe}{Q}"), "already exists", all = FALSE)
})

test_that("what a package does besides define is done: \\definecolor in a start package", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  path <- sty_file("\\definecolor{brandprobe}{rgb}{0,1,0}", "colorprobe.sty")
  load_sty(path)
  green <- function(tex) {
    "#00FF00" %in% toupper(latex_tree(tex, input_mode = "math", render_mode = "path")$records$color)
  }
  expect_true(green("\\usepackage{colorprobe}\\textcolor{brandprobe}{a}"))
  old <- latex_options(packages = "colorprobe")
  on.exit(do.call(latex_options, old), add = TRUE)
  expect_true(green("\\textcolor{brandprobe}{a}"))
})
