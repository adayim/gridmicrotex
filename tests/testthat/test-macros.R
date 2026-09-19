
expand <- function(tex) as.character(gridmicrotex:::expand_latex_cpp(tex))

test_that("a define_macro() macro expands, through other macros, to what it stands for", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  on.exit(clear_macros(), add = TRUE)
  define_macro("RR", "\\mathbb{R}")
  define_macro("dom", "\\RR \\times \\RR")
  layout <- function(tex) {
    t <- latex_tree(tex, input_mode = "math", render_mode = "path")
    list(t$records, t$bbox)
  }
  expect_identical(layout("\\dom"), layout("\\mathbb{R} \\times \\mathbb{R}"))
})

test_that("circular define_macro() macros are an error, not a hang", {
  on.exit(clear_macros(), add = TRUE)
  define_macro("a", "\\b")
  define_macro("b", "\\a")
  expect_error(latex_dims("\\a", input_mode = "math"), "Too many macro expansions")
})

test_that("a define_macro() name is matched as TeX reads names", {
  on.exit(clear_macros(), add = TRUE)
  define_macro("RR", "\\mathbb{R}")
  # `\\RR` is a line break followed by the letters RR, and \RRx is another
  # name; the old regex expander rewrote the first of these.
  expect_equal(expand("a \\\\RR \\RR \\RRx"), "a \\\\RR \\mathbb{R} \\RRx")
})

test_that("\\renewcommand overrides a define_macro() macro for one label only", {
  on.exit(clear_macros(), add = TRUE)
  define_macro("RR", "\\mathbb{R}")
  expect_equal(expand("\\renewcommand{\\RR}{Q}\\RR"), "Q")
  expect_equal(expand("\\RR"), "\\mathbb{R}")
  expect_error(expand("\\newcommand{\\RR}{Q}"), "already exists")
})

test_that("redefining a define_macro() macro is not hidden by the layout cache", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  on.exit(clear_macros(), add = TRUE)
  width <- function() {
    grid::convertWidth(latex_dims("\\V", input_mode = "math")$width, "bigpts",
                       valueOnly = TRUE)
  }
  define_macro("V", "x")
  narrow <- width()
  define_macro("V", "xxxxxx")
  expect_gt(width(), 3 * narrow)
})

test_that("clear_macros() rejects a name that is not a single string", {
  on.exit(clear_macros(), add = TRUE)
  define_macro("RR", "\\mathbb{R}")
  # clear_macros(1) indexed the definitions by position and silently
  # dropped whichever macro came first.
  expect_error(clear_macros(1))
  expect_error(clear_macros(c("RR", "SS")))
  expect_equal(names(list_macros()), "RR")
})
