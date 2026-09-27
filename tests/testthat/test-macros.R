# How the expander matches and scopes these macros is tested with the engine,
# in the MicroTeX fork (test/test_expander.cpp); these check the R side.

layout <- function(tex) {
  t <- latex_tree(tex, input_mode = "math", render_mode = "path")
  list(t$records, t$bbox)
}

test_that("a define_macro() macro expands, through other macros, to what it stands for", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  on.exit(clear_macros(), add = TRUE)
  define_macro("RR", "\\mathbb{R}")
  define_macro("dom", "\\RR \\times \\RR")
  expect_identical(layout("\\dom"), layout("\\mathbb{R} \\times \\mathbb{R}"))
})

test_that("circular define_macro() macros are an error, not a hang", {
  on.exit(clear_macros(), add = TRUE)
  define_macro("a", "\\b")
  define_macro("b", "\\a")
  expect_error(latex_dims("\\a", input_mode = "math"), "Too many macro expansions")
})

test_that("\\renewcommand overrides a define_macro() macro for one label only", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  on.exit(clear_macros(), add = TRUE)
  define_macro("RR", "\\mathbb{R}")
  expect_identical(layout("\\renewcommand{\\RR}{Q}\\RR"), layout("Q"))
  expect_identical(layout("\\RR"), layout("\\mathbb{R}"))
  expect_warning(latex_dims("\\newcommand{\\RR}{Q}", input_mode = "math"), "already exists")
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

test_that("define_macro() does not reach the engine's own formulas, as in 0.1.1", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  on.exit(clear_macros(), add = TRUE)
  # \sec was read with the macros defined at its first use and kept so for
  # the session.
  glyphs <- function(tex) latex_tree(tex, input_mode = "math", render_mode = "path")$records$glyph
  plain <- glyphs("\\sec x")
  define_macro("mathrm", "\\mathsf")
  expect_identical(glyphs("\\sec x"), plain)
})

test_that("\\limits on a predefined formula changes that use of it only", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  size <- function(tex) latex_tree(tex, input_mode = "math")$bbox[c("height", "depth")]
  before <- size("\\displaystyle \\sin_{q} x")
  invisible(size("\\displaystyle \\sin\\limits_{q} y"))
  expect_identical(size("\\displaystyle \\sin_{q} z"), before)
})
