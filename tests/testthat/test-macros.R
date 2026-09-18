
test_that(".expand_macros expands nested macros to a fixed point", {
  on.exit(clear_macros(), add = TRUE)
  define_macro("RR", "\\mathbb{R}")
  define_macro("dom", "\\RR \\times \\RR")
  expect_equal(.expand_macros("\\dom"), "\\mathbb{R} \\times \\mathbb{R}")
})

test_that(".expand_macros warns on circular definitions", {
  on.exit(clear_macros(), add = TRUE)
  define_macro("a", "\\b")
  define_macro("b", "\\a")
  expect_warning(.expand_macros("\\a"), "circular macro")
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
