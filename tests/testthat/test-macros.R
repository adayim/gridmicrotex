
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
