# Named "zz" so testthat runs it last: it tears MicroTeX down and lets it
# re-initialise, which is not something the other files should inherit
# halfway through.

test_that("a release / re-init cycle leaves the macro registry usable", {
  # MicroTeX::release() is MacroInfo::_free_() + NewCommandMacro::_free_().
  # _free_() deletes every value in the static _commands map but never
  # erases it, so afterwards every built-in macro is a dangling pointer and
  # the next MacroInfo::add() double-frees one. That corrupted the heap on
  # every unload/reload, and a later large formula then failed with
  # "vector::_M_default_append" or took the process down outright.
  # microtex_release() no longer calls it -- see src/init.cpp.
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  big <- paste0(rep("\\text{x}", 254), collapse = "")
  before <- nrow(latex_grob(big, input_mode = "math")$layout_df)
  expect_gt(before, 0)

  gridmicrotex:::microtex_release()
  # Force a genuine re-parse; a cache hit would prove nothing.
  latex_cache_clear()

  # Our own macros still resolve...
  expect_equal(
    nrow(latex_grob("\\gmfontfamily{A}{x}", input_mode = "math")$layout_df),
    1L
  )
  expect_no_error(latex_grob("a\\mark{m}b", input_mode = "math"))
  expect_equal(
    .resolve_text_family(
      latex_grob("\\textsf{\\textrm{a}}", input_mode = "math")$layout_df$font_style[1],
      default = "BODY", family = "gridmicrotex.default"
    ),
    "BODY"
  )

  # ...and the formula that used to blow up still parses identically.
  expect_equal(nrow(latex_grob(big, input_mode = "math")$layout_df), before)
})
