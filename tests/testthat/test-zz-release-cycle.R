# Named "zz" so testthat runs it last: it tears MicroTeX down and lets it
# re-initialise, which is not something the other files should inherit
# halfway through.

test_that("a release / re-init cycle leaves the macro registry usable", {
  # MicroTeX::release() is now true final teardown for DLL unload: it frees
  # the dynamic macro registries and singleton state. That is correct at
  # process teardown, but not for an in-process release/re-init cycle because
  # the vendored built-in MacroInfo table is populated at library load time.
  # microtex_release() therefore keeps the process-lifetime registries intact
  # and only drops per-session state -- see src/init.cpp.
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
  # \mark is ours, registered at init. After a release it must still be
  # recognised: the anchor is recorded and the marker itself draws
  # nothing, so the layout is exactly that of the text without it.
  marked <- latex_grob("a\\mark{m}b", input_mode = "math")
  expect_equal(marked$marks$name, "m")
  expect_equal(nrow(marked$layout_df),
               nrow(latex_grob("ab", input_mode = "math")$layout_df))
  expect_equal(
    .resolve_text_family(
      latex_grob("\\textsf{\\textrm{a}}", input_mode = "math")$layout_df$font_style[1],
      default = "BODY", family = "gridmicrotex.default"
    ),
    "BODY"
  )

  # ...and the formula that used to blow up still parses identically.
  expect_equal(nrow(latex_grob(big, input_mode = "math")$layout_df), before)

  # The p{} column type is registered by the vendored spec parser, not by
  # a macro, so it should be unaffected by a release -- assert it rather
  # than assume it.
  wide <- "\\begin{tabular}{p{3cm}}\\text{wrap me over several lines}\\end{tabular}"
  free <- "\\begin{tabular}{l}\\text{wrap me over several lines}\\end{tabular}"
  d <- latex_dims(wide, input_mode = "math", gp = grid::gpar(fontsize = 16))
  # p{3cm} still constrains the column: narrower than the same cell in a
  # free column, and tall enough to have wrapped onto more than one line.
  expect_lt(as.numeric(d$width),
            as.numeric(latex_dims(free, input_mode = "math",
                                  gp = grid::gpar(fontsize = 16))$width))
  expect_gt(as.numeric(d$height), 20)
})
