# Named "zz" so testthat runs it last: it tears MicroTeX down and lets it
# re-initialise, which is not something the other files should inherit
# halfway through.

test_that("a release / re-init cycle leaves the macro registry usable", {
  # MicroTeX::release() is MacroInfo::_free_() + NewCommandMacro::_free_(),
  # i.e. final teardown: it empties the macro registries, which are only
  # ever repopulated from inside MicroTeX::init(). microtex_release() is
  # not paired with a re-init, so it must leave them alone and drop only
  # per-session state -- see src/init.cpp. (Historically _free_() deleted
  # every value in the static _commands map without erasing it, so the
  # next MacroInfo::add() double-freed a built-in; that is fixed at the
  # source now, but calling it from here would still strand the parser
  # without \frac and friends.)
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

test_that("unloading the namespace unloads the DLL, and a reload still parses", {
  # This has to run in a subprocess: the package under test cannot unload
  # itself out from under testthat. Two things are asserted, and the first
  # is the one that silently regressed before -- .onUnload() used to call
  # microtex_release() only, so R kept the shared object mapped and
  # R_unload_gridmicrotex() never ran.
  skip_on_cran()
  rscript <- file.path(R.home("bin"), "Rscript")
  skip_if(
    !any(file.exists(rscript, paste0(rscript, ".exe"))),
    "Rscript not found next to this R"
  )

  probe <- tempfile(fileext = ".R")
  writeLines(c(
    'pdf(NULL)',
    'library(gridmicrotex)',
    'tex <- "\\\\frac{1}{2} + x^2"',
    'before <- nrow(gridmicrotex:::latex_grob(tex, input_mode = "math")$layout_df)',
    'unloadNamespace("gridmicrotex")',
    'cat("GM-DLL-MAPPED:", "gridmicrotex" %in% names(getLoadedDLLs()), "\\n")',
    'library(gridmicrotex)',
    'gridmicrotex::latex_cache_clear()',
    'after <- nrow(gridmicrotex:::latex_grob(tex, input_mode = "math")$layout_df)',
    'cat("GM-ROWS:", before, after, "\\n")'
  ), probe)
  on.exit(unlink(probe), add = TRUE)

  # system2(env=) is a no-op on Windows, so set it here and inherit.
  old <- Sys.getenv("R_LIBS", unset = NA)
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  on.exit(
    if (is.na(old)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = old),
    add = TRUE
  )

  out <- suppressWarnings(
    system2(rscript, c("--vanilla", shQuote(probe)), stdout = TRUE, stderr = TRUE)
  )
  info <- paste(out, collapse = "\n")

  mapped <- grep("^GM-DLL-MAPPED:", out, value = TRUE)
  rows <- grep("^GM-ROWS:", out, value = TRUE)
  expect_length(mapped, 1L)
  expect_length(rows, 1L)

  # The DLL is gone after unloadNamespace(), which is what makes
  # R_unload_gridmicrotex() reachable at all.
  expect_equal(trimws(sub("^GM-DLL-MAPPED:", "", mapped)), "FALSE", info = info)

  # And the reload re-registers the built-ins: \frac parses to the same
  # number of draw records as it did before the teardown.
  n <- as.integer(strsplit(trimws(sub("^GM-ROWS:", "", rows)), " +")[[1]])
  expect_gt(n[1], 0L)
  expect_equal(n[2], n[1], info = info)
})
