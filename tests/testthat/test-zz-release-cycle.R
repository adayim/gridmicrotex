# Named "zz" so it runs last: it releases MicroTeX's per-session state,
# which the other files must not inherit.

test_that("a release / re-init cycle leaves the macro registry usable", {
  # microtex_release() must drop only per-session state and leave the
  # macro registry alone; see src/init.cpp.
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
  # \mark records its anchor and draws nothing.
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

  # ...and the big formula still parses identically.
  expect_equal(nrow(latex_grob(big, input_mode = "math")$layout_df), before)

  # p{} columns still constrain the column: narrower than a free column,
  # and tall enough to have wrapped.
  wide <- "\\begin{tabular}{p{3cm}}\\text{wrap me over several lines}\\end{tabular}"
  free <- "\\begin{tabular}{l}\\text{wrap me over several lines}\\end{tabular}"
  d <- latex_dims(wide, input_mode = "math", gp = grid::gpar(fontsize = 16))
  expect_lt(as.numeric(d$width),
            as.numeric(latex_dims(free, input_mode = "math",
                                  gp = grid::gpar(fontsize = 16))$width))
  expect_gt(as.numeric(d$height), 20)
})

test_that("unloading the namespace unloads the DLL, and a reload still parses", {
  # In a subprocess: the package cannot unload itself under testthat.
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
    'cat("GM-ROWS:", before, after, "\\n")',
    # Our own macros sit behind s_registered guards; see CLAUDE.md.
    'ff <- nrow(gridmicrotex:::latex_grob("\\\\gmfontfamily{mono}{x}",',
    '                                     input_mode = "math")$layout_df)',
    'mk <- gridmicrotex:::latex_grob("a\\\\mark{m}b", input_mode = "math")',
    'cat("GM-OURS:", ff, if (is.null(mk$marks)) "NA" else mk$marks$name, "\\n")'
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

  # The DLL is gone after unloadNamespace().
  expect_equal(trimws(sub("^GM-DLL-MAPPED:", "", mapped)), "FALSE", info = info)

  # The reload re-registers the built-ins.
  n <- as.integer(strsplit(trimws(sub("^GM-ROWS:", "", rows)), " +")[[1]])
  expect_gt(n[1], 0L)
  expect_equal(n[2], n[1], info = info)

  # ...and ours.
  ours <- grep("^GM-OURS:", out, value = TRUE)
  expect_length(ours, 1L)
  parts <- strsplit(trimws(sub("^GM-OURS:", "", ours)), " +")[[1]]
  expect_equal(as.integer(parts[1]), 1L, info = info)
  expect_equal(parts[2], "m", info = info)
})
