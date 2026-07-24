test_that("text measurer creates, measures, and handles styles", {
  measurer <- gridmicrotex:::.make_text_measurer(grid::gpar())
  expect_type(measurer, "closure")

  result <- measurer("Hello", 0L)
  expect_length(result, 3)
  expect_true(all(result > 0))

  # Width scales with text length
  expect_true(measurer("Hello World", 0L)[1] > measurer("Hi", 0L)[1])

  # Bold text wider than plain
  expect_true(measurer("Hello", 2L)[1] >= measurer("Hello", 0L)[1])

  # .resolve_text_face maps style codes
  expect_equal(gridmicrotex:::.resolve_text_face(0L), "plain")
  expect_equal(gridmicrotex:::.resolve_text_face(2L), "bold")
  expect_equal(gridmicrotex:::.resolve_text_face(6L), "bold.italic")
  expect_equal(gridmicrotex:::.resolve_text_face(NA_integer_), "plain")
})

test_that("\\texttt renders and measures in a monospace family", {
  # Bit 128 is MicroTeX's \texttt. It has to reach the *measurer* as well
  # as the renderer: measuring in one family and drawing in another puts
  # the glyphs where they do not fit.
  fam <- gridmicrotex:::.resolve_text_family
  expect_equal(fam(128L), "mono")
  expect_equal(fam(130L), "mono")                 # bold monospace
  expect_null(fam(2L))                            # plain bold: caller's choice
  expect_null(fam(NA_integer_))
  expect_equal(fam(2L, "serif"), "serif")         # default passes through
  expect_equal(fam(128L, "serif"), "mono")        # \texttt still wins

  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # The measurer must answer with monospace metrics, which are the same
  # for every character -- that is what makes the font monospace.
  m <- gridmicrotex:::.make_text_measurer(grid::gpar())
  expect_equal(m("i", 128L)[1], m("W", 128L)[1])
  expect_true(m("i", 1L)[1] < m("W", 1L)[1])

  # End to end: the drawn textGrob carries the family too.
  g <- grid::makeContent(latex_grob("\\texttt{Hg}", input_mode = "math"))
  fams <- vapply(g$children, function(k) k$gp$fontfamily %||% "", character(1))
  expect_true("mono" %in% fams)
})

test_that("a named font family travels from LaTeX to the layout", {
  # MicroTeX has no channel for a font *name* -- a text run carries only
  # FontStyle, a bitfield. \gmfontfamily packs an index into its unused
  # high byte, which C++ resolves back to the name on the record.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  runs <- function(tex) {
    df <- latex_grob(tex, input_mode = "math",
                     gp = grid::gpar(fontsize = 20))$layout_df
    df <- df[df$type == "text", c("text", "font_family", "font_style")]
    rownames(df) <- NULL
    df
  }
  one <- runs("\\gmfontfamily{Georgia}{ab}")
  expect_true(all(one$font_family == "Georgia"))
  # Ordinary text names no family, so gp$fontfamily still decides.
  expect_true(all(is.na(runs("\\text{ab}")$font_family)))

  # Nesting must *replace*: two indices OR'd together would address a
  # third, unrelated family, so "c" would come back as neither A nor B.
  nested <- runs("\\gmfontfamily{A}{a\\gmfontfamily{B}{b}c}")
  expect_equal(nested$font_family, c("A", "B", "A"))

  # The family composes with emphasis instead of replacing it: the low
  # byte keeps the bold bit, the high byte carries the index.
  bold <- runs("\\textbf{\\gmfontfamily{Georgia}{ab}}")
  expect_true(all(bitwAnd(bold$font_style, 2L) != 0L))
  expect_true(all(bold$font_family == "Georgia"))
  # Either order gives the same answer.
  expect_equal(bold, runs("\\gmfontfamily{Georgia}{\\textbf{ab}}"))

  # An empty name is a no-op rather than an error.
  expect_silent(runs("\\gmfontfamily{}{ab}"))
})

test_that("two families in one expression are measured apart", {
  # The C++ measurement cache used to key on the low byte of the style,
  # which is where the family index is *not*. Two families would then
  # share one set of metrics -- whichever was asked for first.
  skip_if_not_installed("ragg")
  f <- tempfile(fileext = ".png")
  ragg::agg_png(f, width = 400, height = 120)
  on.exit({ dev.off(); unlink(f) }, add = TRUE)
  w <- function(tex) as.numeric(latex_dims(tex, input_mode = "math",
                                           gp = grid::gpar(fontsize = 20))$width)
  mono <- w("\\gmfontfamily{mono}{Wig}")
  sans <- w("\\gmfontfamily{sans}{Wig}")
  expect_false(isTRUE(all.equal(mono, sans)))
  # Same two runs side by side must keep their own widths.
  both <- w("\\gmfontfamily{mono}{Wig}\\gmfontfamily{sans}{Wig}")
  expect_equal(both, mono + sans, tolerance = 1)
})

test_that("register/clear measurer lifecycle and integration", {
  m <- gridmicrotex:::.make_text_measurer(grid::gpar())
  expect_silent(register_text_measurer(m))
  expect_silent(clear_text_measurer())

  # Double-register replaces previous without error
  m2 <- gridmicrotex:::.make_text_measurer(grid::gpar(fontfamily = "mono"))
  register_text_measurer(m)
  expect_silent(register_text_measurer(m2))
  clear_text_measurer()

  # CJK layout uses measurer for dimensions
  if (.Platform$OS.type == "windows") {
    expect_no_error(dims <- latex_dims("\\text{\u4F60\u597D\u4E16\u754C}", gp = grid::gpar(fontsize = 20)))
  } else {
    expect_silent(dims <- latex_dims("\\text{\u4F60\u597D\u4E16\u754C}", gp = grid::gpar(fontsize = 20)))
  }
  expect_true(grid::convertWidth(dims$width, "bigpts", valueOnly = TRUE) > 0)
})

test_that("measurer cache returns identical values to a fresh measurement", {
  # Within one closure, repeat calls hit the cache; they must equal the
  # first (un-cached) call bit-for-bit, and must also match a separate
  # fresh closure's first (un-cached) call.
  txt <- "The quick brown fox jumps over the lazy dog"
  m1 <- gridmicrotex:::.make_text_measurer(grid::gpar())
  first  <- m1(txt, 0L)
  second <- m1(txt, 0L)  # cache hit
  expect_identical(first, second)

  m2 <- gridmicrotex:::.make_text_measurer(grid::gpar())
  fresh  <- m2(txt, 0L)  # un-cached, separate closure
  expect_identical(first, fresh)

  # Different font_style must not collide with a cached entry.
  bold_cached <- m1(txt, 2L)  # first time for style=2 on m1
  bold_fresh  <- m2(txt, 2L)
  expect_identical(bold_cached, bold_fresh)
  expect_false(identical(first, bold_cached))
})

test_that("measuring leaves the caller's display list untouched", {
  # Measuring used to pushViewport() on whatever device the caller had open.
  # Viewport pushes are recorded on the graphics engine display list, so the
  # device then looks like it holds a plot and knitr snapshots a spurious
  # blank figure before the real plot's grid.newpage().
  dl_len <- function() length(grDevices::recordPlot()[[1]])

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  grDevices::dev.control("enable")
  expect_identical(dl_len(), 0L)

  m <- gridmicrotex:::.make_text_measurer(grid::gpar())
  m("Heterogeneity", 0L)
  expect_identical(dl_len(), 0L)

  # Same for a full grob construction, which is what callers actually do.
  latex_grob("\\text{This is study A}\\\\\\text{This is study B}",
             input_mode = "math", render_mode = "path",
             gp = grid::gpar(fontsize = 8))
  expect_identical(dl_len(), 0L)
})
