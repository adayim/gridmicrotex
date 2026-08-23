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

  # Two families in one expression must be measured apart. The C++ cache
  # used to key on the low byte of the style, which is where the family
  # index is *not*, so they shared one set of metrics -- whichever was
  # asked for first. Needs a device that resolves families.
  skip_if_not_installed("ragg")
  f <- tempfile(fileext = ".png")
  ragg::agg_png(f, width = 400, height = 120)
  on.exit({ dev.off(); unlink(f) }, add = TRUE)
  # Measured large on purpose. Layout widths are whole big points, so at a
  # normal size that quantum is several percent of a three-letter word: two
  # genuinely different faces whose "Wig" differs by less than one point land
  # on the same number, and the families look identical although nothing is
  # wrong. That is what failed on a CRAN Debian box. It reproduces here at
  # fontsize 10, where stock DejaVu mono and sans both measure 18; at 200 the
  # same pair differs by 14.
  w <- function(tex) as.numeric(latex_dims(tex, input_mode = "math",
                                           gp = grid::gpar(fontsize = 200))$width)
  # A font set with no distinct monospace face resolves both names to one
  # file, which no font size can separate. Probe with \texttt, which takes its
  # family from the FontStyle bit rather than the \gmfontfamily registry, so
  # this cannot mask a regression in the code under test: that would leave
  # \texttt working and fail the assertions below.
  #
  # The message carries what the layout recorded, so a machine we cannot reach
  # reports itself. Built lazily: skip_if() only forces it when it skips.
  tt <- w("\\texttt{Wig}")
  body <- w("\\text{Wig}")
  skip_if(isTRUE(all.equal(tt, body)), {
    d <- latex_grob("\\texttt{Wig}", input_mode = "math",
                    gp = grid::gpar(fontsize = 20))$layout_df
    d <- d[d$type == "text", ]
    paste0("text measurement does not distinguish font families: ",
           "texttt=", tt, " text=", body,
           " font_style=", paste(d$font_style, collapse = "/"),
           " font_family=", paste(d$font_family, collapse = "/"),
           " mono=", basename(systemfonts::match_fonts("mono")$path),
           " sans=", basename(systemfonts::match_fonts("sans")$path))
  })
  mono <- w("\\gmfontfamily{mono}{Wig}")
  sans <- w("\\gmfontfamily{sans}{Wig}")
  expect_false(isTRUE(all.equal(mono, sans)))
  expect_equal(w("\\gmfontfamily{mono}{Wig}\\gmfontfamily{sans}{Wig}"),
               mono + sans, tolerance = 1)
})

test_that("\\textrm returns to the caller's font", {
  # MicroTeX's own \textrm only ORs in FontStyle::rm -- the same bit a plain
  # \text{} run already carries -- so it could never express "reset the
  # family", and did nothing at all. The override names a reserved family
  # instead; see src/MicroTeX/lib/atom/font_family_atom.h.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  fam <- function(tex) {
    df <- latex_grob(tex, input_mode = "math",
                     gp = grid::gpar(fontsize = 20))$layout_df
    df <- df[df$type == "text", ]
    vapply(seq_len(nrow(df)),
           function(i) .resolve_text_family(df$font_style[i], default = "BODY",
                                            family = df$font_family[i]),
           character(1))
  }

  expect_equal(fam("\\textsf{a\\textrm{b}c}"), c("sans", "BODY", "sans"))
  expect_equal(fam("\\texttt{a\\textrm{b}c}"), c("mono", "BODY", "mono"))
  expect_equal(fam("\\gmfontfamily{Georgia}{a\\textrm{b}c}"),
               c("Georgia", "BODY", "Georgia"))

  # On its own it is still ordinary body text, as it always was. One
  # record, not one per letter: consecutive text characters are drawn as
  # a word (see RowAtom::processTextRun). The three cases above stay
  # one-per-record because each letter sits in a different font scope.
  expect_equal(fam("\\textrm{ab}"), "BODY")

  # Emphasis survives: the override keeps upstream's *nested* FontStyleAtom,
  # so only the family is replaced, not the bold/italic bits.
  df <- latex_grob("\\textsf{\\textbf{a\\textrm{b}c}}", input_mode = "math",
                   gp = grid::gpar(fontsize = 20))$layout_df
  expect_true(all(bitwAnd(df$font_style[df$type == "text"], 2L) != 0L))
})

test_that("\\textrm is measured in the font it is drawn in", {
  # Measuring in mono while drawing in the body font puts glyphs where they
  # do not fit, so the reset has to reach the measurer too.
  skip_if_not_installed("ragg")
  f <- tempfile(fileext = ".png")
  ragg::agg_png(f, width = 400, height = 100)
  on.exit({ dev.off(); unlink(f) }, add = TRUE)
  w <- function(tex) as.numeric(latex_dims(tex, input_mode = "math",
                                           gp = grid::gpar(fontsize = 20))$width)

  body <- w("\\text{Wig}")
  skip_if(isTRUE(all.equal(w("\\texttt{Wig}"), body)),
          "device does not distinguish mono from the body font")
  expect_equal(w("\\texttt{\\textrm{Wig}}"), body)
  expect_equal(w("\\textrm{Wig}"), body)
})

test_that("register/clear measurer lifecycle and integration", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  m <- gridmicrotex:::.make_text_measurer(grid::gpar())
  m2 <- gridmicrotex:::.make_text_measurer(grid::gpar(fontfamily = "mono"))
  # The two closures must actually disagree, or "the second registration
  # replaced the first" is unfalsifiable below.
  expect_false(isTRUE(all.equal(m("Wig", 0L), m2("Wig", 0L))))

  register_text_measurer(m)
  clear_text_measurer()
  # Double-register replaces the previous one rather than stacking.
  register_text_measurer(m)
  register_text_measurer(m2)
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
  # The three-argument form, used by \gmfontfamily and \textrm spans.
  m("Heterogeneity", 0L, "Georgia")
  expect_identical(dl_len(), 0L)

  # Same for a full grob construction, which is what callers actually do.
  latex_grob("\\text{This is study A}\\\\\\text{This is study B}",
             input_mode = "math", render_mode = "path",
             gp = grid::gpar(fontsize = 8))
  expect_identical(dl_len(), 0L)
  latex_dims("\\text{measure me}", input_mode = "math")
  expect_identical(dl_len(), 0L)

  # Markdown measures far more text than a formula does, and arrived after
  # the fix above, so it needs its own guard.
  markdown_grob("**bold** and $x^2$")
  expect_identical(dl_len(), 0L)
  markdown_box_grob("# Title\n\nProse with $x^2$.\n\n- one\n- two",
                    width = grid::unit(3, "in"))
  expect_identical(dl_len(), 0L)

  # grobWidth() forces the layout, which is where the measuring happens.
  grid::convertWidth(grid::grobWidth(markdown_grob("hello $x$")),
                     "bigpts", valueOnly = TRUE)
  expect_identical(dl_len(), 0L)

  # A drawing call *must* still record, or the assertions above are vacuous.
  grid::grid.newpage()
  grid::grid.draw(latex_grob("\\text{drawn}", input_mode = "math",
                             render_mode = "path"))
  expect_gt(dl_len(), 0L)
})

test_that("a cached layout is not reused across graphics devices", {
  skip_if_not_installed("ragg")
  # \text{} runs are sized by the R text-measurer, which measures through
  # grid on the *current* device -- and devices disagree. Without the
  # device in the cache key, a layout measured on the screen device was
  # handed back verbatim to a later ggsave(), placing text at widths the
  # output device never agreed to.
  key <- gridmicrotex:::.parse_cache_key
  args <- list("\\text{x}", 20, 10, "#000000", 0, "", "", "", FALSE, "",
               FALSE, FALSE)
  expect_false(identical(do.call(key, c(args, device = "pdf")),
                         do.call(key, c(args, device = "agg_png"))))

  # End to end: the width from a device must not depend on which device
  # happened to parse the same expression first.
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  tex <- "\\text{Hello world}"
  on_agg <- function() {
    ragg::agg_png(f)
    on.exit(grDevices::dev.off(), add = TRUE)
    as.numeric(latex_dims(tex, input_mode = "math")$width)
  }

  latex_cache_clear()
  alone <- on_agg()

  latex_cache_clear()
  grDevices::pdf(NULL)
  latex_dims(tex, input_mode = "math")
  grDevices::dev.off()
  expect_equal(on_agg(), alone)
})
