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

test_that("a layout measured at one resolution is not reused at another", {
  on.exit(latex_cache_clear(), add = TRUE)
  tex <- "\\text{WAVY fi MM 12345}"
  measure_at <- function(res) {
    f <- tempfile(fileext = ".png")
    grDevices::png(f, width = 400, height = 200, res = res)
    on.exit({ grDevices::dev.off(); unlink(f) })
    grid::convertWidth(latex_dims(tex)$width, "bigpts", TRUE)
  }
  latex_cache_clear()
  measure_at(72)
  at_300_after_72 <- measure_at(300)
  latex_cache_clear()
  at_300 <- measure_at(300)
  # png() measures this phrase 1bp apart at 72 and 300 dpi.
  expect_equal(at_300_after_72, at_300)
})

test_that("\\texttt renders and measures in a monospace family", {
  # Bit 128 is \texttt; measurer and renderer must agree on the family.
  fam <- gridmicrotex:::.resolve_text_family
  expect_equal(fam(128L), "mono")
  expect_equal(fam(130L), "mono")                 # bold monospace
  expect_null(fam(2L))                            # plain bold: caller's choice
  expect_null(fam(NA_integer_))
  expect_equal(fam(2L, "serif"), "serif")         # default passes through
  expect_equal(fam(128L, "serif"), "mono")        # \texttt still wins

  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Monospace: every character is the same width.
  m <- gridmicrotex:::.make_text_measurer(grid::gpar())
  expect_equal(m("i", 128L)[1], m("W", 128L)[1])
  expect_true(m("i", 1L)[1] < m("W", 1L)[1])

  # End to end: the drawn textGrob carries the family too.
  g <- grid::makeContent(latex_grob("\\texttt{Hg}", input_mode = "math"))
  fams <- vapply(g$children, function(k) k$gp$fontfamily %||% "", character(1))
  expect_true("mono" %in% fams)
})

test_that("a named font family travels from LaTeX to the layout", {
  # \gmfontfamily packs a family index into FontStyle's high byte.
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

  # Nesting replaces: two indices OR'd together would name a third family.
  nested <- runs("\\gmfontfamily{A}{a\\gmfontfamily{B}{b}c}")
  expect_equal(nested$font_family, c("A", "B", "A"))

  # Composes with emphasis: bold in the low byte, family in the high.
  bold <- runs("\\textbf{\\gmfontfamily{Georgia}{ab}}")
  expect_true(all(bitwAnd(bold$font_style, 2L) != 0L))
  expect_true(all(bold$font_family == "Georgia"))
  # Either order gives the same answer.
  expect_equal(bold, runs("\\gmfontfamily{Georgia}{\\textbf{ab}}"))

  # An empty name is a no-op rather than an error.
  expect_silent(runs("\\gmfontfamily{}{ab}"))

  # Two families in one expression are measured apart. Needs a device that
  # resolves families.
  skip_if_not_installed("ragg")
  f <- tempfile(fileext = ".png")
  ragg::agg_png(f, width = 400, height = 120)
  on.exit({ dev.off(); unlink(f) }, add = TRUE)
  # Measured large: widths are whole big points, and at fontsize 10 DejaVu
  # mono and sans both measure "Wig" as 18 (CRAN Debian).
  w <- function(tex) as.numeric(latex_dims(tex, input_mode = "math",
                                           gp = grid::gpar(fontsize = 200))$width)
  # Skip where mono and sans are one file. \texttt probes this without the
  # \gmfontfamily code under test; the message reports what was recorded.
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
  # See src/MicroTeX/lib/atom/font_family_atom.h.
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

  # Alone it is body text, drawn as one record.
  expect_equal(fam("\\textrm{ab}"), "BODY")

  # Only the family is reset; bold survives.
  df <- latex_grob("\\textsf{\\textbf{a\\textrm{b}c}}", input_mode = "math",
                   gp = grid::gpar(fontsize = 20))$layout_df
  expect_true(all(bitwAnd(df$font_style[df$type == "text"], 2L) != 0L))
})

test_that("\\textnormal sets the normal font, whatever is around it", {
  # As LaTeX's \normalfont: upright, medium, in the caller's family.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  styles <- function(tex) {
    df <- latex_grob(tex, gp = grid::gpar(fontsize = 20))$layout_df
    df <- df[df$type == "text", ]
    stats::setNames(df$font_style, trimws(df$text))
  }
  plain <- styles("\\text{b}")[["b"]]
  for (around in c("\\textbf{a \\textnormal{b}}", "\\textit{\\textsf{a \\textnormal{b}}}",
                   "\\texttt{a \\textnormal{b}}", "\\gmfontfamily{Georgia}{a \\textnormal{b}}")) {
    expect_equal(styles(around)[["b"]], plain, info = around)
  }
  # What is around it keeps its own.
  expect_equal(bitwAnd(styles("\\textbf{a \\textnormal{b}}")[["a"]], 2L), 2L)
})

test_that("\\textrm is measured in the font it is drawn in", {
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

test_that("text widths come from the measurer registered for the parse", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  on.exit(latex_cache_clear(), add = TRUE)
  width_with <- function(ratio) {
    local_mocked_bindings(
      .make_text_measurer = function(text_gp, roles = NULL) {
        function(text, font_style, family = NULL) c(ratio * nchar(text), 0.7, 1)
      },
      .package = "gridmicrotex")
    latex_cache_clear()
    as.numeric(latex_dims("\\text{abcd}", input_mode = "math",
                          gp = grid::gpar(fontsize = 20))$width)
  }
  half <- width_with(0.5)
  expect_equal(half, 0.5 * 4 * 20, tolerance = 1)
  # A second registration replaces the first.
  expect_equal(width_with(1), 2 * half, tolerance = 1)
})

test_that("CJK text is measured without warnings", {
  f <- tempfile(fileext = ".png")
  grDevices::png(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  expect_silent(dims <- latex_dims("\\text{\u4F60\u597D\u4E16\u754C}",
                                   gp = grid::gpar(fontsize = 20)))
  expect_gt(grid::convertWidth(dims$width, "bigpts", valueOnly = TRUE), 0)
})

test_that("measurer cache returns identical values to a fresh measurement", {
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
  # A viewport push on the caller's device made knitr snapshot a spurious
  # blank figure.
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

  latex_grob("\\text{This is study A}\\\\\\text{This is study B}",
             input_mode = "math", render_mode = "path",
             gp = grid::gpar(fontsize = 8))
  expect_identical(dl_len(), 0L)
  latex_dims("\\text{measure me}", input_mode = "math")
  expect_identical(dl_len(), 0L)

  markdown_grob("**bold** and $x^2$")
  expect_identical(dl_len(), 0L)
  markdown_box_grob("# Title\n\nProse with $x^2$.\n\n- one\n- two",
                    width = grid::unit(3, "in"))
  expect_identical(dl_len(), 0L)

  # grobWidth() forces the layout, which is where the measuring happens.
  grid::convertWidth(grid::grobWidth(markdown_grob("hello $x$")),
                     "bigpts", valueOnly = TRUE)
  expect_identical(dl_len(), 0L)

  # Drawing must still record, or the assertions above are vacuous.
  grid::grid.newpage()
  grid::grid.draw(latex_grob("\\text{drawn}", input_mode = "math",
                             render_mode = "path"))
  expect_gt(dl_len(), 0L)
})

test_that("a cached layout is not reused across graphics devices", {
  skip_if_not_installed("ragg")
  # Text is measured on the current device, and devices disagree.
  key <- gridmicrotex:::.parse_cache_key
  args <- list("\\text{x}", 20, 10, "#000000", 0, "", "", "", FALSE, "",
               FALSE, FALSE)
  expect_false(identical(do.call(key, c(args, device = "pdf")),
                         do.call(key, c(args, device = "agg_png"))))

  # End to end: the width must not depend on which device parsed first.
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

test_that("with no device open, a parse opens one for its length and a cache hit none", {
  skip_if(!is.null(grDevices::dev.list()), "a device is already open")
  opened <- 0L
  suppressMessages(trace("pdf", where = asNamespace("grDevices"),
                         tracer = function() opened <<- opened + 1L, print = FALSE))
  on.exit(suppressMessages(untrace("pdf", where = asNamespace("grDevices"))), add = TRUE)
  latex_cache_clear()
  latex_dims("\\text{a word}", input_mode = "math")
  expect_identical(opened, 1L)
  expect_null(grDevices::dev.list())  # and closed again
  for (i in 1:5) latex_dims("\\text{a word}", input_mode = "math")
  expect_identical(opened, 1L)
})
