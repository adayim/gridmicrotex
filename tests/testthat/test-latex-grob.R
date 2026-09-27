# --- latex_grob creation and structure ---

test_that("latex_grob creates valid grob and returns correct dimensions", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  g <- latex_grob("\\frac{x^{2}+1}{\\sqrt{y}}")
  expect_s3_class(g, "latexgrob")
  expect_true(nrow(g$layout_df) > 0)
  expect_true(g$bbox_w > 0)

  # render_mode stored correctly
  g_path <- latex_grob("$x^2$", render_mode = "path")
  g_type <- latex_grob("$x^2$", render_mode = "typeface")
  expect_equal(g_path$render_mode, "path")
  expect_null(g_path$path_layout_df)
  expect_s3_class(g_type$path_layout_df, "data.frame")

  # latex_dims with fontsize scaling
  dims <- latex_dims("\\frac{a}{b}", render_mode = "path")
  expect_true(grid::convertWidth(dims$width, "points", valueOnly = TRUE) > 0)
  w_small <- grid::convertWidth(latex_dims("$x^2$", gp = grid::gpar(fontsize = 10))$width,
                                "bigpts", valueOnly = TRUE)
  w_large <- grid::convertWidth(latex_dims("$x^2$", gp = grid::gpar(fontsize = 40))$width,
                                "bigpts", valueOnly = TRUE)
  expect_true(w_large > w_small)
})

# --- latex_grob parameters ---

test_that("latex_grob parameters work correctly", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Rotation
  expect_equal(latex_grob("$x^2$", rot = 45)$vp$angle, 45)

  # max_width
  g_mw <- latex_grob("$x^2 + y^2 = z^2$", max_width = 50, render_mode = "path")
  expect_true(grid::convertWidth(grid::grobWidth(g_mw), "bigpts", valueOnly = TRUE) > 0)

  # makeContent builds children
  g_mc <- grid::makeContent(latex_grob("\\frac{a}{b}", render_mode = "path"))
  expect_true(length(g_mc$children) > 0)

  # width/height details positive
  g <- latex_grob("\\frac{a}{b}", render_mode = "path")
  expect_true(grid::convertWidth(grid::widthDetails(g), "bigpts", valueOnly = TRUE) > 0)
  expect_true(grid::convertHeight(grid::heightDetails(g), "bigpts", valueOnly = TRUE) > 0)
})

# --- device support and typeface rendering ---

test_that("device support detection and typeface fallback work", {
  kinds <- function(g) {
    kids <- suppressMessages(grid::makeContent(g))$children
    vapply(kids, function(k) class(k)[1], "")
  }
  frac <- function() latex_grob("\\frac{a}{b}", render_mode = "typeface",
                                gp = grid::gpar(fontsize = 20))

  # pdf() and postscript() fall back with a message. pdf() reports glyphs
  # (R >= 4.3) but does not embed the font, which garbled the math in any
  # viewer without it; postscript() has no glyphs at all.
  # Each device is closed on every way out: one left open would be the
  # device every later test measures and draws on.
  falls_back_on <- function(dev) {
    tf <- tempfile()
    get(dev, asNamespace("grDevices"))(tf)
    opened <- grDevices::dev.cur()
    on.exit({ grDevices::dev.off(opened); unlink(tf) }, add = TRUE)
    gridmicrotex:::.clear_typeface_noted()
    expect_false(gridmicrotex:::.device_supports_typeface_glyphs(), info = dev)
    expect_message({
      g <- frac()
      grid::grid.newpage()
      grid::grid.draw(g)
    }, "falling back to path mode", info = dev)
    # The fallback has to substitute the path layout, not merely warn: the
    # children it draws must be outlines, not glyphs the device cannot set.
    expect_true("pathgrob" %in% kinds(g), info = dev)
    expect_false("glyphgrob" %in% kinds(g), info = dev)
  }
  for (dev in c("pdf", "postscript")) falls_back_on(dev)

  # cairo_pdf() embeds the font, so the math stays text.
  skip_if_not(capabilities("cairo"))
  tf <- tempfile(fileext = ".pdf")
  open_before <- length(grDevices::dev.list())
  # A Mac without XQuartz reports cairo, but loading it fails with a
  # warning and no device opens.
  suppressWarnings(grDevices::cairo_pdf(tf))
  skip_if(length(grDevices::dev.list()) == open_before, "cairo_pdf() opened no device")
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  expect_true(gridmicrotex:::.device_supports_typeface_glyphs())
  expect_true("glyphgrob" %in% kinds(frac()))
})

test_that("the text measurer's ascent and descent are the device's own", {
  # It reads them off per-character caches -- R's own rule for one line
  # (GEStrMetric) -- so they must equal measuring the whole string. The
  # engine hands its text over marked "unknown", which once split an emoji
  # into an extra, empty piece. On png(), which can set all of these:
  # pdf() cannot encode them, and under R CMD check --as-cran that is an
  # error.
  skip_if(getRversion() < "4.4.0")
  tf <- tempfile(fileext = ".png")
  grDevices::png(tf)
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  m <- .make_text_measurer(grid::gpar())
  for (s in c("Transformer", "gy", "a b", "é", intToUtf8(c(0x1F916, 0xFE0F)),
              intToUtf8(0x1D4B6))) {
    x <- s
    Encoding(x) <- "unknown"
    tg <- grid::textGrob(s, gp = grid::gpar(fontsize = 72))
    asc <- suppressWarnings(grid::convertHeight(grid::grobAscent(tg), "bigpts", valueOnly = TRUE))
    dsc <- suppressWarnings(grid::convertHeight(grid::grobDescent(tg), "bigpts", valueOnly = TRUE))
    got <- suppressWarnings(m(x, 0L, ""))
    expect_equal(got[2] * 72, asc, info = s)
    expect_equal((got[3] - got[2]) * 72, dsc, info = s)
  }
})

# --- edge cases: empty and invalid input ---

test_that("latex_grob handles empty input as a zero-size grob", {
  g <- latex_grob("")
  expect_s3_class(g, "latexgrob")
  expect_equal(g$bbox_w, 0)
  expect_equal(g$bbox_h, 0)
  expect_equal(nrow(g$layout_df), 0L)
})

test_that("an unknown command is set as its own name, not dropped, and warns", {
  # MicroTeX is lenient rather than strict: \notavalidcommand comes out as
  # the letters of its name followed by its argument. Rendering nothing
  # would hide a typo completely, so assert the name is actually drawn.
  expect_warning(
    g <- latex_grob("\\notavalidcommand{x}", input_mode = "math"),
    "1:1: unknown command \\notavalidcommand", fixed = TRUE
  )
  expect_gte(nrow(g$layout_df), nchar("notavalidcommand"))
  expect_gt(g$bbox_w, 0)
})

# --- editGrob: re-parse on parse-affecting fields ---

test_that("editGrob re-parses when tex changes", {
  g <- latex_grob("x", render_mode = "path")
  g2 <- grid::editGrob(g, tex = "$x^{2} + y^{2} + z^{2}$")
  expect_equal(g2$tex, "$x^{2} + y^{2} + z^{2}$")
  expect_true(g2$bbox_w > g$bbox_w)
  # That the layout is genuinely rebuilt, without assuming how many
  # records a given string produces -- consecutive text is drawn as one
  # run when nothing wraps, so record count is not a proxy for length.
  expect_false(identical(g2$layout_df, g$layout_df))
  # viewport width/height tracked bbox
  expect_equal(
    as.numeric(g2$vp$width),
    as.numeric(grid::unit(g2$bbox_w, "bigpts"))
  )
})

test_that("editGrob re-parses when gp (fontsize) changes", {
  g20 <- latex_grob("$x^2$", render_mode = "path", gp = grid::gpar(fontsize = 20))
  g40 <- grid::editGrob(g20, gp = grid::gpar(fontsize = 40))
  expect_equal(g40$fontsize, 40)
  # 2x font -> ~2x bbox
  expect_equal(g40$bbox_w / g20$bbox_w, 2, tolerance = 0.05)
  # gp is re-stripped after parse (fontsize not carried on the gTree)
  expect_null(g40$gp$fontsize)
})

test_that("editGrob re-parses when math_font / tex_style / render_mode change", {
  g <- latex_grob("\\frac{a}{b}", render_mode = "path", math_font = "lete")
  g2 <- grid::editGrob(g, math_font = "stix")
  expect_false(identical(g$layout_df, g2$layout_df))

  g3 <- latex_grob("$\\sum_{i=1}^{n} i$", render_mode = "path", tex_style = "text")
  g4 <- grid::editGrob(g3, tex_style = "display")
  expect_true(g4$bbox_h > g3$bbox_h)  # display makes \sum taller

  g5 <- latex_grob("$x^2$", render_mode = "path")
  g6 <- grid::editGrob(g5, render_mode = "typeface")
  expect_equal(g6$render_mode, "typeface")
  expect_s3_class(g6$path_layout_df, "data.frame")  # fallback layout generated
})

test_that("editGrob on non-parse fields does not re-parse", {
  g <- latex_grob("\\frac{a}{b}", render_mode = "path")
  orig_layout <- g$layout_df
  g2 <- grid::editGrob(g, debug = TRUE)
  expect_true(isTRUE(g2$debug))
  expect_identical(g2$layout_df, orig_layout)
})

test_that("ascentDetails + descentDetails sum to heightDetails", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  g <- latex_grob("\\frac{a}{b}", render_mode = "path", gp = grid::gpar(fontsize = 24))
  asc  <- grid::convertHeight(grid::ascentDetails(g),  "bigpts", valueOnly = TRUE)
  desc <- grid::convertHeight(grid::descentDetails(g), "bigpts", valueOnly = TRUE)
  h    <- grid::convertHeight(grid::heightDetails(g),  "bigpts", valueOnly = TRUE)
  expect_equal(asc + desc, h, tolerance = 1e-6)
  expect_true(asc > 0)
  expect_true(desc >= 0)
  # Descent matches the bbox_d field exposed for grob-to-grob alignment
  expect_equal(desc, g$bbox_d, tolerance = 1e-6)
})

test_that("editGrob keeps viewport just in sync with hjust/vjust", {
  g <- latex_grob("x", render_mode = "path", hjust = 0.5, vjust = 0.5)
  g2 <- grid::editGrob(g, hjust = 0, vjust = 1)
  expect_equal(g2$hjust, 0)
  expect_equal(g2$vjust, 1)
  expect_equal(as.numeric(g2$vp$valid.just), c(0, 1))
})

test_that("latex_dims respects math_font parameter", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expr <- "$\\int_0^1 f(x)\\,dx + x + y$"
  dims_lete <- latex_dims(expr, math_font = "lete", gp = grid::gpar(fontsize = 20))
  dims_stix <- latex_dims(expr, math_font = "stix", gp = grid::gpar(fontsize = 20))
  w_lete <- grid::convertWidth(dims_lete$width, "bigpts", valueOnly = TRUE)
  w_stix <- grid::convertWidth(dims_stix$width, "bigpts", valueOnly = TRUE)
  expect_true(w_lete > 0)
  expect_true(w_stix > 0)
  expect_false(w_lete == w_stix)
})

# --- \def command ---

test_that("\\def defines a zero-argument macro and renders identically to \\newcommand", {
  # \def\mymacroA{x^2} should produce the same layout as \newcommand{\mymacroB}{x^2}
  layout_nc  <- parse_latex_cpp("\\newcommand{\\mymacroB}{x^2} \\mymacroB", text_size = 20)
  layout_def <- parse_latex_cpp("\\def\\mymacroA{x^2} \\mymacroA",           text_size = 20)
  expect_equal(nrow(layout_def), nrow(layout_nc))
})

test_that("\\def silently overwrites an existing macro, and the last wins", {
  # \newcommand errors on redefinition; \def replaces. Asserting only that
  # it does not error would pass if the *first* body were kept.
  got <- parse_latex_cpp(
    "\\def\\myoverwrite{x} \\def\\myoverwrite{y} \\myoverwrite", text_size = 20)
  expect_equal(got, parse_latex_cpp("y", text_size = 20))
  expect_false(isTRUE(all.equal(got, parse_latex_cpp("x", text_size = 20))))
})

test_that("an error inside a command's argument is reported, not swallowed", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # MicroTeX parsed every argument leniently and kept what came before an
  # error, silently: \text{a <bad array> b} drew "a", and a fraction or a
  # root lost its whole argument. It is a warning now, in an argument as at
  # the top level, and the command it broke is drawn as its name, in red.
  for (tex in c("\\text{a \\begin{array}{q}x\\end{array} b}",
                "\\frac{1}{\\begin{array}{q}x\\end{array}} + y",
                "\\sqrt{\\begin{array}{q}x\\end{array}}",
                "a \\begin{array}{q}x\\end{array} b")) {
    expect_warning(g <- latex_grob(tex, input_mode = "math"), "Invalid alignment",
                   label = tex)
    expect_true("#FF0000" %in% g$layout_df$color, label = tex)
  }
  # Only the bad part is lost: the text around it survives.
  expect_warning(
    d <- latex_grob("\\text{a \\begin{array}{q}x\\end{array} b}", input_mode = "math")$layout_df,
    "Invalid alignment"
  )
  expect_true(all(c("a ", " b") %in% d$text))
  # The deliberate leniency stays: an unknown command is drawn in red, in
  # an argument as at the top level, and the text around it survives (the
  # space after it goes, as TeX drops a space after a control word).
  expect_warning(
    d <- latex_grob("\\text{a \\nosuchcmd b}", input_mode = "math")$layout_df,
    "unknown command \\nosuchcmd", fixed = TRUE
  )
  expect_true(all(c("a ", "b") %in% d$text))
  expect_true("#FF0000" %in% d$color)
})

test_that("redefining a built-in lasts one label and never breaks the next", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  has_rule <- function(tex) "line" %in% latex_grob(tex, input_mode = "math")$layout_df$type
  text_of <- function(tex) {
    d <- latex_grob(tex, input_mode = "math")$layout_df
    d$text[!is.na(d$text)]
  }
  latex_cache_clear(); on.exit(latex_cache_clear(), add = TRUE)
  expect_true(has_rule("\\frac{1}{2}"))

  # As in LaTeX, \newcommand refuses a name that exists. It used to
  # replace the built-in -- and the per-label cleanup then deleted it, so
  # \frac stopped working in every later label of the session.
  expect_warning(rule <- has_rule("\\newcommand{\\frac}{Q}\\frac{1}{2}"), "already exists")
  expect_true(rule)
  latex_cache_clear()
  expect_true(has_rule("\\frac{1}{2}"))

  # \renewcommand and \def may redefine a built-in, for that label only.
  expect_true("R" %in% text_of("\\renewcommand{\\frac}{\\text{R}}\\frac"))
  latex_cache_clear()
  expect_true(has_rule("\\frac{1}{2}"))
  expect_true("D" %in% text_of("\\def\\sqrt{\\text{D}}\\sqrt"))
  latex_cache_clear()
  expect_false("D" %in% text_of("\\sqrt{x}"))
  # A built-in defined in LaTeX source (\degree) comes back too.
  expect_true("G" %in% text_of("\\renewcommand{\\degree}{\\text{G}}\\degree"))
  latex_cache_clear()
  expect_false("G" %in% text_of("90\\degree"))

  # \renewcommand of something that does not exist warns and, as LaTeX
  # does, defines it anyway.
  expect_warning(got <- text_of("\\renewcommand{\\nosuch}{\\text{Q}}\\nosuch"),
                 "\\nosuch was not defined; defined now", fixed = TRUE)
  expect_true("Q" %in% got)
})

test_that("a \\def with an invalid name warns and is dropped whole", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_warning(
    g <- latex_grob("\\def{notacontrolseq}{body}x", input_mode = "math"),
    "expected '\\' before the name", fixed = TRUE
  )
  # Nothing of the definition is drawn, only what follows it.
  expect_equal(nrow(g$layout_df), 1L)
})

test_that("\\def with sequential #1..#N parameters expands like \\newcommand[N]", {
  # Single-arg form: \def\sq#1{#1^2} should match \newcommand{\sq}[1]{#1^2}
  layout_nc1  <- parse_latex_cpp("\\newcommand{\\sqB}[1]{#1^2} \\sqB{a}", text_size = 20)
  layout_def1 <- parse_latex_cpp("\\def\\sqA#1{#1^2} \\sqA{a}",           text_size = 20)
  expect_equal(nrow(layout_def1), nrow(layout_nc1))

  # Two-arg form: \def\pair#1#2{#1+#2}
  layout_nc2  <- parse_latex_cpp("\\newcommand{\\pairB}[2]{#1+#2} \\pairB{a}{b}", text_size = 20)
  layout_def2 <- parse_latex_cpp("\\def\\pairA#1#2{#1+#2} \\pairA{a}{b}",         text_size = 20)
  expect_equal(nrow(layout_def2), nrow(layout_nc2))
})

test_that("\\def rejects non-sequential or malformed parameter patterns", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Each warns and is dropped whole, so only the trailing `y` is drawn --
  # not the parameter text and body as stray characters.
  rejected <- c(
    "\\def\\bad#2#1{#1#2}y" = "parameters must be sequential",   # #2 before #1
    "\\def\\skip#1#3{#1#3}y" = "parameters must be sequential",  # #1 then #3
    "\\def\\noarg#x{x}y" = "'#' must be followed by a digit"     # no digit
  )
  for (tex in names(rejected)) {
    expect_warning(g <- latex_grob(tex, input_mode = "math"), rejected[[tex]],
                   fixed = TRUE, label = tex)
    expect_equal(nrow(g$layout_df), 1L, label = tex)
  }
})

test_that("the typeface fallback warns once per device, not once per grob", {
  # The condition belongs to the device, but makeContent() runs per grob and
  # on every redraw, so a figure with several labels used to raise the same
  # warning once per label.
  tf <- tempfile(fileext = ".ps")
  grDevices::postscript(tf)
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  gridmicrotex:::.clear_typeface_noted()

  draw <- function() {
    g <- latex_grob("\\frac{a}{b}", render_mode = "typeface",
                    gp = grid::gpar(fontsize = 20))
    grid::grid.newpage()
    grid::grid.draw(g)
  }
  w <- character(0)
  withCallingHandlers(
    for (i in 1:5) draw(),
    message = function(cond) {
      w <<- c(w, conditionMessage(cond))
      invokeRestart("muffleMessage")
    }
  )
  fallback <- grep("falling back to path mode", w, value = TRUE)
  expect_length(fallback, 1L)

  # A different device is a different answer, so it is told too.
  tf2 <- tempfile(fileext = ".ps")
  grDevices::postscript(tf2)
  w2 <- character(0)
  withCallingHandlers(draw(), message = function(cond) {
    w2 <<- c(w2, conditionMessage(cond)); invokeRestart("muffleMessage")
  })
  grDevices::dev.off(); unlink(tf2)
  expect_length(grep("falling back to path mode", w2, value = TRUE), 1L)
})

test_that("the fallback is reported only when typeface was asked for", {
  # render_mode defaults to "typeface", so everyone lands in the fallback on
  # a device without glyphs. Reporting that would reach users who never
  # expressed a preference; only an unmet *explicit* request is worth a word.
  msgs <- function(expr) {
    got <- character(0)
    withCallingHandlers(expr, message = function(cond) {
      got <<- c(got, conditionMessage(cond)); invokeRestart("muffleMessage")
    })
    grep("falling back to path mode", got, value = TRUE)
  }
  on_ps <- function(expr) {
    f <- tempfile(fileext = ".ps")
    grDevices::postscript(f)
    on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
    gridmicrotex:::.clear_typeface_noted()
    msgs({ grid::grid.newpage(); expr() })
  }

  # asked for it explicitly -> told once
  expect_length(on_ps(function()
    grid.latex("$x^2$", render_mode = "typeface", gp = grid::gpar(fontsize = 20))), 1L)

  # inherited the default -> silent
  expect_length(on_ps(function()
    grid.latex("$x^2$", gp = grid::gpar(fontsize = 20))), 0L)

  # set globally via latex_options() counts as asking
  old <- latex_options(render_mode = "typeface")
  on.exit(reset_latex_options(), add = TRUE)
  expect_length(on_ps(function()
    grid.latex("$x^2$", gp = grid::gpar(fontsize = 20))), 1L)
})

test_that("a macro that expands to itself is a parse error, not a hang", {
  # Each expansion put the same text back and rewound to it, so the parser
  # never finished. \def rather than \newcommand, which refuses to redefine
  # and so would fail differently the second time the suite runs.
  for (tex in c("\\def\\gmloop{\\gmloop}\\gmloop",
                "\\def\\gmping{\\gmpong}\\def\\gmpong{\\gmping}\\gmping",
                "\\def\\gmgrow{x\\gmgrow}\\gmgrow")) {
    expect_error(parse_latex_cpp(tex, text_size = 20), "macro expansions",
                 label = tex)
  }
})

test_that("an unterminated $$ at the end of text mode lays out as empty", {
  # getGroup() stepped past the end of the string and its caller read one
  # character further. That read only shows under a sanitizer; this pins
  # down that bounding it changed nothing visible.
  expect_equal(nrow(parse_latex_cpp("\\text{$$}", text_size = 20)), 0L)
  a <- parse_latex_cpp("\\text{a$$}", text_size = 20)
  b <- parse_latex_cpp("\\text{a}", text_size = 20)
  expect_equal(attr(a, "bbox_width"), attr(b, "bbox_width"))
})

test_that("a colour with alpha of 50% or more keeps its colour", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  record_colour <- function(alpha) {
    p <- .parse_from_gp(
      tex = "$x$", math_font = "", max_width = 0, tex_style = "",
      render_mode = "path",
      gp = grid::gpar(fontsize = 20, col = grDevices::rgb(1, 0, 0, alpha)))
    unique(p$layout$color)
  }
  # Eight hex digits overflow a signed 32-bit long, which is what `long` is
  # on Windows, so every such colour came back opaque black.
  expect_equal(record_colour(0.3), "#FF00004D")
  expect_equal(record_colour(0.8), "#FF0000CC")
})

test_that("an empty argument is an empty box, as it was in 0.1.1", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # These wrapped the null an empty argument gives, and crashed R.
  glyphs <- function(tex) sum(latex_tree(tex, input_mode = "math")$records$type == "glyph")
  expect_equal(glyphs("\\mathop{}\\!\\mathrm{d}x"), 2)
  expect_equal(glyphs("f\\mathopen{}\\left(x\\right)\\mathclose{}"), 4)
  expect_equal(glyphs("a\\reflectbox{}b"), 2)
  expect_equal(glyphs("a\\raisebox{1pt}{}b"), 2)
})

test_that("an argument read a second time keeps its text", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # \ensuremath in math and an optional argument read their tokens again,
  # and an argument recorded the second time came out empty.
  fills <- function(tex) {
    r <- latex_tree(tex, input_mode = "math")$records
    unique(r$color[r$type != "glyph"])
  }
  expect_identical(fills("\\ensuremath{\\colorbox{yellow}{x}}"), fills("\\colorbox{yellow}{x}"))
  expect_true("#FF0000" %in% latex_tree("\\sqrt[\\textcolor{red}{3}]{x}", input_mode = "math")$records$color)
})

test_that("a number too large, or NaN, still gives a grob that draws", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  for (tex in c(paste0("a\\hspace{", strrep("9", 45), "pt}b"), "a\\scalebox{1e300}{x}b",
                "a\\rotatebox{nan}{x}b")) {
    expect_true(all(is.finite(unlist(latex_tree(tex, input_mode = "math")$bbox))), info = tex)
  }
  expect_true(all(is.finite(unlist(latex_tree("a \\relscale{1e30} b")$bbox))))
})

test_that("what a label defines is its own", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  invisible(latex_tree("\\definecolor{red}{rgb}{0,0,1}x"))
  expect_identical(unique(latex_tree("\\textcolor{red}{still red}")$records$color), "#FF0000")
  invisible(latex_tree("\\arrayrulecolor{blue}\\begin{array}{c}\\hline b\\end{array}", input_mode = "math"))
  r <- latex_tree("\\begin{array}{c}\\hline still black\\end{array}", input_mode = "math")$records
  expect_identical(unique(r$color[r$type != "glyph"]), "#000000")
  invisible(latex_tree("\\breakEverywhere{true}x"))
  expect_identical(nrow(latex_tree("one run of words")$records), 1L)
})
