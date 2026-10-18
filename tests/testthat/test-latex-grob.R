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

  # pdf() does not embed the font and postscript() has no glyphs, so both
  # fall back with a message. Each device is closed on every way out.
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
    # And it draws outlines, not glyphs.
    expect_true("pathgrob" %in% kinds(g), info = dev)
    expect_false("glyphgrob" %in% kinds(g), info = dev)
  }
  for (dev in c("pdf", "postscript")) falls_back_on(dev)

  # cairo_pdf() embeds the font, so the math stays text.
  skip_if_not(capabilities("cairo"))
  tf <- tempfile(fileext = ".pdf")
  open_before <- length(grDevices::dev.list())
  # A Mac without XQuartz reports cairo but opens no device.
  suppressWarnings(grDevices::cairo_pdf(tf))
  skip_if(length(grDevices::dev.list()) == open_before, "cairo_pdf() opened no device")
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  expect_true(gridmicrotex:::.device_supports_typeface_glyphs())
  expect_true("glyphgrob" %in% kinds(frac()))
})

test_that("the text measurer's ascent and descent are the device's own", {
  # Equal to measuring the whole string. On png(): pdf() cannot encode
  # these, which is an error under --as-cran.
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

test_that("max_width, hjust and vjust must be single valid values", {
  for (bad in list(NaN, NA_real_, -1, c(1, 2), "a")) {
    expect_error(latex_dims("x", max_width = bad), "single non-negative number")
  }
  expect_error(latex_grob("x", hjust = c(0, 1)), "hjust must be")
  expect_error(latex_grob("x", vjust = NA_real_), "vjust must be")
})

test_that("latex_cache_limit() takes a whole number", {
  old <- latex_cache_limit()
  on.exit(latex_cache_limit(old), add = TRUE)
  expect_error(latex_cache_limit(1.9), "non-negative integer")
  expect_error(latex_cache_limit(c(1, 2)), "non-negative integer")
  expect_error(latex_cache_limit(-1), "non-negative integer")
  expect_error(latex_cache_limit(Inf), "non-negative integer")
  expect_silent(latex_cache_limit(256))
})

# --- editGrob: re-parse on parse-affecting fields ---

test_that("editGrob re-parses when tex changes", {
  g <- latex_grob("x", render_mode = "path")
  g2 <- grid::editGrob(g, tex = "$x^{2} + y^{2} + z^{2}$")
  expect_equal(g2$tex, "$x^{2} + y^{2} + z^{2}$")
  expect_true(g2$bbox_w > g$bbox_w)
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

test_that("the typeface fallback warns once per device, not once per grob", {
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

  # A different device is told too.
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

test_that("a colour with alpha of 50% or more keeps its colour", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  record_colour <- function(alpha) {
    p <- .parse_from_gp(
      tex = "$x$", math_font = "", max_width = 0, tex_style = "",
      render_mode = "path",
      gp = grid::gpar(fontsize = 20, col = grDevices::rgb(1, 0, 0, alpha)))
    unique(p$layout$color)
  }
  # Eight hex digits overflow a 32-bit long (Windows).
  expect_equal(record_colour(0.3), "#FF00004D")
  expect_equal(record_colour(0.8), "#FF0000CC")
})

test_that("latex_tree honours justify like latex_dims", {
  tex <- paste(rep("alpha beta gamma", 6), collapse = " ")
  d <- latex_dims(tex, max_width = 150, justify = TRUE)
  tr <- latex_tree(tex, max_width = 150, justify = TRUE)
  expect_equal(tr$bbox[["width"]],
               grid::convertWidth(d$width, "bigpts", valueOnly = TRUE),
               tolerance = 1e-6)
  expect_gt(tr$bbox[["width"]],
            latex_tree(tex, max_width = 150)$bbox[["width"]])
})

