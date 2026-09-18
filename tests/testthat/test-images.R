# \includegraphics: resolving a file reference into a sized box, and
# drawing whatever that file deserves.
#
# The resolver runs before every other rewriting step, so most of what is
# under test here is that a real path survives contact with the pipeline.

# A PNG of a known pixel size. `width`/`height` are PIXELS for agg_png,
# which is easy to get wrong -- 1 means one pixel, not one inch.
mk_png <- function(w_px = 300, h_px = 200) {
  f <- tempfile(fileext = ".png")
  ragg::agg_png(f, width = w_px, height = h_px)
  grid::grid.rect(gp = grid::gpar(fill = "tomato", col = NA))
  grDevices::dev.off()
  f
}

mk_svg <- function(w_in = 3, h_in = 2) {
  f <- tempfile(fileext = ".svg")
  svglite::svglite(f, width = w_in, height = h_in)
  grid::grid.circle(r = 0.4, gp = grid::gpar(fill = "steelblue", col = NA))
  grDevices::dev.off()
  f
}

# Warnings are suppressed here on purpose: sizing a small test image to a
# few inches legitimately trips the low-resolution notice, and that notice
# has its own test below. These helpers are about geometry.
wid <- function(tex, ...) {
  suppressWarnings(as.numeric(latex_dims(tex, input_mode = "math", ...)$width))
}
hei <- function(tex, ...) {
  suppressWarnings(as.numeric(latex_dims(tex, input_mode = "math", ...)$height))
}

test_that("a length is read in every unit LaTeX writes one in", {
  bp <- gridmicrotex:::.tex_len_bp
  expect_equal(bp("3in", 20, 0), 216)
  expect_equal(bp("216bp", 20, 0), 216)
  expect_equal(bp("72pt", 20, 0), 72 * 72 / 72.27)
  expect_equal(bp("2.54cm", 20, 0), 72, tolerance = 1e-6)
  expect_equal(bp("25.4mm", 20, 0), 72, tolerance = 1e-6)
  expect_equal(bp("96px", 20, 0), 72)
  expect_equal(bp("2em", 20, 0), 40)
  expect_equal(bp("12", 20, 0), 12)      # bare number is big points

  # \textwidth has no page in a grob, so it resolves against max_width;
  # without one it is unreadable and the caller falls back to the file's
  # own size rather than dropping the figure.
  expect_equal(bp("\\textwidth", 20, 400), 400)
  expect_equal(bp("0.5\\textwidth", 20, 400), 200)
  expect_equal(bp("0.5\\linewidth", 20, 400), 200)
  expect_true(is.na(bp("\\textwidth", 20, 0)))

  # Unreadable rather than guessed.
  expect_true(is.na(bp("3furlongs", 20, 0)))
  expect_true(is.na(bp("", 20, 0)))
})

test_that("a file reference survives the pipeline that would mangle a path", {
  enc <- gridmicrotex:::.image_ref_encode
  dec <- gridmicrotex:::.image_ref_decode
  # Hex, because every one of these breaks a raw path: backslashes are
  # LaTeX escapes, `%` starts a comment that .strip_document_wrappers()
  # would run to end of line, and .expand_macros() would rewrite `\Users`
  # for anyone who had defined a macro of that name.
  for (p in c("C:\\Users\\a\\my fig.png", "a/b/100%plot.png",
              "with space_and_under.png", "plain.png")) {
    expect_equal(dec(enc(p)), p, info = p)
  }
  # Hex is safe in a LaTeX argument: no backslash, brace or percent.
  expect_match(enc("C:\\Users\\a b%c.png"), "^[0-9a-f]+$")
})

test_that("an image reserves a box of exactly the requested size", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  f <- mk_png(300, 200)

  # Intrinsic: pixels at 96 dpi, the convention the block path already used.
  expect_equal(wid(sprintf("\\includegraphics{%s}", f)), 300 * 72 / 96)
  expect_equal(hei(sprintf("\\includegraphics{%s}", f)), 200 * 72 / 96)

  # Absolute, and independent of font size -- it is a picture, not type.
  for (fs in c(10, 40)) {
    expect_equal(
      wid(sprintf("\\includegraphics[width=3in]{%s}", f),
          gp = grid::gpar(fontsize = fs)), 216, info = fs)
  }

  # Aspect is preserved when only one side is given, and deliberately not
  # when both are, as in LaTeX.
  expect_equal(hei(sprintf("\\includegraphics[width=3in]{%s}", f)), 144)
  expect_equal(wid(sprintf("\\includegraphics[height=1in]{%s}", f)), 108)
  # 225 * 0.5 = 112.5; the bbox is reported in whole big points.
  expect_equal(wid(sprintf("\\includegraphics[scale=0.5]{%s}", f)), 112)
  expect_equal(hei(sprintf("\\includegraphics[width=3in,height=1in]{%s}", f)), 72)

  # It sits on the baseline: no depth, as \includegraphics does in LaTeX.
  d <- latex_dims(sprintf("\\includegraphics[width=1in]{%s}", f),
                  input_mode = "math")
  expect_equal(as.numeric(d$depth), 0)
})

test_that("an oversized image is clamped so a markdown column can converge", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  f <- mk_png(300, 200)
  # .md_measure() shrinks max_width and retries up to six times; an
  # absolutely sized image never shrinks, so without a clamp the loop
  # burns six parses and still overflows.
  expect_equal(wid(sprintf("\\includegraphics[width=10in]{%s}", f),
                   max_width = 200), 200)
  expect_lte(wid(sprintf("\\includegraphics{%s}", f), max_width = 100), 100)
})

test_that("the record and the grob carry the file through", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  f <- mk_png()
  g <- latex_grob(sprintf("\\includegraphics[width=1in]{%s}", f),
                  input_mode = "math")
  d <- g$layout_df
  expect_true("image" %in% d$type)
  row <- d[d$type == "image", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$width, 72)
  expect_equal(gridmicrotex:::.image_ref_decode(row$image_ref), f)

  kids <- grid::makeContent(g)$children
  expect_true(any(vapply(kids, inherits, logical(1), "rastergrob")))
})

test_that("an SVG is drawn as real vector, not a raster", {
  skip_if_not_installed("svglite")
  skip_if_not_installed("rsvg")
  skip_if_not_installed("grImport2")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  f <- mk_svg(3, 2)

  # Intrinsic size comes from the SVG header, which svglite writes in pt.
  expect_equal(wid(sprintf("\\includegraphics{%s}", f)), 216)
  expect_equal(hei(sprintf("\\includegraphics{%s}", f)), 144)

  # Installed is not the same as working: a container can carry rsvg and
  # grImport2 while librsvg cannot actually turn the file into a Picture,
  # and the loader then correctly falls back to a raster. Probe the real
  # conversion before asserting a vector came out of it, or this fails on
  # the environment rather than on the package. Seen on R-hub's nold
  # container, where all three packages are installed.
  # A Picture object is not enough: it has to carry drawable content.
  # librsvg on R-hub's nold container returns an empty one, and the
  # loader then correctly falls back to a raster.
  can_vector <- tryCatch(suppressWarnings({
    cairo <- tempfile(fileext = ".svg")
    rsvg::rsvg_svg(f, cairo)
    pic <- grImport2::readPicture(cairo)
    inherits(pic, "Picture") && length(pic@content) > 0
  }), error = function(e) FALSE)
  skip_if(!isTRUE(can_vector),
          "rsvg/grImport2 cannot turn an SVG into a drawable Picture here")

  kids <- grid::makeContent(
    latex_grob(sprintf("\\includegraphics[width=1in]{%s}", f),
               input_mode = "math"))$children
  expect_length(kids, 1L)
  # grImport2 turns the file into grid drawing primitives; a raster would
  # be a rastergrob, and would not scale with the device. How deep the
  # primitives sit depends on the SVG, so walk the whole subtree rather
  # than assuming a fixed nesting, and report the tree when it is absent.
  expect_false(inherits(kids[[1]], "rastergrob"))
  grob_classes <- function(g) {
    c(class(g)[1], unlist(lapply(g$children, grob_classes), use.names = FALSE))
  }
  prims <- grob_classes(kids[[1]])
  expect_true(any(grepl("^pic", prims)),
              info = paste("grob tree:", paste(prims, collapse = " / ")))

  # The wrapper must carry a plain viewport, or .md_shift_grob() -- which
  # realigns a block by nudging vp$x -- would hit pictureGrob's vpStack,
  # whose x is NULL.
  expect_false(is.null(kids[[1]]$vp))
  expect_false(is.null(kids[[1]]$vp$x))
})

test_that("an SVG falls back to a raster when there is no picture reader", {
  skip_if_not_installed("svglite")
  skip_if_not_installed("rsvg")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  f <- mk_svg(2, 1)

  # This branch is for machines that have rsvg but not grImport2. Every
  # other SVG test skips without grImport2, so with it installed the
  # fallback would never execute and would rot unnoticed -- mock the
  # picture reader away to force it.
  # `.package` is given explicitly: without it testthat infers the target
  # from a pkgload context, so this block errored with "No packages
  # loaded with pkgload" under a plain test_dir() / test_file() run --
  # green under devtools::test() and R CMD check, which both supply one.
  testthat::local_mocked_bindings(.image_picture = function(...) NULL,
                                  .package = "gridmicrotex")
  g <- .image_grob(f, 144, 72)
  expect_s3_class(g, "rastergrob")
  # Rasterised at the device's resolution rather than a size baked in when
  # the file was written -- the one thing a vector source makes possible.
  expect_gt(nrow(g$raster), 1L)
  expect_gt(ncol(g$raster), 1L)

  # And with neither reader there is nothing to draw: the grob is NULL and
  # the grid builder warns that the box was left blank.
  testthat::local_mocked_bindings(.image_raster = function(...) NULL,
                                  .package = "gridmicrotex")
  expect_null(.image_grob(tempfile(fileext = ".png"), 72, 72))
})

test_that("an image that cannot be drawn is an error saying why", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # A figure that was asked for and not drawn is easy to miss in the
  # output, so it stops instead of drawing a stand-in. A URL is a path like
  # any other and is not fetched, so it fails the same way.
  for (f in c("no-such-file.png", "https://example.org/fig.png")) {
    expect_error(latex_grob(sprintf("\\includegraphics{%s}", f),
                            input_mode = "math"),
                 "file not found", label = f)
  }

  # A format with no reader has nothing else to ask, so it is refused here.
  for (ext in c(".pdf", ".tiff")) {
    f <- tempfile(fileext = ext); writeBin(as.raw(1:20), f)
    expect_error(latex_dims(sprintf("\\includegraphics{%s}", f),
                            input_mode = "math"),
                 "not a PNG, JPEG or SVG file", label = ext)
  }
})

test_that("a commented-out \\includegraphics is not read", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Commenting out a figure is routine LaTeX. The resolver runs before the
  # comments are stripped, so it has to skip them itself or the missing
  # draft figure stops the whole render.
  expect_equal(wid("x % \\includegraphics{old.png}\ny"), wid("xy"))
  doc <- c("\\documentclass{article}", "\\begin{document}", "Text",
           "% \\includegraphics{draft.png}", "\\end{document}")
  expect_equal(as.numeric(latex_dims(paste(doc, collapse = "\n"))$width),
               as.numeric(latex_dims(paste(doc[-4], collapse = "\n"))$width))
  # `\\` is a line break, so the `%` after it still starts a comment...
  expect_equal(wid("x\\\\% \\includegraphics{old.png}\ny"), wid("x\\\\y"))
  # ...but an escaped `\%` is a percent sign, and the figure is real.
  expect_error(latex_dims("50\\% \\includegraphics{nope.png}", input_mode = "math"),
               "file not found")
  # Likewise `\\includegraphics` is a line break and then a word, not the
  # command; a third backslash makes it the command again.
  expect_equal(gridmicrotex:::.resolve_graphics("a\\\\includegraphics{x}", 20, 0),
               "a\\\\includegraphics{x}")
  expect_error(latex_dims("a\\\\\\includegraphics{nope.png}", input_mode = "math"),
               "file not found")
})

test_that("one warning per file, however many times the layout is measured", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # .md_measure() calls latex_dims() up to seven times for a single render
  # and editDetails() re-runs the pipeline, so deduplicating per parse
  # would not deduplicate at all.
  tex <- sprintf("\\includegraphics[trim=1 2 3 4]{%s}", mk_png())
  seen <- character(0)
  withCallingHandlers(
    for (i in 1:5) latex_dims(tex, input_mode = "math"),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  expect_length(seen, 1L)
})

test_that("effective resolution is reported only when the author chose a size", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  warns <- function(expr) {
    ws <- character(0)
    withCallingHandlers(expr, warning = function(w) {
      ws <<- c(ws, conditionMessage(w)); invokeRestart("muffleWarning") })
    ws
  }
  small <- mk_png(96, 64)
  # Asked for 3in from 96px: 32 dpi, and the fix is more pixels.
  expect_match(warns(latex_dims(sprintf("\\includegraphics[width=3in]{%s}", small),
                                input_mode = "math"))[1], "dpi")
  # At intrinsic size the answer is 96 dpi by construction, so there is
  # nothing to act on and nothing to say.
  expect_length(warns(latex_dims(sprintf("\\includegraphics{%s}", mk_png(96, 64)),
                                 input_mode = "math")), 0L)
  # A vector source has no effective resolution at all.
  skip_if_not_installed("svglite"); skip_if_not_installed("rsvg")
  expect_length(warns(latex_dims(sprintf("\\includegraphics[width=9in]{%s}", mk_svg()),
                                 input_mode = "math")), 0L)
})

test_that("an \\includegraphics inside a macro definition still resolves", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # The resolver sees this one after all: the command sits literally in the
  # \newcommand's replacement text, which is just characters in the string.
  # So it becomes a real image, not a fallback.
  f <- mk_png()
  d <- latex_grob(sprintf("\\newcommand{\\fig}{\\includegraphics[width=1in]{%s}}x\\fig y", f),
                  input_mode = "math")$layout_df
  expect_true("image" %in% d$type)
})

test_that("an \\includegraphics the resolver cannot read is an error", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Two kinds reach MicroTeX unread: malformed input -- no braces, or
  # unbalanced ones -- which the R scanner leaves alone rather than guess
  # at, and one a \newcommand or \def produces, which MicroTeX expands
  # after both resolver passes. The vendored stub drew nothing; the
  # override reports them, and they fail like every other image that cannot
  # be drawn. In "mixed" mode too, where the prose goes inside \text{} --
  # MicroTeX parses that leniently and swallowed an exception thrown there,
  # with the rest of the text.
  for (tex in c("x\\includegraphics{unbalanced", "x\\includegraphics y",
                "\\newcommand{\\ig}{\\includegraphics}\\ig{nope.png}",
                "\\newcommand{\\fig}[1]{\\includegraphics{#1}}\\fig{nope.png}",
                "\\def\\fig#1{\\includegraphics{#1}}\\fig{nope.png}")) {
    for (mode in c("math", "mixed")) {
      expect_error(latex_grob(tex, input_mode = mode), "written directly",
                   label = paste(mode, tex))
    }
  }
  # Where images only warn (base graphics, a box mid-draw), it warns and
  # draws the argument, as for any image.
  .image_reset_warnings(); on.exit(.image_reset_warnings(), add = TRUE)
  alias <- "\\newcommand{\\ig}{\\includegraphics}\\ig{nope.png}"
  expect_warning(g <- .images_lenient(latex_grob(alias, input_mode = "math")),
                 "drawing the file name instead")
  expect_true("nope.png" %in% g$layout_df$text)
  expect_false(is.null(.gm_base_layout(paste("$x$", alias), 12)))
  # And the resolver really did leave them: these are not rewritten forms.
  # A macro parameter is not a file, so it is not read as one either.
  for (tex in c("x\\includegraphics{unbalanced", "x\\includegraphics y",
                "\\def\\fig#1{\\includegraphics{#1}}")) {
    expect_match(gridmicrotex:::.resolve_graphics(tex, 20, 0),
                 "includegraphics", fixed = TRUE)
  }
})

test_that("the layout cache notices a file that changed on disk", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  f <- tempfile(fileext = ".png")
  ragg::agg_png(f, width = 300, height = 200); grid::grid.rect(); dev.off()
  latex_cache_clear()
  w1 <- wid(sprintf("\\includegraphics{%s}", f))
  before <- latex_cache_info()$hits
  wid(sprintf("\\includegraphics{%s}", f))
  expect_equal(latex_cache_info()$hits - before, 1L)   # same file: a hit

  # The cache key is the tex string, which holds no file metadata of its
  # own -- so mtime and size ride along in the encoded reference. Without
  # them an edited figure would keep its old layout forever.
  Sys.sleep(1.1)
  ragg::agg_png(f, width = 600, height = 200); grid::grid.rect(); dev.off()
  expect_false(identical(wid(sprintf("\\includegraphics{%s}", f)), w1))
})

test_that("markdown draws a real image", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  f <- mk_png()
  tex <- gridmicrotex:::.md_to_tex(sprintf("a ![ALT](%s) b", f))
  expect_match(tex, "includegraphics", fixed = TRUE)

  # <img> reaches the same place, and HTML sizes it in pixels.
  expect_match(gridmicrotex:::.md_to_tex(sprintf("a <img src='%s' width='48'> b", f)),
               "width=48px", fixed = TRUE)

  # An <img> alone on its line is an HTML *block* to CommonMark, not an
  # inline tag, and HTML blocks are otherwise dropped. It is drawn, as it
  # is in a browser.
  expect_match(gridmicrotex:::.md_to_tex(sprintf("<img src='%s'>", f)),
               "includegraphics", fixed = TRUE)
  blk <- gridmicrotex:::.md_parse_blocks(sprintf("Intro\n\n<img src='%s'>\n\nEnd", f))
  expect_match(blk[[2]]$tex, "includegraphics", fixed = TRUE)
})

test_that("markdown that names an image it cannot draw is an error", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Every spelling fails the same way, a URL included, and none falls back
  # to its alt text. An <img> with no src names no file at all.
  for (md in c("a ![ALT](nope.png) b", "a ![ALT](https://x/y.png) b",
               "a <img src='nope.png' alt='ALT'> b", "a <img alt='ALT'> b",
               "<img src='nope.png'>", "a <img src='nope.png' alt='a > b'> c")) {
    expect_error(markdown_grob(md), "file not found", label = md)
  }
  # The block renderer lays out only when drawn, so it checks when it is
  # built: an error from inside a draw would leave half a page. That
  # includes an \includegraphics written inside a math span.
  for (md in c("![ALT](nope.png)", "text ![ALT](nope.png) more",
               "Intro\n\n<img src='nope.png'>\n\nEnd",
               "see $\\includegraphics{nope.png}$")) {
    expect_error(markdown_box_grob(md), "file not found", label = md)
  }
  # Code is literal: an \includegraphics shown there names no figure.
  expect_match(gridmicrotex:::.md_to_tex("`$\\includegraphics{nope.png}$`"),
               "\\texttt{", fixed = TRUE)
})

test_that("markdown reads an image's path the way HTML and CommonMark do", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  d <- tempfile("paths"); dir.create(d)
  put <- function(name) { p <- file.path(d, name); file.copy(mk_png(), p); p }
  drawn <- function(md) "image" %in% markdown_grob(md)$layout_df$type

  # A `$...$` pair in a file name is masked as math before CommonMark
  # runs, and has to be put back or the name comes out as "a1c.png".
  dollar <- put("a$b$c.png")
  expect_true(drawn(sprintf("x ![a](%s) y", dollar)))
  expect_true(drawn(sprintf("x <img src='%s'> y", dollar)))
  blk <- gridmicrotex:::.md_parse_blocks(sprintf("![a](%s)", dollar))[[1]]
  expect_identical(blk$path, dollar)

  # A brace in a file name must not end the \includegraphics argument.
  expect_true(drawn(sprintf("x ![a](%s) y", put("a}b.png"))))
  expect_true(drawn(sprintf("x ![a](%s) y", put("a{b.png"))))

  # HTML attributes: names are case-insensitive, values may be unquoted
  # or padded with spaces, and `data-src` is not `src`.
  f <- put("ok.png")
  for (tag in c("<img src=%s>", "<img SRC='%s'>", "<img src=' %s '>",
                "<img data-src='nope.png' src='%s'>",
                "<img src='%s' alt='a > b'>")) {
    expect_true(drawn(paste("x", sprintf(tag, f), "y")), label = tag)
  }
})

test_that("keepaspectratio fits inside the box instead of stretching it", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  f <- mk_png(300, 200)                     # 3:2, intrinsic 225 x 150 bp
  # Without the key, both lengths are obeyed and the picture distorts --
  # that is what LaTeX does too.
  expect_equal(wid(sprintf("\\includegraphics[width=3in,height=3in]{%s}", f)), 216)
  expect_equal(hei(sprintf("\\includegraphics[width=3in,height=3in]{%s}", f)), 216)
  # With it, the smaller scale factor wins and the whole picture fits.
  ka <- sprintf("\\includegraphics[width=3in,height=3in,keepaspectratio]{%s}", f)
  expect_equal(wid(ka), 216)
  expect_equal(hei(ka), 144)
  # It is a graphicx boolean, so `=false` is the plain behaviour again.
  expect_equal(hei(sprintf(
    "\\includegraphics[width=3in,height=3in,keepaspectratio=false]{%s}", f)), 216)
})

test_that("scale multiplies width and height rather than replacing them", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  f <- mk_png(300, 200)
  # graphicx applies scale to whatever width/height settled on.
  expect_equal(wid(sprintf("\\includegraphics[width=4in,scale=0.5]{%s}", f)), 144)
  expect_equal(hei(sprintf("\\includegraphics[width=4in,scale=0.5]{%s}", f)), 96)
})

test_that("a path is found the way graphicx finds one", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  dir <- tempfile("gfx"); dir.create(file.path(dir, "sub"), recursive = TRUE)
  file.copy(mk_png(300, 200), file.path(dir, "sub", "fig.png"))
  old <- setwd(dir); on.exit(setwd(old), add = TRUE)

  # No extension: the idiomatic LaTeX form, and the one that used to warn.
  expect_match(gridmicrotex:::.resolve_graphics("\\includegraphics{sub/fig}", 20, 0),
               "gmgraphics", fixed = TRUE)
  # \graphicspath supplies the directory, and is consumed rather than
  # typeset -- MicroTeX has no such command and would draw its argument.
  out <- gridmicrotex:::.resolve_graphics(
    "\\graphicspath{{sub/}}a\\includegraphics{fig}b", 20, 0)
  expect_match(out, "gmgraphics", fixed = TRUE)
  expect_false(grepl("graphicspath", out, fixed = TRUE))
  expect_false(grepl("sub/", out, fixed = TRUE))
  # ...including when there is no image at all to trigger the scan.
  expect_equal(gridmicrotex:::.resolve_graphics("\\graphicspath{{sub/}}text", 20, 0),
               "text")
})

test_that("the starred and two-argument spellings are the same command", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  f <- mk_png(300, 200)
  # Unmatched, these rendered the option list and the raw path as text.
  expect_equal(wid(sprintf("\\includegraphics*[width=3in]{%s}", f)), 216)
  # The older `[llx,lly][urx,ury]` spelling: two groups, no recognised key,
  # so the picture comes out at its own size rather than as literal text.
  expect_equal(wid(sprintf("\\includegraphics[0,0][10,10]{%s}", f)), 225)
  expect_equal(wid(sprintf("\\includegraphics[0,0][width=3in]{%s}", f)), 216)
})

test_that("an option that changes the picture warns rather than being dropped", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  warns <- function(expr) {
    ws <- character(0)
    withCallingHandlers(expr, warning = function(w) {
      ws <<- c(ws, conditionMessage(w)); invokeRestart("muffleWarning") })
    ws
  }
  # A cropped figure drawn whole is wrong in a way nothing in the output
  # hints at, so it has to say so. (`angle` is not in this set: it is a real
  # rotation now -- see the rotation test above.)
  w <- warns(latex_dims(sprintf("\\includegraphics[origin=c,width=1in]{%s}",
                                mk_png(300, 200)), input_mode = "math"))
  expect_match(paste(w, collapse = " "), "origin")
  w <- warns(latex_dims(sprintf("\\includegraphics[trim=1 2 3 4,clip,width=1in]{%s}",
                                mk_png(300, 200)), input_mode = "math"))
  expect_match(paste(w, collapse = " "), "trim")
  expect_match(paste(w, collapse = " "), "clip")
})

test_that("\\textwidth without max_width keeps the figure at its own size", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  f <- mk_png(300, 200)
  tex <- sprintf("\\includegraphics[width=\\textwidth]{%s}", f)
  ws <- character(0)
  w <- withCallingHandlers(
    suppressMessages(as.numeric(latex_dims(tex, input_mode = "math")$width)),
    warning = function(cnd) {
      ws <<- c(ws, conditionMessage(cnd)); invokeRestart("muffleWarning") })
  # Dropping the figure entirely was the old behaviour; the intrinsic size
  # is nearer to what the document meant, and the warning says so.
  expect_equal(w, 300 * 72 / 96)
  expect_match(paste(ws, collapse = " "), "textwidth")
  # ...and it is not also reported as low-resolution: at intrinsic size the
  # answer is 96 dpi by construction.
  expect_false(any(grepl("dpi", ws)))
})

test_that("an SVG no reader can draw is refused at parse time, not at draw time", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Well-formed XML with a readable width/height, but not an SVG. This is
  # the shape of every "the header parsed but nothing can draw it" case,
  # including a user with no rsvg installed. Sizing a box for it would
  # reserve space and then draw nothing into it -- a silent hole, which is
  # the one outcome this whole path exists to prevent.
  dir <- tempfile("gfx"); dir.create(dir)
  f <- file.path(dir, "notanimage.svg")
  writeLines("<notsvg width='72pt' height='36pt'><x/></notsvg>", f)
  expect_error(gridmicrotex:::.image_dims(f))
  expect_error(latex_grob(sprintf("\\includegraphics{%s}", f), input_mode = "math"),
               "notanimage.svg", fixed = TRUE)
  # Markdown refuses it for the same reason.
  expect_error(gridmicrotex:::.md_to_tex(sprintf("a ![ALT](%s) b", f)),
               "notanimage.svg", fixed = TRUE)

  # An <svg> root in some other namespace is not an SVG either: rsvg
  # rejects it, so it drew a blank box and blamed the file for changing.
  skip_if_not_installed("rsvg")
  foreign <- file.path(dir, "foreign.svg")
  writeLines("<svg xmlns='http://example.org/x' width='72pt' height='36pt'/>",
             foreign)
  expect_error(latex_dims(sprintf("\\includegraphics{%s}", foreign),
                          input_mode = "math"), "no <svg> root")
  # The SVG namespace, declared or left implicit, is fine.
  for (ns in c("", " xmlns='http://www.w3.org/2000/svg'")) {
    ok <- file.path(dir, "ok.svg")
    writeLines(sprintf("<svg%s width='72pt' height='36pt'/>", ns), ok)
    expect_equal(wid(sprintf("\\includegraphics{%s}", ok)), 72, label = ns)
  }
})

test_that("a rotated image is drawn rotated, not dropped", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  f <- mk_png(300, 200)
  rec <- function(tex) {
    d <- suppressWarnings(latex_tree(tex, input_mode = "math")$records)
    d[d$type == "image", ]
  }
  # \rotatebox used to delete the figure outright: no record, no warning,
  # nothing on the page. Everything else survives a rotation -- rects become
  # paths, rules become lines -- so the image was the sole exception.
  r <- rec(sprintf("\\rotatebox{30}{\\includegraphics[width=1in]{%s}}", f))
  expect_equal(nrow(r), 1L)
  expect_equal(r$rotation, -30, tolerance = 1e-3)   # y-down, so negated

  # `angle=` is the same thing spelled the graphicx way, and is routed to
  # \rotatebox rather than warned about.
  expect_equal(rec(sprintf("\\includegraphics[angle=30,width=1in]{%s}", f))$rotation,
               -30, tolerance = 1e-3)
  expect_equal(rec(sprintf("\\includegraphics[width=1in]{%s}", f))$rotation, 0)
  expect_match(gridmicrotex:::.resolve_graphics(
    sprintf("\\includegraphics[angle=45]{%s}", f), 20, 0), "rotatebox", fixed = TRUE)
  # A whole turn is not a rotation.
  expect_false(grepl("rotatebox", gridmicrotex:::.resolve_graphics(
    sprintf("\\includegraphics[angle=360]{%s}", f), 20, 0), fixed = TRUE))

  # The surrounding box grows to the rotated bounds, as in LaTeX.
  flat <- suppressWarnings(latex_dims(sprintf("\\includegraphics[width=1in]{%s}", f),
                                      input_mode = "math"))
  turned <- suppressWarnings(latex_dims(sprintf("\\includegraphics[angle=30,width=1in]{%s}", f),
                                        input_mode = "math"))
  expect_gt(as.numeric(turned$height), as.numeric(flat$height))

  # And it reaches the page as a grob in a rotated viewport.
  g <- suppressWarnings(latex_grob(sprintf("\\includegraphics[angle=30,width=1in]{%s}", f),
                                   input_mode = "math"))
  kid <- grid::makeContent(g)$children[[1]]
  expect_equal(kid$vp$angle, 30, tolerance = 1e-3)
})

test_that("a size that cannot be drawn says why instead of vanishing quietly", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  warns <- function(tex) {
    ws <- character(0)
    withCallingHandlers(latex_dims(tex, input_mode = "math"),
      warning = function(w) { ws <<- c(ws, conditionMessage(w)); invokeRestart("muffleWarning") })
    paste(ws, collapse = " ")
  }
  G <- function(o) sprintf("\\includegraphics%s{%s}", o, mk_png(300, 200))
  # An unreadable length was the dangerous one: the arithmetic ignores it,
  # so a typo came out at the file's own size with nothing said.
  expect_match(warns(G("[width=abc]")), "Cannot read")
  expect_match(warns(G("[width=3 inches]")), "Cannot read")
  expect_match(warns(G("[width=0]")), "nothing to draw")
  expect_match(warns(G("[width=-1in]")), "nothing to draw")
  # 1e-5bp is positive but writes as "0.0000" at the four decimal places the
  # reference carries, so it reserved a box of nothing.
  expect_match(warns(G("[width=0.00001bp]")), "nothing to draw")
  expect_match(warns(G("[scale=-2]")), "positive number")

  # A width no pixel count can express must degrade, not error: the dpi
  # advice used sprintf("%d") on it.
  expect_no_error(suppressWarnings(latex_dims(G("[width=1e7in]"), input_mode = "math")))
})

test_that("an unreadable file is reported in the reader's own words", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  skip_if_not_installed("png"); skip_if_not_installed("jpeg")
  why <- function(path) tryCatch({
    gridmicrotex:::.resolve_graphics(sprintf("\\includegraphics{%s}", path), 20, 0)
    ""
  }, error = conditionMessage)
  dir <- tempfile("gfx"); dir.create(dir)
  # The reader knows its format better than a guess made here. A missing
  # reader package is reported the same way, by R: "there is no package
  # called 'png'".
  empty <- file.path(dir, "empty.png"); file.create(empty)
  expect_match(why(empty), "empty.png': file is not in PNG format", fixed = TRUE)
  fake <- file.path(dir, "fake.jpg"); writeLines("x", fake)
  expect_match(why(fake), "Not a JPEG file", fixed = TRUE)
  expect_match(why(dir), "it is a directory, not a file")
  noext <- file.path(dir, "noext"); writeLines("x", noext)
  expect_match(why(noext), "not a PNG, JPEG or SVG file")
})

test_that("latex_cache_clear() gives back the decoded images too", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  suppressWarnings(latex_dims(sprintf("\\includegraphics{%s}", mk_png(300, 200)),
                              input_mode = "math"))
  expect_gt(length(ls(gridmicrotex:::.image_cache)), 0L)
  # Rasters and pictures are far larger than a layout and have no size
  # limit of their own, so this is the only way to release them.
  latex_cache_clear()
  expect_equal(length(ls(gridmicrotex:::.image_cache)), 0L)
})

test_that("a file that stops being readable after measuring is reported", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  f <- mk_png(300, 200)
  g <- latex_grob(sprintf("\\includegraphics[width=1in]{%s}", f),
                  input_mode = "math")
  unlink(f)
  # The box is already in the layout and cannot be given back, so the only
  # honest thing left is to say the gap is there.
  expect_warning(grid::makeContent(g), "was readable when")

  # A markdown box is checked when it is built and laid out when it is
  # drawn, so it has the same gap between the two -- for a block image, an
  # inline one, and one in a table cell alike. None may error mid-draw.
  f <- mk_png(300, 200)
  g <- markdown_box_grob(sprintf("![a](%s)", f), width = grid::unit(3, "in"))
  unlink(f)
  expect_warning(grid::makeContent(g), "was readable when")
  for (md in c("text ![a](%s) more", "| a |\n|---|\n| ![x](%s) |")) {
    f <- mk_png(300, 200)
    g <- markdown_box_grob(sprintf(md, f), width = grid::unit(3, "in"))
    unlink(f)
    expect_warning(grid::makeContent(g), "file not found", label = md)
  }
})
