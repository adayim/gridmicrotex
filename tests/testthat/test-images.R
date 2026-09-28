# \includegraphics: resolving a file into a sized box, and drawing it.

# A PNG of a known size. agg_png's width/height are pixels.
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

# Geometry only: the low-resolution warning has its own test.
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

  # \textwidth is max_width; without one it is unreadable.
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
  # Backslashes, `%` and macro names would all break a raw path.
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

  # Intrinsic: pixels at 96 dpi.
  expect_equal(wid(sprintf("\\includegraphics{%s}", f)), 300 * 72 / 96)
  expect_equal(hei(sprintf("\\includegraphics{%s}", f)), 200 * 72 / 96)

  # Absolute, and independent of font size.
  for (fs in c(10, 40)) {
    expect_equal(
      wid(sprintf("\\includegraphics[width=3in]{%s}", f),
          gp = grid::gpar(fontsize = fs)), 216, info = fs)
  }

  # Aspect is kept when one side is given, not when both are, as in LaTeX.
  expect_equal(hei(sprintf("\\includegraphics[width=3in]{%s}", f)), 144)
  expect_equal(wid(sprintf("\\includegraphics[height=1in]{%s}", f)), 108)
  # 225 * 0.5 = 112.5; the bbox is reported in whole big points.
  expect_equal(wid(sprintf("\\includegraphics[scale=0.5]{%s}", f)), 112)
  expect_equal(hei(sprintf("\\includegraphics[width=3in,height=1in]{%s}", f)), 72)

  # It sits on the baseline: no depth, as in LaTeX.
  d <- latex_dims(sprintf("\\includegraphics[width=1in]{%s}", f),
                  input_mode = "math")
  expect_equal(as.numeric(d$depth), 0)
})

test_that("an oversized image is clamped so a markdown column can converge", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  f <- mk_png(300, 200)
  # .md_measure() retries with a smaller max_width; a fixed-size image
  # would never shrink.
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

  # Skip where librsvg returns an empty Picture (R-hub's nold container);
  # the loader then rightly falls back to a raster.
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
  # grImport2 primitives, at a depth that depends on the SVG.
  expect_false(inherits(kids[[1]], "rastergrob"))
  grob_classes <- function(g) {
    c(class(g)[1], unlist(lapply(g$children, grob_classes), use.names = FALSE))
  }
  prims <- grob_classes(kids[[1]])
  expect_true(any(grepl("^pic", prims)),
              info = paste("grob tree:", paste(prims, collapse = " / ")))

  # .md_shift_grob() moves a block by its vp$x, which pictureGrob lacks.
  expect_false(is.null(kids[[1]]$vp))
  expect_false(is.null(kids[[1]]$vp$x))
})

test_that("an SVG falls back to a raster when there is no picture reader", {
  skip_if_not_installed("svglite")
  skip_if_not_installed("rsvg")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  f <- mk_svg(2, 1)

  # Mock grImport2 away, so the fallback runs where it is installed.
  # `.package` is needed under a plain test_dir() / test_file().
  testthat::local_mocked_bindings(.image_picture = function(...) NULL,
                                  .package = "gridmicrotex")
  g <- .image_grob(f, 144, 72)
  expect_s3_class(g, "rastergrob")
  expect_gt(nrow(g$raster), 1L)
  expect_gt(ncol(g$raster), 1L)

  # With neither reader there is nothing to draw.
  testthat::local_mocked_bindings(.image_raster = function(...) NULL,
                                  .package = "gridmicrotex")
  expect_null(.image_grob(tempfile(fileext = ".png"), 72, 72))
})

test_that("an image that cannot be drawn is an error saying why", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # A URL is not fetched, so it fails like a missing file.
  for (f in c("no-such-file.png", "https://example.org/fig.png")) {
    expect_error(latex_grob(sprintf("\\includegraphics{%s}", f),
                            input_mode = "math"),
                 "file not found", label = f)
  }

  for (ext in c(".pdf", ".tiff")) {
    f <- tempfile(fileext = ext); writeBin(as.raw(1:20), f)
    expect_error(latex_dims(sprintf("\\includegraphics{%s}", f),
                            input_mode = "math"),
                 "not a PNG, JPEG or SVG file", label = ext)
  }
})

test_that("a figure that cannot be drawn does not cost a whole document", {
  # An error in a label; a warning and the file name in a document.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  .image_reset_warnings()
  f <- tempfile(fileext = ".pdf"); writeBin(as.raw(1:20), f)
  on.exit(unlink(f), add = TRUE)
  tex <- sprintf("Before \\includegraphics{%s} after", f)
  expect_error(latex_grob(tex), "not a PNG, JPEG or SVG file", fixed = TRUE)
  expect_warning(g <- latex_grob(tex, input_mode = "document"),
                 "not a PNG, JPEG or SVG file; drawing the file name instead",
                 fixed = TRUE)
  expect_true(any(grepl(basename(f), g$layout_df$text, fixed = TRUE)))
  expect_false("image" %in% g$layout_df$type)
})

test_that("a commented-out \\includegraphics is not read", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
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
  # `\\includegraphics` is a line break and a word; `\\\` is the command.
  expect_false("image" %in% latex_tree("a\\\\includegraphics{x}", input_mode = "math")$records$type)
  expect_error(latex_dims("a\\\\\\includegraphics{nope.png}", input_mode = "math"),
               "file not found")
})

test_that("one warning per file, however many times the layout is measured", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # One render can measure the layout several times.
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
  # Asked for 3in from 96px: 32 dpi.
  expect_match(warns(latex_dims(sprintf("\\includegraphics[width=3in]{%s}", small),
                                input_mode = "math"))[1], "dpi")
  # At intrinsic size it is 96 dpi by construction.
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
  f <- mk_png()
  d <- latex_grob(sprintf("\\newcommand{\\fig}{\\includegraphics[width=1in]{%s}}x\\fig y", f),
                  input_mode = "math")$layout_df
  expect_true("image" %in% d$type)
})

test_that("an \\includegraphics a macro makes, or a malformed one, is read like any other", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Images are read after macros expand, in both input modes.
  for (tex in c("x\\includegraphics{unbalanced", "x\\includegraphics y",
                "\\newcommand{\\ig}{\\includegraphics}\\ig{nope.png}",
                "\\newcommand{\\fig}[1]{\\includegraphics{#1}}\\fig{nope.png}",
                "\\def\\fig#1{\\includegraphics{#1}}\\fig{nope.png}")) {
    for (mode in c("math", "mixed")) {
      expect_error(suppressWarnings(latex_grob(tex, input_mode = mode)),
                   "Cannot draw image '(unbalanced|y|nope.png)': file not found",
                   label = paste(mode, tex))
    }
  }
  # Where images only warn, the file's name is drawn.
  .image_reset_warnings(); on.exit(.image_reset_warnings(), add = TRUE)
  alias <- "\\newcommand{\\ig}{\\includegraphics}\\ig{nope.png}"
  expect_warning(g <- .images_lenient(latex_grob(alias, input_mode = "math")),
                 "drawing the file name instead")
  expect_true("nope.png" %in% g$layout_df$text)
  expect_false(is.null(.gm_base_layout(paste("$x$", alias), 12)))

  # A real file through a macro is drawn, define_macro()'s included.
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  f <- mk_png()
  d <- latex_grob(sprintf("\\newcommand{\\fig}[1]{\\includegraphics[width=1in]{#1}}x\\fig{%s}y",
                          f), input_mode = "math")$layout_df
  expect_true("image" %in% d$type)
  define_macro("gmfig", sprintf("\\includegraphics[width=1in]{%s}", f))
  on.exit(clear_macros(), add = TRUE)
  expect_true("image" %in% latex_grob("x\\gmfig y", input_mode = "math")$layout_df$type)
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

  # mtime and size are part of the cache key.
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

  # An <img> alone on its line is an HTML block, and is drawn.
  expect_match(gridmicrotex:::.md_to_tex(sprintf("<img src='%s'>", f)),
               "includegraphics", fixed = TRUE)
  blk <- gridmicrotex:::.md_parse_blocks(sprintf("Intro\n\n<img src='%s'>\n\nEnd", f))
  expect_match(blk[[2]]$tex, "includegraphics", fixed = TRUE)
})

test_that("markdown that names an image it cannot draw is an error", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Every spelling fails, a URL included; none falls back to alt text.
  for (md in c("a ![ALT](nope.png) b", "a ![ALT](https://x/y.png) b",
               "a <img src='nope.png' alt='ALT'> b", "a <img alt='ALT'> b",
               "<img src='nope.png'>", "a <img src='nope.png' alt='a > b'> c")) {
    expect_error(markdown_grob(md), "file not found", label = md)
  }
  # The box is checked when built, since it lays out only when drawn.
  for (md in c("![ALT](nope.png)", "text ![ALT](nope.png) more",
               "Intro\n\n<img src='nope.png'>\n\nEnd",
               "see $\\includegraphics{nope.png}$")) {
    expect_error(markdown_box_grob(md), "file not found", label = md)
  }
  # Code is literal: an \includegraphics shown there names no figure.
  expect_match(gridmicrotex:::.md_to_tex("`$\\includegraphics{nope.png}$`"),
               "\\texttt{", fixed = TRUE)
})

test_that("a macro parameter in an image path is not checked as a file name", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # The build-time check reads unexpanded source, so it must skip `#1`.
  def <- "$\\newcommand{\\fig}[1]{\\includegraphics[width=1in]{#1}}$"
  expect_true("image" %in% markdown_grob(paste0(def, " $\\fig{", mk_png(), "}$"))$layout_df$type)
  expect_error(markdown_grob(paste0(def, " $\\fig{nope.png}$")), "file not found")
})

test_that("markdown reads an image's path the way HTML and CommonMark do", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  d <- tempfile("paths"); dir.create(d)
  put <- function(name) { p <- file.path(d, name); file.copy(mk_png(), p); p }
  drawn <- function(md) "image" %in% markdown_grob(md)$layout_df$type

  # A `$...$` pair in a file name is masked as math and must be restored.
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
  # Without the key, both lengths are obeyed, as in LaTeX.
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
  expect_equal(wid(sprintf("\\includegraphics[width=4in,scale=0.5]{%s}", f)), 144)
  expect_equal(hei(sprintf("\\includegraphics[width=4in,scale=0.5]{%s}", f)), 96)
})

test_that("a path is found the way graphicx finds one", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  dir <- tempfile("gfx"); dir.create(file.path(dir, "sub"), recursive = TRUE)
  file.copy(mk_png(300, 200), file.path(dir, "sub", "fig.png"))
  old <- setwd(dir); on.exit(setwd(old), add = TRUE)

  rec <- function(tex) latex_tree(tex, input_mode = "math")$records
  # No extension.
  expect_true("image" %in% rec("\\includegraphics{sub/fig}")$type)
  # \graphicspath supplies the directory, and draws nothing itself.
  expect_true("image" %in% rec("\\graphicspath{{sub/}}a\\includegraphics{fig}b")$type)
  expect_identical(rec("\\graphicspath{{sub/}}ab"), rec("ab"))
  # A later \graphicspath replaces an earlier one, as in LaTeX.
  expect_true("image" %in% rec("\\graphicspath{{no/}}\\graphicspath{{sub/}}\\includegraphics{fig}")$type)
  expect_error(rec("\\graphicspath{{sub/}}\\graphicspath{{no/}}\\includegraphics{fig}"),
               "file not found")
})

test_that("\\graphicspath reaches a list's items and a column's @{}", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  dir <- tempfile("gfx"); dir.create(file.path(dir, "sub"), recursive = TRUE)
  file.copy(mk_png(300, 200), file.path(dir, "sub", "fig.png"))
  old <- setwd(dir); on.exit(setwd(old), add = TRUE)

  # Both are lowered separately from the rest of the input.
  item <- "\\graphicspath{{sub/}}\\begin{itemize}\\item \\includegraphics{fig}\\end{itemize}"
  for (mode in c("mixed", "document")) {
    expect_true("image" %in% latex_tree(item, input_mode = mode)$records$type, info = mode)
  }
  column <- "\\graphicspath{{sub/}}\\begin{array}{@{\\includegraphics{fig}}c}a\\end{array}"
  expect_true("image" %in% latex_tree(column, input_mode = "math")$records$type)
  # The directories are the input's own: the next one starts without them.
  expect_error(latex_tree("\\includegraphics{fig}", input_mode = "math"), "file not found")
})

test_that("the starred and two-argument spellings are the same command", {
  skip_if_not_installed("ragg"); skip_if_not_installed("png")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  f <- mk_png(300, 200)
  expect_equal(wid(sprintf("\\includegraphics*[width=3in]{%s}", f)), 216)
  # The older `[llx,lly][urx,ury]` spelling: no recognised key, own size.
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
  expect_equal(w, 300 * 72 / 96)
  expect_match(paste(ws, collapse = " "), "textwidth")
  # Not also reported as low-resolution: intrinsic size is 96 dpi.
  expect_false(any(grepl("dpi", ws)))
})

test_that("an SVG no reader can draw is refused at parse time, not at draw time", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Well-formed XML with a size, but not an SVG: sizing a box for it would
  # leave a blank hole.
  dir <- tempfile("gfx"); dir.create(dir)
  f <- file.path(dir, "notanimage.svg")
  writeLines("<notsvg width='72pt' height='36pt'><x/></notsvg>", f)
  expect_error(gridmicrotex:::.image_dims(f))
  expect_error(latex_grob(sprintf("\\includegraphics{%s}", f), input_mode = "math"),
               "notanimage.svg", fixed = TRUE)
  # Markdown refuses it for the same reason.
  expect_error(gridmicrotex:::.md_to_tex(sprintf("a ![ALT](%s) b", f)),
               "notanimage.svg", fixed = TRUE)

  # An <svg> root in another namespace is not an SVG either.
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
  r <- rec(sprintf("\\rotatebox{30}{\\includegraphics[width=1in]{%s}}", f))
  expect_equal(nrow(r), 1L)
  expect_equal(r$rotation, -30, tolerance = 1e-3)   # y-down, so negated

  # `angle=` is the graphicx spelling of the same thing.
  expect_equal(rec(sprintf("\\includegraphics[angle=30,width=1in]{%s}", f))$rotation,
               -30, tolerance = 1e-3)
  expect_equal(rec(sprintf("\\includegraphics[width=1in]{%s}", f))$rotation, 0)
  expect_equal(rec(sprintf("\\includegraphics[angle=45]{%s}", f))$rotation, -45,
               tolerance = 1e-3)
  # A whole turn is not a rotation.
  expect_equal(rec(sprintf("\\includegraphics[angle=360]{%s}", f))$rotation, 0)

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
  expect_match(warns(G("[width=abc]")), "Cannot read")
  expect_match(warns(G("[width=3 inches]")), "Cannot read")
  expect_match(warns(G("[width=0]")), "nothing to draw")
  expect_match(warns(G("[width=-1in]")), "nothing to draw")
  # Positive, but 0.0000 at the reference's four decimal places.
  expect_match(warns(G("[width=0.00001bp]")), "nothing to draw")
  expect_match(warns(G("[scale=-2]")), "positive number")

  # A width no pixel count can express must degrade, not error.
  expect_no_error(suppressWarnings(latex_dims(G("[width=1e7in]"), input_mode = "math")))
})

test_that("an unreadable file is reported in the reader's own words", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  skip_if_not_installed("png"); skip_if_not_installed("jpeg")
  why <- function(path) tryCatch({
    latex_dims(sprintf("\\includegraphics{%s}", path), input_mode = "math")
    ""
  }, error = conditionMessage)
  dir <- tempfile("gfx"); dir.create(dir)
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
  expect_warning(grid::makeContent(g), "was readable when")

  # A markdown box likewise, for block, inline and table-cell images; none
  # may error mid-draw.
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
