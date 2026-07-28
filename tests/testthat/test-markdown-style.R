# Styling: the cascade, its two constructors, and the two markup routes.

# Lay a document out with a given style and hand back the items, so a
# test can assert on positions rather than pixels.
lay_md <- function(md, style = NULL, size = 12, width = 300) {
  gp <- grid::gpar(fontsize = size)
  .md_layout(.md_parse_blocks(md, size, .md_as_style(style)),
             width, gp, size, style = .md_as_style(style))
}

# --- md_style() ----------------------------------------------------------

test_that("md_style() translates R names and rejects unknown properties", {
  s <- md_style(font_size = 2, margin_top = 1, color = "red")
  expect_named(unclass(s), c("font-size", "margin-top", "color"))
  # The CSS spelling works too, for anyone copying a declaration across.
  expect_named(unclass(md_style(`font-size` = 2)), "font-size")

  # A constructor can afford to be strict where a pasted stylesheet
  # cannot: a typo here is a mistake, not a property we do not implement.
  expect_error(md_style(colour = "red"), "colour")
  expect_error(md_style(bakground = "red"), "bakground")
  expect_error(md_style("red"), "must be named")

  # NULL is how a tag opts out of a default rather than setting one.
  expect_length(unclass(md_style(color = NULL)), 0L)
})

# --- the stylesheet parser ----------------------------------------------

test_that("the stylesheet parser reads what it supports and skips the rest", {
  r <- .md_parse_stylesheet("
    /* a comment */
    h1, h2 { color: steelblue; font-size: 2.5rem }
    .note  { padding-left: 2em }
    div > p  { color: red }
    a:hover  { color: red }
    #id      { color: red }
    @media print { }
  ")
  # A selector list becomes one rule per selector.
  expect_equal(vapply(r, function(x) x$selector, character(1)),
               c("h1", "h2", ".note"))
  expect_equal(r[[1]]$props[["font-size"]], "2.5rem")
  # A class outranks a tag.
  expect_equal(vapply(r, function(x) x$specificity, integer(1)),
               c(1L, 1L, 10L))
})

test_that("malformed CSS yields no rules rather than an error", {
  for (bad in c("", "h1 { ", "} h1 {}", "h1 {}", "not css at all")) {
    expect_silent(r <- .md_parse_stylesheet(bad))
    expect_length(r, 0L)
  }
})

test_that("css= reads a file when it names one, and text otherwise", {
  f <- tempfile(fileext = ".css")
  writeLines("h1 { color: #123456 }", f)
  on.exit(unlink(f), add = TRUE)

  from_file <- .md_cascade(markdown_style(css = f), "h1")
  from_text <- .md_cascade(markdown_style(css = "h1 { color: #123456 }"), "h1")
  expect_equal(from_file$color, "#123456")
  expect_equal(from_text$color, from_file$color)
})

# --- markdown_style() ----------------------------------------------------

test_that("markdown_style() resolves base, then css, then explicit tags", {
  s <- markdown_style(
    css = "h1 { color: #111111; font-size: 3rem }",
    h1 = md_style(color = "#222222")
  )
  res <- .md_cascade(s, "h1")
  expect_equal(res$color, "#222222")        # the explicit argument wins
  expect_equal(res[["font-size"]], "3rem")  # ...without dropping the rest

  # An existing style can be extended.
  s2 <- markdown_style(s, h1 = md_style(color = "#333333"))
  expect_equal(.md_cascade(s2, "h1")$color, "#333333")

  expect_error(markdown_style(base = 42), "must be NULL")
  expect_error(markdown_style(h1 = "not a style"), "md_style")
  expect_error(markdown_style(md_style(color = "red")), "must be NULL")
})

test_that("every bundled preset parses to a usable style", {
  presets <- .md_preset_names()
  expect_true("github" %in% presets)
  for (p in presets) {
    s <- markdown_style(p)
    expect_s3_class(s, "gridmicrotex_markdown_style")
    # A preset that contributes nothing is a preset with a typo in it.
    expect_gt(length(s$rules), length(markdown_style()$rules))
    expect_false(is.null(.md_cascade(s, "h1")[["font-size"]]))
  }
  expect_error(markdown_style("no-such-preset"), "Unknown preset")
})

# --- the cascade ---------------------------------------------------------

test_that("specificity orders the four origins", {
  s <- markdown_style(css = "h1 { color: #010101 } .note { color: #020202 }")
  expect_equal(.md_cascade(s, "h1")$color, "#010101")
  expect_equal(.md_cascade(s, "h1", classes = "note")$color, "#020202")
  expect_equal(.md_cascade(s, "h1", classes = "note",
                           inline = list(color = "#030303"))$color, "#030303")
  # A later rule of equal specificity wins.
  s2 <- markdown_style(css = "h1 { color: #010101 } h1 { color: #040404 }")
  expect_equal(.md_cascade(s2, "h1")$color, "#040404")
})

test_that("inheritable properties cascade and box properties do not", {
  s <- markdown_style()
  inherited <- list(color = "grey40", "font-family" = "serif",
                    "margin-top" = 99, "padding-left" = 99,
                    "border-left" = 99)
  res <- .md_cascade(s, "p", inherited = inherited)
  expect_equal(res$color, "grey40")
  expect_equal(res[["font-family"]], "serif")
  # p has its own margin default; the container's must not have leaked in.
  expect_equal(res[["margin-top"]], 0.55)
  expect_null(res[["padding-left"]])
  expect_null(res[["border-left"]])
})

# --- lengths -------------------------------------------------------------

test_that("lengths accept a bare number, a CSS string and a unit", {
  # A bare number is rem: a multiple of the body size, not the element's.
  expect_equal(.md_css_length(2.5, body = 10, own = 20), 25)
  expect_equal(.md_css_length("2.5rem", body = 10, own = 20), 25)
  expect_equal(.md_css_length("2em", body = 10, own = 20), 40)
  expect_equal(.md_css_length("150%", body = 10, own = 20), 30)
  expect_equal(.md_css_length("12pt", body = 10), 12)
  expect_equal(.md_css_length("16px", body = 10), 12)
  expect_equal(.md_css_length(grid::unit(1, "in"), body = 10), 72)
  # An absolute length must not drift with the base.
  for (b in c(5, 10, 40)) expect_equal(.md_css_length("10pt", body = b), 10)
  for (bad in list("nonsense", "", "10furlongs")) {
    expect_null(.md_css_length(bad, body = 10))
  }
})

test_that("a unitless CSS string stays invalid, as it is in CSS", {
  # `font-size: 12` is ignored by a browser; only the R spelling
  # md_style(font_size = 12) means a multiple.
  expect_null(.md_css_size("12", 20))
  expect_equal(.md_css_size(2.5, 20), 2.5)
  expect_equal(.md_css_size(grid::unit(10, "bigpts"), 20), 0.5)
})

test_that("the border shorthand yields a width and a colour", {
  b <- .md_css_border("3px solid steelblue", body = 10)
  expect_equal(b$width, 2.25)
  expect_equal(b$color, "#4682B4")
  # Order does not matter, and the style keyword is ignored.
  expect_equal(.md_css_border("#D1D9E0 0.25rem", body = 10)$width, 2.5)
  expect_equal(.md_css_border("none", body = 10)$width, 0)
  # Unstated parts come back NULL so the caller can fall back.
  expect_null(.md_css_border("2pt", body = 10)$color)
  expect_null(.md_css_border(NULL, body = 10))
})

# --- the default style reproduces the previous rendering -----------------

test_that("the built-in defaults carry over the constants they replaced", {
  s <- markdown_style()
  expect_equal(.md_cascade(s, "p")[["margin-top"]], 0.55)
  expect_equal(.md_cascade(s, "h1")[["margin-top"]], 0.95)
  expect_equal(.md_cascade(s, "pre")[["line-height"]], 1.15)
  expect_equal(.md_cascade(s, "blockquote")[["padding-left"]], 0.75)
  expect_equal(.md_cascade(s, "ul")[["marker-gap"]], 0.4)
  # The heading ladder, as measured from the \Huge..\normalsize macros.
  sizes <- vapply(paste0("h", 1:6),
                  function(h) .md_cascade(s, h)[["font-size"]], numeric(1))
  expect_equal(unname(sizes), c(2.5, 2, 1.75, 17 / 12, 7 / 6, 1))
})

# --- the style reaches the layout ----------------------------------------

test_that("each documented knob moves the thing it names", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  md <- "# Head\n\nA paragraph.\n\n> quoted\n\n- one\n- two\n\n---"

  expect_gt(lay_md(md, "p { margin-top: 3rem }")$height, lay_md(md)$height)
  expect_lt(lay_md(md, "p { margin-top: 0 }")$height, lay_md(md)$height)
  expect_gt(lay_md(md, "h1 { font-size: 4rem }")$items[[1]]$h,
            lay_md(md)$items[[1]]$h)

  # text-align is recorded per block, for makeContent() to apply.
  al <- lay_md(md, "h1 { text-align: center } p { text-align: right }")
  expect_equal(al$items[[1]]$align, 0.5)
  expect_equal(al$items[[2]]$align, 1)
  expect_null(lay_md(md)$items[[1]]$align)

  # marker-gap pushes the item body right; the marker stays put.
  wide <- lay_md("- one", "ul { marker-gap: 3rem }")
  expect_equal(wide$items[[1]]$x, lay_md("- one")$items[[1]]$x)
  expect_gt(wide$items[[2]]$x, lay_md("- one")$items[[2]]$x)

  # bullet is the escape hatch for a marker glyph.
  expect_equal(lay_md("- one", "ul { bullet: Z }")$items[[1]]$grob$tex, "Z")

  # border-left is the quote bar.
  bar <- function(l) Filter(function(it) inherits(it$grob, "rect"), l$items)[[1]]
  expect_equal(bar(lay_md(md, "blockquote { border-left: 6pt solid red }"))$w, 6)
})

test_that("colour reaches the drawn grob and inherits into a quote", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  l <- lay_md("> quoted text", "blockquote { color: #FF0000 }")
  txt <- Filter(function(it) inherits(it$grob, "latexgrob"), l$items)
  expect_true("#FF0000" %in% unlist(lapply(txt, function(it)
    it$grob$layout_df$color)))
})

test_that("markdown_box_grob() accepts an object, CSS text or a file path", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  f <- tempfile(fileext = ".css")
  writeLines("h1 { color: #654321 }", f)
  on.exit(unlink(f), add = TRUE)

  for (st in list(markdown_style(css = "h1 { color: #654321 }"),
                  "h1 { color: #654321 }", f)) {
    g <- markdown_box_grob("# H", width = grid::unit(2, "in"), style = st)
    cols <- unlist(lapply(.md_box_layout(g)$items,
                          function(it) it$grob$layout_df$color))
    expect_true("#654321" %in% cols)
  }
  expect_error(markdown_box_grob("x", style = 42), "markdown_style")
})

# --- the <div> route -----------------------------------------------------

test_that("a div with blank lines around it becomes a container block", {
  b <- .md_parse_blocks(
    "Before\n\n<div class=\"note\">\n\n## In\n\nText.\n\n</div>\n\nAfter")
  expect_equal(vapply(b, function(x) x$type, character(1)),
               c("paragraph", "div", "paragraph"))
  d <- b[[2]]
  expect_equal(d$class, "note")
  expect_equal(vapply(d$blocks, function(x) x$type, character(1)),
               c("heading", "paragraph"))

  # Without blank lines cmark hands over one opaque block, which is
  # dropped exactly as it was before divs meant anything.
  expect_length(.md_parse_blocks("<div class=\"n\">\n**b**\n</div>"), 0L)
})

test_that("divs nest, close themselves, and ignore stray markup", {
  outer <- .md_parse_blocks(
    "<div class=\"a\">\n\n<div class=\"b\">\n\nx\n\n</div>\n\n</div>")[[1]]
  expect_equal(outer$class, "a")
  expect_equal(outer$blocks[[1]]$type, "div")
  expect_equal(outer$blocks[[1]]$class, "b")

  # An unclosed div still lays its contents out rather than losing them.
  u <- .md_parse_blocks("<div class=\"a\">\n\nx\n\ny")
  expect_equal(u[[1]]$type, "div")
  expect_length(u[[1]]$blocks, 2L)

  # A stray close, and any other raw block, is dropped.
  expect_equal(vapply(.md_parse_blocks("x\n\n</div>\n\ny"),
                      function(b) b$type, character(1)),
               c("paragraph", "paragraph"))
  expect_equal(vapply(.md_parse_blocks("x\n\n<table><tr></tr></table>\n\ny"),
                      function(b) b$type, character(1)),
               c("paragraph", "paragraph"))
})

test_that("a div styles the chunk inside it", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  plain <- lay_md("Text.")
  padded <- lay_md("<div style=\"padding-left: 3rem\">\n\nText.\n\n</div>")
  expect_equal(padded$items[[1]]$x - plain$items[[1]]$x, 36)

  # A class matches the element carrying it and nothing else, as in CSS:
  # the paragraph inside must not be indented a second time.
  once <- lay_md("<div class=\"note\">\n\nT.\n\n</div>",
                 ".note { padding-left: 2rem }")
  expect_equal(once$items[[1]]$x, 24)

  # Inheritable properties still reach the contents.
  col <- lay_md("<div class=\"note\">\n\nT.\n\n</div>",
                ".note { color: #00FF00 }")
  expect_true("#00FF00" %in% col$items[[1]]$grob$layout_df$color)
})

# --- the <span> route ----------------------------------------------------

test_that("a span resolves its class from the stylesheet", {
  s <- markdown_style(css = ".hot { color: #FF0000; font-weight: bold }")
  expect_match(.md_to_tex('a <span class="hot">b</span>', 20, s),
               "\\textcolor{#FF0000}{\\textbf{b}}", fixed = TRUE)
  # The style attribute outranks the class.
  expect_match(
    .md_to_tex('<span class="hot" style="color: #0000FF">b</span>', 20, s),
    "#0000FF", fixed = TRUE)
  # An unknown class is not an error, it simply matches nothing.
  expect_match(.md_to_tex('<span class="nope">b</span>', 20, s),
               "\\text{b}", fixed = TRUE)
  # Without a stylesheet a class means nothing at all.
  expect_match(.md_to_tex('<span class="hot">b</span>'), "\\text{b}",
               fixed = TRUE)
})

test_that("font-weight and font-style now work on a span", {
  # Previously parsed and dropped: .md_parse_style() knew only colour,
  # size, family and decoration.
  expect_match(.md_to_tex('<span style="font-weight: bold">b</span>'),
               "\\textbf{", fixed = TRUE)
  expect_match(.md_to_tex('<span style="font-weight: 700">b</span>'),
               "\\textbf{", fixed = TRUE)
  expect_match(.md_to_tex('<span style="font-style: italic">b</span>'),
               "\\textit{", fixed = TRUE)
  # Weights below 600 are not bold, and `normal` is not italic.
  expect_false(grepl("textbf", .md_to_tex('<span style="font-weight: 400">b</span>'),
                     fixed = TRUE))
  expect_false(grepl("textit", .md_to_tex('<span style="font-style: normal">b</span>'),
                     fixed = TRUE))
})

# --- markdown_grob() and the global option -------------------------------

test_that("markdown_grob() applies what compiles to LaTeX and ignores layout", {
  # A heading had no size at all in the flattened path before this.
  expect_match(.md_to_tex("# H", 20, markdown_style()), "\\scalebox{",
               fixed = TRUE)
  expect_match(.md_to_tex("# H", 20, .md_as_style("h1 { color: #FF0000 }")),
               "\\textcolor{#FF0000}", fixed = TRUE)
  # Layout properties have nothing to act on here and are dropped.
  expect_equal(.md_to_tex("# H", 20, .md_as_style("h1 { margin-top: 9rem }")),
               .md_to_tex("# H", 20, markdown_style()))
})

test_that("latex_options(markdown_style=) supplies a default both grobs use", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  old <- latex_options(markdown_style = "h1 { color: #0000FF }")
  on.exit(reset_latex_options(), add = TRUE)

  g <- markdown_grob("# H", gp = grid::gpar(fontsize = 12))
  expect_true("#0000FF" %in% g$layout_df$color)

  b <- markdown_box_grob("# H", width = grid::unit(2, "in"),
                         gp = grid::gpar(fontsize = 12))
  expect_true("#0000FF" %in% unlist(lapply(.md_box_layout(b)$items,
                                           function(it) it$grob$layout_df$color)))

  # An argument at the call site still wins over the option.
  g2 <- markdown_grob("# H", style = "h1 { color: #FF00FF }",
                      gp = grid::gpar(fontsize = 12))
  expect_true("#FF00FF" %in% g2$layout_df$color)

  reset_latex_options()
  expect_null(latex_options()$markdown_style)
})

# --- emphasis actually renders (not just emitted) ------------------------

# The font styles MicroTeX reports for a drawn document. 1 = plain,
# 2 = bold, 4 = italic, 6 = both.
styles_of <- function(md, css = NULL, size = 12) {
  g <- markdown_box_grob(md, width = grid::unit(3, "in"), style = css,
                         gp = grid::gpar(fontsize = size))
  d <- unlist(lapply(.md_box_layout(g)$items,
                     function(it) it$grob$layout_df$font_style))
  sort(unique(d[!is.na(d)]))
}

test_that("block font-weight and font-style reach the glyphs", {
  # Emitting \textbf{\text{x}} is not enough: \text{} builds a non-nested
  # FontStyleAtom, so the weight is silently dropped and the run reports
  # plain. The content has to be generated bare instead.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_equal(styles_of("hello"), 1)
  expect_equal(styles_of("hello", "p { font-weight: bold }"), 2)
  expect_equal(styles_of("hello", "p { font-style: italic }"), 4)
  expect_equal(styles_of("hello", "p { font-weight: bold; font-style: italic }"), 6)
})

test_that("headings are bold by default, and can be told not to be", {
  # They were not, before: the old \Huge{}\textbf{\text{H}} had the same
  # reset problem, so no heading this package ever drew was actually bold.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_equal(styles_of("# Head"), 2)
  expect_equal(styles_of("# Head", "h1 { font-weight: normal }"), 1)
})

test_that("emphasis inherits into quotes, list items and divs", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_equal(styles_of("> quoted", "blockquote { font-weight: bold }"), 2)
  expect_equal(styles_of("- item", "li { font-weight: bold }"), 2)
  expect_equal(styles_of("<div class=\"b\">\n\nx\n\n</div>",
                         ".b { font-weight: bold }"), 2)
})

test_that("emphasis does not apply to pre or alt text", {
  # A documented limitation, tested so the documentation stays true: these
  # build their own LaTeX and impose their own font handling.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_equal(styles_of("```\ncode\n```", "pre { font-weight: bold }"), 1)
  expect_equal(styles_of("![alt](nope.png)", "img { font-weight: bold }"), 1)

  # Tables used to be in this list. They are not any more: cell content is
  # generated with its emphasis already applied, so font-weight inherits
  # from `table` into the cells the way CSS says it should.
  expect_equal(styles_of("| a |\n|---|\n| 1 |", "table { font-weight: bold }"), 2)
})

# --- tags and properties that had no effect at all -----------------------

test_that("body seeds the inheritance every block starts from", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  g <- markdown_box_grob("text\n\n# head", width = grid::unit(3, "in"),
                         style = "body { color: #FF0000 }")
  cols <- unlist(lapply(.md_box_layout(g)$items,
                        function(it) it$grob$layout_df$color))
  expect_equal(unique(cols), "#FF0000")
  # ...and a tag rule still beats what it inherits.
  g2 <- markdown_box_grob("text\n\n# head", width = grid::unit(3, "in"),
                          style = "body { color: #FF0000 } h1 { color: #0000FF }")
  cols2 <- unlist(lapply(.md_box_layout(g2)$items,
                         function(it) it$grob$layout_df$color))
  expect_setequal(cols2, c("#FF0000", "#0000FF"))
})

test_that("code styles an inline code span, on top of its monospace default", {
  s <- .md_as_style("code { color: #FF0000 }")
  tex <- .md_to_tex("a `x` b", 20, s)
  expect_match(tex, "\\textcolor{#FF0000}", fixed = TRUE)
  # The \texttt default is added to, not replaced.
  expect_match(tex, "\\texttt{", fixed = TRUE)
})

test_that("margin-bottom adds space below a block", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  h <- function(css) {
    s <- .md_as_style(css)
    .md_layout(.md_parse_blocks("one\n\ntwo", 12, s), 300,
               grid::gpar(fontsize = 12), 12, style = s)$height
  }
  expect_gt(h("p { margin-bottom: 4rem }"), h(NULL))
  # Every built-in default leaves it unset, so nothing moves by itself.
  expect_equal(h("p { margin-bottom: 0 }"), h(NULL))
})

test_that("visual: a styled markdown document", {
  skip_if_not_installed("vdiffr")
  skip_on_os("mac")
  md <- paste(
    "# Styled title", "",
    "A paragraph with $x^2$ and a <span class=\"hot\">warning</span>.", "",
    "> A quoted remark.", "",
    "<div class=\"note\">", "",
    "**Note.** An indented chunk.", "",
    "</div>", "",
    "- alpha", "- beta", "",
    "---",
    sep = "\n")
  vdiffr::expect_doppelganger("markdown-styled", function() {
    grid::grid.draw(markdown_box_grob(
      md,
      width = grid::unit(4.4, "in"),
      padding = grid::unit(10, "pt"),
      box_gp = grid::gpar(fill = "grey98", col = "grey40"),
      style = "
        h1         { color: #2C5F8A; font-size: 1.8rem }
        blockquote { color: #59636E; border-left: 3px solid #2C5F8A }
        .note      { color: #59636E; padding-left: 1.5rem }
        .hot       { color: #B22222; font-weight: bold }
        hr         { border-top: 2px solid #D1D9E0 }
      ",
      gp = grid::gpar(fontsize = 13)
    ))
  })
})

# --- display math blocks -------------------------------------------------

test_that("a lone $$...$$ paragraph becomes a centred mathblock", {
  types <- function(md) vapply(.md_parse_blocks(md), function(b) b$type,
                               character(1))

  # A paragraph that is nothing but display math is its own block.
  expect_identical(types("$$x^2 + y^2 = z^2$$"), "mathblock")
  expect_identical(types("Before\n\n$$\\int_0^1 f(x)dx$$\n\nAfter"),
                   c("paragraph", "mathblock", "paragraph"))

  # Inline math is untouched, and so is display math with prose beside it:
  # the block form is only for a paragraph the span fills completely.
  expect_identical(types("Inline $x^2$ here."), "paragraph")
  expect_identical(types("$$x$$ and text after"), "paragraph")
  expect_identical(types("Text $$x$$ before"), "paragraph")

  # It is centred by default, where a paragraph states no alignment and
  # inherits the box-wide halign instead.
  items <- lay_md("Before\n\n$$x^2$$\n\nAfter")$items
  expect_null(items[[1]]$align)
  expect_equal(items[[2]]$align, 0.5)

  # And it is an ordinary tag, so a stylesheet can move it.
  expect_equal(lay_md("$$x^2$$", "math { text-align: left }")$items[[1]]$align, 0)
})

test_that("a markdown link is styled through the `a` tag", {
  tex <- function(md, style = NULL) .md_to_tex(md, style = .md_as_style(style))
  col <- gridmicrotex:::.MD_LINK_COLOR

  # Blue and underlined by default, as a browser renders <a>.
  expect_equal(tex("[x](u)"),
               paste0("\\textcolor{", col, "}{\\underline{\\text{x}}}"))

  # It is an ordinary tag, so a stylesheet overrides it.
  expect_match(tex("[x](u)", "a { color: red }"), "textcolor\\{#FF0000\\}",
               fixed = FALSE)

  # \underline typesets a fresh sub-formula and would drop the bold around
  # it, so the enclosing style is re-opened inside -- the rule ~~strike~~
  # already follows.
  expect_match(tex("**[x](u)**"), "underline\\{\\\\textbf\\{")

  # The destination is dropped; only the text is kept.
  expect_false(grepl("u}", tex("[x](u)"), fixed = TRUE))
})

test_that("footnotes render markers inline and notes at the foot", {
  types <- function(md) vapply(.md_parse_blocks(md), function(b) b$type,
                               character(1))
  md <- "Body[^one] more[^two].\n\n[^one]: First.\n\n[^two]: Second."

  # CommonMark gathers the definitions at the end already, so the only
  # thing added here is a single separator before the first one.
  expect_identical(types(md),
                   c("paragraph", "thematic_break", "footnote", "footnote"))

  # The marker is a superscript, numbered by reference order.
  expect_match(.md_to_tex("a[^x] b[^y]\n\n[^x]: f\n\n[^y]: s"),
               "textsuperscript\\{1\\}.*textsuperscript\\{2\\}")

  # An inline label has no foot to put notes at: the marker stays, the
  # definition is dropped.
  expect_match(.md_to_tex("Body[^one].\n\n[^one]: note"),
               "textsuperscript\\{1\\}")
  expect_false(grepl("note", .md_to_tex("Body[^one].\n\n[^one]: note"),
                     fixed = TRUE))

  # A document without footnotes is completely unaffected.
  expect_identical(types("Just text."), "paragraph")

  # A reference with no definition is left as the literal text CommonMark
  # gives us, rather than erroring or emitting an empty superscript.
  expect_silent(t <- .md_to_tex("dangling[^nope]"))
  expect_false(grepl("textsuperscript", t, fixed = TRUE))

  # The notes are set smaller, and the tag is styleable like any other.
  expect_equal(.md_cascade(markdown_style(), "footnote")[["font-size"]], 0.85)
  small <- lay_md(md)$items
  big <- lay_md(md, "footnote { font-size: 3 }")$items
  expect_gt(big[[length(big)]]$h, small[[length(small)]]$h)
})

# --- table styling -------------------------------------------------------

# The generated tabular for a two-column table under a given stylesheet.
tbl_tex <- function(style = NULL, avail = 0,
                    md = "| Name | Value |\n|:-----|------:|\n| a | 1 |\n| b | 2 |") {
  st <- .md_as_style(style)
  b <- Filter(function(x) x$type == "table", .md_parse_blocks(md, 12, st))[[1]]
  .md_table_tex(b$nd, b$spans, b$base, st, avail = avail,
                inherited = b$ctx %||% list())
}

test_that("table CSS compiles to the tabular primitives MicroTeX has", {
  # Row and cell fills are different primitives, as in HTML: tr is a row,
  # td/th are cells.
  expect_match(tbl_tex("tr { background: #F6F8FA }"), "rowcolor{#F6F8FA}",
               fixed = TRUE)
  expect_match(tbl_tex("td { background: #DDDDDD }"), "cellcolor{#DDDDDD}",
               fixed = TRUE)
  # A th fill is per-cell, and must not also emit a redundant row fill.
  th <- tbl_tex("th { background: #EEEEEE }")
  expect_match(th, "cellcolor{#EEEEEE}", fixed = TRUE)
  expect_false(grepl("rowcolor", th, fixed = TRUE))

  expect_match(tbl_tex("table { border-color: #D1D9E0 }"),
               "arrayrulecolor{#D1D9E0}", fixed = TRUE)
  # A cell border becomes vertical rules in the column spec.
  expect_match(tbl_tex("td { border-left: 1px solid black }"),
               "{|l|r|}", fixed = TRUE)
  # Cell padding becomes the inter-column @{} material.
  expect_match(tbl_tex("td { padding-left: 6pt }"),
               "@{\\hspace{6.00pt}}", fixed = TRUE)
})

test_that("tr border-bottom rules rows without doubling the bottom rule", {
  t <- tbl_tex("tr { border-bottom: 1px solid black }")
  # One rule between the two body rows, and none before the closing
  # \thickhline, which would otherwise draw two lines on top of each other.
  expect_false(grepl("\\hline \\thickhline", t, fixed = TRUE))
  expect_match(t, "hline", fixed = TRUE)
})

test_that("header cells are bold, which needs parse-time emphasis", {
  # \textbf{\text{x}} reports plain -- \text{} builds a non-nested
  # FontStyleAtom -- so wrapping the finished table could never work. The
  # emphasis has to be applied as each cell is generated.
  expect_match(tbl_tex(), "\\textbf{Name}", fixed = TRUE)
  # And it is a plain rule, so it can be turned off.
  expect_false(grepl("textbf", tbl_tex("th { font-weight: normal }"),
                     fixed = TRUE))
  # font-weight on the table inherits into the cells, as CSS says.
  expect_match(tbl_tex("table { font-weight: bold }"), "\\textbf{a}",
               fixed = TRUE)
})

test_that("table-layout: fixed divides the measure into p{} columns", {
  # Content-sized by default: no p{} anywhere.
  expect_false(grepl("p{", tbl_tex(), fixed = TRUE))
  # Without a known width there is nothing to divide, so it stays content-sized.
  expect_false(grepl("p{", tbl_tex("table { table-layout: fixed }", avail = 0),
                     fixed = TRUE))
  expect_match(tbl_tex("table { table-layout: fixed }", avail = 200),
               "p{", fixed = TRUE)
})

test_that("a fixed-layout table fits the box instead of overflowing", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  wide <- paste0(
    "| Column one | Column two |\n|---|---|\n",
    "| a long sentence that will not fit in a narrow column at all | ",
    "another long sentence that also will not fit anywhere |")
  meas <- function(style) {
    g <- markdown_box_grob(wide, width = grid::unit(3, "in"), style = style,
                           gp = grid::gpar(fontsize = 11))
    lay <- .md_box_layout(g)
    it <- lay$items[[length(lay$items)]]
    c(content = lay$content_w, w = it$w, h = it$h)
  }
  free <- meas(NULL)
  fixed <- meas("table { table-layout: fixed }")

  # The default still overflows -- unchanged behaviour.
  expect_gt(free[["w"]], free[["content"]])
  # Fixed fits, and is taller because the cells now wrap.
  expect_lte(fixed[["w"]], fixed[["content"]] + 1)
  expect_gt(fixed[["h"]], free[["h"]])
})

# --- box properties ------------------------------------------------------

# The LaTeX a class-styled span compiles to.
span_tex <- function(css) {
  .md_to_tex('<span class="x">hi</span>',
             style = .md_as_style(paste0(".x {", css, "}")))
}

test_that("inline box properties compile to MicroTeX primitives", {
  expect_equal(span_tex("background: #FFFF00"),
               "\\bgcolor{#FFFF00}{\\text{hi}}")
  expect_equal(span_tex("text-decoration: overline"),
               "\\overline{\\text{hi}}")
  expect_equal(span_tex("border: 1px solid red"), "\\fbox{\\text{hi}}")
  expect_equal(span_tex("border-style: double"), "\\doublebox{\\text{hi}}")
  expect_equal(span_tex("border-radius: 4pt"), "\\ovalbox{\\text{hi}}")
  expect_equal(span_tex("box-shadow: 2pt 2pt"), "\\shadowbox{\\text{hi}}")
  expect_equal(span_tex("visibility: hidden"), "\\phantom{\\text{hi}}")
  expect_equal(span_tex("vertical-align: super"),
               "\\textsuperscript{\\text{hi}}")
  expect_equal(span_tex("vertical-align: sub"), "\\textsubscript{\\text{hi}}")
  expect_equal(span_tex("vertical-align: 3pt"),
               "\\raisebox{3.00pt}{\\text{hi}}")
  expect_equal(span_tex("transform: rotate(45deg)"),
               "\\rotatebox{45}{\\text{hi}}")
  expect_equal(span_tex("transform: scaleX(-1)"),
               "\\reflectbox{\\text{hi}}")

  # A border takes the background as \fcolorbox's fill, so \bgcolor must
  # not paint it a second time.
  expect_equal(span_tex("border: 1px solid red; background: #EEEEEE"),
               "\\fcolorbox{#FF0000}{#EEEEEE}{\\text{hi}}")
})

test_that("values MicroTeX has no primitive for are ignored, not errors", {
  for (css in c("transform: skew(3deg)", "visibility: visible",
                "box-shadow: none", "vertical-align: nonsense")) {
    expect_equal(span_tex(css), "\\text{hi}", info = css)
  }
})

test_that("inline box properties render", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  for (css in c("background: #FFFF00", "border: 1px solid red",
                "border-style: double", "border-radius: 4pt",
                "box-shadow: 2pt 2pt", "vertical-align: super",
                "transform: rotate(45deg)", "text-decoration: overline")) {
    expect_gt(nrow(latex_tree(span_tex(css), input_mode = "math",
                              gp = grid::gpar(fontsize = 16))$records), 0,
              label = css)
  }
  # \phantom is the exception: it takes space and draws nothing.
  expect_equal(nrow(latex_tree(span_tex("visibility: hidden"),
                               input_mode = "math")$records), 0)
})

test_that("blocks take padding and margins on all four sides", {
  md <- "Alpha\n\nBravo"
  base <- lay_md(md)
  # Left padding and left margin both shift the block right.
  expect_equal(lay_md(md, "p { padding-left: 20pt }")$items[[2]]$x, 20)
  expect_equal(lay_md(md, "p { margin-left: 15pt }")$items[[2]]$x, 15)
  # Vertical padding grows the document.
  expect_gt(lay_md(md, "p { padding-top: 10pt }")$height, base$height)
  expect_gt(lay_md(md, "p { padding-bottom: 10pt }")$height, base$height)
  # Right padding narrows the measure, so long text wraps sooner.
  long <- paste(rep("word", 40), collapse = " ")
  expect_gt(lay_md(long, "p { padding-right: 120pt }")$height,
            lay_md(long)$height)
})

test_that("a block background is a rect drawn behind the block", {
  md <- "Alpha\n\nBravo"
  plain <- lay_md(md)
  bg <- lay_md(md, "p { background: #EEEEEE }")

  # One extra item per styled block, and it comes first so it draws behind.
  expect_equal(length(bg$items), length(plain$items) + 2L)
  expect_s3_class(bg$items[[1]]$grob, "rect")

  # It spans the column -- the thing no MicroTeX box command can do, since
  # every one of them hugs its content.
  expect_equal(bg$items[[1]]$w, 300)

  # And it covers the padding, not just the text.
  padded <- lay_md(md, "p { background: #EEE; padding-top: 10pt }")
  expect_gt(padded$items[[1]]$h, bg$items[[1]]$h)
})

test_that("strong and em are styleable tags, like code", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  cols <- function(md, style) unique(stats::na.omit(
    markdown_grob(md, style = .md_as_style(style))$layout_df$color))

  # The tag vocabulary is HTML's names, and ** / * produce strong / em,
  # so a rule on those names has to reach them -- <code> always did.
  expect_equal(cols("**b**", "strong { color: #B22222 }"), "#B22222")
  expect_equal(cols("*i*", "em { color: #1F6FB2 }"), "#1F6FB2")

  # Only the styled run changes.
  expect_setequal(cols("plain **b** plain", "strong { color: #B22222 }"),
                  c("#000000", "#B22222"))

  # The emphasis itself must survive being wrapped in a colour.
  tex <- .md_to_tex("**b**", style = .md_as_style("strong { color: #B22222 }"))
  expect_match(tex, "\\textbf{", fixed = TRUE)
  expect_match(tex, "\\textcolor{#B22222}", fixed = TRUE)

  # Unstyled output is byte-identical to before the tags were wired up.
  expect_equal(.md_to_tex("**b**"), "\\textbf{b}")
  expect_equal(.md_to_tex("*i*"), "\\textit{i}")
  expect_equal(.md_to_tex("***both***"), "\\textit{\\textbf{both}}")
})
