test_that("math spans survive the markdown parser byte-for-byte", {
  # Unmasked, CommonMark reads `\\` as an escape and a matrix loses a row.
  for (m in c("$x^2$", "$x_i + y_i$", "$\\frac{a}{b}$",
              "$\\sum_{i=1}^n \\alpha_i$",
              "$\\begin{matrix}a\\\\b\\end{matrix}$")) {
    masked <- .md_mask_math(m)
    expect_identical(.md_unmask_math(masked$text, masked$spans), m)
  }
})

test_that("a matrix keeps both rows through markdown_grob()", {
  g <- markdown_grob("matrix $\\begin{matrix}a\\\\b\\end{matrix}$")
  flat <- markdown_grob("matrix ab")
  # Two stacked rows must be taller than the same glyphs on one line.
  expect_gt(g$bbox_h, flat$bbox_h)
  expect_match(.md_to_tex("$\\begin{matrix}a\\\\b\\end{matrix}$"),
               "\\begin{matrix}a\\\\b\\end{matrix}", fixed = TRUE)
})

test_that("inline markdown maps onto the expected LaTeX commands", {
  expect_match(.md_to_tex("**b**"), "\\textbf{", fixed = TRUE)
  expect_match(.md_to_tex("*i*"),   "\\textit{", fixed = TRUE)
  expect_match(.md_to_tex("`c`"),   "\\texttt{", fixed = TRUE)
  # \sout is a horizontal rule; \cancel is a diagonal slash.
  expect_match(.md_to_tex("~~s~~"), "\\sout{", fixed = TRUE)
  expect_false(grepl("\\cancel", .md_to_tex("~~s~~"), fixed = TRUE))
})

test_that("markdown becomes LaTeX text, read as a document", {
  # As pandoc writes it: prose is text, math keeps its delimiters, and the
  # paragraph is not indented.
  expect_identical(.md_to_tex("plain words $x^2$"), "plain words $x^2$")
  g <- markdown_grob("plain words here")
  expect_true(all(g$layout_df$type == "text"))
  expect_equal(min(g$layout_df$x), 0)
})

test_that("markdown-only constructs never emit unknown LaTeX commands", {
  # An unknown command would be drawn as its letters.
  tex <- .md_to_tex(paste(
    "# Heading", "", "a [link](http://x.com)", "",
    "> quote", "", "```", "code", "```", sep = "\n"))
  for (bad in c("\\section", "\\href", "verbatim",
                "\\linewidth", "\\begin{quote}")) {
    expect_false(grepl(bad, tex, fixed = TRUE), label = paste("leaked", bad))
  }
  # A link degrades to its text, which must survive.
  expect_match(.md_to_tex("[label](http://x)"), "label", fixed = TRUE)
})

test_that("TeX special characters in prose are escaped", {
  expect_match(.md_to_tex("100% done"), "100\\% done", fixed = TRUE)
  expect_match(.md_to_tex("a_b"),       "a\\_b",       fixed = TRUE)
  expect_match(.md_to_tex("x & y"),     "x \\& y",     fixed = TRUE)
  expect_match(.md_to_tex("no #1"),     "no \\#1",     fixed = TRUE)
  # `^` and `~` are the characters, not the accents \^{} and \~{}.
  expect_match(.md_to_tex("2 ^ 3"),     "2 \\char94{} 3", fixed = TRUE)
  expect_false(grepl("\\^{}", .md_to_tex("2 ^ 3"), fixed = TRUE))
  expect_match(.md_to_tex("a ~ b"),     "\\char126{}", fixed = TRUE)
  # There is no \textbackslash; \backslash is a math symbol.
  expect_match(.md_to_tex("a \\ b"), "$\\backslash$", fixed = TRUE)
  expect_false(grepl("textbackslash", .md_to_tex("a \\ b"), fixed = TRUE))
})

test_that("a lone dollar sign is literal, not an unclosed math span", {
  tex <- .md_to_tex("costs $ and & more")
  expect_match(tex, "\\$", fixed = TRUE)
  expect_match(tex, "\\&", fixed = TRUE)
  expect_no_warning(.md_to_tex("costs $ and more"))
})

test_that("code spans stay literal and never leak a sentinel", {
  tex <- .md_to_tex("`code with $x$ inside`")
  expect_match(tex, "\\$x\\$", fixed = TRUE)
  # The private-use sentinels must never reach the output.
  expect_false(grepl("", tex, fixed = TRUE))
  expect_false(grepl("", tex, fixed = TRUE))
  expect_false(grepl("", tex, fixed = TRUE))
})

test_that("wrapping still works for single-paragraph markdown", {
  # A stray `\\` in the output would stop max_width from wrapping.
  long <- paste(rep("The quick brown fox jumps over the lazy dog.", 4),
                collapse = " ")
  g <- markdown_grob(paste("**bold**", long), max_width = 200)
  expect_true(g$is_split)
  expect_lte(g$bbox_w, 200)
})

test_that("markdown_grob() validates its input", {
  expect_error(markdown_grob(NA_character_), "must not be NA")
  expect_error(markdown_grob(c("a", "b")), "single character string")
  expect_error(markdown_grob(42), "single character string")
  expect_s3_class(markdown_grob("ok"), "latexgrob")
})

test_that("grid.markdown() draws and returns the grob invisibly", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  grid::grid.newpage()
  expect_invisible(g <- grid.markdown("**hi** $x$"))
  expect_s3_class(g, "latexgrob")
  expect_true(all(c("text", "glyph") %in% g$layout_df$type))
})

# --- block-level markdown (markdown_box_grob) ------------------------

md_doc <- paste(
  "# Title", "",
  "First paragraph that is long enough to need wrapping when the box",
  "is narrow.", "",
  "## Section", "",
  "- alpha", "- beta", "",
  "1. one", "2. two", "",
  "> quoted", "",
  "```", "x <- 1", "y <- 2", "```", "",
  "| a | b |", "|---|---|", "| 1 | 2 |", "",
  "---",
  sep = "\n"
)

test_that("every block type is recognised", {
  types <- vapply(.md_parse_blocks(md_doc), function(b) b$type, character(1))
  expect_setequal(
    types,
    c("heading", "paragraph", "heading", "list", "list",
      "block_quote", "code_block", "table", "thematic_break")
  )
  blks <- .md_parse_blocks(md_doc)
  expect_equal(blks[[1]]$level, 1L)
  expect_equal(blks[[3]]$level, 2L)
  expect_false(Filter(function(b) b$type == "list", blks)[[1]]$ordered)
  expect_true(Filter(function(b) b$type == "list", blks)[[2]]$ordered)
})

test_that("a markdown table becomes a real tabular", {
  tex <- Filter(function(b) b$type == "table", .md_parse_blocks(md_doc))[[1]]$tex
  expect_match(tex, "\\begin{tabular}", fixed = TRUE)
  expect_match(tex, "\\thickhline", fixed = TRUE)
  expect_match(tex, "\\hline", fixed = TRUE)
  expect_match(tex, "&", fixed = TRUE)
  # It must be renderable, not just well-formed.
  expect_gt(as.numeric(latex_dims(tex, input_mode = "math")$width), 0)
})

test_that("measuring and drawing agree, so blocks stay inside the box", {
  skip_if_not_installed("ragg")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- .md_parse_blocks("Some *emphasised* prose with $x^2$ inside it.")[[1]]$tex
  gp <- grid::gpar(fontsize = 13)
  for (w in c(120, 200, 280)) {
    m <- .md_measure(tex, w, gp)
    g <- .md_run_grob(tex, 0, 0, w, gp)
    expect_equal(m$w, g$bbox_w, tolerance = 1e-6)
    expect_lte(g$bbox_w, w + 0.5)
  }
})

test_that("a soft line break renders as a word space", {
  tex <- .md_parse_blocks("alpha\nbeta")[[1]]$tex
  expect_identical(tex, "alpha beta")
  spaced <- .md_measure(tex, 0, grid::gpar())$w
  joined <- .md_measure("alphabeta", 0, grid::gpar())$w
  expect_gt(spaced, joined)
})

test_that("a paragraph keeps within its width, however its runs nest", {
  # A line breaks at the last break before the overflow, even at the end
  # of an earlier run.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  set.seed(1)
  vocab <- c("the", "model", "**fitted**", "*values*", "`lm()`", "$x^2$", "[link](u)",
             "data", "<span style=\"color:blue\">coloured words here</span>", "of",
             "<small>small print</small>", "$\\hat\\beta_1$", "**bold words**")
  for (i in 1:20) {
    p <- paste(sample(vocab, 20, replace = TRUE), collapse = " ")
    for (w in c(120, 200, 300)) {
      expect_lte(markdown_grob(p, max_width = w)$bbox_w, w + 0.5, label = paste(w, p))
    }
  }
})

test_that("the box grows taller as it is made narrower", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  h <- vapply(c(6, 4, 3), function(wi) {
    g <- markdown_box_grob(md_doc, width = grid::unit(wi, "in"))
    grid::convertHeight(grid::grobHeight(g), "in", valueOnly = TRUE)
  }, numeric(1))
  expect_true(all(diff(h) > 0))
})

test_that("padding and margin enlarge the reported size", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  bare <- markdown_box_grob("text", width = grid::unit(3, "in"))
  padded <- markdown_box_grob("text", width = grid::unit(3, "in"),
                              padding = grid::unit(12, "pt"))
  hb <- grid::convertHeight(grid::grobHeight(bare), "bigpts", valueOnly = TRUE)
  hp <- grid::convertHeight(grid::grobHeight(padded), "bigpts", valueOnly = TRUE)
  expect_gt(hp, hb)
  # Width is the requested width; padding eats into the content, not out.
  expect_equal(
    grid::convertWidth(grid::grobWidth(padded), "in", valueOnly = TRUE), 3,
    tolerance = 1e-6
  )
})

test_that("padding and margin reject bad units", {
  expect_error(markdown_box_grob("x", padding = 5), "grid unit")
  expect_error(markdown_box_grob("x", margin = grid::unit(c(1, 2), "pt")),
               "length 1 or 4")
})

test_that("makeContent produces a box plus the stacked content", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  g <- markdown_box_grob(md_doc, width = grid::unit(4, "in"),
                         box_gp = grid::gpar(fill = "grey95"))
  kids <- grid::makeContent(g)$children
  expect_length(kids, 2L)                      # box rect + content tree
  expect_gt(length(kids[[2]]$children), 5L)    # one grob per block/line
  # With no box_gp there is no rect.
  g2 <- markdown_box_grob(md_doc, width = grid::unit(4, "in"))
  expect_length(grid::makeContent(g2)$children, 1L)
})

test_that("markdown_box_grob validates its input", {
  expect_error(markdown_box_grob(NA_character_), "must not be NA")
  expect_error(markdown_box_grob(c("a", "b")), "single character string")
  expect_s3_class(markdown_box_grob("ok"), "markdownbox")
})

test_that("visual: markdown document box", {
  skip_if_not_installed("vdiffr")
  skip_on_os("mac")
  # vdiffr records every family as sans, so the code block looks
  # proportional here; the grob does carry fontfamily = "mono".
  vdiffr::expect_doppelganger("markdown-box", function() {
    grid::grid.draw(markdown_box_grob(
      md_doc,
      width = grid::unit(4.4, "in"),
      padding = grid::unit(10, "pt"),
      box_gp = grid::gpar(fill = "grey96", col = "grey40"),
      gp = grid::gpar(fontsize = 13)
    ))
  })
})

test_that("a raw HTML block is dropped, not typeset", {
  md <- "Before\n\n<div class='x'>raw **html** block</div>\n\nAfter"
  tex <- .md_to_tex(md)
  expect_false(grepl("<div", tex, fixed = TRUE))
  expect_false(grepl("**html**", tex, fixed = TRUE))
  expect_match(tex, "Before", fixed = TRUE)
  expect_match(tex, "After", fixed = TRUE)

  # Both entry points must reach the same conclusion.
  types <- vapply(.md_parse_blocks(md), function(b) b$type, character(1))
  expect_setequal(types, c("paragraph", "paragraph"))

  g <- markdown_grob(md)
  expect_false(grepl("<div", paste(stats::na.omit(g$layout_df$text),
                                   collapse = ""), fixed = TRUE))
})

# --- CommonMark coverage gaps ------------------------------------------

test_that("GFM task list items get a checkbox marker", {
  l <- Filter(function(b) b$type == "list",
              .md_parse_blocks("- [ ] todo\n- [x] done\n- plain"))[[1]]
  expect_equal(l$checked, c(FALSE, TRUE, NA))
  expect_equal(.md_list_marker(FALSE, 1, 1, FALSE, FALSE), "\\square")
  expect_equal(.md_list_marker(FALSE, 1, 2, FALSE, TRUE), "\\blacksquare")
  expect_equal(.md_list_marker(FALSE, 1, 3, FALSE, NA), "\\bullet")
  # Both states are single glyphs of the same size (\boxtimes is larger).
  d_unchecked <- latex_dims("\\square", input_mode = "math")
  d_checked   <- latex_dims("\\blacksquare", input_mode = "math")
  expect_equal(nrow(latex_grob("\\square", input_mode = "math")$layout_df), 1L)
  expect_equal(nrow(latex_grob("\\blacksquare", input_mode = "math")$layout_df), 1L)
  expect_equal(as.numeric(d_checked$width), as.numeric(d_unchecked$width))
  expect_equal(as.numeric(d_checked$height), as.numeric(d_unchecked$height))
})

test_that("a list marker sits on its own item's text baseline", {
  # Including items whose first line is tall (a superscript, a fraction).
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  gp <- grid::gpar(fontsize = 12)
  blocks <- .md_parse_blocks(paste(
    "- plain bullet, no math",
    "- plain bullet with $x^2$",
    "- deep $\\frac{a}{b}$ fraction",
    "- a long bullet item that has to wrap because it is very wide indeed",
    sep = "\n"))
  items <- .md_layout(blocks, 200, gp, 12)$items
  # Items alternate marker, body. `y` is the top of each box and
  # (bbox_h - bbox_bl_bp) the first baseline measured down from it.
  bl <- function(it) it$y + it$grob$bbox_h - it$grob$bbox_bl_bp
  for (k in seq(1L, length(items), by = 2L)) {
    expect_equal(bl(items[[k]]), bl(items[[k + 1L]]),
                 label = items[[k + 1L]]$grob$tex)
    # Which means the marker is pushed *down* from the top of its line.
    expect_gt(items[[k]]$y, items[[k + 1L]]$y - 1e-9)
  }
})

test_that("table column alignment is carried into the tabular spec", {
  tb <- Filter(function(b) b$type == "table",
               .md_parse_blocks("| a | b | c |\n|:--|:-:|--:|\n| 1 | 2 | 3 |"))[[1]]
  expect_match(tb$tex, paste0("\\begin{tabular}{X>{\\centering\\arraybackslash}X",
                              ">{\\raggedleft\\arraybackslash}X}"), fixed = TRUE)
  # Default when the source gives no alignment.
  tb2 <- Filter(function(b) b$type == "table",
                .md_parse_blocks("| a | b |\n|---|---|\n| 1 | 2 |"))[[1]]
  expect_match(tb2$tex, "\\begin{tabular}{XX}", fixed = TRUE)
})

test_that("a wide table fits its box, its columns sharing the width", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  wide <- paste0("| id | description | value |\n|---|---|--:|\n",
                 "| 1 | a long description of the first thing measured in the study | 3.14 |")
  lay <- .md_box_layout(markdown_box_grob(wide, width = grid::unit(200, "bigpts")))
  it <- lay$items[[length(lay$items)]]
  expect_lte(it$w, lay$content_w + 0.5)
  narrow <- "| a | b |\n|---|---|\n| 1 | 2 |"
  lay <- .md_box_layout(markdown_box_grob(narrow, width = grid::unit(200, "bigpts")))
  expect_lt(lay$items[[length(lay$items)]]$w, 100)
})

test_that("ordered lists keep the delimiter the author used", {
  period <- Filter(function(b) b$type == "list",
                   .md_parse_blocks("1. one\n2. two"))[[1]]
  paren <- Filter(function(b) b$type == "list",
                  .md_parse_blocks("1) one\n2) two"))[[1]]
  expect_false(period$paren)
  expect_true(paren$paren)
  expect_equal(.md_list_marker(TRUE, 1, 1, FALSE, NA), "\\text{1.}")
  expect_equal(.md_list_marker(TRUE, 1, 1, TRUE, NA), "\\text{1)}")
})

test_that("a block image is drawn as a raster and scaled to the column", {
  skip_if_not_installed("png")
  f <- tempfile(fileext = ".png")
  png::writePNG(array(0.5, c(100, 400, 3)), f)   # 400x100 px
  on.exit(unlink(f), add = TRUE)
  pdf(NULL); on.exit(dev.off(), add = TRUE)

  # Only a paragraph of nothing but images is an image block.
  types <- vapply(.md_parse_blocks(paste0("Before\n\n![alt](", f, ")\n\nAfter")),
                  function(b) b$type, character(1))
  expect_equal(types, c("paragraph", "image", "paragraph"))
  inline <- paste0("text ![alt](", f, ") more")
  expect_equal(vapply(.md_parse_blocks(inline), function(b) b$type,
                      character(1)), "paragraph")
  expect_match(.md_to_tex(inline), "includegraphics", fixed = TRUE)

  r <- .image_raster(f)
  expect_equal(c(r$w_px, r$h_px), c(400, 100))

  g <- markdown_box_grob(paste0("![x](", f, ")"), width = grid::unit(100, "bigpts"))
  content <- grid::makeContent(g)$children
  content <- content[[length(content)]]$children
  ras <- Filter(function(k) inherits(k, "rastergrob"), content)[[1]]
  w <- grid::convertWidth(ras$width, "bigpts", valueOnly = TRUE)
  h <- grid::convertHeight(ras$height, "bigpts", valueOnly = TRUE)
  expect_lte(w, 100 + 0.5)             # scaled down to the column
  expect_equal(h / w, 100 / 400, tolerance = 0.01)   # aspect preserved
})

# --- inline HTML ---------------------------------------------------------
#
# Each tag renders as HTML's default style prescribes; tags with no default
# style are dropped and their text kept.

md_colours <- function(md) {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  unique(markdown_grob(md)$layout_df$color)
}
md_types <- function(md) {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  unique(markdown_grob(md)$layout_df$type)
}
# Everything observable about a rendered string, for the tag matrix below.
md_render <- function(md, fontsize = 14) {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  g <- markdown_grob(md, gp = grid::gpar(fontsize = fontsize))
  df <- g$layout_df
  fs <- df$font_style[!is.na(df$font_style)]
  list(bold   = any(bitwAnd(fs, 2L) > 0L),
       italic = any(bitwAnd(fs, 4L) > 0L),
       mono   = any(bitwAnd(fs, 128L) > 0L),
       lines  = sum(df$type == "line"),
       fills  = sum(df$type == "fill_rect"),
       colour = any(df$color != "#000000", na.rm = TRUE),
       w = g$bbox_w, h = g$bbox_h, n = nrow(df))
}

# One row per supported tag. The coverage check below fails if a tag is
# added to .MD_HTML_TAGS without an entry here.
md_tag_effect <- list(
  bold    = c("b", "strong"),
  italic  = c("i", "em", "cite", "dfn", "var", "address"),
  mono    = c("code", "kbd", "samp", "tt"),
  rule    = c("u", "ins", "s", "del", "strike"),
  script  = c("sub", "sup"),
  fill    = "mark",
  smaller = "small",
  larger  = "big",
  quotes  = "q"
)

test_that("every supported tag renders what HTML prescribes for it", {
  # Mid-sentence: some tags (<address>) start an HTML block at line start.
  wrap <- function(tag) sprintf("i <%s>Hg</%s> j", tag, tag)
  ref <- md_render("i Hg j")

  for (effect in names(md_tag_effect)) {
    for (tag in md_tag_effect[[effect]]) {
      got <- md_render(wrap(tag))
      lab <- sprintf("<%s> (%s)", tag, effect)
      switch(effect,
        bold    = expect_true(got$bold && !got$italic && !got$mono, label = lab),
        italic  = expect_true(got$italic && !got$bold && !got$mono, label = lab),
        mono    = expect_true(got$mono && !got$bold && !got$italic, label = lab),
        rule    = expect_equal(got$lines, 1L, label = lab),
        # A script is set smaller, so the run is narrower.
        script  = expect_lt(got$w, ref$w),
        fill    = expect_true(got$fills == 1L && got$colour, label = lab),
        smaller = expect_lt(got$w, ref$w),
        larger  = expect_gt(got$w, ref$w),
        # <q> adds quotation marks, so it gains width.
        quotes  = expect_gt(got$w, ref$w, label = lab)
      )
      # Nothing here should draw a rule or a fill it was not asked for.
      if (!identical(effect, "rule")) expect_equal(got$lines, 0L, label = lab)
      if (!identical(effect, "fill")) expect_equal(got$fills, 0L, label = lab)
    }
  }

  # <q> and <span> are handled in .md_html_tag(), not .MD_HTML_TAGS.
  expect_setequal(names(.MD_HTML_TAGS), setdiff(unlist(md_tag_effect), "q"))
  expect_true(all(c("b", "strong", "i", "em", "sub", "sup") %in%
                    names(.MD_HTML_TAGS)))

  # No default style in a browser either, so the text is the rendering. An
  # <a> without href is plain text; <a href> is tested below.
  for (tag in c("a", "abbr", "span", "bdi", "bdo", "data", "time", "wbr",
                "output", "nobr")) {
    expect_equal(.md_to_tex(sprintf("<%s>Hg</%s>", tag, tag)), "Hg", label = tag)
  }
})

test_that("a style attribute is read for colour, size and family", {
  sz <- function(css, base = 20) {
    pdf(NULL); on.exit(dev.off(), add = TRUE)
    markdown_grob(sprintf('<span style="font-size:%s">Hg</span>', css),
                  gp = grid::gpar(fontsize = base))$layout_df$font_size[1]
  }
  fam <- function(css) {
    pdf(NULL); on.exit(dev.off(), add = TRUE)
    df <- markdown_grob(sprintf('<span style="font-family:%s">Hg</span>',
                                css))$layout_df
    df <- df[df$type == "text", ]
    unique(vapply(seq_len(nrow(df)), function(i)
      .resolve_text_family(df$font_style[i], "-", df$font_family[i]),
      character(1)))
  }

  # colour: hex, #abc, rgb(), and any R colour name, resolved to hex.
  for (css in c("red", "#FF0000", "#f00", "rgb(255,0,0)")) {
    expect_true("#FF0000" %in%
                  md_colours(sprintf('<span style="color:%s">x</span>', css)),
                label = css)
  }
  expect_true("#4682B4" %in% md_colours('<span style="color:steelblue">x</span>'))
  # The CSS names R's palette lacks.
  expect_true("#DC143C" %in% md_colours('<span style="color:crimson">x</span>'))
  for (nm in names(.MD_CSS_COLORS)) expect_false(is.null(.md_resolve_color(nm)))
  # Single-quoted attribute.
  expect_true("#FF0000" %in% md_colours("<span style='color:red'>x</span>"))

  # font-size: absolute and relative units.
  expect_equal(sz("10pt"), 10)
  expect_equal(sz("20px"), 15)            # 96 px to the inch
  expect_equal(sz("0.25in"), 18)
  expect_equal(sz("1cm"), 72 / 2.54, tolerance = 0.05)
  expect_equal(sz("5mm"), 72 / 25.4 * 5, tolerance = 0.05)
  expect_equal(sz("0.5em"), 10)
  expect_equal(sz("150%"), 30)
  expect_equal(sz("larger"), 24)
  # An absolute size is absolute: it must not drift with the base.
  for (base in c(10, 20, 40)) expect_equal(sz("10pt", base), 10)
  # Anything unparseable leaves the size alone rather than guessing.
  for (bad in c("nonsense", "-5pt", "0pt", "12", "")) {
    expect_equal(sz(bad), 20, label = bad)
  }

  # font-family: CSS generics map to grid's aliases; a name is kept as is.
  expect_equal(fam("monospace"), "mono")
  expect_equal(fam("sans-serif"), "sans")
  expect_equal(fam("serif"), "serif")
  expect_equal(fam("Georgia"), "Georgia")
  # A fallback list resolves to its first entry, case preserved.
  expect_equal(fam("'Courier New', monospace"), "Courier New")
  expect_match(.md_to_tex('<span style="font-family:Georgia">x</span>'),
               "{\\fontspec{Georgia}{}", fixed = TRUE)
  # The name is spliced into LaTeX, so it must not carry parser syntax.
  expect_equal(.md_to_tex('<span style="font-family:a}b\\c">x</span>'),
               "{\\fontspec{abc}{}x}")
  # A `%` would comment out the closing brace.
  expect_equal(.md_to_tex('<span style="font-family:Foo%bar">x</span>'),
               "{\\fontspec{Foobar}{}x}")
})

test_that("styles nest, compose, and survive a rule", {
  for (md in c("**~~Hg~~**", "<b>~~Hg~~</b>", "**<u>Hg</u>**",
               "<b><u>Hg</u></b>", "<b><s>Hg</s></b>")) {
    got <- md_render(md)
    expect_true(got$bold, label = md)
    expect_equal(got$lines, 1L, label = md)
  }
  # Both nesting orders agree.
  expect_equal(md_render("<b><u>Hg</u></b>"), md_render("<u><b>Hg</b></u>"))
  expect_equal(md_render("**~~Hg~~**"), md_render("~~**Hg**~~"))
  expect_equal(md_render("<b>**Hg**</b>"), md_render("**<b>Hg</b>**"))
  expect_true(md_render("*~~Hg~~*")$italic)
  expect_true(md_render("~~`Hg`~~")$mono)
  expect_true(all(unlist(md_render("**_~~Hg~~_**")[c("bold", "italic")])))
  expect_equal(md_render("<u><s>Hg</s></u>")$lines, 2L)
  expect_true(md_render("<b>H<sup>2</sup></b>")$bold)

  # Tags compose with each other, with a styled span, and with markdown.
  expect_true(all(unlist(md_render("<b><i>Hg</i></b>")[c("bold", "italic")])))
  expect_true(all(unlist(md_render("<b><code>Hg</code></b>")[c("bold", "mono")])))
  expect_true(md_render('<span style="color:red"><b>Hg</b></span>')$bold)
  expect_true(md_render('<b><span style="color:red">Hg</span></b>')$colour)
  expect_true(md_render("<mark><b>Hg</b></mark>")$fills == 1L)
  # Two CSS properties on one span open two commands and close both.
  one <- '<span style="color:red;text-decoration:underline">x</span>'
  expect_true("#FF0000" %in% md_colours(one))
  expect_true("line" %in% md_types(one))
  # Markdown and math inside a span are still parsed, and inherit the style.
  mixed <- '<span style="color:red">a **b** $x^2$ c</span>'
  expect_true("#FF0000" %in% md_colours(mixed))
  expect_match(.md_to_tex(mixed), "\\textbf{", fixed = TRUE)
  expect_gt(md_render("<b>a $x^2$ b</b>")$w, md_render("<b>a b</b>")$w)
})

test_that("a span's family beats gp$fontfamily, and only inside the span", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  df <- markdown_grob('a <span style="font-family:mono">b</span> c',
                      gp = grid::gpar(fontsize = 20,
                                      fontfamily = "serif"))$layout_df
  df <- df[df$type == "text", ]
  fams <- vapply(seq_len(nrow(df)), function(i)
    .resolve_text_family(df$font_style[i], "serif", df$font_family[i]),
    character(1))
  expect_equal(fams[df$text == "b"], "mono")
  expect_true(all(fams[df$text %in% c("a", "c")] == "serif"))
})

test_that("markup we do not interpret is dropped, and stays balanced", {
  # Unrecognised tags, and CSS we cannot honour, keep the text.
  for (md in c("<abbr>x</abbr>", "<span>x</span>",
               '<span style="color:notacolor">x</span>')) {
    expect_equal(.md_to_tex(md), "x", label = md)
  }
  # Block-level HTML is dropped whole.
  tex <- .md_to_tex("Before\n\n<div>raw **html**</div>\n\nAfter")
  expect_false(grepl("<div", tex, fixed = TRUE))
  expect_match(tex, "Before", fixed = TRUE)
  expect_match(tex, "After", fixed = TRUE)

  # A code span is never scanned for tags.
  tex <- .md_to_tex("`<u>x</u>`")
  expect_match(tex, "\\texttt{", fixed = TRUE)
  expect_match(tex, "<u>x</u>", fixed = TRUE)
  expect_false(grepl("\\uline", tex, fixed = TRUE))

  # However malformed the input, every command opened is closed.
  balanced <- function(tex) {
    ob <- lengths(regmatches(tex, gregexpr("(?<!\\\\)\\{", tex, perl = TRUE)))
    cb <- lengths(regmatches(tex, gregexpr("(?<!\\\\)\\}", tex, perl = TRUE)))
    ob == cb
  }
  for (md in c("<u>unclosed", "</u>stray", "<b>unclosed", "</b>stray",
               '<span style="color:red">a<u>b</span>c',
               "<b>a<u>b</b>c", "<u><b>x</u></b>", "<b><i>x</b></i>",
               "<mark>a<b>b</mark>c", "<b><q>x</b></q>")) {
    expect_true(balanced(.md_to_tex(md)), label = md)
  }
  # An unclosed tag still styles what follows it.
  expect_match(.md_to_tex("<u>unclosed"), "\\uline{", fixed = TRUE)
})

test_that("inline HTML works in the block renderer too", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  g <- markdown_box_grob(
    'Text with <span style="color:red">red</span> and <u>under</u>.',
    width = grid::unit(3, "in"))
  kids <- grid::makeContent(g)$children
  inner <- kids[[length(kids)]]$children
  cols <- unlist(lapply(inner, function(k) k$layout_df$color))
  expect_true("#FF0000" %in% cols)
})

test_that("consecutive breaks leave a blank line", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  gp <- grid::gpar(fontsize = 15)
  h <- function(md) .md_measure(.md_to_tex(md), 0, gp)$h
  # "Agp" has an ascender and a descender, so a row is full height.
  one <- h("Agp<br>Agp")
  lead <- one - h("Agp")
  # Each extra break adds exactly one more line, not zero.
  expect_equal(h("Agp<br><br>Agp"), one + lead)
  expect_equal(h("Agp<br><br><br>Agp"), one + 2 * lead)
  # A whitespace-only row between breaks counts as empty.
  expect_equal(h("Agp<br>\n<br>\nAgp"), one + lead)
  expect_equal(h("Agp<br> <br>Agp"), one + lead)
  # Markdown's own hard break (backslash at end of line) goes the same way.
  expect_equal(h("Agp\\\n\\\nAgp"), one + lead)
  # A single break is untouched, and a trailing one adds no dangling row.
  expect_identical(.md_to_tex("a<br>b"), "a\\\\b")
  expect_equal(h("Agp<br>"), h("Agp"))
})

test_that("a registered user font can be named by a span", {
  # A bundled OTF stands in for the user's file.
  skip_if_not_installed("ragg")
  otf <- system.file("fonts", package = "gridmicrotex")
  files <- list.files(otf, pattern = "\\.otf$", full.names = TRUE,
                      recursive = TRUE)
  skip_if(length(files) == 0L, "no bundled OTF to register")
  systemfonts::register_font(name = "gmTestUserFont", plain = files[1])

  f <- tempfile(fileext = ".png")
  ragg::agg_png(f, width = 400, height = 120)
  on.exit(unlink(f), add = TRUE)
  w_user <- as.numeric(latex_dims("\\gmfontfamily{gmTestUserFont}{Wig}",
                                  input_mode = "math")$width)
  w_plain <- as.numeric(latex_dims("\\text{Wig}", input_mode = "math")$width)
  dev.off()
  expect_gt(w_user, 0)
  expect_false(isTRUE(all.equal(w_user, w_plain)))
})

# --- regressions: block nesting and gp handling --------------------------

test_that("an absolute font-size resolves inside a list item or table cell", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Every font size anywhere in the laid-out box.
  sizes <- function(md) {
    out <- numeric()
    walk <- function(g) {
      out <<- c(out, g$layout_df$font_size)
      for (ch in g$children) walk(ch)
    }
    walk(grid::makeContent(markdown_box_grob(md, width = grid::unit(3, "in"))))
    sort(unique(stats::na.omit(out)))
  }
  for (md in c("- <span style=\"font-size:12pt\">big</span> item",
               "| <span style=\"font-size:12pt\">a</span> | b |\n|---|---|\n| 1 | 2 |")) {
    expect_true(12 %in% sizes(md), info = md)
  }
  # And it is a real override, not the default in disguise.
  expect_false(12 %in% sizes("- big item"))
})

test_that("a table cell sizes against the caller's font size", {
  blk <- .md_parse_blocks(
    "| <span style=\"font-size:40pt\">big</span> | b |\n|---|---|\n| 1 | 2 |",
    base = 40)[[1]]
  expect_match(blk$tex, "\\textscale{1.0", fixed = TRUE)
})

test_that("a rule, image or quote nested in a container lays out", {
  # Rules and quote bars are grobs with no viewport.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  kinds <- function(md) {
    out <- character()
    walk <- function(g) {
      out <<- c(out, class(g)[1])
      for (ch in g$children) walk(ch)
    }
    walk(grid::makeContent(markdown_box_grob(md, width = grid::unit(3, "in"))))
    out
  }
  for (md in c("> a\n>\n> ---\n>\n> b",
               "- a\n\n  ---\n\n- b",
               "- item\n\n  > quoted",
               "> outer\n>\n> > inner")) {
    k <- kinds(md)
    # Each case pairs prose with a rect: the rule, or the quote bar.
    expect_true("latexgrob" %in% k, info = md)
    expect_true("rect" %in% k, info = md)
  }
})

test_that("a nested rule is offset by its container, not left at the top", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  blocks <- .md_parse_blocks("intro\n\n> quoted\n>\n> ---\n>\n> more")
  lay <- .md_layout(blocks, 300, grid::gpar(fontsize = 12), 12)
  rules <- Filter(function(it) inherits(it$grob, "rect"), lay$items)
  # The quote's bar, and the rule inside it, below the quote's first line.
  expect_length(rules, 2L)
  expect_gt(max(vapply(rules, function(it) it$y, numeric(1))), 0)
})

test_that("markdown_box_grob() applies gp$cex exactly once", {
  skip_if_not_installed("svglite")
  sizes <- function(gp) {
    f <- tempfile(fileext = ".svg")
    svglite::svglite(f, width = 6, height = 4)
    grid::grid.draw(markdown_box_grob("hello world",
                                      width = grid::unit(3, "in"), gp = gp))
    dev.off()
    on.exit(unlink(f), add = TRUE)
    s <- readLines(f, warn = FALSE)
    unique(unlist(regmatches(s, gregexpr("font-size: *[0-9.]+", s))))
  }
  expect_equal(sizes(grid::gpar(fontsize = 10, cex = 2)),
               sizes(grid::gpar(fontsize = 20)))
})

# The x of a block item: a raster's own `x`, or the viewport's for a vector
# picture (grImport2's vpStack has a NULL `x`).
block_xs <- function(g) {
  kids <- grid::makeContent(g)$children[[1]]$children
  vapply(kids, function(k) {
    u <- if (!is.null(k$x)) k$x else if (!is.null(k$vp)) k$vp$x else NULL
    if (is.null(u)) NA_real_ else grid::convertX(u, "bigpts", valueOnly = TRUE)
  }, numeric(1))
}

test_that("halign moves an image that has room, and leaves a full-width rule", {
  skip_if_not_installed("png")
  f <- tempfile(fileext = ".png")
  png::writePNG(array(0.5, c(20, 40, 3)), f)   # 40x20 px, narrow
  on.exit(unlink(f), add = TRUE)
  pdf(NULL); on.exit(dev.off(), add = TRUE)

  xs <- function(ha) {
    block_xs(markdown_box_grob(paste0("---\n\n![a](", f, ")"),
                               width = grid::unit(4, "in"), halign = ha,
                               gp = grid::gpar(fontsize = 12)))
  }
  left <- xs(0)
  right <- xs(1)
  expect_equal(left[[1]], right[[1]])   # the rule spans the column
  expect_gt(right[[2]], left[[2]])      # the image has slack and uses it
})

test_that("a block image aligns the same whether it is raster or vector", {
  skip_if_not_installed("png")
  skip_if_not_installed("svglite")
  skip_if_not_installed("rsvg")
  skip_if_not_installed("grImport2")
  pdf(NULL); on.exit(dev.off(), add = TRUE)

  p <- tempfile(fileext = ".png")
  png::writePNG(array(0.5, c(20, 40, 3)), p)
  s <- tempfile(fileext = ".svg")
  svglite::svglite(s, width = 40 / 72, height = 20 / 72)
  grid::grid.rect(gp = grid::gpar(fill = "grey50", col = NA))
  grDevices::dev.off()
  on.exit(unlink(c(p, s)), add = TRUE)

  at <- function(f, ha) {
    block_xs(markdown_box_grob(sprintf("![a](%s)", f),
                               width = grid::unit(4, "in"), halign = ha,
                               gp = grid::gpar(fontsize = 12)))[[1]]
  }
  for (f in c(p, s)) {
    expect_equal(at(f, 0), 0, info = f)
    expect_gt(at(f, 1), at(f, 0.5))
    expect_gt(at(f, 0.5), at(f, 0))
  }

  # And the cascade reaches an image block as well as the argument does.
  centred <- block_xs(markdown_box_grob(
    sprintf("![a](%s)", p), width = grid::unit(4, "in"),
    style = markdown_style(img = md_style(text_align = "center")),
    gp = grid::gpar(fontsize = 12)))[[1]]
  expect_gt(centred, 0)
})

test_that("private-use characters in the input cannot forge a math span", {
  # The mask sentinels are private-use codepoints, which icon fonts use.
  inj <- paste0("icon ", intToUtf8(0xE000), "1", intToUtf8(0xE001),
                " and $y$")
  out <- .md_to_tex(inj)
  expect_match(out, "icon 1 and ", fixed = TRUE)
  expect_equal(lengths(regmatches(out, gregexpr("y", out))), 1L)
})

test_that("markdown_grob() rejects input_mode instead of failing obscurely", {
  expect_error(markdown_grob("x", input_mode = "mixed"), "input_mode")
})

test_that("<ruby> annotates its base with \\overset", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)

  # \overset takes the annotation first; base and gloss are text in it.
  expect_equal(.md_to_tex("<ruby>base<rt>gloss</rt></ruby>"),
               "$\\overset{\\text{\\scalebox{0.5}{gloss}}}{\\text{base}}$")
  # It composes with markdown inside the base.
  expect_match(.md_to_tex("<ruby>**b**<rt>g</rt></ruby>"),
               "{\\text{\\textbf{b}}}", fixed = TRUE)
  # Surrounding text is unaffected.
  expect_match(.md_to_tex("x <ruby>a<rt>b</rt></ruby> y"), "^x \\$", perl = TRUE)

  # Degenerate forms keep the text rather than losing it.
  expect_equal(.md_to_tex("<ruby>no rt</ruby>"), "no rt")
  expect_match(.md_to_tex("<ruby>unclosed<rt>g"), "overset", fixed = TRUE)
  expect_equal(.md_to_tex("<rt>orphan</rt>"), "orphan")

  # An annotation adds height, not width -- it sits above the base.
  w <- function(t) .md_measure(t, 0, grid::gpar(fontsize = 16))$w
  h <- function(t) .md_measure(t, 0, grid::gpar(fontsize = 16))$h
  plain <- .md_to_tex("base")
  ruby <- .md_to_tex("<ruby>base<rt>gloss</rt></ruby>")
  expect_equal(w(ruby), w(plain))
  expect_gt(h(ruby), h(plain))
})

test_that("&nbsp; becomes a non-breaking space", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # TeX's `~`: as wide as a space, and no line breaks at it.
  expect_equal(.md_to_tex("a&nbsp;b"), "a~b")
  w <- function(t) .md_measure(t, 0, grid::gpar(fontsize = 16))$w
  expect_equal(w(.md_to_tex("a&nbsp;b")), w(.md_to_tex("a b")), tolerance = 0.1)
  joined <- paste(rep("word", 12), collapse = "&nbsp;")
  g <- markdown_grob(joined, max_width = 100)
  expect_identical(length(unique(round(g$layout_df$y))), 1L)
})

test_that("font-size takes CSS absolute keywords", {
  expect_equal(.md_css_size("medium", 20), 1)
  expect_lt(.md_css_size("xx-small", 20), .md_css_size("x-small", 20))
  expect_lt(.md_css_size("x-small", 20), .md_css_size("small", 20))
  expect_lt(.md_css_size("small", 20), .md_css_size("medium", 20))
  expect_lt(.md_css_size("medium", 20), .md_css_size("large", 20))
  expect_lt(.md_css_size("large", 20), .md_css_size("x-large", 20))
  expect_lt(.md_css_size("x-large", 20), .md_css_size("xx-large", 20))
  # Unknown keywords stay invalid.
  expect_null(.md_css_size("enormous", 20))
})

test_that("width = NULL sizes the box to its content", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  g <- markdown_box_grob(
    "a **long** paragraph that would certainly wrap in a narrow box",
    width = NULL)
  w <- vapply(c(2, 6, 9), function(wd) {
    grid::pushViewport(grid::viewport(width = grid::unit(wd, "in")))
    on.exit(grid::popViewport(), add = TRUE)
    grid::convertWidth(grid::widthDetails(g), "bigpts", valueOnly = TRUE)
  }, numeric(1))
  # The size must not depend on the parent, so ggplot2 can measure it first.
  expect_equal(w, rep(w[1], 3))
  expect_gt(w[1], 100)

  h <- vapply(c(2, 9), function(wd) {
    grid::pushViewport(grid::viewport(width = grid::unit(wd, "in")))
    on.exit(grid::popViewport(), add = TRUE)
    grid::convertHeight(grid::heightDetails(g), "bigpts", valueOnly = TRUE)
  }, numeric(1))
  expect_equal(h[1], h[2])
})

test_that("the natural width does not re-break the line it was measured from", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Widths are rounded to whole big points, which can be just too short.
  head_only <- "## Fuel economy falls with weight"
  one <- markdown_box_grob(head_only, width = NULL)
  h1 <- grid::convertHeight(grid::heightDetails(one), "bigpts",
                            valueOnly = TRUE)
  # One line of an h2, not two.
  expect_lt(h1, 40)

  with_list <- markdown_box_grob(
    paste(head_only, "", "- a bullet", sep = "\n"), width = NULL)
  h2 <- grid::convertHeight(grid::heightDetails(with_list), "bigpts",
                            valueOnly = TRUE)
  expect_lt(h2 - h1, 40)
})

test_that("a rule does not inflate the natural width", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # A rule spans whatever width it is given.
  g <- markdown_box_grob("short\n\n---\n\nalso short", width = NULL)
  w <- grid::convertWidth(grid::widthDetails(g), "bigpts", valueOnly = TRUE)
  expect_lt(w, .MD_PROBE_W / 10)
})

test_that("a natural-width box still draws, and honours hjust", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # makeContent() supplies a viewport of the measured size.
  g <- markdown_box_grob("# H\n\n- a\n- b", width = NULL, hjust = 0)
  expect_null(g$vp)
  # hjust is the viewport's justification; its x stays at 0.5npc.
  for (hj in c(0, 0.5, 1)) {
    ct <- grid::makeContent(
      markdown_box_grob("# H\n\n- a\n- b", width = NULL, hjust = hj))
    expect_s3_class(ct, "markdownbox")
    expect_equal(ct$children[[1]]$vp$justification[1], hj)
  }
})

test_that("an inline <a> is styled by the same rule as [text](url)", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  col <- function(md, ...) {
    d <- markdown_grob(md, gp = grid::gpar(fontsize = 14), ...)$layout_df
    d <- d[d$type == "text" & !is.na(d$text) & d$text == "LINK", ]
    d$color[1]
  }
  expect_equal(col("a <a href='http://x'>LINK</a> c"),
               col("a [LINK](http://x) c"))
  expect_equal(col("a [LINK](http://x) c"), .MD_LINK_COLOR)

  # An anchor with no href is ordinary text, as in a browser.
  expect_equal(.md_to_tex("<a>LINK</a>"), "LINK")
  anchor <- markdown_grob("a <a name='x'>LINK</a> c")$layout_df
  expect_false(.MD_LINK_COLOR %in% anchor$color)

  # Restyling the `a` rule reaches both.
  green <- markdown_style(a = md_style(color = "green"))
  expect_equal(col("a <a href='http://x'>LINK</a> c", style = green),
               col("a [LINK](http://x) c", style = green))
  expect_false(col("a <a href='http://x'>LINK</a> c", style = green) ==
                 .MD_LINK_COLOR)
})
