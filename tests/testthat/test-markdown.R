test_that("math spans survive the markdown parser byte-for-byte", {
  # The regression that motivates masking: CommonMark treats `\` before
  # ASCII punctuation as an escape, so an unmasked `\\` row separator
  # collapses to `\` and the matrix loses its second row.
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
  # \sout is the horizontal rule (ulem); \cancel is a diagonal slash and
  # is NOT the right target for ~~...~~.
  expect_match(.md_to_tex("~~s~~"), "\\sout{", fixed = TRUE)
  expect_false(grepl("\\cancel", .md_to_tex("~~s~~"), fixed = TRUE))
})

test_that("prose is wrapped in \\text{} rather than left as math italics", {
  # Output feeds latex_grob(input_mode = "math"), so bare prose would be
  # typeset as spaced math italics.
  expect_match(.md_to_tex("plain words"), "\\text{plain words}", fixed = TRUE)
  g <- markdown_grob("plain words here")
  expect_true(all(g$layout_df$type == "text"))
})

test_that("markdown-only constructs never emit unknown LaTeX commands", {
  # MicroTeX renders an unknown command as literal glyphs instead of
  # erroring, so a leaked \href would silently typeset the letters.
  tex <- .md_to_tex(paste(
    "# Heading", "", "a [link](http://x.com) and ![alt](i.png)", "",
    "> quote", "", "```", "code", "```", sep = "\n"))
  for (bad in c("\\section", "\\href", "\\includegraphics", "verbatim",
                "\\linewidth", "\\begin{quote}")) {
    expect_false(grepl(bad, tex, fixed = TRUE), label = paste("leaked", bad))
  }
  # Link and image degrade to their text, which must survive.
  expect_match(.md_to_tex("[label](http://x)"), "label", fixed = TRUE)
  expect_match(.md_to_tex("![alt text](i.png)"), "alt text", fixed = TRUE)
})

test_that("TeX special characters in prose are escaped", {
  expect_match(.md_to_tex("100% done"), "100\\% done", fixed = TRUE)
  expect_match(.md_to_tex("a_b"),       "a\\_b",       fixed = TRUE)
  expect_match(.md_to_tex("x & y"),     "x \\& y",     fixed = TRUE)
  expect_match(.md_to_tex("no #1"),     "no \\#1",     fixed = TRUE)
  expect_match(.md_to_tex("2 ^ 3"),     "\\^{}",       fixed = TRUE)
  # MicroTeX has no \textbackslash -- it would typeset the letters.
  expect_match(.md_to_tex("a \\ b"), "\\backslash{}", fixed = TRUE)
  expect_false(grepl("textbackslash", .md_to_tex("a \\ b"), fixed = TRUE))
})

test_that("a lone dollar sign is literal, not an unclosed math span", {
  # In prose a stray `$` is far more often a price than a delimiter.
  # Masking it would swallow the rest of the line and skip escaping.
  tex <- .md_to_tex("costs $ and & more")
  expect_match(tex, "\\$", fixed = TRUE)
  expect_match(tex, "\\&", fixed = TRUE)
  expect_no_warning(.md_to_tex("costs $ and more"))
})

test_that("code spans stay literal and never leak a sentinel", {
  tex <- .md_to_tex("`code with $x$ inside`")
  expect_match(tex, "\\$x\\$", fixed = TRUE)
  # The private-use sentinels must never reach the output.
  expect_false(grepl("\uE000", tex, fixed = TRUE))
  expect_false(grepl("\uE001", tex, fixed = TRUE))
  expect_false(grepl("\uE002", tex, fixed = TRUE))
})

test_that("wrapping still works for single-paragraph markdown", {
  # Guards against a stray `\\` sneaking into the output: MicroTeX
  # silently ignores max_width once the box tree contains a line break.
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
  expect_invisible(grid.markdown("**hi** $x$"))
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
  # latex_dims() defaults to input_mode = "mixed"; measuring a run that
  # the AST walker already wrapped would double-wrap it and report a
  # width the drawn grob does not have. Blocks then overflow the border.
  skip_if_not_installed("ragg")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- .md_parse_blocks("Some *emphasised* prose with $x^2$ inside it.")[[1]]$tex
  gp <- grid::gpar(fontsize = 13)
  for (w in c(120, 200, 280)) {
    m <- .md_measure(tex, w, gp)
    g <- latex_grob(tex, max_width = m$mw, input_mode = "math", gp = gp)
    expect_equal(m$w, g$bbox_w, tolerance = 1e-6)
    expect_lte(g$bbox_w, w + 0.5)
  }
})

test_that("a soft line break renders as a word space", {
  # In math mode a bare space between two \text{} runs is discarded, so
  # the words either side would be run together ("islong").
  tex <- .md_parse_blocks("alpha\nbeta")[[1]]$tex
  expect_match(tex, "\\text{ }", fixed = TRUE)
  spaced <- latex_grob(tex, input_mode = "math")$bbox_w
  joined <- latex_grob("\\text{alpha}\\text{beta}", input_mode = "math")$bbox_w
  expect_gt(spaced, joined)
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

test_that("it draws without error", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  grid::grid.newpage()
  expect_silent(grid::grid.draw(markdown_box_grob(
    md_doc, width = grid::unit(4, "in"),
    padding = grid::unit(8, "pt"),
    box_gp = grid::gpar(fill = "grey95", col = "grey40")
  )))
})

test_that("visual: markdown document box", {
  skip_if_not_installed("vdiffr")
  skip_on_os("mac")
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
  # An html_block holds raw markup as its text. Walking into it typeset
  # the tags and the unparsed markdown inside them, and the inline path
  # and the block path disagreed about the same document.
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
  # cmark emits <tasklist completed="..."> in place of <item>; without
  # handling it the checkbox is silently dropped and a task list looks
  # like an ordinary bullet list.
  l <- Filter(function(b) b$type == "list",
              .md_parse_blocks("- [ ] todo\n- [x] done\n- plain"))[[1]]
  expect_equal(l$checked, c(FALSE, TRUE, NA))
  expect_equal(.md_list_marker(FALSE, 1, 1, FALSE, FALSE), "\\square")
  expect_equal(.md_list_marker(FALSE, 1, 2, FALSE, TRUE), "\\blacksquare")
  expect_equal(.md_list_marker(FALSE, 1, 3, FALSE, NA), "\\bullet")
  # The two checkbox states must be single glyphs AND the same size, or a
  # task list looks ragged. \square and \blacksquare are both 14x13;
  # \boxtimes (the more literal "checked") is 18x16 and was rejected for
  # exactly this reason.
  d_unchecked <- latex_dims("\\square", input_mode = "math")
  d_checked   <- latex_dims("\\blacksquare", input_mode = "math")
  expect_equal(nrow(latex_grob("\\square", input_mode = "math")$layout_df), 1L)
  expect_equal(nrow(latex_grob("\\blacksquare", input_mode = "math")$layout_df), 1L)
  expect_equal(as.numeric(d_checked$width), as.numeric(d_unchecked$width))
  expect_equal(as.numeric(d_checked$height), as.numeric(d_unchecked$height))
})

test_that("list markers baseline-align with the first line of text", {
  # A marker is shifted down from the line top so its baseline sits on the
  # text baseline; top-aligning left bullets floating above the text.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  g <- markdown_box_grob("- a bullet item", width = grid::unit(3, "in"),
                         gp = grid::gpar(fontsize = 13))
  kids <- grid::makeContent(g)$children
  content <- kids[[length(kids)]]$children
  # First child is the marker, second the item text. The marker's top
  # (its vp$y is the top edge, vjust = 1) must sit below the text's top.
  marker_y <- grid::convertY(content[[1]]$vp$y, "bigpts", valueOnly = TRUE)
  text_y   <- grid::convertY(content[[2]]$vp$y, "bigpts", valueOnly = TRUE)
  # Larger y = higher on the page; the marker top is pushed down, so lower.
  expect_lt(marker_y, text_y)
})

test_that("table column alignment is carried into the tabular spec", {
  tb <- Filter(function(b) b$type == "table",
               .md_parse_blocks("| a | b | c |\n|:--|:-:|--:|\n| 1 | 2 | 3 |"))[[1]]
  expect_match(tb$tex, "\\begin{tabular}{lcr}", fixed = TRUE)
  # Default when the source gives no alignment.
  tb2 <- Filter(function(b) b$type == "table",
                .md_parse_blocks("| a | b |\n|---|---|\n| 1 | 2 |"))[[1]]
  expect_match(tb2$tex, "\\begin{tabular}{ll}", fixed = TRUE)
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

test_that("a paragraph of only images becomes image blocks", {
  skip_if_not_installed("png")
  f <- tempfile(fileext = ".png")
  png::writePNG(array(0.5, c(20, 40, 3)), f)
  on.exit(unlink(f), add = TRUE)

  types <- vapply(.md_parse_blocks(paste0("Before\n\n![alt](", f, ")\n\nAfter")),
                  function(b) b$type, character(1))
  expect_equal(types, c("paragraph", "image", "paragraph"))

  r <- .md_image_raster(f)
  expect_false(is.null(r))
  expect_equal(c(r$w_px, r$h_px), c(40, 20))
})

test_that("an image mixed into a sentence stays inline as alt text", {
  # Only a paragraph containing nothing but images is a block image.
  types <- vapply(.md_parse_blocks("text ![alt](x.png) more"),
                  function(b) b$type, character(1))
  expect_equal(types, "paragraph")
  expect_match(.md_to_tex("text ![alt](x.png) more"), "alt", fixed = TRUE)
})

test_that("an unusable image degrades to its alt text", {
  # Missing file, unsupported format, or reader package absent must not
  # error -- png and jpeg are Suggests.
  expect_null(.md_image_raster("does-not-exist.png"))
  expect_null(.md_image_raster("file.svg"))
  expect_null(.md_image_raster(""))
  expect_null(.md_image_raster(NA_character_))

  pdf(NULL); on.exit(dev.off(), add = TRUE)
  g <- markdown_box_grob("![the alt text](missing.png)",
                         width = grid::unit(3, "in"))
  expect_s3_class(grid::makeContent(g), "markdownbox")
})

test_that("a block image is drawn as a raster and scaled to the column", {
  skip_if_not_installed("png")
  f <- tempfile(fileext = ".png")
  png::writePNG(array(0.5, c(100, 400, 3)), f)   # 400x100 px
  on.exit(unlink(f), add = TRUE)
  pdf(NULL); on.exit(dev.off(), add = TRUE)

  g <- markdown_box_grob(paste0("![x](", f, ")"), width = grid::unit(100, "bigpts"))
  kids <- grid::makeContent(g)$children
  content <- kids[[length(kids)]]$children
  expect_true(any(vapply(content, inherits, logical(1), "rastergrob")))
  ras <- Filter(function(k) inherits(k, "rastergrob"), content)[[1]]
  w <- grid::convertWidth(ras$width, "bigpts", valueOnly = TRUE)
  h <- grid::convertHeight(ras$height, "bigpts", valueOnly = TRUE)
  expect_lte(w, 100 + 0.5)             # scaled down to the column
  expect_equal(h / w, 100 / 400, tolerance = 0.01)   # aspect preserved
})

# --- inline HTML presentational spans -----------------------------------
#
# GFM defines no markdown syntax for colour, underline or super/subscript,
# so raw HTML (which CommonMark includes and GFM keeps) is the only
# conformant way to express them. Only the presentational subset is
# interpreted; everything else keeps the old behaviour of dropping the
# markup and keeping the text.

md_colours <- function(md) {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  unique(markdown_grob(md)$layout_df$color)
}
md_types <- function(md) {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  unique(markdown_grob(md)$layout_df$type)
}

test_that("a colour span colours its content", {
  expect_true("#FF0000" %in% md_colours('<span style="color:red">x</span>'))
  expect_true("#4682B4" %in% md_colours('<span style="color:#4682B4">x</span>'))
  # Any R colour name works, because the name is resolved to hex here
  # rather than handed to MicroTeX's smaller named-colour table.
  expect_true("#4682B4" %in% md_colours('<span style="color:steelblue">x</span>'))
  # #abc shorthand and rgb() both normalise to #RRGGBB.
  expect_true("#FF0000" %in% md_colours('<span style="color:#f00">x</span>'))
  expect_true("#FF0000" %in% md_colours('<span style="color:rgb(255,0,0)">x</span>'))
})

test_that("underline and strike tags draw a rule", {
  for (md in c("<u>x</u>", "<ins>x</ins>", "<s>x</s>", "<del>x</del>")) {
    expect_true("line" %in% md_types(md), label = md)
  }
  expect_match(.md_to_tex("<u>x</u>"), "\\underline{", fixed = TRUE)
  expect_match(.md_to_tex("<s>x</s>"), "\\sout{", fixed = TRUE)
})

test_that("sub and sup map to the text script commands", {
  expect_match(.md_to_tex("<sub>i</sub>"), "\\textsubscript{", fixed = TRUE)
  expect_match(.md_to_tex("<sup>2</sup>"), "\\textsuperscript{", fixed = TRUE)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_gt(markdown_grob("x<sup>2</sup>")$bbox_w, 0)
})

test_that("spans nest, and combine with markdown and math", {
  # Colour outside, underline inside.
  cols <- md_colours('<span style="color:red"><u>x</u></span>')
  expect_true("#FF0000" %in% cols)
  expect_true("line" %in% md_types('<span style="color:red"><u>x</u></span>'))
  # Two CSS properties on one span open two commands and close both.
  one <- '<span style="color:red;text-decoration:underline">x</span>'
  expect_true("#FF0000" %in% md_colours(one))
  expect_true("line" %in% md_types(one))
  # Markdown and math inside a span are still parsed, and inherit the colour.
  mixed <- '<span style="color:red">a **b** $x^2$ c</span>'
  expect_true("#FF0000" %in% md_colours(mixed))
  expect_match(.md_to_tex(mixed), "\\textbf{", fixed = TRUE)
})

test_that("bold/italic tags are deliberately NOT interpreted", {
  # Markdown already has ** and *; mapping <b> to \textbf would force its
  # content to be emitted bare, which sibling-level tags cannot do.
  tex <- .md_to_tex("<b>x</b>")
  expect_equal(tex, "\\text{x}")
  expect_false(grepl("\\textbf", tex, fixed = TRUE))
  expect_equal(.md_to_tex("<i>x</i>"), "\\text{x}")
})

test_that("unrecognised markup is dropped but its text is kept", {
  for (md in c("<abbr>x</abbr>", "<span>x</span>",
               '<span style="color:notacolor">x</span>')) {
    expect_equal(.md_to_tex(md), "\\text{x}", label = md)
  }
  expect_equal(md_colours("<abbr>x</abbr>"), "#000000")
})

test_that("HTML inside a code span stays literal", {
  # Code spans are their own node type and are never scanned for tags, so
  # documenting the syntax in backticks is safe.
  tex <- .md_to_tex("`<u>x</u>`")
  expect_match(tex, "\\texttt{", fixed = TRUE)
  expect_match(tex, "<u>x</u>", fixed = TRUE)
  expect_false(grepl("\\underline", tex, fixed = TRUE))
})

test_that("malformed spans still produce balanced braces", {
  balanced <- function(tex) {
    ob <- lengths(regmatches(tex, gregexpr("(?<!\\\\)\\{", tex, perl = TRUE)))
    cb <- lengths(regmatches(tex, gregexpr("(?<!\\\\)\\}", tex, perl = TRUE)))
    ob == cb
  }
  for (md in c("<u>unclosed", "</u>stray",
               '<span style="color:red">a<u>b</span>c',
               "<u><s>both</s></u>")) {
    expect_true(balanced(.md_to_tex(md)), label = md)
  }
  # An unclosed tag still styles what follows it.
  expect_match(.md_to_tex("<u>unclosed"), "\\underline{", fixed = TRUE)
})

test_that("HTML spans work in the block renderer too", {
  # markdown_box_grob() uses the same inline walker.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  g <- markdown_box_grob(
    'Text with <span style="color:red">red</span> and <u>under</u>.',
    width = grid::unit(3, "in"))
  kids <- grid::makeContent(g)$children
  inner <- kids[[length(kids)]]$children
  cols <- unlist(lapply(inner, function(k) k$layout_df$color))
  expect_true("#FF0000" %in% cols)
})

test_that("block-level HTML is still dropped", {
  # Only *inline* HTML changed; <div> arrives as html_block.
  md <- "Before\n\n<div>raw **html**</div>\n\nAfter"
  tex <- .md_to_tex(md)
  expect_false(grepl("<div", tex, fixed = TRUE))
  expect_match(tex, "Before", fixed = TRUE)
  expect_match(tex, "After", fixed = TRUE)
})
