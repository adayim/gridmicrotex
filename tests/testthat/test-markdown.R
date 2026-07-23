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
