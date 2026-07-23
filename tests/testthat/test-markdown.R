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
