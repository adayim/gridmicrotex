# Line breaking in the vendored MicroTeX splitter.
#
# BoxSplitter::split() used to accept only an HBox and return anything
# else untouched, so the moment a formula carried an explicit line break
# its top box became a VBox and max_width was silently ignored -- the
# content just overflowed. These tests pin the fix.

long_text <- paste0("\\text{", paste(rep("longword", 12), collapse = " "), "}")
short_text <- "\\text{short}"

dims <- function(tex, max_width = 0) {
  d <- latex_dims(tex, max_width = max_width)
  list(w = as.numeric(d$width), h = as.numeric(d$height), split = d$is_split)
}

test_that("content separated by \\\\ honours max_width", {
  # Each of these previously came back at full natural width with
  # is_split = FALSE no matter how narrow max_width was.
  for (tex in c(paste0(short_text, "\\\\", long_text),
                paste0(long_text, "\\\\", long_text))) {
    wide <- dims(tex)
    narrow <- dims(tex, 200)
    expect_true(narrow$split)
    expect_lt(narrow$w, wide$w)
    expect_lte(narrow$w, 200)
    # Wrapping trades width for height.
    expect_gt(narrow$h, wide$h)
  }
})

test_that("a single flowing paragraph still wraps as it always did", {
  wide <- dims(long_text)
  narrow <- dims(long_text, 200)
  expect_true(narrow$split)
  expect_lte(narrow$w, 200)
})

test_that("content that already fits is returned untouched", {
  # The splitter must not rebuild a box it does not need to change:
  # identical width and height at any max_width.
  for (tex in c(short_text,
                "\\frac{a}{b}",
                "\\begin{matrix}a&b\\\\c&d\\end{matrix}",
                "\\begin{cases}a&b\\\\c&d\\end{cases}",
                "\\begin{pmatrix}1\\\\2\\end{pmatrix}")) {
    a <- dims(tex)
    b <- dims(tex, 200)
    expect_equal(b$w, a$w, tolerance = 1e-6)
    expect_equal(b$h, a$h, tolerance = 1e-6)
    expect_false(b$split)
  }
})

test_that("vertical centring on the math axis survives a rebuilt VBox", {
  # MatrixAtom overwrites the VBox height/depth to centre it on the math
  # axis. Rebuilding the box without restoring that offset shifts the
  # baseline of every multi-row formula, which no width assertion would
  # catch.
  for (tex in c("\\begin{matrix}a&b\\\\c&d\\end{matrix}",
                "\\begin{cases}x\\\\y\\end{cases}",
                "\\begin{pmatrix}1\\\\2\\end{pmatrix}")) {
    plain <- latex_dims(tex)
    limited <- latex_dims(tex, max_width = 200)
    expect_equal(limited$baseline, plain$baseline, tolerance = 1e-6)
    expect_equal(as.numeric(limited$depth), as.numeric(plain$depth),
                 tolerance = 1e-6)
  }
})

test_that("splitting a wrapped paragraph keeps the text on the page", {
  # Guards the rebuild: every drawn record must sit inside the reported
  # bounding box. A botched height/depth fix-up shows up here as content
  # drawn outside its own box.
  g <- latex_grob(paste0(short_text, "\\\\", long_text), max_width = 200)
  expect_true(g$is_split)
  expect_lte(max(g$layout_df$x, na.rm = TRUE), g$bbox_w + 1)
  expect_gte(min(g$layout_df$x, na.rm = TRUE), -1)
})

test_that("list and matrix cells do not wrap (documented limitation)", {
  # Long items inside itemize/enumerate/align live in WrapperBoxes whose
  # metrics are the row's, precomputed by MatrixAtom. The splitter
  # deliberately does not reach into them. If this ever starts passing,
  # the cell-splitting work landed and R/markdown.R can stop stacking
  # list items itself.
  for (tex in c(paste0("\\begin{itemize}\\item ", long_text, "\\end{itemize}"),
                paste0("\\begin{enumerate}\\item ", long_text, "\\end{enumerate}"))) {
    narrow <- dims(tex, 200)
    expect_false(narrow$split)
  }
})
