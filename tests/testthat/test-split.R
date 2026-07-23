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

# --- justification -----------------------------------------------------
#
# Interword spaces used to be rigid StrutBoxes, so a line had nothing to
# stretch. They are GlueBoxes now, carrying TeX's half-space stretch;
# nothing reads that unless justify = TRUE, so ragged output is
# unchanged (the visual snapshots are the wider guard on that).

just_para <- paste0(
  "\\text{",
  paste(rep("The quick brown fox jumps over the lazy dog.", 4), collapse = " "),
  "}"
)

# Right edge of each rendered line. Text records carry no width, so this
# is the last glyph's origin -- fine for comparing lines with each other,
# which is all these tests do.
line_edges <- function(g) {
  df <- g$layout_df
  ys <- sort(unique(round(df$y, 1)))
  vapply(ys, function(yy) {
    s <- abs(round(df$y, 1) - yy) < 0.01
    max(df$x[s] + ifelse(is.na(df$width[s]), 0, df$width[s]))
  }, numeric(1))
}

test_that("justified text fills the measure exactly", {
  for (w in c(200, 300, 400)) {
    d <- latex_dims(just_para, max_width = w, justify = TRUE)
    expect_equal(as.numeric(d$width), w, tolerance = 1e-6)
  }
})

test_that("justification evens the lines out", {
  g_just <- latex_grob(just_para, max_width = 300, justify = TRUE)
  g_rag  <- latex_grob(just_para, max_width = 300, justify = FALSE)
  # Drop the last line: it is deliberately left ragged.
  e_just <- head(line_edges(g_just), -1)
  e_rag  <- head(line_edges(g_rag), -1)
  expect_gt(length(e_just), 2)
  # Justified lines end at nearly the same place; ragged ones do not.
  expect_lt(diff(range(e_just)), 0.25 * diff(range(e_rag)))
})

test_that("the last line stays ragged", {
  g <- latex_grob(just_para, max_width = 300, justify = TRUE)
  e <- line_edges(g)
  expect_lt(e[length(e)], min(head(e, -1)) - 10)
})

test_that("justification does not change how tall the text is", {
  for (w in c(200, 300)) {
    r <- latex_dims(just_para, max_width = w, justify = FALSE)
    j <- latex_dims(just_para, max_width = w, justify = TRUE)
    expect_equal(as.numeric(j$height), as.numeric(r$height), tolerance = 1e-6)
    expect_equal(j$is_split, r$is_split)
  }
})

test_that("justify does nothing without max_width", {
  # It acts on the lines the wrapper produces, so with nothing to wrap
  # there is nothing to justify.
  a <- latex_dims(just_para, justify = FALSE)
  b <- latex_dims(just_para, justify = TRUE)
  expect_equal(as.numeric(b$width), as.numeric(a$width), tolerance = 1e-6)
})

test_that("the justify flag does not leak into later parses", {
  # It is a global on the splitter, guarded in C++; a leak would justify
  # every subsequent render in the session.
  before <- as.numeric(latex_dims(just_para, max_width = 300)$width)
  invisible(latex_dims(just_para, max_width = 300, justify = TRUE))
  after <- as.numeric(latex_dims(just_para, max_width = 300)$width)
  expect_equal(after, before, tolerance = 1e-6)
  expect_lt(after, 300)  # still ragged
})

test_that("justified and ragged layouts are cached separately", {
  # The cache key must include justify, or the second call returns the
  # first one's layout.
  latex_cache_clear()
  r <- as.numeric(latex_dims(just_para, max_width = 300, justify = FALSE)$width)
  j <- as.numeric(latex_dims(just_para, max_width = 300, justify = TRUE)$width)
  expect_lt(r, j)
  expect_equal(j, 300, tolerance = 1e-6)
})

test_that("justify is validated and settable as an option", {
  expect_error(latex_dims("x", justify = "yes"), "TRUE or FALSE")
  expect_error(latex_dims("x", justify = NA), "TRUE or FALSE")
  expect_error(latex_dims("x", justify = c(TRUE, FALSE)), "TRUE or FALSE")

  latex_options(justify = TRUE)
  expect_true(latex_options()$justify)
  expect_equal(as.numeric(latex_dims(just_para, max_width = 300)$width), 300,
               tolerance = 1e-6)
  reset_latex_options()
  expect_null(latex_options()$justify)
})

test_that("a line of one long word is left alone", {
  # No interword glue to stretch, and nothing should crash or blow up.
  solid <- paste0("\\text{", strrep("x", 80), "}")
  expect_no_error(latex_dims(solid, max_width = 100, justify = TRUE))
})
