test_that("string hjust/vjust resolve to the expected viewport just values", {
  g <- latex_grob("x^2", hjust = "left", vjust = "top")
  expect_equal(g$hjust, 0)
  expect_equal(g$vjust, 1)

  g <- latex_grob("x^2", hjust = "centre", vjust = "middle")
  expect_equal(g$hjust, 0.5)
  expect_equal(g$vjust, 0.5)

  g <- latex_grob("x^2", hjust = "bbright", vjust = "bottom")
  expect_equal(g$hjust, 1)
  expect_equal(g$vjust, 0)
})

test_that("vjust = 'baseline' aligns at bbox_bl_bp / bbox_h", {
  g <- latex_grob("x^2", vjust = "baseline")
  expect_equal(g$vjust, g$bbox_bl_bp / g$bbox_h)
  # baseline should sit below center for an expression with descent (e.g. \sum)
  g2 <- latex_grob(r"(\sum_{i=1}^n i)", vjust = "baseline")
  expect_gt(g2$vjust, 0)
  expect_lt(g2$vjust, 1)
})

test_that("invalid string just values error clearly", {
  expect_error(latex_grob("x", hjust = "wonky"), "hjust must be")
  expect_error(latex_grob("x", vjust = "wonky"), "vjust must be")
})

test_that("\\mark{name} records a queryable anchor", {
  g <- latex_grob(r"(a + \mark{eq} b = c)", x = grid::unit(0.5, "npc"),
                  y = grid::unit(0.5, "npc"))
  expect_s3_class(g$marks, "data.frame")
  expect_equal(nrow(g$marks), 1L)
  expect_equal(g$marks$name, "eq")
  mk <- grobMark(g, "eq")
  expect_true(grid::is.unit(mk$x))
  expect_true(grid::is.unit(mk$y))
})

test_that("grobMark errors when the grob has no marks", {
  g <- latex_grob("x + y")
  expect_error(grobMark(g, "eq"), "no marks")
})

test_that("grobMark errors with a clear message when the name is unknown", {
  g <- latex_grob(r"(\mark{foo} x)")
  expect_error(grobMark(g, "bar"), "not found.*foo")
})

test_that("multiple marks are stored in source order", {
  g <- latex_grob(r"(\mark{a} x + \mark{b} y)")
  expect_equal(nrow(g$marks), 2L)
  expect_equal(g$marks$name, c("a", "b"))
  expect_lt(g$marks$x[1], g$marks$x[2])
})

test_that("editGrob() preserves string-valued vjust across reparse", {
  g <- latex_grob("x^2", vjust = "baseline", gp = grid::gpar(fontsize = 12))
  bl1 <- g$vjust
  g2 <- grid::editGrob(g, gp = grid::gpar(fontsize = 24))
  # bbox changes, but vjust should still resolve "baseline" against the new bbox
  expect_equal(g2$vjust, g2$bbox_bl_bp / g2$bbox_h)
})

# Regression: \def with backslash control sequences in the body used to fail
# on subsequent parses because MicroTeX's static macro table leaked the name
# into the next parse's preprocess pass, which then mis-tokenised the body.
test_that("plain-TeX \\def with parameterised body parses correctly", {
  expect_s3_class(
    latex_grob(r"(\def\norm#1{\left\lVert #1 \right\rVert} \norm{v})",
               input_mode = "math"),
    "latexgrob"
  )
  expect_s3_class(
    latex_grob(r"(\def\inner#1#2{\langle #1, #2 \rangle} \inner{u}{v})",
               input_mode = "math"),
    "latexgrob"
  )
  # The typeface path triggers a second internal parse for the path-mode
  # fallback layout — exercise it explicitly.
  expect_s3_class(
    latex_grob(r"(\def\frob#1{|#1|} \frob{M})", render_mode = "typeface"),
    "latexgrob"
  )
})

test_that("\\newcommand survives the typeface mode double-parse", {
  # The typeface mode parses twice (once for glyph layout, once for path
  # fallback). Before clearUserMacros() landed in parse_latex_cpp, the
  # second internal parse hit "Command already exists!" because
  # MicroTeX's static _codes map persisted the registration from the
  # first parse.
  expect_s3_class(
    latex_grob(r"(\newcommand{\xyznorm}[1]{\lVert #1 \rVert} \xyznorm{v})",
               render_mode = "typeface"),
    "latexgrob"
  )
})

test_that("\\textcolor body inherits the surrounding math/text mode", {
  # Regression: \textcolor used to force its body into text mode, so
  # `\textcolor{red}{c^2}` rendered "c^2" literally with a caret glyph
  # instead of a superscript. Now the body inherits math mode and the
  # ^ produces a real superscript record.
  g <- latex_grob(r"($\textcolor{blue}{c^2}$)", input_mode = "math")
  # The literal '^' should not appear as a TEXT record; if it did, the
  # body was parsed in text mode.
  layout <- g$layout_df
  has_caret_glyph <- any(layout$type == "text" &
                         vapply(layout$text, function(s) {
                           !is.na(s) && grepl("\\^", s)
                         }, logical(1)))
  expect_false(has_caret_glyph,
               info = "Caret '^' rendered as text — \\textcolor body lost math mode")
})

test_that("\\color declaration colours the rest of the enclosing group", {
  # Regression: \color outside array mode was a no-op, so
  # `\color{blue} E = mc^2` rendered black. Now it consumes the rest of
  # the enclosing group and wraps it in a ColorAtom.
  g <- latex_grob(r"($\color{blue} E = mc^2$)", input_mode = "math")
  cols <- unique(g$layout_df$color[g$layout_df$type %in% c("glyph", "path")])
  cols <- cols[!is.na(cols)]
  # At least one ink record must be blue-ish (#0000FF or its alpha variant).
  expect_true(any(grepl("^#0000FF", cols, ignore.case = TRUE)),
              info = paste("Expected blue ink, got:", paste(cols, collapse = ", ")))
})

test_that("\\newcommand and \\def do not leak between independent latex_grob calls", {
  expect_s3_class(
    latex_grob(r"(\newcommand{\xyzleak}{Q} \xyzleak)", input_mode = "math"),
    "latexgrob"
  )
  # A second call with the same definition must succeed — would error
  # "already exists" if state leaked between parses.
  expect_s3_class(
    latex_grob(r"(\newcommand{\xyzleak}{Q} \xyzleak)", input_mode = "math"),
    "latexgrob"
  )
  # And built-in environments must still be available after the leak guard
  # runs (it cleared _codes wholesale in an earlier iteration of the fix).
  expect_s3_class(
    latex_grob(r"(\begin{pmatrix} 1 & 2 \\ 3 & 4 \end{pmatrix})",
               input_mode = "math"),
    "latexgrob"
  )
})
