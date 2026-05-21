# --- itemize / enumerate list environments ---

test_that("itemize renders one row per item", {
  g3 <- latex_grob("\\begin{itemize}\\item a \\item b \\item c\\end{itemize}",
                    render_mode = "path")
  g1 <- latex_grob("\\begin{itemize}\\item a\\end{itemize}", render_mode = "path")
  expect_s3_class(g3, "latexgrob")
  expect_true(nrow(g3$layout_df) > 0)
  # three bulleted rows are taller than one
  expect_true(g3$bbox_h > g1$bbox_h)
})

test_that("enumerate renders and item math is preserved", {
  g <- latex_grob("\\begin{enumerate}\\item x^2 \\item \\sqrt{y}\\end{enumerate}",
                   render_mode = "path")
  expect_s3_class(g, "latexgrob")
  expect_true(g$bbox_w > 0 && g$bbox_h > 0)
})

test_that("optional [label] customises the marker", {
  # custom enumerate counter formats parse without error
  for (lab in c("[\\alph*)]", "[\\Alph*.]", "[\\roman*)]", "[\\Roman*.]",
                "[\\arabic*-]")) {
    g <- latex_grob(
      paste0("\\begin{enumerate}", lab, "\\item a \\item b\\end{enumerate}"),
      render_mode = "path"
    )
    expect_true(g$bbox_h > 0, info = lab)
  }
  # custom itemize marker
  g <- latex_grob("\\begin{itemize}[\\star]\\item a \\item b\\end{itemize}",
                   render_mode = "path")
  expect_true(g$bbox_h > 0)
})

test_that("lists nest", {
  g <- latex_grob(
    paste0("\\begin{itemize}\\item a \\item ",
           "\\begin{enumerate}\\item p \\item q\\end{enumerate}\\end{itemize}"),
    render_mode = "path"
  )
  expect_s3_class(g, "latexgrob")
  expect_true(g$bbox_h > 0)
})

test_that("empty list does not error", {
  g <- latex_grob("\\begin{itemize}\\end{itemize}", render_mode = "path")
  expect_s3_class(g, "latexgrob")
  expect_equal(nrow(g$layout_df), 0)
})

test_that("lists survive default (mixed) input mode", {
  # latex_wrap must treat itemize/enumerate as math envs, otherwise the
  # body is wrapped in \text{} and split around nested environments.
  wrapped <- latex_wrap("\\begin{itemize}\\item a\\end{itemize}")
  expect_false(any(grepl("\\\\text\\{", wrapped)))
  src <- paste0("\\begin{enumerate}\\item x^2 \\item \\text{T:}\\ ",
                "\\begin{array}{|c|c|}a&b\\\\c&d\\end{array}\\end{enumerate}")
  g_mixed <- latex_grob(src, render_mode = "path")
  g_math <- latex_grob(src, render_mode = "path", input_mode = "math")
  # mixed mode must not mangle the body: identical layout to math mode
  expect_equal(g_mixed$bbox_h, g_math$bbox_h)
  expect_equal(nrow(g_mixed$layout_df), nrow(g_math$layout_df))
})
