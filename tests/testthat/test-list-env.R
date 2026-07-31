# --- itemize / enumerate list environments ---

# latex_tree() keeps the x/y of every record, which is what makes the
# claims below checkable. In path mode the coordinates live inside the
# path itself and every row reads as NA, so "it produced a grob" is all
# you can assert there -- which is why these used to assert only that.
items <- function(tex) latex_tree(tex)$records

test_that("itemize puts each item on its own row, marker then content", {
  for (n in 1:3) {
    r <- items(paste0("\\begin{itemize}",
                      paste(paste0("\\item ", letters[seq_len(n)]),
                            collapse = " "),
                      "\\end{itemize}"))
    expect_equal(length(unique(round(r$y, 2))), n)  # one row per item
    expect_equal(nrow(r), 2L * n)                   # a marker and its letter
  }
  # Markers share a column and every letter starts to the right of it.
  r <- items("\\begin{itemize}\\item a \\item b \\item c\\end{itemize}")
  xs <- sort(unique(round(r$x, 2)))
  expect_length(xs, 2L)
  expect_gt(xs[2], xs[1])
})

test_that("math inside an item is still set as math", {
  # Flattened to text these would come out as the literal characters, so
  # assert the two things only a math setter produces: a radical rule,
  # and a superscript set smaller than its base.
  g <- latex_grob("\\begin{enumerate}\\item x^2 \\item \\sqrt{y}\\end{enumerate}",
                  render_mode = "path")
  expect_true("line" %in% g$layout_df$type)

  r <- items("\\begin{enumerate}\\item x^2\\end{enumerate}")
  expect_length(unique(r$font_size), 2L)
  expect_lt(min(r$font_size), max(r$font_size))
})

test_that("the optional [label] changes the marker", {
  right_edge <- function(lab) {
    max(items(paste0("\\begin{enumerate}", lab,
                     "\\item a \\item b\\end{enumerate}"))$x)
  }
  # "1-" is wider than the default "1.", so it pushes the text right.
  expect_gt(right_edge("[\\arabic*-]"), right_edge(""))

  # Each documented counter still yields two rows with a marker on each.
  for (lab in c("[\\alph*)]", "[\\Alph*.]", "[\\roman*)]", "[\\Roman*.]")) {
    r <- items(paste0("\\begin{enumerate}", lab,
                      "\\item a \\item b\\end{enumerate}"))
    expect_equal(length(unique(round(r$y, 2))), 2L, info = lab)
    expect_gt(nrow(r), 2L, label = lab)  # markers as well as the letters
  }
  # A roman counter reaches "ii", which is two glyphs where "b" is one.
  expect_gt(nrow(items("\\begin{enumerate}[\\roman*)]\\item a \\item b\\end{enumerate}")),
            nrow(items("\\begin{enumerate}[\\alph*)]\\item a \\item b\\end{enumerate}")))

  # An itemize marker is customisable too, and a different marker moves
  # the text.
  star  <- items("\\begin{itemize}[\\star]\\item a \\item b\\end{itemize}")
  plain <- items("\\begin{itemize}\\item a \\item b\\end{itemize}")
  expect_gt(max(star$x), max(plain$x))
})

test_that("a nested list is indented past its parent", {
  r <- items(paste0("\\begin{itemize}\\item a \\item ",
                    "\\begin{enumerate}\\item p \\item q\\end{enumerate}",
                    "\\end{itemize}"))
  left <- tapply(r$x, round(r$y, 2), min)
  expect_length(left, 4L)                       # two outer, two inner
  # The two outer bullets share the left margin; the inner rows start
  # further in. Without the indent a nested list is unreadable.
  expect_equal(sum(left == min(left)), 2L)
  expect_gt(max(left), min(left))
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
