# Equation numbers, \tag, \label and \ref, as LaTeX's amsmath sets them, are
# tested with the engine, in the MicroTeX fork (test/test_document.cpp). A
# markdown box parses its blocks one at a time and carries the count and the
# labels from one to the next; that is R's, and is tested here.

# The text of each block of a box, in order: one string per block.
box_texts <- function(md, width = 300) {
  g <- markdown_box_grob(md, width = grid::unit(width, "bigpts"))
  lay <- gridmicrotex:::.md_box_layout(g)
  unlist(lapply(lay$items, function(it) {
    df <- it$grob$layout_df
    if (is.null(df)) return(NULL)
    df <- df[df$type == "text", ]
    if (!nrow(df)) return(NULL)
    paste(df$text[order(round(df$y), df$x)], collapse = "")
  }))
}

test_that("a markdown box numbers its displays as one document's", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  md <- paste(r"($$\begin{equation} a \end{equation}$$)",
              r"($$\begin{align} b \\ c \end{align}$$)", sep = "\n\n")
  expect_identical(box_texts(md), c("(1)", "(2)(3)"))
  # The same block on its own starts at 1 again: a layout cached for one
  # starting number must not answer for another.
  expect_identical(box_texts(r"($$\begin{equation} b \end{equation}$$)"), "(1)")
  expect_identical(box_texts(md), c("(1)", "(2)(3)"))
  # Starred, a display has none to count.
  md <- paste(r"($$\begin{equation*} a \end{equation*}$$)",
              r"($$\begin{equation} b \end{equation}$$)", sep = "\n\n")
  expect_identical(box_texts(md), "(1)")
})

test_that("a reference in a markdown box names a label in another block", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  md <- paste(r"(See $\eqref{e}$ and $\ref{e}$.)",
              r"($$\begin{equation}\label{x} a \end{equation}$$)",
              r"($$\begin{equation}\label{e} b \end{equation}$$)", sep = "\n\n")
  tx <- box_texts(md)
  expect_false(any(grepl("??", tx, fixed = TRUE)))
  expect_match(tx[1], "(2)", fixed = TRUE)
  expect_match(tx[1], "2.", fixed = TRUE)
  expect_identical(tx[-1], c("(1)", "(2)"))
  # And one that is not there is ??.
  expect_match(suppressWarnings(box_texts(r"(See $\ref{nope}$.)")), "??", fixed = TRUE)
})
