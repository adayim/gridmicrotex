# Text is drawn as runs: with no width set, a whole phrase is one run (so
# the device orders it and kerns across spaces); with a width set, one run
# per word, since the spaces are where lines break. How the engine makes the
# runs and orders right-to-left ones is tested with the engine, in the
# MicroTeX fork (test/test_runs.cpp); these need a real device.


test_that("kerning is applied, because the device sees a whole word", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  gp <- grid::gpar(fontsize = 20, fontfamily = "sans")
  for (s in c("AVATAR", "Wave", "To Vary")) {
    w <- as.numeric(latex_dims(paste0("\\text{", s, "}"),
                               input_mode = "math", gp = gp)$width)
    grid::pushViewport(grid::viewport(gp = gp))
    kerned <- grid::convertWidth(grid::stringWidth(s), "bigpts",
                                 valueOnly = TRUE)
    grid::popViewport()
    # Within a big point: latex_dims() reports whole big points.
    expect_lt(abs(w - kerned), 1)
  }
})

test_that("the output holds the word, so a viewer can find it", {
  skip_if_not_installed("svglite")
  f <- tempfile(fileext = ".svg")
  on.exit(unlink(f), add = TRUE)
  svglite::svglite(f, width = 5, height = 1)
  grid.latex("\\text{Hello world}", input_mode = "math",
             gp = grid::gpar(fontsize = 20))
  dev.off()
  svg <- paste(readLines(f, warn = FALSE), collapse = "\n")
  # The whole phrase in one element, so a viewer can search for it.
  expect_equal(length(gregexpr("<text", svg, fixed = TRUE)[[1]]), 1L)
  expect_true(grepl("Hello world", svg, fixed = TRUE))
})

# The words of each line read from the right, across the lines in order.
right_to_left <- function(tex, mw = 110, fn = latex_grob, ...) {
  d <- fn(tex, max_width = mw, gp = grid::gpar(fontsize = 14), ...)$layout_df
  d <- d[d$type == "text" & !is.na(d$text), ]
  unlist(lapply(sort(unique(round(d$y, 1))), function(yy) {
    s <- d[abs(round(d$y, 1) - yy) < 0.01, ]
    s$text[order(-s$x)]
  }))
}

test_that("right-to-left markdown is ordered through its emphasis", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  a <- c("مرحبا", "بك",
         "في", "العالم")
  expect_equal(
    right_to_left(paste(a[1], a[2], paste0("**", a[3], "**"), a[4]),
                  fn = markdown_grob),
    a)
  expect_equal(
    right_to_left(paste(a[1], paste0("*", a[2], "*"), paste0("`", a[3], "`"), a[4]),
                  fn = markdown_grob),
    a)
})

