test_that("string hjust/vjust resolve to the expected viewport just values", {
  g <- latex_grob("$x^2$", hjust = "left", vjust = "top")
  expect_equal(g$hjust, 0)
  expect_equal(g$vjust, 1)

  g <- latex_grob("$x^2$", hjust = "centre", vjust = "middle")
  expect_equal(g$hjust, 0.5)
  expect_equal(g$vjust, 0.5)

  g <- latex_grob("$x^2$", hjust = "bbright", vjust = "bottom")
  expect_equal(g$hjust, 1)
  expect_equal(g$vjust, 0)
})

test_that("vjust = 'baseline' aligns at bbox_bl_bp / bbox_h", {
  g <- latex_grob("$x^2$", vjust = "baseline")
  expect_equal(g$vjust, g$bbox_bl_bp / g$bbox_h)
  # baseline should sit below center for an expression with descent (e.g. \sum)
  g2 <- latex_grob(r"($\sum_{i=1}^n i$)", vjust = "baseline")
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
  g <- latex_grob("$x^2$", vjust = "baseline", gp = grid::gpar(fontsize = 12))
  bl1 <- g$vjust
  g2 <- grid::editGrob(g, gp = grid::gpar(fontsize = 24))
  # bbox changes, but vjust should still resolve "baseline" against the new bbox
  expect_equal(g2$vjust, g2$bbox_bl_bp / g2$bbox_h)
})

# --- grobX / grobY boundary points ---

test_that("grobX/grobY cardinal points hit the bbox edges (rot = 0)", {
  grDevices::pdf(NULL, width = 8, height = 6)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()

  g <- latex_grob("$x^2$", x = grid::unit(2, "in"), y = grid::unit(1.5, "in"),
                  hjust = 0, vjust = 0, gp = grid::gpar(fontsize = 20))
  w <- grid::convertWidth(grid::grobWidth(g), "inches", valueOnly = TRUE)
  h <- grid::convertHeight(grid::grobHeight(g), "inches", valueOnly = TRUE)

  bx <- function(th) grid::convertX(grid::grobX(g, th), "inches", valueOnly = TRUE)
  by <- function(th) grid::convertY(grid::grobY(g, th), "inches", valueOnly = TRUE)

  # Boundary point = ray from the bbox centre at angle theta.
  expect_equal(bx(0),   2 + w,     tolerance = 1e-6)   # east: right edge
  expect_equal(by(0),   1.5 + h/2, tolerance = 1e-6)   #       centre y
  expect_equal(bx(90),  2 + w/2,   tolerance = 1e-6)   # north: centre x
  expect_equal(by(90),  1.5 + h,   tolerance = 1e-6)   #        top edge
  expect_equal(bx(180), 2,         tolerance = 1e-6)   # west: left edge
  expect_equal(by(180), 1.5 + h/2, tolerance = 1e-6)
  expect_equal(bx(270), 2 + w/2,   tolerance = 1e-6)   # south: centre x
  expect_equal(by(270), 1.5,       tolerance = 1e-6)   #        bottom edge

  # Character theta as accepted by grobX/grobY
  expect_equal(grid::convertX(grid::grobX(g, "east"), "inches", valueOnly = TRUE),
               2 + w, tolerance = 1e-6)
  expect_equal(grid::convertY(grid::grobY(g, "north"), "inches", valueOnly = TRUE),
               1.5 + h, tolerance = 1e-6)

  # 45-degree ray leaves through the top edge (bbox is wider than tall)
  expect_equal(by(45), 1.5 + h, tolerance = 1e-6)
  expect_equal(bx(45), 2 + w/2 + h/2, tolerance = 1e-6)
})

test_that("grobX/grobY match a rectGrob with identical geometry across theta", {
  grDevices::pdf(NULL, width = 8, height = 6)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()

  dims <- latex_dims("\\frac{a}{b}", gp = grid::gpar(fontsize = 20))
  thetas <- c(0, 30, 45, 90, 135, 180, 225, 270, 315)

  for (just in list(c(0, 0), c(0.5, 0.5), c(1, 0.25))) {
    g <- latex_grob("\\frac{a}{b}",
                    x = grid::unit(0.25, "npc"), y = grid::unit(0.4, "npc"),
                    hjust = just[1], vjust = just[2],
                    gp = grid::gpar(fontsize = 20))
    r <- grid::rectGrob(x = grid::unit(0.25, "npc"), y = grid::unit(0.4, "npc"),
                        width = dims$width, height = dims$height, just = just)
    for (th in thetas) {
      expect_equal(grid::convertX(grid::grobX(g, th), "in", valueOnly = TRUE),
                   grid::convertX(grid::grobX(r, th), "in", valueOnly = TRUE),
                   tolerance = 1e-6)
      expect_equal(grid::convertY(grid::grobY(g, th), "in", valueOnly = TRUE),
                   grid::convertY(grid::grobY(r, th), "in", valueOnly = TRUE),
                   tolerance = 1e-6)
    }
  }
})

test_that("grobX/grobY on a rotated grob match a rectGrob in the same viewport", {
  grDevices::pdf(NULL, width = 8, height = 6)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()

  dims <- latex_dims("$x^2$", gp = grid::gpar(fontsize = 20))

  for (rot in c(37, 90, 180)) {
    g <- latex_grob("$x^2$", x = grid::unit(0.25, "npc"), y = grid::unit(0.4, "npc"),
                    hjust = 0, vjust = 0, rot = rot, gp = grid::gpar(fontsize = 20))
    # theta is measured in the grob's own (rotated) frame.
    r <- grid::rectGrob(vp = grid::viewport(
      x = grid::unit(0.25, "npc"), y = grid::unit(0.4, "npc"),
      width = dims$width, height = dims$height,
      just = c(0, 0), angle = rot
    ))
    for (th in c(0, 45, 90, 180, 270)) {
      expect_equal(grid::convertX(grid::grobX(g, th), "in", valueOnly = TRUE),
                   grid::convertX(grid::grobX(r, th), "in", valueOnly = TRUE),
                   tolerance = 1e-6)
      expect_equal(grid::convertY(grid::grobY(g, th), "in", valueOnly = TRUE),
                   grid::convertY(grid::grobY(r, th), "in", valueOnly = TRUE),
                   tolerance = 1e-6)
    }
  }
})
