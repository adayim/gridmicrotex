# Commutative diagrams: amscd's CD and tikz-cd's tikzcd, drawn as cells in a
# grid with arrows between them. The layout records say where everything
# is: the cells are glyphs, an arrow is line records (its heads, tails and
# dashes included), and a label is a glyph too.

rec <- function(tex, mode = "math") {
  latex_tree(tex, input_mode = mode, gp = grid::gpar(fontsize = 10),
             render_mode = "typeface")$records
}

warns <- function(tex, mode = "math") {
  out <- character(0)
  withCallingHandlers(
    rec(tex, mode),
    warning = function(x) {
      out <<- c(out, conditionMessage(x))
      invokeRestart("muffleWarning")
    })
  out
}

tikz <- function(body, opts = "") {
  paste0("\\begin{tikzcd}", opts, "\n", body, "\n\\end{tikzcd}")
}

len <- function(r) sqrt((r$x2 - r$x)^2 + (r$y2 - r$y)^2)

# The line records long enough to be a shaft, not a head or a hook.
shafts <- function(r, min = 12) {
  l <- r[r$type == "line", ]
  l[len(l) >= min, ]
}

cells <- function(r) r[r$type %in% c("glyph", "text"), ]

# The glyphs in a region, by their baseline origins.
glyphs <- function(r) {
  g <- r[r$type == "glyph", ]
  g[order(round(g$y), g$x), ]
}

test_that("an arrow runs between two cells, clear of both", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- tikz("A \\arrow[r] & B")
  expect_identical(warns(tex), character(0))
  r <- rec(tex)
  g <- glyphs(r)
  expect_equal(nrow(g), 2L)
  s <- shafts(r)
  expect_equal(nrow(s), 1L)
  # Level, to the right, and with space left either side of it.
  expect_equal(s$y, s$y2)
  expect_gt(s$x, g$x[1])
  expect_lt(s$x2, g$x[2])
  expect_gt(s$x - g$x[1], 5)
  expect_gt(g$x[2] - s$x2, 3)
  # At the height of the math axis above the baseline, not the baseline.
  expect_lt(s$y, g$y[1])
  expect_gt(g$y[1] - s$y, 1)
})

test_that("an arrow has a head, which is more than a shaft", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  r <- rec(tikz("A \\arrow[r] & B"))
  l <- r[r$type == "line", ]
  expect_gt(nrow(l), 8L)
  s <- shafts(r)
  # The head points the way the arrow goes: the shaft's end is its tip,
  # the farthest right of anything drawn.
  expect_equal(max(c(l$x, l$x2)), s$x2, tolerance = 1e-3)
  # A reverse arrow has it at the other end.
  r2 <- rec(tikz("A & B \\arrow[l]"))
  l2 <- r2[r2$type == "line", ]
  s2 <- shafts(r2)
  expect_equal(min(c(l2$x, l2$x2)), min(s2$x, s2$x2), tolerance = 1e-3)
})

test_that("arrows in a row stay level when their cells differ in height", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  axis_above <- function(tex) {
    r <- rec(tex)
    g <- r[r$type == "glyph", ]
    shafts(r)$y - g$y[which.min(g$x)]
  }
  plain <- axis_above(tikz("A \\arrow[r] & B"))
  tall <- axis_above(tikz("A \\arrow[r] & B^{2}_{p}"))
  expect_equal(plain, tall, tolerance = 1e-3)
})

test_that("a label sits on the left of the way an arrow goes, or on the right with a prime", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  above <- rec(tikz("A \\arrow[r, \"f\"] & B"))
  below <- rec(tikz("A \\arrow[r, \"f\"'] & B"))
  for (r in list(above, below)) {
    g <- glyphs(r)
    expect_equal(nrow(g), 3L)  # A, B and the label
  }
  fa <- glyphs(above); fb <- glyphs(below)
  sa <- shafts(above); sb <- shafts(below)
  # The label is the glyph between the cells in x, and above or below the shaft.
  lab <- function(g) g[g$x > min(g$x) + 5 & g$x < max(g$x) - 5, ]
  expect_lt(lab(fa)$y, sa$y)
  expect_gt(lab(fb)$y, sb$y)
  # Centred along the arrow.
  expect_equal(lab(fa)$x, (sa$x + sa$x2) / 2, tolerance = 0.1)
  # Going down, left of the way is the right of the page.
  down <- rec(tikz("A \\arrow[d, \"g\"] \\\\ C"))
  gd <- glyphs(down)
  sd <- shafts(down)
  gl <- gd[gd$y > min(gd$y) & gd$y < max(gd$y), ]
  expect_gt(gl$x, sd$x)
  down2 <- rec(tikz("A \\arrow[d, \"g\"'] \\\\ C"))
  gd2 <- glyphs(down2)
  gl2 <- gd2[gd2$y > min(gd2$y) & gd2$y < max(gd2$y), ]
  expect_lt(gl2$x, shafts(down2)$x)
})

test_that("a label option moves it along the arrow", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  where <- function(opt) {
    r <- rec(tikz(paste0("A \\arrow[r, \"f\"", opt, "] & B")))
    g <- glyphs(r)
    s <- shafts(r)
    g[g$x > min(g$x) + 5 & g$x < max(g$x) - 5, ]$x - s$x
  }
  mid <- where("")
  expect_lt(where(" near start"), mid)
  expect_gt(where(" near end"), mid)
  expect_lt(where("{pos=0.1}"), where(" near start"))
  expect_equal(where("{near start, swap}"), where(" near start"), tolerance = 1e-3)
})

test_that("a description label breaks the arrow around it", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  r <- rec(tikz("A \\arrow[r, \"f\"{description}] & B", "[column sep=large]"))
  s <- shafts(r, min = 3)
  s <- s[abs(s$y - s$y2) < 1e-3, ]
  s <- s[order(s$x), ]
  # Two pieces with a gap, and the label in the gap.
  expect_equal(nrow(s), 2L)
  gap <- c(s$x2[1], s$x[2])
  g <- glyphs(r)
  lab <- g[g$x > min(g$x) + 5 & g$x < max(g$x) - 5, ]
  expect_gt(lab$x, gap[1]); expect_lt(lab$x, gap[2])
})

test_that("a dashed arrow is made of short pieces with gaps, a dotted one of dots", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  solid <- shafts(rec(tikz("A \\arrow[r] & B")), min = 0)
  dash <- rec(tikz("A \\arrow[r, dashed] & B"))
  d <- dash[dash$type == "line" & abs(dash$y - dash$y2) < 1e-3, ]
  d <- d[order(d$x), ]
  expect_gt(nrow(d), 4L)
  # Equal pieces, and a gap after each.
  expect_equal(len(d)[2], len(d)[3], tolerance = 1e-3)
  gaps <- d$x[-1] - d$x2[-nrow(d)]
  expect_true(all(gaps[-length(gaps)] > 0.5))
  dot <- rec(tikz("A \\arrow[r, dotted] & B"))
  o <- dot[dot$type == "line" & abs(dot$y - dot$y2) < 1e-3, ]
  expect_gt(nrow(o), nrow(d))
  expect_lt(max(len(o)), min(len(d)))
})

test_that("a diagonal arrow leaves and enters the cells along the line between them", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  r <- rec(tikz("A \\arrow[dr] & B \\\\ C & D"))
  expect_identical(warns(tikz("A \\arrow[dr] & B \\\\ C & D")), character(0))
  g <- glyphs(r)
  s <- shafts(r)
  expect_equal(nrow(s), 1L)
  # Down and to the right, between A and D.
  expect_gt(s$x2, s$x); expect_gt(s$y2, s$y)
  a <- g[1, ]; d <- g[4, ]
  expect_gt(s$x, a$x); expect_lt(s$x2, d$x + 12)
  # On the line from A's anchor to D's: the two slopes agree.
  expect_equal((s$y2 - s$y) / (s$x2 - s$x), (d$y - a$y) / (d$x - a$x), tolerance = 0.1)
})

test_that("Rightarrow and equal are two parallel lines, equal with no head", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  eq <- shafts(rec(tikz("A \\arrow[r, equal] & B")))
  expect_equal(nrow(eq), 2L)
  expect_equal(eq$x[1], eq$x[2]); expect_equal(eq$x2[1], eq$x2[2])
  expect_gt(abs(eq$y[1] - eq$y[2]), 1)
  # No head: nothing but the two lines.
  all_lines <- rec(tikz("A \\arrow[r, equal] & B"))
  expect_equal(sum(all_lines$type == "line"), 2L)
  imp <- rec(tikz("A \\arrow[r, Rightarrow] & B"))
  expect_gt(sum(imp$type == "line"), 10L)
})

test_that("hook, tail and mapsto add to the start of an arrow", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  base <- function(opt) {
    r <- rec(tikz(paste0("A \\arrow[r", opt, "] & B")))
    l <- r[r$type == "line", ]
    list(n = nrow(l), left = min(c(l$x, l$x2)), shaft = shafts(r)$x)
  }
  plain <- base("")
  for (opt in c(", hook", ", hook'", ", tail", ", mapsto")) {
    b <- base(opt)
    expect_gt(b$n, plain$n)
  }
  # A hook reaches back past the shaft's start; a bar is across it.
  expect_lt(base(", hook")$left, plain$shaft)
  m <- rec(tikz("A \\arrow[r, mapsto] & B"))
  vert <- m[m$type == "line" & abs(m$x - m$x2) < 1e-3, ]
  expect_equal(nrow(vert), 1L)
  expect_equal(vert$x, plain$shaft, tolerance = 1e-3)
  # And two heads put a second head behind the first.
  expect_gt(base(", two heads")$n, plain$n)
})

test_that("bend and shift change the path of an arrow", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  straight <- rec(tikz("A \\arrow[r] & B"))
  bent <- rec(tikz("A \\arrow[r, bend left=40] & B"))
  right <- rec(tikz("A \\arrow[r, bend right=40] & B"))
  # A bend left goes above the level of the straight arrow, a bend right below.
  y0 <- shafts(straight)$y
  yl <- bent[bent$type == "line", ]; yr <- right[right$type == "line", ]
  expect_lt(min(c(yl$y, yl$y2)), y0 - 3)
  expect_gt(max(c(yr$y, yr$y2)), y0 + 3)
  # Shifted left, an arrow is above where it was; right, below.
  up <- shafts(rec(tikz("A \\arrow[r, shift left] & B")))
  down <- shafts(rec(tikz("A \\arrow[r, shift right] & B")))
  expect_lt(up$y, y0); expect_gt(down$y, y0)
  expect_equal(y0 - up$y, down$y - y0, tolerance = 1e-3)
})

test_that("separations set the distance between cells", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  d <- function(opts) {
    r <- rec(tikz("A \\arrow[r] \\arrow[d] & B \\\\ C & D", opts))
    g <- glyphs(r)
    c(col = g$x[2] - g$x[1], row = g$y[3] - g$y[1])
  }
  normal <- d("")
  expect_gt(d("[column sep=large]")["col"], normal["col"])
  expect_equal(d("[column sep=large]")["row"], normal["row"])
  expect_gt(d("[row sep=huge]")["row"], normal["row"])
  expect_lt(d("[row sep=small, column sep=tiny]")["col"], normal["col"])
  # sep sets both, and a size can be a length.
  expect_equal(unname(d("[sep=3em]")["col"] - d("[sep=2em]")["col"]), 10, tolerance = 1e-3)
  expect_equal(unname(d("[sep=3em]")["row"] - d("[sep=2em]")["row"]), 10, tolerance = 1e-3)
})

test_that("what tikz-cd does that is not supported warns once and the arrow is still drawn", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- tikz("A \\arrow[r, wibble, wobble] & B \\arrow[r, wibble] & C")
  w <- warns(tex)
  expect_equal(sum(grepl("wibble", w)), 1L)
  expect_equal(sum(grepl("wobble", w)), 1L)
  expect_equal(nrow(shafts(suppressWarnings(rec(tex)))), 2L)
  expect_match(warns(tikz("A \\arrow[r] & B", "[sep=wide]")), "sep=wide", all = FALSE)
})

test_that("an arrow that goes nowhere or out of the diagram warns and is not drawn", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  out <- tikz("A \\arrow[r] & B \\arrow[r]")
  expect_match(warns(out), "outside the diagram", all = FALSE)
  expect_equal(nrow(shafts(suppressWarnings(rec(out)))), 1L)
  expect_match(warns(tikz("A \\arrow[] & B")), "no direction", all = FALSE)
})

test_that("the older syntax and the shortcuts draw the same arrows", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  key <- function(tex) {
    r <- rec(tex)
    l <- r[r$type == "line", c("x", "y", "x2", "y2")]
    round(l[order(l$x, l$y, l$x2), ], 3)
  }
  expect_identical(key(tikz("A \\rar & B \\dar \\\\ C & D")),
                   key(tikz("A \\arrow[r] & B \\arrow[d] \\\\ C & D")))
  expect_identical(key(tikz("A \\arrow{r}{f} & B")),
                   key(tikz("A \\arrow[r, \"f\"] & B")))
  expect_identical(key(tikz("A \\ar[rr] & B & C")),
                   key(tikz("A \\arrow[rr] & B & C")))
})

test_that("a diagram can be set in a display, in a document, and on a cell with math in it", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- "Here is a square:\n\\[ \\begin{tikzcd} A \\arrow[r, \"\\alpha\"] & B^2 \\end{tikzcd} \\]\nand more."
  expect_identical(warns(tex, "document"), character(0))
  r <- rec(tex, "document")
  expect_gt(sum(r$type == "line"), 8L)
  # Problems inside a cell are the diagram's own.
  expect_match(warns(tikz("\\wibble \\arrow[r] & B")), "wibble", all = FALSE)
})

test_that("a reversed arrow has its head at the start, a both-way one at both", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # The short pieces at each end of the shaft: the strokes of its heads.
  heads <- function(opt) {
    r <- rec(tikz(paste0("A \\arrow[r, ", opt, "] & B")))
    l <- r[r$type == "line", ]
    s <- shafts(r)
    mid <- (min(s$x, s$x2) + max(s$x, s$x2)) / 2
    small <- l[len(l) < 4, ]
    c(left = sum(pmax(small$x, small$x2) < mid), right = sum(pmin(small$x, small$x2) > mid))
  }
  expect_gt(heads("leftarrow")["left"], 0)
  expect_equal(unname(heads("leftarrow")["right"]), 0)
  expect_gt(heads("leftrightarrow")["left"], 0)
  expect_gt(heads("leftrightarrow")["right"], 0)
  expect_gt(heads("Leftarrow")["left"], 0)
  expect_equal(unname(heads("Leftarrow")["right"]), 0)
  expect_gt(heads("Leftrightarrow")["right"], 0)
})

test_that("shorten takes a length off the start or the end of an arrow", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  full <- len(shafts(rec(tikz("A \\arrow[r] & B"))))
  short <- len(shafts(rec(tikz("A \\arrow[r, shorten <=0.5em, shorten >=0.2em] & B"))))
  expect_equal(full - short, 7, tolerance = 1e-3)
  both <- len(shafts(rec(tikz("A \\arrow[r, shorten=0.5em] & B"))))
  expect_equal(full - both, 10, tolerance = 1e-3)
  expect_match(warns(tikz("A \\arrow[r, shorten <=wide] & B")), "not a size", all = FALSE)
})

cd <- function(body) paste0("\\begin{CD}\n", body, "\n\\end{CD}")

test_that("CD sets objects in rows, with arrows of at least 2.5pc between them", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- cd("A @>>> B @>>> C")
  expect_identical(warns(tex), character(0))
  r <- rec(tex)
  g <- glyphs(r)
  expect_equal(nrow(g), 3L)
  s <- shafts(r)
  expect_equal(nrow(s), 2L)
  # 2.5pc is 30 points, 29.9 big points, at a ten-point size.
  expect_gte(min(len(s)), 29.5)
  expect_equal(len(s)[1], len(s)[2], tolerance = 1e-3)
})

test_that("a CD arrow grows to its label", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  short <- len(shafts(rec(cd("A @>f>> B"))))
  long <- len(shafts(rec(cd("A @>{\\text{a rather long label}}>> B"))))
  expect_gt(long, short)
})

test_that("CD labels are above and below, left and right, as the @ says", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  r <- rec(cd("A @>a>b> B @<c<d< C \\\\ @VeVfV @. @AgAhA \\\\ D @= E @>>> F"))
  g <- glyphs(r)
  s <- shafts(r)
  horizontal <- s[abs(s$y - s$y2) < 1e-3, ]
  horizontal$lo <- pmin(horizontal$x, horizontal$x2)
  horizontal$hi <- pmax(horizontal$x, horizontal$x2)
  top <- horizontal[horizontal$y == min(horizontal$y), ]
  top <- top[order(top$lo), ]
  expect_equal(nrow(top), 2L)
  between <- function(row, above) {
    g[(g$y < row$y) == above & g$x > row$lo & g$x < row$hi & abs(g$y - row$y) < 20, ]
  }
  # Right arrow: a above, b below. Left arrow: c above and d below as well,
  # which is the right of the way it goes.
  expect_equal(nrow(between(top[1, ], TRUE)), 1L)
  expect_equal(nrow(between(top[1, ], FALSE)), 1L)
  expect_equal(nrow(between(top[2, ], TRUE)), 1L)
  expect_equal(nrow(between(top[2, ], FALSE)), 1L)
  # Down, in the first column: e on its left, f on its right; up, in the
  # third: g on its left, h on its right.
  vertical <- s[abs(s$x - s$x2) < 1e-3 & s$y != s$y2, ]
  vertical <- vertical[order(vertical$x), ]
  expect_equal(nrow(vertical), 2L)
  beside <- function(v, left) {
    mid <- (v$y + v$y2) / 2
    g[(g$x < v$x) == left & abs(g$y - mid) < 12 & abs(g$x - v$x) < 15, ]
  }
  expect_equal(nrow(beside(vertical[1, ], TRUE)), 1L)
  expect_equal(nrow(beside(vertical[1, ], FALSE)), 1L)
  expect_equal(nrow(beside(vertical[2, ], TRUE)), 1L)
  expect_equal(nrow(beside(vertical[2, ], FALSE)), 1L)
})

test_that("CD = and | are two plain lines, and @. leaves a place empty", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  r <- rec(cd("A @= B \\\\ @| @. \\\\ C"))
  l <- r[r$type == "line", ]
  # Two horizontal and two vertical lines, nothing else: no heads.
  expect_equal(nrow(l), 4L)
  expect_equal(sum(abs(l$y - l$y2) < 1e-3), 2L)
  expect_equal(sum(abs(l$x - l$x2) < 1e-3), 2L)
})

test_that("a CD with something it cannot read warns and goes on", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_match(warns(cd("A @X B @>>> C")), "@X", all = FALSE)
  expect_match(warns(cd("@VVV \\\\ A")), "above", all = FALSE)
  expect_equal(nrow(shafts(suppressWarnings(rec(cd("A @X B @>>> C"))))), 1L)
})
