# Math commands that amsmath, mathtools and TeX itself have and the front end
# read as unknown: each is checked by what it draws, against the form it is
# defined by.

rec <- function(tex, mode = "math") {
  latex_tree(tex, input_mode = mode, render_mode = "typeface")$records
}

# The warnings a layout raises, as one string per warning.
warns <- function(tex, mode = "math") {
  w <- character(0)
  withCallingHandlers(
    latex_tree(tex, input_mode = mode),
    warning = function(x) {
      w <<- c(w, conditionMessage(x))
      invokeRestart("muffleWarning")
    })
  w
}

xs <- function(tex) {
  r <- rec(tex)
  r$x[r$type == "glyph"]
}

test_that("\\mkern, \\mskip, \\mspace and \\hskip leave the space they name", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  quad <- xs(r"(a\quad b)")
  for (tex in c(r"(a\mkern18mu b)", r"(a\mskip18mu b)", r"(a\mspace{18mu}b)")) {
    expect_identical(warns(tex), character(0), info = tex)
    expect_equal(xs(tex), quad, tolerance = 1e-4, info = tex)
  }
  expect_equal(xs(r"(a\hskip1em b)"), xs(r"(a\kern1em b)"), tolerance = 1e-4)
  expect_identical(warns(r"(a\hskip1em b)"), character(0))
  # A negative one takes the second letter back over the first.
  expect_lt(xs(r"(a\mkern-6mu b)")[2], xs(r"(a b)")[2])
})

test_that("dcases sets its cells in display style", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  body <- r"(\sum_{i=1}^n x_i & y)"
  h <- function(env) {
    latex_tree(paste0(r"(\begin{)", env, "}", body, r"(\end{)", env, "}"),
               input_mode = "math")$bbox[["height"]]
  }
  expect_identical(warns(paste0(r"(\begin{dcases})", body, r"(\end{dcases})")),
                   character(0))
  # Limits above and below the sum, not at its side.
  expect_gt(h("dcases"), h("cases"))
})

test_that("subarray is a script-style array, as \\substack is", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  sub <- r"(\sum_{\begin{subarray}{c} i<n \\ j>0 \end{subarray}} x)"
  stack <- r"(\sum_{\substack{ i<n \\ j>0 }} x)"
  expect_identical(warns(sub), character(0))
  expect_identical(rec(sub), rec(stack))
  # {l}: the rows start at the same place.
  left <- rec(r"(\begin{subarray}{l} abc \\ d \end{subarray})")
  g <- left[left$type == "glyph", ]
  expect_equal(g$x[1], g$x[4], tolerance = 1e-4)
})

test_that("\\shortintertext is \\intertext in the same slot", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  mk <- function(cmd) {
    paste0(r"(\begin{align} a&=b \)", cmd, r"({so} c&=d \end{align})")
  }
  expect_identical(warns(mk("shortintertext")), character(0))
  expect_identical(rec(mk("shortintertext")), rec(mk("intertext")))
})

test_that("\\cancelto strikes its base with an arrow and sets the value at the tip", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(warns(r"(\cancelto{0}{x})"), character(0))
  r <- rec(r"(\cancelto{0}{x})")
  plain <- rec(r"(\cancel{x})")
  # The shaft and two strokes of the head.
  expect_equal(sum(r$type == "line"), sum(plain$type == "line") + 2L)
  g <- r[r$type == "glyph", ]
  expect_equal(nrow(g), 2L)
  # The value is to the right of the base and above it.
  expect_gt(g$x[2], g$x[1])
  expect_lt(g$y[2], g$y[1])
})

test_that("\\DeclarePairedDelimiter defines the plain, starred and sized forms", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  decl <- r"(\DeclarePairedDelimiter{\abs}{\lvert}{\rvert})"
  expect_identical(warns(paste0(decl, r"(\abs{x})")), character(0))
  expect_equal(rec(paste0(decl, r"(\abs{x})")), rec(r"(\lvert x\rvert)"),
               tolerance = 1e-4)
  expect_equal(rec(paste0(decl, r"(\abs*{\frac{a}{b}})")),
               rec(r"(\left\lvert\frac{a}{b}\right\rvert)"), tolerance = 1e-4)
  expect_equal(rec(paste0(decl, r"(\abs[\big]{x})")),
               rec(r"(\big\lvert x\big\rvert)"), tolerance = 1e-4)
  # An existing command is not replaced.
  expect_match(warns(r"(\DeclarePairedDelimiter{\frac}{(}{)}x)"),
               "already exists", all = FALSE)
})

test_that("\\leftroot and \\uproot are read and leave the root as it is", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  tex <- r"(\sqrt[\leftroot{2}\uproot{2}3]{x})"
  expect_identical(warns(tex), character(0))
  expect_identical(rec(tex), rec(r"(\sqrt[3]{x})"))
})

test_that("\\cancel rises to the right and \\bcancel falls, as the cancel package draws them", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  line <- function(tex) {
    r <- rec(tex)
    r[r$type == "line", ]
  }
  # y runs down the page: the slash starts low and ends high.
  s <- line(r"(\cancel{x})")
  expect_gt(s$y, s$y2)
  expect_lt(s$x, s$x2)
  b <- line(r"(\bcancel{x})")
  expect_lt(b$y, b$y2)
  expect_lt(b$x, b$x2)
})

test_that("a column's >{\\displaystyle} sets its cells in display style", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  h <- function(spec) {
    latex_tree(paste0(r"(\begin{array}{)", spec, r"(} \sum_{i=1}^n x_i \end{array})"),
               input_mode = "math")$bbox[["height"]]
  }
  expect_gt(h(r"(>{\displaystyle}l)"), h("l"))
  expect_equal(h(r"(>{\displaystyle}l)"),
               latex_tree(r"(\begin{array}{l} \displaystyle\sum_{i=1}^n x_i \end{array})",
                          input_mode = "math")$bbox[["height"]])
  # \scriptscriptstyle holds "scriptstyle": it must not be read as it.
  expect_lt(h(r"(>{\scriptscriptstyle}l)"), h(r"(>{\scriptstyle}l)"))
})

test_that("KaTeX's aliases draw what the commands they stand for draw", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  pairs <- list(
    c(r"(\rarr)", r"(\rightarrow)"), c(r"(\lArr)", r"(\Leftarrow)"),
    c(r"(\harr)", r"(\leftrightarrow)"), c(r"(\Uarr)", r"(\Uparrow)"),
    c(r"(\R)", r"(\mathbb{R})"), c(r"(\N)", r"(\mathbb{N})"), c(r"(\Bbb{Z})", r"(\mathbb{Z})"),
    c(r"(\isin)", r"(\in)"), c(r"(\empty)", r"(\emptyset)"), c(r"(\infin)", r"(\infty)"),
    c(r"(\clubs)", r"(\clubsuit)"), c(r"(\alef)", r"(\aleph)"), c(r"(\sdot)", r"(\cdot)"),
    c(r"(\dotsm)", r"(\cdots)"), c(r"(\argmax_x)", r"(\operatorname*{arg\,max}_x)"),
    c(r"(\bigm|)", r"(\mathrel{\big|})"), c(r"(\bra{a})", r"(\left\langle a\right|)"),
    c(r"(\set{a\VERT b})", r"(\left\{ a\mathrel{|} b\right\})"))
  key <- function(tex) {
    r <- rec(tex)
    round(r[order(r$x, r$y), c("x", "y", "glyph")], 3)
  }
  for (p in pairs) {
    expect_identical(warns(p[1]), character(0), info = p[1])
    expect_equal(key(p[1]), key(p[2]), ignore_attr = TRUE, info = p[1])
  }
})

test_that("the text symbols and letters KaTeX has are text", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  said <- function(tex) {
    r <- rec(tex)
    paste(r$text[r$type == "text"], collapse = "")
  }
  expect_identical(said(r"(\text{\textbackslash\textbar\textdollar\textunderscore})"), "\\|$_")
  expect_identical(said(r"(\text{\AE\oe\ss\o})"), "Æœßø")
  expect_identical(said(r"(\text{\textendash\textemdash\textdegree})"), "–—°")
  expect_identical(warns(r"(\text{\textmd{a}\textup{b}})"), character(0))
})

test_that("a document may define a name KaTeX has as its own", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(warns(r"(\newcommand{\R}{\mathbb{R}}\newcommand{\set}[1]{\{#1\}} \R \set{a})"),
                   character(0))
  expect_identical(warns(r"(\renewcommand{\N}{n}\N)"), character(0))
  # Its own definition is the one used.
  own <- rec(r"(\newcommand{\bra}[1]{[#1]} \bra{a})")
  expect_false(any(own$type == "line"))
  # A name LaTeX itself has is still taken.
  expect_match(warns(r"(\newcommand{\frac}{x})"), "already exists", all = FALSE)
})

test_that("a column spec's colon and \\hdashline are dashed rules", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  small <- function(r) {
    l <- r[r$type == "line", ]
    sum(sqrt((l$x2 - l$x)^2 + (l$y2 - l$y)^2) < 8)
  }
  plain <- rec(r"(\begin{array}{cc} a & b \\ c & d \end{array})")
  colon <- rec(r"(\begin{array}{c:c} a & b \\ c & d \end{array})")
  expect_identical(warns(r"(\begin{array}{c:c} a & b \\ c & d \end{array})"), character(0))
  expect_equal(sum(plain$type == "line"), 0L)
  expect_gt(small(colon), 2L)
  # Between the cells, level with them.
  v <- colon[colon$type == "line", ]
  expect_true(all(abs(v$x - v$x2) < 1e-3))
  expect_gt(min(v$x), min(colon$x[colon$type == "glyph"]))
  dashed <- rec(r"(\begin{array}{cc} a & b \\ \hdashline c & d \end{array})")
  solid <- rec(r"(\begin{array}{cc} a & b \\ \hline c & d \end{array})")
  expect_equal(sum(solid$type == "line"), 1L)
  expect_gt(sum(dashed$type == "line"), 3L)
  h <- dashed[dashed$type == "line", ]
  expect_true(all(abs(h$y - h$y2) < 1e-3))
})
