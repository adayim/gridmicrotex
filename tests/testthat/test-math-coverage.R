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

# --- What KaTeX has that LaTeX's own commands did not give us ----------------

# Two inputs draw the same: the same records, down to the positions.
same <- function(a, b, info = a) {
  cols <- c("type", "x", "y", "x2", "y2", "font_size", "width", "height")
  expect_equal(rec(a)[, cols], rec(b)[, cols], tolerance = 1e-4, info = info)
}

test_that("\\mathchoice draws the choice for the style it is set in", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  same(r"(\displaystyle\mathchoice{a}{b}{c}{d})", r"(\displaystyle a)")
  same(r"(\textstyle\mathchoice{a}{b}{c}{d})", r"(\textstyle b)")
  same(r"(x^{\mathchoice{a}{b}{c}{d}})", r"(x^{c})")
  same(r"(x^{y^{\mathchoice{a}{b}{c}{d}}})", r"(x^{y^{d}})")
  expect_identical(warns(r"(\mathchoice{a}{b}{c}{d})"), character(0))
})

test_that("\\vcenter puts the middle of its box on the math axis", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  middle <- function(tex) {
    r <- rec(tex)
    bar <- r[r$type == "fill_rect", ][1, ]
    bar$y + bar$height / 2
  }
  # A fraction's bar is on the axis.
  expect_equal(middle(r"(\vcenter{\rule{1pt}{2em}})"), middle(r"(\frac{a}{b})"), tolerance = 1e-3)
  expect_identical(warns(r"(\vcenter{x})"), character(0))
})

test_that("\\expandafter, \\noexpand, \\edef, \\xdef and \\futurelet work as TeX's do", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  same(r"(\def\f#1{[#1]}\def\g{abc}\expandafter\f\g)", "[a]bc")
  same(r"(\def\a{1}\edef\b{\a 2}\def\a{9}\b)", "12")
  same(r"(\def\a{1}\edef\b{\noexpand\a 2}\def\a{9}\b)", "92")
  same(r"({\def\a{1}\xdef\b{\a 2}}\b)", "12")
  same(r"(\futurelet\q\mathrm x\q)", r"(\mathrm{x}x)")
  for (tex in c(r"(\expandafter\def\csname)", r"(\def\a{1}\edef\b{\a 2}\b)")) {
    expect_false(any(grepl("expandafter|edef", warns(tex))), info = tex)
  }
})

test_that("the arrows with no stretching glyph are drawn as long as their text", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  span <- function(tex) {
    l <- rec(tex)
    l <- l[l$type == "line", ]
    max(c(l$x, l$x2)) - min(c(l$x, l$x2))
  }
  for (name in c("xtwoheadrightarrow", "xtwoheadleftarrow", "xlongequal", "xtofrom")) {
    short <- paste0("\\", name, "{a}")
    long <- paste0("\\", name, "{abcdefghijkl}")
    expect_identical(warns(short), character(0), info = name)
    expect_gt(span(long), span(short) + 20)
  }
  # Two heads, and two lines for the equal sign.
  heads <- function(tex) sum(rec(tex)$type == "line")
  expect_gt(heads(r"(\xtwoheadrightarrow{abc})"), heads(r"(\xrightarrow{abc})"))
  expect_gte(heads(r"(\xlongequal{abc})"), 2L)
})

test_that("the enclosures KaTeX has are drawn around their text", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  lines <- function(tex) {
    r <- rec(tex)
    r[r$type == "line", ]
  }
  glyph_y <- function(tex) {
    r <- rec(tex)
    r$y[r$type == "glyph"]
  }
  # A segment: a bar over (under) with a tick at each end.
  over <- lines(r"(\overlinesegment{AB})")
  expect_equal(nrow(over), 3L)
  expect_lt(min(c(over$y, over$y2)), min(glyph_y(r"(AB)")) - 5)
  under <- lines(r"(\underlinesegment{AB})")
  expect_equal(nrow(under), 3L)
  expect_gt(max(c(under$y, under$y2)), max(glyph_y(r"(AB)")))
  # \angl is a bar over and one at the right; \angln is \angl n.
  expect_equal(nrow(lines(r"(\angl{n})")), 2L)
  same(r"(\angln)", r"({\angl n})")
  # \phase: a slanting stroke and the line under.
  phase <- lines(r"(\phase{30^\circ})")
  expect_equal(nrow(phase), 2L)
  expect_true(any(abs(phase$x - phase$x2) > 1 & abs(phase$y - phase$y2) > 1))
  # A circle round the letter: its strokes close on themselves.
  circle <- lines(r"(\textcircled{a})")
  expect_gt(nrow(circle), 20L)
  expect_equal(max(c(circle$x, circle$x2)) - min(c(circle$x, circle$x2)),
               max(c(circle$y, circle$y2)) - min(c(circle$y, circle$y2)), tolerance = 0.2)
  for (tex in c(r"(\text{\textcircled{a}})", r"(\text{\H{o}})", r"(\widecheck{ac})",
                r"(\overgroup{AB})", r"(\undergroup{AB})")) {
    expect_identical(warns(tex), character(0), info = tex)
  }
})

test_that("\\ce reads formulas, charges, coefficients, states, bonds and arrows", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  same(r"(\ce{H2O})", r"(\mathrm{H}_{2}\mathrm{O})")
  same(r"(\ce{SO4^2-})", r"(\mathrm{S}\mathrm{O}_{4}^{2-})")
  same(r"(\ce{Fe^{3+}})", r"(\mathrm{Fe}^{3+})")
  same(r"(\ce{Na+})", r"(\mathrm{Na}^{+})")
  same(r"(\ce{2H2 + O2 -> 2H2O})",
       r"(2\,\mathrm{H}_{2}{}+{}\mathrm{O}_{2}\longrightarrow2\,\mathrm{H}_{2}\mathrm{O})")
  same(r"(\ce{A ->[H2O] B})", r"(\mathrm{A}\xrightarrow{\mathrm{H}_{2}\mathrm{O}}\mathrm{B})")
  same(r"(\ce{A <=>[a][b] B})", r"(\mathrm{A}\xrightleftharpoons[\mathrm{b}]{\mathrm{a}}\mathrm{B})")
  same(r"(\ce{H2O(l)})", r"(\mathrm{H}_{2}\mathrm{O}\mathrm{(l)})")
  same(r"(\ce{1/2 H2O})", r"(\frac{1}{2}\,\mathrm{H}_{2}\mathrm{O})")
  same(r"(\ce{C6H5-CHO})", r"(\mathrm{C}_{6}\mathrm{H}_{5}{-}\mathrm{C}\mathrm{H}\mathrm{O})")
  same(r"(\ce{^{227}_{90}Th})", r"({}^{227}_{90}\mathrm{Th})")
  same(r"(\ce{BaSO4 v})", r"(\mathrm{Ba}\mathrm{S}\mathrm{O}_{4}\downarrow)")
  expect_identical(warns(r"(\ce{CO2 + C -> 2 CO})"), character(0))
})

test_that("\\pu sets a number and a unit", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  same(r"(\pu{1.5 mol/L})", r"(1.5\,\mathrm{mol}{/}\mathrm{L})")
  same(r"(\pu{12 mol L-1})", r"(12\,\mathrm{mol}\,\mathrm{L}^{-1})")
  expect_identical(warns(r"(\pu{123 kJ/mol})"), character(0))
})

test_that("\\arraystretch spaces the rows of the array it is set before", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  pitch <- function(tex) {
    r <- rec(tex)
    diff(sort(unique(round(r$y[r$type == "glyph"], 3))))
  }
  base <- pitch(r"(\begin{array}{c} a \\ b \end{array})")
  # Each step of the stretch is a baselineskip (1.2em).
  stretched <- pitch(r"(\def\arraystretch{2}\begin{array}{c} a \\ b \end{array})")
  em <- rec(r"(a)")$font_size[1]
  expect_equal(stretched - base, 1.2 * em, tolerance = 0.05)
  expect_gt(stretched, base)
  # It is read at the array, and ends with the group it was set in.
  after <- pitch(r"({\renewcommand{\arraystretch}{3}}\begin{array}{c} a \\ b \end{array})")
  expect_equal(after, base)
  # Matrices are arrays too.
  expect_gt(pitch(r"(\renewcommand{\arraystretch}{2}\begin{pmatrix} a \\ b \end{pmatrix})")[1],
            pitch(r"(\begin{pmatrix} a \\ b \end{pmatrix})")[1])
})

test_that("\\global makes the definition after it last past its group", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  same(r"({\global\def\a{1}}\a)", "1")
  same(r"({\def\z{9}\global\edef\a{\z}}\a)", "9")
  same(r"({\global\let\a=x}\a)", "x")
  same(r"({\global\futurelet\q\mathrm x\q}\q)", r"(\mathrm{x}xx)")
  # Without \global the group takes it away again.
  expect_match(warns(r"({\def\a{1}}\a)"), "unknown command")
  expect_identical(warns(r"({\global\def\a{1}}\a)"), character(0))
})

test_that("\\@ifstar, \\@ifnextchar and \\@firstoftwo are names without \\makeatletter, as in KaTeX", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  same(r"(\def\f{\@ifstar{S}{N}}\f*)", "S")
  same(r"(\def\f{\@ifstar{S}{N}}\f x)", "Nx")
  same(r"(\def\f{\@ifnextchar[{Y}{N}}\f[)", "Y[")
  same(r"(\def\f{\@ifnextchar[{Y}{N}}\f x)", "Nx")
  same(r"(\@firstoftwo{a}{b})", "a")
  same(r"(\@secondoftwo{a}{b})", "b")
})

test_that("\\TextOrMath, \\sixptsize, \\strut and the text symbols are read", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  same(r"(\TextOrMath{t}{m})", "m")
  same(r"(\text{\TextOrMath{t}{m}})", r"(\text{t})")
  size <- function(tex) rec(tex)$font_size[1]
  expect_equal(size(r"(\sixptsize x)") / size("x"), 0.6, tolerance = 1e-3)
  expect_identical(warns(r"(a\strut b\ordinarycolon)"), character(0))
  expect_identical(warns(r"(\text{\textsection\textparagraph\textcopyright\texttrademark\textquotedbl})"),
                   character(0))
})

test_that("\\Overrightarrow, \\overleftharpoon and \\overrightharpoon are drawn over their text", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  parts <- function(tex) {
    r <- rec(tex)
    list(lines = r[r$type == "line", ], glyphs = r[r$type == "glyph", ])
  }
  for (tex in c(r"(\Overrightarrow{AB})", r"(\overleftharpoon{AB})", r"(\overrightharpoon{AB})")) {
    p <- parts(tex)
    expect_identical(warns(tex), character(0), info = tex)
    # Over the letters, and as wide as they are.
    expect_true(all(c(p$lines$y, p$lines$y2) < min(p$glyphs$y) - 5), info = tex)
    span <- range(c(p$lines$x, p$lines$x2))
    expect_true(diff(span) >= diff(range(p$glyphs$x)), info = tex)
  }
  # The double arrow has two shafts.
  long <- function(l) l[sqrt((l$x2 - l$x)^2 + (l$y2 - l$y)^2) > 5, ]
  expect_equal(nrow(long(parts(r"(\Overrightarrow{AB})")$lines)), 2L)
  expect_equal(nrow(long(parts(r"(\overrightharpoon{AB})")$lines)), 1L)
})
