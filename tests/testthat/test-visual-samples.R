# Visual samples, grouped into two figures so one look shows whether
# anything moved. They do not run on CRAN; test-text-runs.R holds the
# assertions behind the direction figure.

# Draws one labelled sample, positioned in points from the top-left.
place <- function(x, y, label, tex, fontsize, ...) {
  top <- function(dy) grid::unit(1, "npc") - grid::unit(dy, "pt")
  grid::grid.text(label, x = grid::unit(x, "pt"), y = top(y),
                  hjust = 0, vjust = 1,
                  gp = grid::gpar(fontsize = 8, col = "grey45"))
  grid.latex(tex, x = grid::unit(x, "pt"), y = top(y + 13),
             hjust = 0, vjust = 1, input_mode = "math",
             render_mode = "path", gp = grid::gpar(fontsize = fontsize), ...)
}

test_that("visual: math gallery", {
  skip_if_not_installed("vdiffr")
  skip_on_os("mac")
  vdiffr::expect_doppelganger("math-gallery", function() {
    # Left column: the wide samples.
    place(24, 20, "complex formula", paste0(
      "\\begin{array}{l}",
      "  \\forall\\varepsilon\\in\\mathbb{R}_+^*\\ \\exists\\eta>0",
      "\\ |x-x_0|\\leq\\eta\\Longrightarrow|f(x)-f(x_0)|\\leq\\varepsilon\\\\",
      "  \\det",
      "  \\begin{bmatrix}",
      "      a_{11}&a_{12}&\\cdots&a_{1n}\\\\",
      "      a_{21}&\\ddots&&\\vdots\\\\",
      "      \\vdots&&\\ddots&\\vdots\\\\",
      "      a_{n1}&\\cdots&\\cdots&a_{nn}",
      "  \\end{bmatrix}",
      "  \\overset{\\mathrm{def}}{=}\\sum_{\\sigma\\in\\mathfrak{S}_n}",
      "\\varepsilon(\\sigma)\\prod_{k=1}^n a_{k\\sigma(k)}\\\\",
      "  \\int_0^\\infty{x^{2n} e^{-a x^2}\\,dx} = \\frac{2n-1}{2a}",
      " \\int_0^\\infty{x^{2(n-1)} e^{-a x^2}\\,dx}",
      " = \\frac{(2n-1)!!}{2^{n+1}} \\sqrt{\\frac{\\pi}{a^{2n+1}}}\\\\",
      "\\end{array}"
    ), 12)

    place(24, 150, "multicolumn and borders", paste0(
      "\\begin{array}{|c|c|c|c|}",
      "  \\hline",
      "  \\multicolumn{4}{|c|}{\\text{Table Head}}\\\\",
      "  \\hline",
      "  \\text{Matrix}&\\multicolumn{2}{|c|}{\\text{Multicolumns}}",
      "&\\text{Font size commands}\\\\",
      "  \\hline",
      "  \\begin{pmatrix}",
      "      \\alpha_{11}&\\cdots&\\alpha_{1n}\\\\",
      "      \\hdotsfor{3}\\\\",
      "      \\alpha_{n1}&\\cdots&\\alpha_{nn}",
      "  \\end{pmatrix}",
      "  &\\large \\text{Left}&\\small \\text{Right}",
      "  &\\small \\text{small Small}\\\\",
      "  \\hline",
      "  \\multicolumn{4}{|c|}{\\text{Table Foot}}\\\\",
      "  \\hline",
      "\\end{array}"
    ), 12)

    # \Roman* and \alph* counters, nested lists, and \thickhline and \cline.
    place(24, 275, "lists and table rules", paste0(
      "\\begin{enumerate}[\\Roman*.]",
      "  \\item \\text{Limit: }\\forall\\varepsilon>0\\ \\exists\\eta>0",
      "  \\item \\text{Bullets:}\\ ",
      "        \\begin{itemize}",
      "          \\item e^{i\\pi}+1=0",
      "          \\item \\sum_{k=1}^n k=\\tfrac{n(n+1)}{2}",
      "        \\end{itemize}",
      "  \\item \\text{Lettered:}\\ ",
      "        \\begin{enumerate}[\\alph*)]",
      "          \\item \\alpha^2 \\item \\sqrt{\\beta} \\item \\gamma_0",
      "        \\end{enumerate}",
      "  \\item \\text{Ruled table:}\\ ",
      "        \\begin{array}{|c|c|c|}",
      "          \\thickhline x^2&y^2&z^2\\\\",
      "          \\cline{1-2} a&b&c\\\\\\thickhline",
      "        \\end{array}",
      "\\end{enumerate}"
    ), 15)

    # Right column: the narrow ones.
    place(380, 20, "continued fraction",
          "\\cfrac{1}{\\sqrt{2}+\n\\cfrac{1}{\\sqrt{2}+\n\\cfrac{1}{\\sqrt{2}+\\dotsb\n}}}",
          20)
    place(380, 145, "overbrace and underbrace",
          "\\rlap{\\overbrace{\\phantom{1 + a + b + \\cdots + z}}^{\\text{total + 1}}}\n1 + \\underbrace{a + b + \\cdots + z}_{\\text{total}}",
          20)
    place(380, 235, "cancel variants",
          "\\frac{a\\cancel{b}}{\\cancel{b}} = a;\n\\frac{a\\bcancel{b}}{\\bcancel{b}} = a;\n\\frac{a\\xcancel{b}}{\\xcancel{b}} = a;",
          20)
    place(380, 290, "cases", paste0(
      "P_{r-j}=\\begin{cases}",
      "0& \\text{if $r-j$ is odd},\\\\",
      "r!\\,(-1)^{(r-j)/2}& \\text{if $r-j$ is even}.",
      "\\end{cases}"
    ), 12)
  })
})

test_that("visual: text direction", {
  skip_if_not_installed("vdiffr")
  skip_on_os("mac")
  # Needs Perso-Arabic and Hebrew system fonts, which CI images may lack.
  skip_on_ci()

  # Uyghur "xush keldingiz" and "bu dunyagha", and Hebrew "shalom olam".
  hello <- "خۇش كەلدىڭىز"
  more  <- "بۇ دۇنياغا"
  heb   <- "שלום עולם"

  # Unwrapped rows only: vdiffr's device measures Perso-Arabic about twice
  # too wide, so wrapped rows would record the device, not the layout.
  vdiffr::expect_doppelganger("text-direction", function() {
    # Each row twice: ours on the left, grid.text() on the right. The two
    # columns must read alike.
    ref <- function(y, s) {
      grid::grid.text(s, x = grid::unit(380, "pt"),
                      y = grid::unit(1, "npc") - grid::unit(y + 13, "pt"),
                      hjust = 0, vjust = 1,
                      gp = grid::gpar(fontsize = 16))
    }
    colhead <- function(x, s) {
      grid::grid.text(s, x = grid::unit(x, "pt"),
                      y = grid::unit(1, "npc") - grid::unit(8, "pt"),
                      hjust = 0, vjust = 1,
                      gp = grid::gpar(fontsize = 8, col = "grey45"))
    }
    colhead(24, "gridmicrotex::grid.latex()")
    colhead(380, "grid::grid.text() reference")

    rows <- list(
      list(24,  "left-to-right (control)", "Hello world"),
      list(74,  "right-to-left, Uyghur", hello),
      list(124, "right-to-left, Hebrew", heb),
      # The first strong character sets the direction, so these two differ.
      list(174, "right-to-left inside left-to-right",
           paste("The greeting", hello, "means welcome")),
      list(224, "left-to-right inside right-to-left",
           paste(hello, "and", more)),
      # Digits run left to right inside a right-to-left line.
      list(274, "digits inside right-to-left",
           paste(hello, "2026", more))
    )
    for (r in rows) {
      place(24, r[[1]], r[[2]], paste0("\\text{", r[[3]], "}"), 16)
      ref(r[[1]], r[[3]])
    }
  })
})
