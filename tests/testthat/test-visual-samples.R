test_that("visual: complex formula", {
  skip_if_not_installed("vdiffr")
  skip_on_os("mac")
  vdiffr::expect_doppelganger("complex-formula", function() {
    grid.latex(paste0(
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
    ),
    render_mode = "path",
    input_mode = "math")
  })
})

test_that("visual: table with multicolumn and borders", {
  skip_if_not_installed("vdiffr")
  skip_on_os("mac")
  vdiffr::expect_doppelganger("table-multicolumn", function() {
    grid.latex(paste0(
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
    ), render_mode = "path",
    input_mode = "math")
  })
})

test_that("visual: overbrace and cancel", {
  skip_if_not_installed("vdiffr")
  skip_on_os("mac")
  vdiffr::expect_doppelganger("overbrace-underbrace", function() {
    grid.latex(
      "\\rlap{\\overbrace{\\phantom{1 + a + b + \\cdots + z}}^{\\text{total + 1}}}\n1 + \\underbrace{a + b + \\cdots + z}_{\\text{total}}",
      render_mode = "path", gp = grid::gpar(fontsize = 20),
      input_mode = "math"
    )
  })
  vdiffr::expect_doppelganger("cancel-variants", function() {
    grid.latex(
      "\\frac{a\\cancel{b}}{\\cancel{b}} = a;\n\\frac{a\\bcancel{b}}{\\bcancel{b}} = a;\n\\frac{a\\xcancel{b}}{\\xcancel{b}} = a;",
      render_mode = "path", gp = grid::gpar(fontsize = 20),
      input_mode = "math"
    )
  })

})

test_that("visual: cases and split", {
  skip_if_not_installed("vdiffr")
  skip_on_os("mac")
  vdiffr::expect_doppelganger("cases", function() {
    grid.latex(paste0(
      "P_{r-j}=\\begin{cases}",
      "0& \\text{if $r-j$ is odd},\\\\",
      "r!\\,(-1)^{(r-j)/2}& \\text{if $r-j$ is even}.",
      "\\end{cases}"
    ), render_mode = "path",
    input_mode = "math")
  })
})

test_that("visual: continued fraction", {
  skip_if_not_installed("vdiffr")
  skip_on_os("mac")
  vdiffr::expect_doppelganger("cfrac", function() {
    grid.latex(
      "\\cfrac{1}{\\sqrt{2}+\n\\cfrac{1}{\\sqrt{2}+\n\\cfrac{1}{\\sqrt{2}+\\dotsb\n}}}",
      render_mode = "path", gp = grid::gpar(fontsize = 20),
      input_mode = "math"
    )
  })
})

test_that("visual: list environments and table rules", {
  skip_if_not_installed("vdiffr")
  skip_on_os("mac")
  # One enumerate exercising: a custom \Roman* counter, a nested itemize, a
  # nested enumerate with an \alph* counter, and an item whose content is an
  # array using the \thickhline and \cline rules.
  vdiffr::expect_doppelganger("lists-and-rules", function() {
    grid.latex(paste0(
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
    ), render_mode = "path", gp = grid::gpar(fontsize = 15),
    input_mode = "math")
  })
})
