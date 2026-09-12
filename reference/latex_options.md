# Set or query package-wide LaTeX rendering defaults

A single entry point for project-wide defaults used by
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md),
[`grid.latex`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md),
[`latex_dims`](https://adayim.github.io/gridmicrotex/reference/latex_dims.md),
and
[`latex_tree`](https://adayim.github.io/gridmicrotex/reference/latex_tree.md).
Options set here are applied only when the corresponding argument is
*not* supplied at the call site, so explicit arguments always win.

## Usage

``` r
latex_options(
  math_font = NULL,
  render_mode = NULL,
  tex_style = NULL,
  input_mode = NULL,
  justify = NULL,
  line_break = NULL,
  markdown_style = NULL,
  device_math = NULL
)

reset_latex_options()
```

## Arguments

- math_font:

  Math font name or alias (see
  [`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md)).

- render_mode:

  Either `"typeface"` or `"path"`.

- tex_style:

  TeX style override. One of `""` (let the parser decide), `"display"`,
  `"text"`, `"script"`, or `"scriptscript"`. `"display"` forces large
  operators with limits placed over/under, useful for inline labels that
  should still look like display equations.

- input_mode:

  How the input string is interpreted before being handed to MicroTeX.
  `"mixed"` (default) wraps the string in `\text{...}` so it reads as
  ordinary text, with `$...$` (and `\(...\)`) opening math mode: the
  document-level LaTeX convention. Useful when consuming labels from
  other packages that mix prose and math without explicit `\text{}`
  markers. `"math"` treats the whole string as math: the classic
  MicroTeX behaviour, where letters render as math italics and unwrapped
  prose looks wrong.

- justify:

  Logical. When `TRUE`, wrapped text is stretched at its interword
  spaces so every line but the last fills `max_width` exactly. Has no
  effect without `max_width`, since it acts on the lines the wrapper
  produces. `FALSE` (default) leaves the right edge ragged, matching R's
  own text drawing. In a narrow column, justifying alone opens
  noticeably wide word spaces; mark the words that may break with `\-`.

- line_break:

  How lines are chosen when wrapping. `"greedy"` (default) fills each
  line as far as it will go and never reconsiders. `"optimal"` chooses
  the breaks together so the paragraph as a whole reads best, in the
  spirit of Knuth-Plass: pulling one word down early can improve every
  later line, which a greedy pass cannot see. Requires `max_width`, and
  costs a little more layout time.

- markdown_style:

  Default style for
  [`markdown_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_grob.md)
  and
  [`markdown_box_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md):
  a
  [`markdown_style`](https://adayim.github.io/gridmicrotex/reference/markdown_style.md)
  object, CSS text, or a path to a `.css` file.

- device_math:

  Logical. When `TRUE`, text drawn to the graphics device is rendered
  with MicroTeX wherever it contains math. The motivating case is *base*
  graphics, which has no other route to LaTeX: `plot(main=)`, `xlab`,
  `ylab`, [`text`](https://rdrr.io/r/graphics/text.html),
  [`mtext`](https://rdrr.io/r/graphics/mtext.html),
  [`legend`](https://rdrr.io/r/graphics/legend.html), and anything built
  on them such as [`hist()`](https://rdrr.io/r/graphics/hist.html) or a
  package's own `plot` method.

  Interception happens at the device, so it is **not** limited to base
  graphics: text drawn by grid, ggplot2 and lattice, and by other
  packages' plot methods, is affected too. `grid.text("$x^2$")` renders
  math while this is on.
  [`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md),
  [`geom_latex`](https://adayim.github.io/gridmicrotex/reference/geom_latex.md)
  and
  [`element_latex`](https://adayim.github.io/gridmicrotex/reference/element_latex.md)
  give the same rendering confined to one grob, layer or theme element,
  and measure its height correctly.

  The convention is the one
  [`latex_wrap`](https://adayim.github.io/gridmicrotex/reference/latex_wrap.md)
  already uses: `$...$`, `$$...$$`, `\(...\)`, `\[...\]`, with `\$` a
  literal dollar sign. A label is intercepted only when *every*
  delimiter in it is closed and the content looks like math, so
  `"Revenue ($)"`, `"Cost $5-$10"` and `"Budget $1,000 to $5,000"` are
  passed through untouched. Anything that cannot be laid out is drawn as
  plain text rather than raising an error.

  **Height is the one thing that cannot be corrected.** R computes text
  height from the font, never from the string, and a graphics device has
  no string-height entry point to intercept. A tall formula can
  therefore overflow a
  [`legend()`](https://rdrr.io/r/graphics/legend.html) box or the space
  `par("mar")` reserved for it. Widths *are* correct. Reserve the room
  yourself with
  [`latex_dims`](https://adayim.github.io/gridmicrotex/reference/latex_dims.md):


        h  <- latex_dims("$\\frac{a}{b}$",
                         gp = grid::gpar(fontsize = par("ps")))$height
        bp <- grid::convertHeight(h, "bigpts", TRUE)
        # par(mar) counts lines of par("cin"), not grid's "lines".
        need <- ceiling(bp / (par("cin")[2] * 72 * par("mex")))
        par(mar = c(5, need + 1, 4, 2))
        

  Other side effects worth knowing:

  - It is session-wide and reaches every device opened while it is on,
    including text drawn by packages you did not write. Labels without
    math are passed through unchanged.

  - A label containing a newline reaches the device one line at a time,
    so a formula split across lines is not recognised.

  - Math is drawn as vector outlines, so unlike
    [`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)
    it is not selectable in a PDF.

  - Prose inside a bold label is not bolded: face comes from `\textbf`
    rather than from the device.

  - `\includegraphics` is left out, and rounded box corners are drawn
    square.

  - A label that cannot be laid out is drawn as literal text, and
    warnings raised while laying it out are suppressed.

  - showtext replaces the same device callbacks when a plot starts, so
    with `showtext::showtext_auto()` on, labels stay literal.

  - A package that gives `$` its own meaning acts first: corrplot parses
    a label starting with `$` as plotmath.

  - [`expression()`](https://rdrr.io/r/base/expression.html) labels are
    untouched, because R lays plotmath out inside the graphics engine.

  - Output not drawn through an R graphics device is unaffected: plotly
    and other htmlwidgets, and rgl's own `text3d()` labels. rgl's
    `plotmath3d()` (and `text3d(usePlotmath = TRUE)`, which calls it)
    draws into an R device and does pick it up.

  - In R Markdown, turn it off in a later chunk than the one that draws:
    knitr captures a chunk's plots after its last line runs.

  [`vignette("base-graphics")`](https://adayim.github.io/gridmicrotex/articles/base-graphics.md)
  walks through all of this with examples.

## Value

Invisibly returns the previous settings (a list). With no arguments,
returns the current settings visibly.

## Details

Calling `latex_options()` with no arguments returns the current settings
(a list whose `NULL` entries mean "use the built-in default"). Supply
one or more named arguments to update them.

Font size and line spacing are controlled via `gp` parameters
(`fontsize`, `cex`, `lineheight`) at the grob level; see
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md).

## See also

[`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md),
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)

## Examples

``` r
# \donttest{
  latex_options(math_font = "stix", render_mode = "typeface")
  grid.latex("$\\sum_{i=1}^{n} i^{2}$", gp = grid::gpar(fontsize = 14))

  reset_latex_options()

  # Math in base graphics, with no other change to the plotting code.
  latex_options(device_math = TRUE)
  plot(1:10, (1:10)^2,
       main = "Slope $\\hat{\\beta}_1 = \\sum_{i=1}^{n} x_i^2$",
       ylab = "$y^2$")

# }
```
