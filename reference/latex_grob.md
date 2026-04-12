# Create a grid grob from a LaTeX expression

Parses a LaTeX math expression and returns a grid grob object that
renders the formula using native grid graphics primitives. The grob
supports standard grid queries such as
[`grobWidth()`](https://rdrr.io/r/grid/grobWidth.html),
[`grobHeight()`](https://rdrr.io/r/grid/grobWidth.html),
[`grobX()`](https://rdrr.io/r/grid/grobX.html), and
[`grobY()`](https://rdrr.io/r/grid/grobX.html).

A convenience wrapper that creates a `latex_grob` and immediately draws
it on the current device via
[`grid.draw`](https://rdrr.io/r/grid/grid.draw.html).

## Usage

``` r
latex_grob(
  tex,
  x = grid::unit(0.5, "npc"),
  y = grid::unit(0.5, "npc"),
  default.units = "npc",
  hjust = 0.5,
  vjust = 0.5,
  rot = 0,
  fontsize = 20,
  math_font = "",
  line_space = 10,
  max_width = 0,
  render_mode = c("typeface", "path"),
  name = NULL,
  gp = grid::gpar()
)

grid.latex(tex, ...)
```

## Arguments

- tex:

  Character string of LaTeX math code.

- x, y:

  Position in grid coordinates.

- default.units:

  Units for x, y if given as numeric.

- hjust, vjust:

  Horizontal/vertical justification (0-1).

- rot:

  Rotation angle in degrees, counter-clockwise (default: 0). Matches the
  `rot` parameter of
  [`textGrob`](https://rdrr.io/r/grid/grid.text.html).

- fontsize:

  Base font size in points (default: 20).

- math_font:

  Name of the math font to use (e.g., `"xits"`). Use `""` (default) for
  the default Latin Modern Math font. See
  [`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md)
  for loaded fonts.

- line_space:

  Numeric inter-line spacing in big points for multi-line formulas
  (e.g., `\\`, `\begin{array}`). Default is `10`.

- max_width:

  Numeric maximum width in big points for automatic line wrapping. Use
  `0` (default) for no wrapping.

- render_mode:

  Character string: `"typeface"` (default) renders glyphs as native text
  using the math font, producing selectable/accessible text in PDF and
  SVG output. Requires the math font to be installed on the system.
  Falls back to path mode automatically on devices that do not support
  font embedding (e.g., the base
  [`pdf()`](https://rdrr.io/r/grDevices/pdf.html) device). `"path"`
  renders math symbols as filled vector paths (works on all devices but
  text is not selectable in PDF/SVG). For PDF output with
  embedded/selectable text, prefer
  [`cairo_pdf`](https://rdrr.io/r/grDevices/cairo.html).

- name:

  Optional grob name.

- gp:

  Graphical parameters (see [`gpar`](https://rdrr.io/r/grid/gpar.html)).
  Use `gpar(col = "red")` to set the default foreground color for the
  formula. Individual elements can still be overridden with
  `\textcolor{}` in the LaTeX string.

  Font-related parameters (`fontfamily`, `fontface`) control the
  appearance of text inside `\text{}` and `\mbox{}`. For example,
  `gpar(fontfamily = "serif")` renders `\text{Hello}` in R's serif font
  family. Any font available to R's graphics system can be used (base
  families like `"sans"`, `"serif"`, `"mono"`, or fonts registered via
  showtext / systemfonts). Math symbols always use the selected math
  font.

- ...:

  Additional arguments passed to `latex_grob`.

## Value

A `grid` grob of class `"latexgrob"`.

Invisibly returns the grob.

## See also

`grid.latex`,
[`latex_dims`](https://adayim.github.io/gridmicrotex/reference/latex_dims.md),
[`geom_latex`](https://adayim.github.io/gridmicrotex/reference/geom_latex.md),
[`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md)

## Examples

``` r
# \donttest{
  g <- latex_grob("\\frac{a}{b}", fontsize = 30)
  grid::grid.draw(g)

  # Red formula
  grid::grid.draw(latex_grob("x^{2}", gp = grid::gpar(col = "red")))

  # Rotated formula
  grid::grid.draw(latex_grob("x^{2} + y^{2}", fontsize = 24, rot = 45))
# }
# \donttest{
  grid.latex("x^{2} + y^{2} = z^{2}")

# }
```
