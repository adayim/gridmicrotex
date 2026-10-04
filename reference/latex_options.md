# Set or show default rendering options

Sets defaults for
[`latex_grob()`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md),
[`grid.latex()`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md),
[`latex_dims()`](https://adayim.github.io/gridmicrotex/reference/latex_dims.md),
[`latex_tree()`](https://adayim.github.io/gridmicrotex/reference/latex_tree.md)
and the markdown functions. An argument given in a call always wins over
the default.

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
  device_math = NULL,
  main_font = NULL,
  sans_font = NULL,
  mono_font = NULL
)

reset_latex_options()
```

## Arguments

- math_font:

  Math font: a loaded font with a math table; see
  [`available_fonts()`](https://adayim.github.io/gridmicrotex/reference/available_fonts.md).

- render_mode:

  `"typeface"` or `"path"`; see
  [`latex_grob()`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md).

- tex_style:

  `""`, `"display"`, `"text"`, `"script"` or `"scriptscript"`; see
  [`latex_grob()`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md).

- input_mode:

  `"mixed"`, `"math"` or `"document"`; see
  [`latex_grob()`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md).

- justify:

  If `TRUE`, stretch wrapped lines to fill `max_width`.

- line_break:

  `"greedy"` or `"optimal"`; see
  [`latex_grob()`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md).

- markdown_style:

  Default style for
  [`markdown_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_grob.md)
  and
  [`markdown_box_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md):
  a
  [`markdown_style()`](https://adayim.github.io/gridmicrotex/reference/markdown_style.md),
  CSS text, or a path to a `.css` file.

- device_math:

  If `TRUE`, any plot label containing math, such as `"Slope $x^2$"`, is
  typeset as LaTeX. This works for base graphics (`main`, `xlab`,
  [`text()`](https://rdrr.io/r/graphics/text.html),
  [`legend()`](https://rdrr.io/r/graphics/legend.html), ...), and also
  for lattice, grid and ggplot2. Labels without math, such as
  `"Cost $5-$10"`, are left alone.

  Heights are not adjusted: a tall formula can overflow a margin or a
  [`legend()`](https://rdrr.io/r/graphics/legend.html) box. Measure it
  with
  [`latex_dims()`](https://adayim.github.io/gridmicrotex/reference/latex_dims.md)
  and make room with `par(mar = )`. See
  [`vignette("base-graphics")`](https://adayim.github.io/gridmicrotex/articles/base-graphics.md)
  for the rules and limitations.

- main_font, sans_font, mono_font:

  The fonts for text: the body, `\textsf` and `\sffamily`, and
  `\texttt`, `\ttfamily` and `\verb`. Each is the name of a font given
  to
  [`load_font()`](https://adayim.github.io/gridmicrotex/reference/load_font.md),
  an installed family, or a font file, which is loaded. `NULL` keeps the
  defaults: `gp$fontfamily` (or `"sans"`) for the body, `"sans"` and
  `"mono"`. A `gp$fontfamily` in a call wins over `main_font`.

## Value

The previous settings, invisibly. With no arguments, the current
settings.

## Details

With no arguments, returns the current settings (`NULL` means the
built-in default). Setting an option to `NULL` resets it. The previous
settings are returned, so `do.call(latex_options, old)` restores them.

Size and line spacing are set with `gp` (`fontsize`, `cex`,
`lineheight`), not here.

## See also

[`available_fonts()`](https://adayim.github.io/gridmicrotex/reference/available_fonts.md),
[`load_font()`](https://adayim.github.io/gridmicrotex/reference/load_font.md),
[`latex_grob()`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)

## Examples

``` r
# \donttest{
  old <- latex_options(math_font = "stix", render_mode = "typeface")
  grid.latex("$\\sum_{i=1}^{n} i^{2}$", gp = grid::gpar(fontsize = 14))

  do.call(latex_options, old)

  # Math in base graphics, with no other change to the plotting code.
  latex_options(device_math = TRUE)
  plot(1:10, (1:10)^2,
       main = "Slope $\\hat{\\beta}_1 = \\sum_{i=1}^{n} x_i^2$",
       ylab = "$y^2$")

# }
```
