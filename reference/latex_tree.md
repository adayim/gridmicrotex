# The layout of a LaTeX expression

Returns what a formula is drawn from: one row per glyph, line, rectangle
or run of text, with its position. Useful for checking alignment or
building your own grobs.

## Usage

``` r
latex_tree(
  tex,
  math_font = "",
  max_width = 0,
  tex_style = "",
  input_mode = c("mixed", "math", "document"),
  render_mode = c("typeface", "path"),
  justify = FALSE,
  line_break = c("greedy", "optimal"),
  gp = grid::gpar()
)
```

## Arguments

- tex:

  LaTeX, as a character string.

- math_font:

  Math font: `"lete"` (Lete Sans Math, the default), `"stix"` (STIX Two
  Math), or one added with
  [`load_font()`](https://adayim.github.io/gridmicrotex/reference/load_font.md).
  See
  [`available_fonts()`](https://adayim.github.io/gridmicrotex/reference/available_fonts.md).
  A font that is not loaded, or has no math table, is an error.

- max_width:

  Width in big points (1/72 inch) at which lines wrap. `0`, the default,
  does not wrap.

- tex_style:

  Force a TeX style: `"display"`, `"text"`, `"script"` or
  `"scriptscript"`. `""`, the default, follows the delimiters. See
  Details.

- input_mode:

  How `tex` is read:

  - `"mixed"` (default): text, with math between `$...$` or `\(...\)`. A
    newline starts a new line.

  - `"math"`: everything is math; write text in `\text{}`.

  - `"document"`: a LaTeX document body, with paragraphs, numbered
    headings and displayed equations. Use it with `max_width`.

- render_mode:

  `"typeface"` (default) draws glyphs as text, which can be selected in
  PDF and SVG output. It needs a device such as ragg, svglite or
  [`grDevices::cairo_pdf()`](https://rdrr.io/r/grDevices/cairo.html),
  and falls back to `"path"` on others, such as
  [`pdf()`](https://rdrr.io/r/grDevices/pdf.html). `"path"` draws glyphs
  as outlines, which works on every device.

- justify:

  If `TRUE`, wrapped lines are stretched to fill `max_width`, except the
  last. Needs `max_width`.

- line_break:

  `"greedy"` (default) fills one line at a time. `"optimal"` chooses the
  breaks for the whole paragraph. Needs `max_width`.

- gp:

  Graphical parameters from
  [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html): `col`,
  `fontfamily`, `fontsize`, `cex` and `lineheight`. See Details.

## Value

A list of class `"latex_tree"`:

- `records`: a data frame with one row per drawn element, with columns
  such as `type`, `x`, `y`, `glyph`, `font_size`, `color` and `text`.

- `bbox`: `width`, `height`, `depth` and `baseline`, in big points.

- `tex`: the input.

- `render_mode`: the render mode used.

## See also

[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md),
[`latex_dims`](https://adayim.github.io/gridmicrotex/reference/latex_dims.md)

## Examples

``` r
# \donttest{
  tree <- latex_tree("\\frac{a}{b}")
  print(tree)
#> <latex_tree>
#>   tex:         \frac{a}{b}
#>   render_mode: typeface
#>   bbox:        width=7.00  height=25.00  depth=9.00  baseline=0.63 (bigpts)
#>   records:     3
#>     glyph      2
#>     line       1
  head(tree$records)
#>    type         x      y glyph font_size   color    x2     y2 width height rx
#> 1 glyph 0.1890002  7.196  3628        14 #000000    NA     NA    NA     NA NA
#> 2  line 0.0000000 10.596    NA        NA #000000 7.392 10.596    NA     NA NA
#> 3 glyph 0.0000000 25.796  3629        14 #000000    NA     NA    NA     NA NA
#>   ry  lwd text font_style rotation path codepoint
#> 1 NA   NA <NA>         NA        0 NULL        NA
#> 2 NA 1.32 <NA>         NA        0 NULL        NA
#> 3 NA   NA <NA>         NA        0 NULL        NA
#>                                                             font_file
#> 1 /home/runner/work/_temp/Library/gridmicrotex/fonts/LeteSansMath.otf
#> 2                                                                <NA>
#> 3 /home/runner/work/_temp/Library/gridmicrotex/fonts/LeteSansMath.otf
#>   font_family image_ref
#> 1        <NA>      <NA>
#> 2        <NA>      <NA>
#> 3        <NA>      <NA>
# }
```
