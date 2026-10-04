# Size of a LaTeX expression

Size of a LaTeX expression

## Usage

``` r
latex_dims(
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

A list of grid units in big points:

- `width`, `height`: the size of the bounding box.

- `depth`: how far it extends below the baseline.

- `baseline`: the height of the baseline above the bottom.

And `is_split`: `TRUE` if the text was wrapped over several lines.

## Examples

``` r
latex_dims(r"($\frac{a}{b}$)")
#> $width
#> [1] 7bigpts
#> 
#> $height
#> [1] 25bigpts
#> 
#> $depth
#> [1] 9bigpts
#> 
#> $baseline
#> [1] 9.36317294836044bigpts
#> 
#> $is_split
#> [1] FALSE
#> 
```
