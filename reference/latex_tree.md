# Inspect the parsed layout of a LaTeX expression

Returns the raw draw-record table produced by MicroTeX's layout pass
together with the bounding-box metadata. Useful for debugging alignment
issues, building custom grobs on top of the layout, or counting
glyphs/paths/rules in a formula.

## Usage

``` r
latex_tree(
  tex,
  math_font = "",
  max_width = 0,
  tex_style = "",
  input_mode = c("mixed", "math"),
  render_mode = c("typeface", "path"),
  gp = grid::gpar()
)
```

## Arguments

- tex:

  Character string of LaTeX math code.

- math_font:

  Name of the math font to use (e.g., `"stix"`). Use `""` (default) for
  Lete Sans Math, which pairs with R's default sans-serif text font. See
  [`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md)
  for loaded fonts.

- max_width:

  Numeric maximum width in big points for automatic line wrapping. Use
  `0` (default) for no wrapping.

- tex_style:

  Character: TeX style override. One of `""` (default; let the parser
  decide), `"display"`, `"text"`, `"script"`, or `"scriptscript"`. See
  [`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)
  for the semantics of each value.

- input_mode:

  How `tex` is interpreted before being parsed. `"mixed"` (default)
  wraps the input in `\text{...}` so the string reads as ordinary text
  and `$...$` (or `\(...\)`) opens math mode, matching document-level
  LaTeX semantics. Useful for labels that arrive from external sources
  mixing prose and math without explicit `\text{}` markers. `"math"` is
  the classic MicroTeX behaviour — the whole string is treated as math,
  so unwrapped prose renders as spaced math italics. The default can be
  changed globally via
  [`latex_options`](https://adayim.github.io/gridmicrotex/reference/latex_options.md)`(input_mode = "math")`.
  See
  [`latex_wrap`](https://adayim.github.io/gridmicrotex/reference/latex_wrap.md)
  for details on the wrapping process.

- render_mode:

  Character string: `"typeface"` (default) renders glyphs as native text
  using the math font, producing selectable/accessible text in PDF and
  SVG output. Bundled math fonts and any registered via
  [`load_math_font`](https://adayim.github.io/gridmicrotex/reference/load_math_font.md)
  are read directly from their OTF files — no system-wide font install
  is required. Falls back to path mode automatically on devices that
  lack the R \\\geq\\ 4.3 glyph engine (e.g., the base
  [`pdf()`](https://rdrr.io/r/grDevices/pdf.html) device). For
  selectable PDF output, prefer
  [`cairo_pdf`](https://rdrr.io/r/grDevices/cairo.html). `"path"`
  renders math symbols as filled vector paths (works on all devices but
  text is not selectable in PDF/SVG).

- gp:

  Graphical parameters (see [`gpar`](https://rdrr.io/r/grid/gpar.html)).
  Common entries: `col` (formula foreground), `fontfamily` (text font),
  `fontsize` / `cex` (formula size), and `lineheight` (multi-line
  spacing). See
  [`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)
  for how each of these flows through MicroTeX.

## Value

A list with class `"latex_tree"` containing:

- `records`:

  Data frame of draw records (one row per glyph, path, line, rect, or
  text block). Columns include `type`, `x`, `y`, `glyph`, `font_size`,
  `color`, `text`, `codepoint`, `font_file`.

- `bbox`:

  Named numeric vector with `width`, `height`, `depth`, `baseline` (all
  in big points).

- `tex`:

  The (macro-expanded) input string.

- `render_mode`:

  Rendering mode used for the layout.

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
#>   bbox:        width=7.00  height=30.00  depth=11.00  baseline=0.61 (bigpts)
#>   records:     3
#>     glyph      2
#>     line       1
  head(tree$records)
#>    type         x         y glyph font_size   color   x2     y2 width height rx
#> 1 glyph 0.0000000  6.705999  3326        14 #000000   NA     NA    NA     NA NA
#> 2  line 0.0000000 13.245999    NA        NA #000000 7.77 13.246    NA     NA NA
#> 3 glyph 0.2589999 30.105997  3327        14 #000000   NA     NA    NA     NA NA
#>   ry  lwd text font_style rotation path codepoint
#> 1 NA   NA <NA>         NA        0 NULL        NA
#> 2 NA 1.36 <NA>         NA        0 NULL        NA
#> 3 NA   NA <NA>         NA        0 NULL        NA
#>                                                                    font_file
#> 1 /home/runner/work/_temp/Library/gridmicrotex/fonts/STIXTwoMath-Regular.otf
#> 2                                                                       <NA>
#> 3 /home/runner/work/_temp/Library/gridmicrotex/fonts/STIXTwoMath-Regular.otf
#>   font_family
#> 1        <NA>
#> 2        <NA>
#> 3        <NA>
# }
```
