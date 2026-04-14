# Inspect the parsed layout of a LaTeX expression

Returns the raw draw-record table produced by MicroTeX's layout pass
together with the bounding-box metadata. Useful for debugging alignment
issues, building custom grobs on top of the layout, or counting
glyphs/paths/rules in a formula.

## Usage

``` r
latex_tree(
  tex,
  fontsize = 20,
  math_font = "",
  line_space = 0,
  max_width = 0,
  render_mode = c("typeface", "path")
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
#>   bbox:        width=8.00  height=30.00  depth=11.00  baseline=0.61 (bigpts)
#>   records:     3
#>     glyph      2
#>     line       1
  head(tree$records)
#>    type         x      y glyph font_size   color    x2     y2 width height rx
#> 1 glyph 0.0000000  7.238  4421        14 #000000    NA     NA    NA     NA NA
#> 2  line 0.0000000 13.778    NA        NA #000000 8.526 13.778    NA     NA NA
#> 3 glyph 0.1189999 30.638  4422        14 #000000    NA     NA    NA     NA NA
#>   ry  lwd text font_style path codepoint
#> 1 NA   NA <NA>         NA NULL        NA
#> 2 NA 1.36 <NA>         NA NULL        NA
#> 3 NA   NA <NA>         NA NULL        NA
#>                                                                    font_file
#> 1 /home/runner/work/_temp/Library/gridmicrotex/fonts/STIXTwoMath-Regular.otf
#> 2                                                                       <NA>
#> 3 /home/runner/work/_temp/Library/gridmicrotex/fonts/STIXTwoMath-Regular.otf
# }
```
