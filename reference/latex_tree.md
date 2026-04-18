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

- gp:

  Graphical parameters (see [`gpar`](https://rdrr.io/r/grid/gpar.html)).
  Common entries: `col` (formula foreground), `fontfamily` / `fontface`
  (text font), `fontsize` / `cex` (formula size), and `lineheight`
  (multi-line spacing). See
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
#>   bbox:        width=7.00  height=25.00  depth=9.00  baseline=0.63 (bigpts)
#>   records:     3
#>     glyph      2
#>     line       1
  head(tree$records)
#>    type     x      y glyph font_size   color    x2     y2 width height rx ry
#> 1 glyph 0.175  7.224  2701        14 #000000    NA     NA    NA     NA NA NA
#> 2  line 0.000 10.624    NA        NA #000000 7.392 10.624    NA     NA NA NA
#> 3 glyph 0.000 25.824  2702        14 #000000    NA     NA    NA     NA NA NA
#>    lwd text font_style rotation path codepoint
#> 1   NA <NA>         NA        0 NULL        NA
#> 2 1.32 <NA>         NA        0 NULL        NA
#> 3   NA <NA>         NA        0 NULL        NA
#>                                                             font_file
#> 1 /home/runner/work/_temp/Library/gridmicrotex/fonts/LeteSansMath.otf
#> 2                                                                <NA>
#> 3 /home/runner/work/_temp/Library/gridmicrotex/fonts/LeteSansMath.otf
# }
```
