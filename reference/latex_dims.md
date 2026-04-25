# Get dimensions of a LaTeX expression

Get dimensions of a LaTeX expression

## Usage

``` r
latex_dims(
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
  SVG output. Bundled math fonts and any registered via
  [`load_font`](https://adayim.github.io/gridmicrotex/reference/load_font.md)
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
  Common entries: `col` (formula foreground), `fontfamily` / `fontface`
  (text font), `fontsize` / `cex` (formula size), and `lineheight`
  (multi-line spacing). See
  [`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)
  for how each of these flows through MicroTeX.

## Value

A list with the following elements:

- `width`, `height`, `depth`: grid unit objects in big points. `height`
  is total height (ascent + descent).

- `baseline`: grid unit object giving the baseline position measured in
  big points from the *bottom* of the bounding box. Equivalent to
  `height - depth` for single-line formulas. Useful for aligning a
  formula's baseline with surrounding text.

- `is_split`: logical; `TRUE` if the formula was wrapped across multiple
  lines (only possible when `max_width > 0`).

## Examples

``` r
latex_dims("\\frac{a}{b}")
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
