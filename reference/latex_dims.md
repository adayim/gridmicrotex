# Get dimensions of a LaTeX expression

Get dimensions of a LaTeX expression

## Usage

``` r
latex_dims(
  tex,
  fontsize = 20,
  line_space = 10,
  max_width = 0,
  render_mode = c("typeface", "path")
)
```

## Arguments

- tex:

  Character string of LaTeX math code.

- fontsize:

  Base font size in points (default: 20).

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

## Value

A list with `width`, `height`, `depth`, and `baseline` as grid unit
objects.

## Examples

``` r
latex_dims("\\frac{a}{b}")
#> $width
#> [1] 8bigpts
#> 
#> $height
#> [1] 21bigpts
#> 
#> $depth
#> [1] 7bigpts
#> 
#> $baseline
#> [1] 0.6662558
#> 
```
