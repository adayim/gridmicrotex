# Get dimensions of a LaTeX expression

Get dimensions of a LaTeX expression

## Usage

``` r
latex_dims(
  tex,
  math_font = "",
  max_width = 0,
  tex_style = "",
  input_mode = c("mixed", "math"),
  render_mode = c("typeface", "path"),
  justify = FALSE,
  line_break = c("greedy", "optimal"),
  hyphenate = FALSE,
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

- justify:

  Logical. When `TRUE`, wrapped text is stretched at its interword
  spaces so every line but the last fills `max_width` exactly. Requires
  `max_width`: it acts on the lines the wrapper produces, so it does
  nothing on its own. `FALSE` (default) leaves the right edge ragged,
  matching R's own text drawing. In a narrow column, pair it with
  `hyphenate`: justifying without hyphenation opens wide word spaces.

- line_break:

  How lines are chosen when wrapping. `"greedy"` (default) fills each
  line as far as it will go and never reconsiders. `"optimal"` chooses
  the breaks together so the paragraph as a whole reads best, in the
  spirit of Knuth-Plass: pulling one word down early can improve every
  later line, which a greedy pass cannot see. Requires `max_width`, and
  costs a little more layout time.

- hyphenate:

  Logical. When `TRUE`, a long word may be broken across lines at a
  point Liang's algorithm allows, with a hyphen drawn at the break. Uses
  the patterns and the `\lefthyphenmin`/`\righthyphenmin` of 2 and 3
  that TeX uses for American English; the patterns are read on first
  use. Requires `max_width`. `FALSE` is the default. A word carrying an
  explicit `\-` is left alone, as in TeX — use that to override a break
  the patterns get wrong.

- gp:

  Graphical parameters (see [`gpar`](https://rdrr.io/r/grid/gpar.html)).
  Common entries: `col` (formula foreground), `fontfamily` (text font),
  `fontsize` / `cex` (formula size), and `lineheight` (multi-line
  spacing). See
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
