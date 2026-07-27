# A ggplot2 theme element for LaTeX text

Use this as a theme element for axis titles, axis labels, plot titles,
or any other text element in a ggplot2 theme. The text string is parsed
as LaTeX math and rendered via MicroTeX.

## Usage

``` r
element_latex(
  math_font = "",
  fontsize = NULL,
  lineheight = 1.2,
  max_width = 0,
  input_mode = c("mixed", "math"),
  render_mode = c("typeface", "path"),
  ...
)
```

## Arguments

- math_font:

  Name of the math font to use (e.g., `"stix"`). Use `""` (default) for
  Lete Sans Math, which pairs with R's default sans-serif text font. See
  [`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md)
  for loaded fonts.

- fontsize:

  Convenience alias for `size`; when supplied, it is forwarded to
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
  as the text size in points. If `NULL` (default), the theme's inherited
  size is used.

- lineheight:

  Multi-line height multiplier (default 1.2), matching
  [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html) semantics.

- max_width:

  Numeric maximum width in big points for automatic line wrapping. Use
  `0` (default) for no wrapping.

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

- ...:

  Additional arguments passed to
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
  (e.g., `size`, `colour`, `hjust`).

## Value

An S7 object of class `element_latex`, inheriting from
[`ggplot2::element_text`](https://ggplot2.tidyverse.org/reference/element.html).

## Details

Dollar signs (`$...$`) in the label text are stripped automatically so
that both `"\frac{a}{b}"` and `"$\frac{a}{b}$"` work.

This element is an S7 subclass of
[`ggplot2::element_text`](https://ggplot2.tidyverse.org/reference/element.html),
so it inherits all standard text properties (size, colour, hjust, etc.)
from the theme and supports
[`merge_element()`](https://ggplot2.tidyverse.org/reference/merge_element.html)
correctly.

## Examples

``` r
# \donttest{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  ggplot(mtcars, aes(wt, mpg)) + geom_point() +
    labs(x = "$\\beta_1 \\cdot x + \\beta_0$") +
    theme(axis.title.x = element_latex())
}

# }
```
