# A ggplot2 theme element for LaTeX text

Use this as a theme element for axis titles, axis labels, plot titles,
or any other text element in a ggplot2 theme. The text string is parsed
as LaTeX math and rendered via MicroTeX.

## Usage

``` r
element_latex(
  math_font = "",
  fontsize = NULL,
  line_space = 10,
  max_width = 0,
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
