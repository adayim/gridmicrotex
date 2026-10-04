# A ggplot2 theme element for LaTeX text

A theme element that renders text, such as an axis or plot title, as
LaTeX. The `$` signs are optional. It takes the same settings as
[`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
and inherits from the theme like it.

## Usage

``` r
element_latex(
  math_font = "",
  fontsize = NULL,
  lineheight = 1.2,
  max_width = 0,
  input_mode = c("mixed", "math", "document"),
  render_mode = c("typeface", "path"),
  ...
)
```

## Arguments

- math_font:

  Math font: `"lete"` (Lete Sans Math, the default), `"stix"` (STIX Two
  Math), or one added with
  [`load_font()`](https://adayim.github.io/gridmicrotex/reference/load_font.md).
  See
  [`available_fonts()`](https://adayim.github.io/gridmicrotex/reference/available_fonts.md).
  A font that is not loaded, or has no math table, is an error.

- fontsize:

  Font size in points. `NULL` (default) uses the theme's size.

- lineheight:

  Line spacing (default 1.2).

- max_width:

  Width in big points (1/72 inch) at which lines wrap. `0`, the default,
  does not wrap.

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

- ...:

  Passed to
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html),
  such as `colour` or `hjust`.

## Value

A theme element of class `element_latex`, a kind of `element_text`.

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
