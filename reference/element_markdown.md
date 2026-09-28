# A ggplot2 theme element for markdown text

A theme element that renders text, such as an axis or plot title, as
markdown with `$math$`. It takes the same settings as
[`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
and inherits from the theme like it.

## Usage

``` r
element_markdown(
  math_font = "",
  fontsize = NULL,
  lineheight = 1.2,
  max_width = 0,
  render_mode = c("typeface", "path"),
  justify = FALSE,
  style = NA,
  width = NA,
  ...
)
```

## Arguments

- math_font:

  Math font, such as `"stix"`.

- fontsize:

  Font size in points. `NULL` (default) uses the theme's size.

- lineheight:

  Line spacing (default 1.2).

- max_width:

  Width in big points at which lines wrap. `0`, the default, does not
  wrap.

- render_mode:

  `"typeface"` (default) or `"path"`.

- justify:

  If `TRUE`, stretch wrapped lines to fill `max_width`.

- style:

  A
  [`markdown_style()`](https://adayim.github.io/gridmicrotex/reference/markdown_style.md),
  CSS text, or a path to a `.css` file. `NA` (default) uses
  `latex_options(markdown_style = )`.

- width:

  Width at which the label wraps, as a
  [`grid::unit()`](https://rdrr.io/r/grid/unit.html). `NA` (default)
  does not wrap. `unit(1, "npc")` suits a plot title.

- ...:

  Passed to
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html),
  such as `colour` or `hjust`.

## Value

A theme element of class `element_markdown`, a kind of `element_text`.

## Block labels

A label with a heading, a list, a table, a rule or several paragraphs is
laid out as a small document, as in
[`markdown_box_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md):

    labs(title = "## Findings\n\n- slope $\\beta_1$\n- *p* < 0.001")

Its box is styled by the `body` rule, as in
`style = "body { background: grey95; padding: 8px }"`.

- Text wraps only if `width` is given.

- Axis tick labels and rotated labels are never laid out as blocks; a
  rotated one warns.

- `math_font`, `render_mode` and `justify` do not apply to blocks; set
  them with
  [`latex_options()`](https://adayim.github.io/gridmicrotex/reference/latex_options.md).

ggtext also has an `element_markdown()`. If both packages are attached,
write `gridmicrotex::element_markdown()`.

## See also

[`markdown_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_grob.md),
[`element_latex`](https://adayim.github.io/gridmicrotex/reference/element_latex.md)

## Examples

``` r
# \donttest{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  ggplot(mtcars, aes(wt, mpg)) + geom_point() +
    labs(x = "**weight** in $10^3$ lbs") +
    theme(axis.title.x = gridmicrotex::element_markdown())
}

# }
```
