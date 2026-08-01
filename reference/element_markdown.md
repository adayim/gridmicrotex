# A ggplot2 theme element for markdown text

Use as a theme element for axis titles, axis labels, plot titles or any
other text element. The label is parsed as markdown — including `$...$`
math — and rendered via MicroTeX.

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

  Name of the math font to use (e.g. `"stix"`).

- fontsize:

  Convenience alias for `size`; forwarded to
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
  as the text size in points. `NULL` (default) uses the theme's
  inherited size.

- lineheight:

  Multi-line height multiplier (default 1.2).

- max_width:

  Maximum width in big points for automatic line wrapping (default: 0,
  no wrapping).

- render_mode:

  `"typeface"` (default) or `"path"`.

- justify:

  Logical; justify wrapped lines. Requires `max_width`.

- style:

  A
  [`markdown_style`](https://adayim.github.io/gridmicrotex/reference/markdown_style.md)
  object, CSS text, or the path to a `.css` file, applied to labels
  drawn through this theme element. `NA`, the default, means unset — the
  global
  [`latex_options`](https://adayim.github.io/gridmicrotex/reference/latex_options.md)`(markdown_style = )`
  applies instead. Only the properties that compile to LaTeX have an
  effect, unless the label is laid out as blocks (see *Block labels*).
  See
  [`md_style`](https://adayim.github.io/gridmicrotex/reference/md_style.md).

- width:

  Wrapping measure for the label, as a
  [`unit`](https://rdrr.io/r/grid/unit.html). `NA`, the default, means
  unset: the label is sized to its content and does not wrap.
  `unit(1, "npc")` is the useful value for a plot title, whose cell
  really is the full plot width. See *Block labels*.

- ...:

  Additional arguments passed to
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
  (e.g. `colour`, `hjust`).

## Value

An S7 object of class `element_markdown`, inheriting from
[`ggplot2::element_text`](https://ggplot2.tidyverse.org/reference/element.html).

## Details

This is an S7 subclass of
[`ggplot2::element_text`](https://ggplot2.tidyverse.org/reference/element.html),
so it inherits the standard text properties (size, colour, hjust, ...)
from the theme and merges correctly with inherited theme entries.

## Block labels

A label with real block structure — a heading, a list, a table, a rule,
or more than one paragraph — is laid out by
[`markdown_box_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md)
rather than flattened into one run, so list markers, indents and block
spacing survive. That makes a title like this work:

    labs(title = "## Findings\n\n- slope $\\beta_1$\n- *p* < 0.001")

The box's own background, border, padding and corner radius come from
the stylesheet's `body` rule —
`style = "body \{ background: grey95; padding: 8px \}"` — not from
arguments here. See
[`markdown_style`](https://adayim.github.io/gridmicrotex/reference/markdown_style.md).

Three details follow from how ggplot2 measures theme elements:

- **Wrapping is opt-in.** Without `width` the label is sized to its
  content, because ggplot2 asks an element for its height before placing
  it, when a relative width would resolve against the whole device
  rather than the element's cell.

- **Axis tick labels are never laid out as blocks**, whatever they
  contain, for the same reason.

- **A rotated label is never laid out as blocks.** The box cannot
  rotate, so a label with both blocks and a non-zero `angle` keeps the
  angle, is rendered as a single run, and warns. A `width` given with an
  angle becomes the run's wrapping measure instead.

`math_font`, `render_mode` and `justify` are not forwarded to the box; a
block label takes those from
[`latex_options`](https://adayim.github.io/gridmicrotex/reference/latex_options.md).

Note that ggtext also exports a function called `element_markdown()`. If
both packages are attached, the one loaded later wins; call
`gridmicrotex::element_markdown()` explicitly to be unambiguous. The two
are not interchangeable — ggtext renders HTML/CSS and images, this
renders LaTeX math.

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
