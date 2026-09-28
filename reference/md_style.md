# Declarations for one markdown tag

CSS properties for one tag, for use in
[`markdown_style()`](https://adayim.github.io/gridmicrotex/reference/markdown_style.md).
Names are CSS property names with `_` for `-`, so `font_size` sets
`font-size`.

## Usage

``` r
md_style(...)
```

## Arguments

- ...:

  Properties, as named arguments.

## Value

An object of class `gridmicrotex_md_style`.

## Details

A length can be a number, meaning a multiple of the body font size
(`font_size = 2.5`); a CSS string such as `"2.5em"` or `"12pt"`; or a
[`grid::unit()`](https://rdrr.io/r/grid/unit.html).

The supported properties are below. *Inline* ones also work on a
`<span>` and in
[`markdown_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_grob.md);
*block* ones need
[`markdown_box_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md).

|  |  |  |
|----|----|----|
| **property** | **scope** | **notes** |
| `color` | inline + block |  |
| `font_size` | inline + block |  |
| `font_family` | inline + block |  |
| `font_weight` | inline + block |  |
| `font_style` | inline + block |  |
| `text_decoration` | inline + block | `underline`, `overline`, `line-through` |
| `background` | inline + block | a fill behind the text |
| `border` | inline | a frame; the inset is fixed |
| `border_style` | inline | only `double` |
| `border_radius` | inline | rounds the frame |
| `box_shadow` | inline |  |
| `visibility` | inline | `hidden` keeps the space |
| `vertical_align` | inline | `super`, `sub`, or a length |
| `transform` | inline | `rotate()`, [`scale()`](https://rdrr.io/r/base/scale.html), `scaleX(-1)` |
| `line_height` | block | unitless, as [`gpar()`](https://rdrr.io/r/grid/gpar.html) wants it |
| `margin_top`, `margin_bottom` | block | margins do not collapse |
| `margin_left`, `margin_right` | block |  |
| `padding_left`, `padding_right` | block |  |
| `padding_top`, `padding_bottom` | block |  |
| `margin`, `padding` | block | the CSS shorthand: one to four lengths, in CSS's order |
| `text_align` | block | `left`, `center`, `right` |
| `border_left` | block | the `blockquote` bar |
| `border_top` | block | the `hr` rule |
| `border_bottom` | `tr` | a rule under each table row |
| `border_color` | `table` | colour of the table's rules |
| `table_layout` | `table` | `fixed` divides the width evenly between the columns |
| `height` | block | the band an `hr` sits in |
| `bullet` | `ul` | raw LaTeX for the marker glyph |
| `marker_gap` | `ul`, `ol` | marker to text |

`font_size` also takes the keywords `xx-small` to `xx-large`, `smaller`
and `larger`.

On `body`, `background`, `border`, `border_radius`, `padding` and
`margin` style the box of
[`markdown_box_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md)
or of an
[`element_markdown()`](https://adayim.github.io/gridmicrotex/reference/element_markdown.md)
title:

    body { background: grey95; padding: 8px;
            border: 1px solid grey60; border-radius: 4px }

The `box_gp`, `padding`, `margin` and `r` arguments of
[`markdown_box_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md)
override this rule.

An unknown property is an error here, but is ignored in CSS text. Small
caps, `font-variant-numeric` and padding inside an inline `border` are
not supported.

## See also

[`markdown_style`](https://adayim.github.io/gridmicrotex/reference/markdown_style.md),
[`markdown_box_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md)

## Examples

``` r
md_style(color = "steelblue", font_size = 2.5, margin_top = 1.2)
#> <md_style>
#>   color            steelblue
#>   font-size        2.5
#>   margin-top       1.2
```
