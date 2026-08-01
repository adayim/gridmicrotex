# Declarations for one markdown tag

A set of CSS declarations, for use as a named argument to
[`markdown_style`](https://adayim.github.io/gridmicrotex/reference/markdown_style.md).
Argument names are the CSS property names with underscores in place of
hyphens, so `font_size` sets `font-size`.

## Usage

``` r
md_style(...)
```

## Arguments

- ...:

  Named declarations.

## Value

An object of class `gridmicrotex_md_style`.

## Details

Lengths accept three forms: a bare number is `rem`, a multiple of the
body font size (`font_size = 2.5`); a string is whatever CSS says it is
(`"2.5em"`, `"12pt"`, `"150%"`); and a
[`unit`](https://rdrr.io/r/grid/unit.html) is absolute.

These are the supported properties, and where each one has an effect.
*Inline* means it also works on a `<span>` and in
[`markdown_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_grob.md),
which has no block layout; *block* means it needs
[`markdown_box_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md).

|  |  |  |
|----|----|----|
| **property** | **scope** | **notes** |
| `color` | inline + block |  |
| `font_size` | inline + block |  |
| `font_family` | inline + block |  |
| `font_weight` | inline + block | prose blocks only, see below |
| `font_style` | inline + block | prose blocks only, see below |
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
| `table_layout` | `table` | `fixed` divides the width between the columns so a wide table wraps |
| `height` | block | the band an `hr` sits in |
| `bullet` | `ul` | raw LaTeX for the marker glyph |
| `marker_gap` | `ul`, `ol` | marker to text |

`font_size` also accepts CSS's keywords — `xx-small` through `xx-large`,
plus `smaller` and `larger` — taken from the `\tiny`..`\Huge` ladder
MicroTeX implements.

**The `body` rule styles the box itself.** On any other tag,
`background`, `border`, `border_radius`, `padding` and `margin` apply to
that block. On `body` they apply to the whole
[`markdown_box_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md)
— its fill, its frame, its corner radius, and the space inside and
outside it. That is the only way to give a
[`element_markdown`](https://adayim.github.io/gridmicrotex/reference/element_markdown.md)
title a background, since the theme element takes no box arguments of
its own:

    body { background: grey95; padding: 8px;
            border: 1px solid grey60; border-radius: 4px }

An explicit `box_gp`, `padding`, `margin` or `r` argument to
[`markdown_box_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md)
wins over the rule, the way an inline style wins in CSS.

Anything else is an error — unlike a pasted stylesheet, where an unknown
property is ignored the way a browser ignores it.

**One limitation worth knowing.** `font_weight` and `font_style` apply
to blocks whose content is prose — paragraphs, headings, list items,
block quotes, table cells and `<div>`s. They do *not* apply to `pre` or
an image's alt text, which build their own LaTeX and impose their own
font handling. This is a MicroTeX constraint rather than a choice:
`\text{}` resets the font style, so emphasis has to be decided when the
content is generated, not wrapped around it afterwards.

**What cannot be styled at all.** There is no small-caps (`\textsc` is
not a MicroTeX command), no `font-variant-numeric`, no right-to-left or
bidirectional text, and no padding inside an inline `border` — MicroTeX
has no `\fboxsep`, so that inset is fixed.

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
