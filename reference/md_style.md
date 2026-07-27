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
| `text_decoration` | inline + block | `underline`, `line-through` |
| `line_height` | block | unitless, as [`gpar()`](https://rdrr.io/r/grid/gpar.html) wants it |
| `margin_top`, `margin_bottom` | block | margins do not collapse |
| `padding_left` | block |  |
| `text_align` | block | `left`, `center`, `right` |
| `border_left` | block | the `blockquote` bar |
| `border_top` | block | the `hr` rule |
| `height` | block | the band an `hr` sits in |
| `bullet` | `ul` | raw LaTeX for the marker glyph |
| `marker_gap` | `ul`, `ol` | marker to text |

Anything else is an error — unlike a pasted stylesheet, where an unknown
property is ignored the way a browser ignores it.

**One limitation worth knowing.** `font_weight` and `font_style` apply
to blocks whose content is prose — paragraphs, headings, list items,
block quotes and `<div>`s. They do *not* apply to `pre`, `table` or an
image's alt text, which build their own LaTeX and impose their own font
handling. This is a MicroTeX constraint rather than a choice: `\text{}`
resets the font style, so emphasis has to be decided when the content is
generated, not wrapped around it afterwards.

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
