# Render a markdown label as a grid grob

Draws a line of markdown, such as a plot title, with LaTeX math between
`$...$`. `**bold**`, `*italic*`, `` `code` `` and `~~strike~~` work, and
so does some inline HTML. For headings, lists, tables and other blocks,
use
[`markdown_box_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md).

## Usage

``` r
markdown_grob(md, style = NULL, ...)

grid.markdown(md, ...)
```

## Arguments

- md:

  Markdown, as a character string.

- style:

  A
  [`markdown_style()`](https://adayim.github.io/gridmicrotex/reference/markdown_style.md),
  CSS text, or a path to a `.css` file. `NULL` (default) uses
  `latex_options("markdown_style")`, if set. Only properties marked
  *inline* in
  [`md_style()`](https://adayim.github.io/gridmicrotex/reference/md_style.md)
  apply.

- ...:

  Passed to
  [`latex_grob()`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md),
  such as `x`, `y`, `hjust`, `vjust`, `rot`, `max_width` and `gp`.

## Value

A grob, as from
[`latex_grob()`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md).
`grid.markdown()` draws it and returns it invisibly.

## Details

These HTML tags are supported:

|                                                        |                   |
|--------------------------------------------------------|-------------------|
| **tag**                                                | **effect**        |
| `<b>`, `<strong>`                                      | bold              |
| `<i>`, `<em>`, `<cite>`, `<dfn>`, `<var>`, `<address>` | italic            |
| `<code>`, `<kbd>`, `<samp>`, `<tt>`                    | monospace         |
| `<u>`, `<ins>`                                         | underline         |
| `<s>`, `<del>`, `<strike>`                             | strikethrough     |
| `<sub>`, `<sup>`                                       | sub / superscript |
| `<mark>`                                               | yellow highlight  |
| `<small>`, `<big>`                                     | smaller / larger  |
| `<q>`                                                  | quotation marks   |
| `<br>`                                                 | line break        |
| `<span style="...">`                                   | see below         |

A `style` attribute can set `color`, `text-decoration` (`underline`,
`line-through`), `font-size` (such as `12pt`, `1.2em` or `smaller`) and
`font-family`; other properties are ignored. Colours are R colour names,
CSS names, `#rgb`, `#rrggbb` or
[`rgb()`](https://rdrr.io/r/grDevices/rgb.html). `font-family` takes
`serif`, `sans-serif`, `monospace` or an installed font; to use a font
file that is not installed, register it first with
[`systemfonts::register_font()`](https://systemfonts.r-lib.org/reference/register_font.html).

Tags nest and can hold markdown and math. Other tags are dropped and
their text kept. Links keep their text only. Images must be local PNG,
JPEG or SVG files. The characters U+E000 to U+E002 are removed from the
input.

## See also

[`markdown_box_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md),
[`geom_markdown()`](https://adayim.github.io/gridmicrotex/reference/geom_markdown.md)

## Examples

``` r
# \donttest{
  grid::grid.newpage()
  grid.markdown(r"(The **fitted** slope is $\beta_1$, *p* < 0.001)")

# }
```
