# Render markdown as a grid grob

Parses a markdown string and returns a grid grob, so plot labels can mix
ordinary prose formatting with real LaTeX math. Inline markdown
(`**bold**`, `*italic*`, `` `code` ``, `~~strike~~`) is translated to
the equivalent LaTeX commands, while `$...$` (and `\(...\)`, `$$...$$`,
`\[...\]`) math spans are passed through to MicroTeX byte-for-byte.

## Usage

``` r
markdown_grob(md, style = NULL, ...)

grid.markdown(md, ...)
```

## Arguments

- md:

  Character string of markdown.

- style:

  Appearance of the text: a
  [`markdown_style`](https://adayim.github.io/gridmicrotex/reference/markdown_style.md)
  object, CSS text, or a path to a `.css` file. `NULL` (default) uses
  `latex_options("markdown_style")` if set, and the built-in defaults
  otherwise. Only the properties `md_style` marks as *inline* apply here
  — there is no block layout in a single run for a margin, an indent or
  an alignment to act on, so those are ignored.
  [`markdown_box_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md)
  honours them all.

- ...:

  Passed to
  [`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)
  — e.g. `x`, `y`, `hjust`, `vjust`, `rot`, `max_width`, `gp`.

## Value

A `latexgrob`, as returned by
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md).

## Details

Markdown and LaTeX disagree about several characters — most importantly
`\`, which CommonMark treats as an escape. Math spans are therefore
hidden from the markdown parser before it runs and restored afterwards,
so constructs like `$\begin{matrix}a\\b\end{matrix}$` survive intact.

That hiding uses three private-use codepoints (`U+E000`, `U+E001`,
`U+E002`) as markers, so those three characters are *removed* from the
input. They are unassigned in Unicode, but icon fonts such as Nerd Fonts
do put real glyphs there: if your text contains one it will be dropped
rather than drawn. The alternative is worse — a pasted marker would be
spliced together with a math span on the way back out and silently
duplicate a formula.

GFM has no markdown syntax for colour, underline, super/subscript,
highlight or size, so — as in CommonMark, and as ggtext does — these
come from inline HTML. Each tag renders as HTML's own default rendering
prescribes:

|  |  |
|----|----|
| **tag** | **effect** |
| `<b>`, `<strong>` | bold |
| `<i>`, `<em>`, `<cite>`, `<dfn>`, `<var>`, `<address>` | italic |
| `<code>`, `<kbd>`, `<samp>`, `<tt>` | monospace |
| `<u>`, `<ins>` | underline |
| `<s>`, `<del>`, `<strike>` | strikethrough |
| `<sub>`, `<sup>` | sub / superscript |
| `<mark>` | yellow highlight |
| `<small>`, `<big>` | smaller / larger |
| `<q>` | wrapped in quotation marks |
| `<br>` | line break |
| `<span style="...">` | see below |

A `style` attribute is read for `color` (any R colour name, the nine CSS
names R lacks — `crimson`, `teal`, `rebeccapurple` and friends — `#rgb`,
`#rrggbb` or [`rgb()`](https://rdrr.io/r/grDevices/rgb.html); note that
`green`, `gray`, `grey`, `maroon` and `purple` keep their R values, not
their CSS ones), `text-decoration` (`underline`, `line-through`),
`font-size` (`pt`, `px`, `in`, `cm`, `mm`, `em`, `rem`, `%`, `smaller`,
`larger`) and `font-family`. Any other property is ignored.

`font-family` takes the CSS generics `monospace`, `sans-serif` and
`serif`, or any font name; a fallback list resolves to its first entry.
The name is handed to `gp$fontfamily`, so *the device resolves it*: ragg
and svglite see any installed family plus anything registered with
[`systemfonts::register_font()`](https://systemfonts.r-lib.org/reference/register_font.html),
cairo devices see installed families, and base
[`pdf()`](https://rdrr.io/r/grDevices/pdf.html) sees only what
[`pdfFonts()`](https://rdrr.io/r/grDevices/postscriptFonts.html)
declares — a named family will not resolve there. An unavailable font
falls back silently, as it does for `gpar(fontfamily=)`. A font file
that is not installed system-wide is used by registering it first:

    systemfonts::register_font(name = "MyFont", plain = "MyFont.otf")

[`load_math_font`](https://adayim.github.io/gridmicrotex/reference/load_math_font.md)
is *not* the function for this — it registers *math* fonts with
MicroTeX, which is a different mechanism.

A span's own family wins over `gp$fontfamily`, but the width of the
spaces *between* its words still comes from `gp$fontfamily`; set both to
the same family if that shows.

Tags nest and combine freely with markdown. Any other tag — and all
block-level HTML — is dropped, keeping the text inside it, which is also
what a browser shows for the ones (`<a>`, `<abbr>`, `<span>` without a
style) that have no default rendering.

Not every markdown feature has a MicroTeX equivalent. Links keep their
text and drop the destination, and images keep their alt text.

Everything is flattened into a single run here, with paragraphs joined
by line breaks: there is no block layout, so indentation, list markers
and block-quote rules need
[`markdown_box_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md).
A heading still takes the size and weight its `style` gives it, since
those compile to LaTeX commands rather than to layout.

## See also

[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md),
[`latex_wrap`](https://adayim.github.io/gridmicrotex/reference/latex_wrap.md)

## Examples

``` r
# \donttest{
  grid::grid.newpage()
  grid.markdown("The **fitted** slope is $\\beta_1$, *p* < 0.001")

# }
```
