# A style for markdown rendering

Builds the style used by
[`markdown_box_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md)
and
[`markdown_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_grob.md):
a small CSS cascade over the markdown tags. One constructor covers
creating a style, starting from a preset, and extending an existing one.
For the properties themselves, and which are honoured where, see
[`md_style`](https://adayim.github.io/gridmicrotex/reference/md_style.md).

## Usage

``` r
markdown_style(base = NULL, css = NULL, ...)
```

## Arguments

- base:

  `NULL` (default) for the built-in defaults, the name of a bundled
  preset such as `"github"`, or an existing `markdown_style` to extend.

- css:

  A stylesheet: CSS text, or a path to a `.css` file. Read as a file
  when it names one.

- ...:

  Named tag overrides, each an
  [`md_style`](https://adayim.github.io/gridmicrotex/reference/md_style.md).
  Use a leading dot for a class, e.g. `.note = md_style(...)`.

## Value

An object of class `gridmicrotex_markdown_style`.

## Details

Tags are named as in HTML, so a stylesheet reads the way a CSS author
expects: `body` (the document root, which every other tag inherits from
— and which also styles the box itself, see
[`md_style`](https://adayim.github.io/gridmicrotex/reference/md_style.md)),
`p`, `h1` ... `h6`, `ul`, `ol`, `li`, `blockquote`, `pre` (a code
block), `code` (an inline code span), `strong` and `em` (what markdown's
`**` and `*` produce), `table`, `tr`, `td`, `th`, `hr`, `img`, `a` (a
link), `math` (a paragraph that is nothing but `$$...$$`), `footnote`,
`div` and `span`.

The table tags nest as they do in HTML: `tr`, `td` and `th` inherit
through `table`, so `table { color: }` reaches the cells. `background`
on `tr` fills the row, on `td`/`th` the individual cell.

Four things can style a document, and they resolve in CSS's own order:
the built-in defaults, then a type selector (`h1`), then a class
selector (`.note`), then an inline `style` attribute. Ties are broken by
document order, so a later rule wins. Inheritable properties (`color`,
the `font-*` family, `line-height`, `text-align`) fall through into
nested containers, so `blockquote { color: grey40 }` greys everything
quoted; box properties (margins, padding, borders) do not.

The supported CSS is a deliberately small subset: type selectors, class
selectors and selector lists (`h1, h2`). Combinators, pseudo-classes,
attribute selectors, `#id` and at-rules are skipped rather than raised,
so an existing stylesheet can be handed over and the parts that apply
still take effect.

## See also

[`md_style`](https://adayim.github.io/gridmicrotex/reference/md_style.md),
[`markdown_box_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md)

## Examples

``` r
markdown_style()
#> <markdown_style> 19 rules, 19 selectors
#>   p            margin-top: 0.55
#>   math         margin-top: 0.55; text-align: center
#>   a            color: #0969DA; text-decoration: underline
#>   footnote     margin-top: 0.3; font-size: 0.85
#>   blockquote   margin-top: 0.55; padding-left: 0.75; border-left: 0.125
#>   pre          margin-top: 0.55; line-height: 1.15; font-family: mono
#>   table        margin-top: 0.55
#>   th           font-weight: bold
#>   img          margin-top: 0.55
#>   hr           margin-top: 0.55; height: 0.5
#>   ul           margin-top: 0.55; marker-gap: 0.4; bullet: \bullet
#>   ol           margin-top: 0.55; marker-gap: 0.4
#>   li           margin-top: 0.55
#>   h1           margin-top: 0.95; font-size: 2.5; font-weight: bold
#>   h2           margin-top: 0.95; font-size: 2; font-weight: bold
#>   h3           margin-top: 0.95; font-size: 1.75; font-weight: bold
#>   h4           margin-top: 0.95; font-size: 1.416667; font-weight: bold
#>   h5           margin-top: 0.95; font-size: 1.166667; font-weight: bold
#>   h6           margin-top: 0.95; font-size: 1; font-weight: bold
markdown_style("github", h1 = md_style(color = "firebrick"))
#> <markdown_style> 46 rules, 21 selectors
#>   p            margin-top: 0.8rem
#>   math         margin-top: 1rem; text-align: center
#>   a            color: #0969DA; text-decoration: underline
#>   footnote     margin-top: 0.3; font-size: 0.85rem; color: #59636E
#>   blockquote   margin-top: 0.8rem; padding-left: 1rem; border-left: 0.25rem solid #D1D9E0; color: #59636E
#>   pre          margin-top: 0.8rem; line-height: 1.45; font-family: monospace; background: #F6F8FA; padding-left: 0.6rem; padding-right: 0.6rem; padding-top: 0.5rem; padding-bottom: 0.5rem
#>   table        margin-top: 0.8rem; border-color: #D1D9E0
#>   th           font-weight: bold; background: #F6F8FA
#>   img          margin-top: 0.55
#>   hr           margin-top: 1.5rem; height: 1rem; border-top: 0.25rem solid #D1D9E0
#>   ul           margin-top: 0.8rem; marker-gap: 0.5rem; bullet: \bullet
#>   ol           margin-top: 0.8rem; marker-gap: 0.5rem
#>   li           margin-top: 0.25rem
#>   h1           margin-top: 1.2rem; font-size: 2rem; font-weight: bold; color: firebrick
#>   h2           margin-top: 1.2rem; font-size: 1.5rem; font-weight: bold
#>   h3           margin-top: 1.2rem; font-size: 1.25rem; font-weight: bold
#>   h4           margin-top: 1.2rem; font-size: 1rem; font-weight: bold
#>   h5           margin-top: 1.2rem; font-size: 0.875rem; font-weight: bold
#>   h6           margin-top: 1.2rem; font-size: 0.85rem; font-weight: bold; color: #59636E
#>   tr           border-bottom: 1px solid #D1D9E0
#>   td           padding-left: 0.6rem
markdown_style(css = "h1 { color: steelblue } .note { padding-left: 2em }")
#> <markdown_style> 21 rules, 20 selectors
#>   p            margin-top: 0.55
#>   math         margin-top: 0.55; text-align: center
#>   a            color: #0969DA; text-decoration: underline
#>   footnote     margin-top: 0.3; font-size: 0.85
#>   blockquote   margin-top: 0.55; padding-left: 0.75; border-left: 0.125
#>   pre          margin-top: 0.55; line-height: 1.15; font-family: mono
#>   table        margin-top: 0.55
#>   th           font-weight: bold
#>   img          margin-top: 0.55
#>   hr           margin-top: 0.55; height: 0.5
#>   ul           margin-top: 0.55; marker-gap: 0.4; bullet: \bullet
#>   ol           margin-top: 0.55; marker-gap: 0.4
#>   li           margin-top: 0.55
#>   h1           margin-top: 0.95; font-size: 2.5; font-weight: bold; color: steelblue
#>   h2           margin-top: 0.95; font-size: 2; font-weight: bold
#>   h3           margin-top: 0.95; font-size: 1.75; font-weight: bold
#>   h4           margin-top: 0.95; font-size: 1.416667; font-weight: bold
#>   h5           margin-top: 0.95; font-size: 1.166667; font-weight: bold
#>   h6           margin-top: 0.95; font-size: 1; font-weight: bold
#>   .note        padding-left: 2em
```
