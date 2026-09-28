# A style for markdown rendering

Creates a style for
[`markdown_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_grob.md),
[`markdown_box_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md)
and the ggplot2 markdown functions, from CSS, a preset, or
[`md_style()`](https://adayim.github.io/gridmicrotex/reference/md_style.md)
declarations.

## Usage

``` r
markdown_style(base = NULL, css = NULL, ...)
```

## Arguments

- base:

  `NULL` (default) for the built-in style, a preset name such as
  `"github"`, or a `markdown_style` to extend.

- css:

  CSS text, or a path to a `.css` file.

- ...:

  Tag styles, each an
  [`md_style()`](https://adayim.github.io/gridmicrotex/reference/md_style.md).
  Start a name with a dot for a class, as in `.note = md_style(...)`.

## Value

A style object of class `"gridmicrotex_markdown_style"`.

## Details

Tags are named as in HTML: `body`, `p`, `h1` to `h6`, `ul`, `ol`, `li`,
`blockquote`, `pre` (code block), `code` (inline code), `strong`, `em`,
`table`, `tr`, `td`, `th`, `hr`, `img`, `a` (link), `math` (a `$$...$$`
paragraph), `footnote`, `div` and `span`. Every tag inherits from
`body`, and table cells inherit from `table`.

As in CSS, an inline `style` beats a class (`.note`), which beats a tag
(`h1`), and a later rule beats an earlier one. Colour, font, line height
and alignment are inherited by nested blocks; margins, padding and
borders are not.

Tag selectors, class selectors and lists (`h1, h2`) are supported. Other
selectors are ignored.

## See also

[`md_style`](https://adayim.github.io/gridmicrotex/reference/md_style.md),
[`markdown_box_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md)

## Examples

``` r
markdown_style()
#> <markdown_style> 36 rules, 36 selectors
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
#>   .co          color: #59636E
#>   .ot          color: #59636E
#>   .st          color: #0A3069
#>   .ch          color: #0A3069
#>   .kw          color: #CF222E
#>   .cf          color: #CF222E
#>   .pp          color: #CF222E
#>   .cn          color: #0550AE
#>   .dv          color: #0550AE
#>   .bn          color: #0550AE
#>   .fl          color: #0550AE
#>   .at          color: #0550AE
#>   .sc          color: #0550AE
#>   .fu          color: #8250DF
#>   .bu          color: #8250DF
#>   .dt          color: #953800
#>   .va          color: #953800
markdown_style("github", h1 = md_style(color = "firebrick"))
#> <markdown_style> 80 rules, 38 selectors
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
#>   .co          color: #59636E
#>   .ot          color: #59636E
#>   .st          color: #0A3069
#>   .ch          color: #0A3069
#>   .kw          color: #CF222E
#>   .cf          color: #CF222E
#>   .pp          color: #CF222E
#>   .cn          color: #0550AE
#>   .dv          color: #0550AE
#>   .bn          color: #0550AE
#>   .fl          color: #0550AE
#>   .at          color: #0550AE
#>   .sc          color: #0550AE
#>   .fu          color: #8250DF
#>   .bu          color: #8250DF
#>   .dt          color: #953800
#>   .va          color: #953800
#>   tr           border-bottom: 1px solid #D1D9E0
#>   td           padding-left: 0.6rem
markdown_style(css = "h1 { color: steelblue } .note { padding-left: 2em }")
#> <markdown_style> 38 rules, 37 selectors
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
#>   .co          color: #59636E
#>   .ot          color: #59636E
#>   .st          color: #0A3069
#>   .ch          color: #0A3069
#>   .kw          color: #CF222E
#>   .cf          color: #CF222E
#>   .pp          color: #CF222E
#>   .cn          color: #0550AE
#>   .dv          color: #0550AE
#>   .bn          color: #0550AE
#>   .fl          color: #0550AE
#>   .at          color: #0550AE
#>   .sc          color: #0550AE
#>   .fu          color: #8250DF
#>   .bu          color: #8250DF
#>   .dt          color: #953800
#>   .va          color: #953800
#>   .note        padding-left: 2em
```
