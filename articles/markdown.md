# Rendering Markdown with Math

gridmicrotex renders [CommonMark](https://commonmark.org) markdown with
LaTeX math between `$...$`. There are two functions:

- [`markdown_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_grob.md)
  (and
  [`grid.markdown()`](https://adayim.github.io/gridmicrotex/reference/markdown_grob.md))
  for a single label, such as a title.
- [`markdown_box_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md)
  for a document with headings, lists, quotes, code and tables.

Fonts, options and LaTeX support are the same as for
[`latex_grob()`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)
(see
[`vignette("getting-started")`](https://adayim.github.io/gridmicrotex/articles/getting-started.md)).

## Inline markdown

``` r

grid.newpage()
grid.markdown(
  r"(The **fitted** $\hat{\beta} = (X^\top X)^{-1} X^\top y$ has ~~no~~ `se` = *0.42*.)",
  x = 0.02, hjust = 0, gp = gpar(fontsize = 13)
)
```

![](markdown_files/figure-html/inline-1.png)

### Inline HTML

For colour, underline, sub- and superscripts, highlight and size, use
inline HTML:

| tag | effect |
|----|----|
| `<b>`, `<strong>` | bold |
| `<i>`, `<em>`, `<cite>`, `<dfn>`, `<var>` | italic |
| `<code>`, `<kbd>`, `<samp>`, `<tt>` | monospace |
| `<u>`, `<ins>` | underline |
| `<s>`, `<del>`, `<strike>` | strikethrough |
| `<sub>`, `<sup>` | sub / superscript |
| `<mark>` | yellow highlight |
| `<small>`, `<big>` | smaller / larger |
| `<q>` | quotation marks |
| `<ruby>`, `<rt>` | annotation above the text (furigana) |
| `&nbsp;` | non-breaking space |
| `<br>` | line break |
| `<span style="…">` | `color`, `font-size`, `font-family`, `text-decoration`, and the rest of [`?md_style`](https://adayim.github.io/gridmicrotex/reference/md_style.md) |

``` r

grid.newpage()
grid.markdown(
  r"(<span style="font-size:22pt;color:#2E5E8E">Model fit</span>,
     <span style="font-family:serif">serif</span> and
     <span style="font-family:monospace">mono</span>.<br><br>
     The <span style="color:#B22222">**residual**</span> for H<sub>0</sub>
     is <u>within</u> <mark>$2\sigma$</mark>.)",
  x = 0.02, y = 0.9, hjust = 0, vjust = 1, gp = gpar(fontsize = 13)
)
```

![](markdown_files/figure-html/html-css-1.png)

Tags nest, and markdown and math work inside them. Other tags are
dropped and their text kept.

## Documents

[`markdown_box_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md)
stacks the blocks of a document inside an optional box. Text wraps to
`width`.

``` r

md <- r"(
# Model summary

The slope is $\beta_1$ with *p* < 0.001, and this paragraph is long
enough that it wraps inside the column.

## Diagnostics

- residuals look **fine**
- $R^2 = 0.87$
- no influential points

~~~r
fit <- lm(y ~ x, data = d)  # refit
if (anyNA(d)) {
    stop("missing values")
}
~~~

> Assumptions were checked and hold.
)"

grid.newpage()
grid.draw(markdown_box_grob(
  md,
  width   = unit(4.6, "in"),
  padding = unit(10, "pt"),
  box_gp  = gpar(fill = "grey97", col = "grey40"),
  gp      = gpar(fontsize = 13),
  style = markdown_style(css = "pre { background: #ece2f0 }") # Code background
))
```

![](markdown_files/figure-html/block-1.png)

`box_gp = NULL` draws no box, and `r` rounds its corners. `padding` and
`margin` take one unit, or four for top, right, bottom and left.

### Code

A fenced code block that names its language is highlighted.
[`available_highlighters()`](https://adayim.github.io/gridmicrotex/reference/available_highlighters.md)
lists the languages;
[`register_highlighter()`](https://adayim.github.io/gridmicrotex/reference/register_highlighter.md)
adds one from a [KDE syntax file](https://kate-editor.org/syntax/).
Colours use the same class names as Pandoc and knitr, so a Pandoc theme
can be pasted in:

``` r

markdown_style(css = ".co { color: #59636E } .kw { color: #CF222E }")
```

### Lists and tables

Task lists and tables work, with column alignment from the `|:---:|`
markers:

``` r

md <- r"(
### Checklist

- [x] fit the model
- [ ] write it up

Coefficients:

| term | $\beta$  | *p* |
|:-----|------------:|:---:|
| intercept | 30.1 | *** |
| slope | -5.3 | *** |
)"

grid.newpage()
grid.draw(markdown_box_grob(
  md,
  width   = unit(4.4, "in"),
  padding = unit(8, "pt"),
  gp      = gpar(fontsize = 13)
))
```

![](markdown_files/figure-html/lists-tables-1.png)

### Images

An image on its own line is a block, scaled to fit; one inside a
sentence is inline. Images must be local PNG, JPEG or SVG files.

``` r

img <- system.file("img", "Rlogo.png", package = "png")

md <- sprintf("
A figure with a caption below it.

![the R logo](%s)

The caption explains the figure.
", img)

grid.newpage()
grid.draw(markdown_box_grob(
  md, width = unit(4, "in"), padding = unit(8, "pt"),
  gp = gpar(fontsize = 13)
))
```

![](markdown_files/figure-html/image-1.png)

## Styling

`style` takes CSS, as text or a `.css` file:

``` r

doc <- r"(
# Results

The slope is $\beta_1$ with *p* < 0.001.

> Worth a second look.
)"

grid.newpage()
grid.draw(markdown_box_grob(
  doc,
  width = unit(4.5, "in"), padding = unit(10, "pt"),
  style = "
    h1         { color: steelblue; font-size: 1.8rem }
    blockquote { color: grey40; border-left: 3px solid steelblue }
  ",
  gp = gpar(fontsize = 13)
))
```

![](markdown_files/figure-html/style-css-1.png)

Or the same in R:

``` r

markdown_style(
  h1         = md_style(color = "steelblue", font_size = 1.8),
  blockquote = md_style(color = "grey40",
                        border_left = "3px solid steelblue")
)
```

A bare number is a multiple of the body font size (`rem`). Tags are
named as in HTML (`p`, `h1`, `li`, `blockquote`, `pre`, `code`, `table`,
…).
[`?md_style`](https://adayim.github.io/gridmicrotex/reference/md_style.md)
lists the supported properties; others are ignored.

`markdown_style("github")` starts from a GitHub-like preset, and
`latex_options(markdown_style = )` sets a default for the session.

### Styling one part

Wrap a part in a `<div>` with a `class` or `style`, leaving a blank line
after the opening tag and before the closing one:

``` r

doc <- '
Ordinary text.

<div class="note">

**Note.** This chunk is indented and set apart.

</div>

Ordinary text again.
'

grid.newpage()
grid.draw(markdown_box_grob(
  doc,
  width = unit(4.5, "in"), padding = unit(10, "pt"),
  style = ".note { color: grey35; padding-left: 1.5rem }",
  gp = gpar(fontsize = 13)
))
```

![](markdown_files/figure-html/style-div-1.png)

`<span class="...">` does the same inside a line.

### Styling tables

| CSS                             | effect                  |
|---------------------------------|-------------------------|
| `table { border-color }`        | colour of every rule    |
| `tr { background }`             | fills a row             |
| `td`, `th` `{ background }`     | fills a cell            |
| `tr { border-bottom }`          | a rule under each row   |
| `td { border-left }`            | rules between columns   |
| `td { padding-left }`           | the gap between columns |
| `table { table-layout: fixed }` | columns of equal width  |

A table that is too wide for the box wraps its widest columns.

``` r

tbl <- r"(
| Term | Meaning |
|:-----|:--------|
| $\beta_1$ | the slope, which needs a good deal of room to explain |
| $\sigma$ | the residual standard deviation |
)"

grid.newpage()
grid.draw(markdown_box_grob(
  tbl,
  width = unit(4, "in"), padding = unit(8, "pt"),
  style = "
    table { table-layout: fixed; border-color: #D1D9E0 }
    th    { background: #F6F8FA }
    tr    { border-bottom: 1px solid #D1D9E0 }
  ",
  gp = gpar(fontsize = 13)
))
```

![](markdown_files/figure-html/style-table-1.png)

## LaTeX inside markdown

For what markdown cannot do, such as merged or coloured table cells,
write LaTeX in a `$$...$$` block:

``` r

tbl <- r"($$\begin{array}{|l|c|c|}\hline
\rowcolor{#F6F8FA}\multicolumn{3}{|c|}{\textbf{Model comparison}}\\\hline
\textbf{Model}&\textbf{AIC}&\textbf{R}^2\\\hline
\text{linear}&214.3&0.71\\
\cellcolor{#DDF0DD}\text{quadratic}&\cellcolor{#DDF0DD}201.8&\cellcolor{#DDF0DD}0.87\\\hline
\end{array}$$)"

grid.newpage()
grid.draw(markdown_box_grob(
  paste0("Compare the two fits.\n\n", tbl, "\n\nThe quadratic wins."),
  width = unit(4.6, "in"), padding = unit(10, "pt"),
  style = "math { color: #1F3864; font-size: 1.1rem }",
  gp = gpar(fontsize = 13)
))
```

![](markdown_files/figure-html/latex-escape-1.png)

Inside `\textbf{}` you are in text, so write `\textbf{R}^2`, not
`\textbf{R^2}`. CSS table rules do not apply to a LaTeX table; style it
in LaTeX.

## Limitations

- Links show their text only.
- Footnotes are placed at the bottom in
  [`markdown_box_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md);
  in
  [`markdown_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_grob.md)
  only the marker is shown.
- HTML blocks other than `<div>` and `<img>` are dropped, `<table>`
  included. Use markdown tables.
- Markdown tables cannot merge cells; use LaTeX (above).
- Small caps are not supported.
- The syntax class names (`co`, `st`, `kw`, `dt`, …) also apply to your
  own `<div class>`; choose other names for your classes.
- Code is a little narrow on `png(type = "cairo")` and `svglite`; use
  [`ragg::agg_png()`](https://ragg.r-lib.org/reference/agg_png.html) if
  it matters.
