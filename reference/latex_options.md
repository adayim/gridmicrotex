# Set or query package-wide LaTeX rendering defaults

A single entry point for project-wide defaults used by
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md),
[`grid.latex`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md),
[`latex_dims`](https://adayim.github.io/gridmicrotex/reference/latex_dims.md),
and
[`latex_tree`](https://adayim.github.io/gridmicrotex/reference/latex_tree.md).
Options set here are applied only when the corresponding argument is
*not* supplied at the call site, so explicit arguments always win.

## Usage

``` r
latex_options(
  math_font = NULL,
  render_mode = NULL,
  tex_style = NULL,
  input_mode = NULL,
  justify = NULL,
  line_break = NULL,
  markdown_style = NULL
)

reset_latex_options()
```

## Arguments

- math_font:

  Math font name or alias (see
  [`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md)).

- render_mode:

  Either `"typeface"` or `"path"`.

- tex_style:

  TeX style override. One of `""` (let the parser decide), `"display"`,
  `"text"`, `"script"`, or `"scriptscript"`. `"display"` forces large
  operators with limits placed over/under, useful for inline labels that
  should still look like display equations.

- input_mode:

  How the input string is interpreted before being handed to MicroTeX.
  `"mixed"` (default) wraps the string in `\text{...}` so it reads as
  ordinary text, with `$...$` (and `\(...\)`) opening math mode — the
  document-level LaTeX convention. Useful when consuming labels from
  other packages that mix prose and math without explicit `\text{}`
  markers. `"math"` treats the whole string as math — the classic
  MicroTeX behaviour, where letters render as math italics and unwrapped
  prose looks wrong.

- justify:

  Logical. When `TRUE`, wrapped text is stretched at its interword
  spaces so every line but the last fills `max_width` exactly. Has no
  effect without `max_width`, since it acts on the lines the wrapper
  produces. `FALSE` (default) leaves the right edge ragged, matching R's
  own text drawing. In a narrow column, justifying alone opens
  noticeably wide word spaces; mark the words that may break with `\-`.

- line_break:

  How lines are chosen when wrapping. `"greedy"` (default) fills each
  line as far as it will go and never reconsiders. `"optimal"` chooses
  the breaks together so the paragraph as a whole reads best, in the
  spirit of Knuth-Plass: pulling one word down early can improve every
  later line, which a greedy pass cannot see. Requires `max_width`, and
  costs a little more layout time.

- markdown_style:

  Default style for
  [`markdown_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_grob.md)
  and
  [`markdown_box_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md):
  a
  [`markdown_style`](https://adayim.github.io/gridmicrotex/reference/markdown_style.md)
  object, CSS text, or a path to a `.css` file.

## Value

Invisibly returns the previous settings (a list). With no arguments,
returns the current settings visibly.

## Details

Calling `latex_options()` with no arguments returns the current settings
(a list whose `NULL` entries mean "use the built-in default"). Supply
one or more named arguments to update them.

Font size and line spacing are controlled via `gp` parameters
(`fontsize`, `cex`, `lineheight`) at the grob level — see
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md).

## See also

[`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md),
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)

## Examples

``` r
# \donttest{
  latex_options(math_font = "stix", render_mode = "typeface")
  grid.latex("\\sum_{i=1}^{n} i^{2}", gp = grid::gpar(fontsize = 14))

  reset_latex_options()
# }
```
