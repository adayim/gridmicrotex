# Create a grid grob from a LaTeX expression

Parses a LaTeX math expression and returns a grid grob object that
renders the formula using native grid graphics primitives. The grob
supports standard grid queries such as
[`grobWidth()`](https://rdrr.io/r/grid/grobWidth.html),
[`grobHeight()`](https://rdrr.io/r/grid/grobWidth.html),
[`grobX()`](https://rdrr.io/r/grid/grobX.html), and
[`grobY()`](https://rdrr.io/r/grid/grobX.html).

A convenience wrapper that creates a `latex_grob` and immediately draws
it on the current device via
[`grid.draw`](https://rdrr.io/r/grid/grid.draw.html).

## Usage

``` r
latex_grob(
  tex,
  x = grid::unit(0.5, "npc"),
  y = grid::unit(0.5, "npc"),
  default.units = "npc",
  hjust = 0.5,
  vjust = 0.5,
  rot = 0,
  math_font = "",
  max_width = 0,
  tex_style = "",
  input_mode = c("mixed", "math"),
  render_mode = c("typeface", "path"),
  debug = FALSE,
  name = NULL,
  gp = grid::gpar()
)

grid.latex(tex, ...)
```

## Arguments

- tex:

  Character string of LaTeX math code.

- x, y:

  Position in grid coordinates.

- default.units:

  Units for x, y if given as numeric.

- hjust, vjust:

  Horizontal/vertical justification. Accepts the usual numeric values in
  `[0, 1]`. As a convenience, `hjust` also accepts the strings
  `"left"`/`"bbleft"`, `"center"`/`"centre"`/ `"middle"`/`"bbcentre"`,
  and `"right"`/`"bbright"`; `vjust` accepts `"bottom"`,
  `"center"`/`"centre"`/`"middle"`, `"top"`, and `"baseline"`.
  `"baseline"` aligns the formula's math baseline with the anchor point
  — handy for placing a formula in flowing text.

- rot:

  Rotation angle in degrees, counter-clockwise (default: 0). Matches the
  `rot` parameter of
  [`textGrob`](https://rdrr.io/r/grid/grid.text.html).

- math_font:

  Name of the math font to use (e.g., `"stix"`). Use `""` (default) for
  Lete Sans Math, which pairs with R's default sans-serif text font. See
  [`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md)
  for loaded fonts.

- max_width:

  Numeric maximum width in big points for automatic line wrapping. Use
  `0` (default) for no wrapping.

- tex_style:

  Character: TeX style override. One of `""` (default; let the parser
  decide), `"display"`, `"text"`, `"script"`, or `"scriptscript"`. See
  `latex_grob` for the semantics of each value.

- input_mode:

  How `tex` is interpreted before being parsed. `"mixed"` wraps the
  input in `\text{...}` so the string reads as ordinary text and `$...$`
  (or `\(...\)`) opens math mode, matching document-level LaTeX
  semantics. Useful for labels that arrive from external sources mixing
  prose and math without explicit `\text{}` markers. `"math"` (default)
  is the standard MicroTeX behaviour — the whole string is treated as
  math, so unwrapped prose renders as spaced math italics. The default
  can be changed globally via
  [`latex_options`](https://adayim.github.io/gridmicrotex/reference/latex_options.md)`(input_mode = "mixed")`.
  See
  [`latex_wrap`](https://adayim.github.io/gridmicrotex/reference/latex_wrap.md)
  for details on the wrapping process.

- render_mode:

  Character string: `"typeface"` (default) renders glyphs as native text
  using the math font, producing selectable/accessible text in PDF and
  SVG output. Bundled math fonts and any registered via
  [`load_font`](https://adayim.github.io/gridmicrotex/reference/load_font.md)
  are read directly from their OTF files — no system-wide font install
  is required. Falls back to path mode automatically on devices that
  lack the R \\\geq\\ 4.3 glyph engine (e.g., the base
  [`pdf()`](https://rdrr.io/r/grDevices/pdf.html) device). For
  selectable PDF output, prefer
  [`cairo_pdf`](https://rdrr.io/r/grDevices/cairo.html). `"path"`
  renders math symbols as filled vector paths (works on all devices but
  text is not selectable in PDF/SVG).

- debug:

  Logical; if `TRUE`, draws diagnostic overlays on the grob — the full
  bounding box (dashed gray), the baseline (solid red), the depth line
  (dashed gray), and a small dot at each MicroTeX draw record's origin.
  Useful for checking positioning and diagnosing vertical alignment.

- name:

  Optional grob name.

- gp:

  Graphical parameters (see [`gpar`](https://rdrr.io/r/grid/gpar.html)).
  Common entries: `col` (formula foreground), `fontfamily` / `fontface`
  (text font), `fontsize` / `cex` (formula size), and `lineheight`
  (multi-line spacing). See `latex_grob` for how each of these flows
  through MicroTeX.

- ...:

  Additional arguments passed to `latex_grob`.

## Value

A `grid` grob of class `"latexgrob"`.

Invisibly returns the grob.

## Details

### Controlling TeX style with `tex_style`

`tex_style` selects the size-and-spacing regime MicroTeX applies to the
whole expression. It changes the *style* (display vs. text), not the
font size — size is always set via `gp$fontsize` / `gp$cex`;
style-dependent shrinking (for `"script"` and `"scriptscript"`) is
applied on top of that size.

- `""` (default): let the parser choose based on the delimiters in
  `tex`. Inline delimiters (single `$`, or `\(...\)`) produce `"text"`
  style; display delimiters (double `$$`, or `\[...\]`) produce
  `"display"` style. If the string has no delimiters, MicroTeX defaults
  to `"text"` style.

- `"display"`: force display style. Large operators (`\sum`, `\int`,
  `\prod`) render at their full size, limits are placed above/below
  rather than as subscripts/superscripts, and fractions use full-size
  numerators and denominators. Useful when you want a display-style
  equation inline in a label, legend, or
  [`element_latex()`](https://adayim.github.io/gridmicrotex/reference/element_latex.md)
  title.

- `"text"`: force text (inline) style. Big operators shrink to their
  inline size and limits attach as scripts. The right choice for
  formulas embedded in a line of prose.

- `"script"`: force script style — the size normally used for
  first-level subscripts and superscripts. Produces a smaller, tighter
  layout; mainly useful for callouts or sub-labels where a compact
  equation is wanted.

- `"scriptscript"`: force scriptscript style — the smallest style, used
  by TeX for doubly-nested scripts. Rarely needed on its own; primarily
  for very dense annotations.

`tex_style` applies to the entire expression. To override the style of a
sub-expression from within `tex`, use the inline TeX commands
`\displaystyle`, `\textstyle`, `\scriptstyle`, or `\scriptscriptstyle`.

### Graphical parameters (`gp`)

- `col`: default foreground color for the formula. Individual elements
  can still be overridden with an inline `\textcolor` command in the
  LaTeX string.

- `fontfamily` / `fontface`: control the appearance of text inside
  `\text` and `\mbox` blocks. For example, `gpar(fontfamily = "serif")`
  renders `\text` content in R's serif family. Any font available to R's
  graphics system works — base families (`"sans"`, `"serif"`, `"mono"`)
  as well as fonts registered via showtext or systemfonts. Math symbols
  always use the selected math font (see `math_font`).

  `fontfamily` *also* drives MicroTeX's layout metrics for non-math
  text: the matching system font is resolved via systemfonts, a minimal
  metrics file is generated on first use and cached under
  `tools::R_user_dir("gridmicrotex", "cache")`, so MicroTeX's spacing of
  `\text` blocks stays in sync with what grid actually draws. When
  `fontfamily` is unset, the R default (`"sans"`) is used. No manual
  font loading is required for text fonts;
  [`load_font()`](https://adayim.github.io/gridmicrotex/reference/load_font.md)
  remains only for adding custom **math** fonts.

- `fontsize` / `cex`: formula size is `fontsize * cex` big points
  (default 20 \* 1). Both math and text scale together. The effective
  size is baked into the parsed layout, so downstream viewports that
  inherit `cex` will not re-scale the grob (matching `textGrob`
  semantics when `gp` is set explicitly).

- `lineheight`: controls multi-line spacing (default 1.2). The
  inter-line gap is `(lineheight - 1) * fontsize` big points.

## See also

`grid.latex`,
[`latex_dims`](https://adayim.github.io/gridmicrotex/reference/latex_dims.md),
[`geom_latex`](https://adayim.github.io/gridmicrotex/reference/geom_latex.md),
[`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md),
[`latex_wrap`](https://adayim.github.io/gridmicrotex/reference/latex_wrap.md),
[`latex_options`](https://adayim.github.io/gridmicrotex/reference/latex_options.md)

## Examples

``` r
# \donttest{
  g <- latex_grob(r"($\fcolorbox{red}{yellow}{\frac{a}{b}}$)",
                  x = grid::unit(0.3, "npc"),
                  y = grid::unit(0.3, "npc"),
                  gp = grid::gpar(fontsize = 30))
  grid::grid.draw(g)
  # Red formula
  grid::grid.draw(latex_grob("$x^{2}$",
                             x = grid::unit(0.3, "npc"),
                             y = grid::unit(0.8, "npc"),
                             gp = grid::gpar(col = "red")))

                             # Rotated formula
  grid::grid.draw(latex_grob(r"($\colorbox{BurntOrange}{x^{2}} + y^{2}$)",
                             x = grid::unit(0.6, "npc"),
                             y = grid::unit(0.3, "npc"),
                             gp = grid::gpar(fontsize = 24),
                             rot = 45))

  grid.latex(r"($\textcolor{red}{x^{2}} + y^{2} = z^{2}$)",
             x = grid::unit(0.6, "npc"),
             y = grid::unit(0.8, "npc"),)

# }
```
