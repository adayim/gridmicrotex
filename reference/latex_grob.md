# Create a grid grob from a LaTeX expression

Reads LaTeX – a formula, a label mixing text and math, or a document
body – and returns a grid grob object that draws it with native grid
graphics primitives. The grob supports standard grid queries such as
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
  input_mode = c("mixed", "math", "document"),
  render_mode = c("typeface", "path"),
  justify = FALSE,
  line_break = c("greedy", "optimal"),
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

  How `tex` is read. `"mixed"` (default) reads a label: text, as in a
  LaTeX paragraph, with math between `$...$` or `\(...\)`, and a newline
  starts a new line, as `"\n"` does in R. `"math"` reads the whole
  string as math, as between `$...$`, so a word is set as italic
  letters; write text as `\text{...}`. `"document"` reads a LaTeX
  document body by LaTeX's own rules: a newline is a space, a blank line
  (or `\par`) starts an indented paragraph, `\section` and its kin are
  numbered headings, and display math is centred on a line of its own.
  Give `max_width` to break the paragraphs into lines. The default can
  be set for the session with
  [`latex_options`](https://adayim.github.io/gridmicrotex/reference/latex_options.md)`(input_mode = )`.

- render_mode:

  Character string: `"typeface"` (default) renders glyphs as native text
  using the math font, producing selectable/accessible text in PDF and
  SVG output. Bundled math fonts and any registered via
  [`load_math_font`](https://adayim.github.io/gridmicrotex/reference/load_math_font.md)
  are read directly from their OTF files: no system-wide font install is
  required. Falls back to path mode automatically on devices that lack
  the R \\\geq\\ 4.3 glyph engine, and on base
  [`pdf()`](https://rdrr.io/r/grDevices/pdf.html) and
  [`postscript()`](https://rdrr.io/r/grDevices/postscript.html), which
  cannot embed the math font. For selectable PDF output, prefer
  [`cairo_pdf`](https://rdrr.io/r/grDevices/cairo.html). `"path"`
  renders math symbols as filled vector paths (works on all devices but
  text is not selectable in PDF/SVG).

- justify:

  Logical. When `TRUE`, wrapped text is stretched at its interword
  spaces so every line but the last fills `max_width` exactly. Requires
  `max_width`: it acts on the lines the wrapper produces, so it does
  nothing on its own. `FALSE` (default) leaves the right edge ragged,
  matching R's own text drawing. In a narrow column, expect wide word
  spaces; mark the words that may break with `\-` to tighten them.

- line_break:

  How lines are chosen when wrapping. `"greedy"` (default) fills each
  line as far as it will go and never reconsiders. `"optimal"` chooses
  the breaks together so the paragraph as a whole reads best, in the
  spirit of Knuth-Plass: pulling one word down early can improve every
  later line, which a greedy pass cannot see. Requires `max_width`, and
  costs a little more layout time.

- debug:

  Logical; if `TRUE`, draws diagnostic overlays on the grob: the full
  bounding box (dashed gray), the baseline (solid red), the depth line
  (dashed gray), and a small dot at each MicroTeX draw record's origin.
  Useful for checking positioning and diagnosing vertical alignment.

- name:

  Optional grob name.

- gp:

  Graphical parameters (see [`gpar`](https://rdrr.io/r/grid/gpar.html)).
  Common entries: `col` (formula foreground), `fontfamily` (text font),
  `fontsize` / `cex` (formula size), and `lineheight` (multi-line
  spacing). See `latex_grob` for how each of these flows through
  MicroTeX.

- ...:

  Additional arguments passed to `latex_grob`.

## Value

A `grid` grob of class `"latexgrob"`.

Invisibly returns the grob.

## Details

### Controlling TeX style with `tex_style`

`tex_style` selects the size-and-spacing regime MicroTeX applies to the
whole expression. It changes the *style* (display vs. text), not the
font size: size is always set via `gp$fontsize` / `gp$cex`;
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

- `"script"`: force script style, the size normally used for first-level
  subscripts and superscripts. Produces a smaller, tighter layout;
  mainly useful for callouts or sub-labels where a compact equation is
  wanted.

- `"scriptscript"`: force scriptscript style, the smallest style, used
  by TeX for doubly-nested scripts. Rarely needed on its own; primarily
  for very dense annotations.

`tex_style` applies to the entire expression. To override the style of a
sub-expression from within `tex`, use the inline TeX commands
`\displaystyle`, `\textstyle`, `\scriptstyle`, or `\scriptscriptstyle`.

### Graphical parameters (`gp`)

- `col`: default foreground color for the formula. Individual elements
  can still be overridden with an inline `\textcolor` command in the
  LaTeX string.

- `fontfamily`: controls the font of text inside `\text` and `\mbox`
  blocks. For example, `gpar(fontfamily = "serif")` renders `\text`
  content in R's serif family. Any font available to R's graphics system
  works: base families (`"sans"`, `"serif"`, `"mono"`) as well as fonts
  registered via showtext or systemfonts. Math symbols always use the
  selected math font (see `math_font`). Bold/italic text is controlled
  from within the LaTeX source (`\textbf{}`, `\textit{}`, `\bf`, ...),
  not via `gp$fontface`: MicroTeX needs the style at layout time to size
  each run correctly, so a
  [`gpar()`](https://rdrr.io/r/grid/gpar.html)-level face is not
  consulted.

  `fontfamily` *also* drives MicroTeX's layout metrics for non-math
  text: the matching system font is resolved via systemfonts, a minimal
  metrics file is generated on first use and cached under
  `tools::R_user_dir("gridmicrotex", "cache")`, so MicroTeX's spacing of
  `\text` blocks stays in sync with what grid actually draws. When
  `fontfamily` is unset, the R default (`"sans"`) is used. No manual
  font loading is required for text fonts;
  [`load_math_font()`](https://adayim.github.io/gridmicrotex/reference/load_math_font.md)
  remains only for adding custom **math** fonts.

- `fontsize` / `cex`: formula size is `fontsize * cex` big points
  (default 20 \* 1). Both math and text scale together. The effective
  size is baked into the parsed layout, so downstream viewports that
  inherit `cex` will not re-scale the grob (matching `textGrob`
  semantics when `gp` is set explicitly).

- `lineheight`: controls multi-line spacing (default 1.2). The
  inter-line gap is `(lineheight - 1) * fontsize` big points.

### Malformed input

LaTeX that TeX would stop on is read as far as it can be and the rest is
drawn, with one warning listing each problem at its line and column in
`tex`: an unknown command (drawn as its name, in red), an unbalanced
brace, `&` outside an alignment, `_` or `^` outside math, and so on.
Only a macro that expands without end, or input nested 400 levels deep,
is an error.

### Pasted LaTeX and documents

LaTeX written for a document can be given as it is, whole or in part:
output of `print.xtable()` or
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html), a `table`
float, or a paper's body with `input_mode = "document"`. What a grob has
no use for is read and dropped:

- the preamble: in a whole LaTeX file, everything before
  `\begin{document}` is read for its definitions (`\newcommand`,
  `\definecolor`, ...) and not drawn, as LaTeX draws nothing there; a
  size, colour or environment begun there ends at `\begin{document}`.
  What follows `\end{document}` is ignored. Package settings a grob
  cannot honour are not warned about. `\documentclass`, `\usepackage`
  (which loads nothing: every supported command is built in) and
  `\bibliographystyle` draw nothing wherever they are;

- title and cross-reference metadata: `\maketitle`, `\title{}`,
  `\author{}`, `\label{}`;

- alignment: in a label, `\centering`, `\raggedleft` and `\raggedright`
  do nothing (a label is placed by `hjust`), nor do `\flushleft`,
  `\flushright` and `\relax`. A document aligns each line of the
  paragraphs they are in, and of `center`, `flushleft` and `flushright`.

In a label the content of a `table` or `figure` float is set in the line
like any other. A document sets a float where it is written, apart from
its paragraphs, as LaTeX's `[h]` placement would, and a `center`
environment or a `\centering` centres its lines.

Some commands are their nearest equivalent. `\emph` is `\textit`, `\em`
is `\it`, and `\newline` is `\\`. Booktabs' `\toprule` and `\bottomrule`
are thick rules, `\midrule` a plain one, `\cmidrule` a partial one and
`\specialrule{w}{a}{b}` one `w` thick. `\boldmath` sets the math that
follows in its group in bold. A grob has no glue to stretch, so
`\smallskip`, `\medskip` and `\bigskip` are 0.25, 0.5 and 1 em of space,
`\hfill` is a quad and `\vfill` 1 em. And `\caption{X}` is a line of
text where it is written, unnumbered, so a caption written before a
`tabular` is set above it.

`\textwidth`, `\linewidth` and `\columnwidth` in a length
(`0.5\textwidth`) are `max_width`, or with none the 345pt of LaTeX's
article class. An `abstract` is set as article sets it, under a centred
heading, and `thebibliography` as a "References" heading over a list
numbered `[1]`, `[2]`, ... (`\bibitem`'s key has nothing to point at,
and `\newblock` is a space).

A `tabular`'s cells and the items of `itemize` and `enumerate` are text,
as in LaTeX, when the table or list is met in text; met in math
(`input_mode = "math"`, or between `$...$`) they are math. A list item
wraps at `max_width`, as a `p{}` cell does; a cell in an `l`, `c` or `r`
column is one line. A `minipage` sets its paragraphs to its width, and
to its optional height, as LaTeX does. A heading too long for
`max_width` wraps, its title hanging from its number.

Commands that need the rest of a document warn and draw what LaTeX draws
when it cannot resolve them: `\ref` and `\pageref` are a bold `??`,
`\eqref` is `(??)`, `\cite{key}` and natbib's `\citep{key}` are `[?]`
(`\citet` is `(author?) [?]`, `\citealp` a bare `?`), and a
`\footnote`'s text is set where it is written. Equations are not
numbered. Not supported: `\tag`, `\verb`, `\textsc`, the declarations
`\bfseries`, `\itshape` and their kin (use `\textbf{}`, `\textit{}` or
`\bf`, `\it`), the `description` list, theorem environments, and TikZ.

### Images

- `\includegraphics[opts]{file}` draws a PNG, JPEG or SVG inline. The
  starred form is accepted and behaves identically. `width`, `height`
  and `scale` take any LaTeX length (`\textwidth` resolves against
  `max_width`, and without one falls back to the file's own size with a
  warning); `scale` multiplies whatever `width`/`height` settled on, and
  `keepaspectratio` fits inside them instead of stretching to fill.
  `angle` rotates the figure and grows the surrounding box to the
  rotated bounds, as `\rotatebox` does. `origin`, `trim`, `clip` and
  `viewport` are parsed but not applied, and warn once so the difference
  is not silent; so does a length that cannot be read, or one that sizes
  the figure to nothing.

- The extension may be omitted, as in LaTeX: `{plots/fig}` finds
  `plots/fig.svg`, then `.png`, `.jpg`, `.jpeg`.

- An SVG is drawn as real vector and stays sharp at any output
  resolution; a bitmap does not, and warns when it would be shown below
  150 dpi. PDF and EPS are not supported: save the figure as SVG
  instead.

- The file must be local; a URL is not downloaded. A file that cannot be
  drawn – missing, a URL, an unsupported format, or unreadable – is an
  error saying why; in a document (`input_mode = "document"`) it warns
  and draws the file's name, so that one figure does not cost the whole
  document. Each format needs its reader, all *Suggests*: `png` for PNG,
  `jpeg` for JPEG and `rsvg` for SVG; the error names the one to
  install.

- The file is read when the parser meets the command, after macros are
  expanded, so one a macro produces (`\newcommand`, `\def` or
  [`define_macro()`](https://adayim.github.io/gridmicrotex/reference/define_macro.md))
  works like any other. A commented-out `% \includegraphics{...}` is
  ignored.

- `\graphicspath{{dir/}}` names the directories searched, as in LaTeX,
  and `\DeclareGraphicsExtensions{}` is read and dropped.

### Parallelism

The MicroTeX engine keeps mutable C++ state for font caching and text
measurement. Rendering is safe single-threaded and under
separate-process backends such as `future::plan(multisession)`. It is
*not* safe under forked backends
([`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html),
`future::plan(multicore)`) on Unix, because forked workers share that
state without synchronisation. Use a socket/multisession backend
instead.

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


  # A document body: a heading, paragraphs and a display, broken into
  # lines at max_width (in big points).
  grid::grid.newpage()
  doc <- r"(\section{Results}
The fitted line is
\[ \hat{y} = \beta_0 + \beta_1 x, \]
and its slope, $\beta_1$, is positive.)"
  grid.latex(doc, input_mode = "document", max_width = 250,
             x = 0.05, y = 0.95, hjust = 0, vjust = 1,
             gp = grid::gpar(fontsize = 12))

# }
```
