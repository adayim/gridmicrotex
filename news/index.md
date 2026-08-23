# Changelog

## gridmicrotex 0.1.1

- The typeface fallback is now a message rather than a warning, is
  raised only when `render_mode = "typeface"` was actually asked for,
  and at most once per device. A figure holding many math labels no
  longer repeats it.
- Bug fix: unloading the package left its shared object mapped and the
  layout engine’s macro registries allocated.
  [`unloadNamespace()`](https://rdrr.io/r/base/ns-load.html) now
  releases both.

## gridmicrotex 0.1.0

CRAN release: 2026-08-21

- Text inside `\text{}` is drawn a line at a time rather than a letter
  at a time, so kerning is applied, PDF/SVG output can be searched for a
  phrase, and files are several times smaller. Text given a `max_width`
  is drawn a word at a time, since the spaces are where it breaks.
- Right-to-left text renders in the correct order, including across
  emphasis, colour and other font changes, and with or without
  `max_width`.
- New
  [`markdown_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_grob.md)
  and
  [`grid.markdown()`](https://adayim.github.io/gridmicrotex/reference/markdown_grob.md)
  render inline markdown with LaTeX math;
  [`markdown_box_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md)
  renders a block document.
- New
  [`geom_markdown()`](https://adayim.github.io/gridmicrotex/reference/geom_markdown.md)
  and
  [`element_markdown()`](https://adayim.github.io/gridmicrotex/reference/element_markdown.md)
  for ggplot2, both taking `style`. A title containing headings or lists
  is laid out as blocks, not flattened.
- Markdown covers headings, lists, task lists, quotes, code, tables,
  images, footnotes, display `$$…$$`, links and inline HTML. A fenced
  code block keeps its indentation and is syntax-highlighted when it
  names a language.
- New
  [`register_highlighter()`](https://adayim.github.io/gridmicrotex/reference/register_highlighter.md)
  and
  [`available_highlighters()`](https://adayim.github.io/gridmicrotex/reference/available_highlighters.md).
  R, Python, SQL, shell, C++, YAML, JSON, Stan, Julia and LaTeX are
  built in, along with the usual GitHub aliases; token colours are CSS
  classes (`.kw`, `.co`, `.st`, …), the names knitr already writes into
  HTML output. Grammars are KDE syntax XML files.
- New
  [`markdown_style()`](https://adayim.github.io/gridmicrotex/reference/markdown_style.md)
  and
  [`md_style()`](https://adayim.github.io/gridmicrotex/reference/md_style.md)
  style markdown through a CSS cascade of HTML tag names; the `body`
  rule styles the box itself.
- New `latex_options(markdown_style = )` sets a document-wide default.
- New `"github"` style preset, shipped as a CSS file.
- `<div class=>` and `<div style=>` style a chunk of markdown;
  `<span class=>` styles an inline run.
- New `justify` and `line_break` arguments control paragraph line
  breaking.
- New `\gmfontfamily{family}{content}` sets the font for one run of
  text.
- `\includegraphics[width=,height=,scale=,keepaspectratio]{file}` draws
  PNG, JPEG and SVG images inline in a formula; it previously parsed and
  drew nothing. The extension may be omitted and `\graphicspath{}` is
  searched, as in LaTeX. An SVG is drawn as real vector, so it stays
  sharp at any output resolution.
- New `p{len}` column type gives `tabular` fixed-width, wrapping cells.
- `\url{}` and `\href{}{}` render as styled text instead of literally.
- [`load_font()`](https://adayim.github.io/gridmicrotex/reference/gridmicrotex-deprecated.md)
  is renamed
  [`load_math_font()`](https://adayim.github.io/gridmicrotex/reference/load_math_font.md);
  the old name is deprecated.
- [`check_fonts()`](https://adayim.github.io/gridmicrotex/reference/gridmicrotex-deprecated.md)
  is renamed
  [`check_math_fonts()`](https://adayim.github.io/gridmicrotex/reference/check_math_fonts.md);
  the old name is deprecated.
- Bug fix: `\rotatebox` past a quarter turn drew text and glyphs 180
  degrees out, so a `\rotatebox{90}` label came out upside down.
- Bug fix: `\textrm{}` now returns text to `gp$fontfamily`; it
  previously did nothing.
- Bug fix: `\texttt{}` drew in the body font instead of a monospace one.
- Bug fix: `max_width` is now honoured by content containing `\\` line
  breaks.
- Bug fix: in `"mixed"` mode a line break was kept inside the text
  rather than breaking the formula, so anything after it was drawn
  beside the whole block instead of on its own line. A pasted `\caption`
  landed to the left of its table, and math following a line break sat
  between the lines.
- Bug fix: LaTeX tick labels measured 0 x 0, so ggplot2 reserved no room
  and they overlapped the axis title.
- Bug fix: reloading the package corrupted MicroTeX’s macro registry, so
  a later large formula could crash R.
- Bug fix: `\-` offered a line break but drew no hyphen at it.
- Bug fix: an `&` in text was read as an alignment tab and everything
  after it was dropped, so `"Treatment & Control"` rendered as
  `"Treatment "`.
- Bug fix: `tabular*` failed to parse, reporting an invalid alignment;
  its width argument is now dropped along with the star.
- Bug fix: a layout measured on one graphics device could be reused on
  another, placing text at the wrong widths — the layout cache now keys
  on the device.
- Bug fix: the package failed to compile on compilers that no longer
  declare `strtod()` and `strtol()` through other headers, such as clang
  23.

## gridmicrotex 0.0.5

CRAN release: 2026-07-21

- Bug fix: correct
  [`grobX()`](https://rdrr.io/r/grid/grobX.html)/[`grobY()`](https://rdrr.io/r/grid/grobX.html)
  boundary points.
- Bug fix: `geom_latex(fontsize = )` was ignored.
- Bug fix: spurious “font metrics unknown” warnings in `"mixed"` mode.
- Bug fix: CJK fallback width on Windows was ~6x too narrow.
- Bug fix: layout-cache collisions between unresolved text fonts.
- Bug fix: measuring `\text{}` runs no longer pushes a viewport on the
  caller’s device. The push/pop was recorded on the graphics engine
  display list, so the device looked like it already held a plot and
  `knitr` emitted a spurious blank figure ahead of the real one.
- Hardened the OTF MATH reader against malformed fonts that could hang
  R.
- Docs: `input_mode` defaults to `"mixed"`; use `\textbf{}` etc. instead
  of `gp$fontface`.

## gridmicrotex 0.0.4

CRAN release: 2026-06-01

- Accept raw latex code from other packages, like
  `xtable::print.xtable()` /
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) /
  booktabs output.
- New MicroTeX commands `\thickhline` and `\cline{a-b}`.
- New `itemize` and `enumerate` list environments. Lists may nest.
- Bug fix: `$…$` inside tabular cells no longer chops the table.
- Bug fix: starred alignment envs (`align*`, `eqnarray*`, …) now render.
- Bug fix:
  [`latex_wrap()`](https://adayim.github.io/gridmicrotex/reference/latex_wrap.md)
  is now vectorised over its input, matching its documented contract,
  and errors on `NA` input instead of rendering “NA”.
- Bug fix: the `\mark{}` macro survives a `microtex_release()` / re-init
  cycle.
- `gp$col` transparency is now honoured (alpha passed through to
  MicroTeX).
- Macro expansion warns on circular definitions instead of silently
  producing wrong output.

## gridmicrotex 0.0.3

CRAN release: 2026-05-18

- Self-contained
  [`load_font()`](https://adayim.github.io/gridmicrotex/reference/gridmicrotex-deprecated.md)
  example so CRAN’s donttest additional checks no longer fail on the
  unreliable CTAN font download.
- New commands.

## gridmicrotex 0.0.2

CRAN release: 2026-05-16

- Support the `\def` command
- New function `grobMark`.
- Bug fix `ggplot2` integration.
- Bug fix coloring body.
- `ggplot2` integration respects `latex_options`.
- Defer `systemfonts` registration of the bundled Lete and STIX fonts to
  first render. This avoids the `XType: Using static font registry.`
  notice that older macOS SDKs emit on Core Text font registration,
  which had caused spurious WARN/NOTEs on `r-oldrel-macos-arm64`.

## gridmicrotex 0.0.1

CRAN release: 2026-05-08

Initial release.
