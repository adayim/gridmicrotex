# gridmicrotex (development version)

- New `markdown_grob()` and `grid.markdown()` render inline markdown with LaTeX math; `markdown_box_grob()` renders a block document.
- New `geom_markdown()` and `element_markdown()` for ggplot2.
- New `markdown_style()` and `md_style()` style markdown through a CSS cascade.
- New `latex_options(markdown_style = )` sets a document-wide default.
- New `justify` and `line_break` arguments control paragraph line breaking.
- New `\gmfontfamily{family}{content}` sets the font for one run of text.
- `load_font()` is renamed `load_math_font()`; the old name is deprecated.
- `check_fonts()` is renamed `check_math_fonts()`; the old name is deprecated.
- Bug fix: `\textrm{}` now returns text to `gp$fontfamily`; it previously did nothing.
- Bug fix: `\texttt{}` drew in the body font instead of a monospace one.
- Bug fix: `max_width` is now honoured by content containing `\\` line breaks.
- Bug fix: LaTeX tick labels measured 0 x 0, so ggplot2 reserved no room and they overlapped the axis title.
- Bug fix: reloading the package corrupted MicroTeX's macro registry, so a later large formula could crash R.

# gridmicrotex 0.0.5

- Bug fix: correct `grobX()`/`grobY()` boundary points.
- Bug fix: `geom_latex(fontsize = )` was ignored.
- Bug fix: spurious "font metrics unknown" warnings in `"mixed"` mode.
- Bug fix: CJK fallback width on Windows was ~6x too narrow.
- Bug fix: layout-cache collisions between unresolved text fonts.
- Bug fix: measuring `\text{}` runs no longer pushes a viewport on the caller's
  device. The push/pop was recorded on the graphics engine display list, so the
  device looked like it already held a plot and `knitr` emitted a spurious blank
  figure ahead of the real one.
- Hardened the OTF MATH reader against malformed fonts that could hang R.
- Docs: `input_mode` defaults to `"mixed"`; use `\textbf{}` etc. instead of `gp$fontface`.

# gridmicrotex 0.0.4

- Accept raw latex code from other packages, like `xtable::print.xtable()` / `knitr::kable()` / booktabs output.
- New MicroTeX commands `\thickhline` and `\cline{a-b}`.
- New `itemize` and `enumerate` list environments. Lists may nest.
- Bug fix: `$…$` inside tabular cells no longer chops the table.
- Bug fix: starred alignment envs (`align*`, `eqnarray*`, …) now render.
- Bug fix: `latex_wrap()` is now vectorised over its input, matching its
  documented contract, and errors on `NA` input instead of rendering "NA".
- Bug fix: the `\mark{}` macro survives a `microtex_release()` /
  re-init cycle.
- `gp$col` transparency is now honoured (alpha passed through to MicroTeX).
- Macro expansion warns on circular definitions instead of silently
  producing wrong output.

# gridmicrotex 0.0.3

- Self-contained `load_font()` example so CRAN's donttest additional checks no longer fail on the unreliable CTAN font download.
- New commands.

# gridmicrotex 0.0.2

- Support the `\def` command
- New function `grobMark`.
- Bug fix `ggplot2` integration.
- Bug fix coloring body.
- `ggplot2` integration respects `latex_options`.
- Defer `systemfonts` registration of the bundled Lete and STIX fonts to first render. This avoids the `XType: Using static font registry.` notice that older macOS SDKs emit on Core Text font registration, which had caused spurious WARN/NOTEs on `r-oldrel-macos-arm64`.

# gridmicrotex 0.0.1

Initial release.

