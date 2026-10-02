# gridmicrotex (development version)

- New math: `dcases`, `subarray`, `\shortintertext`, `\cancelto`, `\DeclarePairedDelimiter`, `\mkern`, `\mskip`, `\mspace` and `\hskip`. A column's `>{\displaystyle}` (or `\textstyle`, ...) now sets its cells in that style.
- Equations in `equation`, `align`, `gather`, `multline`, `flalign`, `alignat` and `eqnarray` are numbered at the right margin, as in LaTeX, in labels and markdown too: write `align*`, or `\notag`, to leave one out. `\tag`, `\tag*`, `\label`, `\ref`, `\eqref` and `\setcounter{equation}{n}` work, and a markdown box counts across its blocks.
- Tables from `kable()`, kableExtra, xtable, gt and tinytable are read without warnings: `\addlinespace`, `\fontsize`, `\begingroup`, `tabularx` and `tabular*` with their widths, `longtable` with its head and foot, `\makecell`, `\hhline`, `\bfseries` and its kin, `>{}` and `<{}` in a column spec, `\caption*`, and tabularray's `tblr` (rules, spans, colours, fonts and alignments).
- New text items: `\verb` and `verbatim`, `description` lists (and `\item[label]` in any list), `\textsc` and `\scshape`, `\newtheorem` with `proof`. A font switch such as `\itshape` now lasts across a paragraph break in a document.
- New commutative diagrams: tikz-cd's `tikzcd` (labels, `hook`, `two heads`, `dashed`, `bend`, `shift`, `Rightarrow`, `description`, ...) and amscd's `CD`.
- New KaTeX commands: the arrow and set aliases (`\rarr`, `\Reals`, `\R`, `\Bbb`, ...), `\argmax`, `\bra`/`\ket`/`\set`, the text symbols and letters (`\textbar`, `\AE`, `\ss`, ...), `\bigm`, and the colon relations. A document's own definition of a name like `\R` or `\set` replaces ours.
- New `\hdashline` and `:` in a column spec, for dashed rules in arrays and tables.
- A subset of siunitx: `\num`, `\si`, `\SI`, `\qty`, `\ang`, ranges, lists and `\DeclareSIUnit`.
- Everything on KaTeX's lists of supported functions is drawn: the last gaps, `\mathchoice`, `\vcenter`, `\expandafter`, `\noexpand`, `\futurelet`, `\edef`, `\xdef`, `\phase`, `\angl`, `\H`, `\textcircled`, `\xlongequal`, `\xtofrom`, `\xtwoheadrightarrow`, `\overgroup`, `\overlinesegment`, `\widecheck`, `\Overrightarrow`, `\overleftharpoon`, `\TextOrMath`, `\@ifstar`, `\@ifnextchar`, `\@firstoftwo`, `\sixptsize`, and `\global` before `\edef`, `\let` and `\futurelet`, are filled, and `\ce` and `\pu` read a subset of mhchem. `\arraystretch` spaces the rows of an array. The list is `inst/supported` (built by `build_supported()`; the PDF is on the website).
- `\cancel` and `\bcancel` were drawn the wrong way round.
- kableExtra's `scale_down` drew nothing, and a colour defined with capitals in its name was never found.


# gridmicrotex 0.2.0

- LaTeX is read by a new, faster parser that follows TeX's rules, so macros, environments and starred forms work as in LaTeX, and malformed input is drawn with a warning giving its line:col instead of failing. As in TeX, a space after a command is dropped (write `\LaTeX{} is`), and `_`, `^`, `#` and `&` outside math warn.
- New `input_mode = "document"` renders a LaTeX document body, or a whole pasted paper, wrapped at `max_width` (see `vignette("documents")`). Markdown is now laid out the same way, so its paragraphs, styled spans and tables stay within their box.
- New `latex_options(device_math = TRUE)` renders `$…$` math in base graphics labels.
- An image that cannot be drawn is now an error saying why, figures load about ten times faster, and many bugs are fixed.


# gridmicrotex 0.1.1

- The typeface fallback is now a message rather than a warning, is raised only when `render_mode = "typeface"` was actually asked for, and at most once per device. A figure holding many math labels no longer repeats it.
- Bug fix: the layout engine's macro tables were never freed. They leaked at process exit, and `unloadNamespace()` left both them and the shared object in place. All are now released.


# gridmicrotex 0.1.0

- Text inside `\text{}` is drawn a line at a time rather than a letter at a time, so kerning is applied, PDF/SVG output can be searched for a phrase, and files are several times smaller. Text given a `max_width` is drawn a word at a time, since the spaces are where it breaks.
- Right-to-left text renders in the correct order, including across emphasis, colour and other font changes, and with or without `max_width`. 
- New `markdown_grob()` and `grid.markdown()` render inline markdown with LaTeX math; `markdown_box_grob()` renders a block document.
- New `geom_markdown()` and `element_markdown()` for ggplot2, both taking `style`. A title containing headings or lists is laid out as blocks, not flattened.
- Markdown covers headings, lists, task lists, quotes, code, tables, images, footnotes, display `$$…$$`, links and inline HTML. A fenced code block keeps its indentation and is syntax-highlighted when it names a language.
- New `register_highlighter()` and `available_highlighters()`. R, Python, SQL, shell, C++, YAML, JSON, Stan, Julia and LaTeX are built in, along with the usual GitHub aliases; token colours are CSS classes (`.kw`, `.co`, `.st`, …), the names knitr already writes into HTML output. Grammars are KDE syntax XML files.
- New `markdown_style()` and `md_style()` style markdown through a CSS cascade of HTML tag names; the `body` rule styles the box itself.
- New `latex_options(markdown_style = )` sets a document-wide default.
- New `"github"` style preset, shipped as a CSS file.
- `<div class=>` and `<div style=>` style a chunk of markdown; `<span class=>` styles an inline run.
- New `justify` and `line_break` arguments control paragraph line breaking.
- New `\gmfontfamily{family}{content}` sets the font for one run of text.
- `\includegraphics[width=,height=,scale=,keepaspectratio]{file}` draws PNG, JPEG and SVG images inline in a formula; it previously parsed and drew nothing. The extension may be omitted and `\graphicspath{}` is searched, as in LaTeX. An SVG is drawn as real vector, so it stays sharp at any output resolution. 
- New `p{len}` column type gives `tabular` fixed-width, wrapping cells.
- `\url{}` and `\href{}{}` render as styled text instead of literally.
- `load_font()` is renamed `load_math_font()`; the old name is deprecated.
- `check_fonts()` is renamed `check_math_fonts()`; the old name is deprecated.
- Bug fix: `\rotatebox` past a quarter turn drew text and glyphs 180 degrees out, so a `\rotatebox{90}` label came out upside down.
- Bug fix: `\textrm{}` now returns text to `gp$fontfamily`; it previously did nothing.
- Bug fix: `\texttt{}` drew in the body font instead of a monospace one.
- Bug fix: `max_width` is now honoured by content containing `\\` line breaks.
- Bug fix: in `"mixed"` mode a line break was kept inside the text rather than breaking the formula, so anything after it was drawn beside the whole block instead of on its own line. A pasted `\caption` landed to the left of its table, and math following a line break sat between the lines.
- Bug fix: LaTeX tick labels measured 0 x 0, so ggplot2 reserved no room and they overlapped the axis title.
- Bug fix: reloading the package corrupted MicroTeX's macro registry, so a later large formula could crash R.
- Bug fix: `\-` offered a line break but drew no hyphen at it.
- Bug fix: an `&` in text was read as an alignment tab and everything after it was dropped, so `"Treatment & Control"` rendered as `"Treatment "`.
- Bug fix: `tabular*` failed to parse, reporting an invalid alignment; its width argument is now dropped along with the star.
- Bug fix: a layout measured on one graphics device could be reused on another, placing text at the wrong widths — the layout cache now keys on the device.
- Bug fix: the package failed to compile on compilers that no longer declare `strtod()` and `strtol()` through other headers, such as clang 23.


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

