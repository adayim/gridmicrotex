# gridmicrotex 0.2.1

- `\usepackage` now loads packages. New `load_sty()` makes a `.sty` file of your own one, and `latex_options(packages = )` loads packages before every label.
- A name from a package a label does not load can be defined with `\newcommand`, as in LaTeX.
- Bug fix: macros and dimensions were read wrongly in a few cases, such as a parameter next to a control word and letters after a unit.
- Bug fix: the rows of matrices, `cases`, `align` and kin were closer together than in LaTeX, and a `\frac` in a matrix was too large.
- Bug fix: a display holding only its number, such as `\[\tag{1}\]`, crashed when set at a width.
- Bug fix: a colour such as `red!50` was drawn black, so `\rowcolor{gray!10}` painted a row black. Colours are now mixed as in xcolor, and a colour that is not defined warns instead of drawing black. New `\colorlet`.
- `\parbox`, `\setlength`, `\addtolength`, `\newlength` and counters (`\newcounter`, `\setcounter`, `\stepcounter`, `\arabic`, ...) now work. `\parindent`, `\parskip`, `\tabcolsep` and `\arraycolsep` can be set.
- Bug fix: text tables had 1em between their columns and at their edges, 2pt less than LaTeX's `\tabcolsep`; they are now padded as LaTeX pads them.
- Bug fix: `\fbox`, `\colorbox`, `\fcolorbox` and `\boxed` had 5bp of room around their text at a size of 10, LaTeX's `\fboxsep` is 3pt; they are set as LaTeX sets them, and `\fboxsep` and `\fboxrule` can be set.
- Bug fix: a `tablenotes` list (threeparttable, kableExtra's footnotes) drew `\item` as a red name; `\makecell[tl]` and siunitx `S` columns made the whole table fail. Notes are now a list of superscript labels, `\makecell` takes its `[vh]` letters, and an `S` column lines up its decimal markers.
- `\cite` draws the number of its `\bibitem` (or its `[label]`) instead of `[?]`, before or after the `thebibliography`, and `\bibitem[label]` is marked with its label.
- New: `tikzpicture` draws paths, nodes, arrows, `positioning`, `\foreach`, `\tikzset` and `\tikzstyle`, as TikZ does, in grid grobs and on devices. It also draws `ellipse`, `diamond` and `rounded rectangle` nodes, `label` and `pin`, `calc` coordinates, `fit` and background layers. The other shapes, trees and matrices are not drawn yet.
- Bug fix: the lines of a paragraph, or of a label with `\\`, were set from the font's height plus a gap, 11bp apart at a size of 10 where LaTeX's `\baselineskip` is 12 (`gp$lineheight` times the font size, as in grid); and a display was 1em from the lines around it, not LaTeX's `\abovedisplayskip` and `\belowdisplayskip` with their short forms after a short line. Documents, wrapped text and multi-line labels are taller by up to 10%.
- Bug fix: `orange`, `purple`, `violet` and `brown` were dvips's colours, not the ones LaTeX's xcolor has under those names (dvips's are `Orange` ...), and `lime`, `olive`, `pink`, `teal`, `darkgray` and `lightgray` were not defined.

# gridmicrotex 0.2.0

- LaTeX is read by a new, faster parser that follows TeX's rules, so macros, environments and starred forms work as in LaTeX, and malformed input is drawn with a warning giving its line:col instead of failing. As in TeX, a space after a command is dropped (write `\LaTeX{} is`), and `_`, `^`, `#` and `&` outside math warn.
- New `input_mode = "document"` renders a LaTeX document body, or a whole pasted paper, wrapped at `max_width` (see `vignette("documents")`). Markdown is now laid out the same way, so its paragraphs, styled spans and tables stay within their box.
- New `latex_options(device_math = TRUE)` renders `$…$` math in base graphics labels.
- New `load_font()` names a font file or an installed family, with its bold and italic files, and `available_fonts()` lists them. `latex_options()` takes `main_font`, `sans_font` and `mono_font`, the fonts of the body, `\textsf` and `\texttt`. `load_math_font()`, `available_math_fonts()` and `check_math_fonts()` are superseded by `load_font()` and `available_fonts()`; they still work.
- Text in a font given to `load_font()` is measured and drawn from the font file, so it looks the same on every device, base `pdf()` included, where a font file never worked before. Other families are still the device's.
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

