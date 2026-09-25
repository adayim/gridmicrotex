# gridmicrotex 0.2.0

- LaTeX is read by a new parser that follows TeX's rules, and long input is several times faster to read.
- PNG and JPEG figures load about ten times faster, and text is measured about twice as fast.
- New `input_mode = "document"` reads a LaTeX document body: a line end is a space, a blank line or `\par` starts an indented paragraph, `\section` to `\paragraph` are numbered headings, display math is centred on a line of its own, and lists and floats are set apart from the text. See `vignette("documents")`.
- Malformed LaTeX no longer stops a label: the rest is drawn, with one warning listing each problem at its line:col.
- New `latex_options(device_math = TRUE)` renders `$…$` math in labels drawn to the graphics device, so **base** graphics gets real LaTeX — `main`, `xlab`, `ylab`, `text()`, `mtext()`, `legend()` — with no other change to your code. It intercepts the device, so grid, ggplot2 and lattice text is covered too.
- `latex_options()` now resets an option passed as `NULL`, as `options()` does, so the list it returns can be passed back with `do.call()` to restore those settings.
- An image that cannot be drawn is now an error saying why, and names the package to install when a reader is missing. This covers a missing file, a URL, an unsupported format or an unreadable file, in `\includegraphics`, markdown `![]()` and `<img>`. It used to draw the file name or the alt text. Base-graphics labels are unchanged, and a document (`input_mode = "document"`) warns and draws the file name, so that one figure does not cost the whole document.
- A space after a command is dropped, as in TeX: `\LaTeX is` draws "LaTeXis"; write `\LaTeX{} is` or `\LaTeX\ is`. A run of spaces is one space.
- In labels, `_`, `^`, `#` and `&` outside math are drawn with a warning, as LaTeX refuses them there: write `$x^2$`, or `\_`, `\#`, `\&` for the character.
- The items of `itemize` and `enumerate` and the cells of `tabular` are text, as in LaTeX: put math in them between `$…$`. With `input_mode = "math"` they are math, as before.
- `--` and `---` in text are dashes, and `` ` `` and `'`, single or doubled, are curly quotes, as in TeX, except in `\texttt{}`, `\tt` and `\url{}`. Markdown prose keeps its quotes and hyphens, as CommonMark does.
- A prime is TeX's `^{\prime}`, and `` ` `` and `"` in math are those characters, not a backprime and a double prime.
- `\|` is ‖, as in LaTeX, so `Vmatrix` has double bars.
- `\ref`, `\eqref`, `\pageref`, `\cite` and `\footnote` warn and draw what LaTeX draws when it cannot resolve them (a bold `??` or `[?]`, the note's text), instead of their names in red; so do natbib's `\citep`, `\citet` and `\citealp`.
- A pasted paper's `abstract`, `thebibliography` (with `\bibitem` and `\newblock`), `\em`, `\boldmath` and booktabs' `\specialrule` are read, and `\textwidth`, `\linewidth` and `\columnwidth` work in lengths (`0.5\textwidth`). In a whole LaTeX file the preamble is read for its definitions and not drawn, and what follows `\end{document}` is ignored, as in LaTeX.
- `\definecolor` takes xcolor's `RGB` (0–255) and `HTML` (hexadecimal) models.
- Text under `\large`, `\small` and the other sizes, `\color` or `\textcolor` wraps at `max_width`, and so do a list item, hanging under its own text, and a heading, hanging from its number; they used to run past it. List labels of different widths are set right, as in LaTeX.
- `minipage` sets its paragraphs to its width (and height, if given), placed by its `[t]`, `[c]` or `[b]` position, so figures can sit side by side.
- A definition inside `{…}` ends with the group, as in TeX; `\gdef` is global.
- An unknown environment draws its body instead of its name in red: as text in prose, as LaTeX does, and as an array in math. An unclosed environment is closed at the end, and a stray `\end{…}` is dropped.
- Starred forms work (`\operatorname*`, `\newcommand*`, `\DeclareMathOperator*`, `\hspace*`, `\\*`), as do `\def` with delimited parameters, `\let`, `\providecommand`, `\newenvironment` with arguments, and `\ensuremath`.
- Bug fix: `\cline{a-b}` naming a column past a table's last read past the end of its column widths.
- Bug fix: math drawn to base `pdf()` used a font the file did not embed, so it was garbled in any viewer without that font; `pdf()` and `postscript()` now draw it as outlines. Use `cairo_pdf()` for selectable math.
- Bug fix: an argument without braces took one byte rather than one character, garbling `\frac αβ` and `\hat é`.
- Bug fix: a macro argument's own `#2` was replaced by the macro's second argument.
- Bug fix: a `define_macro("RR", …)` macro also replaced the `\RR` in `\\RR`.
- Bug fix: `\\[len]` drew "[len]" instead of adding the space.
- Bug fix: `\color` after `\\` did not colour what followed it.
- Bug fix: `\middle` failed with a named delimiter such as `\vert`, and a delimiter that is not one failed the whole label.
- Bug fix: a `\left` with no `\right` did not stretch its delimiter.
- Bug fix: `\textsuperscript`, `\textsubscript` and `\degree` in text drew a literal `^`.
- Bug fix: `\#`, `\$`, `\%`, `\&` and `\_` in text were drawn as math symbols.
- Bug fix: `\url{}` did not draw `~` and `\` as written.
- Bug fix: a line holding only definitions (`\newcommand`, `\definecolor`, …) left a gap in the label.
- Bug fix: an `<img>` alone on its line in markdown was dropped.
- Bug fix: a markdown image whose file name contains a `$…$` pair or a brace drew its alt text instead of the image, as did an `<img>` whose `src` was unquoted, upper-case or padded with spaces.
- Bug fix: a commented-out `% \includegraphics{…}` warned that its file was missing.
- Bug fix: an error inside a command's argument, such as `\text{}` or `\frac{}{}`, silently dropped the rest of that argument; now only the bad part is lost, with a warning.
- Bug fix: `\newcommand` or `\def` of a built-in command such as `\frac` broke it in every later label. `\newcommand` of an existing command is now refused with a warning, as in LaTeX, and `\renewcommand` or `\def` redefines it for that label only.
- Bug fix: `reset_latex_options()` left a math font set with `latex_options(math_font = )` in effect.
- Bug fix: a colour at 50% opacity or more was drawn black on Windows.
- Bug fix: a macro defined in terms of itself hung R; it is now an error.
- Bug fix: a layout measured on one device was reused on a device of the same kind at a different resolution.
- Bug fix: `geom_latex()` and `geom_markdown()` failed when a mapped `alpha` was `NA`.
- Bug fix: `annotate("latex")` and `annotate("markdown")` ignored `latex_options()`: a label was drawn in the default input mode, math font and render mode. They, and `geom_latex()` and `geom_markdown()`, now read the options when the plot is drawn.
- Bug fix: CSS `border: none` or `border: 0` still drew a frame or table rule, and a `body` border with no colour was not drawn.
- Bug fix: `clear_macros()` given a number removed an unrelated macro.
- Bug fix: Ctrl-C was ignored while text in a formula was being measured.
- Hardened the font reader and the TrueType Collection splitter against malformed input.


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

