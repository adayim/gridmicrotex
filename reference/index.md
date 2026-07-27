# Package index

## Core rendering

Create and draw LaTeX math as grid grobs

- [`latex_grob()`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)
  [`grid.latex()`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)
  : Create a grid grob from a LaTeX expression
- [`latex_dims()`](https://adayim.github.io/gridmicrotex/reference/latex_dims.md)
  : Get dimensions of a LaTeX expression
- [`latex_tree()`](https://adayim.github.io/gridmicrotex/reference/latex_tree.md)
  : Inspect the parsed layout of a LaTeX expression
- [`grobMark()`](https://adayim.github.io/gridmicrotex/reference/grobMark.md)
  : Look up a named anchor inside a LaTeX grob

## Markdown

Render markdown, with LaTeX math inline, as grid grobs

- [`markdown_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_grob.md)
  [`grid.markdown()`](https://adayim.github.io/gridmicrotex/reference/markdown_grob.md)
  : Render markdown as a grid grob
- [`markdown_box_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md)
  : Render a markdown document as a boxed grid grob

## Markdown styling

A small CSS cascade over the markdown tags

- [`markdown_style()`](https://adayim.github.io/gridmicrotex/reference/markdown_style.md)
  : A style for markdown rendering
- [`md_style()`](https://adayim.github.io/gridmicrotex/reference/md_style.md)
  : Declarations for one markdown tag

## Options, macros, and cache

Project-wide defaults and advanced tooling

- [`latex_options()`](https://adayim.github.io/gridmicrotex/reference/latex_options.md)
  [`reset_latex_options()`](https://adayim.github.io/gridmicrotex/reference/latex_options.md)
  : Set or query package-wide LaTeX rendering defaults
- [`define_macro()`](https://adayim.github.io/gridmicrotex/reference/define_macro.md)
  [`clear_macros()`](https://adayim.github.io/gridmicrotex/reference/define_macro.md)
  [`list_macros()`](https://adayim.github.io/gridmicrotex/reference/define_macro.md)
  : Define a user-level LaTeX macro
- [`latex_cache_limit()`](https://adayim.github.io/gridmicrotex/reference/latex_cache_limit.md)
  [`latex_cache_clear()`](https://adayim.github.io/gridmicrotex/reference/latex_cache_limit.md)
  [`latex_cache_info()`](https://adayim.github.io/gridmicrotex/reference/latex_cache_limit.md)
  : Set the maximum number of entries kept in the LaTeX layout cache

## ggplot2 integration

Use LaTeX and markdown in ggplot2 plots

- [`geom_latex()`](https://adayim.github.io/gridmicrotex/reference/geom_latex.md)
  : A ggplot2 geom for LaTeX math labels
- [`element_latex()`](https://adayim.github.io/gridmicrotex/reference/element_latex.md)
  : A ggplot2 theme element for LaTeX text
- [`geom_markdown()`](https://adayim.github.io/gridmicrotex/reference/geom_markdown.md)
  : A ggplot2 geom for markdown labels
- [`element_markdown()`](https://adayim.github.io/gridmicrotex/reference/element_markdown.md)
  : A ggplot2 theme element for markdown text

## Font management

Load and configure math and text fonts

- [`available_math_fonts()`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md)
  : List available math fonts
- [`load_math_font()`](https://adayim.github.io/gridmicrotex/reference/load_math_font.md)
  : Load a math font from an OTF file
- [`check_math_fonts()`](https://adayim.github.io/gridmicrotex/reference/check_math_fonts.md)
  : Check math font status

## Misc utilities

Utilities for working with LaTeX strings and rendering

- [`latex_wrap()`](https://adayim.github.io/gridmicrotex/reference/latex_wrap.md)
  : Wrap standard text for math-first LaTeX renderers
