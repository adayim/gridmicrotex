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

Use LaTeX in ggplot2 plots

- [`geom_latex()`](https://adayim.github.io/gridmicrotex/reference/geom_latex.md)
  : A ggplot2 geom for LaTeX math labels
- [`element_latex()`](https://adayim.github.io/gridmicrotex/reference/element_latex.md)
  : A ggplot2 theme element for LaTeX text

## Font management

Load and configure math and text fonts

- [`available_math_fonts()`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md)
  : List available math fonts
- [`load_font()`](https://adayim.github.io/gridmicrotex/reference/load_font.md)
  : Load a math font from an OTF file
- [`check_fonts()`](https://adayim.github.io/gridmicrotex/reference/check_fonts.md)
  : Check font status

## Misc utilities

Utilities for working with LaTeX strings and rendering

- [`latex_wrap()`](https://adayim.github.io/gridmicrotex/reference/latex_wrap.md)
  : Wrap standard text for math-first LaTeX renderers
