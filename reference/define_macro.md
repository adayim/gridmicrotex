# Define a LaTeX shorthand for every label

`define_macro()` adds a macro without arguments, such as `\RR` for
`\mathbb{R}`, that every later label can use. `list_macros()` shows them
and `clear_macros()` removes them.

## Usage

``` r
define_macro(name, definition)

clear_macros(name = NULL)

list_macros()
```

## Arguments

- name:

  Macro name, without the backslash. For `clear_macros()`, `NULL`
  (default) removes all macros.

- definition:

  The LaTeX the macro stands for.

## Value

`list_macros()` returns a named character vector of macros and their
definitions. The others return `NULL`, invisibly.

## Details

A label can also define its own macros with `\newcommand`, `\def` and
the like, including macros with arguments, but those last for that label
only.

## See also

[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md),
[`latex_options`](https://adayim.github.io/gridmicrotex/reference/latex_options.md)

## Examples

``` r
# \donttest{
  define_macro("RR", "\\mathbb{R}")
  define_macro("eps", "\\varepsilon")
  grid::grid.newpage()
  grid.latex("\\forall \\eps > 0, \\eps \\in \\RR")

  clear_macros()
# }
```
