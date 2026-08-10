# Define a user-level LaTeX macro

Registers a zero-argument shorthand that is expanded by text
substitution before the expression reaches the MicroTeX parser. Useful
for domain-specific notation (e.g. `\RR` for `\mathbb{R}`) you reuse
across many plots.

## Usage

``` r
define_macro(name, definition)

clear_macros(name = NULL)

list_macros()
```

## Arguments

- name:

  Macro name **without** the leading backslash. For `clear_macros`, the
  macro name to drop, or `NULL` (default) to clear all.

- definition:

  LaTeX source the macro expands to.

## Value

- `define_macro`: Invisibly returns `NULL`.

- `clear_macros`: Invisibly returns `NULL`.

- `list_macros`: A named character vector mapping macro names to their
  expansions. Empty if no macros are defined.

## Choosing between this and `\newcommand`

MicroTeX also accepts `\newcommand` and plain-TeX `\def` written inside
the expression itself, and those are the more capable form: they take up
to nine arguments, which `define_macro()` does not — it substitutes text
and nothing else.


      # parameterised, but local to this one expression
      grid.latex(r"(\def\norm#1{\left\lVert #1 \right\rVert}
                    \norm{\vec{v}})")

What they cannot do is persist: the user-macro table is cleared at the
start of every parse, so a `\newcommand` written in one call is gone by
the next. That is the one thing `define_macro()` is for. Use
`\newcommand` / `\def` for an abbreviation local to a single label, and
`define_macro()` for notation you want available to every label in a
script.

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
