# Draw LaTeX directly to the current device

Draw LaTeX directly to the current device

## Usage

``` r
grid.latex(tex, ...)
```

## Arguments

- tex:

  Character string of LaTeX math code.

- ...:

  Additional arguments passed to
  [`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md).

## Value

Invisibly returns the grob.

## See also

[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md),
[`latex_dims`](https://adayim.github.io/gridmicrotex/reference/latex_dims.md)

## Examples

``` r
# \donttest{
  grid.latex("x^{2} + y^{2} = z^{2}")

# }
```
