# List available math fonts

Returns the names of all math fonts currently loaded by MicroTeX. These
names can be passed to the `math_font` parameter of
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)
and
[`grid.latex`](https://adayim.github.io/gridmicrotex/reference/grid.latex.md).

## Usage

``` r
available_math_fonts()
```

## Value

A character vector of math font names.

## Examples

``` r
available_math_fonts()
#> [1] "LatinModernMath-Regular"   "TeXGyreDejaVuMath-Regular"
#> [3] "XITS Math"                
```
