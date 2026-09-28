# Check math font status

Prints which math fonts are available and whether the bundled font files
are present. Text fonts are not covered; use
[`systemfonts::match_fonts()`](https://systemfonts.r-lib.org/reference/match_fonts.html)
to see what a font family resolves to.

## Usage

``` r
check_math_fonts()
```

## Value

The names of the available math fonts, invisibly.

## See also

[`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md),
[`load_math_font`](https://adayim.github.io/gridmicrotex/reference/load_math_font.md)

## Examples

``` r
check_math_fonts()
#> MicroTeX version: 1.0.0
#> Loaded math fonts (2):
#>   - Lete Sans Math
#>   - STIX Two Math
#> Bundled font files:
#>   - LeteSansMath.otf: found
#>   - STIXTwoMath-Regular.otf: found
```
