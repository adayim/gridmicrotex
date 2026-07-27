# Check math font status

Reports which **math** fonts are loaded and available for rendering: the
MicroTeX version, the loaded math fonts, and whether the bundled font
files are present.

## Usage

``` r
check_math_fonts()
```

## Value

Invisibly returns the character vector of available math font names.

## Details

Text fonts are not covered, because they are not registered here — they
are resolved on demand by systemfonts from `gp$fontfamily`. Use
[`systemfonts::match_fonts()`](https://systemfonts.r-lib.org/reference/match_fonts.html)
to see what a text family resolves to.

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
