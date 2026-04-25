# Check font status

Reports which math fonts are loaded and available for rendering. Shows
the MicroTeX version, loaded math fonts, and whether bundled font files
are present.

## Usage

``` r
check_fonts()
```

## Value

Invisibly returns the character vector of available font names.

## Examples

``` r
check_fonts()
#> MicroTeX version: 1.0.0
#> Loaded math fonts (2):
#>   - Lete Sans Math
#>   - STIX Two Math
#> Bundled font files:
#>   - LeteSansMath.otf: found
#>   - STIXTwoMath-Regular.otf: found
```
