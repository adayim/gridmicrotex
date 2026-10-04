# Superseded font functions

`load_math_font()`, `available_math_fonts()` and `check_math_fonts()`
came before
[`load_font()`](https://adayim.github.io/gridmicrotex/reference/load_font.md)
and
[`available_fonts()`](https://adayim.github.io/gridmicrotex/reference/available_fonts.md),
which replace them. They keep working as they did:
[`load_font()`](https://adayim.github.io/gridmicrotex/reference/load_font.md)
also loads a math font, and
[`available_fonts()`](https://adayim.github.io/gridmicrotex/reference/available_fonts.md)
lists the math fonts (`math` is `TRUE`).

## Usage

``` r
available_math_fonts()

load_math_font(otf_path)

check_math_fonts()
```

## Arguments

- otf_path:

  Path to an OTF or TTF math font.

## Value

`load_math_font()` returns `NULL`, invisibly. `available_math_fonts()`
returns a character vector of math font names. `check_math_fonts()`
prints the math fonts that are loaded and whether the bundled font files
are present, and returns their names, invisibly.

## See also

[`load_font()`](https://adayim.github.io/gridmicrotex/reference/load_font.md),
[`available_fonts()`](https://adayim.github.io/gridmicrotex/reference/available_fonts.md)

## Examples

``` r
available_math_fonts()
#> [1] "DejaVu Sans"    "Lete Sans Math" "STIX Two Math" 
```
