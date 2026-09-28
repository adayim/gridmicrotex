# Load a math font from an OTF file

Adds an OpenType math font, such as Latin Modern Math, for use as
`math_font`. The font can then also be used as a `fontfamily` for plot
text.

## Usage

``` r
load_math_font(otf_path)
```

## Arguments

- otf_path:

  Path to an OTF or TTF math font.

## Value

`NULL`, invisibly.

## Details

Only math fonts need loading. For text, set `gp$fontfamily` to any
installed font.

## See also

[`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md),
[`check_math_fonts`](https://adayim.github.io/gridmicrotex/reference/check_math_fonts.md),
[`latex_options`](https://adayim.github.io/gridmicrotex/reference/latex_options.md),
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)

## Examples

``` r
# \donttest{
  # The bundled STIX font stands in for your own math font here
  otf <- system.file("fonts", "STIXTwoMath-Regular.otf",
                     package = "gridmicrotex")
  load_math_font(otf)
  available_math_fonts()
#> [1] "DejaVu Sans"    "Lete Sans Math" "STIX Two Math" 
# }
```
