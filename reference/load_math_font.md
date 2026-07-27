# Load a math font from an OTF file

Loads an OTF/TTF **math** font — one carrying an OpenType MATH table —
into MicroTeX's internal font registry. The MATH table is parsed
directly in C++ and the required metrics are synthesised on the fly. You
can download a free math font such as Latin Modern Math (the LaTeX
default) and load it for math rendering.

## Usage

``` r
load_math_font(otf_path)
```

## Arguments

- otf_path:

  Path to the OTF/TTF font file.

## Value

Invisibly returns `NULL`.

## Details

The font is also registered with the systemfonts package so it can be
selected for surrounding plot text via `gp = gpar(fontfamily = "...")`
without being installed system-wide.

Plain **text** fonts — those used inside `\text{}` blocks — need no
loading at all. They are resolved automatically by systemfonts from
`gp$fontfamily`, or per run with `\gmfontfamily{}{}`.

## See also

[`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md),
[`check_math_fonts`](https://adayim.github.io/gridmicrotex/reference/check_math_fonts.md),
[`latex_options`](https://adayim.github.io/gridmicrotex/reference/latex_options.md),
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)

## Examples

``` r
# \donttest{
  # Load a math font from a local OTF file. Here we point at the
  # bundled STIX font so the example is self-contained and loaded.
  # You don't need to load the bundled fonts to use them — they're registered
  # with systemfonts on first render — but this shows how to load a custom font.
  # in practice you would pass the path to any OTF with an OpenType MATH table.
  otf <- system.file("fonts", "STIXTwoMath-Regular.otf",
                     package = "gridmicrotex")
  load_math_font(otf)
  available_math_fonts()
#> [1] "DejaVu Sans"    "Lete Sans Math" "STIX Two Math" 
# }
```
