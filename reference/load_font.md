# Load a math font from an OTF file

Loads an OTF/TTF math font into MicroTeX's internal font registry. The
font's OpenType MATH table is parsed directly in C++ and the required
metrics are synthesised on the fly. You can download free math fonts
like Latin Modern Math (default math fonts in LaTeX) and load it with
`load_font()` to use it for math rendering.

## Usage

``` r
load_font(otf_path)
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

## Text fonts

This function is only for **math** fonts (fonts with an OpenType MATH
table). Plain text fonts used inside `\text{}` blocks are resolved
automatically by systemfonts from the `gp$fontfamily` argument — no
`load_font()` call required.

## See also

[`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md),
[`latex_options`](https://adayim.github.io/gridmicrotex/reference/latex_options.md),
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)

## Examples

``` r
# \donttest{
  # We will download and load Latin Modern Math
  url <- "https://mirrors.ctan.org/fonts/lm-math/opentype/latinmodern-math.otf"
  math_fnt <- file.path(tempdir(), "latinmodern-math.otf")
  download.file(url = url, destfile = math_fnt, mode = "wb")
  load_font(math_fnt)

  available_math_fonts()
#> [1] "DejaVu Sans"       "Latin Modern Math" "Lete Sans Math"   
#> [4] "STIX Two Math"    
# }

```
