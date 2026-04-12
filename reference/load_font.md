# Load a font file into MicroTeX

Loads an OTF/TTF font into MicroTeX's internal font registry. The font
can then be used as a math font via the `math_font` parameter of
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md).

## Usage

``` r
load_font(otf_path, clm_path = NULL)
```

## Arguments

- otf_path:

  Path to the OTF/TTF font file.

- clm_path:

  Optional legacy metrics path. Usually leave as `NULL`.

## Value

Invisibly returns `NULL`.

## Details

For standard usage, supply only `otf_path`. The package handles the
remaining font loading details internally.

## Text fonts

Text inside `\text{}` is rendered using R's standard text-rendering
system. Control the font with `gp = gpar(fontfamily = "...")` in
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)
— no font loading required. This function is only needed for adding
custom **math** fonts.

## See also

[`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md),
[`set_math_font`](https://adayim.github.io/gridmicrotex/reference/set_math_font.md),
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)

## Examples

``` r
# \donttest{
  # Load a custom math font from OTF:
  # load_font("path/to/font.otf")
# }
```
