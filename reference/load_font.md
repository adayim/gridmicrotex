# Load a font file into MicroTeX

Loads an OTF/TTF font into MicroTeX's internal font registry. The font
can then be used as a math font (via the `math_font` parameter of
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md))
or as a main text font for layout (via
[`set_main_font`](https://adayim.github.io/gridmicrotex/reference/set_main_font.md)).

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

## Font availability in R

This function loads the font into MicroTeX's C++ layout engine only. It
does **not** make the font available to R's graphics system. To render
`\text{}` content in a specific font, set
`gp = gpar(fontfamily = "...")` in
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md).
Packages like showtext or systemfonts can be used to make additional
fonts available to R's graphics devices.

For most workflows, prefer choosing from built-in/loaded math fonts with
[`set_math_font`](https://adayim.github.io/gridmicrotex/reference/set_math_font.md)
and
[`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md).
Use `load_font()` only for custom fonts.

## See also

[`set_main_font`](https://adayim.github.io/gridmicrotex/reference/set_main_font.md),
[`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md),
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)

## Examples

``` r
# \donttest{
  # Load a custom math font from OTF:
  # load_font("path/to/font.otf")
# }
```
