# Load a font

Registers a font under a name that works everywhere a font is named:
`gp = gpar(fontfamily = )`,
[`latex_options()`](https://adayim.github.io/gridmicrotex/reference/latex_options.md)
(`main_font`, `sans_font`, `mono_font`, `math_font`) and
`element_latex(family = )`. The font comes from a file or from an
installed family.

## Usage

``` r
load_font(x, name = NULL, bold = NULL, italic = NULL, bolditalic = NULL)
```

## Arguments

- x:

  A font file (`.otf`, `.ttf` or `.ttc`), or the name of an installed
  font family.

- name:

  The name to register the font under. The default is the font's family
  name.

- bold, italic, bolditalic:

  Files of the other faces of the same family, so that bold and italic
  text uses the real design. Only for a file: the faces of an installed
  family are found automatically. A face left out is drawn with the
  regular file.

## Value

The name the font is registered under, invisibly.

## Details

A math font loaded here can also be used as text. A font that is already
installed does not need loading to be used as `gp$fontfamily`; load it
to give it a short name, or to set it as a role in
[`latex_options()`](https://adayim.github.io/gridmicrotex/reference/latex_options.md).

The text of a loaded font is measured and drawn by gridmicrotex from the
font's own file, so it looks the same on every device, base
[`pdf()`](https://rdrr.io/r/grDevices/pdf.html) included, with its
kerning and ligatures. It is drawn as glyphs where the device has them
and as outlines where it has not (and for rotated text). Other families
are drawn by the device, as are right-to-left text and a character the
font has no glyph for, which the device may find in another font. In
base graphics with `latex_options(device_math = TRUE)` the device draws
all text, so it has to know the font itself.

## See also

[`available_fonts()`](https://adayim.github.io/gridmicrotex/reference/available_fonts.md),
[`latex_options()`](https://adayim.github.io/gridmicrotex/reference/latex_options.md)

## Examples

``` r
# \donttest{
  # The bundled STIX font stands in for your own font file here
  otf <- system.file("fonts", "STIXTwoMath-Regular.otf",
                     package = "gridmicrotex")
  load_font(otf, name = "My Font")
  available_fonts()
#>             name math  mono  bold italic weight
#> 1 Lete Sans Math TRUE FALSE FALSE  FALSE normal
#> 2  STIX Two Math TRUE FALSE FALSE  FALSE normal
#> 3        My Font TRUE FALSE FALSE  FALSE normal
#>                                                                         file
#> 1        /home/runner/work/_temp/Library/gridmicrotex/fonts/LeteSansMath.otf
#> 2 /home/runner/work/_temp/Library/gridmicrotex/fonts/STIXTwoMath-Regular.otf
#> 3 /home/runner/work/_temp/Library/gridmicrotex/fonts/STIXTwoMath-Regular.otf
#>    source
#> 1 bundled
#> 2 bundled
#> 3    file
# }
```
