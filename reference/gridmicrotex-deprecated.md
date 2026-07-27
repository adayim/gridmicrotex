# Deprecated functions

These functions still work but will be removed in a future release. Both
were renamed to say what they actually operate on: only **math** fonts
(those carrying an OpenType MATH table) are ever registered with
MicroTeX. Text fonts need no loading — they are resolved on demand by
systemfonts from `gp$fontfamily`.

## Usage

``` r
load_font(otf_path)

check_fonts()
```

## Arguments

- otf_path:

  Path to the OTF/TTF font file.

## Value

As the replacement function.

## Details

|  |  |
|----|----|
| **Deprecated** | **Use instead** |
| `load_font()` | [`load_math_font()`](https://adayim.github.io/gridmicrotex/reference/load_math_font.md) |
| `check_fonts()` | [`check_math_fonts()`](https://adayim.github.io/gridmicrotex/reference/check_math_fonts.md) |
