# Set the default math font used by MicroTeX

Selects the default math font for formula layout. For most users, this
is the recommended way to switch fonts: choose one of the fonts returned
by
[`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md).

## Usage

``` r
set_math_font(name)
```

## Arguments

- name:

  Math font name or alias (e.g., `"lm"`, `"xits"`).

## Value

Invisibly returns `TRUE` on success.

## Details

This avoids dealing with external OTF/CLM files in normal usage. Use
[`load_font`](https://adayim.github.io/gridmicrotex/reference/load_font.md)
only when you need to add a custom font that is not already loaded.

## See also

[`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md),
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md),
[`load_font`](https://adayim.github.io/gridmicrotex/reference/load_font.md)

## Examples

``` r
# \donttest{
  # Switch default math font to XITS Math (if loaded)
  set_math_font("xits")
# }
```
