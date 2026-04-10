# Set the default main (text) font for MicroTeX layout

Sets the font family that MicroTeX's internal layout engine uses when
computing the bounding box and position of non-math text (e.g., content
inside `\text{}`, `\mbox{}`). The font must have been previously loaded
via
[`load_font`](https://adayim.github.io/gridmicrotex/reference/load_font.md).

## Usage

``` r
set_main_font(family = "")
```

## Arguments

- family:

  Font family name (as returned by
  [`main_font_families`](https://adayim.github.io/gridmicrotex/reference/main_font_families.md)),
  or `""` to fall back to the math font for text layout.

## Value

Logical; `TRUE` if the font was set successfully.

## Two-layer font system

gridmicrotex uses two independent font systems:

- **MicroTeX (C++ layout engine)**:

  Controls *where* text is positioned and how much space it occupies in
  the formula. Changed via `set_main_font()`.

- **R grid (rendering)**:

  Controls *what the text looks like* on screen or in output. Uses
  system fonts available to R. Set via the `gp` parameter of
  [`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)
  (e.g., `gp = gpar(fontfamily = "serif")`).

When `gp$fontfamily` is set in
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md),
the package automatically queries R's font metrics for characters that
are not present in the loaded math font. For standard Latin text,
MicroTeX uses its own internal font metrics for layout, which closely
match typical system fonts.

In most cases, you do **not** need to call `set_main_font()` — just set
the desired font via `gp`.

Call `set_main_font()` only when you need MicroTeX's internal
character-level metrics (e.g., for ligature support from a specific
font) and you are not relying on `gp$fontfamily`.

## See also

[`load_font`](https://adayim.github.io/gridmicrotex/reference/load_font.md),
[`main_font_families`](https://adayim.github.io/gridmicrotex/reference/main_font_families.md),
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)

## Examples

``` r
# \donttest{
  # Reset to default (math font handles text)
  set_main_font("")
#> [1] TRUE
# }
```
