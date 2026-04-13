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

  Optional path to the companion `.clm2` (or legacy `.clm1`) metrics
  file. If `NULL` (the default), `load_font()` searches for a file with
  the same stem as `otf_path`.

## Value

Invisibly returns `NULL`.

## Details

For standard usage, supply only `otf_path`. If a matching `.clm2` file
is present next to the OTF, it is used automatically. Otherwise you must
generate one first (see *Generating a CLM metrics file* below).

## Text fonts

Text inside `\text{}` is rendered using R's standard text-rendering
system. Control the font with `gp = gpar(fontfamily = "...")` in
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)
— no font loading required. This function is only needed for adding
custom **math** fonts.

## Generating a CLM metrics file

MicroTeX reads glyph metrics, the OpenType MATH table, and glyph
outlines from a binary companion file (`.clm2`) rather than from the OTF
directly. A converter script is bundled with the package at
`system.file("otf2clm.py", package = "gridmicrotex")`. It requires
FontForge's embedded Python (`ffpython`), since the standard Python does
not ship the `fontforge` module.

Typical usage from a shell, for a single font:

    ffpython otf2clm.py --single path/to/font.otf true out_dir

Or to convert every OTF in a directory:

    ffpython otf2clm.py --batch in_dir true out_dir

The `true` argument tells the converter to embed glyph outlines
(required for `render_mode = "path"`); pass `false` to generate a
smaller `.clm1` file suitable only for `render_mode = "typeface"`. Run
the script with no arguments to see the full usage including font-style
flags.

Place the generated `.clm2` file next to the OTF (same stem) so that
`load_font()` finds it automatically, or pass it explicitly via
`clm_path`.

## See also

[`available_math_fonts`](https://adayim.github.io/gridmicrotex/reference/available_math_fonts.md),
[`set_math_font`](https://adayim.github.io/gridmicrotex/reference/set_math_font.md),
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)

## Examples

``` r
# \donttest{
  # Load a custom math font from OTF (with a sibling .clm2 file):
  # load_font("path/to/font.otf")

  # Locate the bundled CLM converter:
  # system.file("otf2clm.py", package = "gridmicrotex")
# }
```
