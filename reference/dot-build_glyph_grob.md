# Build a single batched glyphGrob from collected glyph data

Creates a [`grid::glyphGrob`](https://rdrr.io/r/grid/grid.glyph.html)
containing all math glyphs, using
[`grDevices::glyphInfo()`](https://rdrr.io/r/grDevices/glyphInfo.html)
to describe glyph IDs, positions, sizes, colors, and fonts. Each unique
font file becomes an entry in the `glyphFontList`.

## Usage

``` r
.build_glyph_grob(ids, x, y, sizes, cols, font_files, depth = 0)
```

## Arguments

- ids:

  Integer vector of glyph IDs.

- x, y:

  Numeric vectors of glyph positions (already y-flipped, in bigpts).

- sizes:

  Numeric vector of font sizes.

- cols:

  Character vector of colors.

- font_files:

  Character vector of font file paths.

- depth:

  Depth below the baseline in bigpts (default 0).

## Value

A [`grid::glyphGrob`](https://rdrr.io/r/grid/grid.glyph.html) or `NULL`.
