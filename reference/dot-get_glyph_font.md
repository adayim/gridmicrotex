# Get or create a glyphFont object for a font file

Caches
[`grDevices::glyphFont()`](https://rdrr.io/r/grDevices/glyphInfo.html)
objects keyed by file path so that repeated calls for the same font file
reuse the same object.

## Usage

``` r
.get_glyph_font(font_file)
```

## Arguments

- font_file:

  Absolute path to the OTF/TTF font file.

## Value

A `glyphFont` object.
