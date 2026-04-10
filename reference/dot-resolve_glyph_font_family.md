# Resolve an OTF font file path to an R font family name

Looks up the font's actual family name from the system font database
(via systemfonts). If the font is installed, the real family name (e.g.,
`"Latin Modern Math"`) is used so that all R graphics devices recognise
it. Falls back to registering the font via systemfonts and/or sysfonts +
showtext.

## Usage

``` r
.resolve_glyph_font_family(font_file)
```

## Arguments

- font_file:

  Absolute path to the OTF/TTF font file.

## Value

Character string giving the R font family name.
