# Create a text measurement closure for MicroTeX layout

Returns a function that measures text using R's grid graphics system.
The closure is called from C++ during `parse_latex_cpp()` to get
accurate font metrics for `\text{}` blocks.

## Usage

``` r
.make_text_measurer(text_gp)
```

## Arguments

- text_gp:

  A [`gpar`](https://rdrr.io/r/grid/gpar.html) object whose `fontfamily`
  is used for measurement. The face comes from MicroTeX's per-run
  `font_style`, not from `text_gp`.

## Value

A function taking `(text, font_style)` that returns
`c(width_ratio, ascent_ratio, height_ratio)` where ratios are relative
to the font size.
