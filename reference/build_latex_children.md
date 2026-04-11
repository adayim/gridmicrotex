# Build grid children from a MicroTeX layout data.frame

Converts each row of the layout data.frame into the appropriate grid
grob (pathGrob, segmentsGrob, rectGrob, textGrob, glyphGrob).

## Usage

``` r
build_latex_children(
  layout_df,
  total_h,
  depth = 0,
  text_gp = NULL,
  render_mode = "typeface"
)
```

## Arguments

- layout_df:

  Data.frame returned by `parse_latex_cpp()`.

- total_h:

  Total height of the formula (height + depth) in bigpts.

- depth:

  Depth below the baseline in bigpts (default 0).

- text_gp:

  Optional [`gpar`](https://rdrr.io/r/grid/gpar.html) for text grobs
  (from `\text{}` blocks). Controls fontfamily and fontface.

- render_mode:

  Character string: `"path"` or `"typeface"`. In typeface mode, glyph
  records are rendered via `glyphGrob`.

## Value

A [`grid::gList`](https://rdrr.io/r/grid/grid.grob.html) of child grobs.
