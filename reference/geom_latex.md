# A ggplot2 geom for LaTeX math labels

Renders LaTeX math expressions as native grid grobs within a ggplot2
plot. Each label is parsed and laid out by MicroTeX, producing
resolution-independent vector output.

## Usage

``` r
geom_latex(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  fontsize = 11,
  math_font = "",
  line_space = 10,
  max_width = 0,
  render_mode = c("typeface", "path"),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- ...:

  Other arguments passed to
  [`layer`](https://ggplot2.tidyverse.org/reference/layer.html).

- fontsize:

  Default font size in points. Overridden by the `size` aesthetic if
  mapped.

- math_font:

  Name of the math font to use (e.g., `"stix"`).

- line_space:

  Inter-line spacing in big points for multi-line formulas (default:
  10).

- max_width:

  Maximum width in big points for automatic line wrapping (default: 0,
  no wrapping).

- render_mode:

  Character string: `"typeface"` (default) renders glyphs as native text
  using the math font, producing selectable/accessible text in PDF and
  SVG output. Requires the math font to be installed on the system.
  Falls back to path mode automatically on devices that do not support
  font embedding (e.g., the base
  [`pdf()`](https://rdrr.io/r/grDevices/pdf.html) device). `"path"`
  renders math symbols as filled vector paths (works on all devices but
  text is not selectable in PDF/SVG). For PDF output with
  embedded/selectable text, prefer
  [`cairo_pdf`](https://rdrr.io/r/grDevices/cairo.html).

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

## Value

A ggplot2 layer.

## Aesthetics

`geom_latex()` understands the following aesthetics (required aesthetics
are in bold):

- **`x`**

- **`y`**

- **`label`** — LaTeX math string

- `size` — font size in points (default: 11)

- `colour` — text colour (default: `"black"`)

- `angle` — rotation angle in degrees (default: 0)

- `hjust` — horizontal justification, 0–1 (default: 0.5)

- `vjust` — vertical justification, 0–1 (default: 0.5)

- `alpha` — transparency (default: 1)

## Examples

``` r
# \donttest{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  df <- data.frame(
    x = 1:3, y = 1:3,
    eq = c("x^2", "\\frac{a}{b}", "\\sum_{i=1}^n x_i")
  )
  ggplot(df, aes(x, y, label = eq)) + geom_latex()
}

# }
```
