# Convert path segments to a grid pathGrob

Convert path segments to a grid pathGrob

## Usage

``` r
build_path_grob(path_data, col, idx, total_h)
```

## Arguments

- path_data:

  List with `cmd` and `coords` elements.

- col:

  Fill color.

- idx:

  Index for naming.

- total_h:

  Total height for y-axis flipping.

## Value

A [`grid::pathGrob`](https://rdrr.io/r/grid/grid.path.html) or `NULL`.
