# Position of a named point in a LaTeX grob

Returns the position of a `\mark{name}` written in the LaTeX, as grid
units that can be passed straight to other grid functions, for example
to point an arrow at part of a formula.

## Usage

``` r
grobMark(grob, name)
```

## Arguments

- grob:

  A grob from
  [`latex_grob()`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md).

- name:

  The name given to `\mark{}`.

## Value

A list with `x` and `y`, each a
[`grid::unit()`](https://rdrr.io/r/grid/unit.html). Rotation (`rot`) is
not taken into account.

## See also

[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)

## Examples

``` r
# \donttest{
  g <- latex_grob(r"($a\mark{eq}^2 = b + c^2$)",
                  x = grid::unit(0.5, "npc"),
                  y = grid::unit(0.5, "npc"))
  grid::grid.newpage(); grid::grid.draw(g)
  mk <- grobMark(g, "eq")
  grid::grid.points(mk$x, mk$y, pch = 19,
                    gp = grid::gpar(col = "red"))

# }
```
