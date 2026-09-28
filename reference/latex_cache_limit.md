# Layout cache

Recently drawn expressions are cached, so drawing the same one again is
fast. `latex_cache_limit()` sets how many are kept (512 by default),
`latex_cache_clear()` empties the cache, and `latex_cache_info()`
reports on it.

## Usage

``` r
latex_cache_limit(n = 512L)

latex_cache_clear()

latex_cache_info()
```

## Arguments

- n:

  Number of entries to keep. `0` turns the cache off.

## Value

`latex_cache_limit()` returns the previous limit and
`latex_cache_clear()` returns `NULL`, both invisibly.
`latex_cache_info()` returns a list with `size`, `max_size`, `hits` and
`misses`.

## See also

[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md),
[`latex_options`](https://adayim.github.io/gridmicrotex/reference/latex_options.md)

## Examples

``` r
# \donttest{
  latex_cache_limit(256)
  grid.latex("$e^{i\\pi} + 1 = 0$")

  latex_cache_info()
#> $size
#> [1] 18
#> 
#> $max_size
#> [1] 256
#> 
#> $hits
#> [1] 0
#> 
#> $misses
#> [1] 18
#> 
  latex_cache_clear()
# }
```
