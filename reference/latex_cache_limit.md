# Set the maximum number of entries kept in the LaTeX layout cache

The cache stores parsed layout information for recently rendered LaTeX
expressions, keyed by the expression and relevant rendering parameters
(font, size, macros, etc.). This speeds up repeated rendering of the
same expressions, especially in loops or interactive sessions. The
default limit is 512 entries, which should be sufficient for most use
cases. When the limit is exceeded, the least recently used entries are
automatically evicted.

## Usage

``` r
latex_cache_limit(n = 512L)

latex_cache_clear()

latex_cache_info()
```

## Arguments

- n:

  Non-negative integer cache capacity. Default is 512. Set to `0` to
  disable caching.

## Value

- `latex_cache_limit`: Invisibly returns the previous limit.

- `latex_cache_clear`: Invisibly returns `NULL`.

- `latex_cache_info`: A list with elements `size` (entries currently
  stored), `max_size`, `hits`, and `misses`.

## See also

[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md),
[`latex_options`](https://adayim.github.io/gridmicrotex/reference/latex_options.md)

## Examples

``` r
# \donttest{
  latex_cache_limit(256)
  grid.latex("e^{i\\pi} + 1 = 0")

  latex_cache_info()
#> $size
#> [1] 10
#> 
#> $max_size
#> [1] 256
#> 
#> $hits
#> [1] 0
#> 
#> $misses
#> [1] 10
#> 
  latex_cache_clear()
# }
```
