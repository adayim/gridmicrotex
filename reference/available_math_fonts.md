# List available math fonts

Returns the math fonts that can be passed to `math_font`.

## Usage

``` r
available_math_fonts()
```

## Value

A character vector of math font names.

## Font pairing

For a consistent look, pair each math font with a matching `fontfamily`
in `gp`:

|                                    |            |                         |
|------------------------------------|------------|-------------------------|
| **Math font**                      | **Style**  | **Suggested text font** |
| Lete Sans Math (`"lete"`, default) | Sans-serif | `"sans"`                |
| STIX Two Math (`"stix"`)           | Serif      | `"serif"`               |

Additional math fonts can be loaded with
[`load_math_font`](https://adayim.github.io/gridmicrotex/reference/load_math_font.md).

## Examples

``` r
available_math_fonts()
#> [1] "Lete Sans Math" "STIX Two Math" 
```
