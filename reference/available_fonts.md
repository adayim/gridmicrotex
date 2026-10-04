# List the fonts that have been loaded

One row per font that can be named: the bundled math fonts, and every
font given to
[`load_font()`](https://adayim.github.io/gridmicrotex/reference/load_font.md)
or to a font option of
[`latex_options()`](https://adayim.github.io/gridmicrotex/reference/latex_options.md).
The math fonts, which `math_font` takes, are the rows with `math`
`TRUE`.

## Usage

``` r
available_fonts(system = FALSE)
```

## Arguments

- system:

  If `TRUE`, also list the installed font families, which work by name
  without loading. The first call scans the system's fonts, which takes
  a few seconds.

## Value

A data frame with one row per font: `name`; `math` (it has a math table;
`NA` for an installed family that is not loaded); `mono` (monospaced);
`bold` and `italic` (a face of its own is registered, so `\textbf` and
`\textit` draw its real design); `weight` of the regular face; `file` of
the regular face; and `source`, one of `"bundled"`, `"file"` and
`"system"`.

## Font pairing

For a consistent look, pair a math font with a matching `fontfamily` in
`gp`:

|                                    |            |                         |
|------------------------------------|------------|-------------------------|
| **Math font**                      | **Style**  | **Suggested text font** |
| Lete Sans Math (`"lete"`, default) | Sans-serif | `"sans"`                |
| STIX Two Math (`"stix"`)           | Serif      | `"serif"`               |

## See also

[`load_font()`](https://adayim.github.io/gridmicrotex/reference/load_font.md),
[`latex_options()`](https://adayim.github.io/gridmicrotex/reference/latex_options.md)

## Examples

``` r
available_fonts()
#>             name math  mono  bold italic weight
#> 1 Lete Sans Math TRUE FALSE FALSE  FALSE normal
#> 2  STIX Two Math TRUE FALSE FALSE  FALSE normal
#>                                                                         file
#> 1        /home/runner/work/_temp/Library/gridmicrotex/fonts/LeteSansMath.otf
#> 2 /home/runner/work/_temp/Library/gridmicrotex/fonts/STIXTwoMath-Regular.otf
#>    source
#> 1 bundled
#> 2 bundled
```
