# List available math fonts

Returns the names of all math fonts currently loaded by MicroTeX. These
names can be passed to the `math_font` parameter of
[`latex_grob`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)
and
[`grid.latex`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md).

## Usage

``` r
available_math_fonts()
```

## Value

A character vector of math font names.

## Font pairing

The bundled math fonts have different styles. For a consistent look,
pair them with a matching `fontfamily` in `gp`:

|                                   |            |                         |
|-----------------------------------|------------|-------------------------|
| **Math font**                     | **Style**  | **Suggested text font** |
| Latin Modern Math (`"lm"`)        | Serif      | `"serif"`               |
| STIX Two Math (`"stix"`)          | Serif      | `"serif"`               |
| Lete Sans Math (`"lete"`)         | Sans-serif | `"sans"`                |
| TeX Gyre DejaVu Math (`"dejavu"`) | Sans-serif | `"sans"`                |

## Examples

``` r
available_math_fonts()
#> [1] "LatinModernMath-Regular"   "Lete Sans Math"           
#> [3] "STIX Two Math"             "TeXGyreDejaVuMath-Regular"
```
