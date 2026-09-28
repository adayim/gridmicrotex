# Syntax highlighting languages available

Languages that can follow an opening code fence in
[`markdown_box_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md),
including those added with
[`register_highlighter()`](https://adayim.github.io/gridmicrotex/reference/register_highlighter.md).
Aliases such as `py`, `sh`, `c++`, `yml`, `jl` and `tex` also work.
Other languages are shown as plain code.

## Usage

``` r
available_highlighters()
```

## Value

A sorted character vector.

## See also

[`register_highlighter`](https://adayim.github.io/gridmicrotex/reference/register_highlighter.md)

## Examples

``` r
available_highlighters()
#>  [1] "bash"   "cpp"    "json"   "julia"  "latex"  "python" "r"      "sql"   
#>  [9] "stan"   "yaml"  
```
