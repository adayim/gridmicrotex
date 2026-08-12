# Syntax highlighting languages available

Names accepted after an opening code fence in
[`markdown_box_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md).
Includes both the built-in grammars and any added with
[`register_highlighter`](https://adayim.github.io/gridmicrotex/reference/register_highlighter.md).

## Usage

``` r
available_highlighters()
```

## Value

A character vector of language names, sorted.

## Details

Several common aliases also work and are not listed here, among them
`py`, `sh`, `c++`, `yml`, `jl` and `tex`. A fence naming anything else
renders as plain monospace text, without a warning.

## See also

[`register_highlighter`](https://adayim.github.io/gridmicrotex/reference/register_highlighter.md)

## Examples

``` r
available_highlighters()
#>  [1] "bash"   "cpp"    "json"   "julia"  "latex"  "python" "r"      "sql"   
#>  [9] "stan"   "yaml"  
```
