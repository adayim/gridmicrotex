# Wrap the text of a label in `\text{}`

Turns a label that mixes text and math into pure math: text is wrapped
in `\text{}` and math is kept as it is.
[`latex_grob()`](https://adayim.github.io/gridmicrotex/reference/latex_grob.md)
does not need this; use it to pass a label to
`latex_grob(input_mode = "math")` or to another renderer that accepts
only math.

Math between `$...$`, `$$...$$`, `\(...\)` or `\[...\]`, and math
environments, are kept. A newline becomes a line break, and escaped
characters such as `\$` stay text.

## Usage

``` r
latex_wrap(tex, input_mode = c("mixed", "math"))
```

## Arguments

- tex:

  A character vector.

- input_mode:

  `"mixed"` (default) wraps the text. `"math"` returns `tex` unchanged.

## Value

A character vector the same length as `tex`.

## Examples

``` r
latex_wrap(r"(The equation \(E=mc^2\) is famous)")
#> [1] "\\text{The equation }E=mc^2\\text{ is famous}"
latex_wrap(r"(Cost: \$100 for $x$ items)")
#> [1] "\\text{Cost: \\$100 for }x\\text{ items}"
latex_wrap("Line 1\nLine 2")
#> [1] "\\text{Line 1}\\\\\\text{Line 2}"
latex_wrap(r"(\frac{\alpha}{\beta})", input_mode = "math")
#> [1] "\\frac{\\alpha}{\\beta}"
```
