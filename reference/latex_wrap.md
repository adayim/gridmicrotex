# Wrap standard text for math-first LaTeX renderers

Parses character strings to safely isolate standard natural language
from LaTeX math environments. Standard text is wrapped in `\text{}`
blocks, while equations, display math, and specific LaTeX environments
are preserved verbatim. This is heavily optimized for passing
mixed-content strings (like plot titles or axis labels) to pure-math
typesetting engines like MicroTex. The conversion is not perfect, but it
should handle most common cases without user intervention.

## Usage

``` r
latex_wrap(tex, input_mode = c("mixed", "math"))
```

## Arguments

- tex:

  `character`. The string or vector of strings to be processed.

- input_mode:

  `character`. A length-one character vector dictating the parsing
  strategy. If `"mixed"` (default), the string is tokenized and text is
  wrapped. If `"math"`, the parser is bypassed and the string is
  returned unmodified, assuming the user has provided a pure math
  equation.

## Value

A `character` vector of the same length as `tex`, formatted for
math-mode LaTeX rendering.

## Details

`latex_wrap()` operates as a state-machine tokenizer to ensure that
valid LaTeX math is not corrupted by the text-wrapping process. It
features:

- **Delimiter Preservation**: Standard inline (`$`, `\(`) and block
  (`$$`, `\[`) math delimiters are recognized and preserved.

- **Environment Tracking**: Complex nested environments (e.g.,
  `\begin{matrix}`) are safely extracted and bypassed.

- **Newline Conversion**: R newline characters (`\n`) occurring outside
  of math environments are automatically converted to LaTeX line breaks
  (`\\`) inside the `\text{}` wrapper.

- **Literal Escapes**: Escaped LaTeX literals (e.g., `\$`, `\%`, `\#`)
  are safely passed into the `\text{}` block without triggering math
  modes. The escape character for `\$` is automatically resolved for
  MicroTex compatibility.

## Examples

``` r
# "mixed" mode (default) safely wraps text and preserves inline math
latex_wrap(r"(The equation \(E=mc^2\) is famous)")
#> [1] "\\text{The equation }E=mc^2\\text{ is famous}"

# "mixed" mode handles user-escaped characters seamlessly
latex_wrap(r"(Cost: \$100 for $x$ items)")
#> [1] "\\text{Cost: \\$100 for }x\\text{ items}"

# "mixed" mode converts R newlines to stacked text blocks
latex_wrap(r"(Line 1\nLine 2)")
#> [1] "\\text{Line 1\\nLine 2}"

# "math" mode returns the string completely unmodified
latex_wrap(r"(\frac{\alpha}{\beta})", input_mode = "math")
#> [1] "\\frac{\\alpha}{\\beta}"
```
