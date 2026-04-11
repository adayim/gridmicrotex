# Resolve a math font name

Translates short aliases (e.g., `"xits"`, `"lm"`) to the full MicroTeX
font name. Validates that the font is loaded.

## Usage

``` r
resolve_math_font(name)
```

## Arguments

- name:

  Font name or alias. Empty string uses the default font.

## Value

The resolved font name.
