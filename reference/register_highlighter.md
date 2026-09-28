# Add a syntax highlighting grammar

Adds a language for highlighting fenced code blocks in
[`markdown_box_grob()`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md),
from a KDE syntax file (the format used by Kate and Pandoc). The easiest
start is a copy of a built-in grammar:

## Usage

``` r
register_highlighter(lang, file)
```

## Arguments

- lang:

  Language name, as written after the opening fence. Case is ignored. A
  built-in language of the same name is replaced.

- file:

  Path to a KDE syntax XML file.

## Value

`lang`, invisibly.

## Details


      file.copy(system.file("highlight", "python.xml",
                            package = "gridmicrotex"),
                "mylang.xml")

Many files from <https://kate-editor.org/syntax/> work as they are.
Files that use *dynamic* rules are refused with an error. Those files
are mostly GPL or LGPL licensed; check the licence before you
redistribute one.

Colours come from
[`markdown_style()`](https://adayim.github.io/gridmicrotex/reference/markdown_style.md),
using Pandoc's class names (`kw` keyword, `co` comment, `st` string,
...), so a grammar needs no colours of its own.

## See also

[`available_highlighters`](https://adayim.github.io/gridmicrotex/reference/available_highlighters.md),
[`markdown_box_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md),
[`markdown_style`](https://adayim.github.io/gridmicrotex/reference/markdown_style.md)

## Examples

``` r
# Registering a grammar under a name of your own.
f <- system.file("highlight", "python.xml", package = "gridmicrotex")
register_highlighter("mypython", f)
"mypython" %in% available_highlighters()
#> [1] TRUE
```
