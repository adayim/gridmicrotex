# Load a LaTeX package file

`load_sty()` makes a file of your own definitions a package, so that
`\usepackage{name}` loads it in a label or a document, or
`latex_options(packages = "name")` loads it before every one.

## Usage

``` r
load_sty(file, name = NULL)
```

## Arguments

- file:

  Path of a `.sty` file.

- name:

  The name to load it by. By default the file's name without its
  extension. The name of a package that comes with gridmicrotex cannot
  be used.

## Value

The name, invisibly. Problems found in the file are given as a warning,
with their line and column.

## Details

The file is read as LaTeX reads a package: `\newcommand`,
`\renewcommand`, `\def`, `\newenvironment`, `\DeclareMathOperator`,
`\newtheorem` and the like define commands, and `@` may be used in a
name. Registers, boxes and `\catcode` are not supported. A file edited
after it was loaded is read again the next time something is drawn.

## See also

[`latex_options()`](https://adayim.github.io/gridmicrotex/reference/latex_options.md),
[`define_macro()`](https://adayim.github.io/gridmicrotex/reference/define_macro.md)

## Examples

``` r
sty <- tempfile(fileext = ".sty")
writeLines("\\newcommand{\\RR}{\\mathbb{R}}", sty)
name <- load_sty(sty, "mine")
grid::grid.newpage()
grid.latex("\\usepackage{mine}$f\\colon \\RR \\to \\RR$")
```
