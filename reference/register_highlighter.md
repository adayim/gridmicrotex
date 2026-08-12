# Add a syntax highlighting grammar

Registers a KDE/Kate syntax definition so that fenced code blocks tagged
with `lang` are highlighted by
[`markdown_box_grob`](https://adayim.github.io/gridmicrotex/reference/markdown_box_grob.md).
Ten languages are built in — see
[`available_highlighters`](https://adayim.github.io/gridmicrotex/reference/available_highlighters.md)
— and this is how to add another.

## Usage

``` r
register_highlighter(lang, file)
```

## Arguments

- lang:

  Language name, as written after the opening fence. Case is ignored.
  Registering a name that is already built in replaces it.

- file:

  Path to a KDE syntax XML grammar.

## Value

Invisibly, `lang`.

## Details

The grammar is an XML file in the format used by Kate, KDevelop and,
through skylighting, Pandoc. The simplest way to write one is to copy a
built-in grammar and edit it:


      file.copy(system.file("highlight", "python.xml",
                            package = "gridmicrotex"),
                "mylang.xml")

Because the format is KDE's, the several hundred definitions upstream
are a useful *reference* when writing your own — one XML file per
language, catalogued at <https://kate-editor.org/syntax/>, repository at
<https://invent.kde.org/frameworks/syntax-highlighting>.

Many load and work directly: of twenty sampled, thirteen registered,
including `python` (249 contexts), `css`, `yaml`, `makefile` and `go`.
The seven that did not — `bash`, `ruby`, `perl`, `rust`, `lua`,
`javascript` and `markdown` — all use *dynamic* rules, which substitute
part of the match into a later pattern and are not implemented.

Cross-language includes (`##Alerts`, `##Doxygen` and friends) are
skipped rather than followed. Those only add TODO/FIXME marks *inside* a
comment, so the comment is still a comment; the marks are all that is
lost.

They also carry their own licences, mostly GPL or LGPL, which is why
none is bundled with this MIT-licensed package. Borrowing from one
locally is your own decision; redistributing it is subject to its
licence.

Colours come from the `defStyleNum` of each `<itemData>`, which maps
onto the same CSS class names knitr and Pandoc use (`kw` for a keyword,
`co` for a comment, `st` for a string, and so on), so a grammar needs no
colour information of its own — restyle with
[`markdown_style`](https://adayim.github.io/gridmicrotex/reference/markdown_style.md).

At each position the current context's rules are tried in document order
and the first to match wins. Contexts, `IncludeRules`, `lookAhead` and
`fallthroughContext` all work, and the context stack carries across
lines, so a string or block comment may span them.

A rule this engine cannot represent is skipped when it would have stayed
in the current context — costing colour on the text it matched and
nothing more — but *refuses the grammar* when it would have switched,
because a lost context switch leaves the machine in the wrong state and
paints the rest of the file wrongly. The refusal names the construct.

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
