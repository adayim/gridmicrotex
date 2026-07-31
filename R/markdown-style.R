# Styling for markdown rendering: a small CSS cascade.
#
# Everything that can style a block or a run compiles to one internal
# representation -- an ordered list of rules, each a selector, its
# specificity, and a named list of CSS properties:
#
#   markdown_style(h1 = md_style(...))   R constructor
#   markdown_style(css = "...")          a stylesheet, text or a .css file
#   <div style=|class=>                  one chunk, in the markdown
#   <span style=|class=>                 one inline run
#
# Resolution is CSS's own: highest specificity wins, ties broken by
# document order, and inheritable properties fall through into nested
# containers while box properties do not.
#
# The property set is deliberately small -- only what MicroTeX and grid
# can actually honour. Anything else parses and is then ignored, which is
# what CSS itself does and what makes pasting a real stylesheet safe.

# Properties md_style() accepts, in CSS spelling. R arguments use
# underscores (font_size) and are translated on the way in.
.MD_STYLE_PROPS <- c(
  # inline-capable: compiled to LaTeX commands by .md_css_inline_latex()
  "color", "font-size", "font-family", "font-weight", "font-style",
  "text-decoration", "border", "border-style", "border-radius",
  "box-shadow", "visibility", "vertical-align", "transform",
  # block-only: read by the layout. `padding` and `margin` are the CSS
  # shorthands, expanded into the four longhands by .md_expand_shorthand().
  "line-height", "margin", "margin-top", "margin-bottom", "margin-left",
  "margin-right", "padding", "padding-left", "padding-right", "padding-top",
  "padding-bottom", "text-align", "border-left", "border-top", "height",
  # tables: background is read on tr/td/th, the rest on table/tr/td
  "background", "border-bottom", "border-color", "table-layout",
  # no sensible CSS spelling here
  "bullet", "marker-gap"
)

# The properties CSS says are inherited. Box properties are absent on
# purpose: a margin on a blockquote must not become a margin on every
# paragraph inside it.
.MD_INHERITED_PROPS <- c(
  "color", "font-size", "font-family", "font-weight", "font-style",
  "line-height", "text-align"
)

# Built-in defaults, specificity 0. Every value here is the constant it
# replaces in .md_layout(), so the default style reproduces the rendering
# that predates this file.
#
# Absent on purpose: the blockquote bar colour and the rule's colour and
# thickness. Those resolve from gp at draw time (gp$col %||% "grey60" and
# gp$col %||% "black"), a quirk older than this file which a default here
# would silently change.
.md_default_rules <- function() {
  # The heading ladder, measured from the \Huge..\normalsize macros it
  # replaces rather than guessed.
  h <- c(2.5, 2, 1.75, 17 / 12, 7 / 6, 1)
  d <- list(
    p          = list("margin-top" = 0.55),
    # A `$$...$$` paragraph. Centred, as display math is everywhere else.
    math       = list("margin-top" = 0.55, "text-align" = "center"),
    # A link cannot be clicked in a grob, but it should still look like
    # one. Blue and underlined is what a browser does; the LaTeX side
    # follows hyperref's convention instead -- see .strip_document_wrappers().
    a          = list(color = "#0969DA", "text-decoration" = "underline"),
    # The footnote section at the foot of the document: smaller, and set
    # off from the body above it.
    footnote   = list("margin-top" = 0.3, "font-size" = 0.85),
    blockquote = list("margin-top" = 0.55, "padding-left" = 0.75,
                      "border-left" = 0.125),
    pre        = list("margin-top" = 0.55, "line-height" = 1.15,
                      "font-family" = "mono"),
    table      = list("margin-top" = 0.55),
    # A header row is bold everywhere else markdown is rendered. It could
    # not be before: \textbf{\text{x}} reports plain, so the emphasis has
    # to be applied when the cell content is generated -- see
    # .md_table_tex(), which is why this is a `th` rule and not a wrapper.
    th         = list("font-weight" = "bold"),
    img        = list("margin-top" = 0.55),
    hr         = list("margin-top" = 0.55, "height" = 0.5),
    ul         = list("margin-top" = 0.55, "marker-gap" = 0.4,
                      "bullet" = "\\bullet"),
    ol         = list("margin-top" = 0.55, "marker-gap" = 0.4),
    li         = list("margin-top" = 0.55)
  )
  for (i in seq_along(h)) {
    d[[paste0("h", i)]] <- list("margin-top" = 0.95, "font-size" = h[i],
                                "font-weight" = "bold")
  }
  unname(mapply(.md_rule, names(d), d, SIMPLIFY = FALSE))
}

# A selector's specificity: a class beats a tag, an inline style beats
# both. The numbers are CSS's, scaled down to the two levels we support.
.md_specificity <- function(selector) {
  if (startsWith(selector, ".")) 10L else 1L
}

.md_rule <- function(selector, props) {
  list(selector = selector, specificity = .md_specificity(selector),
       props = props)
}

# --- md_style() ------------------------------------------------------

#' Declarations for one markdown tag
#'
#' @description
#' A set of CSS declarations, for use as a named argument to
#' \code{\link{markdown_style}}. Argument names are the CSS property
#' names with underscores in place of hyphens, so \code{font_size} sets
#' \code{font-size}.
#'
#' @details
#' Lengths accept three forms: a bare number is \code{rem}, a multiple of
#' the body font size (\code{font_size = 2.5}); a string is whatever CSS
#' says it is (\code{"2.5em"}, \code{"12pt"}, \code{"150\%"}); and a
#' \code{\link[grid]{unit}} is absolute.
#'
#' These are the supported properties, and where each one has an effect.
#' \emph{Inline} means it also works on a \code{<span>} and in
#' \code{\link{markdown_grob}}, which has no block layout; \emph{block}
#' means it needs \code{\link{markdown_box_grob}}.
#'
#' \tabular{lll}{
#'   \strong{property} \tab \strong{scope} \tab \strong{notes} \cr
#'   \code{color} \tab inline + block \tab \cr
#'   \code{font_size} \tab inline + block \tab \cr
#'   \code{font_family} \tab inline + block \tab \cr
#'   \code{font_weight} \tab inline + block \tab prose blocks only, see below \cr
#'   \code{font_style} \tab inline + block \tab prose blocks only, see below \cr
#'   \code{text_decoration} \tab inline + block \tab \code{underline},
#'     \code{overline}, \code{line-through} \cr
#'   \code{background} \tab inline + block \tab a fill behind the text \cr
#'   \code{border} \tab inline \tab a frame; the inset is fixed \cr
#'   \code{border_style} \tab inline \tab only \code{double} \cr
#'   \code{border_radius} \tab inline \tab rounds the frame \cr
#'   \code{box_shadow} \tab inline \tab \cr
#'   \code{visibility} \tab inline \tab \code{hidden} keeps the space \cr
#'   \code{vertical_align} \tab inline \tab \code{super}, \code{sub},
#'     or a length \cr
#'   \code{transform} \tab inline \tab \code{rotate()}, \code{scale()},
#'     \code{scaleX(-1)} \cr
#'   \code{line_height} \tab block \tab unitless, as \code{gpar()} wants it \cr
#'   \code{margin_top}, \code{margin_bottom} \tab block \tab margins do not
#'     collapse \cr
#'   \code{margin_left}, \code{margin_right} \tab block \tab \cr
#'   \code{padding_left}, \code{padding_right} \tab block \tab \cr
#'   \code{padding_top}, \code{padding_bottom} \tab block \tab \cr
#'   \code{margin}, \code{padding} \tab block \tab the CSS shorthand: one
#'     to four lengths, in CSS's order \cr
#'   \code{text_align} \tab block \tab \code{left}, \code{center},
#'     \code{right} \cr
#'   \code{border_left} \tab block \tab the \code{blockquote} bar \cr
#'   \code{border_top} \tab block \tab the \code{hr} rule \cr
#'   \code{border_bottom} \tab \code{tr} \tab a rule under each table row \cr
#'   \code{border_color} \tab \code{table} \tab colour of the table's rules \cr
#'   \code{table_layout} \tab \code{table} \tab \code{fixed} divides the
#'     width between the columns so a wide table wraps \cr
#'   \code{height} \tab block \tab the band an \code{hr} sits in \cr
#'   \code{bullet} \tab \code{ul} \tab raw LaTeX for the marker glyph \cr
#'   \code{marker_gap} \tab \code{ul}, \code{ol} \tab marker to text \cr
#' }
#'
#' \code{font_size} also accepts CSS's keywords --- \code{xx-small} through
#' \code{xx-large}, plus \code{smaller} and \code{larger} --- taken from the
#' \code{\\tiny}..\code{\\Huge} ladder MicroTeX implements.
#'
#' \strong{The \code{body} rule styles the box itself.} On any other tag,
#' \code{background}, \code{border}, \code{border_radius}, \code{padding}
#' and \code{margin} apply to that block. On \code{body} they apply to the
#' whole \code{\link{markdown_box_grob}} --- its fill, its frame, its
#' corner radius, and the space inside and outside it. That is the only
#' way to give a \code{\link{element_markdown}} title a background, since
#' the theme element takes no box arguments of its own:
#'
#' \preformatted{body \{ background: grey95; padding: 8px;
#'         border: 1px solid grey60; border-radius: 4px \}}
#'
#' An explicit \code{box_gp}, \code{padding}, \code{margin} or \code{r}
#' argument to \code{markdown_box_grob()} wins over the rule, the way an
#' inline style wins in CSS.
#'
#' Anything else is an error --- unlike a pasted stylesheet, where an
#' unknown property is ignored the way a browser ignores it.
#'
#' \strong{One limitation worth knowing.} \code{font_weight} and
#' \code{font_style} apply to blocks whose content is prose --- paragraphs,
#' headings, list items, block quotes, table cells and \code{<div>}s. They
#' do \emph{not} apply to \code{pre} or an image's alt text, which build
#' their own LaTeX and impose their own font handling. This is a MicroTeX
#' constraint rather than a choice: \code{\\text\{\}} resets the font
#' style, so emphasis has to be decided when the content is generated,
#' not wrapped around it afterwards.
#'
#' \strong{What cannot be styled at all.} There is no small-caps
#' (\code{\\textsc} is not a MicroTeX command), no
#' \code{font-variant-numeric}, no right-to-left or bidirectional text,
#' and no padding inside an inline \code{border} --- MicroTeX has no
#' \code{\\fboxsep}, so that inset is fixed.
#'
#' @param ... Named declarations.
#' @return An object of class \code{gridmicrotex_md_style}.
#' @seealso \code{\link{markdown_style}}, \code{\link{markdown_box_grob}}
#' @export
#'
#' @examples
#' md_style(color = "steelblue", font_size = 2.5, margin_top = 1.2)
md_style <- function(...) {
  props <- list(...)
  if (length(props) == 0L) return(structure(list(), class = "gridmicrotex_md_style"))
  nm <- names(props)
  if (is.null(nm) || any(!nzchar(nm))) {
    stop("All arguments to `md_style()` must be named.", call. = FALSE)
  }
  # R spells them with underscores; CSS with hyphens. Accept either.
  nm <- gsub("_", "-", tolower(nm), fixed = TRUE)
  bad <- setdiff(nm, .MD_STYLE_PROPS)
  if (length(bad)) {
    stop("Unsupported ", if (length(bad) > 1L) "properties: " else "property: ",
         paste(sQuote(gsub("-", "_", bad, fixed = TRUE)), collapse = ", "),
         ".\nSupported: ",
         paste(gsub("-", "_", .MD_STYLE_PROPS, fixed = TRUE), collapse = ", "),
         call. = FALSE)
  }
  names(props) <- nm
  # A NULL means "say nothing", which is how a tag opts out of a default.
  props <- props[!vapply(props, is.null, logical(1))]
  structure(.md_expand_shorthand(props), class = "gridmicrotex_md_style")
}

#' @export
print.gridmicrotex_md_style <- function(x, ...) {
  if (length(x) == 0L) {
    cat("<md_style: empty>\n")
  } else {
    cat("<md_style>\n")
    for (p in names(x)) cat(sprintf("  %-16s %s\n", p, .md_fmt_value(x[[p]])))
  }
  invisible(x)
}

.md_fmt_value <- function(v) {
  if (grid::is.unit(v)) return(paste(format(v), collapse = ", "))
  paste(format(v), collapse = ", ")
}

# --- stylesheet parsing ----------------------------------------------

# Read `css` as a file when it names one, as CSS text otherwise. A
# stylesheet always contains a brace, so the two are never ambiguous in
# practice, but the file test comes first and settles it either way.
.md_read_css <- function(css) {
  if (!is.character(css)) {
    stop("`css` must be a character string of CSS, or a path to a .css file.",
         call. = FALSE)
  }
  if (length(css) == 1L && !grepl("{", css, fixed = TRUE) && file.exists(css)) {
    return(paste(readLines(css, warn = FALSE), collapse = "\n"))
  }
  paste(css, collapse = "\n")
}

# Parse a stylesheet into rules. Selectors we cannot honour -- anything
# with a combinator, pseudo-class, attribute or id -- are skipped rather
# than raised, so a real-world stylesheet can be pasted in and the parts
# that do apply still work.
.md_parse_stylesheet <- function(text) {
  text <- gsub("/[*].*?[*]/", "", text)          # strip comments
  chunks <- regmatches(text, gregexpr("[^{}]+[{][^{}]*[}]", text))[[1]]
  rules <- list()
  for (ch in chunks) {
    i <- regexpr("{", ch, fixed = TRUE)
    if (i < 1L) next
    sels <- trimws(strsplit(substr(ch, 1L, i - 1L), ",", fixed = TRUE)[[1]])
    props <- .md_parse_css(substr(ch, i + 1L, nchar(ch) - 1L))
    if (length(props) == 0L) next
    for (s in sels) {
      s <- tolower(trimws(s))
      # A bare tag or a single class. At-rules and everything more
      # elaborate than that are silently ignored.
      if (!grepl("^[.]?[a-z][a-z0-9_-]*$", s)) next
      rules[[length(rules) + 1L]] <- .md_rule(s, props)
    }
  }
  rules
}

# --- markdown_style() ------------------------------------------------

#' A style for markdown rendering
#'
#' @description
#' Builds the style used by \code{\link{markdown_box_grob}} and
#' \code{\link{markdown_grob}}: a small CSS cascade over the markdown
#' tags. One constructor covers creating a style, starting from a preset,
#' and extending an existing one. For the properties themselves, and
#' which are honoured where, see \code{\link{md_style}}.
#'
#' @details
#' Tags are named as in HTML, so a stylesheet reads the way a CSS author
#' expects: \code{body} (the document root, which every other tag
#' inherits from --- and which also styles the box itself, see
#' \code{\link{md_style}}), \code{p}, \code{h1} ... \code{h6}, \code{ul},
#' \code{ol}, \code{li}, \code{blockquote}, \code{pre} (a code block),
#' \code{code} (an inline code span), \code{strong} and \code{em} (what
#' markdown's \code{**} and \code{*} produce), \code{table}, \code{tr},
#' \code{td}, \code{th}, \code{hr}, \code{img}, \code{a} (a link),
#' \code{math} (a paragraph that is nothing but \code{$$...$$}),
#' \code{footnote}, \code{div} and \code{span}.
#'
#' The table tags nest as they do in HTML: \code{tr}, \code{td} and
#' \code{th} inherit through \code{table}, so \code{table \{ color: \}}
#' reaches the cells. \code{background} on \code{tr} fills the row, on
#' \code{td}/\code{th} the individual cell.
#'
#' Four things can style a document, and they resolve in CSS's own order:
#' the built-in defaults, then a type selector (\code{h1}), then a class
#' selector (\code{.note}), then an inline \code{style} attribute. Ties
#' are broken by document order, so a later rule wins. Inheritable
#' properties (\code{color}, the \code{font-*} family,
#' \code{line-height}, \code{text-align}) fall through into nested
#' containers, so \code{blockquote \{ color: grey40 \}} greys everything
#' quoted; box properties (margins, padding, borders) do not.
#'
#' The supported CSS is a deliberately small subset: type selectors,
#' class selectors and selector lists (\code{h1, h2}). Combinators,
#' pseudo-classes, attribute selectors, \code{#id} and at-rules are
#' skipped rather than raised, so an existing stylesheet can be handed
#' over and the parts that apply still take effect.
#'
#' @param base \code{NULL} (default) for the built-in defaults, the name
#'   of a bundled preset such as \code{"github"}, or an existing
#'   \code{markdown_style} to extend.
#' @param css A stylesheet: CSS text, or a path to a \code{.css} file.
#'   Read as a file when it names one.
#' @param ... Named tag overrides, each an \code{\link{md_style}}. Use a
#'   leading dot for a class, e.g. \code{.note = md_style(...)}.
#' @return An object of class \code{gridmicrotex_markdown_style}.
#' @seealso \code{\link{md_style}}, \code{\link{markdown_box_grob}}
#' @export
#'
#' @examples
#' markdown_style()
#' markdown_style("github", h1 = md_style(color = "firebrick"))
#' markdown_style(css = "h1 { color: steelblue } .note { padding-left: 2em }")
markdown_style <- function(base = NULL, css = NULL, ...) {
  rules <- .md_default_rules()

  if (!is.null(base)) {
    if (inherits(base, "gridmicrotex_markdown_style")) {
      rules <- base$rules
    } else if (is.character(base) && length(base) == 1L) {
      rules <- c(rules, .md_parse_stylesheet(.md_read_css(.md_preset_path(base))))
    } else {
      stop("`base` must be NULL, a preset name, or a markdown_style object.",
           call. = FALSE)
    }
  }

  if (!is.null(css)) {
    rules <- c(rules, .md_parse_stylesheet(.md_read_css(css)))
  }

  dots <- list(...)
  if (length(dots)) {
    nm <- names(dots)
    if (is.null(nm) || any(!nzchar(nm))) {
      stop("Tag overrides must be named, e.g. `h1 = md_style(...)`.",
           call. = FALSE)
    }
    for (i in seq_along(dots)) {
      st <- dots[[i]]
      if (!inherits(st, "gridmicrotex_md_style")) {
        stop("`", nm[i], "` must be an `md_style()` object.", call. = FALSE)
      }
      if (length(st)) rules <- c(rules, list(.md_rule(tolower(nm[i]), unclass(st))))
    }
  }

  structure(list(rules = rules), class = "gridmicrotex_markdown_style")
}

# Locate a bundled preset. Presets are CSS files rather than R code, so
# each one doubles as a worked example a user can copy.
.md_preset_path <- function(name) {
  if (!grepl("^[a-z0-9_-]+$", name)) {
    stop("Unknown preset ", sQuote(name), ".", call. = FALSE)
  }
  f <- system.file("css", paste0(name, ".css"), package = "gridmicrotex")
  if (!nzchar(f) || !file.exists(f)) {
    avail <- .md_preset_names()
    stop("Unknown preset ", sQuote(name),
         if (length(avail)) paste0(". Available: ",
                                   paste(sQuote(avail), collapse = ", ")) else "",
         call. = FALSE)
  }
  f
}

.md_preset_names <- function() {
  d <- system.file("css", package = "gridmicrotex")
  if (!nzchar(d)) return(character(0))
  sub("[.]css$", "", list.files(d, pattern = "[.]css$"))
}

#' @export
print.gridmicrotex_markdown_style <- function(x, ...) {
  sels <- vapply(x$rules, function(r) r$selector, character(1))
  cat("<markdown_style>", length(x$rules), "rules,",
      length(unique(sels)), "selectors\n")
  for (s in unique(sels)) {
    props <- character(0)
    for (r in x$rules[sels == s]) {
      for (p in names(r$props)) props[[p]] <- .md_fmt_value(r$props[[p]])
    }
    cat(sprintf("  %-12s %s\n", s,
                paste(names(props), unlist(props), sep = ": ", collapse = "; ")))
  }
  invisible(x)
}

# Coerce whatever `style =` was given into a style object.
#
# A bare string is accepted so the short form needs no constructor:
# style = "h1 { color: red }" or style = "house.css". A preset name would
# be ambiguous with a one-word file path, so presets stay explicit --
# markdown_style("github") -- and are not accepted here.
.md_as_style <- function(style) {
  if (is.null(style)) style <- .opt("markdown_style")
  if (is.null(style)) return(markdown_style())
  if (inherits(style, "gridmicrotex_markdown_style")) return(style)
  if (is.character(style)) return(markdown_style(css = style))
  stop("`style` must be a `markdown_style()` object, CSS text, or a path ",
       "to a .css file.", call. = FALSE)
}

# --- the cascade -----------------------------------------------------

# Resolve the properties in force for one element.
#
#   tag       the HTML-style tag name ("h1", "blockquote", ...)
#   classes   class names in scope, from an enclosing div or a span
#   inline    properties from a `style` attribute, highest priority
#   inherited already-resolved properties of the enclosing container
#
# Inherited values come first so any declared value beats them, which is
# what CSS does; among declared values, specificity then document order.
.md_cascade <- function(style, tag, classes = character(0), inline = NULL,
                        inherited = list()) {
  out <- inherited[intersect(names(inherited), .MD_INHERITED_PROPS)]

  rules <- style$rules
  if (length(rules)) {
    want <- c(tag, paste0(".", classes))
    hit <- which(vapply(rules, function(r) r$selector %in% want, logical(1)))
    if (length(hit)) {
      spec <- vapply(rules[hit], function(r) r$specificity, integer(1))
      # order() is stable, so equal specificity keeps document order.
      for (i in hit[order(spec)]) {
        for (p in names(rules[[i]]$props)) out[[p]] <- rules[[i]]$props[[p]]
      }
    }
  }

  if (length(inline)) for (p in names(inline)) out[[p]] <- inline[[p]]
  out
}

# --- value resolution ------------------------------------------------

# A CSS length in big points.
#
#   body  the document's font size, what `rem` and a bare number mean
#   own   this element's own font size, what `em` and `%` mean
#
# A bare number is `rem` rather than `em` so that the built-in defaults
# -- every one of which was a multiple of the body size before it lived
# here -- carry over unchanged while `em` keeps its proper CSS meaning.
.md_css_length <- function(v, body, own = body, horizontal = FALSE) {
  if (is.null(v)) return(NULL)
  if (grid::is.unit(v)) {
    return(if (horizontal) grid::convertWidth(v, "bigpts", valueOnly = TRUE)
           else grid::convertHeight(v, "bigpts", valueOnly = TRUE))
  }
  if (is.numeric(v)) return(if (is.finite(v)) v * body else NULL)
  x <- trimws(tolower(as.character(v)))
  m <- regmatches(x, regexec("^(-?[0-9]*[.]?[0-9]+)[[:space:]]*([a-z%]*)$", x))[[1]]
  if (length(m) != 3L) return(NULL)
  n <- as.numeric(m[2])
  if (!is.finite(n)) return(NULL)
  # A bare number carries no unit, which switch() cannot express as a
  # name, so it is settled before the switch alongside its synonym.
  u <- m[3]
  if (!nzchar(u) || u == "rem") return(n * body)
  # A big point and a CSS pt are both 1/72in, so absolute units need no
  # reference size.
  switch(u,
    em = n * own, "%" = n / 100 * own,
    pt = n, px = n * 72 / 96, "in" = n * 72,
    cm = n * 72 / 2.54, mm = n * 72 / 25.4,
    NULL)
}

# The `border-<side>` shorthand, of which only the width and the colour
# mean anything here -- every rule this package draws is solid.
# Returns list(width, color); either may be NULL, meaning "not stated".
.md_css_border <- function(v, body) {
  if (is.null(v)) return(NULL)
  if (is.numeric(v) || grid::is.unit(v)) {
    return(list(width = .md_css_length(v, body, horizontal = TRUE), color = NULL))
  }
  width <- NULL
  color <- NULL
  for (p in strsplit(trimws(as.character(v)), "[[:space:]]+")[[1]]) {
    if (tolower(p) %in% c("none", "hidden")) return(list(width = 0, color = NULL))
    if (tolower(p) %in% c("solid", "dotted", "dashed", "double", "groove",
                          "ridge", "inset", "outset")) next
    w <- .md_css_length(p, body, horizontal = TRUE)
    if (!is.null(w)) {
      width <- w
      next
    }
    cc <- .md_resolve_color(p)
    if (!is.null(cc)) color <- cc
  }
  list(width = width, color = color)
}

# The text-mode LaTeX commands a resolved style implies.
#
# These are the ones whose content must be emitted *bare*: MicroTeX
# builds a non-nested FontStyleAtom for \text{}, so \textbf{\text{x}}
# reports plain and the weight is silently lost. Measured, not assumed --
# \textbf{\text{hello}} and \text{hello} come out the same width.
.md_emphasis_cmds <- function(res) {
  cmds <- character(0)
  w <- res[["font-weight"]]
  if (!is.null(w) && .md_css_is_bold(tolower(as.character(w)))) {
    cmds <- c(cmds, "\\textbf{")
  }
  s <- res[["font-style"]]
  if (!is.null(s) && tolower(as.character(s)) %in% c("italic", "oblique")) {
    cmds <- c(cmds, "\\textit{")
  }
  cmds
}

# text-align to the fraction .md_layout()/makeContent() shift by.
.md_css_align <- function(v) {
  if (is.null(v)) return(NULL)
  switch(tolower(trimws(as.character(v))),
         left = 0, start = 0, center = 0.5, centre = 0.5,
         right = 1, end = 1, NULL)
}
