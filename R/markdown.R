# Markdown -> LaTeX -> grid: the inline half.
#
# One run of text, no block layout -- what markdown_grob() produces.
# Block documents are in markdown-box.R, which builds on this file for
# both the inline walker and the CSS value resolvers below.
#
# Pipeline:
#   md -> .md_mask_math()          hide math from the markdown parser
#      -> commonmark::markdown_xml()  CommonMark AST
#      -> xml2::read_xml()
#      -> .md_inline_to_tex()      walk the AST, emit LaTeX
#      -> latex_grob(input_mode = "math")
#
# Why mask first: CommonMark treats a backslash before ASCII punctuation
# as an escape, so `$\begin{matrix}a\\b\end{matrix}$` loses its row
# separator (`\\` collapses to `\`) before we ever see the AST. Masking
# each math span behind a sentinel keeps the LaTeX byte-exact. It also
# stops `*` and `_` inside formulas from being read as emphasis.

# Sentinels use Unicode private-use codepoints, which cannot appear in
# real input and carry no markdown meaning, so cmark passes them through
# as ordinary text. The index lets us restore spans in any order.
# Written as \u escapes so the source stays pure ASCII (R CMD check
# flags non-ASCII bytes in R files as a portability problem).
.MD_SENTINEL_OPEN  <- "\uE000"
.MD_SENTINEL_CLOSE <- "\uE001"
# Placeholder used inside .md_escape_tex() while backslashes are parked.
.MD_ESC_BS <- "\uE002"
# U+00A0, built from its codepoint: a literal one here would be a
# non-ASCII byte in an R source file, which R CMD check flags.
.MD_NBSP <- intToUtf8(160L)

.md_sentinel <- function(i) {
  paste0(.MD_SENTINEL_OPEN, i, .MD_SENTINEL_CLOSE)
}

# Replace every math span with a sentinel. Returns the masked text plus
# the original spans, indexed so .md_unmask_math() can splice them back.
# Math regions come from the same scanner latex_wrap() uses, so the two
# can never disagree about what counts as math (see .scan_math_spans).
#
# Unclosed spans are deliberately NOT masked. latex_wrap() auto-closes
# them at end of string, but in prose a lone `$` is far more often a
# price than an opening delimiter, and swallowing the rest of the line
# into math would also skip escaping it. Leaving it unmasked lets it fall
# through to .md_escape_tex() as the literal character the user typed.
.md_mask_math <- function(text) {
  # The sentinels below are private-use codepoints, which is not the same
  # as impossible: icon fonts (Nerd Fonts and friends) put real glyphs in
  # this block, and a pasted one would be spliced with a math span on the
  # way back out, silently duplicating a formula. Drop them from the
  # input rather than try to tell them apart later.
  text <- gsub(
    paste0("[", .MD_SENTINEL_OPEN, .MD_SENTINEL_CLOSE, .MD_ESC_BS, "]"),
    "", text)
  spans <- .scan_math_spans(text)
  spans <- Filter(function(sp) isTRUE(sp$closed), spans)
  if (length(spans) == 0L) {
    return(list(text = text, spans = character(0)))
  }
  out <- character(0)
  keep <- character(0)
  disp <- logical(0)
  pos <- 1L
  for (sp in spans) {
    if (sp$outer_start > pos) {
      out <- c(out, substr(text, pos, sp$outer_start - 1L))
    }
    keep <- c(keep, substr(text, sp$outer_start, sp$outer_end))
    # `$$...$$` / `\[...\]` asked for display math. Carried as an
    # attribute rather than a new list element so `spans` stays the plain
    # character vector every caller (and .md_unmask_math) indexes into.
    disp <- c(disp, isTRUE(sp$display))
    out <- c(out, .md_sentinel(length(keep)))
    pos <- sp$outer_end + 1L
  }
  if (pos <= nchar(text)) out <- c(out, substr(text, pos, nchar(text)))
  attr(keep, "display") <- disp
  list(text = paste(out, collapse = ""), spans = keep)
}

# Index of the math span a node consists *entirely* of, or NA. Used to
# promote a paragraph that is nothing but `$$...$$` to its own centred
# block, the way every other markdown dialect lays out display math.
.md_lone_math_span <- function(nd, spans) {
  kids <- xml2::xml_children(nd)
  if (length(kids) != 1L || !identical(xml2::xml_name(kids[[1]]), "text")) {
    return(NA_integer_)
  }
  txt <- trimws(xml2::xml_text(kids[[1]]))
  m <- regmatches(txt, regexec(
    paste0("^", .MD_SENTINEL_OPEN, "([0-9]+)", .MD_SENTINEL_CLOSE, "$"), txt))[[1]]
  if (length(m) != 2L) return(NA_integer_)
  i <- as.integer(m[2])
  disp <- attr(spans, "display")
  if (is.null(disp) || is.na(i) || i > length(disp) || !isTRUE(disp[i])) {
    return(NA_integer_)
  }
  i
}

# Splice masked math back into a string, restoring the original bytes.
.md_unmask_math <- function(s, spans) {
  if (length(spans) == 0L) return(s)
  for (i in seq_along(spans)) {
    s <- gsub(.md_sentinel(i), spans[i], s, fixed = TRUE)
  }
  s
}

# Characters that must not reach the MicroTeX parser as-is. Prose from a
# markdown AST is literal text: a stray `_` would open a subscript and
# `%` would start a comment.
.md_escape_tex <- function(s) {
  # Park backslashes behind a placeholder first. Their replacement text
  # contains braces, which the brace rules below would otherwise escape
  # in turn, yielding \backslash\{\}.
  s <- gsub("\\", .MD_ESC_BS, s, fixed = TRUE)
  # A leading backslash is all these need.
  s <- gsub("([{}$&#_%])", "\\\\\\1", s)
  # `~` and `^` are NOT escaped to \~{} and \^{}: those are the tilde and
  # circumflex *accents*, which float above the line the way a diacritic
  # does -- so `lm(y ~ x)` in a code block drew its tilde up at cap
  # height, and prose `~5` did the same.
  #
  # `^` needs nothing at all: inside \text{}, and inside a text command
  # such as \textbf{}, MicroTeX already draws it flat on the baseline.
  # `~` cannot be left bare, because LaTeX reads it as a non-breaking
  # space and MicroTeX duly draws a space, losing the character. Neither
  # \textasciitilde nor \textasciicircum exists -- like \textbackslash
  # they typeset their own letters. \char126 is the spelling that works,
  # and the empty group is required: \char126b swallows the `b`.
  s <- gsub("~", "\\\\char126{}", s)
  # MicroTeX has no \textbackslash -- it would typeset those 13 letters
  # literally. \backslash is the spelling that works; the empty group
  # stops it gluing onto a following letter (\backslashx).
  gsub(.MD_ESC_BS, "\\backslash{}", s, fixed = TRUE)
}

# Render one markdown text node.
#
# Escape the prose, restore the math spans raw, then -- unless `bare` --
# hand the result to latex_wrap(). The output of this file is fed to
# latex_grob() in "math" mode, so prose normally has to arrive already
# wrapped in \text{} or it would be typeset as spaced math italics.
# latex_wrap() also strips the `$` delimiters and adds \displaystyle for
# block math, which is exactly the translation needed.
#
# `bare` is for the inside of a text command such as \textbf{}. Those
# already put their argument in text mode, and a nested \text{} *resets
# the style*: \textbf{bold} reports font style 2 (bold) but
# \textbf{\text{bold}} reports 1 (plain), so wrapping there silently
# throws the emphasis away. Bare content still handles math, because
# `$...$` opens math mode inside a text command too.
#
# Order matters twice over: escape before unmasking, so the escaper never
# touches a formula; unmask before latex_wrap(), so it can see the real
# delimiters while escaped prose dollars stay literal.
.md_text_node <- function(s, spans, bare = FALSE) {
  one <- function(t) {
    raw <- .md_unmask_math(.md_escape_tex(t), spans)
    if (bare) raw else latex_wrap(raw, input_mode = "mixed")
  }
  # CommonMark decodes &nbsp; to a real U+00A0. MicroTeX spells it \nbsp,
  # but only *between* \text{} runs -- inside one it typesets the letters
  # (measured: \text{a}\nbsp\text{b} is 21bp, matching a real space, while
  # \text{a\nbsp b} is 25). So the run is split at each one and rejoined.
  # Left alone when bare, where there is no \text{} to sit between and the
  # character already renders as a space.
  if (bare || !grepl(.MD_NBSP, s, fixed = TRUE)) return(one(s))
  parts <- strsplit(s, .MD_NBSP, fixed = TRUE)[[1]]
  paste(vapply(parts, one, character(1)), collapse = "\\nbsp ")
}

# ---------------------------------------------------------------------
# Inline HTML
#
# GFM defines no markdown syntax for colour, underline or super/subscript
# -- verified: `_x_` is italic, `__x__` bold, `^x^` and `~x~` literal.
# CommonMark's raw HTML, which GFM keeps, is therefore not a fallback for
# these; it is the only conformant way to express them, and it is what
# ggtext uses. cmark hands us each tag as its own `html_inline` node with
# the raw text, so the walker can read them.
#
# Only the presentational subset below is interpreted. Everything else --
# `<abbr>`, `<div>`, anything unrecognised -- keeps its current
# behaviour: the markup is dropped and the text between it is kept.
# GFM's tagfilter (on, via extensions = TRUE) has already stripped
# `<script>`, `<iframe>` and friends before we see them.
# ---------------------------------------------------------------------

# The nine CSS named colours R's 657-name palette does not have. Every
# other CSS name is an R name too, so col2rgb() covers the rest.
#
# The names R and CSS *share* but disagree about -- green, gray, grey,
# maroon, purple -- are deliberately left to R. This package documents
# "any R colour name" and gp$col resolves them R's way everywhere else;
# quietly giving `color: green` a different green inside a style
# attribute than beside it would be worse than the inconsistency.
.MD_CSS_COLORS <- c(
  aqua = "#00FFFF", crimson = "#DC143C", fuchsia = "#FF00FF",
  indigo = "#4B0082", lime = "#00FF00", olive = "#808000",
  rebeccapurple = "#663399", silver = "#C0C0C0", teal = "#008080"
)

# Resolve a CSS colour to "#RRGGBB", or NULL when it is not a colour.
# Feeding hex to \textcolor is the robust path: MicroTeX accepts hex, so
# this frees us from its smaller table of named colours and lets any R
# colour name work.
.md_resolve_color <- function(x) {
  x <- trimws(tolower(x %||% ""))
  if (!nzchar(x)) return(NULL)
  if (grepl("^#[0-9a-f]{6}$", x)) return(toupper(x))
  # #abc is shorthand for #aabbcc.
  if (grepl("^#[0-9a-f]{3}$", x)) {
    d <- strsplit(substring(x, 2L), "")[[1]]
    return(toupper(paste0("#", paste0(d, d, collapse = ""))))
  }
  m <- regmatches(x, regexec("^rgba?\\(\\s*([0-9]+)\\D+([0-9]+)\\D+([0-9]+)", x))[[1]]
  if (length(m) == 4L) {
    v <- pmin(pmax(as.integer(m[2:4]), 0L), 255L)
    return(sprintf("#%02X%02X%02X", v[1], v[2], v[3]))
  }
  if (x %in% names(.MD_CSS_COLORS)) return(unname(.MD_CSS_COLORS[x]))
  rgb <- tryCatch(grDevices::col2rgb(x), error = function(e) NULL)
  if (is.null(rgb)) return(NULL)
  sprintf("#%02X%02X%02X", rgb[1], rgb[2], rgb[3])
}

# The size a CSS length is relative to, resolved the same way latex_grob()
# resolves it so `font-size: 10pt` means 10pt on the page.
.md_base_size <- function(gp) {
  size <- gp$fontsize %||% 20
  if (!is.null(gp$cex)) size <- size * gp$cex
  size
}

# Fold gp$cex into gp$fontsize and drop it.
#
# markdown_box_grob() builds a gTree whose children are latex_grobs, and
# each of those has already scaled itself by cex. grid multiplies a
# parent's cex into every child that does not set its own, so leaving cex
# on the gTree applied it a second time -- gpar(fontsize = 10, cex = 2)
# measured at 20 and drew at 40, overflowing the box. latex_grob() bakes
# and strips the same pair, for the same reason.
.md_fold_cex <- function(gp) {
  if (is.null(gp$cex)) return(gp)
  gp$fontsize <- .md_base_size(gp)
  gp$cex <- NULL
  gp
}

# A CSS length as a scale factor relative to `base` (big points). The
# absolute units are the ones gridtext accepts; em / rem / % are already
# relative, so they need no base at all. NULL for anything unparseable,
# which leaves the size alone rather than guessing.
.md_css_size <- function(x, base) {
  # A style object may hold a bare number or a unit rather than a CSS
  # string, since md_style(font_size = 2.5) is the R spelling of
  # "2.5rem". Both mean a multiple of the base, which is what this
  # returns, so they need no unit handling.
  if (grid::is.unit(x)) {
    if (base <= 0) return(NULL)
    return(grid::convertHeight(x, "bigpts", valueOnly = TRUE) / base)
  }
  if (is.numeric(x)) return(if (is.finite(x) && x > 0) x else NULL)
  x <- trimws(tolower(x))
  if (identical(x, "smaller")) return(1 / 1.2)
  if (identical(x, "larger")) return(1.2)
  # CSS's absolute keywords, taken from the \tiny..\Huge ladder MicroTeX
  # already implements: measured at 9, 13, 15, 16, 18, 22, 26, 33, 37, 47
  # bigpts against an 18 bigpt normalsize, which is the ratios below.
  kw <- c("xx-small" = 0.5, "x-small" = 0.72, small = 0.83, medium = 1,
          large = 1.2, "x-large" = 1.44, "xx-large" = 1.83)
  if (x %in% names(kw)) return(unname(kw[[x]]))
  # A unitless *string* stays invalid, because that is what it is in CSS:
  # `font-size: 12` is ignored by a browser and is ignored here. Only an
  # R value -- md_style(font_size = 2.5) -- means a bare multiple, and it
  # arrives as a number, not as text.
  m <- regmatches(
    x, regexec("^([0-9]*\\.?[0-9]+)\\s*(pt|px|in|cm|mm|em|rem|%)$", x))[[1]]
  if (length(m) != 3L) return(NULL)
  v <- as.numeric(m[2])
  if (!is.finite(v) || v <= 0) return(NULL)
  pt <- switch(m[3],
    pt = v,
    px = v * 72 / 96,
    "in" = v * 72,
    cm = v * 72 / 2.54,
    mm = v * 72 / 25.4,
    em = , rem = return(v),
    "%" = return(v / 100),
    return(NULL))
  if (base <= 0) return(NULL)
  pt / base
}

# Split a declaration list ("color: red; font-size: 2em") into a named
# list of raw values, in declaration order. Property names are lowercased
# -- CSS property names are case-insensitive -- but values keep their
# case, because a font family is a *name* and some font matchers care.
#
# A repeated property keeps its first position and takes its last value,
# which is what CSS says. This is the one place the split changed
# behaviour: the old loop emitted a command per declaration, so
# "color:red;color:blue" wrapped the text twice.
#
# Shared by every route into the style system -- the `style` attribute of
# a span or a div, a rule in a stylesheet, and md_style() -- so they can
# never disagree about what a declaration means.
# `warn = TRUE` reports declarations that will be ignored. Only the
# callers reading a hand-written `style` attribute pass it: a pasted
# stylesheet is *expected* to carry properties this package cannot
# honour (`display`, `float`, ...), and warning about those would punish
# the thing the parser is deliberately lenient for. A `style=` a user
# typed into their own markdown is different -- there, `colour: red` or
# `font-wieght: bold` silently doing nothing is a typo they want told
# about, and md_style() already errors on the same mistake.
.md_parse_css <- function(style, warn = FALSE) {
  out <- list()
  for (decl in strsplit(style %||% "", ";", fixed = TRUE)[[1]]) {
    kv <- strsplit(decl, ":", fixed = TRUE)[[1]]
    if (length(kv) < 2L) next
    prop <- .md_canonical_props(trimws(tolower(kv[1])))
    # A value can legitimately contain a colon, so rejoin what was split.
    if (nzchar(prop)) out[[prop]] <- trimws(paste(kv[-1], collapse = ":"))
  }
  if (warn && length(out)) {
    bad <- setdiff(names(out), .MD_STYLE_PROPS)
    if (length(bad)) {
      warning("Ignoring unsupported CSS ",
              if (length(bad) > 1L) "properties: " else "property: ",
              paste(sQuote(bad), collapse = ", "),
              ". See ?md_style for the supported set.", call. = FALSE)
    }
  }
  .md_expand_shorthand(out)
}

# CSS's `padding` / `margin` shorthand, expanded into the longhands the
# layout actually reads. One to four values, in CSS's order: all; then
# vertical/horizontal; then top/horizontal/bottom; then top/right/bottom/left.
#
# Done here rather than at each read so the shorthand works everywhere a
# declaration can appear -- a stylesheet, a `<div style=>`, and md_style().
.md_expand_shorthand <- function(props) {
  sides <- c("-top", "-right", "-bottom", "-left")
  for (p in c("padding", "margin")) {
    v <- props[[p]]
    if (is.null(v)) next
    props[[p]] <- NULL
    parts <- if (grid::is.unit(v)) {
      lapply(seq_along(v), function(i) v[i])
    } else if (is.numeric(v)) {
      as.list(v)
    } else {
      as.list(strsplit(trimws(as.character(v)), "[[:space:]]+")[[1]])
    }
    n <- length(parts)
    if (n == 0L) next
    pick <- switch(as.character(min(n, 4L)),
      "1" = c(1, 1, 1, 1), "2" = c(1, 2, 1, 2),
      "3" = c(1, 2, 3, 2), "4" = c(1, 2, 3, 4))
    for (i in seq_along(sides)) {
      nm <- paste0(p, sides[i])
      # A longhand alongside the shorthand wins. CSS settles that by
      # source order; these declarations are unordered, so the more
      # specific one always does.
      if (is.null(props[[nm]])) props[[nm]] <- parts[[pick[i]]]
    }
  }
  props
}

# font-family is a fallback list, so take the first entry. Map the CSS
# generics onto the aliases grid already understands and pass anything
# else through: what the name resolves to is the device's business, same
# as gpar(fontfamily=). Returns "" when there is no usable name.
.md_css_family <- function(val_raw) {
  fam <- gsub("^[\"']|[\"']$", "", trimws(strsplit(val_raw %||% "", ",")[[1]]))[1]
  fam <- switch(tolower(fam %||% ""), monospace = "mono",
                "sans-serif" = "sans", fam)
  # The name is about to be spliced into LaTeX, so it must not carry
  # anything the parser reads as syntax.
  gsub("[{}\\\\]", "", fam %||% "")
}

# CSS counts 600 and up as bold, alongside the two keywords.
.md_css_is_bold <- function(val) {
  if (val %in% c("bold", "bolder")) return(TRUE)
  n <- suppressWarnings(as.numeric(val))
  isTRUE(n >= 600)
}

# Turn parsed properties into wrapping commands. Several properties nest,
# so the closer has to match the number opened. `style` names the subset
# that switches to text mode -- the caller has to emit content inside
# those bare, and put them back inside a rule (see .md_inline_to_tex).
#
# Properties with no inline meaning (margin, padding, text-align ...) are
# skipped here and read by the block layout instead.
.md_css_inline_latex <- function(props, base = 20) {
  open <- character(0)
  text_mode <- character(0)
  # Emitted in declaration order, so the nesting matches what the author
  # wrote rather than an order of our choosing.
  for (prop in names(props)) {
    val_raw <- props[[prop]]
    # Only text gets case-folded. tolower() would coerce a number or a
    # unit -- which is how md_style() states a length -- into a string,
    # and a unitless string is not a valid CSS length.
    val <- if (is.character(val_raw)) tolower(val_raw) else val_raw
    if (prop == "color") {
      hex <- .md_resolve_color(val)
      if (!is.null(hex)) open <- c(open, paste0("\\textcolor{", hex, "}{"))
    } else if (prop == "text-decoration") {
      # The three decorations MicroTeX can draw.
      if (grepl("underline", val, fixed = TRUE)) open <- c(open, "\\underline{")
      if (grepl("overline", val, fixed = TRUE)) open <- c(open, "\\overline{")
      if (grepl("line-through", val, fixed = TRUE)) open <- c(open, "\\sout{")
    } else if (prop == "background") {
      # \bgcolor fills behind the glyphs with no padding, which is what a
      # background is; \colorbox would frame them instead. This is how
      # <mark> has always been drawn.
      #
      # A border alongside it takes the fill as \fcolorbox's second
      # argument, so emitting \bgcolor as well would paint it twice.
      hex <- if (is.null(props$border)) .md_resolve_color(val) else NULL
      if (!is.null(hex)) open <- c(open, paste0("\\bgcolor{", hex, "}{"))
    } else if (prop == "border") {
      # \fcolorbox needs both colours, \fbox neither. MicroTeX has no
      # \fboxsep, so the inset is fixed -- see ?md_style.
      bd <- .md_css_border(val_raw, base)
      col <- .md_resolve_color(bd$color %||% "")
      fill <- .md_resolve_color(props$background %||% "")
      open <- c(open, if (!is.null(col) && !is.null(fill)) {
        paste0("\\fcolorbox{", col, "}{", fill, "}{")
      } else "\\fbox{")
    } else if (prop == "border-style") {
      if (identical(val, "double")) open <- c(open, "\\doublebox{")
    } else if (prop == "border-radius") {
      # \ovalbox rounds the frame; \cornersize takes the radius as a
      # fraction of the box's smaller side.
      open <- c(open, "\\ovalbox{")
    } else if (prop == "box-shadow") {
      if (!identical(val, "none")) open <- c(open, "\\shadowbox{")
    } else if (prop == "visibility") {
      # \phantom keeps the space and draws nothing, which is exactly what
      # visibility: hidden means (display: none would remove the space).
      if (identical(val, "hidden")) open <- c(open, "\\phantom{")
    } else if (prop == "vertical-align") {
      cmd <- if (identical(val, "super")) "\\textsuperscript{"
             else if (identical(val, "sub")) "\\textsubscript{"
             else {
               d <- .md_css_length(val_raw, base, base)
               if (!is.null(d)) sprintf("\\raisebox{%.2fpt}{", d) else NULL
             }
      if (!is.null(cmd)) open <- c(open, cmd)
    } else if (prop == "transform") {
      # rotate(Ndeg) / scale(N) / scaleX(-1), the three MicroTeX has.
      m <- regmatches(val, regexec("rotate\\(\\s*(-?[0-9.]+)", val))[[1]]
      if (length(m) == 2L) open <- c(open, paste0("\\rotatebox{", m[2], "}{"))
      m <- regmatches(val, regexec("(?<![a-z])scale\\(\\s*([0-9.]+)", val,
                                   perl = TRUE))[[1]]
      if (length(m) == 2L) open <- c(open, paste0("\\scalebox{", m[2], "}{"))
      if (grepl("scalex(-1", gsub("[[:space:]]", "", val), fixed = TRUE)) {
        open <- c(open, "\\reflectbox{")
      }
    } else if (prop == "font-size") {
      # \scalebox scales the whole box, which is what font-size means, and
      # unlike \underline it keeps the font style around it.
      f <- .md_css_size(val, base)
      # Fixed notation, not format(): that honours getOption("OutDec"), so
      # a comma locale emitted \scalebox{0,75}, and it switches to
      # e-notation for a small factor, which the parser cannot read either.
      if (!is.null(f)) open <- c(open, sprintf("\\scalebox{%.6f}{", f))
    } else if (prop == "font-weight") {
      # \textbf switches to text mode itself, so it joins `style` for the
      # same reason \gmfontfamily does.
      if (.md_css_is_bold(val)) {
        open <- c(open, "\\textbf{")
        text_mode <- c(text_mode, "\\textbf{")
      }
    } else if (prop == "font-style") {
      if (val %in% c("italic", "oblique")) {
        open <- c(open, "\\textit{")
        text_mode <- c(text_mode, "\\textit{")
      }
    } else if (prop == "font-family") {
      fam <- .md_css_family(val_raw)
      if (nzchar(fam)) {
        cmd <- paste0("\\gmfontfamily{", fam, "}{")
        open <- c(open, cmd)
        # \text{} replaces the whole text font style (macro_fonts.h:12
        # builds a non-nested FontStyleAtom), which would drop the family
        # along with any bold. So content goes in bare, and the command
        # joins the set re-opened inside a rule.
        text_mode <- c(text_mode, cmd)
      }
    }
  }
  list(open = paste(open, collapse = ""),
       close = strrep("}", length(open)),
       style = text_mode)
}

# A `style` attribute straight to wrapping commands.
.md_parse_style <- function(style, base = 20) {
  .md_css_inline_latex(.md_parse_css(style), base)
}

# Tags we interpret, and the LaTeX that reproduces the rendering HTML
# itself prescribes for them (HTML Living Standard, "Rendering"). Nothing
# here is invented: every entry is some element's default presentation.
# Elements whose default is *no* visual change -- <a>, <abbr>, <span>
# without a style, <bdi>, <data>, <time>, <wbr> ... -- are absent on
# purpose and fall through to the drop-the-markup-keep-the-text path,
# which is already the right rendering for them.
#
# `bare` says what the command does to the text/math mode:
#   TRUE -- it switches to text mode itself, so its content must NOT be
#           wrapped in \text{} or the style is reset and thrown away
#   NA   -- it leaves the mode alone; content inherits the caller's
#
# `reset` marks the commands that typeset their argument as a fresh
# sub-formula and so lose the bold/italic/mono they sit inside. Measured,
# not assumed: \textbf{\underline{Hg}} reports no font style at all, while
# \textbf{\textcolor{red}{Hg}}, \textbf{{\small Hg}} and \textbf{\texttt{Hg}}
# all keep theirs. Size and colour survive; only the font style is lost.
.md_tag <- function(open, close = "}", bare = NA, reset = FALSE) {
  list(open = open, close = close, bare = bare, reset = reset)
}

.MD_HTML_TAGS <- list(
  # font-weight: bold
  b       = .md_tag("\\textbf{", bare = TRUE),
  strong  = .md_tag("\\textbf{", bare = TRUE),
  # font-style: italic
  i       = .md_tag("\\textit{", bare = TRUE),
  em      = .md_tag("\\textit{", bare = TRUE),
  cite    = .md_tag("\\textit{", bare = TRUE),
  dfn     = .md_tag("\\textit{", bare = TRUE),
  var     = .md_tag("\\textit{", bare = TRUE),
  address = .md_tag("\\textit{", bare = TRUE),
  # font-family: monospace
  code    = .md_tag("\\texttt{", bare = TRUE),
  kbd     = .md_tag("\\texttt{", bare = TRUE),
  samp    = .md_tag("\\texttt{", bare = TRUE),
  tt      = .md_tag("\\texttt{", bare = TRUE),
  # text-decoration
  u       = .md_tag("\\underline{", reset = TRUE),
  ins     = .md_tag("\\underline{", reset = TRUE),
  s       = .md_tag("\\sout{", reset = TRUE),
  del     = .md_tag("\\sout{", reset = TRUE),
  strike  = .md_tag("\\sout{", reset = TRUE),
  # vertical-align
  sub     = .md_tag("\\textsubscript{", reset = TRUE),
  sup     = .md_tag("\\textsuperscript{", reset = TRUE),
  # background: yellow. \bgcolor draws the fill behind the glyphs with no
  # padding, unlike \colorbox, which frames them.
  mark    = .md_tag("\\bgcolor{#FFFF00}{"),
  # font-size: smaller / larger
  small   = .md_tag("{\\small "),
  big     = .md_tag("{\\large ")
)

# Read one attribute out of a tag's attribute text, or NULL if absent.
#
# Match to the *closing* quote of the same kind, not to whichever quote
# comes first: a value may legitimately contain the other one, as
# font-family: 'Courier New', monospace does.
.md_html_attr <- function(attrs, name) {
  for (q in c('"', "'")) {
    m <- regmatches(attrs, regexec(
      paste0(name, "\\s*=\\s*", q, "([^", q, "]*)", q), attrs))[[1]]
    if (length(m) == 2L) return(m[2])
  }
  NULL
}

# The class names on a tag, lowercased -- CSS class matching here is
# case-insensitive because every selector is lowercased on the way in.
.md_html_classes <- function(attrs) {
  v <- .md_html_attr(attrs, "class")
  if (is.null(v)) return(character(0))
  tolower(Filter(nzchar, strsplit(trimws(v), "[[:space:]]+")[[1]]))
}

# Tags whose styling comes from the cascade rather than a fixed command,
# so they honour a stylesheet, a class, and a `style` attribute. `span` is
# the generic one; `a` is here so an inline link is styled by the same `a`
# rule as markdown's own `[text](url)`.
.MD_CASCADE_TAGS <- c("span", "a")

# Classify one html_inline node. `bare` is the mode the tag appears in,
# which <q> needs: its quotation marks are glyphs, not a command, so they
# have to be wrapped or not like any other text.
#
# Returns a list with `kind`:
#   "open"  -- emit `open`, push `close` and `bare`
#   "close" -- pop the matching tag, emit its close
#   "break" -- <br>, a line break; nothing to push
#   "text"  -- emit `text` as-is; a void element, so nothing to push
#   "drop"  -- emit nothing (unrecognised markup)
.md_html_tag <- function(txt, bare = FALSE, base = 20, style = NULL) {
  m <- regmatches(txt, regexec("^<\\s*(/?)\\s*([A-Za-z][A-Za-z0-9]*)([^>]*)>$",
                               trimws(txt)))[[1]]
  if (length(m) != 4L) return(list(kind = "drop"))
  closing <- nzchar(m[2])
  tag <- tolower(m[3])
  attrs <- m[4]

  if (closing) {
    if (tag %in% .MD_CASCADE_TAGS || tag == "q" ||
        !is.null(.MD_HTML_TAGS[[tag]])) {
      return(list(kind = "close", tag = tag))
    }
    return(list(kind = "drop"))
  }
  if (tag == "br") return(list(kind = "break"))
  # An image cannot be drawn by a text grob, so keep its alt text -- which
  # is what markdown's own `![alt](src)` already does. Dropping the tag
  # whole meant the two spellings of one thing disagreed, and `<img>` lost
  # the only part of itself that could be rendered.
  if (tag == "img") {
    src <- .md_html_attr(attrs, "src")
    if (.image_drawable(src)) {
      # HTML sizes an <img> in pixels, which is exactly what `px` means to
      # the resolver. A command, so it is emitted bare either way.
      opt <- character(0)
      for (a in c("width", "height")) {
        v <- .md_html_attr(attrs, a)
        if (!is.null(v) && grepl("^[0-9.]+$", trimws(v))) {
          opt <- c(opt, paste0(a, "=", trimws(v), "px"))
        }
      }
      return(list(kind = "text", text = paste0(
        "\\includegraphics",
        if (length(opt)) paste0("[", paste(opt, collapse = ","), "]") else "",
        "{", src, "}")))
    }
    alt <- .md_html_attr(attrs, "alt")
    if (is.null(alt) || !nzchar(trimws(alt))) return(list(kind = "drop"))
    alt <- .md_escape_tex(alt)
    return(list(kind = "text",
                text = if (bare) alt else paste0("\\text{", alt, "}")))
  }
  if (tag %in% .MD_CASCADE_TAGS) {
    # Only a *link* gets the link rendering. The HTML standard styles
    # `a:link`, which is an <a> carrying an href; a bare <a> is an anchor
    # and renders as ordinary text, exactly like <span>.
    if (tag == "a" && is.null(.md_html_attr(attrs, "href"))) {
      return(list(kind = "drop"))
    }
    # A class names rules in the stylesheet; the style attribute is the
    # highest-priority origin. .md_cascade() settles the two, so a span
    # and a div read their attributes exactly the same way.
    #
    # `<a href>` resolves through the same cascade, under its own `a`
    # selector, so it lands on the rule markdown's `[text](url)` already
    # uses -- and so a user restyling `a` restyles both spellings at once.
    props <- .md_cascade(style %||% markdown_style(), tag,
                         classes = .md_html_classes(attrs),
                         inline = .md_parse_css(.md_html_attr(attrs, "style"),
                                                warn = TRUE))
    st <- .md_css_inline_latex(props, base)
    # A span with no usable style still pushes, so </span> pairs cleanly.
    # font-family opens a text-mode command, so the span then behaves like
    # <code> and its content must be emitted bare.
    return(c(list(kind = "open", tag = tag, style = st$style),
             .md_tag(st$open, st$close,
                     bare = if (length(st$style)) TRUE else NA)))
  }
  # <q> is the one element whose default rendering adds characters rather
  # than a style: content: open-quote / close-quote.
  if (tag == "q") {
    q <- if (bare) c("\u201c", "\u201d") else c("\\text{\u201c}", "\\text{\u201d}")
    return(c(list(kind = "open", tag = "q"), .md_tag(q[1], q[2])))
  }
  cmd <- .MD_HTML_TAGS[[tag]]
  if (is.null(cmd)) return(list(kind = "drop"))
  c(list(kind = "open", tag = tag), cmd)
}

# Walk the inline children of a cmark node and return a LaTeX string.
#
# `spans` carries the masked math so text nodes can restore it. Node
# types cmark can emit that MicroTeX has no equivalent for degrade to
# their text content rather than emitting an unknown command -- MicroTeX
# does not error on an unknown command, it typesets the command name as
# red glyphs, so a stray \href would print a red "href" in the label.
.md_inline_to_tex <- function(node, spans, bare = FALSE,
                              styles = character(0), base = 20,
                              style = NULL) {
  kids <- xml2::xml_contents(node)
  if (length(kids) == 0L) return("")

  # An HTML span's opening and closing tags are *siblings*, with the
  # content it styles as the siblings between them, so the walk needs
  # state across the sequence rather than a per-node map.
  out <- character(0)
  open_tags <- character(0)   # tag names, innermost last
  open_close <- character(0)  # their closing LaTeX, same order
  open_bare <- logical(0)     # the mode each one imposes, NA to inherit
  open_style <- list()        # the font-style commands it adds, if any

  # The mode content is currently in: the innermost tag that imposes one,
  # or the caller's if none does. This is what lets <b> work at all --
  # its content is a *sibling* of the tag, so the only way to emit that
  # sibling bare is to remember that a text-mode tag is open.
  cur_bare <- function() {
    set <- which(!is.na(open_bare))
    if (length(set)) open_bare[set[length(set)]] else bare
  }
  # Font-style commands in force here, outermost first. `styles` carries
  # the ones opened by an enclosing walk (markdown's own ** and *), so a
  # rule nested inside them can put them back.
  cur_styles <- function() c(styles, unlist(open_style))

  # MicroTeX gives a row holding nothing a height of zero, so consecutive
  # breaks collapse into one and `<br><br>` loses the blank line HTML
  # shows. A zero-width strut with the height and depth of a text line
  # puts that row back at exactly the height of a real one.
  row_empty <- TRUE
  emit_break <- function() {
    b <- if (row_empty) "\\vphantom{\\text{Ag}}\\\\" else "\\\\"
    row_empty <<- TRUE
    b
  }

  emit_one <- function(k, nm, bare, sty) {
    switch(nm,
      text = .md_text_node(xml2::xml_text(k), spans, bare = bare),
      # \textbf/\textit/\texttt switch to text mode themselves, so their
      # content is emitted bare -- a nested \text{} would reset the style
      # and lose the emphasis entirely.
      # Routed through the cascade like <code> is, so `strong { color: }`
      # and `em { color: }` work -- the tag vocabulary is HTML's names,
      # and these two are the tags markdown's own ** and * produce.
      strong = {
        cd <- .md_css_inline_latex(
          .md_cascade(style %||% markdown_style(), "strong"), base)
        paste0(cd$open, "\\textbf{",
               .md_inline_to_tex(k, spans, TRUE,
                                 c(sty, cd$style, "\\textbf{"), base, style),
               "}", cd$close)
      },
      emph = {
        cd <- .md_css_inline_latex(
          .md_cascade(style %||% markdown_style(), "em"), base)
        paste0(cd$open, "\\textit{",
               .md_inline_to_tex(k, spans, TRUE,
                                 c(sty, cd$style, "\\textit{"), base, style),
               "}", cd$close)
      },
      # \sout, \cancel, \bcancel and \xcancel are all registered in
      # MicroTeX (macro_def.cpp); \sout is the horizontal rule that
      # matches markdown's ~~strike~~. It typesets its argument as a fresh
      # sub-formula, which drops any font style around it -- so **~~x~~**
      # came out unbolded until the enclosing commands were re-opened
      # inside it.
      strikethrough = paste0("\\sout{", paste(sty, collapse = ""),
                             .md_inline_to_tex(
                               k, spans, if (length(sty)) TRUE else bare,
                               sty, base, style),
                             strrep("}", length(sty)), "}"),
      # Code spans are literal. Restore any masked math first so the
      # sentinel never leaks, then escape the lot -- `$x$` in backticks
      # is meant to be shown as characters, not typeset as math.
      # A `code` rule in the stylesheet wraps the result; \texttt is the
      # built-in default that a rule adds to rather than replaces.
      code = {
        cd <- .md_css_inline_latex(
          .md_cascade(style %||% markdown_style(), "code"), base)
        paste0(cd$open, "\\texttt{",
               .md_escape_tex(.md_unmask_math(xml2::xml_text(k), spans)),
               "}", cd$close)
      },
      # Grid has no clickable link, so the destination is dropped and only
      # the text is kept -- but it is styled through the `a` tag, so it
      # still reads as a link. \underline / \textcolor typeset their
      # argument as a fresh sub-formula and drop the font style around
      # them, so any enclosing bold/italic is re-opened inside, exactly as
      # ~~strike~~ does above.
      link = {
        ln <- .md_css_inline_latex(
          .md_cascade(style %||% markdown_style(), "a"), base)
        if (!nzchar(ln$open)) {
          .md_inline_to_tex(k, spans, bare, sty, base, style)
        } else {
          paste0(ln$open, paste(sty, collapse = ""),
                 .md_inline_to_tex(k, spans,
                                   if (length(sty)) TRUE else bare,
                                   sty, base, style),
                 strrep("}", length(sty)), ln$close)
        }
      },
      # A drawable file becomes a real inline image; anything else -- a web
      # URL, a missing file, an unsupported format -- keeps its alt text,
      # silently, as it always has.
      image         = {
        dest <- xml2::xml_attr(k, "destination")
        if (.image_drawable(dest)) paste0("\\includegraphics{", dest, "}")
        else .md_inline_to_tex(k, spans, bare, sty, base, style)
      },
      # A footnote reference. CommonMark gives the target's id, not a
      # number, so the number is the position of the matching <fn> in the
      # document -- computed once in .md_parse_doc() and carried on
      # `spans`, which is already threaded everywhere this walker goes.
      fnref = {
        idx <- attr(spans, "fn_index")
        dest <- xml2::xml_attr(k, "destination")
        n <- if (!is.null(idx) && !is.na(dest) && dest %in% names(idx)) {
          idx[[dest]]
        } else NA_integer_
        if (is.na(n)) "" else paste0("\\textsuperscript{", n, "}")
      },
      # A soft line break is a word space. In math mode it needs to be an
      # explicit \text{ } box, because a bare space between two \text{}
      # runs is discarded there and the words either side would run
      # together ("islong"). Inside a text command a plain space is
      # already a space, and \text{ } would reset the style.
      softbreak     = if (bare) " " else "\\text{ }",
      # Anything else unknown: drop the markup, keep the text.
      .md_text_node(xml2::xml_text(k), spans, bare = bare)
    )
  }

  # <ruby>base<rt>gloss</rt></ruby> is the one construct whose pieces
  # arrive as siblings but must come out in the *other* order, because
  # \overset takes the annotation first. So it cannot be an open/close
  # pair like every other tag: the base is captured until <rt>, the gloss
  # until </rt>, and both are emitted at </ruby>.
  ruby <- NULL  # NULL = not in a ruby; otherwise list(base=, gloss=, in_rt=)
  ruby_emit <- function() {
    r <- ruby
    ruby <<- NULL
    if (!nzchar(r$base)) return("")
    if (!nzchar(r$gloss)) return(r$base)
    paste0("\\overset{\\scalebox{0.5}{", r$gloss, "}}{", r$base, "}")
  }

  for (k in kids) {
    nm <- xml2::xml_name(k)
    # Route everything through the ruby buffers while one is open.
    if (!is.null(ruby) && identical(nm, "html_inline")) {
      t <- tolower(trimws(xml2::xml_text(k)))
      if (t == "<rt>") { ruby$in_rt <- TRUE; next }
      if (t == "</rt>") { ruby$in_rt <- FALSE; next }
      if (t == "</ruby>") { out <- c(out, ruby_emit()); next }
    }
    if (!is.null(ruby) && !identical(nm, "html_inline")) {
      # Captured exactly as it would render outside a ruby, because a
      # <ruby> with no <rt> falls back to emitting the base alone.
      s <- emit_one(k, nm, cur_bare(), cur_styles())
      if (isTRUE(ruby$in_rt)) ruby$gloss <- paste0(ruby$gloss, s)
      else ruby$base <- paste0(ruby$base, s)
      next
    }
    if (identical(nm, "html_inline") &&
        identical(tolower(trimws(xml2::xml_text(k))), "<ruby>")) {
      ruby <- list(base = "", gloss = "", in_rt = FALSE)
      next
    }
    if (identical(nm, "linebreak")) {
      out <- c(out, emit_break())
      next
    }
    if (!identical(nm, "html_inline")) {
      # A row holding only spaces is still an empty row, so neither a soft
      # break nor whitespace-only text counts as content. It is the common
      # way to write a blank line: "one<br>\n<br>\ntwo".
      blank <- identical(nm, "softbreak") ||
        (identical(nm, "text") && !nzchar(trimws(xml2::xml_text(k))))
      s <- emit_one(k, nm, cur_bare(), cur_styles())
      if (!blank && nzchar(s)) row_empty <- FALSE
      out <- c(out, s)
      next
    }
    tg <- .md_html_tag(xml2::xml_text(k), cur_bare(), base, style)
    if (identical(tg$kind, "open")) {
      sty <- cur_styles()
      # The font-style commands this tag contributes: \textbf, \textit,
      # \texttt and the like. A <span> can bring several (font-family);
      # everything else brings its own open string or none. Captured
      # before `bare` is rewritten below.
      adds <- if (!is.null(tg$style)) tg$style
              else if (isTRUE(tg$bare)) tg$open
              else character(0)
      if (isTRUE(tg$reset) && length(sty)) {
        # Re-open the font-style commands inside, so the rule (or script)
        # does not strip the bold/italic/mono it is nested in.
        out <- c(out, tg$open, sty)
        tg$close <- paste0(strrep("}", length(sty)), tg$close)
        tg$bare <- TRUE
      } else {
        out <- c(out, tg$open)
      }
      open_tags <- c(open_tags, tg$tag)
      open_close <- c(open_close, tg$close)
      open_bare <- c(open_bare, tg$bare)
      open_style <- c(open_style, list(adds))
    } else if (identical(tg$kind, "close")) {
      # Close the innermost matching tag. Anything opened inside it that
      # was never closed is closed here too, so the braces stay balanced
      # however malformed the input was.
      hit <- which(open_tags == tg$tag)
      if (length(hit)) {
        i <- hit[length(hit)]
        out <- c(out, rev(open_close[seq(i, length(open_close))]))
        open_tags <- open_tags[seq_len(i - 1L)]
        open_close <- open_close[seq_len(i - 1L)]
        open_bare <- open_bare[seq_len(i - 1L)]
        open_style <- open_style[seq_len(i - 1L)]
      }
      # An unmatched closing tag is just stray markup: drop it.
    } else if (identical(tg$kind, "break")) {
      out <- c(out, emit_break())
    } else if (identical(tg$kind, "text")) {
      # A void element that renders as characters -- <img>'s alt text.
      # Nothing to push: there is no closing tag to pair with.
      out <- c(out, tg$text)
      row_empty <- FALSE
    }
    # "drop": unrecognised markup contributes nothing.
  }
  # An unclosed <ruby> still renders what it captured, rather than losing
  # the text entirely -- the same forgiveness unclosed spans get below.
  if (!is.null(ruby)) out <- c(out, ruby_emit())
  # Unclosed spans at the end of the run.
  if (length(open_close)) out <- c(out, rev(open_close))

  paste(out, collapse = "")
}

# Mask the math, then parse to a namespace-stripped CommonMark AST.
# Both entry points below need exactly this and must agree about it: a
# difference here would make markdown_grob() and markdown_box_grob()
# read the same document two different ways.
#
# commonmark and xml2 are Imports, so they cannot be missing at run time
# and are not guarded.
.md_parse_doc <- function(md) {
  masked <- .md_mask_math(md)
  doc <- xml2::read_xml(
    # `footnotes` is a separate argument, not one of the five extensions.
    # With it, `[^1]` arrives as <fnref> and its definition as a <fn>
    # block; without it both stay literal text.
    commonmark::markdown_xml(masked$text, extensions = TRUE, footnotes = TRUE)
  )
  # Strip the CommonMark namespace so plain node names match.
  xml2::xml_ns_strip(doc)
  # Number the footnotes by the order their definitions appear, which is
  # the order CommonMark emits them (first reference). Carried on `spans`
  # so the inline walker can reach it without a new parameter.
  ids <- xml2::xml_attr(xml2::xml_find_all(doc, ".//fn"), "id")
  spans <- masked$spans
  if (length(ids)) {
    idx <- seq_along(ids)
    names(idx) <- ids
    attr(spans, "fn_index") <- idx
  }
  list(doc = doc, spans = spans)
}

# A single character string, the only thing either entry point accepts.
.md_check_string <- function(md) {
  if (!is.character(md) || length(md) != 1L) {
    stop("`md` must be a single character string.", call. = FALSE)
  }
  if (is.na(md)) {
    stop("`md` must not be NA.", call. = FALSE)
  }
  invisible(md)
}

# Convert markdown to a single LaTeX string for input_mode = "math",
# flattening block structure: paragraphs are joined with `\\` line
# breaks and headings/lists/quotes lose their block formatting. This is
# what markdown_grob() uses. markdown_box_grob() keeps block structure
# by stacking each block as its own grob instead.
.md_to_tex <- function(md, base = 20, style = NULL) {
  parsed <- .md_parse_doc(md)
  blocks <- xml2::xml_children(parsed$doc)
  if (length(blocks) == 0L) return("")
  parts <- vapply(blocks, function(b) {
    nm <- xml2::xml_name(b)
    # An html_block holds raw markup as its text, so walking into it
    # would typeset the tags -- and the markdown inside them, unparsed.
    # Drop it, matching .md_blocks() on the block path, which would
    # otherwise disagree about the same document. Only *inline* HTML
    # (.md_html_tag) is interpreted.
    if (identical(nm, "html_block")) return("")
    # A footnote definition needs a foot to sit at, and an inline label has
    # none: the marker still renders, the note itself is dropped. Only
    # markdown_box_grob() lays them out.
    if (identical(nm, "fn")) return("")

    # There is no layout here to hang a margin or an indent on, so only
    # the properties that compile to a LaTeX command apply. Everything
    # else is ignored, the way CSS ignores what a medium cannot do.
    tag <- switch(nm,
      paragraph = "p", heading = paste0("h", .md_heading_level(b)),
      block_quote = "blockquote", code_block = "pre", table = "table",
      thematic_break = "hr", item = "li", nm)
    # `body` seeds the inheritance here exactly as it does for a block
    # document (.md_layout), so `body { color: }` reaches an inline label
    # too. Without this seed a stylesheet written against the box grob
    # silently did nothing when handed to markdown_grob().
    st <- style %||% markdown_style()
    ilat <- .md_css_inline_latex(
      .md_cascade(st, tag, inherited = .md_cascade(st, "body")), base)
    # A text-mode command resets the style of a nested \text{}, so the
    # content goes in bare when one is open -- the same rule the span
    # path follows.
    inner <- .md_inline_to_tex(b, parsed$spans,
                               bare = length(ilat$style) > 0L,
                               styles = ilat$style, base = base,
                               style = style)
    if (!nzchar(inner)) return("")
    paste0(ilat$open, inner, ilat$close)
  }, character(1))
  parts <- parts[nzchar(parts)]
  paste(parts, collapse = "\\\\")
}

#' Render markdown as a grid grob
#'
#' @description
#' Parses a markdown string and returns a grid grob, so plot labels can
#' mix ordinary prose formatting with real LaTeX math. Inline markdown
#' (\code{**bold**}, \code{*italic*}, \code{`code`}, \code{~~strike~~})
#' is translated to the equivalent LaTeX commands, while \code{$...$}
#' (and \code{\\(...\\)}, \code{$$...$$}, \code{\\[...\\]}) math spans
#' are passed through to MicroTeX byte-for-byte.
#'
#' @details
#' Markdown and LaTeX disagree about several characters --- most
#' importantly \code{\\}, which CommonMark treats as an escape. Math
#' spans are therefore hidden from the markdown parser before it runs and
#' restored afterwards, so constructs like
#' \code{$\\begin{matrix}a\\\\b\\end{matrix}$} survive intact.
#'
#' That hiding uses three private-use codepoints (\code{U+E000},
#' \code{U+E001}, \code{U+E002}) as markers, so those three characters
#' are \emph{removed} from the input. They are unassigned in Unicode, but
#' icon fonts such as Nerd Fonts do put real glyphs there: if your text
#' contains one it will be dropped rather than drawn. The alternative is
#' worse --- a pasted marker would be spliced together with a math span
#' on the way back out and silently duplicate a formula.
#'
#' GFM has no markdown syntax for colour, underline, super/subscript,
#' highlight or size, so --- as in CommonMark, and as \pkg{ggtext} does
#' --- these come from inline HTML. Each tag renders as HTML's own
#' default rendering prescribes:
#'
#' \tabular{ll}{
#'   \strong{tag} \tab \strong{effect} \cr
#'   \code{<b>}, \code{<strong>} \tab bold \cr
#'   \code{<i>}, \code{<em>}, \code{<cite>}, \code{<dfn>}, \code{<var>},
#'     \code{<address>} \tab italic \cr
#'   \code{<code>}, \code{<kbd>}, \code{<samp>}, \code{<tt>} \tab monospace \cr
#'   \code{<u>}, \code{<ins>} \tab underline \cr
#'   \code{<s>}, \code{<del>}, \code{<strike>} \tab strikethrough \cr
#'   \code{<sub>}, \code{<sup>} \tab sub / superscript \cr
#'   \code{<mark>} \tab yellow highlight \cr
#'   \code{<small>}, \code{<big>} \tab smaller / larger \cr
#'   \code{<q>} \tab wrapped in quotation marks \cr
#'   \code{<br>} \tab line break \cr
#'   \code{<span style="...">} \tab see below
#' }
#'
#' A \code{style} attribute is read for \code{color} (any R colour name,
#' the nine CSS names R lacks --- \code{crimson}, \code{teal},
#' \code{rebeccapurple} and friends --- \code{#rgb}, \code{#rrggbb} or
#' \code{rgb()}; note that \code{green}, \code{gray}, \code{grey},
#' \code{maroon} and \code{purple} keep their R values, not their CSS
#' ones),
#' \code{text-decoration} (\code{underline}, \code{line-through}),
#' \code{font-size} (\code{pt}, \code{px}, \code{in}, \code{cm},
#' \code{mm}, \code{em}, \code{rem}, \code{\%}, \code{smaller},
#' \code{larger}) and \code{font-family}. Any other property is ignored.
#'
#' \code{font-family} takes the CSS generics \code{monospace},
#' \code{sans-serif} and \code{serif}, or any font name; a fallback list
#' resolves to its first entry. The name is handed to
#' \code{gp$fontfamily}, so \emph{the device resolves it}: \pkg{ragg} and
#' \pkg{svglite} see any installed family plus anything registered with
#' \code{systemfonts::register_font()}, cairo devices see installed
#' families, and base \code{pdf()} sees only what \code{pdfFonts()}
#' declares --- a named family will not resolve there. An unavailable
#' font falls back silently, as it does for \code{gpar(fontfamily=)}.
#' A font file that is not installed system-wide is used by registering
#' it first:
#'
#' \preformatted{systemfonts::register_font(name = "MyFont", plain = "MyFont.otf")}
#'
#' \code{\link{load_math_font}} is \emph{not} the function for this ---
#' it registers \emph{math} fonts with MicroTeX, which is a different
#' mechanism.
#'
#' A span's own family wins over \code{gp$fontfamily}, but the width of
#' the spaces \emph{between} its words still comes from
#' \code{gp$fontfamily}; set both to the same family if that shows.
#'
#' Tags nest and combine freely with markdown. Any other tag --- and all
#' block-level HTML --- is dropped, keeping the text inside it, which is
#' also what a browser shows for the ones (\code{<a>}, \code{<abbr>},
#' \code{<span>} without a style) that have no default rendering.
#'
#' Not every markdown feature has a MicroTeX equivalent. Links keep their
#' text and drop the destination, and images keep their alt text.
#'
#' Everything is flattened into a single run here, with paragraphs joined
#' by line breaks: there is no block layout, so indentation, list markers
#' and block-quote rules need \code{\link{markdown_box_grob}}. A heading
#' still takes the size and weight its \code{style} gives it, since those
#' compile to LaTeX commands rather than to layout.
#'
#' @param md Character string of markdown.
#' @param style Appearance of the text: a \code{\link{markdown_style}}
#'   object, CSS text, or a path to a \code{.css} file. \code{NULL}
#'   (default) uses \code{latex_options("markdown_style")} if set, and
#'   the built-in defaults otherwise. Only the properties \code{md_style}
#'   marks as \emph{inline} apply here --- there is no block layout in a
#'   single run for a margin, an indent or an alignment to act on, so
#'   those are ignored. \code{\link{markdown_box_grob}} honours them all.
#' @param ... Passed to \code{\link{latex_grob}} --- e.g. \code{x},
#'   \code{y}, \code{hjust}, \code{vjust}, \code{rot}, \code{max_width},
#'   \code{gp}.
#' @return A \code{latexgrob}, as returned by \code{\link{latex_grob}}.
#' @seealso \code{\link{latex_grob}}, \code{\link{latex_wrap}}
#' @export
#'
#' @examples
#' \donttest{
#'   grid::grid.newpage()
#'   grid.markdown("The **fitted** slope is $\\beta_1$, *p* < 0.001")
#' }
markdown_grob <- function(md, style = NULL, ...) {
  .md_check_string(md)
  # input_mode is fixed below, so letting it through `...` would fail with
  # R's "matched by multiple actual arguments" rather than saying why.
  if ("input_mode" %in% ...names()) {
    stop("`input_mode` is not accepted by markdown_grob(): markdown ",
         "decides for itself what is prose and what is math.", call. = FALSE)
  }
  # The walker has already produced \text{}-wrapped LaTeX, so bypass
  # latex_wrap() -- running it again would double-wrap the prose.
  latex_grob(.md_to_tex(md, .md_base_size(list(...)$gp), .md_as_style(style)),
             input_mode = "math", ...)
}

#' @rdname markdown_grob
#' @export
grid.markdown <- function(md, ...) {
  g <- markdown_grob(md, ...)
  grid::grid.draw(g)
  invisible(g)
}

