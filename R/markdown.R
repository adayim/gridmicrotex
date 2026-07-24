# Markdown -> LaTeX -> grid.
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
  spans <- .scan_math_spans(text)
  spans <- Filter(function(sp) isTRUE(sp$closed), spans)
  if (length(spans) == 0L) {
    return(list(text = text, spans = character(0)))
  }
  out <- character(0)
  keep <- character(0)
  pos <- 1L
  for (sp in spans) {
    if (sp$outer_start > pos) {
      out <- c(out, substr(text, pos, sp$outer_start - 1L))
    }
    keep <- c(keep, substr(text, sp$outer_start, sp$outer_end))
    out <- c(out, .md_sentinel(length(keep)))
    pos <- sp$outer_end + 1L
  }
  if (pos <= nchar(text)) out <- c(out, substr(text, pos, nchar(text)))
  list(text = paste(out, collapse = ""), spans = keep)
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
  s <- gsub("{", "\\{", s, fixed = TRUE)
  s <- gsub("}", "\\}", s, fixed = TRUE)
  s <- gsub("$", "\\$", s, fixed = TRUE)
  s <- gsub("&", "\\&", s, fixed = TRUE)
  s <- gsub("#", "\\#", s, fixed = TRUE)
  s <- gsub("_", "\\_", s, fixed = TRUE)
  s <- gsub("%", "\\%", s, fixed = TRUE)
  s <- gsub("^", "\\^{}", s, fixed = TRUE)
  s <- gsub("~", "\\~{}", s, fixed = TRUE)
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
  raw <- .md_unmask_math(.md_escape_tex(s), spans)
  if (bare) raw else latex_wrap(raw, input_mode = "mixed")
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
  rgb <- tryCatch(grDevices::col2rgb(x), error = function(e) NULL)
  if (is.null(rgb)) return(NULL)
  sprintf("#%02X%02X%02X", rgb[1], rgb[2], rgb[3])
}

# Turn a style attribute into wrapping commands. Several properties nest,
# so the closer has to match the number opened.
.md_parse_style <- function(style) {
  open <- character(0)
  for (decl in strsplit(style %||% "", ";", fixed = TRUE)[[1]]) {
    kv <- strsplit(decl, ":", fixed = TRUE)[[1]]
    if (length(kv) < 2L) next
    prop <- trimws(tolower(kv[1]))
    val <- trimws(tolower(paste(kv[-1], collapse = ":")))
    if (prop == "color") {
      hex <- .md_resolve_color(val)
      if (!is.null(hex)) open <- c(open, paste0("\\textcolor{", hex, "}{"))
    } else if (prop == "text-decoration") {
      # Only the two decorations MicroTeX can draw.
      if (grepl("underline", val, fixed = TRUE)) open <- c(open, "\\underline{")
      if (grepl("line-through", val, fixed = TRUE)) open <- c(open, "\\sout{")
    }
  }
  list(open = paste(open, collapse = ""),
       close = strrep("}", length(open)))
}

# Tags we interpret. Deliberately excludes <b>/<i>/<strong>/<em> and the
# font-weight / font-style properties: markdown already has ** and *, and
# \textbf/\textit switch to text mode, which would mean their content had
# to be emitted bare. Every command below leaves the mode alone, so
# content passes through with `bare` untouched.
.MD_HTML_TAGS <- list(
  u      = "\\underline{",
  ins    = "\\underline{",
  s      = "\\sout{",
  del    = "\\sout{",
  strike = "\\sout{",
  sub    = "\\textsubscript{",
  sup    = "\\textsuperscript{"
)

# Classify one html_inline node.
#
# Returns a list with `kind`:
#   "open"  -- push `close`, emit `open`
#   "close" -- pop the matching tag, emit its close
#   "void"  -- emit `open`, no push (<br>)
#   "drop"  -- emit nothing (unrecognised markup)
.md_html_tag <- function(txt) {
  m <- regmatches(txt, regexec("^<\\s*(/?)\\s*([A-Za-z][A-Za-z0-9]*)([^>]*)>$",
                               trimws(txt)))[[1]]
  if (length(m) != 4L) return(list(kind = "drop"))
  closing <- nzchar(m[2])
  tag <- tolower(m[3])
  attrs <- m[4]

  if (closing) {
    if (tag == "span" || !is.null(.MD_HTML_TAGS[[tag]])) {
      return(list(kind = "close", tag = tag))
    }
    return(list(kind = "drop"))
  }
  if (tag == "br") return(list(kind = "void", open = "\\\\"))
  if (tag == "span") {
    style <- regmatches(attrs,
      regexec("style\\s*=\\s*[\"']([^\"']*)[\"']", attrs))[[1]]
    st <- if (length(style) == 2L) .md_parse_style(style[2]) else
      list(open = "", close = "")
    # A span with no usable style still pushes, so </span> pairs cleanly.
    return(list(kind = "open", tag = "span", open = st$open, close = st$close))
  }
  cmd <- .MD_HTML_TAGS[[tag]]
  if (is.null(cmd)) return(list(kind = "drop"))
  list(kind = "open", tag = tag, open = cmd, close = "}")
}

# Walk the inline children of a cmark node and return a LaTeX string.
#
# `spans` carries the masked math so text nodes can restore it. Node
# types cmark can emit that MicroTeX has no equivalent for degrade to
# their text content rather than emitting an unknown command -- MicroTeX
# does not error on an unknown command, it typesets the command name as
# red glyphs, so a stray \href would print a red "href" in the label.
.md_inline_to_tex <- function(node, spans, bare = FALSE) {
  kids <- xml2::xml_contents(node)
  if (length(kids) == 0L) return("")

  # An HTML span's opening and closing tags are *siblings*, with the
  # content it styles as the siblings between them, so the walk needs
  # state across the sequence rather than a per-node map.
  out <- character(0)
  open_tags <- character(0)   # tag names, innermost last
  open_close <- character(0)  # their closing LaTeX, same order

  emit_one <- function(k, nm) {
    switch(nm,
      text = .md_text_node(xml2::xml_text(k), spans, bare = bare),
      # \textbf/\textit/\texttt switch to text mode themselves, so their
      # content is emitted bare -- a nested \text{} would reset the style
      # and lose the emphasis entirely.
      strong = paste0("\\textbf{", .md_inline_to_tex(k, spans, TRUE), "}"),
      emph   = paste0("\\textit{", .md_inline_to_tex(k, spans, TRUE), "}"),
      # \sout, \cancel, \bcancel and \xcancel are all registered in
      # MicroTeX (macro_def.cpp); \sout is the horizontal rule that
      # matches markdown's ~~strike~~. Unlike the \text* commands it does
      # not set a mode of its own -- it just rules through whatever it is
      # given -- so it inherits the caller's, and prose reaching it in
      # math mode still needs the \text{} that `bare = FALSE` supplies.
      strikethrough = paste0("\\sout{", .md_inline_to_tex(k, spans, bare), "}"),
      # Code spans are literal. Restore any masked math first so the
      # sentinel never leaks, then escape the lot -- `$x$` in backticks
      # is meant to be shown as characters, not typeset as math.
      code = paste0("\\texttt{",
                    .md_escape_tex(.md_unmask_math(xml2::xml_text(k), spans)),
                    "}"),
      # No \href in MicroTeX: keep the link text, drop the destination.
      link          = .md_inline_to_tex(k, spans, bare),
      # Images cannot be drawn by a text grob; keep the alt text.
      image         = .md_inline_to_tex(k, spans, bare),
      # A soft line break is a word space. In math mode it needs to be an
      # explicit \text{ } box, because a bare space between two \text{}
      # runs is discarded there and the words either side would run
      # together ("islong"). Inside a text command a plain space is
      # already a space, and \text{ } would reset the style.
      softbreak     = if (bare) " " else "\\text{ }",
      linebreak     = "\\\\",
      # Anything else unknown: drop the markup, keep the text.
      .md_text_node(xml2::xml_text(k), spans, bare = bare)
    )
  }

  for (k in kids) {
    nm <- xml2::xml_name(k)
    if (!identical(nm, "html_inline")) {
      out <- c(out, emit_one(k, nm))
      next
    }
    tg <- .md_html_tag(xml2::xml_text(k))
    if (identical(tg$kind, "open")) {
      out <- c(out, tg$open)
      open_tags <- c(open_tags, tg$tag)
      open_close <- c(open_close, tg$close)
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
      }
      # An unmatched closing tag is just stray markup: drop it.
    } else if (identical(tg$kind, "void")) {
      out <- c(out, tg$open)
    }
    # "drop": unrecognised markup contributes nothing.
  }
  # Unclosed spans at the end of the run.
  if (length(open_close)) out <- c(out, rev(open_close))

  paste(out, collapse = "")
}

# Convert markdown to a single LaTeX string for input_mode = "math",
# flattening block structure: paragraphs are joined with `\\` line
# breaks and headings/lists/quotes lose their block formatting. This is
# what markdown_grob() uses. markdown_box_grob() keeps block structure
# by stacking each block as its own grob instead.
.md_to_tex <- function(md) {
  if (!requireNamespace("commonmark", quietly = TRUE) ||
      !requireNamespace("xml2", quietly = TRUE)) {
    stop("Markdown support requires the 'commonmark' and 'xml2' packages.",
         call. = FALSE)
  }
  masked <- .md_mask_math(md)
  xml <- commonmark::markdown_xml(masked$text, extensions = TRUE)
  doc <- xml2::read_xml(xml)
  # Strip the CommonMark namespace so plain node names match.
  xml2::xml_ns_strip(doc)
  blocks <- xml2::xml_children(doc)
  if (length(blocks) == 0L) return("")
  parts <- vapply(blocks, function(b) {
    # An html_block holds raw markup as its text, so walking into it
    # would typeset the tags -- and the markdown inside them, unparsed.
    # Drop it, matching .md_blocks() on the block path, which would
    # otherwise disagree about the same document. Only *inline* HTML
    # (.md_html_tag) is interpreted.
    if (identical(xml2::xml_name(b), "html_block")) return("")
    .md_inline_to_tex(b, masked$spans)
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
#' GFM has no markdown syntax for colour, underline or super/subscript,
#' so --- as in CommonMark, and as \pkg{ggtext} does --- these come from
#' inline HTML. The presentational subset is rendered:
#' \code{<span style="color:...">} (any R colour name, \code{#rgb},
#' \code{#rrggbb} or \code{rgb()}, plus
#' \code{text-decoration: underline|line-through}), \code{<u>},
#' \code{<ins>}, \code{<s>}, \code{<del>}, \code{<sub>}, \code{<sup>} and
#' \code{<br>}. Use \code{**}/\code{*} for bold and italic; \code{<b>} and
#' \code{<i>} are not interpreted. Any other tag --- and all block-level
#' HTML --- is dropped, keeping the text inside it.
#'
#' Not every markdown feature has a MicroTeX equivalent. Links keep their
#' text and drop the destination, and images keep their alt text.
#' Block-level markdown (headings, lists, block quotes, tables) is
#' rendered by \code{markdown_box_grob()}.
#'
#' @param md Character string of markdown.
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
markdown_grob <- function(md, ...) {
  if (!is.character(md) || length(md) != 1L) {
    stop("`md` must be a single character string.", call. = FALSE)
  }
  if (is.na(md)) {
    stop("`md` must not be NA.", call. = FALSE)
  }
  # The walker has already produced \text{}-wrapped LaTeX, so bypass
  # latex_wrap() -- running it again would double-wrap the prose.
  latex_grob(.md_to_tex(md), input_mode = "math", ...)
}

#' @rdname markdown_grob
#' @export
grid.markdown <- function(md, ...) {
  g <- markdown_grob(md, ...)
  grid::grid.draw(g)
  invisible(g)
}


# ---------------------------------------------------------------------
# Block-level markdown
#
# MicroTeX cannot lay a document out in one parse: a `\` puts a VBox at
# the top of the box tree and, inside matrix/array cells, max_width is
# not honoured at all (see the note in BoxSplitter::split). So block
# structure is assembled here instead -- one latex_grob per block,
# stacked in grid -- which is also where boxes, padding, background
# fills and rules have to live, none of which MicroTeX models.
# ---------------------------------------------------------------------

# Heading level -> MicroTeX size macro. All ten of the \tiny..\Huge
# family are registered (macro_def.cpp); these six are the ones a
# markdown heading can reach.
.MD_HEADING_SIZE <- c("\\Huge", "\\huge", "\\LARGE",
                      "\\Large", "\\large", "\\normalsize")

# Parse markdown into a list of block descriptors. Recurses through
# containers (lists, block quotes) so nesting is preserved.
.md_blocks <- function(nodes, spans) {
  out <- list()
  for (nd in nodes) {
    nm <- xml2::xml_name(nd)
    # A paragraph holding nothing but images is a block image, which is
    # how markdown renderers treat it. Handled before the switch because
    # one paragraph can yield several image blocks.
    if (identical(nm, "paragraph")) {
      imgs <- .md_image_blocks(nd, spans)
      if (!is.null(imgs)) {
        out <- c(out, imgs)
        next
      }
    }
    blk <- switch(nm,
      paragraph = list(type = "paragraph",
                       tex = .md_inline_to_tex(nd, spans)),
      heading = list(type = "heading",
                     level = .md_heading_level(nd),
                     tex = .md_inline_to_tex(nd, spans)),
      block_quote = list(type = "block_quote",
                         blocks = .md_blocks(xml2::xml_children(nd), spans)),
      code_block = list(type = "code_block",
                        lines = .md_code_lines(nd)),
      list = .md_list_block(nd, spans),
      table = list(type = "table", tex = .md_table_tex(nd, spans)),
      thematic_break = list(type = "thematic_break"),
      # html_block and anything unrecognised is dropped rather than
      # passed through: MicroTeX would typeset the raw markup.
      NULL
    )
    if (!is.null(blk)) out[[length(out) + 1L]] <- blk
  }
  out
}

.md_heading_level <- function(nd) {
  lvl <- suppressWarnings(as.integer(xml2::xml_attr(nd, "level")))
  if (is.na(lvl)) lvl <- 1L
  min(max(lvl, 1L), 6L)
}

# Code blocks are literal. MicroTeX has no verbatim environment, so each
# source line becomes its own \texttt{} block and the stacker puts them
# on separate rows; that also keeps indentation from being collapsed.
.md_code_lines <- function(nd) {
  txt <- xml2::xml_text(nd)
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0L) return(character(0))
  # A trailing newline yields a spurious empty final line.
  if (length(lines) > 1L && !nzchar(lines[length(lines)])) {
    lines <- lines[-length(lines)]
  }
  lines
}

.md_list_block <- function(nd, spans) {
  ordered <- identical(xml2::xml_attr(nd, "type"), "ordered")
  start <- suppressWarnings(as.integer(xml2::xml_attr(nd, "start")))
  if (is.na(start)) start <- 1L
  kids <- xml2::xml_children(nd)
  items <- lapply(kids, function(it) {
    .md_blocks(xml2::xml_children(it), spans)
  })
  # A GFM task list item arrives as <tasklist completed="..."> in place
  # of <item>; NA marks an ordinary item with no checkbox.
  checked <- vapply(kids, function(it) {
    if (!identical(xml2::xml_name(it), "tasklist")) return(NA)
    identical(xml2::xml_attr(it, "completed"), "true")
  }, logical(1))
  list(type = "list", ordered = ordered, start = start,
       # `1)` is as valid as `1.` in CommonMark, and cmark reports which
       # the author used.
       paren = identical(xml2::xml_attr(nd, "delim"), "paren"),
       tight = identical(xml2::xml_attr(nd, "tight"), "true"),
       items = items, checked = checked)
}

# Return one image block per image when a paragraph contains nothing but
# images, otherwise NULL so the paragraph is rendered normally. Images
# mixed into a sentence stay inline, where only their alt text survives.
.md_image_blocks <- function(nd, spans) {
  kids <- xml2::xml_children(nd)
  if (length(kids) == 0L) return(NULL)
  nms <- xml2::xml_name(kids)
  if (!all(nms %in% c("image", "softbreak"))) return(NULL)
  lapply(which(nms == "image"), function(i) {
    im <- kids[[i]]
    list(type = "image",
         path = xml2::xml_attr(im, "destination"),
         alt = .md_inline_to_tex(im, spans))
  })
}

# Load a raster image. Returns NULL -- and the caller falls back to the
# alt text -- when the file is missing, the format is unsupported, or the
# reader package is not installed. png and jpeg are Suggests, so images
# degrade rather than becoming a hard dependency of the whole package.
.md_image_raster <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path)) return(NULL)
  if (!file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  reader <- switch(ext,
    png = if (requireNamespace("png", quietly = TRUE)) png::readPNG else NULL,
    jpg = ,
    jpeg = if (requireNamespace("jpeg", quietly = TRUE)) jpeg::readJPEG else NULL,
    NULL
  )
  if (is.null(reader)) return(NULL)
  ras <- tryCatch(reader(path), error = function(e) NULL)
  if (is.null(ras) || length(dim(ras)) < 2L) return(NULL)
  list(raster = grDevices::as.raster(ras),
       w_px = dim(ras)[2], h_px = dim(ras)[1])
}

# Build a `tabular` from the GFM table extension. MicroTeX supports
# tabular natively along with \hline and the \thickhline added for
# booktabs output, so a markdown table becomes a real typeset table
# rather than an image -- something marquee cannot do at all.
.md_table_tex <- function(nd, spans) {
  rows <- xml2::xml_children(nd)
  if (length(rows) == 0L) return("")
  cells_of <- function(r) {
    vapply(xml2::xml_children(r), .md_inline_to_tex, character(1),
           spans = spans)
  }
  header <- NULL
  body <- list()
  for (r in rows) {
    cs <- cells_of(r)
    if (identical(xml2::xml_name(r), "table_header")) {
      header <- cs
    } else {
      body[[length(body) + 1L]] <- cs
    }
  }
  ncol <- max(c(length(header), vapply(body, length, integer(1))), 0L)
  if (ncol == 0L) return("")
  pad <- function(cs) c(cs, rep("", ncol - length(cs)))
  line <- function(cs) paste(pad(cs), collapse = " & ")

  # Column alignment from `|:--|:-:|--:|`. cmark records it on the header
  # cells only, and MicroTeX honours l/c/r in the tabular spec.
  spec <- rep("l", ncol)
  hdr <- Filter(function(r) identical(xml2::xml_name(r), "table_header"), rows)
  if (length(hdr)) {
    a <- vapply(xml2::xml_children(hdr[[1]]), function(cell) {
      switch(xml2::xml_attr(cell, "align") %||% "left",
             center = "c", right = "r", "l")
    }, character(1))
    a <- a[!is.na(a)]
    if (length(a)) spec[seq_len(min(ncol, length(a)))] <- a[seq_len(min(ncol, length(a)))]
  }

  parts <- c("\\begin{tabular}{", paste(spec, collapse = ""), "}",
             "\\thickhline ")
  if (!is.null(header)) {
    parts <- c(parts, line(header), "\\\\", "\\hline ")
  }
  for (b in body) parts <- c(parts, line(b), "\\\\")
  parts <- c(parts, "\\thickhline", "\\end{tabular}")
  paste(parts, collapse = "")
}

# Parse a markdown string into block descriptors.
.md_parse_blocks <- function(md) {
  if (!requireNamespace("commonmark", quietly = TRUE) ||
      !requireNamespace("xml2", quietly = TRUE)) {
    stop("Markdown support requires the 'commonmark' and 'xml2' packages.",
         call. = FALSE)
  }
  masked <- .md_mask_math(md)
  doc <- xml2::read_xml(
    commonmark::markdown_xml(masked$text, extensions = TRUE)
  )
  xml2::xml_ns_strip(doc)
  .md_blocks(xml2::xml_children(doc), masked$spans)
}

# Marker text for list item `n`.
#
# `checked` is NA for an ordinary item, TRUE/FALSE for a GFM task list
# item. The two checkbox states must be the SAME glyph size or the list
# looks ragged: \square (empty) and \blacksquare (filled) are both 14x13
# in the math font, whereas the more literal \boxtimes is 18x16 and left
# every checked row visibly larger. (\Box is not a glyph at all -- it
# typesets the three letters "Box".)
.md_list_marker <- function(ordered, start, n, paren = FALSE, checked = NA) {
  if (!is.na(checked)) {
    return(if (isTRUE(checked)) "\\blacksquare" else "\\square")
  }
  if (ordered) {
    paste0("\\text{", start + n - 1L, if (paren) ")" else ".", "}")
  } else {
    "\\bullet"
  }
}

# ---------------------------------------------------------------------
# Layout
#
# Each block is measured with latex_dims() at the available width and
# placed at an offset from the top of the stack. Offsets grow downward;
# makeContent turns them into viewports. Working top-down means the
# total height does not have to be known in advance.
#
# All distances are big points, matching what latex_dims() reports.
# ---------------------------------------------------------------------

# One entry in the laid-out stack.
.md_item <- function(grob, x, y, w, h) {
  list(grob = grob, x = x, y = y, w = w, h = h)
}

# Measure a run, and return the max_width that achieved that measurement
# so the caller can draw with exactly the same setting.
#
# input_mode MUST match .md_run_grob(). latex_dims() defaults to
# "mixed", which would run latex_wrap() over LaTeX the AST walker has
# already wrapped -- measuring a different (double-wrapped) string from
# the one drawn. That mismatch shows up as blocks overflowing the box.
#
# The retry works around MicroTeX's line breaker, which is greedy
# first-fit: it takes the last break position that fits and never
# reconsiders, so text spread over several \text{} runs can settle on a
# first line wider than the limit. Asking for a slightly narrower line
# finds a different, fitting set of breaks. Bounded, so genuinely
# unbreakable content (one long word, a table) still returns promptly.
.md_measure <- function(tex, width, gp) {
  d <- latex_dims(tex, max_width = width, input_mode = "math", gp = gp)
  w <- as.numeric(d$width)
  if (width > 0 && w > width) {
    target <- width
    for (i in seq_len(6L)) {
      target <- target * 0.94
      d2 <- latex_dims(tex, max_width = target, input_mode = "math", gp = gp)
      if (as.numeric(d2$width) <= width) {
        return(list(w = as.numeric(d2$width), h = as.numeric(d2$height),
                    bl = as.numeric(d2$baseline), mw = target))
      }
    }
  }
  # `bl` is the baseline as bigpts up from the bottom of the box, so
  # (h - bl) is the baseline measured down from the top -- what the list
  # layout needs to line a marker up with its text.
  list(w = w, h = as.numeric(d$height), bl = as.numeric(d$baseline),
       mw = width)
}

# Build the grob for a run of LaTeX at a given offset. `y` is the
# distance from the top of the content area; vjust = 1 (top) makes the
# offset land on the block's top edge, so stacking is just addition.
.md_run_grob <- function(tex, x, y, width, gp) {
  latex_grob(
    tex,
    x = grid::unit(x, "bigpts"),
    y = grid::unit(1, "npc") - grid::unit(y, "bigpts"),
    hjust = 0, vjust = 1,
    max_width = width,
    input_mode = "math",
    gp = gp
  )
}

# Lay out a list of blocks into positioned items.
#
# Returns list(items = <list of .md_item>, height = <total bigpts>).
# `indent` shifts everything right, which is how nested lists and block
# quotes are built -- each recursion re-measures its content at the
# reduced width so text still wraps inside the indent.
.md_layout <- function(blocks, width, gp, fontsize, indent = 0, first = TRUE) {
  items <- list()
  y <- 0
  gap_para <- 0.55 * fontsize
  # A heading opens a section, so it gets more air above it than the gap
  # between two paragraphs, not less.
  gap_head <- 0.95 * fontsize
  # Code lines advance by a fixed leading rather than by their measured
  # height: a line with no descender measures shorter, and stacking on
  # that would let the next line's ascenders collide with it.
  code_lh <- 1.15 * fontsize

  add <- function(it) items[[length(items) + 1L]] <<- it

  for (i in seq_along(blocks)) {
    blk <- blocks[[i]]
    if (!(first && i == 1L)) {
      y <- y + if (identical(blk$type, "heading")) gap_head else gap_para
    }
    avail <- width - indent

    if (identical(blk$type, "paragraph")) {
      if (!nzchar(blk$tex)) next
      m <- .md_measure(blk$tex, avail, gp)
      # Draw at m$mw, not avail: .md_measure() may have had to narrow the
      # request to get a fitting set of line breaks, and drawing at a
      # different width would re-break the text and undo the fit.
      add(.md_item(.md_run_grob(blk$tex, indent, y, m$mw, gp),
                   indent, y, m$w, m$h))
      y <- y + m$h

    } else if (identical(blk$type, "heading")) {
      tex <- paste0(.MD_HEADING_SIZE[blk$level], "{}\\textbf{", blk$tex, "}")
      m <- .md_measure(tex, avail, gp)
      add(.md_item(.md_run_grob(tex, indent, y, m$mw, gp),
                   indent, y, m$w, m$h))
      y <- y + m$h

    } else if (identical(blk$type, "code_block")) {
      # Code must render in a monospace font. \texttt switches MicroTeX's
      # internal style, but the \text{} content is drawn with the resolved
      # system font, which is the sans body font unless told otherwise --
      # and in a proportional font "<-" comes out misshapen, the "<" set
      # as a raised math relation while the "-" is a low text hyphen.
      gp_code <- gp
      gp_code$fontfamily <- "mono"
      for (ln in blk$lines) {
        tex <- paste0("\\texttt{\\text{", .md_escape_tex(ln), "}}")
        # An empty source line still occupies a row.
        if (!nzchar(ln)) tex <- "\\texttt{\\text{ }}"
        m <- .md_measure(tex, 0, gp_code)
        add(.md_item(.md_run_grob(tex, indent, y, 0, gp_code),
                     indent, y, m$w, code_lh))
        y <- y + code_lh
      }

    } else if (identical(blk$type, "table")) {
      if (!nzchar(blk$tex)) next
      # Tables are not wrapped: max_width does not reach inside tabular
      # cells (see BoxSplitter::split), so asking would silently do
      # nothing and a wide table simply overflows.
      m <- .md_measure(blk$tex, 0, gp)
      add(.md_item(.md_run_grob(blk$tex, indent, y, 0, gp),
                   indent, y, m$w, m$h))
      y <- y + m$h

    } else if (identical(blk$type, "image")) {
      img <- .md_image_raster(blk$path)
      if (is.null(img)) {
        # Missing file, unsupported format, or png/jpeg not installed:
        # show the alt text so the reader still learns what belongs here.
        if (nzchar(blk$alt)) {
          m <- .md_measure(blk$alt, avail, gp)
          add(.md_item(.md_run_grob(blk$alt, indent, y, m$mw, gp),
                       indent, y, m$w, m$h))
          y <- y + m$h
        }
      } else {
        # Pixels are read at 96 dpi, the usual screen assumption, and the
        # image is scaled down to fit the column but never blown up past
        # its natural size.
        nat_w <- img$w_px * 72 / 96
        iw <- min(nat_w, avail)
        ih <- iw * img$h_px / img$w_px
        add(.md_item(
          grid::rasterGrob(
            img$raster,
            x = grid::unit(indent, "bigpts"),
            y = grid::unit(1, "npc") - grid::unit(y, "bigpts"),
            width = grid::unit(iw, "bigpts"),
            height = grid::unit(ih, "bigpts"),
            hjust = 0, vjust = 1, interpolate = TRUE
          ),
          indent, y, iw, ih
        ))
        y <- y + ih
      }

    } else if (identical(blk$type, "thematic_break")) {
      h <- 0.5 * fontsize
      rule <- grid::rectGrob(
        x = grid::unit(indent, "bigpts"),
        y = grid::unit(1, "npc") - grid::unit(y + h / 2, "bigpts"),
        width = grid::unit(avail, "bigpts"),
        height = grid::unit(max(1, fontsize / 20), "bigpts"),
        hjust = 0, vjust = 0.5,
        gp = grid::gpar(fill = gp$col %||% "black", col = NA)
      )
      add(.md_item(rule, indent, y, avail, h))
      y <- y + h

    } else if (identical(blk$type, "block_quote")) {
      bar <- 0.25 * fontsize
      inner <- .md_layout(blk$blocks, width, gp, fontsize,
                          indent = indent + bar * 3, first = TRUE)
      for (it in inner$items) {
        it$grob$vp$y <- it$grob$vp$y - grid::unit(y, "bigpts")
        add(.md_item(it$grob, it$x, y + it$y, it$w, it$h))
      }
      # The rule spans the quoted content, drawn after so it is measured
      # against the final height.
      add(.md_item(
        grid::rectGrob(
          x = grid::unit(indent, "bigpts"),
          y = grid::unit(1, "npc") - grid::unit(y, "bigpts"),
          width = grid::unit(max(1, bar / 2), "bigpts"),
          height = grid::unit(inner$height, "bigpts"),
          hjust = 0, vjust = 1,
          gp = grid::gpar(fill = gp$col %||% "grey60", col = NA)
        ),
        indent, y, bar / 2, inner$height
      ))
      y <- y + inner$height

    } else if (identical(blk$type, "list")) {
      gap_item <- if (isTRUE(blk$tight)) 0.25 * fontsize else gap_para
      # Markers baseline-align to the first line of item text. A reference
      # text line gives that baseline (measured down from the top of the
      # line). Without this the marker is top-aligned, so a bullet floats
      # near the top of the line instead of sitting on the text baseline.
      refd <- latex_dims("\\text{Ag}", input_mode = "math", gp = gp)
      ref_bl_top <- as.numeric(refd$height) - as.numeric(refd$baseline)
      for (j in seq_along(blk$items)) {
        if (j > 1L) y <- y + gap_item
        marker <- .md_list_marker(blk$ordered, blk$start, j,
                                  paren = isTRUE(blk$paren),
                                  checked = if (is.null(blk$checked)) NA
                                            else blk$checked[j])
        mm <- .md_measure(marker, 0, gp)
        # Shift the marker down so its own baseline lands on the text
        # baseline of the first line.
        y_marker <- y + ref_bl_top - (mm$h - mm$bl)
        # Hanging indent: the marker sits at the current indent and the
        # item body is measured at the reduced width, so continuation
        # lines line up under the text rather than under the marker.
        # This is the reason lists are stacked here instead of being
        # handed to MicroTeX's itemize -- max_width does not reach into
        # its cells.
        body_indent <- indent + mm$w + 0.4 * fontsize
        inner <- .md_layout(blk$items[[j]], width, gp, fontsize,
                            indent = body_indent, first = TRUE)
        add(.md_item(.md_run_grob(marker, indent, y_marker, 0, gp),
                     indent, y_marker, mm$w, mm$h))
        for (it in inner$items) {
          it$grob$vp$y <- it$grob$vp$y - grid::unit(y, "bigpts")
          add(.md_item(it$grob, it$x, y + it$y, it$w, it$h))
        }
        # Advance past whichever reaches lower: the body, or a marker that
        # was pushed below it.
        y <- y + max(inner$height, (y_marker - y) + mm$h)
      }
    }
  }

  list(items = items, height = y)
}

# Validate a trbl unit. Split out from .md_trbl() so the constructor can
# reject a bad padding/margin immediately, rather than at draw time when
# the error surfaces far from the call that caused it.
.md_check_trbl <- function(u, what) {
  if (is.null(u)) return(invisible(NULL))
  if (!grid::is.unit(u)) {
    stop("`", what, "` must be a grid unit.", call. = FALSE)
  }
  if (!length(u) %in% c(1L, 4L)) {
    stop("`", what, "` must have length 1 or 4 (top, right, bottom, left).",
         call. = FALSE)
  }
  invisible(NULL)
}

# Resolve a length-1 or length-4 unit into trbl big points.
.md_trbl <- function(u, what) {
  if (is.null(u)) return(c(0, 0, 0, 0))
  .md_check_trbl(u, what)
  n <- length(u)
  v <- vapply(seq_len(n), function(i) {
    # Vertical and horizontal components are converted with the matching
    # function so relative units resolve against the right dimension.
    if (i %% 2L == 1L) {
      grid::convertHeight(u[i], "bigpts", valueOnly = TRUE)
    } else {
      grid::convertWidth(u[i], "bigpts", valueOnly = TRUE)
    }
  }, numeric(1))
  if (n == 1L) rep(v, 4L) else v
}

# Do the geometry once: resolve the box, lay the blocks out, and report
# every dimension makeContent() and the *Details() methods need. Must run
# at draw time, because the width may be relative and because measuring
# text needs an open device.
.md_box_layout <- function(x) {
  margin <- .md_trbl(x$margin, "margin")
  padding <- .md_trbl(x$padding, "padding")

  total_w <- grid::convertWidth(x$width, "bigpts", valueOnly = TRUE)
  box_w <- total_w - margin[2] - margin[4]
  content_w <- max(box_w - padding[2] - padding[4], 1)

  gp <- x$gp %||% grid::gpar()
  fontsize <- gp$fontsize %||% 20
  if (!is.null(gp$cex)) fontsize <- fontsize * gp$cex

  laid <- .md_layout(x$blocks, content_w, gp, fontsize)

  content_h <- laid$height
  box_h <- content_h + padding[1] + padding[3]
  total_h <- if (is.null(x$height)) {
    box_h + margin[1] + margin[3]
  } else {
    grid::convertHeight(x$height, "bigpts", valueOnly = TRUE)
  }
  if (!is.null(x$height)) box_h <- total_h - margin[1] - margin[3]

  list(margin = margin, padding = padding,
       total_w = total_w, total_h = total_h,
       box_w = box_w, box_h = box_h,
       content_w = content_w, content_h = content_h,
       items = laid$items)
}

#' @export
makeContent.markdownbox <- function(x) {
  lay <- .md_box_layout(x)
  kids <- list()

  # Background / border, inset by the margin.
  if (!is.null(x$box_gp)) {
    rr <- x$r %||% grid::unit(0, "pt")
    common <- list(
      x = grid::unit(lay$margin[4], "bigpts"),
      y = grid::unit(1, "npc") - grid::unit(lay$margin[1], "bigpts"),
      width = grid::unit(lay$box_w, "bigpts"),
      height = grid::unit(lay$box_h, "bigpts"),
      hjust = 0, vjust = 1, gp = x$box_gp
    )
    box <- if (grid::convertWidth(rr, "bigpts", valueOnly = TRUE) > 0) {
      do.call(grid::roundrectGrob, c(common, list(r = rr)))
    } else {
      do.call(grid::rectGrob, common)
    }
    kids[[length(kids) + 1L]] <- box
  }

  # Vertical placement of the content inside a taller fixed box.
  slack <- max(lay$box_h - lay$padding[1] - lay$padding[3] - lay$content_h, 0)
  voff <- slack * (1 - (x$valign %||% 1))

  halign <- x$halign %||% 0
  content <- lapply(lay$items, function(it) {
    g <- it$grob
    if (halign != 0 && !is.null(g$vp)) {
      # Slack to the right of this item, so halign cooperates with the
      # per-block indent instead of fighting it.
      shift <- (lay$content_w - it$x - it$w) * halign
      g$vp$x <- g$vp$x + grid::unit(shift, "bigpts")
    }
    g
  })

  kids <- c(kids, list(grid::gTree(
    children = do.call(grid::gList, content),
    vp = grid::viewport(
      x = grid::unit(lay$margin[4] + lay$padding[4], "bigpts"),
      y = grid::unit(1, "npc") -
        grid::unit(lay$margin[1] + lay$padding[1] + voff, "bigpts"),
      width = grid::unit(lay$content_w, "bigpts"),
      height = grid::unit(lay$content_h, "bigpts"),
      just = c(0, 1)
    )
  )))

  grid::setChildren(x, do.call(grid::gList, kids))
}

#' @export
widthDetails.markdownbox <- function(x) {
  grid::unit(.md_box_layout(x)$total_w, "bigpts")
}

#' @export
heightDetails.markdownbox <- function(x) {
  grid::unit(.md_box_layout(x)$total_h, "bigpts")
}

#' Render a markdown document as a boxed grid grob
#'
#' @description
#' Lays markdown out as a block document --- headings, paragraphs, lists,
#' block quotes, code blocks, tables and horizontal rules --- inside an
#' optional padded, filled and bordered box. Prose wraps to the requested
#' width, and \code{$...$} math is typeset by MicroTeX as usual.
#'
#' @details
#' Where \code{\link{markdown_grob}} flattens everything into a single
#' run, this stacks one grob per block. That is what makes headings, list
#' indentation, block-quote rules and background fills possible:
#' MicroTeX has no concept of any of them, and its line breaking does not
#' reach inside the cells it uses for list and table layout.
#'
#' Two consequences worth knowing. Table cells and code lines are not
#' wrapped, so a wide table overflows rather than reflowing. And list
#' items are stacked here rather than handed to MicroTeX's
#' \code{itemize}, which is what gives them a proper hanging indent.
#'
#' @param md Character string of markdown.
#' @param x,y Position of the box in the parent viewport.
#' @param width Width of the box, including \code{margin}.
#' @param height Fixed height, or \code{NULL} (default) to take whatever
#'   height the content needs.
#' @param hjust,vjust Justification of the whole box about \code{x} and
#'   \code{y}.
#' @param halign Horizontal alignment of blocks within the box:
#'   \code{0} left (default), \code{0.5} centred, \code{1} right.
#' @param valign Vertical alignment of the content when \code{height}
#'   leaves room to spare: \code{1} top (default), \code{0} bottom.
#' @param padding,margin A \code{\link[grid]{unit}} of length 1 or 4
#'   giving top, right, bottom and left. Padding is inside the box,
#'   margin outside it.
#' @param box_gp Graphical parameters for the box itself, e.g.
#'   \code{gpar(fill = "grey95", col = "black")}. \code{NULL} (default)
#'   draws no box.
#' @param r Corner radius; a non-zero value draws a rounded box.
#' @param name Optional grob name.
#' @param gp Graphical parameters for the text. \code{fontsize} also sets
#'   the scale for block spacing and list indentation.
#' @param vp Optional viewport.
#' @return A \code{markdownbox} gTree.
#' @seealso \code{\link{markdown_grob}}, \code{\link{latex_grob}}
#' @export
#'
#' @examples
#' \donttest{
#'   md <- paste(
#'     "# Results", "",
#'     "The slope is $\\beta_1$ with *p* < 0.001.", "",
#'     "- first point", "- second point",
#'     sep = "\n"
#'   )
#'   grid::grid.newpage()
#'   grid::grid.draw(markdown_box_grob(
#'     md,
#'     width = grid::unit(4, "in"),
#'     padding = grid::unit(8, "pt"),
#'     box_gp = grid::gpar(fill = "grey95", col = "grey40")
#'   ))
#' }
markdown_box_grob <- function(md,
                              x = grid::unit(0.5, "npc"),
                              y = grid::unit(0.5, "npc"),
                              width = grid::unit(1, "npc"),
                              height = NULL,
                              hjust = 0.5, vjust = 0.5,
                              halign = 0, valign = 1,
                              padding = grid::unit(0, "pt"),
                              margin = grid::unit(0, "pt"),
                              box_gp = NULL,
                              r = grid::unit(0, "pt"),
                              name = NULL,
                              gp = grid::gpar(),
                              vp = NULL) {
  if (!is.character(md) || length(md) != 1L) {
    stop("`md` must be a single character string.", call. = FALSE)
  }
  if (is.na(md)) {
    stop("`md` must not be NA.", call. = FALSE)
  }
  .md_check_trbl(padding, "padding")
  .md_check_trbl(margin, "margin")
  # Parse once at construction; the layout is redone at draw time, when
  # relative widths resolve and a device is available for measuring.
  blocks <- .md_parse_blocks(md)

  grid::gTree(
    md = md, blocks = blocks,
    width = width, height = height,
    halign = halign, valign = valign,
    padding = padding, margin = margin,
    box_gp = box_gp, r = r,
    gp = gp, name = name, cl = "markdownbox",
    vp = vp %||% grid::viewport(
      x = x, y = y,
      width = width,
      height = height %||% grid::unit(1, "npc"),
      just = c(hjust, vjust)
    )
  )
}
