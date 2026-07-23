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
# Escape the prose, restore the math spans raw, then hand the result to
# latex_wrap(): the output of this file is fed to latex_grob() in "math"
# mode, so prose has to arrive already wrapped in \text{} or it would be
# typeset as spaced math italics. latex_wrap() also strips the `$`
# delimiters and adds \displaystyle for block math, which is exactly the
# translation needed here.
#
# Order matters twice over: escape before unmasking, so the escaper never
# touches a formula; unmask before latex_wrap(), so it can see the real
# delimiters while escaped prose dollars stay literal.
.md_text_node <- function(s, spans) {
  latex_wrap(.md_unmask_math(.md_escape_tex(s), spans), input_mode = "mixed")
}

# Walk the inline children of a cmark node and return a LaTeX string.
#
# `spans` carries the masked math so text nodes can restore it. Node
# types cmark can emit that MicroTeX has no equivalent for degrade to
# their text content rather than emitting an unknown command -- MicroTeX
# renders unknown commands as literal glyphs instead of erroring, so a
# stray \href would silently typeset the letters "href".
.md_inline_to_tex <- function(node, spans) {
  kids <- xml2::xml_contents(node)
  if (length(kids) == 0L) return("")
  parts <- vapply(kids, function(k) {
    nm <- xml2::xml_name(k)
    switch(nm,
      text = .md_text_node(xml2::xml_text(k), spans),
      # \sout, \cancel, \bcancel and \xcancel are all registered in
      # MicroTeX (macro_def.cpp); \sout is the horizontal rule that
      # matches markdown's ~~strike~~.
      strikethrough = paste0("\\sout{", .md_inline_to_tex(k, spans), "}"),
      strong        = paste0("\\textbf{", .md_inline_to_tex(k, spans), "}"),
      emph          = paste0("\\textit{", .md_inline_to_tex(k, spans), "}"),
      # Code spans are literal. Restore any masked math first so the
      # sentinel never leaks, then escape the lot -- `$x$` in backticks
      # is meant to be shown as characters, not typeset as math. The
      # explicit \text{} keeps it in text mode rather than math italics.
      code = paste0("\\texttt{\\text{",
                    .md_escape_tex(.md_unmask_math(xml2::xml_text(k), spans)),
                    "}}"),
      # No \href in MicroTeX: keep the link text, drop the destination.
      link          = .md_inline_to_tex(k, spans),
      # Images cannot be drawn by a text grob; keep the alt text.
      image         = .md_inline_to_tex(k, spans),
      softbreak     = " ",
      linebreak     = "\\\\",
      # html_inline and anything else unknown: drop the markup, keep text.
      html_inline   = "",
      .md_text_node(xml2::xml_text(k), spans)
    )
  }, character(1))
  paste(parts, collapse = "")
}

# Convert markdown to a LaTeX string suitable for input_mode = "math".
# Only inline content is handled here; block structure is Phase 3.
# Multiple paragraphs are joined with `\\` line breaks.
#
# NOTE: emitting `\\` disables MicroTeX's max_width wrapping for the
# whole expression (the top box becomes a VBox, which the splitter
# currently declines to descend into), so a multi-paragraph string will
# not wrap. Single-paragraph input -- the common case for a label --
# wraps normally.
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
#' Not every markdown feature has a MicroTeX equivalent. Links keep their
#' text and drop the destination, images keep their alt text, and inline
#' HTML is dropped. Block-level markdown (headings, lists, block quotes,
#' tables) is rendered by \code{markdown_box_grob()}.
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
