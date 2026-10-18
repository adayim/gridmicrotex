# Raw HTML blocks in markdown.
#
# CommonMark hands a block that starts with a block-level tag (`<p align=
# "center">`, `<details>`, `<center>`, `<table>`, a `<div>` with its text on
# the line) over as one opaque `html_block`. They were dropped, text and
# all, which lost a README's centred header and every `<details>`. Here the
# block is read as HTML (libxml2, through xml2) and what it holds is written
# as the markdown that says the same -- paragraphs, headings, lists, tables,
# code, quotes, rules, a centred `<div>` -- which .md_blocks() then lays out
# as it does any markdown.
#
# The text is literal, as it is in HTML: `**x**` in a `<p>` is not emphasis.
# Inline tags the inline walker knows (<sub>, <mark>, <span style>, <a href>,
# <kbd>...) are kept as they are, so a stylesheet reaches them as it reaches
# any other; <img> is checked first, and one that cannot be drawn is left out
# with a warning (a README's badges are URLs, and a hard error would turn a
# block that used to vanish into a failure).
#
# Math stays masked all the way through: the sentinels are private-use
# characters and digits, which no escape here touches, and the markdown is
# parsed with the spans the block came with (.md_parse_doc(spans = )).

# What a browser does not show, or the GFM tagfilter removes: gone with
# their content.
.HB_DROP <- c("script", "style", "iframe", "object", "embed", "noscript",
              "template", "head", "title", "meta", "link", "svg", "canvas",
              "audio", "video", "form", "button", "input", "select",
              "textarea", "xmp", "noembed", "noframes", "plaintext")

# Elements that start a block of their own. Anything else is inline.
.HB_BLOCK <- c("p", "div", "center", "section", "article", "main", "header",
               "footer", "nav", "aside", "figure", "figcaption", "details",
               "summary", "h1", "h2", "h3", "h4", "h5", "h6", "ul", "ol",
               "li", "table", "thead", "tbody", "tfoot", "tr", "blockquote",
               "pre", "hr", "dl", "dt", "dd", "address", "fieldset", "legend")

# Nesting is bounded: the markdown written here has no raw block in it that
# could come back, but a rule that says so is cheaper than a proof.
.hb_state <- new.env(parent = emptyenv())
.hb_state$depth <- 0L

# Evaluate `expr` one level deeper.
.hb_nested <- function(expr) {
  old <- .hb_state$depth
  .hb_state$depth <- old + 1L
  on.exit(.hb_state$depth <- old, add = TRUE)
  expr
}

# The markdown a raw HTML block stands for, "" when it holds nothing a reader
# would see.
.md_html_block_md <- function(txt) {
  if (.hb_state$depth >= 3L) return("")
  doc <- tryCatch(
    xml2::read_html(paste0("<html><body>", txt, "</body></html>"),
                    options = c("RECOVER", "NOERROR", "NOWARNING")),
    error = function(e) NULL)
  body <- if (is.null(doc)) NULL else xml2::xml_find_first(doc, "//body")
  if (is.null(body) || inherits(body, "xml_missing")) return("")
  paste(.hb_blocks(xml2::xml_contents(body)), collapse = "\n\n")
}

# Text as HTML shows it, escaped so that markdown reads it as typed: the
# whitespace runs are one space, and every character that can start markup
# has a backslash. (`$` is left alone: a math span is already a sentinel, and
# a lone dollar is handled by the escaper that draws the text.)
.hb_text <- function(s) {
  s <- gsub("[ \t\r\n]+", " ", s)
  gsub("([\\\\`*_\\[\\]<>#|~&])", "\\\\\\1", s, perl = TRUE)
}

# A paragraph that would read as something else -- "- x", "1. x", "+ x" --
# has its first character escaped.
.hb_guard <- function(s) {
  s <- sub("^([-+])", "\\\\\\1", s)
  sub("^([0-9]+)([.)])", "\\1\\\\\\2", s)
}

# The value of an attribute, or NULL.
.hb_attr <- function(nd, name) {
  v <- xml2::xml_attr(nd, name)
  if (is.na(v)) NULL else v
}

# How a block is aligned: its `align` attribute, a `text-align` in its style,
# or <center> itself. NULL for the default.
.hb_align <- function(nd) {
  a <- tolower(trimws(.hb_attr(nd, "align") %||% ""))
  if (!nzchar(a)) {
    m <- regmatches(.hb_attr(nd, "style") %||% "",
                    regexec("text-align\\s*:\\s*([a-z]+)", .hb_attr(nd, "style") %||% "",
                            ignore.case = TRUE))[[1]]
    if (length(m) == 2L) a <- tolower(m[2])
  }
  if (identical(xml2::xml_name(nd), "center")) a <- "center"
  if (a %in% c("center", "right", "justify")) a else NULL
}

# The blocks an element's children make, with the element's own box: a
# <div> (or an aligned element) is a markdown <div> round them, which carries
# its class, its style and its alignment as it does for markdown written so.
.hb_container <- function(nd, kids, force_div = FALSE) {
  inner <- .hb_blocks(kids)
  align <- .hb_align(nd)
  cls <- .hb_attr(nd, "class")
  sty <- .hb_attr(nd, "style")
  if (!is.null(align) && !grepl("text-align", sty %||% "", fixed = TRUE)) {
    sty <- paste0("text-align: ", align, if (!is.null(sty)) paste0("; ", sty))
  }
  if (!length(inner)) return(character(0))
  if (is.null(cls) && is.null(sty)) return(inner)
  attrs <- c(if (!is.null(cls)) sprintf(" class=\"%s\"", gsub("\"", "&quot;", cls)),
             if (!is.null(sty)) sprintf(" style=\"%s\"", gsub("\"", "&quot;", sty)))
  c(paste0("<div", paste(attrs, collapse = ""), ">"), inner, "</div>")
}

# Blocks of markdown (a character vector, one string a block) for some
# sibling nodes: the inline ones between block elements make a paragraph.
.hb_blocks <- function(nodes) {
  out <- character(0)
  run <- character(0)
  flush <- function() {
    s <- trimws(paste(run, collapse = ""))
    if (nzchar(s)) out <<- c(out, .hb_guard(s))
    run <<- character(0)
  }
  for (nd in nodes) {
    type <- xml2::xml_type(nd)
    if (identical(type, "text")) {
      run <- c(run, .hb_text(xml2::xml_text(nd)))
      next
    }
    if (!identical(type, "element")) next
    nm <- xml2::xml_name(nd)
    if (nm %in% .HB_DROP) next
    if (nm %in% .HB_BLOCK) {
      flush()
      out <- c(out, .hb_block(nd, nm))
    } else {
      run <- c(run, .hb_inline(nd))
    }
  }
  flush()
  out
}

# One block element as markdown.
.hb_block <- function(nd, nm) {
  kids <- xml2::xml_contents(nd)
  switch(nm,
    hr = "---",
    h1 = , h2 = , h3 = , h4 = , h5 = , h6 = {
      text <- trimws(gsub("\n+", " ", .hb_inline_text(kids)))
      if (!nzchar(text)) return(character(0))
      head <- paste0(strrep("#", as.integer(substring(nm, 2L))), " ", text)
      align <- .hb_align(nd)
      if (is.null(align)) head else
        c(sprintf("<div style=\"text-align: %s\">", align), head, "</div>")
    },
    pre = .hb_pre(nd),
    blockquote = {
      inner <- .hb_blocks(kids)
      if (!length(inner)) return(character(0))
      body <- paste(inner, collapse = "\n\n")
      paste(sub("^", "> ", strsplit(body, "\n", fixed = TRUE)[[1]]), collapse = "\n")
    },
    ul = , ol = .hb_list(nd, nm == "ol"),
    table = .hb_table(nd),
    details = .hb_blocks(kids),
    summary = {
      text <- trimws(gsub("\n+", " ", .hb_inline_text(kids)))
      if (nzchar(text)) paste0("**", text, "**") else character(0)
    },
    dt = {
      text <- trimws(gsub("\n+", " ", .hb_inline_text(kids)))
      if (nzchar(text)) paste0("**", text, "**") else character(0)
    },
    # p, div, center, section, dd, li outside a list...: their content, in a
    # <div> of its own when it carries a class, a style or an alignment.
    .hb_container(nd, kids))
}

# <pre> is a fenced block; its fence is longer than any run of backticks in it.
.hb_pre <- function(nd) {
  text <- sub("^\n", "", xml2::xml_text(nd))
  text <- sub("\n$", "", text)
  if (!nzchar(trimws(text))) return(character(0))
  code <- xml2::xml_find_first(nd, "./code")
  cls <- if (inherits(code, "xml_missing")) "" else .hb_attr(code, "class") %||% ""
  lang <- sub(".*\\blanguage-([A-Za-z0-9_+-]+).*|.*", "\\1", cls, perl = TRUE)
  runs <- regmatches(text, gregexpr("`+", text))[[1]]
  fence <- strrep("`", max(3L, nchar(runs) + 1L))
  paste0(fence, lang, "\n", text, "\n", fence)
}

# A list: each <li> is an item, its blocks the item's, indented under the marker.
.hb_list <- function(nd, ordered) {
  items <- xml2::xml_find_all(nd, "./li")
  if (!length(items)) return(character(0))
  start <- suppressWarnings(as.integer(.hb_attr(nd, "start") %||% "1"))
  if (is.na(start)) start <- 1L
  lines <- vapply(seq_along(items), function(i) {
    blocks <- .hb_blocks(xml2::xml_contents(items[[i]]))
    if (!length(blocks)) return("")
    marker <- if (ordered) paste0(start + i - 1L, ". ") else "- "
    pad <- strrep(" ", nchar(marker))
    # A list inside the item follows its text closely: a blank line between
    # them would make the whole list loose.
    joined <- blocks[1L]
    for (b in blocks[-1L]) {
      joined <- paste0(joined, if (grepl("^([-*+] |[0-9]+[.)] )", b)) "\n" else "\n\n", b)
    }
    body <- strsplit(joined, "\n", fixed = TRUE)[[1]]
    body[-1L] <- ifelse(nzchar(body[-1L]), paste0(pad, body[-1L]), "")
    paste0(marker, paste(body, collapse = "\n"))
  }, character(1))
  lines <- lines[nzchar(lines)]
  if (!length(lines)) character(0) else paste(lines, collapse = "\n")
}

# A table is a GFM table: its first row is the header (a row of <th>, or the
# first row), the alignment comes from the header's `align`.
.hb_table <- function(nd) {
  # Its own rows: those of a table inside a cell are that table's. A row of
  # cells with no <tr> round them is one row, as a browser sets it.
  rows <- xml2::xml_find_all(nd, "./tr|./thead/tr|./tbody/tr|./tfoot/tr")
  cells <- lapply(rows, function(r) xml2::xml_find_all(r, "./th|./td"))
  loose <- xml2::xml_find_all(nd, "./th|./td")
  if (length(loose)) cells <- c(list(loose), cells)
  cells <- Filter(length, cells)
  if (!length(cells)) return(character(0))
  ncol <- max(lengths(cells))
  cell_md <- function(cell) {
    text <- trimws(gsub("\n+", " ", .hb_inline_text(xml2::xml_contents(cell))))
    # The text's own pipes are escaped already; one in a code span is not.
    gsub("(?<!\\\\)\\|", "\\\\|", text, perl = TRUE)
  }
  line <- function(cs) {
    v <- vapply(cs, cell_md, character(1))
    paste0("| ", paste(c(v, rep("", ncol - length(v))), collapse = " | "), " |")
  }
  align <- vapply(seq_len(ncol), function(j) {
    a <- if (j <= length(cells[[1L]])) tolower(.hb_attr(cells[[1L]][[j]], "align") %||% "") else ""
    switch(a, center = ":---:", right = "---:", left = ":---", "---")
  }, character(1))
  paste(c(line(cells[[1L]]), paste0("| ", paste(align, collapse = " | "), " |"),
          vapply(cells[-1L], line, character(1))), collapse = "\n")
}

# Inline content as one string of markdown: the text escaped, the elements
# as .hb_inline() writes them.
.hb_inline_text <- function(nodes) {
  paste(vapply(nodes, function(nd) {
    type <- xml2::xml_type(nd)
    if (identical(type, "text")) return(.hb_text(xml2::xml_text(nd)))
    if (!identical(type, "element")) return("")
    nm <- xml2::xml_name(nd)
    if (nm %in% .HB_DROP) return("")
    .hb_inline(nd)
  }, character(1)), collapse = "")
}

# The opening tag of an element, as it was written.
.hb_open_tag <- function(nd) {
  at <- xml2::xml_attrs(nd)
  if (!length(at)) return(paste0("<", xml2::xml_name(nd), ">"))
  paste0("<", xml2::xml_name(nd),
         paste0(" ", names(at), "=\"", gsub("\"", "&quot;", at), "\"", collapse = ""), ">")
}

# One inline element as markdown, or as the inline HTML the inline walker reads.
.hb_inline <- function(nd) {
  nm <- xml2::xml_name(nd)
  kids <- xml2::xml_contents(nd)
  inner <- function() .hb_inline_text(kids)
  wrap <- function(mark) {
    s <- inner()
    if (nzchar(trimws(s))) paste0(mark, trimws(s), mark) else s
  }
  switch(nm,
    br = "  \n",
    b = , strong = wrap("**"),
    i = , em = , cite = , var = , dfn = wrap("*"),
    del = , s = , strike = wrap("~~"),
    code = , tt = , samp = {
      text <- xml2::xml_text(nd)
      if (!nzchar(trimws(text))) return("")
      runs <- regmatches(text, gregexpr("`+", text))[[1]]
      fence <- strrep("`", max(1L, nchar(runs) + 1L))
      paste0(fence, " ", gsub("[ \t\r\n]+", " ", text), " ", fence)
    },
    img = .hb_img(nd),
    {
      # What the inline walker reads (<sub>, <mark>, <span>, <a href>, <q>,
      # <u>, <kbd>...) is kept with its tags; any other element is its text.
      known <- nm %in% c(.MD_CASCADE_TAGS, "q") || !is.null(.MD_HTML_TAGS[[nm]])
      if (known) paste0(.hb_open_tag(nd), inner(), "</", nm, ">") else inner()
    })
}

# An <img> the markdown can draw, or nothing and a warning.
.hb_img <- function(nd) {
  src <- trimws(.hb_attr(nd, "src") %||% "")
  problem <- tryCatch({ .image_check(src); NULL }, error = function(e) conditionMessage(e))
  if (!is.null(problem)) {
    warning("raw HTML <img src=\"", src, "\"> is left out: ", problem, call. = FALSE)
    return("")
  }
  keep <- c("src", "width", "height")
  at <- xml2::xml_attrs(nd)
  at <- at[names(at) %in% keep]
  paste0("<img", if (length(at)) paste0(" ", names(at), "=\"", gsub("\"", "&quot;", at), "\"", collapse = ""), ">")
}
