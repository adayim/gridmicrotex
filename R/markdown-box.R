# Block-level markdown: a document laid out in grid.
#
# The other half of markdown.R, which handles the inline pipeline -- one
# run of text, no layout. The split is the same one the API makes:
# markdown_grob() is a run, markdown_box_grob() is a document.
#
# Everything here builds on markdown.R and not the other way round: the
# block walker calls .md_inline_to_tex() for each block's prose, and the
# CSS value resolvers (.md_css_length(), .md_resolve_color(), ...) live
# over there because the inline path needs them too.
#
# Three stages, in order:
#
#   .md_parse_blocks()  CommonMark AST -> a list of typed blocks, each
#                       carrying the LaTeX for its own content
#   .md_layout()        blocks -> positioned items, measured against a
#                       width; recurses for lists and block quotes
#   .md_box_layout()    the box around them -- margin, padding, chrome --
#                       resolved at draw time, since a relative width
#                       needs a viewport and measuring needs a device
#
# Why any of this is in R rather than MicroTeX: the engine cannot lay a
# document out in one parse. A `\` puts a VBox at the top of the box tree
# and, inside matrix/array cells, max_width is not honoured at all (see
# the note in BoxSplitter::split). So block structure is assembled here
# -- one latex_grob per block, stacked in grid -- which is also where
# boxes, padding, background fills and rules have to live, none of which
# MicroTeX models.

# Heading level -> MicroTeX size macro. All ten of the \tiny..\Huge
# family are registered (macro_def.cpp); these six are the ones a
# markdown heading can reach.
.MD_HEADING_SIZE <- c("\\Huge", "\\huge", "\\LARGE",
                      "\\Large", "\\large", "\\normalsize")

# Classify an html_block as a <div> boundary, or neither.
#
# cmark hands `<div ...>` and `</div>` over as separate html_block
# siblings whenever there are blank lines around them, with the markdown
# between them parsed into real blocks -- which is what makes a div
# usable as a container here. Written without blank lines the whole thing
# arrives as one opaque block instead, and falls through to "other".
.md_div_tag <- function(txt) {
  t <- trimws(txt)
  if (grepl("^</[[:space:]]*div[[:space:]]*>$", t, ignore.case = TRUE)) {
    return(list(kind = "close"))
  }
  m <- regmatches(t, regexec("^<[[:space:]]*div([^>]*)>$", t,
                             ignore.case = TRUE))[[1]]
  if (length(m) != 2L) return(list(kind = "other"))
  list(kind = "open",
       class = .md_html_classes(m[2]),
       # Hand-written markup, so an unsupported property is worth a word
       # -- see the note on .md_parse_css().
       style = .md_parse_css(.md_html_attr(m[2], "style"), warn = TRUE))
}

# Parse markdown into a list of block descriptors. Recurses through
# containers (lists, block quotes) so nesting is preserved, and folds
# <div>...</div> runs into container blocks of their own.
#
# `ctx` is the enclosing container's resolved style. It is threaded here,
# and not only at layout time, because a block's *content* has to know
# whether an emphasis command will wrap it: \textbf{\text{x}} silently
# reports plain, so prose destined for \textbf{} must be emitted bare.
# The cascade is the same function either way, so the two passes cannot
# disagree about the rules.
.md_blocks <- function(nodes, spans, base = 20, style = NULL, ctx = list()) {
  out <- list()
  # Open <div>s, innermost last. Blocks are collected into the innermost
  # one until it closes, and each carries the context its contents see.
  open <- list()
  # Whether the separator before the footnote section has been emitted.
  seen_fn <- FALSE

  cur_ctx <- function() if (length(open)) open[[length(open)]]$ctx else ctx
  emit <- function(blk) {
    k <- length(open)
    if (k) open[[k]]$blocks <<- c(open[[k]]$blocks, list(blk))
    else out <<- c(out, list(blk))
  }
  close_div <- function() {
    d <- open[[length(open)]]
    open[[length(open)]] <<- NULL
    emit(list(type = "div", class = d$class, style = d$style,
              blocks = d$blocks))
  }
  # Prose for a block, emitted bare when emphasis will wrap it.
  prose <- function(nd, tag) {
    res <- .md_cascade(style %||% markdown_style(), tag, inherited = cur_ctx())
    emph <- .md_emphasis_cmds(res)
    .md_inline_to_tex(nd, spans, bare = length(emph) > 0L, styles = emph,
                      base = base, style = style)
  }
  # The context a container hands to its children.
  child_ctx <- function(tag) {
    .md_cascade(style %||% markdown_style(), tag, inherited = cur_ctx())
  }

  for (nd in nodes) {
    nm <- xml2::xml_name(nd)

    if (identical(nm, "html_block")) {
      tg <- .md_div_tag(xml2::xml_text(nd))
      if (identical(tg$kind, "open")) {
        open[[length(open) + 1L]] <- list(
          class = tg$class, style = tg$style, blocks = list(),
          ctx = .md_cascade(style %||% markdown_style(), "div",
                            classes = tg$class, inline = tg$style,
                            inherited = cur_ctx()))
      } else if (identical(tg$kind, "close") && length(open)) {
        close_div()
      }
      # Any other raw block, and a stray </div>, is dropped rather than
      # passed through: MicroTeX would typeset the markup.
      next
    }

    # A paragraph holding nothing but images is a block image, which is
    # how markdown renderers treat it. Handled before the switch because
    # one paragraph can yield several image blocks.
    if (identical(nm, "paragraph")) {
      imgs <- .md_image_blocks(nd, spans, base, style)
      if (!is.null(imgs)) {
        for (im in imgs) emit(im)
        next
      }
    }
    # A paragraph that is nothing but `$$...$$` is a display equation, not
    # prose: it gets its own centred block, as in every other markdown
    # dialect. Inline `$...$` is untouched.
    if (identical(nm, "paragraph") && !is.na(.md_lone_math_span(nd, spans))) {
      emit(list(type = "mathblock", tex = prose(nd, "math")))
      next
    }
    blk <- switch(nm,
      paragraph = list(type = "paragraph", tex = prose(nd, "p")),
      heading = {
        lvl <- .md_heading_level(nd)
        list(type = "heading", level = lvl,
             tex = prose(nd, paste0("h", lvl)))
      },
      block_quote = list(type = "block_quote",
                         blocks = .md_blocks(xml2::xml_children(nd), spans,
                                             base, style,
                                             child_ctx("blockquote"))),
      code_block = list(type = "code_block",
                        lines = .md_code_lines(nd, spans),
                        # The fence's info string, e.g. "python". NA when
                        # the fence names no language.
                        lang = xml2::xml_attr(nd, "info")),
      list = .md_list_block(nd, spans, base, style, cur_ctx()),
      # Built content-sized here, which is what every caller sees. The
      # node is kept as well so .md_layout() can rebuild the table when
      # `table-layout: fixed` asks for the columns to be divided out of
      # the *available* width, which is not known until the block is
      # placed. xml2 keeps the document alive through the node.
      table = list(type = "table",
                   tex = .md_table_tex(nd, spans, base, style,
                                       inherited = cur_ctx()),
                   nd = nd, spans = spans, base = base, ctx = cur_ctx()),
      thematic_break = list(type = "thematic_break"),
      # CommonMark already gathers footnote definitions at the end of the
      # document, in reference order, so they need no reordering here --
      # only a separator before the first one.
      fn = {
        # Plain <- : this runs in .md_blocks()'s own frame, where <<- would
        # skip the local and assign into the enclosing environment.
        if (!seen_fn) {
          seen_fn <- TRUE
          emit(list(type = "thematic_break"))
        }
        list(type = "footnote",
             blocks = .md_blocks(xml2::xml_children(nd), spans, base, style,
                                 child_ctx("footnote")))
      },
      # Anything unrecognised is dropped.
      NULL
    )
    if (!is.null(blk)) emit(blk)
  }

  # A div left open at the end of a container closes here, so the blocks
  # inside it are still laid out rather than lost.
  while (length(open)) close_div()
  out
}

.md_heading_level <- function(nd) {
  lvl <- suppressWarnings(as.integer(xml2::xml_attr(nd, "level")))
  if (is.na(lvl)) lvl <- 1L
  min(max(lvl, 1L), 6L)
}

# Code blocks are literal. MicroTeX has no verbatim environment, so each
# source line becomes its own \texttt{} block and the stacker puts them
# on separate rows.
#
# Indentation is *not* preserved by that split -- MicroTeX collapses a
# run of spaces inside one \text{} run, so the leading whitespace of a
# line used to measure one space wide however deep it was. See
# .hl_segments(), which breaks a line apart wherever spaces run two or
# more deep.
#
# Tabs are expanded here rather than at emit time, because the classifier
# works in character positions and would be thrown off by a later
# substitution that changes the length of a line.
.md_code_lines <- function(nd, spans = NULL) {
  # Math is masked out of the *whole* document before CommonMark parses
  # it, which cannot know about fences -- so a `$...$` written inside a
  # code block arrives here as a sentinel. Restore the original bytes:
  # in code a dollar is literal, and .md_escape_tex() neutralises it.
  # Without this the span's content is lost outright and the sentinel's
  # index is drawn in its place, turning `# see $E = mc^2$ here` into
  # `# see 1 here`. Inline code spans already do this; blocks did not.
  txt <- .md_unmask_math(xml2::xml_text(nd), spans)
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0L) return(character(0))
  # A trailing newline yields a spurious empty final line.
  if (length(lines) > 1L && !nzchar(lines[length(lines)])) {
    lines <- lines[-length(lines)]
  }
  vapply(lines, .md_expand_tabs, character(1), USE.NAMES = FALSE)
}

# A tab advances to the next multiple-of-4 column, which is what every
# editor and terminal does -- not a fixed four spaces. The difference
# shows the moment a tab follows anything: `abc<tab>x` puts x at column
# 5, not 9, so tab-indented code (a Makefile has no choice) lines up.
# Column counting restarts on each line, so this cannot be a gsub over
# the whole block.
.md_expand_tabs <- function(s, width = 4L) {
  if (!grepl("\t", s, fixed = TRUE)) return(s)
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  out <- character(length(chars))
  col <- 0L
  for (i in seq_along(chars)) {
    if (identical(chars[i], "\t")) {
      pad <- width - (col %% width)
      out[i] <- strrep(" ", pad)
      col <- col + pad
    } else {
      out[i] <- chars[i]
      col <- col + 1L
    }
  }
  paste0(out, collapse = "")
}

.md_list_block <- function(nd, spans, base = 20, style = NULL, ctx = list()) {
  ordered <- identical(xml2::xml_attr(nd, "type"), "ordered")
  start <- suppressWarnings(as.integer(xml2::xml_attr(nd, "start")))
  if (is.na(start)) start <- 1L
  kids <- xml2::xml_children(nd)
  # An item's contents see ul/ol and then li, the same chain .md_layout()
  # walks when it resolves the same blocks for drawing.
  st <- style %||% markdown_style()
  item_ctx <- .md_cascade(st, "li",
                          inherited = .md_cascade(st, if (ordered) "ol" else "ul",
                                                  inherited = ctx))
  items <- lapply(kids, function(it) {
    .md_blocks(xml2::xml_children(it), spans, base, style, item_ctx)
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
.md_image_blocks <- function(nd, spans, base = 20, style = NULL) {
  kids <- xml2::xml_children(nd)
  if (length(kids) == 0L) return(NULL)
  nms <- xml2::xml_name(kids)
  if (!all(nms %in% c("image", "softbreak"))) return(NULL)
  lapply(which(nms == "image"), function(i) {
    im <- kids[[i]]
    list(type = "image",
         path = xml2::xml_attr(im, "destination"),
         alt = .md_inline_to_tex(im, spans, base = base, style = style))
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
.md_table_tex <- function(nd, spans, base = 20, style = NULL,
                          avail = 0, inherited = list()) {
  rows <- xml2::xml_children(nd)
  if (length(rows) == 0L) return("")
  st <- style %||% markdown_style()

  # The four table tags resolve once. `tr`/`td`/`th` inherit through
  # `table`, so `table { color: }` reaches the cells as CSS says it should.
  r_table <- .md_cascade(st, "table", inherited = inherited)
  r_tr    <- .md_cascade(st, "tr", inherited = r_table)
  r_td    <- .md_cascade(st, "td", inherited = r_table)
  r_th    <- .md_cascade(st, "th", inherited = r_table)

  # A cell's content, wrapped in whatever emphasis its tag asks for. The
  # emphasis has to be applied here rather than around the finished table:
  # \text{} builds a non-nested FontStyleAtom, so \textbf{\text{x}} comes
  # out plain -- which is why header cells were never bold.
  cell_tex <- function(cell, res) {
    emph <- .md_emphasis_cmds(res)
    inner <- .md_inline_to_tex(cell, spans, bare = length(emph) > 0L,
                               styles = emph, base = base, style = style)
    fill <- .md_resolve_color(res$background)
    paste0(if (!is.null(fill)) paste0("\\cellcolor{", fill, "}"),
           paste(emph, collapse = ""), inner, strrep("}", length(emph)))
  }
  cells_of <- function(r, res) {
    vapply(xml2::xml_children(r), cell_tex, character(1), res = res)
  }

  header <- NULL
  body <- list()
  for (r in rows) {
    is_head <- identical(xml2::xml_name(r), "table_header")
    cs <- cells_of(r, if (is_head) r_th else r_td)
    if (is_head) header <- cs else body[[length(body) + 1L]] <- cs
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
    k <- seq_len(min(ncol, length(a)))
    if (length(k)) spec[k] <- a[k]
  }

  # table-layout: fixed divides the measure between the columns and
  # renders each as a p{} column, so a wide table wraps instead of running
  # off the edge. Content-sized (l/c/r) stays the default.
  fixed <- identical(tolower(as.character(r_table[["table-layout"]] %||% "")),
                     "fixed")
  if (fixed && avail > 0 && ncol > 0) {
    # Each column costs its width plus the inter-column separation, which
    # defaults to 1em. Subtract that share or the table overshoots the
    # measure it was asked to fit into.
    gap_est <- .md_css_length(r_td[["padding-left"]], base, base,
                              horizontal = TRUE) %||% base
    per <- max(avail / ncol - gap_est, base)
    spec <- rep(sprintf("p{%.2fpt}", per), ncol)
  }

  # Vertical rules from a border on the cells, and the gap between
  # columns from their horizontal padding. `@{...}` replaces the default
  # inter-column space entirely, so it is only emitted when asked for.
  vrule <- if (!is.null(.md_css_border(r_td[["border-left"]], base))) "|" else ""
  gap <- .md_css_length(r_td[["padding-left"]], base, base, horizontal = TRUE)
  sep <- if (!is.null(gap)) sprintf("@{\\hspace{%.2fpt}}", gap) else ""
  spec_str <- if (nzchar(vrule)) {
    paste0(vrule, paste(spec, collapse = paste0(vrule, sep)), vrule)
  } else if (nzchar(sep)) {
    paste(spec, collapse = sep)
  } else {
    paste(spec, collapse = "")
  }

  # Row rules: `tr { border-bottom }` picks the weight. MicroTeX has two,
  # so anything above a hairline becomes the thick one. The outer rules
  # keep the booktabs look the default has always had.
  row_rule <- local({
    bd <- .md_css_border(r_tr[["border-bottom"]], base)
    if (is.null(bd) || (bd$width %||% 0) <= 0) "" else
      if ((bd$width %||% 0) > 1.5) "\\thickhline " else "\\hline "
  })

  arc <- .md_resolve_color(r_table[["border-color"]])
  parts <- c(if (!is.null(arc)) paste0("\\arrayrulecolor{", arc, "}"),
             "\\begin{tabular}{", spec_str, "}",
             "\\thickhline ")
  # `tr` is the row and `th`/`td` are the cells, as in HTML: a row fill is
  # a row-leading \rowcolor and applies to the header too, while a cell
  # fill is the \cellcolor already emitted by cell_tex().
  row_fill <- .md_resolve_color(r_tr$background)
  row_lead <- if (!is.null(row_fill)) paste0("\\rowcolor{", row_fill, "}")
  if (!is.null(header)) {
    parts <- c(parts, row_lead, line(header), "\\\\", "\\hline ")
  }
  for (i in seq_along(body)) {
    # The closing \thickhline already rules off the last row; adding the
    # per-row rule as well would draw two lines on top of each other.
    parts <- c(parts, row_lead, line(body[[i]]), "\\\\",
               if (i < length(body)) row_rule)
  }
  parts <- c(parts, "\\thickhline", "\\end{tabular}")
  paste(parts, collapse = "")
}

# Parse a markdown string into block descriptors.
.md_parse_blocks <- function(md, base = 20, style = NULL, ctx = list()) {
  parsed <- .md_parse_doc(md)
  .md_blocks(xml2::xml_children(parsed$doc), parsed$spans, base, style, ctx)
}

# Does this markdown need block layout?
#
# Anything that is not a single plain paragraph does: a heading, a list, a
# quote, a table, a rule, a code block, a footnote, two paragraphs -- or a
# paragraph that is nothing but display math, which .md_blocks() turns
# into a centred `math` block of its own.
#
# Reads the parsed document rather than the built blocks on purpose: only
# the shape matters, and building every block's LaTeX just to count them
# would double the work for the one-paragraph label that is the norm.
.md_needs_blocks <- function(md) {
  parsed <- .md_parse_doc(md)
  nodes <- xml2::xml_children(parsed$doc)
  if (length(nodes) == 0L) return(FALSE)
  if (length(nodes) != 1L) return(TRUE)
  if (!identical(xml2::xml_name(nodes[[1]]), "paragraph")) return(TRUE)
  !is.na(.md_lone_math_span(nodes[[1]], parsed$spans))
}

# Marker text for list item `n`.
#
# `checked` is NA for an ordinary item, TRUE/FALSE for a GFM task list
# item. The two checkbox states must be the SAME glyph size or the list
# looks ragged: \square (empty) and \blacksquare (filled) are both 14x13
# in the math font, whereas the more literal \boxtimes is 18x16 and left
# every checked row visibly larger. (\Box is not a glyph at all -- it
# typesets the three letters "Box".)
.md_list_marker <- function(ordered, start, n, paren = FALSE, checked = NA,
                            bullet = NULL) {
  if (!is.na(checked)) {
    return(if (isTRUE(checked)) "\\blacksquare" else "\\square")
  }
  if (ordered) {
    paste0("\\text{", start + n - 1L, if (paren) ")" else ".", "}")
  } else {
    # `bullet` is raw LaTeX from the style, the one escape hatch for a
    # marker glyph -- there is no CSS spelling for "\\triangleright".
    bullet %||% "\\bullet"
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

# One entry in the laid-out stack. `align` is the block's own text-align
# as a fraction, or NULL to take the box-wide halign.
.md_item <- function(grob, x, y, w, h, align = NULL) {
  list(grob = grob, x = x, y = y, w = w, h = h, align = align)
}

# Move a laid-out grob by (dx, dy) big points, dy measured downward to
# match the stack's own top-down offsets.
#
# Two kinds of grob end up in the stack and they are positioned
# differently. A text block is a latex_grob, which carries its position
# in a viewport; a rule, an image and a block quote's bar are plain
# rect/raster grobs positioned by their own x/y. Assuming the viewport is
# what made a horizontal rule inside a block quote an error rather than a
# rule, and what left images pinned to the left edge under `halign`.
.md_shift_grob <- function(g, dx = 0, dy = 0) {
  if (!is.null(g$vp)) {
    if (dx != 0) g$vp$x <- g$vp$x + grid::unit(dx, "bigpts")
    if (dy != 0) g$vp$y <- g$vp$y - grid::unit(dy, "bigpts")
    return(g)
  }
  if (dx != 0 && !is.null(g$x)) g$x <- g$x + grid::unit(dx, "bigpts")
  if (dy != 0 && !is.null(g$y)) g$y <- g$y - grid::unit(dy, "bigpts")
  g
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

# The style tag a block is matched by. HTML's names, not our internal
# ones, so that `blockquote { }` in a stylesheet reads the way a CSS
# author expects it to.
.md_block_tag <- function(blk) {
  switch(blk$type,
    paragraph      = "p",
    mathblock      = "math",
    footnote       = "footnote",
    heading        = paste0("h", blk$level),
    code_block     = "pre",
    block_quote    = "blockquote",
    thematic_break = "hr",
    image          = "img",
    list           = if (isTRUE(blk$ordered)) "ol" else "ul",
    div            = "div",
    blk$type)
}

# Turn resolved properties into the grid gp a block draws with, plus the
# LaTeX commands that wrap its content.
#
# Colour, family, size and line height ride on gp, which latex_grob()
# bakes into the layout. Weight, slant and decoration have no gp
# equivalent -- gp$fontface is deliberately not honoured, since LaTeX
# controls the face -- so they wrap the content instead, reusing the
# same commands the inline walker emits.
.md_block_style <- function(gp, res, body, inherited_size) {
  size <- .md_css_length(res[["font-size"]], body, inherited_size) %||%
    inherited_size
  gp$fontsize <- size
  # Already folded into `body` by markdown_box_grob(); leaving it would
  # scale the block a second time.
  gp$cex <- NULL

  if (!is.null(res$color)) {
    hex <- .md_resolve_color(res$color)
    if (!is.null(hex)) gp$col <- hex
  }
  if (!is.null(res[["font-family"]])) {
    fam <- .md_css_family(res[["font-family"]])
    if (nzchar(fam)) gp$fontfamily <- fam
  }
  if (!is.null(res[["line-height"]])) {
    # Unitless, as line-height almost always is, and as gpar() wants it.
    lh <- suppressWarnings(as.numeric(res[["line-height"]]))
    if (is.finite(lh)) gp$lineheight <- lh
  }

  # Decorations wrap the emphasis, not the other way round. \underline
  # and \sout typeset their argument as a fresh sub-formula and drop the
  # font style inside it, so \textbf{\underline{x}} loses the bold while
  # \underline{\textbf{x}} keeps it.
  dec <- character(0)
  td <- tolower(as.character(res[["text-decoration"]] %||% ""))
  if (grepl("underline", td, fixed = TRUE)) dec <- c(dec, "\\underline{")
  if (grepl("overline", td, fixed = TRUE)) dec <- c(dec, "\\overline{")
  if (grepl("line-through", td, fixed = TRUE)) dec <- c(dec, "\\sout{")
  emph <- .md_emphasis_cmds(res)
  open <- c(dec, emph)

  list(gp = gp, size = size, open = paste(open, collapse = ""),
       close = strrep("}", length(open)), emph = emph)
}

# Lay out a list of blocks into positioned items.
#
# Returns list(items = <list of .md_item>, height = <total bigpts>).
# `indent` shifts everything right, which is how nested lists and block
# quotes are built -- each recursion re-measures its content at the
# reduced width so text still wraps inside the indent.
#
# `style` is the resolved cascade; `inherited` the properties of the
# enclosing container, so colour and font settings fall through into a
# block quote's contents while its margins and border do not.
.md_layout <- function(blocks, width, gp, fontsize, indent = 0, first = TRUE,
                       style = NULL, inherited = list()) {
  items <- list()
  y <- 0
  style <- style %||% markdown_style()
  # The container's font size, which `em` and `%` resolve against.
  base_size <- .md_css_length(inherited[["font-size"]], fontsize, fontsize) %||%
    fontsize

  add <- function(it) items[[length(items) + 1L]] <<- it
  # Baseline of the first line laid out here, in bigpts down from the top.
  # A list marker needs it to sit on the baseline of its item's text; it
  # cannot be assumed, because a tall first line (a superscript, say)
  # pushes that baseline further down. NA if nothing text-like came first.
  first_bl <- NA_real_
  note_bl <- function(v) if (is.na(first_bl)) first_bl <<- v

  for (i in seq_along(blocks)) {
    blk <- blocks[[i]]
    # A class matches the element carrying it and nothing else, as in
    # CSS: what reaches the blocks inside a styled <div> reaches them by
    # inheritance, not by matching the div's selector again.
    res <- .md_cascade(style, .md_block_tag(blk),
                       classes = blk$class %||% character(0),
                       inline = blk$style, inherited = inherited)
    sty <- .md_block_style(gp, res, fontsize, base_size)
    gp_blk <- sty$gp
    # Wrap once, so every branch below draws what the cascade asked for.
    wrap <- function(tex) paste0(sty$open, tex, sty$close)
    align <- .md_css_align(res[["text-align"]])

    # The first block in a container opens flush; a margin only ever
    # separates one block from another.
    if (!(first && i == 1L)) {
      y <- y + (.md_css_length(res[["margin-top"]], fontsize, sty$size) %||% 0)
    }
    len <- function(p, horizontal = FALSE) {
      .md_css_length(res[[p]], fontsize, sty$size, horizontal = horizontal) %||% 0
    }
    pad_l <- len("padding-left", TRUE)
    pad_r <- len("padding-right", TRUE)
    pad_t <- len("padding-top")
    pad_b <- len("padding-bottom")
    indent_blk <- indent + len("margin-left", TRUE) + pad_l
    avail <- width - indent_blk - pad_r - len("margin-right", TRUE)

    # A block background is the one box property no MicroTeX command can
    # give: every one of them hugs its content, and a code block's fill
    # has to span the column. The rect is emitted after the block's own
    # grob is measured, and inserted before it so it draws behind.
    bg <- .md_resolve_color(res$background)
    y_box <- y
    y <- y + pad_t
    n_before <- length(items)

    if (identical(blk$type, "paragraph") || identical(blk$type, "mathblock")) {
      if (!nzchar(blk$tex)) next
      tex <- wrap(blk$tex)
      m <- .md_measure(tex, avail, gp_blk)
      # Draw at m$mw, not avail: .md_measure() may have had to narrow the
      # request to get a fitting set of line breaks, and drawing at a
      # different width would re-break the text and undo the fit.
      add(.md_item(.md_run_grob(tex, indent_blk, y, m$mw, gp_blk),
                   indent_blk, y, m$w, m$h, align))
      note_bl(y + m$h - m$bl)
      y <- y + m$h

    } else if (identical(blk$type, "heading")) {
      # The size comes from gp, not a \Huge..\normalsize macro: the macro
      # ladder is six fixed steps, and font-size has to be continuous.
      tex <- wrap(blk$tex)
      m <- .md_measure(tex, avail, gp_blk)
      add(.md_item(.md_run_grob(tex, indent_blk, y, m$mw, gp_blk),
                   indent_blk, y, m$w, m$h, align))
      note_bl(y + m$h - m$bl)
      y <- y + m$h

    } else if (identical(blk$type, "code_block")) {
      # Code must render in a monospace font. \texttt switches MicroTeX's
      # internal style, but the \text{} content is drawn with the resolved
      # system font, which is the sans body font unless told otherwise --
      # and in a proportional font "<-" comes out misshapen, the "<" set
      # as a raised math relation while the "-" is a low text hyphen.
      # The default style says so; a stylesheet may say otherwise.
      #
      # Lines advance by a fixed leading rather than by their measured
      # height: a line with no descender measures shorter, and stacking
      # on that would let the next line's ascenders collide with it.
      code_lh <- (gp_blk$lineheight %||% 1.15) * sty$size
      # NULL when the fence names no language, or one we have no grammar
      # for; every line then renders plain, exactly as it used to.
      cls <- .hl_classes(blk$lines, blk$lang)
      if (is.null(cls)) cls <- vector("list", length(blk$lines))
      # A token is wrapped in \textcolor only when its class resolves to
      # a colour *different* from the block's own. An unstyled class
      # therefore emits no markup at all, which is what keeps an
      # unhighlighted block byte-identical to the old output.
      base_col <- .md_resolve_color(res$color)
      seen <- list()
      colour <- function(k) {
        if (!is.null(seen[[k]])) return(seen[[k]]$v)
        r <- .md_cascade(style, "pre",
                         classes = c(blk$class %||% character(0), k),
                         inline = blk$style, inherited = inherited)
        v <- .md_resolve_color(r$color)
        if (!is.null(v) && identical(v, base_col)) v <- NULL
        seen[[k]] <<- list(v = v)
        v
      }
      for (li in seq_along(blk$lines)) {
        tex <- wrap(.hl_code_tex(blk$lines[[li]], cls[[li]], colour))
        m <- .md_measure(tex, 0, gp_blk)
        add(.md_item(.md_run_grob(tex, indent_blk, y, 0, gp_blk),
                     indent_blk, y, m$w, code_lh, align))
        y <- y + code_lh
      }

    } else if (identical(blk$type, "table")) {
      # Content-sized already, from .md_blocks(). Only a fixed layout
      # needs rebuilding, because only it depends on `avail`.
      blk_tex <- blk$tex
      if (identical(tolower(as.character(res[["table-layout"]] %||% "")),
                    "fixed") && !is.null(blk$nd)) {
        blk_tex <- .md_table_tex(blk$nd, blk$spans, blk$base, style,
                                 avail = avail,
                                 inherited = blk$ctx %||% list())
      }
      if (!nzchar(blk_tex)) next
      tex <- wrap(blk_tex)
      m <- .md_measure(tex, 0, gp_blk)
      add(.md_item(.md_run_grob(tex, indent_blk, y, 0, gp_blk),
                   indent_blk, y, m$w, m$h, align))
      y <- y + m$h

    } else if (identical(blk$type, "image")) {
      # Shares the inline path's loader and grob builder, so a block image
      # gains SVG-as-vector for free. The sizing arithmetic below is
      # deliberately unchanged: .image_dims() reports the same
      # `w_px * 72/96` this used to compute inline, so a bitmap lays out
      # exactly as before.
      dims <- .image_dims(blk$path)
      g <- if (is.null(dims)) NULL else {
        # Scaled down to fit the column, never blown up past natural size.
        iw <- min(dims$w, avail)
        ih <- iw * dims$h / dims$w
        gg <- .image_grob(blk$path, iw, ih,
                          x = grid::unit(indent_blk, "bigpts"),
                          y = grid::unit(1, "npc") - grid::unit(y, "bigpts"))
        if (is.null(gg)) NULL else list(g = gg, w = iw, h = ih)
      }
      if (is.null(g)) {
        # Missing file, unsupported format, or no reader installed: show
        # the alt text so the reader still learns what belongs here.
        if (nzchar(blk$alt)) {
          tex <- wrap(blk$alt)
          m <- .md_measure(tex, avail, gp_blk)
          add(.md_item(.md_run_grob(tex, indent_blk, y, m$mw, gp_blk),
                       indent_blk, y, m$w, m$h, align))
          y <- y + m$h
        }
      } else {
        add(.md_item(g$g, indent_blk, y, g$w, g$h, align))
        y <- y + g$h
      }

    } else if (identical(blk$type, "thematic_break")) {
      # The rule sits centred in a band of its own, so `height` is the
      # band and the border is the ink drawn across the middle of it.
      h <- .md_css_length(res$height, fontsize, sty$size) %||% (0.5 * fontsize)
      bd <- .md_css_border(res[["border-top"]], fontsize)
      add(.md_item(
        grid::rectGrob(
          x = grid::unit(indent_blk, "bigpts"),
          y = grid::unit(1, "npc") - grid::unit(y + h / 2, "bigpts"),
          width = grid::unit(avail, "bigpts"),
          # Unstated thickness and colour resolve as they did before the
          # cascade existed: a hairline that grows with the text, inked
          # in the text colour.
          height = grid::unit(bd$width %||% max(1, sty$size / 20), "bigpts"),
          hjust = 0, vjust = 0.5,
          gp = grid::gpar(fill = bd$color %||% gp_blk$col %||% "black", col = NA)
        ),
        indent_blk, y, avail, h, align))
      y <- y + h

    } else if (identical(blk$type, "div")) {
      # A styled chunk. It draws no ink of its own: padding moves its
      # contents right, and its inheritable properties fall through.
      #
      # Transparent to margins too -- unlike a quote or a list item, a
      # div supplies no offset of its own, so its first child keeps its
      # top margin unless the div itself opens the container.
      inner <- .md_layout(blk$blocks, width, gp, fontsize,
                          indent = indent_blk, first = first && i == 1L,
                          style = style, inherited = res)
      for (it in inner$items) {
        add(.md_item(.md_shift_grob(it$grob, dy = y),
                     it$x, y + it$y, it$w, it$h, it$align))
      }
      y <- y + inner$height

    } else if (identical(blk$type, "footnote")) {
      # Laid out like a quote without the bar: the note's blocks, indented
      # by whatever padding the `footnote` rule asks for and inheriting its
      # reduced size.
      inner <- .md_layout(blk$blocks, width, gp, fontsize,
                          indent = indent_blk, first = TRUE,
                          style = style, inherited = res)
      for (it in inner$items) {
        add(.md_item(.md_shift_grob(it$grob, dy = y),
                     it$x, y + it$y, it$w, it$h, it$align))
      }
      y <- y + inner$height

    } else if (identical(blk$type, "block_quote")) {
      bd <- .md_css_border(res[["border-left"]], fontsize)
      bar <- bd$width %||% (0.125 * fontsize)
      # padding-left has already moved indent_blk; the bar is drawn at
      # the quote's own left edge, in the space that padding opened.
      inner <- .md_layout(blk$blocks, width, gp, fontsize,
                          indent = indent_blk, first = TRUE,
                          style = style, inherited = res)
      for (it in inner$items) {
        add(.md_item(.md_shift_grob(it$grob, dy = y),
                     it$x, y + it$y, it$w, it$h, it$align))
      }
      # The bar spans the quoted content, added after so it is measured
      # against the final height.
      add(.md_item(
        grid::rectGrob(
          x = grid::unit(indent, "bigpts"),
          y = grid::unit(1, "npc") - grid::unit(y, "bigpts"),
          width = grid::unit(max(1, bar), "bigpts"),
          height = grid::unit(inner$height, "bigpts"),
          hjust = 0, vjust = 1,
          gp = grid::gpar(fill = bd$color %||% gp_blk$col %||% "grey60",
                          col = NA)
        ),
        indent, y, bar, inner$height
      ))
      y <- y + inner$height

    } else if (identical(blk$type, "list")) {
      # A tight list closes its items up; CommonMark decides which it is.
      res_li <- .md_cascade(style, "li", inherited = res)
      sty_li <- .md_block_style(gp, res_li, fontsize, sty$size)
      gap_item <- if (isTRUE(blk$tight)) 0.25 * fontsize else
        (.md_css_length(res_li[["margin-top"]], fontsize, sty_li$size) %||%
           (0.55 * fontsize))
      # Markers baseline-align to the first line of item text. Without
      # this the marker is top-aligned, so a bullet floats near the top
      # of the line instead of sitting on the text baseline. An ordinary
      # ascender/descender line stands in for a body with no baseline of
      # its own to align to.
      refd <- latex_dims("\\text{Ag}", input_mode = "math", gp = gp_blk)
      ref_bl_top <- as.numeric(refd$height) - as.numeric(refd$baseline)
      marker_gap <- .md_css_length(res[["marker-gap"]], fontsize, sty$size,
                                   horizontal = TRUE) %||% (0.4 * fontsize)
      for (j in seq_along(blk$items)) {
        if (j > 1L) y <- y + gap_item
        marker <- .md_list_marker(blk$ordered, blk$start, j,
                                  paren = isTRUE(blk$paren),
                                  checked = if (is.null(blk$checked)) NA
                                            else blk$checked[j],
                                  bullet = res$bullet)
        mm <- .md_measure(marker, 0, gp_blk)
        # Hanging indent: the marker sits at the current indent and the
        # item body is measured at the reduced width, so continuation
        # lines line up under the text rather than under the marker.
        # This is the reason lists are stacked here instead of being
        # handed to MicroTeX's itemize -- max_width does not reach into
        # its cells.
        body_indent <- indent_blk + mm$w + marker_gap
        inner <- .md_layout(blk$items[[j]], width, gp, fontsize,
                            indent = body_indent, first = TRUE,
                            style = style, inherited = res_li)
        # Shift the marker down so its own baseline lands on the text
        # baseline of the first line -- the item's own baseline, not the
        # reference one, since a tall line ($x^2$, say) sits lower in its
        # box and a marker placed at the generic offset floats above it.
        # The reference is only a fallback for a body that opens with
        # something untextual, like a code block.
        bl_top <- if (is.na(inner$first_bl)) ref_bl_top else inner$first_bl
        y_marker <- y + bl_top - (mm$h - mm$bl)
        note_bl(y_marker + mm$h - mm$bl)
        add(.md_item(.md_run_grob(marker, indent_blk, y_marker, 0, gp_blk),
                     indent_blk, y_marker, mm$w, mm$h))
        for (it in inner$items) {
          add(.md_item(.md_shift_grob(it$grob, dy = y),
                       it$x, y + it$y, it$w, it$h, it$align))
        }
        # Advance past whichever reaches lower: the body, or a marker that
        # was pushed below it.
        y <- y + max(inner$height, (y_marker - y) + mm$h)
      }
    }

    y <- y + pad_b

    # The background spans the block plus its padding. Inserted *before*
    # the grobs it sits behind, matching how the box-level background is
    # emitted first in makeContent.markdownbox().
    if (!is.null(bg) && length(items) > n_before) {
      rect <- .md_item(
        grid::rectGrob(
          x = grid::unit(indent + len("margin-left", TRUE), "bigpts"),
          y = grid::unit(1, "npc") - grid::unit(y_box, "bigpts"),
          width = grid::unit(max(avail + pad_l + pad_r, 1), "bigpts"),
          height = grid::unit(max(y - y_box, 1), "bigpts"),
          hjust = 0, vjust = 1,
          gp = grid::gpar(fill = bg, col = NA)
        ),
        indent, y_box, avail + pad_l + pad_r, y - y_box
      )
      items <- append(items, list(rect), after = n_before)
    }

    # Margins do not collapse here, unlike CSS: the space below a block
    # simply adds to the space above the next. Every built-in default
    # leaves margin-bottom unset, so this changes nothing until a
    # stylesheet asks for it.
    y <- y + (.md_css_length(res[["margin-bottom"]], fontsize, sty$size) %||% 0)
  }

  list(items = items, height = y, first_bl = first_bl)
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

# The measure used to find a box's natural width: wide enough that no
# realistic line wraps, small enough that a `table-layout: fixed` table
# does not ask MicroTeX for absurd column widths.
.MD_PROBE_W <- 1e4

# The box's own chrome, as declared by `body {}` in the stylesheet.
#
# These are box properties, so .md_cascade() deliberately strips them out
# of the seed it hands the blocks -- a background on `body` must not
# become a background on every paragraph. That leaves the box itself as
# the only thing that can consume them, which is what this does. Each
# component is NULL when `body` says nothing, so the caller can tell
# "unset" from "set to zero" and let an explicit argument win.
.md_body_chrome <- function(body, fontsize) {
  side <- function(p, horizontal) {
    .md_css_length(body[[p]], fontsize, fontsize, horizontal = horizontal) %||% 0
  }
  trbl <- function(prefix) {
    nm <- paste0(prefix, c("-top", "-right", "-bottom", "-left"))
    if (!any(nm %in% names(body))) return(NULL)
    c(side(nm[1], FALSE), side(nm[2], TRUE), side(nm[3], FALSE), side(nm[4], TRUE))
  }

  bd <- .md_css_border(body[["border"]], fontsize)
  fill <- .md_resolve_color(body[["background"]])
  col <- bd$color %||% .md_resolve_color(body[["border-color"]])
  box_gp <- if (!is.null(fill) || !is.null(col)) {
    # A border width is a length in big points; gpar()'s lwd is 1/96in.
    lwd <- if (is.null(bd$width)) 1 else bd$width * 96 / 72
    grid::gpar(fill = fill %||% NA, col = col %||% NA, lwd = lwd)
  }
  radius <- .md_css_length(body[["border-radius"]], fontsize, fontsize,
                           horizontal = TRUE)

  list(margin = trbl("margin"), padding = trbl("padding"),
       box_gp = box_gp,
       r = if (!is.null(radius)) grid::unit(radius, "bigpts"))
}

# Do the geometry once: resolve the box, lay the blocks out, and report
# every dimension makeContent() and the *Details() methods need. Must run
# at draw time, because the width may be relative and because measuring
# text needs an open device.
.md_box_layout <- function(x) {
  gp <- x$gp %||% grid::gpar()
  # markdown_box_grob() has already folded cex in, so this is the drawn
  # size; .md_base_size() is the same rule latex_grob() applies.
  fontsize <- .md_base_size(gp)

  style <- x$style %||% markdown_style()
  # `body` is the document root. It seeds the inheritance every block
  # starts from, and -- since .md_cascade() filters that seed down to the
  # inheritable properties -- it is also the only place a box property can
  # be declared without leaking into every block inside the box.
  body <- .md_cascade(style, "body")
  chrome <- .md_body_chrome(body, fontsize)

  # An argument is the inline style: it wins over the stylesheet.
  zero <- c(0, 0, 0, 0)
  margin <- if (is.null(x$margin)) chrome$margin %||% zero else
    .md_trbl(x$margin, "margin")
  padding <- if (is.null(x$padding)) chrome$padding %||% zero else
    .md_trbl(x$padding, "padding")

  if (is.null(x$width)) {
    # Natural width. Lay out once at a width nothing realistic wraps at,
    # then take the widest item -- skipping the ones that are full-width
    # by construction (a rule, and a block background) since those just
    # hand the probe width back.
    probe <- .md_layout(x$blocks, .MD_PROBE_W, gp, fontsize, style = style,
                        inherited = body)
    ink <- vapply(probe$items, function(it) {
      if (it$w < .MD_PROBE_W * 0.99) it$x + it$w else NA_real_
    }, numeric(1))
    content_w <- if (all(is.na(ink))) fontsize * 20 else max(ink, na.rm = TRUE)
    # A point of slack, because latex_dims() reports whole big points:
    # handing a rounded-down width back as the measure is enough to make
    # MicroTeX break the very line it was measured from.
    content_w <- max(content_w, 1) + 1
    box_w <- content_w + padding[2] + padding[4]
    total_w <- box_w + margin[2] + margin[4]
  } else {
    total_w <- grid::convertWidth(x$width, "bigpts", valueOnly = TRUE)
    box_w <- total_w - margin[2] - margin[4]
    content_w <- max(box_w - padding[2] - padding[4], 1)
  }

  laid <- .md_layout(x$blocks, content_w, gp, fontsize, style = style,
                     inherited = body)

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
       box_gp = x$box_gp %||% chrome$box_gp,
       r = x$r %||% chrome$r,
       items = laid$items)
}

#' @export
makeContent.markdownbox <- function(x) {
  lay <- .md_box_layout(x)
  kids <- list()

  # Background / border, inset by the margin.
  if (!is.null(lay$box_gp)) {
    rr <- lay$r %||% grid::unit(0, "pt")
    common <- list(
      x = grid::unit(lay$margin[4], "bigpts"),
      y = grid::unit(1, "npc") - grid::unit(lay$margin[1], "bigpts"),
      width = grid::unit(lay$box_w, "bigpts"),
      height = grid::unit(lay$box_h, "bigpts"),
      # `just`, not hjust/vjust: roundrectGrob() has no hjust/vjust
      # arguments, and rectGrob() honours either.
      just = c(0, 1), gp = lay$box_gp
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
    # A block's own text-align wins; the box-wide halign is the default
    # for blocks that state none.
    a <- it$align %||% halign
    if (a == 0) return(it$grob)
    # Slack to the right of this item, so alignment cooperates with the
    # per-block indent instead of fighting it.
    .md_shift_grob(it$grob, dx = (lay$content_w - it$x - it$w) * a)
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
  kids <- do.call(grid::gList, kids)

  # A natural-width box has no viewport of its own: its size is not known
  # until the blocks have been measured, which is here. Everything above
  # is positioned in big points from the box's top-left, so one viewport
  # of the measured size, placed at the requested just, anchors the lot.
  if (is.null(x$width)) {
    kids <- grid::gList(grid::gTree(
      children = kids,
      vp = grid::viewport(
        x = x$box_x, y = x$box_y,
        width = grid::unit(lay$total_w, "bigpts"),
        height = grid::unit(lay$total_h, "bigpts"),
        just = c(x$box_hjust %||% 0.5, x$box_vjust %||% 0.5)
      )
    ))
  }

  grid::setChildren(x, kids)
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
#' Lays markdown out as a block document --- headings, paragraphs, lists
#' (including GFM task lists), block quotes, code blocks, tables,
#' horizontal rules and images --- inside an optional padded, filled and
#' bordered box. Prose wraps to the requested width, and \code{$...$}
#' math is typeset by MicroTeX as usual. All the inline formatting
#' \code{\link{markdown_grob}} understands, including the inline HTML
#' subset, works inside every block.
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
#' An image on a line of its own is drawn as a raster, scaled to fit the
#' column but never enlarged past its natural size (pixels are read at
#' 96 dpi). PNG needs the \pkg{png} package and JPEG needs \pkg{jpeg},
#' both \emph{Suggests}: when the reader is not installed, the file is
#' missing, or the format is anything else, the image degrades to its alt
#' text. An image \emph{within} a sentence stays inline, where only its
#' alt text survives.
#'
#' The layout is computed at draw time, so an open device is required ---
#' which is what lets a relative \code{width} and the measured height of
#' the text resolve against the viewport the box is actually drawn in.
#'
#' @section Styling:
#' Appearance comes from a small CSS cascade. \code{style} sets the house
#' style for the whole document, by tag:
#'
#' \preformatted{markdown_box_grob(md, style = markdown_style(
#'   h1         = md_style(color = "steelblue", font_size = 2),
#'   blockquote = md_style(border_left = "3px solid grey60")
#' ))}
#'
#' The same thing written as CSS, which \code{style} also takes directly,
#' as text or as the path to a \code{.css} file:
#'
#' \preformatted{markdown_box_grob(md, style = "
#'   h1 \{ color: steelblue; font-size: 2rem \}
#'   blockquote \{ border-left: 3px solid grey60 \}
#' ")}
#'
#' To style one chunk rather than every block of a kind, wrap it in a
#' \code{<div>} carrying a \code{class} or a \code{style}. \strong{Leave
#' blank lines around the tags} --- that is what makes CommonMark parse
#' the markdown between them instead of treating the whole thing as raw
#' HTML:
#'
#' \preformatted{<div class="note">
#'
#' ## This heading only
#'
#' </div>}
#'
#' Inline runs take \code{class} as well as \code{style} on a
#' \code{<span>}. See \code{\link{markdown_style}} for the tag names, the
#' supported properties and how the cascade resolves.
#'
#' @param md Character string of markdown.
#' @param x,y Position of the box in the parent viewport.
#' @param width Width of the box, including \code{margin}. \code{NULL}
#'   sizes the box to its content, so nothing wraps --- useful where the
#'   available width is not known, as in a ggplot2 theme element.
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
#'   margin outside it. \code{NULL} (default) takes them from the
#'   stylesheet's \code{body} rule, and is zero if that says nothing.
#' @param box_gp Graphical parameters for the box itself, e.g.
#'   \code{gpar(fill = "grey95", col = "black")}. \code{NULL} (default)
#'   takes the fill from \code{body \{ background \}} and the border from
#'   \code{body \{ border \}}, and draws no box if neither is set.
#' @param r Corner radius; a non-zero value draws a rounded box.
#'   \code{NULL} (default) takes it from \code{body \{ border-radius \}}.
#' @param style Appearance of the blocks: a \code{\link{markdown_style}}
#'   object, CSS text, or a path to a \code{.css} file. \code{NULL}
#'   (default) uses \code{latex_options("markdown_style")} if set, and
#'   the built-in defaults otherwise.
#' @param name Optional grob name.
#' @param gp Graphical parameters for the text. \code{fontsize} also sets
#'   the scale for block spacing and list indentation, and \code{cex}
#'   multiplies it as elsewhere in \pkg{grid}.
#' @param vp Optional viewport. Supplying one replaces the viewport built
#'   from \code{x}, \code{y}, \code{width}, \code{height}, \code{hjust}
#'   and \code{vjust}, so those are then ignored.
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
                              padding = NULL,
                              margin = NULL,
                              box_gp = NULL,
                              r = NULL,
                              style = NULL,
                              name = NULL,
                              gp = grid::gpar(),
                              vp = NULL) {
  .md_check_string(md)
  .md_check_trbl(padding, "padding")
  .md_check_trbl(margin, "margin")
  gp <- .md_fold_cex(gp)
  style <- .md_as_style(style)
  # Parse once at construction; the layout is redone at draw time, when
  # relative widths resolve and a device is available for measuring.
  # The style has to be in hand here as well as at draw time, because a
  # <span class> compiles to LaTeX commands during the parse.
  blocks <- .md_parse_blocks(md, .md_base_size(gp), style,
                             .md_cascade(style, "body"))

  # A relative width can be resolved by a viewport built here; a natural
  # width cannot, because it is not known until the blocks are measured at
  # draw time. In that case the grob carries no viewport and
  # makeContent() builds one of the measured size instead.
  if (is.null(vp) && !is.null(width)) {
    vp <- grid::viewport(
      x = x, y = y,
      width = width,
      height = height %||% grid::unit(1, "npc"),
      just = c(hjust, vjust)
    )
  }

  grid::gTree(
    md = md, blocks = blocks, style = style,
    width = width, height = height,
    halign = halign, valign = valign,
    padding = padding, margin = margin,
    box_gp = box_gp, r = r,
    box_x = x, box_y = y, box_hjust = hjust, box_vjust = vjust,
    gp = gp, name = name, cl = "markdownbox",
    vp = vp
  )
}
