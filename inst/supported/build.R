# The list of what gridmicrotex draws: every example of examples.tsv set in a
# table, in the order KaTeX's "Supported Functions" page uses, and rendered by
# the package itself to a PDF -- one page at a time, since a grob is one piece
# and has no pages. The page numbers are the footer drawn here.
#
#   source(system.file("supported/build.R", package = "gridmicrotex"))
#   build_supported(tempdir())     # writes supported-functions.tex and .pdf
#
# The .tex it writes is an ordinary LaTeX file (it needs amsmath, amssymb,
# amscd and tikz-cd) that real LaTeX sets as well.

build_supported <- function(dir = ".", pdf = TRUE, name = "supported-functions",
                            examples = system.file("supported/examples.tsv",
                                                   package = "gridmicrotex")) {
  stopifnot(nzchar(examples), file.exists(examples))
  ex <- utils::read.delim(examples, quote = "", comment.char = "", stringsAsFactors = FALSE,
                          encoding = "UTF-8")
  size <- 9                       # points
  page <- c(w = 8.5, h = 11)      # inches
  margin <- c(side = 0.7, top = 0.65, bottom = 0.85)
  budget <- (page[["h"]] - margin[["top"]] - margin[["bottom"]]) * 72
  width <- (page[["w"]] - 2 * margin[["side"]]) * 72

  verb <- function(s) {
    for (d in c("|", "!", "+", "=", "/", "@", "~", ";")) {
      if (!grepl(d, s, fixed = TRUE)) return(paste0("\\verb", d, s, d))
    }
    paste0("\\texttt{", gsub("([#$%&_{}])", "\\\\\\1", s), "}")
  }
  # `s` in pieces of up to `width` characters, cut at spaces.
  wrap_source <- function(s, width) {
    words <- strsplit(s, " ", fixed = TRUE)[[1]]
    lines <- character()
    cur <- ""
    for (w in words) {
      if (nzchar(cur) && nchar(cur) + 1L + nchar(w) > width) {
        lines <- c(lines, cur)
        cur <- w
      } else {
        cur <- if (nzchar(cur)) paste(cur, w) else w
      }
    }
    c(lines, cur)
  }
  math <- function(s) paste0("{\\large $", s, "\n$}")
  small <- function(s) paste0("{\\small", s, "}")

  height <- function(s) {
    d <- tryCatch(suppressWarnings(
      gridmicrotex::latex_dims(s, input_mode = "math", gp = grid::gpar(fontsize = size))),
      error = function(e) NULL)
    if (is.null(d)) return(14)
    max(12, grid::convertHeight(d$height, "bigpts", valueOnly = TRUE)) + 5
  }

  pages <- list()
  cur <- character()
  used <- 0
  buf <- character()
  kind <- "pair2"
  spec <- c(pair2 = "@{}l@{\\qquad}l@{\\qquad\\qquad}l@{\\qquad}l@{}", one = "@{}l@{}",
            names = "@{}lll@{}")
  title <- ""

  flush_table <- function() {
    if (length(buf)) {
      cur <<- c(cur, paste0("\\begin{tabular}{", spec[[kind]], "}"), buf, "\\end{tabular}", "")
    }
    buf <<- character()
  }
  new_page <- function() {
    flush_table()
    pages[[length(pages) + 1L]] <<- cur
    cur <<- character()
    used <<- 0
    if (nzchar(title)) {
      cur <<- paste0("\\subsection*{", title, " (continued)}")
      used <<- 22
    }
  }
  heading <- function(level, text, h, keep = 90) {
    if (used + h + keep > budget) new_page()
    flush_table()
    cur <<- c(cur, paste0("\\", level, "*{", text, "}"))
    used <<- used + h
  }
  row <- function(tex, h, k) {
    if (k != kind || used + h > budget) {
      if (used + h > budget) {
        new_page()
      } else {
        flush_table()
      }
      kind <<- k
    }
    buf <<- c(buf, tex)
    used <<- used + h
  }

  intro <- c(
    "\\begin{center}\\Large\\textbf{What gridmicrotex draws}\\end{center}",
    paste0("Each row is a LaTeX input and what the package makes of it. The examples are those of ",
           "KaTeX's list of supported functions that the package draws, then what KaTeX does not ",
           "have; the last page lists what is not supported. A row here is a test: it must be drawn ",
           "with no warning."),
    "")
  cur <- intro
  used <- 70

  sections <- unique(ex$section)
  last_top <- ""
  for (sec in sections) {
    part <- strsplit(sec, " / ", fixed = TRUE)[[1]]
    e <- ex[ex$section == sec, ]
    top <- part[1]
    sub <- if (length(part) > 1) part[2] else NULL
    # A section heading when the top level changes.
    if (!identical(top, last_top)) {
      heading("section", top, 26, keep = 140)
      last_top <- top
      title <- top
    }
    if (!is.null(sub)) {
      heading("subsection", sub, 20, keep = 110)
      title <- paste(top, "/", sub)
    }
    if (all(e$kind == "no")) {
      items <- vapply(e$source, verb, "", USE.NAMES = FALSE)
      for (i in seq(1L, length(items), by = 3L)) {
        cells <- items[i:min(i + 2L, length(items))]
        row(paste0(paste(c(cells, rep("", 3L - length(cells))), collapse = " & "), " \\\\"), 14, "names")
      }
      next
    }
    e <- e[e$kind == "ok", ]
    long <- nchar(e$source) > 30
    short <- e[!long, ]
    longer <- e[long, ]
    if (nrow(short)) {
      cells <- vapply(short$source, function(s) paste(small(verb(s)), "&", math(s)), "", USE.NAMES = FALSE)
      hs <- vapply(short$source, height, 0, USE.NAMES = FALSE)
      for (i in seq(1L, length(cells), by = 2L)) {
        j <- min(i + 1L, length(cells))
        right <- if (j > i) cells[j] else "&"
        row(paste0(cells[i], " & ", right, " \\\\"), max(hs[i:j]), "pair2")
      }
    }
    # A long one: its source in lines of at most 64 characters, then what it makes.
    for (i in seq_len(nrow(longer))) {
      s <- longer$source[i]
      lines <- wrap_source(s, 64)
      cell <- paste0(paste(vapply(lines, function(l) small(verb(l)), ""), collapse = " \\\\ "),
                     " \\\\ ", math(s), " \\\\[4pt]")
      row(cell, height(s) + 12 * length(lines) + 6, "one")
    }
  }
  new_page()
  pages <- pages[vapply(pages, length, 0L) > 0L]

  tex <- c(
    "% Written by inst/supported/build.R from examples.tsv; set by gridmicrotex or by LaTeX.",
    "\\documentclass[9pt]{extarticle}",
    "\\usepackage[letterpaper,margin=0.7in]{geometry}",
    "\\usepackage{amsmath,amssymb,amscd,tikz-cd}",
    "\\begin{document}",
    unlist(lapply(seq_along(pages), function(k) c(if (k > 1L) "\\newpage", pages[[k]]))),
    "\\end{document}")
  tex_file <- file.path(dir, paste0(name, ".tex"))
  writeLines(tex, tex_file, useBytes = TRUE)

  if (pdf) {
    pdf_file <- file.path(dir, paste0(name, ".pdf"))
    # The base pdf() device: it draws math as paths, and its typewriter is
    # Courier, which is what the layout measures closely enough that the
    # columns of sources do not run into the next. (On some systems the
    # cairo device's "mono" is a much wider font than the one measured.)
    grDevices::pdf(pdf_file, width = page[["w"]], height = page[["h"]], onefile = TRUE)
    on.exit(grDevices::dev.off(), add = TRUE)
    for (k in seq_along(pages)) {
      grid::grid.newpage()
      gridmicrotex::grid.latex(
        paste(pages[[k]], collapse = "\n"), input_mode = "document", max_width = width,
        x = grid::unit(margin[["side"]], "in"), y = grid::unit(page[["h"]] - margin[["top"]], "in"),
        hjust = 0, vjust = 1, gp = grid::gpar(fontsize = size))
      grid::grid.text(sprintf("gridmicrotex: what is supported - page %d of %d", k, length(pages)),
                      x = 0.5, y = grid::unit(0.45, "in"), gp = grid::gpar(fontsize = 8, col = "grey40"))
    }
    return(invisible(c(tex = tex_file, pdf = pdf_file)))
  }
  invisible(c(tex = tex_file))
}
