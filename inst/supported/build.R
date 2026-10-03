# The list of what gridmicrotex draws. examples.tsv holds the examples, in the order of KaTeX's
# "Supported Functions" page; supported-functions.tex sets them, three to a row where they fit, in
# an ordinary LaTeX file that pdflatex compiles; and the package renders that same file to a PDF.
#
#   source(system.file("supported/build.R", package = "gridmicrotex"))
#   build_supported(tempdir())      # writes supported-functions.tex and .pdf
#
# Neither file is kept in the package: they are made from examples.tsv. The website's copies are
# in pkgdown/assets/ -- run build_supported() and copy both there when the examples change.
#
# The package has no pages (a grob is one piece), so a page is what lies between two \newpage
# lines of the file, and the page numbers are the footer drawn here. A row is `ok` when LaTeX sets
# it too (its output is shown), `nolatex` when only the package does (the name is listed, since
# there is nothing to compare it with).

.sup_page <- c(w = 8.5, h = 11)
.sup_margin <- c(side = 0.7, top = 0.65, bottom = 0.85)

.sup_preamble <- c(
  r"(\documentclass[9pt]{extarticle})",
  r"(\usepackage[letterpaper,margin=0.7in]{geometry})",
  r"(\usepackage[utf8]{inputenc})",
  r"(\usepackage{amsmath,amssymb,amscd,mathtools,cancel,bm,mathrsfs,stmaryrd,textcomp,xcolor,mathdots,amsthm})",
  r"(\usepackage[normalem]{ulem})",
  r"(\usepackage[version=4]{mhchem}\usepackage{siunitx,undertilde})",
  r"(\usepackage{tikz-cd}\usetikzlibrary{decorations.pathmorphing})")

supported_tex <- function(path = "supported-functions.tex",
                          examples = system.file("supported/examples.tsv", package = "gridmicrotex")) {
  stopifnot(nzchar(examples), file.exists(examples))
  ex <- utils::read.delim(examples, quote = "", comment.char = "", stringsAsFactors = FALSE,
                          encoding = "UTF-8")
  ex <- ex[ex$kind %in% c("ok", "nolatex") & !grepl("[^ -~]", ex$source), ]
  size <- 9                                   # points
  budget <- (.sup_page[["h"]] - .sup_margin[["top"]] - .sup_margin[["bottom"]]) * 72
  width <- (.sup_page[["w"]] - 2 * .sup_margin[["side"]]) * 72
  tt <- 4.8                                   # a source character, Courier, points (cmtt is narrower)

  verb <- function(s) {
    for (d in c("|", "!", "+", "=", "/", "@", "~", ";")) {
      if (!grepl(d, s, fixed = TRUE)) return(paste0("\\verb", d, s, d))
    }
    paste0("\\texttt{", gsub("([#$%&_{}])", "\\\\\\1", s), "}")
  }
  wrap_source <- function(s, n) {
    words <- strsplit(s, " ", fixed = TRUE)[[1]]
    lines <- character()
    cur <- ""
    for (w in words) {
      if (nzchar(cur) && nchar(cur) + 1L + nchar(w) > n) {
        lines <- c(lines, cur)
        cur <- w
      } else {
        cur <- if (nzchar(cur)) paste(cur, w) else w
      }
    }
    c(lines, cur)
  }
  small <- function(s) paste0("{\\small", s, "}")
  is_display <- function(s) grepl(r"(^\\begin\{(align|alignat|gather|equation|multline|flalign|eqnarray)\*?\})", s) ||
    grepl("\\tag", s, fixed = TRUE)
  display_tex <- function(s) if (grepl("\\tag", s, fixed = TRUE) && !grepl("^\\\\begin", s)) paste0("\\[", s, "\\]") else s

  measure <- function(s) {
    d <- tryCatch(suppressWarnings(
      gridmicrotex::latex_dims(s, input_mode = "math", gp = grid::gpar(fontsize = 10))),
      error = function(e) NULL)
    if (is.null(d)) return(c(w = 40, h = 14))
    c(w = grid::convertWidth(d$width, "bigpts", valueOnly = TRUE),
      h = grid::convertHeight(d$height, "bigpts", valueOnly = TRUE))
  }

  # Pages for a given allowance `fac` on the row heights, which are estimates (see below).
  paginate <- function(fac) {
    pages <- list()
    cur <- character()
    used <- 0
    buf <- character()
    kind <- "t3"
    title <- ""
    spec <- c(
      t3 = "@{}l@{\\hspace{4pt}}l@{\\hspace{14pt}}l@{\\hspace{4pt}}l@{\\hspace{14pt}}l@{\\hspace{4pt}}l@{}",
      t2 = "@{}l@{\\hspace{4pt}}l@{\\hspace{20pt}}l@{\\hspace{4pt}}l@{}",
      one = "@{}l@{}",
      n3 = "@{}l@{\\hspace{14pt}}l@{\\hspace{14pt}}l@{}",
      n1 = "@{}l@{}")
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
        cur <<- c(paste0("\\subsection*{", title, " (continued)}"), "")
        used <<- 22
      }
    }
    heading <- function(level, text, h, keep) {
      h <- h * fac
      if (used + h + keep > budget) {
        title <<- ""                  # the section before it has ended: no "(continued)"
        new_page()
      }
      flush_table()
      cur <<- c(cur, paste0("\\", level, "*{", text, "}"), "")
      used <<- used + h
    }
    row <- function(tex, h, k) {
      h <- h * fac
      if (used + h > budget) {
        new_page()
        kind <<- k
      } else if (k != kind) {
        flush_table()
        kind <<- k
      }
      buf <<- c(buf, tex)
      used <<- used + h
    }
    block <- function(lines, h) {       # a paragraph of its own
      flush_table()
      h <- h * fac
      if (used + h > budget) new_page()
      cur <<- c(cur, lines, "")
      used <<- used + h
    }

    intro <- c(
      "\\begin{center}\\Large\\textbf{What gridmicrotex draws}\\end{center}",
      "",
      paste0("Each row is a LaTeX input and what the package makes of it, three to a line where they fit, in ",
             "the order of KaTeX's list of supported functions, then what KaTeX does not have. A row is a ",
             "test: it must be drawn with no warning. Where LaTeX has nothing to compare with, only the ",
             "name is listed."),
      "",
      "\\textbf{This PDF is rendered with this package}.",
      "")
    cur <- intro
    used <- 90

    last_top <- ""
    for (sec in unique(ex$section)) {
      part <- strsplit(sec, " / ", fixed = TRUE)[[1]]
      e <- ex[ex$section == sec, ]
      top <- part[1]
      sub <- if (length(part) > 1) part[2] else NULL
      if (!identical(top, last_top)) {
        heading("section", top, 26, keep = 140)
        last_top <- top
        title <- top
      }
      if (!is.null(sub)) {
        heading("subsection", sub, 20, keep = 110)
        title <- paste(top, "/", sub)
      }

      ok <- e[e$kind == "ok", ]
      if (nrow(ok)) {
        shown <- !vapply(ok$source, is_display, NA)
        inline <- ok[shown, ]
        m <- t(vapply(inline$source, measure, c(w = 0, h = 0)))
        sw <- nchar(inline$source) * tt
        tier <- ifelse(sw <= 86 & m[, "w"] <= 62, 3L, ifelse(sw <= 150 & m[, "w"] <= 100, 2L, 1L))
        cell <- function(i) paste(small(verb(inline$source[i])), "&", paste0("{\\large $", inline$source[i], "\n$}"))
        hh <- pmax(12, m[, "h"] + 5)
        for (t in 3:2) {
          for (grp in unname(split(which(tier == t), ceiling(seq_along(which(tier == t)) / t)))) {
            cells <- vapply(grp, cell, "")
            cells <- c(cells, rep("&", t - length(cells)))
            row(paste0(paste(cells, collapse = " & "), " \\\\"), max(hh[grp]), paste0("t", t))
          }
        }
        for (i in which(tier == 1L)) {
          lines <- wrap_source(inline$source[i], 64)
          row(paste0(paste(vapply(lines, function(l) small(verb(l)), ""), collapse = " \\\\ "), " \\\\ ",
                     paste0("{\\large $", inline$source[i], "\n$}"), " \\\\[4pt]"),
              hh[i] + 12 * length(lines) + 6, "one")
        }
        for (s in ok$source[!shown]) {
          lines <- wrap_source(s, 64)
          h <- tryCatch(grid::convertHeight(gridmicrotex::latex_dims(display_tex(s), input_mode = "document",
                                                                        max_width = width,
                                                                        gp = grid::gpar(fontsize = 10))$height,
                                           "bigpts", valueOnly = TRUE), error = function(e) 40)
          block(c(small(paste(vapply(lines, verb, ""), collapse = " ")), "", display_tex(s)),
                h + 12 * length(lines) + 34)
        }
      }

      no <- e[e$kind == "nolatex", ]
      if (nrow(no)) {
        block(c("{\\small\\itshape Drawn by the package; LaTeX has no equivalent, so no output is shown:}"), 16)
        short <- nchar(no$source) <= 26
        items <- vapply(no$source, function(s) small(verb(s)), "", USE.NAMES = FALSE)
        for (grp in unname(split(which(short), ceiling(seq_along(which(short)) / 3)))) {
          cells <- c(items[grp], rep("", 3L - length(grp)))
          row(paste0(paste(cells, collapse = " & "), " \\\\"), 12, "n3")
        }
        for (i in which(!short)) row(paste0(items[i], " \\\\"), 12, "n1")
      }
    }
    new_page()
    pages <- pages[vapply(pages, length, 0L) > 0L]
    pages
  }

  # The estimates leave a page short of what the package then sets (a row's pitch is more than
  # its tallest symbol), so each page is measured as it will be drawn and, where one is too tall
  # for its area, the rows are given more room and the pages cut again.
  page_height <- function(p) {
    d <- tryCatch(suppressWarnings(gridmicrotex::latex_dims(
      paste(p, collapse = "\n"), input_mode = "document", max_width = width,
      gp = grid::gpar(fontsize = size))), error = function(e) NULL)
    if (is.null(d)) return(0)
    grid::convertHeight(d$height, "bigpts", valueOnly = TRUE)
  }
  fac <- 1
  for (attempt in 1:8) {
    pages <- paginate(fac)
    worst <- max(vapply(pages, page_height, 0)) / budget
    if (worst <= 1) break
    fac <- fac * worst * 1.02
  }

  body <- unlist(lapply(seq_along(pages), function(k) c(if (k > 1L) "\\newpage", pages[[k]])))
  tex <- c("% Written by inst/supported/build.R from examples.tsv. Compile with pdflatex, or render with gridmicrotex.",
           .sup_preamble, "\\begin{document}", body, "\\end{document}")
  writeLines(tex, path, useBytes = TRUE)
  invisible(path)
}

# The PDF of a file like that: each page, between its \newpage lines, drawn as one grob.
#
# `device = "cairo_pdf"` (the default, where R has cairo) embeds the fonts, so the math is text
# that a viewer can select and search, and the file is a quarter the size. Its typewriter is
# whatever "mono" is on the system, which is not what the layout measured, so the sources are set
# in `mono_font` (an installed family, drawn as glyphs on any device). `device = "pdf"` is the
# fallback: base pdf() draws math as outlines and its typewriter is Courier.
render_supported_tex <- function(tex = "supported-functions.tex", pdf = sub("\\.tex$", ".pdf", tex),
                                 device = c("cairo_pdf", "pdf"), mono_font = "Courier New") {
  device <- match.arg(device)
  if (device == "cairo_pdf") {
    old <- tryCatch(gridmicrotex::latex_options(mono_font = mono_font), error = function(e) NULL)
    if (is.null(old) || !isTRUE(capabilities("cairo"))) {
      warning("cairo_pdf with '", mono_font, "' is not available here; using pdf().", call. = FALSE)
      device <- "pdf"
    } else {
      on.exit(do.call(gridmicrotex::latex_options, old), add = TRUE)
    }
  }
  lines <- readLines(tex, encoding = "UTF-8")
  from <- which(lines == "\\begin{document}")[1]
  to <- which(lines == "\\end{document}")[1]
  body <- lines[(from + 1L):(to - 1L)]
  page <- split(body, cumsum(body == "\\newpage"))
  page <- lapply(page, function(p) p[p != "\\newpage"])
  width <- (.sup_page[["w"]] - 2 * .sup_margin[["side"]]) * 72
  open <- if (device == "cairo_pdf") grDevices::cairo_pdf else grDevices::pdf
  open(pdf, width = .sup_page[["w"]], height = .sup_page[["h"]], onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (k in seq_along(page)) {
    grid::grid.newpage()
    gridmicrotex::grid.latex(
      paste(page[[k]], collapse = "\n"), input_mode = "document", max_width = width,
      x = grid::unit(.sup_margin[["side"]], "in"),
      y = grid::unit(.sup_page[["h"]] - .sup_margin[["top"]], "in"),
      hjust = 0, vjust = 1, gp = grid::gpar(fontsize = 9))
    grid::grid.text(sprintf("gridmicrotex: what is supported - page %d of %d", k, length(page)),
                    x = 0.5, y = grid::unit(0.45, "in"), gp = grid::gpar(fontsize = 8, col = "grey40"))
  }
  invisible(pdf)
}

build_supported <- function(dir = ".", pdf = TRUE, name = "supported-functions",
                            examples = system.file("supported/examples.tsv", package = "gridmicrotex"),
                            device = c("cairo_pdf", "pdf"), mono_font = "Courier New") {
  device <- match.arg(device)
  tex <- supported_tex(file.path(dir, paste0(name, ".tex")), examples)
  if (!pdf) return(invisible(c(tex = tex)))
  invisible(c(tex = tex, pdf = render_supported_tex(tex, file.path(dir, paste0(name, ".pdf")),
                                                    device, mono_font)))
}
