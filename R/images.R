# Images: resolving \includegraphics, and drawing what it refers to.
#
# The split is deliberate. R answers two questions at two different times:
#
#   parse time  .image_dims()  -- how big is this file, so the engine can
#                                 reserve a box of the right size
#   draw  time  .image_grob()  -- what grob actually goes in that box
#
# Keeping them apart is what lets an SVG be drawn as true vector: the box
# is fixed early, but the choice of rasterGrob vs grImport2 pictureGrob is
# made late, and a new reader never needs an engine change.
#
# .resolve_graphics() rewrites
#     \includegraphics[width=2cm]{fig.png}
# into the private, fully-resolved
#     \gmgraphics{<hex>}{<width_bp>}{<height_bp>}
# which src/MicroTeX/lib/atom/image_atom.cpp turns into a box and an IMAGE draw record.

# --- reference encoding ------------------------------------------------
#
# The payload is hex because a real path is hostile to everything between
# here and MicroTeX: Windows paths hold backslashes, paths hold spaces,
# .strip_document_wrappers() eats `%`-to-end-of-line, and .expand_macros()
# would rewrite `\Users` or `\Temp` mid-path for anyone who had defined a
# macro by that name. Hex is [0-9a-f]+ and survives all of it.
#
# mtime and size ride along so the layout cache -- which keys on the tex
# string and holds no file metadata -- misses when a figure is edited in
# place. Without them an edited figure would serve a stale layout.
.image_ref_encode <- function(path) {
  info <- file.info(path)
  payload <- paste(path, as.numeric(info$mtime), info$size, sep = "\x1f")
  paste(sprintf("%02x", as.integer(charToRaw(payload))), collapse = "")
}

.image_ref_decode <- function(hex) {
  n <- nchar(hex)
  if (n == 0L || n %% 2L != 0L) return(NULL)
  bytes <- strtoi(substring(hex, seq(1L, n, 2L), seq(2L, n, 2L)), 16L)
  parts <- strsplit(rawToChar(as.raw(bytes)), "\x1f", fixed = TRUE)[[1]]
  if (!length(parts)) return(NULL)
  parts[1]
}

# --- units -------------------------------------------------------------

# One LaTeX length -> big points. `\textwidth` / `\linewidth` have no page
# to relate to in a grob, so they resolve against max_width; without one
# they read as NA and the caller falls back to the file's own size, which
# keeps the figure on the page. Returns NA when the length cannot be read.
.REL_LEN <- "\\\\(?:text|line|column)width"

.tex_len_bp <- function(s, fontsize, max_width) {
  s <- trimws(s)
  if (!nzchar(s)) return(NA_real_)
  # A leading multiplier on \textwidth / \linewidth, e.g. 0.8\textwidth.
  # Two capture groups, so a match is length 3: whole, multiplier, name.
  m <- regmatches(s, regexec("^([0-9.eE+-]*)\\\\(textwidth|linewidth|columnwidth)$", s))[[1]]
  if (length(m) == 3L) {
    if (!isTRUE(max_width > 0)) return(NA_real_)
    mult <- if (nzchar(m[2])) suppressWarnings(as.numeric(m[2])) else 1
    return(if (is.finite(mult)) mult * max_width else NA_real_)
  }
  # The exponent must be followed by digits, or a greedy [0-9.eE+-]+ would
  # read "2em" as the number "2e" and the unit "m".
  m <- regmatches(s, regexec(
    "^([+-]?(?:[0-9]*\\.?[0-9]+(?:[eE][+-]?[0-9]+)?))\\s*([A-Za-z]*)$", s))[[1]]
  if (length(m) != 3L) return(NA_real_)
  v <- suppressWarnings(as.numeric(m[2]))
  if (!is.finite(v)) return(NA_real_)
  # Big points per unit. `pt` is TeX's 1/72.27in, `bp` the 1/72in this
  # package works in; `px` follows the 96dpi screen convention used for
  # intrinsic sizes. `ex` is approximated as half an em, as elsewhere.
  # A bare number means big points. (Handled before switch(): an empty
  # name is not legal there.)
  unit <- tolower(m[3])
  per <- if (!nzchar(unit)) 1 else switch(unit,
    bp   = 1,
    pt   = 72 / 72.27,
    pc   = 12 * 72 / 72.27,
    `in` = 72,
    cm   = 72 / 2.54,
    mm   = 72 / 25.4,
    px   = 72 / 96,
    em   = fontsize,
    ex   = fontsize / 2,
    NA_real_
  )
  if (is.na(per)) return(NA_real_)
  v * per
}

# --- intrinsic size ----------------------------------------------------

.image_ext <- function(path) tolower(tools::file_ext(path))

# The extensions graphicx tries when the argument has none, vector first,
# mirroring the driver's own `\DeclareGraphicsExtensions{.pdf,.png,...}`.
.IMAGE_EXTS <- c("svg", "png", "jpg", "jpeg")

# Find the file `\includegraphics{...}` names. LaTeX does two things here
# that a bare file.exists() does not: it appends a known extension when the
# argument has none (`{plots/fig}` is the idiomatic form), and it looks in
# the directories `\graphicspath` declared. Returns the path unchanged when
# nothing matches, so the caller reports the name the user wrote.
.image_find <- function(path, dirs = character(0)) {
  if (!nzchar(path)) return(path)
  bases <- path
  # An absolute path ignores \graphicspath, as in LaTeX.
  if (length(dirs) && !grepl("^(?:[A-Za-z]:[/\\\\]|[/\\\\~])", path)) {
    bases <- c(path, file.path(sub("[/\\\\]+$", "", dirs), path))
  }
  for (b in bases) {
    if (file.exists(b) && !dir.exists(b)) return(b)
    for (e in .IMAGE_EXTS) {
      cand <- paste0(b, ".", e)
      if (file.exists(cand)) return(cand)
    }
  }
  path
}

# `\graphicspath{{figs/}{../shared/}}` -> the directories, and the tex with
# the declaration removed. Removed rather than left alone because MicroTeX
# has no such command and would typeset the argument as visible text.
.extract_graphicspath <- function(tex) {
  dirs <- character(0)
  repeat {
    m <- regexpr("\\\\graphicspath\\s*\\{", tex, perl = TRUE)
    if (m == -1L) break
    open <- m + attr(m, "match.length") - 1L
    close <- .find_close_brace(tex, open + 1L)
    if (is.na(close)) break
    inner <- substr(tex, open + 1L, close - 1L)
    dirs <- c(dirs, trimws(regmatches(inner,
      gregexpr("(?<=\\{)[^{}]*(?=\\})", inner, perl = TRUE))[[1]]))
    tex <- paste0(substr(tex, 1L, m - 1L), substr(tex, close + 1L, nchar(tex)))
  }
  list(tex = tex, dirs = dirs[nzchar(dirs)])
}

# Intrinsic size in big points, or NULL when the file cannot be read.
# `px` is a list(w, h) of pixels for bitmaps and NULL for vector sources,
# so the caller knows whether an effective-dpi warning makes sense.
.image_dims <- function(path) {
  if (!nzchar(path) || !file.exists(path)) return(NULL)
  ext <- .image_ext(path)
  if (ext == "svg") {
    # Both draw-time routes go through rsvg -- grImport2 needs it to
    # normalise to Cairo SVG, and the raster fallback *is* it. Without it
    # nothing can be drawn, so refuse the size too: reserving a box we
    # cannot fill would leave a silent hole where the figure belongs.
    if (!requireNamespace("rsvg", quietly = TRUE)) return(NULL)
    d <- .svg_dims_bp(path)
    if (is.null(d)) return(NULL)
    return(list(w = d$w, h = d$h, px = NULL))
  }
  ras <- .image_raster(path)
  if (is.null(ras)) return(NULL)
  # 96 dpi, the screen convention this package already uses for block
  # images in markdown.
  list(w = ras$w_px * 72 / 96, h = ras$h_px * 72 / 96,
       px = list(w = ras$w_px, h = ras$h_px))
}

# An SVG's intrinsic size, from the root element. Read from the file the
# user gave us, not from the rsvg-normalised copy: svglite writes
# `width='216.00pt'` (3in), while the normalised form writes a bare `216`,
# which is user units (px) and would be 2.25in.
.svg_dims_bp <- function(path) {
  x <- tryCatch(xml2::read_xml(path), error = function(e) NULL)
  if (is.null(x)) return(NULL)
  # Well-formed XML is not an SVG. Without this a `.svg` holding anything
  # else with width/height attributes would size a box that no reader can
  # then fill.
  if (!identical(xml2::xml_name(x), "svg")) return(NULL)
  one <- function(a) {
    v <- xml2::xml_attr(x, a)
    if (is.na(v) || !nzchar(v)) return(NA_real_)
    # SVG's own unit set; a bare number is user units, i.e. px.
    m <- regmatches(v, regexec("^\\s*([0-9.eE+-]+)\\s*([a-z%]*)\\s*$", v))[[1]]
    if (length(m) != 3L) return(NA_real_)
    n <- suppressWarnings(as.numeric(m[2]))
    if (!is.finite(n)) return(NA_real_)
    # A bare number is SVG user units, i.e. px at 96dpi.
    per <- if (!nzchar(m[3])) 72 / 96 else switch(m[3],
      px = 72 / 96, pt = 1, pc = 12,
      `in` = 72, cm = 72 / 2.54, mm = 72 / 25.4, NA_real_)
    if (is.na(per)) return(NA_real_)
    n * per
  }
  w <- one("width"); h <- one("height")
  if (is.na(w) || is.na(h)) {
    vb <- xml2::xml_attr(x, "viewBox")
    if (is.na(vb)) return(NULL)
    p <- suppressWarnings(as.numeric(strsplit(trimws(vb), "[ ,]+")[[1]]))
    if (length(p) != 4L || anyNA(p)) return(NULL)
    # viewBox is in user units.
    w <- p[3] * 72 / 96; h <- p[4] * 72 / 96
  }
  if (!is.finite(w) || !is.finite(h) || w <= 0 || h <= 0) return(NULL)
  list(w = w, h = h)
}

# --- loading -----------------------------------------------------------

.image_cache <- new.env(parent = emptyenv())

.image_cache_clear <- function() {
  rm(list = ls(.image_cache, all.names = TRUE), envir = .image_cache)
  invisible(NULL)
}

.image_stamp <- function(path) {
  i <- file.info(path)
  paste(path, as.numeric(i$mtime), i$size, sep = "\x1f")
}

# Read a bitmap. Moved here from R/markdown-box.R unchanged so the inline
# and block paths share one loader; png and jpeg stay Suggests, so a
# missing reader degrades rather than becoming a hard dependency.
.image_raster <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path)) return(NULL)
  if (!file.exists(path)) return(NULL)
  key <- paste0("ras\x1f", .image_stamp(path))
  hit <- .image_cache[[key]]
  if (!is.null(hit)) return(hit)
  reader <- switch(.image_ext(path),
    png = if (requireNamespace("png", quietly = TRUE)) png::readPNG else NULL,
    jpg = ,
    jpeg = if (requireNamespace("jpeg", quietly = TRUE)) jpeg::readJPEG else NULL,
    NULL
  )
  if (is.null(reader)) return(NULL)
  ras <- tryCatch(reader(path), error = function(e) NULL)
  if (is.null(ras) || length(dim(ras)) < 2L) return(NULL)
  out <- list(raster = grDevices::as.raster(ras),
              w_px = dim(ras)[2], h_px = dim(ras)[1])
  .image_cache[[key]] <- out
  out
}

# An SVG as real grid primitives. grImport2 needs Cairo SVG; svglite's
# output is not (verified: readPicture() fails with "color intensity NA"),
# so rsvg normalises it first. checkValidSVG() warns even on input it then
# reads correctly, so its noise is suppressed rather than shown to users.
.image_picture <- function(path) {
  if (!requireNamespace("grImport2", quietly = TRUE) ||
      !requireNamespace("rsvg", quietly = TRUE)) {
    return(NULL)
  }
  key <- paste0("pic\x1f", .image_stamp(path))
  hit <- .image_cache[[key]]
  if (!is.null(hit)) return(hit)
  out <- tryCatch({
    cairo <- tempfile(fileext = ".svg")
    on.exit(unlink(cairo), add = TRUE)
    rsvg::rsvg_svg(path, cairo)
    suppressWarnings(grImport2::readPicture(cairo))
  }, error = function(e) NULL)
  if (is.null(out)) return(NULL)
  .image_cache[[key]] <- out
  out
}

# The device's resolution, for choosing how finely to rasterise a vector
# source. Falls back to 96 when no device can answer.
.device_dpi <- function() {
  dpi <- tryCatch({
    px <- grDevices::dev.size("px"); inch <- grDevices::dev.size("in")
    if (length(px) && length(inch) && inch[1] > 0) px[1] / inch[1] else NA_real_
  }, error = function(e) NA_real_)
  if (!is.finite(dpi) || dpi <= 0) 96 else dpi
}

# The grob to place in the reserved box, chosen at draw time.
#
#   1. SVG as true vector, via grImport2
#   2. SVG rasterised by rsvg at the *output device's* resolution -- the
#      one thing that cannot be decided when the file is written
#   3. PNG / JPEG
#
# NULL when nothing can read it; the resolver has already drawn the name.
.image_grob <- function(path, w_bp, h_bp,
                        x = grid::unit(0, "bigpts"),
                        y = grid::unit(0, "bigpts"), name = NULL,
                        angle = 0) {
  # `x`/`y` are the top-left corner as grid units, so a caller can pass an
  # expression -- the block-markdown layout positions from the top with
  # `unit(1,"npc") - unit(y,"bigpts")`, which a bare number cannot express.
  #
  # Under `angle` they are the box's CENTRE instead, because that is the only
  # anchor a rotation leaves put. The unrotated shapes below are deliberately
  # untouched by this branch: a rotated image is rare, and wrapping every
  # image in a viewport would change what `.md_shift_grob()` and the block
  # layout see.
  if (!isTRUE(all.equal(angle %||% 0, 0))) {
    inner <- .image_grob(path, w_bp, h_bp,
                         x = grid::unit(0, "npc"), y = grid::unit(1, "npc"),
                         name = name)
    if (is.null(inner)) return(NULL)
    return(grid::grobTree(inner, vp = grid::viewport(
      x = x, y = y,
      width = grid::unit(w_bp, "bigpts"), height = grid::unit(h_bp, "bigpts"),
      just = "centre", angle = angle), name = name %||% "image"))
  }
  place <- list(x = x, y = y,
                width = grid::unit(w_bp, "bigpts"),
                height = grid::unit(h_bp, "bigpts"))
  if (.image_ext(path) == "svg") {
    pic <- .image_picture(path)
    if (!is.null(pic)) {
      # Wrapped in a gTree carrying a plain viewport rather than positioned
      # directly. pictureGrob's own `vp` is a vpStack, whose `x` is NULL, so
      # .md_shift_grob() -- which nudges `vp$x` to realign a block -- would
      # do `NULL + unit(...)` and error. With this wrapper both it and the
      # inline builder see an ordinary viewport they can move.
      return(grid::grobTree(
        grImport2::pictureGrob(
          pic, x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc"),
          width = grid::unit(1, "npc"), height = grid::unit(1, "npc"),
          just = "centre", expansion = 0, distort = TRUE),
        vp = grid::viewport(x = place$x, y = place$y,
                            width = place$width, height = place$height,
                            just = c("left", "top")),
        name = name %||% "image"))
    }
    if (requireNamespace("rsvg", quietly = TRUE)) {
      px <- max(1L, as.integer(round(w_bp / 72 * .device_dpi())))
      arr <- tryCatch(rsvg::rsvg(path, width = px), error = function(e) NULL)
      if (!is.null(arr)) {
        return(grid::rasterGrob(
          grDevices::as.raster(arr), x = place$x, y = place$y,
          width = place$width, height = place$height,
          just = c("left", "top"), interpolate = TRUE, name = name))
      }
    }
    return(NULL)
  }
  ras <- .image_raster(path)
  if (is.null(ras)) return(NULL)
  grid::rasterGrob(ras$raster, x = place$x, y = place$y,
                   width = place$width, height = place$height,
                   just = c("left", "top"), interpolate = TRUE, name = name)
}

# Can this reference actually be drawn? Used by the markdown paths, which
# must decide *before* emitting: a drawable file becomes
# `\includegraphics{}`, and anything else -- a web URL, a missing file, an
# unsupported format -- keeps its alt text and stays silent, which is what
# markdown has always done and what stops a remote image warning.
.image_drawable <- function(path) {
  !is.null(path) && !is.na(path) && nzchar(path) && !is.null(.image_dims(path))
}

# --- the resolver ------------------------------------------------------

# One warning per file per *state*, not per parse.
#
# Resetting this per parse would defeat it entirely: .md_measure() calls
# latex_dims() up to seven times and editDetails() re-runs the pipeline,
# and each of those is its own parse. So the key carries the file's stamp
# instead and the record is kept for the session -- a file that is later
# created or edited has a different stamp and is allowed to speak again,
# while a persistently missing one is reported exactly once.
.image_warned <- new.env(parent = emptyenv())

.image_warn_once <- function(key, msg) {
  if (!is.null(.image_warned[[key]])) return(invisible(FALSE))
  .image_warned[[key]] <- TRUE
  warning(msg, call. = FALSE)
  invisible(TRUE)
}

.image_reset_warnings <- function() {
  rm(list = ls(.image_warned, all.names = TRUE), envir = .image_warned)
}

# Split an option list on commas that are not inside braces.
.split_opts <- function(s) {
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  out <- character(0); buf <- character(0); depth <- 0L
  for (ch in chars) {
    if (ch == "{") depth <- depth + 1L
    if (ch == "}") depth <- max(0L, depth - 1L)
    if (ch == "," && depth == 0L) {
      out <- c(out, paste(buf, collapse = "")); buf <- character(0)
    } else {
      buf <- c(buf, ch)
    }
  }
  c(out, paste(buf, collapse = ""))
}

# `[width=2cm,keepaspectratio]` -> list(width = "2cm", keepaspectratio =
# "true"). graphicx keys may be valueless (keepaspectratio, clip, draft),
# which a `key=value` pattern alone would drop.
.parse_graphics_opts <- function(opt) {
  out <- list()
  for (kv in .split_opts(opt)) {
    kv <- trimws(kv)
    if (!nzchar(kv)) next
    p <- regmatches(kv, regexec("^\\s*([A-Za-z]+)\\s*=\\s*(.*?)\\s*$", kv))[[1]]
    if (length(p) == 3L) {
      out[[tolower(p[2])]] <- gsub("^\\{|\\}$", "", p[3])
    } else if (grepl("^[A-Za-z]+$", kv)) {
      out[[tolower(kv)]] <- "true"
    }
  }
  out
}

.opt_flag <- function(v) !is.null(v) && !tolower(v) %in% c("false", "0", "off")

# The final width/height in big points for one image.
.image_size_bp <- function(dims, opts, fontsize, max_width) {
  w <- .tex_len_bp(opts[["width"]] %||% "", fontsize, max_width)
  h <- .tex_len_bp(opts[["height"]] %||% "", fontsize, max_width)
  aspect <- dims$h / dims$w
  if (is.na(w) && is.na(h)) {
    w <- dims$w; h <- dims$h
  } else if (is.na(h)) {
    h <- w * aspect
  } else if (is.na(w)) {
    w <- h / aspect
  } else if (.opt_flag(opts[["keepaspectratio"]])) {
    # graphicx fits the picture *inside* width x height rather than
    # stretching it to fill: the smaller of the two scale factors wins.
    s <- min(w / dims$w, h / dims$h)
    w <- dims$w * s; h <- dims$h * s
  }
  # `scale` multiplies whatever width/height settled on, as in graphicx --
  # it is not an alternative to them.
  sc <- suppressWarnings(as.numeric(opts[["scale"]] %||% NA))
  if (is.finite(sc) && sc > 0) { w <- w * sc; h <- h * sc }
  # An absolutely sized image cannot shrink, so .md_measure()'s retry loop
  # would never converge on one wider than its column: clamp here instead.
  if (isTRUE(max_width > 0) && w > max_width) {
    h <- h * max_width / w
    w <- max_width
  }
  list(w = w, h = h)
}

# Rewrite every \includegraphics in `tex`. Idempotent: the output is
# \gmgraphics, which this never matches.
#
# Runs twice in .parse_from_gp() -- once at the very top, before
# .strip_document_wrappers() and .expand_macros() can mangle a path, and
# once after macro expansion to catch an \includegraphics a user macro
# produced. A third case, \newcommand expanded inside MicroTeX's own
# parser, is out of reach of both and is handled by the C++ override in
# src/MicroTeX/lib/atom/image_atom.cpp, which draws the file's name.
.resolve_graphics <- function(tex, fontsize = 20, max_width = 0) {
  gp <- .extract_graphicspath(tex)
  out <- gp$tex
  if (!grepl("\\includegraphics", out, fixed = TRUE)) return(out)
  from <- 1L
  repeat {
    # The starred form clips to the bounding box; there is nothing outside
    # the box to clip here, so it differs from the plain form only in being
    # spelled with a `*`.
    m <- regexpr("\\\\includegraphics\\*?", substr(out, from, nchar(out)), perl = TRUE)
    if (m == -1L) break
    start <- from + m - 1L
    i <- start + attr(m, "match.length")
    n <- nchar(out)
    skip_ws <- function(k) { while (k <= n && grepl("^[ \t\r\n]$", substr(out, k, k))) k <- k + 1L; k }
    i <- skip_ws(i)
    opt <- character(0)
    bad <- FALSE
    # Up to two bracket groups: the graphicx `[key=val]` and the older
    # `[llx,lly][urx,ury]` spelling, whose bare numbers simply parse as no
    # recognised key.
    while (i <= n && substr(out, i, i) == "[" && length(opt) < 2L) {
      # Scan to the matching `]`, ignoring one inside braces so
      # `[width={0.5\textwidth}]` survives.
      j <- i + 1L; depth <- 0L
      while (j <= n) {
        ch <- substr(out, j, j)
        if (ch == "{") depth <- depth + 1L
        else if (ch == "}") depth <- max(0L, depth - 1L)
        else if (ch == "]" && depth == 0L) break
        j <- j + 1L
      }
      if (j > n) { bad <- TRUE; break }
      opt <- c(opt, substr(out, i + 1L, j - 1L))
      i <- skip_ws(j + 1L)
    }
    if (bad) break
    if (i > n || substr(out, i, i) != "{") { from <- start + 1L; next }
    close <- .find_close_brace(out, i + 1L)
    if (is.na(close)) break
    path <- trimws(substr(out, i + 1L, close - 1L))

    rep <- .resolve_one_graphic(path, paste(opt, collapse = ","),
                                fontsize, max_width, gp$dirs)
    out <- paste0(substr(out, 1L, start - 1L), rep,
                  substr(out, close + 1L, nchar(out)))
    from <- start + nchar(rep)
  }
  out
}

# graphicx keys that change what is drawn but that a fixed box cannot
# honour. Ignoring them silently would draw a figure that is wrong in a way
# nothing in the output hints at. `angle` is NOT among them: it is turned
# into a \rotatebox below, which the recorder and the grid builder carry
# through to a rotated viewport.
.IMAGE_IGNORED_OPTS <- c("origin", "trim", "clip", "viewport", "bb")

.resolve_one_graphic <- function(path, opt, fontsize, max_width,
                                 dirs = character(0)) {
  named <- path
  path <- .image_find(path, dirs)
  fallback <- function() paste0("\\text{", .md_escape_tex(basename(named)), "}")
  dims <- .image_dims(path)
  if (is.null(dims)) {
    ext <- .image_ext(path)
    why <- if (!file.exists(path)) {
      "file not found"
    } else if (dir.exists(path)) {
      "it is a directory, not a file"
    } else if (ext %in% c("pdf", "eps", "ps")) {
      paste0("'", ext, "' is not supported; save the figure as SVG instead")
    } else if (ext %in% c("png", "jpg", "jpeg")) {
      # "no dimensions" has two causes and they need different answers:
      # the reader is absent, or the reader is present and the bytes are
      # not what the extension claims.
      pkg <- if (ext == "png") "png" else "jpeg"
      if (!requireNamespace(pkg, quietly = TRUE)) {
        paste0("the '", pkg, "' package is needed to read it")
      } else {
        paste0("the '", pkg, "' package could not read it -- the file may be ",
               "empty, truncated, or not really ", toupper(ext))
      }
    } else if (!nzchar(ext)) {
      "it has no extension, and no .svg, .png, .jpg or .jpeg of that name exists"
    } else if (ext == "svg") {
      if (!requireNamespace("rsvg", quietly = TRUE)) {
        "drawing an SVG needs the 'rsvg' package"
      } else {
        "it has no <svg> root element, or no readable size on it"
      }
    } else {
      paste0("'", ext, "' is not a supported image format (PNG, JPEG, SVG)")
    }
    .image_warn_once(paste0("miss\x1f", .image_stamp(path)),
                     paste0("Cannot draw '", named, "': ", why,
                            ". Drawing the file name instead."))
    return(fallback())
  }

  opts <- .parse_graphics_opts(opt)

  ignored <- intersect(names(opts), .IMAGE_IGNORED_OPTS)
  if (length(ignored)) {
    .image_warn_once(
      paste0("opt\x1f", .image_stamp(path), "\x1f", paste(ignored, collapse = ",")),
      paste0("Ignoring \\includegraphics option",
             if (length(ignored) > 1L) "s " else " ",
             paste0("`", ignored, "`", collapse = ", "),
             " for '", basename(named), "': the image is drawn whole, ",
             "uncropped, and about its own centre."))
  }

  # A length that cannot be read is silently ignored by the arithmetic
  # below, so `[width=3 inches]` or a typo would come out at the file's own
  # size with nothing to say why. `\textwidth` without a max_width is the
  # one unreadable case with its own message, just below.
  for (k in c("width", "height")) {
    v <- opts[[k]]
    if (is.null(v) || !nzchar(trimws(v))) next
    if (grepl(.REL_LEN, v, perl = TRUE)) next
    if (is.na(.tex_len_bp(v, fontsize, max_width))) {
      .image_warn_once(
        paste0("len\x1f", .image_stamp(path), "\x1f", k, "\x1f", v),
        paste0("Cannot read `", k, "=", v, "` for '", basename(named),
               "'; drawing it at its own size."))
    }
  }
  if (!is.null(opts[["scale"]])) {
    sc <- suppressWarnings(as.numeric(opts[["scale"]]))
    if (!is.finite(sc) || sc <= 0) {
      .image_warn_once(
        paste0("scl\x1f", .image_stamp(path), "\x1f", opts[["scale"]]),
        paste0("Ignoring `scale=", opts[["scale"]], "` for '", basename(named),
               "': a scale must be a positive number."))
    }
  }

  # `\textwidth` has no page to measure in a grob. Falling back to the
  # file's own size keeps the figure visible, which is nearer to what the
  # document meant than dropping it, but it is not what was asked for.
  if (!isTRUE(max_width > 0) &&
      any(grepl(.REL_LEN, unlist(opts[c("width", "height")]), perl = TRUE))) {
    .image_warn_once(
      paste0("relw\x1f", .image_stamp(path)),
      paste0("'", basename(named), "' is sized against `\\textwidth`, which ",
             "has no meaning without `max_width`; drawing it at its own size."))
  }

  size <- tryCatch(.image_size_bp(dims, opts, fontsize, max_width),
                   error = function(e) { .image_warn_once(
                     paste0("size\x1f", .image_stamp(path), "\x1f", opt),
                     paste0("Cannot size '", named, "': ", conditionMessage(e))); NULL })
  # The floor is where `sprintf("%.4f")` below stops being able to say a
  # number: anything smaller writes "0.0000" and reserves a box of nothing,
  # which draws as an invisible gap rather than as a very small picture.
  if (is.null(size) || !is.finite(size$w) || !is.finite(size$h) ||
      size$w < 5e-4 || size$h < 5e-4) {
    if (!is.null(size) && is.finite(size$w) && is.finite(size$h)) {
      .image_warn_once(
        paste0("zero\x1f", .image_stamp(path), "\x1f", opt),
        sprintf(paste0("`%s` sizes '%s' to %.4g x %.4g bp, which is nothing ",
                       "to draw. Drawing the file name instead."),
                opt, basename(named), size$w, size$h))
    }
    return(fallback())
  }

  # Effective resolution, for bitmaps drawn at a size the author chose.
  #
  # Only then: at intrinsic size the answer is 96 dpi by construction --
  # that is the convention pixels are read with -- so warning there would
  # fire for every unsized bitmap and say nothing actionable. The useful
  # case is "you asked for 3in and there are only 96 pixels". The size
  # check, not just the option check, because a `\textwidth` that could not
  # resolve lands back on the intrinsic size with `width` still named.
  #
  # A vector source has no effective resolution at all, which is precisely
  # the reason to prefer SVG.
  sized <- any(c("width", "height", "scale") %in% names(opts)) &&
    !isTRUE(all.equal(size$w, dims$w))
  if (sized && !is.null(dims$px)) {
    dpi <- dims$px$w * 72 / size$w
    if (is.finite(dpi) && dpi < 150) {
      # %.0f, not %d: a preposterous width -- `[width=1e7in]` -- makes a
      # pixel count no integer can hold, and sprintf("%d") errors on it
      # rather than degrading.
      want <- ceiling(size$w / 72 * 300)
      want_h <- ceiling(size$h / 72 * 300)
      .image_warn_once(
        paste0("dpi\x1f", .image_stamp(path), "\x1f", round(size$w)),
        sprintf(paste0("Image '%s' is %.0f dpi at this size; save it at ",
                       "%.0f x %.0f px for 300 dpi, or use SVG."),
                basename(path), dpi, want, want_h))
    }
  }

  # sprintf, never as.character: Units::getUnit() falls back to "pixel" for
  # an unrecognised suffix with no error path, so "1e-05bp" would silently
  # parse as 1 pixel.
  out <- sprintf("\\gmgraphics{%s}{%.4f}{%.4f}",
                 .image_ref_encode(path), size$w, size$h)

  # `angle` becomes a real rotation by handing it to \rotatebox, which the
  # engine already knows: it rotates the transform, the recorder reads the
  # angle off it, and the grid builder places the grob in a rotated
  # viewport. \rotatebox also grows the surrounding box to the rotated
  # bounds, which is what LaTeX does and what a fixed-size record could not
  # have expressed on its own.
  ang <- suppressWarnings(as.numeric(opts[["angle"]] %||% NA))
  if (is.finite(ang) && ang %% 360 != 0) {
    out <- sprintf("\\rotatebox{%.4f}{%s}", ang, out)
  }
  out
}
