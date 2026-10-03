# Font name aliases for convenience
.font_aliases <- c(
  "stix"     = "STIX Two Math",
  "stix2"    = "STIX Two Math",
  "lete"     = "Lete Sans Math",
  "letesans" = "Lete Sans Math"
)

#' Resolve a math font name
#'
#' Translates short aliases (e.g., \code{"stix"}, \code{"lete"}) to the
#' full MicroTeX font name. Validates that the font is loaded.
#'
#' @param name Font name or alias. Empty string uses the default font.
#' @return The resolved font name.
#' @noRd
resolve_math_font <- function(name) {
  if (is.null(name) || !nzchar(name)) return("")

  # Check aliases first
  lower <- tolower(name)
  if (lower %in% names(.font_aliases)) {
    return(.font_aliases[[lower]])
  }

  # Check if it matches a loaded font (case-insensitive). This also
  # covers the exact-match case.
  loaded <- microtex_math_font_names()
  idx <- match(tolower(name), tolower(loaded))
  if (!is.na(idx)) {
    return(loaded[idx])
  }

  # A name given to load_font() for a math font, such as a short alias.
  reg <- .font_lookup(name)
  if (!is.null(reg) && isTRUE(.font_registry$fonts[[reg]]$math)) {
    return(.font_registry$fonts[[reg]]$display)
  }

  stop(
    "Math font '", name, "' not found. Available fonts: ",
    paste(loaded, collapse = ", "),
    "\nAliases: ", paste(names(.font_aliases), collapse = ", "),
    call. = FALSE
  )
}

#' List available math fonts
#'
#' Returns the math fonts that can be passed to `math_font`.
#'
#' @section Font pairing:
#' For a consistent look, pair each math font with a matching
#' `fontfamily` in `gp`:
#'
#' \tabular{lll}{
#'   \strong{Math font}     \tab \strong{Style}  \tab \strong{Suggested text font} \cr
#'   Lete Sans Math (\code{"lete"}, default) \tab Sans-serif \tab \code{"sans"} \cr
#'   STIX Two Math (\code{"stix"})   \tab Serif  \tab \code{"serif"} \cr
#' }
#' Additional math fonts can be loaded with \code{\link{load_math_font}}.
#'
#' @return A character vector of math font names.
#' @export
#'
#' @examples
#' available_math_fonts()
available_math_fonts <- function() {
  microtex_math_font_names()
}

# Internal: set the default math font used by MicroTeX.
# Public entry point is `latex_options(math_font = ...)`.
.set_math_font <- function(name) {
  if (!microtex_is_inited()) {
    stop("MicroTeX is not initialized.", call. = FALSE)
  }

  if (is.null(name) || !nzchar(name)) {
    stop(
      "Please provide a math font name. Use available_math_fonts() to list choices.",
      call. = FALSE
    )
  }

  resolved <- resolve_math_font(name)
  ok <- microtex_set_default_math_font(resolved)
  if (!ok) {
    stop(
      "Failed to set math font '", resolved, "'. Available fonts: ",
      paste(available_math_fonts(), collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# Internal: put MicroTeX back on the math font .onLoad() started it with.
.reset_math_font <- function() {
  if (microtex_is_inited()) {
    microtex_set_default_math_font(resolve_math_font("lete"))
  }
  invisible()
}

#' Load a font
#'
#' Registers a font under a name that works everywhere a font is named:
#' `gp = gpar(fontfamily = )`, [latex_options()] (`main_font`, `sans_font`,
#' `mono_font`, `math_font`) and `element_latex(family = )`. The font comes
#' from a file or from an installed family.
#'
#' A math font loaded here can also be used as text. A font that is already
#' installed does not need loading to be used as `gp$fontfamily`; load it to
#' give it a short name, or to set it as a role in [latex_options()].
#'
#' The text of a loaded font is measured and drawn by gridmicrotex from the
#' font's own file, so it looks the same on every device, base `pdf()`
#' included, with its kerning and ligatures. It is drawn as glyphs where the
#' device has them and as outlines where it has not (and for rotated text).
#' Other families are drawn by the device, as are right-to-left text and a
#' character the font has no glyph for, which the device may find in another
#' font. In base graphics with `latex_options(device_math = TRUE)` the device
#' draws all text, so it has to know the font itself.
#'
#' @param x A font file (`.otf`, `.ttf` or `.ttc`), or the name of an
#'   installed font family.
#' @param name The name to register the font under. The default is the
#'   font's family name.
#' @param bold,italic,bolditalic Files of the other faces of the same
#'   family, so that bold and italic text uses the real design. Only for a
#'   file: the faces of an installed family are found automatically. A face
#'   left out is drawn with the regular file.
#' @return The name the font is registered under, invisibly.
#' @seealso [available_fonts()], [latex_options()], [load_math_font()]
#' @export
#'
#' @examples
#' \donttest{
#'   # The bundled STIX font stands in for your own font file here
#'   otf <- system.file("fonts", "STIXTwoMath-Regular.otf",
#'                      package = "gridmicrotex")
#'   load_font(otf, name = "My Font")
#'   available_fonts()
#' }
load_font <- function(x, name = NULL, bold = NULL, italic = NULL,
                      bolditalic = NULL) {
  .check_string(x, "x")
  if (!is.null(name)) .check_string(name, "name")
  if (!microtex_is_inited()) {
    stop("MicroTeX is not initialized.", call. = FALSE)
  }
  .ensure_bundled_fonts_registered()

  others <- list(bold = bold, italic = italic, bolditalic = bolditalic)
  others <- others[!vapply(others, is.null, logical(1))]

  if (.font_is_file(x)) {
    faces <- list(plain = .font_file_face(x))
    for (nm in names(others)) {
      .check_string(others[[nm]], nm)
      faces[[nm]] <- .font_file_face(others[[nm]])
    }
    source <- "file"
  } else {
    if (length(others)) {
      stop("`bold`, `italic` and `bolditalic` are files of the other faces, ",
           "so they go with a font file; the faces of an installed family ",
           "are found automatically.", call. = FALSE)
    }
    found <- .system_font_faces(x)
    if (is.null(found)) {
      stop("Font '", x, "' is neither a font file nor an installed family.",
           call. = FALSE)
    }
    faces <- found$faces
    source <- "system"
  }

  # The same files again, under the same name: nothing to do. A document that
  # names a font file reaches here at every parse, and a load changes the
  # layout cache key. With no name given the font's own is meant, which an
  # entry carries when its name is the one the engine knows it by.
  for (known in .font_registry$fonts) {
    if (identical(known$faces, faces) && identical(known$source, source) &&
        (if (is.null(name)) identical(known$name, known$display)
         else identical(tolower(known$name), tolower(name)))) {
      return(invisible(known$name))
    }
  }

  # The engine reads one single-face file; a collection is cut down to the
  # face asked for first (R/ttc-splitter.R).
  plain <- faces$plain
  engine_file <- if (.is_ttc_file(plain$path)) {
    .extract_ttc_face(plain$path, plain$index)
  } else {
    plain$path
  }
  display <- microtex_add_font_from_otf(engine_file, 0L)
  if (!nzchar(display)) {
    stop("Could not read the font file: ", plain$path, call. = FALSE)
  }
  math <- display %in% microtex_math_font_names()
  if (!math && display %in% microtex_main_font_families()) {
    .text_font_registered[[engine_file]] <- display
  }
  name <- name %||% display

  # An installed family answers to its own name already; anything else is
  # told to systemfonts, which is what lets ragg and svglite, and
  # gp$fontfamily, find it.
  if (source == "file" || !identical(tolower(name), tolower(found$family))) {
    .register_font_with_systemfonts(name, faces)
  }

  info <- .font_file_info(plain$path, plain$index)
  .font_registry_add(list(
    name = name, display = display, source = source, math = math,
    mono = info$mono, weight = info$weight, faces = faces
  ))
  invisible(name)
}

#' Load a math font from an OTF file
#'
#' Adds an OpenType math font, such as Latin Modern Math, for use as
#' `math_font`. The font can then also be used as a `fontfamily` for
#' plot text. This is [load_font()] for a font that must have a math table.
#'
#' @param otf_path Path to an OTF or TTF math font.
#' @return `NULL`, invisibly.
#' @seealso \code{\link{load_font}}, \code{\link{available_math_fonts}},
#'   \code{\link{check_math_fonts}}, \code{\link{latex_options}},
#'   \code{\link{latex_grob}}
#' @export
#'
#' @examples
#' \donttest{
#'   # The bundled STIX font stands in for your own math font here
#'   otf <- system.file("fonts", "STIXTwoMath-Regular.otf",
#'                      package = "gridmicrotex")
#'   load_math_font(otf)
#'   available_math_fonts()
#' }
load_math_font <- function(otf_path) {
  if (!file.exists(otf_path)) {
    stop("Font file not found: ", otf_path, call. = FALSE)
  }
  name <- load_font(otf_path)
  if (!isTRUE(.font_registry$fonts[[name]]$math)) {
    stop(
      "Could not read OpenType MATH table from: ", basename(otf_path), "\n",
      "The font may not be a math font, or may have an unsupported MATH ",
      "table layout.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' List the fonts that have been loaded
#'
#' One row per font that can be named: the bundled math fonts, and every font
#' given to [load_font()] or to a font option of [latex_options()].
#'
#' @param system If `TRUE`, also list the installed font families, which work
#'   by name without loading. The first call scans the system's fonts, which
#'   takes a few seconds.
#' @return A data frame with one row per font: `name`; `math` (it has a math
#'   table; `NA` for an installed family that is not loaded); `mono`
#'   (monospaced); `bold` and `italic` (a face of its own is registered, so
#'   `\textbf` and `\textit` draw its real design); `weight` of the regular
#'   face; `file` of the regular face; and `source`, one of `"bundled"`,
#'   `"file"` and `"system"`.
#' @seealso [load_font()], [available_math_fonts()]
#' @export
#'
#' @examples
#' available_fonts()
available_fonts <- function(system = FALSE) {
  .check_flag(system, "system")
  .ensure_bundled_fonts_registered()
  fonts <- .font_registry$fonts
  rows <- lapply(fonts, function(f) {
    differs <- function(face) {
      !is.null(f$faces[[face]]) &&
        !identical(f$faces[[face]], f$faces$plain)
    }
    data.frame(
      name = f$name, math = f$math, mono = f$mono,
      bold = differs("bold") || differs("bolditalic"),
      italic = differs("italic") || differs("bolditalic"),
      weight = f$weight, file = f$faces$plain$path, source = f$source,
      stringsAsFactors = FALSE
    )
  })
  out <- if (length(rows)) {
    do.call(rbind, unname(rows))
  } else {
    data.frame(name = character(), math = logical(), mono = logical(),
               bold = logical(), italic = logical(), weight = character(),
               file = character(), source = character())
  }
  if (isTRUE(system)) {
    out <- rbind(out, .system_font_rows(exclude = out$name))
  }
  rownames(out) <- NULL
  out
}

.check_string <- function(x, what) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", what, "` must be a single string.", call. = FALSE)
  }
  invisible(x)
}

.check_flag <- function(x, what) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", what, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(x)
}

# --- The registry ----------------------------------------------------------
#
# name -> one entry: list(name, display, source, math, mono, weight, faces).
# `display` is the family name the engine knows the file by (what
# available_math_fonts() lists); `faces` holds up to four of plain, bold,
# italic and bolditalic, each list(path, index). `generation` goes into the
# layout cache key (.parse_cache_key()): a layout measured before a font was
# loaded, or loaded again from another file, is not an answer after.
.font_registry <- new.env(parent = emptyenv())
.font_registry$fonts <- list()
.font_registry$generation <- 0L

.font_generation <- function() .font_registry$generation

# Add or replace one font. A name that differs only in case replaces the
# earlier one, as lookups ignore case.
.font_registry_add <- function(entry, bump = TRUE) {
  old <- match(tolower(entry$name), tolower(names(.font_registry$fonts)))
  if (!is.na(old)) .font_registry$fonts[[old]] <- NULL
  .font_registry$fonts[[entry$name]] <- entry
  # A family string this name was resolved from before is now stale.
  if (exists(entry$name, envir = .text_font_lookup, inherits = FALSE)) {
    rm(list = entry$name, envir = .text_font_lookup)
  }
  if (bump) .font_registry$generation <- .font_registry$generation + 1L
  invisible(entry$name)
}

# The registered name `x` stands for -- ignoring case, and through the
# "stix" / "lete" aliases -- or NULL.
.font_lookup <- function(x) {
  .ensure_bundled_fonts_registered()
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(NULL)
  key <- tolower(x)
  if (key %in% names(.font_aliases)) key <- tolower(.font_aliases[[key]])
  names_ <- names(.font_registry$fonts)
  hit <- match(key, tolower(names_))
  if (is.na(hit)) NULL else names_[hit]
}

# --- Where a font comes from -----------------------------------------------

.font_is_file <- function(x) {
  (file.exists(x) && !dir.exists(x)) ||
    grepl("\\.(otf|ttf|ttc)$", x, ignore.case = TRUE)
}

.font_file_face <- function(path) {
  if (!file.exists(path) || dir.exists(path)) {
    stop("Font file not found: ", path, call. = FALSE)
  }
  list(path = normalizePath(path, winslash = "/", mustWork = TRUE), index = 0L)
}

# The four faces of an installed family, or NULL when `family` is not one.
# match_fonts() answers every name -- with a fallback such as Arial for one
# it does not know -- so the file it returns must really be that family.
.system_font_faces <- function(family) {
  face <- function(m) list(path = m$path, index = as.integer(m$index))
  match <- function(...) systemfonts::match_fonts(family, ...)
  plain <- match()
  if (!nzchar(plain$path)) return(NULL)
  info <- tryCatch(systemfonts::font_info(path = plain$path, index = plain$index),
                   error = function(e) NULL)
  if (is.null(info) || !identical(tolower(info$family[1]), tolower(family))) {
    return(NULL)
  }
  list(
    family = info$family[1],
    faces = list(
      plain = face(plain),
      bold = face(match(weight = "bold")),
      italic = face(match(italic = TRUE)),
      bolditalic = face(match(italic = TRUE, weight = "bold"))
    )
  )
}

.font_file_info <- function(path, index = 0L) {
  info <- tryCatch(systemfonts::font_info(path = path, index = index),
                   error = function(e) NULL)
  list(
    mono = !is.null(info) && isTRUE(info$monospace[1]),
    weight = if (is.null(info)) NA_character_ else as.character(info$weight[1])
  )
}

# The installed families not in `exclude`, as rows of available_fonts().
.system_font_rows <- function(exclude) {
  sys <- systemfonts::system_fonts()
  sys <- sys[!tolower(sys$family) %in% tolower(exclude), , drop = FALSE]
  fams <- unique(sys$family)
  rows <- lapply(fams, function(f) {
    s <- sys[sys$family == f, , drop = FALSE]
    plain <- s[!s$italic & s$weight == "normal", , drop = FALSE]
    if (!nrow(plain)) plain <- s[1L, , drop = FALSE]
    data.frame(
      name = f, math = NA, mono = isTRUE(plain$monospace[1]),
      bold = any(s$weight >= "bold"), italic = any(s$italic),
      weight = as.character(plain$weight[1]), file = plain$path[1],
      source = "system", stringsAsFactors = FALSE
    )
  })
  if (length(rows)) do.call(rbind, rows) else NULL
}

# Tell systemfonts about a font so gp$fontfamily = <name> resolves to its
# files on ragg, svglite and the like. A face not given is the plain one.
.register_font_with_systemfonts <- function(name, faces, quiet = FALSE) {
  as_arg <- function(f) if (is.null(f)) NULL else list(f$path, f$index)
  args <- list(name = name, plain = as_arg(faces$plain))
  for (nm in c("bold", "italic", "bolditalic")) {
    if (!is.null(faces[[nm]])) args[[nm]] <- as_arg(faces[[nm]])
  }
  tryCatch(
    do.call(systemfonts::register_font, args),
    error = function(e) {
      # An installed family of that name keeps it (systemfonts will not
      # shadow one): the name then draws in the installed font.
      if (!quiet && !grepl("already exists", conditionMessage(e), fixed = TRUE)) {
        warning("Could not register '", name, "' with systemfonts: ",
                conditionMessage(e), call. = FALSE)
      }
    }
  )
  invisible()
}

# Session-level flag for the bundled-font systemfonts registration. Kept
# out of .onLoad so we don't touch Core Text at namespace-load time on
# macOS — older SDKs print "XType: Using static font registry." to stderr,
# which R CMD check captures and flags across many check phases. Running
# this lazily on first render keeps the check log clean.
.fonts_state <- new.env(parent = emptyenv())
.fonts_state$registered <- FALSE

.ensure_bundled_fonts_registered <- function() {
  if (isTRUE(.fonts_state$registered)) return(invisible())
  .fonts_state$registered <- TRUE  # set first so a failure isn't retried each call

  bundled <- list(
    list(file = "LeteSansMath.otf", name = "Lete Sans Math", alias = "lete"),
    list(file = "STIXTwoMath-Regular.otf", name = "STIX Two Math",
         alias = "stix")
  )
  for (b in bundled) {
    path <- system.file("fonts", b$file, package = "gridmicrotex")
    if (!nzchar(path)) next
    faces <- list(plain = list(path = path, index = 0L))
    # Best effort, and silent: an installed copy of the font keeps its name.
    for (nm in c(b$name, b$alias)) {
      .register_font_with_systemfonts(nm, faces, quiet = TRUE)
    }
    info <- .font_file_info(path)
    # Not a change to the registry as far as a cached layout can tell: the
    # engine had these fonts from the start.
    .font_registry_add(list(
      name = b$name, display = b$name, source = "bundled", math = TRUE,
      mono = info$mono, weight = info$weight, faces = faces
    ), bump = FALSE)
  }

  invisible()
}

#' Check math font status
#'
#' Prints which math fonts are available and whether the bundled font
#' files are present. Text fonts are not covered; use
#' `systemfonts::match_fonts()` to see what a font family resolves to.
#'
#' @return The names of the available math fonts, invisibly.
#' @seealso \code{\link{available_math_fonts}}, \code{\link{load_math_font}}
#' @export
#'
#' @examples
#' check_math_fonts()
check_math_fonts <- function() {
  if (!microtex_is_inited()) {
    message("MicroTeX is not initialized.")
    return(invisible(character(0)))
  }

  fonts <- microtex_math_font_names()
  message("MicroTeX version: ", microtex_version())
  message("Loaded math fonts (", length(fonts), "):")
  for (f in fonts) {
    message("  - ", f)
  }

  pkg <- "gridmicrotex"
  message("Bundled font files:")
  for (file in c("LeteSansMath.otf", "STIXTwoMath-Regular.otf")) {
    p <- system.file("fonts", file, package = pkg)
    message("  - ", file, ": ", if (nzchar(p)) "found" else "MISSING")
  }

  invisible(fonts)
}
