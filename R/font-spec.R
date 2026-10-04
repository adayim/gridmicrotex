# The fonts a document names: \setmainfont, \setsansfont, \setmonofont,
# \setmathfont, \fontspec and \fontfamily, in a label or a whole file.
#
# The front end (src/MicroTeX/lib/front/) reads the commands and asks R what
# each font is called, through the callback .font_resolver() installs for the
# length of a parse (.parse_latex_cached(), beside the image resolver). R finds
# it -- registered, a file, installed, or one of LaTeX's own family codes --
# loads it with load_font() and answers with the family name text is measured
# and drawn in. "" means there is no such font, and the front end warns at the
# command's line:col and keeps the default.

.font_resolver <- function() {
  function(name, options, role) {
    tryCatch(
      .resolve_font_spec(name, options, role) %||% "",
      error = function(e) {
        warning("Font '", name, "': ", conditionMessage(e), call. = FALSE)
        ""
      }
    )
  }
}

# The name font `name` goes by, or NULL when it is none. `options` is a named
# character vector of fontspec's options the engine reads; `role` is one of
# "main", "sans", "mono", "math", "font" and "nfss" (\fontfamily's argument,
# which may be a family code of LaTeX's).
#
# For "math" the answer is the engine's name for a loaded math font; for the
# others the name the font is registered under.
.resolve_font_spec <- function(name, options = character(), role = "font") {
  name <- trimws(name)
  if (!nzchar(name)) return(NULL)
  # R's own families, which markdown's CSS generics come to.
  if (!identical(role, "math") && name %in% c("sans", "serif", "mono")) return(name)
  found <- .font_spec_find(name, options, role)
  if (is.null(found)) return(NULL)
  entry <- .font_registry$fonts[[found]]
  if (identical(role, "math")) {
    if (is.null(entry) || !isTRUE(entry$math)) return(NULL)
    return(entry$display)
  }
  found
}

.font_spec_find <- function(name, options, role) {
  files <- .font_spec_files(name, options)
  if (!is.null(files)) {
    return(do.call(load_font, c(list(files$plain, name = name),
                                files[setdiff(names(files), "plain")])))
  }
  if (identical(role, "nfss")) {
    code <- .nfss_font(name)
    if (!is.null(code)) return(if (nzchar(code)) code else NULL)
  }
  registered <- .font_lookup(name)
  if (!is.null(registered)) return(registered)
  if (.font_is_file(name)) return(load_font(name))
  if (!is.null(.system_font_faces(name))) return(load_font(name))
  NULL
}

# fontspec's file options: the files of the faces, `*` standing for the name,
# under Path and with Extension added. NULL when the options name no file.
.font_spec_files <- function(name, options) {
  faces <- c(plain = "UprightFont", bold = "BoldFont", italic = "ItalicFont",
             bolditalic = "BoldItalicFont")
  if (!any(c("Path", "Extension", faces) %in% names(options))) return(NULL)
  # `[[` on a name that is not there is an error for a character vector.
  option <- function(key) if (key %in% names(options)) options[[key]] else NULL
  path <- option("Path") %||% ""
  if (nzchar(path) && !grepl("[/\\\\]$", path)) path <- paste0(path, "/")
  ext <- option("Extension") %||% ""
  if (nzchar(ext) && !startsWith(ext, ".")) ext <- paste0(".", ext)
  file_of <- function(spec) {
    f <- paste0(path, gsub("*", name, spec, fixed = TRUE))
    if (nzchar(ext) && !grepl("\\.[A-Za-z0-9]+$", f)) f <- paste0(f, ext)
    f
  }
  out <- list(plain = file_of(option("UprightFont") %||% "*"))
  for (face in setdiff(names(faces), "plain")) {
    if (!is.null(option(faces[[face]]))) out[[face]] <- file_of(option(faces[[face]]))
  }
  out
}

# --- LaTeX's own family codes -----------------------------------------------
#
# \fontfamily{ppl}\selectfont: the code names a typeface, not a file. Each is
# looked for as an installed family that is that typeface or its standard
# clone, then, if a TeX distribution is installed, as its OpenType files; the
# answer, a miss included, is kept for the session. Nothing runs until a code
# is used: kpsewhich takes about a second, and system_fonts() seconds more.
#
# `families` are the installed candidates, best first; `files` the OpenType
# files kpsewhich can find, regular first, then bold, italic, bold italic.
.nfss_table <- list(
  cmr = list(families = c("Latin Modern Roman", "LM Roman 10", "CMU Serif"),
             files = c("lmroman10-regular.otf", "lmroman10-bold.otf",
                       "lmroman10-italic.otf", "lmroman10-bolditalic.otf")),
  cmss = list(families = c("Latin Modern Sans", "LM Sans 10", "CMU Sans Serif"),
              files = c("lmsans10-regular.otf", "lmsans10-bold.otf",
                        "lmsans10-oblique.otf", "lmsans10-boldoblique.otf")),
  cmtt = list(families = c("Latin Modern Mono", "LM Mono 10", "CMU Typewriter Text"),
              files = c("lmmono10-regular.otf", "lmmonolt10-bold.otf",
                        "lmmono10-italic.otf", "lmmonolt10-boldoblique.otf")),
  ptm = list(families = c("TeX Gyre Termes", "Times New Roman", "Times",
                          "Nimbus Roman", "Nimbus Roman No9 L", "Liberation Serif"),
             files = c("texgyretermes-regular.otf", "texgyretermes-bold.otf",
                       "texgyretermes-italic.otf", "texgyretermes-bolditalic.otf")),
  phv = list(families = c("TeX Gyre Heros", "Helvetica", "Arial", "Nimbus Sans",
                          "Nimbus Sans L", "Liberation Sans"),
             files = c("texgyreheros-regular.otf", "texgyreheros-bold.otf",
                       "texgyreheros-italic.otf", "texgyreheros-bolditalic.otf")),
  pcr = list(families = c("TeX Gyre Cursor", "Courier New", "Courier",
                          "Nimbus Mono", "Nimbus Mono PS", "Liberation Mono"),
             files = c("texgyrecursor-regular.otf", "texgyrecursor-bold.otf",
                       "texgyrecursor-italic.otf", "texgyrecursor-bolditalic.otf")),
  ppl = list(families = c("TeX Gyre Pagella", "Palatino Linotype", "Palatino",
                          "URW Palladio L", "P052"),
             files = c("texgyrepagella-regular.otf", "texgyrepagella-bold.otf",
                       "texgyrepagella-italic.otf", "texgyrepagella-bolditalic.otf")),
  pag = list(families = c("TeX Gyre Adventor", "ITC Avant Garde Gothic",
                          "Avant Garde", "URW Gothic", "URW Gothic L"),
             files = c("texgyreadventor-regular.otf", "texgyreadventor-bold.otf",
                       "texgyreadventor-italic.otf", "texgyreadventor-bolditalic.otf")),
  pbk = list(families = c("TeX Gyre Bonum", "ITC Bookman", "Bookman Old Style",
                          "Bookman", "URW Bookman", "URW Bookman L"),
             files = c("texgyrebonum-regular.otf", "texgyrebonum-bold.otf",
                       "texgyrebonum-italic.otf", "texgyrebonum-bolditalic.otf")),
  pnc = list(families = c("TeX Gyre Schola", "New Century Schoolbook",
                          "Century Schoolbook", "C059"),
             files = c("texgyreschola-regular.otf", "texgyreschola-bold.otf",
                       "texgyreschola-italic.otf", "texgyreschola-bolditalic.otf")),
  put = list(families = c("Utopia", "Adobe Utopia"), files = character()),
  bch = list(families = c("Charter", "Bitstream Charter", "XCharter", "Charis SIL"),
             files = c("XCharter-Roman.otf", "XCharter-Bold.otf",
                       "XCharter-Italic.otf", "XCharter-BoldItalic.otf"))
)
.nfss_table$lmr <- .nfss_table$cmr
.nfss_table$lmss <- .nfss_table$cmss
.nfss_table$lmtt <- .nfss_table$cmtt

.nfss_cache <- new.env(parent = emptyenv())

# NULL when `code` is not one of LaTeX's family codes (so it is read as a font
# name); else the name the typeface is loaded under, or "" when this machine
# has neither an installed family nor TeX files for it.
.nfss_font <- function(code) {
  spec <- .nfss_table[[code]]
  if (is.null(spec)) return(NULL)
  if (!is.null(.nfss_cache[[code]])) return(.nfss_cache[[code]])
  found <- .nfss_installed(spec$families) %||% .nfss_from_tex(spec$files) %||% ""
  .nfss_cache[[code]] <- found
  found
}

.nfss_installed <- function(families) {
  for (family in families) {
    if (!is.null(.font_lookup(family))) return(.font_lookup(family))
    if (!is.null(.system_font_faces(family))) return(load_font(family))
  }
  NULL
}

.nfss_from_tex <- function(files) {
  kpse <- Sys.which("kpsewhich")
  if (!nzchar(kpse) || !length(files)) return(NULL)
  # One call for all of them: it names the files it finds, in the order asked.
  paths <- tryCatch(
    suppressWarnings(system2(kpse, files, stdout = TRUE, stderr = FALSE)),
    error = function(e) character()
  )
  paths <- paths[nzchar(paths) & file.exists(paths)]
  if (!length(paths)) return(NULL)
  path_of <- function(file) {
    hit <- paths[tolower(basename(paths)) == tolower(file)]
    if (length(hit)) hit[[1L]] else NULL
  }
  plain <- path_of(files[[1L]])
  if (is.null(plain)) return(NULL)
  load_font(plain, bold = path_of(files[[2L]]), italic = path_of(files[[3L]]),
            bolditalic = path_of(files[[4L]]))
}
