# The engine's own commands -- \dfrac, pmatrix, \R, ... -- are LaTeX files, not
# C++: inst/tex/prelude.tex is what every input starts with, and the other
# .sty files are packages it loads, or a document asks for with \usepackage.
# The engine reads no file; this hands it their text when the package loads.

# The text of a file, as UTF-8 with LF line ends.
.read_tex_file <- function(path) {
  enc2utf8(paste(readLines(path, encoding = "UTF-8", warn = FALSE),
                 collapse = "\n"))
}

# The packages that came with gridmicrotex, as against load_sty()'s.
.bundled <- new.env(parent = emptyenv())
.bundled$packages <- character(0)

.register_tex_files <- function(pkgname) {
  dir <- system.file("tex", package = pkgname)
  if (!nzchar(dir)) return(invisible(NULL))
  tex_prelude_set_cpp(.read_tex_file(file.path(dir, "prelude.tex")))
  for (path in list.files(dir, pattern = "[.]sty$", full.names = TRUE)) {
    name <- sub("[.]sty$", "", basename(path))
    tex_package_register_cpp(name, .read_tex_file(path))
    .bundled$packages <- c(.bundled$packages, name)
  }
  invisible(NULL)
}

# What went wrong reading the format and the packages every input starts
# with, one line each. Empty when all is well.
.tex_prelude_problems <- function() {
  tex_prelude_problems_cpp()
}

# --- the user's own packages ----------------------------------------------

# name -> list(path, stamp), for the files load_sty() read.
.sty_files <- new.env(parent = emptyenv())

.file_stamp <- function(path) {
  info <- file.info(path)
  paste(as.numeric(info$mtime), info$size)
}

#' Load a LaTeX package file
#'
#' `load_sty()` makes a file of your own definitions a package, so that
#' `\usepackage{name}` loads it in a label or a document, or
#' `latex_options(packages = "name")` loads it before every one.
#'
#' The file is read as LaTeX reads a package: `\newcommand`, `\renewcommand`,
#' `\def`, `\newenvironment`, `\DeclareMathOperator`, `\newtheorem` and the
#' like define commands, and `@` may be used in a name. Registers, boxes and
#' `\catcode` are not supported. A file edited after it was loaded is read
#' again the next time something is drawn.
#'
#' @param file Path of a `.sty` file.
#' @param name The name to load it by. By default the file's name without its
#'   extension. The name of a package that comes with gridmicrotex cannot
#'   be used.
#' @return The name, invisibly. Problems found in the file are given as a
#'   warning, with their line and column.
#' @seealso [latex_options()], [define_macro()]
#' @export
#'
#' @examples
#' sty <- tempfile(fileext = ".sty")
#' writeLines("\\newcommand{\\RR}{\\mathbb{R}}", sty)
#' name <- load_sty(sty, "mine")
#' grid::grid.newpage()
#' grid.latex("\\usepackage{mine}$f\\colon \\RR \\to \\RR$")
load_sty <- function(file, name = NULL) {
  if (!is.character(file) || length(file) != 1L || is.na(file) ||
      !file.exists(file)) {
    stop("`file` must be the path of an existing .sty file.", call. = FALSE)
  }
  name <- name %||% sub("[.][^.]*$", "", basename(file))
  if (!is.character(name) || length(name) != 1L || is.na(name) ||
      !grepl("^[A-Za-z][A-Za-z0-9_.-]*$", name)) {
    stop("`name` must be a package name: letters, digits, `-`, `_` and `.`, ",
         "starting with a letter.", call. = FALSE)
  }
  if (name %in% .bundled$packages) {
    stop("`", name, "` is the name of a package that comes with gridmicrotex; ",
         "give the file another name.", call. = FALSE)
  }
  .read_sty(file, name)
  invisible(name)
}

# Hand the file's text to the engine and say what is wrong with it.
.read_sty <- function(file, name) {
  path <- normalizePath(file, winslash = "/")
  tex_package_register_cpp(name, .read_tex_file(path))
  .sty_files[[name]] <- list(path = path, stamp = .file_stamp(path))
  problems <- tex_package_check_cpp(name)
  if (length(problems)) {
    where <- sub("^package [^ ]+ line ", "", problems)
    warning("package `", name, "` (", file, ") has problems:\n",
            paste0("  ", where, collapse = "\n"), call. = FALSE)
  }
  invisible(name)
}

# A file read by load_sty() that has changed since is read again, before the
# next parse. One that has gone stays as it was read.
.sty_refresh <- function() {
  for (name in ls(.sty_files, all.names = TRUE)) {
    entry <- .sty_files[[name]]
    if (!file.exists(entry$path)) next
    if (!identical(.file_stamp(entry$path), entry$stamp)) .read_sty(entry$path, name)
  }
  invisible(NULL)
}

# The package names a `packages` option stands for: a package we have, or a
# .sty file, which is loaded. The names are what is recorded, so a file is read
# once.
.resolve_packages_option <- function(x) {
  if (!is.character(x) || anyNA(x) || !all(nzchar(x))) {
    stop("`packages` must be package names or .sty files.", call. = FALSE)
  }
  vapply(x, function(one) {
    if (one %in% tex_package_names_cpp()) return(one)
    if (file.exists(one)) return(load_sty(one))
    stop("`packages`: there is no package `", one,
         "`. Load a file of your own with load_sty().", call. = FALSE)
  }, "", USE.NAMES = FALSE)
}
