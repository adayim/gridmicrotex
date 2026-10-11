# The engine's own commands -- \dfrac, pmatrix, \R, ... -- are LaTeX files, not
# C++: inst/tex/prelude.tex is what every input starts with, and the other
# .sty files are packages it loads, or a document asks for with \usepackage.
# The engine reads no file; this hands it their text when the package loads.

# The text of a file, as UTF-8 with LF line ends.
.read_tex_file <- function(path) {
  enc2utf8(paste(readLines(path, encoding = "UTF-8", warn = FALSE),
                 collapse = "\n"))
}

.register_tex_files <- function(pkgname) {
  dir <- system.file("tex", package = pkgname)
  if (!nzchar(dir)) return(invisible(NULL))
  tex_prelude_set_cpp(.read_tex_file(file.path(dir, "prelude.tex")))
  for (path in list.files(dir, pattern = "[.]sty$", full.names = TRUE)) {
    name <- sub("[.]sty$", "", basename(path))
    tex_package_register_cpp(name, .read_tex_file(path))
  }
  invisible(NULL)
}

# What went wrong reading the format and the packages every input starts
# with, one line each. Empty when all is well.
.tex_prelude_problems <- function() {
  tex_prelude_problems_cpp()
}
