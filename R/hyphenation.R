# Hyphenation patterns.
#
# The patterns are Liang's, in the format TeX itself loads: a
# `\patterns{...}` block of tokens like `hy3ph`, optionally followed by a
# `\hyphenation{...}` block of explicitly hyphenated words. The matching
# and the trie live in C++ (src/hyphenator.cpp); this file only reads the
# file and hands the tokens over.

# The bundled set: American English, from hyph-utf8. Its own header gives
# the hyphenmins this uses.
.HYPH_LEFT_MIN <- 2L
.HYPH_RIGHT_MIN <- 3L

# Pull the contents of one `\name{ ... }` block. The pattern files put the
# block on its own lines, and `%` comments run to end of line as in TeX.
.hyph_block <- function(lines, name) {
  open <- grep(paste0("^\\s*\\\\", name, "\\{"), lines)
  if (!length(open)) return(character(0))
  from <- open[1]
  # The block ends at the first line whose only content is the closing
  # brace, which is how every hyph-utf8 file is laid out.
  close <- grep("^\\s*\\}\\s*$", lines)
  close <- close[close > from]
  if (!length(close)) return(character(0))
  body <- lines[seq(from, close[1])]
  # Drop the delimiters themselves, then comments.
  body[1] <- sub(paste0("^\\s*\\\\", name, "\\{"), "", body[1])
  body[length(body)] <- sub("\\}\\s*$", "", body[length(body)])
  body <- sub("%.*$", "", body)
  toks <- unlist(strsplit(body, "[[:space:]]+"), use.names = FALSE)
  toks[nzchar(toks)]
}

# Read a `hyph-*.tex` file into the pieces the C++ side wants.
.hyph_read <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  list(patterns = .hyph_block(lines, "patterns"),
       exceptions = .hyph_block(lines, "hyphenation"))
}

# Register the bundled patterns, once, on first use.
#
# Deliberately not done in .onLoad: work there is what produced the CRAN
# oldrel-macos WARN for the font registration, and most sessions never
# hyphenate anything.
.ensure_hyphenation <- function() {
  if (microtex_has_hyphenation()) return(invisible(TRUE))
  path <- system.file("hyph", "hyph-en-us.tex", package = "gridmicrotex")
  if (!nzchar(path) || !file.exists(path)) {
    warning("Hyphenation patterns are missing from the installation; ",
            "text will be wrapped without hyphenation.", call. = FALSE)
    return(invisible(FALSE))
  }
  p <- .hyph_read(path)
  if (!length(p$patterns)) {
    warning("No hyphenation patterns could be read from ", path, ".",
            call. = FALSE)
    return(invisible(FALSE))
  }
  microtex_register_hyphenation(p$patterns, p$exceptions,
                                .HYPH_LEFT_MIN, .HYPH_RIGHT_MIN)
  invisible(TRUE)
}

# Validate the `hyphenate` argument. Mirrors .check_justify().
.check_hyphenate <- function(hyphenate) {
  if (!is.logical(hyphenate) || length(hyphenate) != 1L || is.na(hyphenate)) {
    stop("`hyphenate` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}
