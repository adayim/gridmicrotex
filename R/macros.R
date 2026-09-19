#' Define a user-level LaTeX macro
#'
#' Registers a zero-argument shorthand that every later expression can use,
#' expanded by the parser as a \code{\\newcommand} without arguments would
#' be. Useful for domain-specific notation (e.g. \code{\\RR} for
#' \code{\\mathbb\{R\}}) you reuse across many plots.
#'
#' @section Choosing between this and \code{\\newcommand}:
#' MicroTeX also accepts \code{\\newcommand} and plain-TeX \code{\\def}
#' written inside the expression itself, and those are the more capable
#' form: they take up to nine arguments, which \code{define_macro()} does
#' not.
#'
#' \preformatted{
#'   # parameterised, but local to this one expression
#'   grid.latex(r"(\\def\\norm#1{\\left\\lVert #1 \\right\\rVert}
#'                 \\norm{\\vec{v}})")
#' }
#'
#' What they cannot do is persist: a \code{\\newcommand} written in one call
#' is gone by the next. That is the one thing \code{define_macro()} is for.
#' Use \code{\\newcommand} / \code{\\def} for an abbreviation local to a
#' single label, and \code{define_macro()} for notation you want available
#' to every label in a script. A \code{\\renewcommand} in one label
#' overrides a \code{define_macro()} macro for that label only.
#'
#' @param name Macro name \strong{without} the leading backslash. For
#'   \code{clear_macros}, the macro name to drop, or \code{NULL}
#'   (default) to clear all.
#' @param definition LaTeX source the macro expands to.
#' @return
#' \itemize{
#'   \item \code{define_macro}: Invisibly returns \code{NULL}.
#'   \item \code{clear_macros}: Invisibly returns \code{NULL}.
#'   \item \code{list_macros}: A named character vector mapping
#'     macro names to their expansions. Empty if no macros are defined.
#' }
#' @seealso \code{\link{latex_grob}}, \code{\link{latex_options}}
#' @export
#'
#' @examples
#' \donttest{
#'   define_macro("RR", "\\mathbb{R}")
#'   define_macro("eps", "\\varepsilon")
#'   grid::grid.newpage()
#'   grid.latex("\\forall \\eps > 0, \\eps \\in \\RR")
#'   clear_macros()
#' }
define_macro <- function(name, definition) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))
  stopifnot(is.character(definition), length(definition) == 1L)
  if (grepl("[^A-Za-z]", name)) {
    stop("Macro name must contain only letters (ASCII a-z, A-Z).",
         call. = FALSE)
  }
  persistent_macro_set_cpp(name, enc2utf8(definition))
  invisible(NULL)
}

#' @rdname define_macro
#' @export
clear_macros <- function(name = NULL) {
  if (is.null(name)) {
    persistent_macro_clear_cpp()
  } else {
    # A number would index the definitions by position and drop whichever
    # macro happens to come first.
    stopifnot(is.character(name), length(name) == 1L, !is.na(name),
              nzchar(name))
    persistent_macro_remove_cpp(name)
  }
  invisible(NULL)
}


#' @rdname define_macro
#' @export
list_macros <- function() {
  defs <- persistent_macro_list_cpp()
  if (length(defs) == 0L) return(character(0))
  defs
}
