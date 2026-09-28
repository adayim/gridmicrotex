#' Define a LaTeX shorthand for every label
#'
#' `define_macro()` adds a macro without arguments, such as `\RR` for
#' `\mathbb{R}`, that every later label can use. `list_macros()` shows
#' them and `clear_macros()` removes them.
#'
#' A label can also define its own macros with `\newcommand`, `\def` and
#' the like, including macros with arguments, but those last for that
#' label only.
#'
#' @param name Macro name, without the backslash. For `clear_macros()`,
#'   `NULL` (default) removes all macros.
#' @param definition The LaTeX the macro stands for.
#' @return `list_macros()` returns a named character vector of macros and
#'   their definitions. The others return `NULL`, invisibly.
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
