.latex_macros <- new.env(parent = emptyenv())
.latex_macros$defs <- list()

#' Define a user-level LaTeX macro
#'
#' Registers a zero-argument shorthand that is expanded by text
#' substitution before the expression reaches the MicroTeX parser.
#' Useful for domain-specific notation (e.g. \code{\\RR} for
#' \code{\\mathbb\{R\}}) you reuse across many plots.
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
  .latex_macros$defs[[name]] <- definition
  invisible(NULL)
}

#' @rdname define_macro
#' @export
clear_macros <- function(name = NULL) {
  if (is.null(name)) {
    .latex_macros$defs <- list()
  } else {
    .latex_macros$defs[[name]] <- NULL
  }
  invisible(NULL)
}


#' @rdname define_macro
#' @export
list_macros <- function() {
  defs <- .latex_macros$defs
  if (length(defs) == 0L) return(character(0))
  unlist(defs)
}

# Apply macro expansion to a tex string. Matches \name where the next
# character is not a letter (so `\RR` matches but `\RRend` does not),
# in iteration until a fixed point so nested macros expand.
.expand_macros <- function(tex) {
  defs <- .latex_macros$defs
  if (length(defs) == 0L) return(tex)
  patterns <- paste0("\\\\", names(defs), "(?![A-Za-z])")
  for (depth in seq_len(8L)) {
    before <- tex
    for (i in seq_along(defs)) {
      # Escape backslashes in the replacement so gsub treats them
      # literally rather than as backreference escapes.
      replacement <- gsub("\\\\", "\\\\\\\\", defs[[i]])
      tex <- gsub(patterns[i], replacement, tex, perl = TRUE)
    }
    if (identical(tex, before)) break
  }
  # A defined macro token still present means expansion never resolved --
  # either a circular definition (\a -> \b -> \a) or one too deep for the
  # 8-iteration cap. Either way the output is wrong, so warn.
  if (any(vapply(patterns, grepl, logical(1), x = tex, perl = TRUE))) {
    warning(
      "Macro expansion did not resolve; check for circular macro ",
      "definitions.",
      call. = FALSE
    )
  }
  tex
}
