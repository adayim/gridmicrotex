#' Deprecated functions
#'
#' These functions still work but will be removed in a future release.
#' Both were renamed to say what they actually operate on: only
#' \strong{math} fonts (those carrying an OpenType MATH table) are ever
#' registered with MicroTeX. Text fonts need no loading --- they are
#' resolved on demand by \pkg{systemfonts} from \code{gp$fontfamily}.
#'
#' \tabular{ll}{
#'   \strong{Deprecated}  \tab \strong{Use instead} \cr
#'   \code{load_font()}   \tab \code{\link{load_math_font}()}  \cr
#'   \code{check_fonts()} \tab \code{\link{check_math_fonts}()} \cr
#' }
#'
#' @param otf_path Path to the OTF/TTF font file.
#' @return As the replacement function.
#' @name gridmicrotex-deprecated
#' @keywords internal
NULL

#' @rdname gridmicrotex-deprecated
#' @export
load_font <- function(otf_path) {
  .Deprecated("load_math_font")
  load_math_font(otf_path)
}

#' @rdname gridmicrotex-deprecated
#' @export
check_fonts <- function() {
  .Deprecated("check_math_fonts")
  check_math_fonts()
}
