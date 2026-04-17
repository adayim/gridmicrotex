#' @keywords internal
#' @useDynLib gridmicrotex, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom grid makeContent widthDetails heightDetails xDetails yDetails editDetails ascentDetails descentDetails
"_PACKAGE"

# Column names used in ggplot2 aes() calls
utils::globalVariables(c(
  "x", "y", "label", "size", "colour", "hjust", "vjust", "angle", "alpha"
))
