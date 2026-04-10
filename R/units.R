#' Convert TeX points to big points
#'
#' TeX points (1/72.27 inch) differ from PostScript/big points (1/72 inch).
#' R's grid "bigpts" unit uses PostScript points.
#'
#' @param tex_pt Numeric value in TeX points.
#' @return Numeric value in big (PostScript) points.
#' @keywords internal
tex_pt_to_bigpt <- function(tex_pt) {
  tex_pt * (72 / 72.27)
}

#' Convert big points to TeX points
#'
#' @param big_pt Numeric value in PostScript points.
#' @return Numeric value in TeX points.
#' @keywords internal
bigpt_to_tex_pt <- function(big_pt) {
  big_pt * (72.27 / 72)
}
