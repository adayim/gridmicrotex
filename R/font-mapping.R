#' Resolve TeX font name to R font family
#'
#' @param tex_font_name TeX font name string.
#' @return R font family string.
#' @keywords internal
resolve_font_family <- function(tex_font_name) {
  # In path rendering mode, glyphs are rendered as vector paths

  # so this mapping is only needed for typeface (text) rendering.
  # For now, return a sensible default.
  map <- list(
    "cmr"   = "serif",
    "cmmi"  = "serif",
    "cmsy"  = "serif",
    "cmex"  = "serif",
    "lmmath" = "serif"
  )
  match <- map[[tex_font_name]]
  if (!is.null(match)) match else "serif"
}

#' Resolve TeX font style flags to R font face
#'
#' @param font_style Integer style flags from MicroTeX.
#' @return R font face string (plain, bold, italic, bold.italic).
#' @keywords internal
resolve_font_face <- function(font_style) {
  # MicroTeX FontStyle: 0 = plain, 1 = bold, 2 = italic, 3 = bold+italic
  switch(as.character(font_style),
    "0" = "plain",
    "1" = "bold",
    "2" = "italic",
    "3" = "bold.italic",
    "plain"
  )
}
