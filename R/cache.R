.latex_cache <- new.env(parent = emptyenv())
.latex_cache$entries <- list()
.latex_cache$order <- character(0)
.latex_cache$max_size <- 512L
.latex_cache$hits <- 0L
.latex_cache$misses <- 0L

.cache_get <- function(key) {
  if (!nzchar(key) || is.null(.latex_cache$entries[[key]])) {
    .latex_cache$misses <- .latex_cache$misses + 1L
    return(NULL)
  }
  .latex_cache$hits <- .latex_cache$hits + 1L
  # touch: move key to the most-recent end
  .latex_cache$order <- c(setdiff(.latex_cache$order, key), key)
  .latex_cache$entries[[key]]
}

.cache_put <- function(key, value) {
  if (!nzchar(key) || .latex_cache$max_size <= 0L) return(invisible(NULL))
  .latex_cache$entries[[key]] <- value
  .latex_cache$order <- c(setdiff(.latex_cache$order, key), key)
  excess <- length(.latex_cache$order) - .latex_cache$max_size
  if (excess > 0L) {
    drop <- .latex_cache$order[seq_len(excess)]
    for (k in drop) .latex_cache$entries[[k]] <- NULL
    .latex_cache$order <- .latex_cache$order[-seq_len(excess)]
  }
  invisible(NULL)
}


#' Set the maximum number of entries kept in the LaTeX layout cache
#'
#' The cache stores parsed layout information for recently rendered
#' LaTeX expressions, keyed by the expression and relevant rendering
#' parameters (font, size, macros, etc.). This speeds up repeated
#' rendering of the same expressions, especially in loops or
#' interactive sessions. The default limit is 512 entries, which
#' should be sufficient for most use cases. When the limit is
#' exceeded, the least recently used entries are automatically
#' evicted.
#'
#' @param n Non-negative integer cache capacity. Default is 512. Set
#'   to \code{0} to disable caching.
#' @return
#' \itemize{
#'   \item \code{latex_cache_limit}: Invisibly returns the previous limit.
#'   \item \code{latex_cache_clear}: Invisibly returns \code{NULL}.
#'   \item \code{latex_cache_info}: A list with elements \code{size}
#'     (entries currently stored), \code{max_size}, \code{hits}, and
#'     \code{misses}.
#' }
#' @seealso \code{\link{latex_grob}}, \code{\link{latex_options}}
#' @export
#'
#' @examples
#' \donttest{
#'   latex_cache_limit(256)
#'   grid.latex("e^{i\\pi} + 1 = 0")
#'   latex_cache_info()
#'   latex_cache_clear()
#' }
latex_cache_limit <- function(n = 512L) {
  n <- as.integer(n)
  if (is.na(n) || n < 0L) {
    stop("n must be a non-negative integer.", call. = FALSE)
  }
  old <- .latex_cache$max_size
  .latex_cache$max_size <- n
  if (n == 0L) latex_cache_clear()
  excess <- length(.latex_cache$order) - n
  if (excess > 0L) {
    drop <- .latex_cache$order[seq_len(excess)]
    for (k in drop) .latex_cache$entries[[k]] <- NULL
    .latex_cache$order <- .latex_cache$order[-seq_len(excess)]
  }
  invisible(old)
}

#' @rdname latex_cache_limit
#' @export
latex_cache_clear <- function() {
  .latex_cache$entries <- list()
  .latex_cache$order <- character(0)
  .latex_cache$hits <- 0L
  .latex_cache$misses <- 0L
  invisible(NULL)
}


#' @rdname latex_cache_limit
#' @export
latex_cache_info <- function() {
  list(
    size = length(.latex_cache$order),
    max_size = .latex_cache$max_size,
    hits = .latex_cache$hits,
    misses = .latex_cache$misses
  )
}

# The graphics device the layout was measured on. `\text{}` runs are sized
# by the R text-measurer callback, which measures through grid on the
# current device -- and devices disagree: the same phrase came out 99bp on
# pdf() and 98bp on ragg, and much further apart when one device cannot
# resolve the requested family and silently substitutes another. Without
# this in the key, a layout measured on the screen device was reused
# verbatim by a later ggsave(), placing text using the wrong widths.
#
# With no device open the measurer opens a pdf(NULL) of its own, so that
# is the device the measurements will come from.
.cache_device <- function() {
  if (grDevices::dev.cur() == 1L) "pdf" else names(grDevices::dev.cur())
}

# Cache key for a parse_latex_cpp call. Concatenation is fine because the
# tex string is included and the remaining inputs are short numerics/strings.
# `text_family` is the *requested* gp$fontfamily, not the resolved
# main_font: two families that both fail MicroTeX resolution (main_font ==
# "") are still measured with the requested family by the R text-measurer
# callback, so they can produce different layouts and must not share a key.
.parse_cache_key <- function(tex, text_size, line_space, fg_color, max_width,
                             math_font, main_font, text_family, use_path,
                             tex_style, justify, optimal_break,
                             device = "") {
  paste(
    tex, "|", text_size, "|", line_space, "|", fg_color, "|",
    max_width, "|", math_font, "|", main_font, "|", text_family, "|",
    as.integer(use_path), "|", tex_style, "|", as.integer(justify),
    "|", as.integer(optimal_break),
    "|", device,
    sep = ""
  )
}

# Cached wrapper around parse_latex_cpp. Same signature plus `text_family`
# (key-only, not forwarded to C++), transparent cache.
.parse_latex_cached <- function(tex, text_size, line_space, fg_color,
                                max_width, math_font, main_font, use_path,
                                tex_style = "", text_family = "",
                                justify = FALSE, optimal_break = FALSE) {
  key <- .parse_cache_key(tex, text_size, line_space, fg_color, max_width,
                          math_font, main_font, text_family, use_path,
                          tex_style, justify, optimal_break,
                          device = .cache_device())
  hit <- .cache_get(key)
  if (!is.null(hit)) return(hit)
  layout <- parse_latex_cpp(
    tex = tex, text_size = text_size, line_space = line_space,
    fg_color = fg_color, max_width = max_width, math_font = math_font,
    main_font = main_font, use_path = use_path, tex_style = tex_style,
    justify = justify, optimal_break = optimal_break
  )
  .cache_put(key, layout)
  layout
}
