.latex_cache <- new.env(parent = emptyenv())
.latex_cache$entries <- list()
.latex_cache$order <- character(0)
.latex_cache$max_size <- 512L
.latex_cache$hits <- 0L
.latex_cache$misses <- 0L

# `valid`: a check an entry must still pass, or it is a miss -- a layout
# whose image file has changed since.
.cache_get <- function(key, valid = NULL) {
  if (!nzchar(key) || is.null(.latex_cache$entries[[key]]) ||
      (!is.null(valid) && !valid(.latex_cache$entries[[key]]))) {
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
#' evicted. The same limit, and \code{latex_cache_clear()}, also apply to
#' a smaller memo of pre-processed input kept alongside the layouts.
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
#'   grid.latex("$e^{i\\pi} + 1 = 0$")
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
  # Decoded rasters and parsed pictures are held per file, and each one is
  # far larger than a layout. They have no size limit of their own, so this
  # is the only way to give that memory back.
  .image_cache_clear()
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
# The resolution is part of it too: one png() measured a phrase at 175bp
# at 72dpi and 174bp at 300, so the name alone let a layout measured at one
# resolution be reused at another.
#
# With no device open the measurer opens a pdf(NULL) of its own, so that
# is the device the measurements will come from.
.cache_device <- function() {
  if (grDevices::dev.cur() == 1L) return("pdf@72")
  dpi <- grDevices::dev.size("px")[1] / grDevices::dev.size("in")[1]
  sprintf("%s@%.0f", names(grDevices::dev.cur()), dpi)
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
                             device = "", input_mode = "math") {
  paste(
    tex, "|", text_size, "|", line_space, "|", fg_color, "|",
    max_width, "|", math_font, "|", main_font, "|", text_family, "|",
    as.integer(use_path), "|", tex_style, "|", as.integer(justify),
    "|", as.integer(optimal_break), "|", input_mode,
    "|", device,
    # define_macro() macros are expanded in C++, so a layout depends on them
    # without `tex` showing it.
    "|", persistent_macro_generation_cpp(),
    sep = ""
  )
}

# Cached wrapper around parse_latex_cpp. Same signature plus `text_family`
# (key-only, not forwarded to C++), transparent cache.
.parse_latex_cached <- function(tex, text_size, line_space, fg_color,
                                max_width, math_font, main_font, use_path,
                                tex_style = "", text_family = "",
                                justify = FALSE, optimal_break = FALSE,
                                input_mode = "math") {
  key <- .parse_cache_key(tex, text_size, line_space, fg_color, max_width,
                          math_font, main_font, text_family, use_path,
                          tex_style, justify, optimal_break,
                          device = .cache_device(), input_mode = input_mode)
  hit <- .cache_get(key, valid = .images_current)
  if (!is.null(hit)) return(hit)
  # Measuring needs a device. With none open, one pdf(NULL) serves the
  # whole parse, where the measurer would open and close its own for every
  # word it measured; a cache hit needs none at all. The key above names
  # it "pdf@72" (.cache_device()).
  if (grDevices::dev.cur() == 1L) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  # The parser asks R for each image as it meets one (.image_resolver()),
  # and the files it read ride along with the layout: the key is the
  # source, which says nothing of an image edited since.
  used <- new.env(parent = emptyenv())
  register_image_resolver(.image_resolver(text_size, max_width, used))
  on.exit(clear_image_resolver(), add = TRUE)
  parse <- function() parse_latex_cpp(
    tex = tex, text_size = text_size, line_space = line_space,
    fg_color = fg_color, max_width = max_width, math_font = math_font,
    main_font = main_font, use_path = use_path, tex_style = tex_style,
    justify = justify, optimal_break = optimal_break, input_mode = input_mode
  )
  # One figure that cannot be drawn -- a PDF, say, as most papers' are --
  # must not cost a whole document: it warns and draws the file's name.
  # A label, where the figure is the point, still stops.
  layout <- if (identical(input_mode, "document")) .images_lenient(parse()) else parse()
  if (length(used$stamps)) attr(layout, "images") <- used$stamps
  .cache_put(key, layout)
  layout
}
