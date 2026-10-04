# Null-coalescing operator. Defined here so it's available to every R
# file in the package (R loads files alphabetically by default, and
# `o` sorts ahead of the files that use it).
`%||%` <- function(x, y) if (is.null(x)) y else x

.latex_options <- new.env(parent = emptyenv())
.latex_options$values <- list(
  math_font   = NULL,
  render_mode = NULL,
  tex_style   = NULL,
  input_mode  = NULL,
  justify     = NULL,
  line_break  = NULL,
  markdown_style = NULL,
  device_math   = NULL,
  main_font   = NULL,
  sans_font   = NULL,
  mono_font   = NULL
)

# Validate the `justify` argument. Kept here so latex_grob(),
# latex_dims() and latex_options() all reject the same things.
.check_justify <- function(justify) {
  if (!is.logical(justify) || length(justify) != 1L || is.na(justify)) {
    stop("`justify` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Set or show default rendering options
#'
#' Sets defaults for [latex_grob()], [grid.latex()], [latex_dims()],
#' [latex_tree()] and the markdown functions. An argument given in a call
#' always wins over the default.
#'
#' With no arguments, returns the current settings (`NULL` means the
#' built-in default). Setting an option to `NULL` resets it. The previous
#' settings are returned, so `do.call(latex_options, old)` restores them.
#'
#' Size and line spacing are set with `gp` (`fontsize`, `cex`,
#' `lineheight`), not here.
#'
#' @param math_font Math font: a loaded font with a math table; see
#'   [available_fonts()].
#' @param render_mode `"typeface"` or `"path"`; see [latex_grob()].
#' @param tex_style `""`, `"display"`, `"text"`, `"script"` or
#'   `"scriptscript"`; see [latex_grob()].
#' @param input_mode `"mixed"`, `"math"` or `"document"`; see
#'   [latex_grob()].
#' @param justify If `TRUE`, stretch wrapped lines to fill `max_width`.
#' @param line_break `"greedy"` or `"optimal"`; see [latex_grob()].
#' @param markdown_style Default style for [markdown_grob()] and
#'   [markdown_box_grob()]: a [markdown_style()], CSS text, or a path to
#'   a `.css` file.
#' @param device_math If `TRUE`, any plot label containing math, such as
#'   `"Slope $x^2$"`, is typeset as LaTeX. This works for base
#'   graphics (`main`, `xlab`, `text()`, `legend()`, ...), and also for
#'   lattice, grid and ggplot2. Labels without math, such as
#'   `"Cost $5-$10"`, are left alone.
#'
#'   Heights are not adjusted: a tall formula can overflow a margin or a
#'   `legend()` box. Measure it with [latex_dims()] and make room with
#'   `par(mar = )`. See `vignette("base-graphics")` for the rules and
#'   limitations.
#' @param main_font,sans_font,mono_font The fonts for text: the body, `\textsf`
#'   and `\sffamily`, and `\texttt`, `\ttfamily` and `\verb`. Each is the
#'   name of a font given to [load_font()], an installed family, or a font
#'   file, which is loaded. `NULL` keeps the defaults: `gp$fontfamily` (or
#'   `"sans"`) for the body, `"sans"` and `"mono"`. A `gp$fontfamily` in a
#'   call wins over `main_font`.
#' @return The previous settings, invisibly. With no arguments, the
#'   current settings.
#' @seealso [available_fonts()], [load_font()], [latex_grob()]
#' @export
#'
#' @examples
#' \donttest{
#'   old <- latex_options(math_font = "stix", render_mode = "typeface")
#'   grid.latex("$\\sum_{i=1}^{n} i^{2}$", gp = grid::gpar(fontsize = 14))
#'   do.call(latex_options, old)
#'
#'   # Math in base graphics, with no other change to the plotting code.
#'   latex_options(device_math = TRUE)
#'   plot(1:10, (1:10)^2,
#'        main = "Slope $\\hat{\\beta}_1 = \\sum_{i=1}^{n} x_i^2$",
#'        ylab = "$y^2$")
#' }
#' \dontshow{
#' # pkgdown saves example figures only after all the example code has run,
#' # so the option must stay on there or the plot shows literal $...$.
#' # Everywhere else, including R CMD check, switch it off again.
#' if (!identical(Sys.getenv("IN_PKGDOWN"), "true"))
#'   latex_options(device_math = FALSE)
#' }
latex_options <- function(math_font = NULL, render_mode = NULL,
                          tex_style = NULL, input_mode = NULL,
                          justify = NULL, line_break = NULL,
                          markdown_style = NULL, device_math = NULL,
                          main_font = NULL, sans_font = NULL,
                          mono_font = NULL) {
  if (nargs() == 0L) {
    return(as.list(.latex_options$values))
  }

  old <- as.list(.latex_options$values)
  # An argument passed as NULL resets its option; one not passed at all is
  # left alone. Telling the two apart is what lets either list this
  # function returns be handed back to restore the settings it describes,
  # unset ones included -- options() works the same way. Treating NULL as
  # "leave alone" made that restore nothing.
  # Every value is checked before any is applied: a call that fails changes
  # nothing, so the settings it was given do not half-apply.
  given <- list()
  take <- function(name, value) given[name] <<- list(value)

  if (!missing(math_font)) {
    if (!is.null(math_font)) {
      stopifnot(is.character(math_font), length(math_font) == 1L)
    }
    take("math_font", math_font)
  }
  if (!missing(render_mode)) {
    if (!is.null(render_mode)) {
      render_mode <- match.arg(render_mode, c("typeface", "path"))
    }
    take("render_mode", render_mode)
  }
  if (!missing(tex_style)) {
    if (!is.null(tex_style)) .check_tex_style(tex_style)
    take("tex_style", tex_style)
  }
  if (!missing(input_mode)) {
    if (!is.null(input_mode)) {
      input_mode <- match.arg(input_mode, c("math", "mixed", "document"))
    }
    take("input_mode", input_mode)
  }
  if (!missing(justify)) {
    if (!is.null(justify)) .check_justify(justify)
    take("justify", justify)
  }
  if (!missing(line_break)) {
    if (!is.null(line_break)) {
      line_break <- match.arg(line_break, c("greedy", "optimal"))
    }
    take("line_break", line_break)
  }
  if (!missing(markdown_style)) {
    # Coerce here rather than at each use, so a bad value is rejected by
    # the call that set it.
    if (!is.null(markdown_style)) markdown_style <- .md_as_style(markdown_style)
    take("markdown_style", markdown_style)
  }
  if (!missing(device_math)) {
    if (!is.null(device_math) &&
        (!is.logical(device_math) || length(device_math) != 1L ||
         is.na(device_math))) {
      stop("`device_math` must be TRUE or FALSE.", call. = FALSE)
    }
    take("device_math", device_math)
  }
  for (role in c("main_font", "sans_font", "mono_font")) {
    if (eval(call("missing", as.name(role)))) next
    value <- get(role)
    if (!is.null(value)) value <- .resolve_font_option(value, role)
    take(role, value)
  }

  # math_font and device_math have an effect beyond the record: the engine
  # font, and the open devices. The engine font goes first, as the one that
  # can still fail (a font that is not loaded); the devices are flipped
  # before recording, so a failure in the C layer leaves the option reading
  # FALSE rather than lying.
  if ("math_font" %in% names(given)) {
    if (is.null(given$math_font)) .reset_math_font() else .set_math_font(given$math_font)
  }
  if ("device_math" %in% names(given)) .gm_base_set(isTRUE(given$device_math))
  for (name in names(given)) .latex_options$values[name] <- given[name]
  invisible(old)
}

#' @rdname latex_options
#'
#' @export
reset_latex_options <- function() {
  # Clearing the values is not enough for two of them. Armed devices would
  # stay hooked while the option read FALSE, and MicroTeX would stay on
  # whichever math font was set last.
  .gm_base_set(FALSE)
  .reset_math_font()
  .latex_options$values <- list(
    math_font   = NULL,
    render_mode = NULL,
    tex_style   = NULL,
    input_mode  = NULL,
    justify     = NULL,
    line_break  = NULL,
    markdown_style = NULL,
    device_math   = NULL,
    main_font   = NULL,
    sans_font   = NULL,
    mono_font   = NULL
  )
  invisible(NULL)
}

# The name a font option's value stands for: a registered font, one of R's
# own families, or else a file or an installed family, which is loaded. The
# value recorded is the name, so a path is read once.
.resolve_font_option <- function(x, what) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", what, "` must be a single font name or file.", call. = FALSE)
  }
  if (x %in% c("sans", "serif", "mono")) return(x)
  registered <- .font_lookup(x)
  if (!is.null(registered)) return(registered)
  tryCatch(
    load_font(x),
    error = function(e) {
      stop("`", what, "`: ", conditionMessage(e), call. = FALSE)
    }
  )
}

# Internal: resolve an argument against latex_options().
.opt <- function(name) {
  .latex_options$values[[name]]
}

# Resolve named formal args of the calling function against
# latex_options(): any arg the caller did not supply explicitly is
# replaced (in the caller's frame) by .opt(<name>) when that option is
# set. Used by latex_grob(), latex_dims(), latex_tree() so the
# missing-arg-then-fallback pattern lives in one place.
.apply_opts <- function(...) {
  env <- parent.frame()
  for (n in c(...)) {
    if (eval(call("missing", as.name(n)), env)) {
      opt <- .opt(n)
      if (!is.null(opt)) assign(n, opt, envir = env)
    }
  }
}

# Internal: validate a tex_style value. Use exact matching (not match.arg)
# because "" is one of the valid choices and partial matching against an
# empty string is ambiguous.
.tex_style_choices <- c("", "display", "text", "script", "scriptscript")
.check_tex_style <- function(x) {
  stopifnot(is.character(x), length(x) == 1L)
  if (!x %in% .tex_style_choices) {
    stop(
      "tex_style must be one of: ",
      paste(sprintf("'%s'", .tex_style_choices), collapse = ", "),
      call. = FALSE
    )
  }
  x
}
