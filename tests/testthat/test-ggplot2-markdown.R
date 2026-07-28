skip_if_not_installed("ggplot2")

test_that("the ggplot2 objects are created at load", {
  expect_s3_class(GeomMarkdown, "Geom")
  expect_false(is.null(.element_markdown_class))
  expect_true(!is.null(getS3method(
    "element_grob", "gridmicrotex::element_markdown",
    optional = TRUE, envir = asNamespace("ggplot2")
  )))
})

test_that("element_markdown keeps the legacy element classes", {
  # S7 inheritance drops ggplot2's legacy "element_text"/"element" S3
  # strings, and without them combine_elements() treats the object as an
  # unrelated sibling and silently discards it when resolving inherited
  # theme entries.
  e <- element_markdown()
  expect_true(inherits(e, "element_text"))
  expect_true(inherits(e, "element"))
  expect_true(inherits(e, "gridmicrotex::element_markdown"))
})

test_that("element_markdown survives theme inheritance", {
  th <- ggplot2::theme_gray() +
    ggplot2::theme(axis.title = element_markdown())
  merged <- ggplot2::calc_element("axis.title.y", th)
  expect_true(inherits(merged, "gridmicrotex::element_markdown"))
})

test_that("emphasis actually reaches the rendered output", {
  # \text{} inside a text command resets the style: \textbf{bold} reports
  # font style 2, \textbf{\text{bold}} reports 1. Wrapping there would
  # silently drop every bold and italic in the document.
  expect_match(.md_to_tex("**b**"), "\\textbf{b}", fixed = TRUE)
  expect_match(.md_to_tex("*i*"), "\\textit{i}", fixed = TRUE)
  expect_false(grepl("\\textbf{\\text{", .md_to_tex("**b**"), fixed = TRUE))

  g <- markdown_grob("**bold** and *ital* and plain")
  faces <- vapply(g$layout_df$font_style, .resolve_text_face, character(1))
  expect_true("bold" %in% faces)
  expect_true("italic" %in% faces)
  expect_true("plain" %in% faces)
})

test_that("prose outside a text command is still \\text{}-wrapped", {
  # Top level is math mode, so unwrapped prose would come out as spaced
  # math italics.
  expect_match(.md_to_tex("plain words"), "\\text{plain words}", fixed = TRUE)
})

test_that("math and escapes survive inside emphasis", {
  tex <- .md_to_tex("**bold with $x^2$ inside**")
  expect_match(tex, "$x^2$", fixed = TRUE)
  g <- markdown_grob("**bold with $x^2$ inside**")
  expect_true("glyph" %in% g$layout_df$type)   # the math
  expect_true("text" %in% g$layout_df$type)    # the prose
})

test_that("geom_markdown builds a plot", {
  df <- data.frame(x = 1:3, y = 1:3,
                   lab = c("**b**", "*i* $\\beta_1$", "`c` $x^2$"))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, label = lab)) +
    geom_markdown(fontsize = 12)
  expect_s3_class(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)), "gtable")
})

test_that("element_markdown works for titles and tick labels", {
  df <- data.frame(x = 1:3, y = 1:3)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    ggplot2::labs(x = "**w** in $10^3$", y = "*eta* $\\eta$") +
    ggplot2::theme(
      axis.title.x = element_markdown(),
      axis.title.y = element_markdown(),
      axis.text.x  = element_markdown(fontsize = 9)
    )
  expect_s3_class(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)), "gtable")
})

test_that("a bundle of tick labels reports a non-zero size", {
  # ggplot2 sizes the axis strip from this grob. When it measured 0 x 0
  # no room was reserved and the labels were drawn over the axis title.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  for (el in list(element_markdown(), element_latex())) {
    g <- ggplot2::element_grob(el, label = c("1.0", "1.5", "2.0"),
                               x = grid::unit(c(0.2, 0.5, 0.8), "npc"))
    expect_s3_class(g, "gridmicrotex_labels")
    expect_gt(grid::convertHeight(grid::grobHeight(g), "pt", valueOnly = TRUE), 0)
    expect_gt(grid::convertWidth(grid::grobWidth(g), "pt", valueOnly = TRUE), 0)
  }
})

test_that("an empty label gives a null grob", {
  expect_s3_class(ggplot2::element_grob(element_markdown(), label = ""),
                  "null")
  expect_s3_class(ggplot2::element_grob(element_markdown(), label = NA),
                  "null")
})

test_that("element_markdown validates justify", {
  expect_error(element_markdown(justify = "yes"), "TRUE or FALSE")
  expect_error(geom_markdown(justify = NA), "TRUE or FALSE")
})

test_that("dollar delimiters are never stripped", {
  # element_latex() strips enclosing $...$ in math mode, where users add
  # them by analogy with plotmath. In markdown a `$` pair is the math
  # delimiter, so stripping would destroy the label.
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  g <- ggplot2::element_grob(element_markdown(), label = "$x^2$")
  expect_s3_class(g, "latexgrob")
  # Rendered as math (glyphs), not as the literal characters.
  expect_true("glyph" %in% g$layout_df$type)
})

test_that("style= reaches both ggplot2 entry points", {
  skip_if_not_installed("ggplot2")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  cols <- function(g) unique(stats::na.omit(g$layout_df$color))

  # element_markdown stores it and element_grob honours it.
  e <- element_markdown(style = "body { color: #B22222 }")
  expect_equal(cols(ggplot2::element_grob(e, label = "**hi**")), "#B22222")
  expect_equal(cols(ggplot2::element_grob(element_markdown(), label = "hi")),
               "#000000")

  # The multi-label path (axis tick labels) carries it too.
  g <- ggplot2::element_grob(e, label = c("**a**", "*b*"))
  ticks <- unlist(lapply(g$children, function(k) cols(k)))
  expect_equal(unique(ticks), "#B22222")

  # geom_markdown takes it as a layer parameter.
  expect_true("style" %in% names(formals(geom_markdown)))

  # The element must still merge into a theme. A NULL default would fail
  # here with "Can't find property <ggplot2::element_text>@style", because
  # ggplot2 reads an unset property off the parent element -- which is why
  # the property defaults to NA and is mapped back to NULL on use.
  p <- ggplot2::ggplot(data.frame(x = 1, y = 1, lab = "**a**"),
                       ggplot2::aes(x, y, label = lab)) +
    ggplot2::geom_point() +
    geom_markdown(style = "body { color: #1F6FB2 }") +
    ggplot2::labs(x = "**wt**") +
    ggplot2::theme(axis.title.x = element_markdown(style = "body { color: red }"))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("body seeds inheritance for inline markdown too", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # markdown_box_grob() has always seeded from `body`; markdown_grob() did
  # not, so a stylesheet written against the box grob silently did nothing
  # when handed to the inline one.
  cols <- function(g) unique(stats::na.omit(g$layout_df$color))
  expect_equal(cols(markdown_grob("hi", style = "body { color: #B22222 }")),
               "#B22222")
  # An explicit tag rule still wins over the body seed.
  expect_equal(
    cols(markdown_grob("hi",
                       style = "body { color: #B22222 } p { color: #1F6FB2 }")),
    "#1F6FB2")
})
