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
  # Without them combine_elements() drops the element from inherited themes.
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
  # \text{} inside \textbf would reset the style.
  expect_match(.md_to_tex("**b**"), "\\textbf{b}", fixed = TRUE)
  expect_match(.md_to_tex("*i*"), "\\textit{i}", fixed = TRUE)
  expect_false(grepl("\\textbf{\\text{", .md_to_tex("**b**"), fixed = TRUE))

  g <- markdown_grob("**bold** and *ital* and plain")
  faces <- vapply(g$layout_df$font_style, .resolve_text_face, character(1))
  expect_true("bold" %in% faces)
  expect_true("italic" %in% faces)
  expect_true("plain" %in% faces)
})

test_that("prose is text, not spaced math italics", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_identical(.md_to_tex("plain words"), "plain words")
  expect_identical(unique(markdown_grob("plain words")$layout_df$type), "text")
})

test_that("math and escapes survive inside emphasis", {
  tex <- .md_to_tex("**bold with $x^2$ inside**")
  expect_match(tex, "$x^2$", fixed = TRUE)
  g <- markdown_grob("**bold with $x^2$ inside**")
  expect_true("glyph" %in% g$layout_df$type)   # the math
  expect_true("text" %in% g$layout_df$type)    # the prose
})

test_that("geom_markdown contributes one rendered grob per row", {
  df <- data.frame(x = 1:3, y = 1:3,
                   lab = c("**b**", "*i* $\\beta_1$", "`c` $x^2$"))
  gs <- ggplot2::layer_grob(
    ggplot2::ggplot(df, ggplot2::aes(x, y, label = lab)) +
      geom_markdown(fontsize = 12))[[1]]
  expect_length(gs, 3L)
  expect_true(all(vapply(gs, inherits, logical(1), "latexgrob")))
  expect_true(all(vapply(gs, function(g) nrow(g$layout_df) > 0, logical(1))))
})

test_that("geom_markdown treats a missing alpha as opaque", {
  df <- data.frame(x = 1, y = 1, lab = "**b**", a = NA_real_)
  g <- ggplot2::layer_grob(
    ggplot2::ggplot(df, ggplot2::aes(x, y, label = lab, alpha = a)) +
      geom_markdown(colour = "red"))[[1]][[1]]
  expect_equal(g$gp$col, "red")
})

test_that("annotate('markdown') follows latex_options(), as geom_markdown() does", {
  on.exit(reset_latex_options(), add = TRUE)
  latex_options(math_font = "stix")
  base <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y))
  grob_of <- function(layer) ggplot2::layer_grob(base + layer)[[1]][[1]]
  geom <- grob_of(geom_markdown(ggplot2::aes(label = "*b* $x^2$")))
  ann <- grob_of(ggplot2::annotate("markdown", x = 1, y = 1, label = "*b* $x^2$"))
  expect_identical(ann$layout_df, geom$layout_df)
  fonts <- ann$layout_df$font_file[ann$layout_df$type == "glyph"]
  expect_true(length(fonts) > 0 && all(grepl("STIX", fonts)))
})

test_that("element_markdown installs our grob as the axis titles", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  df <- data.frame(x = 1:3, y = 1:3)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    ggplot2::labs(x = "**w** in $10^3$", y = "*eta* $\\eta$") +
    ggplot2::theme(
      axis.title.x = element_markdown(),
      axis.title.y = element_markdown()
    )
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  # ggplot2 wraps a text grob that has a margin in a titleGrob.
  named <- function(nm) {
    g <- gt$grobs[[which(gt$layout$name == nm)]]
    if (inherits(g, "titleGrob")) g$children[[1]] else g
  }

  # y is rotated and takes a different branch.
  expect_s3_class(named("xlab-b"), "latexgrob")
  expect_s3_class(named("ylab-l"), "latexgrob")
  # Rendered as markdown, not as the literal asterisks.
  expect_false(any(grepl("*", named("xlab-b")$layout_df$text, fixed = TRUE)))
})

test_that("a bundle of tick labels reports a non-zero size", {
  # ggplot2 sizes the axis from this grob.
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
  # element_latex() strips them; in markdown they are the math.
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

  # Both still apply in a built plot, where the element is merged into the
  # theme (a NULL property default failed that merge).
  p <- ggplot2::ggplot(data.frame(x = 1, y = 1, lab = "**a**"),
                       ggplot2::aes(x, y, label = lab)) +
    ggplot2::geom_point() +
    geom_markdown(style = "body { color: #1F6FB2 }") +
    ggplot2::labs(x = "**wt**") +
    ggplot2::theme(axis.title.x = element_markdown(style = "body { color: red }"))
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  xlab <- gt$grobs[[which(gt$layout$name == "xlab-b")]]
  if (inherits(xlab, "titleGrob")) xlab <- xlab$children[[1]]
  expect_equal(cols(xlab), "#FF0000")
  expect_equal(cols(ggplot2::layer_grob(p, i = 2L)[[1]][[1]]), "#1F6FB2")
})

test_that("body seeds inheritance for inline markdown too", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  cols <- function(g) unique(stats::na.omit(g$layout_df$color))
  expect_equal(cols(markdown_grob("hi", style = "body { color: #B22222 }")),
               "#B22222")
  # An explicit tag rule still wins over the body seed.
  expect_equal(
    cols(markdown_grob("hi",
                       style = "body { color: #B22222 } p { color: #1F6FB2 }")),
    "#1F6FB2")
})

test_that(".md_needs_blocks agrees with what .md_blocks() would build", {
  expect_false(.md_needs_blocks("plain **text** with $x$ math"))
  expect_false(.md_needs_blocks(""))
  # Inline $$..$$ in the middle of a sentence stays a paragraph...
  expect_false(.md_needs_blocks("a $$x$$ inline"))
  # ...but a paragraph that is nothing else becomes a centred math block.
  expect_true(.md_needs_blocks("$$x^2$$"))
  expect_true(.md_needs_blocks("# H"))
  expect_true(.md_needs_blocks("one\n\ntwo"))
  expect_true(.md_needs_blocks("- a\n- b"))
})

test_that("a block-structured label is laid out as blocks, a paragraph is not", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  eg <- function(lab, ...) {
    ggplot2::element_grob(element_markdown(...), label = lab)
  }
  expect_s3_class(eg("The **fitted** slope is $\\beta_1$"), "latexgrob")
  expect_s3_class(eg("## H\n\n- one\n- two"), "markdownbox")
  # A width asks for the box even for a paragraph.
  expect_s3_class(eg("just a paragraph", width = grid::unit(2, "in")),
                  "markdownbox")
})

test_that("a rotated block label keeps its angle and warns", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # The box cannot rotate, so the label is flattened to a run instead.
  expect_warning(
    g <- ggplot2::element_grob(element_markdown(angle = 90),
                               label = "# H\n\n- a"),
    "rotated"
  )
  expect_s3_class(g, "latexgrob")

  # A width with an angle becomes the run's wrapping measure.
  expect_no_warning(
    g2 <- ggplot2::element_grob(
      element_markdown(angle = 90, width = grid::unit(100, "bigpts")),
      label = "a long paragraph that will want to wrap somewhere along it")
  )
  expect_s3_class(g2, "latexgrob")
})

test_that("axis tick labels are never laid out as blocks", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # A box sized in npc would measure against the whole device.
  g <- ggplot2::element_grob(element_markdown(), label = c("# H", "# H"))
  expect_s3_class(g, "gridmicrotex_labels")
  expect_true(all(vapply(g$children, inherits, logical(1), "latexgrob")))
  expect_false(any(vapply(g$children, inherits, logical(1), "markdownbox")))
})

test_that("a promoted label's height does not depend on the parent width", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  g <- ggplot2::element_grob(element_markdown(), label = "# H\n\n- one\n- two")
  h <- vapply(c(2, 6, 9), function(w) {
    grid::pushViewport(grid::viewport(width = grid::unit(w, "in")))
    on.exit(grid::popViewport(), add = TRUE)
    grid::convertHeight(grid::heightDetails(g), "bigpts", valueOnly = TRUE)
  }, numeric(1))
  expect_equal(h, rep(h[1], 3))
})

test_that("a block plot title is promoted to a box, and its style applies", {
  skip_on_cran()
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  mk <- function(...) {
    ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "## Findings\n\n- slope $\\beta_1$\n- *p* < 0.001") +
      ggplot2::theme(plot.title = element_markdown(...))
  }
  title_grob <- function(p) {
    gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
    gt$grobs[[which(gt$layout$name == "title")]]
  }
  height <- function(g) {
    grid::convertHeight(grid::heightDetails(g), "bigpts", valueOnly = TRUE)
  }

  plain <- title_grob(mk())
  expect_s3_class(plain, "markdownbox")

  styled <- title_grob(mk(width = grid::unit(1, "npc"),
                          style = "body { background: #EEF3FB; padding: 8px }"))
  expect_s3_class(styled, "markdownbox")
  # The padding reaches the box.
  expect_gt(height(styled), height(plain))
})
