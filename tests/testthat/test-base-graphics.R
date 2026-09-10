# Base-graphics math interception.
#
# Every test disarms on exit: R CMD check runs examples and tests against
# a shared device, and leaving it hooked would leak into unrelated files.

png_dev <- function(...) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 600, height = 400, res = 100, ...)
  f
}

test_that("the gate rejects ordinary labels that merely contain a dollar", {
  # Each of these is balanced or unbalanced in a way latex_wrap() would
  # happily mangle: "Revenue ($)" becomes \text{Revenue (}) and
  # "Cost $5-$10" sets "5-" as math. Neither may be intercepted.
  literal <- c(
    "Revenue ($)", "Sales in $m", "Price $1,000 to $5,000",
    "Cost $ per kg", "Growth in $ and %", "Cost $5-$10",
    "Price is $5 today", "plain label", "", "1", "100%", "a $$ b",
    "Just \\$100 escaped"
  )
  for (s in literal) {
    expect_false(.gm_base_is_math(s), label = paste0("literal: ", s))
  }
})

test_that("one mathy span does not drag the rest of the label in", {
  # latex_wrap() cannot be told "span 3 is math, leave span 1 alone", so
  # accepting a label because *some* span qualifies sets the others as
  # math too: "Budget $1,000 to $5,000 with $x$ shown" rendered the
  # currency as math and italicised "to".
  expect_false(.gm_base_is_math("Budget $1,000 to $5,000 with $x$ shown"))
  expect_false(.gm_base_is_math("Cost $5-$10 and $\\alpha$"))
  # An escaped dollar is not a span, so this still qualifies.
  expect_true(.gm_base_is_math("Cost: \\$100 for $x$ items"))
})

test_that("a warning during layout does not discard the layout", {
  grDevices::pdf(NULL)
  # The missing-image warning fires once per path, so use a name no other
  # test shares and reset the registry afterwards -- reusing
  # "no-such-file.png" silently consumed the flag test-images.R relies on.
  on.exit({
    .image_reset_warnings()
    grDevices::dev.off()
  }, add = TRUE)
  missing_png <- basename(tempfile("gm-absent-", fileext = ".png"))

  # An unreadable image only warns; the rest of the formula is fine, and
  # treating the warning as failure drew the raw LaTeX source instead.
  expect_silent(
    lay <- .gm_base_layout(sprintf("$x + \\includegraphics{%s}$", missing_png), 20)
  )
  expect_false(is.null(lay))
})

test_that("text runs take their face from the record, not from par(font)", {
  skip_if_not_installed("png")
  on.exit(latex_options(device_math = FALSE), add = TRUE)
  ink_width <- function(draw) {
    f <- tempfile(fileext = ".png")
    grDevices::png(f, width = 600, height = 200, bg = "white")
    graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
    graphics::plot.window(c(0, 1), c(0, 1))
    draw(); grDevices::dev.off()
    a <- png::readPNG(f); unlink(f)
    d <- apply(a[, , 1:3, drop = FALSE], c(1, 2), function(p) any(p < 0.5))
    cs <- which(apply(d, 2, any))
    if (!length(cs)) 0L else max(cs) - min(cs)
  }
  latex_options(device_math = TRUE)
  # MicroTeX measured the prose in its own face. Inheriting par(font = 2)
  # would bold text that was measured plain, overrunning the reported width.
  w_plain <- ink_width(function()
    graphics::text(0.5, 0.5, "Slope $x$ estimate", cex = 2, font = 1))
  w_bold  <- ink_width(function()
    graphics::text(0.5, 0.5, "Slope $x$ estimate", cex = 2, font = 2))
  expect_equal(w_bold, w_plain)
  # ...but \textbf inside the label must still bolden.
  expect_gt(ink_width(function()
              graphics::text(0.5, 0.5, "$\\textbf{ABCDEF}$", cex = 2)),
            ink_width(function()
              graphics::text(0.5, 0.5, "$\\text{ABCDEF}$", cex = 2)))
})

test_that("a translucent colour stays translucent", {
  skip_if_not_installed("png")
  on.exit(latex_options(device_math = FALSE), add = TRUE)
  darkest <- function(draw) {
    f <- tempfile(fileext = ".png")
    grDevices::png(f, width = 300, height = 150, bg = "white")
    graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
    graphics::plot.window(c(0, 1), c(0, 1))
    draw(); grDevices::dev.off()
    a <- png::readPNG(f); unlink(f)
    min(a[, , 1])
  }
  ref <- darkest(function()
    graphics::text(0.5, 0.5, "x2", cex = 4, col = grDevices::rgb(0, 0, 0, 0.2)))
  latex_options(device_math = TRUE)
  got <- darkest(function()
    graphics::text(0.5, 0.5, "$x^2$", cex = 4, col = grDevices::rgb(0, 0, 0, 0.2)))
  # Dropping alpha drew it fully opaque (0); reading the record's 9-char
  # hex in the wrong byte order drew nothing at all (1).
  expect_equal(got, ref, tolerance = 0.05)
})

test_that("the gate accepts every delimiter latex_wrap() documents", {
  mathy <- c(
    "$x$", "$n$", "$\\alpha$", "$x^2$", "$\\hat{\\beta}_1$",
    "Slope $\\hat{\\beta}_1$ estimate", "$$\\sum_{i=1}^{n} x_i$$",
    "Slope \\(\\hat\\beta\\)", "Block \\[x^2\\] here",
    "Cost: \\$100 for $x$ items"
  )
  for (s in mathy) {
    expect_true(.gm_base_is_math(s), label = paste0("math: ", s))
  }
})

test_that("a label with no math lays out to NULL rather than erroring", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_null(.gm_base_layout("Revenue ($)", 12))
  expect_null(.gm_base_layout("plain", 12))
})

test_that("layout metrics are doubles and match latex_dims", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  lay <- .gm_base_layout("$\\hat{\\beta}_1$", 12)
  expect_type(lay$width, "double")
  expect_type(lay$ascent, "double")
  # An integer width here reads as "no horizontal adjustment" in the C
  # emitter and silently left-aligns every centred label.
  d <- latex_dims("$\\hat{\\beta}_1$", render_mode = "path",
                  gp = grid::gpar(fontsize = 12))
  expect_equal(lay$width, grid::convertWidth(d$width, "bigpts", TRUE))
})

test_that("arming tracks devices and releases every one", {
  on.exit(latex_options(device_math = FALSE), add = TRUE)
  expect_equal(.gm_base_armed(), 0L)

  f1 <- png_dev()
  latex_options(device_math = TRUE)
  expect_equal(.gm_base_armed(), 1L)

  f2 <- png_dev()
  expect_equal(.gm_base_armed(), 2L)   # armed on creation, not on plot.new

  grDevices::dev.off()
  expect_equal(.gm_base_armed(), 1L)   # released on device destruction

  latex_options(device_math = FALSE)
  expect_equal(.gm_base_armed(), 0L)
  grDevices::dev.off()
  expect_equal(.gm_base_armed(), 0L)
  unlink(c(f1, f2))
})

test_that("reset_latex_options disarms rather than only clearing the flag", {
  f <- png_dev()
  on.exit({
    latex_options(device_math = FALSE)
    if (grDevices::dev.cur() > 1L) grDevices::dev.off()
    unlink(f)
  }, add = TRUE)
  latex_options(device_math = TRUE)
  expect_equal(.gm_base_armed(), 1L)
  reset_latex_options()
  # Stale callbacks outliving the option that installed them is the
  # crash path, so the count matters more than the option value.
  expect_equal(.gm_base_armed(), 0L)
  expect_null(latex_options()$device_math)
})

test_that("a plot with no math is byte-identical armed and unarmed", {
  on.exit(latex_options(device_math = FALSE), add = TRUE)
  draw <- function(f) {
    grDevices::png(f, width = 600, height = 400, res = 100)
    plot(1:10, (1:10)^2, main = "Revenue ($)", xlab = "Cost $5-$10",
         ylab = "plain")
    graphics::legend("topleft", legend = c("a", "b"), pch = 1:2)
    grDevices::dev.off()
  }
  a <- tempfile(fileext = ".png"); draw(a)
  latex_options(device_math = TRUE)
  b <- tempfile(fileext = ".png"); draw(b)
  latex_options(device_math = FALSE)
  # The contract behind "zero change to user plotting code".
  expect_identical(unname(tools::md5sum(a)), unname(tools::md5sum(b)))
  unlink(c(a, b))
})

test_that("strWidth reports the math width, so centred labels centre", {
  f <- png_dev()
  on.exit({
    latex_options(device_math = FALSE)
    if (grDevices::dev.cur() > 1L) grDevices::dev.off()
    unlink(f)
  }, add = TRUE)
  plot(0:1, 0:1, type = "n")
  literal <- graphics::strwidth("$x^2$", cex = 2)

  latex_options(device_math = TRUE)
  armed <- graphics::strwidth("$x^2$", cex = 2)
  lay <- .gm_base_layout("$x^2$", 24)
  in_user <- graphics::grconvertX(lay$width / 72, "inches", "user") -
             graphics::grconvertX(0, "inches", "user")

  expect_false(isTRUE(all.equal(armed, literal)))
  expect_equal(armed, in_user, tolerance = 1e-6)
})

test_that("a non-math label's width is untouched while armed", {
  f <- png_dev()
  on.exit({
    latex_options(device_math = FALSE)
    if (grDevices::dev.cur() > 1L) grDevices::dev.off()
    unlink(f)
  }, add = TRUE)
  plot(0:1, 0:1, type = "n")
  before <- graphics::strwidth("Revenue ($)", cex = 2)
  latex_options(device_math = TRUE)
  expect_identical(graphics::strwidth("Revenue ($)", cex = 2), before)
})

test_that("device_math rejects non-logical values", {
  expect_error(latex_options(device_math = "yes"), "must be TRUE or FALSE")
  expect_error(latex_options(device_math = NA), "must be TRUE or FALSE")
  expect_equal(.gm_base_armed(), 0L)
})

test_that("recordPlot/replayPlot survive an armed device", {
  # Registering a graphics system adds an element to every recordPlot()
  # snapshot, and restoreRecordedPlot() calls library() on each element's
  # "pkgName" with no NULL guard. An unnamed state makes replay fail with
  # "'package' must be of length 1" -- which breaks dev.copy(), RStudio's
  # Export, and every knitr chunk. No unit test caught this; knitting the
  # README did.
  f <- png_dev()
  on.exit({
    latex_options(device_math = FALSE)
    if (grDevices::dev.cur() > 1L) grDevices::dev.off()
    unlink(f)
  }, add = TRUE)
  grDevices::dev.control(displaylist = "enable")
  latex_options(device_math = TRUE)
  plot(1:5, main = "$\\alpha^2$")

  rp <- grDevices::recordPlot()
  named <- vapply(seq_along(rp)[-1],
                  function(i) length(attr(rp[[i]], "pkgName")), integer(1))
  expect_true(all(named == 1L))
  expect_silent(grDevices::replayPlot(rp))
})

test_that("document-wide options reach base labels", {
  grDevices::pdf(NULL)
  on.exit({ reset_latex_options(); grDevices::dev.off() }, add = TRUE)
  w <- function() .gm_base_layout("$\\sum_{i=1}^{n} x_i$", 20)$width
  h <- function() .gm_base_layout("$\\sum_{i=1}^{n} x_i$", 20)$height

  default_w <- w()
  latex_options(math_font = "stix")
  # Hard-coding math_font = "" here silently ignored the user's font.
  expect_false(isTRUE(all.equal(w(), default_w)))
  reset_latex_options()
  expect_equal(w(), default_w)

  default_h <- h()
  latex_options(tex_style = "display")
  expect_false(isTRUE(all.equal(h(), default_h)))
})

test_that("boxed formulas are stroked, not filled, and match grid", {
  skip_if_not_installed("png")
  on.exit(latex_options(device_math = FALSE), add = TRUE)

  ink <- function(draw) {
    f <- tempfile(fileext = ".png")
    grDevices::png(f, width = 300, height = 120, res = 100, bg = "white")
    draw()
    grDevices::dev.off()
    a <- png::readPNG(f)
    unlink(f)
    mean(apply(a[, , 1:3, drop = FALSE], c(1, 2), function(p) any(p < 0.5)))
  }

  # A "rect" record is an outline (col=colour, fill=NA); filling it turns
  # every \boxed{} and \fbox{} into a solid block. Compare the ink against
  # the grid path, which is the reference implementation.
  via_grid <- ink(function() {
    grid::grid.newpage()
    grid.latex("$\\boxed{x^2}$", render_mode = "path",
               gp = grid::gpar(fontsize = 36))
  })
  latex_options(device_math = TRUE)
  via_base <- ink(function() {
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot.new()
    graphics::text(0.5, 0.5, "$\\boxed{x^2}$", cex = 3)
  })
  latex_options(device_math = FALSE)

  expect_equal(via_base, via_grid, tolerance = 0.15)
  # A solid fill was ~6.5x the reference; make that impossible to pass.
  expect_lt(via_base, via_grid * 2)
})

test_that("an armed device does not crash R when the package is unloaded", {
  skip_on_cran()
  rscript <- file.path(R.home("bin"), "Rscript")
  skip_if(!file.exists(rscript) && !file.exists(paste0(rscript, ".exe")))

  probe <- tempfile(fileext = ".R")
  writeLines(c(
    'library(gridmicrotex)',
    'png(tempfile(), width = 300, height = 200)',
    'latex_options(device_math = TRUE)',
    'plot(1:3, main = "$x^2$")',
    # Unload with the device still open and still armed. Without the
    # teardown in .onUnload this jumps into freed memory on the next draw.
    'unloadNamespace("gridmicrotex")',
    'plot(1:3, main = "after unload $x^2$")',
    'dev.off()',
    'cat("GM-OK:", !("gridmicrotex" %in% names(getLoadedDLLs())), "\n")'
  ), probe)
  # system2(env=) is a no-op on Windows; pass the library path through.
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  out <- suppressWarnings(
    system2(rscript, c("--vanilla", shQuote(probe)), stdout = TRUE, stderr = TRUE)
  )
  expect_true(any(grepl("GM-OK: TRUE", out, fixed = TRUE)),
              info = paste(out, collapse = "\n"))
  unlink(probe)
})
