# device_math. Every test disarms on exit, or the hook leaks into other
# files.

png_dev <- function(...) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 600, height = 400, res = 100, ...)
  f
}

# Run `lines` in a fresh Rscript and return what it printed. The child finds
# this build through R_LIBS (system2(env=) does nothing on Windows), which
# is restored afterwards.
run_probe <- function(lines) {
  rscript <- file.path(R.home("bin"), "Rscript")
  skip_if(!file.exists(rscript) && !file.exists(paste0(rscript, ".exe")))
  probe <- tempfile(fileext = ".R")
  writeLines(lines, probe)
  old <- Sys.getenv("R_LIBS", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = old)
    unlink(probe)
  }, add = TRUE)
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  suppressWarnings(
    system2(rscript, c("--vanilla", shQuote(probe)), stdout = TRUE, stderr = TRUE)
  )
}

# Signal an interrupt the way R's own onintr() does.
signal_interrupt <- function() {
  cond <- structure(class = c("interrupt", "condition"),
                    list(message = "", call = NULL))
  withRestarts({
    signalCondition(cond)
    invokeRestart("abort")
  }, resume = function() invisible())
}

test_that("the gate rejects ordinary labels that merely contain a dollar", {
  literal <- c(
    "Revenue ($)", "Sales in $m", "Price $1,000 to $5,000",
    "Cost $ per kg", "Growth in $ and %", "Cost $5-$10",
    "Price is $5 today", "plain label", "", "1", "100%", "a $$ b",
    "Just \\$100 escaped", "$ $",
    # A whole label that is still a price, not a formula.
    "$5 to $", "$ - $", "$1,000$",
    # R's own deparsed labels, where `$` is column access.
    "Histogram of df$price_usd - df$tax_usd", "dat$dist_m * dat$scale_f",
    "x[[1]]$a_b + f(y)$c_d", "Histogram of `my df`$a_b - `my df`$c_d",
    "Histogram of café$prix_ht - café$tva_ht",
    "Histogram of 数据$价格_元 - 数据$税_元"
  )
  for (s in literal) {
    expect_false(.gm_base_is_math(s), label = paste0("literal: ", s))
  }
})

test_that("one mathy span does not drag the rest of the label in", {
  # Every $...$ pair must qualify, or the currency is set as math too.
  expect_false(.gm_base_is_math("Budget $1,000 to $5,000 with $x$ shown"))
  expect_false(.gm_base_is_math("Cost $5-$10 and $\\alpha$"))
  # An escaped dollar is not a span, so this still qualifies.
  expect_true(.gm_base_is_math("Cost: \\$100 for $x$ items"))
})

test_that("a warning during layout does not discard the layout", {
  grDevices::pdf(NULL)
  # A name no other test uses, since the warning fires once per path.
  on.exit({
    .image_reset_warnings()
    grDevices::dev.off()
  }, add = TRUE)
  missing_png <- basename(tempfile("gm-absent-", fileext = ".png"))

  # In base graphics an unreadable image only warns; the rest is drawn.
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
  # Bolding text that was measured plain would overrun its width.
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
  render <- function(label) {
    f <- tempfile(fileext = ".png")
    grDevices::png(f, width = 300, height = 150, bg = "white")
    graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
    graphics::plot.window(c(0, 1), c(0, 1))
    graphics::text(0.5, 0.5, label, cex = 4, col = grDevices::rgb(0, 0, 0, 0.2))
    grDevices::dev.off()
    a <- png::readPNG(f)[, , 1]; unlink(f)
    list(darkest = min(a), ink_cols = sum(apply(a < 0.95, 2, any)))
  }
  ref <- render("x2")
  literal <- render("$x^2$")
  latex_options(device_math = TRUE)
  got <- render("$x^2$")
  # The math path must actually have run.
  expect_lt(got$ink_cols, literal$ink_cols)
  expect_equal(got$darkest, ref$darkest, tolerance = 0.05)
})

test_that("a translucent record colour is read as #RRGGBBAA", {
  skip_if_not_installed("png")
  on.exit(latex_options(device_math = FALSE), add = TRUE)
  grDevices::pdf(NULL)
  lay <- .gm_base_layout("$\\rule{2cm}{1cm}$", 20)
  grDevices::dev.off()
  # Plant a 50% red record, in the byte order color_to_hex() writes.
  lay$layout$color <- "#FF000080"
  gm_base_set_enabled(TRUE, function(...) lay)

  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 300, height = 200, bg = "white")
  graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
  graphics::text(0.5, 0.5, "$x$")
  grDevices::dev.off()
  a <- png::readPNG(f); unlink(f)
  ink <- a[, , 1] < 0.99 | a[, , 2] < 0.99 | a[, , 3] < 0.99
  got <- vapply(1:3, function(ch) stats::median(a[, , ch][ink]), numeric(1))
  # Read alpha-first, the same bytes are opaque navy: (0, 0, 0.5).
  expect_equal(got, c(1, 0.5, 0.5), tolerance = 0.05)
})

test_that("the gate accepts every delimiter latex_wrap() documents", {
  mathy <- c(
    "$x$", "$n$", "$\\alpha$", "$x^2$", "$\\hat{\\beta}_1$",
    "Slope $\\hat{\\beta}_1$ estimate", "$$\\sum_{i=1}^{n} x_i$$",
    "Slope \\(\\hat\\beta\\)", "Block \\[x^2\\] here",
    "Cost: \\$100 for $x$ items",
    # A label that is one formula and nothing else.
    "$y = 2x + 1$", "$f(x)$", "$p < 0.05$", "$ab$", " $n = 30$ ",
    # LaTeX glued to a word: `_`, `^` or `\` after `$` is not column access.
    "CO$_2$ emissions", "R$^2$ = 0.93", "Area (m$^2$)", "10$^{-3}$ M",
    "x$_i$ and x$_j$", "5$\\times$10$^3$"
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
  # An integer width left-aligns every centred label.
  expect_type(lay$width, "double")
  expect_type(lay$ascent, "double")
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
  # Stale callbacks are the crash path, so the count matters most.
  expect_equal(.gm_base_armed(), 0L)
  expect_null(latex_options()$device_math)
})

test_that("the settings latex_options() returns switch interception back off", {
  f <- png_dev()
  on.exit({
    reset_latex_options()
    if (grDevices::dev.cur() > 1L) grDevices::dev.off()
    unlink(f)
  }, add = TRUE)
  reset_latex_options()
  op <- latex_options(device_math = TRUE)
  expect_equal(.gm_base_armed(), 1L)
  # The usual save/restore idiom.
  do.call(latex_options, op)
  expect_equal(.gm_base_armed(), 0L)
})

test_that("a plot with no math is byte-identical armed and unarmed", {
  on.exit(latex_options(device_math = FALSE), add = TRUE)
  draw <- function(f) {
    grDevices::png(f, width = 600, height = 400, res = 100)
    plot(1:10, (1:10)^2, main = "Revenue ($)", xlab = "Cost $5-$10",
         ylab = "plain", sub = "Histogram of df$price_usd - df$tax_usd")
    graphics::legend("topleft", legend = c("a", "b"), pch = 1:2)
    grDevices::dev.off()
  }
  a <- tempfile(fileext = ".png"); draw(a)
  latex_options(device_math = TRUE)
  b <- tempfile(fileext = ".png"); draw(b)
  latex_options(device_math = FALSE)
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

test_that("an interrupt during layout stops the plot", {
  grDevices::pdf(NULL)
  on.exit({
    latex_options(device_math = FALSE)
    grDevices::dev.off()
  }, add = TRUE)
  calls <- 0L
  gm_base_set_enabled(TRUE, function(str, fontsize, col, fontfamily) {
    calls <<- calls + 1L
    if (identical(str, "$x_5$")) signal_interrupt()
    .gm_base_layout(str, fontsize, col, fontfamily)
  })
  graphics::plot.new()

  drawn <- 0L
  res <- tryCatch({
    for (i in 1:10) {
      graphics::text(0.5, 0.5, sprintf("$x_%d$", i))
      drawn <- drawn + 1L
    }
    "completed"
  }, interrupt = function(e) "interrupted")
  expect_identical(res, "interrupted")
  expect_identical(drawn, 4L)

  # ...and the interceptor is not left believing it is mid-layout.
  before <- calls
  graphics::strwidth("$x_6$")
  expect_gt(calls, before)
})

test_that("an interrupt while text is being measured stops the layout", {
  grDevices::pdf(NULL)
  on.exit({
    latex_options(device_math = FALSE)
    grDevices::dev.off()
  }, add = TRUE)
  measurer <- .make_text_measurer
  local_mocked_bindings(.make_text_measurer = function(text_gp, roles = NULL) {
    measure <- measurer(text_gp, roles)
    function(text, font_style, font_family = "") {
      if (grepl("STOP", text, fixed = TRUE)) signal_interrupt()
      measure(text, font_style, font_family)
    }
  })
  stops <- function() paste(basename(tempfile("STOP")), "$x$")  # never cached

  res <- tryCatch({ latex_dims(stops()); "completed" },
                  interrupt = function(e) "interrupted")
  expect_identical(res, "interrupted")

  latex_options(device_math = TRUE)
  graphics::plot.new()
  drawn <- 0L
  res <- tryCatch({
    for (s in c("Before $x$", stops(), "After $y$")) {
      graphics::text(0.5, 0.5, s)
      drawn <- drawn + 1L
    }
    "completed"
  }, interrupt = function(e) "interrupted")
  expect_identical(res, "interrupted")
  expect_identical(drawn, 1L)
})

test_that("a time limit stops an armed plot", {
  grDevices::pdf(NULL)
  on.exit({
    setTimeLimit()
    latex_options(device_math = FALSE)
    grDevices::dev.off()
  }, add = TRUE)
  local_mocked_bindings(.gm_base_layout_impl = function(...) {
    # Busy-wait: Sys.sleep() never checks the limit on Linux and macOS.
    t0 <- proc.time()[["elapsed"]]
    while (proc.time()[["elapsed"]] - t0 < 2) NULL
    NULL
  })
  latex_options(device_math = TRUE)
  graphics::plot.new()
  res <- tryCatch({
    setTimeLimit(elapsed = 0.3, transient = TRUE)
    graphics::text(0.5, 0.5, "$x$")
    "completed"
  }, error = conditionMessage)
  setTimeLimit()
  expect_match(res, "time limit")
})

test_that("a saved latex_options() query switches interception back off", {
  f <- png_dev()
  on.exit({
    reset_latex_options()
    if (grDevices::dev.cur() > 1L) grDevices::dev.off()
    unlink(f)
  }, add = TRUE)
  reset_latex_options()
  op <- latex_options()
  latex_options(device_math = TRUE)
  expect_equal(.gm_base_armed(), 1L)
  do.call(latex_options, op)
  expect_equal(.gm_base_armed(), 0L)
  expect_null(latex_options()$device_math)
})

test_that("recordPlot/replayPlot survive an armed device", {
  # Replay needs a "pkgName" on every graphics-system state (dev.copy(),
  # RStudio's Export and knitr all replay).
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

  # Compare the ink with the grid path.
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
  # A solid fill was ~6.5x the reference.
  expect_lt(via_base, via_grid * 2)
})

test_that("an armed device does not crash R when the package is unloaded", {
  skip_on_cran()
  r_libs <- Sys.getenv("R_LIBS", unset = NA)
  out <- run_probe(c(
    'library(gridmicrotex)',
    'png(tempfile(), width = 300, height = 200)',
    'latex_options(device_math = TRUE)',
    'plot(1:3, main = "$x^2$")',
    # Unload with the device still open and armed.
    'unloadNamespace("gridmicrotex")',
    'plot(1:3, main = "after unload $x^2$")',
    'dev.off()',
    'cat("GM-OK:", !("gridmicrotex" %in% names(getLoadedDLLs())), "\n")'
  ))
  expect_true(any(grepl("GM-OK: TRUE", out, fixed = TRUE)),
              info = paste(out, collapse = "\n"))
  expect_identical(Sys.getenv("R_LIBS", unset = NA), r_libs)
})

test_that("an armed device survives the DLL being unloaded without .onUnload", {
  skip_on_cran()
  # As pkgload::unload() does when another package imports gridmicrotex.
  out <- run_probe(c(
    'library(gridmicrotex)',
    'png(tempfile(), width = 300, height = 200); plot.new()',
    'latex_options(device_math = TRUE)',
    'library.dynam.unload("gridmicrotex", system.file(package = "gridmicrotex"))',
    'w <- strwidth("Hello")',
    'invisible(dev.off())',
    'cat("GM-OK:", w > 0, "\n")'
  ))
  expect_true(any(grepl("GM-OK: TRUE", out, fixed = TRUE)),
              info = paste(out, collapse = "\n"))
})
