# A subset of siunitx: \num, \si, \SI, \qty, \ang, ranges and lists. Each is
# checked against the LaTeX it stands for, drawn the same.

rec <- function(tex) latex_tree(tex, input_mode = "math", render_mode = "typeface")$records

warns <- function(tex) {
  out <- character(0)
  withCallingHandlers(
    rec(tex),
    warning = function(x) {
      out <<- c(out, conditionMessage(x))
      invokeRestart("muffleWarning")
    })
  out
}

same <- function(a, b) {
  key <- function(tex) {
    r <- rec(tex)
    r <- r[order(r$x, r$y), c("type", "x", "y", "glyph", "text")]
    r$x <- round(r$x, 3)
    r$y <- round(r$y, 3)
    r
  }
  expect_equal(key(a), key(b), ignore_attr = TRUE, info = a)
  expect_identical(warns(a), character(0), info = a)
}

test_that("\\num sets digits in groups, an exponent as a power of ten, and an uncertainty", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  same(r"(\num{1234})", "1234")
  same(r"(\num{12345})", r"(12\,345)")
  same(r"(\num{1234567.12345})", r"(1\,234\,567.123\,45)")
  same(r"(\num{1.5e-4})", r"(1.5\times10^{-4})")
  same(r"(\num{2E+3})", r"(2\times10^{3})")
  same(r"(\num{-3.2(1)})", r"({-}3.2(1))")
  same(r"(\num{1.2+-0.3})", r"(1.2\pm0.3)")
  same(r"(\num{1.2+-0.3e3})", r"(\left(1.2\pm0.3\right)\times10^{3})")
  same(r"(\num{.5})", "0.5")
  same(r"(\num{1,5})", "1{,}5")
  # In text too, as math.
  expect_identical(warns(r"(a number \num{3.5} in a label)"), character(0))
})

test_that("\\num warns of what is not a number and says it as it is", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_match(warns(r"(\num{12x4})"), "not a number", all = FALSE)
  expect_match(warns(r"(\num{1e})"), "not a number", all = FALSE)
})

test_that("units are written with macros or as text, prefixes joined to their unit", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  same(r"(\si{\meter\per\second})", r"(\mathrm{m}\,\mathrm{s}^{-1})")
  same(r"(\si{\kilo\gram\metre\per\second\squared})",
       r"(\mathrm{kg}\,\mathrm{m}\,\mathrm{s}^{-2})")
  same(r"(\unit{\micro\meter\cubed})", "\\mathrm{\u00b5m}^{3}")
  same(r"(\si{\square\metre})", r"(\mathrm{m}^{2})")
  same(r"(\si{m.s^{-1}})", r"(\mathrm{m}\,\mathrm{s}^{-1})")
  same(r"(\si{kg.m/s^2})", r"(\mathrm{kg}\,\mathrm{m}\,\mathrm{s}^{-2})")
  same(r"(\si{\metre\of{eff}})", r"(\mathrm{m}_{\mathrm{eff}})")
  same(r"(\si{\ohm})", r"(\mathrm{\Omega})")
  expect_match(warns(r"(\si{\wibble})"), "unknown unit", all = FALSE)
})

test_that("\\SI and \\qty are a number, a thin space and a unit", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  same(r"(\SI{9.81}{\meter\per\second\squared})", r"(9.81\,\mathrm{m}\,\mathrm{s}^{-2})")
  same(r"(\qty{1.5}{\kilo\gram})", r"(1.5\,\mathrm{kg})")
  same(r"(\SI{3e8}{m/s})", r"(3\times10^{8}\,\mathrm{m}\,\mathrm{s}^{-1})")
  same(r"(\SI[round-mode=places]{12345}{\hertz})", r"(12\,345\,\mathrm{Hz})")
  same(r"(\SI{20}{\degreeCelsius})", r"(20\,\mathrm{{}^{\circ}C})")
})

test_that("angles, ranges and lists", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  same(r"(\ang{12.5})", r"(12.5{}^{\circ})")
  same(r"(\ang{1;2;3})", r"(1{}^{\circ}2{}^{\prime}3{}^{\prime\prime})")
  same(r"(\ang{;;30})", r"(30{}^{\prime\prime})")
  same(r"(\numrange{1}{10})", r"(1\text{ to }10)")
  same(r"(\SIrange{1}{5}{\volt})", r"(1\,\mathrm{V}\text{ to }5\,\mathrm{V})")
  same(r"(\numlist{1;2;3})", r"(1\text{, }2\text{, and }3)")
  same(r"(\numlist{1;2})", r"(1\text{ and }2)")
  same(r"(\SIlist{1;2}{\meter})", r"(1\,\mathrm{m}\text{ and }2\,\mathrm{m})")
})

test_that("\\DeclareSIUnit makes a unit, and a document's own \\num or \\si wins", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  same(r"(\DeclareSIUnit{\foo}{bar}\si{\foo\per\second})", r"(\mathrm{bar}\,\mathrm{s}^{-1})")
  same(r"(\newcommand{\num}[1]{[#1]} \num{12345})", "[12345]")
  same(r"(\renewcommand{\si}[1]{(#1)} \si{x})", "(x)")
  # \sisetup and options are read and leave nothing.
  same(r"(\sisetup{per-mode=symbol} \num{12})", "12")
})
