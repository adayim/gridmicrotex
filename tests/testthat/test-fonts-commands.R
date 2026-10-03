# The fonts a document names -- fontspec's \setmainfont, \setsansfont,
# \setmonofont, \fontspec and \newfontfamily, unicode-math's \setmathfont and
# NFSS's \fontfamily -- in a label and in a whole file.

load_cmd_fonts <- function() {
  load_font(stix(), name = "Cmd Probe Mono")
  load_font(lete(), name = "Cmd Probe Sans")
}

test_that("\\setmonofont, \\setsansfont and \\setmainfont set the fonts of their roles", {
  load_cmd_fonts()
  mono <- record_families("\\setmonofont{Cmd Probe Mono}\\texttt{ab} \\textsf{cd} ef")
  expect_equal(mono[["ab"]], "Cmd Probe Mono")
  expect_true(is.na(mono[["cd"]]))
  expect_true(is.na(mono[["ef"]]))

  sans <- record_families("\\setsansfont{Cmd Probe Sans}\\texttt{ab} \\textsf{cd} ef")
  expect_true(is.na(sans[["ab"]]))
  expect_equal(sans[["cd"]], "Cmd Probe Sans")
  expect_true(is.na(sans[["ef"]]))

  # The body, and \textrm, which returns to it -- but not \texttt.
  main <- record_families("\\setmainfont{Cmd Probe Sans} ab \\textrm{cd} \\texttt{ef}")
  expect_equal(main[["ab"]], "Cmd Probe Sans")
  expect_equal(main[["cd"]], "Cmd Probe Sans")
  expect_true(is.na(main[["ef"]]))

  # \ttfamily and \verb are the typewriter role too.
  expect_equal(
    record_families("\\setmonofont{Cmd Probe Mono}{\\ttfamily ab} \\verb|cd|")[c("ab", "cd")],
    c(ab = "Cmd Probe Mono", cd = "Cmd Probe Mono"))
})

test_that("a font set in a group ends with the group", {
  load_cmd_fonts()
  r <- record_families("{\\setmonofont{Cmd Probe Mono}\\texttt{ab}} \\texttt{cd}")
  expect_equal(r[["ab"]], "Cmd Probe Mono")
  expect_true(is.na(r[["cd"]]))

  r <- record_families("{\\setmainfont{Cmd Probe Sans} ab} cd")
  expect_equal(r[["ab"]], "Cmd Probe Sans")
  expect_true(is.na(r[["cd"]]))
})

test_that("\\fontspec, \\fontfamily and \\newfontfamily set the font of a group", {
  load_cmd_fonts()
  for (tex in c("{\\fontspec{Cmd Probe Mono} ab} cd",
                "{\\fontfamily{Cmd Probe Mono}\\selectfont ab} cd",
                "\\newfontfamily\\cmdprobe{Cmd Probe Mono} {\\cmdprobe ab} cd",
                "\\newfontface\\cmdface{Cmd Probe Mono} {\\cmdface ab} cd")) {
    r <- record_families(tex)
    expect_equal(r[["ab"]], "Cmd Probe Mono", label = tex)
    expect_true(is.na(r[["cd"]]), label = tex)
  }
  # It composes with the face around it, and wins over a role.
  r <- record_families(
    "\\setmonofont{Cmd Probe Sans}{\\fontspec{Cmd Probe Mono}\\texttt{ab}\\textbf{cd}}")
  expect_equal(r[["ab"]], "Cmd Probe Mono")
  expect_equal(r[["cd"]], "Cmd Probe Mono")
})

test_that("a preamble's font covers the whole body, and a body's only its group", {
  load_cmd_fonts()
  doc <- paste0("\\documentclass{article}\\setmonofont{Cmd Probe Mono}",
                "\\begin{document}\\texttt{ab} text {\\setmonofont{Cmd Probe Sans}\\texttt{cd}} ",
                "\\texttt{ef}\\end{document}")
  r <- record_families(doc, input_mode = "document")
  expect_equal(r[["ab"]], "Cmd Probe Mono")
  expect_equal(r[["cd"]], "Cmd Probe Sans")
  expect_equal(r[["ef"]], "Cmd Probe Mono")
  expect_true(is.na(r[["text"]]))
})

test_that("a font file named with fontspec's options is loaded once", {
  skip_if_not(nzchar(stix()))
  tex <- sprintf(
    "\\setmonofont[Path=%s/, Extension=.otf, UprightFont=*Math-Regular]{STIXTwo}\\texttt{ab}",
    dirname(stix()))
  expect_equal(record_families(tex)[["ab"]], "STIXTwo")
  # The same font again is not loaded again, which would change the layout
  # cache key and so empty the cache at every parse.
  gen <- gridmicrotex:::.font_generation()
  latex_cache_clear()
  expect_equal(record_families(tex)[["ab"]], "STIXTwo")
  expect_identical(gridmicrotex:::.font_generation(), gen)
})

test_that("a font that is not there warns at its line and column and draws the default", {
  expect_warning(
    r <- record_families("ab \\setmonofont{No Such Gridmicrotex Font}\\texttt{cd}"),
    "1:4: \\\\setmonofont: font `No Such Gridmicrotex Font' not found")
  expect_true(is.na(r[["cd"]]))
  expect_warning(record_families("\\fontspec{No Such Gridmicrotex Font} x"),
                 "\\\\fontspec: font `No Such Gridmicrotex Font' not found")
})

test_that("an option the engine does not read is warned and ignored", {
  load_cmd_fonts()
  expect_warning(
    r <- record_families("\\setmonofont[Scale=0.9]{Cmd Probe Mono}\\texttt{ab}"),
    "the option `Scale' is not supported")
  expect_equal(r[["ab"]], "Cmd Probe Mono")
})

test_that("a document's font beats gp$fontfamily, which beats the option", {
  load_cmd_fonts()
  on.exit(reset_latex_options(), add = TRUE)
  body <- function(tex, ...) drawn_families(latex_grob(tex, ...))[["ab"]]
  expect_true(is.na(body("\\text{ab}")))

  latex_options(main_font = "Cmd Probe Mono")
  expect_equal(body("\\text{ab}"), "Cmd Probe Mono")
  expect_equal(body("\\text{ab}", gp = grid::gpar(fontfamily = "serif")), "serif")
  expect_equal(body("\\setmainfont{Cmd Probe Sans}\\text{ab}",
                    gp = grid::gpar(fontfamily = "serif")), "Cmd Probe Sans")

  # NULL gives the default back.
  latex_options(main_font = NULL)
  expect_true(is.na(body("\\text{ab}")))

  # A role set in the document wins over the option, too.
  latex_options(mono_font = "Cmd Probe Mono")
  r <- record_families("\\setmonofont{Cmd Probe Sans}\\texttt{ab}")
  expect_equal(r[["ab"]], "Cmd Probe Sans")
})

test_that("\\gmfontfamily still names a family for one run", {
  r <- record_families("\\gmfontfamily{Georgia}{ab} cd")
  expect_equal(r[["ab"]], "Georgia")
  expect_true(is.na(r[["cd"]]))
})

test_that("\\setmathfont changes the math font of its input", {
  expect_equal(glyph_files("$x^2$", math_font = "lete", render_mode = "typeface"),
               "LeteSansMath.otf")
  expect_equal(glyph_files("\\setmathfont{STIX Two Math} $x^2$", math_font = "lete",
                           render_mode = "typeface"),
               "STIXTwoMath-Regular.otf")
  # Wherever it stands, and by its short name.
  expect_equal(glyph_files("$x^2$ \\setmathfont{stix}", math_font = "lete",
                           render_mode = "typeface"),
               "STIXTwoMath-Regular.otf")

  # A formula has one math font: a second, different one is not used.
  expect_warning(
    f <- glyph_files("\\setmathfont{stix} \\setmathfont{lete} $x$", render_mode = "typeface"),
    "1:20: \\\\setmathfont: a formula has one math font")
  expect_equal(f, "STIXTwoMath-Regular.otf")

  # A font with no math table is not one.
  load_cmd_fonts()
  expect_warning(latex_grob("\\setmathfont{No Such Gridmicrotex Font} $x$"),
                 "\\\\setmathfont: font `No Such Gridmicrotex Font' not found")
})

test_that("an NFSS family code is read as the typeface it names", {
  load_cmd_fonts()
  clear <- function() rm(list = ls(gridmicrotex:::.nfss_cache), envir = gridmicrotex:::.nfss_cache)
  clear()
  on.exit(clear(), add = TRUE)

  # Through an installed family: the first of the typeface's candidates.
  local_mocked_bindings(.nfss_installed = function(families) "Cmd Probe Mono",
                        .package = "gridmicrotex")
  expect_equal(record_families("{\\fontfamily{ppl}\\selectfont ab} cd")[["ab"]],
               "Cmd Probe Mono")
  expect_identical(gridmicrotex:::.nfss_font("ppl"), "Cmd Probe Mono")

  # Neither installed nor in a TeX distribution: a warning, and the default.
  # (The layout of the same string is cached, so that goes too.)
  clear()
  latex_cache_clear()
  local_mocked_bindings(.nfss_installed = function(families) NULL,
                        .nfss_from_tex = function(files) NULL,
                        .package = "gridmicrotex")
  expect_warning(r <- record_families("{\\fontfamily{ppl}\\selectfont ab} cd"),
                 "\\\\fontfamily: font `ppl' not found")
  expect_true(is.na(r[["ab"]]))
  # The miss is remembered, not looked up again.
  expect_identical(gridmicrotex:::.nfss_font("ppl"), "")

  # A name that is not a code is read as a font name.
  expect_null(gridmicrotex:::.nfss_font("Cmd Probe Mono"))
})

test_that("an NFSS code is found in a TeX distribution's OpenType files", {
  skip_if_not(nzchar(Sys.which("kpsewhich")), "no TeX distribution")
  files <- gridmicrotex:::.nfss_table$lmtt$files
  found <- suppressWarnings(system2(Sys.which("kpsewhich"), files[[1L]], stdout = TRUE))
  skip_if(!length(found) || !file.exists(found[[1L]]), "Latin Modern Mono is not installed")

  name <- gridmicrotex:::.nfss_from_tex(files)
  expect_true(is.character(name) && nzchar(name))
  row <- available_fonts()
  row <- row[row$name == name, ]
  expect_true(row$mono)
  expect_identical(row$source, "file")
  expect_true(same_file(row$file, found[[1L]]))
})
