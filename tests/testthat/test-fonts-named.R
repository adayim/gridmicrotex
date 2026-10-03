# load_font() and the font options of latex_options(): a font named once,
# from a file or an installed family, and used by that name. The bundled
# fonts stand in for "a font file" so that no test needs a file of its own.

stix <- function() {
  f <- system.file("fonts", "STIXTwoMath-Regular.otf", package = "gridmicrotex")
  skip_if_not(nzchar(f))
  f
}
lete <- function() {
  f <- system.file("fonts", "LeteSansMath.otf", package = "gridmicrotex")
  skip_if_not(nzchar(f))
  f
}

# The family each text run of a label is drawn in, named by its text.
drawn_families <- function(g) {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  kids <- grid::makeContent(g)$children
  txt <- Filter(function(k) inherits(k, "text"), unclass(kids))
  stats::setNames(
    vapply(txt, function(k) if (is.null(k$gp$fontfamily)) NA_character_ else k$gp$fontfamily, character(1)),
    vapply(txt, function(k) trimws(as.character(k$label)), character(1))
  )
}

same_file <- function(a, b) {
  identical(normalizePath(a, winslash = "/"), normalizePath(b, winslash = "/"))
}

test_that("load_font() registers a file under a name and lists it", {
  expect_identical(load_font(stix(), name = "Named Probe A"), "Named Probe A")

  fonts <- available_fonts()
  row <- fonts[fonts$name == "Named Probe A", ]
  expect_equal(nrow(row), 1L)
  expect_true(row$math)
  expect_false(row$mono)
  expect_identical(row$source, "file")
  expect_true(same_file(row$file, stix()))
  # The bundled fonts are listed too, as bundled.
  expect_true(all(c("Lete Sans Math", "STIX Two Math") %in% fonts$name))
  expect_identical(fonts$source[fonts$name == "Lete Sans Math"], "bundled")

  # systemfonts, and so ragg and svglite, resolve the name to that file.
  expect_true(same_file(systemfonts::match_fonts("Named Probe A")$path, stix()))
})

test_that("a name is found whatever its case, and a math font loaded as text is also a math font", {
  load_font(lete(), name = "Named Probe Alias")
  expect_true("Named Probe Alias" %in% available_fonts()$name)
  expect_identical(gridmicrotex:::.font_lookup("named probe alias"),
                   "Named Probe Alias")
  expect_identical(gridmicrotex:::.font_lookup("stix"), "STIX Two Math")
  expect_null(gridmicrotex:::.font_lookup("Named Probe Nope"))

  # The alias works as math_font, too.
  expect_s3_class(
    latex_grob("$x^2$", math_font = "Named Probe Alias",
               gp = grid::gpar(fontsize = 12)),
    "latexgrob"
  )
})

test_that("bold and italic files are registered as the faces of the family", {
  load_font(stix(), name = "Named Probe B", bold = lete())
  row <- available_fonts()
  row <- row[row$name == "Named Probe B", ]
  expect_true(row$bold)
  expect_false(row$italic)
  expect_true(same_file(
    systemfonts::match_fonts("Named Probe B", weight = "bold")$path, lete()))
  expect_true(same_file(systemfonts::match_fonts("Named Probe B")$path, stix()))

  # Left out, bold is the regular file, and the list says so.
  load_font(stix(), name = "Named Probe B2")
  expect_false(available_fonts()$bold[available_fonts()$name == "Named Probe B2"])
})

test_that("load_font() of an installed family finds its faces", {
  fam <- tryCatch(systemfonts::font_info(family = "sans")$family[1],
                  error = function(e) NA_character_)
  skip_if(is.na(fam) || !nzchar(fam), "no installed font to load")
  expect_identical(load_font(fam), fam)
  row <- available_fonts()
  row <- row[row$name == fam, ]
  expect_identical(row$source, "system")
  expect_true(file.exists(row$file))

  # A short name for an installed family.
  expect_identical(load_font(fam, name = "Named Probe Sys"), "Named Probe Sys")
  expect_true(same_file(systemfonts::match_fonts("Named Probe Sys")$path,
                        row$file))
})

test_that("load_font() refuses what it cannot load", {
  expect_error(load_font("NoSuchFamilyForGridmicrotex"),
               "neither a font file nor an installed family")
  expect_error(load_font(file.path(tempdir(), "nothing.otf")),
               "Font file not found")
  expect_error(load_font(stix(), bold = file.path(tempdir(), "nothing.otf")),
               "Font file not found")
  expect_error(load_font(c("a", "b")), "single string")
  expect_error(load_font(stix(), name = ""), "single string")

  fam <- tryCatch(systemfonts::font_info(family = "sans")$family[1],
                  error = function(e) NA_character_)
  skip_if(is.na(fam), "no installed font to load")
  expect_error(load_font(fam, bold = stix()), "installed family")
})

test_that("load_math_font() still loads a math font and still refuses text fonts", {
  expect_silent(load_math_font(stix()))
  expect_true("STIX Two Math" %in% available_math_fonts())

  # An installed text font has no MATH table -- unless this machine's "mono"
  # has one, and then it cannot stand in for one.
  file <- systemfonts::match_fonts("mono")$path
  skip_if(!nzchar(file) || !file.exists(file))
  msg <- tryCatch({ load_math_font(file); NULL },
                  error = function(e) conditionMessage(e))
  skip_if(is.null(msg), "this machine's mono font has a math table")
  expect_match(msg, "MATH table")
})

test_that("loading a font changes the layout cache key", {
  key <- function() {
    gridmicrotex:::.parse_cache_key(
      "\\text{a}", 20, 10, "#000000", 0, "", "", "Named Probe C", TRUE, "",
      FALSE, FALSE)
  }
  before <- key()
  gen <- gridmicrotex:::.font_generation()
  load_font(stix(), name = "Named Probe C")
  expect_gt(gridmicrotex:::.font_generation(), gen)
  expect_false(identical(key(), before))

  # And so do the roles.
  before <- key()
  on.exit(reset_latex_options(), add = TRUE)
  latex_options(mono_font = "Named Probe C")
  expect_false(identical(key(), before))
})

test_that("mono_font and sans_font set the font of \\texttt and \\textsf", {
  on.exit(reset_latex_options(), add = TRUE)
  load_font(stix(), name = "Named Probe M")
  load_font(lete(), name = "Named Probe S")
  tex <- "\\texttt{ab} \\textsf{cd} ef"
  records <- function() {
    d <- latex_grob(tex)$layout_df
    d <- d[d$type == "text" & nzchar(trimws(d$text)), ]
    stats::setNames(d$font_family, trimws(d$text))
  }

  # The defaults stand while nothing is set -- the device's own "mono" and
  # "sans", which the records leave unnamed.
  expect_true(all(is.na(records())))
  expect_equal(drawn_families(latex_grob(tex))[c("ab", "cd")],
               c(ab = "mono", cd = "sans"))

  old <- latex_options(mono_font = "Named Probe M")
  expect_equal(records()[["ab"]], "Named Probe M")
  expect_true(is.na(records()[["cd"]]))
  expect_equal(drawn_families(latex_grob(tex))[c("ab", "cd")],
               c(ab = "Named Probe M", cd = "sans"))

  latex_options(sans_font = "named probe s")
  expect_equal(latex_options()$sans_font, "Named Probe S")
  expect_equal(drawn_families(latex_grob(tex))[c("ab", "cd")],
               c(ab = "Named Probe M", cd = "Named Probe S"))
  # The body is neither.
  expect_true(is.na(records()[["ef"]]))

  # NULL gives the defaults back, and so does replaying what was returned.
  latex_options(mono_font = NULL, sans_font = NULL)
  expect_true(all(is.na(records())))
  do.call(latex_options, old)
  expect_null(latex_options()$mono_font)
  expect_true(all(is.na(records())))
})

test_that("main_font is the body font, and gp$fontfamily wins over it", {
  on.exit(reset_latex_options(), add = TRUE)
  load_font(stix(), name = "Named Probe Body")
  tex <- "\\text{ef} \\texttt{ab}"

  expect_true(is.na(drawn_families(latex_grob(tex))[["ef"]]))

  latex_options(main_font = "Named Probe Body")
  fam <- drawn_families(latex_grob(tex))
  expect_equal(fam[["ef"]], "Named Probe Body")
  # \texttt is not the body.
  expect_equal(fam[["ab"]], "mono")

  fam <- drawn_families(latex_grob(tex, gp = grid::gpar(fontfamily = "serif")))
  expect_equal(fam[["ef"]], "serif")
})

test_that("a font option takes a file, an installed family or one of R's families", {
  on.exit(reset_latex_options(), add = TRUE)
  # A file is loaded, and the option records its name.
  latex_options(mono_font = stix())
  expect_identical(latex_options()$mono_font, "STIX Two Math")

  latex_options(mono_font = "serif")
  expect_identical(latex_options()$mono_font, "serif")

  expect_error(latex_options(mono_font = "NoSuchFamilyForGridmicrotex"),
               "`mono_font`.*neither a font file")
  expect_error(latex_options(main_font = c("a", "b")), "`main_font`")
  # A refused value leaves the setting as it was.
  expect_identical(latex_options()$mono_font, "serif")

  reset_latex_options()
  expect_null(latex_options()$mono_font)
})
