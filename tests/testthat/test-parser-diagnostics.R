# Malformed input is drawn as far as it can be, and each problem is one line
# of a single warning, at its line:col. Only a capacity running out -- a
# macro that expands forever -- is an error (see test-latex-grob.R).

# What is drawn, leaving out the problems found (the layout carries them).
layout_quietly <- function(tex) {
  t <- suppressWarnings(latex_tree(tex, input_mode = "math", render_mode = "path"))
  records <- t$records
  attr(records, "diagnostics") <- NULL
  list(records = records, bbox = t$bbox)
}

test_that("one warning lists every problem, in order, at its line:col", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  w <- character()
  withCallingHandlers(
    latex_grob("a\n\\foo b\n\\bar", input_mode = "math"),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") }
  )
  expect_length(w, 1L)
  expect_equal(
    strsplit(w, "\n", fixed = TRUE)[[1]],
    c("LaTeX input: ",
      "  2:1: unknown command \\foo: drawn as its name",
      "  3:5: missing argument for \\bar: an empty one is used")
  )
})

test_that("a cached layout warns again", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  latex_cache_clear(); on.exit(latex_cache_clear(), add = TRUE)
  expect_warning(latex_grob("x^{2", input_mode = "math"), "1:3: missing } inserted")
  expect_warning(latex_grob("x^{2", input_mode = "math"), "1:3: missing } inserted")
})

test_that("valid input does not warn", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_warning(latex_grob(
    "\\begin{pmatrix} a & b \\\\ c & d \\end{pmatrix} + \\left( \\frac12 \\right)",
    input_mode = "math"
  ))
})

test_that("an unclosed environment is closed at the end of the input", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # bmatrix is defined in LaTeX source, so its closing bracket comes from
  # its \end; without one the bracket went missing.
  for (env in c("bmatrix", "pmatrix", "matrix", "cases")) {
    open <- sprintf("\\begin{%s} a & b", env)
    expect_warning(latex_grob(open, input_mode = "math"),
                   sprintf("missing \\end{%s} inserted", env), fixed = TRUE)
    expect_identical(layout_quietly(open),
                     layout_quietly(sprintf("%s \\end{%s}", open, env)), info = env)
  }
})

test_that("a stray \\end is left out, with a warning", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  for (env in c("array", "bmatrix")) {
    tex <- sprintf("x \\end{%s} y", env)
    expect_warning(latex_grob(tex, input_mode = "math"),
                   sprintf("\\end{%s} without \\begin ignored", env), fixed = TRUE)
    expect_identical(layout_quietly(tex), layout_quietly("x y"), info = env)
  }
})

test_that("an unknown environment draws its body, with a warning", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_warning(g <- latex_grob("\\begin{quote}hello\\end{quote}", input_mode = "math"),
                 "unknown environment quote", fixed = TRUE)
  expect_false("#FF0000" %in% g$layout_df$color)
  expect_identical(layout_quietly("\\begin{quote}hello\\end{quote}"),
                   layout_quietly("\\begin{matrix}hello\\end{matrix}"))
})

test_that("an environment inside an argument or a list is read", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Arguments are handed to the engine's commands as text, which the old
  # parser had already rewritten; an environment in one came out red.
  nested <- "\\begin{itemize} \\item a \\begin{itemize}\\item b\\end{itemize} \\end{itemize}"
  d <- latex_grob(nested, input_mode = "math", render_mode = "path")$layout_df
  expect_false("#FF0000" %in% d$color)
  g <- latex_grob("\\frac{1}{\\begin{matrix} a \\\\ b \\end{matrix}}", input_mode = "math")
  expect_false("#FF0000" %in% g$layout_df$color)
})

test_that("$$ inside \\text does not open display math", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # TeX reads $$ in restricted horizontal mode (\hbox, \text) as an empty
  # formula; read as display math, it swallowed the closing brace.
  expect_no_warning(latex_grob("\\text{a$$}", input_mode = "math"))
  expect_identical(layout_quietly("\\text{a$$}"), layout_quietly("\\text{a}"))
})

test_that("a delimited macro whose delimiter never comes is left out", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # As after TeX's runaway-argument error, the call goes and the rest is
  # read as it stands. Taking the rest as the argument made a macro that
  # calls itself copy the whole input into every expansion (fuzzing: ~2 s).
  for (tex in c("\\def\\a#1.{[#1]}\\a x", "\\def\\a#1.{\\a#1}\\a x")) {
    expect_warning(latex_grob(tex, input_mode = "math"), "missing its delimiter; it is left out",
                   fixed = TRUE, label = tex)
    expect_identical(layout_quietly(tex), layout_quietly("x"), info = tex)
  }
  expect_warning(latex_grob("\\def\\q[#1]{<#1>}\\q x", input_mode = "math"),
                 "does not match its definition; it is left out", fixed = TRUE)
  expect_identical(layout_quietly("\\def\\q[#1]{<#1>}\\q x"), layout_quietly("x"))
})

test_that("an abandoned call puts back what every argument read", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # #1 found its `.`, #2 never found its `!`: all of `x.y z` is read again,
  # not only what #2 read.
  tex <- "\\def\\a#1.#2!{[#1|#2]} \\a x.y z"
  expect_warning(latex_grob(tex, input_mode = "math"), "missing its delimiter", fixed = TRUE)
  expect_identical(layout_quietly(tex), layout_quietly("x.y z"))
})

test_that("an environment left open inside a group ends with the group", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Its expansion opened a group of its own, which took the `}` meant for
  # the argument and put every brace after it out by one.
  env <- "\\newenvironment{gmp}{\\left(}{\\right)}"
  tex <- paste0(env, "\\frac{\\begin{gmp} a}{b} c")
  expect_warning(latex_grob(tex, input_mode = "math"), "missing \\end{gmp} inserted",
                 fixed = TRUE)
  expect_identical(layout_quietly(tex),
                   layout_quietly(paste0(env, "\\frac{\\begin{gmp} a\\end{gmp}}{b} c")))
})

test_that("an environment in an optional argument keeps its name", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_warning(latex_grob("\\sqrt[\\begin{matrix}3\\end{matrix}]{x}", input_mode = "math"))
})

test_that("a comment inside an argument is not drawn", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # parse_latex_cpp() directly: R's own passes strip comments first, but
  # the engine does not rely on them.
  drawn <- function(tex) {
    r <- gridmicrotex:::parse_latex_cpp(tex, use_path = TRUE)
    attr(r, "diagnostics") <- NULL
    r
  }
  expect_identical(drawn("\\text{a % c\nb}"), drawn("\\text{a \nb}"))
  expect_identical(drawn("\\frac{a % c\n}{b}"), drawn("\\frac{a\n}{b}"))
  expect_identical(drawn("\\sqrt{a % }\n}"), drawn("\\sqrt{a\n}"))
})

test_that("a runaway argument in a loop is an error, not a slow render", {
  # Each abandoned call reads the rest of the input again; in a macro that
  # calls itself that was once per turn, 18 s for a 200-byte input.
  tex <- paste0("\\def\\gmrun#1.{}\\newcommand{\\gmloop}{\\gmrun\\gmloop}\\gmloop",
                strrep("1", 2000))
  expect_error(latex_dims(tex, input_mode = "math"), "Too many runaway arguments")
})

test_that("a delimiter that is not one is left out, with a warning", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Found only at layout, it failed the whole label: TeX puts a null
  # delimiter in its place.
  expect_warning(latex_grob("\\left( a \\middle x b \\right)", input_mode = "math"),
                 "\\middle: x is not a delimiter; left out", fixed = TRUE)
  expect_identical(layout_quietly("\\left( a \\middle x b \\right)"),
                   layout_quietly("\\left( a \\middle. b \\right)"))
  expect_warning(latex_grob("\\left x a \\right)", input_mode = "math"),
                 "\\left: x is not a delimiter", fixed = TRUE)
  # In a command the engine builds, the command is drawn as its name.
  expect_warning(g <- latex_grob("\\genfrac{x}{y}{0pt}{}{a}{b} + c", input_mode = "math"),
                 "x is not a delimiter", fixed = TRUE)
  expect_true("#FF0000" %in% g$layout_df$color)
})

test_that("primes and a superscript apart are a double superscript, as in TeX", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  for (tex in c("f'_1'", "f'_1^2")) {
    expect_warning(latex_grob(tex, input_mode = "math"), "double superscript", label = tex)
  }
  expect_identical(layout_quietly("f'_1^2"), layout_quietly("{f'_1}^2"))
})

test_that("a definition that cannot be made is dropped whole", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  for (tex in c("\\newcommand{x}[1]{y}z", "\\def{x}{y}z", "\\newenvironment{}{a}{b}z",
                "\\newcommand{\\gmfoo}[x]{BODY}z", "\\newenvironment{gme}[x]{B}{E}z")) {
    expect_warning(g <- latex_grob(tex, input_mode = "math"), label = tex)
    expect_identical(layout_quietly(tex), layout_quietly("z"), info = tex)
  }
})
