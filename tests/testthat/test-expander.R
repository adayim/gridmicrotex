# The macro expander of the new front end, checked through its text output.
# Each expectation is what TeX / LaTeX make of the input.

ex <- function(tex) as.character(gridmicrotex:::expand_latex_cpp(tex))
diag_of <- function(tex) attr(gridmicrotex:::expand_latex_cpp(tex), "diagnostics")

test_that("text with no user macro comes back byte for byte", {
  for (tex in c("a  % note\n  b\\alpha{}", "\\frac12 + \\sqrt[3]{x}",
                "\\begin{matrix} a & b \\\\ c & d \\end{matrix}",
                "\\text{50\\% off} x^{\\prime}", "\\left( x \\right.  ",
                "α✔️ x", "a\\")) {
    expect_identical(ex(tex), tex, info = tex)
  }
})

test_that("parameters are substituted in one pass", {
  # `#2` inside an argument is the argument's text, not a parameter.
  expect_equal(ex("\\newcommand{\\A}[2]{#1,#2}\\A{\\#2}{z}"), "\\#2,z")
  expect_equal(ex("\\newcommand{\\A}[2]{#1,#2}\\A{#2}{z}"), "#2,z")
  expect_equal(ex("\\newcommand{\\twice}[1]{#1#1}\\twice{ab}"), "abab")
})

test_that("an undelimited argument is one token or one group, never a byte", {
  expect_equal(ex("\\newcommand{\\sq}[1]{#1^2}\\sq α"), "α^2")
  expect_equal(ex("\\newcommand{\\sq}[1]{#1^2}\\sq{ab}"), "ab^2")
  # A command argument is the command alone, as in TeX.
  expect_equal(ex("\\newcommand{\\sq}[1]{(#1)}\\sq\\alpha"), "(\\alpha)")
})

test_that("the optional first argument of \\newcommand takes its default", {
  expect_equal(ex("\\newcommand{\\p}[2][x]{#1_#2}\\p{1}\\p[y]{2}"), "x_1y_2")
})

test_that("starred definition commands define", {
  expect_equal(ex("\\newcommand*{\\f}{x}\\f"), "x")
  expect_equal(ex("\\newcommand\\g{y}\\g"), "y")
  expect_equal(ex("\\DeclareMathOperator{\\am}{am}\\am"), "\\mathop{\\mathrm{am}}\\nolimits")
  expect_equal(ex("\\DeclareMathOperator*{\\am}{am}\\am"), "\\mathop{\\mathrm{am}}\\limits")
})

test_that("starred built-ins become forms the parser reads", {
  expect_equal(ex("\\operatorname*{x}"), "\\mathop{\\mathrm{x}}\\limits")
  expect_equal(ex("\\hspace*{1em}"), "\\hspace{1em}")
  expect_equal(ex("a\\\\*b"), "a\\\\b")
})

test_that("\\def reads delimited parameters", {
  expect_equal(ex("\\def\\foo#1.{[#1]}\\foo abc."), "[abc]")
  # one pair of braces around a delimited argument is removed
  expect_equal(ex("\\def\\p#1,#2.{(#1;#2)}\\p{a,b},c."), "(a,b;c)")
  # a delimiter before the first parameter must be there
  expect_equal(ex("\\def\\q[#1]{<#1>}\\q[x]"), "<x>")
  d <- diag_of("\\def\\q[#1]{<#1>}\\q x")
  expect_match(d$message, "does not match its definition")
})

test_that("## in a definition makes # in the expansion", {
  expect_equal(ex("\\def\\a{\\def\\b##1{<##1>}}\\a\\b{z}"), "<z>")
})

test_that("\\let copies a meaning, and a built-in stays built in", {
  # The classic save-and-redefine does not loop.
  expect_equal(ex("\\let\\oldfrac\\frac\\renewcommand{\\frac}[2]{\\oldfrac{#2}{#1}}\\frac{a}{b}"),
               "\\frac{b}{a}")
  expect_equal(ex("\\newcommand{\\x}{X}\\let\\y\\x\\renewcommand{\\x}{Z}\\y\\x"), "XZ")
  expect_equal(ex("\\let\\c=y\\c"), "y")
})

test_that("\\providecommand defines only what does not exist", {
  expect_equal(ex("\\newcommand{\\x}{1}\\providecommand{\\x}{2}\\x"), "1")
  expect_equal(ex("\\providecommand{\\y}{3}\\y"), "3")
})

test_that("redefinition follows LaTeX's rules", {
  expect_error(ex("\\newcommand{\\frac}{Q}"), "already exists")
  expect_error(ex("\\renewcommand{\\nosuch}{Q}"), "no defined")
  expect_equal(ex("\\renewcommand{\\frac}{Q}\\frac"), "Q")
  expect_equal(ex("\\def\\x{1}\\def\\x{2}\\x"), "2")
  expect_error(ex("\\newcommand{x}{y}"), "Invalid name")
  expect_error(ex("\\def{notacs}{body}"), "expected")
})

test_that("user environments expand at \\begin and \\end, as a group", {
  # The braces make the environment one atom, as the old parser made it.
  expect_equal(ex("\\newenvironment{pm}{\\begin{pmatrix}}{\\end{pmatrix}}\\begin{pm}a\\end{pm}"),
               "{\\begin{pmatrix}a\\end{pmatrix}}")
  expect_equal(ex("\\newenvironment{bx}[1]{[#1:}{]}\\begin{bx}{t}x\\end{bx}"), "{[t:x]}")
  expect_error(ex("\\newenvironment{matrix}{}{}"), "already exists")
})

test_that("a definition made in a group ends with it, as in TeX", {
  # Local: outside the group the name means what it meant before, and
  # nothing when it meant nothing.
  expect_equal(ex("{\\newcommand{\\aa}{x}\\aa}\\aa"), "{x}\\aa")
  expect_equal(ex("\\newcommand{\\q}{a}{\\renewcommand{\\q}{b}\\q}\\q"), "{b}a")
  expect_equal(ex("\\def\\x{1}{\\def\\x{2}\\x}\\x"), "{2}1")
  # \gdef defines globally, as in TeX; \def does not.
  expect_equal(ex("{\\gdef\\g{1}\\def\\d{2}}\\g\\d"), "{}1\\d")
  # A definition at the top level outlives a group that ends after it.
  expect_equal(ex("\\def\\t{4}{x}\\t"), "{x}4")
  # An environment is a group too, including its own.
  expect_equal(ex("\\newenvironment{e}{}{}\\begin{e}\\def\\h{3}\\h\\end{e}\\h"), "{3}\\h")
  expect_equal(ex("{\\newenvironment{f}{[}{]}\\begin{f}x\\end{f}}\\begin{f}y\\end{f}"),
               "{{[x]}}\\begin{f}y\\end{f}")
})

test_that("two tokens that were apart do not run together", {
  expect_equal(ex("\\newcommand{\\x}[1]{#1\\beta}\\x{a}c"), "a\\beta c")
  expect_equal(ex("\\makeatletter\\def\\a@b{Q}\\a@b\\makeatother"),
               "\\makeatletter Q\\makeatother")
})

test_that("runaway recursion stops with an error", {
  expect_error(ex("\\def\\a{\\a}\\a"), "Too many macro expansions")
  expect_error(ex("\\def\\a#1{\\a{#1#1}}\\a{x}"), "too large|Too many")
})

test_that("a missing argument is reported, not fatal", {
  d <- diag_of("\\newcommand{\\f}[1]{<#1>}\\f")
  expect_match(d$message, "missing argument for \\\\f")
  expect_equal(ex("\\newcommand{\\f}[1]{<#1>}\\f"), "<>")
})
