# The lexer of the new front end, checked through its token table. Each
# expectation is TeX's own rule for the input (The TeXbook, ch. 7-8).

lex <- function(tex, ...) gridmicrotex:::lex_latex_cpp(tex, ...)

# "kind:text" for every token but the final `end`.
toks <- function(tex, ...) {
  t <- lex(tex, ...)
  t <- t[t$kind != "end", ]
  paste0(t$kind, ":", t$text)
}

test_that("control words, control symbols and characters are told apart", {
  expect_identical(toks("\\alpha\\,x"),
                   c("control_word:alpha", "control_symbol:,", "char:x"))
  expect_identical(toks("\\\\a"), c("control_symbol:\\", "char:a"))
  expect_identical(toks("\\frac12"),
                   c("control_word:frac", "char:1", "char:2"))
})

test_that("a control word swallows the spaces after it, a control symbol does not", {
  expect_identical(toks("\\alpha   x"), c("control_word:alpha", "char:x"))
  expect_identical(toks("\\, x"), c("control_symbol:,", "space: ", "char:x"))
  # a control space is followed by skipped blanks, like a control word
  expect_identical(toks("a\\   b"), c("char:a", "control_symbol: ", "char:b"))
})

test_that("a run of spaces is one space token", {
  expect_identical(toks("a  \t b"), c("char:a", "space: ", "char:b"))
})

test_that("a line end is a space and the next line's leading spaces are dropped", {
  expect_identical(toks("a\n    b"), c("char:a", "space: ", "char:b"))
  expect_identical(toks("a\r\nb"), c("char:a", "space: ", "char:b"))
  expect_identical(toks("a\rb"), c("char:a", "space: ", "char:b"))
})

test_that("a blank line is a paragraph only when asked for", {
  expect_identical(toks("a\n\nb", blank_line_is_par = TRUE),
                   c("char:a", "space: ", "par:", "char:b"))
  expect_identical(toks("a\n  \n b", blank_line_is_par = TRUE),
                   c("char:a", "space: ", "par:", "char:b"))
  expect_identical(toks("a\n\nb"), c("char:a", "space: ", "char:b"))
})

test_that("a comment runs to the end of the line and takes the line end", {
  expect_identical(toks("a% note\nb"), c("char:a", "char:b"))
  expect_identical(toks("a % note\n   b"), c("char:a", "space: ", "char:b"))
  expect_identical(toks("50\\% off"),
                   c("char:5", "char:0", "control_symbol:%", "space: ",
                     "char:o", "char:f", "char:f"))
  # a comment before a blank line still leaves the paragraph break
  expect_identical(toks("a%\n\nb", blank_line_is_par = TRUE),
                   c("char:a", "par:", "char:b"))
})

test_that("line ends TeX drops are still counted on the next token", {
  t <- lex("\\alpha\nb")
  expect_identical(t$kind[1:2], c("control_word", "char"))
  expect_identical(t$line_ends[2], 1L)
  t <- lex("a\nb")
  expect_identical(t$line_ends, c(0L, 1L, 0L, 0L))
})

test_that("special characters get TeX's category codes", {
  t <- lex("{}$&#^_~a+")
  expect_identical(t$cat[t$kind == "char"], c(1L, 2L, 3L, 4L, 6L, 7L, 8L, 13L, 11L, 12L))
})

test_that("a multi-byte character is one token, and columns count characters", {
  t <- lex("\u03b1\u03b2")
  t <- t[t$kind == "char", ]
  expect_identical(t$cp, c(0x3B1L, 0x3B2L))
  expect_identical(t$col, c(1L, 2L))
  expect_identical(t$offset, c(0L, 2L))
  expect_identical(t$text, c("\u03b1", "\u03b2"))
})

test_that("joined and variation-selected characters stay one token", {
  woman_technologist <- "\U0001F469\u200D\U0001F4BB"
  check_mark <- "\u2714\uFE0F"
  expect_identical(toks(woman_technologist), paste0("char:", woman_technologist))
  expect_identical(toks(check_mark), paste0("char:", check_mark))
})

test_that("positions follow lines", {
  t <- lex("a\n  \\beta x")
  beta <- t[t$text == "beta", ]
  expect_identical(c(beta$line, beta$col, beta$offset), c(2L, 3L, 4L))
})

test_that("invalid UTF-8 becomes U+FFFD with a warning at its position", {
  bad <- rawToChar(as.raw(c(0x61, 0xFF, 0x62)))
  Encoding(bad) <- "bytes"
  t <- lex(bad)
  expect_identical(t$cp[t$kind == "char"], c(0x61L, 0xFFFDL, 0x62L))
  d <- attr(t, "diagnostics")
  expect_identical(nrow(d), 1L)
  expect_identical(c(d$line, d$col), c(1L, 2L))
  expect_match(d$message, "invalid UTF-8")
})

test_that("a backslash at the end is dropped with a warning", {
  t <- lex("x\\")
  expect_identical(t$kind, c("char", "end"))
  expect_match(attr(t, "diagnostics")$message, "backslash at the end")
})

test_that("a byte-order mark is not text", {
  expect_identical(toks("\uFEFFx"), "char:x")
})
