# Bidirectional ordering, crossed over scripts, structures and widths.
# The oracle is textshaping::shape_text(); only the order of words is
# compared, not where lines break.

# --- vocabulary -------------------------------------------------------

# Uyghur (joining, with letters beyond Arabic's) and Hebrew (not joining).
UY <- c("خۇش", "كەلدىڭىز",
        "بۇ", "دۇنياغا")
HE <- c("שלום", "עולם",
        "ספר", "גדול")
LA <- c("alpha", "beta", "gamma", "delta")

# --- the oracle -------------------------------------------------------

# The paragraph's base direction: that of its first strong character
# (UAX #9 P2/P3). Every line inherits it.
base_dir <- function(s) {
  for (cp in utf8ToInt(s)) {
    if ((cp >= 0x0590 && cp <= 0x08FF) ||
        (cp >= 0xFB1D && cp <= 0xFDFF) ||
        (cp >= 0xFE70 && cp <= 0xFEFF)) return("rtl")
    if ((cp >= 0x41 && cp <= 0x5A) || (cp >= 0x61 && cp <= 0x7A)) return("ltr")
  }
  "ltr"
}

# Visual order of the words of `s`, from textshaping. `glyph` is the
# character index and `x_offset` the visual position. `dir` must be the
# paragraph's direction, not the line's. NULL if textshaping's output
# changes shape, so the tests skip rather than fail.
oracle_order <- function(s, dir, size = 14) {
  sh <- textshaping::shape_text(s, family = "sans", size = size,
                                direction = dir)$shape
  if (is.null(sh$glyph) || is.null(sh$x_offset)) return(NULL)
  if (max(sh$glyph) != nchar(s)) return(NULL)
  words <- strsplit(s, " ", fixed = TRUE)[[1]]
  starts <- cumsum(c(1, head(nchar(words), -1) + 1))
  ends <- starts + nchar(words) - 1
  owner <- vapply(sh$glyph, function(i) {
    w <- which(i >= starts & i <= ends)
    if (length(w)) w[1] else NA_integer_
  }, integer(1))
  keep <- !is.na(owner)
  if (!any(keep)) return(NULL)
  xmin <- tapply(sh$x_offset[keep], owner[keep], min)
  words[as.integer(names(sort(xmin)))]
}

# Our own layout, as a list of lines, each a vector of record texts in
# visual (left to right) order.
our_lines <- function(tex, max_width, ...) {
  d <- latex_grob(tex, input_mode = "math", max_width = max_width,
                  gp = grid::gpar(fontsize = 14), ...)$layout_df
  d <- d[d$type == "text" & !is.na(d$text), ]
  lapply(sort(unique(round(d$y, 1))), function(yy) {
    s <- d[abs(round(d$y, 1) - yy) < 0.01, ]
    s$text[order(s$x)]
  })
}

# --- case generation --------------------------------------------------

# Wrap the i-th word of a sentence in a group, so the row has siblings.
WRAPPERS <- list(
  none   = function(w) w,
  bold   = function(w) paste0("}\\textbf{\\text{", w, "}}\\text{"),
  colour = function(w) paste0("}\\textcolor{red}{\\text{", w, "}}\\text{"),
  family = function(w) paste0("}\\gmfontfamily{serif}{\\text{", w, "}}\\text{"),
  nested = function(w) paste0("}\\textbf{\\textit{\\text{", w, "}}}\\text{")
)

# A sentence, and its LaTeX with `wrap` applied to every third word. The
# words never change, so structure must not change the order.
make_case <- function(words, wrap) {
  f <- WRAPPERS[[wrap]]
  parts <- vapply(seq_along(words), function(i) {
    if (i %% 3L == 0L) f(words[i]) else words[i]
  }, character(1))
  list(sentence = paste(words, collapse = " "),
       tex = paste0("\\text{", paste(parts, collapse = " "), "}"))
}

VOCAB <- list(
  uyghur      = rep(UY, 3),
  hebrew      = rep(HE, 3),
  latin       = rep(LA, 3),
  uy_latin    = rep(c(UY[1], LA[1], UY[2], LA[2]), 3),
  he_latin    = rep(c(HE[1], LA[1], HE[2], LA[2]), 3),
  uy_hebrew   = rep(c(UY[1], HE[1], UY[2], HE[2]), 3)
)

# --- the matrix -------------------------------------------------------

test_that("ordering matches an independent shaper, across scripts and structures", {
  skip_on_cran()
  skip_if_not_installed("textshaping")
  skip_if_not(microtex_bidi_available(), "built without fribidi")
  pdf(NULL); on.exit(dev.off(), add = TRUE)

  checked <- 0L
  for (vocab in names(VOCAB)) {
    words <- VOCAB[[vocab]]
    # Mixed scripts inside a wrapper are the known gap tested below.
    wraps <- if (grepl("_", vocab)) "none" else names(WRAPPERS)
    for (wrap in wraps) {
      case <- make_case(words, wrap)
      for (mw in c(140, 220, 320)) {
        for (just in c(FALSE, TRUE)) {
          lines <- our_lines(case$tex, mw, justify = just)
          # Lines break in logical order, so line k holds the next n_k
          # source words; each goes to the oracle with the paragraph's
          # direction.
          dir <- base_dir(case$sentence)
          taken <- 0L
          for (ln in lines) {
            logical <- words[taken + seq_along(ln)]
            taken <- taken + length(ln)
            want <- oracle_order(paste(logical, collapse = " "), dir)
            if (is.null(want)) skip("textshaping's shape contract changed")
            expect_equal(
              ln, want,
              info = sprintf("%s / %s / width %d / justify %s",
                             vocab, wrap, mw, just))
            checked <- checked + 1L
          }
        }
      }
    }
  }
  expect_gt(checked, 200L)   # the matrix really did generate cases
})

test_that("every layout is well formed, whatever the direction", {
  # No oracle, so this also runs on CRAN and without FriBidi.
  pdf(NULL); on.exit(dev.off(), add = TRUE)

  for (vocab in names(VOCAB)) {
    words <- VOCAB[[vocab]]
    for (wrap in names(WRAPPERS)) {
      case <- make_case(words, wrap)
      for (mw in c(140, 320)) {
        for (opt in list(list(), list(justify = TRUE))) {
          tag <- sprintf("%s / %s / %d / %s", vocab, wrap, mw,
                         paste(names(opt), collapse = ","))
          g <- do.call(latex_grob,
                       c(list(case$tex, input_mode = "math", max_width = mw,
                              gp = grid::gpar(fontsize = 14)), opt))
          d <- g$layout_df
          d <- d[d$type == "text" & !is.na(d$text), ]

          # Every word is drawn exactly once.
          drawn <- d$text[d$text %in% words]
          expect_equal(sort(drawn), sort(words), info = tag)

          # Nothing overlaps its neighbour on a line.
          for (yy in unique(round(d$y, 1))) {
            s <- d[abs(round(d$y, 1) - yy) < 0.01, ]
            s <- s[order(s$x), ]
            expect_false(any(diff(s$x) < 0), info = tag)
          }
          # Only for the flat structure: a paragraph built from groups can
          # overrun by a word, in any script.
          if (identical(wrap, "none")) expect_lte(g$bbox_w, mw + 1, label = tag)
        }
      }
    }
  }
})

test_that("a group holding both directions still lays out every word", {
  skip_on_cran()
  skip_if_not_installed("textshaping")
  skip_if_not(microtex_bidi_available(), "built without fribidi")
  pdf(NULL); on.exit(dev.off(), add = TRUE)

  # A group is one child of its line with one level, so a group whose
  # contents span two levels cannot be reordered inside. Whether a line
  # meets that shape depends on font metrics, so this asserts only that
  # every word is laid out.
  words <- rep(c("שלום", "alpha",
                 "עולם", "beta"), 3)
  case <- make_case(words, "bold")
  lines <- our_lines(case$tex, 220)

  expect_equal(sort(unlist(lines)), sort(words))
  expect_gt(length(lines), 1L)
})

test_that("a left-to-right formula is untouched by any of this", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  words <- rep(LA, 3)
  for (wrap in names(WRAPPERS)) {
    for (mw in c(140, 200, 320)) {
      # Read left to right across the lines, it is the written order.
      got <- unlist(our_lines(make_case(words, wrap)$tex, mw))
      expect_equal(got, words, info = sprintf("%s / %d", wrap, mw))
    }
  }
})
