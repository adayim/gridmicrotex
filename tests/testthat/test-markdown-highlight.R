# Helpers shared by the tests below.

# The class of each character of one line, as a plain vector.
hl <- function(line, lang) {
  cls <- .hl_classes(line, lang)
  if (is.null(cls)) NULL else cls[[1]]
}

# The class assigned to the first occurrence of `what` in `line`.
class_of <- function(line, lang, what) {
  cls <- hl(line, lang)
  if (is.null(cls)) return(NA_character_)
  at <- regexpr(what, line, fixed = TRUE)
  if (at < 0) stop("`", what, "` is not in `", line, "`")
  unique(cls[at:(at + nchar(what) - 1L)])
}

# Width of a code line in bigpts, as the emitter renders it.
code_w <- function(line, cls = NULL) {
  tex <- .hl_code_tex(line, cls, function(k) NULL)
  d <- latex_dims(tex, input_mode = "math",
                  gp = grid::gpar(fontsize = 12, fontfamily = "mono"))
  as.numeric(grid::convertWidth(d$width, "bigpts"))
}

# Every LaTeX string the box grob actually builds, gathered by walking
# the children makeContent() produces. Drives the real entry point.
box_tex <- function(md, style = NULL) {
  g <- markdown_box_grob(md, width = grid::unit(4, "in"), style = style)
  out <- character(0)
  walk <- function(x) {
    if (!is.null(x$tex)) out <<- c(out, x$tex)
    for (k in x$children) walk(k)
  }
  walk(grid::makeContent(g))
  out
}

# Write a grammar to a temp file and return its path.
grammar_file <- function(rules, lists = "", lang = "Toy", csens = "1",
                         contexts = "") {
  f <- tempfile(fileext = ".xml")
  writeLines(sprintf(
    '<?xml version="1.0" encoding="UTF-8"?>
     <language name="%s" version="1" kateversion="5.0" casesensitive="%s">
       <highlighting>
         %s
         <contexts>
           <context name="Normal" attribute="Normal Text" lineEndContext="#stay">
             %s
           </context>
           %s
         </contexts>
         <itemDatas>
           <itemData name="Normal Text"  defStyleNum="dsNormal"/>
           <itemData name="Comment"      defStyleNum="dsComment"/>
           <itemData name="String"       defStyleNum="dsString"/>
           <itemData name="Keyword"      defStyleNum="dsKeyword"/>
           <itemData name="Control Flow" defStyleNum="dsControlFlow"/>
           <itemData name="Data Type"    defStyleNum="dsDataType"/>
           <itemData name="Decimal"      defStyleNum="dsDecVal"/>
           <itemData name="Char"         defStyleNum="dsChar"/>
           <itemData name="BaseN"        defStyleNum="dsBaseN"/>
         </itemDatas>
       </highlighting>
     </language>', lang, csens, lists, rules, contexts), f)
  f
}


# --- indentation ------------------------------------------------------

test_that("leading whitespace in a code block survives", {
  # The defect this replaces: a whole line went into one \text{} run and
  # MicroTeX collapses a run of spaces inside one run to a single space,
  # so eight leading spaces measured exactly one space wide.
  one_char <- code_w("xx") - code_w("x")
  base <- code_w("return 1")
  for (n in c(2L, 4L, 8L, 12L)) {
    indented <- code_w(paste0(strrep(" ", n), "return 1"))
    # Within half a character of n characters wider.
    expect_equal(indented - base, n * one_char, tolerance = 0.5 * one_char,
                 info = paste(n, "spaces"))
  }
})

test_that("interior alignment is preserved, not just the left margin", {
  narrow <- code_w("a = 1")
  wide <- code_w("a     = 1")
  one_char <- code_w("xx") - code_w("x")
  expect_equal(wide - narrow, 4 * one_char, tolerance = 0.5 * one_char)
})

test_that("code with no repeated spaces renders exactly as it did before", {
  # Guards the claim that the emitter change is width-neutral for
  # ordinary code: a single space between runs measures the same as a
  # space inside one run, so nothing already in a snapshot moves.
  for (line in c("x<-foo(1)", "a b", "if (x) y else z")) {
    expect_equal(code_w(line),
                 {
                   tex <- paste0("\\texttt{\\text{", .md_escape_tex(line), "}}")
                   d <- latex_dims(tex, input_mode = "math",
                                   gp = grid::gpar(fontsize = 12,
                                                   fontfamily = "mono"))
                   as.numeric(grid::convertWidth(d$width, "bigpts"))
                 },
                 info = line)
  }
})

test_that("~ and ^ are literal characters, not accents", {
  # They used to be escaped to \~{} and \^{}, which are the tilde and
  # circumflex *accents*: `lm(y ~ x)` drew its tilde floating at cap
  # height instead of on the baseline. `^` needs no escape at all, and
  # `~` needs \char126{} -- left bare it is a non-breaking space and the
  # character disappears entirely.
  drawn <- function(line) {
    df <- parse_latex_cpp(.hl_code_tex(line, NULL, function(k) NULL),
                          text_size = 12, use_path = FALSE)
    d <- df[df$type %in% c("text", "glyph"), ]
    paste0(ifelse(is.na(d$text), "<GLYPH>", d$text), collapse = "")
  }
  for (line in c("y ~ x", "a^b", "~/.bashrc", "2^32", "x <- y ~ z")) {
    expect_identical(drawn(line), line, info = line)
  }
  # An accent is drawn as a separate glyph; a literal stays in the run.
  df <- parse_latex_cpp(.hl_code_tex("y ~ x", NULL, function(k) NULL),
                        text_size = 12, use_path = FALSE)
  expect_equal(sum(df$type == "glyph"), 0L)
  # And each occupies exactly one monospace cell, like any operator.
  expect_equal(code_w("y ~ x"), code_w("y - x"))
  expect_equal(code_w("a^b"), code_w("a-b"))
  # The escaper is shared with prose, which had the same fault.
  expect_match(.md_to_tex("approx ~5"), "char126", fixed = TRUE)
  expect_false(grepl("\\~{}", .md_to_tex("approx ~5"), fixed = TRUE))
})

test_that("nothing in a code block is interpreted as math or LaTeX", {
  # Math is masked out of the whole document before CommonMark parses it,
  # and CommonMark cannot tell it about fences. Left masked, the span's
  # *content* is lost and the sentinel's index is drawn in its place:
  # `# see $E = mc^2$ here` came out as `# see 1 here`. Restored here, and
  # then neutralised by the ordinary escaping.
  for (line in c("# see $E = mc^2$ here", "cost is $5 and $10",
                 "s <- \"$x^2$\"", "a $$ b")) {
    tex <- box_tex(paste0("~~~\n", line, "\n~~~\n"))
    # Every dollar survives, escaped, so none can open math.
    expect_equal(lengths(regmatches(tex, gregexpr("\\$", tex, fixed = TRUE))),
                 lengths(regmatches(line, gregexpr("$", line, fixed = TRUE))),
                 info = line)
    expect_false(grepl("\\text{}", tex, fixed = TRUE), info = line)
  }
  # The content itself is intact, not replaced by a sentinel index.
  expect_match(box_tex("~~~\n# see $E = mc^2$ here\n~~~\n"),
               "E = mc^2", fixed = TRUE)
  # And with a language, so the classifier sees the real source too --
  # the dollars are escaped, which is what stops them opening math.
  expect_match(box_tex("~~~r\nx <- \"$a$\"\n~~~\n"), "\\$a\\$", fixed = TRUE)
  # An inline code span was already correct; keep it that way.
  expect_match(.md_to_tex("`$x$`"), "\\$x\\$", fixed = TRUE)
  # Prose outside a fence must still render real math, not a literal.
  expect_match(.md_to_tex("value $x$ here"), "\\text{value }x", fixed = TRUE)
})

test_that("#pop!Context validates its push target", {
  # Unvalidated, an unknown target compiles, the engine then finds no
  # such context, and highlighting dies for the rest of the line *and
  # every line after it* -- the stack never recovers. Exactly the
  # runaway this design refuses everywhere else.
  f <- grammar_file('<DetectChar attribute="String" char="q" context="S"/>',
                    contexts = '<context name="S" attribute="String" lineEndContext="#stay">
                       <DetectChar attribute="String" char="z" context="#pop!Nope"/>
                     </context>')
  expect_error(register_highlighter("popbang", f), "Nope")
  expect_false("popbang" %in% available_highlighters())

  # The valid form still works.
  ok <- grammar_file('<DetectChar attribute="String" char="q" context="S"/>',
                     contexts = '<context name="S" attribute="String" lineEndContext="#stay">
                        <DetectChar attribute="Keyword" char="z" context="#pop!K"/>
                      </context>
                      <context name="K" attribute="Keyword" lineEndContext="#stay"/>')
  expect_silent(register_highlighter("popbang2", ok))
})

test_that("a leading ^ anchors only its own alternative", {
  # Lifting the ^ out into an at_start flag applied it to the whole
  # rule, so `^aaa|bbb` became `aaa|bbb` restricted to column 0 and
  # `bbb` stopped matching anywhere at all.
  f <- grammar_file('<RegExpr attribute="Keyword" String="^aaa|bbb"/>')
  register_highlighter("anch", f)
  expect_identical(class_of("aaa xx bbb", "anch", "aaa"), "kw")
  expect_identical(class_of("aaa xx bbb", "anch", "bbb"), "kw")
  expect_identical(class_of("zz bbb", "anch", "bbb"), "kw")
  # ...and the anchor still bites: `aaa` mid-line is not a match.
  expect_true(is.na(class_of("zz aaa", "anch", "aaa")))
})

test_that("registering under a custom name does not hijack aliases", {
  # `register_highlighter("mybash", bash.xml)` used to repoint ```sh,
  # ```zsh and ```shell at "mybash" -- three fence languages the caller
  # never named.
  expect_identical(.hl_canonical("sh"), "bash")
  register_highlighter("mybash", .hl_grammar_path("bash"))
  expect_identical(.hl_canonical("sh"), "bash")
  expect_true("mybash" %in% available_highlighters())
  # A grammar registered under its own name still contributes aliases.
  register_highlighter("yaml", .hl_grammar_path("yaml"))
  expect_identical(.hl_canonical("yml"), "yaml")
})

test_that("symbol keywords in a <list> match", {
  # \b around the whole alternation cannot bound `+` or `<=>`, so
  # operator keywords silently never matched.
  f <- tempfile(fileext = ".xml")
  writeLines('<?xml version="1.0" encoding="UTF-8"?>
    <language name="Sym" version="1" kateversion="5.0" casesensitive="1">
      <highlighting>
        <list name="ops"><item>+</item><item>&lt;=&gt;</item><item>mod</item></list>
        <contexts><context name="Normal" attribute="Normal Text" lineEndContext="#stay">
          <keyword attribute="Keyword" String="ops"/>
        </context></contexts>
        <itemDatas><itemData name="Normal Text" defStyleNum="dsNormal"/>
          <itemData name="Keyword" defStyleNum="dsKeyword"/>
        </itemDatas></highlighting></language>', f)
  register_highlighter("syms", f)
  expect_identical(class_of("a + b", "syms", "+"), "kw")
  expect_identical(class_of("a <=> b", "syms", "<=>"), "kw")
  expect_identical(class_of("a mod b", "syms", "mod"), "kw")
  # A word keyword still may not match inside an identifier.
  expect_true(is.na(class_of("modulo", "syms", "mod")))
})

test_that("classification is memoised, and re-registering clears it", {
  # .md_layout() runs at draw time, so without this every redraw
  # reclassifies: 0.59s of a 1.09s draw on a 100-line block.
  code <- rep("x <- foo(1)  # hi", 20)
  .hl_state$cache <- list()
  a <- .hl_classes(code, "r")
  expect_length(.hl_state$cache, 1L)
  b <- .hl_classes(code, "r")
  expect_identical(a, b)
  # A different language is a different entry, not a collision.
  .hl_classes(code, "python")
  expect_length(.hl_state$cache, 2L)
  # Registering anything drops it, so an edited grammar takes effect.
  register_highlighter("yaml", .hl_grammar_path("yaml"))
  expect_length(.hl_state$cache, 0L)
})

test_that("a tab advances to the next tab stop, not by a fixed four", {
  # Makefiles have no choice about tabs, and Go is tab-indented by
  # convention; expanding to a fixed four misaligns everything after
  # the first partial column.
  expect_identical(.md_expand_tabs("a\tb"), "a   b")
  expect_identical(.md_expand_tabs("abc\tx"), "abc x")
  expect_identical(.md_expand_tabs("abcd\tx"), "abcd    x")
  expect_identical(.md_expand_tabs("\tx"), "    x")
  expect_identical(.md_expand_tabs("a\tb\tc"), "a   b   c")
  expect_identical(.md_expand_tabs("no tabs"), "no tabs")
  # Column counting restarts per line.
  g <- .md_parse_doc("~~~\nab\tx\ncd\ty\n~~~\n")
  nd <- xml2::xml_find_first(g$doc, "//code_block")
  expect_identical(.md_code_lines(nd, g$spans), c("ab  x", "cd  y"))
})

test_that("tabs become spaces so they can be seen at all", {
  doc <- xml2::read_xml(commonmark::markdown_xml("```\n\tx\n```\n"))
  nd <- xml2::xml_find_first(doc, "//*[local-name()='code_block']")
  expect_identical(.md_code_lines(nd), "    x")
})


# --- the segmentation invariant --------------------------------------

test_that("segments reproduce the source line exactly", {
  # The reason the emitter cannot corrupt code: every segment is a
  # substring of the original line, so no classifier -- ours or a user's
  # -- can drop or duplicate a character.
  cases <- list(
    c("x <- \"a # b\"", "r"),
    c("# say \"hi\"", "python"),
    c("    deep    indent   ", "python"),
    c("aéb # café ü", "python"),
    c("f'{x!r}' + \"unterminated", "python"),
    c("SELECT 'it''s';", "sql"),
    c("<<>>&%$_^~\\{}", "cpp"),
    c("   ", "yaml"),
    c("中文 = 1  # コメント", "python")
  )
  for (cc in cases) {
    cls <- hl(cc[1], cc[2])
    seg <- .hl_segments(cc[1], cls)
    expect_identical(paste0(seg$text, collapse = ""), cc[1], info = cc[1])
    expect_identical(sum(nchar(seg$text)), nchar(cc[1]), info = cc[1])
  }
})

test_that("only runs of spaces are split, and no segment mixes classes", {
  # A run of spaces collapses inside one \text{} and must be split; a
  # lone space does not, and splitting it would multiply the draw
  # records in every code block for nothing.
  expect_identical(.hl_segments("a   b", NULL)$text, c("a", " ", " ", " ", "b"))
  expect_identical(.hl_segments("a b", NULL)$text, "a b")
  expect_identical(.hl_segments("a b c d", NULL)$text, "a b c d")
  expect_identical(.hl_segments("  a", NULL)$text, c(" ", " ", "a"))
  expect_identical(.hl_segments(" ", NULL)$text, " ")
  cls <- hl("x <- 1  # hi", "r")
  seg <- .hl_segments("x <- 1  # hi", cls)
  expect_true(all(vapply(seq_len(nrow(seg)),
                         function(i) length(unique(cls[
                           seq(sum(nchar(seg$text[seq_len(i - 1)])) + 1L,
                               length.out = nchar(seg$text[i]))])) == 1L,
                         logical(1))))
})


# --- the R backend ----------------------------------------------------

test_that("R is classified by R's own parser", {
  line <- "y <- plot(1.5, TRUE)  # note"
  expect_identical(class_of(line, "r", "plot"), "fu")
  expect_identical(class_of(line, "r", "1.5"), "fl")
  expect_identical(class_of(line, "r", "TRUE"), "cn")
  expect_identical(class_of(line, "r", "# note"), "co")
  expect_identical(class_of("if (x) 1L else 2L", "r", "if"), "cf")
  expect_identical(class_of("f <- function(x) x", "r", "function"), "kw")
  expect_identical(class_of("n <- 42L", "r", "42L"), "dv")
})

test_that("a multi-line R string is classified across both lines", {
  # The reason R parses the whole block rather than line by line.
  cls <- .hl_classes(c("s <- \"first", "second\"", "t <- 1"), "r")
  expect_true(all(cls[[1]][6:11] == "st"))
  expect_true(all(cls[[2]][1:7] == "st"))
  expect_identical(unique(cls[[3]][6]), "dv")
})

test_that("an R fragment that does not parse still gets highlighted", {
  # A fenced snippet is very often not a complete program. parse() fails,
  # and the r.xml grammar takes over rather than the block going plain.
  expect_null(tryCatch(parse(text = "for (i in x) {  # note", keep.source = TRUE),
                       error = function(e) NULL))
  expect_identical(class_of("for (i in x) {  # note", "r", "# note"), "co")
  expect_identical(class_of("for (i in x) {  # note", "r", "for"), "cf")
})

test_that("non-ASCII does not shift classes onto neighbouring characters", {
  # getParseData reports byte offsets under some encodings; a span that
  # does not match its own token text is dropped rather than trusted.
  line <- "x <- 1  # café éé"
  cls <- hl(line, "r")
  expect_length(cls, nchar(line))
  at <- regexpr("#", line, fixed = TRUE)
  expect_true(all(cls[at:nchar(line)] == "co"))
  expect_true(all(is.na(cls[1:2])))
})


# --- the grammar engine ----------------------------------------------

test_that("the earliest match wins, whatever order the rules are in", {
  # Both of these come out right without the grammar having to choose.
  # An all-or-nothing "claim if unclaimed" pass gets one of them wrong.
  expect_identical(class_of("# say \"hi\"", "python", "\"hi\""), "co")
  expect_identical(class_of("x = \"a # b\"", "python", "# b\""), "st")
})

test_that("rule order breaks a tie at the same starting position", {
  kw_first <- grammar_file(
    '<keyword attribute="Keyword" String="kw"/>
     <RegExpr attribute="Data Type" String="\\bfoo\\b"/>',
    '<list name="kw"><item>foo</item></list>')
  re_first <- grammar_file(
    '<RegExpr attribute="Data Type" String="\\bfoo\\b"/>
     <keyword attribute="Keyword" String="kw"/>',
    '<list name="kw"><item>foo</item></list>')
  register_highlighter("tie1", kw_first)
  register_highlighter("tie2", re_first)
  expect_identical(class_of("foo", "tie1", "foo"), "kw")
  expect_identical(class_of("foo", "tie2", "foo"), "dt")
})

test_that("defStyleNum maps onto the skylighting class names", {
  f <- grammar_file(
    '<RegExpr attribute="Control Flow" String="c"/>
     <RegExpr attribute="Data Type" String="d"/>
     <RegExpr attribute="Char" String="h"/>
     <RegExpr attribute="Normal Text" String="n"/>')
  register_highlighter("styles", f)
  expect_identical(class_of("c", "styles", "c"), "cf")
  expect_identical(class_of("d", "styles", "d"), "dt")
  expect_identical(class_of("h", "styles", "h"), "ch")
  # dsNormal is not a class: skylighting leaves a NormalTok unmarked.
  expect_true(is.na(class_of("n", "styles", "n")))
})

test_that("each supported rule type matches what it claims to", {
  f <- grammar_file(
    '<DetectChar attribute="Keyword" char="@"/>
     <Detect2Chars attribute="Comment" char="/" char1="/"/>
     <StringDetect attribute="String" String="&lt;&lt;"/>
     <WordDetect attribute="Data Type" String="word"/>
     <AnyChar attribute="Char" String="!?"/>
     <RangeDetect attribute="String" char="[" char1="]"/>
     <HlCHex attribute="BaseN"/>
     <Int attribute="Decimal"/>')
  register_highlighter("rules", f)
  expect_identical(class_of("@", "rules", "@"), "kw")
  expect_identical(class_of("// x", "rules", "//"), "co")
  expect_identical(class_of("a << b", "rules", "<<"), "st")
  expect_identical(class_of("a word b", "rules", "word"), "dt")
  expect_identical(class_of("a ! b", "rules", "!"), "ch")
  expect_identical(class_of("a [xy] b", "rules", "[xy]"), "st")
  expect_identical(class_of("0xFF", "rules", "0xFF"), "bn")
  expect_identical(class_of("42", "rules", "42"), "dv")
  # WordDetect must not match inside a longer identifier.
  expect_true(is.na(class_of("wordy", "rules", "word")))
})

test_that("a rule with no attribute takes its context's, as KDE does", {
  # Not a corner case: 226 of the 714 rules in KDE's own bash.xml leave
  # `attribute` off. Assigning them no class would silently under-colour
  # a third of any grammar downloaded from upstream.
  f <- tempfile(fileext = ".xml")
  writeLines('<?xml version="1.0" encoding="UTF-8"?>
    <language name="Inh" version="1" kateversion="5.0" casesensitive="1">
      <highlighting><contexts>
        <context name="N" attribute="Comment" lineEndContext="#stay">
          <RegExpr String="zap"/>
          <RegExpr attribute="Keyword" String="pow"/>
        </context></contexts>
        <itemDatas>
          <itemData name="Normal Text" defStyleNum="dsNormal"/>
          <itemData name="Comment" defStyleNum="dsComment"/>
          <itemData name="Keyword" defStyleNum="dsKeyword"/>
        </itemDatas></highlighting></language>', f)
  register_highlighter("inherit", f)
  expect_identical(class_of("a zap b", "inherit", "zap"), "co")
  # An explicit attribute still wins over the context's.
  expect_identical(class_of("a pow b", "inherit", "pow"), "kw")
})

test_that("an attribute no <itemData> defines is refused", {
  # A typo here would otherwise produce a grammar that loads happily and
  # colours nothing -- the failure mode this design refuses everywhere else.
  f <- grammar_file('<RegExpr attribute="Typoo" String="zap"/>')
  expect_error(register_highlighter("typo", f), "Typoo")
  expect_false("typo" %in% available_highlighters())
})

test_that("bundled grammar aliases are all in the static alias table", {
  # Aliases are declared twice: in each grammar's alternativeNames and in
  # .HL_ALIASES. Only the static table works before a grammar has been
  # loaded, so an alias present only in the XML resolves or not depending
  # on what else the session rendered first. Pin the invariant.
  for (g in .hl_grammar_names()) {
    for (a in .hl_compile(.hl_grammar_path(g))$aliases) {
      expect_identical(unname(.HL_ALIASES[a]), g,
                       info = paste0(g, ".xml declares alias ", sQuote(a)))
    }
  }
})

test_that("every alias resolves from a cold registry", {
  # The regression the test above guards against, exercised directly.
  old <- list(g = .hl_state$grammars, a = .hl_state$aliases)
  on.exit({ .hl_state$grammars <- old$g; .hl_state$aliases <- old$a }, add = TRUE)
  for (a in names(.HL_ALIASES)) {
    .hl_state$grammars <- list()
    .hl_state$aliases <- list()
    expect_false(is.null(.hl_classes("if x", a)), info = a)
  }
})

test_that("a literal from a grammar is not read as a pattern", {
  # StringDetect, DetectChar, WordDetect and keyword items are literals.
  # An unescaped metacharacter in one would either match the wrong thing
  # or make the whole rule an invalid regex and match nothing.
  bs <- intToUtf8(92)
  for (lit in c("a.c", "x*", "(y)", "[z]", "a+b", paste0("p", bs, "q"), "$v")) {
    esc <- .hl_esc_re(lit)
    expect_true(grepl(esc, lit, perl = TRUE), info = lit)
    # "a.c" must not match "abc": the dot is a literal dot.
    expect_false(grepl(esc, gsub("[^A-Za-z]", "Q", lit), perl = TRUE),
                 info = lit)
  }
})

test_that("a case-insensitive grammar matches either case", {
  f <- grammar_file('<keyword attribute="Keyword" String="kw"/>',
                    '<list name="kw"><item>SELECT</item></list>',
                    csens = "0")
  register_highlighter("ci", f)
  expect_identical(class_of("select 1", "ci", "select"), "kw")
  expect_identical(class_of("SELECT 1", "ci", "SELECT"), "kw")
})

test_that("internal DTD entities are expanded", {
  # The design relies on xml2 doing this: KDE grammars define entities
  # heavily, and an unexpanded &name; would be matched literally.
  f <- tempfile(fileext = ".xml")
  writeLines(
    '<?xml version="1.0" encoding="UTF-8"?>
     <!DOCTYPE language [<!ENTITY num "[0-9]+">]>
     <language name="Ent" version="1" kateversion="5.0" casesensitive="1">
       <highlighting>
         <contexts>
           <context name="Normal" attribute="Normal Text" lineEndContext="#stay">
             <RegExpr attribute="Decimal" String="&num;"/>
           </context>
         </contexts>
         <itemDatas>
           <itemData name="Normal Text" defStyleNum="dsNormal"/>
           <itemData name="Decimal" defStyleNum="dsDecVal"/>
         </itemDatas>
       </highlighting>
     </language>', f)
  register_highlighter("ent", f)
  expect_identical(class_of("x 123", "ent", "123"), "dv")
})


# --- refusing what is not implemented --------------------------------

test_that("an unrepresentable rule that switches context refuses the grammar", {
  # Losing a context switch strands the machine: an unclosed string would
  # paint every following line. These must fail loudly, by name.
  push <- grammar_file('<DetectChar attribute="String" char="&quot;" context="Nowhere"/>')
  expect_error(register_highlighter("bad1", push), "Nowhere")

  dyn <- grammar_file(
    '<RegExpr attribute="String" String="x" dynamic="true" context="Q"/>')
  expect_error(register_highlighter("bad2", dyn), "dynamic")

  cross <- grammar_file('<RegExpr attribute="String" String="x" context="##Doxygen"/>')
  expect_error(register_highlighter("bad3", cross), "another language")

  expect_false(any(c("bad1", "bad2", "bad3") %in% available_highlighters()))
})

test_that("an unrepresentable rule that stays in context is skipped", {
  # The other half of the same principle. Dropping an in-context rule
  # costs colour on the text it would have matched and nothing more, so
  # refusing the whole grammar over it would reject almost every real
  # KDE file for no benefit.
  g <- grammar_file(
    '<RegExpr attribute="String" String="zzz" dynamic="true"/>
     <keyword attribute="Keyword" String="kw"/>',
    '<list name="kw"><item>keep</item></list>')
  register_highlighter("skipped", g)
  # The rule that survived still works...
  expect_identical(class_of("keep zzz", "skipped", "keep"), "kw")
  # ...and the dropped one simply colours nothing.
  expect_true(is.na(class_of("keep zzz", "skipped", "zzz")))
})

test_that("a malformed grammar fails at registration, not at draw time", {
  f <- tempfile(fileext = ".xml")
  writeLines("<not-a-grammar/>", f)
  expect_error(register_highlighter("nope", f), "no <language>")

  writeLines("this is not xml at all", f)
  expect_error(register_highlighter("nope2", f), "as XML")

  empty <- grammar_file("")
  expect_error(register_highlighter("empty", empty), "no usable rules")

  # A keyword list defined in another file, reached through a
  # cross-language include we skipped: nothing usable is left.
  missing_list <- grammar_file('<keyword attribute="Keyword" String="absent"/>')
  expect_error(register_highlighter("ml2", missing_list), "no usable rules")

  expect_error(register_highlighter("gone", tempfile()), "not found")
})


# --- the bundled grammars --------------------------------------------

test_that("every bundled grammar compiles", {
  langs <- .hl_grammar_names()
  expect_gte(length(langs), 10L)
  for (g in langs) {
    expect_silent(gram <- .hl_compile(.hl_grammar_path(g)))
    expect_gt(length(unlist(lapply(gram$contexts, function(x) x$rules),
                            recursive = FALSE)), 0L)
  }
})

test_that("every bundled grammar classifies a keyword, string, comment and number", {
  # line, keyword, string, comment, number. The comment comes last on
  # every line: anything after it is part of the comment, not a token.
  cases <- list(
    r      = list("if (x) s <- \"a\" + 1.5  # c", "if", "\"a\"", "# c", "1.5"),
    python = list("if x: s = \"a\" + 1.5  # c", "if", "\"a\"", "# c", "1.5"),
    sql    = list("SELECT 'a', 1.5 FROM t  -- c", "SELECT", "'a'", "-- c", "1.5"),
    bash   = list("if x; then s=\"a\" 15; fi  # c", "if", "\"a\"", "# c", "15"),
    cpp    = list("if (x) s = \"a\" + 1.5;  // c", "if", "\"a\"", "// c", "1.5"),
    stan   = list("if (x) s = \"a\" + 1.5;  // c", "if", "\"a\"", "// c", "1.5"),
    julia  = list("if x; s = \"a\" + 1.5; end  # c", "if", "\"a\"", "# c", "1.5"),
    yaml   = list("k: \"a\" 1.5  # c", NA, "\"a\"", "# c", "1.5"),
    json   = list("{\"k\": \"a\", \"n\": 1.5}", NA, "\"a\"", NA, "1.5"),
    latex  = list("\\alpha{a} 1.5  % c", "\\alpha", NA, "% c", "1.5")
  )
  expect_setequal(names(cases), .hl_grammar_names())
  for (lang in names(cases)) {
    cc <- cases[[lang]]
    line <- cc[[1]]
    if (!is.na(cc[[2]])) {
      expect_true(class_of(line, lang, cc[[2]]) %in% c("kw", "cf"),
                  info = paste(lang, "keyword"))
    }
    if (!is.na(cc[[3]])) {
      expect_identical(class_of(line, lang, cc[[3]]), "st",
                       info = paste(lang, "string"))
    }
    if (!is.na(cc[[4]])) {
      expect_identical(class_of(line, lang, cc[[4]]), "co",
                       info = paste(lang, "comment"))
    }
    expect_true(class_of(line, lang, cc[[5]]) %in% c("dv", "fl"),
                info = paste(lang, "number"))
  }
})

test_that("a comment after a string containing the comment character", {
  # The case that retired the vectorised fast path: gregexpr consumed the
  # `#` inside the string, so the real trailing comment was never seen.
  # The positional engine is inside the string context there, so its
  # comment rule is not even live.
  expect_identical(class_of('s = "a # b"  # real', "python", "# real"), "co")
  expect_identical(class_of('s = "a # b"  # real', "python", '"a # b"'), "st")
  expect_identical(class_of("x <- \"a # b\"  # real", "r", "# real"), "co")
})

test_that("a multi-context grammar keeps state across lines", {
  # The whole point of contexts: a delimiter opened on one line still
  # governs the next, so a keyword inside a string is not a keyword.
  f <- tempfile(fileext = ".xml")
  writeLines('<?xml version="1.0" encoding="UTF-8"?>
    <language name="Ml" version="1" kateversion="5.0" casesensitive="1">
      <highlighting>
        <list name="kw"><item>keyword</item></list>
        <contexts>
          <context name="Normal" attribute="Normal Text" lineEndContext="#stay">
            <StringDetect attribute="String" String="&lt;&lt;" context="Str"/>
            <keyword attribute="Keyword" String="kw"/>
          </context>
          <context name="Str" attribute="String" lineEndContext="#stay">
            <StringDetect attribute="String" String="&gt;&gt;" context="#pop"/>
          </context>
        </contexts>
        <itemDatas>
          <itemData name="Normal Text" defStyleNum="dsNormal"/>
          <itemData name="String" defStyleNum="dsString"/>
          <itemData name="Keyword" defStyleNum="dsKeyword"/>
        </itemDatas></highlighting></language>', f)
  register_highlighter("ml", f)
  expect_length(.hl_compile(f)$contexts, 2L)

  cls <- .hl_classes(c("keyword << keyword", "keyword", ">> keyword"), "ml")
  # Line 1: the first keyword is a keyword, the one after << is string.
  expect_identical(unique(cls[[1]][1:7]), "kw")
  expect_identical(unique(cls[[1]][12:18]), "st")
  # Line 2 is entirely inside the string opened on line 1.
  expect_identical(unique(cls[[2]]), "st")
  # After >> pops, keywords are keywords again.
  expect_identical(unique(cls[[3]][4:10]), "kw")
})

test_that("KDE's boolean attributes are read in both spellings", {
  # KDE mixes them inside one file: python.xml has lookAhead="1" ninety
  # times and lookAhead="true" not once. Reading only "true" silently
  # turned every look-ahead rule into a consuming one, which mis-coloured
  # every string and comment in every upstream grammar.
  expect_true(.hl_bool("1"))
  expect_true(.hl_bool("true"))
  expect_true(.hl_bool("TRUE"))
  expect_false(.hl_bool("0"))
  expect_false(.hl_bool(NA))
  expect_false(.hl_bool(""))
  expect_true(.hl_bool(NA, default = TRUE))

  f <- tempfile(fileext = ".xml")
  writeLines('<?xml version="1.0" encoding="UTF-8"?>
    <language name="La" version="1" kateversion="5.0" casesensitive="1">
      <highlighting><contexts>
        <context name="Normal" attribute="Normal Text" lineEndContext="#stay">
          <DetectChar attribute="Normal Text" char="q" context="Q" lookAhead="1"/>
        </context>
        <context name="Q" attribute="String" lineEndContext="#stay">
          <DetectChar attribute="String" char="z" context="#pop"/>
        </context></contexts>
        <itemDatas>
          <itemData name="Normal Text" defStyleNum="dsNormal"/>
          <itemData name="String" defStyleNum="dsString"/>
        </itemDatas></highlighting></language>', f)
  register_highlighter("la", f)
  # lookAhead="1" must not consume the q: it belongs to the Str context.
  expect_identical(class_of("aqbz", "la", "q"), "st")
})

test_that("the github preset styles every token class the defaults do", {
  # The preset is a copyable worked example, so a class present in the
  # built-in defaults but missing from the CSS would silently render in
  # the body colour for anyone using the preset.
  st <- markdown_style("github")
  sels <- vapply(st$rules, function(r) r$selector, character(1))
  expect_true(all(paste0(".", names(.MD_CODE_COLORS)) %in% sels))
  # And the two palettes agree, so switching to the preset does not
  # quietly restyle code.
  for (k in names(.MD_CODE_COLORS)) {
    expect_identical(.md_resolve_color(.md_cascade(st, "pre", classes = k)$color),
                     unname(.MD_CODE_COLORS[k]), info = k)
  }
})

test_that("every class a bundled grammar emits has a default colour", {
  # Otherwise a token would be classified and then rendered in the body
  # colour, which looks like the highlighter silently failed.
  for (g in .hl_grammar_names()) {
    gram <- .hl_compile(.hl_grammar_path(g))
    rules <- unlist(lapply(gram$contexts, function(x) x$rules), recursive = FALSE)
    used <- vapply(rules, function(p) p$class, character(1))
    used <- unique(used[!is.na(used)])
    expect_true(all(used %in% names(.MD_CODE_COLORS)),
                info = paste(g, ":", paste(setdiff(used, names(.MD_CODE_COLORS)),
                                           collapse = " ")))
  }
})


# --- language names ---------------------------------------------------

test_that("GFM aliases and knitr's chunk header reach the right grammar", {
  expect_identical(.hl_canonical(.hl_lang("py")), "python")
  expect_identical(.hl_canonical(.hl_lang("c++")), "cpp")
  expect_identical(.hl_canonical(.hl_lang("yml")), "yaml")
  expect_identical(.hl_canonical(.hl_lang("jl")), "julia")
  expect_identical(.hl_canonical(.hl_lang("tex")), "latex")
  expect_identical(.hl_canonical(.hl_lang("sh")), "bash")
  expect_identical(.hl_canonical(.hl_lang("R")), "r")
  # knitr writes the whole chunk header into the info string.
  expect_identical(.hl_canonical(.hl_lang("{r, echo=FALSE}")), "r")
  expect_identical(.hl_canonical(.hl_lang("python title=\"x\"")), "python")
  # A grammar's own alternativeNames are honoured once it is loaded.
  expect_identical(class_of("import x", "python3", "import"), "kw")
})

test_that("an unknown or absent language is silent, not an error", {
  for (lang in list(NA_character_, "", "klingon", NULL)) {
    expect_silent(cls <- .hl_classes("x <- 1", lang))
    expect_null(cls)
  }
})


# --- registration -----------------------------------------------------

test_that("a registered grammar is used and listed", {
  f <- grammar_file('<RegExpr attribute="Keyword" String="\\bzap\\b"/>')
  before <- available_highlighters()
  expect_false("zap" %in% before)
  register_highlighter("zap", f)
  expect_true("zap" %in% available_highlighters())
  expect_identical(class_of("a zap b", "zap", "zap"), "kw")
})

test_that("registering over a bundled name replaces it", {
  f <- grammar_file('<RegExpr attribute="Data Type" String="\\bplot\\b"/>')
  on.exit({
    .hl_state$grammars[["python"]] <- NULL
    .hl_state$user <- setdiff(.hl_state$user, "python")
  }, add = TRUE)
  register_highlighter("python", f)
  expect_identical(class_of("plot(1)", "python", "plot"), "dt")
})


# --- rendering --------------------------------------------------------

test_that("token colours reach the drawn layout", {
  cls <- .hl_classes("foo(1)  # hi", "r")[[1]]
  tex <- .hl_code_tex("foo(1)  # hi", cls,
                      function(k) unname(.MD_CODE_COLORS[k]))
  df <- parse_latex_cpp(tex, text_size = 12, use_path = FALSE)
  got <- unique(df$color)
  expect_true(.MD_CODE_COLORS[["fu"]] %in% got)  # foo
  expect_true(.MD_CODE_COLORS[["dv"]] %in% got)  # 1
  expect_true(.MD_CODE_COLORS[["co"]] %in% got)  # # hi
})

test_that("highlighting survives the whole markdown_box_grob() path", {
  # Everything above works on the emitter directly; this drives the real
  # entry point, so a break in the wiring cannot pass unnoticed.
  md <- "```r\nplot(1)  # hi\n```\n"
  tex <- box_tex(md)
  expect_true(any(grepl(.MD_CODE_COLORS[["fu"]], tex, fixed = TRUE)))
  expect_true(any(grepl(.MD_CODE_COLORS[["co"]], tex, fixed = TRUE)))
  # And a fence with no language stays plain all the way through.
  expect_false(any(grepl("textcolor", box_tex("```\nplot(1)  # hi\n```\n"),
                         fixed = TRUE)))
})

test_that("a stylesheet overrides one token class and only that one", {
  md <- "```r\nplot(1)  # hi\n```\n"
  themed <- box_tex(md, style = ".co { color: #123456 }")
  expect_true(any(grepl("#123456", themed, fixed = TRUE)))
  # The rule names only .co, so the function colour is untouched.
  expect_true(any(grepl(.MD_CODE_COLORS[["fu"]], themed, fixed = TRUE)))
  expect_false(any(grepl(.MD_CODE_COLORS[["co"]], themed, fixed = TRUE)))
})

test_that("a token whose colour equals the block colour emits no markup", {
  # Why the emitter compares against the block colour rather than always
  # wrapping: colouring a token the colour it already is would bloat
  # every line with markup that changes nothing.
  md <- "```r\nplot(1)\n```\n"
  tex <- box_tex(md, style = "pre { color: #112233 } .fu { color: #112233 }")
  expect_false(any(grepl("textcolor{#112233}", tex, fixed = TRUE)))
})

test_that("visual: a highlighted code block", {
  skip_if_not_installed("vdiffr")
  skip_on_os("mac")
  # Evidence, not coverage: this block skips on CRAN, so nothing above is
  # delegated to it. vdiffr writes one family name for every family, so
  # the mono run is recorded as `font-family: sans` here -- that is the
  # snapshot device, not the layout. The colours are real.
  vdiffr::expect_doppelganger("markdown-highlight", function() {
    grid::grid.draw(markdown_box_grob(
      "```r\nfit <- lm(y ~ x)  # refit\nif (bad) {\n    stop(\"no\")\n}\n```\n",
      width = grid::unit(3.4, "in"),
      padding = grid::unit(8, "pt"),
      box_gp = grid::gpar(fill = "grey96", col = "grey40"),
      gp = grid::gpar(fontsize = 12)
    ))
  })
})

test_that("an unhighlighted block emits no colour markup at all", {
  # Keeps a plain fence byte-identical to what it rendered before, which
  # is what stops every existing snapshot from moving.
  for (lang in c(NA_character_, "klingon")) {
    cls <- .hl_classes("x <- 1", lang)
    tex <- .hl_code_tex("x <- 1", if (is.null(cls)) NULL else cls[[1]],
                        function(k) unname(.MD_CODE_COLORS[k]))
    expect_false(grepl("textcolor", tex, fixed = TRUE))
  }
})
