# Syntax highlighting for fenced code blocks.
#
# Grammars are KDE/Kate syntax XML -- the format Kate, KDevelop and
# (through skylighting) Pandoc all read, so a user can point at a file
# from that ecosystem rather than one only this package understands.
# None of KDE's own files are shipped: most are GPL or LGPL and this
# package is MIT. A file *format* is not copyrightable, so the grammars
# in inst/highlight are written here, in KDE's format.
#
# Two backends produce the same thing -- one class per character of a
# line, NA meaning "leave it plain", which is skylighting's own rule that
# a NormalTok is not marked up at all:
#
#   R          utils::getParseData(), i.e. R's own parser. Exact.
#   everything else   the grammar engine below.
#
# The per-character vector is the whole reason the emitter cannot corrupt
# a line: it indexes the original string with substring(), so characters
# can be neither lost nor duplicated no matter what a grammar does.

# --- the class vocabulary --------------------------------------------

# KDE's defStyleNum enum maps 1:1 onto skylighting's short CSS class
# names, which is why this file has no naming decisions in it: the names
# below are what Pandoc -- and therefore knitr -- already emits, taken
# from skylighting's own `short` function. dsNormal is deliberately NA.
.HL_DEF_STYLE <- c(
  dsNormal         = NA_character_,
  dsKeyword        = "kw", dsFunction       = "fu", dsVariable    = "va",
  dsControlFlow    = "cf", dsOperator       = "op", dsBuiltIn     = "bu",
  dsExtension      = "ex", dsPreprocessor   = "pp", dsAttribute   = "at",
  dsChar           = "ch", dsSpecialChar    = "sc", dsString      = "st",
  dsVerbatimString = "vs", dsSpecialString  = "ss", dsImport      = "im",
  dsDataType       = "dt", dsDecVal         = "dv", dsBaseN       = "bn",
  dsFloat          = "fl", dsConstant       = "cn", dsComment     = "co",
  dsDocumentation  = "do", dsAnnotation     = "an", dsCommentVar  = "cv",
  dsInformation    = "in", dsWarning        = "wa", dsAlert       = "al",
  dsError          = "er", dsOthers         = "ot", dsRegionMarker = "re"
)

# Default colour per class, consumed by .md_default_rules(). GitHub's
# light palette: comment grey, string dark blue, keyword red, constant
# and number blue, function and built-in purple, type and variable
# brown. Only the classes the bundled grammars actually emit appear
# here, so nothing in this table is speculative.
.MD_CODE_COLORS <- c(
  co = "#59636E", ot = "#59636E",
  st = "#0A3069", ch = "#0A3069",
  kw = "#CF222E", cf = "#CF222E", pp = "#CF222E",
  cn = "#0550AE", dv = "#0550AE", bn = "#0550AE", fl = "#0550AE",
  at = "#0550AE", sc = "#0550AE",
  fu = "#8250DF", bu = "#8250DF",
  dt = "#953800", va = "#953800"
)

# --- language names --------------------------------------------------

# GFM spellings that should reach a grammar filed under another name.
# A grammar's own `alternativeNames` is registered on top of this when it
# loads, so a downloaded file brings its aliases with it.
.HL_ALIASES <- c(
  rscript = "r",
  py = "python", python3 = "python",
  sh = "bash", shell = "bash", zsh = "bash",
  "c++" = "cpp", cc = "cpp", cxx = "cpp", c = "cpp", hpp = "cpp",
  yml = "yaml",
  jl = "julia",
  tex = "latex",
  postgresql = "sql", mysql = "sql", sqlite = "sql"
)

# An info string carries more than a language name: knitr writes
# ```{r, echo=FALSE} and other tools append attributes. Take the first
# word and lowercase it, which is also what every markdown renderer does.
.hl_lang <- function(info) {
  # xml_attr() gives NA, not NULL, for a fence with no info string.
  if (length(info) != 1L || is.na(info)) return("")
  s <- trimws(info)
  if (!nzchar(s)) return("")
  s <- sub("^\\{+", "", s)
  s <- strsplit(s, "[[:space:],;}]+")[[1]][1]
  if (is.na(s) || !nzchar(s)) return("")
  tolower(s)
}

.hl_canonical <- function(lang) {
  if (!nzchar(lang)) return("")
  a <- .hl_state$aliases[[lang]] %||% unname(.HL_ALIASES[lang])
  if (is.null(a) || is.na(a)) lang else a
}

# --- registry --------------------------------------------------------

# Compiled grammars, keyed by canonical name, plus the alias map that
# grammars contribute as they load. Populated lazily on first use rather
# than in .onLoad: startup stays free, and nothing touches the filesystem
# until a code block actually asks for it.
.hl_state <- new.env(parent = emptyenv())
.hl_state$grammars <- list()
.hl_state$aliases <- list()
.hl_state$user <- character(0)
# Memoised classification, keyed on language + source. See .hl_classes().
.hl_state$cache <- list()

# Locate a bundled grammar. Mirrors .md_preset_path() -- grammars are
# data files rather than R code for the same reason the CSS presets are:
# each one doubles as a worked example a user can copy.
.hl_grammar_path <- function(name) {
  if (!grepl("^[a-z0-9_+-]+$", name)) return(NULL)
  f <- system.file("highlight", paste0(name, ".xml"), package = "gridmicrotex")
  if (!nzchar(f) || !file.exists(f)) NULL else f
}

.hl_grammar_names <- function() {
  d <- system.file("highlight", package = "gridmicrotex")
  if (!nzchar(d)) return(character(0))
  sub("[.]xml$", "", list.files(d, pattern = "[.]xml$"))
}

# Resolve a canonical name to a compiled grammar, compiling and caching a
# bundled file on first use. NULL when there is no such grammar, which is
# how an unrecognised fence ends up rendering plain.
.hl_grammar <- function(lang) {
  if (!nzchar(lang)) return(NULL)
  g <- .hl_state$grammars[[lang]]
  if (!is.null(g)) return(g)
  f <- .hl_grammar_path(lang)
  if (is.null(f)) return(NULL)
  .hl_store(lang, .hl_compile(f))
}

.hl_store <- function(lang, g, own = TRUE) {
  .hl_state$grammars[[lang]] <- g
  # `own` says the grammar is filed under the name it belongs to: a
  # bundled file, or a user registration that used the grammar's own
  # name. Otherwise its alternativeNames are ignored, because
  # `register_highlighter("mybash", bash.xml)` would quietly repoint
  # ```sh, ```zsh and ```shell at "mybash" -- hijacking three fence
  # languages the caller never named. (The bundled names cannot simply
  # be compared against g$name: cpp.xml declares "C++".)
  if (isTRUE(own)) {
    for (a in g$aliases) if (nzchar(a)) .hl_state$aliases[[a]] <- lang
  }
  g
}

# --- KDE XML -> an ordered pattern list -------------------------------

# Every PCRE metacharacter, backslash included -- a literal string from a
# grammar (StringDetect, a keyword item) must not be read as a pattern.
.hl_esc_re <- function(x) {
  gsub("([\\\\^$.|?*+()\\[\\]{}])", "\\\\\\1", x, perl = TRUE)
}

# Rule elements this engine implements. Anything else makes the grammar
# refuse to register rather than render approximately -- a half-supported
# grammar that silently colours the wrong things is worse than one that
# says it is not supported.
.HL_RULES <- c("keyword", "RegExpr", "DetectChar", "Detect2Chars",
               "StringDetect", "WordDetect", "AnyChar", "RangeDetect",
               "Int", "Float", "HlCOct", "HlCHex", "HlCChar",
               "HlCStringChar", "LineContinue",
               "DetectSpaces", "DetectIdentifier")

.hl_compile <- function(file) {
  # Internal DTD entities are expanded by xml2 without asking, which the
  # KDE files lean on heavily (python.xml defines a dozen).
  doc <- tryCatch(xml2::read_xml(file), error = function(e)
    stop("Could not parse ", sQuote(basename(file)), " as XML: ",
         conditionMessage(e), call. = FALSE))
  lang_nd <- if (identical(xml2::xml_name(doc), "language")) doc else
    xml2::xml_find_first(doc, "//language")
  if (inherits(lang_nd, "xml_missing")) {
    stop("Not a syntax grammar: no <language> element in ", sQuote(basename(file)),
         ".", call. = FALSE)
  }

  name <- tolower(xml2::xml_attr(lang_nd, "name") %|NA% "")
  alt <- xml2::xml_attr(lang_nd, "alternativeNames") %|NA% ""
  aliases <- tolower(trimws(strsplit(alt, "[;,]")[[1]]))
  aliases <- aliases[nzchar(aliases)]
  csens <- .hl_bool(xml2::xml_attr(lang_nd, "casesensitive"), default = TRUE)

  # attribute name -> CSS class, via defStyleNum.
  items <- xml2::xml_find_all(lang_nd, ".//itemDatas/itemData")
  attr_cls <- unname(.HL_DEF_STYLE[xml2::xml_attr(items, "defStyleNum")])
  names(attr_cls) <- xml2::xml_attr(items, "name")

  ctxs <- xml2::xml_find_all(lang_nd, ".//contexts/context")
  if (!length(ctxs)) {
    stop("Grammar ", sQuote(basename(file)), " has no <context>.", call. = FALSE)
  }
  # Keyword lists, by name.
  lists <- list()
  for (l in xml2::xml_find_all(lang_nd, ".//list")) {
    lists[[xml2::xml_attr(l, "name")]] <-
      trimws(xml2::xml_text(xml2::xml_find_all(l, "./item")))
  }

  bad <- character(0)
  note <- function(x) bad <<- c(bad, x)

  ctx_names <- xml2::xml_attr(ctxs, "name")

  # One rule element -> the compiled form the engine runs, or NULL.
  #
  # A rule this engine cannot represent is *skipped* when it would have
  # stayed in the current context, and *refuses the whole grammar* when
  # it would have switched. The asymmetry is the point: a dropped
  # in-context rule costs colour on the text it would have matched, which
  # is a visible but local loss. A dropped context switch leaves the
  # machine in the wrong state for the rest of the file -- an unclosed
  # string painting every following line -- which is the runaway
  # mis-colouring this design refuses everywhere.
  one_rule <- function(r, ctx_attr) {
    kind <- xml2::xml_name(r)
    target <- xml2::xml_attr(r, "context") %|NA% "#stay"
    switches <- !identical(target, "#stay")
    give_up <- function(what) {
      if (switches) note(what)
      NULL
    }
    if (!kind %in% .HL_RULES) return(give_up(paste0("<", kind, ">")))
    if (.hl_bool(xml2::xml_attr(r, "dynamic"))) {
      # %1 substitution from the match. A dynamic rule is usually the one
      # that closes a here-doc or a custom delimiter, so skipping one that
      # switches context would strand the machine.
      return(give_up(paste0("<", kind, " dynamic>")))
    }
    if (startsWith(target, "##")) {
      return(give_up(paste0("<", kind, " context=\"", target,
                            "\"> (another language)")))
    }
    re <- tryCatch(.hl_rule_regex(r, kind, lists, file),
                   error = function(e) NULL)
    if (is.null(re)) {
      # Most often a keyword list defined in another file, reached through
      # a cross-language include.
      return(give_up(paste0("<", kind, "> (unresolved ",
                            xml2::xml_attr(r, "String") %|NA% "rule", ")")))
    }
    col <- xml2::xml_attr(r, "column") %|NA% ""
    if (nzchar(col) && !identical(col, "0")) {
      return(give_up(paste0("<", kind, " column=\"", col, "\">")))
    }
    # A leading `^` is left in the pattern rather than lifted into
    # at_start. The engine matches `^.{k}\K(?:re)` against the whole
    # line, so an inner `^` still means start-of-line and simply fails
    # for k > 0 -- which is the correct reading. Lifting it applied the
    # anchor to the *whole* rule, so `^aaa|bbb` became `aaa|bbb`
    # restricted to column 0 and `bbb` stopped matching anywhere.
    #
    # column="0" and firstNonSpace are different: they are attributes of
    # the rule, not of the pattern, so the engine answers them from the
    # scan position instead of a lookbehind PCRE2 would reject.
    at_start <- identical(col, "0")
    first_non_space <- .hl_bool(xml2::xml_attr(r, "firstNonSpace"))
    if (.hl_bool(xml2::xml_attr(r, "insensitive")) ||
        (!csens && kind %in% c("keyword", "WordDetect", "StringDetect"))) {
      re <- paste0("(?i)", re)
    }
    a <- xml2::xml_attr(r, "attribute") %|NA% ctx_attr
    if (nzchar(a) && !a %in% names(attr_cls)) {
      # Always fatal: an attribute naming nothing is an authoring typo,
      # and a rule that matches but paints nothing is the silent failure
      # this package refuses to ship.
      note(paste0("attribute=\"", a, "\" (no <itemData> defines it)"))
      return(NULL)
    }
    list(re = paste0("(?:", re, ")"),
         class = if (nzchar(a)) unname(attr_cls[[a]]) else NA_character_,
         action = .hl_action(target, ctx_names, note),
         look_ahead = .hl_bool(xml2::xml_attr(r, "lookAhead")),
         at_start = at_start, first_non_space = first_non_space)
  }

  # Compile every context, keeping <IncludeRules> unresolved for now.
  ctx <- list()
  for (i in seq_along(ctxs)) {
    nd <- ctxs[[i]]
    own_attr <- xml2::xml_attr(nd, "attribute") %|NA% ""
    rules <- list()
    for (r in xml2::xml_children(nd)) {
      if (identical(xml2::xml_name(r), "IncludeRules")) {
        tgt <- xml2::xml_attr(r, "context") %|NA% ""
        # `##Language` pulls in another grammar file -- almost always
        # ##Alerts, ##Modelines or ##Doxygen, which only add TODO/FIXME
        # style marks *inside* a comment. Skipping one loses those marks
        # and nothing else: the comment is still a comment, because the
        # context that made it one is untouched. It never switches
        # context itself, so it is always safe to drop.
        if (!startsWith(tgt, "##") && tgt %in% ctx_names) {
          rules[[length(rules) + 1L]] <- list(include = tgt)
        }
        next
      }
      cr <- one_rule(r, own_attr)
      if (!is.null(cr)) rules[[length(rules) + 1L]] <- cr
    }
    ctx[[ctx_names[i]]] <- list(
      rules = rules,
      # A character no rule matches takes the context's own attribute --
      # this is how the *body* of a string or comment gets coloured at
      # all. Only the delimiters have rules of their own; everything
      # between them is unmatched text inside the pushed context.
      class = if (nzchar(own_attr) && own_attr %in% names(attr_cls))
        unname(attr_cls[[own_attr]]) else NA_character_,
      line_end = .hl_action(xml2::xml_attr(nd, "lineEndContext") %|NA% "#stay",
                            ctx_names, note),
      # fallthroughContext is frequently "#pop", not a context name, so
      # it has to go through the same action parser as everything else.
      fallthrough = {
        ft <- xml2::xml_attr(nd, "fallthroughContext") %|NA% ""
        if (nzchar(ft) && !startsWith(ft, "##"))
          .hl_action(ft, ctx_names, note) else NULL
      })
  }

  # Splice includes in, depth-first, with a visited set so a cycle cannot
  # loop forever.
  resolve <- function(nm, seen = character(0)) {
    if (nm %in% seen) return(list())
    out <- list()
    for (r in ctx[[nm]]$rules) {
      if (!is.null(r$include)) {
        out <- c(out, resolve(r$include, c(seen, nm)))
      } else {
        out[[length(out) + 1L]] <- r
      }
    }
    out
  }
  for (nm in names(ctx)) ctx[[nm]]$rules <- resolve(nm)

  if (length(bad)) {
    stop("Grammar ", sQuote(basename(file)), " uses constructs this ",
         "highlighter does not implement: ",
         paste(unique(bad), collapse = ", "), ".", call. = FALSE)
  }
  if (!length(unlist(lapply(ctx, function(c) c$rules), recursive = FALSE))) {
    stop("Grammar ", sQuote(basename(file)), " defines no usable rules.",
         call. = FALSE)
  }

  list(name = name, aliases = aliases, contexts = ctx, start = ctx_names[1])
}

# A KDE context target -> what the engine does to the stack.
#   #stay            nothing
#   #pop, #pop#pop   pop that many
#   #popN            pop N
#   #pop!Name        pop one, then push Name
#   Name             push Name
.hl_action <- function(s, ctx_names, note) {
  s <- s %|NA% "#stay"
  if (!nzchar(s) || identical(s, "#stay")) return(list(op = "stay"))
  if (startsWith(s, "#pop")) {
    push <- NA_character_
    bang <- regmatches(s, regexpr("![^#!]+$", s))
    if (length(bang) && nzchar(bang)) {
      push <- substring(bang, 2L)
      s <- sub("![^#!]+$", "", s)
      # Validated exactly as a plain push is. Without this an unknown
      # target compiles, the engine then finds no such context, and
      # highlighting dies for the rest of the line *and every line
      # after it* -- the stack never recovers.
      if (!push %in% ctx_names) {
        note(paste0("context=\"#pop!", push, "\" (no such <context>)"))
        push <- NA_character_
      }
    }
    num <- regmatches(s, regexpr("(?<=#pop)[0-9]+", s, perl = TRUE))
    n <- if (length(num) && nzchar(num)) as.integer(num) else
      length(gregexpr("#pop", s, fixed = TRUE)[[1]])
    return(list(op = "pop", n = max(1L, n), push = push))
  }
  if (startsWith(s, "#")) return(list(op = "stay"))
  if (!s %in% ctx_names) {
    note(paste0("context=\"", s, "\" (no such <context>)"))
    return(list(op = "stay"))
  }
  list(op = "push", name = s)
}

.hl_push <- function(stack, a) {
  switch(a$op,
    stay = stack,
    pop = {
      stack <- stack[seq_len(max(1L, length(stack) - a$n))]
      if (!is.na(a$push)) c(stack, a$push) else stack
    },
    push = c(stack, a$name),
    stack)
}

# One KDE rule element -> one PCRE pattern.
.hl_rule_regex <- function(r, kind, lists, file) {
  at <- function(a) xml2::xml_attr(r, a) %|NA% ""
  switch(kind,
    keyword = {
      nm <- at("String")
      words <- lists[[nm]]
      if (is.null(words) || !length(words)) {
        stop("Grammar ", sQuote(basename(file)), " refers to keyword list ",
             sQuote(nm), ", which it does not define.", call. = FALSE)
      }
      # Longest first so `else if` cannot be shadowed by `else`, and so
      # `<=>` wins over `<`.
      words <- words[nzchar(words)]
      words <- words[order(nchar(words), decreasing = TRUE)]
      # A word boundary is added per item, not around the whole group: a
      # symbol keyword such as `+` or `<=>` has no word character to
      # bound, so a blanket \b(?:...)\b made it impossible to match.
      # Operator keywords are common in imported grammars.
      lead <- ifelse(grepl("^\\w", words), "\\b", "")
      trail <- ifelse(grepl("\\w$", words), "\\b", "")
      paste0("(?:",
             paste0(lead, .hl_esc_re(words), trail, collapse = "|"), ")")
    },
    RegExpr = at("String"),
    DetectChar = .hl_esc_re(at("char")),
    Detect2Chars = paste0(.hl_esc_re(at("char")), .hl_esc_re(at("char1"))),
    StringDetect = .hl_esc_re(at("String")),
    WordDetect = paste0("\\b", .hl_esc_re(at("String")), "\\b"),
    AnyChar = paste0("[", gsub("([\\\\^\\]\\[-])", "\\\\\\1", at("String"),
                               perl = TRUE), "]"),
    RangeDetect = paste0(.hl_esc_re(at("char")), "[^", .hl_esc_re(at("char1")),
                         "]*", .hl_esc_re(at("char1"))),
    Int = "\\b[0-9]+\\b",
    Float = "\\b[0-9]*\\.[0-9]+(?:[eE][-+]?[0-9]+)?\\b|\\b[0-9]+\\.(?![.0-9])",
    HlCOct = "\\b0[0-7]+\\b",
    HlCHex = "\\b0[xX][0-9a-fA-F]+\\b",
    HlCChar = "'(?:\\\\(?:[abefnrtv?\"'\\\\]|x[0-9a-fA-F]+|[0-7]{1,3})|[^\\\\'])'",
    HlCStringChar =
      "\\\\(?:[abefnrtv?\"'\\\\]|x[0-9a-fA-F]+|[0-7]{1,3}|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8})",
    LineContinue = "\\\\$",
    DetectSpaces = "[ \\t]+",
    DetectIdentifier = "[A-Za-z_][A-Za-z0-9_]*",
    NULL
  )
}

# --- the engine ------------------------------------------------------

# Kate's own algorithm: at each position, try the current context's rules
# in order and take the first that matches there; assign its class,
# apply its context switch, and resume past what it consumed. The
# context stack carries across lines via lineEndContext, which is what
# lets a string or block comment span them.
#
# Scanning by position rather than pre-finding every match is what makes
# a *context* grammar possible at all -- the set of live rules changes
# mid-line -- and it also gets these right without the grammar having to
# choose between them:
#
#   # say "hi"      the comment starts first, so it takes the whole line
#   x <- "a # b"    the string starts first, so the # inside is not a comment
.hl_patterns <- function(lines, g) {
  stack <- g$start
  out <- vector("list", length(lines))

  for (li in seq_along(lines)) {
    line <- lines[li]
    n <- nchar(line)
    cls <- rep(NA_character_, n)
    pos <- 1L
    nonblank <- attr(regexpr("^[ \t]*", line), "match.length") + 1L
    # Bounds the work done without consuming a character, so a lookAhead
    # rule that never advances, or a pair of fallthroughs that bounce
    # between two contexts, cannot hang a plot.
    spin <- 0L

    while (pos <= n) {
      cur <- g$contexts[[stack[length(stack)]]]
      if (is.null(cur)) break
      # `\K` drops the skipped prefix from the reported match, so the
      # rule is tried at exactly `pos` while the characters to its left
      # stay visible. Matching a substr() instead would hide them, and
      # \b would then fire inside a word -- colouring the `in` of
      # `begin` as a keyword -- while every lookbehind silently failed.
      # Verified that .{k} counts characters, not bytes.
      pre <- paste0("^.{", pos - 1L, "}\\K")

      hit <- NULL
      len <- 0L
      for (r in cur$rules) {
        if (r$at_start && pos != 1L) next
        if (r$first_non_space && pos != nonblank) next
        m <- tryCatch(regexpr(paste0(pre, r$re), line, perl = TRUE,
                              useBytes = FALSE),
                      error = function(e) -1L)
        if (m[1] != pos) next
        hit <- r
        len <- attr(m, "match.length")
        break
      }

      if (is.null(hit)) {
        if (!is.null(cur$fallthrough) && spin < 20L) {
          stack <- .hl_push(stack, cur$fallthrough)
          spin <- spin + 1L
          next
        }
        cls[pos] <- cur$class
        pos <- pos + 1L
        spin <- 0L
        next
      }

      if (!hit$look_ahead && len > 0L) {
        cls[pos:min(pos + len - 1L, n)] <- hit$class
      }
      stack <- .hl_push(stack, hit$action)
      if (hit$look_ahead || len <= 0L) {
        # Zero-width or look-ahead: the context change is the point, but
        # something must still give or we would sit here forever.
        spin <- spin + 1L
        if (spin > 20L) { pos <- pos + 1L; spin <- 0L }
      } else {
        pos <- pos + len
        spin <- 0L
      }
    }

    out[[li]] <- cls
    cur <- g$contexts[[stack[length(stack)]]]
    if (!is.null(cur)) stack <- .hl_push(stack, cur$line_end)
  }
  out
}

# --- the R backend ---------------------------------------------------

.hl_r_class <- function(token, text) {
  switch(token,
    COMMENT = "co",
    STR_CONST = "st",
    NULL_CONST = "cn",
    SYMBOL_FUNCTION_CALL = "fu",
    # `T` and `F` are not here on purpose: the parser reports them as
    # SYMBOL, not NUM_CONST, because they are ordinary bindings.
    NUM_CONST = if (text %in% c("TRUE", "FALSE", "NA", "Inf", "NaN"))
      "cn" else if (grepl("[.eE]", text)) "fl" else "dv",
    IF = , ELSE = , FOR = , WHILE = , REPEAT = , BREAK = , NEXT = "cf",
    FUNCTION = , IN = "kw",
    NA_character_
  )
}

# R gets its own parser rather than a grammar: it is exact, it is already
# installed, and it sees the whole block at once so a string spanning
# several lines is classified correctly. Returns NULL when the block does
# not parse -- a fenced block is often a fragment -- and the caller then
# falls back to the r.xml grammar.
.hl_r <- function(lines) {
  # Errors only. Catching warnings here would silently drop R's exact
  # backend for a block that parses perfectly well but happens to warn.
  pd <- tryCatch(
    utils::getParseData(parse(text = lines, keep.source = TRUE)),
    error = function(e) NULL)
  if (is.null(pd) || !nrow(pd)) return(NULL)
  pd <- pd[pd$terminal, , drop = FALSE]
  if (!nrow(pd)) return(NULL)

  out <- lapply(nchar(lines), function(n) rep(NA_character_, n))
  for (i in seq_len(nrow(pd))) {
    cls <- .hl_r_class(pd$token[i], pd$text[i])
    if (is.na(cls)) next
    l1 <- pd$line1[i]; l2 <- pd$line2[i]
    if (l1 < 1L || l2 > length(lines)) next
    for (ln in l1:l2) {
      n <- length(out[[ln]])
      if (!n) next
      from <- if (ln == l1) pd$col1[i] else 1L
      to <- if (ln == l2) pd$col2[i] else n
      # getParseData reports byte offsets under some encodings. Rather
      # than guess, check the span really is the token before trusting
      # it; a mismatch leaves those characters plain.
      if (ln == l1 && ln == l2 &&
          !identical(substring(lines[ln], from, to), pd$text[i])) next
      if (from < 1L || to > n || to < from) next
      out[[ln]][from:to] <- cls
    }
  }
  out
}

# --- entry point -----------------------------------------------------

# Classes for every line of a code block, or NULL for "render plain".
# Never signals: a highlighter that threw would take a user's plot with
# it, and a fence naming a language we do not have is not an error -- it
# is what GitHub does too.
.hl_classes <- function(lines, info) {
  tryCatch({
    lang <- .hl_canonical(.hl_lang(info))
    if (!nzchar(lang) || !length(lines)) return(NULL)
    # Classification runs from .md_layout(), which is makeContent() --
    # so it repeats on every single draw, and a resize or an animation
    # pays it again each frame. Measured on a 100-line python block it
    # is 0.59s of a 1.09s draw. The answer for identical input never
    # changes, so remember it.
    key <- paste0(lang, "\r", paste(lines, collapse = "\n"))
    hit <- .hl_state$cache[[key]]
    if (!is.null(hit)) return(hit$value)

    out <- if (identical(lang, "r")) .hl_r(lines) else NULL
    if (is.null(out)) {
      g <- .hl_grammar(lang)
      if (is.null(g)) return(NULL)
      out <- .hl_patterns(lines, g)
    }
    # Bounded so a document with many distinct blocks cannot grow it
    # without limit; the whole cache is dropped rather than aged, which
    # needs no bookkeeping and costs one recomputation.
    if (length(.hl_state$cache) >= 64L) .hl_state$cache <- list()
    .hl_state$cache[[key]] <- list(value = out)
    out
  }, error = function(e) NULL)
}

# xml_attr() returns NA, not "", for an absent attribute.
`%|NA%` <- function(x, y) if (length(x) != 1L || is.na(x)) y else x

# KDE writes booleans both ways and mixes them within one file: python.xml
# has lookAhead="1" ninety times and lookAhead="true" not once, while
# insensitive appears as both. Reading only "true" silently turned every
# look-ahead rule into a consuming one.
.hl_bool <- function(x, default = FALSE) {
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return(default)
  tolower(x) %in% c("1", "true", "on", "yes")
}

# --- one code line -> TeX --------------------------------------------

# Split a line into the runs that will become \text{} groups: one per
# stretch of the same token class, plus one per space wherever spaces
# run two or more deep. A code block used to lose its indentation
# entirely because the whole line went into a single run and MicroTeX
# collapses a run of spaces inside one \text{}.
#
# Kept apart from the TeX building so the invariant that matters can be
# asserted directly -- paste0(.hl_segments(line, cls)$text) == line --
# with no escaping to undo first. Segments index the original line with
# substring(), so the emitted text is the input text whatever the
# classifier did.
.hl_segments <- function(line, cls) {
  n <- nchar(line)
  if (!n) return(data.frame(text = character(0), class = character(0),
                            space = logical(0), stringsAsFactors = FALSE))
  ch <- substring(line, seq_len(n), seq_len(n))
  if (length(cls) != n) cls <- rep(NA_character_, n)
  sp <- ch == " "
  key <- ifelse(is.na(cls), "", cls)

  # Only a *run* of two or more spaces collapses inside one \text{}; a
  # lone space is kept (\text{a }\text{b} and \text{a b} both measure
  # 21bp at 12pt). Splitting only where it is needed matters: MicroTeX
  # emits one draw record per run, so splitting at every space would
  # multiply the records in a code block sixfold and undo the file-size
  # win of drawing text a line at a time.
  multi <- if (n == 1L) FALSE else sp & (c(FALSE, sp[-n]) | c(sp[-1], FALSE))
  brk <- if (n == 1L) TRUE else
    c(TRUE, multi[-1] | multi[-n] | key[-1] != key[-n])
  grp <- cumsum(brk)
  ends <- c(which(diff(grp) != 0L), n)
  starts <- c(1L, utils::head(ends, -1L) + 1L)

  data.frame(text = substring(line, starts, ends),
             class = cls[starts], space = sp[starts],
             stringsAsFactors = FALSE)
}

.hl_code_tex <- function(line, cls, colour) {
  # An empty source line still occupies a row.
  if (!nchar(line)) return("\\texttt{\\text{ }}")
  seg <- .hl_segments(line, cls)
  out <- paste0("\\text{", .md_escape_tex(seg$text), "}")
  for (i in seq_len(nrow(seg))) {
    if (seg$space[i] || is.na(seg$class[i])) next
    col <- colour(seg$class[i])
    if (!is.null(col)) out[i] <- paste0("\\textcolor{", col, "}{", out[i], "}")
  }
  paste0("\\texttt{", paste0(out, collapse = ""), "}")
}

# --- user-facing -----------------------------------------------------

#' Add a syntax highlighting grammar
#'
#' Registers a KDE/Kate syntax definition so that fenced code blocks
#' tagged with \code{lang} are highlighted by
#' \code{\link{markdown_box_grob}}. Ten languages are built in --- see
#' \code{\link{available_highlighters}} --- and this is how to add
#' another.
#'
#' The grammar is an XML file in the format used by Kate, KDevelop and,
#' through \pkg{skylighting}, Pandoc. The simplest way to write one is to
#' copy a built-in grammar and edit it:
#'
#' \preformatted{
#'   file.copy(system.file("highlight", "python.xml",
#'                         package = "gridmicrotex"),
#'             "mylang.xml")
#' }
#'
#' Because the format is KDE's, the several hundred definitions upstream
#' are a useful \emph{reference} when writing your own --- one XML file
#' per language, catalogued at \url{https://kate-editor.org/syntax/},
#' repository at
#' \url{https://invent.kde.org/frameworks/syntax-highlighting}.
#'
#' Many load and work directly: of twenty sampled, thirteen registered,
#' including \code{python} (249 contexts), \code{css}, \code{yaml},
#' \code{makefile} and \code{go}. The seven that did not --- \code{bash},
#' \code{ruby}, \code{perl}, \code{rust}, \code{lua},
#' \code{javascript} and \code{markdown} --- all use \emph{dynamic}
#' rules, which substitute part of the match into a later pattern and are
#' not implemented.
#'
#' Cross-language includes (\code{##Alerts}, \code{##Doxygen} and
#' friends) are skipped rather than followed. Those only add TODO/FIXME
#' marks \emph{inside} a comment, so the comment is still a comment; the
#' marks are all that is lost.
#'
#' They also carry their own licences, mostly GPL or LGPL, which is why
#' none is bundled with this MIT-licensed package. Borrowing from one
#' locally is your own decision; redistributing it is subject to its
#' licence.
#'
#' Colours come from the \code{defStyleNum} of each \code{<itemData>},
#' which maps onto the same CSS class names \pkg{knitr} and Pandoc use
#' (\code{kw} for a keyword, \code{co} for a comment, \code{st} for a
#' string, and so on), so a grammar needs no colour information of its
#' own --- restyle with \code{\link{markdown_style}}.
#'
#' At each position the current context's rules are tried in document
#' order and the first to match wins. Contexts, \code{IncludeRules},
#' \code{lookAhead} and \code{fallthroughContext} all work, and the
#' context stack carries across lines, so a string or block comment may
#' span them.
#'
#' A rule this engine cannot represent is skipped when it would have
#' stayed in the current context --- costing colour on the text it
#' matched and nothing more --- but \emph{refuses the grammar} when it
#' would have switched, because a lost context switch leaves the machine
#' in the wrong state and paints the rest of the file wrongly. The
#' refusal names the construct.
#'
#' @param lang Language name, as written after the opening fence. Case is
#'   ignored. Registering a name that is already built in replaces it.
#' @param file Path to a KDE syntax XML grammar.
#' @return Invisibly, \code{lang}.
#' @seealso \code{\link{available_highlighters}},
#'   \code{\link{markdown_box_grob}}, \code{\link{markdown_style}}
#' @export
#'
#' @examples
#' # Registering a grammar under a name of your own.
#' f <- system.file("highlight", "python.xml", package = "gridmicrotex")
#' register_highlighter("mypython", f)
#' "mypython" %in% available_highlighters()
register_highlighter <- function(lang, file) {
  stopifnot(is.character(lang), length(lang) == 1L, !is.na(lang), nzchar(lang))
  stopifnot(is.character(file), length(file) == 1L, !is.na(file))
  if (!file.exists(file)) {
    stop("Grammar file not found: ", sQuote(file), call. = FALSE)
  }
  lang <- tolower(lang)
  # Compile now, so a broken grammar fails at this call rather than in
  # the middle of drawing a plot.
  g <- .hl_compile(file)
  .hl_store(lang, g, own = identical(lang, g$name))
  # Drop memoised classifications: editing a grammar and registering it
  # again must change what the next draw produces. Only re-registration
  # can invalidate them -- a lazy first load of a bundled grammar adds a
  # language nothing was cached under, so it must not clear the cache
  # (that would wipe it on first use of every language in a document).
  .hl_state$cache <- list()
  .hl_state$user <- union(.hl_state$user, lang)
  invisible(lang)
}

#' Syntax highlighting languages available
#'
#' Names accepted after an opening code fence in
#' \code{\link{markdown_box_grob}}. Includes both the built-in grammars
#' and any added with \code{\link{register_highlighter}}.
#'
#' Several common aliases also work and are not listed here, among them
#' \code{py}, \code{sh}, \code{c++}, \code{yml}, \code{jl} and
#' \code{tex}. A fence naming anything else renders as plain monospace
#' text, without a warning.
#'
#' @return A character vector of language names, sorted.
#' @seealso \code{\link{register_highlighter}}
#' @export
#'
#' @examples
#' available_highlighters()
available_highlighters <- function() {
  sort(union(.hl_grammar_names(), .hl_state$user))
}
