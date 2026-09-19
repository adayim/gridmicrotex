#include "front/parser.h"

#include <algorithm>
#include <utility>

namespace microtex::front {

namespace {

/** Nesting deeper than this is read flat, with an error, rather than
 *  recursing until the stack runs out. */
constexpr int kMaxDepth = 400;

bool isOther(const Token& t, char c) {
  return t.kind == TokKind::character && t.cat == Cat::other && t.cp == static_cast<unsigned char>(c);
}

bool isRule(const std::string& name) {
  return name == "hline" || name == "thickhline" || name == "cline";
}

std::string trim(const std::string& s) {
  const auto first = s.find_first_not_of(" \t\r\n");
  if (first == std::string::npos) return "";
  const auto last = s.find_last_not_of(" \t\r\n");
  return s.substr(first, last - first + 1);
}

char kindCode(ArgKind kind) {
  switch (kind) {
    case ArgKind::math: return 'm';
    case ArgKind::text: return 't';
    case ArgKind::current: return 'c';
    case ArgKind::raw: return 'r';
    case ArgKind::dimen: return 'd';
    case ArgKind::delim: return 'l';
    case ArgKind::url: return 'u';
  }
  return '?';
}

}  // namespace

Parser::Parser(Expander& input, Ast& ast, Diagnostics& diagnostics, ParserOptions options)
    : _in(input), _ast(ast), _diags(diagnostics), _opts(std::move(options)) {}

// --- tokens ------------------------------------------------------------------

ExpandedToken Parser::next() {
  ExpandedToken t;
  bool replay = false;
  if (!_ahead.empty()) {
    t = std::move(_ahead.back().tok);
    replay = _ahead.back().replay;
    _ahead.pop_back();
  } else {
    t = _in.next();
  }
  _lastReplay = replay;
  // A replayed token was recorded when it was first read.
  if (_recording > 0 && !replay) _log.push_back({_consumed, t});
  _consumed++;
  return t;
}

void Parser::unread(ExpandedToken t) {
  _consumed--;
  if (!_log.empty() && _log.back().index == _consumed) _log.pop_back();
  _ahead.push_back({std::move(t), _lastReplay});
}

const ExpandedToken& Parser::peek() {
  ExpandedToken t = next();
  unread(std::move(t));
  return _ahead.back().tok;
}

ExpandedToken Parser::nextNonSpace() {
  ExpandedToken t = next();
  while (t.tok.kind == TokKind::space) t = next();
  return t;
}

std::size_t Parser::startRecording() {
  if (_recording == 0) _log.clear();
  _recording++;
  return _consumed;
}

std::string Parser::stopRecording(std::size_t mark, std::size_t dropLast, bool firstLead) {
  _recording--;
  const std::size_t end = _consumed - dropLast;
  std::string s;
  bool first = true;
  // The log is in consumption order, so the argument's first token is
  // found by bisection: scanning from the front made nested arguments
  // quadratic in the size of the outermost one.
  const auto from = std::lower_bound(_log.begin(), _log.end(), mark,
                                     [](const Logged& l, std::size_t m) { return l.index < m; });
  for (auto it = from; it != _log.end(); ++it) {
    const Logged& l = *it;
    if (l.index >= end) {
      // A dropped closing token: its lead is still inside the argument.
      if (l.index == end) s += l.tok.lead;
      break;
    }
    if (!first || firstLead) s += l.tok.lead;
    s += l.tok.text;
    first = false;
  }
  return s;
}

// --- structure ---------------------------------------------------------------

bool Parser::atStop(const ExpandedToken& t, const Stop& stop) const {
  const Token& k = t.tok;
  if (k.kind == TokKind::end) return true;
  if (stop.group && k.isChar(Cat::endGroup)) return true;
  if ((stop.cell || stop.overArg) &&
      (k.isChar(Cat::alignTab) || k.isCs("\\") || k.isCs("cr"))) {
    return true;
  }
  if (stop.cell && k.isCs("end")) return true;
  if (stop.right && (k.isCs("right") || k.isCs("middle"))) return true;
  if ((stop.dollar || stop.displayDollar) && k.isChar(Cat::mathShift)) return true;
  if (!stop.closeSymbol.empty() && k.kind == TokKind::controlSymbol && k.text == stop.closeSymbol) {
    return true;
  }
  return false;
}

NodeId Parser::emptyList(const SourceSpan& at, Mode mode) {
  Node n;
  n.kind = NodeKind::list;
  n.mode = mode;
  n.span = at;
  return _ast.add(std::move(n), {});
}

NodeId Parser::absentArgument(const SourceSpan& at, Mode mode) {
  Node n;
  n.kind = NodeKind::argument;
  n.mode = mode;
  n.span = at;
  n.flag = false;
  return _ast.add(std::move(n), {});
}

NodeId Parser::character(const ExpandedToken& t, Mode mode) {
  Node n;
  n.kind = NodeKind::character;
  n.mode = mode;
  n.span = t.tok.span;
  n.cp = t.tok.cp;
  n.text = t.tok.text;
  return _ast.add(std::move(n), {});
}

NodeId Parser::parse() {
  _ast.root = parseList(_opts.startMode, Stop{}, SourceSpan{});
  // Anything left over is after a stray closing token at the top level.
  while (true) {
    ExpandedToken t = next();
    if (t.tok.kind == TokKind::end) break;
    _diags.warn(t.tok.span, "extra " + t.text + " ignored");
  }
  return _ast.root;
}

NodeId Parser::parseList(Mode mode, const Stop& stop, const SourceSpan& at) {
  std::vector<NodeId> items;
  if (_depth >= kMaxDepth) {
    // Too deep to recurse: skip to where this list would have ended.
    _diags.error(at, "Input nested too deeply");
    int depth = 0;
    while (true) {
      ExpandedToken t = next();
      if (t.tok.kind == TokKind::end) {
        unread(std::move(t));
        break;
      }
      if (t.tok.isChar(Cat::beginGroup)) depth++;
      if (t.tok.isChar(Cat::endGroup)) {
        if (depth == 0) {
          unread(std::move(t));
          break;
        }
        depth--;
      }
    }
    return emptyList(at, mode);
  }
  _depth++;
  _listDone = false;
  _rowEnded = false;
  while (true) {
    const ExpandedToken& t = peek();
    if (atStop(t, stop)) break;
    const Token& k = t.tok;
    if (mode == Mode::math && (k.isChar(Cat::superscript) || k.isChar(Cat::subscript) ||
                               isOther(k, '\''))) {
      // Scripts belong to the item before them, as in the old parser.
      NodeId base = kNoNode;
      if (!items.empty()) {
        base = items.back();
        items.pop_back();
      }
      items.push_back(parseScripts(base, mode, k.span));
      continue;
    }
    if (!parseItem(mode, stop, items)) break;
    if (_listDone) {
      _listDone = false;
      break;
    }
    if (_rowEnded && stop.cellTop) break;
  }
  _depth--;
  Node n;
  n.kind = NodeKind::list;
  n.mode = mode;
  n.span = at;
  return _ast.add(std::move(n), items);
}

bool Parser::parseItem(Mode mode, const Stop& stop, std::vector<NodeId>& items) {
  ExpandedToken t = next();
  const Token& k = t.tok;

  switch (k.kind) {
    case TokKind::end:
      unread(std::move(t));
      return false;
    case TokKind::space:
    case TokKind::par:
      if (mode == Mode::text) {
        Node s;
        s.kind = NodeKind::space;
        s.mode = mode;
        s.span = k.span;
        // The whole run of whitespace, which the lexer split between this
        // token and the next one's lead: the engine counted its line ends.
        s.raw = t.lead + t.text + peek().lead;
        items.push_back(_ast.add(std::move(s), {}));
      }
      return true;
    case TokKind::controlWord:
    case TokKind::controlSymbol: {
      bool unused = false;
      const NodeId n = parseCommand(std::move(t), mode, stop, items, unused);
      if (n != kNoNode) items.push_back(n);
      return true;
    }
    case TokKind::character:
      break;
  }

  switch (k.cat) {
    case Cat::beginGroup:
      items.push_back(parseGroupAfterOpen(t, mode));
      return true;
    case Cat::endGroup:
      // Not where this list stops: a `}` with no `{`.
      _diags.warn(k.span, "extra } ignored");
      return true;
    case Cat::mathShift: {
      if (mode == Mode::math) return true;  // the old parser ignored `$` in math
      const ExpandedToken& n = peek();
      const bool display = _restricted == 0 && n.tok.isChar(Cat::mathShift) && n.lead.empty();
      if (display) next();
      items.push_back(parseMath(t, display, "", stop.group));
      return true;
    }
    case Cat::alignTab:
      _diags.warn(k.span, "& outside an alignment is drawn as a character");
      items.push_back(character(t, mode));
      return true;
    case Cat::param:
      // In text a `#` is drawn, as it always was (markdown headings in
      // labels); in math it is a macro parameter out of place.
      if (mode == Mode::math) {
        _diags.warn(k.span, "macro parameter # outside a definition is drawn as a character");
      }
      items.push_back(character(t, mode));
      return true;
    case Cat::active: {
      Node s;
      s.kind = NodeKind::space;
      s.mode = mode;
      s.span = k.span;
      s.aux = 1;  // ~, a space that does not break
      s.raw = t.text;
      items.push_back(_ast.add(std::move(s), {}));
      return true;
    }
    default:
      items.push_back(character(t, mode));
      return true;
  }
}

NodeId Parser::parseGroupAfterOpen(const ExpandedToken& open, Mode mode) {
  Stop inner;
  inner.group = true;
  const NodeId list = parseList(mode, inner, open.tok.span);
  ExpandedToken close = next();
  if (!close.tok.isChar(Cat::endGroup)) {
    _diags.warn(open.tok.span, "missing } inserted");
    unread(std::move(close));
  }
  Node g;
  g.kind = NodeKind::group;
  g.mode = mode;
  g.span = open.tok.span;
  return _ast.add(std::move(g), {list});
}

// --- scripts -----------------------------------------------------------------

NodeId Parser::parseScripts(NodeId base, Mode mode, const SourceSpan& at) {
  NodeId sub = kNoNode, sup = kNoNode;
  int primes = 0;
  std::string order;
  while (true) {
    ExpandedToken t = next();
    const Token& k = t.tok;
    if (isOther(k, '\'')) {
      if (sup != kNoNode) {
        unread(std::move(t));
        break;
      }
      // Primes are a superscript: a second run of them, after a subscript
      // (f'_1'), is TeX's double superscript.
      if (primes > 0 && order.back() == '_') {
        _diags.warn(k.span, "double superscript: read as {...}^");
        unread(std::move(t));
        break;
      }
      // Only `'` is a prime, as in TeX: ` and " are characters.
      primes++;
      order += '\'';
      continue;
    }
    if (k.isChar(Cat::superscript)) {
      // Only a `^` right after the primes joins them (f'^2); after a
      // subscript between (f'_1^2) it is a second superscript.
      if (sup != kNoNode || (primes > 0 && order.back() == '_')) {
        _diags.warn(k.span, "double superscript: read as {...}^");
        unread(std::move(t));
        break;
      }
      sup = parseScriptArgument(mode, k.span);
      order += '^';
      continue;
    }
    if (k.isChar(Cat::subscript)) {
      if (sub != kNoNode) {
        _diags.warn(k.span, "double subscript: read as {...}_");
        unread(std::move(t));
        break;
      }
      sub = parseScriptArgument(mode, k.span);
      order += '_';
      continue;
    }
    unread(std::move(t));
    break;
  }
  Node n;
  n.kind = NodeKind::scripts;
  n.mode = mode;
  n.span = at;
  n.aux = static_cast<std::uint16_t>(primes);
  n.flag = sub != kNoNode;
  n.star = sup != kNoNode;
  // The order the parts came in: the old parser built `f_1'` differently
  // from `f'_1`, and the lowering follows it.
  n.text = order;
  // Each absent part gets its own empty list: a node has one parent.
  if (base == kNoNode) base = emptyList(at, mode);
  if (sub == kNoNode) sub = emptyList(at, mode);
  if (sup == kNoNode) sup = emptyList(at, mode);
  return _ast.add(std::move(n), {base, sub, sup});
}

NodeId Parser::parseScriptArgument(Mode mode, const SourceSpan& at) {
  ExpandedToken t = nextNonSpace();
  const Token& k = t.tok;
  if (k.kind == TokKind::end || k.isChar(Cat::endGroup) || k.isChar(Cat::alignTab) ||
      k.isChar(Cat::superscript) || k.isChar(Cat::subscript)) {
    _diags.warn(at, "missing script: an empty one is used");
    unread(std::move(t));
    return emptyList(at, mode);
  }
  std::vector<NodeId> items;
  if (k.isChar(Cat::beginGroup)) {
    items.push_back(parseGroupAfterOpen(t, mode));
  } else if (k.isControl()) {
    // A command with its own arguments, as the old parser read `x^\frac12`.
    bool unused = false;
    Stop single;
    single.argument = true;
    const NodeId n = parseCommand(std::move(t), mode, single, items, unused);
    if (n != kNoNode) items.push_back(n);
  } else {
    items.push_back(character(t, mode));
  }
  Node l;
  l.kind = NodeKind::list;
  l.mode = mode;
  l.span = at;
  return _ast.add(std::move(l), items);
}

// --- commands ----------------------------------------------------------------

NodeId Parser::parseCommand(ExpandedToken t, Mode mode, const Stop& stop,
                            std::vector<NodeId>& items, bool& consumedRest) {
  const std::string name = t.tok.text;
  const SourceSpan at = t.tok.span;
  const std::string who = "\\" + name;
  const CommandSpec* spec = findCommand(name);

  Node n;
  n.kind = NodeKind::command;
  n.mode = mode;
  n.span = at;
  n.text = name;

  if (spec == nullptr) {
    if (!(_opts.isKnownName && _opts.isKnownName(name))) {
      _diags.warn(at, "unknown command " + who + ": drawn as its name");
      n.flag = true;
    }
    return _ast.add(std::move(n), {});
  }

  if (spec->special) {
    if (name == "left") return parseLeftRight(t, mode);
    if (name == "begin") return parseEnvironment(t, mode);
    if (name == "(") return parseMath(t, false, ")", stop.group);
    if (name == "[") return parseMath(t, true, "]", stop.group);
    if (name == "right") {
      _diags.warn(at, "\\right without \\left: drawn as its name");
      n.flag = true;
      return _ast.add(std::move(n), {});
    }
    if (name == "end") {
      const std::string env = readGroupName();
      _diags.warn(at, "\\end{" + env + "} without \\begin ignored");
      return kNoNode;
    }
    if (name == "middle") {
      return _ast.add(std::move(n), {parseDelimiter(who)});
    }
    if (name == "\\" || name == "cr") {
      // A line break outside an alignment (in one, it ends the row and is
      // read by the environment). `\\[4pt]`: only a bracket right after it
      // is the gap, as in amsmath, so `\\ [0,1]` stays text.
      std::vector<NodeId> args;
      if (name == "\\") {
        const ExpandedToken& p = peek();
        if (isOther(p.tok, '[') && p.lead.empty()) {
          args.push_back(parseArgument({ArgKind::dimen, true}, mode, who));
        } else {
          args.push_back(absentArgument(at, mode));
        }
      }
      return _ast.add(std::move(n), args);
    }
    if (isRule(name) || name == "intertext") {
      std::vector<NodeId> args;
      for (const ArgSpec& a : spec->args) args.push_back(parseArgument(a, mode, who));
      if (stop.cellTop) {
        _rowEnded = true;
      } else {
        _diags.warn(at, who + " outside an alignment is ignored");
      }
      return _ast.add(std::move(n), args);
    }
  }

  // Inside a one-token argument (`\frac\bf ab`), a command is just itself.
  const bool single = stop.argument;

  if (spec->shape == Shape::postfix && !single) {
    // \limits: modifies the item before it, as the old parser's popBack did.
    NodeId base = kNoNode;
    if (!items.empty()) {
      base = items.back();
      items.pop_back();
    }
    std::vector<NodeId> kids;
    kids.push_back(base == kNoNode ? emptyList(at, mode) : base);
    return _ast.add(std::move(n), kids);
  }

  std::vector<NodeId> args;
  for (const ArgSpec& a : spec->args) args.push_back(parseArgument(a, mode, who));
  if (spec->bare != Bare::none) args.push_back(parseBare(spec->bare, who));

  if (spec->shape == Shape::infix && !single) {
    // The numerator is everything before it in this list; the denominator
    // everything after, up to where the list ends.
    Node num;
    num.kind = NodeKind::list;
    num.mode = mode;
    num.span = at;
    const NodeId numerator = _ast.add(std::move(num), items);
    items.clear();
    Stop rest = stop;
    rest.cellTop = false;
    // The engine reads the denominator in math mode, even in text.
    const NodeId denominator = parseList(Mode::math, rest, at);
    n.kind = NodeKind::infix;
    std::vector<NodeId> kids{numerator, denominator};
    kids.insert(kids.end(), args.begin(), args.end());
    consumedRest = true;
    _listDone = true;
    return _ast.add(std::move(n), kids);
  }

  const bool declaration = spec->shape == Shape::declaration ||
                           (spec->shape == Shape::groupDeclaration && !stop.cellTop);
  if (declaration && !single) {
    // \color in a cell itself colours the cell, and has no body.
    Stop body = stop;
    body.cellTop = false;
    body.argument = false;
    body.overArg = spec->shape == Shape::declaration;
    const Mode bodyMode = spec->body == ArgKind::math ? Mode::math : mode;
    const NodeId list = parseList(bodyMode, body, at);
    n.kind = NodeKind::declaration;
    args.push_back(list);
    return _ast.add(std::move(n), args);
  }

  return _ast.add(std::move(n), args);
}

NodeId Parser::parseArgument(const ArgSpec& spec, Mode mode, const std::string& who) {
  if (spec.kind == ArgKind::raw || spec.kind == ArgKind::dimen || spec.kind == ArgKind::url ||
      spec.kind == ArgKind::delim) {
    return parseRawArgument(spec, who);
  }
  const Mode argMode = spec.kind == ArgKind::math   ? Mode::math
                       : spec.kind == ArgKind::text ? Mode::text
                                                    : mode;
  Node arg;
  arg.kind = NodeKind::argument;
  arg.mode = argMode;
  arg.flag = true;
  arg.text = std::string(1, kindCode(spec.kind));
  const bool restricted = argMode == Mode::text;
  if (restricted) _restricted++;
  struct Unrestrict {
    int& n;
    bool on;
    ~Unrestrict() {
      if (on) n--;
    }
  } unrestrict{_restricted, restricted};

  if (spec.optional) {
    ExpandedToken t = nextNonSpace();
    if (!isOther(t.tok, '[')) {
      const SourceSpan at = t.tok.span;
      unread(std::move(t));
      return absentArgument(at, argMode);
    }
    arg.span = t.tok.span;
    std::vector<ExpandedToken> toks = collectBracketed(who);
    for (const ExpandedToken& u : toks) {
      arg.raw += u.lead;
      arg.raw += u.text;
    }
    const NodeId list = parseTokens(std::move(toks), argMode, arg.span);
    return _ast.add(std::move(arg), {list});
  }

  ExpandedToken t = nextNonSpace();
  const Token& k = t.tok;
  arg.span = k.span;
  if (k.kind == TokKind::end || k.kind == TokKind::par || k.isChar(Cat::endGroup)) {
    _diags.warn(k.span, "missing argument for " + who + ": an empty one is used");
    unread(std::move(t));
    return absentArgument(k.span, argMode);
  }
  if (k.isChar(Cat::beginGroup)) {
    const std::size_t mark = startRecording();
    Stop inner;
    inner.group = true;
    const NodeId list = parseList(argMode, inner, k.span);
    ExpandedToken close = next();
    const bool closed = close.tok.isChar(Cat::endGroup);
    arg.raw = stopRecording(mark, closed ? 1 : 0);
    if (!closed) {
      _diags.warn(k.span, "missing } inserted");
      unread(std::move(close));
    }
    return _ast.add(std::move(arg), {list});
  }
  // One token: a character, or a command with its own arguments.
  unread(std::move(t));
  const std::size_t mark = startRecording();
  std::vector<NodeId> items;
  Stop single;
  single.argument = true;
  ExpandedToken u = next();
  if (u.tok.isControl()) {
    bool unused = false;
    const NodeId n = parseCommand(std::move(u), argMode, single, items, unused);
    if (n != kNoNode) items.push_back(n);
  } else if (u.tok.isChar(Cat::active)) {
    unread(std::move(u));
    parseItem(argMode, single, items);
  } else {
    items.push_back(character(u, argMode));
  }
  arg.raw = stopRecording(mark, 0, false);
  Node l;
  l.kind = NodeKind::list;
  l.mode = argMode;
  l.span = arg.span;
  const NodeId list = _ast.add(std::move(l), items);
  return _ast.add(std::move(arg), {list});
}

NodeId Parser::parseRawArgument(const ArgSpec& spec, const std::string& who) {
  Node arg;
  arg.kind = NodeKind::argument;
  arg.mode = Mode::text;
  arg.flag = true;
  arg.text = std::string(1, kindCode(spec.kind));

  if (spec.optional) {
    ExpandedToken t = nextNonSpace();
    if (!isOther(t.tok, '[')) {
      const SourceSpan at = t.tok.span;
      unread(std::move(t));
      return absentArgument(at, Mode::text);
    }
    arg.span = t.tok.span;
    for (const ExpandedToken& u : collectBracketed(who)) {
      arg.raw += u.lead;
      arg.raw += u.text;
    }
    return _ast.add(std::move(arg), {});
  }

  ExpandedToken t = nextNonSpace();
  const Token& k = t.tok;
  arg.span = k.span;
  if (k.kind == TokKind::end || k.kind == TokKind::par || k.isChar(Cat::endGroup)) {
    _diags.warn(k.span, "missing argument for " + who + ": an empty one is used");
    unread(std::move(t));
    return absentArgument(k.span, Mode::text);
  }
  if (!k.isChar(Cat::beginGroup)) {
    arg.raw = t.text;
    return _ast.add(std::move(arg), {});
  }
  // A URL or file name reads its special characters as characters. The
  // `{` is read already; nothing after it has been lexed yet.
  struct Saved {
    c32 ch;
    Cat cat;
  };
  std::vector<Saved> saved;
  if (spec.kind == ArgKind::url) {
    for (const char c : std::string("%#_^~&$")) {
      saved.push_back({static_cast<c32>(c), _in.catcode(static_cast<c32>(c))});
      _in.setCatcode(static_cast<c32>(c), Cat::other);
    }
  }
  const std::size_t mark = startRecording();
  int depth = 0;
  bool closed = false;
  while (true) {
    ExpandedToken u = next();
    if (u.tok.kind == TokKind::end) {
      unread(std::move(u));
      break;
    }
    if (u.tok.isChar(Cat::beginGroup)) depth++;
    if (u.tok.isChar(Cat::endGroup)) {
      if (depth == 0) {
        closed = true;
        break;
      }
      depth--;
    }
  }
  arg.raw = stopRecording(mark, closed ? 1 : 0);
  for (const Saved& s : saved) _in.setCatcode(s.ch, s.cat);
  if (!closed) _diags.warn(k.span, "missing } inserted");
  return _ast.add(std::move(arg), {});
}

std::vector<ExpandedToken> Parser::collectBracketed(const std::string& who) {
  std::vector<ExpandedToken> toks;
  int brackets = 0, braces = 0;
  while (true) {
    ExpandedToken t = next();
    const Token& k = t.tok;
    if (k.kind == TokKind::end) {
      _diags.warn(k.span, "missing ] inserted in the optional argument of " + who);
      unread(std::move(t));
      break;
    }
    if (k.isChar(Cat::beginGroup)) braces++;
    if (k.isChar(Cat::endGroup)) {
      if (braces == 0) {
        _diags.warn(k.span, "missing ] inserted in the optional argument of " + who);
        unread(std::move(t));
        break;
      }
      braces--;
    }
    if (braces == 0 && isOther(k, '[')) brackets++;
    if (braces == 0 && isOther(k, ']')) {
      if (brackets == 0) break;
      brackets--;
    }
    toks.push_back(std::move(t));
  }
  return toks;
}

NodeId Parser::parseTokens(std::vector<ExpandedToken> tokens, Mode mode, const SourceSpan& at) {
  // Read the tokens again, then an `end` that stops the list, then carry on
  // with whatever was waiting before.
  std::vector<Pending> saved = std::move(_ahead);
  _ahead.clear();
  ExpandedToken sentinel;
  sentinel.tok.kind = TokKind::end;
  sentinel.tok.span = at;
  _ahead.push_back({std::move(sentinel), true});
  for (auto it = tokens.rbegin(); it != tokens.rend(); ++it) _ahead.push_back({std::move(*it), true});
  const NodeId list = parseList(mode, Stop{}, at);
  next();  // the sentinel
  _ahead = std::move(saved);
  return list;
}

NodeId Parser::parseBare(Bare bare, const std::string& who) {
  Node arg;
  arg.kind = NodeKind::argument;
  arg.mode = Mode::text;
  arg.flag = true;
  arg.text = bare == Bare::dimen ? "d" : "n";
  ExpandedToken t = nextNonSpace();
  arg.span = t.tok.span;
  if (t.tok.isChar(Cat::beginGroup)) {
    // `\kern{3pt}`, which TeX would not take, but the engine's own
    // definitions write.
    unread(std::move(t));
    const NodeId raw = parseRawArgument({ArgKind::dimen, false}, who);
    arg.raw = _ast.node(raw).raw;
    return _ast.add(std::move(arg), {});
  }
  // As the old parser read it: characters up to a space or a command.
  while (t.tok.kind == TokKind::character && !t.tok.isChar(Cat::beginGroup) &&
         !t.tok.isChar(Cat::endGroup) && !t.tok.isChar(Cat::mathShift) &&
         !t.tok.isChar(Cat::alignTab)) {
    if (bare == Bare::number) {
      const c32 c = t.tok.cp;
      const bool ok = c == '\'' || c == '"' || (c >= '0' && c <= '9') ||
                      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
      if (!ok) break;
    }
    arg.raw += t.text;
    t = next();
    if (!t.lead.empty()) break;
  }
  unread(std::move(t));
  if (arg.raw.empty()) _diags.warn(arg.span, "missing value after " + who);
  return _ast.add(std::move(arg), {});
}

// --- \left \right ------------------------------------------------------------

NodeId Parser::parseDelimiter(const std::string& who) {
  return parseRawArgument({ArgKind::delim, false}, who);
}

NodeId Parser::parseLeftRight(const ExpandedToken& left, Mode mode) {
  std::vector<NodeId> kids;
  kids.push_back(parseDelimiter("\\left"));
  Stop inner;
  inner.right = true;
  while (true) {
    // Math even in text: \left exists only in math, and the old parser
    // read what it encloses as math wherever it met it.
    kids.push_back(parseList(Mode::math, inner, left.tok.span));
    ExpandedToken t = next();
    if (t.tok.isCs("middle")) {
      kids.push_back(parseDelimiter("\\middle"));
      continue;
    }
    if (t.tok.isCs("right")) {
      kids.push_back(parseDelimiter("\\right"));
      break;
    }
    _diags.warn(left.tok.span, "missing \\right. inserted");
    unread(std::move(t));
    kids.push_back(absentArgument(left.tok.span, Mode::text));
    break;
  }
  Node n;
  n.kind = NodeKind::leftRight;
  n.mode = mode;
  n.span = left.tok.span;
  return _ast.add(std::move(n), kids);
}

// --- environments ------------------------------------------------------------

std::string Parser::readGroupName() {
  ExpandedToken t = nextNonSpace();
  if (!t.tok.isChar(Cat::beginGroup)) {
    unread(std::move(t));
    return "";
  }
  // Built from the tokens, not from the recorded source: tokens read a
  // second time (an optional argument's) are not recorded again, so the
  // name of an environment in `\sqrt[...]` came out empty.
  std::string name;
  int depth = 0;
  while (true) {
    ExpandedToken u = next();
    if (u.tok.kind == TokKind::end) {
      _diags.warn(t.tok.span, "missing } inserted");
      unread(std::move(u));
      break;
    }
    if (u.tok.isChar(Cat::beginGroup)) depth++;
    if (u.tok.isChar(Cat::endGroup) && depth-- == 0) break;
    name += u.lead;
    name += u.text;
  }
  return trim(name);
}

NodeId Parser::parseEnvironment(const ExpandedToken& begin, Mode mode) {
  const SourceSpan at = begin.tok.span;
  const std::string name = readGroupName();
  const EnvSpec* spec = findEnvironment(name);
  Node n;
  n.kind = NodeKind::environment;
  n.mode = mode;
  n.span = at;
  n.text = name;
  if (spec == nullptr) {
    _diags.warn(at, "unknown environment " + name + ": read as an array");
  }
  std::vector<NodeId> kids;
  if (spec != nullptr) {
    for (const ArgSpec& a : spec->args) kids.push_back(parseArgument(a, mode, "\\begin{" + name + "}"));
  }

  const std::size_t mark = startRecording();
  if (spec != nullptr && spec->body == EnvBody::raw) {
    // Kept as text up to the matching \end{name}, nested ones counted.
    int depth = 0;
    while (true) {
      ExpandedToken t = next();
      if (t.tok.kind == TokKind::end) {
        unread(std::move(t));
        n.raw = stopRecording(mark, 0);
        _diags.warn(at, "missing \\end{" + name + "} inserted");
        return _ast.add(std::move(n), kids);
      }
      if (!t.tok.isCs("begin") && !t.tok.isCs("end")) continue;
      const bool open = t.tok.isCs("begin");
      std::size_t used = 1;
      std::string inner;
      ExpandedToken brace = next();
      used++;
      if (brace.tok.isChar(Cat::beginGroup)) {
        while (true) {
          ExpandedToken u = next();
          if (u.tok.kind == TokKind::end) {
            unread(std::move(u));
            break;
          }
          used++;
          if (u.tok.isChar(Cat::endGroup)) break;
          inner += u.text;
        }
      } else {
        unread(std::move(brace));
        used--;
      }
      if (trim(inner) != name) continue;
      if (open) {
        depth++;
        continue;
      }
      if (depth-- > 0) continue;
      // The body is everything before this `\end{name}`.
      n.raw = stopRecording(mark, used);
      return _ast.add(std::move(n), kids);
    }
  }

  // An alignment: rows at `\\` and `\cr`, cells at `&`; a rule or
  // \intertext ends its row, as it did in the old parser.
  while (true) {
    std::vector<NodeId> cells;
    Node row;
    row.kind = NodeKind::row;
    row.mode = Mode::math;
    bool firstCell = true;
    while (true) {
      Stop cellStop;
      cellStop.cell = true;
      cellStop.cellTop = true;
      const SourceSpan cellAt = peek().tok.span;
      if (firstCell) row.span = cellAt;
      firstCell = false;
      const NodeId list = parseList(Mode::math, cellStop, cellAt);
      Node cell;
      cell.kind = NodeKind::cell;
      cell.mode = Mode::math;
      cell.span = cellAt;
      cells.push_back(_ast.add(std::move(cell), {list}));
      if (_rowEnded) {
        // The rule is the last item of the cell just read.
        const NodeId items = list;
        const std::uint32_t count = _ast.childCount(items);
        row.text = count > 0 ? _ast.node(_ast.child(items, count - 1)).text : "";
        _rowEnded = false;
        break;
      }
      const ExpandedToken& p = peek();
      if (p.tok.isChar(Cat::alignTab)) {
        next();
        continue;
      }
      break;
    }
    if (!row.text.empty()) {
      kids.push_back(_ast.add(std::move(row), cells));
      continue;
    }
    ExpandedToken t = next();
    if (t.tok.isCs("\\")) {
      row.text = "\\";
      const ExpandedToken& p = peek();
      if (isOther(p.tok, '[') && p.lead.empty()) {
        const NodeId gap = parseArgument({ArgKind::dimen, true}, Mode::math, "\\\\");
        row.raw = _ast.node(gap).raw;
      }
      kids.push_back(_ast.add(std::move(row), cells));
      continue;
    }
    if (t.tok.isCs("cr")) {
      row.text = "cr";
      kids.push_back(_ast.add(std::move(row), cells));
      continue;
    }
    // \end, or the end of the input: the last row.
    kids.push_back(_ast.add(std::move(row), cells));
    if (t.tok.isCs("end")) {
      n.raw = stopRecording(mark, 1);
      const std::string closing = readGroupName();
      if (closing != name) {
        _diags.warn(t.tok.span, "\\end{" + closing + "} ends \\begin{" + name + "}");
      }
    } else {
      n.raw = stopRecording(mark, 0);
      _diags.warn(at, "missing \\end{" + name + "} inserted");
      unread(std::move(t));
    }
    break;
  }
  return _ast.add(std::move(n), kids);
}

// --- math in text --------------------------------------------------------------

NodeId Parser::parseMath(const ExpandedToken& open, bool display, const std::string& closeSymbol,
                         bool inGroup) {
  Stop inner;
  inner.group = inGroup;
  if (closeSymbol.empty()) {
    inner.dollar = true;
  } else {
    inner.closeSymbol = closeSymbol;
  }
  const NodeId list = parseList(Mode::math, inner, open.tok.span);
  ExpandedToken close = next();
  bool closed = false;
  if (closeSymbol.empty()) {
    if (close.tok.isChar(Cat::mathShift)) {
      closed = true;
      if (display) {
        const ExpandedToken& p = peek();
        if (p.tok.isChar(Cat::mathShift) && p.lead.empty()) next();
      }
    }
  } else {
    closed = close.tok.kind == TokKind::controlSymbol && close.tok.text == closeSymbol;
  }
  if (!closed) {
    _diags.warn(open.tok.span, closeSymbol.empty() ? "missing $ inserted"
                                                   : "missing \\" + closeSymbol + " inserted");
    unread(std::move(close));
  }
  Node m;
  m.kind = NodeKind::math;
  m.mode = Mode::math;
  m.flag = display;
  m.span = open.tok.span;
  return _ast.add(std::move(m), {list});
}

}  // namespace microtex::front
