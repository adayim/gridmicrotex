#include "front/lower.h"

#include <string>
#include <vector>

#include "atom/atom.h"
#include "atom/atom_basic.h"
#include "atom/atom_char.h"
#include "atom/atom_delim.h"
#include "atom/atom_fence.h"
#include "atom/atom_font.h"
#include "atom/atom_frac.h"
#include "atom/atom_misc.h"
#include "atom/atom_operator.h"
#include "atom/atom_row.h"
#include "atom/atom_scripts.h"
#include "atom/atom_space.h"
#include "atom/atom_text.h"
#include "box/box_factory.h"
#include "core/parser.h"
#include "env/units.h"
#include "front/front.h"
#include "front/spec.h"
#include "graphic/graphic.h"
#include "macro/macro.h"
#include "macro/macro_args.h"
#include "macro/macro_styles.h"
#include "unimath/uni_font.h"
#include "utils/exceptions.h"
#include "utils/string_utils.h"
#include "utils/utf.h"

namespace microtex::front {

namespace {

bool isTextFont(const std::string& n) {
  return n == "bf" || n == "it" || n == "rm" || n == "sf" || n == "tt";
}

float sizeFactor(const std::string& n) {
  if (n == "tiny") return 0.5f;
  if (n == "scriptsize") return 0.7f;
  if (n == "footnotesize") return 0.8f;
  if (n == "small") return 0.9f;
  if (n == "large") return 1.2f;
  if (n == "Large") return 1.4f;
  if (n == "LARGE") return 1.8f;
  if (n == "huge") return 2.f;
  if (n == "Huge") return 2.5f;
  return 1.f;  // normalsize
}

bool isSize(const std::string& n) {
  return n == "tiny" || n == "scriptsize" || n == "footnotesize" || n == "small" ||
         n == "normalsize" || n == "large" || n == "Large" || n == "LARGE" || n == "huge" ||
         n == "Huge";
}

/** How many spaces the old parser drew for a run of whitespace in text:
 *  one for the run, and one more for each line end after its start
 *  (it absorbed spaces, tabs and returns, never a newline). Comments are
 *  dropped first, as its preprocessing dropped them, newline kept. */
int legacySpaceCount(const std::string& run, bool skipFirst) {
  std::string ws;
  bool comment = false;
  for (const char c : run) {
    if (comment) {
      if (c == '\n') {
        comment = false;
        ws += c;
      }
      continue;
    }
    if (c == '%') {
      comment = true;
      continue;
    }
    if (c == ' ' || c == '\t' || c == '\r' || c == '\n') ws += c;
  }
  // \bf and friends skipped one whitespace character before their body.
  if (skipFirst && !ws.empty()) ws.erase(0, 1);
  if (ws.empty()) return 0;
  int n = 1;
  for (std::size_t i = 1; i < ws.size(); i++) {
    if (ws[i] == '\n') n++;
  }
  return n;
}

std::size_t codepoints(const std::string& s) {
  std::size_t n = 0;
  for (std::size_t i = 0; i < s.size();) {
    int len = 1;
    nextUnicode(s, static_cast<int>(i), len);
    i += static_cast<std::size_t>(len > 0 ? len : 1);
    n++;
  }
  return n;
}

class Lowerer {
public:
  Lowerer(const Ast& ast, Diagnostics& diags, const LowerOptions& opts)
      : _ast(ast), _diags(diags), _opts(opts) {}

  void run(Formula& f) {
    if (_ast.root != kNoNode) lowerList(_ast.root, f, 0);
  }

private:
  const Ast& _ast;
  Diagnostics& _diags;
  LowerOptions _opts;

  const Node& node(NodeId id) const { return _ast.node(id); }
  NodeId child(NodeId id, std::uint32_t i) const { return _ast.child(id, i); }
  std::uint32_t count(NodeId id) const { return _ast.childCount(id); }

  // --- the old parser's operations on its formula ------------------------

  static sptr<Atom> popBack(Formula& f) {
    auto a = f._root;
    auto* ra = dynamic_cast<RowAtom*>(a.get());
    if (ra != nullptr) return ra->popBack();
    f._root = nullptr;
    return a;
  }

  static sptr<Atom> popFormulaAtom(Formula& f) {
    auto a = f._root;
    f._root = nullptr;
    return a;
  }

  /** What `{...}` becomes: its content's root -- in a row of its own when
   *  nothing came before it in the formula, which is how the old parser's
   *  getArgument() built it. */
  static sptr<Atom> groupAtom(const sptr<Atom>& inner, const Formula& f) {
    if (f._root == nullptr) {
      auto rm = sptrOf<RowAtom>();
      rm->add(inner);
      return rm;
    }
    return inner;
  }

  // --- lists -------------------------------------------------------------

  /** Replay the items of `list`, from `from` on, into `f`. */
  void lowerList(NodeId list, Formula& f, std::uint32_t from, bool skipFirstSpace = false) {
    const std::uint32_t n = count(list);
    for (std::uint32_t i = from; i < n; i++) {
      const NodeId id = child(list, i);
      const Node& x = node(id);
      if (x.kind == NodeKind::command && !x.flag && (x.text == "\\" || x.text == "cr")) {
        if (f.isArrayMode()) {
          static_cast<ArrayFormula&>(f).addRow();
          continue;
        }
        // A line break outside an alignment: what came before is the first
        // row, and the rest of the list the rows after it.
        ArrayFormula arr;
        arr.add(f._root);
        arr.addRow();
        lowerList(list, arr, i + 1);
        arr.checkDimensions();
        f._root = arr.getAsVRow();
        return;
      }
      if (i == from && skipFirstSpace && x.kind == NodeKind::space && x.aux == 2) {
        spaces(x, f, true);
        continue;
      }
      lowerItem(id, f);
    }
  }

  /** A list built in a formula of its own; its root. */
  sptr<Atom> build(NodeId list, bool skipFirstSpace = false) {
    Formula g;
    lowerList(list, g, 0, skipFirstSpace);
    return g._root;
  }

  void lowerItem(NodeId id, Formula& f) {
    const Node& x = node(id);
    switch (x.kind) {
      case NodeKind::character:
        f.add(charAtom(x));
        return;
      case NodeKind::space:
        spaces(x, f, false);
        return;
      case NodeKind::group: {
        auto atom = groupAtom(build(child(id, 0)), f);
        if (atom != nullptr) atom->_type = AtomType::ordinary;
        f.add(atom);
        return;
      }
      case NodeKind::command:
        command(id, f);
        return;
      case NodeKind::scripts:
        scripts(id, f);
        return;
      case NodeKind::infix:
        infix(id, f);
        return;
      case NodeKind::declaration:
        declaration(id, f);
        return;
      case NodeKind::leftRight:
        f.add(leftRight(id));
        return;
      case NodeKind::environment: {
        auto atom = groupAtom(environment(id), f);
        if (atom != nullptr) atom->_type = AtomType::ordinary;
        f.add(atom);
        return;
      }
      case NodeKind::math:
        f.add(sptrOf<MathAtom>(build(child(id, 0)), x.flag ? TexStyle::display : TexStyle::text));
        return;
      case NodeKind::list:
        lowerList(id, f, 0);
        return;
      default:
        return;
    }
  }

  // --- characters and spaces -----------------------------------------------

  static sptr<Atom> singleChar(c32 chr, bool math) {
    const c32 code = convertToRomanNumber(chr);
    if (math) {
      const auto it = Formula::_charToSymbol.find(code);
      if (it != Formula::_charToSymbol.end()) return SymbolAtom::get(it->second);
    }
    return sptrOf<CharAtom>(code, math);
  }

  static sptr<Atom> charAtom(const Node& x) {
    const bool math = x.mode == Mode::math;
    // Several code points that cannot be drawn apart are one run of text.
    if (codepoints(x.text) > 1) return sptrOf<TextAtom>(x.text, math);
    return singleChar(x.cp, math);
  }

  void spaces(const Node& x, Formula& f, bool skipFirst) {
    if (x.aux == 1) {  // ~
      f.add(sptrOf<SpaceAtom>());
      return;
    }
    if (x.mode == Mode::math) return;
    if (x.aux == 2 && !_opts.keepDroppedSpaces) return;
    const int n = legacySpaceCount(x.raw, skipFirst);
    for (int i = 0; i < n; i++) {
      f.add(sptrOf<SpaceAtom>(false));
      f.add(sptrOf<BreakMarkAtom>());
    }
  }

  // --- commands --------------------------------------------------------------

  static sptr<Atom> unknownAtom(const std::string& name) {
    auto rm = sptrOf<FontStyleAtom>(
      FontStyle::tt, false, Formula("\\mathtt{{\\backslash}" + name + "}")._root);
    return sptrOf<ColorAtom>(rm, TRANSPARENT, RED);
  }

  /** An argument's source text, or "" when it was not given. */
  std::string rawOf(NodeId arg) const {
    const Node& a = node(arg);
    return a.flag ? a.raw : std::string();
  }

  /** Text for an engine handler, which parses it without preprocessing:
   *  the old parser had already rewritten every `\begin{name}` in the whole
   *  input as `\name@env{...}`, and dropped every `%` comment, before any
   *  handler ran. The recorded text keeps comments (they are part of the
   *  whitespace before a token), so without this they were drawn. */
  static std::string legacyText(const std::string& raw) {
    if (raw.find("\\begin") == std::string::npos && raw.find('%') == std::string::npos) return raw;
    try {
      Formula scratch;
      const microtex::Parser tp(true, raw, &scratch, true);
      return tp.latex();
    } catch (const std::exception&) {
      return raw;
    }
  }

  void command(NodeId id, Formula& f) {
    const Node& x = node(id);
    const CommandSpec* spec = x.flag ? nullptr : findCommand(x.text);
    if (spec != nullptr && spec->shape == Shape::postfix && count(id) == 1) {
      // \limits: the item before it is taken back off, marked, and put back.
      lowerItem(child(id, 0), f);
      auto atom = popBack(f);
      if (atom != nullptr) {
        atom->_limitsType = x.text == "limits"   ? LimitsType::limits
                            : x.text == "nolimits" ? LimitsType::noLimits
                                                   : LimitsType::normal;
      }
      f.add(atom);
      return;
    }
    f.add(commandAtom(id));
  }

  /** The atom a command stands for on its own. */
  sptr<Atom> commandAtom(NodeId id) {
    const Node& x = node(id);
    const std::string& name = x.text;
    const CommandSpec* spec = x.flag ? nullptr : findCommand(name);
    if (spec == nullptr) {
      if (!x.flag) {
        if (Formula::isPredefined(name)) return Formula::get(name)->_root;
        if (auto s = SymbolAtom::get(name)) return s;
      }
      return unknownAtom(name);
    }
    if (name == "kern") {
      const auto [value, unit] = Units::getDimen(rawOf(child(id, 0)));
      return sptrOf<SpaceAtom>(unit, value, 0.f, 0.f);
    }
    if (name == "char") return charCode(rawOf(child(id, 0)), x.mode == Mode::math);
    if (name == "middle") return middleAtom(child(id, 0));
    // Rules and \intertext outside an alignment (the parser warned), line
    // breaks (read by the list), and catcode switches (read by the lexer).
    if (spec->special || name == "makeatletter" || name == "makeatother") return nullptr;
    // A declaration or infix command read as a one-token argument has no
    // operand here; the old parser read on past the argument for one.
    if (spec->shape != Shape::prefix) return nullptr;
    return bridge(id, name, spec);
  }

  sptr<Atom> charCode(const std::string& raw, bool math) {
    int radix = 10;
    std::size_t offset = 0;
    if (!raw.empty() && raw[0] == '\'') radix = 8, offset = 1;
    if (!raw.empty() && raw[0] == '"') radix = 16, offset = 1;
    int n = 0;
    str2int(raw.c_str() + offset, raw.length() - offset, n, radix);
    return singleChar(static_cast<c32>(n), math);
  }

  /** A command's arguments as the tree holds them, for a handler written
   *  against CommandArgs: each is lowered here, with the rest of the input,
   *  rather than parsed again from its text. */
  class TreeArgs : public CommandArgs {
  public:
    TreeArgs(Lowerer& lx, bool math, std::vector<std::string> texts, std::vector<NodeId> nodes)
        : _lx(lx), _math(math), _texts(std::move(texts)), _nodes(std::move(nodes)) {}

    const std::string& text(std::size_t i) const override {
      static const std::string none;
      return i < _texts.size() ? _texts[i] : none;
    }

    sptr<Atom> formula(std::size_t i, bool math, bool) override {
      return _lx.argumentFormula(i < _nodes.size() ? _nodes[i] : kNoNode, text(i), math);
    }

    bool isMathMode() const override { return _math; }

    bool isPartial() const override { return true; }

  private:
    Lowerer& _lx;
    bool _math;
    std::vector<std::string> _texts;
    std::vector<NodeId> _nodes;
  };

  /** An argument read as a formula in the mode asked for. The tree has it
   *  when the argument was parsed in that mode; otherwise (a raw argument
   *  that a handler reads as a formula) its text is read now, as a piece of
   *  input of its own. Its problems are not reported: such an argument is
   *  raw because it is not a formula to check -- `\underaccent{\dot}{x}`
   *  gives an accent command without its argument, and means to. */
  sptr<Atom> argumentFormula(NodeId arg, const std::string& text, bool math) {
    if (arg == kNoNode || !node(arg).flag) return nullptr;
    const Node& a = node(arg);
    const Mode want = math ? Mode::math : Mode::text;
    if (count(arg) == 1 && a.mode == want) return build(child(arg, 0));
    if (text.empty()) return nullptr;
    Diagnostics unreported;
    const Ast fragment = parseLatex(text, want, unreported);
    if (fragment.root == kNoNode) return nullptr;
    Formula g;
    Lowerer(fragment, unreported, _opts).lowerList(fragment.root, g, 0);
    return g._root;
  }

  /** Build a command with its engine handler, its arguments laid out as the
   *  old parser laid them out: mandatory ones from 1, optional ones after
   *  them. */
  sptr<Atom> bridge(NodeId id, const std::string& name, const CommandSpec* spec) {
    MacroInfo* mac = MacroInfo::get(name);
    if (mac == nullptr) return unknownAtom(name);
    std::vector<std::string> args(static_cast<std::size_t>(mac->argc) + 12);
    std::vector<NodeId> nodes(args.size(), kNoNode);
    args[0] = name;
    std::size_t mandatory = 1;
    std::size_t optional = static_cast<std::size_t>(mac->argc) + 1;
    const std::uint32_t n = count(id);
    for (std::uint32_t i = 0; i < n && i < spec->args.size(); i++) {
      // A URL's `%` is a character, as the lexer read it there.
      const std::string raw = spec->args[i].kind == ArgKind::url ? rawOf(child(id, i))
                                                                 : legacyText(rawOf(child(id, i)));
      std::size_t& at = spec->args[i].optional ? optional : mandatory;
      if (at < args.size()) {
        nodes[at] = child(id, i);
        args[at++] = raw;
      }
    }
    if (auto* cm = dynamic_cast<CommandMacro*>(mac)) {
      TreeArgs a(*this, node(id).mode == Mode::math, std::move(args), std::move(nodes));
      try {
        return cm->call(a);
      } catch (const std::exception& e) {
        _diags.warn(node(id).span, "\\" + name + ": " + clean(e.what()) + "; drawn as its name");
        return unknownAtom(name);
      }
    }
    return invoke(mac, args, node(id));
  }

  sptr<Atom> invoke(MacroInfo* mac, std::vector<std::string>& args, const Node& at) {
    Formula scratch;
    microtex::Parser tp(true, "", &scratch, false, at.mode == Mode::math);
    try {
      return mac->invoke(tp, args);
    } catch (const std::exception& e) {
      _diags.warn(at.span, "\\" + at.text + ": " + clean(e.what()) + "; drawn as its name");
      return unknownAtom(at.text);
    }
  }

  /** The old handlers' error text, less the wrapper the dispatcher adds. */
  static std::string clean(std::string msg) {
    const std::string mark = "caused by: ";
    const auto p = msg.rfind(mark);
    if (p != std::string::npos) msg = msg.substr(p + mark.size());
    for (char& c : msg) {
      if (c == '\n') c = ' ';
    }
    return msg;
  }

  // --- scripts ---------------------------------------------------------------

  void scripts(NodeId id, Formula& f) {
    const Node& x = node(id);
    const NodeId base = child(id, 0);
    const NodeId sub = child(id, 1);
    const NodeId sup = child(id, 2);
    // The base is in the formula first, as it was when the old parser met
    // the scripts; they take it back off.
    if (node(base).kind != NodeKind::list) lowerItem(base, f);
    const std::string& order = x.text;
    std::size_t i = 0;
    while (i < order.size()) {
      const char c = order[i];
      if (c == '\'' || c == '`') {
        std::size_t j = i;
        while (j < order.size() && order[j] == c) j++;
        f.add(sptrOf<CumulativeScriptsAtom>(popBack(f), nullptr, primes(c == '\'', j - i)));
        i = j;
        continue;
      }
      if (c == '"') {
        f.add(sptrOf<CumulativeScriptsAtom>(popBack(f), nullptr, sptrOf<CharAtom>(0x02033, true)));
        i++;
        continue;
      }
      sptr<Atom> subAtom, supAtom;
      const bool pair = i + 1 < order.size() && (order[i + 1] == '^' || order[i + 1] == '_') &&
                        order[i + 1] != c;
      if (c == '^') {
        supAtom = scriptArgument(sup, f);
        if (pair) subAtom = scriptArgument(sub, f);
      } else {
        subAtom = scriptArgument(sub, f);
        if (pair) supAtom = scriptArgument(sup, f);
      }
      i += pair ? 2 : 1;
      f.add(attach(f, subAtom, supAtom));
    }
  }

  static sptr<Atom> primes(bool prime, std::size_t count) {
    static const c32 primeChars[] = {0x02032, 0x02033, 0x02034, 0x02057};
    static const c32 backChars[] = {0x02035, 0x02036, 0x02037};
    const c32* arr = prime ? primeChars : backChars;
    const std::size_t max = prime ? 4 : 3;
    if (count <= max) return sptrOf<CharAtom>(arr[count - 1], true);
    auto row = sptrOf<RowAtom>();
    for (std::size_t i = 0; i < count; i++) row->add(sptrOf<CharAtom>(arr[0], true));
    return row;
  }

  static sptr<Atom> attach(Formula& f, const sptr<Atom>& sub, const sptr<Atom>& sup) {
    if (f._root == nullptr) return sptrOf<ScriptsAtom>(nullptr, sub, sup);
    sptr<Atom> atom;
    if (auto* rm = dynamic_cast<RowAtom*>(f._root.get())) {
      atom = rm->popBack();
    } else {
      atom = f._root;
      f._root = nullptr;
    }
    if (auto* ca = dynamic_cast<CumulativeScriptsAtom*>(atom.get())) {
      ca->addSubscript(sub);
      ca->addSuperscript(sup);
      return atom;
    }
    if (atom->rightType() == AtomType::bigOperator) return sptrOf<OperatorAtom>(atom, sub, sup);
    return sptrOf<ScriptsAtom>(atom, sub, sup);
  }

  /** A script's argument, as the old parser's getArgument() built it. */
  sptr<Atom> scriptArgument(NodeId list, const Formula& f) {
    if (count(list) == 0) return sptrOf<EmptyAtom>();
    const NodeId item = child(list, 0);
    const Node& y = node(item);
    switch (y.kind) {
      case NodeKind::group:
        return groupAtom(build(child(item, 0)), f);
      case NodeKind::character:
        return charAtom(y);
      case NodeKind::command:
        return commandAtom(item);
      case NodeKind::leftRight:
        return leftRight(item);
      case NodeKind::environment:
        return groupAtom(environment(item), f);
      default: {
        Formula g;
        lowerItem(item, g);
        return g._root;
      }
    }
  }

  // --- \over and kin -----------------------------------------------------------

  void infix(NodeId id, Formula& f) {
    const Node& x = node(id);
    const std::string& name = x.text;
    // The numerator is what was in the formula before the command.
    lowerList(child(id, 0), f, 0);
    auto num = popFormulaAtom(f);
    std::vector<std::string> args;
    for (std::uint32_t i = 2; i < count(id); i++) args.push_back(rawOf(child(id, i)));
    auto den = build(child(id, 1));
    if (num == nullptr || den == nullptr) {
      _diags.warn(x.span, "\\" + name + ": a fraction needs both a numerator and a denominator");
      f.add(num != nullptr ? num : den);
      return;
    }
    const auto delims = [&](bool rule, const Dimen* thickness) -> sptr<Atom> {
      auto frac = thickness != nullptr ? sptrOf<FracAtom>(num, den, rule, *thickness)
                                       : sptrOf<FracAtom>(num, den, rule);
      try {
        return sptrOf<FencedAtom>(frac, args.size() > 0 ? args[0] : "",
                                  args.size() > 1 ? args[1] : "");
      } catch (const std::exception& e) {
        // TeX puts a null delimiter in place of one that is not.
        _diags.warn(x.span, "\\" + name + ": " + clean(e.what()) + "; left out");
        return frac;
      }
    };
    sptr<Atom> out;
    if (name == "over") {
      out = sptrOf<FracAtom>(num, den, true);
    } else if (name == "atop") {
      out = sptrOf<FracAtom>(num, den, false);
    } else if (name == "above") {
      const Dimen thick = Units::getDimen(args.empty() ? "" : args.back());
      out = sptrOf<FracAtom>(num, den, true, thick);
    } else if (name == "choose" || name == "brack" || name == "bangle" || name == "brace") {
      const char* l = name == "choose" ? "lparen" : name == "brack" ? "lbrack"
                      : name == "bangle" ? "langle" : "lbrace";
      const char* r = name == "choose" ? "rparen" : name == "brack" ? "rbrack"
                      : name == "bangle" ? "rangle" : "rbrace";
      out = sptrOf<FencedAtom>(sptrOf<FracAtom>(num, den, false), l, r);
    } else if (name == "overwithdelims") {
      out = delims(true, nullptr);
    } else if (name == "atopwithdelims") {
      out = delims(false, nullptr);
    } else {  // abovewithdelims
      const Dimen thick = Units::getDimen(args.size() > 2 ? args[2] : "");
      out = delims(true, &thick);
    }
    f.add(out);
  }

  // --- declarations ----------------------------------------------------------

  void declaration(NodeId id, Formula& f) {
    const Node& x = node(id);
    const std::string& name = x.text;
    const NodeId body = child(id, count(id) - 1);
    const bool math = x.mode == Mode::math;
    try {
      if (isTextFont(name)) {
        const auto atom = build(body, true);
        f.add(sptrOf<FontStyleAtom>(FontContext::mainFontStyleOf(name), math, atom));
      } else if (isSize(name)) {
        auto a = build(body);
        f.add(sptrOf<ScaleAtom>(a == nullptr ? sptrOf<EmptyAtom>() : a, sizeFactor(name)));
      } else if (name == "color") {
        const color c = ColorAtom::getColor(rawOf(child(id, 0)));
        f.add(sptrOf<ColorAtom>(build(body), TRANSPARENT, c));
      } else {  // \displaystyle and kin
        auto g = build(body);
        f.add(sptrOf<StyleAtom>(texStyleOf(name), g == nullptr ? sptrOf<EmptyAtom>() : g));
      }
    } catch (const std::exception& e) {
      _diags.warn(x.span, "\\" + name + ": " + clean(e.what()));
    }
  }

  // --- \left \middle \right ----------------------------------------------------

  sptr<Atom> delimiter(const std::string& raw) {
    Formula scratch;
    microtex::Parser tp(true, "", &scratch, false, true);
    auto atom = Formula(tp, raw, false)._root;
    if (auto* big = dynamic_cast<BigSymbolAtom*>(atom.get())) atom = big->_delim;
    return atom;
  }

  /** The symbol name a \left, \middle or \right delimiter stands for; one
   *  that is not a delimiter warns and is left out, as TeX puts a null
   *  delimiter in its place. `\middle\vert` used to hand the engine the
   *  text "\vert" and fail at layout. */
  std::string delimiterName(const sptr<Atom>& atom, NodeId arg, const std::string& who) {
    const auto sym = std::dynamic_pointer_cast<CharSymbol>(atom);
    const std::string name = sym != nullptr ? sym->name() : std::string();
    if (name.empty() || name == "." || delimiterSymbol(name) != nullptr) return name;
    _diags.warn(node(arg).span, who + ": " + rawOf(arg) + " is not a delimiter; left out");
    return ".";
  }

  sptr<Atom> middleAtom(NodeId arg) {
    auto atom = delimiter(rawOf(arg));
    if (std::dynamic_pointer_cast<CharSymbol>(atom) == nullptr) {
      if (!rawOf(arg).empty()) {
        _diags.warn(node(arg).span, "\\middle: " + rawOf(arg) + " is not a delimiter; left out");
      }
      return sptrOf<MiddleAtom>(".");
    }
    return sptrOf<MiddleAtom>(delimiterName(atom, arg, "\\middle"));
  }

  sptr<Atom> leftRight(NodeId id) {
    const std::uint32_t n = count(id);
    auto left = delimiter(rawOf(child(id, 0)));
    auto right = delimiter(rawOf(child(id, n - 1)));
    Formula tf;
    for (std::uint32_t i = 1; i + 1 < n; i++) {
      const NodeId c = child(id, i);
      if (node(c).kind == NodeKind::list) {
        lowerList(c, tf, 0);
      } else {
        tf.add(middleAtom(c));
      }
    }
    auto sl = std::dynamic_pointer_cast<CharSymbol>(left);
    auto sr = std::dynamic_pointer_cast<CharSymbol>(right);
    if (sl != nullptr && sr != nullptr) {
      return sptrOf<FencedAtom>(tf._root, delimiterName(left, child(id, 0), "\\left"),
                                delimiterName(right, child(id, n - 1), "\\right"), tf.middle());
    }
    auto ra = sptrOf<RowAtom>();
    ra->add(left);
    ra->add(tf._root);
    ra->add(right);
    return ra;
  }

  // --- environments ----------------------------------------------------------

  /** Built by the engine's environment handler from its source text, as
   *  the old parser's `\name@@env{...}{body}` did. */
  sptr<Atom> environment(NodeId id) {
    const Node& x = node(id);
    const EnvSpec* spec = findEnvironment(x.text);
    const std::string macName = (spec != nullptr ? x.text : std::string("matrix")) + "@@env";
    MacroInfo* mac = MacroInfo::get(macName);
    if (mac == nullptr) return unknownAtom("begin");
    std::vector<std::string> args(static_cast<std::size_t>(mac->argc) + 12);
    args[0] = macName;
    std::size_t k = 1;
    for (std::uint32_t i = 0; i < count(id); i++) {
      const NodeId c = child(id, i);
      if (node(c).kind != NodeKind::argument) break;
      if (k < args.size()) args[k++] = rawOf(c);
    }
    // The old template put the body between spaces.
    if (k < args.size()) args[k] = " " + legacyText(x.raw) + " ";
    Node at = x;
    at.text = "begin{" + x.text + "}";
    return invoke(mac, args, at);
  }
};

}  // namespace

void lowerInto(const Ast& ast, Formula& formula, Diagnostics& diagnostics,
               const LowerOptions& options) {
  Lowerer(ast, diagnostics, options).run(formula);
}

}  // namespace microtex::front
