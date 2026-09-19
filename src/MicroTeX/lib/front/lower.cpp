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
#include "atom/atom_matrix.h"
#include "atom/atom_misc.h"
#include "atom/atom_operator.h"
#include "atom/atom_row.h"
#include "atom/atom_scripts.h"
#include "atom/atom_space.h"
#include "atom/atom_text.h"
#include "box/box_factory.h"
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

bool isRule(const std::string& name) {
  return name == "hline" || name == "thickhline" || name == "cline";
}

bool isSize(const std::string& n) {
  return n == "tiny" || n == "scriptsize" || n == "footnotesize" || n == "small" ||
         n == "normalsize" || n == "large" || n == "Large" || n == "LARGE" || n == "huge" ||
         n == "Huge";
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
  Lowerer(const Ast& ast, Diagnostics& diags) : _ast(ast), _diags(diags) {}

  void run(Formula& f) {
    if (_ast.root != kNoNode) lowerList(_ast.root, f, 0);
  }

private:
  const Ast& _ast;
  Diagnostics& _diags;

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
  void lowerList(NodeId list, Formula& f, std::uint32_t from) {
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
      lowerItem(id, f);
    }
  }

  /** A list built in a formula of its own; its root. */
  sptr<Atom> build(NodeId list) {
    Formula g;
    lowerList(list, g, 0);
    return g._root;
  }

  void lowerItem(NodeId id, Formula& f) {
    const Node& x = node(id);
    switch (x.kind) {
      case NodeKind::character:
        f.add(charAtom(x));
        return;
      case NodeKind::space:
        spaces(x, f);
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
      case NodeKind::leftRight: {
        sptr<Atom> after;
        f.add(leftRight(id, after));
        f.add(after);
        return;
      }
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

  /** A space in text is one space, however much whitespace it was, as in
   *  TeX (the lexer has already dropped what TeX drops: the spaces after a
   *  control word). The old parser drew one more for each line end in the
   *  run, and kept those after a control word. */
  void spaces(const Node& x, Formula& f) {
    if (x.aux == 1) {  // ~
      f.add(sptrOf<SpaceAtom>());
      return;
    }
    if (x.mode == Mode::math) return;
    f.add(sptrOf<SpaceAtom>(false));
    f.add(sptrOf<BreakMarkAtom>());
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

  /** An argument's text for a handler that reads it as text (a length, a
   *  colour, a name). The recorded text keeps `%` comments -- they are part
   *  of the whitespace before a token -- and a handler would read them as
   *  characters. A comment runs to its line end, which stays. */
  static std::string stripComments(const std::string& raw) {
    if (raw.find('%') == std::string::npos) return raw;
    std::string out;
    for (std::size_t i = 0; i < raw.size(); i++) {
      const char c = raw[i];
      if (c == '\\' && i + 1 < raw.size()) {
        out += c;
        out += raw[++i];
        continue;
      }
      if (c == '%') {
        while (i + 1 < raw.size() && raw[i + 1] != '\n') i++;
        continue;
      }
      out += c;
    }
    return out;
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
    auto atom = commandAtom(id, f);
    f.add(atom);
    // A rule in an alignment is a row of its own, as the old parser made it.
    if (f.isArrayMode() && dynamic_cast<HlineAtom*>(atom.get()) != nullptr) {
      static_cast<ArrayFormula&>(f).addRow();
    }
  }

  /** The atom a command stands for on its own. */
  /** The atom a command stands for, built for `f`, the formula it goes
   *  into: a rule or \intertext in an alignment works on it. */
  sptr<Atom> commandAtom(NodeId id, Formula& f) {
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
    if (name == "middle") {
      Formula g;
      middle(child(id, 0), g);
      return g._root;
    }
    // At the top of a cell, \color colours the cell rather than what follows.
    if (name == "color" && f.isArrayMode()) {
      return sptrOf<CellForegroundAtom>(ColorAtom::getColor(stripComments(rawOf(child(id, 0)))));
    }
    // In an alignment, a rule or \intertext is its handler's, which works
    // on the alignment's rows.
    if (f.isArrayMode() && (isRule(name) || name == "intertext")) return bridge(id, name, spec, f);
    // Rules and \intertext outside an alignment (the parser warned), line
    // breaks (read by the list), and catcode switches (read by the lexer).
    if (spec->special || name == "makeatletter" || name == "makeatother") return nullptr;
    // A declaration or infix command read as a one-token argument has no
    // operand here; the old parser read on past the argument for one.
    if (spec->shape != Shape::prefix) return nullptr;
    return bridge(id, name, spec, f);
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
    TreeArgs(Lowerer& lx, bool math, std::vector<std::string> texts, std::vector<NodeId> nodes,
             Formula* here)
        : _lx(lx), _math(math), _texts(std::move(texts)), _nodes(std::move(nodes)), _here(here) {}

    const std::string& text(std::size_t i) const override {
      static const std::string none;
      return i < _texts.size() ? _texts[i] : none;
    }

    sptr<Atom> formula(std::size_t i, bool math, bool) override {
      return _lx.argumentFormula(i < _nodes.size() ? _nodes[i] : kNoNode, text(i), math);
    }

    bool isMathMode() const override { return _math; }

    bool isPartial() const override { return true; }

    sptr<ArrayFormula> alignment(std::size_t i) override {
      return _lx.alignmentOf(i < _nodes.size() ? _nodes[i] : kNoNode);
    }

    sptr<Atom> formulaOf(const std::string& latex, bool math) override {
      return fragment(latex, math);
    }

    sptr<ArrayFormula> alignmentOfText(const std::string& latex) override {
      Diagnostics unreported;
      const Ast ast = parseLatex("\\begin{matrix}" + latex + "\\end{matrix}", Mode::math, unreported);
      const NodeId env = ast.root != kNoNode && ast.childCount(ast.root) > 0
                           ? ast.child(ast.root, 0) : kNoNode;
      return Lowerer(ast, unreported).alignmentOf(env);
    }

    ArrayFormula* alignmentHere() override {
      return _here != nullptr && _here->isArrayMode() ? static_cast<ArrayFormula*>(_here) : nullptr;
    }

  private:
    Lowerer& _lx;
    bool _math;
    std::vector<std::string> _texts;
    std::vector<NodeId> _nodes;
    Formula* _here;
  };

  /** LaTeX not from the input -- a raw argument read as a formula, text a
   *  handler put together -- read by the front end as a piece of input of
   *  its own. Its problems are not reported: they are not the user's. */
  static sptr<Atom> fragment(const std::string& latex, bool math) {
    if (latex.empty()) return nullptr;
    Diagnostics unreported;
    const Ast ast = parseLatex(latex, math ? Mode::math : Mode::text, unreported);
    if (ast.root == kNoNode) return nullptr;
    Formula g;
    Lowerer(ast, unreported).lowerList(ast.root, g, 0);
    return g._root;
  }

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
    return fragment(text, math);
  }

  /** Build a command with its engine handler, its arguments laid out as the
   *  old parser laid them out: mandatory ones from 1, optional ones after
   *  them. */
  sptr<Atom> bridge(NodeId id, const std::string& name, const CommandSpec* spec, Formula& f) {
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
                                                                 : stripComments(rawOf(child(id, i)));
      std::size_t& at = spec->args[i].optional ? optional : mandatory;
      if (at < args.size()) {
        nodes[at] = child(id, i);
        args[at++] = raw;
      }
    }
    auto* cm = dynamic_cast<CommandMacro*>(mac);
    // Every handler the front end reaches reads CommandArgs; the rest serve
    // the old parser only (definitions, \over and kin, ...).
    if (cm == nullptr) return unknownAtom(name);
    TreeArgs a(*this, node(id).mode == Mode::math, std::move(args), std::move(nodes), &f);
    try {
      return cm->call(a);
    } catch (const std::exception& e) {
      _diags.warn(node(id).span, "\\" + name + ": " + clean(e.what()) + "; drawn as its name");
      return unknownAtom(name);
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
    // TeX's prime: `'` is `^{\prime}`, and primes with a `^` after them are
    // one superscript -- f''^2 is f^{\prime\prime 2} -- so f' draws as
    // f^{\prime} does.
    if (order.find('\'') != std::string::npos) {
      Formula primed;
      for (const char c : order) {
        if (c == '\'') primed.add(SymbolAtom::get("prime"));
      }
      if (order.find('^') != std::string::npos) appendScript(sup, primed);
      const sptr<Atom> supAtom = groupAtom(primed._root, f);
      const sptr<Atom> subAtom = order.find('_') != std::string::npos ? scriptArgument(sub, f) : nullptr;
      f.add(attach(f, subAtom, supAtom));
      return;
    }
    std::size_t i = 0;
    while (i < order.size()) {
      const char c = order[i];
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

  static sptr<Atom> attach(Formula& f, const sptr<Atom>& sub, const sptr<Atom>& sup) {
    if (f._root == nullptr) return sptrOf<ScriptsAtom>(nullptr, sub, sup);
    sptr<Atom> atom;
    if (auto* rm = dynamic_cast<RowAtom*>(f._root.get())) {
      atom = rm->popBack();
    } else {
      atom = f._root;
      f._root = nullptr;
    }
    if (atom->rightType() == AtomType::bigOperator) return sptrOf<OperatorAtom>(atom, sub, sup);
    return sptrOf<ScriptsAtom>(atom, sub, sup);
  }

  /** A superscript's content added after primes, as TeX's `'` takes it:
   *  a group's braces go, so f'^{ab} is f^{\prime ab}. */
  void appendScript(NodeId list, Formula& g) {
    if (count(list) == 0) return;
    const NodeId item = child(list, 0);
    if (node(item).kind == NodeKind::group) {
      lowerList(child(item, 0), g, 0);
    } else {
      lowerItem(item, g);
    }
  }

  /** A script's argument, as the old parser's getArgument() built it. */
  sptr<Atom> scriptArgument(NodeId list, Formula& f) {
    if (count(list) == 0) return sptrOf<EmptyAtom>();
    const NodeId item = child(list, 0);
    const Node& y = node(item);
    switch (y.kind) {
      case NodeKind::group:
        return groupAtom(build(child(item, 0)), f);
      case NodeKind::character:
        return charAtom(y);
      case NodeKind::command:
        return commandAtom(item, f);
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
        const auto atom = build(body);
        f.add(sptrOf<FontStyleAtom>(FontContext::mainFontStyleOf(name), math, atom));
      } else if (name == "cal" || name == "frak") {
        // TeX's old switches for \mathcal and \mathfrak.
        const auto style = FontContext::mathFontStyleOf(name == "cal" ? "mathcal" : "mathfrak");
        f.add(sptrOf<FontStyleAtom>(style, true, build(body)));
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
    auto atom = fragment(raw, true);
    if (auto* big = dynamic_cast<BigSymbolAtom*>(atom.get())) atom = big->_delim;
    return atom;
  }

  /** Whether `atom` names a delimiter ("" and "." are none, and are fine). */
  static bool isDelimiter(const sptr<Atom>& atom) {
    const auto sym = std::dynamic_pointer_cast<CharSymbol>(atom);
    if (sym == nullptr) return false;
    const std::string name = sym->name();
    return name.empty() || name == "." || delimiterSymbol(name) != nullptr;
  }

  /** The symbol name of a \left, \middle or \right delimiter. One that is
   *  not a delimiter is TeX's "Missing delimiter": a null delimiter goes in
   *  its place, with a warning, and the token is read again as itself --
   *  `put` says where. `\middle\vert` used to hand the engine the text
   *  "\vert" and fail at layout. */
  std::string delimiterName(const sptr<Atom>& atom, NodeId arg, const std::string& who,
                            const char* put) {
    if (isDelimiter(atom)) return std::dynamic_pointer_cast<CharSymbol>(atom)->name();
    if (!rawOf(arg).empty()) {
      _diags.warn(node(arg).span, who + ": " + rawOf(arg) + " is not a delimiter; drawn " + put);
    }
    return ".";
  }

  /** A \middle, and what it put back when its delimiter is not one. */
  void middle(NodeId arg, Formula& f) {
    auto atom = delimiter(rawOf(arg));
    f.add(sptrOf<MiddleAtom>(delimiterName(atom, arg, "\\middle", "after it")));
    if (!isDelimiter(atom)) f.add(atom);
  }

  /** \left...\right. A \left whose delimiter is not one puts it back inside
   *  the fence; a \right's goes after the fence, into `after`. */
  sptr<Atom> leftRight(NodeId id, sptr<Atom>& after) {
    const std::uint32_t n = count(id);
    auto left = delimiter(rawOf(child(id, 0)));
    auto right = delimiter(rawOf(child(id, n - 1)));
    auto sl = std::dynamic_pointer_cast<CharSymbol>(left);
    auto sr = std::dynamic_pointer_cast<CharSymbol>(right);
    const bool fenced = sl != nullptr && sr != nullptr;
    Formula tf;
    std::string leftName, rightName;
    if (fenced) {
      leftName = delimiterName(left, child(id, 0), "\\left", "inside the fence");
      rightName = delimiterName(right, child(id, n - 1), "\\right", "after the fence");
      if (!isDelimiter(left)) tf.add(left);
      if (!isDelimiter(right)) after = right;
    }
    for (std::uint32_t i = 1; i + 1 < n; i++) {
      const NodeId c = child(id, i);
      if (node(c).kind == NodeKind::list) {
        lowerList(c, tf, 0);
      } else {
        middle(c, tf);
      }
    }
    if (fenced) return sptrOf<FencedAtom>(tf._root, leftName, rightName, tf.middle());
    auto ra = sptrOf<RowAtom>();
    ra->add(left);
    ra->add(tf._root);
    ra->add(right);
    return ra;
  }

  /** \left...\right as one atom, with what a \right put back after it. */
  sptr<Atom> leftRight(NodeId id) {
    sptr<Atom> after;
    auto fence = leftRight(id, after);
    if (after == nullptr) return fence;
    auto ra = sptrOf<RowAtom>();
    ra->add(fence);
    ra->add(after);
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
    if (k < args.size()) args[k] = " " + stripComments(x.raw) + " ";
    Node at = x;
    at.text = "begin{" + x.text + "}";
    auto* cm = dynamic_cast<CommandMacro*>(mac);
    if (cm == nullptr) return unknownAtom(at.text);
    std::vector<NodeId> nodes(args.size(), kNoNode);
    if (k < nodes.size()) nodes[k] = id;
    TreeArgs a(*this, true, std::move(args), std::move(nodes), nullptr);
    try {
      return cm->call(a);
    } catch (const std::exception& e) {
      _diags.warn(at.span, "\\" + at.text + ": " + clean(e.what()) + "; drawn as its name");
      return unknownAtom(at.text);
    }
  }

  /** An alignment's body, from its rows and cells, in the order the old
   *  parser filled its formula: `&` starts a cell, `\\` and \cr end a row,
   *  and a rule or \intertext ends one itself (command()). */
  sptr<ArrayFormula> alignmentOf(NodeId env) {
    auto arr = sptrOf<ArrayFormula>();
    if (env == kNoNode || node(env).kind != NodeKind::environment) return arr;
    for (std::uint32_t r = 0; r < count(env); r++) {
      const NodeId row = child(env, r);
      if (node(row).kind != NodeKind::row) continue;
      for (std::uint32_t c = 0; c < count(row); c++) {
        if (c > 0) arr->addCol();
        lowerList(child(child(row, c), 0), *arr, 0);
      }
      const std::string& end = node(row).text;
      if (!node(row).raw.empty()) arr->addRowGap(Units::getDimen(node(row).raw));
      if (end == "\\" || end == "cr") arr->addRow();
    }
    return arr;
  }
};

}  // namespace

void lowerInto(const Ast& ast, Formula& formula, Diagnostics& diagnostics) {
  Lowerer(ast, diagnostics).run(formula);
}

}  // namespace microtex::front
