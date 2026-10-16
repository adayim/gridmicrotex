#ifndef MICROTEX_MACRO_TIKZ_H
#define MICROTEX_MACRO_TIKZ_H

// tikzpicture: the picture's source read into a TikzAtom (atom/tikz_atom.h). The reader follows TikZ's
// own grammar -- a statement is a command, its options, and a path of coordinates and operations up to a
// `;` -- and what it does not know it says once and leaves out: a picture that is mostly drawn is more use
// than none.

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "atom/atom_basic.h"
#include "atom/tikz_atom.h"
#include "env/units.h"
#include "macro/macro_args.h"
#include "macro/macro_diagram.h"
#include "macro/macro_env.h"

namespace microtex {

namespace tikz {

using diagram::groupAt;
using diagram::splitTop;
using diagram::trim;

inline bool isWordChar(char c) {
  return std::isalnum(static_cast<unsigned char>(c)) != 0;
}

/** pgfmath, as far as a coordinate needs it: numbers, + - * / ^, brackets and the usual functions. */
class Expr {
public:
  explicit Expr(const std::string& s) : _s(s) {}

  /** The value, or false when the text is not an expression this reads whole. */
  bool value(double& out) {
    _i = 0;
    _ok = true;
    out = sum();
    skip();
    return _ok && _i == _s.size();
  }

private:
  std::string _s;
  std::size_t _i = 0;
  bool _ok = true;

  void skip() {
    while (_i < _s.size() && std::isspace(static_cast<unsigned char>(_s[_i])) != 0) _i++;
  }

  double sum() {
    double v = product();
    for (;;) {
      skip();
      if (_i < _s.size() && (_s[_i] == '+' || _s[_i] == '-')) {
        const char op = _s[_i++];
        const double w = product();
        v = op == '+' ? v + w : v - w;
      } else {
        return v;
      }
    }
  }

  double product() {
    double v = power();
    for (;;) {
      skip();
      if (_i < _s.size() && (_s[_i] == '*' || _s[_i] == '/')) {
        const char op = _s[_i++];
        const double w = power();
        if (op == '*') {
          v *= w;
        } else if (std::fabs(w) < 1e-12) {
          _ok = false;
        } else {
          v /= w;
        }
      } else {
        return v;
      }
    }
  }

  double power() {
    double v = unary();
    skip();
    if (_i < _s.size() && _s[_i] == '^') {
      _i++;
      v = std::pow(v, power());
    }
    return v;
  }

  double unary() {
    skip();
    if (_i < _s.size() && (_s[_i] == '-' || _s[_i] == '+')) {
      const char op = _s[_i++];
      const double v = unary();
      return op == '-' ? -v : v;
    }
    return atom();
  }

  double atom() {
    skip();
    if (_i >= _s.size()) {
      _ok = false;
      return 0;
    }
    if (_s[_i] == '(') {
      _i++;
      const double v = sum();
      skip();
      if (_i < _s.size() && _s[_i] == ')') {
        _i++;
      } else {
        _ok = false;
      }
      return v;
    }
    if (std::isdigit(static_cast<unsigned char>(_s[_i])) != 0 || _s[_i] == '.') {
      char* end = nullptr;
      const double v = std::strtod(_s.c_str() + _i, &end);
      _i = static_cast<std::size_t>(end - _s.c_str());
      return v;
    }
    if (std::isalpha(static_cast<unsigned char>(_s[_i])) != 0) {
      std::string name;
      while (_i < _s.size() && std::isalpha(static_cast<unsigned char>(_s[_i])) != 0) name += _s[_i++];
      if (name == "pi") return 3.14159265358979323846;
      if (name == "e") return 2.71828182845904523536;
      skip();
      if (_i >= _s.size() || _s[_i] != '(') {
        _ok = false;
        return 0;
      }
      _i++;
      std::vector<double> args;
      skip();
      if (_i < _s.size() && _s[_i] != ')') {
        args.push_back(sum());
        skip();
        while (_i < _s.size() && _s[_i] == ',') {
          _i++;
          args.push_back(sum());
          skip();
        }
      }
      if (_i < _s.size() && _s[_i] == ')') {
        _i++;
      } else {
        _ok = false;
      }
      const double a = args.empty() ? 0.0 : args[0], b = args.size() > 1 ? args[1] : 0.0;
      const double toRad = 3.14159265358979323846 / 180.0;
      if (name == "sin") return std::sin(a * toRad);
      if (name == "cos") return std::cos(a * toRad);
      if (name == "tan") return std::tan(a * toRad);
      if (name == "sqrt") return std::sqrt(std::max(0.0, a));
      if (name == "abs") return std::fabs(a);
      if (name == "min") return std::min(a, b);
      if (name == "max") return std::max(a, b);
      if (name == "floor") return std::floor(a);
      if (name == "ceil") return std::ceil(a);
      if (name == "round") return std::round(a);
      if (name == "exp") return std::exp(a);
      if (name == "ln") return std::log(a);
      if (name == "pow") return std::pow(a, b);
      _ok = false;
      return 0;
    }
    _ok = false;
    return 0;
  }
};

class Reader {
public:
  Reader(CommandArgs& args, TikzAtom& pic) : _args(args), _pic(pic) {
    // pgf's own styles that a picture may change.
    _styles["edge from parent"] = "draw";
  }

  /** The picture: its options, then its statements. */
  void picture(std::string body) {
    const std::string options = listPeelOptional(body);
    Style root;
    root.draw = Paint();
    applyOptions(root, options, false, 0);
    run(body, root, 0);
  }

private:
  CommandArgs& _args;
  TikzAtom& _pic;
  std::set<std::string> _warned;
  std::map<std::string, std::string> _styles;      // name -> its options
  std::map<std::string, std::string> _appended;
  int _nodes = 0;
  int _treeLevel = 0;                              // how deep in a tree the options being read are
  int _treeNames = 0;                              // parents of trees that had no name
  std::string _nodeExtra;                          // a style a node takes after `every node`: `every child node`
  std::vector<PathItem> _edges;                    // the edges from parents, drawn after the path that grew them

  void warn(const std::string& what) {
    if (_warned.insert(what).second) _args.warn(what);
  }

  // --- numbers and lengths ----------------------------------------------------------------------

  static bool isUnit(const std::string& u) {
    static const std::set<std::string> units = {"pt", "cm", "mm", "in", "bp", "em", "ex", "pc", "sp", "dd", "cc", "mu", "px"};
    return units.count(u) > 0;
  }

  /** A length as TikZ reads one: `2cm`, `1.5`, `\w*2` once it is a number -- a bare number is centimetres. */
  Dimen dimen(const std::string& text) {
    const std::string s = trim(text);
    if (s.empty()) return Dimen();
    // A number and a unit.
    std::size_t i = 0;
    if (s[i] == '+' || s[i] == '-') i++;
    while (i < s.size() && (std::isdigit(static_cast<unsigned char>(s[i])) != 0 || s[i] == '.')) i++;
    if (i > 0 && i < s.size()) {
      const std::string unit = trim(s.substr(i));
      if (isUnit(unit)) return Units::getDimen(trim(s.substr(0, i)) + unit);
    }
    double v = 0;
    Expr e(s);
    if (e.value(v)) return {static_cast<float>(v), UnitType::cm};
    return Dimen();
  }

  bool number(const std::string& text, double& out) {
    Expr e(trim(text));
    return e.value(out);
  }

  // --- colours ----------------------------------------------------------------------------------

  bool colour(const std::string& text, color& out) {
    return ColorAtom::parseColor(trim(text), out);
  }

  // --- options ----------------------------------------------------------------------------------

  static std::string unbrace(const std::string& v) {
    const std::string t = trim(v);
    if (!t.empty() && t[0] == '{') {
      std::size_t at = 0;
      return trim(groupAt(t, at));
    }
    return t;
  }

  void applyOptions(Style& st, const std::string& list, bool node, int depth) {
    if (depth > 12) {
      warn("tikz: styles nest too deeply: left out");
      return;
    }
    // The options are split at commas, but a point such as `(1,2)` in `fit=(1,2)(3,4)` keeps its own.
    std::vector<std::string> pieces;
    {
      std::string held;
      for (const std::string& raw : splitTop(list, ',')) {
        held += held.empty() ? raw : "," + raw;
        if (std::count(held.begin(), held.end(), '(') <= std::count(held.begin(), held.end(), ')')) {
          pieces.push_back(held);
          held.clear();
        }
      }
      if (!held.empty()) pieces.push_back(held);
    }
    for (const std::string& raw : pieces) {
      const std::string item = trim(raw);
      if (item.empty()) continue;
      // The first `=` outside braces and brackets: `-{Stealth[length=4mm]}` is a key.
      std::size_t eq = std::string::npos;
      for (std::size_t k = 0, nest = 0; k < item.size() && eq == std::string::npos; k++) {
        if (item[k] == '{' || item[k] == '[') nest++;
        if ((item[k] == '}' || item[k] == ']') && nest > 0) nest--;
        if (item[k] == '=' && nest == 0) eq = k;
      }
      // A key with no value, or `key=value`; `/.style` keys have a slash before the dot.
      const std::string key = trim(item.substr(0, eq));
      const bool hasValue = eq != std::string::npos;
      option(st, key, hasValue ? unbrace(item.substr(eq + 1)) : std::string(), hasValue, node, depth);
    }
  }

  /** The arrow tip a name stands for: `>` and `<` are whatever `>=` says when the path is laid out. */
  bool tipOf(const std::string& name, Tip& tip) {
    const std::string n = trim(name);
    if (n == ">" || n == "<") {
      tip = Tip();
      tip.kind = Tip::Kind::to;
      tip.defaultTip = true;
      return true;
    }
    return tipByName(n, tip);
  }

  /** `->`, `<-`, `-stealth`, `{Stealth[length=3mm]}-{Latex}`: the tips at the two ends. */
  bool arrowSpec(Style& st, const std::string& key) {
    // Split at the first `-` outside braces.
    int brace = 0;
    std::size_t cut = std::string::npos;
    for (std::size_t i = 0; i < key.size(); i++) {
      if (key[i] == '{' || key[i] == '[') brace++;
      if (key[i] == '}' || key[i] == ']') brace--;
      if (key[i] == '-' && brace == 0) {
        cut = i;
        break;
      }
    }
    if (cut == std::string::npos) return false;
    const std::string left = trim(key.substr(0, cut)), right = trim(key.substr(cut + 1));
    Tip a, b;
    if (!left.empty() && !tipOf(left, a)) return false;
    if (!right.empty() && !tipOf(right, b)) return false;
    st.tipStart = left.empty() ? Tip() : a;
    st.tipEnd = right.empty() ? Tip() : b;
    return true;
  }

  Dist distOf(const std::string& value) {
    Dist d;
    const auto and_ = value.find(" and ");
    if (and_ != std::string::npos) {
      d.pair = true;
      d.y = dimen(value.substr(0, and_));
      d.x = dimen(value.substr(and_ + 5));
    } else {
      d.y = d.x = dimen(value);
    }
    return d;
  }

  void lineWidth(Style& st, double pt) { st.lineWidth = Dimen{static_cast<float>(pt), UnitType::pt}; }

  void dash(Style& st, std::initializer_list<const char*> parts) {
    std::vector<Dimen> d;
    for (const char* p : parts) {
      if (std::string(p) == "w") {
        d.push_back(st.lineWidth ? *st.lineWidth : Dimen{0.4f, UnitType::pt});
      } else {
        d.push_back(Units::getDimen(p));
      }
    }
    st.dash = d;
    st.dashPhase = Dimen{0.f, UnitType::pt};
  }

  void option(Style& st, const std::string& key, const std::string& value, bool hasValue, bool node, int depth) {
    // `name/.style=`: a style of the picture's own.
    const auto slash = key.find("/.");
    if (slash != std::string::npos) {
      const std::string name = trim(key.substr(0, slash)), what = key.substr(slash + 2);
      if (what == "style" || what == "append style") {
        std::string& to = _styles[name];
        to = what == "style" ? value : (to.empty() ? value : to + "," + value);
        return;
      }
      if (what == "default" || what == "estyle") return;
      warn("tikz: " + key + " is not supported: left out");
      return;
    }
    // A style by name.
    const auto defined = _styles.find(key);
    if (!hasValue && defined != _styles.end()) {
      applyOptions(st, defined->second, node, depth + 1);
      return;
    }
    if (hasValue && defined != _styles.end()) {
      std::string text = defined->second;
      for (std::size_t p = text.find("#1"); p != std::string::npos; p = text.find("#1", p + value.size())) {
        text.replace(p, 2, value);
      }
      applyOptions(st, text, node, depth + 1);
      return;
    }
    color c = 0;
    // The drawing options.
    if (key == "draw") {
      if (!hasValue) {
        st.draw.mode = Paint::Mode::current;
      } else if (value == "none") {
        st.draw.mode = Paint::Mode::none;
      } else if (colour(value, c)) {
        st.draw = {Paint::Mode::color, c};
      } else {
        warn("tikz: draw=" + value + " is not a colour: left out");
      }
    } else if (key == "fill") {
      if (!hasValue) {
        st.fill.mode = Paint::Mode::current;
      } else if (value == "none") {
        st.fill.mode = Paint::Mode::none;
      } else if (colour(value, c)) {
        st.fill = {Paint::Mode::color, c};
      } else {
        warn("tikz: fill=" + value + " is not a colour: left out");
      }
    } else if (key == "filldraw") {
      st.fill.mode = st.draw.mode = Paint::Mode::current;
    } else if (key == "color") {
      if (colour(value, c)) {
        st.current = c;
      } else {
        warn("tikz: color=" + value + " is not a colour: left out");
      }
    } else if (key == "text") {
      if (colour(value, c)) st.textColor = c;
    } else if (key == "line width") {
      const Dimen d = dimen(value);
      if (d.isValid()) st.lineWidth = d.unit == UnitType::cm && trim(value).find("cm") == std::string::npos
                                        ? Dimen{d.val, UnitType::pt} : d;
    } else if (key == "ultra thin") {
      lineWidth(st, 0.1);
    } else if (key == "very thin") {
      lineWidth(st, 0.2);
    } else if (key == "thin") {
      lineWidth(st, 0.4);
    } else if (key == "semithick") {
      lineWidth(st, 0.6);
    } else if (key == "thick") {
      lineWidth(st, 0.8);
    } else if (key == "very thick") {
      lineWidth(st, 1.2);
    } else if (key == "ultra thick") {
      lineWidth(st, 1.6);
    } else if (key == "solid") {
      st.dash = std::vector<Dimen>();
    } else if (key == "dashed") {
      dash(st, {"3pt", "3pt"});
    } else if (key == "dotted") {
      dash(st, {"w", "2pt"});
    } else if (key == "densely dashed") {
      dash(st, {"3pt", "2pt"});
    } else if (key == "loosely dashed") {
      dash(st, {"3pt", "6pt"});
    } else if (key == "densely dotted") {
      dash(st, {"w", "1pt"});
    } else if (key == "loosely dotted") {
      dash(st, {"w", "4pt"});
    } else if (key == "dash pattern") {
      // on 4pt off 1pt on ...
      std::vector<Dimen> d;
      std::string v = value;
      for (std::size_t p = 0; p < v.size();) {
        while (p < v.size() && std::isspace(static_cast<unsigned char>(v[p])) != 0) p++;
        std::string word;
        while (p < v.size() && std::isspace(static_cast<unsigned char>(v[p])) == 0) word += v[p++];
        if (word == "on" || word == "off" || word.empty()) continue;
        d.push_back(dimen(word));
      }
      st.dash = d;
    } else if (key == "dash phase") {
      st.dashPhase = dimen(value);
    } else if (key == "line cap") {
      st.cap = value == "round" ? CAP_ROUND : value == "rect" ? CAP_SQUARE : CAP_BUTT;
    } else if (key == "line join") {
      st.join = value == "round" ? JOIN_ROUND : value == "bevel" ? JOIN_BEVEL : JOIN_MITER;
    } else if (key == "opacity" || key == "draw opacity" || key == "fill opacity" || key == "text opacity") {
      double v = 1;
      if (number(value, v)) {
        if (key != "fill opacity" && key != "text opacity") st.drawOpacity = static_cast<float>(v);
        if (key != "draw opacity" && key != "text opacity") st.fillOpacity = static_cast<float>(v);
        if (key == "text opacity" || key == "opacity") st.textOpacity = static_cast<float>(v);
      }
    } else if (key == "rounded corners") {
      st.rounded = hasValue ? dimen(value) : Dimen{4.f, UnitType::pt};
      if (hasValue && st.rounded->unit == UnitType::cm && trim(value).find("cm") == std::string::npos) {
        st.rounded = Dimen{st.rounded->val, UnitType::pt};
      }
    } else if (key == "sharp corners") {
      st.rounded = Dimen{0.f, UnitType::pt};
    } else if (key == "scale" || key == "xscale" || key == "yscale" || key == "rotate") {
      double v = 1;
      if (number(value, v)) {
        XStep s;
        s.kind = key == "scale" ? XStep::Kind::scale : key == "xscale" ? XStep::Kind::xscale
                 : key == "yscale" ? XStep::Kind::yscale : XStep::Kind::rotate;
        s.v = static_cast<float>(v);
        st.transform.push_back(s);
      }
    } else if (key == "xshift" || key == "yshift") {
      XStep s;
      s.kind = key == "xshift" ? XStep::Kind::xshift : XStep::Kind::yshift;
      s.dx = dimen(value);
      st.transform.push_back(s);
    } else if (key == ">") {
      st.defaultTip = value;
    } else if (key == "node distance") {
      st.nodeDist = distOf(value);
    } else if (key == "every node" || key == "every path" || key == "every picture") {
      // Set through `every node/.style`; a bare one has nothing to say.
    } else if (key == "step" || key == "xstep" || key == "ystep") {
      const Dimen d = dimen(value);
      if (key != "ystep") st.xstep = d;
      if (key != "xstep") st.ystep = d;
      if (key == "step") st.step = d;
    } else if (key == "shorten >" || key == "shorten <") {
      (key == "shorten >" ? st.shortenEnd : st.shortenStart) = dimen(value);
    } else if (key == "bend left" || key == "bend right") {
      double a = 30;
      if (hasValue) number(value, a);
      st.to.bend = static_cast<float>(key == "bend left" ? a : -a);
    } else if (key == "out" || key == "in") {
      double a = 0;
      if (number(value, a)) (key == "out" ? st.to.out : st.to.in) = static_cast<float>(a);
    } else if (key == "looseness") {
      double v = 1;
      if (number(value, v)) st.to.looseness = static_cast<float>(v);
    } else if (key == "min distance") {
      st.to.minDistance = dimen(value);
    } else if (key == "loop above" || key == "loop below" || key == "loop left" || key == "loop right") {
      // pgf's: above is out=105 in=75, and looseness 8, 5mm at least, with an arrow and a 1pt shorter end.
      const bool above = key == "loop above", below = key == "loop below", lefty = key == "loop left";
      st.to.out = static_cast<float>(above ? 105 : below ? 285 : lefty ? 195 : 15);
      st.to.in = static_cast<float>(above ? 75 : below ? 255 : lefty ? 165 : -15);
      st.to.loop = true;
      st.to.looseness = 8.f;
      st.to.minDistance = Dimen{5.f, UnitType::mm};
      Tip arrow;
      arrow.kind = Tip::Kind::to;
      arrow.defaultTip = true;
      st.tipEnd = arrow;
      st.shortenEnd = Dimen{1.f, UnitType::pt};
    } else if ((node || key == "label distance" || key == "pin distance" || key == "pin edge" || key == "every label" || key == "every pin") && applyNodeOption(st, key, value, hasValue, c)) {
      // read
    } else if (key == "level distance" && hasValue) {
      st.levelDistance = dimen(value);
    } else if (key == "sibling distance" && hasValue) {
      st.siblingDistance = dimen(value);
    } else if ((key == "grow" || key == "grow'") && hasValue) {
      growOf(st, value, key == "grow'");
    } else if (key == "missing") {
      st.missing = !hasValue || value != "false";
    } else if (key == "edge from parent path" && hasValue) {
      st.edgePath = value;
    } else if ((key == "parent anchor" || key == "child anchor") && hasValue) {
      (key == "parent anchor" ? st.parentAnchor : st.childAnchor) = trim(value) == "border" ? std::string() : trim(value);
    } else if (key == "on background layer" || key == "on main layer" || key == "on foreground layer") {
      st.layer = key == "on background layer" ? -1 : key == "on main layer" ? 0 : 1;
    } else if (key == "cycle") {
    } else if (!hasValue && !key.empty() && arrowSpec(st, key)) {
      // read
    } else if (!hasValue && colour(key, c) && ColorAtom::hasName(key.substr(0, key.find('!')))) {
      st.current = c;
    } else if (!node && applyNodeOption(st, key, value, hasValue, c)) {
      // A path option that is a node's, `above` on a node on it, say.
    } else {
      warn("tikz: option " + key + (hasValue ? "=" + value : std::string()) + " is not supported: left out");
    }
  }

  /** The options of a node alone; false when `key` is not one. */
  bool applyNodeOption(Style& st, const std::string& key, const std::string& value, bool hasValue, color&) {
    // The shapes that are drawn; any other is said, with its name, where it is asked for.
    static const std::set<std::string> shapes = {"rectangle", "circle", "ellipse", "diamond", "coordinate", "rounded rectangle"};
    static const std::set<std::string> known = {"trapezium", "regular polygon", "cylinder", "star", "semicircle", "kite", "dart",
                                                "isosceles triangle", "circular sector", "cross out", "strike out",
                                                "chamfered rectangle", "rectangle split"};
    if (shapes.count(key) > 0 && !hasValue) {
      st.shape = key;
      return true;
    }
    if (known.count(key) > 0 && !hasValue) {
      warn("tikz: the shape " + key + " is not drawn yet: a rectangle is");
      return true;
    }
    if (key == "shape") {
      if (shapes.count(value) > 0) {
        st.shape = value;
      } else {
        warn("tikz: the shape " + value + " is not drawn yet: a rectangle is");
      }
      return true;
    }
    if (key == "matrix of nodes" || key == "matrix of math nodes") {
      st.matrixKind = key == "matrix of nodes" ? 1 : 2;
      return true;
    }
    if ((key == "row sep" || key == "column sep") && hasValue) {
      // `1cm`, or `{1cm, between origins}`: a length, and how it is measured.
      const bool row = key == "row sep";
      for (const std::string& part : splitTop(value, ',')) {
        const std::string p = trim(part);
        if (p == "between origins" || p == "between borders") {
          (row ? st.rowBetweenOrigins : st.columnBetweenOrigins) = p == "between origins";
        } else if (!p.empty()) {
          (row ? st.rowSep : st.columnSep) = dimen(p);
        }
      }
      return true;
    }
    if (key == "nodes" && hasValue) {
      st.nodesStyle = value;
      return true;
    }
    if (key == "cells" && hasValue) {
      st.cellsStyle = value;
      return true;
    }
    if (key == "nodes in empty cells") {
      st.emptyNodes = !hasValue || value != "false";
      return true;
    }
    if (key == "matrix anchor" && hasValue) {
      st.anchor = trim(value);
      return true;
    }
    if (key == "left delimiter" || key == "right delimiter" || key == "above delimiter" || key == "below delimiter") {
      warn("tikz: " + key + " is not drawn: left out");
      return true;
    }
    if (key == "fit" && hasValue) {
      // `fit=(a)(b)`: the points, one after the other.
      st.fit.clear();
      std::size_t i = 0;
      for (;;) {
        skipSpace(value, i);
        if (i >= value.size() || value[i] != '(') break;
        st.fit.push_back(coord(parenAt(value, i)));
      }
      return true;
    }
    if ((key == "label" || key == "pin") && hasValue) {
      st.labelSpecs.emplace_back(key == "pin", value);
      return true;
    }
    if ((key == "label distance" || key == "pin distance") && hasValue) {
      (key == "label distance" ? st.labelDistance : st.pinDistance) = dimen(value);
      return true;
    }
    if (key == "pin edge" && hasValue) {
      st.pinEdge = value;
      return true;
    }
    if (key == "rounded rectangle arc length" && hasValue) {
      st.arcLength = static_cast<float>(std::atof(value.c_str()));
      return true;
    }
    if ((key == "aspect" || key == "shape aspect") && hasValue) {
      st.aspect = static_cast<float>(std::atof(value.c_str()));
      return true;
    }
    if (key == "inner sep") {
      st.innerX = st.innerY = dimen(value);
      return true;
    }
    if (key == "inner xsep" || key == "inner ysep") {
      (key == "inner xsep" ? st.innerX : st.innerY) = dimen(value);
      return true;
    }
    if (key == "outer sep") {
      st.outerSep = dimen(value);
      return true;
    }
    if (key == "minimum width" || key == "minimum height" || key == "minimum size") {
      const Dimen d = dimen(value);
      if (key != "minimum height") st.minWidth = d;
      if (key != "minimum width") st.minHeight = d;
      return true;
    }
    if (key == "text width") {
      st.textWidth = dimen(value);
      return true;
    }
    if (key == "align") {
      st.align = value == "left" ? 'l' : value == "right" ? 'r' : value == "justify" ? 'j' : 'c';
      return true;
    }
    if (key == "anchor") {
      st.anchor = value;
      return true;
    }
    if (key == "font") {
      st.font = value;
      return true;
    }
    if (key == "sloped") {
      st.sloped = true;
      return true;
    }
    if (key == "auto") {
      st.autoSide = true;
      return true;
    }
    if (key == "swap") {
      st.swap = true;
      return true;
    }
    static const std::map<std::string, double> positions = {
      {"midway", 0.5}, {"near start", 0.25}, {"near end", 0.75}, {"very near start", 0.125}, {"very near end", 0.875},
      {"at start", 0.0}, {"at end", 1.0}, {"pos", 0.5}};
    const auto p = positions.find(key);
    if (p != positions.end()) {
      double v = p->second;
      if (key == "pos") number(value, v);
      st.pos = static_cast<float>(v);
      return true;
    }
    static const std::set<std::string> places = {"above", "below", "left", "right", "above left", "above right", "below left", "below right"};
    if (places.count(key) > 0) {
      st.placement = key;
      // `above=of a`, `right=2cm of a`, `above=1mm`.
      if (hasValue) {
        std::string v = value;
        const auto of = v.rfind("of ");
        if (of != std::string::npos && (of == 0 || std::isspace(static_cast<unsigned char>(v[of - 1])) != 0)) {
          st.placeOf = trim(v.substr(of + 3));
          v = trim(v.substr(0, of));
        }
        if (!v.empty()) st.placeDist = distOf(v);
      }
      return true;
    }
    const auto old = key.rfind(" of");
    if (old != std::string::npos && old + 3 == key.size() && hasValue && places.count(key.substr(0, old)) > 0) {
      st.placement = key.substr(0, old);
      st.placeOf = value;
      st.placeCentres = true;
      return true;
    }
    return false;
  }

  // --- coordinates ------------------------------------------------------------------------------

  /** The text between a coordinate's brackets as a Coord. */
  Coord coord(const std::string& inside) {
    Coord c;
    const std::string s = trim(inside);
    if (!s.empty() && s[0] == '$') {
      c.kind = Coord::Kind::calc;
      c.calc = calcOf(s);
      return c;
    }
    // `a:r`, an angle and a radius; not `cs:`.
    const auto colon = s.find(':');
    if (colon != std::string::npos && s.find(',') == std::string::npos && s.compare(0, 3, "cs:") != 0) {
      double a = 0;
      if (number(s.substr(0, colon), a)) {
        c.kind = Coord::Kind::polar;
        c.angle = static_cast<float>(a);
        const std::string rest = trim(s.substr(colon + 1));
        const auto and_ = rest.find(" and ");
        c.r = dimen(and_ == std::string::npos ? rest : rest.substr(0, and_));
        if (and_ != std::string::npos) c.r2 = dimen(rest.substr(and_ + 5));
        return c;
      }
    }
    const auto parts = splitTop(s, ',');
    if (parts.size() == 2) {
      const Dimen x = dimen(parts[0]), y = dimen(parts[1]);
      if (x.isValid() && y.isValid()) {
        c.kind = Coord::Kind::xy;
        c.x = x;
        c.y = y;
        return c;
      }
    }
    // A node, and perhaps an anchor.
    c.kind = Coord::Kind::node;
    const auto dot = s.find('.');
    c.name = trim(dot == std::string::npos ? s : s.substr(0, dot));
    if (dot != std::string::npos) c.anchor = trim(s.substr(dot + 1));
    return c;
  }

  /** The text of `($ ... $)` as tikzlibrarycalc reads it: terms joined by `+` and `-`, each an optional factor
   *  and `*`, a coordinate, and `!` steps (`!0.3!(b)`, `!1cm!(b)`, `!(p)!(b)`, a target turned by `!30:(b)`). */
  std::shared_ptr<CalcExpr> calcOf(const std::string& source) {
    auto out = std::make_shared<CalcExpr>();
    std::string e = trim(source);
    if (!e.empty() && e.front() == '$') e.erase(0, 1);
    if (!e.empty() && e.back() == '$') e.pop_back();
    const auto bad = [&](const std::string& why) {
      warn("tikz: in a calculation, " + why + ": the rest of it is left out");
    };
    // A point, and the angle in front of it if it has one, from `i`.
    const auto target = [&](std::size_t& i, CalcStep& step) {
      skipSpace(e, i);
      const std::size_t paren = e.find('(', i), colon = e.find(':', i);
      if (colon != std::string::npos && paren != std::string::npos && colon < paren) {
        double a = 0;
        if (!number(e.substr(i, colon - i), a)) return false;
        step.rotated = true;
        step.angle = static_cast<float>(a);
        i = colon + 1;
        skipSpace(e, i);
      }
      if (i >= e.size() || e[i] != '(') return false;
      std::size_t at = i;
      step.to = coord(parenAt(e, at));
      i = at;
      return true;
    };
    std::size_t i = 0;
    for (;;) {
      skipSpace(e, i);
      if (i >= e.size()) break;
      CalcTerm term;
      if (e[i] == '+') {
        i++;
      } else if (e[i] == '-') {
        term.factor = -1;
        i++;
      }
      skipSpace(e, i);
      if (i < e.size() && e[i] != '(') {
        // The factor, up to the `*` that is followed by a point.
        std::size_t star = std::string::npos;
        int depth = 0;
        for (std::size_t k = i; k < e.size() && star == std::string::npos; k++) {
          if (e[k] == '(') depth++;
          if (e[k] == ')') depth--;
          if (e[k] != '*' || depth != 0) continue;
          std::size_t j = k + 1;
          skipSpace(e, j);
          if (j < e.size() && e[j] == '(') star = k;
        }
        double f = 1;
        if (star == std::string::npos || !number(e.substr(i, star - i), f)) {
          bad("a factor must be a number and `*` before a point");
          break;
        }
        term.factor *= f;
        i = star + 1;
        skipSpace(e, i);
      }
      if (i >= e.size() || e[i] != '(') {
        bad("a point is expected");
        break;
      }
      std::size_t at = i;
      term.first = coord(parenAt(e, at));
      i = at;
      bool ok = true;
      for (;;) {
        skipSpace(e, i);
        if (i >= e.size() || e[i] != '!') break;
        i++;
        skipSpace(e, i);
        CalcStep step;
        if (i < e.size() && e[i] == '(') {
          step.kind = CalcStep::Kind::project;
          std::size_t from = i;
          step.via = coord(parenAt(e, from));
          i = from;
          skipSpace(e, i);
          if (i >= e.size() || e[i] != '!') {
            ok = false;
            break;
          }
          i++;
        } else {
          const std::size_t bang = e.find('!', i);
          if (bang == std::string::npos) {
            ok = false;
            break;
          }
          const std::string amount = trim(e.substr(i, bang - i));
          i = bang + 1;
          // A length (`1cm`) walks along the line; a number is how far of it.
          std::size_t k = 0;
          if (k < amount.size() && (amount[k] == '+' || amount[k] == '-')) k++;
          while (k < amount.size() && (std::isdigit(static_cast<unsigned char>(amount[k])) != 0 || amount[k] == '.')) k++;
          if (k > 0 && k < amount.size() && isUnit(trim(amount.substr(k)))) {
            step.kind = CalcStep::Kind::distance;
            step.distance = dimen(amount);
          } else if (!number(amount, step.fraction)) {
            ok = false;
            break;
          }
        }
        if (!target(i, step)) {
          ok = false;
          break;
        }
        term.steps.push_back(std::move(step));
      }
      if (!ok) {
        bad("a `!` is not followed by a number, a length or a point, and a point");
        break;
      }
      out->terms.push_back(std::move(term));
    }
    return out;
  }

  // --- trees ------------------------------------------------------------------------------------

  /** `grow=down`, `grow=right`, `grow=30`, `grow=north east`: the way a tree grows. */
  void growOf(Style& st, const std::string& value, bool swap) {
    static const std::map<std::string, float> names = {
      {"down", -90.f},       {"up", 90.f},         {"left", 180.f},        {"right", 0.f},
      {"south", -90.f},      {"north", 90.f},      {"west", 180.f},        {"east", 0.f},
      {"north east", 45.f},  {"north west", 135.f}, {"south east", -45.f},  {"south west", -135.f}};
    const std::string v = trim(value);
    double angle = 0;
    const auto it = names.find(v);
    if (it != names.end()) {
      angle = it->second;
    } else if (!number(v, angle)) {
      warn("tikz: grow=" + v + " is not a direction: left out");
      return;
    }
    st.growAngle = static_cast<float>(angle);
    st.growSwap = swap;
    st.growLevel = _treeLevel;
  }

  /** The index of `needle` as a word of `text` outside every group, bracket and parenthesis, from `from`. */
  static std::size_t findTop(const std::string& text, const std::string& needle, std::size_t from = 0) {
    int depth = 0;
    for (std::size_t k = from; k < text.size(); k++) {
      const char c = text[k];
      if (c == '{' || c == '[' || c == '(') depth++;
      if (c == '}' || c == ']' || c == ')') depth--;
      if (depth == 0 && text.compare(k, needle.size(), needle) == 0 &&
          (k == 0 || std::isalpha(static_cast<unsigned char>(text[k - 1])) == 0) &&
          (k + needle.size() >= text.size() || std::isalpha(static_cast<unsigned char>(text[k + needle.size()])) == 0)) {
        return k;
      }
    }
    return std::string::npos;
  }

  /** The `child` items that follow a node, with the options between them: the node's children, as TikZ grows them
   *  (`tikz.code.tex`, `\tikz@children`). Each child's body is a node and what follows it -- its own children, its
   *  `edge from parent` -- and each edge becomes a path of its own, drawn after the one the tree is on. */
  void readTree(Node& parent, const Style& scope, int level, const std::string& t, std::size_t& i) {
    struct Source {
      std::string options, body;
    };
    const std::size_t start = i;
    const int was = _treeLevel;
    Style ctx = scope;
    std::vector<Source> kids;
    const auto atWord = [&](std::size_t k, const char* w) {
      const std::size_t n = std::string(w).size();
      return t.compare(k, n, w) == 0 && (k + n >= t.size() || std::isalpha(static_cast<unsigned char>(t[k + n])) == 0);
    };
    for (;;) {
      const std::size_t save = i;
      skipSpace(t, i);
      if (i < t.size() && t[i] == '[') {
        // Options for the rest of the path, if a child comes after them.
        std::size_t at = i;
        const std::string o = groupAt(t, at);
        std::size_t k = at;
        skipSpace(t, k);
        if (k < t.size() && (t[k] == '[' || atWord(k, "child"))) {
          _treeLevel = level - 1;
          applyOptions(ctx, o, false, 0);
          i = at;
          continue;
        }
        i = save;
        break;
      }
      if (i < t.size() && atWord(i, "child")) {
        i += 5;
        skipSpace(t, i);
        std::string options;
        if (i < t.size() && t[i] == '[') {
          std::size_t at = i;
          options = groupAt(t, at);
          i = at;
        }
        skipSpace(t, i);
        if (atWord(i, "foreach")) {
          // child foreach \x in {a,b} {body}: one child for each value.
          i += 7;
          skipSpace(t, i);
          std::vector<std::string> vars;
          while (i < t.size() && t[i] == '\\') {
            i++;
            vars.push_back(word(t, i));
            skipSpace(t, i);
            if (i < t.size() && t[i] == '/') i++;
            skipSpace(t, i);
          }
          if (t.compare(i, 2, "in") == 0 && !vars.empty()) {
            i += 2;
            skipSpace(t, i);
            std::string list, body;
            if (i < t.size() && t[i] == '{') {
              std::size_t at = i;
              list = groupAt(t, at);
              i = at;
            }
            skipSpace(t, i);
            if (i < t.size() && t[i] == '{') {
              std::size_t at = i;
              body = groupAt(t, at);
              i = at;
            }
            for (const std::vector<std::string>& values : foreachValues(list, vars.size())) {
              std::string one = body;
              for (std::size_t v = 0; v < vars.size(); v++) {
                one = replaceControl(one, vars[v], v < values.size() ? values[v] : "");
              }
              kids.push_back({options, one});
            }
          } else {
            warn("tikz: child foreach is not in the form `\\x in {list}': left out");
          }
          continue;
        }
        std::string body;
        if (i < t.size() && t[i] == '{') {
          std::size_t at = i;
          body = groupAt(t, at);
          i = at;
        }
        kids.push_back({options, body});
        continue;
      }
      i = save;
      break;
    }
    if (kids.empty()) {
      i = start;
      _treeLevel = was;
      return;
    }
    // The styles of the level, then each child's own.
    _treeLevel = level;
    applyStyleNamed(ctx, "level " + std::to_string(level));
    if (_styles.count("level") > 0) option(ctx, "level", std::to_string(level), true, false, 0);
    if (parent.name.empty()) parent.name = "tikz@tree" + std::to_string(++_treeNames);
    const int count = static_cast<int>(kids.size());
    for (int j = 0; j < count; j++) {
      Child c;
      c.level = level;
      c.style = ctx;
      applyStyleNamed(c.style, "every child");
      applyOptions(c.style, kids[static_cast<std::size_t>(j)].options, false, 0);
      c.missing = c.style.missing;
      c.style.missing = false;
      if (!c.missing) readChild(c, parent.name, j + 1, kids[static_cast<std::size_t>(j)].body);
      parent.children.push_back(std::move(c));
    }
    _treeLevel = was;
  }

  /** One child's body: `node ... child ... edge from parent ...`; a body with no node is a point. */
  void readChild(Child& c, const std::string& parentName, int index, const std::string& source) {
    // `edge from parent` ends what the node and its children are, and starts the edge's own options and labels.
    std::string body = source, edgeText;
    const std::size_t edgeAt = findTop(source, "edge");
    if (edgeAt != std::string::npos) {
      std::size_t k = edgeAt + 4;
      skipSpace(source, k);
      if (source.compare(k, 4, "from") == 0) {
        k += 4;
        skipSpace(source, k);
        if (source.compare(k, 6, "parent") == 0) {
          body = source.substr(0, edgeAt);
          edgeText = source.substr(k + 6);
          // Children written after the edge's labels belong to the node as well.
          const std::size_t childAt = findTop(edgeText, "child");
          if (childAt != std::string::npos) {
            body += " " + edgeText.substr(childAt);
            edgeText = edgeText.substr(0, childAt);
          }
        }
      }
    }
    std::size_t b = 0;
    skipSpace(body, b);
    const bool isNode = body.compare(b, 4, "node") == 0, isCoordinate = body.compare(b, 10, "coordinate") == 0;
    Node cn;
    if (isNode || isCoordinate) {
      b += isNode ? 4 : 10;
      _nodeExtra = "every child node";
      cn = nodeOf(isCoordinate, body, b, c.style, false, c.style);
      _nodeExtra.clear();
    } else {
      cn.style = c.style;
      cn.style.shape = "coordinate";
    }
    if (cn.name.empty()) cn.name = parentName + "-" + std::to_string(index);
    readTree(cn, c.style, c.level + 1, body, b);
    // The edge: a path from the parent to the child, with labels.
    PathItem e;
    e.style = c.style;
    e.style.draw = Paint();
    e.style.fill = Paint();
    applyStyleNamed(e.style, "edge from parent");
    std::size_t k = 0;
    skipSpace(edgeText, k);
    if (k < edgeText.size() && edgeText[k] == '[') {
      std::size_t at = k;
      applyOptions(e.style, groupAt(edgeText, at), false, 0);
      k = at;
    }
    std::string path = e.style.edgePath.empty() ? "(\\tikzparentnode\\tikzparentanchor) -- (\\tikzchildnode\\tikzchildanchor)"
                                                 : e.style.edgePath;
    path = replaceControl(path, "tikzparentanchor", e.style.parentAnchor.empty() ? "" : "." + e.style.parentAnchor);
    path = replaceControl(path, "tikzchildanchor", e.style.childAnchor.empty() ? "" : "." + e.style.childAnchor);
    path = replaceControl(path, "tikzparentnode", parentName);
    path = replaceControl(path, "tikzchildnode", cn.name);
    pathOps(e, path + " " + edgeText.substr(k), 0);
    _edges.push_back(std::move(e));
    c.node = std::move(cn);
  }

  /** The edges the statement just read made, now that its own path is in the picture. */
  void flushEdges() {
    for (PathItem& e : _edges) _pic.paths.push_back(std::move(e));
    _edges.clear();
  }

  // --- statements -------------------------------------------------------------------------------

  static void skipSpace(const std::string& t, std::size_t& i) {
    while (i < t.size() && std::isspace(static_cast<unsigned char>(t[i])) != 0) i++;
  }

  static std::string word(const std::string& t, std::size_t& i) {
    std::string w;
    while (i < t.size() && std::isalpha(static_cast<unsigned char>(t[i])) != 0) w += t[i++];
    return w;
  }

  /** The index of the `;` that ends the statement at `i`, outside every bracket, or the end. */
  static std::size_t statementEnd(const std::string& t, std::size_t i) {
    int brace = 0, bracket = 0, paren = 0;
    for (; i < t.size(); i++) {
      const char c = t[i];
      if (c == '\\' && i + 1 < t.size()) {
        i++;
        continue;
      }
      if (c == '{') brace++;
      if (c == '}') brace--;
      if (c == '[') bracket++;
      if (c == ']') bracket--;
      if (c == '(') paren++;
      if (c == ')') paren--;
      if (c == ';' && brace <= 0 && bracket <= 0 && paren <= 0) return i;
    }
    return t.size();
  }

  void run(const std::string& text, const Style& scope, int depth) {
    if (depth > 24) {
      warn("tikz: scopes nest too deeply: left out");
      return;
    }
    std::size_t i = 0;
    while (true) {
      skipSpace(text, i);
      if (i >= text.size()) return;
      if (text[i] == ';') {
        i++;
        continue;
      }
      if (text[i] == '{') {
        // A group around statements: a scope without options.
        std::size_t at = i;
        const std::string inner = groupAt(text, at);
        run(inner, scope, depth + 1);
        i = at;
        continue;
      }
      if (text[i] != '\\') {
        warn("tikz: `" + text.substr(i, 12) + "' is not a statement: left out");
        i = statementEnd(text, i) + 1;
        continue;
      }
      i++;
      const std::string cmd = word(text, i);
      if (cmd == "draw" || cmd == "path" || cmd == "fill" || cmd == "filldraw" || cmd == "node" || cmd == "coordinate" || cmd == "matrix" ||
          cmd == "clip" || cmd == "shade" || cmd == "shadedraw" || cmd == "useasboundingbox") {
        const std::size_t end = statementEnd(text, i);
        statement(cmd, text.substr(i, end - i), scope);
        i = end + 1;
      } else if (cmd == "begin") {
        skipSpace(text, i);
        std::size_t at = i;
        const std::string env = trim(groupAt(text, at));
        i = at;
        // The matching \end{env}, nested ones counted.
        int level = 1;
        std::size_t j = i;
        std::size_t close = std::string::npos;
        const std::string beginTag = "\\begin{" + env + "}", endTag = "\\end{" + env + "}";
        while (j < text.size()) {
          const auto b = text.find(beginTag, j), e = text.find(endTag, j);
          if (e == std::string::npos) break;
          if (b != std::string::npos && b < e) {
            level++;
            j = b + beginTag.size();
          } else if (--level == 0) {
            close = e;
            break;
          } else {
            j = e + endTag.size();
          }
        }
        if (close == std::string::npos) {
          warn("tikz: \\begin{" + env + "} is not closed: the rest is left out");
          return;
        }
        std::string inner = text.substr(i, close - i);
        i = close + endTag.size();
        if (env == "scope") {
          Style child = scope;
          const std::string options = listPeelOptional(inner);
          applyOptions(child, options, false, 0);
          run(inner, child, depth + 1);
        } else if (env == "pgfonlayer") {
          // `\begin{pgfonlayer}{background}`: the paths in it are drawn on that layer.
          std::size_t at = 0;
          skipSpace(inner, at);
          const std::string name = trim(groupAt(inner, at));
          Style child = scope;
          if (name == "background") {
            child.layer = -1;
          } else if (name == "main") {
            child.layer = 0;
          } else if (name == "foreground") {
            child.layer = 1;
          } else {
            warn("tikz: the layer " + name + " is not declared here: the main layer is used");
          }
          run(inner.substr(at), child, depth + 1);
        } else {
          warn("tikz: \\begin{" + env + "} is not supported in a picture: left out");
        }
      } else if (cmd == "foreach") {
        i = foreach (text, i, scope, depth);
      } else if (cmd == "tikzset") {
        skipSpace(text, i);
        std::size_t at = i;
        const std::string body = groupAt(text, at);
        i = at;
        Style ignored;
        applyOptions(ignored, body, false, 0);
      } else if (cmd == "usetikzlibrary" || cmd == "pgfmathsetseed" || cmd == "pgfkeys") {
        skipSpace(text, i);
        if (i < text.size() && text[i] == '{') {
          std::size_t at = i;
          groupAt(text, at);
          i = at;
        }
      } else {
        warn("tikz: \\" + cmd + " is not supported in a picture: left out");
        i = statementEnd(text, i) + 1;
      }
    }
  }

  /** \foreach \x in {list} body: the body once for each value, `\x` replaced. */
  std::size_t foreach (const std::string& text, std::size_t i, const Style& scope, int depth) {
    skipSpace(text, i);
    // The variables: \x or \x/\y.
    std::vector<std::string> vars;
    while (i < text.size() && text[i] == '\\') {
      i++;
      vars.push_back(word(text, i));
      skipSpace(text, i);
      if (i < text.size() && text[i] == '/') {
        i++;
        skipSpace(text, i);
      }
    }
    skipSpace(text, i);
    if (text.compare(i, 2, "in") != 0 || vars.empty()) {
      warn("tikz: \\foreach is not in the form `\\x in {list}': left out");
      return statementEnd(text, i) + 1;
    }
    i += 2;
    skipSpace(text, i);
    std::string list;
    if (i < text.size() && text[i] == '{') {
      std::size_t at = i;
      list = groupAt(text, at);
      i = at;
    }
    skipSpace(text, i);
    // The body: a group, or the one statement.
    std::string body;
    if (i < text.size() && text[i] == '{') {
      std::size_t at = i;
      body = groupAt(text, at);
      i = at;
    } else {
      const std::size_t end = statementEnd(text, i);
      body = text.substr(i, end + 1 - i);
      i = end + 1;
    }
    for (const std::vector<std::string>& values : foreachValues(list, vars.size())) {
      std::string one = body;
      for (std::size_t v = 0; v < vars.size(); v++) one = replaceControl(one, vars[v], v < values.size() ? values[v] : "");
      run(one, scope, depth + 1);
    }
    return i;
  }

  /** The values of a \foreach list: `1,2,3`, `0,60,...,300`, `a/1,b/2`. */
  std::vector<std::vector<std::string>> foreachValues(const std::string& list, std::size_t vars) {
    std::vector<std::string> items;
    for (const std::string& it : splitTop(list, ',')) items.push_back(trim(it));
    // An ellipsis: the step is the difference of the two values before it.
    std::vector<std::string> expanded;
    for (std::size_t k = 0; k < items.size(); k++) {
      if (items[k] == "..." && k >= 1 && k + 1 < items.size()) {
        double last = 0, prev = 0, stop = 0;
        const bool two = k >= 2 && number(items[k - 2], prev) && number(items[k - 1], last);
        const bool one = !two && number(items[k - 1], last);
        if ((two || one) && number(items[k + 1], stop)) {
          const double step = two ? last - prev : (stop >= last ? 1.0 : -1.0);
          if (std::fabs(step) > 1e-9) {
            for (double v = last + step; step > 0 ? v < stop - 1e-9 : v > stop + 1e-9; v += step) {
              expanded.push_back(formatNumber(v));
              if (expanded.size() > 4096) break;
            }
          }
        }
        continue;
      }
      if (items[k].empty()) continue;
      expanded.push_back(items[k]);
    }
    std::vector<std::vector<std::string>> out;
    for (const std::string& e : expanded) {
      std::vector<std::string> parts = splitTop(e, '/');
      for (std::string& p : parts) p = trim(p);
      if (vars == 1) parts = {e};
      out.push_back(parts);
    }
    return out;
  }

  static std::string formatNumber(double v) {
    char buf[40];
    std::snprintf(buf, sizeof buf, "%.6g", v);
    return buf;
  }

  /** `text` with the control word `\name` replaced by `value`. */
  static std::string replaceControl(const std::string& text, const std::string& name, const std::string& value) {
    std::string out;
    const std::string tag = "\\" + name;
    for (std::size_t i = 0; i < text.size();) {
      if (text.compare(i, tag.size(), tag) == 0 &&
          (i + tag.size() >= text.size() || std::isalpha(static_cast<unsigned char>(text[i + tag.size()])) == 0)) {
        out += value;
        i += tag.size();
      } else {
        out += text[i++];
      }
    }
    return out;
  }

  // --- a path -----------------------------------------------------------------------------------

  /** `\matrix (name) [options] at (point) { cell & cell \\ ... };` as a matrix of nodes (`matrix of nodes`, `matrix of
   *  math nodes`): each cell is a node named `name-row-column`, the styles of `every cell`, `column n`, `row n`, `row r
   *  column c` and the cell's own `|[options]|` applied in pgf's order. */
  void matrixStatement(const std::string& text, const Style& scope) {
    PathItem item;
    item.style = scope;
    auto grid = std::make_shared<NodeMatrix>();
    Node& mn = grid->node;
    mn.style = scope;
    mn.style.draw = Paint();
    mn.style.fill = Paint();
    mn.style.shape = "rectangle";
    applyStyleNamed(mn.style, "every matrix");
    std::size_t i = 0;
    std::string body;
    for (;;) {
      skipSpace(text, i);
      if (i >= text.size()) break;
      if (text[i] == '[') {
        std::size_t at = i;
        applyOptions(mn.style, groupAt(text, at), true, 0);
        i = at;
      } else if (text[i] == '(') {
        std::size_t at = i;
        mn.name = trim(parenAt(text, at));
        i = at;
      } else if (text.compare(i, 2, "at") == 0 && i + 2 < text.size() && (text[i + 2] == ' ' || text[i + 2] == '(')) {
        i += 2;
        skipSpace(text, i);
        std::size_t at = i;
        if (i < text.size() && text[i] == '(') {
          mn.at = coord(parenAt(text, at));
          mn.hasAt = true;
        }
        i = at;
      } else if (text[i] == '{') {
        std::size_t at = i;
        body = groupAt(text, at);
        i = at;
        break;
      } else {
        break;
      }
    }
    if (mn.style.matrixKind == 0) {
      warn("tikz: only `matrix of nodes` and `matrix of math nodes` are drawn: a matrix of paths is left out");
      return;
    }
    if (mn.name.empty()) mn.name = "tikz@matrix" + std::to_string(++_treeNames);
    // Rows end at `\\`, cells at `&`, outside every group.
    std::vector<std::string> rowTexts;
    std::vector<Dimen> skips{Dimen()};
    {
      std::string cur;
      int depth = 0;
      for (std::size_t k = 0; k < body.size(); k++) {
        const char c = body[k];
        if (c == '{' || c == '[') depth++;
        if (c == '}' || c == ']') depth--;
        if (depth == 0 && c == '\\' && k + 1 < body.size() && body[k + 1] == '\\') {
          k++;
          Dimen skip;
          std::size_t j = k + 1;
          while (j < body.size() && std::isspace(static_cast<unsigned char>(body[j])) != 0) j++;
          if (j < body.size() && body[j] == '[') {
            std::size_t at = j;
            skip = dimen(groupAt(body, at));
            k = at - 1;
          }
          rowTexts.push_back(cur);
          skips.push_back(skip);
          cur.clear();
          continue;
        }
        if (c == '\\' && k + 1 < body.size()) {
          cur += c;
          cur += body[++k];
          continue;
        }
        cur += c;
      }
      if (!trim(cur).empty()) rowTexts.push_back(cur);
    }
    int cols = 0;
    for (std::size_t r = 0; r < rowTexts.size(); r++) {
      const std::vector<std::string> cells = splitTop(rowTexts[r], '&');
      cols = std::max(cols, static_cast<int>(cells.size()));
      for (std::size_t c = 0; c < cells.size(); c++) {
        MatrixCell cell;
        cell.row = static_cast<int>(r) + 1;
        cell.col = static_cast<int>(c) + 1;
        cell.node = matrixCell(mn, cell.row, cell.col, cells[c], scope, cell.empty);
        grid->cells.push_back(std::move(cell));
      }
    }
    grid->rows = static_cast<int>(rowTexts.size());
    grid->cols = cols;
    grid->rowSkip = skips;
    item.grid = std::move(grid);
    _pic.paths.push_back(std::move(item));
  }

  /** One cell: the styles in the order of `matrix/inner style order`, then its own options. */
  Node matrixCell(const Node& matrix, int row, int col, const std::string& source, const Style& scope, bool& empty) {
    Node n;
    n.style = scope;
    n.style.draw = Paint();
    n.style.fill = Paint();
    n.style.tipStart.reset();
    n.style.tipEnd.reset();
    n.style.rounded.reset();
    n.style.shape = "rectangle";
    // The colour and the font of the matrix are the cells'.
    n.style.current = matrix.style.current;
    n.style.textColor = matrix.style.textColor;
    n.style.font = matrix.style.font;
    n.style.matrixKind = 0;
    n.style.nodesStyle.clear();
    applyStyleNamed(n.style, "every node");
    applyOptions(n.style, matrix.style.nodesStyle, true, 0);
    const std::string r = std::to_string(row), c = std::to_string(col);
    // `nodes={...}` in a cell's scope is for the node it makes: applied where it is read.
    const auto step = [&](const std::string& name) {
      applyStyleNamed(n.style, name);
      if (!n.style.nodesStyle.empty()) {
        const std::string inner = n.style.nodesStyle;
        n.style.nodesStyle.clear();
        applyOptions(n.style, inner, true, 0);
      }
    };
    step("every cell");
    applyOptions(n.style, matrix.style.cellsStyle, true, 0);
    step("column " + c);
    step(col % 2 == 1 ? "every odd column" : "every even column");
    step("row " + r);
    step(row % 2 == 1 ? "every odd row" : "every even row");
    step("row " + r + " column " + c);
    std::string text = trim(source);
    if (text.size() >= 2 && text[0] == '|' && text[1] == '[') {
      const std::size_t close = text.find("]|");
      if (close != std::string::npos) {
        applyOptions(n.style, text.substr(2, close - 2), true, 0);
        text = trim(text.substr(close + 2));
      }
    }
    n.style.labelSpecs.clear();
    n.name = matrix.name + "-" + r + "-" + c;
    empty = text.empty() && !matrix.style.emptyNodes;
    if (matrix.style.matrixKind == 2 && !text.empty()) text = "$" + text + "$";
    n.content = text.empty() ? nullptr : contentOf(text, n.style);
    return n;
  }

  void statement(const std::string& cmd, const std::string& text, const Style& scope) {
    if (cmd == "matrix") {
      matrixStatement(text, scope);
      return;
    }
    PathItem path;
    path.style = scope;
    std::size_t i = 0;
    // Nodes and paths take the picture's style for them first.
    const bool nodeCommand = cmd == "node" || cmd == "coordinate";
    if (!nodeCommand) applyStyleNamed(path.style, "every path");
    if (cmd == "draw") {
      path.style.draw.mode = Paint::Mode::current;
    } else if (cmd == "fill") {
      path.style.fill.mode = Paint::Mode::current;
    } else if (cmd == "filldraw") {
      path.style.draw.mode = path.style.fill.mode = Paint::Mode::current;
    } else if (cmd == "clip" || cmd == "useasboundingbox") {
      path.clip = true;
    }
    skipSpace(text, i);
    if (i < text.size() && text[i] == '[') {
      const std::string options = groupAt(text, i);
      applyOptions(path.style, options, false, 0);
    }
    if (nodeCommand) {
      // \node[options] (name) at (coord) {text};
      i = 0;
      Node n = nodeOf(cmd == "coordinate", text, i, path.style, true, scope);
      readTree(n, scope, 1, text, i);
      path.isNode = true;
      path.nodes.push_back(std::move(n));
      _pic.paths.push_back(std::move(path));
      flushEdges();
      return;
    }
    if (cmd == "clip") warn("tikz: \\clip is not supported: the path is drawn as it stands");
    pathOps(path, text, i);
    _pic.paths.push_back(std::move(path));
    flushEdges();
  }

  void applyStyleNamed(Style& st, const std::string& name) {
    const auto it = _styles.find(name);
    if (it != _styles.end()) applyOptions(st, it->second, name == "every node", 0);
  }

  /** `node [options] (name) at (coord) {text}` from `i`, which is on the `[`, the `(` or the `{` after the word. */
  Node nodeOf(bool coordinateOnly, const std::string& t, std::size_t& i, const Style& pathStyle, bool standalone,
              const Style& enclosing) {
    Node n;
    n.style = pathStyle;
    n.style.labelSpecs.clear();  // the node's label= was read once already, as an option of its path
    // A node is not a path: what is drawn, filled or pointed is its own.
    if (!standalone || true) {
      n.style.draw = Paint();
      n.style.fill = Paint();
      n.style.tipStart.reset();
      n.style.tipEnd.reset();
      n.style.rounded.reset();
    }
    applyStyleNamed(n.style, "every node");
    if (!_nodeExtra.empty()) applyStyleNamed(n.style, _nodeExtra);
    std::string optionsText;
    bool optionsRead = false;
    std::string text;
    bool haveText = false;
    for (;;) {
      skipSpace(t, i);
      if (i >= t.size()) break;
      if (t[i] == '[' && !optionsRead) {
        optionsText = groupAt(t, i);
        optionsRead = true;
        // `circle`, `draw` and kin may come later in the options: apply them as they stand.
        applyOptions(n.style, optionsText, true, 0);
        continue;
      }
      if (t[i] == '(') {
        std::size_t at = i;
        const std::string inside = parenAt(t, at);
        n.name = trim(inside);
        i = at;
        continue;
      }
      if (t.compare(i, 2, "at") == 0 && (i + 2 >= t.size() || std::isspace(static_cast<unsigned char>(t[i + 2])) != 0 || t[i + 2] == '(')) {
        i += 2;
        skipSpace(t, i);
        if (i < t.size() && t[i] == '(') {
          std::size_t at = i;
          n.at = coord(parenAt(t, at));
          n.hasAt = true;
          i = at;
        }
        continue;
      }
      if (t[i] == '{') {
        std::size_t at = i;
        text = groupAt(t, at);
        haveText = true;
        i = at;
        break;
      }
      break;
    }
    if (n.style.shape.empty()) n.style.shape = coordinateOnly ? "coordinate" : "rectangle";
    if (coordinateOnly) n.style.shape = "coordinate";
    n.pos = n.style.pos.value_or(0.5f);
    n.posGiven = n.style.pos.has_value();
    n.content = haveText && !coordinateOnly ? contentOf(text, n.style) : nullptr;
    // TikZ reads `label distance`, `pin distance` and `pin edge` where the label is set up, after the node's own
    // options are closed: the scope round the node decides, whatever the node says (checked with pdfTeX).
    n.style.labelDistance = enclosing.labelDistance;
    n.style.pinDistance = enclosing.pinDistance;
    n.style.pinEdge = enclosing.pinEdge;
    for (const auto& spec : n.style.labelSpecs) n.labels.push_back(labelOf(spec.first, spec.second, n.style));
    n.style.labelSpecs.clear();
    return n;
  }

  /** `label=[options]angle:text` and `pin=...`: the angle is a direction (`above`, `30`) and defaults to
   *  `above`; the options and the text are those of a node of its own, drawn beside the one it belongs to. */
  Label labelOf(bool pin, const std::string& spec, const Style& owner) {
    Label l;
    l.pin = pin;
    std::string rest = trim(spec), options;
    if (!rest.empty() && rest[0] == '[') {
      std::size_t at = 0;
      options = groupAt(rest, at);
      rest = trim(rest.substr(at));
    }
    std::size_t colon = std::string::npos;
    int depth = 0;
    for (std::size_t k = 0; k < rest.size() && colon == std::string::npos; k++) {
      if (rest[k] == '{' || rest[k] == '[') depth++;
      if (rest[k] == '}' || rest[k] == ']') depth--;
      if (depth == 0 && rest[k] == ':') colon = k;
    }
    std::string text = rest;
    if (colon != std::string::npos) {
      l.angle = trim(rest.substr(0, colon));
      text = rest.substr(colon + 1);
    }
    if (l.angle.empty()) l.angle = "above";
    Style s;
    s.current = owner.current;
    s.textColor = owner.textColor;
    s.font = owner.font;
    s.shape = "rectangle";
    applyStyleNamed(s, pin ? "every pin" : "every label");
    applyOptions(s, options, true, 0);
    s.labelSpecs.clear();
    l.style = s;
    l.content = contentOf(trim(text), s);
    if (pin) {
      // `every pin edge` is `help lines`: gray, 0.2pt.
      Style e;
      color gray = 0;
      if (colour("gray", gray)) e.current = gray;
      e.draw.mode = Paint::Mode::current;
      e.lineWidth = Dimen{0.2f, UnitType::pt};
      applyStyleNamed(e, "every pin edge");
      // The pin's own `pin edge=` wins over the scope's.
      applyOptions(e, s.pinEdge.empty() ? owner.pinEdge : s.pinEdge, false, 0);
      l.edge = e;
    }
    return l;
  }

  /** The text of a node as an atom: set to its text width and alignment, in its font. */
  sptr<Atom> contentOf(const std::string& text, const Style& st) {
    std::string tex = text;
    if (st.textWidth) {
      const char a = st.align.value_or('l');
      const std::string decl = a == 'c' ? "\\centering " : a == 'r' ? "\\raggedleft " : "";
      tex = "\\begin{minipage}[t]{" + dimenText(*st.textWidth) + "}" + decl + text + "\\end{minipage}";
    } else if (st.align && *st.align != 'l') {
      // Aligned text with no width is lines set against each other.
      tex = text;
    }
    if (!st.font.empty()) tex = "{" + st.font + " " + tex + "}";
    if (tex.empty()) return nullptr;
    return _args.formulaChecked(tex, false);
  }

  static std::string dimenText(const Dimen& d) {
    static const char* names[] = {"em", "ex", "px", "bp", "pc", "mu", "cm", "mm", "in", "sp", "pt", "dd", "cc", "tt", "tw"};
    char buf[48];
    std::snprintf(buf, sizeof buf, "%g", static_cast<double>(d.val));
    const int u = static_cast<int>(d.unit);
    return std::string(buf) + (u >= 0 && u < 15 ? names[u] : "pt");
  }

  /** The text of the `(`...`)` at `i`, and `i` past it. */
  static std::string parenAt(const std::string& t, std::size_t& i) {
    int depth = 0;
    for (std::size_t j = i; j < t.size(); j++) {
      if (t[j] == '(') depth++;
      if (t[j] == ')' && --depth == 0) {
        const std::string inner = t.substr(i + 1, j - i - 1);
        i = j + 1;
        return inner;
      }
    }
    const std::string rest = t.substr(i + 1);
    i = t.size();
    return rest;
  }

  void pathOps(PathItem& path, const std::string& t, std::size_t i) {
    enum class Pending { none, line, hline, vline, rect, to, edge, grid } pending = Pending::none;
    bool nodeBetween = false;                  // a node was written after the connector, before its coordinate
    std::vector<int> betweenNodes;
    PathOp lastOp;
    std::string toOptions;
    const auto push = [&](PathOp op) {
      const int index = static_cast<int>(path.ops.size());
      path.ops.push_back(std::move(op));
      for (const int n : betweenNodes) path.nodes[static_cast<std::size_t>(n)].alongOp = index;
      betweenNodes.clear();
      nodeBetween = false;
      pending = Pending::none;
      toOptions.clear();
      return index;
    };
    for (;;) {
      skipSpace(t, i);
      if (i >= t.size()) break;
      const char c = t[i];
      int relative = 0;
      if (c == '+') {
        relative = 1;
        i++;
        if (i < t.size() && t[i] == '+') {
          relative = 2;
          i++;
        }
        skipSpace(t, i);
        if (i >= t.size() || t[i] != '(') {
          warn("tikz: `+' is not followed by a coordinate: left out");
          continue;
        }
      }
      if (t[i] == '(') {
        std::size_t at = i;
        Coord co = coord(parenAt(t, at));
        co.relative = relative;
        i = at;
        PathOp op;
        op.p = co;
        if (pending == Pending::to || pending == Pending::edge) {
          // A `to` with options of its own, and an edge, which always has: its path style is the path's, drawn.
          if (pending == Pending::edge || !toOptions.empty()) {
            Style s = path.style;
            if (pending == Pending::edge && s.draw.mode != Paint::Mode::color) s.draw.mode = Paint::Mode::current;
            applyOptions(s, toOptions, false, 0);
            op.own = s;
          }
        }
        switch (pending) {
          case Pending::none: op.kind = PathOp::Kind::move; break;
          case Pending::line: op.kind = PathOp::Kind::line; break;
          case Pending::hline: op.kind = PathOp::Kind::hline; break;
          case Pending::vline: op.kind = PathOp::Kind::vline; break;
          case Pending::rect: op.kind = PathOp::Kind::rect; break;
          case Pending::to: op.kind = PathOp::Kind::to; break;
          case Pending::edge: op.kind = PathOp::Kind::to; op.edge = true; break;
          case Pending::grid: op.kind = PathOp::Kind::grid; break;
        }
        push(std::move(op));
        continue;
      }
      if (t.compare(i, 2, "--") == 0) {
        pending = Pending::line;
        i += 2;
        continue;
      }
      if (t.compare(i, 2, "-|") == 0) {
        pending = Pending::hline;
        i += 2;
        continue;
      }
      if (t.compare(i, 2, "|-") == 0) {
        pending = Pending::vline;
        i += 2;
        continue;
      }
      if (t.compare(i, 2, "..") == 0) {
        // `.. controls (c1) and (c2) .. (p)`.
        i += 2;
        skipSpace(t, i);
        const std::string w = word(t, i);
        if (w != "controls") {
          warn("tikz: `.. " + w + "' is not supported: left out");
          continue;
        }
        PathOp op;
        op.kind = PathOp::Kind::curve;
        skipSpace(t, i);
        if (i < t.size() && t[i] == '(') {
          std::size_t at = i;
          op.c1 = coord(parenAt(t, at));
          i = at;
        }
        skipSpace(t, i);
        if (t.compare(i, 3, "and") == 0) {
          i += 3;
          skipSpace(t, i);
          if (i < t.size() && t[i] == '(') {
            std::size_t at = i;
            op.c2 = coord(parenAt(t, at));
            i = at;
          }
        } else {
          op.c2 = op.c1;
        }
        skipSpace(t, i);
        if (t.compare(i, 2, "..") == 0) i += 2;
        skipSpace(t, i);
        if (i < t.size() && t[i] == '+') {
          op.p.relative = 1;
          i++;
          if (i < t.size() && t[i] == '+') {
            op.p.relative = 2;
            i++;
          }
        }
        if (i < t.size() && t[i] == '(') {
          std::size_t at = i;
          const int rel = op.p.relative;
          op.p = coord(parenAt(t, at));
          op.p.relative = rel;
          i = at;
        }
        push(std::move(op));
        continue;
      }
      if (c == '[') {
        // Options of the operation that is pending or has just been read.
        const std::string options = groupAt(t, i);
        if (pending == Pending::to || pending == Pending::edge) {
          toOptions = options;
        } else if (pending == Pending::grid) {
          applyOptions(path.style, options, false, 0);
        } else {
          warn("tikz: options in the middle of a path are not supported: left out");
        }
        continue;
      }
      if (c == '{') {
        // A group inside a path: `{[rounded corners] ...}` is not read.
        warn("tikz: a group inside a path is not supported: left out");
        groupAt(t, i);
        continue;
      }
      if (std::isalpha(static_cast<unsigned char>(c)) != 0) {
        const std::size_t before = i;
        const std::string w = word(t, i);
        if (w == "cycle") {
          PathOp op;
          op.kind = PathOp::Kind::close;
          push(std::move(op));
        } else if (w == "rectangle") {
          pending = Pending::rect;
        } else if (w == "to") {
          pending = Pending::to;
        } else if (w == "edge") {
          pending = Pending::edge;
        } else if (w == "grid") {
          pending = Pending::grid;
        } else if (w == "circle" || w == "ellipse") {
          PathOp op;
          op.kind = w == "circle" ? PathOp::Kind::circle : PathOp::Kind::ellipse;
          readRadii(t, i, op);
          push(std::move(op));
        } else if (w == "arc") {
          PathOp op;
          op.kind = PathOp::Kind::arc;
          readArc(t, i, op);
          push(std::move(op));
        } else if (w == "node" || w == "coordinate") {
          Node n = nodeOf(w == "coordinate", t, i, path.style, false, path.style);
          readTree(n, path.style, 1, t, i);
          const bool before = pending != Pending::none;
          n.between = before;
          path.nodes.push_back(std::move(n));
          PathOp op;
          op.kind = PathOp::Kind::node;
          op.node = static_cast<int>(path.nodes.size()) - 1;
          if (before) {
            // Written between an operation and its coordinate: it sits on the segment that coordinate ends.
            betweenNodes.push_back(op.node);
            nodeBetween = true;
            const auto keep = pending;
            const int idx = static_cast<int>(path.ops.size());
            path.ops.push_back(std::move(op));
            (void)idx;
            pending = keep;
          } else {
            path.ops.push_back(std::move(op));
          }
        } else if (w == "at") {
          // `\path at (x,y) node ...`: the node takes the position; nothing to do here.
        } else if (w == "plot") {
          warn("tikz: plot is not supported: left out");
          i = statementEnd(t, i);
        } else if (w == "and") {
        } else {
          i = before + 1;
          warn("tikz: `" + w + "' in a path is not supported: left out");
          i = before + w.size();
        }
        continue;
      }
      warn(std::string("tikz: `") + c + "' in a path is not read");
      i++;
    }
    (void)nodeBetween;
    (void)lastOp;
  }

  /** `circle (1)`, `circle (1 and 2)`, `circle[radius=1]`, `ellipse[x radius=2, y radius=1]`. */
  void readRadii(const std::string& t, std::size_t& i, PathOp& op) {
    skipSpace(t, i);
    if (i < t.size() && t[i] == '(') {
      const std::string inside = parenAt(t, i);
      const auto and_ = inside.find(" and ");
      op.r = dimen(and_ == std::string::npos ? inside : inside.substr(0, and_));
      if (and_ != std::string::npos) {
        op.r2 = dimen(inside.substr(and_ + 5));
        op.kind = PathOp::Kind::ellipse;
      }
      return;
    }
    if (i < t.size() && t[i] == '[') {
      const std::string options = groupAt(t, i);
      for (const std::string& raw : splitTop(options, ',')) {
        const auto eq = raw.find('=');
        if (eq == std::string::npos) continue;
        const std::string k = trim(raw.substr(0, eq)), v = unbrace(raw.substr(eq + 1));
        if (k == "radius" || k == "x radius") op.r = dimen(v);
        if (k == "y radius") {
          op.r2 = dimen(v);
          op.kind = PathOp::Kind::ellipse;
        }
      }
      if (op.kind == PathOp::Kind::ellipse && !op.r2.isValid()) op.r2 = op.r;
    }
  }

  void readArc(const std::string& t, std::size_t& i, PathOp& op) {
    skipSpace(t, i);
    if (i < t.size() && t[i] == '(') {
      const std::string inside = parenAt(t, i);
      const auto parts = splitTop(inside, ':');
      double a0 = 0, a1 = 0;
      if (parts.size() >= 3 && number(parts[0], a0) && number(parts[1], a1)) {
        op.a0 = static_cast<float>(a0);
        op.a1 = static_cast<float>(a1);
        const std::string rest = trim(parts[2]);
        const auto and_ = rest.find(" and ");
        op.r = dimen(and_ == std::string::npos ? rest : rest.substr(0, and_));
        if (and_ != std::string::npos) op.r2 = dimen(rest.substr(and_ + 5));
      }
      return;
    }
    if (i < t.size() && t[i] == '[') {
      const std::string options = groupAt(t, i);
      double delta = 0;
      bool haveDelta = false;
      for (const std::string& raw : splitTop(options, ',')) {
        const auto eq = raw.find('=');
        if (eq == std::string::npos) continue;
        const std::string k = trim(raw.substr(0, eq)), v = unbrace(raw.substr(eq + 1));
        double x = 0;
        if (k == "start angle" && number(v, x)) op.a0 = static_cast<float>(x);
        if (k == "end angle" && number(v, x)) op.a1 = static_cast<float>(x);
        if (k == "delta angle" && number(v, x)) {
          delta = x;
          haveDelta = true;
        }
        if (k == "radius" || k == "x radius") op.r = dimen(v);
        if (k == "y radius") op.r2 = dimen(v);
      }
      if (haveDelta) op.a1 = op.a0 + static_cast<float>(delta);
    }
  }
};

}  // namespace tikz

inline cmdmacro(tikzpictureATATenv) {
  auto atom = sptrOf<TikzAtom>();
  tikz::Reader reader(args, *atom);
  reader.picture(args.text(1));
  return atom;
}

}  // namespace microtex

#endif  // MICROTEX_MACRO_TIKZ_H
