#pragma once

// TikzAtom: a tikzpicture -- paths, shapes and the nodes that hold text -- laid out as TikZ does and
// drawn with the recorder's stroked and filled paths. macro/macro_tikz.h reads the picture's source
// into this model; nothing here reads LaTeX. The model keeps lengths as the source gave them (a bare
// number is centimetres, as in TikZ) and names for nodes and anchors, because a coordinate such as
// `(a.north)` or `below=of a` is known only once the node it names has been given its size, which
// takes the font: createBox() resolves everything in the order the picture is written.

#include <map>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "atom/atom.h"
#include "box/box.h"
#include "env/units.h"
#include "graphic/graphic_basic.h"

namespace microtex {
namespace tikz {

/** A point of a picture, as written: `(1,2)`, `(30:2cm)`, `(a)`, `(a.north)`, `+(1,0)`. */
struct Coord {
  enum class Kind { xy, polar, node, calc } kind = Kind::xy;
  Dimen x, y;                    // xy
  float angle = 0.f;             // polar: degrees
  Dimen r, r2;                   // polar: the radius, and the second one of an ellipse
  std::string name, anchor;      // node: its name, and the anchor after the dot
  std::string expr;              // calc: the text between the dollars
  int relative = 0;              // 0 absolute, 1 `+` (the base stays), 2 `++` (the base moves)
};

/** One of `draw`, `fill`: not said, off, in the current colour, or in a colour of its own. */
struct Paint {
  enum class Mode : char { unset, none, current, color } mode = Mode::unset;
  color c = 0;
};

/** A step of a coordinate transformation; each applies to the coordinates first, after those before it. */
struct XStep {
  enum class Kind { shift, scale, xscale, yscale, rotate, xshift, yshift } kind = Kind::shift;
  float v = 0.f;                 // scale factors, degrees
  Dimen dx, dy;                  // shifts
};

/** An arrow tip, `->` and kin, at one end of a path. `to` is TikZ's default (Computer Modern's arrow),
 *  `latex` and `stealth` the old tips, `newLatex` and `newStealth` arrows.meta's. */
struct Tip {
  enum class Kind { none, to, stealth, latex, newStealth, newLatex, triangleMeta, triangle45, bar, circle } kind = Kind::none;
  bool open = false, round = false;
  bool defaultTip = false;                       // `>` and `<`: whichever `>=` says, when the path is laid out
  std::optional<Dimen> length, width, inset, lineWidth;
  float scale = 1.f;
};

/** `2cm`, or `1cm and 2cm`: the first is vertical, as in `node distance`. */
struct Dist {
  Dimen y, x;
  bool pair = false;
};

/** How a `to` or an `edge` runs from one point to the other: straight, or a curve. */
struct ToSpec {
  std::optional<float> bend;                     // degrees, positive to the left
  std::optional<float> out, in;                  // absolute angles at the two ends
  float looseness = 1.f;
  bool loop = false;
  std::optional<Dimen> minDistance;
  bool curved() const { return bend || out || in || loop; }
};

/** The tip a name stands for (`to`, `stealth`, `Latex[length=3mm]` ...); false when there is none. */
bool tipByName(std::string name, Tip& tip);

struct Style {
  Paint draw, fill;
  std::optional<color> current;                  // `color=`, or a colour's name alone
  std::optional<Dimen> lineWidth;
  std::optional<std::vector<Dimen>> dash;        // on, off, on, off ...
  std::optional<Dimen> dashPhase;
  std::optional<int> cap, join;                  // microtex::Cap, Join
  std::optional<float> drawOpacity, fillOpacity;
  std::optional<Dimen> rounded;                  // rounded corners; zero is none
  std::vector<XStep> transform;
  std::optional<Tip> tipStart, tipEnd;
  std::optional<Dimen> shortenStart, shortenEnd;
  std::string defaultTip;                        // `>=stealth`
  std::optional<Dimen> step, xstep, ystep;       // grid
  ToSpec to;
  // Nodes.
  std::string shape;                             // rectangle, circle, ... ("" is the default)
  std::optional<float> aspect;                   // `aspect=`, `shape aspect=`: a diamond's width over its height
  std::optional<float> arcLength;                // `rounded rectangle arc length=`, degrees
  std::optional<Dimen> innerX, innerY, outerSep;
  std::optional<Dimen> minWidth, minHeight;
  std::optional<Dimen> textWidth;
  std::optional<char> align;                     // l, c, r (and j)
  std::optional<color> textColor;
  std::optional<float> textOpacity;
  std::string anchor;                            // anchor=: which point of the node is put at the position
  std::string placement;                         // above, below left, ...
  std::optional<Dist> placeDist;                 // `right=2cm of a`
  std::string placeOf;                           // the node `of` which it is put
  bool placeCentres = false;                     // `right of=a`: centre to centre
  std::optional<Dist> nodeDist;                  // `node distance=1cm and 2cm`
  std::string font;                              // `font=\small`
  std::optional<float> pos;                      // `pos=`, `midway` ...: along the segment before a node
  bool sloped = false, autoSide = false, swap = false;
  std::optional<float> rotate;                   // a node's own rotation
};

/** A step of a path. */
struct PathOp {
  enum class Kind { move, line, hline, vline, curve, rect, circle, ellipse, arc, close, node, to, grid } kind = Kind::move;
  Coord p, c1, c2;
  Dimen r, r2;
  float a0 = 0.f, a1 = 0.f;
  int node = -1;                                 // index into PathItem::nodes
  bool edge = false;                             // `edge` rather than `to`: a path of its own, the current point stays
  std::optional<Style> own;                      // a `to` or `edge` with options of its own
};

struct Node {
  std::string name;
  Style style;
  sptr<Atom> content;
  Coord at;
  bool hasAt = false;
  float pos = 0.5f;
  bool posGiven = false;
  bool between = false;                          // written between an operation and its coordinate
  int alongOp = -1;                              // then: the operation it sits on
};

struct PathItem {
  Style style;
  std::vector<PathOp> ops;
  std::vector<Node> nodes;
  bool isNode = false;                           // a \node or \coordinate: one node, no path
  bool clip = false;
};

}  // namespace tikz

class TikzAtom : public Atom {
public:
  std::vector<tikz::PathItem> paths;

  TikzAtom();

  sptr<Box> createBox(Env& env) override;
};

}  // namespace microtex
