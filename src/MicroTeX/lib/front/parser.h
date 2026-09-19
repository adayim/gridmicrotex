#ifndef GRIDMICROTEX_FRONT_PARSER_H
#define GRIDMICROTEX_FRONT_PARSER_H

#include <cstddef>
#include <functional>
#include <string>
#include <vector>

#include "front/ast.h"
#include "front/diagnostics.h"
#include "front/expander.h"
#include "front/spec.h"

namespace microtex::front {

struct ParserOptions {
  Mode startMode = Mode::math;
  /** Names the engine knows that take no arguments and have no spec:
   *  symbols (\alpha) and predefined formulas (\sin). Anything else not in
   *  the spec is reported as unknown. */
  std::function<bool(const std::string&)> isKnownName;
};

/**
 * Builds the syntax tree of an input from the expander's tokens.
 *
 * Recursive descent, driven by the spec table (front/spec.h), tracking math
 * and text mode. It never stops at a mistake: each one is recorded in the
 * diagnostics with its position, and the tree gets what TeX's own recovery
 * would give -- a missing `}` is inserted, `a^b^c` reads as `{a^b}^c`, an
 * unknown command is kept (and drawn red).
 *
 * The structure follows the old parser where the two differ in shape but
 * not in meaning, so that each command can still be built by its old
 * handler: an infix command's denominator is the rest of the list; a
 * declaration like \bf runs to the end of the list or the next `&` / `\\`;
 * a rule (\hline) ends its row.
 */
class Parser {
public:
  Parser(Expander& input, Ast& ast, Diagnostics& diagnostics, ParserOptions options);

  /** Parse the whole input. Returns (and sets) the root list. */
  NodeId parse();

private:
  /** Where a list ends, besides the end of the input. */
  struct Stop {
    /** At `}`, which is left for the caller. */
    bool group = false;
    /** At `&`, `\\`, `\cr` and `\end`: a cell of an alignment. */
    bool cell = false;
    /** This list is the cell itself, not something nested in it. */
    bool cellTop = false;
    /** At `\\` and `\cr` even outside an alignment, and at `&`: the reach
     *  of a declaration such as \bf. */
    bool overArg = false;
    /** At `\middle` and `\right`. */
    bool right = false;
    /** At `$` (inline) or `$$` (display) closing math. */
    bool dollar = false;
    bool displayDollar = false;
    /** At this control symbol: ")" for \), "]" for \]. */
    std::string closeSymbol;
    /** Reading a one-token argument: a command is only itself (no body
     *  for a declaration, no denominator for \over). */
    bool argument = false;
  };

  /** A token put back. `replay` marks one read a second time (an optional
   *  argument's content), which is not recorded again. */
  struct Pending {
    ExpandedToken tok;
    bool replay;
  };

  struct Logged {
    std::size_t index;
    ExpandedToken tok;
  };

  Expander& _in;
  Ast& _ast;
  Diagnostics& _diags;
  ParserOptions _opts;

  /** Tokens put back, the next one last. */
  std::vector<Pending> _ahead;
  bool _lastReplay = false;
  /** Tokens consumed while an argument's source text is being recorded,
   *  each with its position in the whole stream of consumed tokens. */
  std::vector<Logged> _log;
  std::size_t _consumed = 0;
  int _recording = 0;
  /** A space TeX dropped after a control word is kept, marked, so text
   *  can be read either way (see the space node). */
  bool _afterControlWord = false;
  int _depth = 0;
  /** Inside an argument read as text (\text{}, \mbox{}): TeX's restricted
   *  horizontal mode, where `$$` is an empty formula, not display math. */
  int _restricted = 0;
  /** Set when an infix command took the rest of the list. */
  bool _listDone = false;
  /** Set when a rule or \intertext ended the row of the cell being read. */
  bool _rowEnded = false;

  ExpandedToken next();
  void unread(ExpandedToken t);
  const ExpandedToken& peek();
  ExpandedToken nextNonSpace();

  std::size_t startRecording();
  /** The source of the tokens consumed since `mark`, less `dropLast`
   *  tokens at the end (a closing brace) whose lead is kept. */
  std::string stopRecording(std::size_t mark, std::size_t dropLast = 0, bool firstLead = true);

  bool atStop(const ExpandedToken& t, const Stop& stop) const;

  NodeId parseList(Mode mode, const Stop& stop, const SourceSpan& at);
  /** One item, appended to `items`. False at a stop token. */
  bool parseItem(Mode mode, const Stop& stop, std::vector<NodeId>& items);
  NodeId parseCommand(ExpandedToken t, Mode mode, const Stop& stop,
                      std::vector<NodeId>& items, bool& consumedRest);
  NodeId parseArgument(const ArgSpec& spec, Mode mode, const std::string& who);
  NodeId parseRawArgument(const ArgSpec& spec, const std::string& who);
  NodeId parseTokens(std::vector<ExpandedToken> tokens, Mode mode, const SourceSpan& at);
  NodeId parseGroupAfterOpen(const ExpandedToken& open, Mode mode);
  NodeId parseScripts(NodeId base, Mode mode, const SourceSpan& at);
  NodeId parseScriptArgument(Mode mode, const SourceSpan& at);
  NodeId parseLeftRight(const ExpandedToken& left, Mode mode);
  NodeId parseDelimiter(const std::string& who);
  NodeId parseEnvironment(const ExpandedToken& begin, Mode mode);
  /** `inGroup`: the math sits in a group, whose `}` ends it too. */
  NodeId parseMath(const ExpandedToken& open, bool display, const std::string& closeSymbol,
                   bool inGroup);
  NodeId parseBare(Bare bare, const std::string& who);
  std::vector<ExpandedToken> collectBracketed(const std::string& who);
  std::string readGroupName();

  NodeId emptyList(const SourceSpan& at, Mode mode);
  NodeId character(const ExpandedToken& t, Mode mode);
  NodeId absentArgument(const SourceSpan& at, Mode mode);
};

}  // namespace microtex::front

#endif
