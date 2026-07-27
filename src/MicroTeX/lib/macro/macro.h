#ifndef MACRO_H_INCLUDED
#define MACRO_H_INCLUDED

#include <map>
#include <set>
#include <string>

#include "atom/atom.h"
#include "utils/utils.h"

namespace microtex {

class Parser;

class Macro {
public:
  virtual void execute(Parser& tp, std::vector<std::string>& args) = 0;

  virtual ~Macro() = default;
};

class NewCommandMacro : public Macro {
protected:
  static std::map<std::string, std::string> _codes;
  static std::map<std::string, std::string> _replacements;
  static Macro* _instance;
  // Snapshot of the macro names known at the end of _init_(). Anything
  // not in this set is considered user-defined and is dropped by
  // clearUserMacros(). Populated once by snapshotBuiltins().
  static std::set<std::string> _builtin_names;

  static void checkNew(const std::string& name);

  static void checkRenew(const std::string& name);

public:
  /**
   * If notify a fatal error when defining a new command but it has been
   * defined already or redefine a command but it has not been defined,
   * default is true.
   */
  static bool _errIfConflict;

  void execute(Parser& tp, std::vector<std::string>& args) override;

  static void addNewCommand(const std::string& name, const std::string& code, int argc);

  static void
  addNewCommand(const std::string& name, const std::string& code, int argc, const std::string& def);

  static void addRenewCommand(const std::string& name, const std::string& code, int argc);

  static void addRenewCommand(
    const std::string& name,
    const std::string& code,
    int argc,
    const std::string& def
  );

  /**
   * Define or silently overwrite a macro with plain-TeX \def semantics.
   * Supports the sequential parameter form \def\foo#1#2{body} (argc 0..9).
   * No conflict check is performed.
   */
  static void addDefCommand(const std::string& name, const std::string& code, int argc);

  static bool isMacro(const std::string& name);

  /**
   * Drop every user-defined macro currently registered (\newcommand,
   * \renewcommand, \def). Built-in macros and environments registered
   * by _init_() are preserved via the snapshot taken by
   * snapshotBuiltins().
   *
   * Call this between independent parses so that the static state
   * doesn't leak macros across calls and so that the typeface/path
   * double-parse for a single grob doesn't hit "already exists"
   * errors on the second pass.
   */
  static void clearUserMacros();

  /**
   * Record the set of macro names currently in `_codes`. Anything
   * present at the time of the call is considered "built-in" and will
   * not be removed by clearUserMacros(). Idempotent — only the first
   * call has an effect, so multiple init paths can call it safely.
   */
  static void snapshotBuiltins();

  static void _init_();

  static void _free_();

  ~NewCommandMacro() override = default;
};

class NewEnvironmentMacro : public NewCommandMacro {
public:
  static void addNewEnvironment(
    const std::string& name,
    const std::string& begDef,
    const std::string& endDef,
    int argc
  );

  static void addRenewEnvironment(
    const std::string& name,
    const std::string& begDef,
    const std::string& endDef,
    int argc
  );
};

class MacroInfo {
private:
  static std::map<std::string, MacroInfo*> _commands;

public:
  /** Add a macro, replace it if the macro is exists. */
  static void add(const std::string& name, MacroInfo* mac);

  /** Get the macro info from given name, return nullptr if not found. */
  static MacroInfo* get(const std::string& name);

  /** Remove and delete the macro info entry for the given name. No-op
   *  if the name is not registered. */
  static void remove(const std::string& name);

  // Number of arguments
  const int argc;
  // Options' position, can be  0, 1 and 2
  // 0 represents this macro has no options
  // 1 represents the options appear after the command name, e.g.:
  //      \sqrt[3]{2}
  // 2 represents the options appear after the first argument, e.g.:
  //      \scalebox{0.5}[2]{\LaTeX}
  const int opt;

  no_copy_assign(MacroInfo);

  MacroInfo() : argc(0), opt(0) {}

  MacroInfo(int argc, int opt) : argc(argc), opt(opt) {}

  explicit MacroInfo(int argc) : argc(argc), opt(0) {}

  virtual sptr<Atom> invoke(Parser& tp, std::vector<std::string>& args) { return nullptr; }

  virtual ~MacroInfo() = default;

  static void _free_();
};

class InflationMacroInfo : public MacroInfo {
private:
  // The actual macro to execute
  Macro* const _macro;

public:
  no_copy_assign(InflationMacroInfo);

  InflationMacroInfo(Macro* macro, int argc) : MacroInfo(argc), _macro(macro) {}

  InflationMacroInfo(Macro* macro, int argc, int posOpts)
      : MacroInfo(argc, posOpts), _macro(macro) {}

  sptr<Atom> invoke(Parser& tp, std::vector<std::string>& args) override {
    _macro->execute(tp, args);
    return nullptr;
  }
};

typedef sptr<Atom> (*MacroDelegate)(Parser& tp, std::vector<std::string>& args);

class PreDefMacro : public MacroInfo {
private:
  MacroDelegate _delegate;

public:
  no_copy_assign(PreDefMacro);

  PreDefMacro(int argc, int posOpts, MacroDelegate delegate)
      : MacroInfo(argc, posOpts), _delegate(delegate) {}

  PreDefMacro(int argc, MacroDelegate delegate) : MacroInfo(argc), _delegate(delegate) {}

  sptr<Atom> invoke(Parser& tp, std::vector<std::string>& args) override;
};

}  // namespace microtex

#endif  // MACRO_H_INCLUDED
