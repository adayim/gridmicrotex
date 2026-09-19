#ifndef MICROTEX_ATOM_VROW_H
#define MICROTEX_ATOM_VROW_H

#include <cstddef>
#include <map>

#include "atom/atom.h"
#include "atom/atom_space.h"
#include "env/units.h"

namespace microtex {

/** An atom representing a vertical row of other atoms. */
class VRowAtom : public Atom {
private:
  std::vector<sptr<Atom>> _elements;
  sptr<SpaceAtom> _raise;
  bool _addInterline;
  /** Extra space below an element, by its index: `\\[len]`. */
  std::map<std::size_t, Dimen> _gaps;

public:
  Alignment _valign = Alignment::none;
  Alignment _halign = Alignment::none;

  VRowAtom();

  explicit VRowAtom(const sptr<Atom>& base);

  inline void setAddInterline(bool addInterline) { _addInterline = addInterline; }

  inline bool isAddInterline() const { return _addInterline; }

  inline void setAlignTop(bool vtop) { _valign = vtop ? Alignment::top : Alignment::center; }

  inline bool isAlignTop() const { return _valign == Alignment::top; }

  void setRaise(UnitType unit, float r);

  sptr<Atom> popLastAtom();

  /** Add an atom at the front */
  void prepend(const sptr<Atom>& el);

  /** Add an atom at the tail */
  void append(const sptr<Atom>& el);

  /** Extra space below the last atom added, before the next one. */
  void addGapAfterLast(const Dimen& gap);

  sptr<Box> createBox(Env& env) override;
};

}  // namespace microtex

#endif  // MICROTEX_ATOM_VROW_H
