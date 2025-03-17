/*Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.*/
#ifndef MV_DEPENDENCE_H
#define MV_DEPENDENCE_H


#include <UTILS/mv_stl_various.h>


#include "mv_ikeyword_containers.h"
#include "mv_conditional_items.h"



/** @name Attributes depending on conditional expressions */
//@{

/// Dependence class
class MvDependence_t : public MvConditionalItems_t<MvIKeywordSet_t> {

public: /** @name Constructors and destructor*/
  //@{
  /// Constructor
  inline MvDependence_t() : MvConditionalItems_t<MvIKeywordSet_t>() {}
  /// Destructor
  inline ~MvDependence_t() {}
  //@}

public: /** @name Consulting */
  //@{
  /// Gets the right (verified condition) ikeywords for the given condition (index)
  inline const MvIKeywordSet_t &getIKeywords(int ind)      const { return getItems(ind); }
  /// Gets the wrong (not verified condition) ikeywords for the given condition (index)
  inline const MvIKeywordSet_t &getWrongIKeywords(int ind) const { return getWrongItems(ind); }
  /// Gets the right (verified condition) ikeywords for the default
  inline const MvIKeywordSet_t &getDefaultIKeywords()      const { return getDefaultItems(); }
  /// Gets the wrong (not verified condition) ikeywords for default
  inline const MvIKeywordSet_t &getDefaultWrongIKeywords() const { return getDefaultWrongItems(); }

  //@}

public: // Output into an output stream
  ostream &display(ostream &os,const MvDescriptor_t &descr) const;

};



/// List of dependences
typedef vector<const MvDependence_t *> MvDependenceList_t;


//@}

#endif //MV_DEPENDENCE_H




