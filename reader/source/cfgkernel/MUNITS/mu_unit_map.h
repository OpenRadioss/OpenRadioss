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

//


#ifndef MU_UNIT_MAP_H
#define MU_UNIT_MAP_H

#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>
#include <MUNITS/mu_dimension.h>
#include <MUNITS/mu_named_unit.h>



/// Map dimension -> units
class  MuUnitMap_t : public map<MuDimension_e,MuNamedUnit_t> {

public: /** @name Constructors and destructor */
  //@{
  /// Constructor
  inline MuUnitMap_t() : map<MuDimension_e,MuNamedUnit_t>() {}
  //@}

public: /* @name Access to data */
  //@{
  /// Expanses the map (computing missing units)
  void setUnit(MuDimension_e dim,const string &formula);
  //@}

public: /* @name Expansion */
  //@{
  /// Expanses the map (computing missing units)
  void expand();
  /// Expanses the map (computing missing units)
  MuUnitMap_t *getExpanded(MuUnitMap_t *expanded_p=NULL) const;
  //@}

};



/// Checking unit compatibility (returns true if the unit is valid)
bool MU_check_unit(const string &dim,const string &unit);

/// Checking unit compatibility (returns true if the unit is valid)
bool MU_check_unit(MuDimension_e dim,const string &unit);


#endif //MU_UNIT_MAP_H

