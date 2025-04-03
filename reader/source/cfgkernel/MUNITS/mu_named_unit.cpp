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

#include <UTILS/error.h>

#include "mu_named_unit.h"


/* --------- Constructors and destructor --------- */


MuNamedUnit_t::MuNamedUnit_t(MuDimension_e dim,const string &formula,bool do_check) :
  MuUnit_t(MU_search_unit(dim,formula)),
  myName(formula)
{
  if(do_check) {
    static MuUnit_t a_none(0,0);
    if(*this==a_none) throw MvError_t("MuNamedUnit_t::MuNamedUnit_t -> \"%s\" is not a valid unit for a %s",
				      formula.c_str(),MU_get_dimension(dim).c_str());
  }
}

// Constructor from name and coefficients
MuNamedUnit_t::MuNamedUnit_t(const string &name,double coeff,double offset) :
  MuUnit_t(coeff,offset),
  myName(name)
{}

// Constructor from a name and a unit
MuNamedUnit_t::MuNamedUnit_t(const string &name,const MuUnit_t &unit) :
  MuUnit_t(unit),
  myName(name)
{}


bool MuNamedUnit_t::operator==(const MuUnit_t &unit) const {
  bool a_result=MuUnit_t::operator==(unit);
  return a_result;
}

bool MuNamedUnit_t::operator==(const MuNamedUnit_t &unit) const {
//  if(!myName.empty() && myName==unit.myName) return true;
  //
  bool a_result=MuUnit_t::operator==((MuUnit_t &)unit);
  return a_result;
}