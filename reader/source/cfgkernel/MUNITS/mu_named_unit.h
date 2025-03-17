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

#ifndef MU_NAMED_UNIT_H
#define MU_NAMED_UNIT_H

//#include <MUNITS/mu_unit.h>
#include <MUNITS/mu_dimension.h>


/// Units with a name
class MuNamedUnit_t : public MuUnit_t {

 public: /** @name Constructors and destructor */
  //@{
  
  MuNamedUnit_t(MuDimension_e dim,const string &formula,bool do_check=true);
  /// Constructor from name and coefficients
  MuNamedUnit_t(const string &name="",double coeff=1,double offset=0);
  /// Constructor from a name and a unit
  MuNamedUnit_t(const string &name,const MuUnit_t &unit);
  //@}

 public: /** @name Access to data */
  //@{
  /// Access to the name  
  inline const string &getName() const { return myName; }
  /// Converting into C string
  inline const char *c_str() const { return getName().c_str(); }
  //@}

 public:
   bool operator==(const MuUnit_t &unit) const;
   bool operator==(const MuNamedUnit_t &unit) const;

 private: // Data
  string myName;

};


#endif //MU_NAMED_UNIT_H
