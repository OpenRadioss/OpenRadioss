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


#ifndef MU_UNIT_MANAGER_H
#define MU_UNIT_MANAGER_H

#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>

#include "mu_unit.h"


typedef set<string> MuUnitNames_t;
typedef set<int>    MuUnitIndexes_t;


class MuUnitManager_t {

 public:  // Constructors and destructors
  MuUnitManager_t(int nb_units=0);
  ~MuUnitManager_t();

 public:  // Adding units
  void reserve(int nb_units);
  void addUnit(const string &name,const MuUnit_t &unit);
  inline void addUnit(const string &name,double coeff,double offset=0.) {
    addUnit(name,MuUnit_t(coeff,offset));
  }

 public:  // Access, single reseach
  inline int getNbUnits() const { return (int)(myUnits.size()); }
  inline const MuUnit_t &getUnit(int i) const { return myUnits[i]; }
  inline const string &operator[](int i) const { return myNames[i]; }
  int operator[](const string &name) const;  

 public:  // Multiple research
  MuUnitIndexes_t *searchUnits(const MuUnit_t &unit,MuUnitIndexes_t *indexes_p=NULL) const;
  MuUnitNames_t   *searchUnitNames(const MuUnit_t &unit,MuUnitNames_t *names_p=NULL) const;

 private: // Types
  typedef vector<string>   MyNameArray_t;
  typedef vector<MuUnit_t> MyUnitArray_t;
  typedef map<string,int>  MyIndexesByName_t;

 private: // Data
  MyNameArray_t     myNames;
  MyUnitArray_t     myUnits;
  MyIndexesByName_t myIndexesByName;

};


#endif //MU_UNIT_MANAGER_H
