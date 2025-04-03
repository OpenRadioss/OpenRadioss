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


#include <UTILS/win32_utils.h>

#include "mu_unit_manager.h"


/* --------- Constructors and destructors --------- */

MuUnitManager_t::MuUnitManager_t(int nb_units) :
  myNames(),
  myUnits(),
  myIndexesByName()
{
  if(nb_units>0) {
    myNames.reserve(nb_units);
    myUnits.reserve(nb_units);
  }
}

MuUnitManager_t::~MuUnitManager_t() {
}


/* ---------  Adding units --------- */

void MuUnitManager_t::reserve(int nb_units) {
  myNames.reserve(nb_units);
  myUnits.reserve(nb_units);
}

void MuUnitManager_t::addUnit(const string &name,const MuUnit_t &unit) {
  int a_index=(int)(myUnits.size());
  //
  myNames.push_back(name);
  myUnits.push_back(unit);
  myIndexesByName[name]=a_index;
}


/* --------- Access, single reseach --------- */

int MuUnitManager_t::operator[](const string &name) const {
  MyIndexesByName_t::const_iterator a_it=myIndexesByName.find(name);
  if(a_it==myIndexesByName.end()) return -1;
  return (*a_it).second;
}


/* --------- Multiple research --------- */

MuUnitIndexes_t *MuUnitManager_t::searchUnits(const MuUnit_t  &unit,
					      MuUnitIndexes_t *indexes_p) const
{
  MuUnitIndexes_t *a_indexes_p=(indexes_p==NULL ? new MuUnitIndexes_t() : indexes_p);
  //
  int a_nb_units=getNbUnits();
  //
  for(int i=0;i<a_nb_units;++i) {
    const MuUnit_t &a_unit=getUnit(i);
    if(a_unit==unit) {
      a_indexes_p->insert(i);
    }
  }
  //
  return a_indexes_p;
}

MuUnitNames_t *MuUnitManager_t::searchUnitNames(const MuUnit_t &unit,
						  MuUnitNames_t  *names_p) const
{
  MuUnitNames_t *a_names_p=(names_p==NULL ? new MuUnitNames_t() : names_p);
  //
  MuUnitIndexes_t a_indexes;
  searchUnits(unit,&a_indexes);
  //
  MuUnitIndexes_t::iterator a_it_begin = a_indexes.begin();
  MuUnitIndexes_t::iterator a_it_end   = a_indexes.end();
  MuUnitIndexes_t::iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) a_names_p->insert((*this)[*a_it]);
  //
  return a_names_p;
}

