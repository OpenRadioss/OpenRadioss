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

#include <UTILS/set_utils.h>

#include "mu_quantity_manager.h"


/* --------- Constructors and destructors --------- */

MuQuantityManager_t::MuQuantityManager_t(int nb_quantities) :
  myIndexesByName(),
  myNames(),
  myQuantities(),
  myBasicUnits()
{
  if(nb_quantities>0) {
    myNames.reserve(nb_quantities);
    myQuantities.reserve(nb_quantities);
    myBasicUnits.reserve(nb_quantities);
  }
}

MuQuantityManager_t::~MuQuantityManager_t() {
}


/* --------- Adding quantities --------- */

void MuQuantityManager_t::setQuantity(int                 i,
				      const string       &name,
				      const MuQuantity_t &quantity,
				      int                 nb_units)
{
  int a_nb_quantities=getNbQuantities();
  //
  if(i>=a_nb_quantities) {
    a_nb_quantities=i+1;
    myNames.resize(a_nb_quantities);
    myQuantities.resize(a_nb_quantities);
    myBasicUnits.resize(a_nb_quantities);
  }
  //
  myIndexesByName[name] = i;
  myNames[i]            = name;
  myQuantities[i]       = quantity;
  //
  if(nb_units>0) myBasicUnits[i].reserve(nb_units);
}


/* --------- Access, single reseach --------- */

int MuQuantityManager_t::operator[](const string &name) const {
  MyIndexesByName_t::const_iterator a_it=myIndexesByName.find(name);
  if(a_it==myIndexesByName.end()) return -1;
  return (*a_it).second;
}


/* --------- Multiple reseach --------- */

MuQuantityIndexes_t *MuQuantityManager_t::searchQuantities(const MuQuantity_t  &quantity,
							   MuQuantityIndexes_t *indexes_p) const
{
  MuQuantityIndexes_t *a_indexes_p=(indexes_p==NULL ? new MuQuantityIndexes_t() : indexes_p);
  //
  int a_nb_quantities=getNbQuantities();
  //
  for(int i=0;i<a_nb_quantities;++i) {
    const MuQuantity_t &a_quantity=getQuantity(i);
    if(a_quantity==quantity) {
      a_indexes_p->insert(i);
    }
  }
  //
  return a_indexes_p;
}

MuQuantityNames_t *MuQuantityManager_t::searchQuantityNames(const MuQuantity_t &quantity,
							    MuQuantityNames_t  *names_p) const
{
  MuQuantityNames_t *a_names_p=(names_p==NULL ? new MuQuantityNames_t() : names_p);
  //
  MuQuantityIndexes_t a_indexes;
  searchQuantities(quantity,&a_indexes);
  //
  MuQuantityIndexes_t::iterator a_it_begin = a_indexes.begin();
  MuQuantityIndexes_t::iterator a_it_end   = a_indexes.end();
  MuQuantityIndexes_t::iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) a_names_p->insert((*this)[*a_it]);
  //
  return a_names_p;
}


/* --------- Basic units --------- */

void MuQuantityManager_t::addBasicUnit(int             quantity_ind,
				       const string   &unit_name,
				       const MuUnit_t &unit)
{
  myBasicUnits[quantity_ind].addUnit(unit_name,unit);
  myQuantitiesByBasicUnit[unit_name].insert(quantity_ind);
}

int MuQuantityManager_t::getNbBasicUnits(int quantity_ind) const {
  return myBasicUnits[quantity_ind].getNbUnits();
}

const string &MuQuantityManager_t::getBasicUnitName(int quantity_ind,int unit_ind) const {
  return myBasicUnits[quantity_ind][unit_ind];
}

const MuUnit_t &MuQuantityManager_t::getBasicUnit(int quantity_ind,int unit_ind) const {
  return myBasicUnits[quantity_ind].getUnit(unit_ind);
}

const MuUnit_t &MuQuantityManager_t::getBasicUnit(int           quantity_ind,
						  const string &unit_name) const
{
  int a_unit_ind=myBasicUnits[quantity_ind][unit_name];
  return myBasicUnits[quantity_ind].getUnit(a_unit_ind);
}

MuQuantityIndexes_t *MuQuantityManager_t::searchQuantitiesFromBasicUnit(const string        &unit_name,
									MuQuantityIndexes_t *indexes_p) const
{
  MuQuantityIndexes_t *a_indexes_p=(indexes_p==NULL ? new MuQuantityIndexes_t() : indexes_p);
  //
  MyQuantitiesByBasicUnit_t::const_iterator a_it=myQuantitiesByBasicUnit.find(unit_name);
  if(a_it!=myQuantitiesByBasicUnit.end()) {
    const MuQuantityIndexes_t &a_indexes=(*a_it).second;
    (*a_indexes_p)+=a_indexes;
  }
  //
  return a_indexes_p;
}
