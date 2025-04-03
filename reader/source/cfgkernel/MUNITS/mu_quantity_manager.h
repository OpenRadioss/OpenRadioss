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


#ifndef MU_QUANTITY_MANAGER_H
#define MU_QUANTITY_MANAGER_H

#include <UTILS/mv_stl_various.h>

#include "mu_quantity.h"
#include "mu_unit_manager.h"


typedef set<string> MuQuantityNames_t;
typedef set<int>    MuQuantityIndexes_t;


class MuQuantityManager_t {

 public:  // Constructors and destructors
  MuQuantityManager_t(int nb_quantities=150);
  ~MuQuantityManager_t();

 public:  // Adding quantities
  void setQuantity(int i,const string &name,const MuQuantity_t &quantity,int nb_units=0);

 public:  // Access, single reseach
  inline int getNbQuantities() const { return (int)(myQuantities.size()); }
  inline const MuQuantity_t &getQuantity(int i) const { return myQuantities[i]; }
  inline const string &operator[](int i) const { return myNames[i]; }
  int operator[](const string &name) const;

 public:  // Multiple research
  MuQuantityIndexes_t *searchQuantities(const MuQuantity_t &quantity,MuQuantityIndexes_t *indexes_p=NULL) const;
  MuQuantityNames_t   *searchQuantityNames(const MuQuantity_t &quantity,MuQuantityNames_t *names_p=NULL) const;

 public:  // Basic units
  void                 addBasicUnit(int quantity_ind,const string &unit_name,const MuUnit_t &unit);
  int                  getNbBasicUnits(int quantity_ind) const;
  const string        &getBasicUnitName(int quantity_ind,int unit_ind) const;
  const MuUnit_t      &getBasicUnit(int quantity_ind,int unit_ind) const;
  const MuUnit_t      &getBasicUnit(int quantity_ind,const string &unit_name) const;
  MuQuantityIndexes_t *searchQuantitiesFromBasicUnit(const string        &unit_name,
						     MuQuantityIndexes_t *indexes_p=NULL) const;
  
 private: // Types
  typedef map<string,int>                 MyIndexesByName_t;
  typedef vector<string>                  MyNameArray_t;
  typedef vector<MuQuantity_t>            MyQuantityArray_t;
  typedef vector<MuUnitManager_t>         MyUnitManagerArray_t;
  typedef map<string,MuQuantityIndexes_t> MyQuantitiesByBasicUnit_t;
  
 private: // Data
  MyIndexesByName_t         myIndexesByName;
  MyNameArray_t             myNames;
  MyQuantityArray_t         myQuantities;
  MyUnitManagerArray_t      myBasicUnits;
  MyQuantitiesByBasicUnit_t myQuantitiesByBasicUnit;

};


#endif //MU_QUANTITY_MANAGER_H
