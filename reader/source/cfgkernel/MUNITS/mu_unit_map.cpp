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

#include <UTILS/win32_utils.h>

#include <UTILS/str_utils.h>
#include <UTILS/set_utils.h>

#include "mu_unit_map.h"



static const MuDimensions_t &loc_get_primary_dimensions();
static const MuDimensions_t &loc_get_secondary_dimensions();

static MuNamedUnit_t          loc_get_unit(MuDimension_e dimension,const MuUnitMap_t &primary_units);
static MuNamedUnit_t          loc_get_unit(const MuNamedUnit_t &unit,double power);




/* --------- Access to data ---------*/


void MuUnitMap_t::setUnit(MuDimension_e dim,const string &formula) {
  (*this)[dim]=MuNamedUnit_t(dim,formula);
}



/* --------- Expansion --------- */



void MuUnitMap_t::expand() {
  const MuDimensions_t &a_primary_dimensions   = loc_get_primary_dimensions();
  const MuDimensions_t &a_secondary_dimensions = loc_get_secondary_dimensions();
  //
  MuDimensions_t::const_iterator a_pd_it_begin = a_primary_dimensions.begin();
  MuDimensions_t::const_iterator a_pd_it_end   = a_primary_dimensions.end();
  MuDimensions_t::const_iterator a_pd_it;
  for(a_pd_it=a_pd_it_begin;a_pd_it!=a_pd_it_end;++a_pd_it) {
    MuDimension_e a_dimension=(*a_pd_it);
    //
    MuUnitMap_t::iterator a_it=find(a_dimension);
    if(a_it==end()) (*this)[a_dimension]=MuNamedUnit_t(a_dimension,MU_get_SI_unit(a_dimension));
  }
  //
  MuDimensions_t::const_iterator a_sd_it_begin = a_secondary_dimensions.begin();
  MuDimensions_t::const_iterator a_sd_it_end   = a_secondary_dimensions.end();
  MuDimensions_t::const_iterator a_sd_it;
  for(a_sd_it=a_sd_it_begin;a_sd_it!=a_sd_it_end;++a_sd_it) {
    MuDimension_e a_dimension=(*a_sd_it);
    //
    MuUnitMap_t::iterator a_it=find(a_dimension);
    if(a_it==end()) (*this)[a_dimension]=loc_get_unit(a_dimension,*this);
  }
}



MuUnitMap_t *MuUnitMap_t::getExpanded(MuUnitMap_t *expanded_p) const {
  MuUnitMap_t *a_expanded_p=(expanded_p!=NULL ? expanded_p : new MuUnitMap_t());
  //
  *a_expanded_p=(*this);
  a_expanded_p->expand();
  //
  return a_expanded_p;
}


/* --------- Checking unit compatibility --------- */

bool MU_check_unit(const string &dim,const string &unit) {
  
  MuDimension_e a_dim=MU_get_dimension(dim);
  return MU_check_unit(a_dim,unit);
  
}

bool MU_check_unit(MuDimension_e dim,const string &unit) {
  
  return MU_is_valid_unit(dim,unit);
  
}


/* --------- Static functions --------- */


static const MuDimensions_t &loc_get_primary_dimensions() {
  static MuDimensions_t a_dimensions;
  static bool           a_first=true;
  //
  if(a_first) {
    a_first=false;
    //
    for(int i=QTY_UNKNOWN+1;i<QTY_LAST;++i) {
      MuEQuantity_e a_equantity = (MuEQuantity_e)i;
      MuDimension_e a_dimension = MU_get_dimension(a_equantity);
      //
      a_dimensions.insert(a_dimension);
    }
    //
    a_dimensions.insert(UDI_DIMENSIONLESS);
  }
  //
  return a_dimensions;
}



static const MuDimensions_t &loc_get_secondary_dimensions() {
  static MuDimensions_t a_dimensions;
  static bool           a_first=true;
  //
  if(a_first) {
    a_first=false;
    //
    MuDimension_e a_dim_begin = MU_get_dimension_first();
    MuDimension_e a_dim_end   = MU_get_dimension_end();
    for(int a_dim=a_dim_begin;a_dim<a_dim_end;++a_dim) {
      a_dimensions.insert((MuDimension_e)a_dim);
    }
    //
    a_dimensions-=loc_get_primary_dimensions();
  }
  //
  return a_dimensions;
}




static MuNamedUnit_t loc_get_unit(MuDimension_e dimension,const MuUnitMap_t &primary_units) {
  typedef vector<MuEQuantity_e> LocEQuantites_t;
  LocEQuantites_t a_neg_exponents;
  a_neg_exponents.reserve(QTY_LAST-QTY_UNKNOWN-1);
  //
  bool     a_do_formula=true;
  string   a_formula="";
  MuUnit_t a_unit(1);
  //
  const MuQuantity_t &a_quantity         = MU_get_quantity(dimension);
  int                 a_nb_pos_exponents = 0;
  //
  for(int i=QTY_UNKNOWN+1;i<QTY_LAST;++i) {
    MuEQuantity_e a_equantity = (MuEQuantity_e)i;
    double        a_exp       = a_quantity[a_equantity];
    //
    if(a_exp>0) {
      MuDimension_e a_loc_dimension = MU_get_dimension(a_equantity);
      MuNamedUnit_t a_loc_unit      = (*(primary_units.find(a_loc_dimension))).second;
      //
      if(a_loc_unit.getName().empty()) a_do_formula=false;
      if((a_nb_pos_exponents++)>0 && a_do_formula) a_formula+="*";
      a_loc_unit=loc_get_unit(a_loc_unit,a_exp);
      if(a_do_formula) a_formula+=a_loc_unit.getName();
      a_unit*=a_loc_unit;
    } else if(a_exp<0) {
      a_neg_exponents.push_back(a_equantity);
    }
  }
  //
  int a_nb_neg_exponents=(int)(a_neg_exponents.size());
  //
  if(a_nb_neg_exponents>0 && a_nb_pos_exponents>0 && a_do_formula) {
    a_formula+="/";
    if(a_nb_neg_exponents>1) a_formula+="(";
  }
  //
  for(int j=0;j<a_nb_neg_exponents;++j) {
    MuEQuantity_e a_equantity = a_neg_exponents[j];
    double        a_exp       = a_quantity[a_equantity];
    //
    MuDimension_e a_loc_dimension = MU_get_dimension(a_equantity);
    MuNamedUnit_t a_loc_unit      = (*(primary_units.find(a_loc_dimension))).second;
    //
    if(a_loc_unit.getName().empty()) a_do_formula=false;
    if(j>0 && a_do_formula) a_formula+="*";
    if(a_nb_neg_exponents>0 && a_nb_pos_exponents>0) a_exp=(-a_exp);
    a_loc_unit=loc_get_unit(a_loc_unit,a_exp);
    if(a_do_formula) a_formula+=a_loc_unit.getName();
    if(a_exp<0) a_unit*=a_loc_unit; else a_unit/=a_loc_unit;
  }
  //
  if(a_nb_neg_exponents>1 && a_nb_pos_exponents>0 && a_do_formula) a_formula+=")";
  //
  //MuUnit_t a_unit   = MU_search_unit(dimension,a_formula);
  //
  if(!a_do_formula) {
    a_formula="";
  } else if(dimension!=UDI_ACCELERATION) {
    string   a_result = MU_search_unit(dimension,a_unit);
    if(a_result!="") a_formula=a_result;
  }
  //
  return MuNamedUnit_t(a_formula,a_unit);
}





static MuNamedUnit_t loc_get_unit(const MuNamedUnit_t &unit,double power) {
  const string   &a_formula = unit.getName();
  const MuUnit_t &a_unit    = unit;
  //
  MuUnit_t a_loc_unit=a_unit;
  a_loc_unit^=power;
  //
  string a_loc_formula=a_formula;
  if(a_loc_formula!="" && power!=1) {
    if(power==2) {
      a_loc_formula+="^2";
    } else if(power==3) {
      a_loc_formula+="^3";
    } else {
      a_loc_formula=str_printf("%s^%lg",a_formula.c_str(),power);
    }
  }
  //
  return MuNamedUnit_t(a_loc_formula,a_loc_unit);
}


