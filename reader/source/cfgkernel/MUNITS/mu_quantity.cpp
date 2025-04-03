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

#include <UTILS/mv_cmath.h>

#include "mu_quantity.h"

#define LOC_EPSILON 1E-8


MuQuantity_t::MyEQuantityNames_t   MuQuantity_t::myEQuantityNames;
MuQuantity_t::MyEQuantityByNames_t MuQuantity_t::myEQuantityByNames;


/* --------- Logical operators --------- */

bool MuQuantity_t::operator==(const MuQuantity_t &quantity) const {
  for(int i=QTY_UNKNOWN+1;i<QTY_LAST;++i) {
    MuEQuantity_e a_equantity = (MuEQuantity_e)i;
    double        a_lcoeff    = (*this)[a_equantity];
    double        a_rcoeff    = quantity[a_equantity];
    //
    if(a_lcoeff==0.) {
      if(a_rcoeff!=0.) return false;
    } else {
      double a_diff=fabs((a_rcoeff-a_lcoeff)/a_lcoeff);
      if(a_diff>LOC_EPSILON) return false;
    }
  }
  //
  return true;
}


/* --------- Arithmetical operators --------- */

MuQuantity_t &MuQuantity_t::operator*=(const MuQuantity_t &quantity) {
  for(int i=QTY_UNKNOWN+1;i<QTY_LAST;++i) {
    MuEQuantity_e a_equantity=(MuEQuantity_e)i;
    (*this)[a_equantity]+=quantity[a_equantity];
  }
  return *this;
}

MuQuantity_t &MuQuantity_t::operator/=(const MuQuantity_t &quantity) {
  for(int i=QTY_UNKNOWN+1;i<QTY_LAST;++i) {
    MuEQuantity_e a_equantity=(MuEQuantity_e)i;
    (*this)[a_equantity]-=quantity[a_equantity];
  }
  return *this;
}

MuQuantity_t &MuQuantity_t::operator^=(double exponent) {
  for(int i=QTY_UNKNOWN+1;i<QTY_LAST;++i) {
    MuEQuantity_e a_equantity=(MuEQuantity_e)i;
    (*this)[a_equantity]*=exponent;
  }
  return *this;
}


/* --------- Names --------- */

const string &MuQuantity_t::getEQuantity(MuEQuantity_e equantity) {
  if(myEQuantityNames.empty()) {
    myEQuantityNames.resize(QTY_LAST-QTY_UNKNOWN-1);
    //
    myEQuantityNames[QTY_MASS       -QTY_UNKNOWN-1] = "MASS";
    myEQuantityNames[QTY_LENGTH     -QTY_UNKNOWN-1] = "LENGTH";
    myEQuantityNames[QTY_TIME       -QTY_UNKNOWN-1] = "TIME";
    myEQuantityNames[QTY_CURRENT    -QTY_UNKNOWN-1] = "CURRENT";
    myEQuantityNames[QTY_TEMPERATURE-QTY_UNKNOWN-1] = "TEMPERATURE";
    myEQuantityNames[QTY_MOLES      -QTY_UNKNOWN-1] = "MOLES";
    myEQuantityNames[QTY_LIGHT      -QTY_UNKNOWN-1] = "LIGHT";
    myEQuantityNames[QTY_PLANE_ANGLE-QTY_UNKNOWN-1] = "PLANE_ANGLE";
    myEQuantityNames[QTY_SOLID_ANGLE-QTY_UNKNOWN-1] = "SOLID_ANGLE";
  }
  //
  return myEQuantityNames[int(equantity-QTY_UNKNOWN-1)];
}

MuEQuantity_e MuQuantity_t::getEQuantity(const string &equantity) {
  if(myEQuantityByNames.empty()) {
    for(int i=QTY_UNKNOWN+1;i<QTY_LAST;++i) {
      MuEQuantity_e a_equantity=(MuEQuantity_e)i;
      myEQuantityByNames[getEQuantity(a_equantity)]=a_equantity;
    }
  }
  //
  MyEQuantityByNames_t::iterator a_it=myEQuantityByNames.find(equantity);
  if(a_it==myEQuantityByNames.end()) return QTY_UNKNOWN;
  return (*a_it).second;
}

void MuQuantity_t::storeExpr(MuEQuantity_e quantity, const string &expr)
{
    if (myEQuantityAttributeExpr.empty()) {
        myEQuantityAttributeExpr.resize(QTY_LAST - QTY_UNKNOWN - 1);
    }
    myEQuantityAttributeExpr[quantity-QTY_UNKNOWN-1] = expr;
}

const string &MuQuantity_t::getExpr(MuEQuantity_e quantity) const
{
    static string emptystring;
    if(myEQuantityAttributeExpr.size() <= quantity) return emptystring;
    return myEQuantityAttributeExpr[quantity-QTY_UNKNOWN-1];
}

void MuQuantity_t::storeArgs(const vector<string> &args)
{
    vector<string>::const_iterator iter_b = args.begin();
    vector<string>::const_iterator iter_e = args.end();
    vector<string>::const_iterator iter;
    for (iter = iter_b; iter != iter_e; ++iter)
    {
        string a_arg = *iter;
        string pend_arg = "";
        int size = (int)a_arg.size();
        for (int i = 0; i < size; i++)
        {
            if (a_arg[i] == ' ')
                continue;
            pend_arg += a_arg[i];
        }
        myEQuantityCustomArgList.push_back(pend_arg);
    }
}
/* --------- Non member functions --------- */

ostream &operator<<(ostream &os,const MuQuantity_t &quantity) {
  typedef vector<MuEQuantity_e> LocEQuantites_t;
  //
  bool            a_is_pos_exponent=false;
  LocEQuantites_t a_neg_exponents;
  a_neg_exponents.reserve(QTY_LAST-QTY_UNKNOWN-1);
  //
  for(int i=QTY_UNKNOWN+1;i<QTY_LAST;++i) {
    MuEQuantity_e a_equantity = (MuEQuantity_e)i;
    double        a_exp       = quantity[a_equantity];
    //
    if(a_exp>0) {
      if(a_is_pos_exponent) os << "*";
      os << MuQuantity_t::getEQuantity(a_equantity);
      if(a_exp==2)      os << "^2";
      else if(a_exp==3) os << "^3";
      else if(a_exp!=1)  os << "^" << a_exp;
      a_is_pos_exponent=true;
    } else if(a_exp<0) {
      a_neg_exponents.push_back(a_equantity);
    }
  }
  //
  int a_nb_neg_exponents=(int)(a_neg_exponents.size());
  //
  if(!a_is_pos_exponent) {
    if(a_nb_neg_exponents==0) {
      os << "DIMENSIONLESS";
    } else if(a_nb_neg_exponents==1) {
      MuEQuantity_e a_equantity = a_neg_exponents[0];
      double        a_exp       = quantity[a_equantity];
      //
      os << MuQuantity_t::getEQuantity(a_equantity);
      if(a_exp!=-1) os << "^(" << a_exp << ")";
    } else {
      os << "1/("; 
      for(int j=0;j<a_nb_neg_exponents;++j) {
	MuEQuantity_e a_equantity = a_neg_exponents[j];
	double        a_exp       = -quantity[a_equantity];
	//
	if(j>0) os << "*";
	os << MuQuantity_t::getEQuantity(a_equantity);
	if(a_exp==2)      os << "^2";
	else if(a_exp==3) os << "^3";
	else if(a_exp!=1) os << "^" << a_exp;
      }
      os << ")"; 
    }
  } else if(a_nb_neg_exponents>0) {
    os << "/";
    if(a_nb_neg_exponents>1) os << "(";
    //
    for(int j=0;j<a_nb_neg_exponents;++j) {
      MuEQuantity_e a_equantity = a_neg_exponents[j];
      double        a_exp       = -quantity[a_equantity];
      //
      if(j>0) os << "*";
      os << MuQuantity_t::getEQuantity(a_equantity);
      if(a_exp==2)      os << "^2";
      else if(a_exp==3) os << "^3";
      else if(a_exp!=1) os << "^" << a_exp;
    }
    if(a_nb_neg_exponents>1) os << ")";
  }
  //
  return os;
}
