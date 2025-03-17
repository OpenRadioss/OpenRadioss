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

#include "mu_unit.h"

#define LOC_EPSILON 1E-10


/* --------- Constructors and destructor --------- */

MuUnit_t::MuUnit_t(double coeff,double offset,const MuUnit_t &ref_unit) :
  myCoeff(coeff*ref_unit.myCoeff),
  myOffset(ref_unit.myCoeff*offset+ref_unit.myOffset)
{}


/* --------- Logical operators --------- */

bool MuUnit_t::operator==(const MuUnit_t &unit) const {
  // Comparing coeffs
  double a_ratio=fabs((unit.myCoeff-myCoeff)/myCoeff);
  if(a_ratio>LOC_EPSILON) return false;
  // Comparing offsets
  if(myOffset==0.) return unit.myOffset==0.;
  a_ratio=fabs((unit.myOffset-myOffset)/myOffset);
  return a_ratio<LOC_EPSILON;
}


/* --------- Arithmetical operators --------- */

MuUnit_t &MuUnit_t::operator*=(const MuUnit_t &unit) {
  myCoeff*=unit.myCoeff;
  myOffset=0.;
  //
  return *this;
}

MuUnit_t &MuUnit_t::operator/=(const MuUnit_t &unit) {
  myCoeff/=unit.myCoeff;
  myOffset=0.;
  //
  return *this;
}

MuUnit_t &MuUnit_t::operator^=(double exponent) {
  myCoeff=pow(myCoeff,exponent);
  myOffset=0.;
  //
  return *this;
}


/* --------- Non member functions --------- */

ostream &operator<<(ostream &os,const MuUnit_t &unit) {
  MuUnit_t a_si(1,0);
  double   a_offset = unit.convertTo(0,a_si);
  double   a_coeff  = unit.convertTo(1,a_si)-a_offset;
  //
  return os << "(" << a_coeff << "," << a_offset << ")";
}
