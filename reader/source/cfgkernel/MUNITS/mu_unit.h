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


#ifndef MU_UNIT_H
#define MU_UNIT_H

#include <UTILS/mv_iostream.h>
#include <HCDI/hcdi.h>

class HC_DATA_DLL_API MuUnit_t {

 public: // Constructors and destructor
  inline MuUnit_t(double coeff=1.,double offset=0.) : 
    myCoeff(coeff), myOffset(offset) {}
  MuUnit_t(double coeff,double offset,const MuUnit_t &ref_unit);

 public: // Access to data
   inline double getCoeff()  const { return myCoeff; }
   inline double getOffset() const { return myOffset; }

 public: // Convertions
  inline double convertFrom(double value,const MuUnit_t &unit) const {
    double a_ref_value=value*unit.myCoeff+unit.myOffset;
    return (a_ref_value-myOffset)/myCoeff;
  }
  inline double convertTo(double value,const MuUnit_t &unit) const { 
    return unit.convertFrom(value,*this);
  }

 public: // Logical operators
  bool operator==(const MuUnit_t &unit) const;
  inline bool operator!=(const MuUnit_t &unit) const { return !(*this==unit); }

 public: // Arithmetical operators
  MuUnit_t &operator*=(const MuUnit_t &unit);
  MuUnit_t &operator/=(const MuUnit_t &unit);
  MuUnit_t &operator^=(double exponent);
  inline MuUnit_t operator*(const MuUnit_t &unit) const {
    MuUnit_t a_unit(*this); return a_unit*=unit;
  }
  inline MuUnit_t operator/(const MuUnit_t &unit) const {
    MuUnit_t a_unit(*this); return a_unit/=unit;
  }
  inline MuUnit_t operator^(double exponent) const {
    MuUnit_t a_unit(*this); return a_unit^=exponent;
  }

 private:
  double myCoeff;
  double myOffset;

};


ostream &operator<<(ostream &os,const MuUnit_t &unit);


#endif //MU_UNIT_H
