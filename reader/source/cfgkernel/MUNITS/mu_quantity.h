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


#ifndef MU_QUANTITY_H
#define MU_QUANTITY_H

#include <UTILS/mv_string.h>
#include <UTILS/mv_iostream.h>
#include <UTILS/mv_stl_various.h>

#include <HCDI/hcdi.h>

#if defined _WIN32 || defined WIN32 
// C4251 - needs to have dll-interface to be used by clients of class.
//         windows stl template classes generate these.
#pragma warning(disable:4251)
#endif 

enum MuEQuantity_s {
  QTY_UNKNOWN,
  QTY_MASS,
  QTY_LENGTH,
  QTY_TIME,
  QTY_CURRENT,
  QTY_TEMPERATURE,
  QTY_MOLES,
  QTY_LIGHT,
  QTY_PLANE_ANGLE,
  QTY_SOLID_ANGLE,
  QTY_LAST
};

typedef enum MuEQuantity_s MuEQuantity_e;


class HC_DATA_DLL_API MuQuantity_t {

 public: // Constructors and destructor
  inline MuQuantity_t() : myEQuantityArray(QTY_LAST-QTY_UNKNOWN-1) {}
  inline ~MuQuantity_t() {}

 public: // Access to elementary quantities
  inline double operator[](MuEQuantity_e quantity) const { 
    return myEQuantityArray[quantity-QTY_UNKNOWN-1];
  }
  inline double &operator[](MuEQuantity_e quantity) { 
    return myEQuantityArray[quantity-QTY_UNKNOWN-1];
  }
  void storeExpr(MuEQuantity_e quantity, const string& expr);
  void storeArgs(const vector<string> &args);
  const string &getExpr(MuEQuantity_e quantity) const;
  const vector<string> &getArgsVect() const {return myEQuantityCustomArgList;}
  bool isCustomQuantity() { return !myEQuantityCustomArgList.empty(); }
 public: // Logical operators
  bool operator==(const MuQuantity_t &quantity) const;

 public: // Arithmetical operators
  MuQuantity_t &operator*=(const MuQuantity_t &quantity);
  MuQuantity_t &operator/=(const MuQuantity_t &quantity);
  MuQuantity_t &operator^=(double exponent);
  inline MuQuantity_t operator*(const MuQuantity_t &quantity) const {
    MuQuantity_t a_quantity(*this); return a_quantity*=quantity;
  }
  inline MuQuantity_t operator/(const MuQuantity_t &quantity) const {
    MuQuantity_t a_quantity(*this); return a_quantity/=quantity;
  }
  inline MuQuantity_t operator^(double exponent) const {
    MuQuantity_t a_quantity(*this); return a_quantity^=exponent;
  }

 public: // Names
  static const string  &getEQuantity(MuEQuantity_e equantity);
  static MuEQuantity_e  getEQuantity(const string &equantity);

 private:
  typedef vector<double>            MyEQuantityArray_t;
  typedef vector<string>            MyEQuantityNames_t;
  typedef map<string,MuEQuantity_e> MyEQuantityByNames_t;

 private:
  MyEQuantityArray_t myEQuantityArray;
  vector<string>       myEQuantityAttributeExpr;
  vector<string>       myEQuantityCustomArgList;
 private:
  static MyEQuantityNames_t   myEQuantityNames;
  static MyEQuantityByNames_t myEQuantityByNames;

};


ostream &operator<<(ostream &os,const MuQuantity_t &quantity);


#endif //MU_QUANTITY_H
