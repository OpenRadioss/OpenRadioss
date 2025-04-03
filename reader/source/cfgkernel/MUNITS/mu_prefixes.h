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


#ifndef MU_PREFIXES_H
#define MU_PREFIXES_H

#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>


class MuPrefixes_t {
 public:
  MuPrefixes_t();
  ~MuPrefixes_t();
 public:
  double        getMultiplyer(const string &name) const;
  const string &getName(double multiplyer) const;
 public:
  inline int getNbPrefixes() const { return  (int)(myMultiplyers.size()); }
  const string &operator[](int i) const;
 private:
  void addPrefix(const string &name,double multiplyer);
 private:
  class MyLessDouble_t {
  public:
    bool operator()(const double &lvalue,const double &rvalue) const;
  };
  typedef map<string,double>                MyMultiplyers_t;
  typedef map<double,string,MyLessDouble_t> MyNames_t;
  typedef vector<string>                    MyNameArray_t;
 private:
  MyMultiplyers_t       myMultiplyers;
  MyNames_t             myNames;
  mutable MyNameArray_t myNameArray;
 private:
  static string myEmptyName;
};


#endif //MU_PREFIXES_H
