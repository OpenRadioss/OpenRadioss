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

#include "mu_prefixes.h"

#define LOC_EPSILON 1E-10


string MuPrefixes_t::myEmptyName="";


MuPrefixes_t::MuPrefixes_t() :
  myMultiplyers(),
  myNames(),
  myNameArray()
{
  addPrefix("y", 1.E-24);
  addPrefix("z", 1.E-21);
  addPrefix("a", 1.E-18);
  addPrefix("f", 1.E-15);
  addPrefix("p", 1.E-12);
  addPrefix("n", 1.E-09);
  addPrefix("mu", 1.E-06); 
  addPrefix("m", 1.E-03);
  addPrefix("c", 1.E-02);
  addPrefix("d", 1.E-01);
  addPrefix("da",1.E+01);
  addPrefix("h", 1.E+02);
  addPrefix("k", 1.E+03);
  addPrefix("M", 1.E+06);
  addPrefix("G", 1.E+09);
  addPrefix("T", 1.E+12);
  addPrefix("P", 1.E+15);
  addPrefix("E", 1.E+18);
  addPrefix("Z", 1.E+21);
  addPrefix("Y", 1.E+24);
}

MuPrefixes_t::~MuPrefixes_t() {
}

double MuPrefixes_t::getMultiplyer(const string &name) const {
  MyMultiplyers_t::const_iterator a_it=myMultiplyers.find(name);
  return a_it==myMultiplyers.end() ? 0. : (*a_it).second;
}

const string &MuPrefixes_t::getName(double multiplyer) const {
  MyNames_t::const_iterator a_it=myNames.find(multiplyer);
  return a_it==myNames.end() ? myEmptyName : (*a_it).second;
}

const string &MuPrefixes_t::operator[](int i) const {
  if(myNameArray.empty()) {
    myNameArray.reserve(getNbPrefixes());
    //
    MyMultiplyers_t::const_iterator a_it_begin = myMultiplyers.begin();
    MyMultiplyers_t::const_iterator a_it_end   = myMultiplyers.end();
    MyMultiplyers_t::const_iterator a_it;
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) myNameArray.push_back((*a_it).first);
  }
  //
  return myNameArray[i];
}

void MuPrefixes_t::addPrefix(const string &name,double multiplyer) {
  myMultiplyers[name]=multiplyer;
  myNames[multiplyer]=name;
}

bool MuPrefixes_t::MyLessDouble_t::operator()(const double &lvalue,const double &rvalue) const {
  if(rvalue!=0.) {
    double a_error=fabs((lvalue-rvalue)/rvalue);
    if(a_error<LOC_EPSILON) return false;
  }
  //
  return lvalue<rvalue;
}
