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

#include <UTILS/mv_cstdio.h> 
#include <UTILS/mv_stl_various.h>
#include <UTILS/error.h>



#include "mv_compare.h"


template<class Type_t> bool loc_compare(const Type_t &lvalue,const string &comparator,const Type_t &rvalue) {
  if(comparator=="==") return lvalue==rvalue;
  if(comparator=="!=") return lvalue!=rvalue;
  if(comparator==">")  return lvalue>rvalue;
  if(comparator==">=") return lvalue>=rvalue;
  if(comparator=="<")  return lvalue<rvalue;
  if(comparator=="<=") return lvalue<=rvalue;
  throw MvError_t("\"%s\" is a wrong comparator",comparator.c_str());
}


/* --------- Comparing entity to entity --------- */


bool MV_compare(int lvalue,const string &comparator,int rvalue) {
  return loc_compare(lvalue,comparator,rvalue);
}



bool MV_compare(double lvalue,const string &comparator,double rvalue) {
  return loc_compare(lvalue,comparator,rvalue);
}



bool MV_compare(const string &lvalue,const string &comparator,const string &rvalue) {
  return loc_compare(lvalue,comparator,rvalue);
}






/* ------ Comparing entity to string --------- */

bool MV_compare(int lvalue,const string &comparator,const string &rvalue) {
  try {
    int a_rvalue=0;
    if(rvalue=="TRUE") {
      a_rvalue=1;
    } else if(rvalue=="FALSE") {
      a_rvalue=0;
    } else {
      sscanf(rvalue.c_str(),"%i",&a_rvalue);
    }
    return loc_compare(lvalue,comparator,a_rvalue);
  } catch(MvError_t &a_error) {
    throw MvError_t("MV_compare -> %s for integers",a_error.c_str());
  }
}

bool MV_compare(double lvalue,const string &comparator,const string &rvalue) {
  try {
    double a_rvalue=0;
    sscanf(rvalue.c_str(),"%lf",&a_rvalue);
    return loc_compare(lvalue,comparator,a_rvalue);
  } catch(MvError_t &a_error) {
    throw MvError_t("MV_compare -> %s for floats",a_error.c_str());
  }
}


/*
bool MV_compare(const string &lvalue,const string &comparator,const string &rvalue) {
  try {
    return loc_compare(lvalue,comparator,rvalue);
  } catch(MvError_t &a_error) {
    throw MvError_t("MV_compare -> %s for strings",a_error.c_str());
  }
}
*/






