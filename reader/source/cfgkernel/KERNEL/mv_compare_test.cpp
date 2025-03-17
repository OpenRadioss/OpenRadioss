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
#include <UTILS/str_utils.h>
#include <UTILS/error.h>
#include <MESSAGE/mv_messages.h>



#include "mv_descriptor.h"        
#include "mv_compare.h"
#include "mv_compare_test.h"


/* --------- Constructors and destructor --------- */

MvCompareTest_t::MvCompareTest_t(int ikeyword,const string &comp,const string &value) :
  MvTest_t(),
  myIKeyword(ikeyword),
  myComparator(comp),
  myValue(value)
{}


/* --------- Public methods --------- */


bool MvCompareTest_t::isValid(const MvIKeywordSet_t &valid_ikeywords) const {
  return myIKeyword<valid_ikeywords;
}



string MvCompareTest_t::getReport(const MvDescriptor_t *descr_p) const {
  string a_report=descr_p->getSKeyword(getIKeyword());
  a_report+=getComparator();
  a_report+=getValue();
  return a_report;
}



/* --------- Checking --------- */


bool MvCompareTest_t::checkValue(int value,bool do_not_check_defaults) const {
  if(value==0 && do_not_check_defaults) return true; 
  return MV_compare(value,myComparator,myValue);
}


bool MvCompareTest_t::checkValue(double value) const {
  if(value==0.) return true; 
  return MV_compare(value,myComparator,myValue);
}

bool MvCompareTest_t::checkValue(const string &value) const {
  return MV_compare(value,myComparator,myValue);
}

