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
#include <UTILS/mv_stl_various.h>

#include "mv_comparator.h"

typedef map<string,MvComparator_e> MvStrComparatorMap_t;


/* --------- Internal classes --------- */

class MvComparatorMap_t {
public:
  MvComparatorMap_t();
public:
  MvComparator_e getComparator(const string &keyword)   const;
  const string  &getComparator(MvComparator_e comparator) const;
private:
  void InitComparatorMap();
  void InitComparatorArray();
private:
  MvStrComparatorMap_t myComparatorMap;
  string               myComparatorArray[CMPT_LAST];
};


MvComparatorMap_t::MvComparatorMap_t() {
  InitComparatorMap();
  InitComparatorArray();
}

void MvComparatorMap_t::InitComparatorMap() {
  myComparatorMap[">"]  = CMPT_GT; 
  myComparatorMap[">="] = CMPT_GE; 
  myComparatorMap["<"]  = CMPT_LT; 
  myComparatorMap["<="] = CMPT_LE; 
  myComparatorMap["=="] = CMPT_EQ;
  myComparatorMap["!="] = CMPT_NE;
}

void MvComparatorMap_t::InitComparatorArray() {
  int a_nb_comparators=(int)CMPT_LAST;
  for(int i=0;i<a_nb_comparators;i++) myComparatorArray[i]="UNKNOWN";
  //
  MvStrComparatorMap_t::const_iterator a_it_begin = myComparatorMap.begin();
  MvStrComparatorMap_t::const_iterator a_it_end   = myComparatorMap.end();
  MvStrComparatorMap_t::const_iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
    int a_comparator=(int)((*a_it).second);
    myComparatorArray[a_comparator]=(*a_it).first;
  }
}

MvComparator_e MvComparatorMap_t::getComparator(const string &keyword) const {
  MvStrComparatorMap_t::const_iterator it=myComparatorMap.find(keyword);
  if(it!=myComparatorMap.end()) return (*it).second;
  return CMPT_UNKNOWN;
}

const string &MvComparatorMap_t::getComparator(MvComparator_e comparator) const {
  return myComparatorArray[comparator];
}


/* --------- Static functions --------- */

static const MvComparatorMap_t &get_comparator_map() {
  static const MvComparatorMap_t MV_COMPARATOR_MAP;  /*multimap no sure*/
  return MV_COMPARATOR_MAP;
}


/* --------- Public functions --------- */

MvComparator_e MV_get_comparator(const string &cmp) {
  return get_comparator_map().getComparator(cmp);  
}

const string &MV_get_comparator(MvComparator_e cmp) {
  return get_comparator_map().getComparator(cmp);
}




