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
#include "win32_utils.h"  

#include "mv_quick_keyword_map.h"
#include "error.h"


/** @name Constructor and destructor */

MvQuickKeywordMap_t::MvQuickKeywordMap_t(int unknown_ikw,const string &unknown_skw) : 
  myMap(), 
  myArray(0), 
  myOffset(0),
  myUnknownIKeyword(unknown_ikw),
  myUnknownSKeyword(unknown_skw)
{}


/* --------- Before building --------- */
typedef map<string, int> LocMapStringInt_t; 
void MvQuickKeywordMap_t::build() {
  if(!(myMap.empty())) {
    int a_min=(*(myMap.begin())).second;
    int a_max=a_min;
    // Getting min & max
    
    LocMapStringInt_t::iterator a_it_begin;
    a_it_begin = myMap.begin();
    LocMapStringInt_t::iterator a_it_end   = myMap.end();
    LocMapStringInt_t::iterator a_it;
    
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
      int a_ikeyword=(*a_it).second;
      if(a_ikeyword<a_min)      a_min=a_ikeyword;
      else if(a_ikeyword>a_max) a_max=a_ikeyword;
    }
    // Setting size and offset
    myOffset=(-a_min);
    myArray.resize(a_max-a_min+1);
    // Filling the array
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) myArray[myOffset+(*a_it).second]=(*a_it).first;
  }
}


/* --------- After building --------- */

const string &MvQuickKeywordMap_t::operator[](int ikeyword) const {
#ifdef DEBUG
  if(!isBuilt()) throw MvError_t("MvQuickKeywordMap_t::operator[](int ikeyword) -> Not built");
#endif 
  int a_ind=myOffset+ikeyword;
/* FB_01_07_05_146  if(a_ind<0 || a_ind>=myArray.size()) return myUnknownSKeyword;*/
  if(a_ind<0 || a_ind>=(int)myArray.size()) return myUnknownSKeyword;
  return myArray[a_ind];
}

int MvQuickKeywordMap_t::operator[](const string &skeyword) const {
#ifdef DEBUG
  if(!isBuilt()) throw MvError_t("MvQuickKeywordMap_t::operator[](const string &skeyword) -> Not built");
#endif 
  LocMapStringInt_t::const_iterator a_it=myMap.find(skeyword); 
  if(a_it==myMap.end()) return myUnknownIKeyword;
  return (*a_it).second;
}


/* --------- Misc --------- */

ostream &MvQuickKeywordMap_t::display(ostream &os) const {
  
  LocMapStringInt_t::const_iterator a_it_begin = myMap.begin();
  LocMapStringInt_t::const_iterator a_it_end   = myMap.end();
  LocMapStringInt_t::const_iterator a_it;
  
  //
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
    const string &a_skeyword=(*a_it).first;
    int           a_ikeyword=(*a_it).second;
    os << setw(8) << a_ikeyword;
    os << " <-> " << a_skeyword << endl;
  }
  //
  return os;
}


/* --------- Non member functions --------- */

ostream &operator<<(ostream &os,const MvQuickKeywordMap_t &qkm) { 
  return qkm.display(os); 
}





