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
#ifndef _MAP_UTILS_H_
#define _MAP_UTILS_H_


#include <UTILS/mv_stl_various.h>



/** @name Map tools */
//@{

/// Removing an item from the map
template<class Key_t,class Item_t,class Comp_t> 
map<Key_t,Item_t,Comp_t> &operator-=(map<Key_t,Item_t,Comp_t> &m,const Item_t &item) {
  m.erase(item);
  return m;
}

/// Union of 2 maps
template<class Key_t,class Item_t,class Comp_t> 
map<Key_t,Item_t,Comp_t> &operator+=(map<Key_t,Item_t,Comp_t> &m0,const map<Key_t,Item_t,Comp_t> &m1) 
{
  for(typename map<Key_t,Item_t,Comp_t>::const_iterator it=m1.begin();it!=m1.end();++it) m0[(*it).first]=(*it).second;
  return m0;
}

/// Union of 2 maps
template<class Key_t,class Item_t,class Comp_t> 
map<Key_t,Item_t,Comp_t> operator+(const map<Key_t,Item_t,Comp_t> &m0,const map<Key_t,Item_t,Comp_t> &m1) 
{
  map<Key_t,Item_t,Comp_t> a_result=m0;
  a_result+=m1;
  return a_result;
}

/// Substraction of 2 maps
template<class Key_t,class Item_t,class Comp_t> 
map<Key_t,Item_t,Comp_t> &operator-=(map<Key_t,Item_t,Comp_t> &m0,const map<Key_t,Item_t,Comp_t> &m1) 
{
  for(typename map<Key_t,Item_t,Comp_t>::const_iterator it=m1.begin();it!=m1.end();++it) m0.erase((*it).first);
  return m0;
}

/// Substraction of 2 maps
template<class Key_t,class Item_t,class Comp_t> 
map<Key_t,Item_t,Comp_t> operator-(const map<Key_t,Item_t,Comp_t> &m0,const map<Key_t,Item_t,Comp_t> &m1) 
{
  map<Key_t,Item_t,Comp_t> a_result=m0;
  a_result-=m1;
  return a_result;
}



/// Inclusion test of key
template<class Key_t,class Item_t,class Comp_t> 
bool operator<(const Key_t &key,const map<Key_t,Item_t,Comp_t> &m) {
#ifdef WIN32
  typename std::map<Key_t,Item_t,Comp_t>::const_iterator a_it=m.find(key);
#else   //WIN32
  typename map<Key_t,Item_t,Comp_t>::const_iterator a_it=m.find(key);
#endif //WIN32
  return a_it!=m.end();
}



/// Inclusion tests
template<class Key_t,class Item_t,class CompS_t,class CompM_t> 
bool operator<(const set<Key_t,CompS_t> &s,const map<Key_t,Item_t,CompM_t> &m) {
  for(typename set<Key_t,CompS_t>::const_iterator it=s.begin();it!=s.end();++it) if(!((*it)<m)) return false;
  return true;
}



/// Adding a map into a set (adding keys)
template<class Key_t,class Item_t,class CompS_t,class CompM_t>
set<Key_t,CompS_t> &operator+=(set<Key_t,CompS_t> &s,const map<Key_t,Item_t,CompM_t> &m) {
  for(typename map<Key_t,Item_t,CompM_t>::const_iterator it=m.begin();it!=m.end();++it) s.insert((*it).first);
  return s;
}



/// Substracting a map from a set (substracting keys)
template<class Key_t,class Item_t,class CompS_t,class CompM_t>
set<Key_t,CompS_t> &operator-=(set<Key_t,CompS_t> &s,const map<Key_t,Item_t,CompM_t> &m) {
  for(typename map<Key_t,Item_t,CompM_t>::const_iterator it=m.begin();it!=m.end();++it) s.erase((*it).first);
  return s;
}


//@}


#endif //_MAP_UTILS_H_




