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
#ifndef _SET_UTILS_H_
#define _SET_UTILS_H_

#
#include <UTILS/mv_stl_various.h>



/** @name Set tools */
//@{

/// Inserting an item into the set
template<class Item_t,class Comp_t> set<Item_t,Comp_t> &operator+=(set<Item_t,Comp_t> &s,const Item_t &item) {
  s.insert(item);
  return s;
}

/// Removing an item from the set
template<class Item_t,class Comp_t> set<Item_t,Comp_t> &operator-=(set<Item_t,Comp_t> &s,const Item_t &item) {
  s.erase(item);
  return s;
}

/// Union of 2 sets
template<class Item_t,class Comp_t> set<Item_t,Comp_t> &operator+=(set<Item_t,Comp_t> &s0,
								   const set<Item_t,Comp_t> &s1) 
{
  
#ifdef _WIN32
  for(typename std::set<Item_t,Comp_t>::const_iterator it=s1.begin();it!=s1.end();++it) s0.insert(*it);
#else
  for(typename set<Item_t,Comp_t>::const_iterator it=s1.begin();it!=s1.end();++it) s0.insert(*it);
#endif
  
  return s0;
}

/// Union of 2 sets
template<class Item_t,class Comp_t> set<Item_t,Comp_t> operator+(const set<Item_t,Comp_t> &s0,
								 const set<Item_t,Comp_t> &s1) 
{
  set<Item_t,Comp_t> a_result=s0;
  a_result+=s1;
  return a_result;
}

/// Substraction of 2 sets
template<class Item_t,class Comp_t> set<Item_t,Comp_t> &operator-=(set<Item_t,Comp_t> &s0,
								   const set<Item_t,Comp_t> &s1) 
{
  
#ifdef _WIN32
  for(typename std::set<Item_t,Comp_t>::const_iterator it=s1.begin();it!=s1.end();++it) s0.erase(*it);
#else
  for(typename set<Item_t,Comp_t>::const_iterator it=s1.begin();it!=s1.end();++it) s0.erase(*it);
#endif
  
  return s0;
}

/// Substraction of 2 sets
template<class Item_t,class Comp_t> set<Item_t,Comp_t> operator-(const set<Item_t,Comp_t> &s0,
								 const set<Item_t,Comp_t> &s1) 
{
  set<Item_t,Comp_t> a_result=s0;
  a_result-=s1;
  return a_result;
}

/// Inclusion test (large)
template<class Item_t,class Comp_t> bool operator<=(const set<Item_t,Comp_t> &s0,const set<Item_t,Comp_t> &s1) {
  bool a_ok=true;
  
#ifdef _WIN32
  typename std::set<Item_t,Comp_t>::const_iterator a_s1_it_end=s1.end();
  for(typename std::set<Item_t,Comp_t>::const_iterator it=s0.begin();a_ok && it!=s0.end();++it) {
    typename std::set<Item_t,Comp_t>::const_iterator a_s1_it=s1.find(*it);
    a_ok=(a_s1_it!=a_s1_it_end);
  }
#else
  typename set<Item_t,Comp_t>::const_iterator a_s1_it_end=s1.end();
  for(typename set<Item_t,Comp_t>::const_iterator it=s0.begin();a_ok && it!=s0.end();++it) {
    typename set<Item_t,Comp_t>::const_iterator a_s1_it=s1.find(*it);
    a_ok=(a_s1_it!=a_s1_it_end);
  }
#endif
  
  return a_ok;
}

/// Inclusion test
template<class Item_t,class Comp_t> bool operator<(const Item_t &item,const set<Item_t,Comp_t> &s) {
  
#ifdef _WIN32
  typename std::set<Item_t,Comp_t>::const_iterator a_it=s.find(item);
#else
  typename set<Item_t,Comp_t>::const_iterator a_it=s.find(item);
#endif
  
  return a_it!=s.end();
}

/// Intersection
template<class Item_t,class Comp_t> set<Item_t,Comp_t> *inter(const set<Item_t,Comp_t> &s0,
							      const set<Item_t,Comp_t> &s1,
							      set<Item_t,Comp_t> *sr_p=NULL)
{
  if(sr_p==NULL) sr_p=new set<Item_t,Comp_t>();
  //
  
#ifdef _WIN32
  typename std::set<Item_t,Comp_t>::const_iterator a_it_begin = s0.begin();
  typename std::set<Item_t,Comp_t>::const_iterator a_it_end   = s0.end();
  typename std::set<Item_t,Comp_t>::const_iterator a_it;
#else
  typename set<Item_t,Comp_t>::const_iterator a_it_begin = s0.begin();
  typename set<Item_t,Comp_t>::const_iterator a_it_end   = s0.end();
  typename set<Item_t,Comp_t>::const_iterator a_it;
#endif
  
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
    const Item_t &a_item=(*a_it);
    if(a_item<s1) sr_p->insert(a_item);
  }
  //
  return sr_p;
}

/// Intersection
template<class Item_t,class Comp_t> set<Item_t,Comp_t> operator^(const set<Item_t,Comp_t> &s0,
								 const set<Item_t,Comp_t> &s1)
{
  set<Item_t,Comp_t> a_sr;
  inter(s0,s1,&a_sr);
  return a_sr;
}

//@}


#endif //_SET_UTILS_H_




