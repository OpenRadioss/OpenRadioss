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
#ifndef MV_DEFAULT_MAP_H
#define MV_DEFAULT_MAP_H


#include <UTILS/mv_stl_various.h>



template <class MyType_t> 
class MvDefaultMap_t : public map<int,MyType_t> {
public: // Constructors & destructor
  inline MvDefaultMap_t() : map<int,MyType_t>() {}
public: // Access
  inline bool insert(int ikeyword,MyType_t value) { 
    return insert(MvDefaultMap_t::value_type(ikeyword,value)).second; 
  }
};

typedef MvDefaultMap_t<int>    MvIntDefaultMap_t;
typedef MvDefaultMap_t<unsigned int>    MvUIntDefaultMap_t;
typedef MvDefaultMap_t<double> MvFloatDefaultMap_t;
typedef MvDefaultMap_t<string> MvStringDefaultMap_t;
typedef MvDefaultMap_t<unsigned int>    MvObjectDefaultMap_t;

#endif //MV_DEFAULT_PAIR_H




