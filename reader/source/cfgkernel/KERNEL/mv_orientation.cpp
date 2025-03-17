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



#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>

#include "mv_orientation.h"

typedef map<string,MvOrientation_e> MvStrOrientationMap_t;


class MvOrientationMap_t {
public:
  MvOrientationMap_t();
  ~MvOrientationMap_t();
public:
  MvOrientation_e  getOrientation(const string &keyword)   const;
  const string  &getOrientation(MvOrientation_e orientation) const;
private:
  void InitOrientationMap();
  void InitOrientationArray();
private:
  MvStrOrientationMap_t  myOrientationMap;
  int                  myNbOrientations;
  string              *myOrientationArray;
};

MvOrientationMap_t::MvOrientationMap_t() : myNbOrientations(0), myOrientationArray(NULL) {
  InitOrientationMap();
  InitOrientationArray();
}

MvOrientationMap_t::~MvOrientationMap_t() {
  if(myOrientationArray!=NULL) delete [] myOrientationArray;
}

void MvOrientationMap_t::InitOrientationMap() {
  myOrientationMap["UNKNOWN"] = ORI_UNKNOWN;
  myOrientationMap["ROW"]     = ORI_ROW;
  myOrientationMap["COLUMN"]  = ORI_COLUMN;
  myOrientationMap["MATRIX"]  = ORI_MATRIX;
  myOrientationMap["MENU"]    = ORI_MENU;
  myOrientationMap["LAST"]    = ORI_LAST;
}

MvOrientation_e MvOrientationMap_t::getOrientation(const string &keyword) const {
  MvStrOrientationMap_t::const_iterator it=myOrientationMap.find(keyword);
  if(it!=myOrientationMap.end()) return (*it).second;
  return ORI_UNKNOWN;
}

const string &MvOrientationMap_t::getOrientation(MvOrientation_e orientation) const {
  return myOrientationArray[orientation];
}

void MvOrientationMap_t::InitOrientationArray() {
  // Counting
  myNbOrientations=0;
  MvStrOrientationMap_t::const_iterator it;
  for(it=myOrientationMap.begin();it!=myOrientationMap.end();++it) {
    int orientation=(int)((*it).second);
    if(orientation>myNbOrientations) myNbOrientations=orientation;
  }
  // Allocating
  myOrientationArray=new string[++myNbOrientations];
  // Filling
  for(int i=0;i<myNbOrientations;i++) myOrientationArray[i]="UNKNOWN";
  for(it=myOrientationMap.begin();it!=myOrientationMap.end();++it) {
    int orientation=(int)((*it).second);
    if(orientation>0) myOrientationArray[orientation]=(*it).first;
  }
}

static const MvOrientationMap_t &get_orientation_map() {
  static const MvOrientationMap_t MV_ORIENTATION_MAP;
  return MV_ORIENTATION_MAP;
}

MvOrientation_e MV_get_orientation(const string &keyword) {
  return get_orientation_map().getOrientation(keyword);
}

const string &MV_get_orientation(MvOrientation_e orientation) {
  return get_orientation_map().getOrientation(orientation);
}




