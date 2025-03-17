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



#include "mv_data_feature.h"


/* --------- Constructors & destructor --------- */

MvDataFeature_t::MvDataFeature_t(MvDataFeatureType_e type,const string &title) :
  myType(type),
  myTitle(title),
  myIsOptional(false),
  myLockedDomains(0),   
  myLevel(0),
  data_feature_next(NULL),
  myIsVisible(true),
  myIsEditable(true),
  myIsParameterized(true),
  myDisplayStatus(1),
  myHasVisibleDef(false),
  myHasEditableDef(false),
  myHasParameterizedDef(false),
  myHasDisplayStatusDef(false)
{
    // inorder to disable parameter for flag feature
    if (type == DFT_FLAG)
    {
        myIsParameterized = false;
        myHasParameterizedDef = true;
    }
}

MvDataFeature_t::~MvDataFeature_t() {
}


/* --------- Output in an output stream --------- */

ostream &MvDataFeature_t::display(ostream &os,const MvDescriptor_t &,int level) const {
  for(int i=0;i<level;i++) os << "  ";
  os << "FEATURE(TITLE=\"" << getTitle() << "\")";
  return os;
}


ostream &MvDataFeature_t::display_props(ostream &os) const {
  if(isGraphical()) os << "graphical ";
  //
  int i,a_domains=0;
  for(i=(int)(DOM_UNKNOWN+1);i<(int)DOM_LAST;i<<=1) if(isLocked(i)) a_domains|=i;
  //
  if(a_domains) {
    os << "locked(";
    bool a_first=true;
    for(i=(int)(DOM_UNKNOWN+1);i<(int)DOM_LAST;i<<=1) if(a_domains & i) {
      if(a_first) a_first=false; else os << ",";
      os << MV_get_domain((MvDomain_e)i);
    }
    os << ") ";
  }
  //
  return os;
}





