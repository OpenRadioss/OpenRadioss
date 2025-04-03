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

#include "mv_descriptor.h"
#include "mv_data_append_feature.h"


/* --------- Constructors & destructor --------- */

MvDataAppendFeature_t::MvDataAppendFeature_t(const string &name,int loc_ikeyword,
					     const MvFullType_t &full_type,const string &ext_skeyword,
					     bool is_default) :
  MvDataSingleFeature_t(DFT_APPEND,name,loc_ikeyword),
  myExternalFullType(full_type),
  myExternalSKeyword(ext_skeyword),
  myIsDefault(is_default)
{}

MvDataAppendFeature_t::~MvDataAppendFeature_t() {
}


/* --------- Output in an output stream --------- */

ostream &MvDataAppendFeature_t::display(ostream &os,const MvDescriptor_t &descr,int level) const {
  for(int i=0;i<level;i++) os << "  ";
  display_props(os); 
  //
  os << "APPEND(TITLE=\"" << getTitle() << "\"";
  if(!isThisKeyword()) os << ",LOC_KEYWORD="     << descr.getSKeyword(getIKeyword());
  os << ",EXT_FULLTYPE=" << (string)getExternalFullType();
  os << ",EXT_KEYWORD="  << getExternalSKeyword();
  if(isDefault()) os << ",DEFAULT";
  os << ")";
  //
  return os;
}




