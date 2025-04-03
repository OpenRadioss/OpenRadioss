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

#include "mv_type.h"
#include "mv_descriptor.h"
#include "mv_data_support_feature.h"


/* --------- Constructors & destructor --------- */

MvDataSupportFeature_t::MvDataSupportFeature_t(const string &name,int nb_attributes) :
  MvDataMultiFeature_t(DFT_SUPPORT,name,nb_attributes),
  myFullTypeSet(),
  myCompleteFullTypeSet()
{}

MvDataSupportFeature_t::~MvDataSupportFeature_t() {
}


/* --------- Accessors --------- */

void MvDataSupportFeature_t::setAttribute(const MvDescriptor_t &descr,int ind,int ikeyword,const string &name) { 
  setFeature(ind,ikeyword,name);
  value_type_e a_vtype=descr.getValueType(ikeyword);
  if(a_vtype==VTYPE_OBJECT) {
    object_type_e a_otype=descr.getObjectType(ikeyword);
    if(a_otype == HCDI_OBJ_TYPE_SETS) myCompleteFullTypeSet.insert(MvFullType_t(a_otype));
  }
}

void MvDataSupportFeature_t::addObjectFullType(const MvFullType_t &full_type) { 
  myFullTypeSet.insert(full_type);
  myCompleteFullTypeSet.insert(full_type);
}


/* --------- Output in an output stream --------- */

ostream &MvDataSupportFeature_t::display(ostream &os,const MvDescriptor_t &descr,int level) const {
  int j;
  for(j=0;j<level;++j) os << "  ";
  display_props(os); 
  //
  os << "SUPPORT(TITLE=\"" << getTitle() << "\") {" << endl;
  //
  int a_nb_attributes=getNbAttributes();
  ++level;
  for(int i=0;i<a_nb_attributes;++i) {
    for(j=0;j<level;++j) os << "  ";
    os << "ATTRIB[" << i 
       <<"]=("      << descr.getSKeyword(getAttributeIKeyword(i)) 
       << ",\""     << getAttributeTitle(i)
       << "\")"     << endl;
  }
  //
  if(getAllowedObjectFullTypes().size()>0) {
    for(j=0;j<level;++j) os << "  ";
    os << "OBJECTS=(";
    MvFullTypeSet_t::const_iterator it_begin = getAllowedObjectFullTypes().begin();
    MvFullTypeSet_t::const_iterator it_end   = getAllowedObjectFullTypes().end();
    MvFullTypeSet_t::const_iterator it;
    for(it=it_begin;it!=it_end;++it) {
      if(it!=it_begin) os << ",";
      os << (string)(*it);
    }
    os << ")" << endl;
  }
  --level;
  //
  for(j=0;j<level;++j) os << "  ";
  os << "}";
  //
  return os;
}




