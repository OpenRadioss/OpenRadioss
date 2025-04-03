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
#include <UTILS/memory_utils.h>

#include "mv_type.h"
#include "mv_descriptor.h"
#include "mv_data_data_feature.h"
#include <cstring>

/* --------- Constructors & destructor --------- */

MvDataDataFeature_t::MvDataDataFeature_t(const string &name,int ikeyword,object_type_e obj_type) :
  MvDataObjectFeature_t(DFT_DATA,name,ikeyword,obj_type),
  myFullTypeSet(),
  myFilter(NULL)
{}

MvDataDataFeature_t::~MvDataDataFeature_t()
{
    if (myFilter)
    {
        delete myFilter;
        myFilter = NULL;
    }
}

void MvDataDataFeature_t::createFilter(int nb, StringVect_t &attribs, StringVect_t &values, StringVect_t &criterias, StringVect_t &units, StringVect_t& messgs)
{
    if (nb)
    {
        myFilter = new EntityFilter(nb, attribs, values, criterias, units, messgs);
    }
}
/* --------- Output in an output stream --------- */

ostream &MvDataDataFeature_t::display(ostream &os,const MvDescriptor_t &descr,int level) const {
  for(int i=0;i<level;i++) os << "  ";
  display_props(os); 
  //
  os << "DATA(TITLE=\"" << getTitle() << "\""
     << ",KEYWORD="     << descr.getSKeyword(getIKeyword()) 
     << ",OBJECT_TYPE=" << MV_get_type(getObjectType());
  //
  os << ",SUBTYPES=(";
  MvFullTypeSet_t::const_iterator a_ofts_begin = myFullTypeSet.begin();
  MvFullTypeSet_t::const_iterator a_ofts_end   = myFullTypeSet.end();
  MvFullTypeSet_t::const_iterator a_ofts_it;
  for(a_ofts_it=a_ofts_begin;a_ofts_it!=a_ofts_end;++a_ofts_it) {
    if(a_ofts_it!=a_ofts_begin) os << ",";
    os << (string)(*a_ofts_it);
  }
  os << ")";
  //
  os << ")";
  return os;
}




