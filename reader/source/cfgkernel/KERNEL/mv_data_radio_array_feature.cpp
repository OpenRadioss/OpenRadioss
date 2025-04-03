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
#include "mv_data_radio_feature.h"
#include "mv_data_radio_array_feature.h"


/* --------- Constructors & destructor --------- */

MvDataRadioArrayFeature_t::MvDataRadioArrayFeature_t(const string &name,int ikeyword,int nb_values,int size) : 
  MvDataStaticArrayFeature_t(name,1,size)
{
  myType=DFT_RADIO_ARRAY;
  MvDataRadioFeature_t *a_drf_p=new MvDataRadioFeature_t(name,ikeyword,VTYPE_INT,nb_values); 
  setDataFeature(0,(MvDataFeature_t *)a_drf_p);
}

MvDataRadioArrayFeature_t::~MvDataRadioArrayFeature_t() {
}


/* --------- Output in an output stream --------- */

ostream &MvDataRadioArrayFeature_t::display(ostream &os,const MvDescriptor_t &descr,int level) const {
  int i;
  for(i=0;i<level;i++) os << "  ";
  display_props(os); 
  //
  os << "RADIO_ARRAY(TITLE=\"" << MvDataFeature_t::getTitle() << "\""
     << ",KEYWORD="      << descr.getSKeyword(getIKeyword())
     << ") {" << endl;
  //
  for(int j=0;j<getNbRadios();j++) {
    for(i=0;i<level;i++) os << "  ";
    os << "  BUTTON[" << j 
       << "]=("       << getRadioValue(j) 
       << ",\""       << getRadioName(j) << "\""
       << ")"         << endl;
  }
  //
  for(i=0;i<getSize();++i) {
    for(int j=0;j<=level;j++) os << "  ";
    os << "TITLE[" << i << "]=\"" << getTitle(i) << "\"" << endl;
  }
  //
  for(i=0;i<level;i++) os << "  ";
  os << "}";
  return os;
}




