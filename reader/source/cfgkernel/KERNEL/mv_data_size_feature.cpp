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

#include "mv_descriptor.h"
#include "mv_data_size_feature.h"


/* --------- Constructors & destructor --------- */

MvDataSizeFeature_t::MvDataSizeFeature_t(const string &name,int ikeyword) :
  MvDataSingleFeature_t(DFT_SIZE,name,ikeyword),
  myNbArrays(0),
  myArrayTab(NULL)
{}

MvDataSizeFeature_t::~MvDataSizeFeature_t() {
  if(myArrayTab!=NULL) myfree(myArrayTab);
}


/* --------- Associated dynamic arrays --------- */

void MvDataSizeFeature_t::addArray(MvDataDynamicArrayFeature_t *daf_p) {
  myArrayTab=(MvDataDynamicArrayFeature_t **)myrealloc(myArrayTab, (size_t)((myNbArrays+1)*sizeof(MvDataDynamicArrayFeature_t *)));
  myArrayTab[myNbArrays++]=daf_p;
  daf_p->setSizeFeature(this);
}


/* --------- Output in an output stream --------- */

ostream &MvDataSizeFeature_t::display(ostream &os,const MvDescriptor_t &descr,int level) const {
  int i;
  for(i=0;i<level;i++) os << "  ";
  display_props(os); 
  //
  os << "SIZE(TITLE=\"" << getTitle() << "\""
     << ",KEYWORD="     << descr.getSKeyword(getIKeyword()) 
     << ") {"           << endl;
  //
  int a_nb_arrays=getNbArrays();
  ++level;
  for(i=0;i<a_nb_arrays;++i) getArrayPtr(i)->display(os,descr,level) << endl;
  --level;
  //
  for(i=0;i<level;i++) os << "  ";
  os << "}";
  //
  return os;
}




