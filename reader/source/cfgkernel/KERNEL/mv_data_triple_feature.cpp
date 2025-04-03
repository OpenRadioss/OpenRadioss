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
#include "mv_data_triple_feature.h"


/* --------- Constructors & destructor --------- */

MvDataTripleFeature_t::MvDataTripleFeature_t(const string &name,
                         int ikw, const string& title,
                         MvDimension_e dimension,
                         vector<string>* argVect,
                         MvDataTripleFeatureType_e type) :
  MvDataFeature_t(DFT_TRIPLE, name),
  MvDataDimensionFeature_t(dimension, argVect)
{
  myPointTypeFlag = type;
  myIkeyword = ikw;
  myTitle = title;
}

MvDataTripleFeature_t::~MvDataTripleFeature_t() {
}


/* --------- Output in an output stream --------- */

ostream &MvDataTripleFeature_t::display(ostream &os,const MvDescriptor_t &descr,int level) const {
  for(int i=0;i<level;i++) os << "  ";
  display_props(os); 
  //
  os << "VECTOR(TITLE=\"" << getTitle() << "\""
     << ",KEYWORDS=("     
     << descr.getSKeyword(getIKeyword()) << ")"
     << ",TITLES=("     
     << "\"" << getName() << "\")"
     << ",DIMENSION=\"" 
     << MV_get_dimension(getDimension()) << "\")";
  return os;
}




