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
#include <UTILS/error.h>



#include "mv_utils.h"
#include "mv_data_multi_feature.h"


/* --------- Constructors & destructor --------- */

MvDataMultiFeature_t::MvDataMultiFeature_t(MvDataFeatureType_e type,const string &name,int nb_features) :
  MvDataFeature_t(type,name),
  myNbFeatures(nb_features>0 ? nb_features : 0),
  myIKeywordArray(myNbFeatures>0 ? new int[myNbFeatures] : NULL),
  myNameArray(myNbFeatures>0 ? new string[myNbFeatures] : NULL)
{
  for(int i=0;i<myNbFeatures;i++) {
    myIKeywordArray[i] = END_ARGS;
    myNameArray[i]     = "No name";
  }
}

MvDataMultiFeature_t::~MvDataMultiFeature_t() {
  if(myIKeywordArray!=NULL) delete [] myIKeywordArray;
  if(myNameArray!=NULL)     delete [] myNameArray;
}


/* --------- Accessors --------- */

void MvDataMultiFeature_t::setFeature(int ind,int ikeyword,const string &name) {
  // Error management
  if(ind<0 || ind>=myNbFeatures) {
    throw MvError_t("MvDataMultiFeature_t::setFeature -> index %i out of bounds",ind);
  }
  // Setting feature
  myIKeywordArray[ind] = ikeyword;
  myNameArray[ind]     = name;
}




