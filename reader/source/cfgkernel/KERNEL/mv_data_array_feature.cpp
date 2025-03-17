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
#include "mv_data_if_feature.h"
#include "mv_data_array_feature.h"


/* --------- Constructors & destructor --------- */

typedef const MvDataFeature_t *MvConstDataFeaturePtr_t;

MvDataArrayFeature_t::MvDataArrayFeature_t(MvDataFeatureType_e type,
					   const string &name,int nb_features,int size) :
  MvDataFeature_t(type,name),
  myDataFeatureType(DFT_UNKNOWN),
  myNbFeatures(nb_features>0 ? nb_features : 0),
  mySize(size),
  myDataFeaturePtrArray(myNbFeatures==0 ? NULL : new MvConstDataFeaturePtr_t[myNbFeatures])
{}

MvDataArrayFeature_t::~MvDataArrayFeature_t() {
  if(myDataFeaturePtrArray!=NULL) {
      for (int i = 0; i < myNbFeatures; ++i)
      {
          if (myDataFeaturePtrArray[i]->getType() == DFT_IF)
          {
              MvDataIfFeature_t* a_feature = (MvDataIfFeature_t*)myDataFeaturePtrArray[i];
              MvDataFeatureSet_t allFeaturesLst;
              a_feature->getAllFeatures(allFeaturesLst);
              MvDataFeatureSet_t::iterator a_it_begin = allFeaturesLst.begin();
              MvDataFeatureSet_t::iterator a_it_end = allFeaturesLst.end();
              MvDataFeatureSet_t::iterator a_it;
              for (a_it = a_it_begin; a_it != a_it_end; ++a_it)
              {
                 delete (*a_it);
              }
          }
          delete myDataFeaturePtrArray[i];
      }
    delete [] myDataFeaturePtrArray;
  }
}


/* --------- Accessors --------- */

void MvDataArrayFeature_t::setDataFeature(int ind,const MvDataFeature_t *df_p) {
  if(myDataFeatureType==DFT_UNKNOWN && df_p!=NULL) myDataFeatureType=df_p->getType();
  myDataFeaturePtrArray[ind]=df_p;
}
