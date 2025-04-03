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
#ifndef MV_DATA_ARRAY_FEATURE_H
#define MV_DATA_ARRAY_FEATURE_H

#include "mv_data_feature.h"


/// Data array feature
class MvDataArrayFeature_t : public MvDataFeature_t {

public: /** @name Constructors & destructor */
//@{
  /// Constructor
  MvDataArrayFeature_t(MvDataFeatureType_e type,const string &name,int nb_features,int size);
  /// Destructor
  virtual ~MvDataArrayFeature_t();
//@}

public: /** @name Accessors */
  //@{
  /// Gets the type of contained features (if heterogeneous array, returns DFT_UNKNOWN)
  MvDataFeatureType_e getDataFeatureType() const { return myDataFeatureType; }
  /// Gets the size (integer keyword for a dynamic array, simply the size for a static array)
  inline int getSize()   const { return mySize; }
  /// Gets the number of features
  inline int getNumber() const { return myNbFeatures; }
  /// Gets a feature
  inline const MvDataFeature_t *getDataFeature(int ind) const { return myDataFeaturePtrArray[ind]; }
  /// Sets a feature
  void setDataFeature(int ind,const MvDataFeature_t *df_p);
  //@}

protected: // Datas
  MvDataFeatureType_e     myDataFeatureType;
  int                     myNbFeatures,mySize;
  const MvDataFeature_t **myDataFeaturePtrArray;
};


#endif //MV_DATA_ARRAY_FEATURE_H




