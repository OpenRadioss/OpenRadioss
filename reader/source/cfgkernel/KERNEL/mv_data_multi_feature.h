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
#ifndef MV_DATA_MULTI_FEATURE_H
#define MV_DATA_MULTI_FEATURE_H

#include "mv_data_feature.h"


/// Data multi feature
class MvDataMultiFeature_t : public MvDataFeature_t {

public: /** @name Constructors & destructor */
//@{
  /// Constructor
  MvDataMultiFeature_t(MvDataFeatureType_e type,const string &name,int nb_features);
  /// Destructor
  virtual ~MvDataMultiFeature_t();
  /// Gets the reference integer keyword
  virtual int          getIKeyword()        const { return getIKeyword(0); }
  // Gets a name
  inline const string& getName(int ind)     const { return myNameArray[ind]; }
//@}

public:
  // Gets an integer keyword
  inline int           getIKeyword(int ind) const { return myIKeywordArray[ind]; }
protected: // Accessors
  // Gets the number of feature
  inline int           getNbFeatures()      const { return myNbFeatures; }
  // Sets a feature
  void setFeature(int ind,int ikeyword,const string &name);

private: // Datas
  int     myNbFeatures;
  int    *myIKeywordArray;
  string *myNameArray;
};


#endif //MV_DATA_MULTI_FEATURE_H




