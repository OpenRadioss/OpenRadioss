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

#ifndef MV_DATA_ARRAY_TO_SINGLE_FEATURE_H
#define MV_DATA_ARRAY_TO_SINGLE_FEATURE_H

#include "mv_data_array_feature.h"


/// Data array to single feature
class MvDataArrayToSingleFeature_t : public MvDataArrayFeature_t {

public: /** @name Constructors & destructor */
//@{
  /// Constructor
  MvDataArrayToSingleFeature_t(const string &name,int nb_features,int index);
  /// Destructor
  virtual ~MvDataArrayToSingleFeature_t();
//@}

public: /** @name Access to the size feature */
//@{
  /// Gets the index of the array
  inline int getIndex() const { return myIndex; }
//@}

protected: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const;

protected: // Datas
  int myIndex;

};


#endif //MV_DATA_ARRAY_TO_SINGLE_FEATURE_H




