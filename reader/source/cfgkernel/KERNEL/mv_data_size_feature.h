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
#ifndef MV_DATA_SIZE_FEATURE_H
#define MV_DATA_SIZE_FEATURE_H

#include "mv_data_dimension_feature.h"
#include "mv_data_dynamic_array_feature.h"
#include "mv_data_single_feature.h"


/// Data size feature
class MvDataSizeFeature_t : public MvDataSingleFeature_t {

public: /** @name Constructors & destructor */
//@{
  /// Constructor
  MvDataSizeFeature_t(const string &name,int ikeyword);
  /// Destructor
  virtual ~MvDataSizeFeature_t();
//@}

public: /** @name Associated dynamic arrays */
//@{
  /// Adds a dynamic array
  void addArray(MvDataDynamicArrayFeature_t *daf_p);
  /// Gets the number of dynamic arrays
  inline int getNbArrays() const { return myNbArrays; }
  /// Gets the number of dynamic arrays
  inline MvDataDynamicArrayFeature_t *getArrayPtr(int ind) const { return myArrayTab[ind]; }
//@}

public:
//@{
  /// Sets the number of dynamic arrays
  void setNbArrays(int value) {myNbArrays =  value;}
  /// Initializes the array ptr
  void setArrayPtr(MvDataDynamicArrayFeature_t **value) {myArrayTab = value;}
  /// Gets the size radio feature flag
  virtual bool getIsSizeRadioFeature() {return false;}
//@}
protected: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const;

private:
  int                           myNbArrays;
  MvDataDynamicArrayFeature_t **myArrayTab;

};

#endif //MV_DATA_SIZE_FEATURE_H




