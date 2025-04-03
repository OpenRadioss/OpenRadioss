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
#ifndef MV_DATA_POINT_FEATURE_H
#define MV_DATA_POINT_FEATURE_H

#include "mv_data_triple_feature.h"





/// Data vector feature
class MvDataPointFeature_t : public MvDataTripleFeature_t {

public: /** @name Constructors & destructor */
  //@{
  /// Constructor
  MvDataPointFeature_t(const string &name,
		       int ikw,const string &title,
		       MvDimension_e dimension=UDI_DIMENSIONLESS,
               vector<string>* argVect=NULL,
               MvDataTripleFeatureType_e type=DTP_TYPE_BASE);
  /// Destructor
  virtual ~MvDataPointFeature_t();

public:
  /// Is the point type is Base or Relative
 // inline MvDataTripleFeatureType_e getPointType() const { return myPointTypeFlag; }

private:
//  MvDataTripleFeatureType_e myPointTypeFlag;
  //@}

protected: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const;

};


#endif //MV_DATA_POINT_FEATURE_H




