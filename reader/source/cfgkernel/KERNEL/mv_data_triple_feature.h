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
#ifndef MV_DATA_TRIPLE_FEATURE_H
#define MV_DATA_TRIPLE_FEATURE_H

#include "mv_data_dimension_feature.h"
#include "mv_data_multi_feature.h"



/// Enum for Point feature types
enum MvDataTripleFeatureType_s {
	/** */ DTP_TYPE_UNKNOWN,
	/** */ DTP_TYPE_POSITION  = 2,   //! Couple or triple is a position
	/** */ DTP_TYPE_DIRECTION = 3,   //! Couple or triple is a direction
	/** */ DTP_TYPE_VECTOR    = 4,   //! Couple or triple is a direction, with a starting point (base)
	/** */ DTP_TYPE_RGB       = 15,  //! Int triple used for RGB color value
	/** */ DTP_TYPE_BASE,
	/** */ DTP_TYPE_RELATIVE,

	/** */ DTP_TYPE_LAST
};
/// Type flag for point data feature
typedef enum MvDataTripleFeatureType_s MvDataTripleFeatureType_e;
/// Data vector feature
class MvDataTripleFeature_t : public MvDataFeature_t, public MvDataDimensionFeature_t {

public: /** @name Constructors & destructor */
  //@{
  /// Constructor
  MvDataTripleFeature_t(const string &name,
            int ikw, const string& title,
            MvDimension_e dimension = UDI_DIMENSIONLESS,
            vector<string>* argVect = NULL,
            MvDataTripleFeatureType_e type = DTP_TYPE_BASE);
  /// Destructor
  virtual ~MvDataTripleFeature_t();
  //@}

public: /** @name Accessors */
  //@{
  /// Gets the reference integer keyword
  virtual int getIKeyword() const { return myIkeyword; }
  /// Gets the reference integer title
  inline const string &getName() const { return myTitle; }
  /// Gets the 
  virtual  MvDataTripleFeatureType_e getDifferentiatorType() const { return myPointTypeFlag; }
  //@}

protected: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const;
protected:
	MvDataTripleFeatureType_e myPointTypeFlag;

private:
    int      myIkeyword;
    string   myTitle;
};


#endif //MV_DATA_TRIPLE_FEATURE_H




