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
#ifndef MV_DATA_FUNCTION_FEATURE_H
#define MV_DATA_FUNCTION_FEATURE_H

#include "mv_dimension.h"
#include "mv_function_types.h"
#include "mv_data_multi_feature.h"
#include "mv_filter.h"


/// Data function feature
class MvDataFunctionFeature_t : public MvDataMultiFeature_t {

 public: /** @name Constructors & destructor */
  //@{
  
  
  /// Constructor
  MvDataFunctionFeature_t(MvDataFeatureType_e dft,const string &title,
			  int func_ikw,int x_scal_ikw=END_ARGS,int y_scal_ikw=END_ARGS,int z_scal_ikw=END_ARGS);
  
  
  /// Destructor
  virtual ~MvDataFunctionFeature_t();
  //@}
  
 public: /** @name Function accessors */
  //@{
  /// Gets the integer keyword
  inline int            getFunctionIKeyword() const { return getIKeyword(0); }
  
  
  //@}

 public: /** @name Scaling factor accessors */
  //@{
  
  /// Returns true if a X-scaling factor is present
  inline bool           isXScaled()              const { return myXScaleInd>0; }
  /// Gets the integer keyword of the X-scaling factor
  inline int            getXScalingIKeyword() const { return getIKeyword(myXScaleInd); }
  /// Returns true if a Y-scaling factor is present
  inline bool           isYScaled()              const { return myYScaleInd>0; }
  /// Gets the integer keyword of the Y-scaling factor
  inline int            getYScalingIKeyword() const { return getIKeyword(myYScaleInd); }
  /// Returns true if a Z-scaling factor is present
  inline bool           isZScaled()              const { return myZScaleInd > 0; }
  /// Gets the integer keyword of the Z-scaling factor
  inline int            getZScalingIKeyword() const { return getIKeyword(myZScaleInd); }
  /// Returns true if a Y-scaling factor is present
  inline bool           isScaled()               const { return isYScaled(); }
  /// Gets the integer keyword of the Y-scaling factor
  inline int            getScalingIKeyword()  const { return getYScalingIKeyword(); }
  
  
  
  //@}

  
 public:
  /// Sets the type of function
  inline void             setFunctionType(MvFunctionType_e func_type) { myFuncType=func_type; }
  /// Gets the type of function
  inline MvFunctionType_e getFunctionType() const { return myFuncType; }
  
  /** Sets the X values can be -ve */
  void setXCanBeNegative(bool status) {myXCanBeNegative = status;}
  /** Gets the X values can ve -ve */
  bool getXCanBeNegative() {return myXCanBeNegative;}
  /** Set filter information*/
  void createFilter(int nb, StringVect_t &attribs, StringVect_t &values, StringVect_t &criterias, StringVect_t &units, StringVect_t& messgs);
  /** Get filter information*/
  EntityFilter *getFilter() { return myFilter; }
 protected: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const=0;

  
 protected:
  int              myXScaleInd;
  int              myYScaleInd;
  int              myZScaleInd;
  MvFunctionType_e myFuncType;  
  
  bool             myXCanBeNegative;
  EntityFilter    *myFilter;
};



#endif //MV_DATA_FUNCTION_FEATURE_H




