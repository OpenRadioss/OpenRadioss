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

//

#ifndef MV_DATA_UNCOND_FUNCTION_FEATURE_H
#define MV_DATA_UNCOND_FUNCTION_FEATURE_H

#include "mv_dimension.h"
#include "mv_data_function_feature.h"

class MvDataUncondFunctionFeature_t : public MvDataFunctionFeature_t {

 public: // Constructors & destructor
  
  // Constructor
  
  MvDataUncondFunctionFeature_t(const string &title,int func_ikw,const string &func_name,
                string x_name,MvDimension_e x_dim, vector<string>& x_arg_vect,
                string y_name,MvDimension_e y_dim, vector<string>& y_arg_vect,
                string z_name,MvDimension_e z_dim, vector<string>& z_arg_vect,
                string sub_x_name="",MvDimension_e sub_x_dim=UDI_UNKNOWN, vector<string>* sub_x_arg_vect=NULL,
                string sub_y_name="",MvDimension_e sub_y_dim=UDI_UNKNOWN, vector<string>* sub_y_arg_vect=NULL,
                string sub_z_name="",MvDimension_e sub_z_dim=UDI_UNKNOWN, vector<string>* sub_z_arg_vect=NULL);
  
  
  // Constructor
  MvDataUncondFunctionFeature_t(const string &title,int func_ikw,const string &func_name,
                string x_name, MvDimension_e x_dim, vector<string>& x_arg_vect,
                string y_name, MvDimension_e y_dim, vector<string>& y_arg_vect,
                string z_name, MvDimension_e z_dim, vector<string>& z_arg_vect,
                int x_scal_ikw,const string &x_scal_name,
                int y_scal_ikw,const string &y_scal_name,
                int z_scal_ikw,const string&z_scal_name);
  
  
  // Destructor
  virtual ~MvDataUncondFunctionFeature_t();

 public: // Function accessors
  // Gets the name
  virtual inline const string &getFunctionName()       const { return getName(0); }
  // Gets the title of the X-values
  virtual inline const string &getFunctionXTitle()     const { return myXTitle; }
  // Gets the title of the Y-values
  virtual inline const string &getFunctionYTitle()     const { return myYTitle; }
  // Gets the dimension of the X-values
  inline MvDimension_e getFunctionXDimension() const { return myXDimension; }
  // Gets the dimension of the Y-values
  inline MvDimension_e getFunctionYDimension() const { return myYDimension; }
  // Gets the dimension of the Z-values
  inline MvDimension_e getFunctionZDimension() const { return myZDimension; }
  // Gets the argument list of custom dimension of the X-values
  inline const vector<string> &getFunctionXArgVect() const { return myXArgVect; }
  // Gets the argument list of custom dimension of the Y-values
  inline const vector<string> &getFunctionYArgVect() const { return myYArgVect; }
  // Gets the argument list of custom dimension of the Z-values
  inline const vector<string> &getFunctionZArgVect() const { return myZArgVect; }
  
  // Gets the name of the X-scaling factor
  inline const string &getXScalingName() const { return getName(myXScaleInd); }
  // Gets the name of the Y-scaling factor
  inline const string &getYScalingName() const { return getName(myYScaleInd); }
  // Gets the name of the Z-scaling factor
  inline const string& getZScalingName() const { return getName(myZScaleInd); }
  // Gets the name of the Y-scaling factor
  inline const string &getScalingName() const { return getName(myYScaleInd); }
  /// Gets the title of X-values of sub-functions
  virtual inline const string& getSubFunctionXTitle()     const { return mySubXTitle; }
  /// Gets the title of Y-values of sub-functions
  virtual inline const string& getSubFunctionYTitle()     const { return mySubYTitle; }
  /// Gets the title of Z-values of sub-functions
  virtual inline const string& getSubFunctionZTitle()     const { return mySubZTitle; }
  /// Gets the dimension of the X-values of sub-functions
  virtual inline MvDimension_e getSubFunctionXDimension() const { return mySubXDimension; }
  /// Gets the dimension of the Y-values of sub-functions
  virtual inline MvDimension_e getSubFunctionYDimension() const { return mySubYDimension; }
  /// Gets the dimension of the Z-values of sub-functions
  virtual inline MvDimension_e getSubFunctionZDimension() const { return mySubZDimension; }
  // Gets the argument list of custom dimension of the X-values of sub-functions
  inline const vector<string>& getSubFunctionXArgVect() const { return mySubXArgVect; }
  // Gets the argument list of custom dimension of the Y-values of sub-functions
  inline const vector<string>& getSubFunctionYArgVect() const { return mySubYArgVect; }
  // Gets the argument list of custom dimension of the Z-values of sub-functions
  inline const vector<string>& getSubFunctionZArgVect() const { return mySubZArgVect; }
 
 

 public: // Scaling factor accessors

  
 public:
  /// Sets the type of function
  inline void             setFunctionType(MvFunctionType_e func_type) { myFuncType=func_type; }
  /// Gets the type of function
  inline MvFunctionType_e getFunctionType() const { return myFuncType; }
  

 protected: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const;

 protected: // Data
  string        myXTitle,myYTitle,myZTitle;
  MvDimension_e myXDimension,myYDimension,myZDimension;
  string        mySubXTitle,mySubYTitle,mySubZTitle;         
  MvDimension_e mySubXDimension,mySubYDimension,mySubZDimension; 
  vector<string> myXArgVect, myYArgVect, myZArgVect;
  vector<string> mySubXArgVect, mySubYArgVect, mySubZArgVect;

};


#endif //MV_DATA_UNCOND_FUNCTION_FEATURE_H
