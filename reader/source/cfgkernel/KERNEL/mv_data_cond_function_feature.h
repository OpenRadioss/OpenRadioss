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

#ifndef MV_DATA_COND_FUNCTION_FEATURE_H
#define MV_DATA_COND_FUNCTION_FEATURE_H

#include "mv_dimension.h"
#include "mv_data_function_feature.h"
#include "mv_data_conditional_feature.h" 


class MvDataCondFunctionFeature_t : public MvDataFunctionFeature_t, public MvDataConditionalFeature_t { 

 public: // Constructors & destructor
  
  // Constructor
  MvDataCondFunctionFeature_t(const string &title,int nb_tests,int func_ikw);
  
  // Constructor
  MvDataCondFunctionFeature_t(const string &title,int nb_tests,
			      int func_ikw,int x_scal_ikw,int y_scal_ikw, int z_scal_ikw);
  
  
  // Destructor
  virtual ~MvDataCondFunctionFeature_t();
  //@}

 public: // Creation
  
  
  // Adding a conditional dimension
  void setConditionalData(int                       i,
              const MvDataFeatureSet_t &tests,
              MvExpression_t           *expr_p,
              const string             &func_name,
              const string             &func_xtitle,
              const string             &func_ytitle,
              const string             &func_ztitle,
              vector<string>            &x_arg_vect,
              vector<string>            &y_arg_vect,
              vector<string>            &z_arg_vect,
              MvDimension_e             func_xdimension,
              MvDimension_e             func_ydimension,
              MvDimension_e             func_zdimension,
              const string             &x_scaling_name,
              const string             &y_scaling_name,
              const string             &z_scaling_name,
              const string             &sub_func_xtitle="",
              const string             &sub_func_ytitle="",
              const string             &sub_func_ztitle = "",
              MvDimension_e             sub_func_xdimension=UDI_UNKNOWN,
              MvDimension_e             sub_func_ydimension=UDI_UNKNOWN,
              MvDimension_e             sub_func_zdimension=UDI_UNKNOWN,
              vector<string>* sub_x_arg_vect = NULL,
              vector<string>* sub_y_arg_vect = NULL,
              vector<string>* sub_z_arg_vect = NULL);
  // Adding a default dimension
  void setDefaultData(const string  &func_name,
              const string  &func_xtitle,
              const string  &func_ytitle,
              const string  &func_ztitle,
              vector<string>& x_arg_vect,
              vector<string>& y_arg_vect,
              vector<string>& z_arg_vect,
              MvDimension_e  func_xdimension,
              MvDimension_e  func_ydimension,
              MvDimension_e  func_zdimension,
              const string  &x_scaling_name,
              const string  &y_scaling_name,
              const string  &z_scaling_name,
              const string  &sub_func_xtitle="",
              const string  &sub_func_ytitle="",
              const string  &sub_func_ztitle = "",
              MvDimension_e  sub_func_xdimension = UDI_UNKNOWN,
              MvDimension_e  sub_func_ydimension = UDI_UNKNOWN,
              MvDimension_e  sub_func_zdimension = UDI_UNKNOWN,
              vector<string>* sub_x_arg_vect = NULL,
              vector<string>* sub_y_arg_vect = NULL,
              vector<string>* sub_z_arg_vect = NULL);
  
  

 public: // Consulting (high level)
  
  // Gets the test features
  inline const MvDataFeatureSet_t &getTestFeatures() const { return myTestFeatures; }
  /// Gets the condition features
  virtual MvDataFeatureSet_t *getConditionFeatures(MvDataFeatureSet_t *cond_features_p=NULL) const;
  

 public: // Consulting (low level)
  // Gets the number of tests
  inline int getNbTests() const { return myNbTests; }
  /// Gets the test for the given index
  inline const MvExpression_t *getExpressionPtr(int i) const { return myExpressionPtrTab[i]; }
  // Gets the name concerned by the test of given index
  inline const string &getConditionalFunctionName(int i)       const { return myNameTab[i]; }
  // Gets the default name
  inline const string &getDefaultFunctionName()                const { return myNameTab[myNbTests]; }
  // Gets the title of the X-values concerned by the test of given index
  inline const string &getConditionalFunctionXTitle(int i)     const { return myXTitleTab[i]; }
  // Gets the default title of the X-values
  inline const string &getDefaultFunctionXTitle()              const { return myXTitleTab[myNbTests]; }
  // Gets the title of the Y-values concerned by the test of given index
  inline const string &getConditionalFunctionYTitle(int i)     const { return myYTitleTab[i]; }
  // Gets the title of the Z-values concerned by the test of given index
  inline const string& getConditionalFunctionZTitle(int i)     const { return myZTitleTab[i]; }
  // Gets the default title of the Y-values
  inline const string &getDefaultFunctionYTitle()              const { return myYTitleTab[myNbTests]; }
  // Gets the default title of the Z-values
  inline const string& getDefaultFunctionZTitle()              const { return myZTitleTab[myNbTests]; }
  // Gets the dimension of the X-values concerned by the test of given index
  inline MvDimension_e getConditionalFunctionXDimension(int i) const { return myXDimensionTab[i]; }
  // Gets the default dimension of the X-values
  inline MvDimension_e getDefaultFunctionXDimension()          const { return myXDimensionTab[myNbTests]; }
  // Gets the dimension of the Y-values concerned by the test of given index
  inline MvDimension_e getConditionalFunctionYDimension(int i) const { return myYDimensionTab[i]; }
  // Gets the default dimension of the Y-values
  inline MvDimension_e getDefaultFunctionYDimension()          const { return myYDimensionTab[myNbTests]; }
  // Gets the dimension of the Z-values concerned by the test of given index
  inline MvDimension_e getConditionalFunctionZDimension(int i) const { return myZDimensionTab[i]; }
  // Gets the default dimension of the Z-values
  inline MvDimension_e getDefaultFunctionZDimension()          const { return myZDimensionTab[myNbTests]; }
  // Gets the argument list of custom dimension of the X-values concerned by the test of given index
  inline vector<string>& getConditionalFunctionXArgVect(int i) const { return myXArgVectTab[i]; }
  //  Gets the argument list of the default custom dimension of the X-values
  inline vector<string>& getDefaultFunctionXArgVect()          const { return myXArgVectTab[myNbTests]; }
  // Gets the argument list of custom dimension of the X-values concerned by the test of given index
  inline vector<string>& getConditionalFunctionYArgVect(int i) const { return myYArgVectTab[i]; }
  //  Gets the argument list of the default custom dimension of the X-values
  inline vector<string>& getDefaultFunctionYArgVect()          const { return myYArgVectTab[myNbTests]; }
  // Gets the argument list of custom dimension of the X-values concerned by the test of given index
  inline vector<string>& getConditionalFunctionZArgVect(int i) const { return myZArgVectTab[i]; }
  //  Gets the argument list of the default custom dimension of the X-values
  inline vector<string>& getDefaultFunctionZArgVect()          const { return myZArgVectTab[myNbTests]; }
  
  // Gets the name of the X-scaling factor concerned by the test of given index
  inline const string &getConditionalXScalingName(int i)        const { return myXScalingNameTab[i]; }
  // Gets the default name of the X-scaling factor
  inline const string &getDefaultXScalingName()                 const { return myXScalingNameTab[myNbTests]; }
  // Gets the name of the Y-scaling factor concerned by the test of given index
  inline const string &getConditionalYScalingName(int i)        const { return myYScalingNameTab[i]; }
  // Gets the default name of the Y-scaling factor
  inline const string &getDefaultYScalingName()                 const { return myYScalingNameTab[myNbTests]; }
  // Gets the name of the Z-scaling factor concerned by the test of given index
  inline const string& getConditionalZScalingName(int i)        const { return myZScalingNameTab[i]; }
  // Gets the default name of the Z-scaling factor
  inline const string& getDefaultZScalingName()                 const { return myZScalingNameTab[myNbTests]; }
  // Gets the name of the Y-scaling factor concerned by the test of given index
  inline const string &getConditionalScalingName(int i)        const { return getConditionalYScalingName(i); }
  // Gets the default name of the Y-scaling factor
  inline const string &getDefaultScalingName()                 const { return getDefaultYScalingName(); }
  
  
  // Gets the title of the X-values concerned by the test of given index for sub-functions
  inline const string &getConditionalSubFunctionXTitle(int i)     const { return mySubXTitleTab[i]; }
  // Gets the default title of the X-values for sub-fonctions
  inline const string &getDefaultSubFunctionXTitle()              const { return mySubXTitleTab[myNbTests]; }
  // Gets the title of the Y-values concerned by the test of given index for sub-functions
  inline const string &getConditionalSubFunctionYTitle(int i)     const { return mySubYTitleTab[i]; }
  // Gets the default title of the Y-values for sub-fonctions
  inline const string &getDefaultSubFunctionYTitle()              const { return mySubYTitleTab[myNbTests]; }
  // Gets the title of the Z-values concerned by the test of given index for sub-functions
  inline const string& getConditionalSubFunctionZTitle(int i)     const { return mySubZTitleTab[i]; }
  // Gets the default title of the Z-values for sub-fonctions
  inline const string& getDefaultSubFunctionZTitle()              const { return mySubZTitleTab[myNbTests]; }
  // Gets the dimension of the X-values concerned by the test of given index for sub-functions
  inline MvDimension_e getConditionalSubFunctionXDimension(int i) const { return mySubXDimensionTab[i]; }
  // Gets the default dimension of the X-values for sub-functions
  inline MvDimension_e getDefaultSubFunctionXDimension()          const { return mySubXDimensionTab[myNbTests]; }
  // Gets the dimension of the Y-values concerned by the test of given index for sub-functions
  inline MvDimension_e getConditionalSubFunctionYDimension(int i) const { return mySubYDimensionTab[i]; }
  // Gets the dimension of the Z-values concerned by the test of given index for sub-functions
  inline MvDimension_e getConditionalSubFunctionZDimension(int i) const { return mySubZDimensionTab[i]; }
  // Gets the default dimension of the Y-values for sub-functions
  inline MvDimension_e getDefaultSubFunctionYDimension()          const { return mySubYDimensionTab[myNbTests]; }
  // Gets the default dimension of the Z-values for sub-functions
  inline MvDimension_e getDefaultSubFunctionZDimension()          const { return mySubZDimensionTab[myNbTests]; }
  // Gets the argument list of custom dimension of the X-values concerned by the test of given index for sub-functions
  inline vector<string>& getConditionalSubFunctionXArgVect(int i) const { return mySubXArgVectTab[i]; }
  // Gets the argument list of custom default dimension of the X-values for sub-functions
  inline vector<string>& getDefaultSubFunctionXArgVect()          const { return mySubXArgVectTab[myNbTests]; }
  // Gets the argument list of custom dimension of the Y-values concerned by the test of given index for sub-functions
  inline vector<string>& getConditionalSubFunctionYArgVect(int i) const { return mySubYArgVectTab[i]; }
  // Gets the argument list of custom default dimension of the Y-values for sub-functions
  inline vector<string>& getDefaultSubFunctionYArgVect()          const { return mySubYArgVectTab[myNbTests]; }
  // Gets the argument list of custom dimension of the Z-values concerned by the test of given index for sub-functions
  inline vector<string>& getConditionalSubFunctionZArgVect(int i) const { return mySubZArgVectTab[i]; }
  // Gets the argument list of custom default dimension of the Z-values for sub-functions
  inline vector<string>& getDefaultSubFunctionZArgVect()          const { return mySubZArgVectTab[myNbTests]; }
  

protected: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const;

protected: // Data
  int                  myNbTests;
  MvExpression_t     **myExpressionPtrTab;
  string              *myNameTab,*myXScalingNameTab,*myYScalingNameTab,*myZScalingNameTab;                          
  string              *myXTitleTab,*myYTitleTab,*myZTitleTab,*mySubXTitleTab,*mySubYTitleTab,*mySubZTitleTab;                 
  MvDimension_e       *myXDimensionTab,*myYDimensionTab,*myZDimensionTab,*mySubXDimensionTab,*mySubYDimensionTab,*mySubZDimensionTab; 
  MvDataFeatureSet_t   myTestFeatures;
  vector<string>      *myXArgVectTab, * myYArgVectTab, * myZArgVectTab, * mySubXArgVectTab, * mySubYArgVectTab, * mySubZArgVectTab;
};


#endif //MV_DATA_COND_FUNCTION_FEATURE_H
