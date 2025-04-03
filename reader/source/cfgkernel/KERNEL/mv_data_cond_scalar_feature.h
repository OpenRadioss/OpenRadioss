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

#ifndef MV_DATA_COND_SCALAR_FEATURE_H
#define MV_DATA_COND_SCALAR_FEATURE_H

#include "mv_data_scalar_feature.h"
#include "mv_data_conditional_feature.h"
#include "mv_expression.h"
#include "mv_dimension.h"



class MvDataCondScalarFeature_t : public MvDataScalarFeature_t, public MvDataConditionalFeature_t {

 public: // Constructors & destructor
  MvDataCondScalarFeature_t(int nb_tests,const string &name,int ikeyword);
  virtual ~MvDataCondScalarFeature_t();

 public: // Creation
  // Adding a conditional dimension
  void setConditionalData(int                       i,
			  const MvDataFeatureSet_t &tests,
			  MvExpression_t           *expr_p,
			  MvDimension_e             dimension,
			  const string             &name,
              vector<string>& arg_vect);
  // Adding a default dimension
  void setDefaultData(MvDimension_e dimension,const string &name, vector<string>& arg_vect);

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
  // Gets the dimension concerned by the test for the given index
  inline MvDimension_e getConditionalDimension(int i) const { return myDimensionTab[i]; }
  // Gets the default dimension
  inline MvDimension_e getDefaultDimension() const { return myDimensionTab[myNbTests]; }
  // Gets the argument list of custom dimension by the test for the given index
  inline vector<string> &getConditionalArgVect(int i) const { return myArgVectTab[i]; }
  // Gets the argument list of custom dimension
  inline vector<string> &getDefaultArgVect() const { return myArgVectTab[myNbTests]; }
  // Gets the title concerned by the test for the given index
  inline const string &getConditionalTitle(int i) const { return myNameTab[i]; }
  // Gets the default title
  inline const string &getDefaultTitle() const { return myNameTab[myNbTests]; }
 
 protected: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const;

 private: // Data
  int                  myNbTests;
  MvExpression_t     **myExpressionPtrTab;
  MvDimension_e       *myDimensionTab;
  string              *myNameTab;
  MvDataFeatureSet_t   myTestFeatures;
  vector<string>       *myArgVectTab;
};


#endif //MV_DATA_COND_SCALAR_FEATURE_H
