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
#ifndef MV_DATA_IF_FEATURE_H
#define MV_DATA_IF_FEATURE_H

#include "mv_data_feature.h"
#include "mv_expression.h"



/// Data "if" feature
class MvDataIfFeature_t : public MvDataFeature_t {

public: /** @name Constructors & destructor */
  //@{
  /// Constructor
  MvDataIfFeature_t(int nb_tests);
  /// Destructor
  virtual ~MvDataIfFeature_t();
  //@}

public: /** @name Creation */
  //@{
  
  /// Adding features attached to a test
  void setTest(const MvDescriptor_t *descr_p,int i,
           const MvDataFeatureSet_t &tests,MvExpression_t *expr_p,const MvDataFeatureList_t &ftl, const MvDataFeatureList_t& ftl_reduced);
  
  /// Adding features attached to the final "ELSE" (default)
  void setDefault(const MvDataFeatureList_t &ftl);
  /// Adding reduced features attached to the final "ELSE" (default)
  void setDefaultReduced(const MvDataFeatureList_t& ftl);
  /// Updates wrong features lists
  void updateWrongFeaturePtrLists();
  /// sets the prev feature in if feature in GUI block
  void setPrevFeature(MvDataFeature_t *feature) { myPrevFeature = feature; }
  /// gets the prev feature
  MvDataFeature_t *getPrevFeature() { return myPrevFeature; }
  //@}

public: /** @name Consulting (high level) */
  //@{
  /// Gets the test features
  inline const MvDataFeatureSet_t &getTestFeatures() const { return myTestFeatures; }
  //@}
  
public: /** @name Consulting (low level) */
  //@{
  /// Gets the number of tests
  inline int getNbTests() const { return myNbTests; }
  /// Gets the test for the given index
  inline const MvExpression_t *getExpressionPtr(int i) const { return myExpressionPtrArray[i]; }
  /// Gets the feature list concerned by the test for the given index
  inline const MvDataFeatureList_t &getFeaturePtrList(int i) const { return myFeaturePtrListArray[i]; }
  /// Gets the wrong feature list concerned by the test for the given index
  inline const MvDataFeatureList_t &getWrongFeaturePtrList(int i) const { return myWrongFeaturePtrListArray[i]; }
  /// Gets the reduced feature list concerned by the test for the given index
  inline const MvDataFeatureList_t& getFeaturePtrReducedList(int i) const { return myFeaturePtrReducedListArray[i]; }
  /// Gets the reduced wrong feature list concerned by the test for the given index
  inline const MvDataFeatureList_t& getWrongFeaturePtrReducedList(int i) const { return myWrongFeaturePtrReducedListArray[i]; }
  /// Gets the default feature list (final "else")
  inline const MvDataFeatureList_t &getDefaultFeaturePtrList() const { return myFeaturePtrListArray[myNbTests]; }
  /// Gets the default feature list (final "else")
  inline const MvDataFeatureList_t& getDefaultFeaturePtrReducedList() const { return myFeaturePtrReducedListArray[myNbTests]; }
  /// Gets the default wrong feature list (final "else")
  inline const MvDataFeatureList_t &getDefaultWrongFeaturePtrList() const { 
    return myWrongFeaturePtrListArray[myNbTests]; 
  }
  /// Gets the default wrong feature list (final "else")
  inline const MvDataFeatureList_t& getDefaultWrongFeaturePtrReducedList() const {
      return myWrongFeaturePtrReducedListArray[myNbTests];
  }
  bool getAllFeatures(MvDataFeatureSet_t& allFeaturesLst)
  {
      if (myNbTests <= 0)
          return false;
      MvDataFeatureList_t& a_right_fl = myFeaturePtrListArray[0];
      MvDataFeatureList_t::iterator a_it_begin = a_right_fl.begin();
      MvDataFeatureList_t::iterator a_it_end = a_right_fl.end();
      MvDataFeatureList_t::iterator a_it;
      for (a_it = a_it_begin; a_it != a_it_end; ++a_it)
          allFeaturesLst.insert(*a_it);

      MvDataFeatureList_t& a_wrong_fl = myWrongFeaturePtrListArray[0];
      MvDataFeatureList_t::iterator a_it_begin1 = a_wrong_fl.begin();
      MvDataFeatureList_t::iterator a_it_end1 = a_wrong_fl.end();
      MvDataFeatureList_t::iterator a_it1;
      for (a_it1 = a_it_begin1; a_it1 != a_it_end1; ++a_it1)
          allFeaturesLst.insert(*a_it1);
      return true;
  }
  //@}

protected: // Output in an output stream
  virtual ostream &display(ostream &os,const MvDescriptor_t &descr,int level=0) const;

protected: // Datas
  int                     myNbTests;
  MvExpression_t        **myExpressionPtrArray;
  MvDataFeatureList_t    *myFeaturePtrListArray;
  MvDataFeatureList_t    *myFeaturePtrReducedListArray;
  MvDataFeatureList_t    *myWrongFeaturePtrListArray;
  MvDataFeatureList_t    *myWrongFeaturePtrReducedListArray;
  MvDataFeatureSet_t      myTestFeatures;
  MvDataFeature_t        *myPrevFeature;

};


#endif //MV_DATA_IF_FEATURE_H




