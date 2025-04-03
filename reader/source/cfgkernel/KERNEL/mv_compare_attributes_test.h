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
#ifndef MV_COMPARE_ATTRIBUTES_TEST_H
#define MV_COMPARE_ATTRIBUTES_TEST_H

#include "mv_test.h"


/// Comparison test class
class MvCompareAttributesTest_t : public MvTest_t {

public: /** @name Constructors and destructor */
  //@{
  /// Constructor
  MvCompareAttributesTest_t(int left_ikw,const string &comp,int right_ikw);
  //@}

public: /** @name MvTest_t redefined public methods */
  //@{
  /// Gets the type of test
  virtual inline MvTestType_e getType() const { return TST_COMPARE_ATTRIBUTES; }
  /// Ckecking the validity of the test
  virtual bool isValid(const MvIKeywordSet_t &valid_ikeywords) const;
  /// Converts the test into a string
  virtual string getReport(const MvDescriptor_t *descr_p) const;
  //@}

public: /** @name Access to data */
  //@{
  /// Gets the integer keyword of the considered attribute
  inline int           getLeftIKeyword()  const { return myLeftIKeyword; }
  /// Gets the integer keyword of the considered attribute
  inline const string &getComparator()    const { return myComparator; }
  /// Gets the integer keyword of the considered attribute
  inline int           getRightIKeyword() const { return myRightIKeyword; }
  //@}



protected:
  int    myLeftIKeyword,myRightIKeyword;
  string myComparator;
};


#endif //MV_COMPARE_ATTRIBUTES_TEST_H




