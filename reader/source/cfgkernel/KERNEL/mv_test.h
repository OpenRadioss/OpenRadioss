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
#ifndef MV_TEST_H
#define MV_TEST_H


#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>


#include "mv_ikeyword_containers.h" 

class MvDescriptor_t; 

/** @name Tests (check) */
//@{

/// Test types
enum MvTestType_s {
  /** */ TST_UNKNOWN,
  /** */ TST_COMPARE,
  /** */ TST_COMPARE_ATTRIBUTES, 
  /** */ TST_LAST
};
/// Test types
typedef enum MvTestType_s MvTestType_e;

/// List of messages
typedef vector<string>           MvMessageList_t;
/// Test's messages (map)
typedef map<int,MvMessageList_t> MvTestMessages_t;


/// Test class
class MvTest_t {

public: /** @name Constructors and destructor */
  //@{
  /// Constructor
  inline MvTest_t() {}
  /// Destructor
  virtual inline ~MvTest_t() {}
  //@}

public: /** @name Public methods */
  //@{
  /// Gets the type of test
  virtual inline MvTestType_e getType() const { return TST_UNKNOWN; }
  /// Ckecking the validity of the test
  virtual bool isValid(const MvIKeywordSet_t &valid_ikeywords) const=0; 
  /// Converts the test into a string
  virtual string getReport(const MvDescriptor_t *descr_p) const=0; 
  //@}

};


/// Test list
typedef vector<const MvTest_t *> MvTestList_t;


//@}


#endif //MV_TEST_H




