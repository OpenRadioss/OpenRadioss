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

#ifndef MV_OPERATOR_MANAGER_H
#define MV_OPERATOR_MANAGER_H

#include <UTILS/mv_string.h>       
#include <UTILS/mv_stl_various.h>

#include "mv_operation.h"


/** @name Operator manager */
//@{


/** Array of operand types */
class MvOperandTypeArray_t : public vector<int> {

public: /** @name Constructors and destructor */
  //@{
  /// Constructor
  inline MvOperandTypeArray_t(int nb_types=0) : vector<int>(nb_types) {}
  /// Constructor
  inline MvOperandTypeArray_t(const MvOperandTypeArray_t &type_tab) : vector<int>(type_tab) {}
  /// Destructor
  virtual inline ~MvOperandTypeArray_t() {}
  //@}

public:  /** @name Other methods */
  //@{
  /// Comparison operator
  bool operator<(const MvOperandTypeArray_t &type_tab) const;
  //@}

};


/// Set of operator names
typedef set<string> MvOperatorKeywords_t;


/** Operator manager */
class MvOperatorManager_t {

public:  /** @name Constructors and destructor */
  //@{
  /// Constructor
  inline MvOperatorManager_t(bool do_delete_operators=true) :
    myDODeleteOperators(do_delete_operators), myOperators() {}
  /// Destructor
  ~MvOperatorManager_t();
  //@}

public:  /** @name Operator management */
  //@{
  /// Adds an operator
  void addOperator(const MvOperator_t *operator_p,
		   const string &skeyword,const MvOperandTypeArray_t &operand_types);
  /// Adds an operator ("..." is the list of operand types, ending by 0)
  void addOperator(const MvOperator_t *operator_p,const char *skeyword,...);
  /// Adds an unary operator
  inline void addUnaryOperator(const MvOperator_t *operator_p,
			       const string &skeyword,int operand_type)
  { 
    addOperator(operator_p,skeyword.c_str(),operand_type,0);
  }
  /// Adds a binary operator
  void addBinaryOperator(const MvOperator_t *operator_p,
			 const string &skeyword,int left_operand_type,int right_operand_type)
  { 
    addOperator(operator_p,skeyword.c_str(),left_operand_type,right_operand_type,0);
  }
  /** Gets an operator.<br>
      Returns NULL if not found
  */
  const MvOperator_t *getOperatorPtr(const string &skeyword,
				     const MvOperandTypeArray_t &operand_types) const;
  /** Gets an operator ("..." is the list of operand types, ending by 0).<br>
      Returns NULL if not found
  */
  const MvOperator_t *getOperatorPtr(const char *skeyword,...) const;
  /** Gets an unary operator.<br>
      Returns NULL if not found
  */
  inline const MvOperator_t *getUnaryOperatorPtr(const string &skeyword,
						 int operand_type) const
  {
    return getOperatorPtr(skeyword.c_str(),operand_type,0);
  }
  /** Gets a binary operator.<br>
      Returns NULL if not found
  */
  inline const MvOperator_t *getBinaryOperatorPtr(const string &skeyword,
						 int left_operand_type,
						 int right_operand_type) const
  {
    return getOperatorPtr(skeyword.c_str(),left_operand_type,right_operand_type,0);
  }
  //@}

public:  /** @name Generation of operations */
  //@{
  
  MvOperation_t *newOperation(const string        &operator_skeyword,
			      MvOperandPtrArray_t &operand_tab,
			      int                 *error_p=NULL) const;
  
  MvOperation_t *newOperation(const char *operator_skeyword,...) const;
  
  inline MvOperation_t *newUnaryOperation(const string &operator_skeyword,
					  MvOperand_t  *operand_p,
					  int          *error_p=NULL) const
  {
    return newOperation(operator_skeyword.c_str(),operand_p,NULL,error_p);
  }
  
  inline MvOperation_t *newBinaryOperation(const string &operator_skeyword,
					   MvOperand_t  *left_operand_p,
					   MvOperand_t  *right_operand_p,
					   int          *error_p=NULL) const
  {
    return newOperation(operator_skeyword.c_str(),left_operand_p,right_operand_p,NULL,error_p);
  }
  //@}

public:  /** @name Searching operators */
  //@{
  
  /// Getting all operators
  MvOperatorKeywords_t *getAllOperators(MvOperatorKeywords_t *operators_p=NULL) const;
  
  /// Searching operators
  MvOperatorKeywords_t *searchOperators(MvOperandPtrArray_t  &operand_tab,
					MvOperatorKeywords_t *operators_p=NULL) const;
  /// Searching unary operators
  MvOperatorKeywords_t *searchOperators(MvOperand_t          *operand_p,
					MvOperatorKeywords_t *operators_p=NULL) const;
  /// Searching binary operators
  MvOperatorKeywords_t *searchOperators(MvOperand_t          *left_operand_p,
					MvOperand_t          *right_operand_p,
					MvOperatorKeywords_t *operators_p=NULL) const;
  //@}


private: // Types
  typedef map<MvOperandTypeArray_t,const MvOperator_t *> MyOperandTypes2Operator_t;
  typedef map<string,MyOperandTypes2Operator_t>          MyOperatorMap_t;

private: // Data
  bool            myDODeleteOperators;
  MyOperatorMap_t myOperators;

};


//@}


#endif //MV_OPERATOR_MANAGER_H




