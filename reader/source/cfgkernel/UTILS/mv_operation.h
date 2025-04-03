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

#ifndef MV_OPERATION_H
#define MV_OPERATION_H


#include "mv_operator.h"


/** @name Operation */
//@{


/// Operation
class MvOperation_t : public MvOperand_t {

public:  /** @name Constructors and destructor */
  //@{
  /// Constructor
  MvOperation_t(const MvOperator_t *operator_p,const MvOperandPtrArray_t &operands);
  /// Constructor of unary operation
  MvOperation_t(const MvOperator_t *unary_operator_p,MvOperand_t *operand_p);
  /// Constructor of binary operation
  MvOperation_t(const MvOperator_t *binary_operator_p,
		MvOperand_t *left_operand_p,MvOperand_t *right_operand_p);
  /// Destructor
  virtual ~MvOperation_t();
  //@}

public:  /** @name Informations */
  //@{
  /// Gets the operator
  inline const MvOperator_t *getOperatorPtr()     const { return myOperatorPtr; }
  /// Gets the number of operands
  inline int                 getNbOperands()      const { return (int)(myOperandPtrTab.size()); }
  /// Gets the indexed operand
  inline MvOperand_t        *getOperandPtr(int i) const { return myOperandPtrTab[i]; }
  //@}

public: /** @name Methods from class MvOperand_t */
  //@{
  /// Type
  virtual inline int getType() const { return myOperatorPtr->getResultType(); }
  
  /// Evaluation
  virtual MvOperand_t *evaluate(bool is_deletable=false);
  
  /// Freeing memory
  virtual void clear();
  /// For deleting the operation tree
  virtual inline bool isDeletable() const { return true; }

  // @@ MPOST R.A 07/07/2004 V.2.2.h MP_DEV_2004_67: result computation fiabilisation 
  virtual string getTitleOperand() const {return string("OPER");}
  //@}

private: // Data
  const MvOperator_t  *myOperatorPtr;
  MvOperandPtrArray_t  myOperandPtrTab;
  MvOperand_t         *myResultPtr;

};


//@}


#endif //MV_OPERATION_H




