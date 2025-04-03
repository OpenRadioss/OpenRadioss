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

#include <UTILS/win32_utils.h>

#include "mv_operation.h"


/* --------- Constructors and destructor --------- */

MvOperation_t::MvOperation_t(const MvOperator_t *operator_p,const MvOperandPtrArray_t &operands) :
  MvOperand_t(),
  myOperatorPtr(operator_p),
  myOperandPtrTab(operands),
  myResultPtr(NULL)
{}

MvOperation_t::MvOperation_t(const MvOperator_t *unary_operator_p,MvOperand_t *operand_p) :
  MvOperand_t(),
  myOperatorPtr(unary_operator_p),
  myOperandPtrTab(1),
  myResultPtr(NULL)
{
  myOperandPtrTab[0]=operand_p;
}

MvOperation_t::MvOperation_t(const MvOperator_t *binary_operator_p,
			     MvOperand_t *left_operand_p,MvOperand_t *right_operand_p) :
  MvOperand_t(),
  myOperatorPtr(binary_operator_p),
  myOperandPtrTab(2),
  myResultPtr(NULL)
{
  myOperandPtrTab[0]=left_operand_p;
  myOperandPtrTab[1]=right_operand_p;
}

MvOperation_t::~MvOperation_t() {
  int a_nb_operands=getNbOperands();
  for(int i=0;i<a_nb_operands;++i) {
    MvOperand_t *a_operand_pf=getOperandPtr(i);
    if(a_operand_pf->isDeletable()) delete a_operand_pf;
  }
}


/* --------- Methods from class MvOperand_t --------- */


MvOperand_t *MvOperation_t::evaluate(bool is_deletable) {
  int                 i,a_nb_operands=getNbOperands();
  MvOperandPtrArray_t a_evaluated_operands(a_nb_operands);
  for(i=0;i<a_nb_operands;++i) {
    MvOperand_t *a_operand_p    = getOperandPtr(i);
    bool         a_is_deletable = a_operand_p->isDeletable();
    //
    a_evaluated_operands[i]=a_operand_p->evaluate(a_is_deletable);
  }	 
  //
  clear();
  const MvOperator_t *a_operator_p=getOperatorPtr();
  myResultPtr=a_operator_p->evaluate(a_evaluated_operands,is_deletable);
  //
  for(i=0;i<a_nb_operands;++i) getOperandPtr(i)->clear();
  //
  return myResultPtr;
}


void MvOperation_t::clear() {
  if(myResultPtr!=NULL) {
    delete myResultPtr;
    myResultPtr=NULL;
  }
}




