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


#include <UTILS/win32_utils.h>


#include "mu_operations.h"


static MuQuantity_t loc_quantity_product(const MuQuantity_t &lq,const MuQuantity_t &rq);
static MuQuantity_t loc_quantity_quotient(const MuQuantity_t &lq,const MuQuantity_t &rq);
static MuQuantity_t loc_quantity_power(const MuQuantity_t &lq,double exp);
static MuUnit_t     loc_unit_product(const MuUnit_t &lu,const MuUnit_t &rq);
static MuUnit_t     loc_unit_quotient(const MuUnit_t &lu,const MuUnit_t &rq);
static MuUnit_t     loc_unit_power(const MuUnit_t &lu,double exp);


MvOperand_t *MuQuantitiesUnitsVsQuantitiesUnitsOperator_t::evaluate(const MvOperandPtrArray_t &operands,bool) const {
  const MuQuantitiesUnitsOperand_t *a_left_operand_p  = (const MuQuantitiesUnitsOperand_t *)(operands[0]);
  const MuQuantitiesUnitsOperand_t *a_right_operand_p = (const MuQuantitiesUnitsOperand_t *)(operands[1]);
  //
  int a_left_nb_quantities  = a_left_operand_p->getNbQuantities();
  int a_right_nb_quantities = a_right_operand_p->getNbQuantities();
  int a_nb_quantities       = a_left_nb_quantities*a_right_nb_quantities;
  //
  MuQuantitiesUnitsOperand_t *a_result_p=new MuQuantitiesUnitsOperand_t(a_nb_quantities);
  a_nb_quantities=0;
  //
  for(int i=0;i<a_left_nb_quantities;++i) {
    const MuQuantity_t &a_left_quantity = a_left_operand_p->getQuantity(i);
    const MuUnit_t     &a_left_unit     = a_left_operand_p->getUnit(i);
    //
    for(int j=0;j<a_right_nb_quantities;++j) {
      const MuQuantity_t &a_right_quantity = a_right_operand_p->getQuantity(j);
      const MuUnit_t     &a_right_unit     = a_right_operand_p->getUnit(j);
      //
      a_result_p->setQuantity(a_nb_quantities,myQuantityFunc(a_left_quantity,a_right_quantity));
      a_result_p->setUnit(a_nb_quantities++,myUnitFunc(a_left_unit,a_right_unit));
    }
  }
  //
  return a_result_p;
}


MvOperand_t *MuQuantitiesUnitsVsScalarOperator_t::evaluate(const MvOperandPtrArray_t &operands,bool) const {
  const MuQuantitiesUnitsOperand_t *a_left_operand_p  = (const MuQuantitiesUnitsOperand_t *)(operands[0]);
  const MuScalarOperand_t          *a_right_operand_p = (const MuScalarOperand_t *)(operands[1]);
  //
  int a_nb_quantities       = a_left_operand_p->getNbQuantities();;
  //
  MuQuantitiesUnitsOperand_t *a_result_p=new MuQuantitiesUnitsOperand_t(a_nb_quantities);
  //
  for(int i=0;i<a_nb_quantities;++i) {
    const MuQuantity_t &a_left_quantity = a_left_operand_p->getQuantity(i);
    const MuUnit_t     &a_left_unit     = a_left_operand_p->getUnit(i);
    double              a_right_value   = a_right_operand_p->getValue();
    //
    a_result_p->setQuantity(i,myQuantityFunc(a_left_quantity,a_right_value));
    a_result_p->setUnit(i,myUnitFunc(a_left_unit,a_right_value));
  }
  //
  return a_result_p;
}


MuQuantitiesUnitsOperatorManager_t::MuQuantitiesUnitsOperatorManager_t() :
  MvOperatorManager_t()
{
  addBinaryOperator("*",(MuQuantityQuantityFunction_t)loc_quantity_product,(MuUnitUnitFunction_t)loc_unit_product);
  addBinaryOperator("/",(MuQuantityQuantityFunction_t)loc_quantity_quotient,(MuUnitUnitFunction_t)loc_unit_quotient);
  addBinaryOperator("^",(MuQuantityScalarFunction_t)loc_quantity_power,(MuUnitScalarFunction_t)loc_unit_power);
}

void MuQuantitiesUnitsOperatorManager_t::addBinaryOperator(const string                       &skeyword,
						      const MuQuantityQuantityFunction_t &qfunc,
						      const MuUnitUnitFunction_t         &ufunc)
{
  MuQuantitiesUnitsVsQuantitiesUnitsOperator_t *a_operator_p=new MuQuantitiesUnitsVsQuantitiesUnitsOperator_t(qfunc,ufunc);
  MvOperatorManager_t::addBinaryOperator(a_operator_p,skeyword,MUOPT_QUANTITIES_UNITS,MUOPT_QUANTITIES_UNITS);
}

void MuQuantitiesUnitsOperatorManager_t::addBinaryOperator(const string                     &skeyword,
						      const MuQuantityScalarFunction_t &qfunc,
						      const MuUnitScalarFunction_t     &ufunc)
{
  MuQuantitiesUnitsVsScalarOperator_t *a_operator_p=new MuQuantitiesUnitsVsScalarOperator_t(qfunc,ufunc);
  MvOperatorManager_t::addBinaryOperator(a_operator_p,skeyword,MUOPT_QUANTITIES_UNITS,MUOPT_SCALAR);
}


static MuQuantity_t loc_quantity_product(const MuQuantity_t &lq,const MuQuantity_t &rq) {
  
  /*
  cout << "--------- loc_quantity_product beg ---------" << endl;
  cout << "LEFT_QUANTITY   = " << lq << endl;
  cout << "RIGHT_QUANTITY  = " << rq << endl;
  cout << "RESULT_QUANTITY = " << lq*rq << endl;
  cout << "--------- loc_quantity_product end ---------" << endl;
  */
  
  return lq*rq;
}

static MuQuantity_t loc_quantity_quotient(const MuQuantity_t &lq,const MuQuantity_t &rq) {
  
  /*
  cout << "--------- loc_quantity_quotient beg ---------" << endl << flush;
  cout << "LEFT_QUANTITY   = " << lq << endl << flush;
  cout << "RIGHT_QUANTITY  = " << rq << endl << flush;
  cout << "RESULT_QUANTITY = " << lq/rq << endl << flush;
  cout << "--------- loc_quantity_quotient end ---------" << endl << flush;
  */
  
  return lq/rq;
}

static MuQuantity_t loc_quantity_power(const MuQuantity_t &lq,double exp) {
  
  /*
  cout << "--------- loc_quantity_power beg ---------" << endl;
  cout << "LEFT_QUANTITY   = " << lq << endl;
  cout << "RIGHT_EXPONENT  = " << exp << endl;
  MuQuantity_t a_result(lq^exp);
  cout << "RESULT_QUANTITY = " << a_result << endl;
  cout << "--------- loc_quantity_power end ---------" << endl;
  */
  
  return lq^exp;
}

static MuUnit_t loc_unit_product(const MuUnit_t &lu,const MuUnit_t &ru) {
  
  /*
  cout << "--------- loc_unit_product beg ---------" << endl;
  cout << "LEFT_UNIT   = " << lu << endl;
  cout << "RIGHT_UNIT  = " << ru << endl;
  cout << "RESULT_UNIT = " << lu*ru << endl;
  cout << "--------- loc_unit_product end ---------" << endl;
  */
  
  return lu*ru;
}

static MuUnit_t loc_unit_quotient(const MuUnit_t &lu,const MuUnit_t &ru) {
  
  /*
  cout << "--------- loc_unit_quotient beg ---------" << endl;
  cout << "LEFT_UNIT   = " << lu    << endl;
  cout << "RIGHT_UNIT  = " << ru    << endl;
  cout << "RESULT_UNIT = " << lu/ru << endl;
  cout << "--------- loc_unit_quotient end ---------" << endl;
  */
  
  return lu/ru;
}

static MuUnit_t loc_unit_power(const MuUnit_t &lu,double exp) {
  
  /*
  cout << "--------- loc_unit_power beg ---------" << endl;
  cout << "LEFT_UNIT   = "     << lu     << endl;
  cout << "RIGHT_EXPONENT  = " << exp    << endl;
  MuUnit_t a_result(lu^exp);
  cout << "RESULT_UNIT = "     << a_result << endl;
  cout << "--------- loc_unit_power end ---------" << endl;
  */
  
  return lu^exp;
}
