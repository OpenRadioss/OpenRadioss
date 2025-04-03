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

#include <UTILS/mv_cmath.h> 
#include <UTILS/mv_quick_keyword_map.h> 

#include "dr_binary_operator.h"

#define EPSILON 1.E-30

typedef double (*DrBinaryOperatorFuncPtr_t)(double,double);

static const MvQuickKeywordMap_t &get_operator_map();
static DrBinaryOperatorFuncPtr_t  get_operator_ptr(DrBinaryOperarorType_e operator_type);


/* --------- Constructors and destructor --------- */

//******************************************************************************
// DrBinaryOperaror_t::DrBinaryOperaror_t
//******************************************************************************
DrBinaryOperaror_t::DrBinaryOperaror_t(DrBinaryOperarorType_e operator_type,
				       const DrExpression_t *left_expr_p,const DrExpression_t *right_expr_p) :
  DrExpression_t(), 
  myOperatorType(operator_type), 
  myLeftExpressionPtr(left_expr_p),
  myRightExpressionPtr(right_expr_p)
{}

//******************************************************************************
// DrBinaryOperaror_t::~DrBinaryOperaror_t
//******************************************************************************
DrBinaryOperaror_t::~DrBinaryOperaror_t() {
  delete myLeftExpressionPtr;
  delete myRightExpressionPtr;
}


/* --------- Evaluation --------- */

//******************************************************************************
// DrBinaryOperaror_t::evaluate
//******************************************************************************
double DrBinaryOperaror_t::evaluate(const hwCFGDrawableInf *hwcfg_draw_inf) const {
  DrBinaryOperatorFuncPtr_t a_operator_p=get_operator_ptr(myOperatorType);
  return (*a_operator_p)(myLeftExpressionPtr->evaluate(hwcfg_draw_inf),myRightExpressionPtr->evaluate(hwcfg_draw_inf));
}


/* --------- Output --------- */

//******************************************************************************
// DrBinaryOperaror_t::display
//******************************************************************************
ostream &DrBinaryOperaror_t::display(ostream &os) const {
  myLeftExpressionPtr->display(os);
  os << DR_get_binary_operator_type(myOperatorType);
  return myRightExpressionPtr->display(os);    
}


/* --------- Conversions (non member functions) --------- */

//******************************************************************************
// DR_get_binary_operator_type
//******************************************************************************
DrBinaryOperarorType_e DR_get_binary_operator_type(const string &type) {
  return (DrBinaryOperarorType_e)(get_operator_map()[type]);
}

//******************************************************************************
// DR_get_binary_operator_type
//******************************************************************************
const string &DR_get_binary_operator_type(DrBinaryOperarorType_e type) {
  return get_operator_map()[type];
}


/* --------- static functions --------- */

static const MvQuickKeywordMap_t &get_operator_map() {
  static MvQuickKeywordMap_t a_operator_map(DRBO_UNKNOWN,"UNKNOWN");/*multimodel No change required*/
  static bool                a_first=true;
  //
  if(a_first) {
    a_first=false;
    //
    a_operator_map.addKeyword(DRBO_PLUS,    "+");
    a_operator_map.addKeyword(DRBO_LESS,    "-");
    a_operator_map.addKeyword(DRBO_MULTIPLY,"*");
    a_operator_map.addKeyword(DRBO_DIVIDE,  "/");
    a_operator_map.addKeyword(DRBO_POWER,   "^");
    //
    a_operator_map.build();
  }
  //
  return a_operator_map;
}

static double drbo_plus(double lvalue,double rvalue)     { return lvalue+rvalue; }
static double drbo_less(double lvalue,double rvalue)     { return lvalue-rvalue; }
static double drbo_multiply(double lvalue,double rvalue) { return lvalue*rvalue; }
static double drbo_divide(double lvalue,double rvalue)   { 
  return ((fabs(rvalue)<EPSILON) ? 0. : (lvalue/rvalue)); 
}
static double drbo_power(double lvalue,double rvalue)    { return pow(lvalue,rvalue); }

static DrBinaryOperatorFuncPtr_t get_operator_ptr(DrBinaryOperarorType_e operator_type) {
  static DrBinaryOperatorFuncPtr_t a_operator_array[DRBO_LAST-DRBO_UNKNOWN];
  static bool                      a_first=true;
  //
  if(a_first) {
    a_first=false;
    //
    a_operator_array[DRBO_PLUS     - DRBO_UNKNOWN]=drbo_plus;
    a_operator_array[DRBO_LESS     - DRBO_UNKNOWN]=drbo_less;
    a_operator_array[DRBO_MULTIPLY - DRBO_UNKNOWN]=drbo_multiply;
    a_operator_array[DRBO_DIVIDE   - DRBO_UNKNOWN]=drbo_divide;
    a_operator_array[DRBO_POWER    - DRBO_UNKNOWN]=drbo_power;
  }
  //
  return a_operator_array[operator_type-DRBO_UNKNOWN];
}




