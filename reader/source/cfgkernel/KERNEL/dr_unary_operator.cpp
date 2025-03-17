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
#include <UTILS/mv_quick_keyword_map.h>   

#include "dr_unary_operator.h"

typedef double (*DrUnaryOperatorFuncPtr_t)(double);

static const MvQuickKeywordMap_t &get_operator_map();
static bool                       hc_is_function(DrUnaryOperarorType_e operator_type);
static DrUnaryOperatorFuncPtr_t  get_operator_ptr(DrUnaryOperarorType_e operator_type);


/* --------- Constructors and destructor --------- */

//******************************************************************************
// DrUnaryOperaror_t::DrUnaryOperaror_t
//******************************************************************************
DrUnaryOperaror_t::DrUnaryOperaror_t(DrUnaryOperarorType_e operator_type,const DrExpression_t *sub_expr_p) :
  DrExpression_t(), myOperatorType(operator_type), myExpressionPtr(sub_expr_p)
{}

//******************************************************************************
// DrUnaryOperaror_t::~DrUnaryOperaror_t
//******************************************************************************
DrUnaryOperaror_t::~DrUnaryOperaror_t() {
  delete myExpressionPtr;
}


/* --------- Evaluation --------- */

//******************************************************************************
// DrUnaryOperaror_t::evaluate
//******************************************************************************
double DrUnaryOperaror_t::evaluate(const hwCFGDrawableInf *hwcfg_draw_inf) const {
  DrUnaryOperatorFuncPtr_t a_operator_p=get_operator_ptr(myOperatorType);
  return (*a_operator_p)(myExpressionPtr->evaluate(hwcfg_draw_inf));
}


/* --------- Output --------- */

//******************************************************************************
// DrUnaryOperaror_t::display
//******************************************************************************
ostream &DrUnaryOperaror_t::display(ostream &os) const {
  if(hc_is_function(myOperatorType)) {
    os << DR_get_unary_operator_type(myOperatorType) << "("; 
    return myExpressionPtr->display(os) << ")";
  } else {
    os << DR_get_unary_operator_type(myOperatorType);
    return myExpressionPtr->display(os);    
  }
}


/* --------- Conversions (non member functions) --------- */

//******************************************************************************
// DR_get_unary_operator_type
//******************************************************************************
DrUnaryOperarorType_e DR_get_unary_operator_type(const string &type) {
  return (DrUnaryOperarorType_e)(get_operator_map()[type]);
}

//******************************************************************************
// DR_get_unary_operator_type
//******************************************************************************
const string &DR_get_unary_operator_type(DrUnaryOperarorType_e type) {
  return get_operator_map()[type];
}


/* --------- static functions --------- */

static const MvQuickKeywordMap_t &get_operator_map() {
  static MvQuickKeywordMap_t a_operator_map(DRUO_UNKNOWN,"UNKNOWN");
  static bool                a_first=true;
  //
  if(a_first) {
    a_first=false;
    //
    a_operator_map.addKeyword(DRUO_MINUS,"-");
    //
    a_operator_map.build();
  }
  //
  return a_operator_map;
}

static bool hc_is_function(DrUnaryOperarorType_e operator_type) {
  return operator_type==DRUO_MINUS ? false : true;
}

static double drso_minus(double value) { return -value; }

static DrUnaryOperatorFuncPtr_t get_operator_ptr(DrUnaryOperarorType_e operator_type) {
  static DrUnaryOperatorFuncPtr_t a_operator_array[DRUO_LAST-DRUO_UNKNOWN];
  static bool                      a_first=true;
  //
  if(a_first) {
    a_first=false;
    //
    a_operator_array[DRUO_MINUS-DRUO_UNKNOWN]=drso_minus;
  }
  //
  return a_operator_array[operator_type-DRUO_UNKNOWN];
}




