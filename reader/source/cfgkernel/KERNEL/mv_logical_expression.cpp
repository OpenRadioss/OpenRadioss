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



#include <UTILS/mv_cstdio.h> 
#include <UTILS/error.h>         
#include <UTILS/str_utils.h>     
#include <MESSAGE/mv_messages.h> 

#include <KERNEL_BASE/expression_API.h>
#include <KERNEL_BASE/utils.h>
#include "mv_logical_expression.h"


/* --------- Constructors and destructors --------- */

MvLogicalExpression_t::MvLogicalExpression_t(expression_t *expr_p,bool do_delete) :
  MvExpression_t(expr_p,do_delete)
{}

MvLogicalExpression_t::MvLogicalExpression_t(MvLogicalOperator_e op,const MvExpression_t *expr_p,bool do_delete) :
  MvExpression_t(NULL,do_delete)
{
  MCDS_new_logical_expression(&myExpressionPtr,expr_p->getExpressionPtr(),op,NULL);  
}

MvLogicalExpression_t::MvLogicalExpression_t(const MvExpression_t *expr0_p,
					     MvLogicalOperator_e   op,
					     const MvExpression_t *expr1_p,
					     bool do_delete) :
  MvExpression_t(NULL,do_delete)
{
  MCDS_new_logical_expression(&myExpressionPtr,expr0_p->getExpressionPtr(),op,expr1_p->getExpressionPtr());  
}

MvLogicalExpression_t::~MvLogicalExpression_t() {
}


/* --------- Public hidden functions --------- */

ostream &MvLogicalExpression_t::display(ostream &os,const MvDescriptor_t &descr) const {
  expression_t        *a_expr0_p  = NULL;
  expression_t        *a_expr1_p  = NULL;
  MvLogicalOperator_e  a_operator = LGOP_UNKNOWN;
  //
  MCDS_get_expression_attributes(myExpressionPtr,
				 EXPR_FIRST_EXPR, &a_expr0_p,
				 EXPR_OPERATOR,   &a_operator,
				 EXPR_SECOND_EXPR,&a_expr1_p,
				 END_ARGS);
  //
  if(a_operator==LGOP_NOT && a_expr1_p==NULL) {
    MvExpression_t a_exp(a_expr0_p,false);
    os << "!(" ; 
    a_exp.display(os,descr) << ")";
  } else {
    MvExpression_t a_exp0(a_expr0_p,false);
    MvExpression_t a_exp1(a_expr1_p,false);
    os << "("; a_exp0.display(os,descr) << ") ";
    os << MV_get_logical_operator(a_operator);
    os << " ("; a_exp1.display(os,descr) << ")";
  }
  //
  return os;
}




