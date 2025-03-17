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



#include <UTILS/mv_stl_various.h> 
#include <UTILS/memory_utils.h>   
#include <UTILS/error.h>          

#include <KERNEL_BASE/expression_API.h>
#include "mv_model_descriptors.h"    
#include "mv_expression.h"
#include "mv_logical_expression.h"
#include "mv_attribute_expression.h"



/* --------- Constructors and destructors --------- */

MvExpression_t::MvExpression_t(expression_t *expr_p,bool do_delete) : 
  myDoDelete(do_delete), 
  myExpressionPtr(expr_p)
{}

MvExpression_t::~MvExpression_t() {
  if(myDoDelete && myExpressionPtr!=NULL) {
    MCDS_delete_expression(myExpressionPtr,true);
    myfree(myExpressionPtr);    
  }
}


/* --------- Logical evaluation --------- */












/* --------- Public hidden functions --------- */

ostream &MvExpression_t::display(ostream &os,const MvDescriptor_t &descr) const {
  expression_type_e a_etype=EXPRT_UNKNOWN;
  //
  MCDS_get_expression_attributes(myExpressionPtr,EXPR_TYPE,&a_etype,END_ARGS);
  //
  switch(a_etype) {
  case EXPRT_ATTRIBUTE:
    {
      MvAttributeExpression_t a_expr(myExpressionPtr,false);
      a_expr.display(os,descr);
    }
    break;
  case EXPRT_LOGICAL: 
    {
      MvLogicalExpression_t a_expr(myExpressionPtr,false);
      a_expr.display(os,descr);
    }
    break;
  default: 
    throw MvError_t("MvExpression_t::display -> wrong expression type");
    //break;
  }
  //
  return os;
}




