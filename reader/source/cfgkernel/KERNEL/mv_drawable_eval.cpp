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
#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>
#include <UTILS/error.h>


#include "dr_scalar_operator.h"
#include "dr_drawable_operator.h"
#include "dr_unary_operator.h"
#include "dr_binary_operator.h"
#include "mv_descriptor.h"
#include "mv_drawable_eval.h"

typedef enum EvalOperandType_s {
  EOT_UNKNOWN,
  EOT_OPERATOR,
  EOT_SCALAR,
  EOT_DRAWABLE,
  EOT_EXPRESSION,
  EOT_LAST
} EvalOperatorType_e;

typedef pair<EvalOperatorType_e,string> EvalOperand_t;
typedef vector<EvalOperand_t>           EvalOperandList_t;

static string          remove_spaces(const string &s);
static bool            is_operator(char c);
static bool            hc_is_scalar(const string &s);
static int             get_operator_level(const string &op);
static void            get_operand_list(const MvDescriptor_t *descr_p,const string &formula,
					EvalOperandList_t *op_list_p);
static DrExpression_t *get_expression(const MvDescriptor_t *descr_p,
				      EvalOperandList_t::const_iterator ol_it_begin,
				      EvalOperandList_t::const_iterator ol_it_end);
static DrExpression_t *get_expression(const string &formula,const MvDescriptor_t *descr_p);


/* --------- Constructors and destructor --------- */

//******************************************************************************
// MvDrawableEval_t::MvDrawableEval_t
//******************************************************************************
MvDrawableEval_t::MvDrawableEval_t(const string &name,MvDrawableAccess_e access,
				   const string &formula,const MvDescriptor_t *descr_p) :
  MvDrawable_t(name,access),
  myExpressionPtr(get_expression(remove_spaces(formula),descr_p))
{}

//******************************************************************************
// MvDrawableEval_t::~MvDrawableEval_t
//******************************************************************************
MvDrawableEval_t::~MvDrawableEval_t() {
  if(myExpressionPtr!=NULL) delete myExpressionPtr;
}

  
/* --------- Evaluation --------- */

//******************************************************************************
// MvDrawableEval_t::evaluate
//******************************************************************************
double MvDrawableEval_t::evaluate(const hwCFGDrawableInf *hwcfg_draw_inf) const {
  return myExpressionPtr==NULL ? 0. : myExpressionPtr->evaluate(hwcfg_draw_inf);
}


/* --------- static functions --------- */

//******************************************************************************
// remove_spaces
//******************************************************************************
static string remove_spaces(const string &s) {
  string a_result="";
  //
  string::const_iterator a_it_begin = s.begin();
  string::const_iterator a_it_end   = s.end();
  string::const_iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
    char a_char=(*a_it);
    if(a_char!=' ' && a_char!='\t') a_result+=a_char;
  }
  //
  return a_result;
}

//******************************************************************************
// is_operator
//******************************************************************************
static bool is_operator(char c) {
  return (c=='+' || c=='-' || c=='*' || c=='/' || c=='^');
}

//******************************************************************************
// hc_is_scalar
//******************************************************************************
static bool hc_is_scalar(const string &s) {
  char *a_res;
  strtod(s.c_str(),&a_res);
  return (a_res!=s.c_str());
}

//******************************************************************************
// get_operator_level
//******************************************************************************
typedef map<string, int> LocMapStringInt_t; 
static int get_operator_level(const string &op) {
  static map<string,int> a_op_level_map;
  static bool            a_first=true;
  //
  if(a_first) {
    a_first=false;
    //
    a_op_level_map["+"]=a_op_level_map["-"]=1;
    a_op_level_map["*"]=a_op_level_map["/"]=2;
    a_op_level_map["^"]=3;
  }
  //
  LocMapStringInt_t::iterator a_it=a_op_level_map.find(op); 
  if(a_it==a_op_level_map.end()) throw MvError_t("get_operator_level -> \"%s\" is not an operator");
  return (*a_it).second;
}

//******************************************************************************
// get_operand_list
//******************************************************************************
static void get_operand_list(const MvDescriptor_t *descr_p,const string &formula,EvalOperandList_t *op_list_p) {
  string::const_iterator a_it     = formula.begin();
  string::const_iterator a_it_end = formula.end();
  //
  while(a_it!=a_it_end) {
    char a_char=(*(a_it++));
    //
    if(is_operator(a_char)) {    // Operator
      string a_op=""; a_op+=a_char;
      EvalOperand_t a_eval_op(EOT_OPERATOR,a_op);
      op_list_p->push_back(a_eval_op);
    } else if(a_char=='(') {     // Expression
      string a_expr="";
      int a_nb_parenthesis=1;
      while(a_nb_parenthesis>0 && a_it!=a_it_end) {
	a_char=(*(a_it++));
	if(a_char=='(') ++a_nb_parenthesis; else if(a_char==')') --a_nb_parenthesis;
	if(a_nb_parenthesis>0) a_expr+=a_char;
      }
      if(a_nb_parenthesis>0) throw MvError_t("get_operand_list -> missing ')'");
      EvalOperand_t a_eval_op(EOT_EXPRESSION,a_expr);
      op_list_p->push_back(a_eval_op);
    } else {                     // Scalar or drawable?
      string a_operand="";
      while(!is_operator(a_char) && a_it!=a_it_end) {
	a_operand+=a_char;
	a_char=(*(a_it++));
      }
      if(a_it==a_it_end) a_operand+=a_char; else --a_it;
      //
      if(hc_is_scalar(a_operand)) { // Scalar
	EvalOperand_t a_scalar_op(EOT_SCALAR,a_operand);
	op_list_p->push_back(a_scalar_op);
      } else {                   // Drawable
	if(descr_p->getDrawablePtr(a_operand)==NULL) {
	  throw MvError_t("get_operand_list -> \"%s\" is not a drawable",a_operand.c_str());
	}
	EvalOperand_t a_drawable_op(EOT_DRAWABLE,a_operand);
	op_list_p->push_back(a_drawable_op);	
      }
    }
  }
}

//******************************************************************************
// get_expression
//******************************************************************************
static DrExpression_t *get_expression(const MvDescriptor_t *descr_p,
				      EvalOperandList_t::const_iterator ol_it_begin,
				      EvalOperandList_t::const_iterator ol_it_end)
{
  DrExpression_t *a_expr_p   = NULL;
  int             a_cur_level = 1;
  // Unary and binary expressions
  while(a_cur_level<=4 && a_expr_p==NULL) {
    EvalOperandList_t::const_iterator a_ol_it=ol_it_begin;
    while(a_ol_it!=ol_it_end && a_expr_p==NULL) {
      const EvalOperand_t &a_eval_op  = (*a_ol_it);
      EvalOperatorType_e   a_eo_type  = a_eval_op.first;
      const string        &a_eo_value = a_eval_op.second;
      if(a_eo_type==EOT_OPERATOR) {
	int a_op_level=4;
	if(a_ol_it!=ol_it_begin && (*(a_ol_it-1)).first!=EOT_OPERATOR) {
	  a_op_level=get_operator_level(a_eo_value);
	}
	if(a_op_level==a_cur_level) {
	  if(a_op_level==4) { // Unary expression
	    EvalOperandList_t::const_iterator a_it=(a_ol_it+1);
	    if(a_it==ol_it_end) throw MvError_t("get_expression -> unary operator error");
	    DrExpression_t *a_sub_expr_p=get_expression(descr_p,a_it,ol_it_end);
	    if(a_sub_expr_p==NULL) throw MvError_t("get_expression -> wrong unary operator sub-expression");
	    a_expr_p=new DrUnaryOperaror_t(DR_get_unary_operator_type(a_eo_value),a_sub_expr_p);
	  } else {            // Binary expression
	    EvalOperandList_t::const_iterator a_it=(a_ol_it+1);
	    if(a_it==ol_it_end) throw MvError_t("get_expression -> binary operator error");
	    DrExpression_t *a_left_expr_p=get_expression(descr_p,ol_it_begin,a_ol_it);
	    if(a_left_expr_p==NULL) throw MvError_t("get_expression -> wrong binary operator left expression");
	    DrExpression_t *a_right_expr_p=get_expression(descr_p,a_it,ol_it_end);
	    if(a_right_expr_p==NULL) throw MvError_t("get_expression -> wrong binary operator right expression");
	    a_expr_p=new DrBinaryOperaror_t(DR_get_binary_operator_type(a_eo_value),
					    a_left_expr_p,a_right_expr_p);
	  }
	}
      }
      ++a_ol_it;
    }
    ++a_cur_level;
  }
  // Scalars, drawables, expressions
  if(a_expr_p==NULL && ol_it_begin!=ol_it_end) {
    EvalOperandList_t::const_iterator a_it=(ol_it_begin+1);
    if(a_it==ol_it_end) {
      const EvalOperand_t &a_operand  = (*ol_it_begin);
      EvalOperatorType_e   a_eo_type  = a_operand.first;
      const string        &a_eo_value = a_operand.second;
      switch(a_eo_type) {
      case EOT_SCALAR:
	{
	  double a_value=0.;
	  sscanf(a_eo_value.c_str(),"%lf",&a_value);
	  a_expr_p=new DrScalarOperaror_t(a_value);
	}
	break;
      case EOT_DRAWABLE:
	{
	  const MvDrawable_t *a_drawable_p=descr_p->getDrawablePtr(a_eo_value);
	  a_expr_p=new DrDrawableOperaror_t(a_drawable_p);
	}
	break;
      case EOT_EXPRESSION:
	{
	  a_expr_p=get_expression(a_eo_value,descr_p);
	}
	break;
      default:
	throw MvError_t("get_expression -> wrong expression type");
      }
    }
  }
  //
  return a_expr_p;
}

//******************************************************************************
// get_expression
//******************************************************************************
static DrExpression_t *get_expression(const string &formula,const MvDescriptor_t *descr_p) {
  DrExpression_t *a_expr_p=NULL;
  //
  try {
    EvalOperandList_t a_operand_list;
    get_operand_list(descr_p,formula,&a_operand_list);
    const EvalOperandList_t &a_op_list=a_operand_list;
    a_expr_p=get_expression(descr_p,a_op_list.begin(),a_op_list.end());
  } catch(MvError_t &error) {
    a_expr_p=NULL;
    throw error;
  }
  //
  return a_expr_p;
}




