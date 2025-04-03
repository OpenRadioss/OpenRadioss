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

#include <string.h>
#include "utils.h"
#include "expression_API.h"
#include "descriptor_API.h"
#include <stdarg.h>

static int loc_eval_int(int lvalue,comparator_e op,int rvalue);
static int loc_eval_float(double lvalue,comparator_e op,double rvalue);
static int loc_eval_string(const char *lvalue,comparator_e op,const char *rvalue);

/* --------- Public functions --------- */

int MCDS_new_attribute_expression(expression_t **expr_pp,
				  const descriptor_t *descr_p,
				  int ikeyword,comparator_e comparator,const void *value_p, int rikeyword)
{
  value_type_e            a_vtype  = VTYPE_UNKNOWN;
  attribute_expression_t *a_expr_p = NULL;

  if(expr_pp==NULL) return 1;
  if(descr_p==NULL) return 2;
  if(value_p==NULL) return 3;

  a_expr_p=(attribute_expression_t *)my_malloc(1,sizeof(attribute_expression_t));
  *expr_pp=(expression_t *)a_expr_p;
  if(rikeyword > 0)
      a_expr_p->my_rhs_ikeyword = rikeyword;
  else
      a_expr_p->my_rhs_ikeyword = -1;
  (*expr_pp)->my_type   = EXPRT_ATTRIBUTE;
  a_expr_p->my_ikeyword   = ikeyword;
  a_expr_p->my_comparator = comparator;
  if(rikeyword <= 0)
  {
      MCDS_get_descriptor_attributes(descr_p,ikeyword,DESCR_VALUE_TYPE,&a_vtype,END_ARGS);
      switch(a_vtype) {
      case VTYPE_INT:
      case VTYPE_BOOL:
          a_expr_p->my_rvalue_type            = RVAT_INT;
          a_expr_p->my_rvalue.my_int_value    = *((int *)value_p);
          break;
      case VTYPE_UINT:
          a_expr_p->my_rvalue_type            = RVAT_UINT;
          a_expr_p->my_rvalue.my_uint_value    = *((unsigned int *)value_p);
          break;
      case VTYPE_FLOAT:
          a_expr_p->my_rvalue_type            = RVAT_FLOAT;
          a_expr_p->my_rvalue.my_float_value  = *((double *)value_p);
          break;
      case VTYPE_STRING:
          a_expr_p->my_rvalue_type            = RVAT_STRING;
          a_expr_p->my_rvalue.my_string_value = my_strcpy(a_expr_p->my_rvalue.my_string_value,
              *((char **)value_p));
          break;
      case VTYPE_OBJECT:
          a_expr_p->my_rvalue_type            = RVAT_OBJECT;
          break;
      default:
          break;
      }
  }
  return 0;
}

int MCDS_new_logical_expression(expression_t **expr_pp,
				expression_t *first_expr_p,
				logical_operator_e op,
				expression_t *second_expr_p)
{
  logical_expression_t *a_expr_p = NULL;

  if(expr_pp==NULL)                       return 1;
  if(first_expr_p==NULL)                  return 2;
  if(second_expr_p==NULL && op!=LGOP_NOT) return 3;

  a_expr_p=(logical_expression_t *)my_malloc(1,sizeof(logical_expression_t));
  *expr_pp=(expression_t *)a_expr_p;

  (*expr_pp)->my_type        = EXPRT_LOGICAL;
  a_expr_p->my_operator      = op;
  a_expr_p->my_first_expr_p  = first_expr_p;
  a_expr_p->my_second_expr_p = second_expr_p;

  return 0;
}

#define GET_ATTRIB(arglist,type,expr_p,field) {\
  type *a_value_p=va_arg(arglist,type *);      \
  *a_value_p=expr_p->field;                    \
}

typedef char         *pchar_t;
typedef expression_t *pexpression_t;

int MCDS_get_expression_attributes(const expression_t *expr_p,...) {
  va_list   a_arglist;
  int       a_ikeyword;

  if(expr_p==NULL) return 1;
  va_start(a_arglist,expr_p);
  while((a_ikeyword=va_arg(a_arglist,int))!=END_ARGS) switch(a_ikeyword) {
  case EXPR_TYPE:
    GET_ATTRIB(a_arglist,expression_type_e,expr_p,my_type);
    break;
  default:
    switch(expr_p->my_type) {
    case EXPRT_ATTRIBUTE:
      {
	attribute_expression_t *a_expr_p=(attribute_expression_t *)expr_p;
	switch(a_ikeyword) {
	case EXPR_IKEYWORD:    GET_ATTRIB(a_arglist,int,          a_expr_p,my_ikeyword);    break;
	case EXPR_COMPARATOR:  GET_ATTRIB(a_arglist,comparator_e, a_expr_p,my_comparator);  break;
	case EXPR_RVALUE_TYPE: GET_ATTRIB(a_arglist,rvalue_type_e,a_expr_p,my_rvalue_type); break;
	case EXPR_RVALUE:
	  {
	    union rvalue_s *a_rvalue_p=(&(a_expr_p->my_rvalue));
	    switch(a_expr_p->my_rvalue_type) {
	    case RVAT_INT:    GET_ATTRIB(a_arglist,int,    a_rvalue_p,my_int_value);    break;
	    case RVAT_UINT:    GET_ATTRIB(a_arglist,unsigned int,    a_rvalue_p,my_uint_value);    break;
	    case RVAT_FLOAT:  GET_ATTRIB(a_arglist,double, a_rvalue_p,my_float_value);  break;
	    case RVAT_STRING: GET_ATTRIB(a_arglist,pchar_t,a_rvalue_p,my_string_value); break;
	    default:
	      break;
	    }
	  }
	  break;
  case EXPR_RHS_IKEYWORD:
    GET_ATTRIB(a_arglist,int,a_expr_p,my_rhs_ikeyword);
    break;
	default: return 2; /*break;*/
	}
      }
      break;
    case EXPRT_LOGICAL:
      {
	logical_expression_t *a_expr_p=(logical_expression_t *)expr_p;
	switch(a_ikeyword) {
	case EXPR_FIRST_EXPR:  GET_ATTRIB(a_arglist,pexpression_t,     a_expr_p,my_first_expr_p);  break;
	case EXPR_SECOND_EXPR: GET_ATTRIB(a_arglist,pexpression_t,     a_expr_p,my_second_expr_p); break;
	case EXPR_OPERATOR:    GET_ATTRIB(a_arglist,logical_operator_e,a_expr_p,my_operator);      break;
	default:               return 2; /*break;*/
	}
      }
      break;
    default:
      return 2; 
      /*break;*/
    }
    break;
  }
  va_end(a_arglist);
  return 0;
}

#define SET_ATTRIB(arglist,type,expr_p,field) {\
  expr_p->field=va_arg(arglist,type);          \
}

int MCDS_set_expression_attributes(expression_t *expr_p,...) {
  va_list   a_arglist;
  int       a_ikeyword;

  if(expr_p==NULL) return 1;
  va_start(a_arglist,expr_p);
  while((a_ikeyword=va_arg(a_arglist,int))!=END_ARGS) switch(a_ikeyword) {
  case EXPR_TYPE:
    SET_ATTRIB(a_arglist,expression_type_e,expr_p,my_type);
    break;
  default:
    switch(expr_p->my_type) {
    case EXPRT_ATTRIBUTE:
      {
	attribute_expression_t *a_expr_p=(attribute_expression_t *)expr_p;
	switch(a_ikeyword) {
	case EXPR_IKEYWORD:    SET_ATTRIB(a_arglist,int,          a_expr_p,my_ikeyword);    break;
	case EXPR_COMPARATOR:  SET_ATTRIB(a_arglist,comparator_e, a_expr_p,my_comparator);  break;
	case EXPR_RVALUE_TYPE: SET_ATTRIB(a_arglist,rvalue_type_e,a_expr_p,my_rvalue_type); break;
	case EXPR_RVALUE:
	  {
	    union rvalue_s *a_rvalue_p=(&(a_expr_p->my_rvalue));
	    switch(a_expr_p->my_rvalue_type) {
	    case RVAT_INT:    SET_ATTRIB(a_arglist,int,    a_rvalue_p,my_int_value);    break;
	    case RVAT_UINT:    SET_ATTRIB(a_arglist,unsigned int,    a_rvalue_p,my_uint_value);    break;
	    case RVAT_FLOAT:  SET_ATTRIB(a_arglist,double, a_rvalue_p,my_float_value);  break;
	    case RVAT_STRING: SET_ATTRIB(a_arglist,pchar_t,a_rvalue_p,my_string_value); break;
	    case RVAT_OBJECT:
	    default:
	      break;
	    }
	  }
	  break;
  case EXPR_RHS_IKEYWORD:
    SET_ATTRIB(a_arglist,int,a_expr_p,my_rhs_ikeyword);
    break;
  default: return 2; /*break;*/
	}
      }
      break;
    case EXPRT_LOGICAL:
      {
	logical_expression_t *a_expr_p=(logical_expression_t *)expr_p;
	switch(a_ikeyword) {
	case EXPR_FIRST_EXPR:  SET_ATTRIB(a_arglist,pexpression_t,     a_expr_p,my_first_expr_p);  break;
	case EXPR_SECOND_EXPR: SET_ATTRIB(a_arglist,pexpression_t,     a_expr_p,my_second_expr_p); break;
	case EXPR_OPERATOR:    SET_ATTRIB(a_arglist,logical_operator_e,a_expr_p,my_operator);      break;
	default:               return 2; /*break;*/
	}
      }
      break;
    default:
      return 2; 
      /*break;*/
    }
    break;
  }
  va_end(a_arglist);
  return 0;
}


int MCDS_delete_expression(expression_t *expr_p,int do_recursively) {
  int a_error=0;

  if(expr_p==NULL) return 1;

  switch(expr_p->my_type) {
  case EXPRT_ATTRIBUTE:
    {
      attribute_expression_t *a_expr_p=(attribute_expression_t *)expr_p;
      if(a_expr_p->my_rvalue_type==RVAT_STRING) my_free(a_expr_p->my_rvalue.my_string_value);
    }
    break;
  case EXPRT_LOGICAL:
    {
      logical_expression_t *a_expr_p=(logical_expression_t *)expr_p;
      if(do_recursively) {
	if(a_expr_p->my_first_expr_p!=NULL) {
	  a_error+=MCDS_delete_expression(a_expr_p->my_first_expr_p,do_recursively);
	  my_free(a_expr_p->my_first_expr_p);
	}
	if(a_expr_p->my_second_expr_p!=NULL) {
	  a_error+=MCDS_delete_expression(a_expr_p->my_second_expr_p,do_recursively);	
	  my_free(a_expr_p->my_second_expr_p);
	}
      }
    }
    break;
  default:
    return 2;
    break;
  }

  return a_error ? 3 : 0;
}


/* --------- Static functions --------- */


static int loc_eval_int(int lvalue,comparator_e op,int rvalue) {
  switch(op) {
  case CMPT_GT: return lvalue>rvalue;  break;
  case CMPT_GE: return lvalue>=rvalue; break;
  case CMPT_LT: return lvalue<rvalue;  break;
  case CMPT_LE: return lvalue<=rvalue; break;
  case CMPT_EQ: return lvalue==rvalue; break;
  case CMPT_NE: return lvalue!=rvalue; break;
  default:      break;
  }
  return -1;
}

static int loc_eval_float(double lvalue,comparator_e op,double rvalue) {
  switch(op) {
  case CMPT_GT: return lvalue>rvalue;  break;
  case CMPT_GE: return lvalue>=rvalue; break;
  case CMPT_LT: return lvalue<rvalue;  break;
  case CMPT_LE: return lvalue<=rvalue; break;
  case CMPT_EQ: return lvalue==rvalue; break;
  case CMPT_NE: return lvalue!=rvalue; break;
  default:      break;
  }
  return -1;
}

static int loc_eval_string(const char *lvalue,comparator_e op,const char *rvalue) {
  switch(op) {
  case CMPT_GT: return my_strcmp(lvalue,rvalue)>0;  break;
  case CMPT_GE: return my_strcmp(lvalue,rvalue)>=0; break;
  case CMPT_LT: return my_strcmp(lvalue,rvalue)<0;  break;
  case CMPT_LE: return my_strcmp(lvalue,rvalue)<=0; break;
  case CMPT_EQ: return my_strcmp(lvalue,rvalue)==0; break;
  case CMPT_NE: return my_strcmp(lvalue,rvalue)!=0; break;
  default:      break;
  }
  return -1;
}



