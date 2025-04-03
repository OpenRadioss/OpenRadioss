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
#include <UTILS/mv_stl_various.h>
#include <UTILS/error.h>
#include <UTILS/str_utils.h>
#include <MESSAGE/mv_messages.h>



#include "mv_descriptor.h" 
#include "mv_attribute_expression.h"
#include <KERNEL_BASE/Structure_expression.h>
#include <KERNEL_BASE/expression_API.h>

#include "mvc_utils.h"
/* --------- Constructors and destructors --------- */

MvAttributeExpression_t::MvAttributeExpression_t(expression_t *expr_p,bool do_delete) :
  MvExpression_t(expr_p,do_delete)
{}

MvAttributeExpression_t::MvAttributeExpression_t(const MvDescriptor_t *descr_p,
						 int ikeyword,const string &cmp,const string &value,
						 bool do_delete) :
  MvExpression_t(NULL,do_delete)
{
    int           a_int_value    = 0;
    unsigned int  a_uint_value    = 0;
    double      a_float_value  = 0;
    const char *a_string_value = NULL;
    void       *a_value_p      = NULL;
    int         rikeyword      = 0;
    bool        isValue        = false;
    //
    switch(descr_p->getValueType(ikeyword)) {
    case VTYPE_INT:
    case VTYPE_BOOL:
        {
            rikeyword =  descr_p->getIKeyword(value);
            if(rikeyword <= 0)
            {
                if(value == "FALSE" || value == "TRUE")
                    isValue = true;
                else
                    isValue = (mvc_isNumber(value.c_str()) == 1) ? true : false ;
            }
            if(rikeyword <= 0 && isValue == false)
                throw MvError_t("%s not found in attrib block",value.c_str());
            if(rikeyword > 0)
            {
                value_type_e r_skeyword_type = descr_p->getValueType(rikeyword);
                if(r_skeyword_type == VTYPE_OBJECT || r_skeyword_type == VTYPE_STRING)
                    throw MvError_t("%s is different type",value.c_str());
                a_value_p=(&rikeyword);
            }
            else if(rikeyword <= 0 && isValue == true)
            {
                if(value=="FALSE")     a_int_value=0; 
                else if(value=="TRUE") a_int_value=1;
                else sscanf(value.c_str(),"%i",&a_int_value);
                a_value_p=(&a_int_value);
            }
        }
        break;
    case VTYPE_UINT:
        {
            rikeyword =  descr_p->getIKeyword(value);
            if(rikeyword <= 0)
			{
                if(value == "FALSE" || value == "TRUE")
                    isValue = true;
                else
                    isValue = (mvc_isNumber(value.c_str()) == 1) ? true : false;
            }
            if(rikeyword <= 0 && isValue == false)
                throw MvError_t("%s not found in attrib block",value.c_str());
            if(rikeyword > 0)
            {
                value_type_e r_skeyword_type = descr_p->getValueType(rikeyword);
                if(r_skeyword_type == VTYPE_OBJECT || r_skeyword_type == VTYPE_STRING)
                    throw MvError_t("%s is different type",value.c_str());
                a_value_p=(&rikeyword);
            }
            else if(rikeyword <= 0 && isValue == true)
            {
                if(value=="FALSE")     a_uint_value=0; 
                else if(value=="TRUE") a_uint_value=1; 
                else sscanf(value.c_str(),"%u",&a_uint_value);
                a_value_p=(&a_uint_value);
            }
        }
        break;
    case VTYPE_FLOAT:
        {
            rikeyword =  descr_p->getIKeyword(value);
            if(rikeyword <= 0)
                isValue = (mvc_isNumber(value.c_str()) == 1) ? true : false;
            if(rikeyword <= 0 && isValue == false)
                throw MvError_t("%s not found in attrib block",value.c_str());
            if(rikeyword > 0)
            {
                value_type_e r_skeyword_type = descr_p->getValueType(rikeyword);
                if(r_skeyword_type == VTYPE_OBJECT || r_skeyword_type == VTYPE_STRING)
                    throw MvError_t("%s is different type",value.c_str());
                a_value_p=(&rikeyword);
            }
            else if(rikeyword <= 0 && isValue == true)
            {
                sscanf(value.c_str(),"%lf",&a_float_value);
                a_value_p=(&a_float_value);
            }
        }
        break;
    case VTYPE_OBJECT:
        if(value!="NONE") {
            throw MvError_t(MV_get_msg_array(MSGT_KERNEL)[47],descr_p->getSKeyword(ikeyword).c_str());
        }
        // no break
    case VTYPE_STRING:
        {
            a_string_value=value.c_str();
            a_value_p=(&a_string_value);

            
        }
        break;
    default:
        throw MvError_t("MvAttributeExpression_t::setValue -> wrong value type");
        //break;
    }

    MCDS_new_attribute_expression(&myExpressionPtr,descr_p->getDescriptorPtr(),
                                  ikeyword,MV_get_comparator(cmp),a_value_p, rikeyword);
}

  MvAttributeExpression_t::~MvAttributeExpression_t() {
  }

/* --------- Public hidden functions --------- */

ostream &MvAttributeExpression_t::display(ostream &os,const MvDescriptor_t &descr) const {
  int            a_ikeyword   = END_ARGS;
  MvComparator_e a_comparator = CMPT_UNKNOWN;
  //
  MCDS_get_expression_attributes(myExpressionPtr,
				 EXPR_IKEYWORD,  &a_ikeyword,
				 EXPR_COMPARATOR,&a_comparator,
				 END_ARGS);
  os << descr.getSKeyword(a_ikeyword) << MV_get_comparator(a_comparator);
  //
  value_type_e a_vtype=descr.getValueType(a_ikeyword);
  switch(a_vtype) {
  case VTYPE_INT:
  case VTYPE_BOOL:
    {
      int a_value=0;
      MCDS_get_expression_attributes(myExpressionPtr,EXPR_RVALUE,&a_value,END_ARGS);
      os << a_value;
    }
    break;
  case VTYPE_UINT:
    {
      unsigned int a_value=0;
      MCDS_get_expression_attributes(myExpressionPtr,EXPR_RVALUE,&a_value,END_ARGS);
      os << a_value;
    }
    break;
  case VTYPE_FLOAT:
    {
      double a_value=0.;
      MCDS_get_expression_attributes(myExpressionPtr,EXPR_RVALUE,&a_value,END_ARGS);
      os << a_value;
    }
    break;
  case VTYPE_STRING:
    {
      const char *a_value=NULL;
      MCDS_get_expression_attributes(myExpressionPtr,EXPR_RVALUE,&a_value,END_ARGS);
      os << a_value;
    }
    break;
  case VTYPE_OBJECT:
    os << "NONE";
    break;
  default:
    throw MvError_t("MvAttributeExpression_t::display -> wrong value type");
    //break;
  }
  //
  return os;
}





