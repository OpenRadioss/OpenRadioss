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

#include "mv_cstdarg.h"
#include "mv_cstdlib.h"

#include "mv_operator_manager.h"


//=============================================
// Array of operand types
//=============================================

/* --------- Other methods --------- */

bool MvOperandTypeArray_t::operator<(const MvOperandTypeArray_t &type_tab) const {
  int a_loc_nb_types = (int)(size());
  int a_nb_types     = (int)(type_tab.size());
  //
  if(a_loc_nb_types<a_nb_types) return true;
  if(a_loc_nb_types>a_nb_types) return false;
  //
  bool a_found  = false;
  bool a_result = false;
  for(int i=0;!a_found && i<a_nb_types;++i) {
    if((*this)[i]<type_tab[i])      a_found=a_result=true;
    else if((*this)[i]>type_tab[i]) a_found=true;
  }
  //
  return a_result;
}


//=============================================
// Operator manager
//=============================================

/* --------- Constructors and destructor --------- */

MvOperatorManager_t::~MvOperatorManager_t() {
  if(myDODeleteOperators) {
    MyOperatorMap_t::iterator a_om_it_begin = myOperators.begin();
    MyOperatorMap_t::iterator a_om_it_end   = myOperators.end();
    MyOperatorMap_t::iterator a_om_it;
    for(a_om_it=a_om_it_begin;a_om_it!=a_om_it_end;++a_om_it) {
      MyOperandTypes2Operator_t &a_ot2o=(*a_om_it).second;
      //
      MyOperandTypes2Operator_t::iterator a_ot2o_it_begin = a_ot2o.begin();
      MyOperandTypes2Operator_t::iterator a_ot2o_it_end   = a_ot2o.end();
      MyOperandTypes2Operator_t::iterator a_ot2o_it;
      for(a_ot2o_it=a_ot2o_it_begin;a_ot2o_it!=a_ot2o_it_end;++a_ot2o_it) {
	const MvOperator_t *a_operator_pf=(*a_ot2o_it).second;
	//
	delete a_operator_pf;
      }
    }
  }
}


/* --------- Operator management --------- */

void MvOperatorManager_t::addOperator(const MvOperator_t *operator_p,
				      const string &skeyword,
				      const MvOperandTypeArray_t &operand_types)
{
  myOperators[skeyword][operand_types]=operator_p;
}

void MvOperatorManager_t::addOperator(const MvOperator_t *operator_p,
				      const char *skeyword,...)
{
  MvOperandTypeArray_t a_operand_types;
  //
  va_list a_arglist;
  va_start(a_arglist,skeyword);
  //
  int a_operand_type=va_arg(a_arglist,int);
  while(a_operand_type) {
    a_operand_types.push_back(a_operand_type);
    a_operand_type=va_arg(a_arglist,int);
  }
  //
  va_end(a_arglist);
  //
  addOperator(operator_p,string(skeyword),a_operand_types);
}

const MvOperator_t *MvOperatorManager_t::getOperatorPtr(const string &skeyword,
							const MvOperandTypeArray_t &operand_types) const
{
  MyOperatorMap_t::const_iterator a_om_it=myOperators.find(skeyword);
  if(a_om_it==myOperators.end()) return NULL;
  //
  const MyOperandTypes2Operator_t &a_ot2o=(*a_om_it).second;
  MyOperandTypes2Operator_t::const_iterator a_ot2o_it=a_ot2o.find(operand_types);
  if(a_ot2o_it==a_ot2o.end()) return NULL;
  //
  return (*a_ot2o_it).second;
}

const MvOperator_t *MvOperatorManager_t::getOperatorPtr(const char *skeyword,...) const {
  MvOperandTypeArray_t a_operand_types;
  //
  va_list a_arglist;
  va_start(a_arglist,skeyword);
  //
  int a_operand_type=va_arg(a_arglist,int);
  while(a_operand_type) {
    a_operand_types.push_back(a_operand_type);
    a_operand_type=va_arg(a_arglist,int);
  }
  //
  va_end(a_arglist);
  //
  return getOperatorPtr(string(skeyword),a_operand_types);
}


/* --------- Generation of operations --------- */

MvOperation_t *MvOperatorManager_t::newOperation(const string        &operator_skeyword,
						 MvOperandPtrArray_t &operand_tab,
						 int                 *error_p) const
{
  int                  a_nb_operands=(int)(operand_tab.size());
  MvOperandTypeArray_t a_operand_types(a_nb_operands);
  //
  for(int i=0;i<a_nb_operands;++i) a_operand_types[i]=operand_tab[i]->getType();
  //
  const MvOperator_t *a_operator_p=getOperatorPtr(operator_skeyword,a_operand_types);
  if(a_operator_p==NULL) {
    if(error_p!=NULL) *error_p=1;
    return NULL;
  }
  //
  int a_error=a_operator_p->getError(operand_tab);
  if(error_p!=NULL) *error_p=a_error;
  if(a_error) return NULL;
  //
  return new MvOperation_t(a_operator_p,operand_tab);
}

MvOperation_t *MvOperatorManager_t::newOperation(const char *operator_skeyword,...) const {
  MvOperandPtrArray_t a_operands;
  //
  va_list a_arglist;
  va_start(a_arglist,operator_skeyword);
  //
  MvOperand_t *a_operand_p=va_arg(a_arglist,MvOperand_t *);
  while(a_operand_p!=NULL) {
    a_operands.push_back(a_operand_p);
    a_operand_p=va_arg(a_arglist,MvOperand_t *);
  }
  //
  int *a_error_p=va_arg(a_arglist,int *);
  va_end(a_arglist);
  //
  return newOperation(string(operator_skeyword),a_operands,a_error_p);
}

/* --------- Searching operators --------- */


MvOperatorKeywords_t *MvOperatorManager_t::getAllOperators(MvOperatorKeywords_t *operators_p) const {
  MvOperatorKeywords_t *a_operators_p=(operators_p!=NULL ? operators_p : new MvOperatorKeywords_t());
  //
  MyOperatorMap_t::const_iterator a_it_begin = myOperators.begin();
  MyOperatorMap_t::const_iterator a_it_end   = myOperators.end();
  MyOperatorMap_t::const_iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
    const string &a_name = (*a_it).first;
    a_operators_p->insert(a_name);
  }
  //
  return a_operators_p;
}


MvOperatorKeywords_t *MvOperatorManager_t::searchOperators(MvOperandPtrArray_t  &operand_tab,
							   MvOperatorKeywords_t *operators_p) const
{
  MvOperatorKeywords_t *a_operators_p=(operators_p!=NULL ? operators_p : new MvOperatorKeywords_t());
  //
  int                  a_nb_operands=(int)(operand_tab.size());
  MvOperandTypeArray_t a_operand_types(a_nb_operands);
  for(int i=0;i<a_nb_operands;++i) a_operand_types[i]=operand_tab[i]->getType();  
  //
  MyOperatorMap_t::const_iterator a_it_begin = myOperators.begin();
  MyOperatorMap_t::const_iterator a_it_end   = myOperators.end();
  MyOperatorMap_t::const_iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
    const string                    &a_name = (*a_it).first;
    const MyOperandTypes2Operator_t &a_ot2o = (*a_it).second;
    MyOperandTypes2Operator_t::const_iterator a_ot2o_it=a_ot2o.find(a_operand_types);
    if(a_ot2o_it!=a_ot2o.end()) {
      const MvOperator_t *a_operator_p=(*a_ot2o_it).second;
      if(!a_operator_p->getError(operand_tab)) a_operators_p->insert(a_name);
    }
  }
  //
  return a_operators_p;
}

MvOperatorKeywords_t *MvOperatorManager_t::searchOperators(MvOperand_t          *operand_p,
							   MvOperatorKeywords_t *operators_p) const
{
  MvOperandPtrArray_t a_operand_tab(1);
  a_operand_tab[0]=operand_p;
  //
  return searchOperators(a_operand_tab,operators_p);
}

MvOperatorKeywords_t *MvOperatorManager_t::searchOperators(MvOperand_t          *left_operand_p,
							   MvOperand_t          *right_operand_p,
							   MvOperatorKeywords_t *operators_p) const
{
  MvOperandPtrArray_t a_operand_tab(2);
  a_operand_tab[0]=left_operand_p;
  a_operand_tab[1]=right_operand_p;
  //
  return searchOperators(a_operand_tab,operators_p);
}




