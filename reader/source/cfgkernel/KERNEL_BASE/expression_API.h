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

#ifndef EXPRESSION_API_H
#define EXPRESSION_API_H

#include "Structure_expression.h"
#include "Structure_descriptor.h"
#include <HCDI/hcdi.h>
/**@name Expressions */
/*@{*/

/** Expression attributes */
enum expression_attribute_s {
  /** Type of expression */
  EXPR_TYPE=1,
  /**@name Attribute expressions */
  /*@{*/
  /** I-keyword of the attribute */
  EXPR_IKEYWORD,
  /** Comparator */
  EXPR_COMPARATOR,
  /** Type of the attribute/rvalue */
  EXPR_RVALUE_TYPE,
  /** Value the attribute must be compared with */
  EXPR_RVALUE,
  /*@}*/
  /**@name Logical expressions */
  /*@{*/
  /** First expression */
  EXPR_FIRST_EXPR,
  /** Second expression */
  EXPR_SECOND_EXPR,
  /** Logical operator */
  EXPR_OPERATOR,
  /** Attiribute comparision */
  EXPR_RHS_IKEYWORD
  /*@}*/
};
/** Expression attributes */
typedef enum expression_attribute_s expression_attribute_e;

#ifdef __cplusplus
extern "C" {
#endif
  int MCDS_new_attribute_expression(expression_t **expr_pp,
				    const descriptor_t *descr_p,
				    int ikeyword,comparator_e comparator,const void *value_p, int rikeyword);

  int MCDS_new_logical_expression(expression_t **expr_pp,
				  expression_t *first_expr_p,
				  logical_operator_e op,
				  expression_t *second_expr_p);

  HC_DATA_DLL_API int MCDS_get_expression_attributes(const expression_t *expr_p,...);

  int MCDS_set_expression_attributes(expression_t *expr_p,...);

  
  

  int MCDS_delete_expression(expression_t *expr_p,int do_recursively);

#ifdef __cplusplus
}
#endif

/*@}*/


#endif /*EXPRESSION_API_H*/




