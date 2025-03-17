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

#ifndef STRUCTURE_EXPRESSION_H
#define STRUCTURE_EXPRESSION_H


/* --------- Expression (base) --------- */

enum expression_type_s {
  EXPRT_UNKNOWN,
  EXPRT_ATTRIBUTE,
  EXPRT_LOGICAL,
  EXPRT_LAST
};
typedef enum expression_type_s expression_type_e;

typedef struct expression_s {
  expression_type_e my_type;
} expression_t;


/* --------- Attribute expression --------- */

enum comparator_s {
  CMPT_UNKNOWN,
  CMPT_GT,
  CMPT_GE,
  CMPT_LT,
  CMPT_LE,
  CMPT_EQ,
  CMPT_NE,
  CMPT_LAST
};
typedef enum comparator_s comparator_e;

enum rvalue_type_s {
  RVAT_UNKNOWN,
  RVAT_INT,
  RVAT_UINT,
  RVAT_FLOAT,
  RVAT_STRING,
  RVAT_OBJECT,
  RVAT_RSKEYWORD,
  RVAT_LAST
};
typedef enum rvalue_type_s rvalue_type_e;

typedef struct attribute_expression_s {
  expression_t  my_expression_part;
  int           my_ikeyword;
  comparator_e  my_comparator;
  rvalue_type_e my_rvalue_type;
#if !defined(__cplusplus) ||  !defined(__CPLUSPLUS__)
  union rvalue_s {
    int           my_int_value;
    unsigned int  my_uint_value;
    double        my_float_value;
    char         *my_string_value;
  }             my_rvalue;
  int           my_rhs_ikeyword;
#endif /*__cplusplus */
} attribute_expression_t;


/* --------- Logical expression --------- */

enum logical_operator_s {
  LGOP_UNKNOWN,
  LGOP_AND,
  LGOP_OR,
  LGOP_NOT,
  LGOP_LAST
};
typedef enum logical_operator_s logical_operator_e;

typedef struct logical_expression_s {
  expression_t        my_expression_part;
  logical_operator_e  my_operator;
  expression_t       *my_first_expr_p;
  expression_t       *my_second_expr_p;
} logical_expression_t;


#endif /*STRUCTURE_EXPRESSION_H*/




