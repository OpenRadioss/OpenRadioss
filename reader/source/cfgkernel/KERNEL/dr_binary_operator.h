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
#ifndef DR_BINARY_OPERATOR_H
#define DR_BINARY_OPERATOR_H

#include "dr_expression.h"


enum DrBinaryOperarorType_s {
  DRBO_UNKNOWN,
  DRBO_PLUS,
  DRBO_LESS,
  DRBO_MULTIPLY,
  DRBO_DIVIDE,
  DRBO_POWER,
  DRBO_LAST
};
typedef enum DrBinaryOperarorType_s DrBinaryOperarorType_e;


// Binary operator
class DrBinaryOperaror_t : public DrExpression_t {
public: // Constructors and destructors
  DrBinaryOperaror_t(DrBinaryOperarorType_e operator_type,
		     const DrExpression_t *left_expr_p,const DrExpression_t *right_expr_p);
  virtual ~DrBinaryOperaror_t();
public: // Evaluation
  virtual double evaluate(const hwCFGDrawableInf *hwcfg_draw_inf) const;
public: // Output
  virtual ostream &display(ostream &os) const;
private:
  DrBinaryOperarorType_e  myOperatorType;
  const DrExpression_t   *myLeftExpressionPtr;
  const DrExpression_t   *myRightExpressionPtr;
};

// Conversions
DrBinaryOperarorType_e  DR_get_binary_operator_type(const string &type);
const string           &DR_get_binary_operator_type(DrBinaryOperarorType_e type);

#endif //DR_BINARY_OPERATOR_H




