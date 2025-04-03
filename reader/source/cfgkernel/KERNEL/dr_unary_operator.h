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
#ifndef DR_UNARY_OPERATOR_H
#define DR_UNARY_OPERATOR_H

#include "dr_expression.h"


enum DrUnaryOperarorType_s {
  DRUO_UNKNOWN,
  DRUO_MINUS,
  DRUO_LAST
};
typedef enum DrUnaryOperarorType_s DrUnaryOperarorType_e;


// Unary operator
class DrUnaryOperaror_t : public DrExpression_t {
public: // Constructors and destructors
  DrUnaryOperaror_t(DrUnaryOperarorType_e operator_type,const DrExpression_t *sub_expr_p);
  virtual ~DrUnaryOperaror_t();
public: // Evaluation
  virtual double evaluate(const hwCFGDrawableInf *hwcfg_draw_inf) const;
public: // Output
  virtual ostream &display(ostream &os) const;
private:
  DrUnaryOperarorType_e  myOperatorType;
  const DrExpression_t   *myExpressionPtr;
};

// Conversions
DrUnaryOperarorType_e  DR_get_unary_operator_type(const string &type);
const string          &DR_get_unary_operator_type(DrUnaryOperarorType_e type);


#endif //DR_UNARY_OPERATOR_H




