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

#ifndef MV_LOGICAL_EXPRESSION_H
#define MV_LOGICAL_EXPRESSION_H

#include <UTILS/mv_iostream.h>

#include "mv_logical_operator.h"
#include "mv_expression.h"


/// Logical expression
class MvLogicalExpression_t : public MvExpression_t {

public: /** @name Constructors and destructors */
  //@{
  /// Constructor
  MvLogicalExpression_t(expression_t *expr_p,bool do_delete=true);
  /// Constructor (unary operator)
  MvLogicalExpression_t(MvLogicalOperator_e op,const MvExpression_t *expr_p,bool do_delete=true);
  /// Constructor (binary operator)
  MvLogicalExpression_t(const MvExpression_t *expr0_p,MvLogicalOperator_e op,const MvExpression_t *expr1_p,bool do_delete=true);
  /// Destructor
  virtual ~MvLogicalExpression_t();
  //@}

public:  // Public hidden functions 
  ostream &display(ostream &os,const MvDescriptor_t &descr) const;

private:
  
  /*
  MvLogicalOperator_e   myOperator;
  const MvExpression_t *myFirstExprPtr;
  const MvExpression_t *mySecondExprPtr;
  */
  

};


#endif //MV_LOGICAL_EXPRESSION_H




