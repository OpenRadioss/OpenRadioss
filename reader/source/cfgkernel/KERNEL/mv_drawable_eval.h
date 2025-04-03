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
#ifndef MV_DRAWABLE_EVAL_H
#define MV_DRAWABLE_EVAL_H

#include "dr_expression.h"
#include "mv_drawable.h"

class MvDescriptor_t;


/// Eval drawable class
class MvDrawableEval_t : public MvDrawable_t {

public: /** @name Constructors and destructor */
  //@{
  /// Constructor
  MvDrawableEval_t(const string &name,MvDrawableAccess_e access,
		   const string &formula,const MvDescriptor_t *descr_p);
  /// Destructor
  virtual ~MvDrawableEval_t();
  //@}
  
public: /** @name Accessors */
  //@{
  /// Gets the type
  virtual inline MvDrawableType_e getType() const{ return DRT_EVAL; }
  //@}
  
public: /** @name Evaluation */
  //@{
  /// Evaluation on an object
  virtual double evaluate(const hwCFGDrawableInf *hwcfg_draw_inf) const;
  //@}
  
protected: // Data
  DrExpression_t *myExpressionPtr;

};


#endif //MV_DRAWABLE_EVAL_H




