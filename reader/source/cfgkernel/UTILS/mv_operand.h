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

#ifndef MV_OPERAND_H
#define MV_OPERAND_H


// @@ MPOST R.A 07/07/2004 V.2.2.h MP_DEV_2004_67: result computation fiabilisation 
#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>


/** @name Operand */
//@{


/// Operand
class MvOperand_t {

public:/** @name Constructors and destructor */
  //@{
  /// Constructor
  inline MvOperand_t() {}
  /// Destructor
  virtual inline ~MvOperand_t() {}
  //@}

public: /** @name Other methods */
  //@{
  /// Type
  virtual int getType() const=0;
  
  /// Evaluation
  virtual inline MvOperand_t *evaluate(bool=false) { return this; }
  
  /// Freeing memory
  virtual inline void clear() {}
  /// For deleting the operation tree
  virtual inline bool isDeletable() const { return false; }
  // @@ MPOST R.A 07/07/2004 V.2.2.h MP_DEV_2004_67: result computation fiabilisation
  /// Getting title of the operand
  virtual string getTitleOperand() const=0;
  //@}

};


/// Operand array
typedef vector<MvOperand_t *> MvOperandPtrArray_t;


//@}


#endif //MV_OPERATOR_H




