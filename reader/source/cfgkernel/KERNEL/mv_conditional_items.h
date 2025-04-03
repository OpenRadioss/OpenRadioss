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

#ifndef MV_CONDITIONAL_ITEMS_H
#define MV_CONDITIONAL_ITEMS_H

#include <UTILS/mv_stl_various.h>

#include "mv_expression.h"

/// Template class for conditional items (Items_t must be a set)
template<class Items_t> class MvConditionalItems_t {

public: /** @name Constructors and destructor */
  //@{
  /// Constructor
  MvConditionalItems_t();
  /// Destructor
  ~MvConditionalItems_t();
  //@}

public: /** @name Creation */
  //@{
  /// Adding conditional items
  void addCondition(MvExpression_t *cond_p,const Items_t &items);
  /// Adding default
  void addDefault(const Items_t &items);
  /// Post-treatment (to do at the end of the creation)
  void posttreat();
  //@}

public: /** @name Consulting */
  //@{
  /// Gets the number or conditions
  inline int                   getNbConditions()         const { return (int)(myConditionArray.size()); }
  /// Gets the expression (condition) for the given index
  inline const MvExpression_t *getExpressionPtr(int ind) const { return myConditionArray[ind]; }
  /// Gets the expression (condition) for the given index
  inline MvExpression_t       *getExpressionPtr(int ind)       { return myConditionArray[ind]; }
  /// Gets the right (verified condition) items for the given condition (index)
  inline const Items_t        &getItems(int ind)         const { return myItemsArray[ind]; }
  /// Gets the wrong (not verified condition) items for the given condition (index)
  inline const Items_t        &getWrongItems(int ind)    const { return myWrongItemsArray[ind]; }
  /// Gets the right (verified condition) items for the default
  inline const Items_t        &getDefaultItems()         const { return myItemsArray[getNbConditions()]; }
  /// Gets the wrong (not verified condition) items for default
  inline const Items_t        &getDefaultWrongItems()    const { return myWrongItemsArray[getNbConditions()]; }

  //@}

private: // Defined types
  typedef vector<MvExpression_t *> MyExpressionPtrArray_t;
  typedef vector<Items_t>          MyItemsArray_t;

private: // Data
  MyExpressionPtrArray_t myConditionArray;
  MyItemsArray_t         myItemsArray;
  MyItemsArray_t         myWrongItemsArray;

};


#ifndef  MANAGED_TEMPLATES
#define  MV_CONDITIONAL_ITEMS_C
#include "mv_conditional_items.cpp"
#undef   MV_CONDITIONAL_ITEMS_C
#endif   //MANAGED_TEMPLATES


#endif //MV_CONDITIONAL_ITEMS_H
