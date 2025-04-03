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

#ifdef   MANAGED_TEMPLATES
#include "mv_conditional_items.h"
#define  MV_CONDITIONAL_ITEMS_C
#endif //MANAGED_TEMPLATES


#ifdef MV_CONDITIONAL_ITEMS_C



/* --------- Constructors and destructor --------- */

template<class Items_t>
MvConditionalItems_t<Items_t>::MvConditionalItems_t() {
}

template<class Items_t>
MvConditionalItems_t<Items_t>::~MvConditionalItems_t() {
  int a_nb_conds=getNbConditions();
  for(int i=0;i<a_nb_conds;++i) delete myConditionArray[i];
}


/* --------- Creation --------- */

template<class Items_t>
void MvConditionalItems_t<Items_t>::addCondition(MvExpression_t *cond_p,const Items_t &items) {
  myConditionArray.push_back(cond_p);
  myItemsArray.push_back(items);
}

template<class Items_t>
void MvConditionalItems_t<Items_t>::addDefault(const Items_t &items) {
  myItemsArray.push_back(items);
}

template<class Items_t>
void MvConditionalItems_t<Items_t>::posttreat() {
  Items_t a_items;
  //
  if(myItemsArray.size()==myConditionArray.size()) addDefault(a_items);
  //
  int i,a_nb_conds=getNbConditions();
  for(i=0;i<=a_nb_conds;++i) {
    const Items_t &a_cond_items=myItemsArray[i];
    typename Items_t::const_iterator a_it_begin = a_cond_items.begin();
    typename Items_t::const_iterator a_it_end   = a_cond_items.end();
    typename Items_t::const_iterator a_it;
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) a_items.insert(*a_it);
  }
  //
  for(i=0;i<=a_nb_conds;++i) {
    myWrongItemsArray.push_back(a_items);
    Items_t       &a_wrong_items = myWrongItemsArray[i];
    const Items_t &a_right_items = myItemsArray[i];
    typename Items_t::const_iterator a_it_begin = a_right_items.begin();
    typename Items_t::const_iterator a_it_end   = a_right_items.end();
    typename Items_t::const_iterator a_it;
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) a_wrong_items.erase(*a_it);
  }
}
#endif //MV_CONDITIONAL_ITEMS_C
