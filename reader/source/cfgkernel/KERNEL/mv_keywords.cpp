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

#include <UTILS/win32_utils.h>  



#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>


#include "mv_type.h"
#include "mv_keywords.h"

#include "HCDI/hcdi_multicfgkernelmgr.h"
/* --------- Local definitions --------- */




//static const MvKeywordMap_t &get_keyword_map(object_type_e type);




//static const MvKeywords_t p_mv_keywords[FF_LAST];
/* --------- Public function implementation --------- */

int CFGKernel::get_ikeyword(object_type_e obj_type,const string &skeyword) const {
  const MvKeywordMap_t &a_keymap=get_keyword_map(obj_type);
  MvKeywordMap_t::const_iterator it=a_keymap.find(skeyword);
  if(it==a_keymap.end()) return END_ARGS;
  return (*it).second;
}


/* --------- MvKeywords_t class implementation (local) --------- */

MvKeywords_t::MvKeywords_t() {
  initUsers();              
}

void MvKeywords_t::initUsers() {
  MvKeywordMap_t &a_keymap=myUserKeywordMap;
  a_keymap["solverkeyword"] = USER_SOLVER_KEYWORD;
}



/* --------- Static functions --------- */

const MvKeywordMap_t& CFGKernel::get_keyword_map(object_type_e type) const {
  /* need_to_be_checked */
  
  if (is_user(type)) { 
      return p_mv_keywords.getUserKeywordMap();
  }

  return p_mv_keywords[type];
}




