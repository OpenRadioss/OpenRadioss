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
#ifndef MV_KEYWORDS_H
#define MV_KEYWORDS_H

#include <UTILS/mv_string.h>
#include "mv_utils.h"

typedef map<string, int> MvKeywordMap_t;
class MvKeywords_t {
public:
    MvKeywords_t();
public:
    inline const MvKeywordMap_t& operator[](int i)   const { return myKeywordMapArray[i]; }
    inline const MvKeywordMap_t& getUserKeywordMap() const { return myUserKeywordMap; }
    
private:
    void initUsers();             
private:
    MvKeywordMap_t myKeywordMapArray[MV_NB_MCDS_TYPES];
    MvKeywordMap_t myUserKeywordMap; 
};
enum userobject_attribute_s {
    /** Title */
    USER_SOLVER_KEYWORD=1,
    /* Last */
    USER_LAST
};
//int MV_get_ikeyword(object_type_e obj_type,const string &skeyword);


#endif //MV_KEYWORDS_H




