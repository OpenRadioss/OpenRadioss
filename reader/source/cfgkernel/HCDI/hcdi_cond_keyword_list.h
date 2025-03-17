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
#pragma once
#include "hcdi_dimensions.h"

// Returns the keyword for which the conditions are true
template <class ENTITYREAD, class IDENTIFIER, class VALUE, class VALUEENTITY, class VALUESTRING>
static const string& HCDI_GetKeyword(
    const ENTITYREAD* entity, const IDescriptor* pDescr, const MvCondKeywordList_t& kwdList,
    int index, IdenKeyType keytype)
{
    static string empty_keyword;
    if(entity == nullptr) return empty_keyword;
    if(pDescr == nullptr) return empty_keyword;

    for(MvCondKeywordList_t::const_iterator kwdIt = kwdList.begin(); kwdIt != kwdList.end(); ++kwdIt)
    {
        const MvExpressionList_t& expList = kwdIt->second;
        bool isOk = true;
        for(MvExpressionList_t::const_iterator expIt = expList.begin(); expIt != expList.end(); ++expIt)
        {
            isOk = HCEvaluateExpression<ENTITYREAD, IDENTIFIER, VALUE, VALUEENTITY, VALUESTRING>
                (entity, pDescr, &(*expIt), index, keytype);
            if(!isOk) break;
        }
        if(isOk) return kwdIt->first;
    }

    return empty_keyword;
}

// This should probably go to another header file!
template <class ENTITYREAD, class IDENTIFIER, class VALUE, class VALUEENTITY, class VALUESTRING>
const string& HCDI_GetSKeywordFromSolverName(
    const ENTITYREAD* entity, const IDescriptor* pDescr, const string& solvername, int index, IdenKeyType keytype)
{
    static string empty_keyword;
    if(pDescr == nullptr) return empty_keyword;
    const MvCondKeywordList_t& kwdList = pDescr->getSKeywordsFromSolverName(solvername);
    const string& skeyword = HCDI_GetKeyword<ENTITYREAD, IDENTIFIER, VALUE, VALUEENTITY, VALUESTRING>
        (entity, pDescr, kwdList, index, keytype);
    if(!skeyword.empty()) return skeyword;
    // If the above didn't work, we "fall back" to the deprecated API:
    return pDescr->getSKeywordFromSolverName(solvername);
}

