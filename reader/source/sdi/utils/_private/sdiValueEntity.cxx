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




////////////////////////////////////////////////////////////////////////////////////

#include <sdiValueEntity.h>
#include <sdiValuePtr.h>

#include <algorithm>
#include <set>


 static void _AddUIntLists(sdiUIntList& list, const sdiUIntList& listAdd)
 {
     // Assume that each list contains no duplicates
     std::set<unsigned int> slist;
     for (unsigned int i = 0; i != (unsigned int)list.size(); ++i)
     {
         slist.insert(list[i]);
     }

     list.reserve(list.size() + listAdd.size());
     for (unsigned int i = 0; i != (unsigned int)listAdd.size(); ++i)
     {
         if (slist.count(listAdd[i]) == 0)
         {
             list.push_back(listAdd[i]);
         }
     }
 }


 static void _SubtractUIntLists(sdiUIntList& list, const sdiUIntList& listSubtract)
 {
     // Assume that each list contains no duplicates
     std::set<unsigned int> slistSubtract;
     for (unsigned int i = 0; i != (unsigned int)listSubtract.size(); ++i)
     {
         slistSubtract.insert(list[i]);
     }

     const sdiUIntList listTemp = list;
     list.resize(0);
     for (unsigned int i = 0; i != (unsigned int)listTemp.size(); ++i)
     {
         if (slistSubtract.count(listTemp[i]) == 0)
         {
             list.push_back(listTemp[i]);
         }
     }
 }

 // Comparison between two hwUIntList; return true equal
 bool _EqualUIntList(const sdiUIntList& aListA, const sdiUIntList& aListB, const bool considerOrder)
 {
     if (aListA.size() != aListB.size())
     {
         return false;
     }

     if (!aListA.size() && !aListB.size())
     {
         return true;
     }

     if (considerOrder)
     {
         for (unsigned int i = 0; i < aListA.size(); i++)
         {
             if (aListA[i] != aListB[i])
             {
                 return false;
             }
         }
     }
     else
     {
         //! Sort copies of both; to make entry by entry comparison.
         sdiUIntList _aListA = aListA;
         sdiUIntList _aListB = aListB;
         std::sort(_aListA.begin(), _aListA.end());
         std::sort(_aListB.begin(), _aListB.end());
         for (unsigned int i = 0; i < _aListA.size(); i++)
         {
             if (_aListA[i] != _aListB[i])
             {
                 return false;
             }
         }
     }

     return true;
 }


// *********************************************************************************
// sdiValueEntityList Implementation
// *********************************************************************************

bool sdiValueEntityList::operator==(const sdiValueEntityList& other) const
{
    if (p_entityFullType != other.p_entityFullType)
    {
        return false;
    }

    return _EqualUIntList(p_aId, other.p_aId, true);
}


// *********************************************************************************
// sdiValueEntityList2 Implementation
// *********************************************************************************

bool sdiValueEntityList2::operator==(const sdiValueEntityList2& other) const
{
    if (p_entityFullType != other.p_entityFullType)
    {
        return false;
    }
    if (p_aaId.size() != other.p_aaId.size())
    {
        return false;
    }
    for (unsigned int i = 0; i < p_aaId.size(); i++)
    {
        if (!_EqualUIntList(p_aaId[i], other.p_aaId[i], true))
        {
            return false;
        }
    }
    return true;
}


