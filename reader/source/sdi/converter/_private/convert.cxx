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




////////////////////////////////////////////////////////////////////

#include <convert.h>

using namespace std;
using namespace sdi;

namespace sdiConvert
{

LogQueryHandle Convert::conversionLog = {};

void Convert::PushToConversionLog(pair<HandleRead, SDIHandlReadList> pairToPush)
{
    conversionLog.insert(pairToPush);
}

void Convert::RemoveFromConversionLog(ModelViewEdit* sdiMVEditPtr, const sdi::HandleEdit& destHEdit)
{
    auto itr = find_if(conversionLog.begin(), conversionLog.end(),
        [&destHEdit, &sdiMVEditPtr](const pair<sdi::HandleRead, SDIHandlReadList>& tempPair)
        {
            EntityRead destEntEdit(sdiMVEditPtr, destHEdit);
            sdiString keyWord = destEntEdit.GetKeyword();
            unsigned int entityId = destEntEdit.GetId();

            EntityRead findEntEdit(sdiMVEditPtr, tempPair.first);
            sdiString findEntKeyWord = findEntEdit.GetKeyword();
            unsigned int findEntityId = findEntEdit.GetId();
            return (keyWord == findEntKeyWord && entityId == findEntityId);
        }
    );
    if (itr != conversionLog.end())
    {
        conversionLog.erase(itr);
    }

}

void Convert::GetSourceHandles(ModelViewEdit* sdiMVEditPtr, const sdi::HandleRead& destHRead, SDIHandlReadList& dynaHRead)
{
    auto itr = find_if(conversionLog.begin(), conversionLog.end(),
        [&destHRead, &sdiMVEditPtr](const pair<sdi::HandleRead, SDIHandlReadList>& tempPair)
        {
            EntityRead destEntEdit(sdiMVEditPtr, destHRead);
            sdiString keyWord = destEntEdit.GetKeyword();
            unsigned int entityId = destEntEdit.GetId();

            EntityRead findEntEdit(sdiMVEditPtr, tempPair.first);
            sdiString findEntKeyWord = findEntEdit.GetKeyword();
            unsigned int findEntityId = findEntEdit.GetId();

            return (keyWord == findEntKeyWord && entityId == findEntityId);
        }
    );
    if (itr != conversionLog.end())
    {
        dynaHRead = itr->second;
    }
}


void Convert::GetConvertedHandles(const sdi::HandleRead& srcHandle, SDIHandlReadList& destHandles,
                                  sdi::EntityType destType)
{
    for (auto tempPair : conversionLog)
    {
        if( ENTITY_TYPE_NONE         == destType || // no type specified
            tempPair.first.GetType() == destType)   // caller requests a given type only
        {
            for(int i=0; i < tempPair.second.size(); i=i+1)
            {
                if(srcHandle == tempPair.second[i])
                {
                    destHandles.push_back(tempPair.first);
                    break;
                }
            }
        }
    }
}

} // namespace sdiConvert
