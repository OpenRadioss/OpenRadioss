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
#include <typedef.h>
#include <dyna2rad/propertymaterialrelation.h>

using namespace std;
using namespace sdiD2R;
using namespace sdiConvert;
using namespace sdi;

void sdiD2R::PropertyMaterialRelation::PopulatePropMatReferences()
{

    EntityType matEntType = p_LsdynaModel->GetEntityType("*MAT");
    EntityType propEntType = p_LsdynaModel->GetEntityType("*SECTION");
    p_MapPropIdToMatReadList.clear();
    p_MapMatIdToPropReadList.clear();
    SelectionRead partsSelect(p_LsdynaModel, "*PART");

    while (partsSelect.Next())
    {
        HandleRead matHandle;
        partsSelect->GetEntityHandle(sdiIdentifier("MID"), matHandle);
        EntityId matId = matHandle.GetId(p_LsdynaModel);

        HandleRead propHandle;
        partsSelect->GetEntityHandle(sdiIdentifier("SECID"), propHandle);
        EntityId propId = propHandle.GetId(p_LsdynaModel);

        map<HandleRead, EntityId> mapEntityEntityId = {{propHandle, matId} , {matHandle, propId} };
        for (auto tempPair : mapEntityEntityId)
        {
            HandleRead readHandle = tempPair.first;
            if (!readHandle.IsValid())
                continue;
            EntityRead entReadHandle(p_LsdynaModel, readHandle);
            EntityId refEntityId = tempPair.second;
            EntityId entityId = entReadHandle.GetId();
            sdiConvert::MapEntityIdToEntityReadList* mapToModify;
            if (entReadHandle.GetKeyword().find("*SECTION") != string::npos)
                mapToModify = &p_MapMatIdToPropReadList;
            else
                mapToModify = &p_MapPropIdToMatReadList;
            auto entityReadList = (*mapToModify)[refEntityId];
            if (!entityReadList.size())
            {
                (*mapToModify)[refEntityId] = { {entReadHandle} };
            }
            else
            {
                auto itr = std::find_if(entityReadList.begin(), entityReadList.end(),
                    [&](const EntityRead& obj) { return obj.GetId() == entityId; });
                if (entityReadList.end() == itr)
                {
                    entityReadList.push_back(entReadHandle);
                    (*mapToModify)[refEntityId] = entityReadList;
                }
            }
        }
    }
}



void sdiD2R::PropertyMaterialRelation::GetReferenceList(const EntityType& enityType, const EntityId& entityID, SDIEntityReadList& entityList)
{
    MapEntityIdToEntityReadList* mapToQuery;
    if (enityType == p_LsdynaModel->GetEntityType("*SECTION"))
    {
        mapToQuery = &p_MapPropIdToMatReadList;
    }
    else
    {
        mapToQuery = &p_MapMatIdToPropReadList;
    }
    auto itr = find_if((*mapToQuery).begin(), (*mapToQuery).end(),
        [&](pair<const EntityId, SDIEntityReadList>& pairObj) { return pairObj.first == entityID; });
    if ((*mapToQuery).end() != itr)
        entityList = (*mapToQuery)[entityID];
}

void sdiD2R::PropertyMaterialRelation::UpdateReferenceList(const EntityType& enityType, const EntityId& entityID, const EntityRead& refEntityRead)
{
    MapEntityIdToEntityReadList* mapToQuery;
    if (enityType == p_LsdynaModel->GetEntityType("*SECTION"))
    {
        mapToQuery = &p_MapPropIdToMatReadList;
    }
    else
    {
        mapToQuery = &p_MapMatIdToPropReadList;
    }

    auto itr = find_if((*mapToQuery).begin(), (*mapToQuery).end(),
        [&](pair<const EntityId, SDIEntityReadList>& pairObj) { return pairObj.first == entityID; });
    EntityId refEntityId = refEntityRead.GetId();
    if ((*mapToQuery).end() != itr)
    {
        auto entityList = (*mapToQuery)[entityID];
        auto itr = find_if(entityList.begin(), entityList.end(),
            [&](EntityRead& obj) {return obj.GetId() == refEntityId; });
        if (entityList.end() != itr)
        {
            entityList.erase(itr);
            (*mapToQuery)[entityID] = entityList;
        }

    }
}

void sdiD2R::PropertyMaterialRelation::ClearMapPropMatsRelations()
{
    p_MapPropIdToMatReadList.clear(); 
}

void sdiD2R::PropertyMaterialRelation::ClearMapMatPropsRelations()
{ 
    p_MapMatIdToPropReadList.clear(); 
}
