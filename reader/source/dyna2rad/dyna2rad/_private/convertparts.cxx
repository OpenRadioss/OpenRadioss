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

#include <dyna2rad/dyna2rad.h>
#include <dyna2rad/convertparts.h>
#include <dyna2rad/convertutils.h>

using namespace sdiD2R;
using namespace std;
using namespace sdi;
using namespace sdiConvert;

void ConvertPart::ConvertParts()
{
    ConvertEntities();
}

void ConvertPart::ConvertEntities()
{
    sdiValueEntityType subsetEntityType(p_radiossModel->GetEntityType("/SUBSET"));
    
    // These aren't LS-Dyna cards, but converting them allows HW users to keep their assembly structure
    SelectionRead assemsSelect(p_lsdynaModel, "HMASSEM");
    // First scan to store the assembly ids:
    
    //   non-existing child assems into a parent /SUBSET
    // - The bool is used to indicate:
    //   - false: This subset is not yet put as child in a parent subset.
    //   - true:  This subset is already put as child in a parent subset, so must not be put again.
    map<unsigned int, bool> assemIds;
    while(assemsSelect.Next())
    {
        assemIds[assemsSelect->GetId()] = false;
    }
    // Second scan to create /SUBSET and populate a map for the parts
    map<unsigned int, unsigned int> assemOfPart;
    assemsSelect.Restart();
    while(assemsSelect.Next())
    {
        // Create /SUBSET
        HandleEdit subsetEditHandle;
        unsigned int subsetId = assemsSelect->GetId();
        p_radiossModel->CreateEntity(subsetEditHandle, "/SUBSET" , assemsSelect->GetName(), subsetId);
        if (subsetEditHandle.IsValid())
        {
            EntityEdit radSubsetEntEdit(p_radiossModel, subsetEditHandle);

            // Copy valid sub assembly ids
            sdiValueEntityList subAssems = GetValue<sdiValueEntityList>(*assemsSelect, "assemblies");
            const sdiUIntList& subAssemIds = subAssems.GetList();
            sdiUIntList subSubsetIds;
            for(unsigned int i = 0; i < subAssemIds.size(); ++i)
            {
                if(assemIds.count(subAssemIds[i]) > 0 && assemIds[subAssemIds[i]] == false)
                {
                    subSubsetIds.push_back(subAssemIds[i]);
                    assemIds[subAssemIds[i]] = true;
                }
            }
            if(subSubsetIds.size() > 0)
            {
                radSubsetEntEdit.SetValue(sdiIdentifier("numberofassemblies"),
                    sdiValue((int) subSubsetIds.size())); // should be automatic, but isn't yet
                radSubsetEntEdit.SetValue(sdiIdentifier("assemblies"),
                    sdiValue(sdiValueEntityList(subsetEntityType, subSubsetIds)));
            }

            // Store assemOfPart for use when creating the parts
            sdiValueEntityList parts = GetValue<sdiValueEntityList>(*assemsSelect, "components");
            const sdiUIntList& partIds = parts.GetList();
            for(unsigned int i = 0; i < partIds.size(); ++i)
            {
                assemOfPart[partIds[i]] = subsetId;
            }

            SDIHandlReadList sourceAssems = { {assemsSelect->GetHandle()} };
            Convert::PushToConversionLog(std::make_pair(subsetEditHandle, sourceAssems));
        }
    }

    SelectionRead partsSelect(p_lsdynaModel,srcCard);
    while (partsSelect.Next())
    {
        HandleRead matHandle;
        HandleRead propHandle;
        sdiString compName = partsSelect->GetName();

        partsSelect->GetEntityHandle(sdiIdentifier("SECID"), propHandle);

        HandleEdit partEditHandle;
        unsigned int partId = partsSelect->GetId();
        
        HandleRead p_PartBeingConverted = partsSelect->GetHandle();

        HandleRead partHRead;
        EntityRead partEntRead(p_lsdynaModel, p_PartBeingConverted);
        sdiString partCard = partEntRead.GetKeyword();
        p_radiossModel->CreateEntity(partEditHandle, destCard , compName, partId);
        if (partEditHandle.IsValid())
        {
            EntityEdit radPartEntEdit(p_radiossModel, partEditHandle);
            if( partCard.find("COMPOSITE") != string::npos)
            {
                partsSelect->GetEntityHandle(sdiIdentifier("MID",0,0), matHandle);
            }
            else
            {
                partsSelect->GetEntityHandle(sdiIdentifier("MID"), matHandle);
            }
            if (matHandle.IsValid())
                radPartEntEdit.SetValue(sdiIdentifier("mat_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/MAT"), matHandle.GetId(p_lsdynaModel))));

            /*mapping contact optional thickness*/
            if (partsSelect->GetKeyword().find("CONTACT") != string::npos)
            {
                sdiValue tempValue(0.0);
                partsSelect->GetValue(sdiIdentifier("OPTT"), tempValue);
                radPartEntEdit.SetValue(sdiIdentifier("THICK"), tempValue);
            }

            if(assemOfPart.count(partId) > 0)
            {
                radPartEntEdit.SetValue(sdiIdentifier("subset_ID"),
                    sdiValue(sdiValueEntity(subsetEntityType, assemOfPart[partId])));
            }
            SDIHandlReadList sourceParts = { {partsSelect->GetHandle()} };
            Convert::PushToConversionLog(std::make_pair(partEditHandle, sourceParts));
        }
    }
}

