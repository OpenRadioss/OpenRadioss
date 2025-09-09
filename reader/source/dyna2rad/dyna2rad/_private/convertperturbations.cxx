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

#include <dyna2rad/convertperturbations.h>
#include <dyna2rad/dyna2rad.h>

using namespace sdi;
using namespace std;

void sdiD2R::ConvertPerturbation::ConvertAllPerturbations()
{
    ConvertEntities();
}

void sdiD2R::ConvertPerturbation::ConvertEntities()
{
    ConvertPerturbationNode();
}

void sdiD2R::ConvertPerturbation::ConvertPerturbationNode()
{
    EntityType radRandomType = p_radiossModel->GetEntityType("/RANDOM");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    SelectionRead selPerturbationNodeGlobal(p_lsdynaModel, "*PERTURBATION_NODE");

    while (selPerturbationNodeGlobal.Next())
    {
        sdiString PerturbationNodeName = selPerturbationNodeGlobal->GetName();
        unsigned int PerturbationNodeGlobalId = selPerturbationNodeGlobal->GetId();
        sdiString PerturbationNodeCard  = selPerturbationNodeGlobal->GetKeyword();


        if (!p_radiossModel->IsIdAvailable(radRandomType, PerturbationNodeGlobalId))
            PerturbationNodeGlobalId = p_ConvertUtils.GetDynaMaxEntityID(srcEntityType);
        HandleEdit randomHEdit;

        int TYPE = 0;
        sdiValue tempVal(TYPE);
        selPerturbationNodeGlobal->GetValue(sdiIdentifier("TYPE"), tempVal);
        tempVal.GetValue(TYPE);

        if (TYPE == 3) 
        {
            DynaToRad::ShowMessage(sdiMessageHandler::Level::Warning, 25,
            PerturbationNodeCard.c_str(), PerturbationNodeGlobalId, PerturbationNodeName.c_str());
        }
        else
        {
            sdiValueEntity nsidEntity;
            sdiValue tempVal1(nsidEntity);
            selPerturbationNodeGlobal->GetValue(sdiIdentifier("NSID"), tempVal1);
            tempVal1.GetValue(nsidEntity);
            unsigned int nsid = nsidEntity.GetId();

            double lsdSCL = 0;
            tempVal = sdiValue(lsdSCL);
            selPerturbationNodeGlobal->GetValue(sdiIdentifier("SCL"), tempVal);
            tempVal.GetValue(lsdSCL);

            double lsdAMPL = 0;
            tempVal = sdiValue(lsdAMPL);
            selPerturbationNodeGlobal->GetValue(sdiIdentifier("AMPL"), tempVal);
            tempVal.GetValue(lsdAMPL);

            HandleEdit radSetHEdit;
            p_radiossModel->CreateEntity(radSetHEdit, "/SET/GENERAL", PerturbationNodeName);
            EntityEdit SetEntityEdit(p_radiossModel, radSetHEdit);

            SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));

            if (nsid > 0)
            {
                p_radiossModel->CreateEntity(randomHEdit, "/RANDOM/GRNOD", selPerturbationNodeGlobal->GetName());
            }
            else
            {
                p_radiossModel->CreateEntity(randomHEdit, "/RANDOM", selPerturbationNodeGlobal->GetName());
            }

            EntityEdit randomEdit(p_radiossModel, randomHEdit);

            if (nsid > 0)  randomEdit.SetValue(sdiIdentifier("GRNOD_ID"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(nsid, "*SET_NODE" ))));
             
            if (TYPE == 1 || TYPE == 8)
            {
                randomEdit.SetValue(sdiIdentifier("XALEA"), sdiValue(lsdSCL*lsdAMPL));
            }
            else
            {
                randomEdit.SetValue(sdiIdentifier("XALEA"), sdiValue(lsdSCL));
            }


            if (randomHEdit.IsValid())
            {
                sdiConvert::SDIHandlReadList sourcePerturbationNode = { {selPerturbationNodeGlobal->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(randomHEdit, sourcePerturbationNode));
            }
        }
    }
}


