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

#include <dyna2rad/convertconstrainedspotwelds.h>
#include <dyna2rad/dyna2rad.h>

using namespace std;
using namespace sdi;

void sdiD2R::ConvertSpotWeld::ConvertConstrainedSpotWelds()
{
    ConvertEntities();
}

void sdiD2R::ConvertSpotWeld::ConvertEntities()
{
    SelectionRead selConstSpotWeld(p_lsdynaModel, srcCard);
    map <sdiString, sdiDoubleList> failParamListMap;
    map <sdiString, vector<sdiUIntList>> elemNodesMap;
    map <sdiString, sdiConvert::SDIHandlReadList> sourceHandlesMap;
    while (selConstSpotWeld.Next())
    {
        // Check if *CONTROL_UNITS is in the model. If not, then add an error message
        SelectionRead selCtrlUnits(p_lsdynaModel, "*CONTROL_UNITS");
        if (selCtrlUnits.Count() == 0)
        {
            DynaToRad::ShowMessage(sdiMessageHandler::Level::Error, 41,
                selConstSpotWeld->GetKeyword().c_str(), selConstSpotWeld->GetId(), selConstSpotWeld->GetName().c_str());
        }

        unsigned int constrSpotWeldId = selConstSpotWeld->GetId();
        sdiValueEntity node1Entity;
        sdiValueEntity node2Entity;

        sdiValue tempVal(node1Entity);
        selConstSpotWeld->GetValue(sdiIdentifier("N1"), tempVal);
        tempVal.GetValue(node1Entity);
        unsigned int node1Id = node1Entity.GetId();

        tempVal = sdiValue(node2Entity);
        selConstSpotWeld->GetValue(sdiIdentifier("N2"), tempVal);
        tempVal.GetValue(node2Entity);
        unsigned int node2Id = node2Entity.GetId();

        if (node1Id && node2Id)
        {
            double lsdSN;
            double lsdSS;
            double lsdN;
            double lsdM;
            double lsdTF;
            sdiStringList doubleAttrName({ "SN", "SS", "N", "M", "TF" });
            vector<reference_wrapper<double>> doubleAttrVals({ lsdSN, lsdSS, lsdN, lsdM, lsdTF });
            p_ConvertUtils.GetAttribValues(*selConstSpotWeld, doubleAttrName, doubleAttrVals);
            sdiString sortingKey = to_string(lsdSN) + ":" + to_string(lsdSS) + ":" + to_string(lsdN) + ":" +  to_string(lsdM) + ":" + to_string(lsdTF);
            elemNodesMap[sortingKey].push_back({ {node1Id, node2Id} });
            failParamListMap[sortingKey] = sdiDoubleList({ lsdSN, lsdSS, lsdN, lsdM, lsdTF });
            sourceHandlesMap[sortingKey].push_back(selConstSpotWeld->GetHandle());
        }
    }

    for (auto tempPair : elemNodesMap)
    {
        sdiString mapKey = tempPair.first;
        vector <sdiUIntList> nodesList = tempPair.second;
        sdiDoubleList failParams = failParamListMap[mapKey];
        HandleEdit sprPartHedit;
        sdiConvert::SDIHandlReadList sourceHandleList = sourceHandlesMap[mapKey];
        p_radiossModel->CreateEntity(sprPartHedit, "/PART", "CONSTRAINED_SPOTWELD_" + mapKey);
        for(size_t i = 0; i< nodesList.size(); ++i)
        {
            HandleElementEdit sprHElemEdit;
            p_radiossModel->CreateElement(sprHElemEdit,"/SPRING",nodesList[i],sprPartHedit,0);
            //p_radiossModel->CreateElement(sprHElemEdit, "/SPRING");
            if (sprHElemEdit.IsValid())
            {
                sdiConvert::SDIHandlReadList sourceConsSpotWelds = { { sourceHandleList[i] } };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(sprHElemEdit, sourceConsSpotWelds));
            }
        }

        HandleEdit propHEdit;
        p_radiossModel->CreateEntity(propHEdit, "/PROP/TYPE25", "CONSTRAINED_SPOTWELD_" + mapKey, p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*SECTION")));
        EntityEdit propEdit(p_radiossModel, propHEdit);

        sprPartHedit.SetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), propHEdit);

        sdiStringList tempAttrNameList({ "Mass", "Inertia","K1", "K2", "K3", "K4", "DeltaMax1","DeltaMin1", "DeltaMax2", "DeltaMin2", "beta1", "beta2" });
        sdiDoubleList prop25Values;
        p_CurrentUnitSyst.GetDefaultProp25Vals(prop25Values);
        prop25Values.push_back(failParams[0]);
        prop25Values.push_back(-failParams[0]);
        prop25Values.push_back(failParams[1]);
        prop25Values.push_back(-failParams[1]);
        prop25Values.push_back(failParams[2]);
        prop25Values.push_back(failParams[3]);


        for (size_t i = 0; i < prop25Values.size(); ++i)
        {
            propEdit.SetValue(sdiIdentifier(tempAttrNameList[i]), sdiValue(prop25Values[i]));
        }
        propEdit.SetValue(sdiIdentifier("Ifail"), sdiValue(1));
        propEdit.SetValue(sdiIdentifier("Ifail2"), sdiValue(2));

        //if (lsdTF == 0.0)
        //    lsdTF = 10E20;

        //HandleEdit sensorHEdit;
        //p_radiossModel->CreateEntity(sensorHEdit, "/SENSOR/TIME", "CONSTRAINED_SPOTWELD_" + to_string(constrSpotWeldId));
        //if (sensorHEdit.IsValid())
        //{
        //    sensorHEdit.SetValue(p_radiossModel, sdiIdentifier("Tdelay"), sdiValue(lsdTF));
        //    propEdit.SetEntityHandle(sdiIdentifier("sens_ID"), sensorHEdit);
        //    propEdit.SetValue(sdiIdentifier("Isflag"), sdiValue(1));
        //}

        if (sprPartHedit.IsValid())
            sdiConvert::Convert::PushToConversionLog(std::make_pair(sprPartHedit, sourceHandleList));
        if (propHEdit.IsValid())
            sdiConvert::Convert::PushToConversionLog(std::make_pair(propHEdit, sourceHandleList));
        //if (sensorHEdit.IsValid())
        //    sdiConvert::Convert::PushToConversionLog(std::make_pair(sensorHEdit, sourceHandleList));
    }
}
