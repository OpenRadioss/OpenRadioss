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

#include <dyna2rad/convertconstrainedinterpolations.h>
#include <dyna2rad/dyna2rad.h>

using namespace std;
using namespace sdi;

void sdiD2R::ConvertInterpolation::ConvertConstrainedInterpolations()
{
    ConvertEntities();
}

void sdiD2R::ConvertInterpolation::ConvertEntities()
{
    EntityType radRbe3Type = p_radiossModel->GetEntityType("/RBE3");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    SelectionRead selInterpolationGlobal(p_lsdynaModel, "*CONSTRAINED_INTERPOLATION");

    while (selInterpolationGlobal.Next())
    {
        unsigned int constrainedInterpolationId = selInterpolationGlobal->GetId();
        sdiString constrainedInterpolationName = selInterpolationGlobal->GetName();

        int DNID = 0;
        sdiValue tempVal(DNID);
        selInterpolationGlobal->GetValue(sdiIdentifier("DNID"), tempVal);
        tempVal.GetValue(DNID);

        HandleRead nodeHandle;
        selInterpolationGlobal->GetEntityHandle(sdiIdentifier("DNID"), nodeHandle);

        int ITYP = 0;
        tempVal = sdiValue(ITYP);
        selInterpolationGlobal->GetValue(sdiIdentifier("LSD_ITYP"), tempVal);
        tempVal.GetValue(ITYP);

        int DDOF = 0;
        tempVal = sdiValue(DDOF);
        selInterpolationGlobal->GetValue(sdiIdentifier("DDOF"), tempVal);
        tempVal.GetValue(DDOF);

        int radiossDDof[6] = {0, 0, 0, 0, 0, 0} ;
        while(DDOF >=1)
        {
            if(DDOF%10 <= 6) radiossDDof[DDOF%10 - 1] = 1;
            DDOF=DDOF/10;
        }

        HandleRead skewHandle;
        selInterpolationGlobal->GetEntityHandle(sdiIdentifier("CIDD"), skewHandle);
        
        int independentnodesmax = 0;
        tempVal = sdiValue(independentnodesmax);
        selInterpolationGlobal->GetValue(sdiIdentifier("independentnodesmax"), tempVal);
        tempVal.GetValue(independentnodesmax);

        HandleEdit constrainedHEdit;
        p_radiossModel->CreateEntity(constrainedHEdit, "/RBE3", selInterpolationGlobal->GetName(), constrainedInterpolationId);
        EntityEdit constrainedEdit(p_radiossModel, constrainedHEdit);

        sdiUIntList nodesToAddIdList;
        sdiValueEntityList nodesToAddList;
        selInterpolationGlobal->GetValue(sdiIdentifier("independentnodes"), tempVal);
        tempVal.GetValue(nodesToAddList);
        nodesToAddList.GetIdList(nodesToAddIdList);

        sdiUIntList setsToAddIdList;
        sdiValueEntityList setsToAddList;
        selInterpolationGlobal->GetValue(sdiIdentifier("independentnodesets"), tempVal);
        tempVal.GetValue(setsToAddList);
        setsToAddList.GetIdList(setsToAddIdList);

        constrainedEdit.SetValue(sdiIdentifier("nset"), sdiValue(1));
        constrainedEdit.SetValue(sdiIdentifier("Node_IDr"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/NODE"), nodeHandle.GetId(p_lsdynaModel))));

        constrainedEdit.SetValue(sdiIdentifier("LTX"), sdiValue(radiossDDof[0]));
        constrainedEdit.SetValue(sdiIdentifier("LTY"), sdiValue(radiossDDof[1]));
        constrainedEdit.SetValue(sdiIdentifier("LTZ"), sdiValue(radiossDDof[2]));
        constrainedEdit.SetValue(sdiIdentifier("LRX"), sdiValue(radiossDDof[3]));
        constrainedEdit.SetValue(sdiIdentifier("LRY"), sdiValue(radiossDDof[4]));
        constrainedEdit.SetValue(sdiIdentifier("LRZ"), sdiValue(radiossDDof[5]));

        constrainedEdit.SetValue(sdiIdentifier("N_set"), sdiValue(1));
        constrainedEdit.SetValue(sdiIdentifier("I_Modif"), sdiValue(2));

        switch (ITYP)
        {
        case 0:
        {
            HandleEdit radSetHEdit;
            p_radiossModel->CreateEntity(radSetHEdit, "/SET/GENERAL", constrainedInterpolationName);
            EntityEdit SetEntityEdit(p_radiossModel, radSetHEdit);

            SetEntityEdit.SetValue(sdiIdentifier("iset_Type"), sdiValue(0));
            SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue((int) nodesToAddIdList.size())); 
            if (nodesToAddIdList.size()) SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/NODE"), nodesToAddIdList)));

            constrainedEdit.SetEntityHandle(sdiIdentifier("independentnodesets",0 ,0), radSetHEdit);

            sdiConvert::SDIHandlReadList sourceConstrained = { {selInterpolationGlobal->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radSetHEdit, sourceConstrained));
            break;
        }
        default:
            if (setsToAddIdList.size())
            {
                constrainedEdit.SetValue(sdiIdentifier("independentnodesets", 0, 0),
                    sdiValue(sdiValueEntity(sdiValueEntityType(radSetType), setsToAddIdList[0])));
            }
            break;
        }

        if (skewHandle.GetId(p_lsdynaModel) > 0) constrainedEdit.SetValue(sdiIdentifier("SKEW_ARRAY", 0, 0), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SKEW"), skewHandle.GetId(p_lsdynaModel))));

        sdiIntList  idofIntList;
        selInterpolationGlobal->GetValue(sdiIdentifier("independentdofs"), tempVal);
        tempVal.GetValue(idofIntList);

        if(idofIntList.size() > 0)
        {
            int IDOF = idofIntList[0];
            int radiossIDof[6] = {0, 0, 0, 0, 0, 0} ;
            while(IDOF >=1)
            {
                if(IDOF%10 <= 6) radiossIDof[IDOF%10 - 1] = 1;
                IDOF=IDOF/10;
            }

            constrainedEdit.SetValue(sdiIdentifier("tx"), sdiValue(radiossIDof[0]));
            constrainedEdit.SetValue(sdiIdentifier("ty"), sdiValue(radiossIDof[1]));
            constrainedEdit.SetValue(sdiIdentifier("tz"), sdiValue(radiossIDof[2]));
            constrainedEdit.SetValue(sdiIdentifier("rx"), sdiValue(radiossIDof[3]));
            constrainedEdit.SetValue(sdiIdentifier("ry"), sdiValue(radiossIDof[4]));
            constrainedEdit.SetValue(sdiIdentifier("rz"), sdiValue(radiossIDof[5]));
        }


        sdiIntList  twghtxList;
        selInterpolationGlobal->GetValue(sdiIdentifier("LSD_TWGHTX"), tempVal);
        tempVal.GetValue(twghtxList);

        if(twghtxList.size() > 0)
        { 
            constrainedEdit.SetValue(sdiIdentifier("independentnodesetcoeffs"), sdiValue(twghtxList[0]));
        }


        if (constrainedHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceConstrained = { {selInterpolationGlobal->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(constrainedHEdit, sourceConstrained));
        }
    }

}
