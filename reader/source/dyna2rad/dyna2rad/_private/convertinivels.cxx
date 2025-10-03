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

#include <dyna2rad/convertinivels.h>
#include <dyna2rad/dyna2rad.h>
#include <dyna2rad/sdiUtils.h>

using namespace sdi;
using namespace std;

static void ConvertIniVelToLocalCoordinates(ModelViewRead* lsdyaModel, const EntityRead& Inivel, const sdiD2R::ConvertUtils& convertUtils, EntityEdit& radInivelEdit);

void sdiD2R::ConvertInitialVelocity::ConvertInitialVelocities()
{
    ConvertEntities();
}

void sdiD2R::ConvertInitialVelocity::PopulateRbodyDetails()
{
    SelectionRead selCnrb(p_lsdynaModel, "*CONSTRAINED_NODAL_RIGID_BODY");
    while (selCnrb.Next())
    {
        bool isInertia = false;
        if (selCnrb->GetKeyword().find("INERTIA") != string::npos)
            isInertia = true;
        mapCNRBIdVsIsInerta[selCnrb->GetId()] = isInertia;
    }

    SelectionRead selRbody(p_radiossModel, "/RBODY");
    while (selRbody.Next())
    {
        HandleRead rbodyNsetHread;
        selRbody->GetEntityHandle(sdiIdentifier("grnd_ID"), rbodyNsetHread);
        if (rbodyNsetHread.IsValid())
        {
            sdiString rbodySetkeyType;
            sdiValue tempVal(rbodySetkeyType);
            rbodyNsetHread.GetValue(p_radiossModel, sdiIdentifier("KEY_type", 0, 0), tempVal);
            tempVal.GetValue(rbodySetkeyType);
            sdiUIntList rbodyEntityList;
            if (rbodySetkeyType == "PART")
            {
                int idsMax = 0;
                tempVal = sdiValue(idsMax);
                rbodyNsetHread.GetValue(p_radiossModel, sdiIdentifier("idsmax"), tempVal);
                tempVal.GetValue(idsMax);
                for (int j = 0; j < idsMax; ++j)
                {
                    sdiValueEntity entity;
                    tempVal = sdiValue(entity);
                    rbodyNsetHread.GetValue(p_radiossModel, sdiIdentifier("ids", 0, 0, j), tempVal);
                    tempVal.GetValue(entity);
                    rbodyEntityList.push_back(entity.GetId());
                }
            }
            else if (rbodySetkeyType == "NODE")
                p_ConvertUtils.ExtractNodesFromRadiossSet(rbodyNsetHread, rbodyEntityList);
            rbodyDetails[selRbody->GetId()].first = rbodySetkeyType;
            rbodyDetails[selRbody->GetId()].second = rbodyEntityList;
        }
    }

    SelectionRead selLsdPart(p_lsdynaModel, "*PART");
    while (selLsdPart.Next())
    {
        bool isInerta = false;
        if (selLsdPart->GetKeyword().find("INERTIA") != string::npos)
            isInerta = true;
        mapLsdPartVsIsInerta[selLsdPart->GetId()] = isInerta;
    }
}

void sdiD2R::ConvertInitialVelocity::ConvertEntities()
{
    PopulateRbodyDetails();

    ConvertInivelGeneration();

    ConvertInitialVelocityNode();

    ConvertInitialVelocityNone();
}

void sdiD2R::ConvertInitialVelocity::ConvertInivelGeneration()
{
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    EntityType radFrameType = p_radiossModel->GetEntityType("/FRAME");
    SelectionRead selInivel(p_lsdynaModel, "*INITIAL_VELOCITY_GENERATION");
    while (selInivel.Next())
    {
        sdiString keyWord = selInivel->GetKeyword();
        if (keyWord.find("START") != keyWord.npos)
            continue;
        double lsdVX;
        double lsdVY;
        double lsdVZ;
        double lsdOmega;
        double lsdXC;
        double lsdYC;
        double lsdZC;
        double lsdNX;
        double lsdNY;
        double lsdNZ;
        int setType = -1;
        unsigned int inivelId = selInivel->GetId();

        vector<reference_wrapper<double>> attribVals({ lsdVX, lsdVY, lsdVZ, lsdOmega, lsdXC, lsdYC, lsdZC, lsdNX, lsdNY, lsdNZ });
        vector<sdiString> attribNames({ "VX", "VY", "VZ", "OMEGA", "XC", "YC", "ZC", "LSD_NX", "LSD_NY", "LSD_NZ" });
        p_ConvertUtils.GetAttribValues(*selInivel, attribNames, attribVals);

        HandleEdit radInivelHEdit;
        if (p_radiossModel->IsIdAvailable(destEntityType, inivelId) && inivelId != 0)
            p_radiossModel->CreateEntity(radInivelHEdit, "/INIVEL/AXIS", selInivel->GetName(), inivelId);
        else
            p_radiossModel->CreateEntity(radInivelHEdit, "/INIVEL/AXIS", selInivel->GetName(), p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*INITIAL_VELOCITY")));
        if (radInivelHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceInivel = { {selInivel->GetHandle()} };

            EntityEdit radInivelEdit(p_radiossModel, radInivelHEdit);
            radInivelEdit.SetValue(sdiIdentifier("displayname"), sdiValue(selInivel->GetName()));

            radInivelEdit.SetValue(sdiIdentifier("DIR"), sdiValue(sdiString("Z")));
            HandleRead nodeNyHRead;
            HandleRead nodeNzHRead;
            selInivel->GetEntityHandle(sdiIdentifier("plane_node1"), nodeNyHRead);
            selInivel->GetEntityHandle(sdiIdentifier("plane_node2"), nodeNzHRead);

            radInivelEdit.SetValue(sdiIdentifier("VR"), sdiValue(lsdOmega));

            sdiValueEntity csidEntity = GetValue<sdiValueEntity>(*selInivel, "ICID");
            unsigned int csId=csidEntity.GetId();

            sdiValue tempValue;
            sdiTriple origVect(0.0, 0.0, 0.0); // origin of ICID
            sdiTriple Vect1(0.0, 0.0, 0.0);  // (Reference Y axis) is the second direction of ICID
            sdiTriple Vect2(0.0, 0.0, 0.0);  // (Reference Z axis) is the third direction of ICID
            sdiTriple Vect3(0.0, 0.0, 0.0);  // (Reference X axis) is the first direction of ICID

            sdiStringList attribOrigin({ "Ox", "Oy", "Oz" });
            sdiStringList attribVect1({ "X1", "Y1", "Z1" });
            sdiStringList attribVect2({ "X2", "Y2", "Z2" });
            
            if(csId > 0)
            {
                // Reference frame is ICID
                EntityType radskewType = p_radiossModel->GetEntityType("/SKEW/FIX");
                HandleRead radskewHread;
                p_radiossModel->FindById(radskewType, csId, radskewHread);
                if(radskewHread.IsValid())
                {
                    EntityRead radskewRead(p_radiossModel, radskewHread);

                    for (size_t i = 0; i < 3; ++i)
                    {
                        tempValue = sdiValue(origVect[i]);
                        radskewRead.GetValue(sdiIdentifier(attribOrigin[i]), tempValue);
                        tempValue.GetValue(origVect[i]);

                        tempValue = sdiValue(Vect1[i]);
                        radskewRead.GetValue(sdiIdentifier(attribVect1[i]), tempValue);
                        tempValue.GetValue(Vect1[i]);

                        tempValue = sdiValue(Vect2[i]);
                        radskewRead.GetValue(sdiIdentifier(attribVect2[i]), tempValue);
                        tempValue.GetValue(Vect2[i]);

                        Vect3 = Vect1 * Vect2;
                        Vect3 = Vect3.Normalize();

                    }
                }
            }
            else
            {
                // Reference frame is global system
                Vect1 = sdiTriple(0.0, 1.0, 0.0);  // (Reference Y axis) is the second direction
                Vect2 = sdiTriple(0.0, 0.0, 1.0);  // (Reference Z axis) is the third direction
                Vect3 = sdiTriple(1.0, 0.0, 0.0);  // (Reference X axis) is the first direction
            }

            //-----------------------
            // Create local FRAME/FIX
            //-----------------------

            HandleEdit frameHandle;
            p_radiossModel->CreateEntity(frameHandle, "/FRAME/FIX", "FRAME_FIX_INIVEL_GEN_" + to_string(inivelId),
            p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*DEFINE_COORDINATE")));
            if (frameHandle.IsValid())
            {
                EntityEdit frameEntityEdit(p_radiossModel, frameHandle);
                frameEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue("FRAME_FIX_INIVEL_GEN_" + to_string(inivelId)));
                sdiTriple Frameorigin(lsdXC, lsdYC, lsdZC);
                sdiTriple FrameVect1(0.0, 0.0, 0.0);  // local fix frame Y axis is the second direction
                sdiTriple FrameVect2(0.0, 0.0, 0.0);  // local fix frame Z axis is the third direction
                sdiTriple FrameVect3(0.0, 0.0, 0.0);  // local fix frame X axis is the first direction

 	            if (lsdNX == 0.0  && lsdNY == 0.0 && lsdNZ == 0.0)
                {
                    FrameVect2 = Vect2;  // z-direction
                    FrameVect1 = FrameVect2 * Vect3;  // y-direction
                    FrameVect1 = FrameVect1.Normalize();
                    FrameVect3 = FrameVect1 * FrameVect2;  // X-direction
                    FrameVect3 = FrameVect3.Normalize();
                    radInivelEdit.SetValue(sdiIdentifier("VR"), sdiValue(0.0));
                }
                else if(lsdNX == -999)
                {
                    NodeRead nodeNyRead(p_lsdynaModel, nodeNyHRead);
                    NodeRead nodeNzRead(p_lsdynaModel, nodeNzHRead);
                    Frameorigin = nodeNyRead.GetPosition();
                    FrameVect2 = nodeNzRead.GetPosition() - Frameorigin;  // z-direction
                    FrameVect2 = FrameVect2.Normalize();
                    FrameVect1 = FrameVect2 * Vect3;  // y-direction
                    if (FrameVect1 == sdiTriple(0.0, 0.0, 0.0))
                    {
                        FrameVect1 = FrameVect2 * Vect2;
                    }
                    FrameVect1 = FrameVect1.Normalize();
                    FrameVect3 = FrameVect1 * FrameVect2;  // X-direction
                    FrameVect3 = FrameVect3.Normalize();
                }
                else
                {
                    FrameVect2 = sdiTriple(lsdNX, lsdNY, lsdNZ);  // z-direction
                    FrameVect2 = FrameVect2.Normalize();
                    FrameVect1 = FrameVect2 * Vect3;  // y-direction
                    if (FrameVect1 == sdiTriple(0.0, 0.0, 0.0))
                    {
                        FrameVect1 = FrameVect2 * Vect2;
                    }
                    FrameVect1 = FrameVect1.Normalize();
                    FrameVect3 = FrameVect1 * FrameVect2;  // X-direction
                    FrameVect3 = FrameVect3.Normalize();
                }
                for (size_t i = 0; i < 3; ++i)
                {
                    frameEntityEdit.SetValue(sdiIdentifier(attribOrigin[i]), sdiValue(Frameorigin[i]));
                    frameEntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(FrameVect1[i]));
                    frameEntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(FrameVect2[i]));
                }

                // othogonal projection of the velocity vector (VX, VY, VZ) on the local fix frame

                FrameVect1 = FrameVect2 * FrameVect3;
                FrameVect1 = FrameVect1.Normalize();

                double Vx = FrameVect3[0] * lsdVX + FrameVect1[0] * lsdVY + FrameVect2[0] * lsdVZ;
                double Vy = FrameVect3[1] * lsdVX + FrameVect1[1] * lsdVY + FrameVect2[1] * lsdVZ;
                double Vz = FrameVect3[2] * lsdVX + FrameVect1[2] * lsdVY + FrameVect2[2] * lsdVZ;

                radInivelEdit.SetValue(sdiIdentifier("Vxt"), sdiValue(Vx));
                radInivelEdit.SetValue(sdiIdentifier("Vyt"), sdiValue(Vy));
                radInivelEdit.SetValue(sdiIdentifier("Vzt"), sdiValue(Vz));

                radInivelEdit.SetValue(sdiIdentifier("inputsystem"), sdiValue(sdiValueEntity(radFrameType, frameEntityEdit.GetId())));
            }
            
            int setType = GetValue<int>(*selInivel, "STYP");

            HandleEdit allNodesetEdit;
            HandleEdit setEdit;
            HandleRead psidHRead;
            HandleRead lsdNSetHRead;
            switch (setType)
            {
            case 0:
            {
                p_radiossModel->CreateEntity(allNodesetEdit, "/SET/GENERAL", "ALL_NODE_SET_GENERAL_INIVEL_GEN");
                EntityEdit SetEntityEdit(p_radiossModel, allNodesetEdit);

                SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));
                radInivelEdit.SetEntityHandle(sdiIdentifier("GRNOD_ID"), allNodesetEdit);
                break;
            }
            case 1:
            {
                /*part set*/
                selInivel->GetEntityHandle(sdiIdentifier("PSID"), psidHRead);
                if (psidHRead.IsValid())
                {
                    EntityRead psidRead(p_lsdynaModel, psidHRead);
                    radInivelEdit.SetValue(sdiIdentifier("GRNOD_ID"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(psidRead.GetId(), "*SET_PART" ))));
                }
                else
                {
                    if (!allNodesetEdit.IsValid())
                    {
                        p_radiossModel->CreateEntity(allNodesetEdit, "/SET/GENERAL", "ALL_NODE_SET_GENERAL_INIVEL_GEN" + to_string(inivelId));
                        EntityEdit SetEntityEdit(p_radiossModel, allNodesetEdit);

                        SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                        SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));
                    }
                    radInivelEdit.SetEntityHandle(sdiIdentifier("GRNOD_ID"), allNodesetEdit);
                }
                break;
            }
            case 2:
            {
                /*component*/
                sdiValueEntity lsdPart = GetValue<sdiValueEntity>(*selInivel, "PID");
                unsigned int lsdPartId=lsdPart.GetId();
                /* create /set with part id */
                if (lsdPartId)
                {
                    p_radiossModel->CreateEntity(setEdit, "/SET/GENERAL", "SET_GENERAL_INIVEL_GEN_" + to_string(inivelId));
                    EntityEdit SetEntityEdit(p_radiossModel, setEdit);

                    SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                    SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
                    SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                    SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/PART"), sdiUIntList(1, lsdPartId))));
                    radInivelEdit.SetEntityHandle(sdiIdentifier("GRNOD_ID"), setEdit);
                }
                else
                {
                    if (!allNodesetEdit.IsValid())
                    {
                        p_radiossModel->CreateEntity(allNodesetEdit, "/SET/GENERAL", "ALL_NODE_SET_GENERAL_INIVEL_GEN" + to_string(inivelId));
                        EntityEdit SetEntityEdit(p_radiossModel, allNodesetEdit);

                        SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                        SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));
                    }
                    radInivelEdit.SetEntityHandle(sdiIdentifier("GRNOD_ID"), allNodesetEdit);
                }
                break;
            }
            case 3:
            {
                /*node set*/
                selInivel->GetEntityHandle(sdiIdentifier("LSD_NSID"), lsdNSetHRead);
                if (lsdNSetHRead.IsValid())
                {
                    EntityRead lsdNSetRead(p_lsdynaModel, lsdNSetHRead);
                    radInivelEdit.SetValue(sdiIdentifier("GRNOD_ID"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(lsdNSetRead.GetId(), "*SET_NODE" ))));
                }
                else
                {
                    if (!allNodesetEdit.IsValid())
                    {
                        p_radiossModel->CreateEntity(allNodesetEdit, "/SET/GENERAL", "ALL_NODE_SET_GENERAL_INIVEL_GEN");
                        EntityEdit SetEntityEdit(p_radiossModel, allNodesetEdit);

                        SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                        SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));
                    }
                    radInivelEdit.SetEntityHandle(sdiIdentifier("GRNOD_ID"), allNodesetEdit);
                }

                break;
            }
            default:
                break;
            }


            sdiConvert::Convert::PushToConversionLog(std::make_pair(radInivelHEdit, sourceInivel));

            if (setEdit.IsValid())
            {
                HandleIrgidOption(*selInivel, setEdit);
                sdiConvert::Convert::PushToConversionLog(std::make_pair(setEdit, sourceInivel));
            }
            else if (allNodesetEdit.IsValid())
            {
                HandleIrgidOption(*selInivel, allNodesetEdit);
                sdiConvert::Convert::PushToConversionLog(std::make_pair(allNodesetEdit, sourceInivel));
            }
            else if (psidHRead.IsValid())
            {
                EntityRead psidRead(p_lsdynaModel, psidHRead);
                unsigned int radPsid = DynaToRad::GetRadiossSetIdFromLsdSet(psidRead.GetId(), "*SET_PART" );
                HandleEdit foundRadSetHEdit;
                if (p_radiossModel->FindById(radSetType, radPsid, foundRadSetHEdit))
                    HandleIrgidOption(*selInivel, foundRadSetHEdit);
            }            
            else if (lsdNSetHRead.IsValid())
            {
                EntityRead lsdNSetRead(p_lsdynaModel, lsdNSetHRead);
                unsigned int radNSetId = DynaToRad::GetRadiossSetIdFromLsdSet(lsdNSetRead.GetId(), "*SET_NODE" );
                HandleEdit foundRadSetHEdit;
                if (p_radiossModel->FindById(radSetType, radNSetId, foundRadSetHEdit))
                    HandleIrgidOption(*selInivel, foundRadSetHEdit);
            }
        }
    }
}

void sdiD2R::ConvertInitialVelocity::ConvertInitialVelocityNode()
{
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");

    SelectionRead selInivel(p_lsdynaModel, "*INITIAL_VELOCITY_NODE");
    while (selInivel.Next())
    {
        sdiString keyWord = selInivel->GetKeyword();
        double lsdVX;
        double lsdVY;
        double lsdVZ;
        double lsdVXR;
        double lsdVYR;
        double lsdVZR;

        vector<reference_wrapper<double>> attribVals({ lsdVX, lsdVY, lsdVZ, lsdVXR, lsdVYR, lsdVZR });
        vector<sdiString> attribNames({ "VX", "VY", "VZ", "VXR", "VYR", "VZR" });
        p_ConvertUtils.GetAttribValues(*selInivel, attribNames, attribVals);

        sdiTriple traVel(lsdVX, lsdVY, lsdVZ);
        sdiTriple rotVel(lsdVXR, lsdVYR, lsdVZR);
        sdiValueEntity nodeEntity;
        sdiValue tempVal(nodeEntity);
        selInivel->GetValue(sdiIdentifier("NID"), tempVal);
        tempVal.GetValue(nodeEntity);
        unsigned int nodeId = nodeEntity.GetId();

        if (nodeId)
        {
            sdiStringList radVelAttrNames({ "Vx", "Vy", "Vz" });
            sdiString inivelName = selInivel->GetName();
            unsigned int inivelId = selInivel->GetId();
            HandleEdit setHEdit;
            p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", "SET_GEN_INIVEL_NODE_" + to_string(inivelId));
            EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

            SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, sdiUIntList(1, nodeId))));

            bool isInivelRraCreated = false;

            if (lsdVX != 0.0 || lsdVY != 0.0 || lsdVZ != 0.0)
            {
                HandleEdit inivelHEdit;
                if (p_radiossModel->IsIdAvailable(destEntityType, inivelId) && inivelId != 0)
                    p_radiossModel->CreateEntity(inivelHEdit, "/INIVEL/TRA", inivelName, inivelId);
                else
                    p_radiossModel->CreateEntity(inivelHEdit, "/INIVEL/TRA", inivelName, p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*INITIAL_VELOCITY")));
                EntityEdit inivelEdit(p_radiossModel, inivelHEdit);
                //sdiString strFromVelVect;
                for (size_t j = 0; j < 3; ++j)
                {
                    //strFromVelVect += ":" + to_string(traVel[j]);
                    inivelEdit.SetValue(sdiIdentifier(radVelAttrNames[j]), sdiValue(traVel[j]));
                }
                inivelEdit.SetEntityHandle(sdiIdentifier("Gnod_id"), setHEdit);

                inivelEdit.SetValue(sdiIdentifier("displayname"), sdiValue(inivelName));
                inivelEdit.SetValue(sdiIdentifier("rad_inivel_type"), sdiValue(sdiString("TRA")));

                isInivelRraCreated = true;

                if (inivelHEdit.IsValid())
                {
                    sdiConvert::SDIHandlReadList sourceInivel = { {selInivel->GetHandle()} };
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(inivelHEdit, sourceInivel));
                }
            }
            if (lsdVXR != 0.0 || lsdVYR != 0.0 || lsdVZR != 0.0)
            {
                HandleEdit inivelHEdit;
                if (p_radiossModel->IsIdAvailable(destEntityType, inivelId) && inivelId != 0)
                    p_radiossModel->CreateEntity(inivelHEdit, "/INIVEL/ROT", inivelName, inivelId);
                else
                    p_radiossModel->CreateEntity(inivelHEdit, "/INIVEL/ROT", inivelName, p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*INITIAL_VELOCITY")));
                EntityEdit inivelEdit(p_radiossModel, inivelHEdit);
                //sdiString strFromVelVect;
                for (size_t j = 0; j < 3; ++j)
                {
                    //strFromVelVect += ":" + to_string(rotVel[j]);
                    inivelEdit.SetValue(sdiIdentifier(radVelAttrNames[j]), sdiValue(rotVel[j]));
                }
                inivelEdit.SetEntityHandle(sdiIdentifier("Gnod_id"), setHEdit);

                inivelEdit.SetValue(sdiIdentifier("displayname"), sdiValue(inivelName));
                inivelEdit.SetValue(sdiIdentifier("rad_inivel_type"), sdiValue(sdiString("ROT")));

                if (inivelHEdit.IsValid())
                {
                    sdiConvert::SDIHandlReadList sourceInivel = { {selInivel->GetHandle()} };
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(inivelHEdit, sourceInivel));
                }
            }
            SelectionRead selRbody(p_radiossModel, "/RBODY");
            sdiUIntList masterNodesToAdd;
            masterNodesToAdd.reserve(selRbody.Count());
            while (selRbody.Next())
            {
                bool updateNeeded = true;
                HandleRead rbodyNsetHread;
                selRbody->GetEntityHandle(sdiIdentifier("grnd_ID"), rbodyNsetHread);
                if (rbodyNsetHread.IsValid())
                {
                    sdiUIntList rbodyNodeList;
                    p_ConvertUtils.ExtractNodesFromRadiossSet(rbodyNsetHread, rbodyNodeList);

                    auto itr = find(rbodyNodeList.begin(), rbodyNodeList.end(), nodeId);
                    if (itr != rbodyNodeList.end())
                    {
                        sdiValueEntity mastNodeEntity;
                        tempVal = sdiValue(mastNodeEntity);
                        selRbody->GetValue(sdiIdentifier("node_ID"), tempVal);
                        tempVal.GetValue(mastNodeEntity);
                        unsigned int mastNode = mastNodeEntity.GetId();
                        if (mastNode)
                            masterNodesToAdd.push_back(mastNode);
                    }
                }
            }
            if (!masterNodesToAdd.empty())
            {
                EntityEdit setEdit(p_radiossModel, setHEdit);
                setEdit.SetValue(sdiIdentifier("KEY_type", 0, 1), sdiValue(sdiString("NODE")));
                setEdit.SetValue(sdiIdentifier("idsmax", 0, 1), sdiValue((int)masterNodesToAdd.size()));
                setEdit.SetValue(sdiIdentifier("ids", 0, 1), sdiValue(sdiValueEntityList(1, masterNodesToAdd)));
                setEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(2));
                RemoveRbodyMasterNodeFromInivelSet(masterNodesToAdd);
            }
        }
    }
}

void sdiD2R::ConvertInitialVelocity::ConvertInitialVelocityNone()
{
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    SelectionRead selInivel(p_lsdynaModel, "*INITIAL_VELOCITY");
    sdiUIntList rigidNodes;
    rigidNodes.reserve(selInivel.Count());
    while (selInivel.Next())
    {
        sdiString keyWord = selInivel->GetKeyword();
        if (keyWord.find("NODE") != keyWord.npos || keyWord.find("GENERATION") != keyWord.npos)
            continue;

        double lsdVX;
        double lsdVY;
        double lsdVZ;
        double lsdVXR;
        double lsdVYR;
        double lsdVZR;

        vector<reference_wrapper<double>> attribVals({ lsdVX, lsdVY, lsdVZ, lsdVXR, lsdVYR, lsdVZR });
        vector<sdiString> attribNames({ "VX", "VY", "VZ", "VXR", "VYR", "VZR" });
        p_ConvertUtils.GetAttribValues(*selInivel, attribNames, attribVals);

        sdiTriple traVel(lsdVX, lsdVY, lsdVZ);
        sdiTriple rotVel(lsdVXR, lsdVYR, lsdVZR);

        unsigned int inivelId = selInivel->GetId();
        sdiValueEntity csidEntity;
        sdiValue tempVal(csidEntity);
        selInivel->GetValue(sdiIdentifier("ICID"), tempVal);
        tempVal.GetValue(csidEntity);
        EntityType radSkewType(p_radiossModel->GetEntityType("/SKEW"));

        unsigned int nodeSetId = 0;
        unsigned int nodeExcludeSetId = 0;
        HandleEdit setHEdit;
        if (keyWord.find("RIGID") != keyWord.npos)
        {
            sdiValueEntity pidEntity;
            tempVal = sdiValue(pidEntity);
            selInivel->GetValue(sdiIdentifier("PID"), tempVal);
            tempVal.GetValue(pidEntity);
            unsigned int pid = pidEntity.GetId();
            unsigned int nodeId = 0;

            if (DynaToRad::storeRbodyPIDVsMasterNode.find(pid) != DynaToRad::storeRbodyPIDVsMasterNode.end())
            {
                nodeId = DynaToRad::storeRbodyPIDVsMasterNode[pid];
            }
            else if (DynaToRad::storeRigidPartVsMasterNode.find(pid) != DynaToRad::storeRigidPartVsMasterNode.end())
            {
                nodeId = DynaToRad::storeRigidPartVsMasterNode[pid];
            }
            if (nodeId)
            {
                rigidNodes.push_back(nodeId);
                p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", "SET_GEN_INIVEL_RBODY" + to_string(inivelId));
                EntityEdit SetEntityEdit(p_radiossModel, setHEdit);
                SetEntityEdit.SetValue(sdiIdentifier("set_Type"), sdiValue(sdiString("GENERAL")));
                SetEntityEdit.SetValue(sdiIdentifier("iset_Type"), sdiValue(0));
                SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
                SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, sdiUIntList(1, nodeId))));
                nodeSetId = setHEdit.GetId(p_radiossModel);
                RemoveRbodyMasterNodeFromInivelSet(sdiUIntList(1, nodeId));
            }
            else
            {
                // waring / error no pid found
            }
        }
        else
        {
            HandleRead nSetHread;
            selInivel->GetEntityHandle(sdiIdentifier("NSID"), nSetHread);

            HandleRead nExcludeSetHread;
            selInivel->GetEntityHandle(sdiIdentifier("NSIDEX"), nExcludeSetHread);

            if (nSetHread.IsValid())
            {
                EntityRead nSetRead(p_lsdynaModel, nSetHread);
                nodeSetId = DynaToRad::GetRadiossSetIdFromLsdSet(nSetRead.GetId(), "*SET_NODE" );
            }

            if (nExcludeSetHread.IsValid())
            {
                EntityRead nExcludeSetRead(p_lsdynaModel, nExcludeSetHread);
                nodeExcludeSetId = DynaToRad::GetRadiossSetIdFromLsdSet(nExcludeSetRead.GetId(), "*SET_NODE" );
            }
        } 

        if (!nodeSetId && !nodeExcludeSetId)
        {
            p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", "ALL_NODE_SET_GEN_INIVEL_" + to_string(inivelId));
            EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

            SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));
        }
        else if (!nodeSetId && nodeExcludeSetId)
        {
            p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", "NODE_SET_GEN_INIVEL_" + to_string(inivelId));
            EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

            sdiUIntList excludeNodesList;

            SelectionEdit selectRgdBody(p_radiossModel, "/RBODY");

            sdiUIntList NMasterNodeId;
            unsigned int NsidId;

            HandleRead nExcludeSetHread;
            selInivel->GetEntityHandle(sdiIdentifier("NSIDEX"), nExcludeSetHread);
            
            p_ConvertUtils.GetNodeIdsFromNodeSet(nExcludeSetHread.GetId(p_lsdynaModel), excludeNodesList);
            sdiVectorSort(excludeNodesList);
            sdiVectorUnique(excludeNodesList);

            if(nExcludeSetHread.IsValid())
            {
                while (selectRgdBody.Next())
                {   
                    sdiUIntList slaveNodeslRigidBody;
                    sdiValue tempValue;
                    HandleRead NsidHRead;
                    EntityRead nExcludeSetRead(p_lsdynaModel, nExcludeSetHread);

                    sdiValueEntity NMasterNode;
                    selectRgdBody->GetValue(sdiIdentifier("node_ID"), tempValue);
                    tempValue.GetValue(NMasterNode);

                    sdiValueEntity Nsid;
                    selectRgdBody->GetValue(sdiIdentifier("grnd_ID"), tempValue);
                    tempValue.GetValue(Nsid);
                    NsidId=Nsid.GetId();

                    p_radiossModel->FindById(p_radiossModel->GetEntityType("/SET/GENERAL"), NsidId, NsidHRead);
                    p_ConvertUtils.ExtractNodesFromRadiossSet(NsidHRead, slaveNodeslRigidBody);
                    sdiVectorSort(slaveNodeslRigidBody);
                    sdiVectorUnique(slaveNodeslRigidBody);

                    sdiUIntList intersectList;
                    
                    intersectList.assign(std::max(slaveNodeslRigidBody.size(), excludeNodesList.size()),0);
                    sdiUIntList::iterator it = std::set_intersection(slaveNodeslRigidBody.begin(), slaveNodeslRigidBody.end(),
                        excludeNodesList.begin(), excludeNodesList.end(), intersectList.begin());
                        
                    intersectList.resize(it-intersectList.begin()); 
                       
                    if (intersectList.size() != 0) NMasterNodeId.push_back(NMasterNode.GetId());

                }
            }
            
            if(NMasterNodeId.size() > 0)
            {
                SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(3));
            }
            else
            {
                SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(2));
            }

            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
            SetEntityEdit.SetValue(sdiIdentifier("opt_G", 0, 0), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("genemax", 0, 0), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("start", 0, 0), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("end", 0, 0), sdiValue(999999999));
            SetEntityEdit.SetValue(sdiIdentifier("by", 0, 0), sdiValue(1));

            SetEntityEdit.SetValue(sdiIdentifier("opt_D", 0, 1), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 1), sdiValue(sdiString("SET")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 1), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 1), sdiValue(sdiValueEntityList(radSetType, sdiUIntList(1, nodeExcludeSetId))));

            if(NMasterNodeId.size() > 0)
            {
                SetEntityEdit.SetValue(sdiIdentifier("opt_D", 0, 2), sdiValue(1));
                SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 2), sdiValue(sdiString("NODE")));
                SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 2), sdiValue((int)NMasterNodeId.size())); 
                SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 2), sdiValue(sdiValueEntityList(1, NMasterNodeId)));; 
            }
        }
        else if (nodeSetId && nodeExcludeSetId)
        {
            p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", "NODE_SET_GEN_INIVEL_" + to_string(inivelId));
            EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

            SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(2));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));

            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("SET")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radSetType, sdiUIntList(1, nodeSetId))));

            SetEntityEdit.SetValue(sdiIdentifier("opt_D", 0, 1), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 1), sdiValue(sdiString("SET")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 1), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 1), sdiValue(sdiValueEntityList(radSetType, sdiUIntList(1, nodeExcludeSetId))));
        }

        sdiStringList radVelAttrNames({ "Vx", "Vy", "Vz" });
        sdiString inivelName = selInivel->GetName();

        bool isInivelRraCreated = false;

        if (lsdVX != 0.0 || lsdVY != 0.0 || lsdVZ != 0.0)
        {
            HandleEdit inivelHEdit;
            if (p_radiossModel->IsIdAvailable(destEntityType, inivelId) && inivelId != 0)
                p_radiossModel->CreateEntity(inivelHEdit, "/INIVEL/TRA", inivelName, inivelId);
            else
                p_radiossModel->CreateEntity(inivelHEdit, "/INIVEL/TRA", inivelName, p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*INITIAL_VELOCITY")));
            EntityEdit inivelEdit(p_radiossModel, inivelHEdit);
            //sdiString strFromVelVect;
            for (size_t j = 0; j < 3; ++j)
            {
                //strFromVelVect += ":" + to_string(traVel[j]);
                inivelEdit.SetValue(sdiIdentifier(radVelAttrNames[j]), sdiValue(traVel[j]));
            }
            if (nodeSetId)
                inivelEdit.SetValue(sdiIdentifier("Gnod_id"), sdiValue(sdiValueEntity(radSetType, nodeSetId)));
            else if (setHEdit.IsValid())
                inivelEdit.SetEntityHandle(sdiIdentifier("Gnod_id"), setHEdit);

            inivelEdit.SetValue(sdiIdentifier("displayname"), sdiValue(inivelName));
            inivelEdit.SetValue(sdiIdentifier("rad_inivel_type"), sdiValue(sdiString("TRA")));
            if (csidEntity.GetId())
                inivelEdit.SetValue(sdiIdentifier("Skew_id"), sdiValue(sdiValueEntity(radSkewType, csidEntity.GetId())));

            isInivelRraCreated = true;

            if (inivelHEdit.IsValid())
            {
                sdiConvert::SDIHandlReadList sourceInivel = { {selInivel->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(inivelHEdit, sourceInivel));
            }
        }
        if (lsdVXR != 0.0 || lsdVYR != 0.0 || lsdVZR != 0.0)
        {
            HandleEdit inivelHEdit;
            if (p_radiossModel->IsIdAvailable(destEntityType, inivelId) && inivelId != 0)
                p_radiossModel->CreateEntity(inivelHEdit, "/INIVEL/ROT", inivelName, inivelId);
            else
                p_radiossModel->CreateEntity(inivelHEdit, "/INIVEL/ROT", inivelName, p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*INITIAL_VELOCITY")));
            EntityEdit inivelEdit(p_radiossModel, inivelHEdit);
            //sdiString strFromVelVect;
            for (size_t j = 0; j < 3; ++j)
            {
                //strFromVelVect += ":" + to_string(rotVel[j]);
                inivelEdit.SetValue(sdiIdentifier(radVelAttrNames[j]), sdiValue(rotVel[j]));
            }
            if (nodeSetId)
                inivelEdit.SetValue(sdiIdentifier("Gnod_id"), sdiValue(sdiValueEntity(radSetType, nodeSetId)));
            else if (setHEdit.IsValid())
                inivelEdit.SetEntityHandle(sdiIdentifier("Gnod_id"), setHEdit);

            inivelEdit.SetValue(sdiIdentifier("displayname"), sdiValue(inivelName));
            inivelEdit.SetValue(sdiIdentifier("rad_inivel_type"), sdiValue(sdiString("ROT")));
            if (csidEntity.GetId())
                inivelEdit.SetValue(sdiIdentifier("Skew_id"), sdiValue(sdiValueEntity(radSkewType, csidEntity.GetId())));

            if (inivelHEdit.IsValid())
            {
                sdiConvert::SDIHandlReadList sourceInivel = { {selInivel->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(inivelHEdit, sourceInivel));
            }
        }

        /* IRIGID*/
        if (nodeSetId)
        {
            HandleEdit radSetHread;
            if (p_radiossModel->FindById(radSetType, nodeSetId, radSetHread))
                HandleIrgidOption(*selInivel, radSetHread);
        }
        else
            HandleIrgidOption(*selInivel, setHEdit);
    }
    if (!rigidNodes.empty())
        RemoveInivelRbodyNodesFromOtherInivelCards(rigidNodes);
}

void sdiD2R::ConvertInitialVelocity::HandleIrgidOption(const EntityRead& lsdInivelRead, HandleEdit& radSetHEdit)
{
    sdiString keyWord = lsdInivelRead.GetKeyword();
    int lsdIRIGID = 0;
    sdiValue tempVal(lsdIRIGID);
    lsdInivelRead.GetValue(sdiIdentifier("IRIGID"), tempVal);
    tempVal.GetValue(lsdIRIGID);

    EntityEdit radSetEdit(p_radiossModel, radSetHEdit);

    int clausesMax = 0;
    tempVal = sdiValue(clausesMax);
    radSetEdit.GetValue(sdiIdentifier("clausesmax"), tempVal);
    tempVal.GetValue(clausesMax);
    if (lsdIRIGID < -2 && lsdIRIGID > 1)
    {
        // invalid irgid value warning
        return;
    }
    int optionD = 0;
    if (lsdIRIGID == 0)
        optionD = 1;
    bool isInivelGen = false;
    if (keyWord.find("GENERATION") != keyWord.npos)
        isInivelGen = true;

    sdiString iniVelSetkeyType;
    tempVal = sdiValue(iniVelSetkeyType);
    radSetEdit.GetValue(sdiIdentifier("KEY_type", 0, 0), tempVal);
    tempVal.GetValue(iniVelSetkeyType);

    if (clausesMax == 1 || iniVelSetkeyType == "ALL")
    {
        if (iniVelSetkeyType == "ALL" && clausesMax == 1)
        {
            SelectionRead selRbody(p_radiossModel, "/RBODY");
            sdiUIntList irigidDependentMasterNodes;
            irigidDependentMasterNodes.reserve(selRbody.Count());
            while (selRbody.Next())
            {
                sdiValueEntity masterNode;
                tempVal = sdiValue(masterNode);
                selRbody->GetValue(sdiIdentifier("node_ID"), tempVal);
                tempVal.GetValue(masterNode);
                unsigned int rbodyMasterNode = masterNode.GetId();
                unsigned int rigidBodyId = selRbody->GetId();
                if (mapCNRBIdVsIsInerta.find(rigidBodyId) != mapCNRBIdVsIsInerta.end())
                {
                    if (mapCNRBIdVsIsInerta[rigidBodyId])
                    {
                        irigidDependentMasterNodes.push_back(rbodyMasterNode);
                        continue;
                    }
                }
                else if (rbodyDetails[rigidBodyId].first == "PART")
                {
                    if (rbodyDetails[rigidBodyId].second.size() == 1)
                    {
                        if (mapLsdPartVsIsInerta[rbodyDetails[rigidBodyId].second[0]])
                        {
                            irigidDependentMasterNodes.push_back(rbodyMasterNode);
                            continue;
                        }
                    }
                }
            }
            if (!irigidDependentMasterNodes.empty())
            {
                radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, clausesMax), sdiValue(sdiString("NODE")));
                radSetEdit.SetValue(sdiIdentifier("idsmax", 0, clausesMax), sdiValue((int)irigidDependentMasterNodes.size()));
                radSetEdit.SetValue(sdiIdentifier("ids", 0, clausesMax), sdiValue(sdiValueEntityList(1, irigidDependentMasterNodes)));
                radSetEdit.SetValue(sdiIdentifier("opt_D", 0, clausesMax), sdiValue(optionD));
                radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(++clausesMax));
                if (!optionD)
                    RemoveRbodyMasterNodeFromInivelSet(irigidDependentMasterNodes);
            }
        }
        else
        {
            sdiUIntList inivelEntityList;
            sdiUIntList dPartList;
            sdiUIntList masterNodesToRemoveDPart;
            int dPartListSize = 0;
            if (iniVelSetkeyType == "PART")
            {
                int idsMax = 0;
                tempVal = sdiValue(idsMax);
                radSetEdit.GetValue(sdiIdentifier("idsmax", 0, 0), tempVal);
                tempVal.GetValue(idsMax);
                for (int j = 0; j < idsMax; ++j)
                {
                    sdiValueEntity entity;
                    tempVal = sdiValue(entity);
                    radSetEdit.GetValue(sdiIdentifier("ids", 0, 0, j), tempVal);
                    tempVal.GetValue(entity);
                    inivelEntityList.push_back(entity.GetId());
                }
            }
            else if (iniVelSetkeyType == "NODE" || iniVelSetkeyType == "ALL")
                p_ConvertUtils.ExtractNodesFromRadiossSet(radSetHEdit, inivelEntityList);

            if (iniVelSetkeyType == "ALL")
            {
                p_ConvertUtils.ExtractDPartsFromSetWithClauseALL(radSetHEdit, dPartList);
                dPartListSize = (int)dPartList.size();
            }

            if (!inivelEntityList.empty())
            {
                sdiVectorSort(inivelEntityList);
                SelectionRead selRbody(p_radiossModel, "/RBODY");
                sdiUIntList masterNodesToAdd;/*add or remove based on whether inertia is defined on not*/
                sdiUIntList masterNodesToAddAlways;
                masterNodesToAdd.reserve(selRbody.Count());
                masterNodesToAddAlways.reserve(selRbody.Count());
                masterNodesToRemoveDPart.reserve(selRbody.Count());
                sdiUIntList inivelNodeList;
                while (selRbody.Next())
                {
                    bool isCNRBInertia = false;
                    sdiValueEntity masterNode;
                    tempVal = sdiValue(masterNode);
                    selRbody->GetValue(sdiIdentifier("node_ID"), tempVal);
                    tempVal.GetValue(masterNode);

                    unsigned int rigidBodyId = selRbody->GetId();
                    sdiString rigidBodyName = selRbody->GetName();
                    if (dPartListSize)
                    {
                        auto itr = find_if(dPartList.begin(), dPartList.end(),
                            [&](unsigned int partId) { return rigidBodyName.find("PART_" + to_string(partId)) != rigidBodyName.npos; });
                        if (itr != dPartList.end())
                            masterNodesToRemoveDPart.push_back(masterNode.GetId());
                    }
                    if (mapCNRBIdVsIsInerta.find(rigidBodyId) != mapCNRBIdVsIsInerta.end())
                    {
                        if (mapCNRBIdVsIsInerta[rigidBodyId])
                            isCNRBInertia = true;
                    }

                    sdiString rbodySetkeyType = rbodyDetails[selRbody->GetId()].first;
                    sdiUIntList rbodyEntityList = rbodyDetails[selRbody->GetId()].second;
                    sdiUIntList rbodySlaveNodeList;
                    if (!rbodyEntityList.empty())
                    {
                        if (rbodySetkeyType != iniVelSetkeyType)
                        {
                            if ((iniVelSetkeyType == "PART") &&
                                inivelNodeList.empty()) // only needs to be done once
                            {
                                p_ConvertUtils.ExtractNodesFromRadiossSet(radSetHEdit, inivelNodeList);
                            }
                        }

                        size_t numRbodyEntities = rbodyEntityList.size();
                        for (size_t i = 0; i < numRbodyEntities; ++i)
                        {
                            bool isPartInertiaDefined = false;
                            sdiString rbodySlavePartCard;
                            if (rbodySetkeyType == "PART")
                            {
                                HandleRead rbodySlavePartHRead;
                                if (p_lsdynaModel->FindById("*PART", rbodyEntityList[i], rbodySlavePartHRead))
                                {
                                    EntityRead rbodySlavePartRead(p_lsdynaModel, rbodySlavePartHRead);
                                    rbodySlavePartCard = rbodySlavePartRead.GetKeyword();
                                    if (rbodySlavePartCard == "*PART_INERTIA")
                                        isPartInertiaDefined = true;
                                    if (rbodySetkeyType != iniVelSetkeyType)
                                    {
                                        sdiUIntList partNodeList;
                                        p_ConvertUtils.GetNodesOfParts(sdiUIntList(1, rbodyEntityList[i]), partNodeList);
                                        sdiVectorSort(partNodeList);
                                        for (unsigned int rbodySlaveNode : partNodeList)
                                        {
                                            if (std::binary_search(inivelEntityList.begin(), inivelEntityList.end(), rbodySlaveNode))
                                            {
                                                if (isCNRBInertia || isPartInertiaDefined)
                                                    masterNodesToAdd.push_back(masterNode.GetId());
                                                else
                                                    masterNodesToAddAlways.push_back(masterNode.GetId());
                                                break;
                                            }
                                        }
                                        break;
                                    }
                                    else
                                    {
                                        if (std::binary_search(inivelEntityList.begin(), inivelEntityList.end(), rbodyEntityList[i]))
                                        {
                                            if (isCNRBInertia || isPartInertiaDefined)
                                                masterNodesToAdd.push_back(masterNode.GetId());
                                            else
                                                masterNodesToAddAlways.push_back(masterNode.GetId());
                                            break;
                                        }
                                    }
                                }
                            }
                            if (rbodySetkeyType == "NODE")
                            {
                                bool matchFound = false;
                                if (iniVelSetkeyType == "PART")
                                {
                                    if (std::binary_search(inivelNodeList.begin(), inivelNodeList.end(), rbodyEntityList[i]))
                                        matchFound = true;
                                }
                                else
                                {
                                    if (std::binary_search(inivelEntityList.begin(), inivelEntityList.end(), rbodyEntityList[i]))
                                        matchFound = true;
                                }
                                if (matchFound)
                                {
                                    if (isCNRBInertia || isPartInertiaDefined)
                                        masterNodesToAdd.push_back(masterNode.GetId());
                                    else
                                        masterNodesToAddAlways.push_back(masterNode.GetId());
                                    break;
                                }
                            }
                        }
                    }
                }
                if (!masterNodesToAddAlways.empty())
                {
                    radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, clausesMax), sdiValue(sdiString("NODE")));
                    radSetEdit.SetValue(sdiIdentifier("opt_D", 0, clausesMax), sdiValue(0));
                    radSetEdit.SetValue(sdiIdentifier("idsmax", 0, clausesMax), sdiValue((int)masterNodesToAddAlways.size()));
                    radSetEdit.SetValue(sdiIdentifier("ids", 0, clausesMax), sdiValue(sdiValueEntityList(1, masterNodesToAddAlways)));
                    radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(++clausesMax));
                }
                if (!masterNodesToAdd.empty())
                {
                    radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, clausesMax), sdiValue(sdiString("NODE")));
                    radSetEdit.SetValue(sdiIdentifier("opt_D", 0, clausesMax), sdiValue(optionD));
                    radSetEdit.SetValue(sdiIdentifier("idsmax", 0, clausesMax), sdiValue((int)masterNodesToAdd.size()));
                    radSetEdit.SetValue(sdiIdentifier("ids", 0, clausesMax), sdiValue(sdiValueEntityList(1, masterNodesToAdd)));
                    radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(++clausesMax));
                    if (!optionD)
                        RemoveRbodyMasterNodeFromInivelSet(masterNodesToAdd);
                }
                if (!masterNodesToRemoveDPart.empty())
                {
                    radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, clausesMax), sdiValue(sdiString("NODE")));
                    radSetEdit.SetValue(sdiIdentifier("opt_D", 0, clausesMax), sdiValue(1));
                    radSetEdit.SetValue(sdiIdentifier("idsmax", 0, clausesMax), sdiValue((int)masterNodesToRemoveDPart.size()));
                    radSetEdit.SetValue(sdiIdentifier("ids", 0, clausesMax), sdiValue(sdiValueEntityList(1, masterNodesToRemoveDPart)));
                    radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(clausesMax + 1));
                }
            }
        }
    }
}

void sdiD2R::ConvertInitialVelocity::RemoveRbodyMasterNodeFromInivelSet(const sdiUIntList& rbodyMasteNodes)
{
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    SelectionEdit selInivel(p_radiossModel, "/INIVEL");
    vector<HandleEdit> cardsToDelete;
    cardsToDelete.reserve(selInivel.Count());
    while (selInivel.Next())
    {
        sdiValueEntity grnodEntity;
        sdiValue tempVal(grnodEntity);
        selInivel->GetValue(sdiIdentifier("Gnod_id"), tempVal);
        tempVal.GetValue(grnodEntity);
        unsigned int grnodId = grnodEntity.GetId();
        if (grnodId)
        {
            HandleEdit grnodHedit;
            if (p_radiossModel->FindById(radSetType, grnodId, grnodHedit))
            {
                EntityEdit grnodEdit(p_radiossModel, grnodHedit);

                int clausesMax = 0;
                tempVal = sdiValue(clausesMax);
                grnodEdit.GetValue(sdiIdentifier("clausesmax"), tempVal);
                tempVal.GetValue(clausesMax);
                if (clausesMax == 1)
                {
                    sdiString keyType;
                    sdiValue tempVal(keyType);
                    grnodEdit.GetValue(sdiIdentifier("KEY_type", 0, 0), tempVal);
                    tempVal.GetValue(keyType);

                    if (keyType == "NODE")
                    {
                        sdiUIntList inivelNodeidList;
                        p_ConvertUtils.ExtractNodesFromRadiossSet(grnodHedit, inivelNodeidList);
                        if (inivelNodeidList.size() == 1)
                        {
                            for (unsigned int rbodyNode : rbodyMasteNodes)
                            {
                                if (rbodyNode == inivelNodeidList[0])
                                    cardsToDelete.push_back(selInivel->GetHandle());
                            }
                        }
                    }
                }
            }
        }
    }
    if (!cardsToDelete.empty())
    {
        for (HandleEdit hEdit : cardsToDelete)
        {
            sdiConvert::Convert::RemoveFromConversionLog(p_radiossModel, hEdit);
            p_radiossModel->Delete(hEdit);
        }
    }
}

void sdiD2R::ConvertInitialVelocity::RemoveInivelRbodyNodesFromOtherInivelCards(const sdiUIntList& inivelRbodyNodes)
{
    SelectionRead selRadInivel(p_radiossModel, "/INIVEL");
    while (selRadInivel.Next())
    {
        sdiString keyWord = selRadInivel->GetKeyword();
        sdiString grndIdendifier = (keyWord == "/INIVEL/AXIS") ? "GRNOD_ID" : "Gnod_id";
        HandleEdit grnodHedit;
        selRadInivel->GetEntityHandle(sdiIdentifier(grndIdendifier), grnodHedit);
        if (grnodHedit.IsValid())
        {
            EntityEdit grnodEdit(p_radiossModel, grnodHedit);
            if (grnodEdit.GetName().find("SET_GEN_INIVEL_RBODY") != string::npos)
                continue;
            int clausesMax = 0;
            sdiValue tempVal(clausesMax);
            grnodEdit.GetValue(sdiIdentifier("clausesmax"), tempVal);
            tempVal.GetValue(clausesMax);
            
            grnodEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(clausesMax + 1));
            grnodEdit.SetValue(sdiIdentifier("KEY_type", 0, clausesMax), sdiValue(sdiString("NODE")));
            grnodEdit.SetValue(sdiIdentifier("opt_D", 0, clausesMax), sdiValue(1));
            grnodEdit.SetValue(sdiIdentifier("idsmax", 0, clausesMax), sdiValue((int)inivelRbodyNodes.size()));
            grnodEdit.SetValue(sdiIdentifier("ids", 0, clausesMax), sdiValue(sdiValueEntityList(1, inivelRbodyNodes)));
        }
    }
}

void ConvertIniVelToLocalCoordinates(ModelViewRead* lsdyaModel, const EntityRead& lsdIniVel, const sdiD2R::ConvertUtils& convertUtils, EntityEdit& radInivelEdit)
{
    double lsdVX;
    double lsdVY;
    double lsdVZ;
    double lsdXC;
    double lsdYC;
    double lsdZC;
    double lsdNX;
    double lsdNY;
    double lsdNZ;

    vector<reference_wrapper<double>> attribVals({ lsdVX, lsdVY, lsdVZ, lsdXC, lsdYC, lsdZC, lsdNX, lsdNY, lsdNZ });
    vector<sdiString> attribNames({ "VX", "VY", "VZ", "XC", "YC", "ZC", "LSD_NX", "LSD_NY", "LSD_NZ" });
    convertUtils.GetAttribValues(lsdIniVel, attribNames, attribVals);
    HandleRead nodeNyHRead;
    HandleRead nodeNzHRead;
    lsdIniVel.GetEntityHandle(sdiIdentifier("plane_node1"), nodeNyHRead);
    lsdIniVel.GetEntityHandle(sdiIdentifier("plane_node2"), nodeNzHRead);
    if (((lsdNX != 0.0 && lsdNX != -999) || lsdNY != 0.0 || lsdNZ != 0.0) ||
        (lsdNX == -999 && nodeNyHRead.IsValid() && nodeNzHRead.IsValid()))
    {
        sdiTriple origin(lsdXC, lsdYC, lsdZC);
        sdiTriple skewX2(lsdNX, lsdNY, lsdNZ);
        sdiTriple skewX1(0.0, 0.0, 1.0);
        if (lsdNX == -999)
        {
            NodeRead nodeNyRead(lsdyaModel, nodeNyHRead);
            NodeRead nodeNzRead(lsdyaModel, nodeNzHRead);
            origin = nodeNyRead.GetPosition();
            skewX2 = nodeNzRead.GetPosition() - origin;
        }
        if (skewX2 * skewX1 == sdiTriple(0.0, 0.0, 0.0))
        {
            skewX1 = sdiTriple(0.0, 1.0, 0.0);
            if (skewX2 * skewX1 == sdiTriple(0.0, 0.0, 0.0))
            {
                skewX1 = sdiTriple(0.0, 1.0, 0.0);
                if (skewX2 * skewX1 == sdiTriple(0.0, 0.0, 0.0))
                    skewX1 = sdiTriple(0.0, 0.0, 1.0);
            }
        }

        sdiTriple vectX0 = skewX1 * skewX2;
        vectX0.Normalize();
        sdiTriple vectY0 = skewX2 * vectX0;
        vectY0.Normalize();
        sdiTriple velVect(lsdVX, lsdVY, lsdVZ);
        lsdVX = velVect[0] * vectX0[0] + velVect[1] * vectX0[1] + velVect[2] * vectX0[2];
        lsdVY = velVect[0] * vectY0[0] + velVect[1] * vectY0[1] + velVect[2] * vectY0[2];
        lsdVZ = velVect[0] * skewX2[0] + velVect[1] * skewX2[1] + velVect[2] * skewX2[2];

        radInivelEdit.SetValue(sdiIdentifier("Vxt"), sdiValue(lsdVX));
        radInivelEdit.SetValue(sdiIdentifier("Vyt"), sdiValue(lsdVY));
        radInivelEdit.SetValue(sdiIdentifier("Vzt"), sdiValue(lsdVZ));
    }
}
