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

#include <dyna2rad/convertrigids.h>
#include <dyna2rad/dyna2rad.h>
#include <typedef.h>

using namespace std;
using namespace sdi;
using namespace sdiD2R;

void ConvertRigid::ConvertAllRigids()
{
    ConvertEntities();
}

void ConvertRigid::ConvertEntities()
{
    ConvertConstrainedNodalRbodies();

    ConvertMatsRigid();

    ConvertConstrainedExtraNodes();

    ConvertRigidBodies();

    ConvertConstrainedGeneralizedWeldSpot();

    p_CreateTHRBody();
}

void ConvertRigid::ConvertConstrainedNodalRbodies()
{
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    EntityType radSkewType = p_radiossModel->GetEntityType("/SKEW");
    SelectionRead selectRgdBody(p_lsdynaModel, srcCard);
    p_IprtRbodies.reserve(selectRgdBody.Count());
    while (selectRgdBody.Next())
    {
        HandleRead lsdPNSIDHread;
        HandleRead lsdPNODEHread;
        HandleRead lsdCIDHread;
        HandleRead lsdCID2Hread;
        unsigned int lsdPNSID;

        vector<reference_wrapper<HandleRead>> entityAttrVals({ lsdPNSIDHread, lsdPNODEHread,lsdCIDHread, lsdCID2Hread });
        vector<sdiString> entityAttrNames({ "NSID", "PNODE","LSD_CID", "LSD_CID2" });
        p_ConvertUtils.GetEntityHandles(*selectRgdBody, entityAttrNames, entityAttrVals);

        EntityRead lsdPNSIDRead(p_lsdynaModel, lsdPNSIDHread);
        lsdPNSID = lsdPNSIDRead.GetId();

        if(lsdPNSID == 0) lsdPNSID = selectRgdBody->GetId();

        unsigned int radSetId = DynaToRad::GetRadiossSetIdFromLsdSet(lsdPNSID, "*SET_NODE");
        HandleEdit radNsidHEdit;
        p_radiossModel->FindById(p_radiossModel->GetEntityType("/SET/GENERAL"), radSetId, radNsidHEdit);
        if (radNsidHEdit.IsValid())
        {
            EntityEdit radNsidEdit(p_radiossModel, radNsidHEdit);
            sdiTriple centroid(0.0, 0.0, 0.0);
            p_ConvertUtils.GetCentroid(radNsidHEdit, centroid);
            HandleEdit rbodyHEdit;
            HandleNodeEdit nodeHEdit;
            p_radiossModel->CreateEntity(rbodyHEdit, "/RBODY", selectRgdBody->GetName(), selectRgdBody->GetId());
            if (rbodyHEdit.IsValid())
            {
                EntityEdit rbodyEdit(p_radiossModel, rbodyHEdit);
                rbodyEdit.SetValue(sdiIdentifier("TITLE"), sdiValue(selectRgdBody->GetName()));
                rbodyEdit.SetValue(sdiIdentifier("ICOG"), sdiValue(0));

                sdiString keyWord = selectRgdBody->GetKeyword();
                if (keyWord.find("INERTIA") != keyWord.npos)
                    HandleInertiaOption(*selectRgdBody, radNsidEdit, rbodyEdit);
                else
                {
                    if (lsdPNODEHread.IsValid())
                    {

                        NodeRead pnodeRead(p_lsdynaModel, lsdPNODEHread);
                        sdiTriple pnodePos = pnodeRead.GetPosition();
                        if (pnodePos != centroid)
                            pnodePos = centroid;;
                        p_radiossModel->CreateNode(nodeHEdit, "/NODE", pnodePos);
                    }
                    else
                        p_radiossModel->CreateNode(nodeHEdit, "/NODE", centroid);

                    if (nodeHEdit.IsValid())
                        rbodyEdit.SetValue(sdiIdentifier("node_ID"), sdiValue(sdiValueEntity(1, nodeHEdit.GetId(p_radiossModel))));

                    vector<sdiString> radInertiaAttrNames({ "Mass", "Jxx", "Jyy", "Jzz" });
                    for (size_t i = 0; i < radInertiaAttrNames.size(); ++i)
                        rbodyEdit.SetValue(sdiIdentifier(radInertiaAttrNames[i]), sdiValue(1e-20));
                }
           
                int ircs = 0;
                int iprt = 0;
                vector<reference_wrapper<int>> attrIntVals({ ircs, iprt });
                sdiStringList intAttrNames( { "IRCS", "IPRT" } );
                p_ConvertUtils.GetAttribValues(*selectRgdBody, intAttrNames, attrIntVals);

                rbodyEdit.SetValue(sdiIdentifier("grnd_ID"), sdiValue(sdiValueEntity(radSetType, radNsidEdit.GetId())));
                if (lsdCID2Hread.IsValid())
                    rbodyEdit.SetValue(sdiIdentifier("Skew_ID"), sdiValue(sdiValueEntity(radSkewType, lsdCID2Hread.GetId(p_lsdynaModel))));
                else if (lsdCIDHread.IsValid() && !ircs)
                    rbodyEdit.SetValue(sdiIdentifier("Skew_ID"), sdiValue(sdiValueEntity(radSkewType, lsdCIDHread.GetId(p_lsdynaModel))));
                else if (ircs)
                    p_CreateNewSkewForIRCSFlag(*selectRgdBody, rbodyEdit);

                if (keyWord.find("SPC") != keyWord.npos)
                    HandleSPCOption(*selectRgdBody, rbodyEdit);

                if (iprt)
                    p_IprtRbodies.push_back(rbodyEdit.GetId());

                if (lsdPNODEHread.IsValid())
                    p_PushPnodeIntoSlaveSet(lsdPNODEHread.GetId(p_lsdynaModel), lsdPNSIDHread, radNsidEdit);

                sdiValueEntity nodeIdEntity;
                sdiValue tempValue(nodeIdEntity);
                rbodyEdit.GetValue(sdiIdentifier("node_ID"), tempValue);
                tempValue.GetValue(nodeIdEntity);
                unsigned int nodeId = nodeIdEntity.GetId();
                DynaToRad::storeRbodyPIDVsMasterNode[selectRgdBody->GetId()] = nodeIdEntity.GetId();

                sdiConvert::SDIHandlReadList sourcergdBody = { {selectRgdBody->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(rbodyHEdit, sourcergdBody));
            }
        }
    }
}

void sdiD2R::ConvertRigid::ConvertMatsRigid()
{
    SelectionRead selMatRigid(p_lsdynaModel, "*MAT_RIGID");
    ConvertRigidMaterials(selMatRigid);

    SelectionRead selMat_020(p_lsdynaModel, "*MAT_020");
    ConvertRigidMaterials(selMat_020);

    SelectionRead selMat009(p_lsdynaModel, "*MAT_009");
    ConvertRigidMaterials(selMat009);
}


void sdiD2R::ConvertRigid::ConvertRigidMaterials(SelectionRead &selMatRigid)
{
    EntityType radSkewType = p_radiossModel->GetEntityType("/SKEW");
    while (selMatRigid.Next())
    {
        double lsdCMO = 0.0;
        sdiValue tempValue(lsdCMO);
        selMatRigid->GetValue(sdiIdentifier("CMO"), tempValue);
        tempValue.GetValue(lsdCMO);

        SelectionRead selRigidPart(p_lsdynaModel, "*PART", 
            FilterValue(sdiIdentifier("MID"), sdiValue(sdiValueEntity(p_lsdynaModel->GetEntityType("*MAT"), selMatRigid->GetId()))));

        while (selRigidPart.Next())
        {
            sdiString keyWord = selRigidPart->GetKeyword();
            sdiConvert::EntityId rigidPartId = selRigidPart->GetId();

            HandleEdit setHEdit;
            p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", "SET_GEN_RIGID_PART" + to_string(rigidPartId));
            EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

            SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/PART"), sdiUIntList(1, rigidPartId))));

            HandleEdit hRbodyEdit;
            p_radiossModel->CreateEntity(hRbodyEdit, "/RBODY", "rigid_PART_" + to_string(rigidPartId));
            EntityEdit rbodyEdit(p_radiossModel, hRbodyEdit);
            mapPartIdVsRbodyHandle[rigidPartId] = hRbodyEdit;

            rbodyEdit.SetValue(sdiIdentifier("TITLE"), sdiValue("rigid_PART_" + to_string(rigidPartId)));
            rbodyEdit.SetValue(sdiIdentifier("ICOG"), sdiValue(1));

            rbodyEdit.SetEntityHandle(sdiIdentifier("grnd_ID"), setHEdit);

            if (keyWord.find("INERTIA") != keyWord.npos)
                HandleInertiaOption(*selRigidPart, SetEntityEdit, rbodyEdit);
            else
            {
                sdiTriple centroid(0.0, 0.0, 0.0);
                p_ConvertUtils.GetCentroid(*selRigidPart, centroid);
                HandleNodeEdit hNodeEdit;
                p_radiossModel->CreateNode(hNodeEdit, "/NODE", centroid);
                //rbodyEdit.SetEntityHandle(sdiIdentifier("node_ID"), hNodeEdit);
                rbodyEdit.SetValue(sdiIdentifier("node_ID"), sdiValue(sdiValueEntity(1, hNodeEdit.GetId(p_radiossModel))));

                vector<sdiString> radInertiaAttrNames({ "Mass", "Jxx", "Jyy", "Jzz" });
                for (size_t i = 0; i < radInertiaAttrNames.size(); ++i)
                {
                    rbodyEdit.SetValue(sdiIdentifier(radInertiaAttrNames[i]), sdiValue(1e-20));
                }
            }
            sdiValueEntity localSystEntity;
            sdiValue tempVal(localSystEntity);
            selRigidPart->GetValue(sdiIdentifier("LSD_P_CID"), tempVal);
            tempVal.GetValue(localSystEntity);
            unsigned int locSystId = localSystEntity.GetId();
            if (locSystId)
            {
                rbodyEdit.SetValue(sdiIdentifier("Skew_ID"), sdiValue(sdiValueEntity(radSkewType, locSystId)));
            }
            if (lsdCMO == 1.0 || lsdCMO == -1.0)
                HandleSPCOption(*selMatRigid, rbodyEdit);

            sdiValueEntity nodeIdEntity;
            sdiValue tempValue(nodeIdEntity);
            rbodyEdit.GetValue(sdiIdentifier("node_ID"), tempValue);
            tempValue.GetValue(nodeIdEntity);
            unsigned int nodeId = nodeIdEntity.GetId();
            DynaToRad::storeRbodyPIDVsMasterNode[rigidPartId] = nodeIdEntity.GetId();

            sdiConvert::SDIHandlReadList sourcehandleList = { {selMatRigid->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(setHEdit, sourcehandleList));
            sdiConvert::Convert::PushToConversionLog(std::make_pair(hRbodyEdit, sourcehandleList));
        }
    }
}

void sdiD2R::ConvertRigid::ConvertConstrainedExtraNodes()
{
    SelectionRead selConsExtNodes(p_lsdynaModel, "*CONSTRAINED_EXTRA_NODES");
    while (selConsExtNodes.Next())
    {
        sdiString slaveAttName("NID");
        EntityType slaveEntityType = 1;
        int slaveType = 2;
        sdiString keyWord = selConsExtNodes->GetKeyword();
        if (keyWord.find("SET") != keyWord.npos)
        {
            slaveType = 3;
            slaveAttName = "NSID";
            slaveEntityType = p_radiossModel->GetEntityType("/SET/GENERAL");
        }
        HandleRead masterPidHRead;
        sdiValueEntity slaveEntity;
        selConsExtNodes->GetEntityHandle(sdiIdentifier("PID"), masterPidHRead);
        if (masterPidHRead.IsValid())
        {
            EntityRead masterPidRead(p_lsdynaModel, masterPidHRead);
            sdiString pidKeyWord = masterPidRead.GetKeyword();
            bool isInertiaComp = false;
            if (pidKeyWord.find("INERTIA") != pidKeyWord.npos)
                isInertiaComp = true;
            unsigned int masterEntityId = masterPidHRead.GetId(p_lsdynaModel);

            sdiValue tempValue(slaveEntity);
            selConsExtNodes->GetValue(sdiIdentifier(slaveAttName), tempValue);
            tempValue.GetValue(slaveEntity);
            unsigned int slaveEntityId = 0;
            if (slaveAttName == "NID") 
            { 
                slaveEntityId = slaveEntity.GetId();
            }
            else if (slaveAttName == "NSID") 
            { 
                slaveEntityId = DynaToRad::GetRadiossSetIdFromLsdSet(slaveEntity.GetId(),"*SET_NODE");
            }

            //-----
            int slaveID = slaveEntityId;
            bool MnodeSetExist = false ;
            if(isInertiaComp == true)
            {
                HandleRead dynapartHread;
                p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), masterEntityId, dynapartHread);
                if(dynapartHread.IsValid())
                {
                    HandleRead dynaPIDinertiaHRead;
                    dynapartHread.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), dynaPIDinertiaHRead);
                    EntityRead dynaPIDinertiaRead(p_lsdynaModel, dynaPIDinertiaHRead);

                    HandleRead dynaNODEIDHRead;
                    dynapartHread.GetEntityHandle(p_lsdynaModel, sdiIdentifier("NODEID"), dynaNODEIDHRead);
                    EntityRead dynaNODEIDRead(p_lsdynaModel, dynaNODEIDHRead);

                    if(slaveAttName == "NID")
                    {
                        if(slaveEntityId  == dynaNODEIDRead.GetId() &&
                           masterEntityId == dynaPIDinertiaRead.GetId())break;
                    }
                    else if(slaveAttName == "NSID")
                    {
                        // create a new set without inertia master node
                        sdiUIntList NSIDNodesList;
                        sdiUIntList slaveSetEntityIdList;
                        p_ConvertUtils.GetNodeIdsFromNodeSet(slaveEntityId, NSIDNodesList);

                        if(masterEntityId == dynaPIDinertiaRead.GetId())
                        {
                            slaveSetEntityIdList.reserve(NSIDNodesList.size());

                            for (size_t i = 0; i < NSIDNodesList.size(); ++i)
                            {
                                if( dynaNODEIDRead.GetId() != NSIDNodesList[i])
                                {
                                    slaveSetEntityIdList.push_back(NSIDNodesList[i]);
                                }
                                else
                                {
                                    MnodeSetExist = true ;
                                }
                            }

                            if (MnodeSetExist)
                            {
                                // create new set
                                HandleEdit setHEdit;
                                p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", "SET_GENERAL_MERGE_RBODY_" + to_string(selConsExtNodes->GetId()));
                                EntityEdit SetEntityEdit(p_radiossModel, setHEdit);
                                slaveID = SetEntityEdit.GetId();

                                SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                                SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
                                SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue((int)slaveSetEntityIdList.size())); 
                                SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, slaveSetEntityIdList))); 

                                if (setHEdit.IsValid())
                                {
                                    sdiConvert::SDIHandlReadList sourcehandleList = { {selConsExtNodes->GetHandle()} };
                                    sdiConvert::Convert::PushToConversionLog(std::make_pair(setHEdit, sourcehandleList));
                                }
                            }
                        }
                    }

                 }
            } // if(isInertiaComp == true)
            //-----

            int iflag = 0;
            tempValue = sdiValue(iflag);
            selConsExtNodes->GetValue(sdiIdentifier("IFLAG"), tempValue);
            tempValue.GetValue(iflag);
            iflag = (isInertiaComp && iflag == 0) ? 1 : 2;

            HandleEdit mergeRbodyHedit;
            p_radiossModel->CreateEntity(mergeRbodyHedit, "/MERGE/RBODY", selConsExtNodes->GetName(), selConsExtNodes->GetId());
            EntityEdit mergeRbodyEdit(p_radiossModel, mergeRbodyHedit);

            mergeRbodyEdit.SetValue(sdiIdentifier("NB_SUBOBJVE"), sdiValue(1));
            mergeRbodyEdit.SetValue(sdiIdentifier("M_type", 0, 0), sdiValue(1));
            mergeRbodyEdit.SetValue(sdiIdentifier("S_type", 0, 0), sdiValue(slaveType));
            mergeRbodyEdit.SetValue(sdiIdentifier("Secon_ID", 0, 0), sdiValue(sdiValueEntity(slaveEntityType, slaveID)));
            mergeRbodyEdit.SetValue(sdiIdentifier("Iflag", 0, 0), sdiValue(iflag));
            if (mapPartIdVsRbodyHandle.find(masterEntityId) != mapPartIdVsRbodyHandle.end())
                mergeRbodyEdit.SetEntityHandle(sdiIdentifier("Main_ID", 0, 0), mapPartIdVsRbodyHandle[masterEntityId]);

            sdiConvert::SDIHandlReadList sourcehandleList = { {selConsExtNodes->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(mergeRbodyHedit, sourcehandleList));
        }
    }
}

void sdiD2R::ConvertRigid::ConvertRigidBodies()
{
    EntityType dynaConstRbdType = p_lsdynaModel->GetEntityType("*CONSTRAINED_RIGID_BODIES");
    EntityType radMergRbdType = p_radiossModel->GetEntityType("/MERGE/RBODY");
    SelectionRead selConsRgdBodies(p_lsdynaModel, "*CONSTRAINED_RIGID_BODIES");
    while (selConsRgdBodies.Next())
    {

        int numComps = 0;
        sdiValue tempVal(numComps);
        selConsRgdBodies->GetValue(sdiIdentifier("Number_of_comps"), tempVal);
        tempVal.GetValue(numComps);
        HandleEdit mergeRbodyHedit;
        unsigned int mergeRbdId = selConsRgdBodies->GetId();
        if (!p_radiossModel->IsIdAvailable(radMergRbdType, mergeRbdId))
            mergeRbdId = p_ConvertUtils.GetDynaMaxEntityID(dynaConstRbdType);
        p_radiossModel->CreateEntity(mergeRbodyHedit, "/MERGE/RBODY", selConsRgdBodies->GetName(), mergeRbdId);
        EntityEdit mergeRbodyEdit(p_radiossModel, mergeRbodyHedit);
        mergeRbodyEdit.SetValue(sdiIdentifier("NB_SUBOBJVE"), sdiValue(numComps));

        sdiConvert::SDIHandlReadList sourcehandleList = { {selConsRgdBodies->GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(mergeRbodyHedit, sourcehandleList));
        for (int i = 0; i < numComps; ++i)
        {
            int iflag = 0;
            HandleRead masterPidHRead;
            selConsRgdBodies->GetEntityHandle(sdiIdentifier("PIDM", 0, i), masterPidHRead);
            if (masterPidHRead.IsValid())
            {
                EntityRead masterPidRead(p_lsdynaModel, masterPidHRead);
                sdiString pidKeyWord = masterPidRead.GetKeyword();
                bool isInertiaComp = false;
                if (pidKeyWord.find("INERTIA") != pidKeyWord.npos)
                    isInertiaComp = true;
                unsigned int masterComp = masterPidRead.GetId();

                sdiValueEntityList slaveParts;
                tempVal = sdiValue(slaveParts);
                sdiUIntList slaveComps;
                selConsRgdBodies->GetValue(sdiIdentifier("PIDS", 0, i), tempVal);
                tempVal.GetValue(slaveParts);
                slaveParts.GetIdList(slaveComps);

                tempVal = sdiValue(iflag);
                selConsRgdBodies->GetValue(sdiIdentifier("IFLAG", 0, i), tempVal);
                tempVal.GetValue(iflag);

                iflag = (isInertiaComp && iflag == 0) ? 1 : 2;

                mergeRbodyEdit.SetValue(sdiIdentifier("M_type", 0, i), sdiValue(1));
                mergeRbodyEdit.SetValue(sdiIdentifier("S_type", 0, i), sdiValue(1));
                mergeRbodyEdit.SetValue(sdiIdentifier("Iflag", 0, i), sdiValue(iflag));

                if (mapPartIdVsRbodyHandle.find(masterComp) != mapPartIdVsRbodyHandle.end())
                    mergeRbodyEdit.SetEntityHandle(sdiIdentifier("Main_ID", 0, i), mapPartIdVsRbodyHandle[masterComp]);

                if (mapPartIdVsRbodyHandle.find(slaveComps[0]) != mapPartIdVsRbodyHandle.end())
                    mergeRbodyEdit.SetEntityHandle(sdiIdentifier("Secon_ID", 0, i), mapPartIdVsRbodyHandle[slaveComps[0]]);
            }
        }
    }
}

void sdiD2R::ConvertRigid::ConvertConstrainedGeneralizedWeldSpot()
{
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    EntityType radRbodyType = p_radiossModel->GetEntityType("/RBODY");
    EntityType radNodeType = p_radiossModel->GetEntityType("/NODE");
    SelectionRead seldynaConstGeneWeldSpot(p_lsdynaModel, "*CONSTRAINED_GENERALIZED_WELD_SPOT");
    while (seldynaConstGeneWeldSpot.Next())
    {
        unsigned int rbodyId = 0;
        HandleRead checkRbodyHEdit;
        p_radiossModel->FindById(p_radiossModel->GetEntityType("/RBODY"), seldynaConstGeneWeldSpot->GetId(), checkRbodyHEdit);
        if(!checkRbodyHEdit.IsValid()) 
        {
            rbodyId = seldynaConstGeneWeldSpot->GetId();
        }
        else
        {
            rbodyId = p_radiossModel->GetNextAvailableId(p_radiossModel->GetEntityType("/RBODY"));
        }

        HandleEdit rbodyHEdit;
        p_radiossModel->CreateEntity(rbodyHEdit, "/RBODY", seldynaConstGeneWeldSpot->GetName(), rbodyId);

        HandleRead lsdPNSIDHread;
        seldynaConstGeneWeldSpot->GetEntityHandle(sdiIdentifier("NSID"), lsdPNSIDHread);
        EntityRead lsdPNSIDRead(p_lsdynaModel, lsdPNSIDHread);


        unsigned int radSetId = DynaToRad::GetRadiossSetIdFromLsdSet(lsdPNSIDRead.GetId(), lsdPNSIDRead.GetKeyword());
        HandleEdit radNsidHEdit;
        p_radiossModel->FindById(p_radiossModel->GetEntityType("/SET/GENERAL"), radSetId, radNsidHEdit);

        if (rbodyHEdit.IsValid())
        {
            EntityEdit rbodyEdit(p_radiossModel, rbodyHEdit);

            HandleNodeEdit nodeHEdit;

            sdiTriple nodeLoc;
            p_ConvertUtils.GetCentroid(lsdPNSIDRead.GetId(), nodeLoc);
            p_radiossModel->CreateNode(nodeHEdit, "/NODE", nodeLoc);
            unsigned int nodeId = nodeHEdit.GetId(p_radiossModel);

            rbodyEdit.SetValue(sdiIdentifier("node_ID"), sdiValue(sdiValueEntity(radNodeType, nodeId)));

            rbodyEdit.SetValue(sdiIdentifier("grnd_ID"), sdiValue(sdiValueEntity(radSetType, radSetId)));

            rbodyEdit.SetValue(sdiIdentifier("TITLE"), sdiValue(seldynaConstGeneWeldSpot->GetName()));
            rbodyEdit.SetValue(sdiIdentifier("MASS"), sdiValue(1e-20));
            rbodyEdit.SetValue(sdiIdentifier("JXX"), sdiValue(1e-20));
            rbodyEdit.SetValue(sdiIdentifier("JYY"), sdiValue(1e-20));
            rbodyEdit.SetValue(sdiIdentifier("JZZ"), sdiValue(1e-20));
            rbodyEdit.SetValue(sdiIdentifier("Ifail"), sdiValue(1));

            double valueFloat;
            sdiValue tempValueFloat;
            tempValueFloat = sdiValue(valueFloat);

            seldynaConstGeneWeldSpot->GetValue(sdiIdentifier("SN"), tempValueFloat);
            tempValueFloat.GetValue(valueFloat);
            rbodyEdit.SetValue(sdiIdentifier("FN"), sdiValue(valueFloat));

            seldynaConstGeneWeldSpot->GetValue(sdiIdentifier("SS"), tempValueFloat);
            tempValueFloat.GetValue(valueFloat);
            rbodyEdit.SetValue(sdiIdentifier("FT"), sdiValue(valueFloat));

            seldynaConstGeneWeldSpot->GetValue(sdiIdentifier("N"), tempValueFloat);
            tempValueFloat.GetValue(valueFloat);
            rbodyEdit.SetValue(sdiIdentifier("expN"), sdiValue(valueFloat));

            seldynaConstGeneWeldSpot->GetValue(sdiIdentifier("M"), tempValueFloat);
            tempValueFloat.GetValue(valueFloat);
            rbodyEdit.SetValue(sdiIdentifier("expT"), sdiValue(valueFloat));

            sdiConvert::SDIHandlReadList sourceconstGeneWeldSpot = { {seldynaConstGeneWeldSpot->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(rbodyHEdit, sourceconstGeneWeldSpot));
        }
    }
}

void sdiD2R::ConvertRigid::HandleInertiaOption(const EntityRead& lsdRgdBody, EntityEdit& nsidEdit, EntityEdit& radRbody)
{
    double lsdXC;
    double lsdYC;
    double lsdZC;
    double lsdTM;
    double lsdIXX;
    double lsdIXY;
    double lsdIXZ;
    double lsdIYY;
    double lsdIYZ;
    double lsdIZZ;
    double lsdVTX;
    double lsdVTY;
    double lsdVTZ;
    double lsdVRX;
    double lsdVRY;
    double lsdVRZ;
    vector<reference_wrapper<double>> lsdDoubleVals({   lsdTM, lsdIXX, lsdIXY, lsdIXZ,
                                                        lsdIYY, lsdIYZ, lsdIZZ,
                                                        lsdVTX, lsdVTY, lsdVTZ,
                                                        lsdVRX, lsdVRY, lsdVRZ,
                                                        lsdXC, lsdYC, lsdZC, });
    vector<sdiString> lsdDoubleAttrNames({"TM", "IXX", "IXY", "IXZ",
                                         "IYY", "IYZ", "IZZ",
                                         "VTX", "VTY", "VTZ",
                                         "VRX", "VRY", "VRZ",
                                         "XC", "YC", "ZC", });
    p_ConvertUtils.GetAttribValues(lsdRgdBody, lsdDoubleAttrNames, lsdDoubleVals);

    vector<sdiString> radDoubleAttrNames({ "Mass", "Jxx", "Jxy", "Jxz", "Jyy", "Jyz", "Jzz" });
    for (size_t i = 0; i < radDoubleAttrNames.size(); ++i)
        radRbody.SetValue(sdiIdentifier(radDoubleAttrNames[i]), sdiValue(lsdDoubleVals[i]));

    sdiValueEntity NodeIdEntity;
    sdiValue tempValue(NodeIdEntity);
    lsdRgdBody.GetValue(sdiIdentifier("NODEID"), tempValue);
    tempValue.GetValue(NodeIdEntity);
    unsigned int nodeId = NodeIdEntity.GetId();
    radRbody.SetValue(sdiIdentifier("ICOG"), sdiValue(4));
    if (!nodeId)
    {
        sdiTriple nodeLoc;
        if (lsdXC != 0.0 || lsdYC != 0.0 || lsdZC || 0.0)
        {
            nodeLoc = sdiTriple(lsdXC, lsdYC, lsdZC);
        }
        else
        {
            sdiValueEntity nsidEntity;
            tempValue = sdiValue(nsidEntity);
            lsdRgdBody.GetValue(sdiIdentifier("NSID"), tempValue);
            tempValue.GetValue(nsidEntity);
            p_ConvertUtils.GetCentroid(nsidEntity.GetId(), nodeLoc);
            radRbody.SetValue(sdiIdentifier("ICOG"), sdiValue(1));
        }
        HandleNodeEdit nodeHEdit;
        p_radiossModel->CreateNode(nodeHEdit, "/NODE", nodeLoc);
        nodeId = nodeHEdit.GetId(p_radiossModel);
    }
    else
    {
        sdiUIntList slaveNodeids;
        p_ConvertUtils.ExtractNodesFromRadiossSet(nsidEdit.GetHandle(), slaveNodeids);

        auto itr = find(slaveNodeids.begin(), slaveNodeids.end(), nodeId);
        if (itr != slaveNodeids.end())
        {
            int clausesMax = 0;
            tempValue = sdiValue(clausesMax);
            nsidEdit.GetValue(sdiIdentifier("clausesmax"), tempValue);
            tempValue.GetValue(clausesMax);
            nsidEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(clausesMax +1));
            nsidEdit.SetValue(sdiIdentifier("KEY_type", 0, clausesMax), sdiValue(sdiString("NODE")));
            nsidEdit.SetValue(sdiIdentifier("opt_D", 0, clausesMax), sdiValue(1));
            nsidEdit.SetValue(sdiIdentifier("idsmax", 0, clausesMax), sdiValue(1));
            nsidEdit.SetValue(sdiIdentifier("ids", 0, clausesMax), sdiValue(sdiValueEntityList(1, sdiUIntList(1, nodeId))));
        }
    }
    radRbody.SetValue(sdiIdentifier("node_ID"), sdiValue(sdiValueEntity(1, nodeId)));

    sdiStringList iniVeltypeList({ "TRA", "ROT" });
    sdiStringList radVelAttrNames({ "Vx", "Vy", "Vz" });
    vector<sdiTriple> radVelVectList({ sdiTriple(lsdVTX, lsdVTY,lsdVTZ), sdiTriple(lsdVRX, lsdVRY, lsdVRZ) });
    HandleEdit radInivelSet;
    for (size_t i = 0; i < 2; ++i)
    {
        if (radVelVectList[i][0] != 0.0 || radVelVectList[i][1] != 0.0 || radVelVectList[i][2] != 0.0)
        {
            HandleEdit inivelHEdit;
            p_radiossModel->CreateEntity(inivelHEdit, "/INIVEL/" + iniVeltypeList[i],"", p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*INITIAL_VELOCITY")));
            EntityEdit inivelEdit(p_radiossModel, inivelHEdit);
            sdiString strFromVelVect;
            for (size_t j = 0; j < 3; ++j)
            {
                strFromVelVect += "/" + to_string(radVelVectList[i][j]);
                inivelEdit.SetValue(sdiIdentifier(radVelAttrNames[j]), sdiValue(radVelVectList[i][j]));
            }
            inivelEdit.SetValue(sdiIdentifier("displayname"), sdiValue(lsdRgdBody.GetName()));
            inivelEdit.SetValue(sdiIdentifier("rad_inivel_type"), sdiValue(iniVeltypeList[i]));
            if (!radInivelSet.IsValid())
            {
                p_radiossModel->CreateEntity(radInivelSet, "/SET/GENERAL", lsdRgdBody.GetName());
                EntityEdit radinivelSetEdit(p_radiossModel, radInivelSet);
                radinivelSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                radinivelSetEdit.SetValue(sdiIdentifier("KEY_type", 0 , 0), sdiValue(sdiString("NODE")));
                radinivelSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                radinivelSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, sdiUIntList(1,nodeId))));
            }
            inivelEdit.SetEntityHandle(sdiIdentifier("Gnod_id"), radInivelSet);
            sdiConvert::SDIHandlReadList sourcehandleList = { {lsdRgdBody.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(inivelHEdit, sourcehandleList));
        }
    }
}

void sdiD2R::ConvertRigid::HandleSPCOption(const EntityRead& srcEntityRead, EntityEdit& radRbody)
{

    sdiValueEntity masterNodeEntity;
    sdiValue tempValue(masterNodeEntity);
    radRbody.GetValue(sdiIdentifier("node_ID"), tempValue);
    tempValue.GetValue(masterNodeEntity);
    if (masterNodeEntity.GetId())
    {
        int lsdCON1;
        int lsdCON2;
        double lsdCMO = 0.0;
        map<int, sdiString> mapCONVsConstraints
        (
            {
                {1,sdiString("100")},
                {2,sdiString("010")},
                {3,sdiString("001")},
                {4,sdiString("110")},
                {5,sdiString("011")},
                {6,sdiString("101")},
                {7,sdiString("111")},
            }
        );

        tempValue = sdiValue(lsdCMO);
        srcEntityRead.GetValue(sdiIdentifier("CMO"), tempValue);
        tempValue.GetValue(lsdCMO);

        sdiString constraintStr;
        constraintStr.reserve(6);
        unsigned int con1SystId = 0;
        unsigned int rgdBodyCID = 0;
        if (lsdCMO == 1.0)
        {
            vector<reference_wrapper<int>> lsdIntVals({ lsdCON1, lsdCON2 });
            vector<sdiString> lsdIntAttrNames({ "LSD_CON1", "LSD_CON2int" });
            p_ConvertUtils.GetAttribValues(srcEntityRead, lsdIntAttrNames, lsdIntVals);

            if (lsdCON1)
            {
                constraintStr = mapCONVsConstraints[lsdCON1];
                if (mapCONVsConstraints[lsdCON2].empty())
                    constraintStr += "000";
            }
            if (lsdCON2)
            {
                if (mapCONVsConstraints[lsdCON1].empty())
                    constraintStr = "000";
                constraintStr += mapCONVsConstraints[lsdCON2];
            }
        }
        else if (lsdCMO == -1.0)
        {
            sdiString keyWord = srcEntityRead.GetKeyword();
            if (keyWord.find("MAT_RIGID") != keyWord.npos)
            {
                sdiValueEntity con1SystemEntity;
                tempValue = sdiValue(con1SystemEntity);
                srcEntityRead.GetValue(sdiIdentifier("CON1"), tempValue);
                tempValue.GetValue(con1SystemEntity);
                con1SystId = con1SystemEntity.GetId();
                int ConstXTra = 0;
                int ConstYTra = 0;
                int ConstZTra = 0;
                int ConstXRot = 0;
                int ConstYRot = 0;
                int ConstZRot = 0;
                vector<reference_wrapper<int>> lsdIntAttrValues({ ConstXTra , ConstYTra , ConstZTra, ConstXRot, ConstYRot, ConstZRot });
                vector<sdiString> lsdIntAttrNames({ "ConstrainedXTranslation", "ConstrainedYTranslation", "ConstrainedZTranslation", "ConstrainedXRotation", "ConstrainedYRotation", "ConstrainedZRotation" });
                p_ConvertUtils.GetAttribValues(srcEntityRead, lsdIntAttrNames, lsdIntAttrValues);
                constraintStr = sdiString((to_string(ConstXTra)) +
                    to_string(ConstYTra) +
                    to_string(ConstZTra) +
                    to_string(ConstXRot) +
                    to_string(ConstYRot) +
                    to_string(ConstZRot));
            }
            else
            {
                sdiValueEntity con1SystemEntity;
                tempValue = sdiValue(con1SystemEntity);
                srcEntityRead.GetValue(sdiIdentifier("LSD_CON1_SID"), tempValue);
                tempValue.GetValue(con1SystemEntity);
                con1SystId = con1SystemEntity.GetId();

                tempValue = sdiValue(constraintStr);
                srcEntityRead.GetValue(sdiIdentifier("LSDYNA_CON2"), tempValue);
                tempValue.GetValue(constraintStr);
            }
        }
        if (!constraintStr.empty())
        {
            HandleEdit bcsHEdit;
            p_radiossModel->CreateEntity(bcsHEdit, "/BCS", "", p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*BOUNDARY")));
            EntityEdit bcsEdit(p_radiossModel, bcsHEdit);
            bcsEdit.SetValue(sdiIdentifier("displayname"), sdiValue(sdiString("BCS_RIGID_" + constraintStr)));
            if (con1SystId)
                bcsEdit.SetValue(sdiIdentifier("inputsystem"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SKEW"), con1SystId)));
            else if (rgdBodyCID)
                bcsEdit.SetValue(sdiIdentifier("inputsystem"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SKEW"), rgdBodyCID)));

            for (size_t i = 0; i < 6; ++i)
            {
                bcsEdit.SetValue(sdiIdentifier("dof" + to_string(i + 1)), sdiValue((int)(constraintStr[i] - 48)));
            }

            HandleEdit radInivelSet;
            p_radiossModel->CreateEntity(radInivelSet, "/SET/GENERAL", "SET_GEN_BCS_RIGID_" + constraintStr);
            EntityEdit radinivelSetEdit(p_radiossModel, radInivelSet);
            radinivelSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
            radinivelSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
            radinivelSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
            radinivelSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, sdiUIntList(1, masterNodeEntity.GetId()))));
            bcsEdit.SetEntityHandle(sdiIdentifier("entityid"), radInivelSet);

            sdiConvert::SDIHandlReadList sourcehandleList = { {srcEntityRead.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(bcsHEdit, sourcehandleList));
        }
    }
}

void sdiD2R::ConvertRigid::p_CreateTHRBody()
{
    sdiUIntList allRbodyList;
    SelectionRead selDatabaseRbdout(p_lsdynaModel, "*DATABASE_RBDOUT");
    if (selDatabaseRbdout.Count())
    {
        SelectionRead selRbody(p_radiossModel, "/RBODY");
        allRbodyList.reserve(selRbody.Count());
        while (selRbody.Next())
            allRbodyList.push_back(selRbody->GetId());
    }
    HandleEdit radThRbodyHEdit;
    if (!allRbodyList.empty() || !p_IprtRbodies.empty())
        p_radiossModel->CreateEntity(radThRbodyHEdit, "/TH/RBODY", "TH-RBODY");

    if (radThRbodyHEdit.IsValid())
    {
        EntityEdit radThRbodyEdit(p_radiossModel, radThRbodyHEdit);
        radThRbodyEdit.SetValue(sdiIdentifier("Number_Of_Variables"), sdiValue(1));

        radThRbodyEdit.SetValue(sdiIdentifier("var"), sdiValue(sdiStringList({ "DEF" })));

        if (!allRbodyList.empty())
        {
            radThRbodyEdit.SetValue(sdiIdentifier("idsmax"), sdiValue((int)allRbodyList.size()));
            radThRbodyHEdit.SetValue(p_radiossModel, sdiIdentifier("Obj"), sdiValue(sdiValueEntityList(destEntityType, allRbodyList)));
            sdiConvert::SDIHandlReadList sourceHandles = { {selDatabaseRbdout->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radThRbodyHEdit, sourceHandles));
        }
        else if (!p_IprtRbodies.empty())
        {
            radThRbodyEdit.SetValue(sdiIdentifier("idsmax"), sdiValue((int)p_IprtRbodies.size()));
            radThRbodyHEdit.SetValue(p_radiossModel, sdiIdentifier("Obj"), sdiValue(sdiValueEntityList(destEntityType, p_IprtRbodies)));
        }
    }
}

void sdiD2R::ConvertRigid::p_CreateNewSkewForIRCSFlag(const EntityRead& lsdRgdBody, EntityEdit& radRbodyEdit)
{
    double lsdXL = 0.0;
    double lsdYL = 0.0;
    double lsdZL = 0.0;
    double lsdXLIP = 0.0;
    double lsdYLIP = 0.0;
    double lsdZLIP = 0.0;

    vector<reference_wrapper<double>> localCoordVals({ lsdXL, lsdYL, lsdZL, lsdXLIP, lsdYLIP, lsdZLIP });
    sdiStringList localCoordAttrNames({ "XL", "YL", "ZL", "XLIP", "YLIP", "ZLIP" });
    p_ConvertUtils.GetAttribValues(lsdRgdBody, localCoordAttrNames, localCoordVals);

    sdiTriple origin(0.0, 0.0, 0.0);
    sdiTriple vectL(lsdXL, lsdYL, lsdZL);
    sdiTriple vectLIP(lsdXLIP, lsdYLIP, lsdZLIP);
    sdiTriple vectX2(vectL * vectLIP);
    sdiTriple vectX1(vectX2 * vectL);

    HandleEdit radSkewHEdit;
    p_radiossModel->CreateEntity(radSkewHEdit, "/SKEW/FIX");
    if (radSkewHEdit.IsValid())
    {
        EntityEdit radSkewEdit(p_radiossModel, radSkewHEdit);
        vector<sdiString> attribOrigin({ "Ox", "Oy", "Oz" });
        vector<sdiString> attribVect1({ "X1", "Y1", "Z1" });
        vector<sdiString> attribVect2({ "X2", "Y2", "Z2" });
        for (size_t i = 0; i < 3; ++i)
        {
            radSkewEdit.SetValue(sdiIdentifier(attribOrigin[i]), sdiValue(origin[i]));
            radSkewEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(vectX1[i]));
            radSkewEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(vectX2[i]));
        }
        radSkewEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(sdiString(lsdRgdBody.GetName())));
        radRbodyEdit.SetEntityHandle(sdiIdentifier("Skew_ID"), radSkewHEdit);

        sdiConvert::SDIHandlReadList sourcergdBody = { {lsdRgdBody.GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(radSkewHEdit, sourcergdBody));
    }
}

void sdiD2R::ConvertRigid::p_PushPnodeIntoSlaveSet(const unsigned int& pnodeId, const HandleRead lsdSlaveSet, EntityEdit& radSlaveSet)
{
    EntityRead lsdSlaveSetRead(p_lsdynaModel, lsdSlaveSet);
    sdiString lsdNsidKeyWord = lsdSlaveSetRead.GetKeyword();
    size_t pos = lsdNsidKeyWord.find("_TITLE");
    if (pos != string::npos)
        lsdNsidKeyWord.erase(pos);

    if (lsdNsidKeyWord == "*SET_NODE" || lsdNsidKeyWord == "*SET_NODE_LIST")
    {
        sdiUIntList slaveNodeids;
        p_ConvertUtils.ExtractNodesFromRadiossSet(radSlaveSet.GetHandle(), slaveNodeids);
        auto itr = find(slaveNodeids.begin(), slaveNodeids.end(), pnodeId);
        if (itr == slaveNodeids.end())
        {
            slaveNodeids.push_back(pnodeId);
            radSlaveSet.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue((int)slaveNodeids.size()));
            radSlaveSet.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, slaveNodeids)));
        }
    }
    else
    {
        int clausesMax = 0;
        sdiValue tempVal(clausesMax);
        radSlaveSet.GetValue(sdiIdentifier("clausesmax"), tempVal);
        tempVal.GetValue(clausesMax);

        radSlaveSet.SetValue(sdiIdentifier("KEY_type", 0, clausesMax), sdiValue(sdiString("NODE")));
        radSlaveSet.SetValue(sdiIdentifier("idsmax", 0, clausesMax), sdiValue(1));
        radSlaveSet.SetValue(sdiIdentifier("ids", 0, clausesMax), sdiValue(sdiValueEntityList(1, sdiUIntList(1,pnodeId))));
        radSlaveSet.SetValue(sdiIdentifier("clausesmax"), sdiValue(clausesMax + 1));
    }
}
