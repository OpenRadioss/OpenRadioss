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

#include <dyna2rad/convertbcs.h>
#include <dyna2rad/dyna2rad.h>
#include <dyna2rad/sdiUtils.h>

using namespace sdi;
using namespace std;

void UpdateIMPCard(EntityEdit& impEdit, int lsdVAD, const sdiString& displayName, const EntityType& entityType, 
    unsigned int entityId, const EntityType& lcidType, unsigned int lcidId, const EntityType& skewType, 
    unsigned int skewId, double lsdSF, double lsdDEATH, double lsdBIRTH, const sdiString& lsdDIR);

void sdiD2R::ConvertBcs::ConvertAllBcs()
{
    ConvertEntities();
}

void sdiD2R::ConvertBcs::ConvertEntities()
{
    ConvertBoundarySPC();

    ConvertBoundaryMotion();
}

void sdiD2R::ConvertBcs::ConvertBoundarySPC()
{
    EntityType radSkewType = p_radiossModel->GetEntityType("/SKEW");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    SelectionRead selBoundSpc(p_lsdynaModel, "*BOUNDARY_SPC");
    while (selBoundSpc.Next())
    {
        sdiString keyWord = selBoundSpc->GetKeyword();
        if (keyWord.find("SYMMETRY") != keyWord.npos)
            continue;
        sdiValueEntity lsdEntity;
        sdiValueEntity LsdCID;
        vector<reference_wrapper<sdiValueEntity>> lsdAttrVals({ lsdEntity, LsdCID });
        vector<sdiString> lsdAttrNames({ "LSD_NSID", "CID" });
        p_ConvertUtils.GetAttribValues(*selBoundSpc, lsdAttrNames, lsdAttrVals);

        
        bool isBcs = true;
        if (keyWord.find("SET") != keyWord.npos && lsdEntity.GetId() == 0)
        {
            isBcs = false;
            DynaToRad::ShowMessage(sdiMessageHandler::Level::Warning, 37,
                sdiString("*BOUNDARY_SPC_SET").c_str(),
                selBoundSpc->GetId(), selBoundSpc->GetName().c_str());
        }

        int lsdDOF_X;
        int lsdDOF_Y;
        int lsdDOF_Z;
        int lsdDOF_RX;
        int lsdDOF_RY;
        int lsdDOF_RZ;
        vector<reference_wrapper<int>> lsdIntAttrVals({ lsdDOF_X, lsdDOF_Y, lsdDOF_Z, lsdDOF_RX, lsdDOF_RY, lsdDOF_RZ });
        lsdAttrNames = { "DOF_X", "DOF_Y", "DOF_Z", "DOF_RX", "DOF_RY", "DOF_RZ" };
        p_ConvertUtils.GetAttribValues(*selBoundSpc, lsdAttrNames, lsdIntAttrVals);

        HandleEdit bcsHEdit;
        unsigned int bcsId = selBoundSpc->GetId();
        if (!p_radiossModel->IsIdAvailable(destEntityType, bcsId))
            bcsId = p_ConvertUtils.GetDynaMaxEntityID(srcEntityType);
        if(isBcs) 
            p_radiossModel->CreateEntity(bcsHEdit, "/BCS", selBoundSpc->GetName(), bcsId);
        if (bcsHEdit.IsValid())
        {
            EntityEdit bcsEdit(p_radiossModel, bcsHEdit);
            sdiStringList radDofAttrNames({ "dof1", "dof2", "dof3", "dof4", "dof5", "dof6" });
            for (size_t i = 0; i < 6; ++i)
            {
                int dofVal = lsdIntAttrVals[i];
                if (dofVal == 0 || dofVal == 1)
                    bcsEdit.SetValue(sdiIdentifier(radDofAttrNames[i]), sdiValue(dofVal));
            }
            bcsEdit.SetValue(sdiIdentifier("displayname"), sdiValue(selBoundSpc->GetName()));
            bcsEdit.SetValue(sdiIdentifier("skew_ID"), sdiValue(sdiValueEntity(radSkewType, LsdCID.GetId())));

            sdiConvert::SDIHandlReadList sourceBcs = { {selBoundSpc->GetHandle()} };

            if (keyWord.find("SET") != keyWord.npos)
            {
                HandleRead nSetHread;
                selBoundSpc->GetEntityHandle(sdiIdentifier("LSD_NSID"), nSetHread);
                if (nSetHread.IsValid())
                {

                    EntityRead setRead(p_lsdynaModel, nSetHread);

                    sdiUIntList slaveNodesBcs;
                    p_ConvertUtils.GetNodeIdsFromNodeSet(setRead.GetId(), slaveNodesBcs);

                    sdiVectorSort(slaveNodesBcs);
                    sdiVectorUnique(slaveNodesBcs);

                    SelectionEdit selectRgdBody(p_radiossModel, "/RBODY");
                    sdiUIntList slaveNodeslRigidBody;

                    unsigned int NMasterNodeId;
                    unsigned int NsidId;

                    bool isFound = false;
                    while (selectRgdBody.Next() && !isFound)
                    { 
                        sdiValue tempValue;
                        HandleRead NsidHRead;

                        sdiValueEntity NMasterNode;
                        selectRgdBody->GetValue(sdiIdentifier("node_ID"), tempValue);
                        tempValue.GetValue(NMasterNode);
                        NMasterNodeId=NMasterNode.GetId();

                        sdiValueEntity Nsid;
                        selectRgdBody->GetValue(sdiIdentifier("grnd_ID"), tempValue);
                        tempValue.GetValue(Nsid);
                        NsidId=Nsid.GetId();

                        p_radiossModel->FindById(p_radiossModel->GetEntityType("/SET/GENERAL"), NsidId, NsidHRead);

                        p_ConvertUtils.ExtractNodesFromRadiossSet(NsidHRead, slaveNodeslRigidBody);

                        sdiVectorSort(slaveNodeslRigidBody);
                        sdiVectorUnique(slaveNodeslRigidBody);

                        sdiUIntList intersectList;
                        intersectList.assign(std::max(slaveNodeslRigidBody.size(), slaveNodesBcs.size()),0);
                        sdiUIntList::iterator it = std::set_intersection(slaveNodeslRigidBody.begin(), slaveNodeslRigidBody.end(),
                            slaveNodesBcs.begin(), slaveNodesBcs.end(), intersectList.begin());
                
                        intersectList.resize(it-intersectList.begin()); 
                        
                        if (intersectList.size() != 0)
                        {
                            sdiUIntList differenceList;
                            differenceList.assign(std::max(slaveNodeslRigidBody.size(), slaveNodesBcs.size()),0);
                            sdiUIntList::iterator it = std::set_difference(slaveNodeslRigidBody.begin(), slaveNodeslRigidBody.end(),
                                slaveNodesBcs.begin(), slaveNodesBcs.end(), differenceList.begin());
                
                            differenceList.resize(it-differenceList.begin()); 
                            differenceList.push_back(NMasterNodeId); 
                            
                            HandleEdit setHEdit;
                            p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", "SET_GENERAL_BOUNDARY_SPC_" + to_string(selBoundSpc->GetId()));
                            EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

                            SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
                            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1)); 
                            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, differenceList))); 

                            bcsEdit.SetEntityHandle(sdiIdentifier("grnod_ID"), setHEdit);
                            sdiConvert::Convert::PushToConversionLog(std::make_pair(setHEdit, sourceBcs));
                            isFound = true;
                        }
                        else
                        {
                            bcsEdit.SetValue(sdiIdentifier("grnod_ID"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(setRead.GetId(), "*SET_NODE"))));
                        }
                    }
                    if (!isFound) bcsEdit.SetValue(sdiIdentifier("grnod_ID"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(setRead.GetId(), "*SET_NODE"))));
                }
            }
            else
            {
                SelectionEdit selectRgdBody(p_radiossModel, "/RBODY");
                sdiUIntList slaveNodeslRigidBody;
                sdiUIntList slaveNodesBcs;
                HandleEdit setHEdit;

                unsigned int NMasterNodeId;
                unsigned int NsidId;

                bool isFound = false;
                while (selectRgdBody.Next() && !isFound)
                {
                    sdiValue tempValue;
                    HandleRead NsidHRead;

                    sdiValueEntity NMasterNode;
                    selectRgdBody->GetValue(sdiIdentifier("node_ID"), tempValue);
                    tempValue.GetValue(NMasterNode);
                    NMasterNodeId=NMasterNode.GetId();

                    sdiValueEntity Nsid;
                    selectRgdBody->GetValue(sdiIdentifier("grnd_ID"), tempValue);
                    tempValue.GetValue(Nsid);
                    NsidId=Nsid.GetId();

                    p_radiossModel->FindById(p_radiossModel->GetEntityType("/SET/GENERAL"), NsidId, NsidHRead);

                    p_ConvertUtils.ExtractNodesFromRadiossSet(NsidHRead, slaveNodeslRigidBody);

                    sdiVectorSort(slaveNodeslRigidBody);
                    sdiVectorUnique(slaveNodeslRigidBody);
                    
                    slaveNodesBcs.push_back(lsdEntity.GetId());
                    
                    sdiUIntList intersectList;
                    intersectList.assign(std::max(slaveNodeslRigidBody.size(), slaveNodesBcs.size()),0);
                    sdiUIntList::iterator it = std::set_intersection(slaveNodeslRigidBody.begin(), slaveNodeslRigidBody.end(),
                        slaveNodesBcs.begin(), slaveNodesBcs.end(), intersectList.begin());
                    
                    intersectList.resize(it-intersectList.begin()); 
                        
                    if (intersectList.size() != 0)
                    {

                        p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", "SET_GENERAL_BOUNDARY_SPC_" + to_string(selBoundSpc->GetId()));
                        EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

                        SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                        SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
                        SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));

                        selectRgdBody->SetValue(sdiIdentifier("ICOG"), sdiValue(3));

                        SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, sdiUIntList(1, NMasterNodeId))));
                        bcsEdit.SetEntityHandle(sdiIdentifier("grnod_ID"), setHEdit);


                        sdiTriple nodeLoc;
                        HandleRead nodeHRead;
                        p_radiossModel->FindById(1, lsdEntity.GetId(), nodeHRead);
                        NodeRead nodeRead(p_radiossModel, nodeHRead);
                        nodeLoc = nodeRead.GetPosition();

                        HandleEdit NMasterNodeHEdit;
                        p_radiossModel->FindById(1, NMasterNodeId, NMasterNodeHEdit);

                        NodeEdit NMasterNodeEdit(p_radiossModel, NMasterNodeHEdit);
                        NMasterNodeEdit.SetPosition(nodeLoc);

                        isFound = true;
                    }
                }

                if (!isFound) 
                {
                        
                    p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", "SET_GENERAL_BOUNDARY_SPC_" + to_string(selBoundSpc->GetId()));
                    EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

                    SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                    SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
                    SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                    SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, sdiUIntList(1, lsdEntity.GetId()))));

                    bcsEdit.SetEntityHandle(sdiIdentifier("grnod_ID"), setHEdit);
                }

                sdiConvert::Convert::PushToConversionLog(std::make_pair(setHEdit, sourceBcs));
            }
            sdiConvert::Convert::PushToConversionLog(std::make_pair(bcsHEdit, sourceBcs));
        }
    }
}

void sdiD2R::ConvertBcs::ConvertBoundaryMotion()
{
    SelectionRead selBoundaryMotion(p_lsdynaModel, "*BOUNDARY_PRESCRIBED_MOTION");
    while (selBoundaryMotion.Next())
    {
        double lsdDEATH;
        double lsdBIRTH;
        double lsdSF;
        sdiString keyWord;
        int lsdVAD=0;
        int lsdDOF;
        unsigned int setId;
        sdiValueEntity lsdEntity;
        sdiValueEntity lsdLCID;
        sdiValueEntity lsdVID;
        HandleEdit impHEdit;
        HandleEdit impYHEdit;
        HandleEdit impZHEdit;
        HandleEdit setHEdit;
        HandleEdit zeroFunctEdit;

        sdiValue tempValue;
        sdiString destCard;
        sdiConvert::ContainIntVsStr mapDofVsDir({ {1,"X"},{2, "Y"},{3, "Z"},{4, "X"}, {5, "XX"},{6, "YY"},{7, "ZZ"},{8, "XX"} });
        sdiConvert::SDIHandlReadList sourcehandleList = { {selBoundaryMotion->GetHandle()} };

        selBoundaryMotion->GetValue(sdiIdentifier("VAD"), tempValue);
        tempValue.GetValue(lsdVAD);

        if (lsdVAD > 3)
            continue;

        selBoundaryMotion->GetValue(sdiIdentifier("DOF"), tempValue);
        tempValue.GetValue(lsdDOF);

        vector<reference_wrapper<sdiValueEntity>> lsdEntityAttribVals({ lsdEntity, lsdLCID, lsdVID });
        vector <sdiString> lsdEntityAttribNames({ "typeID", "LCID", "VID" });
        p_ConvertUtils.GetAttribValues(*selBoundaryMotion, lsdEntityAttribNames, lsdEntityAttribVals);

        vector<reference_wrapper<double>> lsdAttribVals({ lsdSF, lsdDEATH, lsdBIRTH });
        vector <sdiString> lsdAttribNames({ "SF", "DEATH", "BIRTH" });
        p_ConvertUtils.GetAttribValues(*selBoundaryMotion, lsdAttribNames, lsdAttribVals);

        keyWord = selBoundaryMotion->GetKeyword();
        destCard = (lsdVAD == 0 ) ? "/IMPVEL" : (lsdVAD == 1) ? "/IMPACC" : (lsdVAD == 2) ? "/IMPDISP" : "";

        unsigned int impMotionId = selBoundaryMotion->GetId();
        if (keyWord.find("RIGID") != keyWord.npos && lsdVAD == 3)
        {
            destCard = "/IMPVEL";
            DynaToRad::ShowMessage(sdiMessageHandler ::Level::Warning, 2,
                sdiString("*BOUNDARY_PRESCRIBED_MOTION_RIGID").c_str(), impMotionId, selBoundaryMotion->GetName().c_str());
        }

        if (destCard.empty())
            continue;
        unsigned int radSkew = DynaToRad::GetRadiossSkewIdFromLsdVID(lsdVID.GetId());

        if (!p_radiossModel->IsIdAvailable(p_radiossModel->GetEntityType(destCard), impMotionId))
            impMotionId = p_ConvertUtils.GetRadiossMaxEntityID(p_radiossModel->GetEntityType(destCard));
        p_radiossModel->CreateEntity(impHEdit, destCard, selBoundaryMotion->GetName(), impMotionId);
        EntityEdit impEdit(p_radiossModel, impHEdit);

        if (keyWord.find("NODE") != keyWord.npos)
        {
            p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", selBoundaryMotion->GetName());
            EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

            sdiUIntList slaveNodesBcs;
            slaveNodesBcs.push_back(lsdEntity.GetId());



            SelectionEdit selectRgdBody(p_radiossModel, "/RBODY");
            sdiUIntList slaveNodeslRigidBody;

            unsigned int NMasterNodeId;
            unsigned int NsidId;

            bool isFound = false;
            while (selectRgdBody.Next() && !isFound)
            { 
                sdiValue tempValue;
                HandleRead NsidHRead;

                sdiValueEntity NMasterNode;
                selectRgdBody->GetValue(sdiIdentifier("node_ID"), tempValue);
                tempValue.GetValue(NMasterNode);
                NMasterNodeId=NMasterNode.GetId();

                sdiValueEntity Nsid;
                selectRgdBody->GetValue(sdiIdentifier("grnd_ID"), tempValue);
                tempValue.GetValue(Nsid);
                NsidId=Nsid.GetId();

                p_radiossModel->FindById(p_radiossModel->GetEntityType("/SET/GENERAL"), NsidId, NsidHRead);

                p_ConvertUtils.ExtractNodesFromRadiossSet(NsidHRead, slaveNodeslRigidBody);

                std::sort(slaveNodeslRigidBody.begin(), slaveNodeslRigidBody.end());
                slaveNodeslRigidBody.erase(std::unique(slaveNodeslRigidBody.begin(), slaveNodeslRigidBody.end()), slaveNodeslRigidBody.end());
 


                sdiUIntList intersectList;
                intersectList.assign(std::max(slaveNodeslRigidBody.size(), slaveNodesBcs.size()),0);
                sdiUIntList::iterator it = std::set_intersection(slaveNodeslRigidBody.begin(), slaveNodeslRigidBody.end(),
                    slaveNodesBcs.begin(), slaveNodesBcs.end(), intersectList.begin());
            
                intersectList.resize(it-intersectList.begin()); 

                if (intersectList.size() != 0)
                {
                    sdiUIntList nodeList;
                    nodeList.push_back(NMasterNodeId); 
                    

                    SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                    SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
                    SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1)); 
                    SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, nodeList))); 

                    setId = DynaToRad::GetRadiossSetIdFromLsdSet(SetEntityEdit.GetId(), "*SET_NODE");
                    //bcsEdit.SetEntityHandle(sdiIdentifier("grnod_ID"), setHEdit);
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(setHEdit, sourcehandleList));
                    isFound = true;
                }
                else
                {
                    SetEntityEdit.SetValue(sdiIdentifier("set_Type"), sdiValue(sdiString("GENERAL")));
                    SetEntityEdit.SetValue(sdiIdentifier("iset_Type"), sdiValue(0));
                    SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                    SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
                    SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                    SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, sdiUIntList(1, lsdEntity.GetId()))));
                    setId = SetEntityEdit.GetId();
                }
            }
            if(isFound == false)   
            {     
                SetEntityEdit.SetValue(sdiIdentifier("set_Type"), sdiValue(sdiString("GENERAL")));
                SetEntityEdit.SetValue(sdiIdentifier("iset_Type"), sdiValue(0));
                SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
                SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, sdiUIntList(1, lsdEntity.GetId()))));
                setId = SetEntityEdit.GetId();
            }

        }
        else if (keyWord.find("RIGID") != keyWord.npos)
        {
            p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", selBoundaryMotion->GetName());
            EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

            unsigned int pid = lsdEntity.GetId();
            unsigned int nodeId = 0;
            setId = 0;
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
                SetEntityEdit.SetValue(sdiIdentifier("set_Type"), sdiValue(sdiString("GENERAL")));
                SetEntityEdit.SetValue(sdiIdentifier("iset_Type"), sdiValue(0));
                SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
                SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, sdiUIntList(1, nodeId))));
                setId = setHEdit.GetId(p_radiossModel);
            }
            else
            {
                // warning / error no pid found
            }

        }
        else
        {
            SelectionEdit selectRgdBody(p_radiossModel, "/RBODY");
            if (selectRgdBody.Count() > 0)
            {
                HandleRead nSetHread;
                selBoundaryMotion->GetEntityHandle(sdiIdentifier("typeID"), nSetHread);
                EntityRead setRead(p_lsdynaModel, nSetHread);

                sdiUIntList slaveNodesBcs;
                p_ConvertUtils.GetNodeIdsFromNodeSet(setRead.GetId(), slaveNodesBcs);

                std::sort(slaveNodesBcs.begin(), slaveNodesBcs.end());
                slaveNodesBcs.erase(std::unique(slaveNodesBcs.begin(), slaveNodesBcs.end()), slaveNodesBcs.end());

                sdiUIntList slaveNodeslRigidBody;

                unsigned int NMasterNodeId;
                unsigned int NsidId;

                bool isFound = false;
                sdiUIntList nodeList;
                while (selectRgdBody.Next())
                {
                    sdiValue tempValue;
                    HandleRead NsidHRead;

                    sdiValueEntity NMasterNode;
                    selectRgdBody->GetValue(sdiIdentifier("node_ID"), tempValue);
                    tempValue.GetValue(NMasterNode);
                    NMasterNodeId=NMasterNode.GetId();

                    sdiValueEntity Nsid;
                    selectRgdBody->GetValue(sdiIdentifier("grnd_ID"), tempValue);
                    tempValue.GetValue(Nsid);
                    NsidId=Nsid.GetId();

                    p_radiossModel->FindById(p_radiossModel->GetEntityType("/SET/GENERAL"), NsidId, NsidHRead);

                    p_ConvertUtils.ExtractNodesFromRadiossSet(NsidHRead, slaveNodeslRigidBody);

                    std::sort(slaveNodeslRigidBody.begin(), slaveNodeslRigidBody.end());
                    slaveNodeslRigidBody.erase(std::unique(slaveNodeslRigidBody.begin(), slaveNodeslRigidBody.end()), slaveNodeslRigidBody.end());

                    sdiUIntList intersectList;
                    intersectList.assign(std::max(slaveNodeslRigidBody.size(), slaveNodesBcs.size()),0);
                    sdiUIntList::iterator it = std::set_intersection(slaveNodeslRigidBody.begin(), slaveNodeslRigidBody.end(),
                        slaveNodesBcs.begin(), slaveNodesBcs.end(), intersectList.begin());
            
                    intersectList.resize(it-intersectList.begin()); 
                    
                    if (intersectList.size() != 0)
                    {
                        nodeList.push_back(NMasterNodeId); 
                        isFound = true;
                    }
                    else
                    {
                        sdiUIntList tmpNodeList;
                        p_ConvertUtils.ExtractNodesFromRadiossSet(NsidHRead, tmpNodeList);
                        //nodeList.merge(tmpNodeList);
                        for(int j = 0 ; j < tmpNodeList.size() ; j++ )
                        {
                            nodeList.push_back(tmpNodeList[j]);
                        }
                    }
                }

                if(isFound == true)   
                {     
                    HandleEdit setHEdit;
                    p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", selBoundaryMotion->GetName());
                    EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

                    SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                    SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
                    SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1)); 
                    SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, nodeList))); 

                    setId = DynaToRad::GetRadiossSetIdFromLsdSet(SetEntityEdit.GetId(), "*SET_NODE");
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(setHEdit, sourcehandleList));
                }
                else 
                {    
                    setId = DynaToRad::GetRadiossSetIdFromLsdSet(lsdEntity.GetId(), "*SET_NODE");
                } 
            }
            else
            {
                setId = DynaToRad::GetRadiossSetIdFromLsdSet(lsdEntity.GetId(), "*SET_NODE");
            }
        }

        if (lsdDOF > 0 && lsdDOF < 9)
            UpdateIMPCard(impEdit, lsdVAD, selBoundaryMotion->GetName(), p_radiossModel->GetEntityType("/SET"), setId,
                p_radiossModel->GetEntityType("/FUNCT"), lsdLCID.GetId(), p_radiossModel->GetEntityType("/SKEW"),
                radSkew, lsdSF, lsdDEATH, lsdBIRTH, mapDofVsDir[lsdDOF]);
        else if (lsdDOF == -4)
        {
            UpdateIMPCard(impEdit, lsdVAD, selBoundaryMotion->GetName(), p_radiossModel->GetEntityType("/SET"), setId,
                p_radiossModel->GetEntityType("/FUNCT"), lsdLCID.GetId(), p_radiossModel->GetEntityType("/SKEW"),
                radSkew, lsdSF, lsdDEATH, lsdBIRTH, "X");

            p_ConvertUtils.CreateCurve("funct_IDT" + to_string(selBoundaryMotion->GetId()), 2, { { 0.0, 0.0 , 1.0, 0.0 } }, zeroFunctEdit);
            p_radiossModel->CreateEntity(impYHEdit, destCard, selBoundaryMotion->GetName(), p_ConvertUtils.GetRadiossMaxEntityID(p_radiossModel->GetEntityType(destCard)));
            EntityEdit impYEdit(p_radiossModel, impYHEdit);
            UpdateIMPCard(impYEdit, lsdVAD, selBoundaryMotion->GetName(), p_radiossModel->GetEntityType("/SET"), setId,
                p_radiossModel->GetEntityType("/FUNCT"), zeroFunctEdit.GetId(p_radiossModel), p_radiossModel->GetEntityType("/SKEW"),
                radSkew, lsdSF, lsdDEATH, lsdBIRTH, "Y");
           
            p_radiossModel->CreateEntity(impZHEdit, destCard, selBoundaryMotion->GetName(), p_ConvertUtils.GetRadiossMaxEntityID(p_radiossModel->GetEntityType(destCard)));
            EntityEdit impZEdit(p_radiossModel, impZHEdit);
            UpdateIMPCard(impZEdit, lsdVAD, selBoundaryMotion->GetName(), p_radiossModel->GetEntityType("/SET"), setId,
                p_radiossModel->GetEntityType("/FUNCT"), zeroFunctEdit.GetId(p_radiossModel), p_radiossModel->GetEntityType("/SKEW"),
                radSkew, lsdSF, lsdDEATH, lsdBIRTH, "Z");
        }
        else if (lsdDOF == -8)
        {
            UpdateIMPCard(impEdit, lsdVAD, selBoundaryMotion->GetName(), p_radiossModel->GetEntityType("/SET"), setId,
                p_radiossModel->GetEntityType("/FUNCT"), lsdLCID.GetId(), p_radiossModel->GetEntityType("/SKEW"),
                radSkew, lsdSF, lsdDEATH, lsdBIRTH, "XX");

            p_ConvertUtils.CreateCurve("funct_IDT" + to_string(selBoundaryMotion->GetId()), 2, { { 0.0, 0.0 , 1.0, 0.0 } }, zeroFunctEdit);
            p_radiossModel->CreateEntity(impYHEdit, destCard, selBoundaryMotion->GetName(), p_ConvertUtils.GetRadiossMaxEntityID(p_radiossModel->GetEntityType(destCard)));
            EntityEdit impYEdit(p_radiossModel, impYHEdit);
            UpdateIMPCard(impYEdit, lsdVAD, selBoundaryMotion->GetName(), p_radiossModel->GetEntityType("/SET"), setId,
                p_radiossModel->GetEntityType("/FUNCT"), zeroFunctEdit.GetId(p_radiossModel), p_radiossModel->GetEntityType("/SKEW"),
                radSkew, lsdSF, lsdDEATH, lsdBIRTH, "YY");

            p_radiossModel->CreateEntity(impZHEdit, destCard, selBoundaryMotion->GetName(), p_ConvertUtils.GetRadiossMaxEntityID(p_radiossModel->GetEntityType(destCard)));
            EntityEdit impZEdit(p_radiossModel, impZHEdit);
            UpdateIMPCard(impZEdit, lsdVAD, selBoundaryMotion->GetName(), p_radiossModel->GetEntityType("/SET"), setId,
                p_radiossModel->GetEntityType("/FUNCT"), zeroFunctEdit.GetId(p_radiossModel), p_radiossModel->GetEntityType("/SKEW"),
                radSkew, lsdSF, lsdDEATH, lsdBIRTH, "ZZ");
        }

        for (HandleEdit tempHEdit : {impHEdit, impYHEdit, impZHEdit, setHEdit, zeroFunctEdit })
        {
            if (tempHEdit.IsValid())
                sdiConvert::Convert::PushToConversionLog(std::make_pair(tempHEdit, sourcehandleList));
        }
    }
}


void UpdateIMPCard(EntityEdit & impEdit, int lsdVAD, const sdiString &displayName, const EntityType& entityType, unsigned int entityId,
    const EntityType& lcidType, unsigned int lcidId, const EntityType& skewType, unsigned int skewId, double lsdSF, double lsdDEATH,
    double lsdBIRTH, const sdiString& lsdDIR)
{
    impEdit.SetValue(sdiIdentifier("displayname"), sdiValue(displayName));
    impEdit.SetValue(sdiIdentifier((lsdVAD == 2) ? "Gnod_id" : "grnod_ID"), sdiValue(sdiValueEntity(entityType, entityId)));
    impEdit.SetValue(sdiIdentifier((lsdVAD == 2) ? "Ifunct" : "funct_IDT"), sdiValue(sdiValueEntity(lcidType, lcidId)));
    impEdit.SetValue(sdiIdentifier((lsdVAD == 1) ? "Fscale_Y" : "Scale_y"), sdiValue(lsdSF));
    impEdit.SetValue(sdiIdentifier("rad_system_input_type"), sdiValue(1));
    if (skewId > 0)
        impEdit.SetValue(sdiIdentifier("inputsystem"), sdiValue(sdiValueEntity(skewType, skewId)));
    impEdit.SetValue(sdiIdentifier("Tstop"), sdiValue(lsdDEATH));
    impEdit.SetValue(sdiIdentifier("Tstart"), sdiValue(lsdBIRTH));
    impEdit.SetValue(sdiIdentifier((lsdVAD == 2) ? "DIR" : "Dir"), sdiValue(lsdDIR));
}
