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

#include <dyna2rad/convertloads.h>
#include <dyna2rad/dyna2rad.h>

using namespace sdi;
using namespace std;

void sdiD2R::ConvertLoad::ConvertAllLoads()
{
    ConvertEntities();
}

void sdiD2R::ConvertLoad::ConvertEntities()
{
    ConvertLoadNode();

    ConvertLoadBody();

    ConvertLoadShell();

    ConvertLoadGravity();

    ConvertLoadRigidBody();

    ConvertLoadSegment();
}

void sdiD2R::ConvertLoad::ConvertLoadNode()
{
    EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");
    EntityType radSkewType = p_radiossModel->GetEntityType("/SKEW");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    SelectionRead selLoadNode(p_lsdynaModel, "*LOAD_NODE");
    while (selLoadNode.Next())
    {
        sdiConvert::ContainIntVsStr mapDofVsDir({ {1,"X"},{2, "Y"},{3, "Z"},{4, "Z"},{5, "XX"},{6, "YY"},{7, "ZZ"},{8, "ZZ"} });
        sdiString keyWord = selLoadNode->GetKeyword();
        sdiString attrNameEntityId("NSID");
        if (keyWord.find("POINT") != keyWord.npos)
            attrNameEntityId = "NODE";

        double lsdSF = 0;
        sdiValue tempVal(lsdSF);
        selLoadNode->GetValue(sdiIdentifier("SF"), tempVal);
        tempVal.GetValue(lsdSF);

        int lsdDOF = 0;
        tempVal = sdiValue(lsdDOF);
        selLoadNode->GetValue(sdiIdentifier("DOF"), tempVal);
        tempVal.GetValue(lsdDOF);

        sdiValueEntity lsdEntity;
        sdiValueEntity lsdLCID;
        sdiValueEntity lsdCID;
        sdiValueEntity lsdM1;
        sdiValueEntity lsdM2;
        sdiValueEntity lsdM3;
        vector<reference_wrapper<sdiValueEntity>> lsdEntityAttribVals({ lsdEntity, lsdLCID, lsdCID, lsdM1, lsdM2, lsdM3 });
        vector <sdiString> lsdEntityAttribNames({ attrNameEntityId, "LCID", "CID", "M1", "M2", "M3" });
        p_ConvertUtils.GetAttribValues(*selLoadNode, lsdEntityAttribNames, lsdEntityAttribVals);

        unsigned int lcid = lsdLCID.GetId();
        unsigned int cid = lsdCID.GetId();
        unsigned int node1Id = lsdM1.GetId();
        unsigned int node2Id = lsdM2.GetId();
        unsigned int node3Id = lsdM3.GetId();
        unsigned int entityId = lsdEntity.GetId();
        unsigned int loadNodeId = selLoadNode->GetId();

        sdiUIntList loadNodes;
        if (attrNameEntityId == "NSID")
            p_ConvertUtils.GetNodeIdsFromNodeSet(entityId, loadNodes);
        if (lsdDOF == 4 && node1Id > 0 && node2Id > 0 && node3Id == 0)
        {
            mapDofVsDir[4] = "X";
            if (!loadNodes.empty())
                node3Id = loadNodes.front();
        }
        else if (lsdDOF == 8 && node1Id > 0 && node2Id > 0 && node3Id == 0)
        {
            mapDofVsDir[8] = "XX";
            if (!loadNodes.empty())
                node3Id = loadNodes.front();
        }
        HandleEdit skewHEdit;
        if (node1Id > 0 && node2Id > 0 && node3Id > 0 && cid == 0)
        {
            p_radiossModel->CreateEntity(skewHEdit, "/SKEW/MOV", "SKEW_MOV_"+ keyWord +"_" + to_string(loadNodeId), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
            EntityEdit skewEntityEdit(p_radiossModel, skewHEdit);
            skewEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(sdiString("SKEW_MOV_" + keyWord + "_" + to_string(loadNodeId))));
            skewEntityEdit.SetValue(sdiIdentifier("N1"), sdiValue(sdiValueEntity(sdiValueEntityType(1), node1Id)));
            skewEntityEdit.SetValue(sdiIdentifier("N2"), sdiValue(sdiValueEntity(sdiValueEntityType(1), node2Id)));
            skewEntityEdit.SetValue(sdiIdentifier("N3"), sdiValue(sdiValueEntity(sdiValueEntityType(1), node3Id)));
        }
        HandleEdit cloadHEdit;
        p_radiossModel->CreateEntity(cloadHEdit, "/CLOAD", selLoadNode->GetName(), loadNodeId);
        EntityEdit cloadEdit(p_radiossModel, cloadHEdit);

        cloadEdit.SetValue(sdiIdentifier("displayname"), sdiValue(selLoadNode->GetName()));
        cloadEdit.SetValue(sdiIdentifier("Dir"), sdiValue(mapDofVsDir[lsdDOF]));
        cloadEdit.SetValue(sdiIdentifier("funct_IDT"), sdiValue(sdiValueEntity(radFunctType,lcid)));
        cloadEdit.SetValue(sdiIdentifier("Ascalex"), sdiValue(0.0));
        cloadEdit.SetValue(sdiIdentifier("Fscaley"), sdiValue(lsdSF));
        cloadEdit.SetValue(sdiIdentifier("skew_ID"), sdiValue(sdiValueEntity(radSkewType, cid)));
        if (skewHEdit.IsValid())
            cloadEdit.SetEntityHandle(sdiIdentifier("skew_ID"), skewHEdit);
        if (attrNameEntityId == "NSID")
        {
            HandleRead nSetHread;
            selLoadNode->GetEntityHandle(sdiIdentifier(attrNameEntityId), nSetHread);
            if (nSetHread.IsValid())
            {
                EntityRead setRead(p_lsdynaModel, nSetHread);
                cloadEdit.SetValue(sdiIdentifier("grnod_ID"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(setRead.GetId(), "*SET_NODE" ))));
            }
        }
        else
        {
            HandleEdit setHEdit;
            p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", "SET_GEN_LOAD_NODE_" + to_string(loadNodeId));
            EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

            SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, sdiUIntList(1, entityId))));
            cloadEdit.SetEntityHandle(sdiIdentifier("grnod_ID"), setHEdit);
        }

        if (cloadHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceLoad = { {selLoadNode->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(cloadHEdit, sourceLoad));
        }
    }
}

void sdiD2R::ConvertLoad::ConvertLoadBody()
{
    SelectionRead selLoadBody(p_lsdynaModel, "*LOAD_BODY");
    HandleEdit allNodeSetHEdit;
    EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");
    EntityType radSkewType = p_radiossModel->GetEntityType("/SKEW");

    int grnodid = 0;
    sdiValue tempValue;
    while (selLoadBody.Next())
    {
        sdiString keyWord = selLoadBody->GetKeyword();
        sdiString lsdoption = p_ConvertUtils.SplitString(keyWord, "_").back();
        if (lsdoption == "PARTS")
        {
            // set of parts to be assigned into /GRAV or /LOAD/CENTRI
            sdiValueEntity grnodSet;
            sdiValue tempVal(grnodSet);
            selLoadBody->GetValue(sdiIdentifier("PSID"), tempVal);
            tempVal.GetValue(grnodSet);
            grnodid = grnodSet.GetId();
         }
    }

    selLoadBody.Restart();

    while (selLoadBody.Next())
    {
        sdiString keyWord = selLoadBody->GetKeyword();
        sdiString dynaDir = p_ConvertUtils.SplitString(keyWord, "_").back();
        sdiString radDir = "X"; // default value
        if (dynaDir == "X" || dynaDir == "Y" || dynaDir == "Z")
        {
            // /GRAV

            sdiString attrLcidName;
            sdiString attrSFName;
            if (dynaDir == "X")
            {
                attrLcidName = "LSD_LCID";
                attrSFName = "LSD_SFX";
                radDir = "X";
            }
            else if (dynaDir == "Y")
            {
                attrLcidName = "LSD_LCID3";
                attrSFName = "LSD_SFY";
                radDir = "Y";
            }
            else if (dynaDir == "Z")
            {
                attrLcidName = "LSD_LCID5";
                attrSFName = "LSD_SFZ";
                radDir = "Z";
            }
            sdiValueEntity lcidEntity;
            sdiValue tempVal(lcidEntity);
            selLoadBody->GetValue(sdiIdentifier(attrLcidName), tempVal);
            tempVal.GetValue(lcidEntity);
            unsigned int lcid = lcidEntity.GetId();

            double lsdSF = 0;
            tempVal = sdiValue(lsdSF);
            selLoadBody->GetValue(sdiIdentifier(attrSFName), tempVal);
            tempVal.GetValue(lsdSF);
            if (lsdSF == 0.0) lsdSF = 1.0; // by default

            sdiValueEntity CIDEntity = GetValue<sdiValueEntity>(*selLoadBody, "CID");
            unsigned int CID = CIDEntity.GetId();
            
            if(grnodid == 0)
            {
                if (!allNodeSetHEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(allNodeSetHEdit, "/SET/GENERAL", "ALL_NODE_SET_GENERAL_LOAD_BODY" + to_string(selLoadBody->GetId()));
                    EntityEdit SetEntityEdit(p_radiossModel, allNodeSetHEdit);

                    SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                    SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));
                }
            }

            HandleEdit gravHEdit;
            p_radiossModel->CreateEntity(gravHEdit, "/GRAV", selLoadBody->GetName(), selLoadBody->GetId());
            EntityEdit gravEdit(p_radiossModel, gravHEdit);
            gravEdit.SetValue(sdiIdentifier("displayname"), sdiValue(selLoadBody->GetName()));
            gravEdit.SetValue(sdiIdentifier("Ascale_x"), sdiValue(0.0));
            gravEdit.SetValue(sdiIdentifier("Fscale_Y"), sdiValue(-lsdSF));
            gravEdit.SetValue(sdiIdentifier("DIR"), sdiValue(radDir));
            //gravEdit.SetEntityHandle(sdiIdentifier("grnod_ID"), allNodeSetHEdit);
            if (grnodid == 0) gravEdit.SetEntityHandle(sdiIdentifier("grnod_ID"), allNodeSetHEdit);
            else if (grnodid > 0) gravEdit.SetValue(sdiIdentifier("grnod_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"), grnodid)));

            if (lcid)
                gravEdit.SetValue(sdiIdentifier("funct_IDT"), sdiValue(sdiValueEntity(radFunctType, lcid)));
            
            if (CID > 0)
                gravEdit.SetValue(sdiIdentifier("skew_ID"), sdiValue(sdiValueEntity(radSkewType, CID)));

            if (gravHEdit.IsValid())
            {
                sdiConvert::SDIHandlReadList sourceLoad = { {selLoadBody->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(gravHEdit, sourceLoad));
            }
        }
        else if (dynaDir == "RX" || dynaDir == "RY" || dynaDir == "RZ")
        {
            // /LOAD/CENTRI - centrifugal loading

            sdiString attrLcidName;
            sdiString attrSFName;
            if (dynaDir == "RX")
            {
                attrLcidName = "LSD_LCID7";
                attrSFName = "LSD_SFRX";
                radDir = "X";
            }
            else if (dynaDir == "RY")
            {
                attrLcidName = "LSD_LCID9";
                attrSFName = "LSD_SFRY";
                radDir = "Y";
            }
            else if (dynaDir == "RZ")
            {
                attrLcidName = "LSD_LCID11";
                attrSFName = "LSD_SFRZ";
                radDir = "Z";
            }

            sdiValueEntity lcidEntity = GetValue<sdiValueEntity>(*selLoadBody, attrLcidName);
            unsigned int lcid = lcidEntity.GetId();

            double lsdSF = GetValue<double>(*selLoadBody, attrSFName);
            if (lsdSF == 0.0) lsdSF = 1.0; // by default

            if(grnodid == 0)
            {
                if (!allNodeSetHEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(allNodeSetHEdit, "/SET/GENERAL", "ALL_NODE_SET_GENERAL_LOAD_BODY" + to_string(selLoadBody->GetId()));
                    EntityEdit SetEntityEdit(p_radiossModel, allNodeSetHEdit);

                    SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                    SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));
                }
            }

            HandleEdit centriHEdit;
            p_radiossModel->CreateEntity(centriHEdit, "/LOAD/CENTRI", selLoadBody->GetName(), selLoadBody->GetId());
            EntityEdit centriEdit(p_radiossModel, centriHEdit);
            centriEdit.SetValue(sdiIdentifier("displayname"), sdiValue(selLoadBody->GetName()));
            centriEdit.SetValue(sdiIdentifier("Ascalex"), sdiValue(0.0));
            centriEdit.SetValue(sdiIdentifier("Fscaley"), sdiValue(-lsdSF));
            centriEdit.SetValue(sdiIdentifier("Dir"), sdiValue(radDir));
            //centriEdit.SetEntityHandle(sdiIdentifier("grnod_ID"), allNodeSetHEdit);
            if (grnodid == 0) centriEdit.SetEntityHandle(sdiIdentifier("grnod_ID"), allNodeSetHEdit);
            else if (grnodid > 0) centriEdit.SetValue(sdiIdentifier("grnod_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"), grnodid)));
            if (lcid)
                centriEdit.SetValue(sdiIdentifier("funct_IDT"), sdiValue(sdiValueEntity(radFunctType, lcid)));

            // create new SKEW or FRAME, and reference it
            sdiValueEntity CIDEntity = GetValue<sdiValueEntity>(*selLoadBody, "CID");
            unsigned int CID = CIDEntity.GetId();

            if (CID > 0)
            {
                EntityType lsdCIDType = p_lsdynaModel->GetEntityType("*DEFINE_COORDINATE");
                HandleRead dynSkewHread;
                p_lsdynaModel->FindById(lsdCIDType, CID, dynSkewHread);
                if (dynSkewHread.IsValid())
                {
                    EntityRead dynSkewread(p_lsdynaModel, dynSkewHread);
                    sdiString skewCard = dynSkewread.GetKeyword();
                    int lsdFlag = GetValue<int>(dynSkewread, "FLAG");

                    if(skewCard.find("SYSTEM") != string::npos)
                    {
                        // create fix frame  -->  from defined radioss fix SKEW
                        EntityType radskewType = p_radiossModel->GetEntityType("/SKEW/FIX");
                        HandleRead radskewHread;
                        p_radiossModel->FindById(radskewType, CID, radskewHread);
                        if(radskewHread.IsValid())
                        {
                            EntityRead radskewRead(p_radiossModel, radskewHread);
                            // new fix frame
                            HandleEdit skewHandle;
                            p_radiossModel->CreateEntity(skewHandle, "/FRAME/FIX", "FRAME_FIX_LOAD_CENTRI_" + to_string(selLoadBody->GetId()),
                            p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*DEFINE_COORDINATE")));
                            if (skewHandle.IsValid())
                            {
                                EntityEdit skewEntityEdit(p_radiossModel, skewHandle);
                                skewEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue("FRAME_FIX_LOAD_CENTRI_" + to_string(selLoadBody->GetId())));

                                sdiTriple origVect(0.0, 0.0, 0.0);
                                sdiTriple Vect1(0.0, 0.0, 0.0);
                                sdiTriple Vect2(0.0, 0.0, 0.0);
                                sdiStringList attribOrigin({ "Ox", "Oy", "Oz" });
                                sdiStringList attribVect1({ "X1", "Y1", "Z1" });
                                sdiStringList attribVect2({ "X2", "Y2", "Z2" });

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
                                }

                                for (size_t i = 0; i < 3; ++i)
                                {
                                    skewEntityEdit.SetValue(sdiIdentifier(attribOrigin[i]), sdiValue(origVect[i]));
                                    skewEntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(Vect1[i]));
                                    skewEntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(Vect2[i]));
                                }
                                centriEdit.SetEntityHandle(sdiIdentifier("frame_ID"), skewHandle);
                            }
                        }
                    }
                    else if(skewCard.find("NODE") != string::npos)
                    {
                        if (lsdFlag == 1)
                        {
                            // create mov frame -->  from defined radioss mov 
                            EntityType radskewType = p_radiossModel->GetEntityType("/SKEW/MOV");
                            HandleRead radskewHread;
                            p_radiossModel->FindById(radskewType, CID, radskewHread);
                            if(radskewHread.IsValid())
                            {
                                EntityRead radskewRead(p_radiossModel, radskewHread);
                                // new mov frame
                                HandleEdit skewHandle;
                                p_radiossModel->CreateEntity(skewHandle, "/FRAME/MOV", "FRAME_MOV_LOAD_CENTRI_" + to_string(selLoadBody->GetId()),
                                p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*DEFINE_COORDINATE")));
                                if (skewHandle.IsValid())
                                {
                                    EntityEdit skewEntityEdit(p_radiossModel, skewHandle);
                                    skewEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue("FRAME_MOV_LOAD_CENTRI_" + to_string(selLoadBody->GetId())));
                                
                                    HandleRead handleN1;
                                    HandleRead handleN2;
                                    HandleRead handleN3;
                                    vector<sdiString> attribName({"N1", "N2", "N3"});
                                    vector<reference_wrapper<HandleRead>> attribVals({ handleN1, handleN2, handleN3 });
                                    p_ConvertUtils.GetEntityHandles(radskewRead, attribName, attribVals);
                                    sdiString lsdDir = GetValue<sdiString>(radskewRead, "DIR");
                                    for (size_t i = 0; i < 3; ++i)
                                    {
                                        skewEntityEdit.SetValue(sdiIdentifier(attribName[i]),sdiValue(sdiValueEntity(sdiValueEntityType(p_radiossModel->GetEntityType("/NODE")), 
                                        attribVals[i].get().GetId(p_radiossModel))));
                                    }
                                    skewEntityEdit.SetValue(sdiIdentifier("DIR"), sdiValue(lsdDir));
                                    centriEdit.SetEntityHandle(sdiIdentifier("frame_ID"), skewHandle);
                                }
                            }
                        }
                        else
                        {
                            // create fix frame -->  from defined radioss fix SKEW
                            EntityType radskewType = p_radiossModel->GetEntityType("/SKEW/FIX");
                            HandleRead radskewHread;
                            p_radiossModel->FindById(radskewType, CID, radskewHread);
                            if(radskewHread.IsValid())
                            {
                                EntityRead radskewRead(p_radiossModel, radskewHread);
                                // new fix frame
                                HandleEdit skewHandle;
                                p_radiossModel->CreateEntity(skewHandle, "/FRAME/FIX", "FRAME_FIX_LOAD_CENTRI_" + to_string(selLoadBody->GetId()),
                                p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*DEFINE_COORDINATE")));
                                if (skewHandle.IsValid())
                                {
                                    EntityEdit skewEntityEdit(p_radiossModel, skewHandle);
                                    skewEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue("FRAME_FIX_LOAD_CENTRI_" + to_string(selLoadBody->GetId())));

                                    sdiTriple origVect(0.0, 0.0, 0.0);
                                    sdiTriple Vect1(0.0, 0.0, 0.0);
                                    sdiTriple Vect2(0.0, 0.0, 0.0);
                                    sdiStringList attribOrigin({ "Ox", "Oy", "Oz" });
                                    sdiStringList attribVect1({ "X1", "Y1", "Z1" });
                                    sdiStringList attribVect2({ "X2", "Y2", "Z2" });

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
                                    }

                                    for (size_t i = 0; i < 3; ++i)
                                    {
                                        skewEntityEdit.SetValue(sdiIdentifier(attribOrigin[i]), sdiValue(origVect[i]));
                                        skewEntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(Vect1[i]));
                                        skewEntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(Vect2[i]));
                                    }
                                    centriEdit.SetEntityHandle(sdiIdentifier("frame_ID"), skewHandle);
                                }
                            }
                        }
                    }
                }
            }
            else
            {
                // create fix frame -->  XC,YC,ZC
                HandleEdit skewHandle;
                p_radiossModel->CreateEntity(skewHandle, "/FRAME/FIX", "FRAME_FIX_LOAD_CENTRI_" + to_string(selLoadBody->GetId()),
                p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*DEFINE_COORDINATE")));
                if (skewHandle.IsValid())
                {
                    EntityEdit skewEntityEdit(p_radiossModel, skewHandle);
                    skewEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue("FRAME_FIX_LOAD_CENTRI_" + to_string(selLoadBody->GetId())));

                    double lsdXC = GetValue<double>(*selLoadBody, "XC");
                    double lsdYC = GetValue<double>(*selLoadBody, "YC");
                    double lsdZC = GetValue<double>(*selLoadBody, "ZC");

                    sdiTriple origVect(lsdXC, lsdYC, lsdZC);
                    sdiTriple Vect1(0.0, 1.0, 0.0);
                    sdiTriple Vect2(0.0, 0.0, 1.0);
                    sdiStringList attribOrigin({ "Ox", "Oy", "Oz" });
                    sdiStringList attribVect1({ "X1", "Y1", "Z1" });
                    sdiStringList attribVect2({ "X2", "Y2", "Z2" });

                    for (size_t i = 0; i < 3; ++i)
                    {
                        skewEntityEdit.SetValue(sdiIdentifier(attribOrigin[i]), sdiValue(origVect[i]));
                        skewEntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(Vect1[i]));
                        skewEntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(Vect2[i]));
                    }
                    centriEdit.SetEntityHandle(sdiIdentifier("frame_ID"), skewHandle);
                }
            }

            if (centriHEdit.IsValid())
            {
                sdiConvert::SDIHandlReadList sourceLoad = { {selLoadBody->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(centriHEdit, sourceLoad));
            }
        }
        else if (dynaDir == "VECTOR")
        {
            // _VECTOR ->  /GRAV - acceleration loading
            int VectorOpt = GetValue<int>(*selLoadBody, "VectorOpt");

            sdiString attrLcidName;
            sdiString attrSFName;
            if (VectorOpt == 1)
            {
                attrLcidName = "LSD_LCID13";
                attrSFName = "LSD_SFRV";
                radDir ="VECTOR";

                sdiValueEntity lcidEntity = GetValue<sdiValueEntity>(*selLoadBody, attrLcidName);
                unsigned int lcid = lcidEntity.GetId();

                double lsdSF = GetValue<double>(*selLoadBody, attrSFName);
                if (lsdSF == 0.0) lsdSF = 1.0; // by default

                if(grnodid == 0)
                {
                    if (!allNodeSetHEdit.IsValid())
                    {
                       p_radiossModel->CreateEntity(allNodeSetHEdit, "/SET/GENERAL", "ALL_NODE_SET_GENERAL_LOAD_BODY" + to_string(selLoadBody->GetId()));
                        EntityEdit SetEntityEdit(p_radiossModel, allNodeSetHEdit);

                        SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                        SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));
                    }
                }

                HandleEdit gravHEdit;
                p_radiossModel->CreateEntity(gravHEdit, "/GRAV", selLoadBody->GetName(), selLoadBody->GetId());
                EntityEdit gravEdit(p_radiossModel, gravHEdit);
                gravEdit.SetValue(sdiIdentifier("displayname"), sdiValue(selLoadBody->GetName()));
                gravEdit.SetValue(sdiIdentifier("Ascale_x"), sdiValue(0.0));
                gravEdit.SetValue(sdiIdentifier("Fscale_Y"), sdiValue(-lsdSF));
                sdiString radir = "X";
                gravEdit.SetValue(sdiIdentifier("DIR"), sdiValue(radir));
                //gravEdit.SetEntityHandle(sdiIdentifier("grnod_ID"), allNodeSetHEdit);
                if (grnodid == 0) gravEdit.SetEntityHandle(sdiIdentifier("grnod_ID"), allNodeSetHEdit);
                else if (grnodid > 0) gravEdit.SetValue(sdiIdentifier("grnod_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"), grnodid)));

                if (lcid)
                    gravEdit.SetValue(sdiIdentifier("funct_IDT"), sdiValue(sdiValueEntity(radFunctType, lcid)));

                // create fix skew
                //------------------------
                sdiValueEntity csidEntity = GetValue<sdiValueEntity>(*selLoadBody, "CID");
                unsigned int csId=csidEntity.GetId();
                
                double lsdVX = GetValue<double>(*selLoadBody, "V1");
                double lsdVY = GetValue<double>(*selLoadBody, "V2");
                double lsdVZ = GetValue<double>(*selLoadBody, "V3");

                double lsdXC = GetValue<double>(*selLoadBody, "XC");
                double lsdYC = GetValue<double>(*selLoadBody, "YC");
                double lsdZC = GetValue<double>(*selLoadBody, "ZC");

                sdiTriple skeworigin(lsdXC, lsdYC, lsdZC);
                sdiTriple skew1Vect(0.0, 0.0, 0.0);
                sdiTriple skew2Vect(0.0, 0.0, 0.0);
                sdiTriple skew3Vect(0.0, 0.0, 0.0);

                sdiValue tempValue;
                sdiTriple cidorigin(0.0, 0.0, 0.0); // origin of ICID
                sdiTriple cidVect1(0.0, 0.0, 0.0);  // (Reference Y axis) is the second direction of ICID
                sdiTriple cidVect2(0.0, 0.0, 0.0);  // (Reference Z axis) is the third direction of ICID
                sdiTriple cidVect3(0.0, 0.0, 0.0);  // (Reference X axis) is the first direction of ICID

                sdiStringList attribOrigin({ "Ox", "Oy", "Oz" });
                sdiStringList attribskew1Vect({ "X1", "Y1", "Z1" });
                sdiStringList attribskew2Vect({ "X2", "Y2", "Z2" });

                if(lsdVX == 0.0 && lsdVY == 0.0 && lsdVZ == 0.0)
                {
                    // Reference skew is global system
                    skew1Vect = sdiTriple(0.0, 1.0, 0.0);  // (Reference Y axis) is the second direction
                    skew2Vect = sdiTriple(0.0, 0.0, 1.0);  // (Reference Z axis) is the third direction
                    skew3Vect = sdiTriple(1.0, 0.0, 0.0);  // (Reference X axis) is the first direction
                }
                else if(csId == 0 && (lsdVX != 0.0 || lsdVY != 0.0 || lsdVZ != 0.0))
                {
                    sdiTriple inputDirVect(lsdVX, lsdVY, lsdVZ);
                    if ((inputDirVect * sdiTriple(0.0, 1.0, 0.0)).Len() != 0.0)
                        skew2Vect = inputDirVect * sdiTriple(0.0, 1.0, 0.0);
                    else
                        skew2Vect = inputDirVect * sdiTriple(0.0, 0.0, 1.0);

                    skew2Vect = skew2Vect.Normalize();
                    skew1Vect = skew2Vect * inputDirVect;
                    skew1Vect = skew1Vect.Normalize();
                }
                else
                {
                    // CID is defined
                    sdiTriple inputDirVect(lsdVX, lsdVY, lsdVZ);
                    EntityType radskewType = p_radiossModel->GetEntityType("/SKEW/FIX");
                    HandleRead radskewHread;
                    p_radiossModel->FindById(radskewType, csId, radskewHread);
                    if(radskewHread.IsValid())
                    {
                        EntityRead radskewRead(p_radiossModel, radskewHread);

                        for (size_t i = 0; i < 3; ++i)
                        {
                            tempValue = sdiValue(cidorigin[i]);
                            radskewRead.GetValue(sdiIdentifier(attribOrigin[i]), tempValue);
                            tempValue.GetValue(cidorigin[i]);

                            tempValue = sdiValue(cidVect1[i]);
                            radskewRead.GetValue(sdiIdentifier(attribskew1Vect[i]), tempValue);
                            tempValue.GetValue(cidVect1[i]);

                            tempValue = sdiValue(cidVect2[i]);
                            radskewRead.GetValue(sdiIdentifier(attribskew2Vect[i]), tempValue);
                            tempValue.GetValue(cidVect2[i]);
                        }
                        cidVect1 = cidVect1.Normalize();  // (Reference Y axis) is the second direction of ICID
                        cidVect2 = cidVect2.Normalize();  // (Reference Z axis) is the third direction of ICID
                        cidVect3 = cidVect1 * cidVect2;   // (Reference X axis) is the first direction of ICID
                        cidVect3 = cidVect3.Normalize();
                    }

                    // othogonal projection of the velocity vector (VX, VY, VZ) on the local fix frame
                    double newVx = cidVect3[0] * lsdVX + cidVect1[0] * lsdVY + cidVect2[0] * lsdVZ;
                    double newVy = cidVect3[1] * lsdVX + cidVect1[1] * lsdVY + cidVect2[1] * lsdVZ;
                    double newVz = cidVect3[2] * lsdVX + cidVect1[2] * lsdVY + cidVect2[2] * lsdVZ;
                    sdiTriple newinputDirVect(newVx, newVy, newVz);

                    if ((newinputDirVect * cidVect3).Len() != 0.0)
                        skew2Vect = newinputDirVect * cidVect3;
                    else
                        skew2Vect = newinputDirVect * cidVect2;

                    skew2Vect = skew2Vect.Normalize();
                    skew1Vect = skew2Vect * newinputDirVect;
                    skew1Vect = skew1Vect.Normalize();
                }

                //-----------------------
                // Create local FRAME/FIX
                //-----------------------

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
                        radSkewEdit.SetValue(sdiIdentifier(attribOrigin[i]), sdiValue(skeworigin[i]));
                        radSkewEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(skew1Vect[i]));
                        radSkewEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(skew2Vect[i]));
                    }
                    radSkewEdit.SetValue(sdiIdentifier("titlestr"), sdiValue("SKEW_FIX_GRAV_" + to_string(selLoadBody->GetId())));
                    gravEdit.SetValue(sdiIdentifier("rad_system_input_type"), sdiValue(1));
                    gravEdit.SetEntityHandle(sdiIdentifier("inputsystem"), radSkewHEdit);
                }

                if (gravHEdit.IsValid())
                {
                    sdiConvert::SDIHandlReadList sourceLoad = { {selLoadBody->GetHandle()} };
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(gravHEdit, sourceLoad));
                }
            }
        }
    }
}

void sdiD2R::ConvertLoad::ConvertLoadShell()
{
    SelectionRead selLoadShell(p_lsdynaModel, "*LOAD_SHELL");
    HandleEdit partSetHedit;
    HandleEdit sensorTimeHedit;
    EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    EntityType radShellType = p_radiossModel->GetEntityType("/SHELL");
    EntityType radSh3nType = p_radiossModel->GetEntityType("/SH3N");
    while (selLoadShell.Next())
    {
        sdiString keyWord = selLoadShell->GetKeyword();

        sdiValueEntity lcidEntity;
        sdiValue tempVal(lcidEntity);
        selLoadShell->GetValue(sdiIdentifier("curveid"), tempVal);
        tempVal.GetValue(lcidEntity);
        unsigned int lcid = lcidEntity.GetId();

        double lsdSF = 0;
        tempVal = sdiValue(lsdSF);
        selLoadShell->GetValue(sdiIdentifier("SF"), tempVal);
        tempVal.GetValue(lsdSF);

        double lsdAT = 0;
        tempVal = sdiValue(lsdAT);
        selLoadShell->GetValue(sdiIdentifier("AT"), tempVal);
        tempVal.GetValue(lsdAT);


        HandleEdit ploadHEdit;
        p_radiossModel->CreateEntity(ploadHEdit, "/PLOAD", selLoadShell->GetName(), selLoadShell->GetId());
        EntityEdit ploadEdit(p_radiossModel, ploadHEdit);
        ploadEdit.SetValue(sdiIdentifier("displayname"), sdiValue(selLoadShell->GetName()));
        ploadEdit.SetValue(sdiIdentifier("Ascale_x"), sdiValue(0.0));
        ploadEdit.SetValue(sdiIdentifier("Fscale_Y"), sdiValue(-lsdSF));

        if (keyWord.find("ELEMENT") != keyWord.npos)
        {
            sdiValueEntityList eidEntities;
            sdiValue tempVal(eidEntities);
            selLoadShell->GetValue(sdiIdentifier("EID", 0), tempVal);
            tempVal.GetValue(eidEntities);

            sdiUIntList shellIds, sh3nIds;
            unsigned int eidEntitiesCount = eidEntities.GetIdListCount();
            shellIds.reserve(eidEntitiesCount);
            sh3nIds.reserve(eidEntitiesCount);

            for(unsigned int i=0; i < eidEntitiesCount; ++i)
            {
                unsigned int eid = 0;
                eidEntities.GetId(i, eid);

                bool isSh4n = false;

                HandleRead ElementHRead;
                if(p_radiossModel->FindById("/SHELL", eid, ElementHRead)) isSh4n = true;

                if(isSh4n) shellIds.push_back(eid);
                else       sh3nIds.push_back(eid);
            }

            HandleEdit shellSetHedit;
            int clausesmax = 0;
            if(shellIds.size() > 0 && sh3nIds.size() == 0)
            {
                p_radiossModel->CreateEntity(shellSetHedit, "/SET/GENERAL", "SH4N_SET_GENERAL_LOAD_SHELL" + to_string(selLoadShell->GetId()));
                clausesmax = 1;
            }
            else if(shellIds.size() == 0 && sh3nIds.size() > 0)
            {
                p_radiossModel->CreateEntity(shellSetHedit, "/SET/GENERAL", "SH3N_SET_GENERAL_LOAD_SHELL" + to_string(selLoadShell->GetId()));
                clausesmax = 1;
            }
            else if(shellIds.size() > 0 && sh3nIds.size() > 0)
            {
                p_radiossModel->CreateEntity(shellSetHedit, "/SET/GENERAL", "SET_GENERAL_LOAD_SHELL" + to_string(selLoadShell->GetId()));
                clausesmax = 2;
            }

            if (shellSetHedit.IsValid())
            {
                EntityEdit SetEntityEdit(p_radiossModel, shellSetHedit);

                SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(clausesmax));

                unsigned int sh3nClause = 0;
                if(shellIds.size() > 0)
                {
                    SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("SHELL")));
                    SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue((int) shellIds.size()));
                    SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radShellType, shellIds)));
                    sh3nClause = 1;
                }
                if(sh3nIds.size() > 0)
                {
                    SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, sh3nClause), sdiValue(sdiString("SH3N")));
                    SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, sh3nClause), sdiValue((int) sh3nIds.size()));
                    SetEntityEdit.SetValue(sdiIdentifier("ids", 0, sh3nClause), sdiValue(sdiValueEntityList(radSh3nType, sh3nIds)));
                }

                ploadEdit.SetEntityHandle(sdiIdentifier("surf_ID"), shellSetHedit);
            }
        }
        else if (keyWord.find("SET") != keyWord.npos)
        {
            sdiValueEntity sidEntity;
            sdiValue tempVal(sidEntity);
            selLoadShell->GetValue(sdiIdentifier("entityid"), tempVal);
            tempVal.GetValue(sidEntity);
            unsigned int esid = sidEntity.GetId();

            ploadEdit.SetValue(sdiIdentifier("surf_ID"),sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(esid,"*SET_SHELL"))));    
        }

        if (lsdAT > 0)
        {
            p_radiossModel->CreateEntity(sensorTimeHedit, "/SENSOR/TIME", selLoadShell->GetName());
            EntityEdit sensorTimeEntityEdit(p_radiossModel, sensorTimeHedit);
            sensorTimeEntityEdit.SetValue(sdiIdentifier("Tdelay"), sdiValue(lsdAT));
            ploadEdit.SetEntityHandle(sdiIdentifier("sensor_ID"), sensorTimeHedit);

            sdiConvert::SDIHandlReadList sourceLoad = { {selLoadShell->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(sensorTimeHedit, sourceLoad));
        }

        if (lcid)
            ploadEdit.SetValue(sdiIdentifier("functIDT"), sdiValue(sdiValueEntity(radFunctType, lcid)));

        if (ploadHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceLoad = { {selLoadShell->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(ploadHEdit, sourceLoad));
        }
    }
}

void sdiD2R::ConvertLoad::ConvertLoadGravity()
{
    SelectionRead selLoadGravity(p_lsdynaModel, "*LOAD_GRAVITY");
    HandleEdit partSetHedit;
    HandleEdit nodeSetHedit;
    EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    EntityType radNodeType = p_radiossModel->GetEntityType("/NODE");
    while (selLoadGravity.Next())
    {
        sdiString keyWord = selLoadGravity->GetKeyword();

        sdiValueEntity lcidEntity;
        sdiValue tempVal(lcidEntity);
        selLoadGravity->GetValue(sdiIdentifier("LC"), tempVal);
        tempVal.GetValue(lcidEntity);
        unsigned int lcid = lcidEntity.GetId();

        double lsdACCEL = 0;
        tempVal = sdiValue(lsdACCEL);
        selLoadGravity->GetValue(sdiIdentifier("ACCEL"), tempVal);
        tempVal.GetValue(lsdACCEL);
        if(lsdACCEL == 0.0) lsdACCEL = 1.0;

        int lsdDOF = 0;
        tempVal = sdiValue(lsdDOF);
        selLoadGravity->GetValue(sdiIdentifier("DOF"), tempVal);
        tempVal.GetValue(lsdDOF);

        HandleEdit gravHEdit;
        p_radiossModel->CreateEntity(gravHEdit, "/GRAV", selLoadGravity->GetName(), selLoadGravity->GetId());
        EntityEdit gravEdit(p_radiossModel, gravHEdit);
        gravEdit.SetValue(sdiIdentifier("displayname"), sdiValue(selLoadGravity->GetName()));
        gravEdit.SetValue(sdiIdentifier("Ascale_x"), sdiValue(0.0));
        gravEdit.SetValue(sdiIdentifier("Fscale_Y"), sdiValue(-lsdACCEL));


        if (lsdDOF == 1) gravEdit.SetValue(sdiIdentifier("DIR"), sdiValue(sdiString("X")));
        if (lsdDOF == 2) gravEdit.SetValue(sdiIdentifier("DIR"), sdiValue(sdiString("Y")));
        if (lsdDOF == 3) gravEdit.SetValue(sdiIdentifier("DIR"), sdiValue(sdiString("Z")));

        if (keyWord.find("PART_SET") != keyWord.npos)
        {
            sdiValueEntity psidEntity;
            sdiValue tempVal(psidEntity);
            selLoadGravity->GetValue(sdiIdentifier("PSID"), tempVal);
            tempVal.GetValue(psidEntity);
            unsigned int psid = psidEntity.GetId();

            gravEdit.SetValue(sdiIdentifier("grnod_ID"),sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(psid,"*SET_PART"))));    
        }

        else if (keyWord.find("PART") != keyWord.npos)
        {
            sdiValueEntity pidEntity;
            sdiValue tempVal(pidEntity);
            selLoadGravity->GetValue(sdiIdentifier("PID"), tempVal);
            tempVal.GetValue(pidEntity);
            unsigned int pid = pidEntity.GetId();
            unsigned int nodeId = 0;
 

            if (DynaToRad::storeRbodyPIDVsMasterNode.find(pid) != DynaToRad::storeRbodyPIDVsMasterNode.end())
            {
                nodeId = DynaToRad::storeRbodyPIDVsMasterNode[pid];
            }

            if(nodeId != 0)
            {
                p_radiossModel->CreateEntity(nodeSetHedit, "/SET/GENERAL", "NODE_SET_GENERAL_LOAD_GRAVITY" + to_string(selLoadGravity->GetId()));
                EntityEdit SetEntityEdit(p_radiossModel, nodeSetHedit);

                SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
                SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radNodeType,nodeId)));
                gravEdit.SetEntityHandle(sdiIdentifier("grnod_ID"), nodeSetHedit);
            }
            else
            {

                p_radiossModel->CreateEntity(partSetHedit, "/SET/GENERAL", "PART_SET_GENERAL_LOAD_GRAVITY" + to_string(selLoadGravity->GetId()));
                EntityEdit SetEntityEdit(p_radiossModel, partSetHedit);

                SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
                SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(pidEntity));
                gravEdit.SetEntityHandle(sdiIdentifier("grnod_ID"), partSetHedit);
            }
        }

        if (lcid != 0)
            gravEdit.SetValue(sdiIdentifier("funct_IDT"), sdiValue(sdiValueEntity(radFunctType, lcid)));

        if (gravHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceLoad = { {selLoadGravity->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(gravHEdit, sourceLoad));
        }
        if (nodeSetHedit.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceLoad = { {selLoadGravity->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(nodeSetHedit, sourceLoad));
        }
    }
}
void sdiD2R::ConvertLoad::ConvertLoadRigidBody()
{
    EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");
    EntityType radSkewType = p_radiossModel->GetEntityType("/SKEW");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    SelectionRead selLoadRigidBody(p_lsdynaModel, "*LOAD_RIGID_BODY");
    while (selLoadRigidBody.Next())
    {
        sdiConvert::ContainIntVsStr mapDofVsDir({ {1,"X"},{2, "Y"},{3, "Z"},{4, "Z"},{5, "XX"},{6, "YY"},{7, "ZZ"},{8, "ZZ"} });
        sdiString keyWord = selLoadRigidBody->GetKeyword();
        sdiString attrNameEntityId("PID");

        double lsdSF = 0;
        sdiValue tempVal(lsdSF);
        selLoadRigidBody->GetValue(sdiIdentifier("SF"), tempVal);
        tempVal.GetValue(lsdSF);
        if (lsdSF == 0.0) lsdSF = 1.0; // by default

        int lsdDOF = 0;
        tempVal = sdiValue(lsdDOF);
        selLoadRigidBody->GetValue(sdiIdentifier("DOF"), tempVal);
        tempVal.GetValue(lsdDOF);

        sdiValueEntity lsdEntity;
        sdiValueEntity lsdLCID;
        sdiValueEntity lsdCID;
        sdiValueEntity lsdM1;
        sdiValueEntity lsdM2;
        sdiValueEntity lsdM3;
        vector<reference_wrapper<sdiValueEntity>> lsdEntityAttribVals({ lsdEntity, lsdLCID, lsdCID, lsdM1, lsdM2, lsdM3 });
        vector <sdiString> lsdEntityAttribNames({ attrNameEntityId, "LCID", "CID", "M1", "M2", "M3" });
        p_ConvertUtils.GetAttribValues(*selLoadRigidBody, lsdEntityAttribNames, lsdEntityAttribVals);

        unsigned int lcid = lsdLCID.GetId();
        unsigned int cid = lsdCID.GetId();
        unsigned int node1Id = lsdM1.GetId();
        unsigned int node2Id = lsdM2.GetId();
        unsigned int node3Id = lsdM3.GetId();
        unsigned int rbPartId = lsdEntity.GetId();
        unsigned int loadRigidBodyId = selLoadRigidBody->GetId();

        int lsdnegLCID = 0;
        tempVal = sdiValue(lsdnegLCID);
        selLoadRigidBody->GetValue(sdiIdentifier("negLCIDFlag"), tempVal);
        tempVal.GetValue(lsdnegLCID);

        if(lsdnegLCID > 0)
        {
            
            DynaToRad::ShowMessage(sdiMessageHandler::Level::Error, 33,
                selLoadRigidBody->GetKeyword().c_str(), selLoadRigidBody->GetId(), selLoadRigidBody->GetName().c_str());
        }

        HandleRead dynapartHread;
        p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), rbPartId, dynapartHread);

        if(dynapartHread.IsValid())
        {
            HandleRead dynamatHRead;
            dynapartHread.GetEntityHandle(p_lsdynaModel, sdiIdentifier("MID"), dynamatHRead);
            EntityRead dynamatRead(p_lsdynaModel, dynamatHRead);

            if (dynamatHRead.IsValid())
            {
                  EntityRead dynamatRead(p_lsdynaModel, dynamatHRead);
                  sdiString matCard = dynamatRead.GetKeyword();

                  if( matCard.compare(0, 13, "*MAT_RIGID"))
                  {
                      // "Load applied to non-rigid component"
                      DynaToRad::ShowMessage(sdiMessageHandler::Level::Error, 34,
                          selLoadRigidBody->GetKeyword().c_str(), selLoadRigidBody->GetId(), selLoadRigidBody->GetName().c_str());
                  }
            }
        }

        HandleEdit skewHEdit;
        if (node1Id > 0 && node2Id > 0 && node3Id > 0 && cid == 0)
        {
            p_radiossModel->CreateEntity(skewHEdit, "/SKEW/MOV", "SKEW_MOV_"+ keyWord +"_" + to_string(loadRigidBodyId), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
            EntityEdit skewEntityEdit(p_radiossModel, skewHEdit);
            skewEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(sdiString("SKEW_MOV_" + keyWord + "_" + to_string(loadRigidBodyId))));
            skewEntityEdit.SetValue(sdiIdentifier("N1"), sdiValue(sdiValueEntity(sdiValueEntityType(1), node1Id)));
            skewEntityEdit.SetValue(sdiIdentifier("N2"), sdiValue(sdiValueEntity(sdiValueEntityType(1), node2Id)));
            skewEntityEdit.SetValue(sdiIdentifier("N3"), sdiValue(sdiValueEntity(sdiValueEntityType(1), node3Id)));
        }
        HandleEdit cloadHEdit;
        p_radiossModel->CreateEntity(cloadHEdit, "/CLOAD", selLoadRigidBody->GetName(), loadRigidBodyId);
        EntityEdit cloadEdit(p_radiossModel, cloadHEdit);

        cloadEdit.SetValue(sdiIdentifier("displayname"), sdiValue(selLoadRigidBody->GetName()));
        cloadEdit.SetValue(sdiIdentifier("Dir"), sdiValue(mapDofVsDir[lsdDOF]));
        cloadEdit.SetValue(sdiIdentifier("funct_IDT"), sdiValue(sdiValueEntity(radFunctType,lcid)));
        cloadEdit.SetValue(sdiIdentifier("Ascalex"), sdiValue(0.0));
        cloadEdit.SetValue(sdiIdentifier("Fscaley"), sdiValue(lsdSF));
        cloadEdit.SetValue(sdiIdentifier("skew_ID"), sdiValue(sdiValueEntity(radSkewType, cid)));
        if (skewHEdit.IsValid())
            cloadEdit.SetEntityHandle(sdiIdentifier("skew_ID"), skewHEdit);

        // get Radioss Rigid Body main node
        //---
        unsigned int rbmainNodeId = DynaToRad:: storeRbodyPIDVsMasterNode[rbPartId];
        //---

        HandleEdit setHEdit;
        p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", "SET_GEN_LOAD_RIGID_BODY" + to_string(loadRigidBodyId));
        EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

        SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
        SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
        SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
        SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, sdiUIntList(1, rbmainNodeId))));
        cloadEdit.SetEntityHandle(sdiIdentifier("grnod_ID"), setHEdit);

        sdiConvert::SDIHandlReadList sourceLoad = { {selLoadRigidBody->GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(setHEdit, sourceLoad));

        if (cloadHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceLoad = { {selLoadRigidBody->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(cloadHEdit, sourceLoad));
        }
    }
}
void sdiD2R::ConvertLoad::ConvertLoadSegment()
{
    SelectionRead selLoadSegment(p_lsdynaModel, "*LOAD_SEGMENT");

    EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    EntityType radNodeType = p_radiossModel->GetEntityType("/NODE");
    EntityType radSensorType = p_radiossModel->GetEntityType("/SENSOR");

    while (selLoadSegment.Next())
    {

        unsigned int loadSegmentId = selLoadSegment->GetId();
        HandleEdit ploadHEdit;
        p_radiossModel->CreateEntity(ploadHEdit, "/PLOAD", selLoadSegment->GetName(), loadSegmentId);
        EntityEdit ploadEdit(p_radiossModel, ploadHEdit);

        ploadEdit.SetValue(sdiIdentifier("displayname"), sdiValue(selLoadSegment->GetName()));

        double lsdSF = GetValue<double>(*selLoadSegment, "SF");
        if(lsdSF == 0.0) lsdSF = 1.0;
        ploadEdit.SetValue(sdiIdentifier("Fscale_y"), sdiValue(-lsdSF));

        int LSD_LCIDOpt = 0;
        sdiValue tempVal;
        selLoadSegment->GetValue(sdiIdentifier("LSD_LCIDOpt"), tempVal);
        tempVal.GetValue(LSD_LCIDOpt);

        int lcidFunc = 0;
        if(LSD_LCIDOpt == 0)
        {
            // FUNCTION_ID 
            tempVal = sdiValue(lcidFunc);
            selLoadSegment->GetValue(sdiIdentifier("LCID"), tempVal);
            tempVal.GetValue(lcidFunc);
        }
        else
        {
            // CURVE_ID 
            sdiValueEntity lcidEntity;
            tempVal = sdiValue(lcidEntity);
            selLoadSegment->GetValue(sdiIdentifier("curveid"), tempVal);
            tempVal.GetValue(lcidEntity);
            lcidFunc = lcidEntity.GetId();
        }
        if (lcidFunc > 0) 
            ploadEdit.SetValue(sdiIdentifier("functIDT"), sdiValue(sdiValueEntity(radFunctType, lcidFunc)));

        sdiString keyWordLog = selLoadSegment->GetKeyword();
        if (keyWordLog.find("SET") != keyWordLog.npos)
        {
            sdiValueEntity surfSet;
            selLoadSegment->GetValue(sdiIdentifier("SSID"), tempVal);
            tempVal.GetValue(surfSet);
            if (surfSet.GetId())
                ploadEdit.SetValue(sdiIdentifier("surf_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SURF"), surfSet.GetId())));
        }
        else
        {
            sdiValueEntity node1;
            sdiValueEntity node2;
            sdiValueEntity node3;
            sdiValueEntity node4;

            tempVal = sdiValue(node1);
            selLoadSegment->GetValue(sdiIdentifier("N1"), tempVal);
            tempVal.GetValue(node1);
            unsigned int node1Id = node1.GetId();

            tempVal = sdiValue(node2);
            selLoadSegment->GetValue(sdiIdentifier("N2"), tempVal);
            tempVal.GetValue(node2);
            unsigned int node2Id = node2.GetId();

            tempVal = sdiValue(node3);
            selLoadSegment->GetValue(sdiIdentifier("N3"), tempVal);
            tempVal.GetValue(node3);
            unsigned int node3Id = node3.GetId();

            tempVal = sdiValue(node4);
            selLoadSegment->GetValue(sdiIdentifier("N4"), tempVal);
            tempVal.GetValue(node4);
            unsigned int node4Id = node4.GetId();

            //create a new segment set
            HandleEdit segSetHedit;
            if(node1Id != 0 && node2Id != 0 && node3Id != 0)
            {
                p_radiossModel->CreateEntity(segSetHedit, "/SET/GENERAL", "SEG_SET_GENERAL_LOAD_SEGMENT" + to_string(selLoadSegment->GetId()));
                EntityEdit segSetedit(p_radiossModel, segSetHedit);

                segSetedit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                segSetedit.SetValue(sdiIdentifier("segmax", 0, 0), sdiValue(1));
                segSetedit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("SEG")));
                segSetedit.SetValue(sdiIdentifier("segid", 0, 0), sdiValue(1));
                segSetedit.SetValue(sdiIdentifier("ids1", 0, 0), sdiValue(sdiValueEntity(radNodeType, node1Id)));
                segSetedit.SetValue(sdiIdentifier("ids2", 0, 0), sdiValue(sdiValueEntity(radNodeType, node2Id)));
                segSetedit.SetValue(sdiIdentifier("ids3", 0, 0), sdiValue(sdiValueEntity(radNodeType, node3Id)));
                segSetedit.SetValue(sdiIdentifier("ids4", 0, 0), sdiValue(sdiValueEntity(radNodeType, node4Id)));

                //ploadEdit.SetEntityHandle(sdiIdentifier("surf_ID"), segSetHedit);
                ploadEdit.SetValue(sdiIdentifier("surf_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SURF"), segSetedit.GetId())));

                sdiConvert::SDIHandlReadList sourceLoad = { {selLoadSegment->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(segSetHedit, sourceLoad));
            }
        }


        double lsdAT = 0.0;
        tempVal = sdiValue(lsdAT);
        selLoadSegment->GetValue(sdiIdentifier("AT"), tempVal);
        tempVal.GetValue(lsdAT);

        if( lsdAT > 0.0)
        {
            //create SENSOR/TIME
            HandleEdit sensorTimeHedit;
            p_radiossModel->CreateEntity(sensorTimeHedit, "/SENSOR/TIME", selLoadSegment->GetName());
            EntityEdit sensorTimeEntityEdit(p_radiossModel, sensorTimeHedit);
            sensorTimeEntityEdit.SetValue(sdiIdentifier("Tdelay"), sdiValue(lsdAT));
            ploadHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("sensor_ID"), sensorTimeHedit);
            //ploadHEdit.SetValue(p_radiossModel, sdiIdentifier("sensor_ID"), sdiValue(sdiValueEntityList(radSensorType, sdiUIntList(1, sensorTimeHedit.GetId(p_radiossModel)))));

            sdiConvert::SDIHandlReadList sourceConVol = { {selLoadSegment->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(sensorTimeHedit, sourceConVol));
        }

        if (ploadHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceLoad = { {selLoadSegment->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(ploadHEdit, sourceLoad));
        }
    }
}
