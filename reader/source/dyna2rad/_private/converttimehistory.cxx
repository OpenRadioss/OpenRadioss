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

#include <dyna2rad/converttimehistory.h>
#include <dyna2rad/dyna2rad.h>
#include <typedef.h>
#include <dyna2rad/sdiUtils.h>

using namespace std;
using namespace sdi;

void sdiD2R::ConvertTimeHistory::ConvertTimeHistories()
{
    ConvertEntities();
}

void sdiD2R::ConvertTimeHistory::ConvertEntities()
{
    p_CreateDefaultTHPart();

    p_ConvertAllDBHistroy();
}

void sdiD2R::ConvertTimeHistory::p_CreateDefaultTHPart()
{
    SelectionRead selectPart(p_radiossModel, "/PART");

    int cptTh = 0;
    sdiUIntList partIdList;
    while (selectPart.Next())
    {
        sdiConvert::SDIHandlReadList dynaSourceHandles;
        sdiConvert::Convert::GetSourceHandles(p_radiossModel, selectPart->GetHandle(), dynaSourceHandles);
        if(dynaSourceHandles.size() > 0) cptTh = cptTh + 1;
    }

    partIdList.reserve(cptTh);
    selectPart.Restart();

    while (selectPart.Next())
    {
        sdiConvert::SDIHandlReadList dynaSourceHandles;
        sdiConvert::Convert::GetSourceHandles(p_radiossModel, selectPart->GetHandle(), dynaSourceHandles);
        if(dynaSourceHandles.size() > 0)
        {
            HandleRead radPropHandle;
            selectPart->GetEntityHandle(sdiIdentifier("prop_ID"), radPropHandle);
            if (radPropHandle.IsValid())
                partIdList.push_back(selectPart->GetId());
        }
    }
    sdiVectorUnique(partIdList);
    partIdList.shrink_to_fit();
    if (!partIdList.empty())
    {
        int idsMax = (int)partIdList.size();
        HandleEdit thHandleEdit;
        p_radiossModel->CreateEntity(thHandleEdit, "/TH/PART", "Default_TH_PART");
        EntityEdit thEdit(p_radiossModel, thHandleEdit);
        thEdit.SetValue(sdiIdentifier("idsmax"), sdiValue(idsMax));
        thEdit.SetValue(sdiIdentifier("Obj"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/PART"), partIdList)));
        thEdit.SetValue(sdiIdentifier("Number_Of_Variables"), sdiValue(1));
        thEdit.SetValue(sdiIdentifier("var"), sdiValue(sdiStringList({"DEF"})));

    }
}

void sdiD2R::ConvertTimeHistory::p_ConvertAllDBHistroy()
{
    SelectionRead selectTH(p_lsdynaModel, srcCard);
    while (selectTH.Next())
    {
        sdiString keyWord = selectTH->GetKeyword();
        sdiStringList keywordSplitTokens = p_ConvertUtils.SplitString(keyWord, "_");
        sdiString entityType = keywordSplitTokens[2];
        sdiValueEntityList entityList;
        sdiValueEntityList skewList;
        sdiStringList nameList;
        sdiValue tempVal(entityList);

        if (keyWord.find("_SET") != keyWord.npos)
        {
            int nSets = 0;
            sdiValue tempVal(nSets);
            sdiUIntList elemIdList;
            selectTH->GetValue(sdiIdentifier("idsmax"), tempVal);
            tempVal.GetValue(nSets);

            for (int i = 0; i < nSets; ++i)
            {
                sdiValueEntityList tempEntityList;
                HandleRead handleRSet;
                sdiUIntList tempelemIdsList;
                selectTH->GetEntityHandle(sdiIdentifier("ids",0, i,0), handleRSet);
                tempVal = sdiValue(tempEntityList);
                handleRSet.GetValue(p_lsdynaModel, sdiIdentifier("ids"), tempVal);
                tempVal.GetValue(tempEntityList);
                tempEntityList.GetIdList(tempelemIdsList);

                HandleEdit DynaSetHread;
                p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*SET"), handleRSet.GetId(p_lsdynaModel), DynaSetHread);
                EntityRead DynaSetEntread(p_lsdynaModel, DynaSetHread);
                sdiString setType = DynaSetEntread.GetKeyword();

                if (setType.find("_ADD") != setType.npos)
                {
                   DynaToRad::ShowMessage(sdiMessageHandler::Level::Warning, 38,selectTH->GetKeyword().c_str(), DynaSetEntread.GetId());
                   break;
                }

                int set_ID = 0;
                if (DynaSetHread.IsValid()) set_ID = DynaSetEntread.GetId();

                int numClauses = 0;
                sdiValue tempVal(numClauses);
                handleRSet.GetValue(p_lsdynaModel,sdiIdentifier("clausesmax", 0, i), tempVal);
                tempVal.GetValue(numClauses);

                int nbClausePart = 0;

                for (int j = 0; j < numClauses; ++j)
                {
                    int idsmax = 0;
                    sdiString idsType;
                    sdiString lsdKey = " ";
 
                    tempVal = sdiValue(idsType);
                    handleRSet.GetValue(p_lsdynaModel,sdiIdentifier("ids_type", 0, j), tempVal);
                    tempVal.GetValue(idsType);

                    tempVal = sdiValue(lsdKey);
                    handleRSet.GetValue(p_lsdynaModel,sdiIdentifier("KEY", 0, j), tempVal);
                    tempVal.GetValue(lsdKey);
                    if(lsdKey == "PART") ++nbClausePart;
                }

                sdiUIntList PartSetList;
                int partID = 0;
                int elem_Id; 

                if (nbClausePart > 0)
                {
                    p_ConvertUtils.GetPartIdsFromSetGeneral(setType, set_ID, PartSetList);
                    // tag elements of parts
                    EntityType partType = p_lsdynaModel->GetEntityType("*PART");

                    for (int j = 0; j < PartSetList.size(); ++j)
                    {
                        partID = PartSetList[j];
                        HandleRead partHread;
                        p_lsdynaModel->FindById(partType, partID, partHread);
                        if(partHread.IsValid())
                        {
                            EntityRead partEdit(p_lsdynaModel, partHread);
                            SelectionElementRead selPartElem(partEdit);
                            while(selPartElem.Next())
                            {
                                elem_Id = selPartElem->GetId();
                                if (elem_Id) elemIdList.push_back(elem_Id);
                            }
                        }
                    }
                }
                else if (setType == "*SET_PART_LIST_TITLE")
                {
                    p_ConvertUtils.GetPartIdsFromPartSet(setType, set_ID, PartSetList);
                    // tag elements of parts
                    EntityType partType = p_lsdynaModel->GetEntityType("*PART");
                    for (int j = 0; j < PartSetList.size(); ++j)
                    {
                        partID = PartSetList[j];
                        HandleRead partHread;
                        p_lsdynaModel->FindById(partType, partID, partHread);
                        if(partHread.IsValid())
                        {
                            EntityRead partEdit(p_lsdynaModel, partHread);
                            SelectionElementRead selPartElem(partEdit);
                            while(selPartElem.Next())
                            {
                                elem_Id = selPartElem->GetId();
                                if (elem_Id) elemIdList.push_back(elem_Id);
                            }
                        }
                    }
                }
                else
                {
                    // not a set of parts
                    for (unsigned int elemId : tempelemIdsList)
                        elemIdList.push_back(elemId);
                }
            }
            sdiVectorSort(elemIdList);
            sdiVectorUnique(elemIdList);
            if (entityType == "NODE" || entityType == "SPH")
                entityList = sdiValueEntityList(p_lsdynaModel->GetEntityType("*NODE") , elemIdList);
            else
                entityList = sdiValueEntityList(p_lsdynaModel->GetEntityType("*ELEMENT_" + entityType), elemIdList);
        }
        else
        {
            selectTH->GetValue(sdiIdentifier("ids"), tempVal);
            tempVal.GetValue(entityList);
        }
        if (keyWord.find("_ID") != keyWord.npos)
        {
            tempVal = sdiValue(nameList);
            selectTH->GetValue(sdiIdentifier("Comments"), tempVal);
            tempVal.GetValue(nameList);
        }
        if (keyWord.find("LOCAL") != keyWord.npos)
        {
            tempVal = sdiValue(skewList);
            selectTH->GetValue(sdiIdentifier("DH_cid"), tempVal);
            tempVal.GetValue(skewList);
        }
        map<sdiString, sdiUIntList> containKeywordVsElemIds;
        sdiStringList outVars({ "DEF" });

        size_t lengthTH = sdiString("*DATABASE_HISTORY_").size();
        size_t keyWordLen = keyWord.size();
        if (entityType == "BEAM")
        {
            for (sdiValueEntity entity : entityList)
            {
                sdiString radKeyWord = "/BEAM";
                HandleElementRead elemHRead;
                p_ConvertUtils.FindRadElement(entity, radKeyWord, elemHRead);
                if (elemHRead.IsValid())
                {
                    ElementRead elemRead(p_radiossModel, elemHRead);
                    containKeywordVsElemIds[radKeyWord].push_back(elemRead.GetId());
                }
            }
        }
        else if (entityType == "DISCRETE")
        {
            sdiUIntList elemidList;
            entityList.GetIdList(elemidList);
            containKeywordVsElemIds["/SPRING"] = elemidList;
        }
        else if (entityType == "SHELL")
        {
            for (sdiValueEntity entity : entityList)
            {
                sdiString radKeyWord = "/SHELL";
                HandleElementRead elemHRead;
                p_ConvertUtils.FindRadElement(entity, radKeyWord, elemHRead);
                if (elemHRead.IsValid())
                {
                    ElementRead elemRead(p_radiossModel, elemHRead);
                    containKeywordVsElemIds[radKeyWord].push_back(elemRead.GetId());
                }
            }
            outVars.push_back("STRAIN");
        }
        else if (entityType == "SOLID")
        {
            sdiUIntList elemidList;
            entityList.GetIdList(elemidList);
            containKeywordVsElemIds["/BRICK"] = elemidList;
            outVars.push_back("STRAIN");
        }
        else if (entityType == "NODE")
        {
            sdiUIntList nodeidList;
            entityList.GetIdList(nodeidList);
            containKeywordVsElemIds["/NODE"] = nodeidList;
            outVars.push_back("A");
            outVars.push_back("AR");
            outVars.push_back("VR");
        }
        
        else if (entityType == "SPH")
        {
            sdiUIntList nodeidList;
            entityList.GetIdList(nodeidList);
            containKeywordVsElemIds["/SPHCEL"] = nodeidList;
            //outVars.push_back("ALL");
        }
        
        //------------------------------
        else if (entityType == "SEATBELT")
        {
            sdiUIntList elemidList;
            entityList.GetIdList(elemidList);
            //containKeywordVsElemIds["/SPRING"] = elemidList;
//==================
            // *element_seatbelt + *section_seatbelt --> 1D SPRING TH
            // *element_seatbelt + *section_shell    --> 2D SHELL TH

            int elemId = elemidList[0];

            HandleRead elementHRead;
            p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*ELEMENT_SEATBELT"), elemId, elementHRead);

            if (elementHRead.IsValid())
            {
                HandleRead partHRead;
                elementHRead.GetEntityHandle(p_lsdynaModel, sdiIdentifier("PID"), partHRead);
                if (partHRead.IsValid())
                {
                    HandleRead propHRead;
                    partHRead.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHRead);
                    if (propHRead.IsValid())
                    {
                        EntityRead propEntRead(p_lsdynaModel, propHRead);
                        sdiString propCard = propEntRead.GetKeyword();
                        if( propCard.find("*SECTION_SHELL") != string::npos)
                        {
                            containKeywordVsElemIds["/SHELL"] = elemidList;
                        }
                        else if( propCard.find("*SECTION_SEATBELT") != string::npos)
                        {
                            containKeywordVsElemIds["/SPRING"] = elemidList;
                        }
                    }
                }
            }
//==================
        }
        //------------------------------
        for (auto tempPair : containKeywordVsElemIds)
        {
            sdiString entityTypeNamed = tempPair.first;
            EntityType entityType = p_radiossModel->GetEntityType(entityTypeNamed);
            if (entityTypeNamed == "/SHELL")
                entityTypeNamed = "/SHEL";
            else if (entityTypeNamed == "/BRICK")
                entityTypeNamed = "/BRIC";
            sdiString radTHkeyWord = "/TH" + entityTypeNamed;
            sdiUIntList entityIdList = tempPair.second;
            HandleEdit thHandleEdit;
            int idArrSIze = (int)entityIdList.size();
            if (idArrSIze)
            {
                p_radiossModel->CreateEntity(thHandleEdit, radTHkeyWord, selectTH->GetName());
                EntityEdit  thEdit(p_radiossModel, thHandleEdit);
                thEdit.SetValue(sdiIdentifier("idsmax"), sdiValue(idArrSIze));
                thEdit.SetValue(sdiIdentifier("elem_ID"), sdiValue(sdiValueEntityList(entityType, entityIdList)));
                thEdit.SetValue(sdiIdentifier("Number_Of_Variables"), sdiValue((int)outVars.size()));
                thEdit.SetValue(sdiIdentifier("var"), sdiValue(outVars));
                if (!nameList.empty())
                    thEdit.SetValue(sdiIdentifier("elem_name"), sdiValue(nameList));
                if (!skewList.IsEmpty())
                {
                    p_UpdateThBasedOnREFFlag(*selectTH, idArrSIze, skewList, thEdit);
                }
                sdiConvert::SDIHandlReadList dynaTh = { {selectTH->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(thHandleEdit, dynaTh));
            }
        }
    }
}

void sdiD2R::ConvertTimeHistory::p_UpdateThBasedOnREFFlag(const sdi::EntityRead& dynaDH, const int& idArrSIze, const sdiValueEntityList& skewList, sdi::EntityEdit& radTHEdit)
{
    sdiUIntList skewIdList;
    sdiIntList refFlagList;
    skewList.GetIdList(skewIdList);
    EntityType lsdDefCoordType = p_lsdynaModel->GetEntityType("*DEFINE_COORDINATE");
    if (skewIdList.size() != idArrSIze)
    {
        skewIdList = sdiUIntList(idArrSIze, skewIdList[0]);
    }
    else if (skewIdList.size() == idArrSIze)
    {
        sdiValue tempVal(refFlagList);
        dynaDH.GetValue(sdiIdentifier("DH_ref"), tempVal);
        tempVal.GetValue(refFlagList);
    }
    EntityType radSkewType = p_radiossModel->GetEntityType("/SKEW");
    if (!refFlagList.empty())
    {
        int i = 0;
        for (size_t i = 0; i < idArrSIze; ++i)
        {
            HandleRead skewHanlde;
            p_radiossModel->FindById(radSkewType, skewIdList[i], skewHanlde);
            if (skewHanlde.IsValid())
            {
                sdiStringList skewNodeAttrbStrList({ "N1","N2","N3" });
                EntityRead skewEntRead(p_radiossModel, skewHanlde);
                if (refFlagList[i] == 2)
                {
                    if (skewEntRead.GetKeyword() == "/SKEW/MOV")
                    {
                        HandleEdit frameHandleEdit;
                        p_radiossModel->CreateEntity(frameHandleEdit, "/FRAME/MOV", "", p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                        EntityEdit framEntEdit(p_radiossModel, frameHandleEdit);
                        for (size_t j = 0; j < 3; j++)
                        {
                            sdiValue tempValue;
                            skewEntRead.GetValue(sdiIdentifier(skewNodeAttrbStrList[j]), tempValue);
                            framEntEdit.SetValue(sdiIdentifier(skewNodeAttrbStrList[j]), tempValue);
                        }
                        sdiString radDIR;
                        sdiValue tempVal(radDIR);
                        skewEntRead.GetValue(sdiIdentifier("DIR"), tempVal);
                        tempVal.GetValue(radDIR);
                        framEntEdit.SetValue(sdiIdentifier("DIR"), sdiValue(radDIR));

                        framEntEdit.SetValue(sdiIdentifier("titlestr"), sdiValue("FrameFix_TH_NODE_" + to_string(dynaDH.GetId())));
                        skewIdList[i] = framEntEdit.GetId();
                    }
                    else if (skewEntRead.GetKeyword() == "/SKEW/FIX")
                    {
                        // create anyway a "/FRAME/MOV"
                        SelectionRead systSelect(p_lsdynaModel, "*DEFINE_COORDINATE_NODES");
                        while(systSelect.Next())
                        {
                            if(systSelect->GetId() == skewIdList[i])
                            {
                                EntityType radNodeType = p_radiossModel->GetEntityType("/NODE");
                                HandleRead handleN1;
                                HandleRead handleN2;
                                HandleRead handleN3;
                                vector<sdiString> attribName({"N1", "N2", "N3"});
                                vector<reference_wrapper<HandleRead>> attribVals({ handleN1, handleN2, handleN3 });
                                p_ConvertUtils.GetEntityHandles(*systSelect, attribName, attribVals);

                                if (!handleN1.IsValid() || !handleN2.IsValid() || !handleN3.IsValid())
                                  continue;

                                sdiString lsdDir;
                                sdiValue tempVal(lsdDir);
                                systSelect->GetValue(sdiIdentifier("DIR"), tempVal);
                                tempVal.GetValue(lsdDir);
                                if(lsdDir == "" ) lsdDir = "X";

                                HandleEdit frameHandleEdit;
                                p_radiossModel->CreateEntity(frameHandleEdit, "/FRAME/MOV", "", p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                                EntityEdit framEntEdit(p_radiossModel, frameHandleEdit);

                                for (size_t j = 0; j < 3; ++j)
                                {
                                    framEntEdit.SetValue(sdiIdentifier(attribName[j]),sdiValue(sdiValueEntity(sdiValueEntityType(radNodeType), attribVals[j].get().GetId(p_lsdynaModel))));
                                }
                                framEntEdit.SetValue(sdiIdentifier("DIR"), sdiValue(lsdDir));
                                framEntEdit.SetValue(sdiIdentifier("titlestr"), sdiValue("FrameFix_TH_NODE_" + to_string(dynaDH.GetId())));
                                skewIdList[i] = framEntEdit.GetId();
                            }
                        }
                    }
                    else
                        skewIdList[i] = 0;
                }
                else if (refFlagList[i] == 0)
                {
                    auto test = skewEntRead.GetKeyword();
                    if (skewEntRead.GetKeyword() == "/SKEW/MOV")
                    {
                        HandleRead dynaSystHandle;
                        if (p_lsdynaModel->FindById(lsdDefCoordType, skewIdList[i], dynaSystHandle))
                        {
                            EntityRead dynaSystEntRead(p_lsdynaModel, dynaSystHandle);
                            HandleEdit skewHandleEdit;
                            p_radiossModel->CreateEntity(skewHandleEdit, "/SKEW/FIX");
                            EntityEdit skewEntEdit(p_radiossModel, skewHandleEdit);
                            skewEntEdit.SetValue(sdiIdentifier("titlestr"), sdiValue("SkewFix_TH_NODE_" + to_string(dynaDH.GetId())));
                            HandleRead handleN1;
                            HandleRead handleN2;
                            HandleRead handleN3;
                            dynaSystEntRead.GetEntityHandle(sdiIdentifier("N1"), handleN1);
                            dynaSystEntRead.GetEntityHandle(sdiIdentifier("N2"), handleN2);
                            dynaSystEntRead.GetEntityHandle(sdiIdentifier("N3"), handleN3);
                            NodeRead nodeReadN1(p_lsdynaModel, handleN1);
                            NodeRead nodeReadN2(p_lsdynaModel, handleN2);
                            NodeRead nodeReadN3(p_lsdynaModel, handleN3);
                            sdiTriple n1loc = nodeReadN1.GetPosition();
                            sdiTriple n2loc = nodeReadN2.GetPosition();
                            sdiTriple n3loc = nodeReadN3.GetPosition();

                            sdiTriple m1 = n3loc - n1loc;
                            sdiTriple m2 = (n2loc - n1loc) * m1;
                            sdiStringList originIdentifiers({ "Ox" , "Oy" , "Oz" });
                            sdiStringList m1Identifiers({ "X1" , "Y1" , "Z1" });
                            sdiStringList m2Identifiers({ "X2" , "Y2" , "Z2" });
                            for (size_t i = 0; i < 3; ++i)
                            {
                                skewEntEdit.SetValue(sdiIdentifier(originIdentifiers[i]), sdiValue(n1loc[i]));
                                skewEntEdit.SetValue(sdiIdentifier(m1Identifiers[i]), sdiValue(m1[i]));
                                skewEntEdit.SetValue(sdiIdentifier(m2Identifiers[i]), sdiValue(m2[i]));
                            }
                        }
                    }
                }
            }
        }
    }
    radTHEdit.SetValue(sdiIdentifier("skew_ID"), sdiValue(sdiValueEntityList(radSkewType, skewIdList)));
}
