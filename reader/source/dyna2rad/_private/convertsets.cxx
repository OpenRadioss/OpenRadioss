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

#include <unordered_set>
#include <dyna2rad/convertsets.h>
#include <dyna2rad/dyna2rad.h>
#include <dyna2rad/sdiUtils.h>

using namespace std;
using namespace sdi;

void sdiD2R::ConvertSet::ConvertSets()
{
    ConvertEntities();
}

void sdiD2R::ConvertSet::ConvertEntities()
{
    p_GetRad1DElemIdElemTypeMap();

    p_ConvertSetSegments();

    p_ConvertAllSets();

    p_ConvertSetSegmentGeneral();
}

void sdiD2R::ConvertSet::p_ConvertAllSets()
{
    SelectionRead selDynaSetParAdd(p_lsdynaModel, "*SET_PART_ADD");
    SelectionRead selDynaSet(p_lsdynaModel, "*SET");
    unsigned int setId = UINT_MAX;
    unsigned int setIdPrevious = UINT_MAX;
    unsigned int RenumberedSetIdPrevious = UINT_MAX;
    sdiString setType = "\0";
    sdiString keyWord = "\0";
    HandleEdit setHEdit;

    SelectionRead selRadiossSetCheck(p_radiossModel, "/SET");
    SelectionRead selDynaSetCheck(p_lsdynaModel, "*SET");
    sdiIntList setIds;
    while (selDynaSetCheck.Next())
    {
        setIds.push_back(selDynaSetCheck->GetId());
    }
    sdiVectorSort(setIds);
    
    sdiIntList dupplicatesIds;
    for (int i = 1; i < setIds.size(); i++)
    {
        if (setIds[i-1] == setIds[i]) 
        {
            dupplicatesIds.push_back(setIds[i]);
        }
    }

    sdiIntList dupplicatesIdsUsed;
    while (selRadiossSetCheck.Next())
    {
        dupplicatesIdsUsed.push_back(selRadiossSetCheck->GetId());
    }
    sdiVectorSort(dupplicatesIdsUsed);


    unsigned int IdNodeMaxDyna = p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*NODE"));

    while (selDynaSet.Next())
    {
        sdiValue tempVal;
        sdiString setTypePrevious(setType);
        sdiString KeywordPrevious(keyWord);
        keyWord = selDynaSet->GetKeyword();

        if (keyWord.find("SEGMENT_ADD") != keyWord.npos)
            setType = "SET";
        else if (keyWord.find("SEGMENT_INTER") != keyWord.npos || keyWord.find("INTER") != keyWord.npos)
            setType = "SET";
        else if (keyWord.find("SEGMENT") != keyWord.npos)
            continue;
        else if (keyWord.find("PART_ADD") != keyWord.npos)
            setType = "PART_SET";
        else if (keyWord.find("ADD") != keyWord.npos)
            setType = "SET";
        else if (keyWord.find("NODE") != keyWord.npos)
            setType = "NODE";
        else if (keyWord.find("PART") != keyWord.npos)
            setType = "PART";
        else if (keyWord.find("SOLID") != keyWord.npos)
            setType = "SOLID";
        else if (keyWord.find("SHELL") != keyWord.npos)
            setType = "SHELL";
        else if (keyWord.find("BEAM") != keyWord.npos)
            setType = "BEAM";
        else if (keyWord.find("DISCRETE") != keyWord.npos)
            setType = "SPRING";
        else
            continue;

        if (setType != "PART_SET" )
        {
            HandleEdit setHEdit;
            setId = selDynaSet->GetId();

            int iSetType = 0;
            int iSetCumul = 0;
            sdiString radiossSetCard = destCard;
            if (keyWord.find("COLLECT") != keyWord.npos)
            {
                radiossSetCard = "/SET/COLLECT";
                iSetType = 1;
            }

            if ( (strcmp(KeywordPrevious.c_str(),keyWord.c_str()) == 0) && (setId == setIdPrevious) ) {
                if (iSetType != 1) {
                    DynaToRad::ShowMessage(sdiMessageHandler::Level::Error, 23,keyWord.c_str(), setId);
                }
                else
                {
                    iSetCumul = 1;
                }
            }

            setIdPrevious = setId;
 
            auto dupplicatesIdsIter = std::find(dupplicatesIds.begin(), dupplicatesIds.end(), setId);
            auto dupplicatesIdsUsedIter = std::find(dupplicatesIdsUsed.begin(), dupplicatesIdsUsed.end(), setId);

            if ( (dupplicatesIdsIter != dupplicatesIds.end()) &&
                 (dupplicatesIdsUsedIter != dupplicatesIdsUsed.end() ) &&
                 iSetCumul == 0)
            {
 
                 setId = p_ConvertUtils.GetDynaMaxEntityID(srcEntityType);
            }
            else if ( (dupplicatesIdsIter != dupplicatesIds.end()) && iSetCumul == 0)
            {
                dupplicatesIdsUsed.push_back(setId);
                sdiVectorSort(dupplicatesIdsUsed);
            }
            else if(iSetCumul == 1)
            {
                setId = RenumberedSetIdPrevious;
            }

            RenumberedSetIdPrevious = setId;

            DynaToRad::PushToSetsMappingDetails(selDynaSet->GetId(), keyWord, setId);

            p_radiossModel->CreateEntity(setHEdit, radiossSetCard, selDynaSet->GetName(), setId);

            if (setHEdit.IsValid())
            {
                EntityEdit SetEntityEdit(p_radiossModel, setHEdit);
                SetEntityEdit.SetValue(sdiIdentifier("iset_Type"), sdiValue(iSetType));/*iset_type = 0 for general, 1 for collect*/

                if (setType == "NODE" ||
                    setType == "PART" ||
                    setType == "SET" ||
                    setType == "PART_SET" ||
                    setType == "SOLID" ||
                    setType == "SHELL" ||
                    setType == "BEAM" ||
                    setType == "SPRING")
                {
                    if (setType == "PART_SET")
                    {
                        p_ConvertSetPartADD(*selDynaSet, SetEntityEdit);
                    }
                    else if (keyWord.find("GENERATE") != keyWord.npos)
                    {
                        int geneMax = 0;
                        tempVal = sdiValue(geneMax);
                        selDynaSet->GetValue(sdiIdentifier("genemax"), tempVal);
                        tempVal.GetValue(geneMax);
                        if (geneMax)
                        {
                            sdiIntList startIdList;
                            sdiIntList endIdList;
                            sdiIntList incrList;
                            int incr = 0;
                            vector<reference_wrapper<sdiIntList>> attribVals({ startIdList , endIdList });
                            vector<sdiString> attribNames({ "BBEG" , "BEND" });
                            p_ConvertUtils.GetAttribValues(*selDynaSet, attribNames, attribVals);
                            if (setType == "NODE")
                            {
                                for (size_t j = 0; j < startIdList.size(); ++j)
                                {
                                    if (unsigned(startIdList[j]) > IdNodeMaxDyna)
                                        startIdList[j] = IdNodeMaxDyna;
                                    if (unsigned(endIdList[j]) > IdNodeMaxDyna)
                                        endIdList[j] = IdNodeMaxDyna;
                                }
                            }

                            if (keyWord.find("INCREMENT") != keyWord.npos)
                            {
                                tempVal = sdiValue(incrList);
                                selDynaSet->GetValue(sdiIdentifier("INCR"), tempVal);
                                tempVal.GetValue(incrList);
                            }

                            sdiStringList elemTypeNames;
                            if (setType == "BEAM")
                                elemTypeNames = sdiStringList({ "BEAM", "SPRING", "TRUSS" });
                            else if (setType == "SHELL")
                                elemTypeNames = sdiStringList({ "SHELL", "SH3N" });
                            else
                                elemTypeNames = sdiStringList({ setType });
                            for (size_t i = 0; i < elemTypeNames.size(); ++i)
                            {
                                int tempI = (int)i;
                                SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, tempI), sdiValue(elemTypeNames[i]));
                                SetEntityEdit.SetValue(sdiIdentifier("opt_G", 0, tempI), sdiValue(1));
                                SetEntityEdit.SetValue(sdiIdentifier("genemax", 0, tempI), sdiValue((int)startIdList.size()));
                                SetEntityEdit.SetValue(sdiIdentifier("start", 0, tempI), sdiValue(startIdList));
                                SetEntityEdit.SetValue(sdiIdentifier("end", 0, tempI), sdiValue(endIdList));
                                if (!incrList.empty())
                                    SetEntityEdit.SetValue(sdiIdentifier("by", 0, tempI), sdiValue(incrList));
                                else
                                    SetEntityEdit.SetValue(sdiIdentifier("by", 0, tempI), sdiValue(sdiIntList(geneMax, 1)));

                            }
                            SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue((int)elemTypeNames.size()));
                        }
                    }
                    else if (keyWord.find("GENERAL") != keyWord.npos)
                    {
                        int numClauses = 0;
                        tempVal = sdiValue(numClauses);
                        selDynaSet->GetValue(sdiIdentifier("clausesmax"), tempVal);
                        tempVal.GetValue(numClauses);
                        int addClauses = 0;
                        int index = 0;
                        for (int i = 0; i < numClauses; ++i)
                        {
                            int idsmax = 0;
                            int optD = 0;
                            sdiString idsType;
                            sdiString lsdKey;
                            sdiUIntList idList;

                            tempVal = sdiValue(idsmax);
                            selDynaSet->GetValue(sdiIdentifier("idsmax", 0, i), tempVal);
                            tempVal.GetValue(idsmax);
                            idList.reserve(idsmax);

                            tempVal = sdiValue(idsType);
                            selDynaSet->GetValue(sdiIdentifier("ids_type", 0, i), tempVal);
                            tempVal.GetValue(idsType);

                            tempVal = sdiValue(lsdKey);
                            selDynaSet->GetValue(sdiIdentifier("KEY", 0, i), tempVal);
                            tempVal.GetValue(lsdKey);

                            tempVal = sdiValue(optD);
                            selDynaSet->GetValue(sdiIdentifier("opt_D", 0, i), tempVal);
                            tempVal.GetValue(optD);

                            for (int j = 0; j < idsmax; ++j)
                            {
                                sdiValueEntity entity;
                                sdiValue descValue;
                                selDynaSet->GetValue(sdiIdentifier("ids", 0, i, j), descValue);
                                descValue.GetValue(entity);
                                idList.push_back(entity.GetId());
                            }

                            if (lsdKey == "ALL")
                            {
                                if (setType == "SHELL" || setType == "BEAM" || setType == "SPRING")
                                {
                                    sdiStringList elemTypeNames;
                                    if (setType == "BEAM")
                                        elemTypeNames = sdiStringList({ "BEAM", "SPRING", "TRUSS"});
                                    else if (setType == "SHELL")
                                        elemTypeNames = sdiStringList({ "SHELL", "SH3N" });
                                    else
                                        elemTypeNames = sdiStringList({ setType });
                                    for (size_t j = 0; j < elemTypeNames.size(); ++j)
                                    {
                                        if (j > 0)
                                            ++addClauses;
                                        int indexVal = i + addClauses;
                                        SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, indexVal), sdiValue(elemTypeNames[j]));
                                        SetEntityEdit.SetValue(sdiIdentifier("opt_G", 0, indexVal), sdiValue(1));
                                        SetEntityEdit.SetValue(sdiIdentifier("genemax", 0, indexVal), sdiValue(1));
                                        SetEntityEdit.SetValue(sdiIdentifier("start", 0, indexVal), sdiValue(sdiUIntList(1, 1)));
                                        SetEntityEdit.SetValue(sdiIdentifier("end", 0, indexVal), sdiValue(sdiUIntList(1, 999999999)));
                                        SetEntityEdit.SetValue(sdiIdentifier("by", 0, indexVal), sdiValue(sdiUIntList(1, 1)));
                                    }
                                }
                                else
                                    SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, index), sdiValue(lsdKey));
                            }
                            else if (lsdKey == "BOX" && (setType == "BEAM" || setType == "SHELL" || setType == "SPRING"))
                            {
                                sdiStringList elemTypeNames;
                                if (setType == "BEAM")
                                    elemTypeNames = sdiStringList({ "BEAM", "SPRING", "TRUSS" });
                                else if (setType == "SHELL")
                                    elemTypeNames = sdiStringList({ "SHELL", "SH3N" });
                                else
                                    elemTypeNames = sdiStringList({ setType });

                                for (size_t j = 0; j < elemTypeNames.size(); ++j)
                                {
                                    if (j > 0)
                                        ++addClauses;
                                    int indexVal = i + addClauses;
                                    SetEntityEdit.SetValue(sdiIdentifier("opt_D", 0, indexVal), sdiValue(optD));
                                    SetEntityEdit.SetValue(sdiIdentifier("opt_B", 0, indexVal), sdiValue(1));
                                    SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, indexVal), sdiValue(elemTypeNames[j]));
                                    SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, indexVal), sdiValue((int)idList.size()));
                                    SetEntityEdit.SetValue(sdiIdentifier("ids", 0, indexVal), sdiValue(sdiValueEntityList(radBOXType, idList)));
                                }
                            }
                            else if (lsdKey == "ELEM")
                            {
                                if (setType == "BEAM")
                                {
                                    p_ConvertSetBeam(i, *selDynaSet, SetEntityEdit, addClauses);
                                }
                                else if (setType == "SHELL")
                                {
                                    p_ConvertSetShell(i, *selDynaSet, SetEntityEdit, addClauses);
                                }
                                else if (setType == "SOLID" || setType == "SPRING")
                                {
                                    SetEntityEdit.SetValue(sdiIdentifier("opt_D", 0, i + addClauses), sdiValue(optD));
                                    SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, i + addClauses), sdiValue(setType));
                                    SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, i + addClauses), sdiValue((int)idList.size()));
                                    if (setType == "SOLID")
                                        SetEntityEdit.SetValue(sdiIdentifier("ids", 0, i + addClauses), sdiValue(sdiValueEntityList(radSOLIDType, idList)));
                                    else
                                        SetEntityEdit.SetValue(sdiIdentifier("ids", 0, i + addClauses), sdiValue(sdiValueEntityList(radSPRINGType, idList)));
                                }
                            }
                            else
                            {
                                SetEntityEdit.SetValue(sdiIdentifier("opt_D", 0, i + addClauses), sdiValue(optD));
                                SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, i + addClauses), sdiValue(lsdKey));
                                SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, i + addClauses), sdiValue((int)idList.size()));
                                SetEntityEdit.SetValue(sdiIdentifier("ids", 0, i + addClauses), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/" + lsdKey), idList)));
                            }
                        }
                        SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(numClauses + addClauses));
                    }
                    else
                    {
                        int dummyInt = 0;
                        if (setType == "SHELL")
                        {
                            int addClauses = 0;
                            p_ConvertSetShell(-1, *selDynaSet, SetEntityEdit, dummyInt);
                        }
                        else if (setType == "BEAM")
                        {
                            p_ConvertSetBeam(-1, *selDynaSet, SetEntityEdit, dummyInt);
                        }
                        else
                        {
                            sdiUIntList idList;
                            sdiValueEntityList entityList;
                            tempVal = sdiValue(entityList);
                            selDynaSet->GetValue(sdiIdentifier("ids"), tempVal);
                            tempVal.GetValue(entityList);
                            entityList.GetIdList(idList);

                            if (keyWord.find("INTER") != keyWord.npos)
                            {
                                sdiUIntList radSetIdList;
                                p_GetRadSetIdListForLsdSetIdList(idList, keyWord, radSetIdList);

                                unsigned int idListFront = radSetIdList.front();
                                SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue((int)radSetIdList.size()));

                                SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(setType));
                                SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                                SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/" + setType),
                                                         sdiUIntList(1,idListFront))));

                                int sizeList = (int) radSetIdList.size();
                                for (int j = 1; j < sizeList ; j = j+1)
                                {
                                    SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, j), sdiValue(setType));
                                    SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, j), sdiValue(1));
                                    SetEntityEdit.SetValue(sdiIdentifier("opt_I", 0, j), sdiValue(1));

                                    radSetIdList.erase(radSetIdList.begin());
                                    unsigned int idListFront = radSetIdList.front();
                                    SetEntityEdit.SetValue(sdiIdentifier("ids", 0, j), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/" + setType),
                                                         sdiUIntList(1,idListFront))));
                                }
                            }

                            else if(idList.size() > 0)

                            {
                                SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                                SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(setType));
                                SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue((int)idList.size()));
                                if (setType == "SET")
                                {
                                    sdiUIntList radSetIdList;
                                    p_GetRadSetIdListForLsdSetIdList(idList, keyWord, radSetIdList);
                                    SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(destEntityType, radSetIdList)));
                                }
                                else if (setType == "SOLID")
                                    SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radSOLIDType, idList)));
                                else
                                    SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/" + setType), idList)));
                            }

                        }
                    }
                }

                sdiConvert::SDIHandlReadList dynaSet = { {selDynaSet->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(setHEdit, dynaSet));
            }
        }
    }


    
    while (selDynaSetParAdd.Next())
    {
        sdiValue tempVal;
        sdiString setTypePrevious(setType);
        sdiString KeywordPrevious(keyWord);
        keyWord = selDynaSetParAdd->GetKeyword();


        HandleEdit setHEdit;
        setId = selDynaSetParAdd->GetId();

        int iSetType = 0;
        int iSetCumul = 0;
        sdiString radiossSetCard = destCard;

        setIdPrevious = setId;
        
        auto dupplicatesIdsIter = std::find(dupplicatesIds.begin(), dupplicatesIds.end(), setId);
        auto dupplicatesIdsUsedIter = std::find(dupplicatesIdsUsed.begin(), dupplicatesIdsUsed.end(), setId);

        if ( (dupplicatesIdsIter != dupplicatesIds.end()) && 
             (dupplicatesIdsUsedIter != dupplicatesIdsUsed.end() ) &&
             iSetCumul == 0)
        {
            
             setId = p_ConvertUtils.GetDynaMaxEntityID(srcEntityType);
        }
        else if ( (dupplicatesIdsIter != dupplicatesIds.end()) && iSetCumul == 0)
        {
            dupplicatesIdsUsed.push_back(setId);
            sdiVectorSort(dupplicatesIdsUsed); 
        }

        RenumberedSetIdPrevious = setId;

        DynaToRad::PushToSetsMappingDetails(selDynaSetParAdd->GetId(), keyWord, setId);

        p_radiossModel->CreateEntity(setHEdit, radiossSetCard, selDynaSetParAdd->GetName(), setId);

        if (setHEdit.IsValid())
        {
            EntityEdit SetEntityEdit(p_radiossModel, setHEdit);
            SetEntityEdit.SetValue(sdiIdentifier("iset_Type"), sdiValue(iSetType));/*iset_type = 0 for general, 1 for collect*/

            p_ConvertSetPartADD(*selDynaSetParAdd, SetEntityEdit);
            

            sdiConvert::SDIHandlReadList dynaSet = { {selDynaSetParAdd->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(setHEdit, dynaSet));
        }
    }

}

void sdiD2R::ConvertSet::p_ConvertSetSegments()
{
    SelectionRead selDynaSetSeg(p_lsdynaModel, "*SET_SEGMENT");
    EntityType radNodeType = p_radiossModel->GetEntityType("/NODE");

    while (selDynaSetSeg.Next())
    {
        sdiString setSegKeyWord = selDynaSetSeg->GetKeyword();
        size_t pos = setSegKeyWord.find("_TITLE");
        if (pos != string::npos)
            setSegKeyWord.erase(pos);

        sdiString radiossSetCard = destCard;
        int iSetType = 0;
        if (setSegKeyWord.find("COLL") != setSegKeyWord.npos)
        {
            radiossSetCard = "/SET/COLLECT";
            iSetType = 1;
        }
        else if (setSegKeyWord.size() > 12)
            continue;
        int idsmax = 0;
        sdiValue tempVal(idsmax);
        selDynaSetSeg->GetValue(sdiIdentifier("idsmax"), tempVal);
        tempVal.GetValue(idsmax);
        if (idsmax)
        {
            sdiValueEntityList node1EntList;
            sdiValueEntityList node2EntList;
            sdiValueEntityList node3EntList;
            sdiValueEntityList node4EntList;
            sdiStringList nodeAttrNameList({ "node1", "node2", "node3", "node4" });
            vector<reference_wrapper<sdiValueEntityList>> nodeEntityList({ node1EntList , node2EntList , node3EntList , node4EntList });
            p_ConvertUtils.GetAttribValues(*selDynaSetSeg, nodeAttrNameList, nodeEntityList);

            sdiUIntList node1List;
            node1EntList.GetIdList(node1List);
            sdiUIntList node2List;
            node2EntList.GetIdList(node2List);
            sdiUIntList node3List;
            node3EntList.GetIdList(node3List);
            sdiUIntList node4List;
            node4EntList.GetIdList(node4List);
            sdiIntList idsList;
            idsList.reserve(idsmax);
            for (size_t i = 0; i < idsmax; ++i)
                idsList.push_back(++maxSegId);
            HandleEdit setHEdit;
            unsigned int setId = selDynaSetSeg->GetId();
            if (!p_radiossModel->IsIdAvailable(destEntityType, setId))
            {
                setId = p_ConvertUtils.GetDynaMaxEntityID(srcEntityType);
            }
            DynaToRad::PushToSetsMappingDetails(selDynaSetSeg->GetId(), selDynaSetSeg->GetKeyword(), setId);
            p_radiossModel->CreateEntity(setHEdit, radiossSetCard, selDynaSetSeg->GetName(), setId);
            if (setHEdit.IsValid())
            {
                EntityEdit SetEntityEdit(p_radiossModel, setHEdit);
                SetEntityEdit.SetValue(sdiIdentifier("iset_Type"), sdiValue(iSetType));

                SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                SetEntityEdit.SetValue(sdiIdentifier("segmax", 0, 0), sdiValue(idsmax));
                SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("SEG")));
                SetEntityEdit.SetValue(sdiIdentifier("segid", 0, 0), sdiValue(idsList));
                SetEntityEdit.SetValue(sdiIdentifier("ids1", 0, 0), sdiValue(sdiValueEntityList(radNodeType, node1List)));
                SetEntityEdit.SetValue(sdiIdentifier("ids2", 0, 0), sdiValue(sdiValueEntityList(radNodeType, node2List)));
                SetEntityEdit.SetValue(sdiIdentifier("ids3", 0, 0), sdiValue(sdiValueEntityList(radNodeType, node3List)));
                SetEntityEdit.SetValue(sdiIdentifier("ids4", 0, 0), sdiValue(sdiValueEntityList(radNodeType, node4List)));

                sdiConvert::SDIHandlReadList dynaSet = { {selDynaSetSeg->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(setHEdit, dynaSet));
            }
        }
    }
}

void sdiD2R::ConvertSet::p_ConvertSetSegmentGeneral()
{
    SelectionRead selDynaSetSegGen(p_lsdynaModel, "*SET_SEGMENT_GENERAL");

    if (selDynaSetSegGen.Count())
    {
        map<sdiString, pair<sdiString, vector<int>>> mapLsdKeyVsMapRadKeyVsOpts =
        {
            {{"ALL"}, {"ALL", {{0, 0, 0}}}},
            {{"BOX"}, {"BOX", {{0, 0, 0}}}},
            {{"BOX_SHELL"}, {"SHELL", {{0, 1, 0}}}},
            {{"BOX_SLDIO"}, {"BOX", {{0, 0, 1}}}},
            {{"BOX_SOLID"}, {"SOLID", {{0, 1, 0}}}},
            {{"DBOX"}, {"BOX", {{1, 0, 0}}}},
            {{"DBOX_SHELL"}, {"SHELL", {{1, 1, 0}}}},
            {{"DBOX_SOLID"}, {"SOLID", {{1, 1, 0}}}},
            {{"DPART"}, {"PART", {{1, 0, 0}}}},
            {{"PART"}, {"PART", {{0, 0, 0}}}},
            {{"PART_IO"}, {"PART", {{0, 0, 1}}}},
            {{"SEG"}, {"SEG", {{0, 0, 0}}}},
            {{"DSEG"}, {"SEG", {{1, 0, 0}}}},
            {{"SET_SHELL"}, {"SET", {{0, 0, 0}}}},
            {{"SET_SOLID"}, {"SET", {{0, 0, 0}}}},
            {{"SET_SLDIO"}, {"SET", {{0, 0, 0}}}},
            {{"SHELL"}, {"SHELL", {{0, 0, 0}}}},
        };

        if (p_MapShelElemIdVsElemType.empty())
            p_GetRad1DElemIdElemTypeMap();

        while (selDynaSetSegGen.Next())
        {
            sdiString keyWord = selDynaSetSegGen->GetKeyword();
            unsigned int setId = selDynaSetSegGen->GetId();
            if (!p_radiossModel->IsIdAvailable(destEntityType, setId))
            {
                setId = p_ConvertUtils.GetDynaMaxEntityID(srcEntityType);
            }
            DynaToRad::PushToSetsMappingDetails(selDynaSetSegGen->GetId(), keyWord, setId);

            HandleEdit setHEdit;
            p_radiossModel->CreateEntity(setHEdit, destCard, selDynaSetSegGen->GetName(), setId);
            if (setHEdit.IsValid())
            {
                EntityEdit SetEntityEdit(p_radiossModel, setHEdit);
                SetEntityEdit.SetValue(sdiIdentifier("iset_Type"), sdiValue(0));
                int numClauses = 0;
                sdiValue tempVal(numClauses);
                selDynaSetSegGen->GetValue(sdiIdentifier("clausesmax"), tempVal);
                tempVal.GetValue(numClauses);
                int numNewClauses = 0;
                for (int i = 0; i < numClauses; ++i)
                {
                    int idsmax = 0;
                    int optD = 0;
                    sdiString lsdKey;

                    tempVal = sdiValue(lsdKey);
                    selDynaSetSegGen->GetValue(sdiIdentifier("KEY", 0, i), tempVal);
                    tempVal.GetValue(lsdKey);
                    auto itr = mapLsdKeyVsMapRadKeyVsOpts.find(lsdKey);
                    if (itr != mapLsdKeyVsMapRadKeyVsOpts.end())
                    {
                        auto radData = itr->second;
                        sdiString radKey = radData.first;
                        vector<int> options = radData.second;
                        SetEntityEdit.SetValue(sdiIdentifier("opt_D", 0, i + numNewClauses), sdiValue(options[0]));
                        SetEntityEdit.SetValue(sdiIdentifier("opt_B", 0, i + numNewClauses), sdiValue(options[1]));
                        SetEntityEdit.SetValue(sdiIdentifier("opt_A", 0, i + numNewClauses), sdiValue(options[2]));
                        if (lsdKey == "ALL")
                        {
                            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, i + numNewClauses), sdiValue(radKey));
                        }
                        else if (lsdKey == "SEG" || lsdKey == "DSEG")
                        {
                            SetEntityEdit.SetValue(sdiIdentifier("segmax", 0, i + numNewClauses), sdiValue(1));
                            SetEntityEdit.SetValue(sdiIdentifier("segid", 0, i + numNewClauses, 0), sdiValue(++maxSegId));
                            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, i + numNewClauses), sdiValue(sdiString(radKey)));
                            for (int k = 1; k <= 4; ++k)
                            {
                                sdiValueEntity nodeEntity;
                                sdiValue tempValue(nodeEntity);
                                selDynaSetSegGen->GetValue(sdiIdentifier("N" + to_string(k), 0, i), tempValue);
                                tempValue.GetValue(nodeEntity);
                                SetEntityEdit.SetValue(sdiIdentifier("ids" + to_string(k), 0, i + numNewClauses), sdiValue(sdiValueEntityList(1, sdiUIntList(1, nodeEntity.GetId()))));
                            }
                        }
                        else
                        {
                            sdiUIntList entityList;
                            if (!options[0])
                            {
                                entityList.reserve(3);
                                for (int k = 0; k < 3; ++k)
                                {
                                    sdiValueEntity entity;
                                    sdiValue tempValue(entity);
                                    selDynaSetSegGen->GetValue(sdiIdentifier("ids" + to_string(k), 0, i), tempValue);
                                    tempValue.GetValue(entity);
                                    if (entity.GetId())
                                        entityList.push_back(entity.GetId());
                                }
                            }
                            else
                            {
                                int idsmax = 0;
                                tempVal = sdiValue(idsmax);
                                selDynaSetSegGen->GetValue(sdiIdentifier("idsmax", 0, i), tempVal);
                                tempVal.GetValue(idsmax);
                                entityList.clear();
                                entityList.reserve(idsmax);

                                for (int j = 0; j < idsmax; ++j)
                                {
                                    sdiValueEntity entity;
                                    tempVal = sdiValue(entity);
                                    selDynaSetSegGen->GetValue(sdiIdentifier("ids", 0, i, j), tempVal);
                                    tempVal.GetValue(entity);
                                    if (entity.GetId())
                                        entityList.push_back(entity.GetId());
                                }
                            }
                            entityList.shrink_to_fit();
                            if (lsdKey == "SHELL")
                            {
                                sdiUIntList sh3nList;
                                sh3nList.reserve(3);
                                sdiUIntList tempEntityist(entityList);
                                for (const unsigned int entityId : tempEntityist)
                                {
                                    if (p_MapShelElemIdVsElemType[entityId] == "SH3N")
                                    {
                                        sh3nList.push_back(entityId);
                                        sdiVectorRemove(entityList,entityId);
                                    }
                                }
                                if (!entityList.empty())
                                {
                                    SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, i + numNewClauses), sdiValue(radKey));
                                    SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, i + numNewClauses), sdiValue((int)entityList.size()));
                                    SetEntityEdit.SetValue(sdiIdentifier("ids", 0, i + numNewClauses), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/" + radKey), entityList)));
                                }
                                if (!sh3nList.empty())
                                {
                                    if (!entityList.empty())
                                    {
                                        ++numNewClauses;
                                        SetEntityEdit.SetValue(sdiIdentifier("opt_D", 0, i + numNewClauses), sdiValue(options[0]));
                                        SetEntityEdit.SetValue(sdiIdentifier("opt_B", 0, i + numNewClauses), sdiValue(options[1]));
                                        SetEntityEdit.SetValue(sdiIdentifier("opt_A", 0, i + numNewClauses), sdiValue(options[2]));
                                        SetEntityEdit.SetValue(sdiIdentifier("segmax", 0, i + numNewClauses), sdiValue(0));
                                        SetEntityEdit.SetValue(sdiIdentifier("segid", 0, i + numNewClauses), sdiValue(0));
                                    }
                                    SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, i + numNewClauses), sdiValue(sdiString("SH3N")));
                                    SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, i + numNewClauses), sdiValue((int)sh3nList.size()));
                                    SetEntityEdit.SetValue(sdiIdentifier("ids", 0, i + numNewClauses), sdiValue(sdiValueEntityList(radSH3NType, sh3nList)));
                                }
                            }
                            else
                            {
                                SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, i + numNewClauses), sdiValue(radKey));
                                SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, i + numNewClauses), sdiValue((int)entityList.size()));
                                if (radKey == "SOLID")
                                    SetEntityEdit.SetValue(sdiIdentifier("ids", 0, i + numNewClauses), sdiValue(sdiValueEntityList(radSOLIDType, entityList)));
                                else
                                    SetEntityEdit.SetValue(sdiIdentifier("ids", 0, i + numNewClauses), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/" + radKey), entityList)));
                                if (lsdKey == "BOX_SHELL" || lsdKey == "DBOX_SHELL")
                                {
                                    ++numNewClauses;
                                    SetEntityEdit.SetValue(sdiIdentifier("opt_D", 0, i + numNewClauses), sdiValue(options[0]));
                                    SetEntityEdit.SetValue(sdiIdentifier("opt_B", 0, i + numNewClauses), sdiValue(options[1]));
                                    SetEntityEdit.SetValue(sdiIdentifier("opt_A", 0, i + numNewClauses), sdiValue(options[2]));
                                    SetEntityEdit.SetValue(sdiIdentifier("segmax", 0, i + numNewClauses), sdiValue(0));
                                    SetEntityEdit.SetValue(sdiIdentifier("segid", 0, i + numNewClauses), sdiValue(0));
                                    SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, i + numNewClauses), sdiValue(sdiString("SH3N")));
                                    SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, i + numNewClauses), sdiValue((int)entityList.size()));
                                    SetEntityEdit.SetValue(sdiIdentifier("ids", 0, i + numNewClauses), sdiValue(sdiValueEntityList(radSH3NType, entityList)));
                                }
                            }
                        }
                    }
                }
                SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(numClauses + numNewClauses));

                sdiConvert::SDIHandlReadList dynaSet = { {selDynaSetSegGen->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(setHEdit, dynaSet));
            }
        }
    }
}

void sdiD2R::ConvertSet::p_ConvertSetPartADD(const EntityRead& dynaSetRead, EntityEdit& SetEntityEdit)
{
    sdiString setType = "SET";
    int idsmax = 0;
    sdiValue tempVal(idsmax);
    dynaSetRead.GetValue(sdiIdentifier("idsmax"), tempVal);
    tempVal.GetValue(idsmax);

    sdiIntList setIds;
    tempVal = sdiValue(setIds);
    dynaSetRead.GetValue(sdiIdentifier("ids"), tempVal);
    tempVal.GetValue(setIds);

    sdiUIntList idsToAdd;
    sdiUIntList idsToRemove;
    sdiIntList startIdList;
    sdiIntList endIdList;
    idsToAdd.reserve(idsmax);
    idsToRemove.reserve(idsmax);
    startIdList.reserve(idsmax);
    endIdList.reserve(idsmax);

    for (size_t i = 0; i < idsmax; ++i)
    {
        int endId = DynaToRad::GetRadiossSetIdFromLsdSet(setIds[i], "*SET_PART" );
        if (endId < 0)
        {
            HandleRead setHRead;
            int startId = DynaToRad::GetRadiossSetIdFromLsdSet(setIds[i - 1], "*SET_PART" );
            endId = abs(endId);
            if (endId > startId)
            {
                startIdList.push_back(startId);
                endIdList.push_back(endId);
            }
            idsToRemove.push_back(startId);
        }
        else
            idsToAdd.push_back(DynaToRad::GetRadiossSetIdFromLsdSet(setIds[i], "*SET_PART" ));
    }
    for (unsigned int id : idsToRemove)
    {
        sdiVectorRemove(idsToAdd,id);
    }
    int clauseMax = -1;
    if (!idsToAdd.empty())
    {
        ++clauseMax;
        SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, clauseMax), sdiValue(setType));
        SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, clauseMax), sdiValue((int)idsToAdd.size()));
        SetEntityEdit.SetValue(sdiIdentifier("ids", 0, clauseMax), sdiValue(sdiValueEntityList(destEntityType, idsToAdd)));
    }
    if (!startIdList.empty() && !endIdList.empty())
    {
        ++clauseMax;
        SetEntityEdit.SetValue(sdiIdentifier("opt_G", 0, clauseMax), sdiValue(1));
        SetEntityEdit.SetValue(sdiIdentifier("genemax", 0, clauseMax), sdiValue((int)startIdList.size()));
        SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, clauseMax), sdiValue(setType));
        SetEntityEdit.SetValue(sdiIdentifier("start", 0, clauseMax), sdiValue(startIdList));
        SetEntityEdit.SetValue(sdiIdentifier("end", 0, clauseMax), sdiValue(endIdList));
        SetEntityEdit.SetValue(sdiIdentifier("by", 0, clauseMax), sdiValue(sdiIntList(endIdList.size(), 1)));

    }
    SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(++clauseMax));
}

void sdiD2R::ConvertSet::p_ConvertSetShell(const int& index, const EntityRead& dynaSetRead, EntityEdit& SetEntityEdit, int& addClauses)
{
    sdiUIntList sh3Nlist;
    sdiUIntList sh4Nlist;
    unsigned short int counter = 0;
    if (index == -1)
    {
        int idsmax = 0;
        sdiValue tempVal(idsmax);
        dynaSetRead.GetValue(sdiIdentifier("idsmax"), tempVal);
        tempVal.GetValue(idsmax);
        sh3Nlist.reserve(idsmax);
        sh4Nlist.reserve(idsmax);
        int clausesMax = -1;
        for (int i = 0; i < idsmax; ++i)
        {
            sdiValueEntity elemEntity;
            sdiValue tempVal(elemEntity);
            dynaSetRead.GetValue(sdiIdentifier("ids", 0, i), tempVal);
            tempVal.GetValue(elemEntity);
            unsigned int elemId = elemEntity.GetId();
            if (elemId)
            {
                if (p_MapShelElemIdVsElemType[elemId] == "SH3N")
                    sh3Nlist.push_back(elemId);
                else
                    sh4Nlist.push_back(elemId);
            }
        }
        if (!sh4Nlist.empty())
        {
            ++clausesMax;
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, clausesMax), sdiValue(sdiString("SHELL")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, clausesMax), sdiValue((int)sh4Nlist.size()));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, clausesMax), sdiValue(sdiValueEntityList(radSHELLType, sh4Nlist)));

        }
        if (!sh3Nlist.empty())
        {
            ++clausesMax;
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, clausesMax), sdiValue(sdiString("SH3N")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, clausesMax), sdiValue((int)sh3Nlist.size()));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, clausesMax), sdiValue(sdiValueEntityList(radSH3NType, sh3Nlist)));
        }
        SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(++clausesMax));
    }
    else
    {
        int idsmax = 0;
        sdiValue tempVal(idsmax);
        dynaSetRead.GetValue(sdiIdentifier("idsmax", 0, index), tempVal);
        tempVal.GetValue(idsmax);

        for (int i = 0; i < idsmax; ++i)
        {
            sdiValueEntity elemEntity;
            sdiValue tempVal(elemEntity);
            dynaSetRead.GetValue(sdiIdentifier("ids", 0, index, i), tempVal);
            tempVal.GetValue(elemEntity);
            unsigned int elemId = elemEntity.GetId();
            if (elemId)
            {
                if (p_MapShelElemIdVsElemType[elemId] == "SH3N")
                    sh3Nlist.push_back(elemId);
                else
                    sh4Nlist.push_back(elemId);
            }
        }
        int optD = 0;
        tempVal = sdiValue(optD);
        dynaSetRead.GetValue(sdiIdentifier("opt_D", 0, index), tempVal);
        tempVal.GetValue(optD);
        if (!sh4Nlist.empty())
        {
            ++counter;
            SetEntityEdit.SetValue(sdiIdentifier("opt_D", 0, index + addClauses), sdiValue(optD));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, index + addClauses), sdiValue(sdiString("SHELL")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, index + addClauses), sdiValue((int)sh4Nlist.size()));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, index + addClauses), sdiValue(sdiValueEntityList(radSHELLType, sh4Nlist)));

        }
        if (!sh3Nlist.empty())
        {
            if (counter == 1)
                ++addClauses;
            SetEntityEdit.SetValue(sdiIdentifier("opt_D", 0, index + addClauses), sdiValue(optD));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, index + addClauses), sdiValue(sdiString("SH3N")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, index + addClauses), sdiValue((int)sh3Nlist.size()));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, index + addClauses), sdiValue(sdiValueEntityList(radSH3NType, sh3Nlist)));
        }
    }

}

void sdiD2R::ConvertSet::p_ConvertSetBeam(const int& index, const sdi::EntityRead& dynaSetRead, sdi::EntityEdit& SetEntityEdit, int& addClauses)
{
    sdiUIntList trussList;
    sdiUIntList sprList;
    sdiUIntList beamList;
    unsigned short int counter = 0;
    if (index == -1)
    {
        int idsmax = 0;
        sdiValue tempVal(idsmax);
        dynaSetRead.GetValue(sdiIdentifier("idsmax"), tempVal);
        tempVal.GetValue(idsmax);
        beamList.reserve(idsmax);
        trussList.reserve(idsmax);
        sprList.reserve(idsmax);
        int clausesMax = -1;
        for (int i = 0; i < idsmax; ++i)
        {
            sdiValueEntity elemEntity;
            sdiValue tempVal(elemEntity);
            dynaSetRead.GetValue(sdiIdentifier("ids", 0, i), tempVal);
            tempVal.GetValue(elemEntity);
            unsigned int elemId = elemEntity.GetId();
            if (elemId)
            {
                sdiString elemType = p_MapBeamElemIdVsElemType[elemId];
                if (elemType == "TRUSS")
                    trussList.push_back(elemId);
                else if (elemType == "SPRING")
                    sprList.push_back(elemId);
                else
                    beamList.push_back(elemId);
            }
        }
        if (!beamList.empty())
        {
            ++clausesMax;
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, clausesMax), sdiValue(sdiString("BEAM")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, clausesMax), sdiValue((int)beamList.size()));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, clausesMax), sdiValue(sdiValueEntityList(radBEAMType, beamList)));
        }
        if (!sprList.empty())
        {
            ++clausesMax;
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, clausesMax), sdiValue(sdiString("SPRING")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, clausesMax), sdiValue((int)sprList.size()));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, clausesMax), sdiValue(sdiValueEntityList(radSPRINGType, sprList)));
        }
        if (!trussList.empty())
        {
            ++clausesMax;
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, clausesMax), sdiValue(sdiString("TRUSS")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, clausesMax), sdiValue((int)trussList.size()));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, clausesMax), sdiValue(sdiValueEntityList(radTRUSSType, trussList)));
        }
        SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(++clausesMax));
    }
    else
    {
        int idsmax = 0;
        sdiValue tempVal(idsmax);
        dynaSetRead.GetValue(sdiIdentifier("idsmax", 0, index), tempVal);
        tempVal.GetValue(idsmax);

        for (int i = 0; i < idsmax; ++i)
        {
            sdiValueEntity elemEntity;
            sdiValue tempVal(elemEntity);
            dynaSetRead.GetValue(sdiIdentifier("ids", 0, index, i), tempVal);
            tempVal.GetValue(elemEntity);
            unsigned int elemId = elemEntity.GetId();
            if (elemId)
            {
                sdiString elemType = p_MapBeamElemIdVsElemType[elemId];
                if (elemType == "TRUSS")
                    trussList.push_back(elemId);
                else if (elemType == "SPRING")
                    sprList.push_back(elemId);
                else
                    beamList.push_back(elemId);
            }
        }
        int optD = 0;
        tempVal = sdiValue(optD);
        dynaSetRead.GetValue(sdiIdentifier("opt_D", 0, index), tempVal);
        tempVal.GetValue(optD);
        if (!beamList.empty())
        {
            ++counter;
            SetEntityEdit.SetValue(sdiIdentifier("opt_D", 0, index + addClauses), sdiValue(optD));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, index + addClauses), sdiValue(sdiString("BEAM")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, index + addClauses), sdiValue((int)beamList.size()));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, index + addClauses), sdiValue(sdiValueEntityList(radBEAMType, beamList)));

        }
        if (!sprList.empty())
        {
            if (counter == 1)
                ++addClauses;
            SetEntityEdit.SetValue(sdiIdentifier("opt_D", 0, index + addClauses), sdiValue(optD));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, index + addClauses), sdiValue(sdiString("SPRING")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, index + addClauses), sdiValue((int)sprList.size()));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, index + addClauses), sdiValue(sdiValueEntityList(radSPRINGType, sprList)));
            ++counter;
        }
        if (!trussList.empty())
        {
            if (counter >= 1)
                ++addClauses;
            SetEntityEdit.SetValue(sdiIdentifier("opt_D", 0, index + addClauses), sdiValue(optD));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, index + addClauses), sdiValue(sdiString("TRUSS")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, index + addClauses), sdiValue((int)trussList.size()));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, index + addClauses), sdiValue(sdiValueEntityList(radTRUSSType, trussList)));
        }
    }

}


void sdiD2R::ConvertSet::p_GetRad1DElemIdElemTypeMap()
{
    SelectionElementRead selShel4N(p_radiossModel, "/SHELL");
    while (selShel4N.Next())
        p_MapShelElemIdVsElemType[selShel4N->GetId()] = "SHELL";

    SelectionElementRead selShel3N(p_radiossModel, "/SH3N");
    while (selShel3N.Next())
        p_MapShelElemIdVsElemType[selShel3N->GetId()] = "SH3N";

    SelectionElementRead selBeam(p_radiossModel, "/BEAM");
    while (selBeam.Next())
        p_MapBeamElemIdVsElemType[selBeam->GetId()] = "BEAM";

    SelectionElementRead selTruss(p_radiossModel, "/TRUSS");
    while (selTruss.Next())
        p_MapBeamElemIdVsElemType[selTruss->GetId()] = "TRUSS";

    SelectionElementRead selSpring(p_radiossModel, "/SPRING");
    while (selSpring.Next())
        p_MapBeamElemIdVsElemType[selSpring->GetId()] = "SPRING";

}

void sdiD2R::ConvertSet::p_GetRadSetIdListForLsdSetIdList(const sdiUIntList& lsdSetIdList, const sdiString& setKeyWord, sdiUIntList& radSetIdLIst)
{
    sdiString setKey = setKeyWord;
    size_t pos1 = setKey.find("_ADD");
    size_t pos2 = setKey.find("_INTER");
    if (pos1 != string::npos)
        setKey.erase(pos1);
    else if (pos2 != string::npos)
    {
        setKey.erase(pos2);
    }
    radSetIdLIst.reserve(lsdSetIdList.size());
    for (const unsigned int& lsdSetId : lsdSetIdList)
    {
        radSetIdLIst.push_back(DynaToRad::GetRadiossSetIdFromLsdSet(lsdSetId, setKey));
    }
}

namespace sdiD2R
{

ConvertRadiossSetsToGroups::ConvertRadiossSetsToGroups(sdi::ModelViewEdit* _pModelView,
    sdiConvert::ClientInfo* IsUsed) :
    pModelView(_pModelView), groupInfos(CompKeyword), pIsUsed(IsUsed)
{
    assert(nullptr != pModelView);
    if(nullptr == pModelView) return;

    //                            BEAM      BOX        BOX2        NODE    PART        QUAD      RBODY SEG     SET       SETCOL    SH3N      SHELL     SOLID         SPRING    SUBM        SUBS      TRIA      TRUSS
    vector<sdiString> grbeamChild({"/BEAM",  "/BOX"    ,"/BOX2"    ,"",     "/PART"    ,"",       "",   "",     "/GRBEAM","/GRBEAM","",       "",       "",           "",       "/SUBMODEL","/SUBSET","",       ""       });
    vector<sdiString> grbricChild({"",       "/BOX"    ,"/BOX2"    ,"",     "/PART"    ,"",       "",   "",     "/GRBRIC","/GRBRIC","",       "",       "/BRIC",      "",       "/SUBMODEL","/SUBSET","",       ""       });
    vector<sdiString> grnodChild ({"/GRBEAM","/BOX"    ,"/BOX2"    ,"/NODE","/PART"    ,"/GRQUAD","",   "/NODE","/GRNOD" ,"/GRNOD", "/GRSH3N","/GRSHEL","/GRBRIC",    "/GRSPRI","/SUBMODEL","/SUBSET","/GRTRIA","/GRTRUS"});
    vector<sdiString> grpartChild({"",       ""        ,""         ,"",     "/PART"    ,"",       "",   "",     "/GRPART","/GRPART","",       "",       "",           "",       "/SUBMODEL","/SUBSET","",       ""       });
    vector<sdiString> grsh3nChild({"",       "/BOX"    ,"/BOX2"    ,"",     "/PART"    ,"",       "",   "",     "/GRSH3N","/GRSH3N","/SH3N",  "",       "",           "",       "/SUBMODEL","/SUBSET","",       ""       });
    vector<sdiString> grshelChild({"",       "/BOX"    ,"/BOX2"    ,"",     "/PART"    ,"",       "",   "",     "/GRSHEL","/GRSHEL","",       "/SHEL",  "",           "",       "/SUBMODEL","/SUBSET","",       ""       });
    vector<sdiString> grspriChild({"",       "/BOX"    ,"/BOX2"    ,"",     "/PART"    ,"",       "",   "",     "/GRSPRI","/GRSPRI","",       "",       "",           "/SPRI",  "/SUBMODEL","/SUBSET","",       ""       });
    vector<sdiString> grtrusChild({"",       "/BOX"    ,"/BOX2"    ,"",     "/PART"    ,"",       "",   "",     "/GRTRUS","/GRTRUS","",       "",       "",           "",       "/SUBMODEL","/SUBSET","",       "/TRUS"  });
    vector<sdiString> surfChild  ({"",       "/BOX/EXT","/BOX2/EXT","",     "/PART/EXT","",       "",   "/SEG", "/SURF"  ,"/SURF"  ,"/GRSH3N","/GRSHEL","/GRBRIC/EXT","",       "/SUBMODEL","/SUBSET","",       ""       });
    groupInfos["/GRBEAM"] = GroupInfo("/GRBEAM", "GRBEAM", grbeamChild, pModelView);
    groupInfos["/GRBRIC"] = GroupInfo("/GRBRIC", "GRBRIC", grbricChild, pModelView);
    groupInfos["/GRNOD" ] = GroupInfo("/GRNOD" , "GRNOD" , grnodChild , pModelView);
    groupInfos["/GRPART"] = GroupInfo("/GRPART", "GRPART", grpartChild, pModelView);
    // groupInfos["/GRQUAD"] = GroupInfo("/GRQUAD", "GRQUAD",    Child, pModelView);
    groupInfos["/GRSH3N"] = GroupInfo("/GRSH3N", "GRSH3N", grsh3nChild, pModelView);
    groupInfos["/GRSHEL"] = GroupInfo("/GRSHEL", "GRSHEL", grshelChild, pModelView);
    groupInfos["/GRSPRI"] = GroupInfo("/GRSPRI", "GRSPRI", grspriChild, pModelView);
    // groupInfos["/GRTRIA"] = GroupInfo("/GRTRIA", "GRTRIA",    Child, pModelView);
    groupInfos["/GRTRUS"] = GroupInfo("/GRTRUS", "GRTRUS", grtrusChild, pModelView);
    // groupInfos["/LINE"  ] = GroupInfo("/LINE"  , "LINE"  ,    Child, pModelView);
    groupInfos["/SURF"  ] = GroupInfo("/SURF"  , "DSURF" , surfChild  , pModelView);

    clauseTypes["BEAM"  ] = CLAUSETYPE_BEAM  ;
    clauseTypes["BOX"   ] = CLAUSETYPE_BOX   ;
    clauseTypes["BOX2"  ] = CLAUSETYPE_BOX2  ;
    clauseTypes["NODE"  ] = CLAUSETYPE_NODE  ;
    clauseTypes["PART"  ] = CLAUSETYPE_PART  ;
    clauseTypes["QUAD"  ] = CLAUSETYPE_QUAD  ;
    clauseTypes["RBODY" ] = CLAUSETYPE_RBODY ;
    clauseTypes["SEG"   ] = CLAUSETYPE_SEG   ;
    clauseTypes["SET"   ] = CLAUSETYPE_SET   ;
    clauseTypes["SETCOL"] = CLAUSETYPE_SETCOL;
    clauseTypes["SH3N"  ] = CLAUSETYPE_SH3N  ;
    clauseTypes["SHELL" ] = CLAUSETYPE_SHELL ;
    clauseTypes["SOLID" ] = CLAUSETYPE_SOLID ;
    clauseTypes["SPRING"] = CLAUSETYPE_SPRING;
    clauseTypes["SUBM"  ] = CLAUSETYPE_SUBM  ;
    clauseTypes["SUBS"  ] = CLAUSETYPE_SUBS  ;
    clauseTypes["TRIA"  ] = CLAUSETYPE_TRIA  ;
    clauseTypes["TRUSS" ] = CLAUSETYPE_TRUSS ;

    setType = pModelView->GetEntityType("/SET");
}

void ConvertRadiossSetsToGroups::ConvertSets()
{
    if(nullptr == pModelView) return;
    EntityType setType = pModelView->GetEntityType("/SET");

    SelectionEdit selSet(pModelView, "/SET");

    // Get max id
    unsigned int maxSetId = 0;
    while(selSet.Next())
    {
        unsigned int setId = selSet->GetId();
        if(setId > maxSetId) maxSetId = setId;
    }
    for(auto it = groupInfos.begin(); it != groupInfos.end(); ++it) maxIds[it->first] = maxSetId;

    // convert
    selSet.Restart();
    vector<HandleEdit> hSets; // for deletion later
    hSets.reserve(selSet.Count());
    while(selSet.Next())
    {
        for(auto it = groupInfos.begin(); it != groupInfos.end(); ++it)
        {
            const sdiString& groupKeyword = it->first;
            EntityType groupType = it->second.type;

            unsigned int setId = selSet->GetId();
            if(nullptr != pIsUsed)
            {
                bool isUsed = false;
                bool isOk = pIsUsed->GetEntityInfo(groupType, setId, isUsed);
                if(isOk && !isUsed) continue;
            }

            // set include of set as current include
            const EntityRead& set = *selSet; // need an EntityRead, to be cleaned in SDI
            HandleRead currentInclude = set.GetInclude();
            pModelView->SetCurrentCollector(currentInclude);

            ConvertSet(*selSet, groupKeyword);
        }

        hSets.push_back(selSet->GetHandle());
    }

    // delete /SET
    for(auto it = hSets.begin(); it != hSets.end(); ++it)
    {
        pModelView->Delete(*it);
    }
}

bool ConvertRadiossSetsToGroups::ConvertSet(const sdi::EntityRead& set, const sdiString& groupKeyword)
{
    unsigned int setId = set.GetId();
    const GroupInfo& groupInfo = groupInfos[groupKeyword];
    std::set<unsigned int>& convertedGroups = convertedGroupsByType[groupKeyword];
    std::set<unsigned int>& unconvertibleGroups = unconvertibleGroupsByType[groupKeyword];

    // set might already be converted (or found as unconvertible) as child of a set of sets
    if(unconvertibleGroups.count(setId) != 0 || convertedGroups.count(setId) != 0) return false;

    unsigned int nbClause = GetValue<int>(set, "clausesmax");
    sdiUIntList subgroupIds, negSubgroupIds;
    subgroupIds.reserve(nbClause);
    negSubgroupIds.reserve(nbClause);
    HandleEdit hSubgroup; // declaring outside of loop, so keeping the last one, in case there is only one
    for(unsigned int iClause = 0; iClause < nbClause; ++iClause)
    {
        sdiString parentKeyword;
        unsigned int subgroupId = 0;

        sdiString KEY_type = GetValue<sdiString>(set, "KEY_type", iClause);
        int opt_D = GetValue<int>(set, "opt_D", iClause);
        int opt_G = GetValue<int>(set, "opt_G", iClause);

        if(KEY_type == "ALL")
        {
            if(groupKeyword != "/SURF" && groupKeyword != "/LINE")
            {
                sdiString subgroupKeyword = groupKeyword + "/GENE";
                subgroupId = ++maxIds[groupKeyword];
                pModelView->CreateEntity(hSubgroup, subgroupKeyword, set.GetName(), subgroupId);
                EntityEdit subgroup(pModelView, hSubgroup);
                subgroup.SetInclude(HandleRead()); // set into main file
                subgroup.SetValue(sdiIdentifier("grnodGenArrCnt"), sdiValue(1));
                /*
                subgroup.SetValue(sdiIdentifier("Ifirst"), sdiValue(sdiIntList(1, 1)));
                subgroup.SetValue(sdiIdentifier("Ilast"), sdiValue(sdiIntList(1, 99999999)));
                entity list for now. Dummy type node, but isn't used anyways... */
                subgroup.SetValue(sdiIdentifier("Ifirst"),
                    sdiValue(sdiValueEntityList(sdiValueEntityType("/NODE"), sdiUIntList(1, 1))));
                subgroup.SetValue(sdiIdentifier("Ilast"),
                    sdiValue(sdiValueEntityList(sdiValueEntityType("/NODE"), sdiUIntList(1, 99999999))));
                // this is necessary because we have no specific cfg file for each subtype:
                subgroup.SetValue(sdiIdentifier("set_Type"), sdiValue(sdiString("GENE")));
            }
            else
            {
                // keep what we have done until here but ignore the rest
                break;
            }
        }
        else if(0 == KEY_type.compare(0, 3, "SET") && 0 == opt_G)
        {
            sdiValue idsVal;
            set.GetValue(sdiIdentifier("ids", 0, iClause), idsVal);
            sdiValueEntityList list;
            idsVal.GetValue(list);
            const sdiUIntList& ids = list.GetList();
            for(unsigned int i = 0; i < ids.size(); ++i)
            {
                unsigned int id = ids[i];
                if(unconvertibleGroups.count(id) != 0)
                {
                    // child set has earlier been found as unconvertible, so skip
                    continue;
                }
                else if(convertedGroups.count(id) == 0)
                {
                    // try to convert child set
                    HandleRead hChildSet;
                    pModelView->FindById(setType, id, hChildSet);
                    if(!hChildSet.IsValid()) continue; // shouldn't happen
                    EntityRead childSet(pModelView, hChildSet);
                    bool isOk = ConvertSet(childSet, groupKeyword);
                    if(!isOk) continue; // conversion not successful, so skip
                }
                if(0 == opt_D) subgroupIds.push_back(id);
                else           negSubgroupIds.push_back(id);
            }
            continue; // skip the common portion for this clause
        }
        else
        {
            ClauseType clauseType = GetClauseType(KEY_type);
            assert(CLAUSETYPE_LAST != clauseType);

            parentKeyword = groupInfo.parentKeyword[clauseType];
            if(parentKeyword.size() == 0) continue; // group cannot contain KEY_type

            int opt_B = GetValue<int>(set, "opt_B", iClause);
            if(1 == opt_B)
            {
                parentKeyword = "BOX";
            }
            else
            {
                int opt_C = GetValue<int>(set, "opt_C", iClause);
                if(1 == opt_C) parentKeyword = "BOX2";
            }

            sdiString subgroupKeyword = groupKeyword + parentKeyword;
            subgroupId = ++maxIds[groupKeyword];
            pModelView->CreateEntity(hSubgroup, subgroupKeyword, set.GetName(), subgroupId);
            HandleEdit hEntgroup = hSubgroup; // by default the subgroup itself contains the entities

                                              // if the parentKeyword of the entities is a group itself, we have just created a layer
                                              // in between, we now have to create the actual subsubgroup with the entities
                                              // example: /GRNOD/GRSHEL with /GRSHEL/SHEL
            if(CLAUSETYPE_SET != clauseType && CLAUSETYPE_SETCOL != clauseType &&
                groupInfos.count(parentKeyword) > 0)
            {
                const GroupInfo& subsubgroupInfo = groupInfos[parentKeyword];
                sdiString subsubgroupKeyword =
                    subsubgroupInfo.keyword + subsubgroupInfo.parentKeyword[clauseType];
                unsigned int subsubgroupId = ++maxIds[subsubgroupInfo.keyword];
                pModelView->CreateEntity(hEntgroup, subsubgroupKeyword, set.GetName(), subsubgroupId);
                // first set subsubgroup into subgroup
                EntityEdit subgroup(pModelView, hSubgroup);
                subgroup.SetValue(sdiIdentifier("idsmax"), sdiValue(1));
                subgroup.SetValue(sdiIdentifier("ids"),
                    sdiValue(sdiValueEntityList(sdiValueEntityType(subsubgroupInfo.keyword), sdiUIntList(1, subsubgroupId))));
                // this is necessary because we have no specific cfg file for each subtype:
                if(parentKeyword.find('/', 4) == parentKeyword.npos)
                {
                    subgroup.SetValue(sdiIdentifier("set_Type"), sdiValue(parentKeyword.substr(1)));
                }
                else
                {
                    // for */EXT and */ALL
                    subgroup.SetValue(sdiIdentifier("set_Type"),
                        sdiValue(parentKeyword.substr(1, parentKeyword.find('/', 4) - 1)));
                }
                // now set local variables, so that following code will populate subsubgroup
                parentKeyword = subsubgroupInfo.parentKeyword[clauseType];
            }

            EntityEdit entgroup(pModelView, hEntgroup);

            if(1 == opt_G)
            {
                sdiValue incrVal;
                sdiIntList ids = GetValue<sdiIntList>(set, "start", iClause);
                entgroup.SetValue(sdiIdentifier("grnodGenArrCnt"), sdiValue((int)ids.size()));
                entgroup.SetValue(sdiIdentifier("Ifirst"),
                    sdiValue(sdiValueEntityList(sdiValueEntityType(parentKeyword), vector<unsigned int>(ids.begin(), ids.end()))));
                ids = GetValue<sdiIntList>(set, "end", iClause);
                entgroup.SetValue(sdiIdentifier("Ilast"),
                    sdiValue(sdiValueEntityList(sdiValueEntityType(parentKeyword), vector<unsigned int>(ids.begin(), ids.end()))));
                /* TBD
                set.GetValue(sdiIdentifier("by", 0, iClause), incrVal);
                entgroup.SetValue(sdiIdentifier("Iincr"), incrVal);
                ... GENE_INCR if anything other than 1
                */
                // this is necessary because we have no specific cfg file for each subtype:
                entgroup.SetValue(sdiIdentifier("set_Type"), sdiValue(sdiString("GENE")));
            }
            else if(CLAUSETYPE_SEG == clauseType)
            {
                if(parentKeyword == "/SEG")
                {
                    sdiValue idsval, nodesVal;
                    set.GetValue(sdiIdentifier("segid", 0, iClause), idsval);
                    sdiIntList ids;
                    idsval.GetValue(ids);
                    entgroup.SetValue(sdiIdentifier("segmax"), sdiValue((int)ids.size()));
                    entgroup.SetValue(sdiIdentifier("SEGidArray"), idsval);
                    set.GetValue(sdiIdentifier("ids1", 0, iClause), nodesVal);
                    entgroup.SetValue(sdiIdentifier("N1"), nodesVal);
                    set.GetValue(sdiIdentifier("ids2", 0, iClause), nodesVal);
                    entgroup.SetValue(sdiIdentifier("N2"), nodesVal);
                    set.GetValue(sdiIdentifier("ids3", 0, iClause), nodesVal);
                    entgroup.SetValue(sdiIdentifier("N3"), nodesVal);
                    set.GetValue(sdiIdentifier("ids4", 0, iClause), nodesVal);
                    entgroup.SetValue(sdiIdentifier("N4"), nodesVal);
                    // this is necessary because we have no specific cfg file for each subtype:
                    entgroup.SetValue(sdiIdentifier("set_Type"), sdiValue(sdiString("SEG")));
                }
                else if(parentKeyword == "/NODE")
                {
                    sdiValue nodesVal;
                    sdiValueEntityList ids, allIds;
                    set.GetValue(sdiIdentifier("ids1", 0, iClause), nodesVal);
                    nodesVal.GetValue(allIds);
                    allIds.GetList().reserve(allIds.GetList().size() * 4);
                    set.GetValue(sdiIdentifier("ids2", 0, iClause), nodesVal);
                    nodesVal.GetValue(ids);
                    allIds.GetList().insert(allIds.GetList().end(), ids.GetList().begin(), ids.GetList().end());
                    set.GetValue(sdiIdentifier("ids3", 0, iClause), nodesVal);
                    nodesVal.GetValue(ids);
                    allIds.GetList().insert(allIds.GetList().end(), ids.GetList().begin(), ids.GetList().end());
                    set.GetValue(sdiIdentifier("ids4", 0, iClause), nodesVal);
                    nodesVal.GetValue(ids);
                    allIds.GetList().insert(allIds.GetList().end(), ids.GetList().begin(), ids.GetList().end());
                    sdiVectorSort(allIds.GetList());
                    sdiVectorUnique(allIds.GetList());
                    entgroup.SetValue(sdiIdentifier("idsmax"), sdiValue((int)allIds.GetList().size()));
                    entgroup.SetValue(sdiIdentifier("ids"), sdiValue(allIds));
                    // this is necessary because we have no specific cfg file for each subtype:
                    entgroup.SetValue(sdiIdentifier("set_Type"), sdiValue(sdiString("NODE")));
                }
                else
                {
                    continue; // shouldn't get here
                }
            }
            else
            {
                sdiValue idsVal;
                set.GetValue(sdiIdentifier("ids", 0, iClause), idsVal);
                sdiValueEntityList ids;
                idsVal.GetValue(ids);
                if(ids.GetIdListCount() == 0) continue;
                entgroup.SetValue(sdiIdentifier("idsmax"), sdiValue(ids.GetIdListCount()));
                entgroup.SetValue(sdiIdentifier("ids"), idsVal);
                // this is necessary because we have no specific cfg file for each subtype:
                if(parentKeyword.find('/', 4) == parentKeyword.npos)
                {
                    entgroup.SetValue(sdiIdentifier("set_Type"), sdiValue(parentKeyword.substr(1)));
                }
                else
                {
                    // for */EXT and */ALL
                    entgroup.SetValue(sdiIdentifier("set_Type"),
                        sdiValue(parentKeyword.substr(1, parentKeyword.find('/', 4) - 1)));
                }
            }
        }

        if(0 == opt_D) subgroupIds.push_back(subgroupId);
        else           negSubgroupIds.push_back(subgroupId);
    }

    if(subgroupIds.size() == 1 && negSubgroupIds.size() == 0 && hSubgroup.IsValid())
    {
        // subgroup is becoming main group
        hSubgroup.SetId(pModelView, setId);
    }
    else if(subgroupIds.size() >= 1)
    {
        // create and populate parent group
        HandleEdit hGroup;
        sdiString keyword = groupKeyword + "/" + groupInfo.childKeyword;
        pModelView->CreateEntity(hGroup, keyword, set.GetName(), setId);
        EntityEdit group(pModelView, hGroup);
        group.SetValue(sdiIdentifier("set_Type"), sdiValue(groupInfo.childKeyword));
        sdiValueEntityList subgroups(sdiValueEntityType(groupKeyword), subgroupIds);
        group.SetValue(sdiIdentifier("idsmax"), sdiValue(subgroups.GetIdListCount()));
        group.SetValue(sdiIdentifier("ids"), sdiValue(subgroups));
        if(negSubgroupIds.size() > 0)
        {
            sdiValueEntityList negSubgroups(sdiValueEntityType(groupKeyword), negSubgroupIds);
            group.SetValue(sdiIdentifier("negativeIdsmax"), sdiValue(negSubgroups.GetIdListCount()));
            group.SetValue(sdiIdentifier("negativeIds"), sdiValue(negSubgroups));
        }
    }
    else
    {
        unconvertibleGroups.insert(setId);
        return false;
    }

    convertedGroups.insert(setId);
    return true;
}

ConvertRadiossSetsToGroups::ClauseType ConvertRadiossSetsToGroups::GetClauseType(const sdiString& key) const
{
    if(clauseTypes.count(key) > 0) return clauseTypes.at(key);
    else                           return CLAUSETYPE_LAST;
}

bool ConvertRadiossSetsToGroups::CompKeyword(const sdiString& keyword1, const sdiString& keyword2)
{
    size_t sizeMin = std::min(keyword1.size(), keyword2.size());
    return keyword1.compare(0, sizeMin, keyword2, 0, sizeMin) < 0;
}

} // namespace sdiD2R
