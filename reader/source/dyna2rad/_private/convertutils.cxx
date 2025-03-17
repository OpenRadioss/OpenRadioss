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
#include <dyna2rad/convertutils.h>
#include <dyna2rad/sdiUtils.h>
#include <sstream>
#include <sdiValue.h>
#include <iostream>

using namespace sdiD2R;
using namespace std;
using namespace sdi;
using namespace sdiConvert;

/////////////////////////////////////////////////////////////////////////////
// return 0 for MASS & SPH, 1 for 1D, 2 for 2D 3 for 3D
/////////////////////////////////////////////////////////////////////////////
int ConvertUtils::GetComponentType(const EntityRead& compHRead) const
{
    int retVal = -2;
    SelectionElementRead elems(compHRead);
    while (elems.Next())
    {
        size_t startPos = std::string("*ELEMENT_").length();
        const sdiString elemCard = elems->GetKeyword();
        if (!elemCard.compare(startPos, 4, "MASS"))
            retVal = 0;
        else if (!elemCard.compare(startPos, 3, "SPH"))
            retVal = 0;
        else if (!elemCard.compare(startPos, 8, "DISCRETE"))
            retVal = 1;
        else if (!elemCard.compare(startPos, 4, "BEAM"))
            retVal = 1;
        else if (!elemCard.compare(startPos, 8, "SEATBELT"))
            retVal = 1;
        else if (!elemCard.compare(startPos, 6, "PLOTEL"))
            retVal = 1;
        else if (!elemCard.compare(startPos, 5, "SHELL"))
            retVal = 2;
        else if (!elemCard.compare(startPos, 5, "SOLID"))
            retVal = 3;
        else if (!elemCard.compare(startPos, 6, "TSHELL"))
            retVal = 3;
        break;
    }
    return retVal;
}

/*  Returns Element selection handle for given keyword(prop or mat)*/
SelectionElementRead ConvertUtils::p_GetElements(const sdiString& keyWord, const EntityId& entityId) const
{
    EntityType entityType = p_lsdynaModel->GetEntityType(keyWord);
    EntityType radentityType = 0;
    sdiIdentifier identifier;
    if (entityType == p_lsdynaModel->GetEntityType("*MAT"))
    {
        radentityType = p_radiossModel->GetEntityType("/MAT");
        identifier = sdiIdentifier("mat_ID");
    }
    else if (entityType == p_lsdynaModel->GetEntityType("*SECTION"))
    {
        radentityType = p_radiossModel->GetEntityType("/PROP");
        identifier = sdiIdentifier("prop_ID");
    }

    sdiValueEntity entity(radentityType, entityId);
    sdiValue value(entity);
    sdi::FilterValue filter(identifier, value);
    sdi::SelectionRead compSelect(p_radiossModel, "/PART", filter);

    return SelectionElementRead(compSelect);
}

/* Return unique Element keyword list for a given property or material*/
void ConvertUtils::GetElemKeywords(const sdiString& keyWord, const unsigned int& entityId, sdiStringList& keyWordList) const
{

    auto elemSelect = p_GetElements(keyWord, entityId);
    keyWordList.reserve(elemSelect.Count());
    while (elemSelect.Next())
    {
        sdiString keyword = elemSelect->GetKeyword();
        if (!sdiVectorContains(keyWordList,keyword))
            keyWordList.push_back(keyword);
    }
    sdiVectorUnique(keyWordList);
}

void ConvertUtils::CreateCurve(   const sdiString& crvName, 
                                    const int& numPoints,
                                    const sdiDoubleList& crvPoints,
                                    HandleEdit& crvHEdit,
                                    const double scaleX,
                                    const double scaleY,
                                    const double offsetX,
                                    const double offsetY ) const
{
    if (numPoints && (numPoints * 2 == crvPoints.size()))
    {
        p_radiossModel->CreateEntity(crvHEdit, "/FUNCT", crvName, GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*DEFINE_CURVE")));
        if (crvHEdit.IsValid())
        {
            crvHEdit.SetValue(p_radiossModel, sdiIdentifier("numberofpoints"), sdiValue(numPoints));
            crvHEdit.SetValue(p_radiossModel, sdiIdentifier("points"), sdiValue(crvPoints));

            HandleEdit radMvFunct;
            p_radiossModel->CreateEntity(radMvFunct, "/MOVE_FUNCT", crvName, crvHEdit.GetId(p_radiossModel));

            if (radMvFunct.IsValid())
            {
                EntityEdit radMvFnctEdit(p_radiossModel, radMvFunct);
                vector<double> defaultVals = { scaleX, scaleY, offsetX, offsetY };
                vector<sdiString> radSetAttribs = {"Ascalex", "Fscaley", "Ashiftx", "Fshifty"};
                for (int i = 0; i < defaultVals.size(); ++i)
                {
                    radMvFnctEdit.SetValue(sdiIdentifier(radSetAttribs[i]), sdiValue(defaultVals[i]));
                }
            }
        }
    }

}

EntityId ConvertUtils::GetDynaMaxEntityID(const EntityType& entityType) const
{
    sdiString dynaKeyWord;
    sdiString radKeyWord;
    if (entityType == p_lsdynaModel->GetEntityType("*SECTION"))
    {
        dynaKeyWord = "*SECTION";
        radKeyWord = "/PROP";
    }
    else if (entityType == p_lsdynaModel->GetEntityType("*MAT"))
    {
        dynaKeyWord = "*MAT";
        radKeyWord = "/MAT";
    }
    else if (entityType == p_lsdynaModel->GetEntityType("*DEFINE_CURVE"))
    {
        dynaKeyWord = "*DEFINE_CURVE";
        radKeyWord = "/FUNCT";
    }
    else if (entityType == p_lsdynaModel->GetEntityType("*DEFINE_COORDINATE"))
    {
        dynaKeyWord = "*DEFINE_COORDINATE";
        radKeyWord = "/SKEW";
    }
    else if (entityType == p_lsdynaModel->GetEntityType("*INITIAL_VELOCITY"))
    {
        dynaKeyWord = "*INITIAL_VELOCITY";
        radKeyWord = "/INIVEL";
    }
    else if (entityType == p_lsdynaModel->GetEntityType("*BOUNDARY"))
    {
        dynaKeyWord = "*BOUNDARY";
        radKeyWord = "/BCS";
    }
    else if (entityType == p_lsdynaModel->GetEntityType("*RIGIDWALL"))
    {
        dynaKeyWord = "*RIGIDWALL";
        radKeyWord = "/RWALL";
    }
    else if (entityType == p_lsdynaModel->GetEntityType("*CONSTRAINED_RIGID_BODIES") || 
             entityType == p_lsdynaModel->GetEntityType("*CONSTRAINED_EXTRA_NODES"))
    {
        dynaKeyWord = "*CONSTRAINED_RIGID_BODIES";
        radKeyWord = "/MERGE";
    }
    else if (entityType == p_lsdynaModel->GetEntityType("*SET"))
    {
        dynaKeyWord = "*SET";
        radKeyWord = "/SET";
    }
    else if (entityType == p_lsdynaModel->GetEntityType("*DAMPING"))
    {
        dynaKeyWord = "*DAMPING";
        radKeyWord = "/DAMP";
    }
    else if (entityType == p_lsdynaModel->GetEntityType("*NODE"))
    {
        dynaKeyWord = "*NODE";
        radKeyWord = "/NODE";
    }
    else {
        return 0;
    }
    EntityId maxID = 0;
    map< sdiString, ModelViewRead*> tempMap({ {dynaKeyWord,p_lsdynaModel},  {radKeyWord, p_radiossModel, } });
    if (radKeyWord == "/FUNCT")
        tempMap["/TABLE"] = p_radiossModel; // TABLE and /FUNCT are in same id pool
    if (radKeyWord == "/SKEW")
        tempMap["/FRAME"] = p_radiossModel;
    if (radKeyWord == "/MERGE")
        tempMap["*CONSTRAINED_EXTRA_NODES"] = p_lsdynaModel;
    for (auto tempPair: tempMap)
    {
        SelectionRead selectEntity(tempPair.second, tempPair.first);
        while (selectEntity.Next())
        {
            EntityId  entityId = selectEntity->GetId();
            if (entityId > maxID)
                maxID = entityId;
        }
    }
    // The returned value is meant to be used for creation of new entities.
    // If there is no entity in the model yet, we leave it up to model by returning 0.
    // This is important in case the entity goes into an include with offset.
    if(maxID > 0) return ++maxID;
    else return 0;
}

EntityId ConvertUtils::GetRadiossMaxEntityID(const EntityType& entityType) const
{
    sdiString dynaKeyWord;
    sdiString radKeyWord;
    if (entityType == p_radiossModel->GetEntityType("/IMPDISP"))
    {
        dynaKeyWord = "*BOUNDARY_PRESCRIBED_MOTION";
        radKeyWord = "/IMPDISP";
    }    
    else if (entityType == p_radiossModel->GetEntityType("/IMPVEL"))
    {
        dynaKeyWord = "*BOUNDARY_PRESCRIBED_MOTION";
        radKeyWord = "/IMPVEL";
    }
    else if (entityType == p_radiossModel->GetEntityType("/IMPACC"))
    {
        dynaKeyWord = "*BOUNDARY_PRESCRIBED_MOTION";
        radKeyWord = "/IMPACC";
    }
    else if (entityType == p_radiossModel->GetEntityType("/MAT"))
    {
        dynaKeyWord = "*MAT";
        radKeyWord = "/MAT";
    }
    else {
        return 0;
    }
    EntityId maxID = 0;
    map< sdiString, ModelViewRead*> tempMap({ {dynaKeyWord,p_lsdynaModel},  {radKeyWord, p_radiossModel, } });
    if (radKeyWord == "/FUNCT")
        tempMap["/TABLE"] = p_radiossModel; // TABLE and /FUNCT are in same id pool
    if (radKeyWord == "/SKEW")
        tempMap["/FRAME"] = p_radiossModel;
    if (radKeyWord == "/MERGE")
        tempMap["*CONSTRAINED_EXTRA_NODES"] = p_lsdynaModel;
    for (auto tempPair: tempMap)
    {
        SelectionRead selectEntity(tempPair.second, tempPair.first);
        while (selectEntity.Next())
        {
            EntityId  entityId = selectEntity->GetId();
            if (entityId > maxID)
                maxID = entityId;
        }
    }
    // The returned value is meant to be used for creation of new entities.
    // If there is no entity in the model yet, we leave it up to model by returning 0.
    // This is important in case the entity goes into an include with offset.
    if(maxID > 0) return ++maxID;
    else return 0;
}


void sdiD2R::ConvertUtils::FindRadElement(const sdiValueEntity& entity, sdiString& keyWord, HandleElementRead& elemHandleRead) const
{
    EntityId elemId = entity.GetId();
    SelectionElementRead selElem(p_radiossModel, keyWord);
    while (selElem.Next())
    {
        if (selElem->GetId() == elemId)
        {
            elemHandleRead = selElem->GetHandle();
            return;
        }
    }
    if (keyWord == "/BEAM" && !elemHandleRead.IsValid())
    {
        keyWord = "/SPRING";
        SelectionElementRead selElem(p_radiossModel, keyWord);
        while (selElem.Next())
        {
            if (selElem->GetId() == elemId)
            {
                elemHandleRead = selElem->GetHandle();
                return;
            }
        }
        if (!elemHandleRead.IsValid())
        {
            keyWord = "/TRUSS";
            SelectionElementRead selElem(p_radiossModel, keyWord);
            while (selElem.Next())
            {
                if (selElem->GetId() == elemId)
                {
                    elemHandleRead = selElem->GetHandle();
                    return;
                }
            }
        }
    }

    if (keyWord == "/SHELL" && !elemHandleRead.IsValid())
    {
        keyWord = "/SH3N";
        SelectionElementRead selElem(p_radiossModel, keyWord);
        while (selElem.Next())
        {
            if (selElem->GetId() == elemId)
            {
                elemHandleRead = selElem->GetHandle();
                return;
            }
        }
    }
}

void sdiD2R::ConvertUtils::GetCentroid(const HandleRead& radNsidHRead , sdiTriple& centroid)
{
     if (radNsidHRead.IsValid())
    {
        EntityRead setRead(p_radiossModel, radNsidHRead);
        sdiUIntList slaveNodes;
        ExtractNodesFromRadiossSet(radNsidHRead, slaveNodes);
        size_t numNodesinSet = slaveNodes.size();
        for (size_t i = 0; i < numNodesinSet; ++i)
        {
            HandleRead nodeHRead;
            p_lsdynaModel->FindById(1, slaveNodes[i], nodeHRead);
            if (nodeHRead.IsValid())
            {
                NodeRead nodeRead(p_lsdynaModel, nodeHRead);
                sdiTriple nodeLoc = nodeRead.GetPosition();
                for (int j = 0; j < 3; ++j)
                    centroid[j] += nodeLoc[j];
            }
        }
        if(numNodesinSet != 0u)
        {
            for (int j = 0; j < 3; ++j)
                centroid[j] /= numNodesinSet;
        }
    }
}
void sdiD2R::ConvertUtils::GetCentroid(const sdi::EntityRead& PartRead, sdiTriple& centroid)
{
    SelectionElementRead selPartElem(PartRead);
    sdiUIntList partNodes;
    partNodes.reserve(selPartElem.Count() * 4);
    int nodeNumber = 0;

    while (selPartElem.Next())
    {
        sdiUIntList elemNodeList;
        selPartElem->GetNodeIds(elemNodeList);
        for (unsigned int nodeId : elemNodeList)
        {
            auto itr = find(partNodes.begin(), partNodes.end(), nodeId);
            if (partNodes.end() == itr)
            {
                partNodes.push_back(nodeId);
                HandleRead nodeHRead;
                p_lsdynaModel->FindById(1, nodeId, nodeHRead);
                if (nodeHRead.IsValid())
                {
                    nodeNumber = nodeNumber + 1;
                    NodeRead nodeRead(p_lsdynaModel, nodeHRead);
                    sdiTriple nodeLoc = nodeRead.GetPosition();
                    for (int j = 0; j < 3; ++j)
                        centroid[j] += nodeLoc[j];
                }
            }
        }
    }

    if (nodeNumber != 0)
    {
        for (int j = 0; j < 3; ++j)
            centroid[j] /= nodeNumber;
    }
}

void sdiD2R::ConvertUtils::GetNodeIdsFromNodeSet(const unsigned int& setId, sdiUIntList& nodeIdList)
{
    HandleRead lsdSetHRead;
    sdiUIntList partIdList;
    p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*SET_NODE"), setId, lsdSetHRead);
    if (lsdSetHRead.IsValid())
    {
        EntityRead setRead(p_lsdynaModel, lsdSetHRead);
        int maxIds = 0;
        sdiString lsdKey;
        sdiValue tempVal(maxIds);
        setRead.GetValue(sdiIdentifier("idsmax"), tempVal);
        tempVal.GetValue(maxIds);

        tempVal = sdiValue(lsdKey);
        setRead.GetValue(sdiIdentifier("KEY"), tempVal);
        tempVal.GetValue(lsdKey);

        if (maxIds)
        {
            if (lsdKey == "PART")
            {
                partIdList.reserve(maxIds);
                sdiValueEntityList partEntityList;
                tempVal = sdiValue(partEntityList);
                setRead.GetValue(sdiIdentifier("ids"), tempVal);
                tempVal.GetValue(partEntityList);
                partEntityList.GetIdList(partIdList);
                GetNodesOfParts(partIdList, nodeIdList);
            }
            else
            {
                nodeIdList.reserve(maxIds);
                sdiValueEntityList nodeEntityList;
                tempVal = sdiValue(nodeEntityList);
                setRead.GetValue(sdiIdentifier("ids"), tempVal);
                tempVal.GetValue(nodeEntityList);
                nodeEntityList.GetIdList(nodeIdList);
            }
        }
    }
}

void sdiD2R::ConvertUtils::GetCurveDetails(const HandleRead curveHandleRead, int& numPoints, sdiDoubleList& crvPoints, vector<reference_wrapper<double>>& otherDetailsList)
{
    EntityRead crvEntRead(p_lsdynaModel, curveHandleRead);
    sdiValue tempValue(numPoints);
    crvEntRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
    tempValue.GetValue(numPoints);
    if (numPoints)
    {
        crvPoints.reserve(numPoints * 2);
        tempValue = sdiValue(crvPoints);
        crvEntRead.GetValue(sdiIdentifier("points"), tempValue);
        tempValue.GetValue(crvPoints);

        otherDetailsList[0].get() = 1.0;
        otherDetailsList[1].get() = 1.0;
        otherDetailsList[2].get() = 0.0;
        otherDetailsList[3].get() = 0.0;

        vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
        GetAttribValues(crvEntRead, lsdQueryAttribs, otherDetailsList);

        otherDetailsList[0].get() = (otherDetailsList[0].get() == 0) ? 1.0 : otherDetailsList[0].get();
        otherDetailsList[1].get() = (otherDetailsList[1].get() == 0) ? 1.0 : otherDetailsList[1].get();
    }
}

void sdiD2R::ConvertUtils::GetNodesOfParts(const sdiUIntList &partList, sdiUIntList& nodeList)
{
    EntityType partType = p_radiossModel->GetEntityType("/PART");
    sdiUIntList elemNodeList;
    for(unsigned int partId : partList)
    {
        HandleRead partHread;
        if(p_radiossModel->FindById(partType, partId, partHread))
        {
            EntityRead partEdit(p_radiossModel, partHread);
            SelectionElementRead selPartElem(partEdit);
            while(selPartElem.Next())
            {
                elemNodeList.resize(0);
                selPartElem->GetNodeIds(elemNodeList);
                for(unsigned int nodeId : elemNodeList)
                {
                    if(0 != nodeId) nodeList.push_back(nodeId);
                }
            }
        }
    }
    sdiVectorSort(nodeList);
    sdiVectorUnique(nodeList);
}

void sdiD2R::ConvertUtils::GetNodesOfParts(const unsigned int &partId, sdiUIntList& nodeList)
{
    EntityType partType = p_radiossModel->GetEntityType("/PART");
    sdiUIntList elemNodeList;
    HandleRead partHread;
    if(p_radiossModel->FindById(partType, partId, partHread))
    {
        EntityRead partEdit(p_radiossModel, partHread);
        SelectionElementRead selPartElem(partEdit);
        while(selPartElem.Next())
        {
            elemNodeList.resize(0);
            selPartElem->GetNodeIds(elemNodeList);
            for(unsigned int nodeId : elemNodeList)
            {
                if(0 != nodeId) nodeList.push_back(nodeId);
            }
        }
    }
    sdiVectorSort(nodeList);
    sdiVectorUnique(nodeList);
}

void sdiD2R::ConvertUtils::ExtractNodesFromRadiossSet(const HandleRead& setHread, sdiUIntList& nodeList)
{
    EntityRead setread(p_radiossModel, setHread);
    int clausesMax = 0;
    sdiValue tempVal(clausesMax);
    setread.GetValue(sdiIdentifier("clausesmax"), tempVal);
    tempVal.GetValue(clausesMax);
    nodeList.clear();
    for (int i = 0; i < clausesMax; ++i)
    {
        sdiString keyType;
        tempVal = sdiValue(keyType);
        setread.GetValue(sdiIdentifier("KEY_type",0, i), tempVal);
        tempVal.GetValue(keyType);
        int idsMax = 0;
        tempVal = sdiValue(idsMax);
        setread.GetValue(sdiIdentifier("idsmax",0, i), tempVal);
        tempVal.GetValue(idsMax);

        int optD = 0;
        tempVal = sdiValue(optD);
        setread.GetValue(sdiIdentifier("opt_D",0, i), tempVal);
        tempVal.GetValue(optD);

        int optG = 0;
        tempVal = sdiValue(optG);
        setread.GetValue(sdiIdentifier("opt_G",0, i), tempVal);
        tempVal.GetValue(optG);

        sdiUIntList entityList;
        for (int j = 0; j < idsMax; ++j)
        {
            sdiValueEntity entity;
            tempVal = sdiValue(entity);
            setread.GetValue(sdiIdentifier("ids", 0, i, j), tempVal);
            tempVal.GetValue(entity);
            entityList.push_back(entity.GetId());
        }

        if (keyType == "PART")
        {
            if(optD)
            {
                sdiUIntList nodeListD;
                GetNodesOfParts(entityList, nodeListD);
                IdListRemove(nodeList, nodeListD);
            }
            else
                GetNodesOfParts(entityList, nodeList);
        }
        else if (keyType == "NODE")
        {
            if(optD)
            {
                sdiUIntList nodeListD;
                for(unsigned int nodeId : entityList)
                {
                    nodeListD.push_back(nodeId);
                }
                sdiVectorSort(nodeListD);
                IdListRemove(nodeList, nodeListD);
            }
            if(optG)
            {
                int setGenStart;
                tempVal = sdiValue(setGenStart);
                setread.GetValue(sdiIdentifier("start",0, i, 0), tempVal);
                tempVal.GetValue(setGenStart);

                int setGenEnd;
                tempVal = sdiValue(setGenEnd);
                setread.GetValue(sdiIdentifier("end",0, i, 0), tempVal);
                tempVal.GetValue(setGenEnd);

                int setGenBy = 0;
                tempVal = sdiValue(setGenBy);
                setread.GetValue(sdiIdentifier("by",0, i), tempVal);
                tempVal.GetValue(setGenBy);

                for(unsigned int nodeId = setGenStart ; nodeId <= unsigned(setGenEnd) ; nodeId = nodeId + setGenBy)
                {
                    nodeList.push_back(nodeId);
                }
                sdiVectorSort(nodeList);
                sdiVectorUnique(nodeList);
            }
            else
            {
                for(unsigned int nodeId : entityList)
                {
                    nodeList.push_back(nodeId);
                }
                sdiVectorSort(nodeList);
                sdiVectorUnique(nodeList);
            }
        }
        else if (keyType == "ALL")
        {
            SelectionNodeRead selNode(p_radiossModel, "/NODE");
            while (selNode.Next())
            {
                nodeList.push_back(selNode->GetId());
            }
            sdiVectorSort(nodeList);
            sdiVectorUnique(nodeList); // normally not necessary, just in case of duplicate ids
        }
    }
}


void sdiD2R::ConvertUtils::ExtractDPartsFromSetWithClauseALL(const HandleRead& setHread, sdiUIntList& partList)
{
    EntityRead setread(p_radiossModel, setHread);
    int clausesMax = 0;
    sdiValue tempVal(clausesMax);
    setread.GetValue(sdiIdentifier("clausesmax"), tempVal);
    tempVal.GetValue(clausesMax);
    partList.clear();
    for (int i = 0; i < clausesMax; ++i)
    {
        sdiString keyType;
        tempVal = sdiValue(keyType);
        setread.GetValue(sdiIdentifier("KEY_type", 0, i), tempVal);
        tempVal.GetValue(keyType);

        if (keyType == "PART")
        {
            int optD = 0;
            tempVal = sdiValue(optD);
            setread.GetValue(sdiIdentifier("opt_D", 0, i), tempVal);
            tempVal.GetValue(optD);
            if (optD)
            {
                int idsMax = 0;
                tempVal = sdiValue(idsMax);
                setread.GetValue(sdiIdentifier("idsmax", 0, i), tempVal);
                tempVal.GetValue(idsMax);
                for (int j = 0; j < idsMax; ++j)
                {
                    sdiValueEntity entity;
                    tempVal = sdiValue(entity);
                    setread.GetValue(sdiIdentifier("ids", 0, i, j), tempVal);
                    tempVal.GetValue(entity);
                    partList.push_back(entity.GetId());
                }
            }
        }
    }
}

void sdiD2R::ConvertUtils::PropagateInclude(const sdi::EntityRead& entitySource)
{
    unsigned int currentIncludeId = entitySource.GetInclude().GetId(p_lsdynaModel);
    HandleRead currentInclude;
    p_radiossModel->FindById(p_radiossIncludeType, currentIncludeId, currentInclude);
    p_radiossModel->SetCurrentCollector(currentInclude);
}

void sdiD2R::ConvertUtils::PropagateInclude(const sdi::HandleRead& handleSource)
{
    if(handleSource.IsValid())
    {
        EntityRead entitySource(p_lsdynaModel, handleSource);
        PropagateInclude(entitySource);
    }
}

void sdiD2R::ConvertUtils::InversConnectivityNode2Elem(sdiString& elemType, int elemnNbNode, int *knod2elem, int *nod2elem, int *elementNodes)
{
/////////////////////////////////////////////////////////////////////////////////////
// Elem loop to get inverse connectivity of elements 
///////////////////////////////////////////////////////////////////////////////////// 
        SelectionNodeRead nodes(p_radiossModel, "/NODE");
        SelectionElementRead elems(p_radiossModel, elemType);

        sdiVector<unsigned int> aNodeId;
        int numnod = 0;
        numnod = nodes.Count();
        int nbElems = 0;
        nbElems = elems.Count();

        for(int k=0; k<=elemnNbNode*nbElems; k=k+1)
        {  
            nod2elem[k] = 0;
        }

        for(int k=0; k<=numnod; k=k+1)
        {  
            knod2elem[k] = 0;
        }

        map<int, int> nodeIndexes;
        map<int, int> elementIndexes;

        map<int, int> nodeIds;
        map<int, int> elementIds;

// Node Id Map
        int cpt = 0;
        int n = 0;
        while (nodes.Next())
        {  
            nodeIndexes.insert(pair<int, int>(nodes->GetId(),cpt));
            nodeIds.insert(pair<int, int>(cpt,nodes->GetId()));
            cpt++;
        }

// Elem Id Map
        cpt = 0;
        SelectionElementRead elems1(p_radiossModel, elemType);
        while(elems1.Next())
        {  
            elementIndexes.insert(pair<int, int>(elems1->GetId(),cpt));
            elementIds.insert(pair<int, int>(cpt,elems1->GetId()));
            cpt++;
        }
// Reversed Connectivity Creation
        for(int k=0; k<elemnNbNode; k=k+1)
        {  
        SelectionElementRead elems2(p_radiossModel, elemType);
            while(elems2.Next())
            {  
                elems2->GetNodeIds(aNodeId);   
                n = nodeIndexes[aNodeId[k]];
                knod2elem[n] = knod2elem[n] + 1;
                aNodeId.resize(0);  
            }
        }
//
        for(int k=0; k<numnod; k=k+1)
        {  
             knod2elem[k+1] = knod2elem[k+1] + knod2elem[k];
        }
//
        for(int k=numnod-1; k>=0; k=k-1)
        {  
             knod2elem[k+1] = knod2elem[k];
        }
        knod2elem[0]=0; 
//
        for(int k=0; k<elemnNbNode; k=k+1)
        {  
        SelectionElementRead elems3(p_radiossModel, elemType);
            cpt = 0;
            while(elems3.Next())
            {  
                elems3->GetNodeIds(aNodeId);   
                n = nodeIndexes[aNodeId[k]];
                nod2elem[knod2elem[n]] = cpt;
                knod2elem[n] = knod2elem[n] + 1;
                cpt++;
                aNodeId.resize(0);  
            }
        }
//
        for(int k=numnod-1; k>=0; k=k-1)
        {  
             knod2elem[k+1] = knod2elem[k];
        }
        knod2elem[0]=0;
}

void sdiD2R::LogParameterizedValue(const sdi::EntityBaseRead &entity, const sdiString& dataname,
    unsigned int row, unsigned int column,
    const char* filename, int line)
{
    bool isParameterized = entity.IsParameterized(sdiIdentifier(dataname, 0, row, column));
    if(isParameterized && 0 < line)
    {
        std::stringstream buf;
        buf << entity.GetKeyword().c_str() << " " << entity.GetId() << ": " << dataname.c_str();
        const char* slash = strrchr(filename, '/');
        if(nullptr == slash) slash = strrchr(filename, '\\');
        if(nullptr != slash) filename = slash + 1;
        buf << "   (" << filename << ", line " << line << ")";
        DynaToRad::GetParameterizedValues().push_back(buf.str());
    }
}

void sdiD2R::ConvertUtils::GetPartIdsFromPartSet(const sdiString& setType, const unsigned int& setId, sdiUIntList& partIdList)
{

    HandleRead lsdSetHRead;
    p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType(setType), setId, lsdSetHRead);

    if (lsdSetHRead.IsValid())
    {
        EntityRead setRead(p_lsdynaModel, lsdSetHRead);
        int maxIds = 0;
        sdiString lsdKey;
        sdiValue tempVal(maxIds);
        setRead.GetValue(sdiIdentifier("idsmax"), tempVal);
        tempVal.GetValue(maxIds);

        if (maxIds)
        {
                partIdList.reserve(maxIds);
                sdiValueEntityList partEntityList;
                tempVal = sdiValue(partEntityList);
                setRead.GetValue(sdiIdentifier("ids"), tempVal);
                tempVal.GetValue(partEntityList);
                partEntityList.GetIdList(partIdList);
        }
    }
}

void sdiD2R::ConvertUtils::GetPartIdsFromSetGeneral(const sdiString& setType, const unsigned int& setId, sdiUIntList& partIdList)
{

    HandleRead lsdSetHRead;
    p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType(setType), setId, lsdSetHRead);

    if (lsdSetHRead.IsValid())
    {

        int numClauses = 0;
        sdiValue tempVal(numClauses);
        lsdSetHRead.GetValue(p_lsdynaModel,sdiIdentifier("clausesmax"), tempVal);
        tempVal.GetValue(numClauses);
        partIdList.reserve(numClauses*7);

        for (int i = 0; i < numClauses; ++i)
        {

            EntityRead setRead(p_lsdynaModel, lsdSetHRead);
            int maxIds = 0;
            sdiString lsdKey;
            sdiValue tempVal(maxIds);
            setRead.GetValue(sdiIdentifier("idsmax",0,i), tempVal);
            tempVal.GetValue(maxIds);

            if (maxIds)
            {
                for (int j = 0; j < maxIds; ++j)
                {
                    sdiValueEntity entity;
                    setRead.GetValue(sdiIdentifier("ids", 0, i, j), tempVal);
                    tempVal.GetValue(entity);
                    partIdList.push_back(entity.GetId());
                }
            }
        }
        sdiVectorUnique(partIdList);
    }
}

/*  automatic conversion 1 Dyna option -> 1 Radioss Option */
void sdiD2R::ConvertUtils::Convert1To1(sdi::SelectionRead& selection, sdiString& keyWord)
{
    convertRuleMap conversionMap = DynaToRad::GetConversionMap();

    sdiVector<sdiIdentifier> aIdentifier;
    HandleRead optionReadHandle = selection->GetHandle();
    if (optionReadHandle.IsValid())
    {
        EntityRead dynaOptionEntityRead(p_lsdynaModel, optionReadHandle);
        dynaOptionEntityRead.GetAttributes(aIdentifier);

        //sdiString radiossOption = "/" + conversionMap.getMapValue(keyWord,sdiString("OPTION"));
        if(conversionMap.hasRule(keyWord))
        {
            convertRule myRule = conversionMap.getRule(keyWord);
            sdiString radiossOption = myRule.getTargetKeyword();   
            HandleEdit radOptionHandleEdit;
            p_radiossModel->CreateEntity(radOptionHandleEdit, radiossOption,selection->GetName(),selection->GetId());

            if (radOptionHandleEdit.IsValid())
            {
                EntityEdit radOptionEntityEdit(p_radiossModel, radOptionHandleEdit);
                for (int i = 0; i < aIdentifier.size(); ++i)
                {
                    sdiValue tempVal;
                    if(myRule.hasTargetAttribute(aIdentifier[i].GetNameKey()))
                    {
                        std::string key = myRule.getTargetAttribute(aIdentifier[i].GetNameKey());
                        dynaOptionEntityRead.GetValue(aIdentifier[i], tempVal);
                        radOptionEntityEdit.SetValue(sdiIdentifier(key), tempVal);
                        //printf("1to1   aIdentifier[i] %s %s \n",aIdentifier[i].GetNameKey().c_str(),key.c_str());
                    }
                    else
                    {
                        dynaOptionEntityRead.GetValue(aIdentifier[i], tempVal);
                        radOptionEntityEdit.SetValue(aIdentifier[i], tempVal);
                        //printf("aIdentifier[i] %s \n",aIdentifier[i].GetNameKey().c_str());
                    }
                }
            }
        }
        else
        {
            sdiString radiossOption = selection->GetKeyword();
            radiossOption[0] = '/';
            
            HandleEdit radOptionHandleEdit;
            p_radiossModel->CreateEntity(radOptionHandleEdit, radiossOption,selection->GetName(),selection->GetId());

            if (radOptionHandleEdit.IsValid())
            {
                EntityEdit radOptionEntityEdit(p_radiossModel, radOptionHandleEdit);
                for (int i = 0; i < aIdentifier.size(); ++i)
                {
                    sdiValue tempVal;
                    dynaOptionEntityRead.GetValue(aIdentifier[i], tempVal);
                    radOptionEntityEdit.SetValue(aIdentifier[i], tempVal);
                        //printf("aIdentifier[i] %s \n",aIdentifier[i].GetNameKey().c_str());
                }
            }

        }
    }
    return;
}


/*  automatic conversion 1 Dyna option -> 1 Radioss Option */
void sdiD2R::ConvertUtils::Convert1To1(const sdi::EntityRead& optionEntity, sdiString& OptionKey , unsigned int& OptionNumber, unsigned int& newOptionId)
{
    const convertRuleMap &conversionMap = DynaToRad::GetConversionMap();

    sdiString dynaOption;
    if(OptionNumber <10){
         dynaOption = OptionKey + "_00" + to_string(OptionNumber);
    }
    else if(OptionNumber <100){
         dynaOption = OptionKey + "_0" + to_string(OptionNumber);
    }
    else{
         dynaOption = OptionKey + "_" + to_string(OptionNumber);
    }

    //sdiString keyWord = "/" + conversionMap.getMapValue(dynaOption,sdiString("OPTION"));

    if(conversionMap.hasRule(dynaOption))
    {
        convertRule myRuleKey = conversionMap.getRule(OptionKey);
        convertRule myRuleOption = conversionMap.getRule(dynaOption);

        sdiString keyWord = '/' + myRuleOption.getTargetKeyword();  

        sdiVector<sdiIdentifier> aIdentifier;
        HandleRead optionReadHandle = optionEntity.GetHandle();
        if (optionReadHandle.IsValid())
        {
            EntityRead dynaOptionEntityRead(p_lsdynaModel, optionReadHandle);
            dynaOptionEntityRead.GetAttributes(aIdentifier);

            HandleEdit radOptionHandleEdit;
            p_radiossModel->CreateEntity(radOptionHandleEdit, keyWord,optionEntity.GetName(),newOptionId);

            if (radOptionHandleEdit.IsValid())
            {
                EntityEdit radOptionEntityEdit(p_radiossModel, radOptionHandleEdit);
                for (int i = 0; i < aIdentifier.size(); ++i)
                {
                    sdiValue tempVal;
                    if(myRuleKey.hasTargetAttribute(aIdentifier[i].GetNameKey()))
                    {
                        std::string key = myRuleKey.getTargetAttribute(aIdentifier[i].GetNameKey());
                        dynaOptionEntityRead.GetValue(aIdentifier[i], tempVal);
                        radOptionEntityEdit.SetValue(sdiIdentifier(key), tempVal);
                        //printf("1to1 gene  aIdentifier[i] %s %s \n",aIdentifier[i].GetNameKey().c_str(),key.c_str());
                    }
                    else if(myRuleOption.hasTargetAttribute(aIdentifier[i].GetNameKey()))
                    {
                        std::string key = myRuleOption.getTargetAttribute(aIdentifier[i].GetNameKey());
                    
                        dynaOptionEntityRead.GetValue(aIdentifier[i], tempVal);
                        radOptionEntityEdit.SetValue(sdiIdentifier(key), tempVal);
                        //printf("1to1 option  aIdentifier[i] %s %s \n",aIdentifier[i].GetNameKey().c_str(),key.c_str());
                    }
                    else
                    {
                        dynaOptionEntityRead.GetValue(aIdentifier[i], tempVal);
                        radOptionEntityEdit.SetValue(aIdentifier[i], tempVal);
                        //printf("aIdentifier[i] %s \n",aIdentifier[i].GetNameKey().c_str());
                    }
                }
            }
        
            if (radOptionHandleEdit.IsValid())
            {
                sdiConvert::SDIHandlReadList sourcemat = { {optionEntity.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(radOptionHandleEdit, sourcemat));
                
            }
        }
    }
    else
    {
        sdiString radiossOption = "/MAT/LAW" + OptionNumber;
        
        HandleEdit radOptionHandleEdit;
        p_radiossModel->CreateEntity(radOptionHandleEdit, radiossOption,optionEntity.GetName(),newOptionId);

        if (radOptionHandleEdit.IsValid())
        {

            sdiVector<sdiIdentifier> aIdentifier;
            HandleRead optionReadHandle = optionEntity.GetHandle();
            EntityEdit radOptionEntityEdit(p_radiossModel, radOptionHandleEdit);
            EntityRead dynaOptionEntityRead(p_lsdynaModel, optionReadHandle);
            dynaOptionEntityRead.GetAttributes(aIdentifier);

            for (int i = 0; i < aIdentifier.size(); ++i)
            {
                sdiValue tempVal;
                dynaOptionEntityRead.GetValue(aIdentifier[i], tempVal);
                radOptionEntityEdit.SetValue(aIdentifier[i], tempVal);
                //printf("aIdentifier[i] %s \n",aIdentifier[i].GetNameKey().c_str());
            }
        }
    }
    return;
}



sdiTriple ConvertUtils::rotateVector(sdiTriple& u,sdiTriple& x,sdiTriple& y,sdiTriple& z)
{
    sdiTriple w;
    w[0] = u[0] * x[0] + u[1] * y[0] + u[2] * z[0];
    w[1] = u[0] * x[1] + u[1] * y[1] + u[2] * z[1];
    w[2] = u[0] * x[2] + u[1] * y[2] + u[2] * z[2];
    return w;
}

void sdiD2R::ConvertUtils::ShellThickPosLayerList(sdiString& propCard, int NIP, int IPOS, sdiDoubleList& ThickPosLayerList, sdiDoubleList& ThickLayerList)
{
/////////////////////////////////////////////////////////////////////////////////////
// Get through shell integration points position
///////////////////////////////////////////////////////////////////////////////////// 
    if( propCard.find("TYPE51") != propCard.npos || propCard.find("SH_SANDW") != propCard.npos ||
        propCard.find("SH_FABR") != propCard.npos)
    {
        // compozite : /PROP/TYPE51, /PROP/TYPE16, PROP/TYPE11
        // NIP -> layers
        double lsdThickShell = 0.;
        for (int j = 0; j < NIP; ++j)
        {
            lsdThickShell = lsdThickShell + ThickLayerList[j];
        }
        double radZSHIFT = 0.0;
        if (IPOS == 0)
            radZSHIFT = -0.5;
        //else if(radIPOS == 1)  // not yet supported
            // user defined position 
        //else if(radIPOS == 2)  // not yet supported
            //radZSHIFT =  -radZSHIFT /MAX(THICKT,EM20)
        else if(IPOS == 3)
            radZSHIFT = -1.0;
        else if(IPOS == 4)
            radZSHIFT = 0.0;
                                                    
        sdiDoubleList ThickRatioLayerList;
        ThickRatioLayerList.reserve(NIP);
        for (int j = 0; j < NIP; ++j)
        {
            ThickRatioLayerList.push_back(ThickLayerList[j]/lsdThickShell);
        }

        // compute automaticaly the integration point position 
        double thickpos; 
        for (int j = 0; j < NIP; ++j)
        {                                              
            if(j==0)
                thickpos = radZSHIFT + 0.5*ThickRatioLayerList[0];
            else
                thickpos = ThickPosLayerList[j-1]+ 0.5*(ThickRatioLayerList[j]+ThickRatioLayerList[j-1]);

            ThickPosLayerList.push_back(thickpos);
        }
    }
    else
    {
        // other shell props types: /PROP/TYPE1, /PROP/TYPE9
        // NIP -> integration points through shell thickness

        //---
        // Radioss Lobatto scheme
        //---

        //Number of columns
	    int num_col = 9;

	    // Number of rows
        int num_row = 9;

        // Initializing a single row
        vector<double> row(num_col, 0);

        // Initializing the 2-D vector
	    vector<vector<double>> Zpos(num_row, row) ;

        Zpos = {
        {0.        ,0.        ,0.        ,0.        ,0.        ,
         0.        ,0.        ,0.        ,0.        ,0.        },
        {.5        ,0.5       ,0.        ,0.        ,0.        ,
         0.        ,0.        ,0.        ,0.        ,0.        },
        {-.5       ,0.        ,0.5       ,0.        ,0.        ,
         0.        ,0.        ,0.        ,0.        ,0.        },
        {-.5       ,-.1666667 ,0.1666667 ,0.5       ,0.        ,
         0.        ,0.        ,0.        ,0.        ,0.        },
        {-.5       ,-.25      ,0.        ,0.25      ,0.5       ,
         0.        ,0.        ,0.        ,0.        ,0.        },
        {-.5       ,-.3       ,-.1       ,0.1       ,0.3       ,
         0.5       ,0.        ,0.        ,0.        ,0.        },
        {.5        ,-.3333333 ,-.1666667 ,0.0       ,0.1666667 ,
         0.3333333 ,0.5       ,0.        ,0.        ,0.        },
        {-.5       ,-.3571429 ,-.2142857 ,-.0714286 ,0.0714286 ,
         0.2142857 ,0.3571429 ,0.5       ,0.        ,0.        },
        {-.5       ,-.375     ,-.25      ,-.125     ,0.0       ,
         0.125     ,0.25      ,0.375     ,0.5       ,0.        },
        {-.5       ,-.3888889 ,-.2777778 ,-.1666667 ,-.0555555 ,
         0.0555555 ,0.1666667 ,0.2777778 ,0.3888889 ,0.5       } };

        // Line i=NIP;
        for (int j = 0; j < NIP; ++j)
        {
            ThickPosLayerList.push_back(Zpos[NIP-1][j]);
        }
    } // if( partCard.find("COMPOSITE") != string::npos)
}

void sdiD2R::ConvertUtils::ExtractShellsFromSet(const HandleRead& setHread, sdiUIntList& shellList)
{
    EntityRead setread(p_radiossModel, setHread);
    int clausesMax = 0;
    sdiValue tempVal(clausesMax);
    setread.GetValue(sdiIdentifier("clausesmax"), tempVal);
    tempVal.GetValue(clausesMax);
    shellList.clear();
    for (int i = 0; i < clausesMax; ++i)
    {
        sdiString keyType;
        tempVal = sdiValue(keyType);
        setread.GetValue(sdiIdentifier("KEY_type", 0, i), tempVal);
        tempVal.GetValue(keyType);

        if (keyType == "SHELL")
        {
            int idsMax = 0;
            tempVal = sdiValue(idsMax);
            setread.GetValue(sdiIdentifier("idsmax", 0, i), tempVal);
            tempVal.GetValue(idsMax);
            for (int j = 0; j < idsMax; ++j)
            {
                sdiValueEntity entity;
                tempVal = sdiValue(entity);
                setread.GetValue(sdiIdentifier("ids", 0, i, j), tempVal);
                tempVal.GetValue(entity);
                shellList.push_back(entity.GetId());
            }
        }
    }
}

void sdiD2R::ConvertUtils::ExtractSh3nsFromSet(const HandleRead& setHread, sdiUIntList& sh3nList)
{
    EntityRead setread(p_radiossModel, setHread);
    int clausesMax = 0;
    sdiValue tempVal(clausesMax);
    setread.GetValue(sdiIdentifier("clausesmax"), tempVal);
    tempVal.GetValue(clausesMax);
    sh3nList.clear();
    for (int i = 0; i < clausesMax; ++i)
    {
        sdiString keyType;
        tempVal = sdiValue(keyType);
        setread.GetValue(sdiIdentifier("KEY_type", 0, i), tempVal);
        tempVal.GetValue(keyType);

        if (keyType == "SH3N")
        {
            int idsMax = 0;
            tempVal = sdiValue(idsMax);
            setread.GetValue(sdiIdentifier("idsmax", 0, i), tempVal);
            tempVal.GetValue(idsMax);
            for (int j = 0; j < idsMax; ++j)
            {
                sdiValueEntity entity;
                tempVal = sdiValue(entity);
                setread.GetValue(sdiIdentifier("ids", 0, i, j), tempVal);
                tempVal.GetValue(entity);
                sh3nList.push_back(entity.GetId());
            }
        }
    }
}