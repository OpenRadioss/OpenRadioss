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

#include <cfloat>
#include <dyna2rad/dyna2rad.h>
#include <dyna2rad/convertelements.h>

using namespace sdiD2R;
using namespace std;
using namespace sdi;


void sdiD2R::ConvertElem::ConvertElems()
{
    ConvertEntities();

    ConvertSeatbeltAccelerometer();

    p_CreateThAccel();

    ConvertSeatbeltSlipring();

    ConvertSeatbeltSensor();

    ConvertSeatbeltRetractor();

    p_CreateThSlipring();
}

void ConvertElem::ConvertEntities()
{
    sdiValueEntityType nodeEntityType(p_radiossModel->GetEntityType("/NODE"));
    sdiValueEntityType partEntityType(p_radiossModel->GetEntityType("/PART"));
    HandleEdit partPlotelHEdit;
    HandleEdit propPlotelHEdit;

    map<sdiString, sdiString> elemConvertMap({{ 
        {"*ELEMENT_SHELL" ,"/SHELL"},
        {"*ELEMENT_DISCRETE", "/SPRING"},
        {"*ELEMENT_BEAM", "/BEAM"},
        {"*ELEMENT_SOLID", "/BRICK"},
        {"*ELEMENT_SEATBELT", "/SPRING"},
        {"*ELEMENT_PLOTEL", "/SPRING"},
        {"*ELEMENT_SPH", "/SPHCEL"},
        {"*ELEMENT_TSHELL", "/BRICK"}}});

    for (auto elemKeysPair : elemConvertMap)
    {
        sdiString srcElem = elemKeysPair.first;
        SelectionElementRead elemSelect(p_lsdynaModel, srcElem);
        while (elemSelect.Next())
        {
            sdiString destElem = elemKeysPair.second;
            unsigned int pid = elemSelect->GetComponentId();
            HandleRead partHRead;
            if (p_lsdynaModel->FindById("*PART", pid, partHRead));
            unsigned int nodeCount = elemSelect->GetNodeCount();
            sdiUIntList elemNodes;
            HandleElementEdit radElem;
            elemSelect->GetNodeIds(elemNodes);
            unsigned int elemId = elemSelect->GetId();
            const sdiString& elemKeyWord = elemSelect->GetKeyword();

            if (elemId > 0)
            {
                if (elemKeyWord == "*ELEMENT_SEATBELT")
                    if (4 == nodeCount && (elemNodes[2] != 0) && (elemNodes[3] != 0) )
                    {
                        destElem = "/SHELL";
                        nodeCount = 4;
                    }
                    else
                    {
                        elemNodes.resize(2);
                    }
                else if (srcElem == "*ELEMENT_SHELL")
                {
                    if (4 == nodeCount && (elemNodes[3] == elemNodes[2]))
                    {
                        /* create /SH3N */
                        elemNodes.pop_back();
                        destElem = "/SH3N";
                        nodeCount = 3;
                    }
                    else
                        nodeCount = 4;
                }
                else if (srcElem == "*ELEMENT_SOLID")
                {
                    if (4 == nodeCount ||
                        (8 <= nodeCount &&
                         elemNodes[4] == elemNodes[3] &&
                         elemNodes[5] == elemNodes[3] &&
                         elemNodes[6] == elemNodes[3] &&
                         elemNodes[7] == elemNodes[3]))
                    {
                        // /TETRA4 or /BRICK with 4 zero nodes
                        elemNodes.resize(8);
                        destElem = "/TETRA4";
                        unsigned int temp = elemNodes[2];
                        elemNodes[2] = elemNodes[3];
                        elemNodes[3] = temp;
                        for (size_t i = 4; i < 8 ;++ i)
                            elemNodes[i] = 0;
                    }
                    else if(10 > nodeCount)
                        elemNodes.resize(8); // /BRICK
                    else if (elemNodes[8] != 0 &&
                             elemNodes[9] != 0 &&
                             (10 == nodeCount || elemNodes[10] == 0))
                    {
                        elemNodes.resize(10);
                        destElem = "/TETRA10";
                        unsigned int tempN8 = elemNodes[7];
                        unsigned int tempN9 = elemNodes[8];
                        unsigned int tempN10 = elemNodes[9];
                        elemNodes[8] = tempN8;
                        elemNodes[9] = tempN9;
                        elemNodes[7] = tempN10;
                    }
                    else if (20 == nodeCount && elemNodes[19] != 0)
                        destElem = "/BRIC20";
                    else
                        elemNodes.resize(8); 
                }
                else if (srcElem == "*ELEMENT_TSHELL")
                {
                    elemNodes.resize(8); // thick shell element to /BRICK
                }
                else if (srcElem == "*ELEMENT_BEAM")
                {
                    HandleRead compReadHandle;
                    HandleRead matHRead;

                    unsigned int myPID = elemSelect->GetComponentId();
                    p_lsdynaModel->FindById("*PART", myPID, compReadHandle);
                    //elemSelect->GetEntityHandle(sdiIdentifier("PID"), compReadHandle);

                    compReadHandle.GetEntityHandle(p_lsdynaModel, sdiIdentifier("MID"), matHRead);
                    if (matHRead.IsValid())
                    {
                        EntityRead matRead(p_lsdynaModel, matHRead);

                        HandleRead propHRead;
                        compReadHandle.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHRead);
                        int elform = 0;
                        sdiValue tempVal(elform);
                        propHRead.GetValue(p_lsdynaModel, sdiIdentifier("ELFORM"), tempVal);
                        tempVal.GetValue(elform);

                        if (( matRead.GetKeyword().find("*MAT_NULL") != string::npos || matRead.GetKeyword().find("*MAT_009") != string::npos ) && elform == 6)
                        {
                            destElem = "/SPRING";
                            elemNodes.erase( elemNodes.begin() + 2, elemNodes.begin() + 3 );
                        }
                        else if (( matRead.GetKeyword().find("*MAT_NULL") != string::npos || matRead.GetKeyword().find("*MAT_009") != string::npos ) 
                                   && elform != 0 && elform != 1 && elform != 2)
                        {
                            destElem = "/TRUSS";
                            elemNodes.erase( elemNodes.begin() + 2, elemNodes.begin() + 3 );
                        }
                        else if (matRead.GetKeyword().find("*MAT_SPOTWELD") != string::npos || matRead.GetKeyword().find("*MAT_100") != string::npos)
                            destElem = "/SPRING";
                        else if (matRead.GetKeyword().find("*MAT_MUSCLE") != string::npos || matRead.GetKeyword().find("*MAT_156") != string::npos)
                        {
                            destElem = "/SPRING";
                        }
                        else
                        {
                            switch (elform)
                            {
                            case 3:
                                destElem = "/TRUSS";
                                elemNodes.erase( elemNodes.begin() + 2, elemNodes.begin() + 3 );
                                break;
                            case 6:
                                destElem = "/SPRING";
                                elemNodes.erase( elemNodes.begin() + 2, elemNodes.begin() + 3 );
                                break;
                            default:
                                break;
                            }
                        }
                        if (destElem != "/TRUSS")
                        {
                            /*to move to new function*/
                            if (elemKeyWord.find("ORIENT") != elemKeyWord.npos)
                            {
                                sdiTriple orientVect(0.0, 0.0, 0.0);
                                sdiStringList orientVectAttrNames({ "VX" , "VY", "VZ" });
                                for (size_t i = 0; i < 3; ++i)
                                {
                                    sdiValue tempVal(orientVect[i]);
                                    elemSelect->GetValue(sdiIdentifier(orientVectAttrNames[i]), tempVal);
                                    tempVal.GetValue(orientVect[i]);
                                    if(orientVect[i] == DBL_MAX) orientVect[i] = 0. ;
                                }
                                if (orientVect.Len() != 0.0)
                                {
                                    HandleRead node1HRead;
                                    if (p_radiossModel->FindById(1, elemNodes[0], node1HRead))
                                    {
                                        NodeRead node1Read(p_radiossModel, node1HRead);
                                        sdiTriple n1Pos = node1Read.GetPosition();
                                        sdiTriple n3Pos = n1Pos + orientVect;
                                        HandleNodeEdit n3NodeHEdit;
                                        p_radiossModel->CreateNode(n3NodeHEdit, "/NODE", n3Pos);
                                        if (n3NodeHEdit.IsValid())
                                        {
                                            elemNodes[2] = n3NodeHEdit.GetId(p_radiossModel);
                                            elemNodes.resize(3);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                else if (srcElem == "*ELEMENT_PLOTEL")
                {
                    HandleEdit partHEdit;
                    p_lsdynaModel->FindById("*PART", 10000000, partHEdit);

                    if (!partHEdit.IsValid() && !partPlotelHEdit.IsValid())
                    {
                        p_radiossModel->CreateEntity(partPlotelHEdit, "/PART", "PLOTEL", 10000000);
                    }
                    else if (!partPlotelHEdit.IsValid())
                    {
                        p_radiossModel->CreateEntity(partPlotelHEdit, "/PART", "PLOTEL");
                    }
                    pid = partPlotelHEdit.GetId(p_radiossModel);
                    partHRead = partPlotelHEdit;

                    EntityEdit partPlotelEdit(p_radiossModel, partPlotelHEdit);

                    HandleEdit propHEdit;
                    p_lsdynaModel->FindById("*SECTION", 10000000, propHEdit);
                    unsigned int propid;
                    

                    if (!propHEdit.IsValid() && !propPlotelHEdit.IsValid())
                    {
                        p_radiossModel->CreateEntity(propPlotelHEdit, "/PROP/TYPE4", "PLOTEL", 10000000);
                        propid = propPlotelHEdit.GetId(p_radiossModel);
                        partPlotelEdit.SetValue(sdiIdentifier("prop_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/PROP"), propid)));
                        EntityEdit propEdit(p_radiossModel, propPlotelHEdit);
                        propEdit.SetValue(sdiIdentifier("MASS"), sdiValue(1.1*pow(10.0,-15)));
                    }
                    else if (!propPlotelHEdit.IsValid())
                    {
                        p_radiossModel->CreateEntity(propPlotelHEdit, "/PROP/TYPE4", "PLOTEL");
                        propid = propPlotelHEdit.GetId(p_radiossModel);
                        partPlotelEdit.SetValue(sdiIdentifier("prop_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/PROP"), propid)));
                        EntityEdit propEdit(p_radiossModel, propPlotelHEdit);
                        propEdit.SetValue(sdiIdentifier("MASS"), sdiValue(1.1*pow(10.0,-15)));
                    }

                }
                /* create RADIOSS Element */
                p_radiossModel->CreateElement(radElem,destElem,elemNodes,partHRead,elemId);

                
                if (srcElem == "*ELEMENT_SHELL")
                {
                    if(elemKeyWord.find("THICK") != elemKeyWord.npos)
                    {
                        static vector<sdiIdentifier> identifierList({ sdiIdentifier("THIC1"), sdiIdentifier("THIC2"), sdiIdentifier("THIC3"), sdiIdentifier("THIC4") });
                        double radThick = 0;
                        for (unsigned int i = 0; i < nodeCount; ++i)
                        {
                            double nodeThick = 0.0;
                            sdiValue tempVal(nodeThick);
                            elemSelect->GetValue(identifierList[i], tempVal);
                            tempVal.GetValue(nodeThick);
                            if(nodeThick != DBL_MAX)
                                radThick += nodeThick;
                        }
                        radThick = radThick /= nodeCount;
                        if (radThick != 0.0)
                            radElem.SetValue(p_radiossModel,sdiIdentifier("Thick"),sdiValue(radThick));
                    }
                    double elemBeta = 0.0;
                    sdiValue tempVal(elemBeta);
                    elemSelect->GetValue(sdiIdentifier("BETA"), tempVal);
                    tempVal.GetValue(elemBeta);
                    radElem.SetValue(p_radiossModel,sdiIdentifier("PHI_Z"),sdiValue(elemBeta));
                }
                else if (srcElem == "*ELEMENT_DISCRETE")
                {
                    p_UpdateVIDForSprings(*elemSelect, elemNodes, radElem);
                }
                else if (srcElem == "*ELEMENT_SPH")
                {
                    double elemMass = 0.0;
                    sdiValue tempVal(elemMass);
                    elemSelect->GetValue(sdiIdentifier("MASS"), tempVal);
                    tempVal.GetValue(elemMass);
                    radElem.SetValue(p_radiossModel,sdiIdentifier("MASS"),sdiValue(elemMass));
                }
            }
        }
    }
}

void ConvertElem::ConvertSeatbeltAccelerometer()
{
    sdiUIntList accelIdList;
    SelectionRead selSeatbeltAccel(p_lsdynaModel, "*ELEMENT_SEATBELT_ACCELEROMETER");
    
    accelIdList.reserve(selSeatbeltAccel.Count());
    while (selSeatbeltAccel.Next())
    {
        double lsdMASS=0.0;
        sdiUIntList nodeIdList;
        sdiDoubleList massList;
        sdiValueEntity node1, node2, node3;
        HandleEdit admasHEdit, seatbeltAccelHEdit, skewHEdit;
        sdiValue tempValue;

        selSeatbeltAccel->GetValue(sdiIdentifier("NID1"), tempValue);
        tempValue.GetValue(node1);

        selSeatbeltAccel->GetValue(sdiIdentifier("NID2"), tempValue);
        tempValue.GetValue(node2);

        selSeatbeltAccel->GetValue(sdiIdentifier("NID3"), tempValue);
        tempValue.GetValue(node3);

        tempValue = sdiValue(lsdMASS);
        selSeatbeltAccel->GetValue(sdiIdentifier("MASS"), tempValue);
        tempValue.GetValue(lsdMASS);

        sdiConvert::SDIHandlReadList sourceHandles = { {selSeatbeltAccel->GetHandle()} };

        if (node1.GetId())
        {
            p_radiossModel->CreateEntity(seatbeltAccelHEdit, "/ACCEL");
            seatbeltAccelHEdit.SetValue(p_radiossModel, sdiIdentifier("Node"), sdiValue(sdiValueEntity(sdiValueEntityType(1), node1.GetId())));

            if (node2.GetId() && node3.GetId())
            {
                p_radiossModel->CreateEntity(skewHEdit, "/SKEW/MOV", selSeatbeltAccel->GetName());
                EntityEdit skewEntityEdit(p_radiossModel, skewHEdit);
                skewEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(selSeatbeltAccel->GetName()));
                skewEntityEdit.SetValue(sdiIdentifier("N1"), sdiValue(sdiValueEntity(sdiValueEntityType(1), node1.GetId())));
                skewEntityEdit.SetValue(sdiIdentifier("N2"), sdiValue(sdiValueEntity(sdiValueEntityType(1), node2.GetId())));
                skewEntityEdit.SetValue(sdiIdentifier("N3"), sdiValue(sdiValueEntity(sdiValueEntityType(1), node3.GetId())));

                seatbeltAccelHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("Iskew"), skewHEdit);

                if (skewHEdit.IsValid())
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(skewHEdit, sourceHandles));
            }

            massList.push_back(lsdMASS);
            nodeIdList.push_back(node1.GetId());
            p_radiossModel->CreateEntity(admasHEdit, "/ADMAS");
            admasHEdit.SetValue(p_radiossModel, sdiIdentifier("entityids_type"), sdiValue(sdiString("NODE")));
            admasHEdit.SetValue(p_radiossModel, sdiIdentifier("entityidsmax"), sdiValue((int)nodeIdList.size()));
            admasHEdit.SetValue(p_radiossModel, sdiIdentifier("type"), sdiValue(5));
            admasHEdit.SetValue(p_radiossModel, sdiIdentifier("node_ID"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/NODE"), nodeIdList)));
            admasHEdit.SetValue(p_radiossModel, sdiIdentifier("MASS"), sdiValue(massList));

            accelIdList.push_back(seatbeltAccelHEdit.GetId(p_radiossModel));

            if (admasHEdit.IsValid())
                sdiConvert::Convert::PushToConversionLog(std::make_pair(admasHEdit, sourceHandles));
        }

        if (seatbeltAccelHEdit.IsValid())
            sdiConvert::Convert::PushToConversionLog(std::make_pair(seatbeltAccelHEdit, sourceHandles));
    }   
}

void sdiD2R::ConvertElem::p_CreateThAccel()
{
    sdiUIntList accelIdList;
    sdiConvert::SDIHandlReadList targetHandlesList;
    SelectionRead selAccel(p_radiossModel, "/ACCEL");

    int cptAccel = 0;
    while (selAccel.Next())
    {
        sdiConvert::SDIHandlReadList dynaSourceHandles;
        sdiConvert::Convert::GetSourceHandles(p_radiossModel, selAccel->GetHandle(), dynaSourceHandles);
        if(dynaSourceHandles.size() > 0) cptAccel = cptAccel + 1;
    }

    selAccel.Restart();
    accelIdList.reserve(cptAccel);


    while (selAccel.Next())
    {
        sdiConvert::SDIHandlReadList dynaSourceHandles;
        sdiConvert::Convert::GetSourceHandles(p_radiossModel, selAccel->GetHandle(), dynaSourceHandles);
        if(dynaSourceHandles.size() > 0)
        {
            accelIdList.push_back(selAccel->GetId());
            targetHandlesList.push_back(selAccel->GetHandle());
        }
    }

    if (!accelIdList.empty())
    {
        HandleEdit radThAccelHEdit;
        p_radiossModel->CreateEntity(radThAccelHEdit, "/TH/ACCEL", "TH-ACCEL");

        EntityEdit radThAccelEdit(p_radiossModel, radThAccelHEdit);
        radThAccelEdit.SetValue(sdiIdentifier("idsmax"), sdiValue((int)accelIdList.size()));
        radThAccelEdit.SetValue(sdiIdentifier("Number_Of_Variables"), sdiValue(1));
        radThAccelEdit.SetValue(sdiIdentifier("Obj"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/ACCEL"), accelIdList)));
        radThAccelEdit.SetValue(sdiIdentifier("var"), sdiValue(sdiStringList({ "DEF" })));

        if (radThAccelHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList radThAccelSourceHandles;
            for (int i = 0; i < targetHandlesList.size(); i=i+1)
            {
                sdiConvert::SDIHandlReadList radAccelSourceHandles;
                sdiConvert::Convert::GetSourceHandles(p_radiossModel, targetHandlesList[i], radAccelSourceHandles);
                for(size_t i = 0; i < radAccelSourceHandles.size(); ++i)
                {
                    radThAccelSourceHandles.push_back(radAccelSourceHandles[i]);
                }
            }
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radThAccelHEdit, radThAccelSourceHandles));
        }
    }
}

void sdiD2R::ConvertElem::p_UpdateVIDForSprings(const ElementRead& lsdElemRead, const sdiUIntList& elemNodes, HandleElementEdit& radElemHEdit)
{

    HandleRead vidHread;
    lsdElemRead.GetEntityHandle(sdiIdentifier("VID"), vidHread);
    unsigned int vidID = vidHread.GetId(p_lsdynaModel);
    static EntityType radSkewType = p_radiossModel->GetEntityType("/SKEW");
    if (vidID)
    {
        auto itr = DynaToRad::storeLsdSDorientIdVsSkewId.find(vidID);
        if (itr != DynaToRad::storeLsdSDorientIdVsSkewId.end())
            radElemHEdit.SetValue(p_radiossModel, sdiIdentifier("Skew_ID"), sdiValue(sdiValueEntity(radSkewType, itr->second)));
    }
    else
    {
        HandleRead node1HRead;
        if (p_radiossModel->FindById(1, elemNodes[0], node1HRead))
        {
            HandleEdit node2HEdit;
            if (p_radiossModel->FindById(1, elemNodes[1], node2HEdit))
            {
                NodeRead node1Read(p_radiossModel, node1HRead);
                NodeRead node2Read(p_radiossModel, node2HEdit);
                sdiTriple n1Pos = node1Read.GetPosition();
                sdiTriple n2Pos = node2Read.GetPosition();
                sdiTriple n3pos;
                sdiTriple vectN1N2 = n2Pos - n1Pos;
                if (vectN1N2.Len() == 0.0)
                    return;
                sdiTriple arbitrary(1.0, 2.0, 3.0);
                vectN1N2 = (n2Pos - n1Pos).Normalize();
                n3pos = n1Pos + arbitrary;
                sdiTriple vectN1N3 = (n3pos - n1Pos).Normalize();
                if ((vectN1N2 * vectN1N3).Len() == 0.0)
                {
                    n3pos = n1Pos + sdiTriple(4.0, 5.0, 6.0);
                    vectN1N3 = (n3pos - n1Pos).Normalize();
                }
                if (n3pos.Length() != 0.0)
                {
                    HandleNodeEdit node3HandleEdit;
                    p_radiossModel->CreateNode(node3HandleEdit, "/NODE", n3pos);
                    HandleEdit skewHandle;
                    p_radiossModel->CreateEntity(skewHandle, "/SKEW/MOV");
                    EntityEdit skewEntityEdit(p_radiossModel, skewHandle);
                    skewEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue("Elem_Disc_" + to_string(lsdElemRead.GetId())));
                    skewEntityEdit.SetValue(sdiIdentifier("N1"), sdiValue(sdiValueEntity(1, node1Read.GetId())));
                    skewEntityEdit.SetValue(sdiIdentifier("N2"), sdiValue(sdiValueEntity(1, node2Read.GetId())));
                    skewEntityEdit.SetValue(sdiIdentifier("N3"), sdiValue(sdiValueEntity(1, node3HandleEdit.GetId(p_radiossModel))));

                    radElemHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("Skew_ID"), skewHandle);
                    sdiConvert::SDIHandlReadList sourceList = { {lsdElemRead.GetHandle()} };
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(skewHandle, sourceList));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(node3HandleEdit, sourceList));
                }
            }
        }
    }
}
void ConvertElem::ConvertSeatbeltSlipring()
{
    sdiUIntList accelIdList;
    SelectionRead selSeatbeltSlipring(p_lsdynaModel, "*ELEMENT_SEATBELT_SLIPRING");

    EntityType radSensorType = p_radiossModel->GetEntityType("/SENSOR");

    while (selSeatbeltSlipring.Next())
    {
        HandleEdit radSlipringHEdit;
        
        sdiValueEntity lsdSBID1 = GetValue<sdiValueEntity>(*selSeatbeltSlipring, "SBID1");
        unsigned int EL_ID1 = lsdSBID1.GetId();

        sdiValueEntity lsdSBID2 = GetValue<sdiValueEntity>(*selSeatbeltSlipring, "SBID2");
        unsigned int EL_ID2 = lsdSBID2.GetId();

        if ( EL_ID1 > 0 && EL_ID2 > 0)
        {
            int lsdSBRNID_FLAG = GetValue<int>(*selSeatbeltSlipring, "SBRNID_FLAG");

            if (lsdSBRNID_FLAG == 0 )
                p_radiossModel->CreateEntity(radSlipringHEdit, "/SLIPRING/SPRING", selSeatbeltSlipring->GetName(), selSeatbeltSlipring->GetId());
            if (lsdSBRNID_FLAG == 1 )
                p_radiossModel->CreateEntity(radSlipringHEdit, "/SLIPRING/SHELL", selSeatbeltSlipring->GetName(), selSeatbeltSlipring->GetId());

            if(radSlipringHEdit.IsValid())
            {
             	EntityEdit radSlipringEdit(p_radiossModel, radSlipringHEdit);
                if (lsdSBRNID_FLAG == 0 )
                {
             	    p_ConvertUtils.CopyValue(*selSeatbeltSlipring, radSlipringEdit, "SBID1", "EL_ID1");
             	    p_ConvertUtils.CopyValue(*selSeatbeltSlipring, radSlipringEdit, "SBID2", "EL_ID2");
                }
                else if (lsdSBRNID_FLAG == 1 )
                {
             	    p_ConvertUtils.CopyValue(*selSeatbeltSlipring, radSlipringEdit, "SBID1", "EL_SET1");
             	    p_ConvertUtils.CopyValue(*selSeatbeltSlipring, radSlipringEdit, "SBID2", "EL_SET2");
                }

                int lsdFC_FLAG = GetValue<int>(*selSeatbeltSlipring, "FC_FLAG");

             	if(lsdFC_FLAG ==0)
             	{
             	    // scalar
             	    p_ConvertUtils.CopyValue(*selSeatbeltSlipring, radSlipringEdit, "FC", "Fricd");
             	}
             	else if(lsdFC_FLAG ==1)
             	{
             	    // function
             	    HandleRead FCcrvHandle;
             	    selSeatbeltSlipring->GetEntityHandle(sdiIdentifier("FC_FUNC"), FCcrvHandle);

             	    if (FCcrvHandle.IsValid())
             	    {
             		unsigned int FCcrvId = FCcrvHandle.GetId(p_lsdynaModel);
             		radSlipringEdit.SetValue(sdiIdentifier("Fct_ID1"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), FCcrvId)));
             	    }
             	}


             	if(lsdSBRNID_FLAG ==0)
             	{
             	    // "Seatbelt elements and Node for definition of the slipring"
             	    p_ConvertUtils.CopyValue(*selSeatbeltSlipring, radSlipringEdit, "SBRNID", "Node_ID");
             	}
             	else if(lsdSBRNID_FLAG ==1)
             	{
             	    // "Shell sets and Node set for definition of the slipring"
             	    p_ConvertUtils.CopyValue(*selSeatbeltSlipring, radSlipringEdit, "SBRNID", "Node_SET");
             	}

             	int slipring_sensorID;
                double lsdLTIME = GetValue<double>(*selSeatbeltSlipring, "LTIME");

           	    HandleEdit sensorTimeHedit;
          	    p_radiossModel->CreateEntity(sensorTimeHedit, "/SENSOR/TIME", selSeatbeltSlipring->GetName());
             	if (sensorTimeHedit.IsValid())
             	{
                    EntityEdit sensorTimeEntityEdit(p_radiossModel, sensorTimeHedit);
             	    sensorTimeEntityEdit.SetValue(sdiIdentifier("Tdelay"), sdiValue(lsdLTIME));
             	    //radSlipringHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("Sens_ID"), sensorTimeHedit);
             	    slipring_sensorID = sensorTimeHedit.GetId(p_radiossModel);
                    radSlipringHEdit.SetValue(p_radiossModel,sdiIdentifier("Sens_ID"), sdiValue(sdiValueEntity(sdiValueEntityType("/SENSOR"), slipring_sensorID)));
             	    sdiConvert::SDIHandlReadList sourceSlipring = { {selSeatbeltSlipring->GetHandle()} };
             	    sdiConvert::Convert::PushToConversionLog(std::make_pair(sensorTimeHedit, sourceSlipring));
             	}

                int lsdFCS_FLAG = GetValue<int>(*selSeatbeltSlipring, "FCS_FLAG");

             	if(lsdFCS_FLAG ==0)
             	{
             	    // scalar
             	    p_ConvertUtils.CopyValue(*selSeatbeltSlipring, radSlipringEdit, "FCS", "Frics");
             	}
             	else if(lsdFCS_FLAG ==1)
             	{
             	    // function
             	    HandleRead FCScrvHandle;
             	    selSeatbeltSlipring->GetEntityHandle(sdiIdentifier("FCS_FUNC"), FCScrvHandle);

             	    if (FCScrvHandle.IsValid())
             	    {
             		unsigned int FCScrvId = FCScrvHandle.GetId(p_lsdynaModel);
             		radSlipringEdit.SetValue(sdiIdentifier("Fct_ID3"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), FCScrvId)));
             	    }
             	}

             	p_ConvertUtils.CopyValue(*selSeatbeltSlipring, radSlipringEdit, "ONID", "Node_ID2");
             	p_ConvertUtils.CopyValue(*selSeatbeltSlipring, radSlipringEdit, "K", "A");

                int lsdDirect = GetValue<int>(*selSeatbeltSlipring, "DIRECT");

             	if(lsdDirect == 0)
             	    radSlipringEdit.SetValue(sdiIdentifier("Flow_flag"), sdiValue(0));
             	else if(lsdDirect == 12)
             	    radSlipringEdit.SetValue(sdiIdentifier("Flow_flag"), sdiValue(1));
             	else if(lsdDirect == 21)
             	    radSlipringEdit.SetValue(sdiIdentifier("Flow_flag"), sdiValue(2));

             	p_ConvertUtils.CopyValue(*selSeatbeltSlipring, radSlipringEdit, "DC", "Ed_factor");

             	// function "LCNFFD"
             	HandleRead LCNFFDcrvHandle;
             	selSeatbeltSlipring->GetEntityHandle(sdiIdentifier("LCNFFD"), LCNFFDcrvHandle);

             	if (LCNFFDcrvHandle.IsValid())
             	{
             	    unsigned int LCNFFDcrvId = LCNFFDcrvHandle.GetId(p_lsdynaModel);
             	    radSlipringEdit.SetValue(sdiIdentifier("Fct_ID2"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCNFFDcrvId)));
             	}

             	// function "LCNFFS"
             	HandleRead LCNFFScrvHandle;
             	selSeatbeltSlipring->GetEntityHandle(sdiIdentifier("LCNFFS"), LCNFFScrvHandle);
             	if (LCNFFScrvHandle.IsValid())
             	{
             	    unsigned int LCNFFScrvId = LCNFFScrvHandle.GetId(p_lsdynaModel);
             	    radSlipringEdit.SetValue(sdiIdentifier("Fct_ID4"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCNFFScrvId)));
             	}
//---
             	sdiConvert::SDIHandlReadList sourceHandles = { {selSeatbeltSlipring->GetHandle()} };
             	sdiConvert::Convert::PushToConversionLog(std::make_pair(radSlipringHEdit, sourceHandles));
            }
        }
    }
}

void sdiD2R::ConvertElem::p_CreateThSlipring()
{
    sdiUIntList slipringIdList;
    sdiConvert::SDIHandlReadList targetHandlesList;
    SelectionRead selSlipring(p_radiossModel, "/SLIPRING/SPRING");

    int cptSlipring = 0;
    while (selSlipring.Next())
    {
        sdiConvert::SDIHandlReadList dynaSourceHandles;
        sdiConvert::Convert::GetSourceHandles(p_radiossModel, selSlipring->GetHandle(), dynaSourceHandles);
        if(dynaSourceHandles.size() > 0) cptSlipring = cptSlipring + 1;
    }

    selSlipring.Restart();
    slipringIdList.reserve(cptSlipring);


    while (selSlipring.Next())
    {
        sdiConvert::SDIHandlReadList dynaSourceHandles;
        sdiConvert::Convert::GetSourceHandles(p_radiossModel, selSlipring->GetHandle(), dynaSourceHandles);
        if(dynaSourceHandles.size() > 0)
        {
            slipringIdList.push_back(selSlipring->GetId());
            targetHandlesList.push_back(selSlipring->GetHandle());
        }
    }

    if (!slipringIdList.empty())
    {
        HandleEdit radThSlipringHEdit;
        p_radiossModel->CreateEntity(radThSlipringHEdit, "/TH/SLIPRING", "TH-SLIPRING");

        EntityEdit radThAccelEdit(p_radiossModel, radThSlipringHEdit);
        radThAccelEdit.SetValue(sdiIdentifier("idsmax"), sdiValue((int)slipringIdList.size()));
        radThAccelEdit.SetValue(sdiIdentifier("Number_Of_Variables"), sdiValue(1));
        radThAccelEdit.SetValue(sdiIdentifier("Obj"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/SLIPRING"), slipringIdList)));
        radThAccelEdit.SetValue(sdiIdentifier("var"), sdiValue(sdiStringList({ "DEF" })));

        if (radThSlipringHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList radThSlipringSourceHandles;
            for (int i = 0; i < targetHandlesList.size(); i=i+1)
            {
                sdiConvert::SDIHandlReadList radSlipringSourceHandles;
                sdiConvert::Convert::GetSourceHandles(p_radiossModel, targetHandlesList[i], radSlipringSourceHandles);
                for(size_t i = 0; i < radSlipringSourceHandles.size(); ++i)
                {
                    radThSlipringSourceHandles.push_back(radSlipringSourceHandles[i]);
                }
            }
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radThSlipringHEdit, radThSlipringSourceHandles));
        }
    }
}

void ConvertElem::ConvertSeatbeltSensor()
{
    SelectionRead selSeatbeltSensor(p_lsdynaModel, "*ELEMENT_SEATBELT_SENSOR");
    EntityType radSensorType = p_radiossModel->GetEntityType("/SENSOR");
     
    while (selSeatbeltSensor.Next())
    {    
        int lsdSBSTYP = GetValue<int>(*selSeatbeltSensor, "SBSTYP");

        if(lsdSBSTYP == 1)
        {
            sdiString name = "Accelerometer sensor";
            HandleEdit radSensorACCEHEdit;
            p_radiossModel->CreateEntity(radSensorACCEHEdit, "/SENSOR/ACCE", name, selSeatbeltSensor->GetId());

            if(radSensorACCEHEdit.IsValid())
            {
             	EntityEdit radSensorACCEEdit(p_radiossModel, radSensorACCEHEdit);
                radSensorACCEEdit.SetValue(sdiIdentifier("Sensor_Type"), sdiValue(2));

                sdiString DIR;
                int lsdDOF = GetValue<int>(*selSeatbeltSensor, "DOF");
                if(lsdDOF == 1)
                {
                       DIR = "X";
                }
                else if(lsdDOF == 2)
                {
                    DIR = "Y";
                }
                else if(lsdDOF == 3)
                {
                       DIR = "Z";
                }

                radSensorACCEEdit.SetValue(sdiIdentifier("Nacc"), sdiValue(1));
                radSensorACCEEdit.SetValue(sdiIdentifier("Tdelay"), sdiValue(0.0));
                radSensorACCEEdit.SetValue(sdiIdentifier("dir"), sdiValue(DIR));
                p_ConvertUtils.CopyValue(*selSeatbeltSensor, radSensorACCEEdit, "ACC", "Tomin");
                p_ConvertUtils.CopyValue(*selSeatbeltSensor, radSensorACCEEdit, "ATIME", "Tmin");

                sdiValueEntity lsdNID = GetValue<sdiValueEntity>(*selSeatbeltSensor, "NID");
                unsigned int NID = lsdNID.GetId();

                // create /ACCEL card
                if( NID > 0)
                {
                    HandleEdit radACCELHEdit;
                    p_radiossModel->CreateEntity(radACCELHEdit,"/ACCEL",name);
                    EntityEdit radACCELEdit(p_radiossModel, radACCELHEdit);

                    radACCELEdit.SetValue(sdiIdentifier("nodeid"), sdiValue(sdiValueEntity(sdiValueEntityType(1), NID)));
                    radACCELEdit.SetValue(sdiIdentifier("skewid"), sdiValue(sdiValueEntity(sdiValueEntityType("/SKEW"), 0)));
                    radACCELEdit.SetValue(sdiIdentifier("cutoff"), sdiValue(0.0));
                    radSensorACCEEdit.SetEntityHandle(sdiIdentifier("accel_ID"), radACCELHEdit);
                }
                sdiConvert::SDIHandlReadList sourceSensorAccel = { {selSeatbeltSensor->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(radSensorACCEHEdit, sourceSensorAccel));
            }
        }
        else if(lsdSBSTYP == 3)
        {
            sdiString name = "Time sensor";
            HandleEdit radSensorTIMEHEdit;
            p_radiossModel->CreateEntity(radSensorTIMEHEdit, "/SENSOR/TIME", name, selSeatbeltSensor->GetId());

            if(radSensorTIMEHEdit.IsValid())
            {
             	EntityEdit radSensorTIMEEdit(p_radiossModel, radSensorTIMEHEdit);
                radSensorTIMEEdit.SetValue(sdiIdentifier("Sensor_Type"), sdiValue(1));
          	    p_ConvertUtils.CopyValue(*selSeatbeltSensor, radSensorTIMEEdit, "TIME", "Tdelay");
                    
            }
            sdiConvert::SDIHandlReadList sourceSensorTime = { {selSeatbeltSensor->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radSensorTIMEHEdit, sourceSensorTime));
        }
        else if(lsdSBSTYP == 4)
        {
            sdiString name = "Displacement sensor";
            HandleEdit radSensorDispHEdit;
            p_radiossModel->CreateEntity(radSensorDispHEdit, "/SENSOR/DIST", name, selSeatbeltSensor->GetId());

            if(radSensorDispHEdit.IsValid())
            {
             	EntityEdit radSensorDispEdit(p_radiossModel, radSensorDispHEdit);
          	    radSensorDispEdit.SetValue(sdiIdentifier("Sensor_Type"), sdiValue(3));
                p_ConvertUtils.CopyValue(*selSeatbeltSensor, radSensorDispEdit, "NID1", "N1");
                p_ConvertUtils.CopyValue(*selSeatbeltSensor, radSensorDispEdit, "NID2", "N2");
                p_ConvertUtils.CopyValue(*selSeatbeltSensor, radSensorDispEdit, "DMN", "Dmin");                   
                p_ConvertUtils.CopyValue(*selSeatbeltSensor, radSensorDispEdit, "DMX", "Dmax");
                radSensorDispEdit.SetValue(sdiIdentifier("Tdelay"), sdiValue(0.0));
            }
            sdiConvert::SDIHandlReadList sourceSensorDist = { {selSeatbeltSensor->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radSensorDispHEdit, sourceSensorDist));
        }
    }
}

void ConvertElem::ConvertSeatbeltRetractor()
{
    SelectionRead selSeatbeltRetractor(p_lsdynaModel, "*ELEMENT_SEATBELT_RETRACTOR");

    SelectionRead selSeatbeltPretensioner(p_lsdynaModel, "*ELEMENT_SEATBELT_PRETENSIONER");
    
    while (selSeatbeltRetractor.Next())
    {
        sdiValueEntity lsdSBRNID = GetValue<sdiValueEntity>(*selSeatbeltRetractor, "SBRNID");
        unsigned int SBRNID = lsdSBRNID.GetId();
        sdiValueEntity lsdSBID = GetValue<sdiValueEntity>(*selSeatbeltRetractor, "SBID");
        unsigned int SBID = lsdSBID.GetId();

        sdiValueEntity lsdSID1 = GetValue<sdiValueEntity>(*selSeatbeltRetractor, "SID1");
        unsigned int SID1 = lsdSID1.GetId();
        sdiValueEntity lsdSID2 = GetValue<sdiValueEntity>(*selSeatbeltRetractor, "SID2");
        unsigned int SID2 = lsdSID2.GetId();
        sdiValueEntity lsdSID3 = GetValue<sdiValueEntity>(*selSeatbeltRetractor, "SID3");
        unsigned int SID3 = lsdSID3.GetId();
        sdiValueEntity lsdSID4 = GetValue<sdiValueEntity>(*selSeatbeltRetractor, "SID4");
        unsigned int SID4 = lsdSID4.GetId();

        int sensor_ID_1 = SID1;
        if(sensor_ID_1 == 0)
            sensor_ID_1 = SID2;
        if(sensor_ID_1 == 0)
            sensor_ID_1 = SID3;
        if(sensor_ID_1 == 0)
            sensor_ID_1 = SID4;

        sdiString name = "Retractor_"+ to_string(selSeatbeltRetractor->GetId());
        double lsdTDEL = GetValue<double>(*selSeatbeltRetractor, "TDEL");

        HandleEdit radRetractorHEdit;
        p_radiossModel->CreateEntity(radRetractorHEdit, "/RETRACTOR/SPRING", name, selSeatbeltRetractor->GetId());

        if(radRetractorHEdit.IsValid())
        {
             	
            EntityEdit radRetractorEdit(p_radiossModel, radRetractorHEdit);

            p_ConvertUtils.CopyValue(*selSeatbeltRetractor, radRetractorEdit, "SBRNID", "Node_ID");
            p_ConvertUtils.CopyValue(*selSeatbeltRetractor, radRetractorEdit, "SBID", "EL_ID");

            if(lsdTDEL == 0.0)
            {
                radRetractorEdit.SetValue(sdiIdentifier("Sens_ID1"), sdiValue(sdiValueEntity(sdiValueEntityType("/SENSOR"), sensor_ID_1)));
            }
            else if(lsdTDEL > 0.0)
            {
                // create a new sensor (same type as the original sensor in the retractor)
                // check the original sensor type: accelerometer, time, displacement

                HandleRead dynaRetractorSensorHRead;
                selSeatbeltRetractor->GetEntityHandle(sdiIdentifier("SID1"), dynaRetractorSensorHRead);
                if(!dynaRetractorSensorHRead.IsValid())
                {
                    selSeatbeltRetractor->GetEntityHandle(sdiIdentifier("SID2"), dynaRetractorSensorHRead);
                }
                if(!dynaRetractorSensorHRead.IsValid())
                {
                    selSeatbeltRetractor->GetEntityHandle(sdiIdentifier("SID3"), dynaRetractorSensorHRead);
                }
                if(!dynaRetractorSensorHRead.IsValid())
                {
                    selSeatbeltRetractor->GetEntityHandle(sdiIdentifier("SID4"), dynaRetractorSensorHRead);
                }

                EntityRead dynaRetractorSensorRead(p_lsdynaModel, dynaRetractorSensorHRead);
                int lsdSBSTYP = GetValue<int>(dynaRetractorSensorRead, "SBSTYP");

                int radSBSTYP = 0;
                sdiString sensor_name_type = " ";
                if(lsdSBSTYP == 1)
                {
                    sensor_name_type = "/ACCE";
                    radSBSTYP = 2;
                }
                else if(lsdSBSTYP == 3)
                {
                    sensor_name_type = "/TIME";
                    radSBSTYP = 1;
                }
                else if(lsdSBSTYP == 4)
                {
                    sensor_name_type = "/DIST";
                    radSBSTYP = 3;
                }

                sdiString name_sensor_1 = "Duplicate_sensor_"+ to_string(sensor_ID_1);
                HandleEdit radSensorID1HEdit;
                p_radiossModel->CreateEntity(radSensorID1HEdit,"/SENSOR"+sensor_name_type,name_sensor_1);
                EntityEdit radSensorID1Edit(p_radiossModel, radSensorID1HEdit);
                radSensorID1Edit.SetValue(sdiIdentifier("Sensor_Type"), sdiValue(radSBSTYP));
                p_ConvertUtils.CopyValue(*selSeatbeltRetractor, radSensorID1Edit, "TDEL", "Tdelay");
                radRetractorEdit.SetValue(sdiIdentifier("Sens_ID1"), sdiValue(sdiValueEntity(sdiValueEntityType("/SENSOR"), radSensorID1HEdit.GetId(p_radiossModel))));
            }

            p_ConvertUtils.CopyValue(*selSeatbeltRetractor, radRetractorEdit, "PULL", "Pullout");
            p_ConvertUtils.CopyValue(*selSeatbeltRetractor, radRetractorEdit, "LLCID", "Fct_ID1");
            p_ConvertUtils.CopyValue(*selSeatbeltRetractor, radRetractorEdit, "ULCID", "Fct_ID2");
            p_ConvertUtils.CopyValue(*selSeatbeltRetractor, radRetractorEdit, "LFED", "Elem_size");

            //---
            // loop over Pretensioners
            while (selSeatbeltPretensioner.Next())
            {
                sdiValueEntity lsdSBRID = GetValue<sdiValueEntity>(*selSeatbeltPretensioner, "SBRID");
                unsigned int SBRID = lsdSBRID.GetId();

                if (selSeatbeltRetractor->GetId() == SBRID)
                {
                    sdiValueEntity lsdSBSID1 = GetValue<sdiValueEntity>(*selSeatbeltPretensioner, "SBSID1");
                    unsigned int SBSID1 = lsdSBSID1.GetId();
                    sdiValueEntity lsdSBSID2 = GetValue<sdiValueEntity>(*selSeatbeltPretensioner, "SBSID2");
                    unsigned int SBSID2 = lsdSBSID2.GetId();
                    sdiValueEntity lsdSBSID3 = GetValue<sdiValueEntity>(*selSeatbeltPretensioner, "SBSID3");
                    unsigned int SBSID3 = lsdSBSID3.GetId();
                    sdiValueEntity lsdSBSID4 = GetValue<sdiValueEntity>(*selSeatbeltPretensioner, "SBSID4");
                    unsigned int SBSID4 = lsdSBSID4.GetId();

                    int sensor_ID_2 = SBSID1;
                    if(sensor_ID_2 == 0)
                       sensor_ID_2 = SBSID2;
                    if(sensor_ID_2 == 0)
                       sensor_ID_2 = SBSID3;
                    if(sensor_ID_2 == 0)
                       sensor_ID_2 = SBSID4;

                    double lsdTIME = GetValue<double>(*selSeatbeltPretensioner, "TIME");

                    if(lsdTIME == 0.0)
                    {
                        radRetractorEdit.SetValue(sdiIdentifier("Sens_ID2"), sdiValue(sdiValueEntity(sdiValueEntityType("/SENSOR"), sensor_ID_2)));
                    }
                    else if(lsdTIME > 0.0)
                    {
                        // create a new sensor (same type as the original sensor in the retractor)
                        // check the original sensor type: accelerometer, time, displacement

                        HandleRead dynaPretensionerSensorHRead;
                        selSeatbeltPretensioner->GetEntityHandle(sdiIdentifier("SBSID1"), dynaPretensionerSensorHRead);
                        if(!dynaPretensionerSensorHRead.IsValid())
                        {
                            selSeatbeltPretensioner->GetEntityHandle(sdiIdentifier("SBSID2"), dynaPretensionerSensorHRead);
                        }
                        if(!dynaPretensionerSensorHRead.IsValid())
                        {
                            selSeatbeltPretensioner->GetEntityHandle(sdiIdentifier("SBSID3"), dynaPretensionerSensorHRead);
                        }
                        if(!dynaPretensionerSensorHRead.IsValid())
                        {
                            selSeatbeltPretensioner->GetEntityHandle(sdiIdentifier("SBSID4"), dynaPretensionerSensorHRead);
                        }

                        EntityRead dynaPretensionerSensorRead(p_lsdynaModel, dynaPretensionerSensorHRead);
                        int lsdSBSTYP = GetValue<int>(dynaPretensionerSensorRead, "SBSTYP");

                        int radSBSTYP = 0;
                        sdiString sensor_name_type = " ";
                        if(lsdSBSTYP == 1)
                        {
                            sensor_name_type = "/ACCE";
                            radSBSTYP = 2;
                        }
                        else if(lsdSBSTYP == 3)
                        {
                            sensor_name_type = "/TIME";
                            radSBSTYP = 1;
                        }
                        else if(lsdSBSTYP == 4)
                        {
                            sensor_name_type = "/DIST";
                            radSBSTYP = 3;
                        }

                        sdiString name_sensor_2 = "Duplicate_sensor_"+ to_string(sensor_ID_2);
                        HandleEdit radSensorID2HEdit;
                        p_radiossModel->CreateEntity(radSensorID2HEdit,"/SENSOR"+sensor_name_type,name_sensor_2);
                        EntityEdit radSensorID2Edit(p_radiossModel, radSensorID2HEdit);
                        radSensorID2Edit.SetValue(sdiIdentifier("Sensor_Type"), sdiValue(radSBSTYP));
                        p_ConvertUtils.CopyValue(*selSeatbeltPretensioner, radSensorID2Edit, "TIME", "Tdelay");
                        radRetractorEdit.SetValue(sdiIdentifier("Sens_ID2"), sdiValue(sdiValueEntity(sdiValueEntityType("/SENSOR"), radSensorID2HEdit.GetId(p_radiossModel))));
                    }
                    p_ConvertUtils.CopyValue(*selSeatbeltPretensioner, radRetractorEdit, "PTLCID", "Fct_ID3");
                    p_ConvertUtils.CopyValue(*selSeatbeltPretensioner, radRetractorEdit, "LMTFRC", "Force");
                    int lsdSBPRTY = GetValue<int>(*selSeatbeltPretensioner, "SBPRTY");

                    int radTens_typ = 0;
                    if(lsdSBPRTY == 1 || lsdSBPRTY == 5)
                    {
                        radTens_typ = 1;
                    }
                    else if(lsdSBPRTY == 4)
                    {
                        radTens_typ = 2;
                    }
                    else if(lsdSBPRTY == 6 || lsdSBPRTY == 7)
                    {
                        radTens_typ = 3;
                    }
                    else if(lsdSBPRTY == 8)
                    {
                        radTens_typ = 5;
                    }
                    radRetractorEdit.SetValue(sdiIdentifier("Tens_typ"), sdiValue(radTens_typ));
                    break;
                }
            }
            //---
            sdiConvert::SDIHandlReadList sourceRetractor = { {selSeatbeltRetractor->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radRetractorHEdit, sourceRetractor));
        }
    }
}