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

#include <dyna2rad/convertcrosssections.h>
#include <dyna2rad/dyna2rad.h>
using namespace std;
using namespace sdi;


void sdiD2R::ConvertSection::ConvertCrossSections()
{
    ConvertEntities();
}

void sdiD2R::ConvertSection::ConvertEntities()
{
    p_ConvertSectionPlane();

    p_ConvertSectionSet();

    p_CreateTHSect();
}

void sdiD2R::ConvertSection::p_ConvertSectionPlane()
{
    SelectionRead selSectPlane(p_lsdynaModel, srcCard + "_PLANE");
    while (selSectPlane.Next())
    {
        double lsdXCT;
        double lsdYCT;
        double lsdZCT;
        double lsdXCH;
        double lsdYCH;
        double lsdZCH;
        double lsdRad;
        double lsdXHEV;
        double lsdYHEV;
        double lsdZHEV;
        double lsdLenL;
        double lsdLenM;
        sdiString destCard("/SECT");
        vector<reference_wrapper<double>> doubleAttrVals({ lsdXCT, lsdYCT, lsdZCT, lsdXCH, lsdYCH, lsdZCH, lsdRad, lsdXHEV, lsdYHEV, lsdZHEV, lsdLenL, lsdLenM });
        sdiStringList doubleAttrNames({ "XCT", "YCT", "ZCT", "XCH", "YCH", "ZCH", "RADIUS","XHEV", "YHEV", "ZHEV", "LENL", "LENM" });
        p_ConvertUtils.GetAttribValues(*selSectPlane, doubleAttrNames, doubleAttrVals);

        int sectType = 1;/*1 - infinite, 2-finite, 3- circular*/
        if (lsdRad != 0.0)
        {
            sectType = 3;
            destCard = "/SECT/CIRCLE";
        }
        else if (lsdLenL != 0.0 && lsdLenM != 0.0)
        {
            sectType = 2;
            destCard = "/SECT/PARAL";
        }
        sdiConvert::SDIHandlReadList sourceList = { {selSectPlane->GetHandle()} };
        EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
        if (sectType)
        {
            HandleEdit sectHEdit;
            unsigned int sectId = selSectPlane->GetId();
            sdiString sectName = selSectPlane->GetName();
            p_radiossModel->CreateEntity(sectHEdit, destCard, sectName, sectId);
            if (sectHEdit.IsValid())
            {
                EntityEdit sectEdit(p_radiossModel, sectHEdit);
                HandleRead psidHread;
                selSectPlane->GetEntityHandle(sdiIdentifier("PSID"), psidHread);
                if (psidHread.IsValid())
                {
                    EntityRead setRead(p_lsdynaModel, psidHread);
                    unsigned int psid = DynaToRad::GetRadiossSetIdFromLsdSet(setRead.GetId(), setRead.GetKeyword());
                    sectEdit.SetValue(sdiIdentifier("grbric_ID"), sdiValue(sdiValueEntity(radSetType, psid)));
                    sectEdit.SetValue(sdiIdentifier("grshel_ID"), sdiValue(sdiValueEntity(radSetType, psid)));
                    sectEdit.SetValue(sdiIdentifier("grtria_ID"), sdiValue(sdiValueEntity(radSetType, psid)));
                }
                else
                {
                    HandleEdit radSetHEdit;
                    p_radiossModel->CreateEntity(radSetHEdit, "/SET/GENERAL","SET_GENERAL_ALL_DB_CROSS_SECTION_" + to_string(sectId));
                    EntityEdit radSetEdit(p_radiossModel, radSetHEdit);
                    radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                    radSetEdit.SetValue(sdiIdentifier("KEY_type"), sdiValue(sdiString("ALL")));
                    sectEdit.SetEntityHandle(sdiIdentifier("grbric_ID"), radSetHEdit);
                    sectEdit.SetEntityHandle(sdiIdentifier("grshel_ID"), radSetHEdit);
                    sectEdit.SetEntityHandle(sdiIdentifier("grtria_ID"), radSetHEdit);
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(radSetHEdit, sourceList));
                }
                sdiTriple node1Coords(lsdXCT, lsdYCT, lsdZCT);
                sdiTriple vectHEV(lsdXHEV, lsdYHEV, lsdZHEV);
                sdiTriple node2Coords(lsdXHEV, lsdYHEV, lsdZHEV);
                sdiTriple vectCH(lsdXCH, lsdYCH, lsdZCH);
                if (sectType != 2)
                {
                    vectHEV = sdiTriple(lsdXCT + 1.0, lsdYCT, lsdZCT);
                    sdiTriple vectCTCH(vectCH - node1Coords);
                    sdiTriple vectCTHEV(vectHEV - node1Coords);
                    if ((vectCTCH * vectCTHEV).Len() == 0.0)
                        vectHEV = sdiTriple(lsdXCT, lsdYCT + 1.0, lsdZCT);
                }
                sdiTriple node3Coords(node1Coords +
                    (vectCH - node1Coords).Normalize() * (vectHEV - node1Coords).Normalize());
                if (node2Coords.Len() == 0.0 && sectType != 2)
                    node2Coords = node1Coords +
                    ((vectCH - node1Coords).Normalize() * (node3Coords - node1Coords).Normalize());

                HandleNodeEdit n1HEdit;
                HandleNodeEdit n2HEdit;
                HandleNodeEdit n3HEdit;
                p_radiossModel->CreateNode(n1HEdit, "/NODE", node1Coords);
                p_radiossModel->CreateNode(n2HEdit, "/NODE", node2Coords);
                p_radiossModel->CreateNode(n3HEdit, "/NODE", node3Coords);
                if (n1HEdit.IsValid() && n2HEdit.IsValid() && n3HEdit.IsValid())
                {
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(n1HEdit, sourceList));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(n2HEdit, sourceList));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(n3HEdit, sourceList));
                    if (sectType == 1)
                    {
                        HandleEdit frameHEdit;
                        p_radiossModel->CreateEntity(frameHEdit, "/FRAME/MOV","", p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*DEFINE_COORDINATE")));
                        if (frameHEdit.IsValid())
                        {
                            EntityEdit framEntEdit(p_radiossModel, frameHEdit);
                            framEntEdit.SetValue(sdiIdentifier("N1"), sdiValue(sdiValueEntity(1, n1HEdit.GetId(p_radiossModel))));
                            framEntEdit.SetValue(sdiIdentifier("N2"), sdiValue(sdiValueEntity(1, n2HEdit.GetId(p_radiossModel))));
                            framEntEdit.SetValue(sdiIdentifier("N3"), sdiValue(sdiValueEntity(1, n3HEdit.GetId(p_radiossModel))));
                            framEntEdit.SetValue(sdiIdentifier("titlestr"), sdiValue("FRAME_MOV_DB_CROSS_SECTION_" + to_string(sectId)));
                            sectEdit.SetEntityHandle(sdiIdentifier("Frame_ID"), frameHEdit);
                            sdiConvert::Convert::PushToConversionLog(std::make_pair(frameHEdit, sourceList));
                        }
                    }
                    else if (sectType != 1)
                    {
                        sectEdit.SetValue(sdiIdentifier("node_ID1"),sdiValue(sdiValueEntity(1,n1HEdit.GetId(p_radiossModel))));
                        sectEdit.SetValue(sdiIdentifier("node_ID2"), sdiValue(sdiValueEntity(1,n2HEdit.GetId(p_radiossModel))));
                        sectEdit.SetValue(sdiIdentifier("node_ID3"), sdiValue(sdiValueEntity(1,n3HEdit.GetId(p_radiossModel))));
                    }
                }
                sdiStringList tempStrList({ "X", "Y", "Z" });
                sdiTriple vectM1 = node1Coords + (vectHEV - node1Coords).Normalize() * lsdLenL;
                sdiTriple vectM2 = node1Coords + ((vectCH - node1Coords) * (vectHEV - node1Coords)).Normalize() * lsdLenM;
                for (size_t i = 0; i < 3; ++i)
                {
                    if (sectType != 1)
                        sectEdit.SetValue(sdiIdentifier(tempStrList[i] + "M"), sdiValue(node1Coords[i]));
                    if (sectType == 2)
                    {
                        sectEdit.SetValue(sdiIdentifier(tempStrList[i] + "M1"), sdiValue(vectM1[i]));
                        sectEdit.SetValue(sdiIdentifier(tempStrList[i] + "M2"), sdiValue(vectM2[i]));
                    }
                }
                if (sectType == 3)
                {
                    sdiTriple dirVect((vectCH - node1Coords).Normalize());
                    sectEdit.SetValue(sdiIdentifier("R"), sdiValue(lsdRad));
                    for (size_t i = 0; i < 3; ++i)
                        sectEdit.SetValue(sdiIdentifier("Dir_" + tempStrList[i]), sdiValue(dirVect[i]));
                }
                //sectEdit.SetValue(sdiIdentifier("Iframe"), sdiValue(1));
                sdiConvert::Convert::PushToConversionLog(std::make_pair(sectHEdit, sourceList));
            }
        }
    }

}

void sdiD2R::ConvertSection::p_ConvertSectionSet()
{
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    EntityType radNodeType = p_radiossModel->GetEntityType("/NODE");
    SelectionRead selSectSet(p_lsdynaModel, srcCard + "_SET");
    while (selSectSet.Next())
    {
        HandleEdit radSectHEdit;
        p_radiossModel->CreateEntity(radSectHEdit, destCard, selSectSet->GetName(), selSectSet->GetId());
        if (radSectHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceList = { {selSectSet->GetHandle()} };
            EntityEdit radSectEdit(p_radiossModel, radSectHEdit);
            unsigned int node1Id = 0;
            unsigned int node2Id = 0;
            unsigned int node3Id = 0;
            HandleRead node1HRead;
            HandleRead node2HRead;
            HandleRead node3HRead;
            HandleNodeEdit node1Hedit;
            HandleNodeEdit node2Hedit;
            HandleNodeEdit node3Hedit;
            HandleRead nsidHread;
            sdiTriple node1Pos(0.0, 0.0, 0.0);
            sdiTriple node2Pos(0.0, 0.0, 0.0);
            sdiTriple node3Pos(0.0, 0.0, 0.0);
            selSectSet->GetEntityHandle(sdiIdentifier("NSID"), nsidHread);
            if (nsidHread.IsValid())
            {
                EntityRead nsidRead(p_lsdynaModel, nsidHread);
                sdiValueEntityList nodeEntityList;
                sdiUIntList nodeIdList;
                sdiValue tempVal(nodeEntityList);
                nsidRead.GetValue(sdiIdentifier("ids"), tempVal);
                tempVal.GetValue(nodeEntityList);
                nodeEntityList.GetIdList(nodeIdList);

                if (nodeIdList.size() >= 3)
                {
                    nsidRead.GetEntityHandle(sdiIdentifier("ids", 0, 0), node1HRead);
                    nsidRead.GetEntityHandle(sdiIdentifier("ids", 0, 1), node2HRead);
                    nsidRead.GetEntityHandle(sdiIdentifier("ids", 0, 2), node3HRead);
                    NodeRead node1Read(p_lsdynaModel, node1HRead);
                    NodeRead node2Read(p_lsdynaModel, node2HRead);
                    NodeRead node3Read(p_lsdynaModel, node3HRead);
                    node1Pos = node1Read.GetPosition();
                    node2Pos = node2Read.GetPosition();
                    node3Pos = node3Read.GetPosition();
                }

                if (((node2Pos - node1Pos) * (node3Pos - node2Pos)).Len() <= 1e-20 || nodeIdList.size() < 3)
                {
                    p_radiossModel->CreateNode(node1Hedit, "/NODE", sdiTriple(0.0, 0.0, 0.0));
                    p_radiossModel->CreateNode(node2Hedit, "/NODE", sdiTriple(1.0, 0.0, 0.0));
                    p_radiossModel->CreateNode(node3Hedit, "/NODE", sdiTriple(0.0, 1.0, 0.0));
                }
                radSectEdit.SetValue(sdiIdentifier("grnod_ID"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(nsidRead.GetId(), "*SET_NODE" ))));
            }

            if (node1Hedit.IsValid() && node2Hedit.IsValid() && node3Hedit.IsValid())
            {
                node1Id = node1Hedit.GetId(p_radiossModel);
                node2Id = node2Hedit.GetId(p_radiossModel);
                node3Id = node3Hedit.GetId(p_radiossModel);
                sdiConvert::Convert::PushToConversionLog(std::make_pair(node1Hedit, sourceList));
                sdiConvert::Convert::PushToConversionLog(std::make_pair(node2Hedit, sourceList));
                sdiConvert::Convert::PushToConversionLog(std::make_pair(node3Hedit, sourceList));
            }
            else if (node1HRead.IsValid() && node2HRead.IsValid() && node3HRead.IsValid())
            {
                node1Id = node1HRead.GetId(p_lsdynaModel);
                node2Id = node2HRead.GetId(p_lsdynaModel);
                node3Id = node3HRead.GetId(p_lsdynaModel);
            }
            if (node1Id != 0) 
                radSectEdit.SetValue(sdiIdentifier("node_ID1"), sdiValue(sdiValueEntity(radNodeType, node1Id)));
            if (node2Id != 0)
                radSectEdit.SetValue(sdiIdentifier("node_ID2"), sdiValue(sdiValueEntity(radNodeType, node2Id)));
            if (node3Id != 0)
                radSectEdit.SetValue(sdiIdentifier("node_ID3"), sdiValue(sdiValueEntity(radNodeType, node3Id)));

            HandleRead hsidHread;
            HandleRead bsidHread;
            HandleRead ssidHread;
            HandleRead tsidHread;
            HandleRead dsidHread;

            vector<reference_wrapper<HandleRead>> entityAttrVals({ hsidHread, bsidHread, ssidHread, tsidHread, dsidHread });
            sdiStringList entityAttrNames({ "HSID" , "BSID", "SSID", "TSID", "DSID" });
            p_ConvertUtils.GetEntityHandles(*selSectSet, entityAttrNames, entityAttrVals);

            if (ssidHread.IsValid())
            {
                EntityRead ssidRead(p_lsdynaModel, ssidHread);
                unsigned int ssid = DynaToRad::GetRadiossSetIdFromLsdSet(ssidRead.GetId(), ssidRead.GetKeyword());
                radSectEdit.SetValue(sdiIdentifier("grshel_ID"), sdiValue(sdiValueEntity(radSetType, ssid)));
                radSectEdit.SetValue(sdiIdentifier("grtria_ID"), sdiValue(sdiValueEntity(radSetType, ssid)));
            }
            if (bsidHread.IsValid())
            {
                EntityRead bsidRead(p_lsdynaModel, bsidHread);
                unsigned int bsid = DynaToRad::GetRadiossSetIdFromLsdSet(bsidRead.GetId(), bsidRead.GetKeyword());
                radSectEdit.SetValue(sdiIdentifier("grbeam_ID"), sdiValue(sdiValueEntity(radSetType, bsid)));
                radSectEdit.SetValue(sdiIdentifier("grsprg_ID"), sdiValue(sdiValueEntity(radSetType, bsid)));
                radSectEdit.SetValue(sdiIdentifier("grtrus_ID"), sdiValue(sdiValueEntity(radSetType, bsid)));
            }
            if (hsidHread.IsValid())
            {
                EntityRead hsidRead(p_lsdynaModel, hsidHread);
                radSectEdit.SetValue(sdiIdentifier("grbric_ID"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(hsidRead.GetId(), hsidRead.GetKeyword()))));
            }

            //if (tsidEntity.GetId())
            //    radSectEdit.SetValue(sdiIdentifier("grbric_ID"), sdiValue(sdiValueEntity(radSetType, tsidEntity.GetId())));

            if (dsidHread.IsValid())
            {
                EntityRead dsidRead(p_lsdynaModel, dsidHread);
                unsigned int dsid = DynaToRad::GetRadiossSetIdFromLsdSet(dsidRead.GetId(), dsidRead.GetKeyword());
                if (bsidHread.IsValid())
                {
                    EntityRead bsidRead(p_lsdynaModel, bsidHread);
                    sdiUIntList setids = { { DynaToRad::GetRadiossSetIdFromLsdSet(bsidRead.GetId(), bsidRead.GetKeyword()), dsid } };
                    HandleEdit combinedSetHEdit;
                    p_radiossModel->CreateEntity(combinedSetHEdit, "/SET/GENERAL", "Combined_BSID_DSID_Cross_Section_" + to_string(selSectSet->GetId()));
                    if (combinedSetHEdit.IsValid())
                    {
                        EntityEdit radSetEdit(p_radiossModel, combinedSetHEdit);
                        radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                        radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("SET")));
                        radSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(2));
                        radSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radSetType, setids)));
                        radSectEdit.SetEntityHandle(sdiIdentifier("grsprg_ID"), combinedSetHEdit);
                        sdiConvert::Convert::PushToConversionLog(std::make_pair(combinedSetHEdit, sourceList));
                    }
                }
                else
                    radSectEdit.SetValue(sdiIdentifier("grsprg_ID"), sdiValue(sdiValueEntity(radSetType, dsid)));
            }

            radSectEdit.SetValue(sdiIdentifier("Iframe"), sdiValue(2));
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radSectHEdit, sourceList));
        }
    }
}

void sdiD2R::ConvertSection::p_CreateTHSect()
{
    SelectionRead selDatabaseSecFo(p_lsdynaModel, "*DATABASE_SECFORC");
    while (selDatabaseSecFo.Next())
    {
        sdiUIntList allSectList;
        SelectionRead selRadSect(p_radiossModel, "/SECT");
        allSectList.reserve(selRadSect.Count());
        while (selRadSect.Next())
        {
            allSectList.push_back(selRadSect->GetId());
        }
        if (!allSectList.empty())
        {
            HandleEdit radThSectHEdit;
            p_radiossModel->CreateEntity(radThSectHEdit, "/TH/SECTIO", "TH-SECTIO");
            EntityEdit radThSectEdit(p_radiossModel, radThSectHEdit);

            radThSectEdit.SetValue(sdiIdentifier("idsmax"), sdiValue((int)allSectList.size()));
            radThSectEdit.SetValue(sdiIdentifier("Number_Of_Variables"), sdiValue(1));
            radThSectEdit.SetValue(sdiIdentifier("Obj"), sdiValue(sdiValueEntityList(destEntityType, allSectList)));
            radThSectEdit.SetValue(sdiIdentifier("var"), sdiValue(sdiStringList({ "DEF"})));

            sdiConvert::SDIHandlReadList sourceHandles = { {selDatabaseSecFo->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radThSectHEdit, sourceHandles));
        }
        break;
    }
}
