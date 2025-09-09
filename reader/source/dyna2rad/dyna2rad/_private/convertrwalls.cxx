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

#include <dyna2rad/convertrwalls.h>
#include <dyna2rad/dyna2rad.h>
#include <typedef.h>

using namespace std;
using namespace sdi;

void sdiD2R::ConvertRwall::ConvertRigidwalls()
{
    ConvertEntities();
}

void sdiD2R::ConvertRwall::ConvertEntities()
{
    p_ConvertRwallPlanar();

    p_ConvertRwallGeometric();

    p_CreateThRwall();

}

void sdiD2R::ConvertRwall::p_ConvertRwallPlanar()
{
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    SelectionRead selRwallPlanar(p_lsdynaModel, "*RIGIDWALL_PLANAR");
    while (selRwallPlanar.Next())
    {
        sdiString keyWord = selRwallPlanar->GetKeyword();
        sdiString rwallName = selRwallPlanar->GetName();
        unsigned int rwallId = selRwallPlanar->GetId();
        bool isRwallFinite = false;
        sdiConvert::SDIHandlReadList sourcerList = { {selRwallPlanar->GetHandle()} };

        sdiString radKeyWord;
        if (keyWord.find("FINITE") != keyWord.npos)
        {
            isRwallFinite = true;
            radKeyWord = "/RWALL/PARAL";
        }
        else
        {
            radKeyWord = "/RWALL/PLANE";
        }
        HandleEdit radRwallHEdit;
        p_radiossModel->CreateEntity(radRwallHEdit, radKeyWord, rwallName, rwallId);
        if (radRwallHEdit.IsValid())
        {
            HandleRead nsidHRead;
            HandleRead nsidEXHRead;
            sdiStringList entityAttNames({ "NSID", "NSIDEX" });
            vector<reference_wrapper<HandleRead>> entityAttrVals({ nsidHRead, nsidEXHRead });
            p_ConvertUtils.GetEntityHandles(*selRwallPlanar, entityAttNames, entityAttrVals);

            double lsdOFFSET;
            double lsdXT;
            double lsdYT;
            double lsdZT;
            double lsdXH;
            double lsdYH;
            double lsdZH;
            double lsdFRIC;
            double lsdXHEV;
            double lsdYHEV;
            double lsdZHEV;
            double lsdLENL;
            double lsdLENM;
            double lsdMASS;
            double lsdV0;
            sdiStringList doubleAttrNames({ "OFFSET", "XT", "YT", "ZT", "XH", "YH", "ZH", "FRIC",
                                           "XHEV", "YHEV", "ZHEV", "LENL", "LENM", "MASS", "V0" });
            vector<reference_wrapper<double>> doubleAttrVals({ lsdOFFSET, lsdXT, lsdYT, lsdZT, lsdXH, lsdYH, lsdZH, lsdFRIC,
                                                               lsdXHEV, lsdYHEV, lsdZHEV, lsdLENL, lsdLENM, lsdMASS, lsdV0 });
            p_ConvertUtils.GetAttribValues(*selRwallPlanar, doubleAttrNames, doubleAttrVals);

            EntityEdit radRwallEdit(p_radiossModel, radRwallHEdit);
            if (nsidHRead.IsValid())
            {
                EntityRead nsidRead(p_lsdynaModel, nsidHRead);
                radRwallEdit.SetValue(sdiIdentifier("grnd_ID1"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(nsidRead.GetId(), "*SET_NODE" ))));
            }
            if (nsidEXHRead.IsValid())
            {
                EntityRead nsidExRead(p_lsdynaModel, nsidEXHRead);
                radRwallEdit.SetValue(sdiIdentifier("grnd_ID2"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(nsidExRead.GetId(), "*SET_NODE" ))));
            }
            if (!nsidHRead.IsValid())
                radRwallEdit.SetValue(sdiIdentifier("d"), sdiValue(1E20));
            else
                radRwallEdit.SetValue(sdiIdentifier("d"), sdiValue(lsdOFFSET));

            if (lsdFRIC == 0.0 || lsdFRIC == 2.0)
                radRwallEdit.SetValue(sdiIdentifier("Slide"), sdiValue(0));
            else if (lsdFRIC == 1.0 || lsdFRIC == 3.0)
                radRwallEdit.SetValue(sdiIdentifier("Slide"), sdiValue(1));
            else
            {
                radRwallEdit.SetValue(sdiIdentifier("Slide"), sdiValue(2));
                radRwallEdit.SetValue(sdiIdentifier("fric"), sdiValue(lsdFRIC));
            }

            if (keyWord.find("MOVING") != keyWord.npos)
            {
                HandleNodeEdit baseNodeHEdit;
                p_radiossModel->CreateNode(baseNodeHEdit, "/NODE", sdiTriple(lsdXT, lsdYT, lsdZT));
                if (baseNodeHEdit.IsValid())
                {
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(baseNodeHEdit, sourcerList));
                    //radRwallEdit.SetEntityHandle(sdiIdentifier("node_ID"), baseNodeHEdit);
                    radRwallEdit.SetValue(sdiIdentifier("node_ID"), sdiValue(sdiValueEntity(1, baseNodeHEdit.GetId(p_radiossModel))));
                }
                radRwallEdit.SetValue(sdiIdentifier("Mass"), sdiValue(lsdMASS));
                sdiTriple normVector(lsdXH - lsdXT, lsdYH - lsdYT, lsdZH - lsdZT) ;
                normVector = normVector.Normalize();
                radRwallEdit.SetValue(sdiIdentifier("VX0"), sdiValue(lsdV0 * normVector[0]));
                radRwallEdit.SetValue(sdiIdentifier("VY0"), sdiValue(lsdV0 * normVector[1]));
                radRwallEdit.SetValue(sdiIdentifier("VZ0"), sdiValue(lsdV0 * normVector[2]));
            }
            else
            {
                radRwallEdit.SetValue(sdiIdentifier("XM"), sdiValue(lsdXT));
                radRwallEdit.SetValue(sdiIdentifier("YM"), sdiValue(lsdYT));
                radRwallEdit.SetValue(sdiIdentifier("ZM"), sdiValue(lsdZT));
            }
            if (isRwallFinite)
            {
                sdiTriple dir1Vect(lsdXHEV - lsdXT, lsdYHEV - lsdYT, lsdZHEV - lsdZT);
                dir1Vect = dir1Vect.Normalize();
                sdiTriple dir2Vect(sdiTriple(lsdXH - lsdXT, lsdYH - lsdYT, lsdZH - lsdZT) * dir1Vect);
                dir2Vect = dir2Vect.Normalize();
                radRwallEdit.SetValue(sdiIdentifier("XM1"), sdiValue(lsdXT + lsdLENL * dir1Vect[0]));
                radRwallEdit.SetValue(sdiIdentifier("YM1"), sdiValue(lsdYT + lsdLENL * dir1Vect[1]));
                radRwallEdit.SetValue(sdiIdentifier("ZM1"), sdiValue(lsdZT + lsdLENL * dir1Vect[2]));
                radRwallEdit.SetValue(sdiIdentifier("XM2"), sdiValue(lsdXT + lsdLENM * dir2Vect[0]));
                radRwallEdit.SetValue(sdiIdentifier("YM2"), sdiValue(lsdYT + lsdLENM * dir2Vect[1]));
                radRwallEdit.SetValue(sdiIdentifier("ZM2"), sdiValue(lsdZT + lsdLENM * dir2Vect[2]));
            }
            else
            {
                radRwallEdit.SetValue(sdiIdentifier("XM1"), sdiValue(lsdXH));
                radRwallEdit.SetValue(sdiIdentifier("YM1"), sdiValue(lsdYH));
                radRwallEdit.SetValue(sdiIdentifier("ZM1"), sdiValue(lsdZH));
            }
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radRwallHEdit, sourcerList));
        }
    }

}

void sdiD2R::ConvertRwall::p_ConvertRwallGeometric()
{
    SelectionRead selRwallGeom(p_lsdynaModel, "*RIGIDWALL_GEOMETRIC");
    while (selRwallGeom.Next())
    {
        double lsdXT;
        double lsdYT;
        double lsdZT;
        double lsdXH;
        double lsdYH;
        double lsdZH;
        double lsdFRIC;
        double lsdRadCyl;
        double lsdLenCyl;
        double lsdXHEV;
        double lsdYHEV;
        double lsdZHEV;
        double lsdLenL;
        double lsdLenM;
        double lsdRadSph;
        sdiStringList doubleAttNames({ "XT", "YT", "ZT", "XH", "YH", "ZH", "FRIC", "RADCYL", "LENCYL", "XHEV", "YHEV", "ZHEV", "LENL", "LENM", "RADSPH"});
        vector<reference_wrapper<double>> doubleAttValues({ lsdXT, lsdYT, lsdZT, lsdXH, lsdYH, lsdZH, lsdFRIC, lsdRadCyl, lsdLenCyl,lsdXHEV, lsdYHEV, lsdZHEV, lsdLenL, lsdLenM , lsdRadSph });
        p_ConvertUtils.GetAttribValues(*selRwallGeom, doubleAttNames, doubleAttValues);
        sdiString keyWord = selRwallGeom->GetKeyword();
        sdiString rwallName = selRwallGeom->GetName();
        sdiString destCard;
        int GeomType = 0; /* 1- paral, 2 - cyl, 3 - sphere*/
        if (keyWord.find("FLAT") != keyWord.npos || keyWord.find("PRISM") != keyWord.npos)
        {
            destCard = "/RWALL/PARAL";
            GeomType = 1;
        }
        else if (keyWord.find("CYLINDER") != keyWord.npos)
        {
            destCard = "/RWALL/CYL";
            GeomType = 2;
        }
        else if (keyWord.find("SPHERE") != keyWord.npos)
        {
            destCard = "/RWALL/SPHER";
            GeomType = 3;
        }

        if (GeomType)
        {
            HandleEdit radRwallHEdit;
            p_radiossModel->CreateEntity(radRwallHEdit, destCard, rwallName, selRwallGeom->GetId());
            if (radRwallHEdit.IsValid())
            {
                EntityEdit radRwallEdit(p_radiossModel, radRwallHEdit);
                p_ConvertRwallSets(*selRwallGeom, radRwallEdit);

                if (keyWord.find("MOTION") != keyWord.npos)
                    p_ConvertRwallMotion(*selRwallGeom, radRwallEdit);
                else
                {
                    radRwallEdit.SetValue(sdiIdentifier("XM"), sdiValue(lsdXT));
                    radRwallEdit.SetValue(sdiIdentifier("YM"), sdiValue(lsdYT));
                    radRwallEdit.SetValue(sdiIdentifier("ZM"), sdiValue(lsdZT));
                }

                if (lsdFRIC > 0.0)
                {
                    radRwallEdit.SetValue(sdiIdentifier("Slide"), sdiValue(2));
                    radRwallEdit.SetValue(sdiIdentifier("fric"), sdiValue(lsdFRIC));
                }
                if (GeomType == 2)
                {
                    radRwallEdit.SetValue(sdiIdentifier("XM1"), sdiValue(lsdXH));
                    radRwallEdit.SetValue(sdiIdentifier("YM1"), sdiValue(lsdYH));
                    radRwallEdit.SetValue(sdiIdentifier("ZM1"), sdiValue(lsdZH));
                    radRwallEdit.SetValue(sdiIdentifier("Diameter"), sdiValue(2 * lsdRadCyl));
                }
                else if (GeomType == 3)
                    radRwallEdit.SetValue(sdiIdentifier("Diameter"), sdiValue(2 * lsdRadSph));
                else
                {
                    lsdLenL = (lsdLenL == 0) ? 1E20 : lsdLenL;
                    lsdLenM = (lsdLenM == 0) ? 1E20 : lsdLenM;
                    sdiTriple vectH(lsdXH, lsdYH, lsdZH);
                    sdiTriple vectHEV(lsdXHEV, lsdYHEV, lsdZHEV);
                    sdiTriple vectT(lsdXT, lsdYT, lsdZT);
                    sdiTriple vectN(vectH - vectT);
                    vectN = vectN.Normalize();
                    sdiTriple vectL(vectHEV - vectT);
                    vectL = vectL.Normalize();
                    sdiTriple vectM(vectN * vectL);
                    sdiTriple vectM1 = vectT + (vectL * lsdLenL);
                    sdiTriple vectM2 = vectT + (vectM * lsdLenM);
                    sdiStringList tempList({ "X", "Y", "Z" });
                    for (size_t i = 0; i < tempList.size(); ++i)
                    {
                        radRwallEdit.SetValue(sdiIdentifier(tempList[i] + "M1"), sdiValue(vectM1[i]));
                        radRwallEdit.SetValue(sdiIdentifier(tempList[i] + "M2"), sdiValue(vectM2[i]));
                    }
                    if (keyWord.find("PRISM") != keyWord.npos)
                    {
                        p_ConvertRwallGeomPRISM(*selRwallGeom,radRwallEdit, sdiTripleList({ vectT, vectN, vectM, vectL }));
                    }
                }
                sdiConvert::SDIHandlReadList sourcerList = { {selRwallGeom->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(radRwallHEdit, sourcerList));
            }
        }
    }
}

void sdiD2R::ConvertRwall::p_ConvertRwallGeomPRISM(const EntityRead& dynaRwallRead, const EntityEdit& radRwallEdit, const sdiTripleList& tripleList)
{

    /* vectT index 0, vectN index 1, vectM index 2, vectL index 3*/
    const sdiTriple vectT = tripleList[0];
    const sdiTriple vectN = tripleList[1];
    const sdiTriple vectM = tripleList[2];
    const sdiTriple vectL = tripleList[3];
    const sdiString rwallName = radRwallEdit.GetName();
    const sdiString keyWord = dynaRwallRead.GetKeyword();
    sdiConvert::SDIHandlReadList sourceList = { {dynaRwallRead.GetHandle()} };
    sdiUIntList createdNodes;
    createdNodes.reserve(6);

    HandleEdit pris2Hedit;
    HandleEdit pris3Hedit;
    HandleEdit pris4Hedit;
    HandleEdit pris5Hedit;
    HandleEdit pris6Hedit;
    double lsdLenL = 0;
    double lsdLenM = 0;
    double lsdLenP = 0;
    sdiStringList doubleAttNames({ "LENL", "LENM","LENP"});
    vector<reference_wrapper<double>> doubleAttValues({ lsdLenL, lsdLenM , lsdLenP });
    p_ConvertUtils.GetAttribValues(dynaRwallRead, doubleAttNames, doubleAttValues);

    lsdLenL = (lsdLenL == 0.0) ? 1E20 : lsdLenL;
    lsdLenM = (lsdLenM == 0.0) ? 1E20 : lsdLenM;

    bool isMotion = false;
    if (keyWord.find("MOTION") != keyWord.npos)
    {
        isMotion = true;
        sdiValueEntity nodeIdEntity;
        sdiValue tempValue(nodeIdEntity);
        radRwallEdit.GetValue(sdiIdentifier("node_ID"), tempValue);
        tempValue.GetValue(nodeIdEntity);
        unsigned int nodeId = nodeIdEntity.GetId();
        if (nodeId)
            createdNodes.push_back(nodeId);
    }

    vector<reference_wrapper<HandleEdit>> newFlatEdits({ pris2Hedit , pris2Hedit , pris2Hedit , pris2Hedit, pris2Hedit });
    for (size_t i = 0; i < newFlatEdits.size(); ++i)
    {
        HandleEdit newFlatHEdit = newFlatEdits[i];
        p_radiossModel->CreateEntity(newFlatHEdit, "/RWALL/PARAL", rwallName + "_FLAT_" + to_string(i+2), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
        EntityEdit newFlatEdit(p_radiossModel, newFlatHEdit);
        sdiStringList attribList({ "Slide", "grnd_ID1", "fric","grnd_ID2" });
        for (size_t j = 0; j < attribList.size(); ++j)
        {
            sdiValue tempValue;
            radRwallEdit.GetValue(sdiIdentifier(attribList[j]), tempValue);
            newFlatEdit.SetValue(sdiIdentifier(attribList[j]), tempValue);
        }
        sdiStringList tempStrList({ "X", "Y", "Z" });
        sdiTriple vectXM(vectT);
        sdiTriple vectM1;
        sdiTriple vectM2;

        switch (i)
        {
        case 0:
            vectM1 = vectXM - vectN * lsdLenP;
            vectM2 = vectXM + vectL * lsdLenL;
            break;
        case 1:
            vectM1 = vectXM + vectM * lsdLenM;
            vectM2 = vectXM - vectN * lsdLenP;
            break;
        case 2:
            vectXM = vectXM + vectM * lsdLenM;
            vectM1 = vectXM + vectL * lsdLenL;
            vectM2 = vectXM - vectN * lsdLenP;
            break;
        case 3:
            vectXM = vectXM + vectM * lsdLenM + vectL * lsdLenL;
            vectM1 = vectXM - vectM * lsdLenM;
            vectM2 = vectXM - vectN * lsdLenP;
            break;
        case 4:
            vectXM = vectXM - vectN * lsdLenP ;
            vectM1 = vectXM + vectM * lsdLenM;
            vectM2 = vectXM + vectL * lsdLenL;
            break;
        default:
            break;
        }
        for (size_t k = 0; k < 3; ++k)
        {
            if (!isMotion)
                newFlatEdit.SetValue(sdiIdentifier(tempStrList[k] + "M"), sdiValue(vectXM[k]));
            newFlatEdit.SetValue(sdiIdentifier(tempStrList[k] + "M1"), sdiValue(vectM1[k]));
            newFlatEdit.SetValue(sdiIdentifier(tempStrList[k] + "M2"), sdiValue(vectM2[k]));
        }

        if (isMotion)
        {
            HandleNodeEdit nodeHEdit;
            p_radiossModel->CreateNode(nodeHEdit, "/NODE", vectXM);
            if (nodeHEdit.IsValid())
            {
                unsigned int nodeId = nodeHEdit.GetId(p_radiossModel);
                newFlatEdit.SetValue(sdiIdentifier("node_ID"), sdiValue(sdiValueEntity(1, nodeId)));
                createdNodes.push_back(nodeId);
                sdiConvert::Convert::PushToConversionLog(std::make_pair(newFlatHEdit, sourceList));
            }
        }
    }
    if (!createdNodes.empty())
    {
        HandleEdit radSetHEdit;
        EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
        sdiValueEntity setEntity;
        sdiValue tempVal;
        auto itr = RwallIdVsImpHandleMap.find(radRwallEdit.GetId());
        if (itr != RwallIdVsImpHandleMap.end())
        {
            HandleEdit impHandle = itr->second;
            EntityEdit impEdit(p_radiossModel, impHandle);
            sdiString impKeyWord = impEdit.GetKeyword();
            sdiString grNodAttr = (impKeyWord == "/IMPVEL") ? "grnod_ID" : "Gnod_id";

            tempVal = sdiValue(setEntity);
            impEdit.GetValue(sdiIdentifier(grNodAttr), tempVal);
            tempVal.GetValue(setEntity);
        }
        if (setEntity.GetId())
        {
            size_t nodeListSize = createdNodes.size();
            if (p_radiossModel->FindById(radSetType, setEntity.GetId(), radSetHEdit))
            {
                EntityEdit radSetEdit(p_radiossModel, radSetHEdit);
                radSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue((int)createdNodes.size()));
                radSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, createdNodes)));
            }
        }
    }
}

void sdiD2R::ConvertRwall::p_ConvertRwallSets(const EntityRead& dynaRwallRead, EntityEdit& radRwallEdit)
{
    sdiValueEntity nsidEntity;
    sdiValueEntity boxidEntity;
    sdiValueEntity nsidExEntity;
    sdiStringList entityAttNames({ "NSID", "BOXID", "NSIDEX" });
    vector<reference_wrapper<sdiValueEntity>> entityAttValues({ nsidEntity, boxidEntity, nsidExEntity });
    p_ConvertUtils.GetAttribValues(dynaRwallRead, entityAttNames, entityAttValues);
    unsigned int nsid = nsidEntity.GetId();
    unsigned int boxid = boxidEntity.GetId();
    unsigned int nsidEx = nsidExEntity.GetId();
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    sdiString rwallName = dynaRwallRead.GetName();

    if (nsid && boxid || nsid)
    {
        HandleRead nsidHRead;
        dynaRwallRead.GetEntityHandle(sdiIdentifier("NSID"), nsidHRead);
        if (nsidHRead.IsValid())
        {
            EntityRead nsidRead(p_lsdynaModel, nsidHRead);
            radRwallEdit.SetValue(sdiIdentifier("grnd_ID1"), sdiValue(sdiValueEntity(radSetType,
                DynaToRad::GetRadiossSetIdFromLsdSet(nsidRead.GetId(), "*SET_NODE" ))));
        }
    }
    else if (boxid)
    {

        HandleEdit radSetHEdit;
        p_radiossModel->CreateEntity(radSetHEdit, "/SET/GENERAL", rwallName);
        EntityEdit radSetEdit(p_radiossModel, radSetHEdit);
        radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
        radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("BOX")));
        radSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
        radSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/BOX"), sdiUIntList(1, boxid))));
        radRwallEdit.SetEntityHandle(sdiIdentifier("grnd_ID1"), radSetHEdit);
        sdiConvert::SDIHandlReadList sourceHandleList = { {dynaRwallRead.GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(radSetHEdit, sourceHandleList));
    }
    else
    {
        HandleEdit radSetHEdit;
        p_radiossModel->CreateEntity(radSetHEdit, "/SET/GENERAL", rwallName);
        EntityEdit radSetEdit(p_radiossModel, radSetHEdit);
        radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
        radSetEdit.SetValue(sdiIdentifier("KEY_type"), sdiValue(sdiString("ALL")));
        radRwallEdit.SetEntityHandle(sdiIdentifier("grnd_ID1"), radSetHEdit);
        sdiConvert::SDIHandlReadList sourceHandleList = { {dynaRwallRead.GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(radSetHEdit, sourceHandleList));
    }
    if (nsidEx)
    {
        HandleRead nsidExHRead;
        dynaRwallRead.GetEntityHandle(sdiIdentifier("NSIDEX"), nsidExHRead);
        if (nsidExHRead.IsValid())
        {
            EntityRead nsidExRead(p_lsdynaModel, nsidExHRead);
            radRwallEdit.SetValue(sdiIdentifier("grnd_ID2"), sdiValue(sdiValueEntity(radSetType,
                DynaToRad::GetRadiossSetIdFromLsdSet(nsidExRead.GetId(), "*SET_NODE" ))));
        }
    }
}

void sdiD2R::ConvertRwall::p_ConvertRwallMotion(const EntityRead& dynaRwallRead, EntityEdit& radRwallEdit)
{
    double lsdXT;
    double lsdYT;
    double lsdZT;
    double lsdVX;
    double lsdVY;
    double lsdVZ;
    sdiStringList doubleAttNames({ "XT", "YT", "ZT", "VX", "VY", "VZ" });
    vector<reference_wrapper<double>> doubleAttValues({ lsdXT, lsdYT, lsdZT, lsdVX, lsdVY, lsdVZ });
    p_ConvertUtils.GetAttribValues(dynaRwallRead, doubleAttNames, doubleAttValues);

    int lsdOPT = 0;
    sdiValue tempVal(lsdOPT);
    dynaRwallRead.GetValue(sdiIdentifier("OPT"), tempVal);
    tempVal.GetValue(lsdOPT);

    sdiValueEntity lsdLCIDEntity;
    tempVal = sdiValue(lsdLCIDEntity);
    dynaRwallRead.GetValue(sdiIdentifier("LCID"), tempVal);
    tempVal.GetValue(lsdLCIDEntity);
    unsigned int lcid = lsdLCIDEntity.GetId();
    sdiString rwallName = dynaRwallRead.GetName();
    EntityType radFucntType = p_radiossModel->GetEntityType("/FUNCT");
    EntityType radNodeType =  p_radiossModel->GetEntityType("/NODE");
    EntityType radSetType =  p_radiossModel->GetEntityType("/SET/GENERAL");
    sdiConvert::SDIHandlReadList sourceList = { {dynaRwallRead.GetHandle()} };

    HandleEdit radSetHEdit;
    sdiTriple nodePos(lsdXT, lsdYT, lsdZT);
    HandleNodeEdit nodeHEdit;
    p_radiossModel->CreateNode(nodeHEdit, "/NODE", nodePos);
    if (nodeHEdit.IsValid())
    {
        unsigned int nodeId =  nodeHEdit.GetId(p_radiossModel);
        radRwallEdit.SetValue(sdiIdentifier("node_ID"), sdiValue(sdiValueEntity(radNodeType, nodeId)));
        sdiConvert::Convert::PushToConversionLog(std::make_pair(nodeHEdit, sourceList));

        p_radiossModel->CreateEntity(radSetHEdit, "/SET/GENERAL", rwallName);
        if (radSetHEdit.IsValid())
        {
            EntityEdit radSetEdit(p_radiossModel, radSetHEdit);
            radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
            radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
            radSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
            if (dynaRwallRead.GetKeyword().find("PRISM") == string::npos)
                radSetEdit.SetValue(sdiIdentifier("ids", 0, 0, 0), sdiValue(sdiValueEntityList(radNodeType, sdiUIntList(1, nodeId))));
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radSetHEdit, sourceList));
        }
    }

    if (lcid)
    {
        sdiString impCard = (lsdOPT == 0) ? "/IMPVEL" : "/IMPDISP";
        sdiString fucntIdentfier = (lsdOPT == 0) ? "funct_IDT" : "Ifunct";
        sdiString grnodIdentfier = (lsdOPT == 0) ? "grnod_ID" : "Gnod_id";
        sdiString dirIdentifier = (lsdOPT == 0) ? "Dir" : "DIR";

        HandleEdit impHEdit;
        p_radiossModel->CreateEntity(impHEdit, impCard, rwallName);
        EntityEdit impEdit(p_radiossModel, impHEdit);

        impEdit.SetValue(sdiIdentifier("displayname"), sdiValue(rwallName));
        impEdit.SetValue(sdiIdentifier(fucntIdentfier), sdiValue(sdiValueEntity(radFucntType, lcid)));
        impEdit.SetValue(sdiIdentifier(dirIdentifier), sdiValue(sdiString("X")));
        if (radSetHEdit.IsValid())
            impEdit.SetEntityHandle(sdiIdentifier(grnodIdentfier), radSetHEdit);

        sdiConvert::Convert::PushToConversionLog(std::make_pair(impHEdit, sourceList));
        RwallIdVsImpHandleMap[radRwallEdit.GetId()] = impHEdit;
        /*create node set*/

        if (lsdVX != 0.0 || lsdVY != 0.0 || lsdVZ != 0.0)
        {
            sdiTriple origin(0.0, 0.0, 0.0);
            origin = origin.Normalize();
            sdiTriple inputDirVect(lsdVX, lsdVY, lsdVZ);
            sdiTriple dir1Vect;
            if ((inputDirVect * sdiTriple(0.0, 0.0, 1.0)).Len() != 0.0)
                dir1Vect = sdiTriple(0.0, 0.0, 1.0) * inputDirVect;
            else
                dir1Vect = sdiTriple(1.0, 0.0, 0.0) * inputDirVect;
            sdiTriple dir2Vect(inputDirVect * dir1Vect);

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
                    radSkewEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(dir1Vect[i]));
                    radSkewEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(dir2Vect[i]));
                }
                radSkewEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(sdiString(rwallName)));
                impEdit.SetValue(sdiIdentifier("rad_system_input_type"), sdiValue(1));
                impEdit.SetEntityHandle(sdiIdentifier("inputsystem"), radSkewHEdit);

                sdiConvert::Convert::PushToConversionLog(std::make_pair(radSkewHEdit, sourceList));
            }
        }
    }
}

void sdiD2R::ConvertRwall::p_CreateThRwall()
{
    SelectionRead selDbRwallForce(p_lsdynaModel, "*DATABASE_RWFORC");
    if (selDbRwallForce.Count())
    {
        sdiUIntList rwallList;
        SelectionRead selRadRwall(p_radiossModel, destCard);
        rwallList.reserve(selRadRwall.Count());
        while (selRadRwall.Next())
            rwallList.push_back(selRadRwall->GetId());

        if (!rwallList.empty())
        {
            HandleEdit radThRwallHEdit;
            p_radiossModel->CreateEntity(radThRwallHEdit, "/TH/RWALL", "TH-RWALL");
            EntityEdit radThRwallEdit(p_radiossModel, radThRwallHEdit);

            radThRwallEdit.SetValue(sdiIdentifier("idsmax"), sdiValue((int)rwallList.size()));
            radThRwallEdit.SetValue(sdiIdentifier("Number_Of_Variables"), sdiValue(1));
            radThRwallEdit.SetValue(sdiIdentifier("Obj"), sdiValue(sdiValueEntityList(destEntityType, rwallList)));
            radThRwallEdit.SetValue(sdiIdentifier("var"), sdiValue(sdiStringList({ "DEF" })));

            sdiConvert::SDIHandlReadList sourceHandles = { {selDbRwallForce->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radThRwallHEdit, sourceHandles));
        }
    }
}
