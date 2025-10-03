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
#include <dyna2rad/convertsystems.h>
#include <typedef.h>

using namespace sdiD2R;
using namespace sdi;
using namespace std;

void ConvertSystem::ConvertSystems()
{
    ConvertEntities();
}

void ConvertSystem::ConvertEntities()
{
    p_ConvertCoordinateSystems();

    p_ConvertCoordinateNodes();

    p_ConvertSDOrientation();

    p_ConvertCoordinateVector();

    p_ConvertDefineVector();
}

void ConvertSystem::p_ConvertCoordinateSystems()
{
    SelectionRead systSelect(p_lsdynaModel, "*DEFINE_COORDINATE_SYSTEM");
    while (systSelect.Next())
    {
        double lsdXO;
        double lsdYO;
        double lsdZO;
        double lsdXL;
        double lsdYL;
        double lsdZL;
        double lsdXP;
        double lsdYP;
        double lsdZP;
        vector<reference_wrapper<double>> attribVals({ lsdXO, lsdYO, lsdZO, lsdXL, lsdYL, lsdZL, lsdXP, lsdYP, lsdZP });
        vector<sdiString> attribNames({ "XO", "YO", "ZO", "XL", "YL", "ZL" , "XP", "YP", "ZP" });
        p_ConvertUtils.GetAttribValues(*systSelect, attribNames, attribVals);

        HandleEdit skewHandle;
        p_radiossModel->CreateEntity(skewHandle, "/SKEW/FIX", systSelect->GetName(), systSelect->GetId());
        EntityEdit skewEntityEdit(p_radiossModel, skewHandle);
        vector<sdiString> radAttNames({ "Ox", "Oy", "Oz" });
        skewEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(systSelect->GetName()));
        sdiTriple vectXL({ (lsdXL - lsdXO), (lsdYL - lsdYO), (lsdZL - lsdZO) });
        vectXL.Normalize();
        sdiTriple vectXP({ (lsdXP - lsdXO), (lsdYP - lsdYO), (lsdZP - lsdZO) });
        vectXP.Normalize();

        sdiTriple skewVect2 = vectXL * vectXP ;
        skewVect2.Normalize();
        sdiTriple skewVect1 = skewVect2 * vectXL;
        skewVect2.Normalize();
        vector<sdiString> attribVect1({ "X1", "Y1", "Z1" });
        vector<sdiString> attribVect2({ "X2", "Y2", "Z2" });

        if (skewHandle.IsValid())
        {
            for (size_t i = 0; i < 3; i++)
            {
                skewEntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(attribVals[i]));
                skewEntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(skewVect1[i]));
                skewEntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(skewVect2[i]));
            }
            sdiConvert::SDIHandlReadList sourceSyst = { {systSelect->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(skewHandle, sourceSyst));
        }
    }
}

void ConvertSystem::p_ConvertCoordinateNodes()
{
    EntityType radNodeType = p_radiossModel->GetEntityType("/NODE");
    EntityType lsdNodeType = p_lsdynaModel->GetEntityType("*NODE");
    SelectionRead systSelect(p_lsdynaModel, "*DEFINE_COORDINATE_NODES");
    while(systSelect.Next())
    {
        HandleRead handleN1;
        HandleRead handleN2;
        HandleRead handleN3;
        vector<sdiString> attribName({"N1", "N2", "N3"});
        vector<reference_wrapper<HandleRead>> attribVals({ handleN1, handleN2, handleN3 });
        p_ConvertUtils.GetEntityHandles(*systSelect, attribName, attribVals);

        if (!handleN1.IsValid() || !handleN2.IsValid() || !handleN3.IsValid())
            continue;
        int lsdFlag = 0;
        sdiValue tempVal(lsdFlag);
        systSelect->GetValue(sdiIdentifier("FLAG"), tempVal);
        tempVal.GetValue(lsdFlag);

        sdiString lsdDir;
        tempVal = sdiValue(lsdDir);
        systSelect->GetValue(sdiIdentifier("DIR"), tempVal);
        tempVal.GetValue(lsdDir);
        if(lsdDir == "" ) lsdDir = "X";
        HandleEdit skewHandle;
        if (lsdFlag == 1)
        {
            p_radiossModel->CreateEntity(skewHandle, "/SKEW/MOV", systSelect->GetName(), systSelect->GetId());
            EntityEdit skewEntityEdit(p_radiossModel, skewHandle);
            for (size_t i = 0; i < 3; ++i)
            {
                skewEntityEdit.SetValue(sdiIdentifier(attribName[i]),sdiValue(sdiValueEntity(sdiValueEntityType(radNodeType), attribVals[i].get().GetId(p_lsdynaModel))));
            }
            skewEntityEdit.SetValue(sdiIdentifier("DIR"), sdiValue(lsdDir));
            skewEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(systSelect->GetName()));
        }
        else if (lsdFlag == 0)
        {
            p_radiossModel->CreateEntity(skewHandle, "/SKEW/FIX", systSelect->GetName(), systSelect->GetId());
            EntityEdit skewEntityEdit(p_radiossModel, skewHandle);

            skewEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(systSelect->GetName()));
            NodeRead nodeReadN1(p_lsdynaModel, handleN1);
            sdiTriple posN1 = nodeReadN1.GetPosition();

            NodeRead nodeReadN2(p_lsdynaModel, handleN2);
            sdiTriple posN2 = nodeReadN2.GetPosition();

            NodeRead nodeReadN3(p_lsdynaModel, handleN3);
            sdiTriple posN3 = nodeReadN3.GetPosition();

            sdiTriple vectN1N2 = posN2 - posN1;
            vectN1N2.Normalize();
            sdiTriple vectN1N3 = posN3 - posN1;
            vectN1N3.Normalize();

            sdiTriple vectZ = vectN1N2 * vectN1N3;
            vectZ.Normalize();
            sdiTriple vectY = vectZ * vectN1N2;
            vectY.Normalize();
            if (lsdDir == "Y")
            {
                vectZ = sdiTriple(vectY);
                vectY = sdiTriple(vectN1N2);
            }
            else if (lsdDir == "Z")
            {
                vectY = sdiTriple(vectZ);
                vectZ = sdiTriple(vectN1N2);
            }
            vector<sdiString> attribOrigin({ "Ox", "Oy", "Oz" });
            vector<sdiString> attribVect1({ "X1", "Y1", "Z1" });
            vector<sdiString> attribVect2({ "X2", "Y2", "Z2" });
            for (size_t i = 0; i < 3; ++i)
            {
                skewEntityEdit.SetValue(sdiIdentifier(attribOrigin[i]), sdiValue(posN1[i]));
                skewEntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(vectY[i]));
                skewEntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(vectZ[i]));
            }

        }
        if (skewHandle.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceSyst = { {systSelect->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(skewHandle, sourceSyst));
        }
    }
}

void ConvertSystem::p_ConvertSDOrientation()
{
    EntityType radNodeType = p_radiossModel->GetEntityType("/NODE");
    EntityType lsdNodeType = p_lsdynaModel->GetEntityType("*NODE");
    SelectionRead selSdOrient(p_lsdynaModel, "*DEFINE_SD_ORIENTATION");
    while (selSdOrient.Next())
    {
        int lsdIOP = - 1;
        sdiValue tempVal(lsdIOP);
        selSdOrient->GetValue(sdiIdentifier("IOP"), tempVal);
        tempVal.GetValue(lsdIOP);
        HandleEdit skewHandle;
        if (lsdIOP == 0)
        {
            double lsdXT;
            double lsdYT;
            double lsdZT;
            vector<reference_wrapper<double>> attribVals({ lsdXT, lsdYT, lsdZT });
            vector<sdiString> attribNames({ "XT", "YT", "ZT" });
            p_ConvertUtils.GetAttribValues(*selSdOrient, attribNames, attribVals);
            sdiTriple xTyTzT(lsdXT, lsdYT, lsdZT);
            xTyTzT.Normalize();
            sdiTriple origin(0.0,0.0,0.0);
            sdiTriple vectx1y1z1;
            if (!(abs(lsdXT - 1.0) <= 0.000001 && abs(lsdYT - 0) <= 0.000001 && abs(lsdZT - 0) <= 0.000001))
                vectx1y1z1 = sdiTriple(1.0, 0.0, 0.0);
            else if (!((lsdXT - 0.0) <= 0.0000001 && abs(lsdYT - 1.0) <= 0.0000001 && abs(lsdZT - 0) <= 0.0000001))
                vectx1y1z1 = sdiTriple(0.0, 1.0, 0.0);
            sdiTriple vectx2y2z2 = sdiTriple(lsdXT, lsdYT, lsdZT) * vectx1y1z1;
            vectx2y2z2.Normalize();
            p_radiossModel->CreateEntity(skewHandle, "/SKEW/FIX", selSdOrient->GetName());
            EntityEdit skewEntityEdit(p_radiossModel, skewHandle);
            skewEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(selSdOrient->GetName()));
            vector<sdiString> attribOrigin({ "Ox", "Oy", "Oz" });
            vector<sdiString> attribVect1({ "X1", "Y1", "Z1" });
            vector<sdiString> attribVect2({ "X2", "Y2", "Z2" });
            for (size_t i = 0; i < 3; ++i)
            {
                skewEntityEdit.SetValue(sdiIdentifier(attribOrigin[i]), sdiValue(origin[i]));
                skewEntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(vectx1y1z1[i]));
                skewEntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(vectx2y2z2[i]));
            }
        }
        else if (lsdIOP == 2)
        {
            HandleRead hReadNode1;
            HandleRead hReadNode2;
            vector<reference_wrapper<HandleRead>> attribVals({ hReadNode1, hReadNode2 });
            vector<sdiString> attribNames({ "NID1", "NID2" });
            p_ConvertUtils.GetEntityHandles(*selSdOrient, attribNames, attribVals);
            if (!hReadNode1.IsValid() || !hReadNode2.IsValid())
                continue;
            NodeRead node1Read(p_lsdynaModel, hReadNode1);
            NodeRead node2Read(p_lsdynaModel, hReadNode2);

            sdiTriple node1pos = node1Read.GetPosition();
            sdiTriple node2pos = node2Read.GetPosition();
            sdiTriple node3pos;
            sdiTriple vectN1N2 = node2pos - node1pos;
            vectN1N2.Normalize();
            sdiTriple globalXDir(1.0, 0.0, 0.0);
            sdiTriple globalYDir(0.0, 1.0, 0.0);
            if ((vectN1N2 * globalXDir).Len() != 0.0)
                node3pos = node1pos + globalXDir;
            else if ((vectN1N2 * globalYDir).Len() != 0.0)
                node3pos = node1pos + sdiTriple(0.0, 1.0, 0.0);
            if (node3pos.Length() != 0.0)
            {
                HandleNodeEdit node3HandleEdit;
                p_radiossModel->CreateNode(node3HandleEdit, "/NODE", node3pos);

                p_radiossModel->CreateEntity(skewHandle, "/SKEW/MOV");
                EntityEdit skewEntityEdit(p_radiossModel, skewHandle);
                skewEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(selSdOrient->GetName()));
                skewEntityEdit.SetValue(sdiIdentifier("N1"), sdiValue(sdiValueEntity(sdiValueEntityType(radNodeType), node1Read.GetId())));
                skewEntityEdit.SetValue(sdiIdentifier("N2"), sdiValue(sdiValueEntity(sdiValueEntityType(radNodeType), node2Read.GetId())));
                skewEntityEdit.SetValue(sdiIdentifier("N3"), sdiValue(sdiValueEntity(sdiValueEntityType(radNodeType), node3HandleEdit.GetId(p_radiossModel))));
            }
        }
        if (skewHandle.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceSyst = { {selSdOrient->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(skewHandle, sourceSyst));
            DynaToRad::storeLsdSDorientIdVsSkewId[selSdOrient->GetId()] = skewHandle.GetId(p_radiossModel);
        }
    }
}

void ConvertSystem::p_ConvertCoordinateVector()
{
    SelectionRead selCoordVect(p_lsdynaModel, "*DEFINE_COORDINATE_VECTOR");
    while (selCoordVect.Next())
    {
        double lsdXX;
        double lsdYX;
        double lsdZX;
        double lsdXV;
        double lsdYV;
        double lsdZV;

        vector<reference_wrapper<double>> attribVals({ lsdXX, lsdYX, lsdZX, lsdXV, lsdYV, lsdZV });
        vector<sdiString> attribNames({ "XX", "YX", "ZX", "XV", "YV", "ZV"});
        p_ConvertUtils.GetAttribValues(*selCoordVect, attribNames, attribVals);

        sdiTriple locXVect(lsdXX, lsdYX, lsdZX);
        sdiTriple locZVect(locXVect * sdiTriple(lsdXV, lsdYV, lsdZV));
        sdiTriple locYVect(locZVect * locXVect);

        sdiTriple origin(0.0, 0.0, 0.0);
        HandleEdit skewHandle;
        p_radiossModel->CreateEntity(skewHandle, "/SKEW/FIX", selCoordVect->GetName(), selCoordVect->GetId());
        EntityEdit skewEntityEdit(p_radiossModel, skewHandle);
        skewEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(selCoordVect->GetName()));
        vector<sdiString> originAttr({ "Ox", "Oy", "Oz" });
        vector<sdiString> attribVect1({ "X1", "Y1", "Z1" });
        vector<sdiString> attribVect2({ "X2", "Y2", "Z2" });
        for (size_t i = 0; i < 3; ++i)
        {
            skewEntityEdit.SetValue(sdiIdentifier(originAttr[i]), sdiValue(origin[i]));
            skewEntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(locYVect[i]));
            skewEntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(locZVect[i]));
        }
        sdiConvert::SDIHandlReadList sourceVect = { {selCoordVect->GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(skewHandle, sourceVect));

    }
}

void ConvertSystem::p_ConvertDefineVector()
{
    EntityType lsdDefCoordType = p_lsdynaModel->GetEntityType("*DEFINE_COORDINATE");
    SelectionRead selDefineVect(p_lsdynaModel, "*DEFINE_VECTOR");
    while (selDefineVect.Next())
    {
        sdiString selDefineVectKeyWord = selDefineVect->GetKeyword();
        if (selDefineVectKeyWord.find("NODES") != selDefineVectKeyWord.npos)
        {
            HandleRead lsdNT;
            HandleRead lsdNH;
            vector<sdiString> attribName({"NODET", "NODEH"});
            vector<reference_wrapper<HandleRead>> attribVals({ lsdNT, lsdNH, });
            p_ConvertUtils.GetEntityHandles(*selDefineVect, attribName, attribVals);

            HandleEdit skewHEdit;

            if (lsdNT.IsValid() && lsdNH.IsValid())
            {
                NodeRead node1Read(p_lsdynaModel, lsdNT);
                NodeRead node2Read(p_lsdynaModel, lsdNH);
                unsigned int nodetId = node1Read.GetId();
                unsigned int nodehId = node2Read.GetId();

                sdiTriple node1pos = node1Read.GetPosition();
                sdiTriple node2pos = node2Read.GetPosition();
                sdiTriple node3pos;
                node3pos = sdiTriple( node1pos.GetY()*node2pos.GetZ()-node1pos.GetZ()*node2pos.GetY(), 
                                     node1pos.GetZ()*node2pos.GetX()-node1pos.GetX()*node2pos.GetZ(),
                                     node1pos.GetX()*node2pos.GetY()-node1pos.GetY()*node2pos.GetX() );

                HandleNodeEdit node3HandleEdit;
                p_radiossModel->CreateNode(node3HandleEdit, "/NODE", node3pos);
                NodeRead node3Read(p_radiossModel, node3HandleEdit);

                unsigned int vid = selDefineVect->GetId();
                p_radiossModel->CreateEntity(skewHEdit, "/SKEW/MOV", "SKEW_MOV_"+ selDefineVectKeyWord +"_" + to_string(vid), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType)); 
                EntityEdit skewEntityEdit(p_radiossModel, skewHEdit);
                skewEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(selDefineVect->GetName()));
                skewEntityEdit.SetValue(sdiIdentifier("N1"), sdiValue(sdiValueEntity(sdiValueEntityType(1), nodetId)));
                skewEntityEdit.SetValue(sdiIdentifier("N2"), sdiValue(sdiValueEntity(sdiValueEntityType(1), nodehId)));
                skewEntityEdit.SetValue(sdiIdentifier("N3"), sdiValue(sdiValueEntity(sdiValueEntityType(1), node3Read.GetId())));
                skewEntityEdit.SetValue(sdiIdentifier("DIR"), sdiValue(sdiString("X")));
            }
            else
            {
                DynaToRad::ShowMessage(sdiMessageHandler ::Level::Error, 36,
                    selDefineVectKeyWord.c_str(), selDefineVect->GetId(), 
                    selDefineVect->GetName().c_str());
            }
            if (skewHEdit.IsValid())
            {
                sdiConvert::SDIHandlReadList sourceVect = { {selDefineVect->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(skewHEdit, sourceVect));
            }
        }
        else
        {
            double lsdXT;
            double lsdYT;
            double lsdZT;
            double lsdXH;
            double lsdYH;
            double lsdZH;
            sdiStringList attrNameList({ "XT", "YT", "ZT", "XH", "YH", "ZH" });
            vector<reference_wrapper<double>> attrVals({ lsdXT, lsdYT, lsdZT, lsdXH, lsdYH, lsdZH });
            p_ConvertUtils.GetAttribValues(*selDefineVect, attrNameList, attrVals);

            int CID = 0;
            sdiValueEntity lsdCID = GetValue<sdiValueEntity>(*selDefineVect, "CID");
            CID = lsdCID.GetId();

            sdiTriple origin(0,0,0);
            sdiTriple localXDir(0,0,0);
            sdiTriple vectorYprime(0,0,0);
            sdiTriple localZDir(0,0,0);
            sdiTriple localYDir(0,0,0);
            if (CID == 0)
            {
                origin = sdiTriple(lsdXT, lsdYT, lsdZT);
                localXDir = origin - sdiTriple(lsdXH, lsdYH, lsdZH);
                localXDir = localXDir.Normalize();
                vectorYprime = localXDir * sdiTriple(0, 0, 1);
                if (vectorYprime == sdiTriple(0,0,0))
                    vectorYprime = localXDir * sdiTriple(1, 0, 0);
                vectorYprime = vectorYprime.Normalize();
                localZDir = vectorYprime * localXDir;
                localZDir = localZDir.Normalize();
                localYDir =  localXDir * localZDir;
                localYDir = localYDir.Normalize();
            }
            else
            {
                HandleRead skewHRead;
                selDefineVect->GetEntityHandle(sdiIdentifier("CID"), skewHRead);
                EntityRead skewRead(p_lsdynaModel, skewHRead);
                double lsdX0 = GetValue<double>(skewRead, "globaloriginx");
                double lsdY0 = GetValue<double>(skewRead, "globaloriginy");
                double lsdZ0 = GetValue<double>(skewRead, "globaloriginz");
                double lsdXL = GetValue<double>(skewRead, "xaxisx");
                double lsdYL = GetValue<double>(skewRead, "xaxisy");
                double lsdZL = GetValue<double>(skewRead, "xaxisz");
                double lsdXP = GetValue<double>(skewRead, "SYST_XYx");
                double lsdYP = GetValue<double>(skewRead, "SYST_XYy");
                double lsdZP = GetValue<double>(skewRead, "SYST_XYz");

                sdiTriple vectXL({ (lsdXL - lsdX0), (lsdYL - lsdY0), (lsdZL - lsdZ0) });
                vectXL.Normalize();
                sdiTriple vectXP({ (lsdXP - lsdX0), (lsdYP - lsdY0), (lsdZP - lsdZ0) });
                vectXP.Normalize();

                sdiTriple skewZaxis = vectXL * vectXP ;
                skewZaxis.Normalize();

                origin = sdiTriple(lsdXT, lsdYT, lsdZT);
                localXDir = origin - sdiTriple(lsdXH, lsdYH, lsdZH);
                localXDir = localXDir.Normalize();
                vectorYprime = localXDir * skewZaxis;
                if(vectorYprime == sdiTriple(0,0,0))
                    vectorYprime = localXDir * vectXL;
                localZDir = vectorYprime * localXDir;
                localZDir = localZDir.Normalize();
                localYDir = localXDir * localZDir;
                localYDir = localYDir.Normalize();
            }

            HandleEdit skewHandle;
            unsigned int defVectId = selDefineVect->GetId();
            if (p_radiossModel->IsIdAvailable(destEntityType, defVectId))
                p_radiossModel->CreateEntity(skewHandle, "/SKEW/FIX", selDefineVect->GetName(), defVectId);
            else
            {
                defVectId = p_radiossModel->GetNextAvailableId(destEntityType);
                p_radiossModel->CreateEntity(skewHandle, "/SKEW/FIX", selDefineVect->GetName(), defVectId);
            }

            DynaToRad::PushIntoStoreLsdVIDVsRadSkewId(selDefineVect->GetId(), defVectId);

            EntityEdit skewEntityEdit(p_radiossModel, skewHandle);
            skewEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(selDefineVect->GetName()));

            vector<sdiString> originAttr({ "Ox", "Oy", "Oz" });
            vector<sdiString> attribVect1({ "X1", "Y1", "Z1" });
            vector<sdiString> attribVect2({ "X2", "Y2", "Z2" });
            for (size_t i = 0; i < 3; ++i)
            {
                skewEntityEdit.SetValue(sdiIdentifier(originAttr[i]), sdiValue(origin[i]));
                skewEntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(localYDir[i]));
                skewEntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(localZDir[i]));
            }
            if (skewHandle.IsValid())
            {
                sdiConvert::SDIHandlReadList sourceVect = { {selDefineVect->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(skewHandle, sourceVect));
            }
        }
    }
}
