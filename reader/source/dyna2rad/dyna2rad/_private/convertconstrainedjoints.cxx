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


#include <dyna2rad/convertconstrainedjoints.h>
#include <dyna2rad/dyna2rad.h>
#include <dyna2rad/sdiUtils.h>

#include <cfgkernel/sdi/sdiModelViewCFG.h>


using namespace sdi;
using namespace std;

void sdiD2R::ConvertJoint::ConvertConstrainedJoints()
{
    ConvertEntities();
}

void sdiD2R::ConvertJoint::ConvertEntities()
{
    ConvertStiffGenJoints();

    ConvertStiffTransJoints();

    ConvertRegularJoints();
}


void sdiD2R::ConvertJoint::ConvertStiffGenJoints()
{
    EntityType radSkewType = p_radiossModel->GetEntityType("/SKEW");
    SelectionRead selConsStiffGen(p_lsdynaModel, "*CONSTRAINED_JOINT_STIFFNESS_GENERALIZED");
    unsigned int numStiffGenJnts = selConsStiffGen.Count();
    EntityType lsdDefCoordType = p_lsdynaModel->GetEntityType("*DEFINE_COORDINATE");

    if (numStiffGenJnts)
    {
        size_t reserveCapacity = (size_t)numStiffGenJnts * 3;
        convertedJoints.reserve(reserveCapacity);
        PopulateConstExtraNodePartRelation();
        EntityType radCrvType = p_radiossModel->GetEntityType("/FUNCT");

        while (selConsStiffGen.Next())
        {
            HandleRead pidAHread;
            HandleRead pidBHread;
            HandleRead cidAHread;
            HandleRead cidBHread;
            HandleRead jidHread;
            HandleRead lciphHread;
            HandleRead lcidtHread;
            HandleRead lcidpsHread;
            HandleRead dlcidpHread;
            HandleRead dlcidtHread;
            HandleRead dlcidpsHread;
            HandleRead fmphCrvHread;
            HandleRead fmtCrvHread;
            HandleRead fmpsCrvHread;

            sdiStringList attrNameList({ "PIDA", "PIDB", "CIDA", "CIDB", "JID", "LCIDPH",
                                        "LCIDT", "LCIDPS", "DLCIDPH", "DLCIDT", "DLCIDPS",
                                        "LSD_FMPH_CURVE", "LSD_FMT_CURVE", "LSD_FMPS_CURVE" });
            vector<reference_wrapper<HandleRead>> entityHandlesList({ pidAHread , pidBHread, cidAHread, cidBHread,
                                                                      jidHread, lciphHread, lcidtHread, lcidpsHread,
                                                                      dlcidpHread, dlcidtHread, dlcidpsHread, 
                                                                      fmphCrvHread, fmtCrvHread, fmpsCrvHread });

            p_ConvertUtils.GetEntityHandles(*selConsStiffGen, attrNameList, entityHandlesList);

            double lsdESPH;
            double lsdFMPH;
            double lsdEST;
            double lsdFMT;
            double lsdESPS;
            double lsdFMPS;
            double lsdNSAPH;
            double lsdPSAPH;
            double lsdNSAT;
            double lsdPSAT;
            double lsdNSAPS;
            double lsdPSAPS;

            attrNameList = { { "ESPH", "LSD_FMPH", "EST", "LSD_FMT", "ESPS", "LSD_FMPS", "NSAPH", "PSAPH", "NSAT", "PSAT", "NSAPS", "PSAPS" } };
            vector<reference_wrapper<double>> dblAttrList({ lsdESPH, lsdFMPH, lsdEST, lsdFMT, lsdESPS, lsdFMPS, lsdNSAPH, lsdPSAPH, lsdNSAT, lsdPSAT, lsdNSAPS, lsdPSAPS });

            p_ConvertUtils.GetAttribValues(*selConsStiffGen, attrNameList, dblAttrList);

            sdiUIntList nodeList;
            sdiString keyWord;
            int jntType = 0;
            unsigned int jntId = 0;
            
            if (!jidHread.IsValid())
            {
                /*find the joint*/
                sdiUIntList partsNodeListA;
                sdiUIntList partsNodeListB;
                unsigned int partIdA = pidAHread.GetId(p_lsdynaModel);
                unsigned int partIdB = pidBHread.GetId(p_lsdynaModel);
                p_ConvertUtils.GetNodesOfParts(partIdA, partsNodeListA);
                p_ConvertUtils.GetNodesOfParts(partIdB, partsNodeListB);
                sdiVectorSort(partsNodeListA);
                sdiVectorSort(partsNodeListB);

                for (const unsigned int partId : {partIdA, partIdB})
                {
                    if (!mapPartIdVsExtraNodeList[partId].empty())
                    {
                        for (const unsigned int nodeId : mapPartIdVsExtraNodeList[partId])
                        {
                            if(partId == partIdA)partsNodeListA.push_back(nodeId);
                            if(partId == partIdB)partsNodeListB.push_back(nodeId);
                        }
                    }
                }

                sdiVectorSort(partsNodeListA);
                sdiVectorSort(partsNodeListB);

                if (!partsNodeListA.empty() && !partsNodeListB.empty())
                {
                    SelectionRead selConstrJnts(p_lsdynaModel, srcCard);
                    while (selConstrJnts.Next())
                    {
                        jntId = 0;
                        keyWord = selConstrJnts->GetKeyword();
                        if (keyWord.find("SPH") != keyWord.npos)
                            jntType = 1;
                        else if (keyWord.find("REV") != keyWord.npos)
                            jntType = 2;
                        else if (keyWord.find("CYL") != keyWord.npos)
                            jntType = 3;
                        else if (keyWord.find("TRANS") != keyWord.npos)
                            jntType = 6;
                        else if (keyWord.find("LOCK") != keyWord.npos)
                            jntType = 5;
                        else
                            continue;

                        if (jntType)
                        {
                            sdiValueEntity N1Entity;
                            sdiValueEntity N2Entity;
                            sdiValueEntity N3Entity;
                            sdiValueEntity N4Entity;
                            sdiStringList nodeAttrNames({ "N1", "N2", "N3", "N4"});
                            vector<reference_wrapper<sdiValueEntity>> nodeAttrVals({ N1Entity, N2Entity, N3Entity, N4Entity });
                            p_ConvertUtils.GetAttribValues(*selConstrJnts, nodeAttrNames, nodeAttrVals);

                            int nbNodesFoundA = 0;
                            int nbNodesFoundB = 0;

                            nodeList = { { N1Entity.GetId(), N2Entity.GetId(), N3Entity.GetId(), N4Entity.GetId()} };
                            for (unsigned int jntNode : nodeList)
                            {
                                if (jntNode != 0 && binary_search(partsNodeListA.begin(), partsNodeListA.end(), jntNode))
                                    nbNodesFoundA = 1;

                                if (jntNode != 0 && binary_search(partsNodeListB.begin(), partsNodeListB.end(), jntNode))
                                    nbNodesFoundB = 1;
                            }

                            if (nbNodesFoundA == 1 && nbNodesFoundB == 1)
                            {
                                jntId = selConstrJnts->GetId();
                                jidHread = selConstrJnts->GetHandle();
                            }
                        }

                        if (jntId)
                        {
                            sdiConvert::SDIHandlReadList sourceList = { {jidHread , selConsStiffGen->GetHandle()} };
                            HandleElementEdit sprElemHEdit;

                            nodeList.resize(3);
                            if (jntType == 1)
                                nodeList.resize(2);
                                
                            HandleEdit partHEdit;
                            HandleEdit jointpropHEdit;

                            sdiString jointStiffName = selConsStiffGen->GetName();
                            if (selConsStiffGen->GetKeyword().find("_TITLE") == string::npos)
                                jointStiffName = "CONSTRAINED_JOINT_STIFFNESS_" + to_string(selConsStiffGen->GetId());
                            p_radiossModel->CreateEntity(partHEdit, "/PART", jointStiffName);
                            sdiConvert::Convert::PushToConversionLog(std::make_pair(partHEdit, sourceList));

                            p_radiossModel->CreateElement(sprElemHEdit,"/SPRING",nodeList,partHEdit,p_radiossModel->GetNextAvailableId(p_radiossModel->GetEntityType("/SPRING")));

                            if (sprElemHEdit.IsValid())
                            {
                                sdiConvert::Convert::PushToConversionLog(std::make_pair(sprElemHEdit, sourceList));
                                p_radiossModel->CreateEntity(jointpropHEdit, "/PROP/TYPE45", jointStiffName);
                                jointpropHEdit.SetValue(p_radiossModel, sdiIdentifier("Type"), sdiValue(jntType));
                                partHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), jointpropHEdit);
                                sdiConvert::Convert::PushToConversionLog(std::make_pair(jointpropHEdit, sourceList));

                                /* map values to created prop*/
                                EntityEdit jntPropEntEdit(p_radiossModel, jointpropHEdit);
                                if (jntType == 3)
                                {
                                    if (cidAHread.IsValid())
                                        jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                                    for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSAPH}, {"SAx+", lsdPSAPH }}))
                                    {
                                        int multplier = 1;
                                        if (pair.first.find("-") != string::npos)
                                            multplier = -1;
                                        jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second)));
                                    }

                                    for (const auto pair : map<sdiString, double>({ {"Kfrx", lsdESPH}, { "FMx", lsdFMPH } }))
                                        jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));
                                        
                                    jntPropEntEdit.SetValue(sdiIdentifier("Ktx"), sdiValue(0));
                                    jntPropEntEdit.SetValue(sdiIdentifier("Ctx"), sdiValue(0));
                                    jntPropEntEdit.SetValue(sdiIdentifier("Kftx"), sdiValue(0));
                                    jntPropEntEdit.SetValue(sdiIdentifier("Crx"), sdiValue(0));
                                    jntPropEntEdit.SetValue(sdiIdentifier("Krx"), sdiValue(0));

                                    for (const auto pair : map<sdiString, HandleRead>({ { "fct_Krx", lciphHread }, { "fct_Crx", dlcidpHread }, { "fct_fmx", fmphCrvHread } }))
                                    {
                                        if (pair.second.IsValid())
                                            jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                                    }
                                }
                                else if (jntType != 2 && jntType != 6)
                                {
                                    if (cidAHread.IsValid())
                                        jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                                    if (cidBHread.IsValid())
                                        jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, cidBHread.GetId(p_lsdynaModel))));
                                    for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSAPH}, {"SAx+", lsdPSAPH }, { "SAy-", lsdNSAT }, { "SAy+", lsdPSAT }, { "SAz-", lsdNSAPS }, { "SAz+", lsdPSAPS } }))
                                    {
                                        int multplier = 1;
                                        if (pair.first.find("-") != string::npos)
                                            multplier = -1;
                                        jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second) * 0.01745));
                                    }

                                    for (const auto pair : map<sdiString, double>({ {"Kfrx", lsdESPH}, {"Kfry", lsdEST }, { "Kfrz", lsdESPS }, { "FMx", lsdFMPH }, { "FMy", lsdFMT  }, { "FMz", lsdFMPS } }))
                                        jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

                                    for (const auto pair : map<sdiString, HandleRead>({ { "fct_Krx", lciphHread }, { "fct_Kry", lcidtHread }, { "fct_Krz", lcidpsHread }, { "fct_Crx", dlcidpHread }, { "fct_Cry", dlcidtHread }, { "fct_Crz", dlcidpsHread }, { "fct_fmx", fmphCrvHread }, { "fct_fmy", fmtCrvHread }, { "fct_fmz", fmpsCrvHread } }))
                                    {
                                        if (pair.second.IsValid())
                                            jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                                    }
                                }
                                else 
                                {
                                    if (cidAHread.IsValid())
                                        jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                                    if (cidBHread.IsValid())
                                        jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, cidBHread.GetId(p_lsdynaModel))));
                                    if (cidAHread.IsValid())
                                    {
                                        EntityRead jntEntRead(p_lsdynaModel, jidHread);
                                        HandleRead node1Hread;
                                        HandleRead node3Hread;
                                        jntEntRead.GetEntityHandle(sdiIdentifier("N1"), node1Hread);
                                        jntEntRead.GetEntityHandle(sdiIdentifier("N3"), node3Hread);
                                        NodeRead node1Read(p_lsdynaModel, node1Hread);
                                        NodeRead node3Read(p_lsdynaModel, node3Hread);
                                        sdiTriple vectN3N1 = (node3Read.GetPosition() - node1Read.GetPosition()).Normalize();

                                        EntityRead cidAEntRead(p_lsdynaModel, cidAHread);
                                        EntityRead cidBEntRead(p_lsdynaModel, cidBHread);
                                        vector<sdiTriple> localAxisList({ sdiTriple(0.0, 0.0, 0.0), sdiTriple(0.0, 0.0, 0.0), sdiTriple(0.0, 0.0, 0.0) });
                                        vector<sdiTriple> localAxisList2({ sdiTriple(0.0, 0.0, 0.0), sdiTriple(0.0, 0.0, 0.0), sdiTriple(0.0, 0.0, 0.0) });
                                        vector<double> axisOrigin({0.0, 0.0, 0.0});
                                        vector<double> axisOrigin2({0.0, 0.0, 0.0});
                                        p_GetLocalAxesFromDefineCoordinate(cidAEntRead, localAxisList, axisOrigin);
                                        p_GetLocalAxesFromDefineCoordinate(cidBEntRead, localAxisList2, axisOrigin2);
                                        int axisIndex = -1;
                                        for (size_t i = 0; i < 3; ++i)
                                        {
                                            // if cos(vectN3N1,localAxisList) > 0.99
                                            if (   abs(  (vectN3N1[0]*localAxisList[i][0]+vectN3N1[1]*localAxisList[i][1]+vectN3N1[2]*localAxisList[i][2]) /
                                                   ( sqrt(vectN3N1[0]*vectN3N1[0]+vectN3N1[1]*vectN3N1[1]+vectN3N1[2]*vectN3N1[2])  *
                                                     sqrt(localAxisList[i][0]*localAxisList[i][0]+localAxisList[i][1]*localAxisList[i][1]+localAxisList[i][2]*localAxisList[i][2]) ))   > 0.99 )
                                            {
                                                axisIndex = (int)i;
                                                break;
                                            }
                                        }
                                        if (axisIndex != -1)
                                        {
                                            vector<sdiString> radAttNames({ "Ox", "Oy", "Oz" });
                                            vector<sdiString> attribVect1({ "X1", "Y1", "Z1" });
                                            vector<sdiString> attribVect2({ "X2", "Y2", "Z2" });
                                            HandleEdit skew1Handle;
                                            HandleEdit skew2Handle;
                                            int tempint = 1;  
                                            switch (axisIndex)
                                            {
                                            case 0:
                                                if (cidAHread.IsValid())
                                                    jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, skew1Handle.GetId(p_lsdynaModel))));
                                                if (cidBHread.IsValid())
                                                    jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, skew2Handle.GetId(p_lsdynaModel))));
                                                for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSAPH}, {"SAx+", lsdPSAPH } }))
                                                {
                                                    int multplier = 1;
                                                    if (pair.first.find("-") != string::npos)
                                                        multplier = -1;
                                                    jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second) * 0.01745));
                                                    ++tempint;
                                                }
                                                for (const auto pair : map<sdiString, double>({ {"Kfrx", lsdESPH}, { "FMx", lsdFMPH }}))
                                                    jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

                                                for (const auto pair : map<sdiString, HandleRead>({ { "fct_Krx", lciphHread }, { "fct_Crx", dlcidpHread }, { "fct_fmx", fmphCrvHread } }))
                                                {
                                                    if (pair.second.IsValid())
                                                        jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                                                }
                                                break;
                                            case 1:

                                                for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSAT}, {"SAx+", lsdPSAT } }))
                                                {
                                                    int multplier = 1;
                                                    if (pair.first.find("-") != string::npos)
                                                        multplier = -1;
                                                    jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second) * 0.01745));
                                                    ++tempint;
                                                }
                                                for (const auto pair : map<sdiString, double>({ {"Kfrx", lsdEST }, { "FMx", lsdFMT  },}))
                                                    jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

                                                for (const auto pair : map<sdiString, HandleRead>({ { "fct_Krx", lcidtHread }, { "fct_Crx", dlcidtHread }, { "fct_fmx", fmtCrvHread } }))
                                                {
                                                    if (pair.second.IsValid())
                                                        jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                                                }
                                                break;
                                            case 2:
                                                for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSAPS}, {"SAx+", lsdNSAPS } }))
                                                {
                                                    int multplier = 1;
                                                    if (pair.first.find("-") != string::npos)
                                                        multplier = -1;
                                                    jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second) * 0.01745));
                                                    ++tempint;
                                                }
                                                for (const auto pair : map<sdiString, double>({ { "Kfrx", lsdESPS }, { "FMx", lsdFMPS } }))
                                                    jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

                                                for (const auto pair : map<sdiString, HandleRead>({ { "fct_Krx", lcidpsHread }, { "fct_Crx", dlcidpsHread }, { "fct_fmx", fmpsCrvHread } }))
                                                {
                                                    if (pair.second.IsValid())
                                                        jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                                                }
                                                break;
                                            default:
                                                break;
                                            }
                                            if (axisIndex == 0)
                                            {
                                                if (cidAHread.IsValid())
                                                    jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                                                if (cidBHread.IsValid())
                                                    jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, cidBHread.GetId(p_lsdynaModel))));
                                            }
                                            else if (axisIndex == 1)
                                            {
                                                p_radiossModel->CreateEntity(skew1Handle, "/SKEW/FIX", selConsStiffGen->GetName(), p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                                                EntityEdit skew1EntityEdit(p_radiossModel, skew1Handle);
                                                p_radiossModel->CreateEntity(skew2Handle, "/SKEW/FIX", selConsStiffGen->GetName(), p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                                                EntityEdit skew2EntityEdit(p_radiossModel, skew2Handle);
                                                if (skew1Handle.IsValid() && skew2Handle.IsValid())
                                                {
                                                    for (size_t i = 0; i < 3; i++)
                                                    {
                                                        skew1EntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(axisOrigin[i]));
                                                        skew1EntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(-localAxisList[0][i]));
                                                        skew1EntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(localAxisList[2][i]));
                                                        skew2EntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(axisOrigin2[i]));
                                                        skew2EntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(-localAxisList2[0][i]));
                                                        skew2EntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(localAxisList2[2][i]));
                                                    }
                                                    sdiConvert::Convert::PushToConversionLog(std::make_pair(skew1Handle, sourceList));
                                                    sdiConvert::Convert::PushToConversionLog(std::make_pair(skew2Handle, sourceList));
                                                    if (cidAHread.IsValid())
                                                        jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, skew1Handle.GetId(p_lsdynaModel))));
                                                    if (cidBHread.IsValid())
                                                        jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, skew2Handle.GetId(p_lsdynaModel))));
                                                }
                                            }
                                            else if (axisIndex == 2)
                                            {
                                                p_radiossModel->CreateEntity(skew1Handle, "/SKEW/FIX", selConsStiffGen->GetName(), p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                                                EntityEdit skew1EntityEdit(p_radiossModel, skew1Handle);
                                                p_radiossModel->CreateEntity(skew2Handle, "/SKEW/FIX", selConsStiffGen->GetName(), p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                                                EntityEdit skew2EntityEdit(p_radiossModel, skew2Handle);
                                                if (skew1Handle.IsValid() && skew2Handle.IsValid())
                                                {
                                                    for (size_t i = 0; i < 3; i++)
                                                    {
                                                        skew1EntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(axisOrigin[i]));
                                                        skew1EntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(localAxisList[1][i]));
                                                        skew1EntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(-localAxisList[0][i]));
                                                        skew2EntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(axisOrigin2[i]));
                                                        skew2EntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(localAxisList2[1][i]));
                                                        skew2EntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(-localAxisList2[0][i]));
                                                    }
                                                    sdiConvert::Convert::PushToConversionLog(std::make_pair(skew1Handle, sourceList));
                                                    sdiConvert::Convert::PushToConversionLog(std::make_pair(skew2Handle, sourceList));
                                                    if (cidAHread.IsValid())
                                                        jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, skew1Handle.GetId(p_lsdynaModel))));
                                                    if (cidBHread.IsValid())
                                                        jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, skew2Handle.GetId(p_lsdynaModel))));
                                                }
                                            }
                                        }
                                        else
                                        {
                                            if (cidAHread.IsValid())
                                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                                        }
                                        
                                    }
                                    else
                                    {
                                        /* // post warning /error */
                                    }
                                }
                                convertedJoints.push_back(jntId);
                            }
                        }

                    }


                }
            }
            else
            {
                EntityRead jntEntRead(p_lsdynaModel, jidHread);
                keyWord = jntEntRead.GetKeyword();
                jntId = jntEntRead.GetId();
                sdiValueEntity N1Entity;
                sdiValueEntity N2Entity;
                sdiValueEntity N3Entity;
                sdiValueEntity N4Entity;
                sdiStringList nodeAttrNames({ "N1", "N2", "N3", "N4" });
                vector<reference_wrapper<sdiValueEntity>> nodeAttrVals({ N1Entity, N2Entity, N3Entity, N4Entity });
                p_ConvertUtils.GetAttribValues(jntEntRead, nodeAttrNames, nodeAttrVals);

                nodeList = { { N1Entity.GetId(), N2Entity.GetId(), N3Entity.GetId(), N4Entity.GetId()} };

                if (jntId)
                {
                    sdiConvert::SDIHandlReadList sourceList = { {jidHread , selConsStiffGen->GetHandle()} };
                    HandleElementEdit sprElemHEdit;
                    nodeList.resize(3);
                    if (jntType == 1)
                        nodeList.resize(2);
                    HandleEdit partHEdit;
                    HandleEdit jointpropHEdit;

                    sdiString jointStiffName = selConsStiffGen->GetName();
                    if (selConsStiffGen->GetKeyword().find("_TITLE") == string::npos)
                        jointStiffName = "CONSTRAINED_JOINT_STIFFNESS_" + to_string(selConsStiffGen->GetId());
                    p_radiossModel->CreateEntity(partHEdit, "/PART", jointStiffName);
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(partHEdit, sourceList));
                        
                    p_radiossModel->CreateElement(sprElemHEdit,"/SPRING",nodeList,partHEdit,p_radiossModel->GetNextAvailableId(p_radiossModel->GetEntityType("/SPRING")));
                    if (sprElemHEdit.IsValid())
                    {
                        sdiConvert::Convert::PushToConversionLog(std::make_pair(sprElemHEdit, sourceList));
                        p_radiossModel->CreateEntity(jointpropHEdit, "/PROP/TYPE45", jointStiffName);
                        jointpropHEdit.SetValue(p_radiossModel, sdiIdentifier("Type"), sdiValue(jntType));
                        partHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), jointpropHEdit);
                        sdiConvert::Convert::PushToConversionLog(std::make_pair(jointpropHEdit, sourceList));

                        /* map values to created prop*/
                        EntityEdit jntPropEntEdit(p_radiossModel, jointpropHEdit);
                        if (jntType == 3)
                        {
                            if (cidAHread.IsValid())
                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                            for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSAPH}, {"SAx+", lsdPSAPH }}))
                            {
                                int multplier = 1;
                                if (pair.first.find("-") != string::npos)
                                    multplier = -1;
                                jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second)));
                            }

                            for (const auto pair : map<sdiString, double>({ {"Kfrx", lsdESPH}, { "FMx", lsdFMPH } }))
                                jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));
                                        
                            jntPropEntEdit.SetValue(sdiIdentifier("Ktx"), sdiValue(0));
                            jntPropEntEdit.SetValue(sdiIdentifier("Ctx"), sdiValue(0));
                            jntPropEntEdit.SetValue(sdiIdentifier("Kftx"), sdiValue(0));
                            jntPropEntEdit.SetValue(sdiIdentifier("Crx"), sdiValue(0));
                            jntPropEntEdit.SetValue(sdiIdentifier("Krx"), sdiValue(0));

                            for (const auto pair : map<sdiString, HandleRead>({ { "fct_Krx", lciphHread }, { "fct_Crx", dlcidpHread }, { "fct_fmx", fmphCrvHread } }))
                            {
                                if (pair.second.IsValid())
                                    jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                            }
                        }
                        if (jntType != 2 && jntType != 6)
                        {
                            if (cidAHread.IsValid())
                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                            if (cidBHread.IsValid())
                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, cidBHread.GetId(p_lsdynaModel))));
                            for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSAPH}, {"SAx+", lsdPSAPH }, { "SAy-", lsdNSAT }, { "SAy+", lsdPSAT }, { "SAz-", lsdNSAPS }, { "SAz+", lsdPSAPS } }))
                            {
                                int multplier = 1;
                                if (pair.first.find("-") != string::npos)
                                    multplier = -1;
                                jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second) * 0.01745));
                            }

                            for (const auto pair : map<sdiString, double>({ {"Kfrx", lsdESPH}, {"Kfry", lsdEST }, { "Kfrz", lsdESPS }, { "FMx", lsdFMPH }, { "FMy", lsdFMT  }, { "FMz", lsdFMPS } }))
                                jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

                            for (const auto pair : map<sdiString, HandleRead>({ { "fct_Krx", lciphHread }, { "fct_Kry", lcidtHread }, { "fct_Krz", lcidpsHread }, { "fct_Crx", dlcidpHread }, { "fct_Cry", dlcidtHread }, { "fct_Crz", dlcidpsHread }, { "fct_fmx", fmphCrvHread }, { "fct_fmy", fmtCrvHread }, { "fct_fmz", fmpsCrvHread } }))
                            {
                                if (pair.second.IsValid())
                                    jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                            }
                        }
                        else 
                        {
                            if (cidAHread.IsValid())
                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                            if (cidBHread.IsValid())
                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, cidBHread.GetId(p_lsdynaModel))));
                            if (cidAHread.IsValid())
                            {
                                EntityRead jntEntRead(p_lsdynaModel, jidHread);
                                HandleRead node1Hread;
                                HandleRead node3Hread;
                                jntEntRead.GetEntityHandle(sdiIdentifier("N1"), node1Hread);
                                jntEntRead.GetEntityHandle(sdiIdentifier("N3"), node3Hread);
                                NodeRead node1Read(p_lsdynaModel, node1Hread);
                                NodeRead node3Read(p_lsdynaModel, node3Hread);
                                sdiTriple vectN3N1 = (node3Read.GetPosition() - node1Read.GetPosition()).Normalize();

                                EntityRead cidAEntRead(p_lsdynaModel, cidAHread);
                                EntityRead cidBEntRead(p_lsdynaModel, cidBHread);
                                vector<sdiTriple> localAxisList({ sdiTriple(0.0, 0.0, 0.0), sdiTriple(0.0, 0.0, 0.0), sdiTriple(0.0, 0.0, 0.0) });
                                vector<sdiTriple> localAxisList2({ sdiTriple(0.0, 0.0, 0.0), sdiTriple(0.0, 0.0, 0.0), sdiTriple(0.0, 0.0, 0.0) });
                                vector<double> axisOrigin({0.0, 0.0, 0.0});
                                vector<double> axisOrigin2({0.0, 0.0, 0.0});
                                p_GetLocalAxesFromDefineCoordinate(cidAEntRead, localAxisList, axisOrigin);
                                p_GetLocalAxesFromDefineCoordinate(cidBEntRead, localAxisList2, axisOrigin2);
                                int axisIndex = -1;
                                for (size_t i = 0; i < 3; ++i)
                                {
                                    // if cos(vectN3N1,localAxisList) > 0.99
                                    if (   abs(  (vectN3N1[0]*localAxisList[i][0]+vectN3N1[1]*localAxisList[i][1]+vectN3N1[2]*localAxisList[i][2]) /
                                           ( sqrt(vectN3N1[0]*vectN3N1[0]+vectN3N1[1]*vectN3N1[1]+vectN3N1[2]*vectN3N1[2])  *
                                             sqrt(localAxisList[i][0]*localAxisList[i][0]+localAxisList[i][1]*localAxisList[i][1]+localAxisList[i][2]*localAxisList[i][2]) ))   > 0.99 )
                                    {
                                        axisIndex = (int)i;
                                        break;
                                    }
                                }
                                if (axisIndex != -1)
                                {
                                    vector<sdiString> radAttNames({ "Ox", "Oy", "Oz" });
                                    vector<sdiString> attribVect1({ "X1", "Y1", "Z1" });
                                    vector<sdiString> attribVect2({ "X2", "Y2", "Z2" });
                                    HandleEdit skew1Handle;
                                    HandleEdit skew2Handle;
                                    int tempint = 1;  
                                    switch (axisIndex)
                                    {
                                    case 0:
                                        if (cidAHread.IsValid())
                                            jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, skew1Handle.GetId(p_lsdynaModel))));
                                        if (cidBHread.IsValid())
                                            jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, skew2Handle.GetId(p_lsdynaModel))));
                                        for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSAPH}, {"SAx+", lsdPSAPH } }))
                                        {
                                            int multplier = 1;
                                            if (pair.first.find("-") != string::npos)
                                                multplier = -1;
                                            jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second) * 0.01745));
                                            ++tempint;
                                        }
                                        for (const auto pair : map<sdiString, double>({ {"Kfrx", lsdESPH}, { "FMx", lsdFMPH }}))
                                            jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

                                        for (const auto pair : map<sdiString, HandleRead>({ { "fct_Krx", lciphHread }, { "fct_Crx", dlcidpHread }, { "fct_fmx", fmphCrvHread } }))
                                        {
                                            if (pair.second.IsValid())
                                                jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                                        }
                                        break;
                                    case 1:

                                        for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSAT}, {"SAx+", lsdPSAT } }))
                                        {
                                            int multplier = 1;
                                            if (pair.first.find("-") != string::npos)
                                                multplier = -1;
                                            jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second) * 0.01745));
                                            ++tempint;
                                        }
                                        for (const auto pair : map<sdiString, double>({ {"Kfrx", lsdEST }, { "FMx", lsdFMT  },}))
                                            jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

                                        for (const auto pair : map<sdiString, HandleRead>({ { "fct_Krx", lcidtHread }, { "fct_Crx", dlcidtHread }, { "fct_fmx", fmtCrvHread } }))
                                        {
                                            if (pair.second.IsValid())
                                                jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                                        }
                                        break;
                                    case 2:
                                        for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSAPS}, {"SAx+", lsdNSAPS } }))
                                        {
                                            int multplier = 1;
                                            if (pair.first.find("-") != string::npos)
                                                multplier = -1;
                                            jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second) * 0.01745));
                                            ++tempint;
                                        }
                                        for (const auto pair : map<sdiString, double>({ { "Kfrx", lsdESPS }, { "FMx", lsdFMPS } }))
                                            jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

                                        for (const auto pair : map<sdiString, HandleRead>({ { "fct_Krx", lcidpsHread }, { "fct_Crx", dlcidpsHread }, { "fct_fmx", fmpsCrvHread } }))
                                        {
                                            if (pair.second.IsValid())
                                                jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                                        }
                                        break;
                                    default:
                                        break;
                                    }
                                    if (axisIndex == 0)
                                    {
                                        if (cidAHread.IsValid())
                                            jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                                        if (cidBHread.IsValid())
                                            jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, cidBHread.GetId(p_lsdynaModel))));
                                    }
                                    else if (axisIndex == 1)
                                    {
                                        p_radiossModel->CreateEntity(skew1Handle, "/SKEW/FIX", selConsStiffGen->GetName(), p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                                        EntityEdit skew1EntityEdit(p_radiossModel, skew1Handle);
                                        p_radiossModel->CreateEntity(skew2Handle, "/SKEW/FIX", selConsStiffGen->GetName(), p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                                        EntityEdit skew2EntityEdit(p_radiossModel, skew2Handle);
                                        if (skew1Handle.IsValid() && skew2Handle.IsValid())
                                        {
                                            for (size_t i = 0; i < 3; i++)
                                            {
                                                skew1EntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(axisOrigin[i]));
                                                skew1EntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(-localAxisList[0][i]));
                                                skew1EntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(localAxisList[2][i]));
                                                skew2EntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(axisOrigin2[i]));
                                                skew2EntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(-localAxisList2[0][i]));
                                                skew2EntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(localAxisList2[2][i]));
                                            }
                                            sdiConvert::Convert::PushToConversionLog(std::make_pair(skew1Handle, sourceList));
                                            sdiConvert::Convert::PushToConversionLog(std::make_pair(skew2Handle, sourceList));
                                            if (cidAHread.IsValid())
                                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, skew1Handle.GetId(p_lsdynaModel))));
                                            if (cidBHread.IsValid())
                                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, skew2Handle.GetId(p_lsdynaModel))));
                                        }
                                    }
                                    else if (axisIndex == 2)
                                    {
                                        p_radiossModel->CreateEntity(skew1Handle, "/SKEW/FIX", selConsStiffGen->GetName(), p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                                        EntityEdit skew1EntityEdit(p_radiossModel, skew1Handle);
                                        p_radiossModel->CreateEntity(skew2Handle, "/SKEW/FIX", selConsStiffGen->GetName(), p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                                        EntityEdit skew2EntityEdit(p_radiossModel, skew2Handle);
                                        if (skew1Handle.IsValid() && skew2Handle.IsValid())
                                        {
                                            for (size_t i = 0; i < 3; i++)
                                            {
                                                skew1EntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(axisOrigin[i]));
                                                skew1EntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(localAxisList[1][i]));
                                                skew1EntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(-localAxisList[0][i]));
                                                skew2EntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(axisOrigin2[i]));
                                                skew2EntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(localAxisList2[1][i]));
                                                skew2EntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(-localAxisList2[0][i]));
                                            }
                                            sdiConvert::Convert::PushToConversionLog(std::make_pair(skew1Handle, sourceList));
                                            sdiConvert::Convert::PushToConversionLog(std::make_pair(skew2Handle, sourceList));
                                            if (cidAHread.IsValid())
                                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, skew1Handle.GetId(p_lsdynaModel))));
                                            if (cidBHread.IsValid())
                                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, skew2Handle.GetId(p_lsdynaModel))));
                                        }
                                    }
                                }
                                else
                                {
                                    if (cidAHread.IsValid())
                                        jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                                }
                                
                            }
                            else
                            {
                                /* // post warning /error */
                            }
                        }
                        convertedJoints.push_back(jntId);
                    }
                }
            }
        }
    }
}



void sdiD2R::ConvertJoint::ConvertStiffTransJoints()
{
    EntityType radSkewType = p_radiossModel->GetEntityType("/SKEW");
    SelectionRead selConsStiffGen(p_lsdynaModel, "*CONSTRAINED_JOINT_STIFFNESS_TRANSLATIONAL");
    unsigned int numStiffGenJnts = selConsStiffGen.Count();
    EntityType lsdDefCoordType = p_lsdynaModel->GetEntityType("*DEFINE_COORDINATE");

    if (numStiffGenJnts)
    {
        size_t reserveCapacity = (size_t)numStiffGenJnts * 3;
        convertedJoints.reserve(reserveCapacity);
        PopulateConstExtraNodePartRelation();
        EntityType radCrvType = p_radiossModel->GetEntityType("/FUNCT");

        while (selConsStiffGen.Next())
        {
            HandleRead pidAHread;
            HandleRead pidBHread;
            HandleRead cidAHread;
            HandleRead cidBHread;
            HandleRead jidHread;
            HandleRead lcidxHread;
            HandleRead lcidyHread;
            HandleRead lcidzHread;
            HandleRead dlcidxHread;
            HandleRead dlcidyHread;
            HandleRead dlcidzHread;
            HandleRead ffxCrvHread;
            HandleRead ffyCrvHread;
            HandleRead ffzCrvHread;

            sdiStringList attrNameList({ "PIDA", "PIDB", "CIDA", "CIDB", "JID", "LCIDX",
                                        "LCIDY", "LCIDZ", "DLCIDX", "DLCIDY", "DLCIDZ",
                                        "FFX_CURVE", "FFY_CURVE", "FFZ_CURVE" });
            vector<reference_wrapper<HandleRead>> entityHandlesList({ pidAHread , pidBHread, cidAHread, cidBHread,
                                                                      jidHread, lcidxHread, lcidyHread, lcidzHread,
                                                                      dlcidxHread, dlcidyHread, dlcidzHread, 
                                                                      ffxCrvHread, ffyCrvHread, ffzCrvHread });

            p_ConvertUtils.GetEntityHandles(*selConsStiffGen, attrNameList, entityHandlesList);

            double lsdESX;
            double lsdFFX;
            double lsdESY;
            double lsdFFY;
            double lsdESZ;
            double lsdFFZ;
            double lsdNSDX;
            double lsdPSDX;
            double lsdNSDY;
            double lsdPSDY;
            double lsdNSDZ;
            double lsdPSDZ;

            attrNameList = { { "ESX", "LSD_FFX", "ESY", "LSD_FFY", "ESZ", "LSD_FFZ", "NSDX", "PSDX", "NSDY", "PSDY", "NSDY", "PSDY" } };
            vector<reference_wrapper<double>> dblAttrList({ lsdESX, lsdFFX, lsdESY, lsdFFY, lsdESZ, lsdFFZ, lsdNSDX, lsdPSDX, lsdNSDY, lsdPSDY, lsdNSDZ, lsdPSDZ });

            p_ConvertUtils.GetAttribValues(*selConsStiffGen, attrNameList, dblAttrList);

            sdiUIntList nodeList;
            sdiString keyWord;
            int jntType = 0;
            unsigned int jntId = 0;
            if (!jidHread.IsValid())
            {
                /*find the joint*/
                sdiUIntList partsNodeListA;
                sdiUIntList partsNodeListB;
                unsigned int partIdA = pidAHread.GetId(p_lsdynaModel);
                unsigned int partIdB = pidBHread.GetId(p_lsdynaModel);
                p_ConvertUtils.GetNodesOfParts(partIdA, partsNodeListA);
                p_ConvertUtils.GetNodesOfParts(partIdB, partsNodeListB);
                sdiVectorSort(partsNodeListA);
                sdiVectorSort(partsNodeListB);

                for (const unsigned int partId : {partIdA, partIdB})
                {
                    if (!mapPartIdVsExtraNodeList[partId].empty())
                    {
                        for (const unsigned int nodeId : mapPartIdVsExtraNodeList[partId])
                        {
                            if(partId == partIdA)partsNodeListA.push_back(nodeId);
                            if(partId == partIdB)partsNodeListB.push_back(nodeId);
                        }
                    }
                }

                sdiVectorSort(partsNodeListA);
                sdiVectorSort(partsNodeListB);

                if (!partsNodeListA.empty() && !partsNodeListB.empty())
                {
                    SelectionRead selConstrJnts(p_lsdynaModel, srcCard);
                    while (selConstrJnts.Next())
                    {
                        jntId = 0;
                        keyWord = selConstrJnts->GetKeyword();
                        if (keyWord.find("SPH") != keyWord.npos)
                            jntType = 1;
                        else if (keyWord.find("REV") != keyWord.npos)
                            jntType = 2;
                        else if (keyWord.find("CYL") != keyWord.npos)
                            jntType = 3;
                        else if (keyWord.find("TRANS") != keyWord.npos)
                            jntType = 6;
                        else if (keyWord.find("LOCK") != keyWord.npos)
                            jntType = 5;
                        else
                            continue;

                        if (jntType)
                        {
                            sdiValueEntity N1Entity;
                            sdiValueEntity N2Entity;
                            sdiValueEntity N3Entity;
                            sdiValueEntity N4Entity;
                            sdiStringList nodeAttrNames({ "N1", "N2", "N3", "N4"});
                            vector<reference_wrapper<sdiValueEntity>> nodeAttrVals({ N1Entity, N2Entity, N3Entity, N4Entity });
                            p_ConvertUtils.GetAttribValues(*selConstrJnts, nodeAttrNames, nodeAttrVals);

                            int nbNodesFoundA = 0;
                            int nbNodesFoundB = 0;

                            nodeList = { { N1Entity.GetId(), N2Entity.GetId(), N3Entity.GetId(), N4Entity.GetId()} };
                            for (unsigned int jntNode : nodeList)
                            {
                                if (jntNode != 0 && binary_search(partsNodeListA.begin(), partsNodeListA.end(), jntNode))
                                    nbNodesFoundA = 1;

                                if (jntNode != 0 && binary_search(partsNodeListB.begin(), partsNodeListB.end(), jntNode))
                                    nbNodesFoundB = 1;
                            }

                            if (nbNodesFoundA == 1 && nbNodesFoundB == 1)
                            {
                                jntId = selConstrJnts->GetId();
                                jidHread = selConstrJnts->GetHandle();
                            }
                        }

                        if (jntId)
                        {
                            sdiConvert::SDIHandlReadList sourceList = { {jidHread , selConsStiffGen->GetHandle()} };
                            HandleElementEdit sprElemHEdit;
                            nodeList.resize(3);
                            if (jntType == 1)
                                nodeList.resize(2);
                            HandleEdit partHEdit;
                            HandleEdit jointpropHEdit;

                            sdiString jointStiffName = selConsStiffGen->GetName();
                            if (selConsStiffGen->GetKeyword().find("_TITLE") == string::npos)
                                jointStiffName = "CONSTRAINED_JOINT_STIFFNESS_" + to_string(selConsStiffGen->GetId());
                            p_radiossModel->CreateEntity(partHEdit, "/PART", jointStiffName);
                            sdiConvert::Convert::PushToConversionLog(std::make_pair(partHEdit, sourceList));

                            p_radiossModel->CreateElement(sprElemHEdit,"/SPRING",nodeList,partHEdit,p_radiossModel->GetNextAvailableId(p_radiossModel->GetEntityType("/SPRING")));
                            if (sprElemHEdit.IsValid())
                            {
                                sdiConvert::Convert::PushToConversionLog(std::make_pair(sprElemHEdit, sourceList));
                                p_radiossModel->CreateEntity(jointpropHEdit, "/PROP/TYPE45", jointStiffName);
                                jointpropHEdit.SetValue(p_radiossModel, sdiIdentifier("Type"), sdiValue(jntType));
                                partHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), jointpropHEdit);
                                sdiConvert::Convert::PushToConversionLog(std::make_pair(jointpropHEdit, sourceList));

                                /* map values to created prop*/
                                EntityEdit jntPropEntEdit(p_radiossModel, jointpropHEdit);
                                if (jntType == 3)
                                {
                                    if (cidAHread.IsValid())
                                        jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                                    for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSDX}, {"SAx+", lsdPSDX }}))
                                    {
                                        int multplier = 1;
                                        if (pair.first.find("-") != string::npos)
                                            multplier = -1;
                                        jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second)));
                                    }

                                    for (const auto pair : map<sdiString, double>({ {"Kfrx", lsdESX}, { "FMx", lsdFFX } }))
                                        jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));
                                        
                                    jntPropEntEdit.SetValue(sdiIdentifier("Ktx"), sdiValue(0));
                                    jntPropEntEdit.SetValue(sdiIdentifier("Ctx"), sdiValue(0));
                                    jntPropEntEdit.SetValue(sdiIdentifier("Kftx"), sdiValue(0));
                                    jntPropEntEdit.SetValue(sdiIdentifier("Crx"), sdiValue(0));
                                    jntPropEntEdit.SetValue(sdiIdentifier("Krx"), sdiValue(0));

                                    for (const auto pair : map<sdiString, HandleRead>({ { "fct_Krx", lcidxHread }, { "fct_Crx", dlcidxHread }, { "fct_fmx", ffxCrvHread } }))
                                    {
                                        if (pair.second.IsValid())
                                            jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                                    }
                                }
                                else if (jntType == 1)
                                {
                                    if (cidAHread.IsValid())
                                        jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                                    for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSDX}, {"SAx+", lsdPSDX }, { "SAy-", lsdNSDY }, { "SAy+", lsdPSDY }, { "SAz-", lsdNSDZ }, { "SAz+", lsdPSDZ } }))
                                    {
                                        int multplier = 1;
                                        if (pair.first.find("-") != string::npos)
                                            multplier = -1;
                                        jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second)));
                                    }

                                    for (const auto pair : map<sdiString, double>({ {"Kfrx", lsdESX}, {"Kfry", lsdESY }, { "Kfrz", lsdESZ }, { "FMx", lsdFFX }, { "FMy", lsdFFY  }, { "FMz", lsdFFZ } }))
                                        jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

                                    for (const auto pair : map<sdiString, HandleRead>({ { "fct_Crx", dlcidxHread }, { "fct_Cry", dlcidyHread }, { "fct_Crz", dlcidzHread }, { "fct_fmx", ffxCrvHread }, { "fct_fmy", ffyCrvHread }, { "fct_fmz", ffzCrvHread } }))
                                    {
                                        if (pair.second.IsValid())
                                            jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                                    }
                                    jntPropEntEdit.SetValue(sdiIdentifier("Krx"), sdiValue(0));
                                    jntPropEntEdit.SetValue(sdiIdentifier("Crx"), sdiValue(0));
                                    jntPropEntEdit.SetValue(sdiIdentifier("fct_Krx"), sdiValue(0));
                                    jntPropEntEdit.SetValue(sdiIdentifier("Kry"), sdiValue(0));
                                    jntPropEntEdit.SetValue(sdiIdentifier("Cry"), sdiValue(0));
                                    jntPropEntEdit.SetValue(sdiIdentifier("fct_Kry"), sdiValue(0));
                                    jntPropEntEdit.SetValue(sdiIdentifier("Krz"), sdiValue(0));
                                    jntPropEntEdit.SetValue(sdiIdentifier("Crz"), sdiValue(0));
                                    jntPropEntEdit.SetValue(sdiIdentifier("fct_Krz"), sdiValue(0));
                                }
                                else if (jntType != 2 && jntType != 6)
                                {
                                    if (cidAHread.IsValid())
                                        jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                                    for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSDX}, {"SAx+", lsdPSDX }, { "SAy-", lsdNSDY }, { "SAy+", lsdPSDY }, { "SAz-", lsdNSDZ }, { "SAz+", lsdPSDZ } }))
                                    {
                                        int multplier = 1;
                                        if (pair.first.find("-") != string::npos)
                                            multplier = -1;
                                        jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second)));
                                    }

                                    for (const auto pair : map<sdiString, double>({ {"Kfrx", lsdESX}, {"Kfry", lsdESY }, { "Kfrz", lsdESZ }, { "FMx", lsdFFX }, { "FMy", lsdFFY  }, { "FMz", lsdFFZ } }))
                                        jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

                                    for (const auto pair : map<sdiString, HandleRead>({ { "fct_Krx", lcidxHread }, { "fct_Kry", lcidyHread }, { "fct_Krz", lcidzHread }, { "fct_Crx", dlcidxHread }, { "fct_Cry", dlcidyHread }, { "fct_Crz", dlcidzHread }, { "fct_fmx", ffxCrvHread }, { "fct_fmy", ffyCrvHread }, { "fct_fmz", ffzCrvHread } }))
                                    {
                                        if (pair.second.IsValid())
                                            jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                                    }
                                }
                                else 
                                {
                                    if (cidAHread.IsValid())
                                        jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                                    if (cidAHread.IsValid())
                                    {
                                        EntityRead jntEntRead(p_lsdynaModel, jidHread);
                                        HandleRead node1Hread;
                                        HandleRead node3Hread;
                                        jntEntRead.GetEntityHandle(sdiIdentifier("N1"), node1Hread);
                                        jntEntRead.GetEntityHandle(sdiIdentifier("N3"), node3Hread);
                                        NodeRead node1Read(p_lsdynaModel, node1Hread);
                                        NodeRead node3Read(p_lsdynaModel, node3Hread);
                                        sdiTriple vectN3N1 = (node3Read.GetPosition() - node1Read.GetPosition()).Normalize();

                                        EntityRead cidAEntRead(p_lsdynaModel, cidAHread);
                                        EntityRead cidBEntRead(p_lsdynaModel, cidBHread);
                                        vector<sdiTriple> localAxisList({ sdiTriple(0.0, 0.0, 0.0), sdiTriple(0.0, 0.0, 0.0), sdiTriple(0.0, 0.0, 0.0) });
                                        vector<sdiTriple> localAxisList2({ sdiTriple(0.0, 0.0, 0.0), sdiTriple(0.0, 0.0, 0.0), sdiTriple(0.0, 0.0, 0.0) });
                                        vector<double> axisOrigin({0.0, 0.0, 0.0});
                                        vector<double> axisOrigin2({0.0, 0.0, 0.0});
                                        p_GetLocalAxesFromDefineCoordinate(cidAEntRead, localAxisList, axisOrigin);
                                        p_GetLocalAxesFromDefineCoordinate(cidBEntRead, localAxisList2, axisOrigin2);
                                        int axisIndex = -1;
                                        for (size_t i = 0; i < 3; ++i)
                                        {
                                            // if cos(vectN3N1,localAxisList) > 0.99
                                            if (   abs(  (vectN3N1[0]*localAxisList[i][0]+vectN3N1[1]*localAxisList[i][1]+vectN3N1[2]*localAxisList[i][2]) /
                                                   ( sqrt(vectN3N1[0]*vectN3N1[0]+vectN3N1[1]*vectN3N1[1]+vectN3N1[2]*vectN3N1[2])  *
                                                     sqrt(localAxisList[i][0]*localAxisList[i][0]+localAxisList[i][1]*localAxisList[i][1]+localAxisList[i][2]*localAxisList[i][2]) ))   > 0.99 )
                                            {
                                                axisIndex = (int)i;
                                                break;
                                            }
                                        }
                                        if (axisIndex != -1)
                                        {
                                            vector<sdiString> radAttNames({ "Ox", "Oy", "Oz" });
                                            vector<sdiString> attribVect1({ "X1", "Y1", "Z1" });
                                            vector<sdiString> attribVect2({ "X2", "Y2", "Z2" });
                                            HandleEdit skew1Handle;
                                            HandleEdit skew2Handle;
                                            int tempint = 1;
 
                                            switch (axisIndex)
                                            {
                                            case 0:
                                                if (cidAHread.IsValid())
                                                    jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, skew1Handle.GetId(p_lsdynaModel))));
                                                for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSDX}, {"SAx+", lsdPSDX } }))
                                                {
                                                    int multplier = 1;
                                                    if (pair.first.find("-") != string::npos)
                                                        multplier = -1;
                                                    jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second)));
                                                    ++tempint;
                                                }
                                                for (const auto pair : map<sdiString, double>({ {"Kftx", lsdESX}, { "FMx", lsdFFX }}))
                                                    jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

                                                for (const auto pair : map<sdiString, HandleRead>({ { "fct_Ktx", lcidxHread }, { "fct_Ctx", dlcidxHread }, { "fct_fmx", ffxCrvHread } }))
                                                {
                                                    if (pair.second.IsValid())
                                                        jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                                                }
                                                break;
                                            case 1:

                                                for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSDY}, {"SAx+", lsdPSDY } }))
                                                {
                                                    int multplier = 1;
                                                    if (pair.first.find("-") != string::npos)
                                                        multplier = -1;
                                                    jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second)));
                                                    ++tempint;
                                                }
                                                for (const auto pair : map<sdiString, double>({ {"Kftx", lsdESY }, { "FMx", lsdFFY  },}))
                                                    jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

                                                for (const auto pair : map<sdiString, HandleRead>({ { "fct_Ktx", lcidyHread }, { "fct_Ctx", dlcidyHread }, { "fct_fmx", ffyCrvHread } }))
                                                {
                                                    if (pair.second.IsValid())
                                                        jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                                                }
                                                break;
                                            case 2:
                                                for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSDZ}, {"SAx+", lsdNSDZ } }))
                                                {
                                                    int multplier = 1;
                                                    if (pair.first.find("-") != string::npos)
                                                        multplier = -1;
                                                    jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second)));
                                                    ++tempint;
                                                }
                                                for (const auto pair : map<sdiString, double>({ { "Kftx", lsdESZ }, { "FMx", lsdFFZ } }))
                                                    jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

                                                for (const auto pair : map<sdiString, HandleRead>({ { "fct_Ktx", lcidzHread }, { "fct_Ctx", dlcidzHread }, { "fct_fmx", ffzCrvHread } }))
                                                {
                                                    if (pair.second.IsValid())
                                                        jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                                                }
                                                break;
                                            default:
                                                break;
                                            }
                                            if (axisIndex == 0)
                                            {
                                                if (cidAHread.IsValid())
                                                    jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                                            }
                                            else if (axisIndex == 1)
                                            {
                                                p_radiossModel->CreateEntity(skew1Handle, "/SKEW/FIX", selConsStiffGen->GetName(), p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                                                EntityEdit skew1EntityEdit(p_radiossModel, skew1Handle);
                                                p_radiossModel->CreateEntity(skew2Handle, "/SKEW/FIX", selConsStiffGen->GetName(), p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                                                EntityEdit skew2EntityEdit(p_radiossModel, skew2Handle);
                                                if (skew1Handle.IsValid() && skew2Handle.IsValid())
                                                {
                                                    for (size_t i = 0; i < 3; i++)
                                                    {
                                                        skew1EntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(axisOrigin[i]));
                                                        skew1EntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(-localAxisList[0][i]));
                                                        skew1EntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(localAxisList[2][i]));
                                                        skew2EntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(axisOrigin2[i]));
                                                        skew2EntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(-localAxisList2[0][i]));
                                                        skew2EntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(localAxisList2[2][i]));
                                                    }
                                                    sdiConvert::Convert::PushToConversionLog(std::make_pair(skew1Handle, sourceList));
                                                    sdiConvert::Convert::PushToConversionLog(std::make_pair(skew2Handle, sourceList));
                                                    if (cidAHread.IsValid())
                                                        jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, skew1Handle.GetId(p_lsdynaModel))));
                                                }
                                            }
                                            else if (axisIndex == 2)
                                            {
                                                p_radiossModel->CreateEntity(skew1Handle, "/SKEW/FIX", selConsStiffGen->GetName(), p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                                                EntityEdit skew1EntityEdit(p_radiossModel, skew1Handle);
                                                p_radiossModel->CreateEntity(skew2Handle, "/SKEW/FIX", selConsStiffGen->GetName(), p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                                                EntityEdit skew2EntityEdit(p_radiossModel, skew2Handle);
                                                if (skew1Handle.IsValid() && skew2Handle.IsValid())
                                                {
                                                    for (size_t i = 0; i < 3; i++)
                                                    {
                                                        skew1EntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(axisOrigin[i]));
                                                        skew1EntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(localAxisList[1][i]));
                                                        skew1EntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(-localAxisList[0][i]));
                                                        skew2EntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(axisOrigin2[i]));
                                                        skew2EntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(localAxisList2[1][i]));
                                                        skew2EntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(-localAxisList2[0][i]));
                                                    }
                                                    sdiConvert::Convert::PushToConversionLog(std::make_pair(skew1Handle, sourceList));
                                                    sdiConvert::Convert::PushToConversionLog(std::make_pair(skew2Handle, sourceList));
                                                    if (cidAHread.IsValid())
                                                        jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, skew1Handle.GetId(p_lsdynaModel))));
                                                }
                                            }
                                        }
                                        else
                                        {
                                            if (cidAHread.IsValid())
                                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                                        }
                                        
                                    }
                                    else
                                    {
                                        /* // post warning /error */
                                    }
                                }
                                convertedJoints.push_back(jntId);
                            }
                        }

                    }


                }
            }
            else
            {
                EntityRead jntEntRead(p_lsdynaModel, jidHread);
                keyWord = jntEntRead.GetKeyword();
                jntId = jntEntRead.GetId();
                sdiValueEntity N1Entity;
                sdiValueEntity N2Entity;
                sdiValueEntity N3Entity;
                sdiValueEntity N4Entity;
                sdiStringList nodeAttrNames({ "N1", "N2", "N3", "N4" });
                vector<reference_wrapper<sdiValueEntity>> nodeAttrVals({ N1Entity, N2Entity, N3Entity, N4Entity });
                p_ConvertUtils.GetAttribValues(jntEntRead, nodeAttrNames, nodeAttrVals);

                nodeList = { { N1Entity.GetId(), N2Entity.GetId(), N3Entity.GetId(), N4Entity.GetId()} };

                if (jntId)
                {
                    sdiConvert::SDIHandlReadList sourceList = { {jidHread , selConsStiffGen->GetHandle()} };
                    HandleElementEdit sprElemHEdit;
                    nodeList.resize(3);
                    if (jntType == 1)
                        nodeList.resize(2);
                    HandleEdit partHEdit;
                    HandleEdit jointpropHEdit;

                    sdiString jointStiffName = selConsStiffGen->GetName();
                    if (selConsStiffGen->GetKeyword().find("_TITLE") == string::npos)
                        jointStiffName = "CONSTRAINED_JOINT_STIFFNESS_" + to_string(selConsStiffGen->GetId());
                    p_radiossModel->CreateEntity(partHEdit, "/PART", jointStiffName);
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(partHEdit, sourceList));

                    p_radiossModel->CreateElement(sprElemHEdit,"/SPRING",nodeList,partHEdit,p_radiossModel->GetNextAvailableId(p_radiossModel->GetEntityType("/SPRING")));
                    if (sprElemHEdit.IsValid())
                    {
                        sdiConvert::Convert::PushToConversionLog(std::make_pair(sprElemHEdit, sourceList));
                        p_radiossModel->CreateEntity(jointpropHEdit, "/PROP/TYPE45", jointStiffName);
                        jointpropHEdit.SetValue(p_radiossModel, sdiIdentifier("Type"), sdiValue(jntType));
                        partHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), jointpropHEdit);
                        sdiConvert::Convert::PushToConversionLog(std::make_pair(jointpropHEdit, sourceList));

                        /* map values to created prop*/
                        EntityEdit jntPropEntEdit(p_radiossModel, jointpropHEdit);
                        if (jntType == 3)
                        {
                            if (cidAHread.IsValid())
                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                            if (cidBHread.IsValid())
                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, cidBHread.GetId(p_lsdynaModel))));
                            for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSDX}, {"SAx+", lsdPSDX }}))
                            {
                                int multplier = 1;
                                if (pair.first.find("-") != string::npos)
                                    multplier = -1;
                                jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second)));
                            }

                            for (const auto pair : map<sdiString, double>({ {"Kfrx", lsdESX}, { "FMx", lsdFFX } }))
                                jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));
                                        
                            jntPropEntEdit.SetValue(sdiIdentifier("Ktx"), sdiValue(0));
                            jntPropEntEdit.SetValue(sdiIdentifier("Ctx"), sdiValue(0));
                            jntPropEntEdit.SetValue(sdiIdentifier("Kftx"), sdiValue(0));
                            jntPropEntEdit.SetValue(sdiIdentifier("Crx"), sdiValue(0));
                            jntPropEntEdit.SetValue(sdiIdentifier("Krx"), sdiValue(0));

                            for (const auto pair : map<sdiString, HandleRead>({ { "fct_Krx", lcidxHread }, { "fct_Crx", dlcidxHread }, { "fct_fmx", ffxCrvHread } }))
                            {
                                if (pair.second.IsValid())
                                    jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                            }
                        }
                        else if (jntType != 2 && jntType != 6)
                        {
                            if (cidAHread.IsValid())
                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                            if (cidBHread.IsValid())
                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, cidBHread.GetId(p_lsdynaModel))));
                            for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSDX}, {"SAx+", lsdPSDX }, { "SAy-", lsdNSDY }, { "SAy+", lsdPSDY }, { "SAz-", lsdNSDZ }, { "SAz+", lsdPSDZ } }))
                            {
                                int multplier = 1;
                                if (pair.first.find("-") != string::npos)
                                    multplier = -1;
                                jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second)));
                            }

                            for (const auto pair : map<sdiString, double>({ {"Kfrx", lsdESX}, {"Kfry", lsdESY }, { "Kfrz", lsdESZ }, { "FMx", lsdFFX }, { "FMy", lsdFFY  }, { "FMz", lsdFFZ } }))
                                jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

                            for (const auto pair : map<sdiString, HandleRead>({ { "fct_Krx", lcidxHread }, { "fct_Kry", lcidyHread }, { "fct_Krz", lcidzHread }, { "fct_Crx", dlcidxHread }, { "fct_Cry", dlcidyHread }, { "fct_Crz", dlcidzHread }, { "fct_fmx", ffxCrvHread }, { "fct_fmy", ffyCrvHread }, { "fct_fmz", ffzCrvHread } }))
                            {
                                if (pair.second.IsValid())
                                    jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                            }
                        }
                        else 
                        {
                            if (cidAHread.IsValid())
                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                            if (cidBHread.IsValid())
                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, cidBHread.GetId(p_lsdynaModel))));
                            if (cidAHread.IsValid())
                            {
                                EntityRead jntEntRead(p_lsdynaModel, jidHread);
                                HandleRead node1Hread;
                                HandleRead node3Hread;
                                jntEntRead.GetEntityHandle(sdiIdentifier("N1"), node1Hread);
                                jntEntRead.GetEntityHandle(sdiIdentifier("N3"), node3Hread);
                                NodeRead node1Read(p_lsdynaModel, node1Hread);
                                NodeRead node3Read(p_lsdynaModel, node3Hread);
                                sdiTriple vectN3N1 = (node3Read.GetPosition() - node1Read.GetPosition()).Normalize();

                                EntityRead cidAEntRead(p_lsdynaModel, cidAHread);
                                EntityRead cidBEntRead(p_lsdynaModel, cidBHread);
                                vector<sdiTriple> localAxisList({ sdiTriple(0.0, 0.0, 0.0), sdiTriple(0.0, 0.0, 0.0), sdiTriple(0.0, 0.0, 0.0) });
                                vector<sdiTriple> localAxisList2({ sdiTriple(0.0, 0.0, 0.0), sdiTriple(0.0, 0.0, 0.0), sdiTriple(0.0, 0.0, 0.0) });
                                vector<double> axisOrigin({0.0, 0.0, 0.0});
                                vector<double> axisOrigin2({0.0, 0.0, 0.0});
                                p_GetLocalAxesFromDefineCoordinate(cidAEntRead, localAxisList, axisOrigin);
                                p_GetLocalAxesFromDefineCoordinate(cidBEntRead, localAxisList2, axisOrigin2);
                                int axisIndex = -1;
                                for (size_t i = 0; i < 3; ++i)
                                {
                                    // if cos(vectN3N1,localAxisList) > 0.99
                                    if (   abs(  (vectN3N1[0]*localAxisList[i][0]+vectN3N1[1]*localAxisList[i][1]+vectN3N1[2]*localAxisList[i][2]) /
                                           ( sqrt(vectN3N1[0]*vectN3N1[0]+vectN3N1[1]*vectN3N1[1]+vectN3N1[2]*vectN3N1[2])  *
                                             sqrt(localAxisList[i][0]*localAxisList[i][0]+localAxisList[i][1]*localAxisList[i][1]+localAxisList[i][2]*localAxisList[i][2]) ))   > 0.99 )
                                    {
                                        axisIndex = (int)i;
                                        break;
                                    }
                                }
                                if (axisIndex != -1)
                                {
                                    vector<sdiString> radAttNames({ "Ox", "Oy", "Oz" });
                                    vector<sdiString> attribVect1({ "X1", "Y1", "Z1" });
                                    vector<sdiString> attribVect2({ "X2", "Y2", "Z2" });
                                    HandleEdit skew1Handle;
                                    HandleEdit skew2Handle;
                                    int tempint = 1;  
                                    switch (axisIndex)
                                    {
                                    case 0:
                                        if (cidAHread.IsValid())
                                            jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, skew1Handle.GetId(p_lsdynaModel))));
                                        if (cidBHread.IsValid())
                                            jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, skew2Handle.GetId(p_lsdynaModel))));
                                        for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSDX}, {"SAx+", lsdPSDX } }))
                                        {
                                            int multplier = 1;
                                            if (pair.first.find("-") != string::npos)
                                                multplier = -1;
                                            jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second)));
                                            ++tempint;
                                        }
                                        for (const auto pair : map<sdiString, double>({ {"Kftx", lsdESX}, { "FMx", lsdFFX }}))
                                            jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

                                        for (const auto pair : map<sdiString, HandleRead>({ { "fct_Ktx", lcidxHread }, { "fct_Ctx", dlcidxHread }, { "fct_fmx", ffxCrvHread } }))
                                        {
                                            if (pair.second.IsValid())
                                                jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                                        }
                                        break;
                                    case 1:

                                        for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSDY}, {"SAx+", lsdPSDY } }))
                                        {
                                            int multplier = 1;
                                            if (pair.first.find("-") != string::npos)
                                                multplier = -1;
                                            jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second)));
                                            ++tempint;
                                        }
                                        for (const auto pair : map<sdiString, double>({ {"Kftx", lsdESY }, { "FMx", lsdFFY  },}))
                                            jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

                                        for (const auto pair : map<sdiString, HandleRead>({ { "fct_Ktx", lcidyHread }, { "fct_Ctx", dlcidyHread }, { "fct_fmx", ffyCrvHread } }))
                                        {
                                            if (pair.second.IsValid())
                                                jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                                        }
                                        break;
                                    case 2:
                                        for (const auto pair : map<sdiString, double>({ {"SAx-", lsdNSDZ}, {"SAx+", lsdNSDZ } }))
                                        {
                                            int multplier = 1;
                                            if (pair.first.find("-") != string::npos)
                                                multplier = -1;
                                            jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(multplier * abs(pair.second)));
                                            ++tempint;
                                        }
                                        for (const auto pair : map<sdiString, double>({ { "Kftx", lsdESZ }, { "FMx", lsdFFZ } }))
                                            jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

                                        for (const auto pair : map<sdiString, HandleRead>({ { "fct_Ktx", lcidzHread }, { "fct_Ctx", dlcidzHread }, { "fct_fmx", ffzCrvHread } }))
                                        {
                                            if (pair.second.IsValid())
                                                jntPropEntEdit.SetValue(sdiIdentifier(pair.first), sdiValue(sdiValueEntity(radCrvType, pair.second.GetId(p_lsdynaModel))));
                                        }
                                        break;
                                    default:
                                        break;
                                    }
                                    if (axisIndex == 0)
                                    {
                                        if (cidAHread.IsValid())
                                            jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                                        if (cidBHread.IsValid())
                                            jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, cidBHread.GetId(p_lsdynaModel))));
                                    }
                                    else if (axisIndex == 1)
                                    {
                                        p_radiossModel->CreateEntity(skew1Handle, "/SKEW/FIX", selConsStiffGen->GetName(), p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                                        EntityEdit skew1EntityEdit(p_radiossModel, skew1Handle);
                                        p_radiossModel->CreateEntity(skew2Handle, "/SKEW/FIX", selConsStiffGen->GetName(), p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                                        EntityEdit skew2EntityEdit(p_radiossModel, skew2Handle);
                                        if (skew1Handle.IsValid() && skew2Handle.IsValid())
                                        {
                                            for (size_t i = 0; i < 3; i++)
                                            {
                                                skew1EntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(axisOrigin[i]));
                                                skew1EntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(-localAxisList[0][i]));
                                                skew1EntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(localAxisList[2][i]));
                                                skew2EntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(axisOrigin2[i]));
                                                skew2EntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(-localAxisList2[0][i]));
                                                skew2EntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(localAxisList2[2][i]));
                                            }
                                            sdiConvert::Convert::PushToConversionLog(std::make_pair(skew1Handle, sourceList));
                                            sdiConvert::Convert::PushToConversionLog(std::make_pair(skew2Handle, sourceList));
                                            if (cidAHread.IsValid())
                                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, skew1Handle.GetId(p_lsdynaModel))));
                                            if (cidBHread.IsValid())
                                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, skew2Handle.GetId(p_lsdynaModel))));
                                        }
                                    }
                                    else if (axisIndex == 2)
                                    {
                                        p_radiossModel->CreateEntity(skew1Handle, "/SKEW/FIX", selConsStiffGen->GetName(), p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                                        EntityEdit skew1EntityEdit(p_radiossModel, skew1Handle);
                                        p_radiossModel->CreateEntity(skew2Handle, "/SKEW/FIX", selConsStiffGen->GetName(), p_ConvertUtils.GetDynaMaxEntityID(lsdDefCoordType));
                                        EntityEdit skew2EntityEdit(p_radiossModel, skew2Handle);
                                        if (skew1Handle.IsValid() && skew2Handle.IsValid())
                                        {
                                            for (size_t i = 0; i < 3; i++)
                                            {
                                                skew1EntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(axisOrigin[i]));
                                                skew1EntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(localAxisList[1][i]));
                                                skew1EntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(-localAxisList[0][i]));
                                                skew2EntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(axisOrigin2[i]));
                                                skew2EntityEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(localAxisList2[1][i]));
                                                skew2EntityEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(-localAxisList2[0][i]));
                                            }
                                            sdiConvert::Convert::PushToConversionLog(std::make_pair(skew1Handle, sourceList));
                                            sdiConvert::Convert::PushToConversionLog(std::make_pair(skew2Handle, sourceList));
                                            if (cidAHread.IsValid())
                                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, skew1Handle.GetId(p_lsdynaModel))));
                                            if (cidBHread.IsValid())
                                                jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID2"), sdiValue(sdiValueEntity(radSkewType, skew2Handle.GetId(p_lsdynaModel))));
                                        }
                                    }
                                }
                                else
                                {
                                    if (cidAHread.IsValid())
                                        jntPropEntEdit.SetValue(sdiIdentifier("Skew_ID1"), sdiValue(sdiValueEntity(radSkewType, cidAHread.GetId(p_lsdynaModel))));
                                }
                                
                            }
                            else
                            {
                                /* // post warning /error */
                            }
                        }
                        convertedJoints.push_back(jntId);
                    }
                }
            }
        }
    }
}





void sdiD2R::ConvertJoint::PopulateConstExtraNodePartRelation()
{
    SelectionRead selConsExtNodes(p_lsdynaModel, "*CONSTRAINED_EXTRA_NODES");
    EntityType radNodeType = p_radiossModel->GetEntityType("/NODE");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    EntityType lsdDefCoordType = p_lsdynaModel->GetEntityType("*DEFINE_COORDINATE");

    while (selConsExtNodes.Next())
    {
        sdiString slaveAttName("NID");
        EntityType slaveEntityType = radNodeType;
        sdiString keyWord = selConsExtNodes->GetKeyword();
        if (keyWord.find("SET") != keyWord.npos)
            slaveAttName = "NSID";
        HandleRead masterPidHRead;
        selConsExtNodes->GetEntityHandle(sdiIdentifier("PID"), masterPidHRead);
        if (masterPidHRead.IsValid())
        {
            HandleRead nodeEntHRead;
            selConsExtNodes->GetEntityHandle(sdiIdentifier(slaveAttName), nodeEntHRead);
            if (slaveAttName == "NID")
                mapPartIdVsExtraNodeList[masterPidHRead.GetId(p_lsdynaModel)].push_back(nodeEntHRead.GetId(p_lsdynaModel));
            else
            {
                sdiUIntList constExtraNodeList;
                HandleRead radSetHread;
                if (p_radiossModel->FindById("/SET/GENERAL", DynaToRad::GetRadiossSetIdFromLsdSet(nodeEntHRead.GetId(p_lsdynaModel),"*SET_NODE"), radSetHread))
                {
                    p_ConvertUtils.ExtractNodesFromRadiossSet(radSetHread, constExtraNodeList);
                    if (!constExtraNodeList.empty())
                    {
                        for(unsigned int constNode : constExtraNodeList)
                        mapPartIdVsExtraNodeList[masterPidHRead.GetId(p_lsdynaModel)].push_back(constNode);
                    }
                }
            }
        }
    }
}

void sdiD2R::ConvertJoint::p_GetLocalAxesFromDefineCoordinate(const EntityRead systEntRead, vector<sdiTriple>& axisList, vector<double>& axisOrigin)
{
    sdiString systKeyword = systEntRead.GetKeyword();
    if (systKeyword.find("NODE") != systKeyword.npos)
    {
        HandleRead systN1;
        HandleRead systN2;
        HandleRead systN3;
        systEntRead.GetEntityHandle(sdiIdentifier("N1"), systN1);
        systEntRead.GetEntityHandle(sdiIdentifier("N2"), systN2);
        systEntRead.GetEntityHandle(sdiIdentifier("N3"), systN3);

        NodeRead systN1Read(p_lsdynaModel, systN1);
        NodeRead systN2Read(p_lsdynaModel, systN2);
        NodeRead systN3Read(p_lsdynaModel, systN3);

        sdiString systDir;
        sdiValue value(systDir);
        systEntRead.GetValue(sdiIdentifier("DIR"), value);
        value.GetValue(systDir);

        sdiTriple axis1 = (systN2Read.GetPosition() - systN1Read.GetPosition()).Normalize();
        sdiTriple temp = (systN3Read.GetPosition() - systN1Read.GetPosition()).Normalize();
        sdiTriple axis3 = axis1 * temp;
        sdiTriple axis2 = axis1 * axis3;

        if (systDir.empty() || systDir == "X")
        {
            axisList[0] = axis1;
            axisList[1] = axis2;
            axisList[2] = axis3;
        }
        else if (systDir == "Y")
        {
            axisList[0] = axis3;
            axisList[1] = axis1;
            axisList[2] = axis2;
        }
        else if (systDir == "Z")
        {
            axisList[0] = axis2;
            axisList[1] = axis3;
            axisList[2] = axis1;
        }

        sdiTriple posN1 = systN1Read.GetPosition();
        axisOrigin[0] = posN1[0];
        axisOrigin[1] = posN1[1];
        axisOrigin[2] = posN1[2];
    }
    else
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
        p_ConvertUtils.GetAttribValues(systEntRead, attribNames, attribVals);
        axisList[0] = sdiTriple(lsdXL - lsdXO, lsdYL - lsdYO, lsdZL - lsdZO);
        sdiTriple temp = sdiTriple(lsdXP - lsdXO, lsdYP - lsdYO, lsdZP - lsdZO);
        axisList[2] = axisList[0] * temp;
        axisList[1] = axisList[0] * axisList[2];
        axisOrigin[0] = lsdXO;
        axisOrigin[1] = lsdYO;
        axisOrigin[2] = lsdZO;
    }

}


void sdiD2R::ConvertJoint::ConvertRegularJoints()
{
    EntityType radNodeType = p_radiossModel->GetEntityType("/NODE");
    EntityType radPartType = p_radiossModel->GetEntityType("/PART");
    HandleEdit cylJointPartHEdit;
    HandleEdit cylJointpropHEdit;
    HandleEdit lockJointPartHEdit;
    HandleEdit lockJointPropHEdit;
    HandleEdit revJointPartHEdit;
    HandleEdit revJointPropHEdit;
    HandleEdit sphJointPartHEdit;
    HandleEdit sphJointPropHEdit;
    HandleEdit traJointPartHEdit;
    HandleEdit traJointPropHEdit;
    HandleEdit univJointPartHEdit;
    HandleEdit univJointPropHEdit;
    HandleEdit planarJointPartHEdit;
    HandleEdit planarJointPropHEdit;
    SelectionRead selConstrJnts(p_lsdynaModel, srcCard);

    while (selConstrJnts.Next())
    {
        sdiVectorSort(convertedJoints);
        sdiString keyWord = selConstrJnts->GetKeyword();
        if (keyWord.find("STIFF") != keyWord.npos || binary_search(convertedJoints.begin(), convertedJoints.end(), selConstrJnts->GetId()))
            continue;

        sdiValueEntity N1Entity;
        sdiValueEntity N2Entity;
        sdiValueEntity N3Entity;
        sdiValueEntity N4Entity;
        sdiValueEntity N5Entity;
        sdiStringList nodeAttrNames({ "N1", "N2", "N3", "N4", "N5" });
        vector<reference_wrapper<sdiValueEntity>> nodeAttrVals({ N1Entity, N2Entity, N3Entity, N4Entity, N5Entity });
        p_ConvertUtils.GetAttribValues(*selConstrJnts, nodeAttrNames, nodeAttrVals);
        sdiUIntList nodeList({ N1Entity.GetId(), N2Entity.GetId(), N3Entity.GetId(), N4Entity.GetId(), N5Entity.GetId() });
        int propJntType = 0;

        if ((nodeList[0] != 0 && nodeList[0] != UINT_MAX) && (nodeList[1] != 0 && nodeList[1] != UINT_MAX))
        {
            sdiConvert::SDIHandlReadList sourceList = { {selConstrJnts->GetHandle()} };
            HandleElementEdit sprElemHEdit;

            if (keyWord.find("CYL") != keyWord.npos)
            {
                nodeList.resize(3);
                if (!cylJointPartHEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(cylJointPartHEdit, "/PART", "CONSTRAINED_JOINT_CYLINDRICAL");
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(cylJointPartHEdit, sourceList));
                }
                if (!cylJointpropHEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(cylJointpropHEdit, "/PROP/TYPE45", "CONSTRAINED_JOINT_CYLINDRICAL");
                    cylJointpropHEdit.SetValue(p_radiossModel, sdiIdentifier("Type"), sdiValue(3));
                    cylJointPartHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), cylJointpropHEdit);
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(cylJointpropHEdit, sourceList));
                }

                if (cylJointPartHEdit.IsValid())
                {
                    p_radiossModel->CreateElement(sprElemHEdit, "/SPRING", nodeList, cylJointPartHEdit,p_radiossModel->GetNextAvailableId(p_radiossModel->GetEntityType("/SPRING")));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(sprElemHEdit, sourceList));
                }
            }
            else if (keyWord.find("LOCK") != keyWord.npos)
            {
                nodeList[3] = nodeList[4];
                nodeList.resize(4);
                if (!lockJointPartHEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(lockJointPartHEdit, "/PART", "CONSTRAINED_JOINT_LOCKING");
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(lockJointPartHEdit, sourceList));
                }
                if (!lockJointPropHEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(lockJointPropHEdit, "/PROP/TYPE45", "CONSTRAINED_JOINT_LOCKING");
                    lockJointPropHEdit.SetValue(p_radiossModel, sdiIdentifier("Type"), sdiValue(8));
                    lockJointPartHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), lockJointPropHEdit);
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(lockJointPropHEdit, sourceList));
                }

                if (lockJointPartHEdit.IsValid())
                {
                    p_radiossModel->CreateElement(sprElemHEdit, "/SPRING", nodeList, lockJointPartHEdit,p_radiossModel->GetNextAvailableId(p_radiossModel->GetEntityType("/SPRING")));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(sprElemHEdit, sourceList));
                }
            }
            else if (keyWord.find("REVOLUTE") != keyWord.npos)
            {
                nodeList.resize(3);
                if (!revJointPartHEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(revJointPartHEdit, "/PART", "CONSTRAINED_JOINT_REVOLUTE");
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(revJointPartHEdit, sourceList));
                }
                if (!revJointPropHEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(revJointPropHEdit, "/PROP/TYPE45", "CONSTRAINED_JOINT_REVOLUTE");
                    revJointPropHEdit.SetValue(p_radiossModel, sdiIdentifier("Type"), sdiValue(2));
                    revJointPartHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), revJointPropHEdit);
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(revJointPropHEdit, sourceList));
                }

                if (revJointPartHEdit.IsValid())
                {
                    p_radiossModel->CreateElement(sprElemHEdit, "/SPRING", nodeList, revJointPartHEdit,p_radiossModel->GetNextAvailableId(p_radiossModel->GetEntityType("/SPRING")));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(sprElemHEdit, sourceList));
                }
            }
            else if (keyWord.find("SPHER") != keyWord.npos)
            {
                nodeList.resize(2);
                if (!sphJointPartHEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(sphJointPartHEdit, "/PART", "CONSTRAINED_JOINT_SPHERICAL");
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(sphJointPartHEdit, sourceList));
                }
                if (!sphJointPropHEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(sphJointPropHEdit, "/PROP/TYPE45", "CONSTRAINED_JOINT_SPHERICAL");
                    sphJointPropHEdit.SetValue(p_radiossModel, sdiIdentifier("Type"), sdiValue(1));
                    sphJointPartHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), sphJointPropHEdit);
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(sphJointPropHEdit, sourceList));
                }

                if (sphJointPartHEdit.IsValid())
                {
                    p_radiossModel->CreateElement(sprElemHEdit, "/SPRING", nodeList, sphJointPartHEdit,p_radiossModel->GetNextAvailableId(p_radiossModel->GetEntityType("/SPRING")));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(sprElemHEdit, sourceList));
                }
            }
            else if (keyWord.find("TRANS") != keyWord.npos)
            {
                nodeList.resize(3);
                if (!traJointPartHEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(traJointPartHEdit, "/PART", "CONSTRAINED_JOINT_TRANSLATIONAL");
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(traJointPartHEdit, sourceList));
                }
                if (!traJointPropHEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(traJointPropHEdit, "/PROP/TYPE45", "CONSTRAINED_JOINT_TRANSLATIONAL");
                    traJointPropHEdit.SetValue(p_radiossModel, sdiIdentifier("Type"), sdiValue(6));
                    traJointPartHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), traJointPropHEdit);
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(traJointPropHEdit, sourceList));
                }

                if (traJointPartHEdit.IsValid())
                {
                    p_radiossModel->CreateElement(sprElemHEdit, "/SPRING", nodeList, traJointPartHEdit,p_radiossModel->GetNextAvailableId(p_radiossModel->GetEntityType("/SPRING")));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(sprElemHEdit, sourceList));
                }
            }
            else if (keyWord.find("UNIV") != keyWord.npos)
            {
                nodeList.resize(4);
                if (!univJointPartHEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(univJointPartHEdit, "/PART", "CONSTRAINED_JOINT_UNIVERSAL");
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(univJointPartHEdit, sourceList));
                }
                if (!univJointPropHEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(univJointPropHEdit, "/PROP/TYPE45", "CONSTRAINED_JOINT_UNIVERSAL");
                    univJointPropHEdit.SetValue(p_radiossModel, sdiIdentifier("Type"), sdiValue(5));
                    univJointPartHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), univJointPropHEdit);
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(univJointPropHEdit, sourceList));
                }

                if (univJointPartHEdit.IsValid())
                {
                    p_radiossModel->CreateElement(sprElemHEdit, "/SPRING", nodeList, univJointPartHEdit,p_radiossModel->GetNextAvailableId(p_radiossModel->GetEntityType("/SPRING")));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(sprElemHEdit, sourceList));
                }
            }
            else if (keyWord.find("PLAN") != keyWord.npos)
            {
                nodeList.resize(3);
                if (!planarJointPartHEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(planarJointPartHEdit, "/PART", "CONSTRAINED_JOINT_PLANAR");
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(planarJointPartHEdit, sourceList));
                }
                if (!planarJointPropHEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(planarJointPropHEdit, "/PROP/TYPE45", "CONSTRAINED_JOINT_PLANAR");
                    planarJointPropHEdit.SetValue(p_radiossModel, sdiIdentifier("Type"), sdiValue(4));
                    planarJointPartHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), planarJointPropHEdit);
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(planarJointPropHEdit, sourceList));
                }

                if (planarJointPartHEdit.IsValid())
                {
                    p_radiossModel->CreateElement(sprElemHEdit, "/SPRING", nodeList, planarJointPartHEdit,p_radiossModel->GetNextAvailableId(p_radiossModel->GetEntityType("/SPRING")));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(sprElemHEdit, sourceList));
                }
            }
            else
                continue;
        }
    }
}
