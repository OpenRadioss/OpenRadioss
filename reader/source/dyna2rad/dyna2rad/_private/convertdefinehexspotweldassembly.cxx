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

#include <dyna2rad/convertdefinehexspotweldassembly.h>
#include <dyna2rad/dyna2rad.h>

using namespace std;
using namespace sdi;

void sdiD2R::ConvertHexSpotweld::ConvertDefineHexSpotweldAssembly()
{
    ConvertEntities();
}

void sdiD2R::ConvertHexSpotweld::ConvertEntities()
{
    ConvertHexSpotwelds();

    p_CreateThCluster();
}

void sdiD2R::ConvertHexSpotweld::ConvertHexSpotwelds()
{
    SelectionRead selHexSpotWeld(p_lsdynaModel, "*DEFINE_HEX_SPOTWELD_ASSEMBLY");
    EntityType radSolidType = p_radiossModel->GetEntityType("/BRICK");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    EntityType radSkewType = p_radiossModel->GetEntityType("/SKEW");

    while (selHexSpotWeld.Next())
    {
        sdiConvert::SDIHandlReadList sourceCluster = { {selHexSpotWeld->GetHandle()} };
        sdiString hexName = selHexSpotWeld->GetName();
        int idsmax = GetValue<int>(*selHexSpotWeld, "idsmax");
        
        sdiUIntList sdiList;
        for (int i = 0; i < idsmax; ++i)
        {
            sdiValueEntity lsdids = GetValue<sdiValueEntity>(*selHexSpotWeld, (idsmax == 1) ? "ids" : "ids", i);
            sdiList.push_back(lsdids.GetId());
        }

        HandleEdit radClusterHEdit;
        p_radiossModel->CreateEntity(radClusterHEdit, "/CLUSTER/BRICK", hexName, selHexSpotWeld->GetId());

        if(radClusterHEdit.IsValid())
        {
            EntityEdit radClusterEdit(p_radiossModel, radClusterHEdit);

            radClusterEdit.SetValue(sdiIdentifier("elem_type"), sdiValue(1));
            radClusterEdit.SetValue(sdiIdentifier("Ifail"), sdiValue(3));
            radClusterEdit.SetValue(sdiIdentifier("skew_ID"), sdiValue(sdiValueEntity(radSkewType, 0)));
            radClusterEdit.SetValue(sdiIdentifier("a1"), sdiValue(1.0));
            radClusterEdit.SetValue(sdiIdentifier("a2"), sdiValue(1.0));
            radClusterEdit.SetValue(sdiIdentifier("a3"), sdiValue(1.0));
            radClusterEdit.SetValue(sdiIdentifier("a4"), sdiValue(1.0));
            radClusterEdit.SetValue(sdiIdentifier("b1"), sdiValue(1.0));
            radClusterEdit.SetValue(sdiIdentifier("b2"), sdiValue(1.0));
            radClusterEdit.SetValue(sdiIdentifier("b3"), sdiValue(1.0));
            radClusterEdit.SetValue(sdiIdentifier("b4"), sdiValue(1.0));

            // new SET
            if(sdiList.size() > 0)
            {
                HandleEdit SolidSetHedit;
                p_radiossModel->CreateEntity(SolidSetHedit, "/SET/GENERAL", "GENERAL_SOLID_HEX_SPOTWELD_" + to_string(selHexSpotWeld->GetId()));
                EntityEdit SolidSetedit(p_radiossModel, SolidSetHedit);

                SolidSetedit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                SolidSetedit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("SOLID")));
                SolidSetedit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue((int) sdiList.size()));
                SolidSetedit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radSolidType, sdiList)));
                //radClusterEdit.SetEntityHandle(sdiIdentifier("group_ID"), SolidSetHedit);
                radClusterEdit.SetValue(sdiIdentifier("group_ID"), sdiValue(sdiValueEntity(radSetType, SolidSetedit.GetId())));

                sdiConvert::Convert::PushToConversionLog(std::make_pair(SolidSetHedit, sourceCluster));

                // dyna part read (attached to SOLID of HEX_SPOTWELD)
                int eid = sdiList[0];
                HandleRead elementdynaHRead;
                p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*ELEMENT_SOLID"), eid, elementdynaHRead);
                HandleRead partdynaHRead;
                elementdynaHRead.GetEntityHandle(p_lsdynaModel,sdiIdentifier("PID"), partdynaHRead);
              
                if(partdynaHRead.IsValid())
                {
                    HandleRead matdynaHRead;
                    partdynaHRead.GetEntityHandle(p_lsdynaModel, sdiIdentifier("MID"), matdynaHRead);
                    if(partdynaHRead.IsValid())
                    {
                        EntityRead matdynaRead(p_lsdynaModel, matdynaHRead);

                        //-----------------
                        // - NRR - function
                        //-----------------
                        int NRRFlag = GetValue<int>(matdynaRead, "NRRFlag");

                        if(NRRFlag == 0)
                        {
                            p_ConvertUtils.CopyValue(matdynaRead, radClusterEdit, "LSD_MAT100_NRR", "fn_fail1");
                        }
                        else if(NRRFlag == 1)
                        {
                            HandleRead curveNRR;
                            matdynaRead.GetEntityHandle(sdiIdentifier("LSD_LCID2"), curveNRR);
                            if (curveNRR.IsValid())
                            {
                                EntityRead crvNRRRead(p_lsdynaModel, curveNRR);

                                int crvLen = GetValue<int>(crvNRRRead, "numberofpoints");
                                if (crvLen)
                                {
                                    sdiDoubleList curvePnts = GetValue<sdiDoubleList>(crvNRRRead, "points");
                                    double lsdNRR = curvePnts[1];
                                    radClusterEdit.SetValue(sdiIdentifier("fn_fail1"), sdiValue(lsdNRR));
                                }
                            }
                        }
                        //-----------------
                        // - NRS - function
                        //-----------------
                        int NRSFlag = GetValue<int>(matdynaRead, "NRSFlag");

                        double lsdNRS = 0.0;
                        if(NRSFlag == 0)
                        {
                            lsdNRS = GetValue<double>(matdynaRead, "LSD_MAT100_NRS");
                        }
                        else if(NRSFlag == 1)
                        {
                            HandleRead curveNRS;
                            matdynaRead.GetEntityHandle(sdiIdentifier("LSD_LCID"), curveNRS);
                            if (curveNRS.IsValid())
                            {
                                EntityRead crvNRSRead(p_lsdynaModel, curveNRS);

                                int crvLen = GetValue<int>(crvNRSRead, "numberofpoints");
                                if (crvLen)
                                {
                                    sdiDoubleList curvePnts = GetValue<sdiDoubleList>(crvNRSRead, "points");
                                    lsdNRS = curvePnts[1];
                                }
                            }
                        }
                        //-----------------
                        // - NRT - function
                        //-----------------
                        int NRTFlag = GetValue<int>(matdynaRead, "NRTFlag");

                        double lsdNRT = 0.0;
                        if(NRTFlag == 0)
                        {
                            lsdNRT = GetValue<double>(matdynaRead, "LSD_MAT100_NRT");
                        }
                        else if(NRTFlag == 1)
                        {
                            HandleRead curveNRT;
                            matdynaRead.GetEntityHandle(sdiIdentifier("LSD_NRTID"), curveNRT);
                            if (curveNRT.IsValid())
                            {
                                EntityRead crvNRTRead(p_lsdynaModel, curveNRT);

                                int crvLen = GetValue<int>(crvNRTRead, "numberofpoints");
                                if (crvLen)
                                {
                                    sdiDoubleList curvePnts = GetValue<sdiDoubleList>(crvNRTRead, "points");
                                    lsdNRT = curvePnts[1];
                                }
                            }
                        }

                        double fs_fail = sqrt(pow(lsdNRS,2) + pow(lsdNRT,2));
                        radClusterEdit.SetValue(sdiIdentifier("fs_fail"), sdiValue(fs_fail));

                        //-----------------
                        // - MRR - function
                        //-----------------
                        int MRRFlag = GetValue<int>(matdynaRead, "MRRFlag");

                        if(MRRFlag == 0)
                        {
                            p_ConvertUtils.CopyValue(matdynaRead, radClusterEdit, "LSD_MAT100_MRR", "mt_fail");
                        }
                        else if(MRRFlag == 1)
                        {
                            HandleRead curveMRR;
                            matdynaRead.GetEntityHandle(sdiIdentifier("LSD_MRRID"), curveMRR);
                            if (curveMRR.IsValid())
                            {
                                EntityRead crvMRRRead(p_lsdynaModel, curveMRR);

                                int crvLen = GetValue<int>(crvMRRRead, "numberofpoints");
                                if (crvLen)
                                {
                                    sdiDoubleList curvePnts = GetValue<sdiDoubleList>(crvMRRRead, "points");
                                    double lsdMRR = curvePnts[1];
                                    radClusterEdit.SetValue(sdiIdentifier("mt_fail"), sdiValue(lsdMRR));
                                }
                            }
                        }
                        //-----------------
                        // - MSS - function
                        //-----------------
                        int MSSFlag = GetValue<int>(matdynaRead, "MSSFlag");

                        double lsdMSS = 0.0;
                        if(MSSFlag == 0)
                        {
                            lsdMSS = GetValue<double>(matdynaRead, "LSD_MAT100_MSS");
                        }
                        else if(MSSFlag == 1)
                        {
                            HandleRead curveMSS;
                            matdynaRead.GetEntityHandle(sdiIdentifier("LSD_MSSID"), curveMSS);
                            if (curveMSS.IsValid())
                            {
                                EntityRead crvMSSRead(p_lsdynaModel, curveMSS);

                                int crvLen = GetValue<int>(crvMSSRead, "numberofpoints");
                                if (crvLen)
                                {
                                    sdiDoubleList curvePnts = GetValue<sdiDoubleList>(crvMSSRead, "points");
                                    lsdMSS = curvePnts[1];
                                }
                            }
                        }
                        //-----------------
                        // - MTT - function
                        //-----------------
                        int MTTFlag = GetValue<int>(matdynaRead, "MTTFlag");

                        double lsdMTT = 0.0;
                        if(MTTFlag == 0)
                        {
                            lsdMTT = GetValue<double>(matdynaRead, "LSD_MAT100_MTT");
                        }
                        else if(MTTFlag == 1)
                        {
                            HandleRead curveMTT;
                            matdynaRead.GetEntityHandle(sdiIdentifier("LSD_MTTID"), curveMTT);
                            if (curveMTT.IsValid())
                            {
                                EntityRead crvMTTRead(p_lsdynaModel, curveMTT);

                                int crvLen = GetValue<int>(crvMTTRead, "numberofpoints");
                                if (crvLen)
                                {
                                    sdiDoubleList curvePnts = GetValue<sdiDoubleList>(crvMTTRead, "points");
                                    lsdMTT = curvePnts[1];
                                }
                            }
                        }
                        double mb_fail = sqrt(pow(lsdMSS,2) + pow(lsdMTT,2));
                        radClusterEdit.SetValue(sdiIdentifier("mb_fail"), sdiValue(mb_fail));
                    }
                }
            }
            //---
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radClusterHEdit, sourceCluster));
        }
    }
}

void sdiD2R::ConvertHexSpotweld::p_CreateThCluster()
{
    sdiUIntList clusterIdList;
    sdiConvert::SDIHandlReadList targetHandlesList;
    SelectionRead selCluster(p_radiossModel, "/CLUSTER");

    int cptCluster = 0;
    while (selCluster.Next())
    {
        sdiConvert::SDIHandlReadList dynaSourceHandles;
        sdiConvert::Convert::GetSourceHandles(p_radiossModel, selCluster->GetHandle(), dynaSourceHandles);
        if(dynaSourceHandles.size() > 0) cptCluster = cptCluster + 1;
    }

    selCluster.Restart();
    clusterIdList.reserve(cptCluster);


    while (selCluster.Next())
    {
        sdiConvert::SDIHandlReadList dynaSourceHandles;
        sdiConvert::Convert::GetSourceHandles(p_radiossModel, selCluster->GetHandle(), dynaSourceHandles);
        if(dynaSourceHandles.size() > 0)
        {
            clusterIdList.push_back(selCluster->GetId());
            targetHandlesList.push_back(selCluster->GetHandle());
        }
    }

    if (!clusterIdList.empty())
    {
        HandleEdit radThClusterHEdit;
        p_radiossModel->CreateEntity(radThClusterHEdit, "/TH/CLUSTER", "TH-CLUSTER");

        EntityEdit radThClusterEdit(p_radiossModel, radThClusterHEdit);
        radThClusterEdit.SetValue(sdiIdentifier("idsmax"), sdiValue((int)clusterIdList.size()));
        radThClusterEdit.SetValue(sdiIdentifier("Number_Of_Variables"), sdiValue(1));
        radThClusterEdit.SetValue(sdiIdentifier("Obj"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/CLUSTER"), clusterIdList)));
        radThClusterEdit.SetValue(sdiIdentifier("var"), sdiValue(sdiStringList({ "DEF" })));

        if (radThClusterHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList radThClusterSourceHandles;
            for (int i = 0; i < targetHandlesList.size(); i=i+1)
            {
                sdiConvert::SDIHandlReadList radClusterSourceHandles;
                sdiConvert::Convert::GetSourceHandles(p_radiossModel, targetHandlesList[i], radClusterSourceHandles);
                for(size_t i = 0; i < radClusterSourceHandles.size(); ++i)
                {
                    radThClusterSourceHandles.push_back(radClusterSourceHandles[i]);
                }
            }
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radThClusterHEdit, radThClusterSourceHandles));
        }
    }
}