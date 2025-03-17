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
#include <dyna2rad/convertcurves.h>

using namespace sdiD2R;
using namespace std;
using namespace sdi;

void ConvertCurve::ConvertCurves()
{
    ConvertEntities();
}

void ConvertCurve::ConvertEntities()
{
    ConvertDefineCurves();

    ConvertDefineTables();

    ConvertDefineCurves_Smooth();
}


void ConvertCurve::ConvertDefineTables()
{
    SelectionRead readTable(p_lsdynaModel, "*DEFINE_TABLE");


    while (readTable.Next())
    {
        sdiUIntList funcIdList;
        sdiValueEntityList curveList;
        sdiValue tempValue(curveList);
        readTable->GetValue(sdiIdentifier("CurveIds"), tempValue);

        sdiString keyWordLog = readTable->GetKeyword();

        if (keyWordLog.find("DEFINE_TABLE_3D") != keyWordLog.npos)
        {
            // table 3D
            int n2DTab = 0;
            sdiUIntList tabIdList;
            sdiValueEntityList tabList;
            sdiValue tempValue(tabList);
            readTable->GetValue(sdiIdentifier("TableIds"), tempValue);
            tempValue.GetValue(tabList);
            tabList.GetIdList(tabIdList);
            n2DTab = (int)tabIdList.size();

            sdiDoubleList strainRates3DList;
            tempValue = sdiValue(strainRates3DList);
            readTable->GetValue(sdiIdentifier("VALUE"), tempValue);
            tempValue.GetValue(strainRates3DList);

            HandleEdit Rad3DTableEditHandle;
            p_radiossModel->CreateEntity(Rad3DTableEditHandle, "/TABLE/1", readTable->GetName(), readTable->GetId());
            EntityEdit rad3DTableEntEdit(p_radiossModel, Rad3DTableEditHandle);

            //  loop over n2DTab of table 3D

            int nFunct3D = 0;
            // get dimension of table 3D
            for (int i = 0; i < n2DTab; i++)
            {
                // loop over functions of each table
                HandleRead tab2dHandle;
                p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*DEFINE_TABLE"), tabIdList[i], tab2dHandle);

                int nFunct = 0;
                sdiUIntList funcIdList;
                sdiValueEntityList curveList;
                sdiValue tempValue(curveList);
                tab2dHandle.GetValue(p_lsdynaModel,sdiIdentifier("CurveIds"), tempValue);
                tempValue.GetValue(curveList);
                curveList.GetIdList(funcIdList);
                nFunct = (int)funcIdList.size();
                nFunct3D = nFunct3D + nFunct;
            }

            sdiUIntList func3DIdList;
            sdiDoubleList A3List,B3List;
            func3DIdList.reserve(nFunct3D);
            A3List.reserve(nFunct3D);
            B3List.reserve(nFunct3D);
            sdiDoubleList scale3DList(nFunct3D, 0.0);

            for (int i = 0; i < n2DTab; i++)
            {
                // loop over functions of each table
 
                HandleRead tab2dHandle;
                p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*DEFINE_TABLE"), tabIdList[i], tab2dHandle);

                double lsdSFA = p_ConvertUtils.GetValue<double>(*readTable, "SFA");
                double lsdOFFA = p_ConvertUtils.GetValue<double>(*readTable, "OFFA");

                if(lsdSFA != 0.0) strainRates3DList[i] = lsdSFA*(strainRates3DList[i] + lsdOFFA);

                int nFunct = 0;
                sdiUIntList funcIdList;
                sdiValueEntityList curveList;
                sdiValue tempValue(curveList);
                tab2dHandle.GetValue(p_lsdynaModel,sdiIdentifier("CurveIds"), tempValue);
                tempValue.GetValue(curveList);
                curveList.GetIdList(funcIdList);
                nFunct = (int)funcIdList.size();

                sdiDoubleList strainRates2DList;
                tempValue = sdiValue(strainRates2DList);
                tab2dHandle.GetValue(p_lsdynaModel,sdiIdentifier("VALUE"), tempValue);
                tempValue.GetValue(strainRates2DList);

                for (int j = 0; j < nFunct; j++)
                {
                    func3DIdList.push_back(funcIdList[j]);
                    A3List.push_back(strainRates3DList[i]);
                    B3List.push_back(strainRates2DList[j]);
                }
            }

            rad3DTableEntEdit.SetValue(sdiIdentifier("dimension"), sdiValue(3));
            rad3DTableEntEdit.SetValue(sdiIdentifier("curverows"), sdiValue(int(nFunct3D)));
            rad3DTableEntEdit.SetValue(sdiIdentifier("fct_ID"), sdiValue(sdiValueEntityList(destEntityType, func3DIdList)));
            rad3DTableEntEdit.SetValue(sdiIdentifier("A"), sdiValue(A3List));
            rad3DTableEntEdit.SetValue(sdiIdentifier("B"), sdiValue(B3List));

            rad3DTableEntEdit.SetValue(sdiIdentifier("Scale_y"), sdiValue(scale3DList));

            SDIHandlReadList dynaTables = { {readTable->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(Rad3DTableEditHandle, dynaTables));
        }
        else
        {
            // table 2D

            int nFunct = 0;
            sdiUIntList funcIdList;
            sdiValueEntityList curveList;
            sdiValue tempValue(curveList);
            readTable->GetValue(sdiIdentifier("CurveIds"), tempValue);
            tempValue.GetValue(curveList);
            curveList.GetIdList(funcIdList);
            nFunct = (int)funcIdList.size();
            
            double lsdSFA = p_ConvertUtils.GetValue<double>(*readTable, "SFA");
            double lsdOFFA = p_ConvertUtils.GetValue<double>(*readTable, "OFFA");

            sdiDoubleList strainRatesList;
            tempValue = sdiValue(strainRatesList);
            readTable->GetValue(sdiIdentifier("VALUE"), tempValue);
            tempValue.GetValue(strainRatesList);

            for (int i = 0; i < nFunct; i++)
            {
                if(lsdSFA != 0.0) strainRatesList[i] = lsdSFA*(strainRatesList[i] + lsdOFFA);
            }

            sdiDoubleList scaleList(nFunct, 0.0);

            HandleEdit RadTableEditHandle;
            p_radiossModel->CreateEntity(RadTableEditHandle, "/TABLE/1", readTable->GetName(), readTable->GetId());
            EntityEdit radTableEntEdit(p_radiossModel, RadTableEditHandle);

            radTableEntEdit.SetValue(sdiIdentifier("dimension"), sdiValue(2));
            radTableEntEdit.SetValue(sdiIdentifier("curverows"), sdiValue(nFunct));
            radTableEntEdit.SetValue(sdiIdentifier("fct_ID"), sdiValue(sdiValueEntityList(destEntityType, funcIdList)));
            
            radTableEntEdit.SetValue(sdiIdentifier("A"), sdiValue(strainRatesList));
            radTableEntEdit.SetValue(sdiIdentifier("Scale_y"), sdiValue(scaleList));

            SDIHandlReadList dynaTables = { {readTable->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(RadTableEditHandle, dynaTables));
        }
    }
}

void ConvertCurve::ConvertDefineCurves()
{
    SelectionRead readCurves(p_lsdynaModel, "*DEFINE_CURVE");
    while (readCurves.Next())
    {
      sdiString functCard = readCurves->GetKeyword();
      if( functCard.find("SMOOTH") == string::npos )
      {
        HandleEdit radCurve;
        p_radiossModel->CreateEntity(radCurve, "/FUNCT", readCurves->GetName(), readCurves->GetId());
        if (radCurve.IsValid())
        {
            EntityEdit radCurveEdit(p_radiossModel, radCurve);
            int nPnts = 0;
            sdiString curveName = readCurves->GetName();
            unsigned int curveId = readCurves->GetId();
            sdiValue tempValue;
            readCurves->GetValue(sdiIdentifier("numberofpoints"), tempValue);
            radCurveEdit.SetValue(sdiIdentifier("numberofpoints"), tempValue);
            tempValue.GetValue(nPnts);

            sdiValue tempCurePtsValue;
            readCurves->GetValue(sdiIdentifier("points"), tempCurePtsValue);
            radCurveEdit.SetValue(sdiIdentifier("points"), tempCurePtsValue);

            HandleEdit radMvFunct;
            p_radiossModel->CreateEntity(radMvFunct, "/MOVE_FUNCT", curveName, curveId);

            SDIHandlReadList dynaCurves = { {readCurves->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radCurve, dynaCurves));

            if (radMvFunct.IsValid())
            {
                double lsdSFA = p_ConvertUtils.GetValue<double>(*readCurves, "SFA");
                double lsdSFO = p_ConvertUtils.GetValue<double>(*readCurves, "SFO");
                double lsdOFFA = p_ConvertUtils.GetValue<double>(*readCurves, "OFFA");
                double lsdOFFO = p_ConvertUtils.GetValue<double>(*readCurves, "OFFO");

                bool lsdSFAisParam = readCurves->IsParameterized(sdiIdentifier("SFA"));
                bool lsdSFOisParam = readCurves->IsParameterized(sdiIdentifier("SFO"));
                bool lsdOFFAisParam = readCurves->IsParameterized(sdiIdentifier("OFFA"));
                bool lsdOFFOisParam = readCurves->IsParameterized(sdiIdentifier("OFFO"));

                EntityEdit radMvFnctEdit(p_radiossModel, radMvFunct);

                if(lsdSFAisParam) p_ConvertUtils.CopyValue(*readCurves, radMvFnctEdit, "SFA", "Ascalex");
                else if(lsdSFA != 0.0) radMvFnctEdit.SetValue(sdiIdentifier("Ascalex"), sdiValue(lsdSFA));
                else radMvFnctEdit.SetValue(sdiIdentifier("Ascalex"), sdiValue(1.0));

                if(lsdSFOisParam) p_ConvertUtils.CopyValue(*readCurves, radMvFnctEdit, "SFO", "Fscaley");
                else if(lsdSFO != 0.0) radMvFnctEdit.SetValue(sdiIdentifier("Fscaley"), sdiValue(lsdSFO));
                else radMvFnctEdit.SetValue(sdiIdentifier("Fscaley"), sdiValue(1.0));

                if(lsdSFAisParam && (lsdOFFAisParam || 0 != lsdOFFA))
                {
                    // This would be correct, but doesn't work in 2022.1 because of "==":
                    // p_ConvertUtils.SetExpressionValue(*readCurves, radMvFnctEdit, "((SFA==0)+SFA)*OFFA", "Ashiftx");
                    if(0 == lsdSFA) p_ConvertUtils.CopyValue(*readCurves, radMvFnctEdit, "OFFA", "Ashiftx");
                    else p_ConvertUtils.SetExpressionValue(*readCurves, radMvFnctEdit, "SFA*OFFA", "Ashiftx");
                }
                else if(lsdOFFAisParam && (1 == lsdSFA || 0 == lsdSFA))
                {
                    p_ConvertUtils.CopyValue(*readCurves, radMvFnctEdit, "OFFA", "Ashiftx");
                }
                else if(lsdOFFAisParam)
                {
                    p_ConvertUtils.SetExpressionValue(*readCurves, radMvFnctEdit, "SFA*OFFA", "Ashiftx");
                }
                else
                {
                    lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
                    radMvFnctEdit.SetValue(sdiIdentifier("Ashiftx"), sdiValue(lsdSFA*lsdOFFA));
                }

                if(lsdSFOisParam && (lsdOFFOisParam || 0 != lsdOFFO))
                {
                    // This would be correct, but doesn't work in 2022.1 because of "==":
                    // p_ConvertUtils.SetExpressionValue(*readCurves, radMvFnctEdit, "((SFO==0)+SFO)*OFFO", "Fshifty");
                    if(0 == lsdSFO) p_ConvertUtils.CopyValue(*readCurves, radMvFnctEdit, "OFFO", "Fshifty");
                    else p_ConvertUtils.SetExpressionValue(*readCurves, radMvFnctEdit, "SFO*OFFO", "Fshifty");
                }
                else if(lsdOFFOisParam && (1 == lsdSFO || 0 == lsdSFO))
                {
                    p_ConvertUtils.CopyValue(*readCurves, radMvFnctEdit, "OFFO", "Fshifty");
                }
                else if(lsdOFFOisParam)
                {
                    p_ConvertUtils.SetExpressionValue(*readCurves, radMvFnctEdit, "SFO*OFFO", "Fshifty");
                }
                else
                {
                    lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
                    radMvFnctEdit.SetValue(sdiIdentifier("Fshifty"), sdiValue(lsdSFO*lsdOFFO));
                }
            }
        }
      }
    }
}

void ConvertCurve::ConvertDefineCurves_Smooth()
{
    SelectionRead readCurves(p_lsdynaModel, "*DEFINE_CURVE_SMOOTH");
    while (readCurves.Next())
    {
        double lsdDIST = GetValue<double>(*readCurves, "DIST");
        double lsdTSTART = GetValue<double>(*readCurves, "TSTART");
        double lsdTEND = GetValue<double>(*readCurves, "TEND");
        double lsdTRISE = GetValue<double>(*readCurves, "TRISE");
        double lsdVMAX = GetValue<double>(*readCurves, "VMAX");

        HandleEdit radCurve;
        p_radiossModel->CreateEntity(radCurve, "/FUNCT_SMOOTH", readCurves->GetName(), readCurves->GetId());
        if (radCurve.IsValid())
        {
            EntityEdit radCurveEdit(p_radiossModel, radCurve);
            int nPnts = 4;
            sdiString curveName = readCurves->GetName();
            unsigned int curveId = readCurves->GetId();

            if(lsdVMAX == 0.0 && lsdTEND == 0.0)
            {
                // error message
                DynaToRad::ShowMessage(sdiMessageHandler::Level::Error, 39,readCurves->GetKeyword().c_str(), readCurves->GetId());
                break;
            }
            else if(lsdVMAX == 0.0 && (lsdTEND-lsdTSTART-lsdTRISE) != 0.0)
            {
                lsdVMAX = lsdDIST/(lsdTEND-lsdTSTART-lsdTRISE);
            }
            else if(lsdVMAX != 0.0)
            {
                lsdTEND =   lsdDIST/lsdVMAX + lsdTSTART + lsdTRISE;
            }
            
            radCurveEdit.SetValue(sdiIdentifier("numberofpoints"), sdiValue(nPnts));
            sdiDoubleList pntsToCreateCrv;

            pntsToCreateCrv.push_back(lsdTSTART);
            pntsToCreateCrv.push_back(0.0);
            pntsToCreateCrv.push_back(lsdTSTART+lsdTRISE);
            pntsToCreateCrv.push_back(lsdVMAX);
            pntsToCreateCrv.push_back(lsdTEND-lsdTRISE);
            pntsToCreateCrv.push_back(lsdVMAX);
            pntsToCreateCrv.push_back(lsdTEND);
            pntsToCreateCrv.push_back(0.0);

            radCurveEdit.SetValue(sdiIdentifier("points"), sdiValue(pntsToCreateCrv));
            radCurveEdit.SetValue(sdiIdentifier("Ascalex"), sdiValue(1.0));
            radCurveEdit.SetValue(sdiIdentifier("Fscaley"), sdiValue(1.0));
            radCurveEdit.SetValue(sdiIdentifier("Ashiftx"), sdiValue(0.0));
            radCurveEdit.SetValue(sdiIdentifier("Fshifty"), sdiValue(0.0));

            //-----------------
            SDIHandlReadList dynaCurves = { {readCurves->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radCurve, dynaCurves));
            //-----------------
        }
    }
}