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

#include <dyna2rad/convertcontrolvols.h>
#include <dyna2rad/dyna2rad.h>
#include <dyna2rad/sdiUtils.h>
#include <typedef.h>

using namespace sdi;
using namespace std;

void sdiD2R::ConvertControlVolume::ConvertControlVolumes()
{
    ConvertEntities();
}

void sdiD2R::ConvertControlVolume::ConvertEntities()
{
    ConvertAirbagPressureVolume();
    ConvertAirbagSimpleModel();
    ConvertAirbagAdiabaticGasModel();
    p_CreateTHMonVolForDBAbstat();
    ConvertAirbagLoadCurve();
    ConvertInitialFoamReferenceGeometry();
    ConvertAirbagShellReferenceGeometry();
    ConvertAirbagParticle();
    ConvertAirbagLinearFluid();
}

void sdiD2R::ConvertControlVolume::ConvertAirbagPressureVolume()
{
    EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");
    EntityType radSurfType = p_radiossModel->GetEntityType("/SURF");

    SelectionRead selAirbagPressure(p_lsdynaModel, "*AIRBAG_SIMPLE_PRESSURE_VOLUME");
    while (selAirbagPressure.Next())
    {
        sdiValueEntity surfSet;
        int lsdCNLcidOpt = 0;
        double lsdCN = 0.0;
        double lsdBETA = 0.0;
        sdiValue tempValue;
        HandleEdit functEdit;
        HandleRead lcidHandle;

        tempValue = sdiValue(lsdCNLcidOpt);
        selAirbagPressure->GetValue(sdiIdentifier("LSD_LCIDOpt"), tempValue);
        tempValue.GetValue(lsdCNLcidOpt);
        
        int lsdSSTYP;
        selAirbagPressure->GetValue(sdiIdentifier("SIDTYP"), tempValue);
        tempValue.GetValue(lsdSSTYP);

        selAirbagPressure->GetEntityHandle(sdiIdentifier("LCID"), lcidHandle);

        HandleEdit radAirbagHEdit;
        p_radiossModel->CreateEntity(radAirbagHEdit, "/MONVOL/PRES", selAirbagPressure->GetName(), selAirbagPressure->GetId());
        EntityEdit radAirbagEdit(p_radiossModel, radAirbagHEdit);
        if (radAirbagHEdit.IsValid())
        {
            selAirbagPressure->GetValue(sdiIdentifier("SID"), tempValue);
            tempValue.GetValue(surfSet);
            

            if(lsdSSTYP == 0)
            {
                radAirbagEdit.SetValue(sdiIdentifier("surf_IDex"), sdiValue(sdiValueEntity(radSurfType, DynaToRad::GetRadiossSetIdFromLsdSet(surfSet.GetId(), "*SET_SEGMENT"))));
            }
            else
            {
                radAirbagEdit.SetValue(sdiIdentifier("surf_IDex"), sdiValue(sdiValueEntity(radSurfType, DynaToRad::GetRadiossSetIdFromLsdSet(surfSet.GetId(), "*SET_PART"))));
            }

            tempValue = sdiValue(lsdBETA);
            selAirbagPressure->GetValue(sdiIdentifier("BETA"), tempValue);
            tempValue.GetValue(lsdBETA);
            lsdBETA = (lsdBETA == 0) ? 1 : lsdBETA;
            //radAirbagEdit.SetValue(sdiIdentifier("Ffunc"), sdiValue(lsdBETA));

            if (lcidHandle.IsValid())
            {
                radAirbagEdit.SetValue(sdiIdentifier("Itypfun"), sdiValue(2));
                unsigned int lcid = lcidHandle.GetId(p_lsdynaModel);
                radAirbagEdit.SetValue(sdiIdentifier("fct_ID"), sdiValue(sdiValueEntity(radFunctType, lcid)));
            }
            else if (lsdCNLcidOpt == 0)
            {
                tempValue = sdiValue(lsdCN);
                selAirbagPressure->GetValue(sdiIdentifier("LSD_CN"), tempValue);
                tempValue.GetValue(lsdCN);

                sdiDoubleList abscissaList({ 1E-4, 1E-3, 1E-2, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0,
                                        1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 10.0, 100.0, 1000.0, 10000.0 });
                sdiDoubleList pointsList;
                pointsList.reserve(2 * abscissaList.size());
                for (double abscissa : abscissaList)
                {
                    pointsList.push_back(abscissa);
                    pointsList.push_back(lsdBETA * lsdCN * abscissa);
                }
                p_ConvertUtils.CreateCurve(selAirbagPressure->GetName(), (int)abscissaList.size(), pointsList, functEdit);
                if (functEdit.IsValid())
                    radAirbagHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("fct_ID"), functEdit);
            }
            else if (lsdCNLcidOpt > 0)
            {
                HandleEdit lcidEdit;
                selAirbagPressure->GetEntityHandle(sdiIdentifier("LSD_LCID10"), lcidHandle);
                if (lcidHandle.IsValid())
                {
                    radAirbagEdit.SetValue(sdiIdentifier("Itypfun"), sdiValue(3));
                    unsigned int lcid = lcidHandle.GetId(p_lsdynaModel);
                    radAirbagEdit.SetValue(sdiIdentifier("fct_ID"), sdiValue(sdiValueEntity(radFunctType, lcid)));
                }
            }

            sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagPressure->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radAirbagHEdit, sourceConVol));
            if (functEdit.IsValid())
                sdiConvert::Convert::PushToConversionLog(std::make_pair(functEdit, sourceConVol));
        }
    }
}

void sdiD2R::ConvertControlVolume::ConvertAirbagSimpleModel()
{
    EntityType radFunctType =  p_radiossModel->GetEntityType("/FUNCT");
    EntityType radMatType = p_radiossModel->GetEntityType("/MAT");
    EntityType radPropType = p_radiossModel->GetEntityType("/PROP");
    EntityType radSurfType = p_radiossModel->GetEntityType("/SURF");
    SelectionRead selSimpleAirbag(p_lsdynaModel, "*AIRBAG_SIMPLE_AIRBAG_MODEL");
    while (selSimpleAirbag.Next())
    {
        unsigned int airbagId = selSimpleAirbag->GetId();

        unsigned int rbid = 0;
        sdiValue tempValue(rbid);
        selSimpleAirbag->GetValue(sdiIdentifier("RBID"), tempValue);
        tempValue.GetValue(rbid);

        double lsdCV;
        double lsdCP;
        double lsdaT;
        double lsdMu;
        double lsdPE;
        double lsdArea;
        double lsdTExt;
        double lsdaA;
        double lsdaB;
        double lsdMW;
        double lsdGasc;
        double lsdRO;
        unsigned int lcidId;

        sdiValueEntity surfSet;
        selSimpleAirbag->GetValue(sdiIdentifier("SID"), tempValue);
        tempValue.GetValue(surfSet);
        
        int lsdSSTYP;
        selSimpleAirbag->GetValue(sdiIdentifier("SIDTYP"), tempValue);
        tempValue.GetValue(lsdSSTYP);

        vector<reference_wrapper<double>> attribVals({ lsdCV, lsdCP, lsdaT, lsdMu, lsdPE, lsdTExt,
                                                       lsdaB, lsdMW, lsdGasc, lsdArea, lsdaA, lsdRO});
        vector<sdiString> attribNames({ "CV", "CP", "T", "LSD_MU", "PE", "LSD_TEXT", "B", "MW", "GASC", "LSD_A", "LSD_aA", "RO"});
        p_ConvertUtils.GetAttribValues(*selSimpleAirbag, attribNames, attribVals);
        lsdTExt = (lsdTExt == 0) ? 295.0 : lsdTExt;
        if (lsdGasc != 0.0)
            lsdRO = (lsdRO == 0) ? lsdPE * lsdMW / lsdTExt / lsdGasc : lsdRO;
        sdiValueEntity lcidEntity;
        selSimpleAirbag->GetValue(sdiIdentifier("LCID"), tempValue);
        tempValue.GetValue(lcidEntity);
        lcidId = lcidEntity.GetId();

        HandleRead muCrvHread;
        HandleRead areaCrvHread;
        HandleRead louCrvHread;
        vector<reference_wrapper<HandleRead>> crvHandles({ muCrvHread, areaCrvHread, louCrvHread });
        sdiStringList crvAttrNames({ "LSD_MUEnt_Airbag", "LSD_AEnt", "LOU" });
        p_ConvertUtils.GetEntityHandles(*selSimpleAirbag, crvAttrNames, crvHandles);

        HandleEdit radAirbagHEdit;
        p_radiossModel->CreateEntity(radAirbagHEdit, "/MONVOL/AIRBAG1/", selSimpleAirbag->GetName(), airbagId);

        EntityEdit radAirbagEdit(p_radiossModel, radAirbagHEdit);

        if(lsdSSTYP == 0)
        {
            radAirbagEdit.SetValue(sdiIdentifier("surf_IDex"), sdiValue(sdiValueEntity(radSurfType, DynaToRad::GetRadiossSetIdFromLsdSet(surfSet.GetId(), "*SET_SEGMENT"))));
        }
        else
        {
            radAirbagEdit.SetValue(sdiIdentifier("surf_IDex"), sdiValue(sdiValueEntity(radSurfType, DynaToRad::GetRadiossSetIdFromLsdSet(surfSet.GetId(), "*SET_PART"))));
        }
            
        radAirbagEdit.SetValue(sdiIdentifier("Pext"), sdiValue(lsdPE));
        radAirbagEdit.SetValue(sdiIdentifier("T0"), sdiValue(lsdTExt));

        HandleRead iPorpCrvHread;
        double avent = lsdMu * lsdArea;
        sdiString radCrvAttr = "fct_IDP";
        double scaleFactor = 1;
        int numPoints = 0;
        sdiDoubleList ptsList;
        double lsdSFA;
        double lsdSFO;
        double lsdOFFA;
        double lsdOFFO;
        HandleEdit curveHEdit;
        if (areaCrvHread.IsValid() && muCrvHread.IsValid())
        {
            vector< reference_wrapper<double>> curveDetails({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
            p_ConvertUtils.GetCurveDetails(areaCrvHread, numPoints, ptsList, curveDetails);

            double lsdMuSFA;
            double lsdMuSFO;
            double lsdMuOFFA;
            double lsdMuOFFO;
            sdiDoubleList muPtsList;
            int numMuPoints = 0;
            vector< reference_wrapper<double>> muCurveDetails({ lsdMuSFA, lsdMuSFO, lsdMuOFFA, lsdMuOFFO });
            p_ConvertUtils.GetCurveDetails(muCrvHread, numMuPoints, muPtsList, muCurveDetails);
            if (numPoints == numMuPoints)
            {
                for (size_t i = 0; i < 2 * numPoints; i += 2)
                {
                    ptsList[i] = lsdSFA * (ptsList[i] + lsdOFFA) + lsdMuSFA * (muPtsList[i] + lsdMuOFFA);
                    ptsList[i+1] = lsdSFA * (ptsList[i+1] + lsdOFFO) * lsdMuSFO * (muPtsList[i+1] + lsdMuOFFO);
                }
            }
            avent = 1.0;
            p_ConvertUtils.CreateCurve("Recomputed Curve Using Area And Mu Curves", numPoints, ptsList, curveHEdit, 1.0, 1.0, lsdPE, 0.0);
        }
        else if (lsdMu > 0.0 && areaCrvHread.IsValid())
        {
            avent = lsdMu;
            iPorpCrvHread = areaCrvHread;
        }
        else if (muCrvHread.IsValid() && lsdArea > 0.0)
        {
            avent = lsdArea;
            iPorpCrvHread = muCrvHread;
        }
        else if (avent == 0.0 && louCrvHread.IsValid() && lsdRO > 0.0 && lsdCV == 0.0)
        {
            avent = 1.0;
            iPorpCrvHread = louCrvHread;
            scaleFactor = 1 / lsdRO;
            radAirbagEdit.SetValue(sdiIdentifier("Iform"), sdiValue(2));
            radCrvAttr = "fct_IDvvh";
        }
        radAirbagEdit.SetValue(sdiIdentifier("Avent"), sdiValue(sdiDoubleList(1, avent)));
        if (curveHEdit.IsValid())
        {
            radAirbagHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier(radCrvAttr), curveHEdit);
            radAirbagEdit.SetValue(sdiIdentifier("Nvent"), sdiValue(1));
        }
        else if (iPorpCrvHread.IsValid())
        {
            vector< reference_wrapper<double>> curveDetails({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
            p_ConvertUtils.GetCurveDetails(iPorpCrvHread, numPoints, ptsList, curveDetails);

            radAirbagEdit.SetValue(sdiIdentifier("Nvent"), sdiValue(1));
            if (lsdPE != 0.0 || scaleFactor != 1.0)
            {
                p_ConvertUtils.CreateCurve(iPorpCrvHread.GetName(p_radiossModel), numPoints, ptsList, curveHEdit, lsdSFA, lsdSFO * scaleFactor, lsdSFA * lsdOFFA + lsdPE, lsdSFO * lsdOFFO);
                radAirbagHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier(radCrvAttr, 0, 0), curveHEdit);
            }
            else
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier(radCrvAttr, 0, 0), sdiValue(sdiValueEntity(radFunctType, iPorpCrvHread.GetId(p_lsdynaModel))));
        }

        HandleEdit radMatGasHEdit;
        int matGasType;
        if (lsdCV > 0)
        {
            p_radiossModel->CreateEntity(radMatGasHEdit, "/MAT/GAS/CSTA", selSimpleAirbag->GetName());
            matGasType = 4;
        }
        else
        {
            p_radiossModel->CreateEntity(radMatGasHEdit, "/MAT/GAS/MOLE", selSimpleAirbag->GetName());
            matGasType = 2;
        }
        if (radMatGasHEdit.IsValid())
        {
            EntityEdit radMatGasEdit(p_radiossModel, radMatGasHEdit);
            radMatGasEdit.SetValue(sdiIdentifier("MGAS_TYPE"), sdiValue(matGasType));
            if (lsdCV > 0)
            {
                radMatGasEdit.SetValue(sdiIdentifier("Cp"), sdiValue(lsdCP));
                radMatGasEdit.SetValue(sdiIdentifier("Cv"), sdiValue(lsdCV));
            }
            else
            {
                radMatGasEdit.SetValue(sdiIdentifier("Cpa"), sdiValue(lsdaA));
                radMatGasEdit.SetValue(sdiIdentifier("Cpb"), sdiValue(lsdaB));
                radMatGasEdit.SetValue(sdiIdentifier("MW"), sdiValue(lsdMW));
            }
            radAirbagHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("mat_ID"), radMatGasHEdit);
        }

        HandleEdit fctIdEdit;
        HandleEdit radPropInjectHEdit;
        p_radiossModel->CreateEntity(radPropInjectHEdit, "/PROP/INJECT1", selSimpleAirbag->GetName());
        if (radPropInjectHEdit.IsValid())
        {
            EntityEdit radPropInjectEdit(p_radiossModel, radPropInjectHEdit);
            if (radMatGasHEdit.IsValid())
                radPropInjectEdit.SetValue(sdiIdentifier("Mat_ID"), sdiValue(sdiValueEntityList(radMatType, sdiUIntList(1, radMatGasHEdit.GetId(p_radiossModel)))));
            if (lcidId)
                radPropInjectEdit.SetValue(sdiIdentifier("fun_ID_M"), sdiValue(sdiValueEntityList(radFunctType, sdiUIntList(1, lcidId))));
            radPropInjectEdit.SetValue(sdiIdentifier("N_gases"), sdiValue(1));
            radPropInjectEdit.SetValue(sdiIdentifier("Iflow"), sdiValue(1));

            p_ConvertUtils.CreateCurve(selSimpleAirbag->GetName(), 2, { {0, lsdaT, 1, lsdaT} }, fctIdEdit);
            if (fctIdEdit.IsValid())
                radPropInjectHEdit.SetValue(p_radiossModel, sdiIdentifier("fun_ID_T"), sdiValue(sdiValueEntityList(radFunctType, sdiUIntList(1, fctIdEdit.GetId(p_radiossModel)))));

            radAirbagEdit.SetValue(sdiIdentifier("Njet"), sdiValue(1));
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("inject_ID"), sdiValue(sdiValueEntityList(radPropType, sdiUIntList(1, radPropInjectHEdit.GetId(p_radiossModel)))));
        }
        sdiConvert::SDIHandlReadList sourceConVol = { {selSimpleAirbag->GetHandle()} };
        for (HandleEdit hEdit : {radAirbagHEdit, curveHEdit, fctIdEdit, radMatGasHEdit, radPropInjectHEdit})
        {
            if (hEdit.IsValid())
                sdiConvert::Convert::PushToConversionLog(std::make_pair(hEdit, sourceConVol));
        }
    }
}

void sdiD2R::ConvertControlVolume::ConvertAirbagAdiabaticGasModel()
{
    EntityType radSurfType = p_radiossModel->GetEntityType("/SURF");
    SelectionRead selAirbagAdiabaticGasModel(p_lsdynaModel, "*AIRBAG_ADIABATIC_GAS_MODEL");
    while (selAirbagAdiabaticGasModel.Next())
    {
        double lsdGamma = 0.0;
        double lsdPe = 0.0;
        double lsdP0 = 0.0;
        double lsdPsf = 0.0;
        double radPini = 0.0;
        sdiValue tempValue;
        sdiValueEntity surfSet;
        
        int lsdSSTYP;
        selAirbagAdiabaticGasModel->GetValue(sdiIdentifier("SIDTYP"), tempValue);
        tempValue.GetValue(lsdSSTYP);

        HandleEdit radAirbagHEdit;
        p_radiossModel->CreateEntity(radAirbagHEdit, "/MONVOL/GAS", selAirbagAdiabaticGasModel->GetName(), selAirbagAdiabaticGasModel->GetId());
        EntityEdit radAirbagEdit(p_radiossModel, radAirbagHEdit);
        if (radAirbagHEdit.IsValid())
        {
            tempValue = sdiValue(lsdGamma);
            selAirbagAdiabaticGasModel->GetValue(sdiIdentifier("GAMMA"), tempValue);
            tempValue.GetValue(lsdGamma);
            radAirbagEdit.SetValue(sdiIdentifier("Gamma"), sdiValue(lsdGamma));
//
            selAirbagAdiabaticGasModel->GetValue(sdiIdentifier("PE"), tempValue);
            tempValue.GetValue(lsdPe);
            radAirbagEdit.SetValue(sdiIdentifier("Pext"), sdiValue(lsdPe));
//
            selAirbagAdiabaticGasModel->GetValue(sdiIdentifier("P0"), tempValue);
            tempValue.GetValue(lsdP0);
//
            selAirbagAdiabaticGasModel->GetValue(sdiIdentifier("PSF"), tempValue);
            tempValue.GetValue(lsdPsf);
            if(lsdPsf == 0.0) lsdPsf = 1.0;  // default in LS-DYNA
//
            radPini = lsdPsf*lsdP0;
            radAirbagEdit.SetValue(sdiIdentifier("Pini"), sdiValue(radPini));
//
            selAirbagAdiabaticGasModel->GetValue(sdiIdentifier("SID"), tempValue);
            tempValue.GetValue(surfSet);

            if(lsdSSTYP == 0)
            {
                radAirbagEdit.SetValue(sdiIdentifier("surf_IDex"), sdiValue(sdiValueEntity(radSurfType, DynaToRad::GetRadiossSetIdFromLsdSet(surfSet.GetId(), "*SET_SEGMENT"))));
            }
            else
            {
                radAirbagEdit.SetValue(sdiIdentifier("surf_IDex"), sdiValue(sdiValueEntity(radSurfType, DynaToRad::GetRadiossSetIdFromLsdSet(surfSet.GetId(), "*SET_PART"))));
            }
//
            sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagAdiabaticGasModel->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radAirbagHEdit, sourceConVol));
        }
    }
}



void sdiD2R::ConvertControlVolume::p_CreateTHMonVolForDBAbstat()
{
    SelectionRead selDatabaseAbstat(p_lsdynaModel, "*DATABASE_ABSTAT");
    HandleEdit radThMonvHEdit;
    while (selDatabaseAbstat.Next())
    {
        sdiUIntList monVolList;
        SelectionRead selMonvol(p_radiossModel, destCard);
        monVolList.reserve(selMonvol.Count());
        while (selMonvol.Next())
            monVolList.push_back(selMonvol->GetId());

        if (!radThMonvHEdit.IsValid() && monVolList.size() > 0)
        {
            p_radiossModel->CreateEntity(radThMonvHEdit, "/TH/MONV", "TH-MONV");
            EntityEdit radThMonvEdit(p_radiossModel, radThMonvHEdit);

            radThMonvEdit.SetValue(sdiIdentifier("idsmax"), sdiValue((int)monVolList.size()));
            radThMonvEdit.SetValue(sdiIdentifier("Number_Of_Variables"), sdiValue(2));
            radThMonvEdit.SetValue(sdiIdentifier("Obj"), sdiValue(sdiValueEntityList(destEntityType, monVolList)));
            radThMonvEdit.SetValue(sdiIdentifier("var"), sdiValue(sdiStringList({ "DEF", "GAS" })));

            sdiConvert::SDIHandlReadList sourceHandles = { {selDatabaseAbstat->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radThMonvHEdit, sourceHandles));
        }
        break;
    }
}

void sdiD2R::ConvertControlVolume::ConvertAirbagLoadCurve()
{
    SelectionRead selAirbagLoadFunc(p_lsdynaModel, "*AIRBAG_LOAD_CURVE");
    EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");
    EntityType radSurfType = p_radiossModel->GetEntityType("/SURF");

    HandleEdit functEdit;

    while (selAirbagLoadFunc.Next())
    {
        unsigned int lcidId;

        HandleEdit radAirbagHEdit;
        p_radiossModel->CreateEntity(radAirbagHEdit, "/MONVOL/PRES", selAirbagLoadFunc->GetName(), selAirbagLoadFunc->GetId());
        EntityEdit radAirbagEdit(p_radiossModel, radAirbagHEdit);

        sdiValue tempValue;
        sdiValueEntity surfSet;
        selAirbagLoadFunc->GetValue(sdiIdentifier("SID"), tempValue);
        tempValue.GetValue(surfSet);
        
        int lsdSSTYP;
        selAirbagLoadFunc->GetValue(sdiIdentifier("SIDTYP"), tempValue);
        tempValue.GetValue(lsdSSTYP);

        if(lsdSSTYP == 0)
        {
            radAirbagEdit.SetValue(sdiIdentifier("surf_IDex"), sdiValue(sdiValueEntity(radSurfType, DynaToRad::GetRadiossSetIdFromLsdSet(surfSet.GetId(), "*SET_SEGMENT"))));
        }
        else
        {
            radAirbagEdit.SetValue(sdiIdentifier("surf_IDex"), sdiValue(sdiValueEntity(radSurfType, DynaToRad::GetRadiossSetIdFromLsdSet(surfSet.GetId(), "*SET_PART"))));
        }

        double LSD_STIME = 0.0;
        sdiValue tempVal(LSD_STIME);
        selAirbagLoadFunc->GetValue(sdiIdentifier("STIME"), tempVal);
        tempVal.GetValue(LSD_STIME);

        sdiValueEntity lcidEntity;
        selAirbagLoadFunc->GetValue(sdiIdentifier("LCID"), tempValue);
        tempValue.GetValue(lcidEntity);
        lcidId = lcidEntity.GetId();

        radAirbagEdit.SetValue(sdiIdentifier("Ascalet"), sdiValue(0.0));
        radAirbagEdit.SetValue(sdiIdentifier("Fscale"), sdiValue(0.0));
        radAirbagEdit.SetValue(sdiIdentifier("Itypfun"), sdiValue(1));

        HandleRead lcidHandle;
        selAirbagLoadFunc->GetEntityHandle(sdiIdentifier("LCID"), lcidHandle);

        int nPoints = 0;
        tempValue = sdiValue(nPoints);
        lcidHandle.GetValue(p_lsdynaModel, sdiIdentifier("numberofpoints"), tempValue);
        tempValue.GetValue(nPoints);

        //  Curve abscisa offset + add new function point (-1,0) :

        if (nPoints)
        {
            sdiDoubleList crvPoints;
            crvPoints.reserve(2 * nPoints + 2);
            tempValue = sdiValue(crvPoints);
            lcidHandle.GetValue(p_lsdynaModel, sdiIdentifier("points"), tempValue);
            tempValue.GetValue(crvPoints);

            
            for (int i = 0; i < 2 * nPoints; i += 2)
            {
               // abscisa offset
               crvPoints[i] = crvPoints[i] + LSD_STIME;
            }

            // add new function point (-1,0)
            crvPoints.insert(crvPoints.begin(), 0.0);
            crvPoints.insert(crvPoints.begin(), -1.0);
            nPoints = nPoints + 1;

            p_ConvertUtils.CreateCurve(selAirbagLoadFunc->GetName(), (int)nPoints, crvPoints, functEdit);

            if (lcidId)
                radAirbagEdit.SetValue(sdiIdentifier("fct_ID"), sdiValue(sdiValueEntity(radFunctType, functEdit.GetId(p_radiossModel))));
        }


        sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagLoadFunc->GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(radAirbagHEdit, sourceConVol));
        if (functEdit.IsValid())
          sdiConvert::Convert::PushToConversionLog(std::make_pair(functEdit, sourceConVol));
    }

}
void sdiD2R::ConvertControlVolume::ConvertInitialFoamReferenceGeometry()
{
    SelectionRead selInitialFoamRefGeom(p_lsdynaModel, "*INITIAL_FOAM_REFERENCE_GEOMETRY");

    EntityType radPart = p_radiossModel->GetEntityType("/PART");
    EntityType radNode = p_radiossModel->GetEntityType("/NODE");

    if(selInitialFoamRefGeom.Count() == 0) return;

    int numnod = 0;
    int npart = 0;
    int nodeCount = 0;
    int numelsolid = 0;
    int numelshell = 0;
    int numelsh3n = 0;
    int eltype = 0;
    int nbelemtype = 0;
    int partID = 0;
    int PartIdnodeRef;
    int elemId = 0;
    int elemnNbNode = 0;  // element nb of nodes
    sdiString elemType = "\0";
    sdiUIntList listPartNodeRef;


// Node Id Map
    SelectionRead selNodes(p_radiossModel, "/NODE");
    map<int, int> nodeIndexes;
    map<int, int> nodeIds;
    int cpt = 0;
    while (selNodes.Next())
    {
      nodeIndexes.insert(pair<int, int>(selNodes->GetId(),cpt));
      nodeIds.insert(pair<int, int>(cpt,selNodes->GetId()));
      cpt++;
    }
    numnod = selNodes.Count();

// SOLID Id Map
    SelectionRead selSolid(p_radiossModel, "/BRICK");
    map<int, int> solidIndexes;
    map<int, int> solidIds;
    map<int, sdiString> mapSolidElemType;
    cpt = 0;
    while (selSolid.Next())
    {
       solidIndexes.insert(pair<int, int>(selSolid->GetId(),cpt));
       solidIds.insert(pair<int, int>(cpt,selSolid->GetId()));
       mapSolidElemType.insert(pair<int, sdiString>(selSolid->GetId(),"BRICK"));
       cpt++;
    }
    numelsolid = cpt;
// Shell Id Map
    SelectionRead selShell(p_radiossModel, "/SHELL");
    map<int, int> shellIndexes;
    map<int, int> shellIds;
    map<int, sdiString> mapShellElemType;
    cpt = 0;
    while (selShell.Next())
    {
       shellIndexes.insert(pair<int, int>(selShell->GetId(),cpt));
       shellIds.insert(pair<int, int>(cpt,selShell->GetId()));
       mapShellElemType.insert(pair<int, sdiString>(selShell->GetId(),"SHELL"));
       cpt++;
    }
    numelshell = cpt;
// Sh3n Id Map
    SelectionRead selSh3n(p_radiossModel, "/SH3N");
    map<int, int> sh3nIndexes;
    map<int, int> sh3nIds;
    map<int, sdiString> mapSh3nElemType;
    cpt = 0;
    while (selSh3n.Next())
    {
       sh3nIndexes.insert(pair<int, int>(selSh3n->GetId(),cpt));
       sh3nIds.insert(pair<int, int>(cpt,selSh3n->GetId()));
       mapSh3nElemType.insert(pair<int, sdiString>(selSh3n->GetId(),"SH3N"));
       cpt++;
    }
    numelsh3n = cpt;

// Part Id Map
    SelectionRead selParts(p_radiossModel, "/PART");
    map<int, int> partIndexes;
    map<int, int> partIds;
    cpt = 0;
    while (selParts.Next())
    {
      partIndexes.insert(pair<int, int>(selParts->GetId(),cpt));
      partIds.insert(pair<int, int>(cpt,selParts->GetId()));
      cpt++;
    }
    npart = selParts.Count();

// Ndtrrg for each part
    int *partNdtrrg;
    partNdtrrg=(int*) malloc(sizeof(int)*npart);
    for (int i = 0; i < npart; i++)
    {
        partNdtrrg[i] = 0;
    }

    nbelemtype = 3;
    std::vector<int> type = { 1, 2, 3 };
    type[0] = 0;
    type[1] = 0;
    type[2] = 0;
    if(numelsolid > 0) type[0] = 1;
    if(numelshell > 0) type[1] = 2;
    if(numelsh3n > 0)  type[2] = 3;

    //---
    //---  node <-> element inverse connectivity
    //---

    //  solid
    elemType = "/BRICK";
    elemnNbNode = 8;
    int *knod2solid, *nod2solid, *solidNodes;
    knod2solid=(int*) malloc(sizeof(int)*(numnod+1));
    nod2solid=(int*) malloc(sizeof(int)*(elemnNbNode*numelsolid+1));
    solidNodes=(int*) malloc(sizeof(int)*elemnNbNode*numelsolid);
    p_ConvertUtils.InversConnectivityNode2Elem(elemType, elemnNbNode, knod2solid, nod2solid, solidNodes);
    //---

    //  shell
    elemType = "/SHELL";
    elemnNbNode = 4;
    int *knod2shell, *nod2shell, *shellNodes;
    knod2shell=(int*) malloc(sizeof(int)*(numnod+1));
    nod2shell=(int*) malloc(sizeof(int)*(elemnNbNode*numelshell+1));
    shellNodes=(int*) malloc(sizeof(int)*elemnNbNode*numelshell);
    p_ConvertUtils.InversConnectivityNode2Elem(elemType, elemnNbNode, knod2shell, nod2shell, shellNodes);
    //---

    //  sh3n
    elemType = "/SH3N";
    elemnNbNode = 3;
    int *knod2sh3n, *nod2sh3n, *sh3nNodes;
    knod2sh3n=(int*) malloc(sizeof(int)*(numnod+1));
    nod2sh3n=(int*) malloc(sizeof(int)*(elemnNbNode*numelsh3n+1));
    sh3nNodes=(int*) malloc(sizeof(int)*elemnNbNode*numelsh3n);
    p_ConvertUtils.InversConnectivityNode2Elem(elemType, elemnNbNode, knod2sh3n, nod2sh3n, sh3nNodes);
    //---


    // =======================================================
    //  step 1 - tag parts option's nodes
    // =======================================================

    while (selInitialFoamRefGeom.Next())
    {
      sdiValue tempValue;
      int table_count = 0;
      tempValue =sdiValue(table_count);
      selInitialFoamRefGeom->GetValue(sdiIdentifier("table_count"), tempValue);
      tempValue.GetValue(table_count);

      for (int j = 0; j < nbelemtype; ++j) // loop over type of elems (solid + shell + sh3n)
      {
        sdiString elemType = "\0";
        if(type[j] > 0)
        {
          if (type[j] == 1) elemType = "/BRICK";
          if (type[j] == 2) elemType = "/SHELL";
          if (type[j] == 3) elemType = "/SH3N";

// Find for each listed node in option, the belonging part

          for (int i = 0; i < table_count; ++i) // loop over Xref nodes
          {
            // nodeRef --> tag elems --> tag part_ID attached to nodeRef
            sdiValueEntity nodeId;
            tempValue = sdiValue(nodeId);
            selInitialFoamRefGeom->GetValue(sdiIdentifier("nodes_table_node",0,i), tempValue);
            tempValue.GetValue(nodeId);
            unsigned int nodeRef = nodeId.GetId();

            int ndtrrg = 0;
            tempValue =sdiValue(ndtrrg);
            selInitialFoamRefGeom->GetValue(sdiIdentifier("NDTRRG"), tempValue);
            tempValue.GetValue(ndtrrg);

            // element_type < - > node connectivity

            if( elemType == "/BRICK" )
            {
              for(int k=knod2solid[nodeIndexes[nodeRef]]; k<knod2solid[nodeIndexes[nodeRef]+1]; k=k+1)
              {
                elemId = solidIds[nod2solid[k]];

                HandleRead elementHRead;
                p_radiossModel->FindById(p_radiossModel->GetEntityType(elemType), elemId, elementHRead);

                if (elementHRead.IsValid())
                {
                  HandleRead partHRead;
                  elementHRead.GetEntityHandle(p_radiossModel, sdiIdentifier("part_ID"), partHRead);
                  if (partHRead.IsValid())
                  {
                    PartIdnodeRef = partHRead.GetId(p_radiossModel);
                    listPartNodeRef.push_back(PartIdnodeRef);
                    if(ndtrrg > partNdtrrg[partIndexes[PartIdnodeRef]])
                       partNdtrrg[partIndexes[PartIdnodeRef]] = ndtrrg;
                  } // if (partHRead.IsValid())
                } // if (elementHRead.IsValid()
              } // for(int k=knod2solid[nodeIndexes[nodeRef]]
            }
            else if( elemType == "/SHELL" )
            {
              for(int k=knod2shell[nodeIndexes[nodeRef]]; k<knod2shell[nodeIndexes[nodeRef]+1]; k=k+1)
              {
                elemId = shellIds[nod2shell[k]];

                HandleRead elementHRead;
                p_radiossModel->FindById(p_radiossModel->GetEntityType(elemType), elemId, elementHRead);

                if (elementHRead.IsValid())
                {
                  HandleRead partHRead;
                  elementHRead.GetEntityHandle(p_radiossModel, sdiIdentifier("part_ID"), partHRead);
                  if (partHRead.IsValid())
                  {
                    PartIdnodeRef = partHRead.GetId(p_radiossModel);
                    listPartNodeRef.push_back(PartIdnodeRef);
                    // max of "NDTRRG" for each PID
                    if(ndtrrg > partNdtrrg[partIndexes[PartIdnodeRef]])
                       partNdtrrg[partIndexes[PartIdnodeRef]] = ndtrrg;
                  } // if (partHRead.IsValid())
                } // if (elementHRead.IsValid()
              } // for(int k=knod2shell[nodeIndexes[nodeRef]]
            }
            else if( elemType == "/SH3N" )
            {
              for(int k=knod2sh3n[nodeIndexes[nodeRef]]; k<knod2sh3n[nodeIndexes[nodeRef]+1]; k=k+1)
              {
                elemId = sh3nIds[nod2shell[k]];

                HandleRead elementHRead;
                p_radiossModel->FindById(p_radiossModel->GetEntityType(elemType), elemId, elementHRead);

                if (elementHRead.IsValid())
                {
                  HandleRead partHRead;
                  elementHRead.GetEntityHandle(p_radiossModel, sdiIdentifier("part_ID"), partHRead);
                  if (partHRead.IsValid())
                  {
                    PartIdnodeRef = partHRead.GetId(p_radiossModel);
                    listPartNodeRef.push_back(PartIdnodeRef);
                    if(ndtrrg > partNdtrrg[partIndexes[PartIdnodeRef]])
                       partNdtrrg[partIndexes[PartIdnodeRef]] = ndtrrg;
                  } // if (partHRead.IsValid())
                } // if (elementHRead.IsValid()
              } // for(int k=knod2sh3n[nodeIndexes[nodeRef]]
            }
          } // for (int i = 0; i < table_count; ++i) // loop over Xref nodes
          sdiVectorSort(listPartNodeRef);
          sdiVectorUnique(listPartNodeRef);
        } // if(type[j] == 0)
      } // for (int j = 0; j < nbelemtype; ++j) // loop over type of elems (solid + shell)
    } // while (selInitialFoamRefGeom.Next())


    // =======================================================
    //  step 2 - converting *INITIAL_FOAM_REFERENCE_GEOMETRY card
    // =======================================================


    // loop over total parts of Xref nodes to fill par part_ID

    for (int j = 0; j < listPartNodeRef.size(); ++j)
    {
      partID = listPartNodeRef[j];
      int radNitrs = 0;
      // type of elements of partID
      //====================
      size_t nbsh4N = 0;
      size_t nbsh3N = 0;

      EntityType partType = p_radiossModel->GetEntityType("/PART");
      sdiUIntList solidList,sh4Nlist,sh3Nlist;
      HandleRead partHread;
      p_radiossModel->FindById(partType, partID, partHread);
      if(partHread.IsValid())
      {
        EntityRead partEdit(p_radiossModel, partHread);
        SelectionElementRead selPartElem(partEdit);

        while(selPartElem.Next())
        {
          elemId = selPartElem->GetId();
          if (elemId)
          {
            if (mapSolidElemType[elemId] == "BRICK") solidList.push_back(elemId);
            if (mapShellElemType[elemId] == "SHELL") sh4Nlist.push_back(elemId);
            if (mapSh3nElemType[elemId] == "SH3N")   sh3Nlist.push_back(elemId);
          }
        }

        if (!solidList.empty()) elemType = "/BRICK";
        // if (!sh4Nlist.empty())  elemType = "/SHELL";
        // if (!sh3Nlist.empty())  elemType = "/SH3N";
        if (!sh4Nlist.empty() || !sh3Nlist.empty())  elemType = "/SHELL";
        nbsh4N = sh4Nlist.size();
        nbsh3N = sh3Nlist.size();
      } // if(partHread.IsValid())


      //====================

      nodeCount = 0;
      //int *tagRefNod;
      //tagRefNod=(int*) malloc(sizeof(int)*numnod);
      //for (int i = 0; i < numnod; ++i) // loop over Xref nodes
      //{
      //  tagRefNod[i] = 0;
      //}
      SelectionRead selInitialFoamRefGeom(p_lsdynaModel, "*INITIAL_FOAM_REFERENCE_GEOMETRY");

      HandleEdit radInitialFoamHEdit;
      p_radiossModel->CreateEntity(radInitialFoamHEdit, "/XREF");
      if(radInitialFoamHEdit.IsValid())
      {

        EntityEdit radInitialFoamEdit(p_radiossModel, radInitialFoamHEdit);

        while (selInitialFoamRefGeom.Next())
        {
          sdiValue tempValue;
          int table_count = 0;
          tempValue =sdiValue(table_count);
          selInitialFoamRefGeom->GetValue(sdiIdentifier("table_count"), tempValue);
          tempValue.GetValue(table_count);
          /*
          int ndtrrg = 0;
          tempValue =sdiValue(ndtrrg);
          selInitialFoamRefGeom->GetValue(sdiIdentifier("NDTRRG"), tempValue);
          tempValue.GetValue(ndtrrg);
          */
          for (int i = 0; i < table_count; ++i) // loop over Xref nodes
          {
            // nodeRef --> tag elems --> tag part_ID attached to nodeRef

            sdiValueEntity nodeId;
            tempValue = sdiValue(nodeId);
            selInitialFoamRefGeom->GetValue(sdiIdentifier("nodes_table_node",0,i), tempValue);
            tempValue.GetValue(nodeId);
            unsigned int nodeRef = nodeId.GetId();

            double nodes_x = 0.0;
            double nodes_y = 0.0;
            double nodes_z = 0.0;
            int tagNodPartSh4n = 0;

            tempValue = sdiValue(nodes_x);
            selInitialFoamRefGeom->GetValue(sdiIdentifier("nodes_table_x",0,i), tempValue);
            tempValue.GetValue(nodes_x);

            tempValue = sdiValue(nodes_y);
            selInitialFoamRefGeom->GetValue(sdiIdentifier("nodes_table_y",0,i), tempValue);
            tempValue.GetValue(nodes_y);

            tempValue = sdiValue(nodes_z);
            selInitialFoamRefGeom->GetValue(sdiIdentifier("nodes_table_z",0,i), tempValue);
            tempValue.GetValue(nodes_z);

            // ---
            // node connectivity par element type
            // ---

            if( elemType == "/BRICK" )
            {
              for(int k=knod2solid[nodeIndexes[nodeRef]]; k<knod2solid[nodeIndexes[nodeRef]+1]; k=k+1)
              {
                int elemId = solidIds[nod2solid[k]];

                HandleRead elementHRead;
                p_radiossModel->FindById(p_radiossModel->GetEntityType("/BRICK"), elemId, elementHRead);

                if (elementHRead.IsValid())
                {
                  HandleRead partHRead;
                  elementHRead.GetEntityHandle(p_radiossModel, sdiIdentifier("part_ID"), partHRead);
                  if (partHRead.IsValid())
                  {
                    PartIdnodeRef = partHRead.GetId(p_radiossModel);
                    //if (PartIdnodeRef == partID && tagRefNod[nodeIndexes[nodeRef]] == 0) // "nodeRef" is in part, plus remode doubles
                    if (PartIdnodeRef == partID) // "nodeRef" is in part
                    {
                      radInitialFoamEdit.SetValue(sdiIdentifier("node_ID",0,nodeCount),sdiValue(sdiValueEntity(radNode, nodeRef)));
                      radInitialFoamEdit.SetValue(sdiIdentifier("globalx",0,nodeCount), sdiValue(nodes_x));
                      radInitialFoamEdit.SetValue(sdiIdentifier("globaly",0,nodeCount), sdiValue(nodes_y));
                      radInitialFoamEdit.SetValue(sdiIdentifier("globalz",0,nodeCount), sdiValue(nodes_z));
                      nodeCount = nodeCount + 1; // nodeRef found on part partID
                      //tagRefNod[nodeIndexes[nodeRef]] = 1;
                      break;
                    } // if (PartIdnodeRef == partID)
                  } // if (partHRead.IsValid())
                } // if (elementHRead.IsValid()
              } // for(int k=knod2solid[nodeIndexes[nodeRef]]
            }
            else if( elemType == "/SHELL" )
            {
              // -- sh4n --
              if(nbsh4N > 0)
              {
                for(int k=knod2shell[nodeIndexes[nodeRef]]; k<knod2shell[nodeIndexes[nodeRef]+1]; k=k+1)
                {
                  int elemId = shellIds[nod2shell[k]];

                  HandleRead elementHRead;
                  p_radiossModel->FindById(p_radiossModel->GetEntityType("/SHELL"), elemId, elementHRead);

                  if (elementHRead.IsValid())
                  {
                    HandleRead partHRead;
                    elementHRead.GetEntityHandle(p_radiossModel, sdiIdentifier("part_ID"), partHRead);
                    if (partHRead.IsValid())
                    {
                      PartIdnodeRef = partHRead.GetId(p_radiossModel);
                      //if (PartIdnodeRef == partID && tagRefNod[nodeIndexes[nodeRef]] == 0) // "nodeRef" is in part, plus remode doubles
                      if (PartIdnodeRef == partID) // "nodeRef" is in part
                      {
                        radInitialFoamEdit.SetValue(sdiIdentifier("node_ID",0,nodeCount),sdiValue(sdiValueEntity(radNode, nodeRef)));
                        radInitialFoamEdit.SetValue(sdiIdentifier("globalx",0,nodeCount), sdiValue(nodes_x));
                        radInitialFoamEdit.SetValue(sdiIdentifier("globaly",0,nodeCount), sdiValue(nodes_y));
                        radInitialFoamEdit.SetValue(sdiIdentifier("globalz",0,nodeCount), sdiValue(nodes_z));
                        nodeCount = nodeCount + 1; // nodeRef found on part partID
                        tagNodPartSh4n = 1; // nodeRef already tag in sh4n (no need to be tagged in sh3n of same part)
                        //tagRefNod[nodeIndexes[nodeRef]] = 1;
                        break;
                      } // if (PartIdnodeRef == partID)
                    } // if (partHRead.IsValid())
                  } // if (elementHRead.IsValid()
                } // for(int k=knod2shell[nodeIndexes[nodeRef]]
              } // if(nbsh4N > 0)

              // -- sh3n --
              if(nbsh3N > 0)
              {
                for(int k=knod2sh3n[nodeIndexes[nodeRef]]; k<knod2sh3n[nodeIndexes[nodeRef]+1]; k=k+1)
                {
                  int elemId = sh3nIds[nod2sh3n[k]];

                  HandleRead elementHRead;
                  p_radiossModel->FindById(p_radiossModel->GetEntityType("/SH3N"), elemId, elementHRead);

                  if (elementHRead.IsValid())
                  {
                    HandleRead partHRead;
                    elementHRead.GetEntityHandle(p_radiossModel, sdiIdentifier("part_ID"), partHRead);
                    if (partHRead.IsValid())
                    {
                      PartIdnodeRef = partHRead.GetId(p_radiossModel);
                      //if (PartIdnodeRef == partID && tagRefNod[nodeIndexes[nodeRef]] == 0) // "nodeRef" is in part, plus remode doubles
                      if (PartIdnodeRef == partID && tagNodPartSh4n == 0) // "nodeRef" is in part
                      {
                        radInitialFoamEdit.SetValue(sdiIdentifier("node_ID",0,nodeCount),sdiValue(sdiValueEntity(radNode, nodeRef)));
                        radInitialFoamEdit.SetValue(sdiIdentifier("globalx",0,nodeCount), sdiValue(nodes_x));
                        radInitialFoamEdit.SetValue(sdiIdentifier("globaly",0,nodeCount), sdiValue(nodes_y));
                        radInitialFoamEdit.SetValue(sdiIdentifier("globalz",0,nodeCount), sdiValue(nodes_z));
                        nodeCount = nodeCount + 1; // nodeRef found on part partID
                        //tagRefNod[nodeIndexes[nodeRef]] = 1;
                        break;
                      } // if (PartIdnodeRef == partID)
                    } // if (partHRead.IsValid())
                  } // if (elementHRead.IsValid()
                } // for(int k=knod2sh3n[nodeIndexes[nodeRef]]
              } // if(nbsh3N > 0)
            } // if( elemType == "/BRICK" )
          } // for (int i = 0; i < table_count; ++i) // loop over Xref nodes
        } // while (selInitialFoamRefGeom.Next())

        radNitrs = partNdtrrg[partIndexes[PartIdnodeRef]];
        radInitialFoamEdit.SetValue(sdiIdentifier("NITRS"),sdiValue(radNitrs));
        radInitialFoamEdit.SetValue(sdiIdentifier("refnodesmax"), sdiValue(nodeCount));
        radInitialFoamEdit.SetValue(sdiIdentifier("Comp_Id"),sdiValue(sdiValueEntity(radPart, partID)));

        sdiConvert::SDIHandlReadList sourceConVol = { {selInitialFoamRefGeom->GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(radInitialFoamHEdit, sourceConVol));
        //free(tagRefNod);
      } // if(radInitialFoamHEdit.IsValid())
    } // for (int j = 0; j < listPartNodeRef.size(); ++j)
     
    if(partNdtrrg)free(partNdtrrg);
    if(nod2solid)free(nod2solid);
    if(solidNodes)free(solidNodes);
    if(knod2solid)free(knod2solid);
    if(nod2shell)free(nod2shell);
    if(shellNodes)free(shellNodes);
    if(knod2shell)free(knod2shell);
    if(nod2sh3n)free(nod2sh3n);
    if(sh3nNodes)free(sh3nNodes);
    if(knod2sh3n)free(knod2sh3n);
}


void sdiD2R::ConvertControlVolume::ConvertAirbagShellReferenceGeometry()
{
    SelectionRead selAirbagShellRefGeom(p_lsdynaModel, "*AIRBAG_SHELL_REFERENCE_GEOMETRY");

    EntityType radPart = p_radiossModel->GetEntityType("/PART");
    EntityType radNode = p_radiossModel->GetEntityType("/NODE");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    EntityType radShellType = p_radiossModel->GetEntityType("/EREF/SHELL");
    EntityType radSh3nType = p_radiossModel->GetEntityType("/EREF/SH3N");
    EntityType radNbShellType = p_radiossModel->GetEntityType("/SHELL");
    EntityType radNbSh3nType = p_radiossModel->GetEntityType("/SH3N");
    if(selAirbagShellRefGeom.Count() == 0) return;

    // check for initial reference geometry
    SelectionRead selAirbagRefGeom(p_lsdynaModel, "*AIRBAG_REFERENCE_GEOMETRY");

    int offsetNodeId = 0;
    sdiUIntList airbagTransformNodeList;
    sdiValue tempValue;
    bool AirbagRefGeom = false;

    // Map of AIRBAG_REFERENCE_GEOMETRY node_Id + coordinates
    map<int, double> nodeXAirbagRef;
    map<int, double> nodeYAirbagRef;
    map<int, double> nodeZAirbagRef;

    sdiUIntList listPartRef;

    if(selAirbagRefGeom.Count())
    {
      // tag max node_Id in input deck, to offset all "*AIRBAG_REFERENCE_GEOMETRY" nodes:
      offsetNodeId = p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*NODE"));

      AirbagRefGeom = true;

      while (selAirbagRefGeom.Next())
      {
        int tablecount = 0;
        tempValue =sdiValue(tablecount);
        selAirbagRefGeom->GetValue(sdiIdentifier("table_count"), tempValue);
        tempValue.GetValue(tablecount);

        for (int i = 0; i < tablecount; ++i)
        {
          sdiValueEntity nidId;
          tempValue = sdiValue(nidId);
          selAirbagRefGeom->GetValue(sdiIdentifier("nodes_table_node",0,i), tempValue);
          tempValue.GetValue(nidId);
          unsigned int nid = nidId.GetId();

          int nodRefId = nid+offsetNodeId;

          double lsdXcoor = 0.0;
          double lsdYcoor = 0.0;
          double lsdZcoor = 0.0;
          tempValue = sdiValue(lsdXcoor);
          selAirbagRefGeom->GetValue(sdiIdentifier("X",0,i), tempValue);
          tempValue.GetValue(lsdXcoor);

          tempValue = sdiValue(lsdYcoor);
          selAirbagRefGeom->GetValue(sdiIdentifier("Y",0,i), tempValue);
          tempValue.GetValue(lsdYcoor);

          tempValue = sdiValue(lsdZcoor);
          selAirbagRefGeom->GetValue(sdiIdentifier("Z",0,i), tempValue);
          tempValue.GetValue(lsdZcoor);

          nodeXAirbagRef.insert(pair<int, double>(nid,lsdXcoor));
          nodeYAirbagRef.insert(pair<int, double>(nid,lsdYcoor));
          nodeZAirbagRef.insert(pair<int, double>(nid,lsdZcoor));

          sdiTriple refNodeoffCoord(lsdXcoor, lsdYcoor, lsdZcoor) ;
          HandleNodeEdit nodeHEdit;
          p_radiossModel->CreateNode(nodeHEdit, "/NODE", refNodeoffCoord,nodRefId);

        } // for (int i = 0; i < tablecount; ++i)
      } // while (selAirbagRefGeom.Next())
    } // if(selAirbagRefGeom.Count())
//----------------------------
//----------------------------
    while (selAirbagShellRefGeom.Next())
    {
        int table_count = 0;
        tempValue =sdiValue(table_count);
        selAirbagShellRefGeom->GetValue(sdiIdentifier("table_count"), tempValue);
        tempValue.GetValue(table_count);

        for (int i = 0; i < table_count; ++i)
        {
            sdiValueEntity eidId;
            tempValue = sdiValue(eidId);
            selAirbagShellRefGeom->GetValue(sdiIdentifier("elems_table_elem",0,i), tempValue);
            tempValue.GetValue(eidId);
            unsigned int eid = eidId.GetId();


            sdiValueEntity partId;
            tempValue = sdiValue(partId);
            selAirbagShellRefGeom->GetValue(sdiIdentifier("elems_table_component",0,i), tempValue);
            tempValue.GetValue(partId);
            unsigned int pid = partId.GetId();

            listPartRef.push_back(pid);
        }
        sdiVectorSort(listPartRef);
        sdiVectorUnique(listPartRef);
    } // while (selAirbagShellRefGeom.Next())

    int part_ID = 0;
    for (int j = 0; j < listPartRef.size(); ++j)
    {
      part_ID = listPartRef[j];

      SelectionRead selAirbagShellRefGeom(p_lsdynaModel, "*AIRBAG_SHELL_REFERENCE_GEOMETRY");
      while (selAirbagShellRefGeom.Next())
      {
          int flagTransform = 0;
          int nodeNID0 = 0;
          // check for _OPTION = _ID

          sdiString keyWord = selAirbagShellRefGeom->GetKeyword();

          double lsdSX = 0.0;
          double lsdSY = 0.0;
          double lsdSZ = 0.0;
          if (keyWord.find("_ID") != keyWord.npos)
          {
              tempValue = sdiValue(lsdSX);
              selAirbagShellRefGeom->GetValue(sdiIdentifier("SX"), tempValue);
              tempValue.GetValue(lsdSX);

              tempValue = sdiValue(lsdSY);
              selAirbagShellRefGeom->GetValue(sdiIdentifier("SY"), tempValue);
              tempValue.GetValue(lsdSY);

              tempValue = sdiValue(lsdSZ);
              selAirbagShellRefGeom->GetValue(sdiIdentifier("SZ"), tempValue);
              tempValue.GetValue(lsdSZ);

              sdiValueEntity lsdNID0;
              tempValue =sdiValue(lsdNID0);
              selAirbagShellRefGeom->GetValue(sdiIdentifier("NID0"), tempValue);
              tempValue.GetValue(lsdNID0);
              nodeNID0 = lsdNID0.GetId();

              if(lsdSX > 0.0 || lsdSY > 0.0 || lsdSZ > 0.0) flagTransform = 1; 
              // default scale values
              if(lsdSX == 0.0) lsdSX = 1.0;
              if(lsdSY == 0.0) lsdSY = 1.0;
              if(lsdSZ == 0.0) lsdSZ = 1.0;
          }

          int nbSh3n =0;
          int nbShell =0;

          int table_count = 0;
          tempValue =sdiValue(table_count);
          selAirbagShellRefGeom->GetValue(sdiIdentifier("table_count"), tempValue);
          tempValue.GetValue(table_count);
          for (int i = 0; i < table_count; ++i)
          {
            sdiValueEntity eidId;
            tempValue = sdiValue(eidId);
            selAirbagShellRefGeom->GetValue(sdiIdentifier("elems_table_elem",0,i), tempValue);
            tempValue.GetValue(eidId);
            unsigned int eid = eidId.GetId();

            sdiValueEntity partId;
            tempValue = sdiValue(partId);
            selAirbagShellRefGeom->GetValue(sdiIdentifier("elems_table_component",0,i), tempValue);
            tempValue.GetValue(partId);
            unsigned int pid = partId.GetId();

            HandleRead elementHRead;
            p_radiossModel->FindById(p_radiossModel->GetEntityType("/SHELL"), eid, elementHRead);
            if ( elementHRead.IsValid() && part_ID == pid) nbShell++;
            p_radiossModel->FindById(p_radiossModel->GetEntityType("/SH3N"), eid, elementHRead);
            if ( elementHRead.IsValid() && part_ID == pid) nbSh3n++;
          }
          HandleEdit radAirbagshellHEdit;
          HandleEdit radAirbagsh3nHEdit;
          if (nbShell > 0) p_radiossModel->CreateEntity(radAirbagshellHEdit, "/EREF/SHELL");
          if (nbSh3n > 0)  p_radiossModel->CreateEntity(radAirbagsh3nHEdit, "/EREF/SH3N");


          // ---------------------------------------------
          //  with / without "*AIRBAG_REFERENCE_GEOMETRY"
          // ---------------------------------------------

          //----------------------------------------------------
          // *********** /EREF/SHELL ( SHELL 4N ) **************
          //----------------------------------------------------

          if(radAirbagshellHEdit.IsValid())
          {

            EntityEdit radAirbagshellEdit(p_radiossModel, radAirbagshellHEdit);
            //
            int cnt_elem = 0;
            int cnt_node = 0;
            for (int i = 0; i < table_count; ++i)
            {
              sdiValueEntity eidId;
              tempValue = sdiValue(eidId);
              selAirbagShellRefGeom->GetValue(sdiIdentifier("elems_table_elem",0,i), tempValue);
              tempValue.GetValue(eidId);
              unsigned int eid = eidId.GetId();

              sdiValueEntity partId;
              tempValue = sdiValue(partId);
              selAirbagShellRefGeom->GetValue(sdiIdentifier("elems_table_component",0,i), tempValue);
              tempValue.GetValue(partId);
              unsigned int pid = partId.GetId();

              int node1=0;
              int node2=0;
              int node3=0;
              int node4=0;

              sdiValueEntity nodeId1;
              sdiValueEntity nodeId2;
              sdiValueEntity nodeId3;
              sdiValueEntity nodeId4;
              tempValue = sdiValue(nodeId1);
              selAirbagShellRefGeom->GetValue(sdiIdentifier("elems_table_n1",0,i), tempValue);
              tempValue.GetValue(nodeId1);
              node1 = nodeId1.GetId() + offsetNodeId;

              tempValue = sdiValue(nodeId2);
              selAirbagShellRefGeom->GetValue(sdiIdentifier("elems_table_n2",0,i), tempValue);
              tempValue.GetValue(nodeId2);
              node2 = nodeId2.GetId() + offsetNodeId;

              tempValue = sdiValue(nodeId3);
              selAirbagShellRefGeom->GetValue(sdiIdentifier("elems_table_n3",0,i), tempValue);
              tempValue.GetValue(nodeId3);
              node3 = nodeId3.GetId() + offsetNodeId;

              tempValue = sdiValue(nodeId4);
              selAirbagShellRefGeom->GetValue(sdiIdentifier("elems_table_n4",0,i), tempValue);
              tempValue.GetValue(nodeId4);
              node4 = nodeId4.GetId() + offsetNodeId;

              HandleRead sh4nHRead;
              p_radiossModel->FindById(p_radiossModel->GetEntityType("/SHELL"), eid, sh4nHRead);

              if(sh4nHRead.IsValid() && part_ID == pid)
              {
                radAirbagshellEdit.SetValue(sdiIdentifier("shell_ID",0,cnt_elem),sdiValue(sdiValueEntity(radShellType, eid)));
                radAirbagshellEdit.SetValue(sdiIdentifier("part_ID"),sdiValue(sdiValueEntity(radPart, pid)));
                radAirbagshellEdit.SetValue(sdiIdentifier("elems_table_n1",0,cnt_elem,cnt_node)  ,sdiValue(sdiValueEntity(radNode, node1)));
                radAirbagshellEdit.SetValue(sdiIdentifier("node_ID2",0,cnt_elem,cnt_node+1),sdiValue(sdiValueEntity(radNode, node2)));
                radAirbagshellEdit.SetValue(sdiIdentifier("node_ID3",0,cnt_elem,cnt_node+2),sdiValue(sdiValueEntity(radNode, node3)));
                radAirbagshellEdit.SetValue(sdiIdentifier("node_ID4",0,cnt_elem,cnt_node+3),sdiValue(sdiValueEntity(radNode, node4)));
                cnt_node = cnt_node + 4;
                //--------------------
                sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagShellRefGeom->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(radAirbagshellHEdit, sourceConVol));
                //--------------------
                cnt_elem = cnt_elem + 1;
                if(flagTransform > 0) airbagTransformNodeList.push_back(node1);
                if(flagTransform > 0) airbagTransformNodeList.push_back(node2);
                if(flagTransform > 0) airbagTransformNodeList.push_back(node3);
                if(flagTransform > 0) airbagTransformNodeList.push_back(node4);
              } // if ( sh4nHRead.IsValid())
            } // for (int i = 0; i < table_count; ++i)
            if(cnt_elem > 0) radAirbagshellEdit.SetValue(sdiIdentifier("table_count"), sdiValue(cnt_elem));
          } // if(radAirbagshellHEdit.IsValid())

          //----------------------------------------------------
          // *********** /EREF/SH3N ( SHELL 3N ) **************
          //----------------------------------------------------

          if(radAirbagsh3nHEdit.IsValid())
          {
            EntityEdit radAirbagsh3nEdit(p_radiossModel, radAirbagsh3nHEdit);
            //
            int cnt_elem = 0;
            int cnt_node = 0;
            for (int i = 0; i < table_count; ++i)
            {
              sdiValueEntity eidId;
              tempValue = sdiValue(eidId);
              selAirbagShellRefGeom->GetValue(sdiIdentifier("elems_table_elem",0,i), tempValue);
              tempValue.GetValue(eidId);
              unsigned int eid = eidId.GetId();

              sdiValueEntity partId;
              tempValue = sdiValue(partId);
              selAirbagShellRefGeom->GetValue(sdiIdentifier("elems_table_component",0,i), tempValue);
              tempValue.GetValue(partId);
              unsigned int pid = partId.GetId();

              int node1=0;
              int node2=0;
              int node3=0;
              int node4=0;

              sdiValueEntity nodeId1;
              sdiValueEntity nodeId2;
              sdiValueEntity nodeId3;
              sdiValueEntity nodeId4;
              tempValue = sdiValue(nodeId1);
              selAirbagShellRefGeom->GetValue(sdiIdentifier("elems_table_n1",0,i), tempValue);
              tempValue.GetValue(nodeId1);
              node1 = nodeId1.GetId() + offsetNodeId;

              tempValue = sdiValue(nodeId2);
              selAirbagShellRefGeom->GetValue(sdiIdentifier("elems_table_n2",0,i), tempValue);
              tempValue.GetValue(nodeId2);
              node2 = nodeId2.GetId() + offsetNodeId;

              tempValue = sdiValue(nodeId3);
              selAirbagShellRefGeom->GetValue(sdiIdentifier("elems_table_n3",0,i), tempValue);
              tempValue.GetValue(nodeId3);
              node3 = nodeId3.GetId() + offsetNodeId;

              HandleRead sh3nHRead;
              p_radiossModel->FindById(p_radiossModel->GetEntityType("/SH3N"), eid, sh3nHRead);

              if(sh3nHRead.IsValid() && part_ID == pid)
              {
                radAirbagsh3nEdit.SetValue(sdiIdentifier("shell_ID",0,cnt_elem),sdiValue(sdiValueEntity(radSh3nType, eid)));
                radAirbagsh3nEdit.SetValue(sdiIdentifier("part_ID"),sdiValue(sdiValueEntity(radPart, pid)));
                radAirbagsh3nEdit.SetValue(sdiIdentifier("elems_table_n1",0,cnt_elem,cnt_node)  ,sdiValue(sdiValueEntity(radNode, node1)));
                radAirbagsh3nEdit.SetValue(sdiIdentifier("node_ID2",0,cnt_elem,cnt_node+1),sdiValue(sdiValueEntity(radNode, node2)));
                radAirbagsh3nEdit.SetValue(sdiIdentifier("node_ID3",0,cnt_elem,cnt_node+2),sdiValue(sdiValueEntity(radNode, node3)));
                cnt_node = cnt_node + 3;
                //--------------------
                sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagShellRefGeom->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(radAirbagsh3nHEdit, sourceConVol));
                //--------------------
                cnt_elem = cnt_elem + 1;
                if(flagTransform > 0) airbagTransformNodeList.push_back(node1);
                if(flagTransform > 0) airbagTransformNodeList.push_back(node2);
                if(flagTransform > 0) airbagTransformNodeList.push_back(node3);
              } // if ( sh3nHRead.IsValid())
            } // for (int i = 0; i < table_count; ++i)
            if(cnt_elem > 0) radAirbagsh3nEdit.SetValue(sdiIdentifier("table_count"), sdiValue(cnt_elem));
          } // if(radAirbagsh3nHEdit.IsValid())


          //-------------------
          //   +  /TRANSFORM/SCA
          //-------------------
          if(flagTransform > 0)
          {
            sdiVectorSort(airbagTransformNodeList);
            sdiVectorUnique(airbagTransformNodeList);
            // /TRANSFORM/SCA card
            HandleEdit transformHEdit;
            p_radiossModel->CreateEntity(transformHEdit, "/TRANSFORM/SCA", "/TRANSFORM/SCA_AIRBAG_SHELL_REFERENCE_GEOMETRY_" + to_string(selAirbagShellRefGeom->GetId()));
            EntityEdit transformEdit(p_radiossModel, transformHEdit);

            transformEdit.SetValue(sdiIdentifier("scalefactor_x"), sdiValue(lsdSX));
            transformEdit.SetValue(sdiIdentifier("scalefactor_y"), sdiValue(lsdSY));
            transformEdit.SetValue(sdiIdentifier("scalefactor_z"), sdiValue(lsdSZ));

            if(nodeNID0 > 0)
            {
               transformEdit.SetValue(sdiIdentifier("node1"), sdiValue(sdiValueEntity(1, nodeNID0)));
            }
            else // create node
            {
              HandleNodeEdit nodeHEdit;
              sdiTriple centroid(0.0, 0.0, 0.0);
              p_radiossModel->CreateNode(nodeHEdit, "/NODE", centroid);
              if (nodeHEdit.IsValid())
              {
                transformEdit.SetValue(sdiIdentifier("node1"), sdiValue(sdiValueEntity(sdiValueEntityType(radNode), nodeHEdit.GetId(p_radiossModel))));
              }
            }
            sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagShellRefGeom->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(transformHEdit, sourceConVol));

            // node SET for /TRANSFORM/SCA node group
            HandleEdit setHEdit;
            p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", "SET_GENERAL_AIRBAG_SHELL_REFERENCE_GEOMETRY_" + to_string(selAirbagShellRefGeom->GetId()));
            EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

            SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue((int)airbagTransformNodeList.size())); 
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(1, airbagTransformNodeList))); 
            transformEdit.SetEntityHandle(sdiIdentifier("grnod_ID"), setHEdit);

            if (setHEdit.IsValid())
                sdiConvert::Convert::PushToConversionLog(std::make_pair(setHEdit, sourceConVol));
          } //if(flagTransform == 0)

      } // while (selAirbagShellRefGeom.Next())

    } // for (int j = 0; j < listPartRef.size(); ++j)
}
void sdiD2R::ConvertControlVolume::ConvertAirbagParticle()
{
    SelectionRead selAirbagParticle(p_lsdynaModel, "*AIRBAG_PARTICLE");

    while (selAirbagParticle.Next())
    {
        unsigned int airbagId = selAirbagParticle->GetId();
        HandleEdit radAirbagHEdit;

        p_radiossModel->CreateEntity(radAirbagHEdit, "/MONVOL/FVMBAG2/", selAirbagParticle->GetName(), airbagId);
        EntityEdit radAirbagEdit(p_radiossModel, radAirbagHEdit);

        sdiValueEntity surfSet;
        EntityType radMatType = p_radiossModel->GetEntityType("/MAT");
        EntityType radPropType = p_radiossModel->GetEntityType("/PROP");
        EntityType radSensorType = p_radiossModel->GetEntityType("/SENSOR");
        EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");
        EntityType radPartType = p_radiossModel->GetEntityType("/PART");

        sdiValue tempValue;

        double lsdXMAIR;
        double lsdAAIR;
        double lsdBAIR;
        double lsdCAIR;
        double lsdPAIR;
        double lsdTAIR;
        int lsd_IAIR,lsd_NGAS;
        vector<reference_wrapper<double>> attribVals({ lsdXMAIR, lsdAAIR, lsdBAIR, lsdCAIR, lsdPAIR, lsdTAIR });
        vector<sdiString> attribNames({ "XMAIR", "AAIR", "BAIR", "CAIR", "PAIR", "TAIR" });
        p_ConvertUtils.GetAttribValues(*selAirbagParticle, attribNames, attribVals);

        vector<reference_wrapper<int>> attribIntVals({ lsd_IAIR, lsd_NGAS });
        vector<sdiString> attribIntNames({ "IAIR", "NGAS" });
        p_ConvertUtils.GetAttribValues(*selAirbagParticle, attribIntNames, attribIntVals);

        HandleEdit radMatGasHEdit;

        int matGasType,lsdUNIT;
        double lsdPEXT,lsdPATM;

        tempValue =sdiValue(lsdPEXT);
        selAirbagParticle->GetValue(sdiIdentifier("PAIR"), tempValue);
        tempValue.GetValue(lsdPEXT);

        tempValue =sdiValue(lsdUNIT);
        selAirbagParticle->GetValue(sdiIdentifier("UNIT"), tempValue);
        tempValue.GetValue(lsdUNIT);

        if(lsd_IAIR == 0)
        {
        // create a predefined air gas material
            p_radiossModel->CreateEntity(radMatGasHEdit, "/MAT/GAS/PREDEF", selAirbagParticle->GetName()+ "_AIR");
            matGasType = 3;
            if (radMatGasHEdit.IsValid())
            {
                EntityEdit radMatGasEdit(p_radiossModel, radMatGasHEdit);
                radMatGasEdit.SetValue(sdiIdentifier("MGAS_TYPE"), sdiValue(matGasType));
                radMatGasEdit.SetValue(sdiIdentifier("GAS"), sdiValue(sdiString({ "AIR" })));
                radAirbagHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("mat_ID"), radMatGasHEdit);
            }
            // Create initial air preassure/temperature input
            p_ConvertUtils.CopyValue(*selAirbagParticle, radAirbagEdit, "PATM", "Pext");
            p_ConvertUtils.CopyValue(*selAirbagParticle, radAirbagEdit, "TATM", "T0");

            tempValue =sdiValue(lsdPATM);
            selAirbagParticle->GetValue(sdiIdentifier("PATM"), tempValue);
            tempValue.GetValue(lsdPATM);
            lsdPEXT = lsdPATM;

        }
        else
        {
        // create an air gas material
            p_radiossModel->CreateEntity(radMatGasHEdit, "/MAT/GAS/MOLE", selAirbagParticle->GetName()+ "_AIR");
            matGasType = 2;
            if (radMatGasHEdit.IsValid())
            {
                EntityEdit radMatGasEdit(p_radiossModel, radMatGasHEdit);
                radMatGasEdit.SetValue(sdiIdentifier("MGAS_TYPE"), sdiValue(matGasType));
                p_ConvertUtils.CopyValue(*selAirbagParticle, radMatGasEdit, "XMAIR", "MW");
                p_ConvertUtils.CopyValue(*selAirbagParticle, radMatGasEdit, "AAIR", "Cpa");
                p_ConvertUtils.CopyValue(*selAirbagParticle, radMatGasEdit, "BAIR", "Cpb");
                p_ConvertUtils.CopyValue(*selAirbagParticle, radMatGasEdit, "CAIR", "Cpc");
                radMatGasEdit.SetValue(sdiIdentifier("Cpd"), sdiValue(0));
                radMatGasEdit.SetValue(sdiIdentifier("Cpe"), sdiValue(0));
                radAirbagHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("mat_ID"), radMatGasHEdit);

                sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(radMatGasHEdit, sourceConVol));
            }

            // Create initial air preassure/temperature input
            p_ConvertUtils.CopyValue(*selAirbagParticle, radAirbagEdit, "PAIR", "Pext");
            p_ConvertUtils.CopyValue(*selAirbagParticle, radAirbagEdit, "TAIR", "T0");
        }

        HandleEdit radMatGasiHEdit;
        sdiUIntList lsdmatIDList;
        sdiUIntList lsdLCMiIDList;
        sdiUIntList lsdLCTiIDList;
        sdiDoubleList TTFList;

        if(lsd_NGAS > 0)
        {
        // Create NGAS gas materials for injection

            for (int i = 0; i < lsd_NGAS; ++i)
            {
                p_radiossModel->CreateEntity(radMatGasiHEdit, "/MAT/GAS/MOLE", selAirbagParticle->GetName() + "_GAS_" + to_string(i+1));
                matGasType = 2;

                if (radMatGasiHEdit.IsValid())
                {
                    EntityEdit radMatGasiEdit(p_radiossModel, radMatGasiHEdit);
                    radMatGasiEdit.SetValue(sdiIdentifier("MGAS_TYPE"), sdiValue(matGasType));

                    int mat_ID = radMatGasiHEdit.GetId(p_radiossModel);
                    lsdmatIDList.push_back(mat_ID);

                    double lsdXMi;
                    double lsdAi;
                    double lsdBi;
                    double lsdCi;
                    tempValue =sdiValue(lsdXMi);
                    selAirbagParticle->GetValue(sdiIdentifier("XMi",0,i), tempValue);
                    tempValue.GetValue(lsdXMi);
                    radMatGasiEdit.SetValue(sdiIdentifier("MW"), sdiValue(lsdXMi));
                    //p_ConvertUtils.CopyValue(*selAirbagParticle, radMatGasiEdit, "XMi", "MW");
                    tempValue =sdiValue(lsdAi);
                    selAirbagParticle->GetValue(sdiIdentifier("Ai",0,i), tempValue);
                    tempValue.GetValue(lsdAi);
                    radMatGasiEdit.SetValue(sdiIdentifier("Cpa"), sdiValue(lsdAi));
                    tempValue =sdiValue(lsdBi);
                    selAirbagParticle->GetValue(sdiIdentifier("Bi",0,i), tempValue);
                    tempValue.GetValue(lsdBi);
                    radMatGasiEdit.SetValue(sdiIdentifier("Cpb"), sdiValue(lsdBi));
                    tempValue =sdiValue(lsdCi);
                    selAirbagParticle->GetValue(sdiIdentifier("Ci",0,i), tempValue);
                    tempValue.GetValue(lsdCi);
                    radMatGasiEdit.SetValue(sdiIdentifier("Cpc"), sdiValue(lsdCi));

                    sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(radMatGasiHEdit, sourceConVol));
                }
            }
        }
        //---
        // Conversion of injector property
        //---
        double TTF = 0.0, OFFA_TTF = 0.0;
        int airbag_sensorID = 0;
        HandleEdit radPropInjectHEdit;
        p_radiossModel->CreateEntity(radPropInjectHEdit, "/PROP/INJECT1", selAirbagParticle->GetName());
        if (radPropInjectHEdit.IsValid())
        {
            EntityEdit radPropInjectEdit(p_radiossModel, radPropInjectHEdit);
            if(lsd_NGAS > 0)
            {
                radPropInjectEdit.SetValue(sdiIdentifier("N_gases"), sdiValue(lsd_NGAS));
                radPropInjectEdit.SetValue(sdiIdentifier("Iflow"), sdiValue(1));

                if(lsdmatIDList.size())
                   radPropInjectEdit.SetValue(sdiIdentifier("Mat_ID"), sdiValue(sdiValueEntityList(radMatType, lsdmatIDList)));

                for (int i = 0; i < lsd_NGAS; ++i)
                {
                    int lsdLCMiId,lsdLCTiId;
                    sdiValueEntity lsdLCMiEntity;
                    selAirbagParticle->GetValue(sdiIdentifier("LCMi",0,i), tempValue);
                    tempValue.GetValue(lsdLCMiEntity);
                    lsdLCMiId = lsdLCMiEntity.GetId();

                    HandleRead lsdLCMiHandle;
                    selAirbagParticle->GetEntityHandle(sdiIdentifier("LCMi",0,i), lsdLCMiHandle);

                    int nPointsLCMi = 0;
                    tempValue = sdiValue(nPointsLCMi);
                    lsdLCMiHandle.GetValue(p_lsdynaModel, sdiIdentifier("numberofpoints"), tempValue);
                    tempValue.GetValue(nPointsLCMi);

                    sdiValueEntity lsdLCTiEntity;
                    selAirbagParticle->GetValue(sdiIdentifier("LCTi",0,i), tempValue);
                    tempValue.GetValue(lsdLCTiEntity);
                    lsdLCTiId = lsdLCTiEntity.GetId();

                    HandleRead lsdLCTiHandle;
                    selAirbagParticle->GetEntityHandle(sdiIdentifier("LCTi",0,i), lsdLCTiHandle);

                    int nPointsLCTi = 0;
                    tempValue = sdiValue(nPointsLCTi);
                    lsdLCTiHandle.GetValue(p_lsdynaModel, sdiIdentifier("numberofpoints"), tempValue);
                    tempValue.GetValue(nPointsLCTi);

                    double lsdSFA, lsdSFO, lsdOFFA, lsdOFFO;
                    double minval=0.0 , maxval=0.0;
                    double ordonate;
                    double min_abscisa=0.0, max_abscisa=0.0;
                    //------------------
                    // functions: "LCMi"
                    //------------------
                    if (nPointsLCMi > 0)
                    {
                        sdiDoubleList crvPointsLCMi;
                        tempValue = sdiValue(crvPointsLCMi);
                        lsdLCMiHandle.GetValue(p_lsdynaModel, sdiIdentifier("points"), tempValue);
                        tempValue.GetValue(crvPointsLCMi);

                        //-------------------------------------------
                        // find max_abscisa for which ordonate = 0.0

                        tempValue = sdiValue(lsdSFA);
                        lsdLCMiHandle.GetValue(p_lsdynaModel, sdiIdentifier("SFA"), tempValue);
                        tempValue.GetValue(lsdSFA);
                        lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;

                        tempValue = sdiValue(lsdSFO);
                        lsdLCMiHandle.GetValue(p_lsdynaModel, sdiIdentifier("SFO"), tempValue);
                        tempValue.GetValue(lsdSFO);
                        lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

                        tempValue = sdiValue(lsdOFFA);
                        lsdLCMiHandle.GetValue(p_lsdynaModel, sdiIdentifier("OFFA"), tempValue);
                        tempValue.GetValue(lsdOFFA);

                        tempValue = sdiValue(lsdOFFO);
                        lsdLCMiHandle.GetValue(p_lsdynaModel, sdiIdentifier("OFFO"), tempValue);
                        tempValue.GetValue(lsdOFFO);

                        max_abscisa = crvPointsLCMi[0];
                        //min_abscisa = (crvPointsLCMi[2*nPointsLCMi-2] + lsdOFFO)*lsdSFO;

                        if (i == 0) OFFA_TTF = lsdOFFA;
                        for (int j = 2; j < 2 * nPointsLCMi; j += 2)
                        {
                            //abscisa = crvPointsLCMi[j];
                            ordonate = crvPointsLCMi[j+1];
                            // get min abscisa for ordonate= 0
                            if (crvPointsLCMi[j] > max_abscisa && ordonate == 0.0)
                            {
                                max_abscisa = crvPointsLCMi[j];
                            }
                            else
                            {
                                break;
                            }
                        }

                        TTFList.push_back(max_abscisa);

                        // Assume first element as maximum and minimum
                        double maxval = TTFList[0];
                        double minval = TTFList[0];
                        // Find maximum and minimum in all list elements.
                        for (int k = 0; k < TTFList.size(); k++)
                        {
                            // If current element is greater than max
                            if (TTFList[k] > maxval)
                                maxval = TTFList[k];
                            // If current element is smaller than min
                            if (TTFList[k] < minval)
                                minval = TTFList[k];
                        }

                        min_abscisa = minval;
                        TTF = min_abscisa;
                        lsdOFFA = -TTF;
                        //-------------------------------------------
                        HandleEdit fctHEdit;
                        p_ConvertUtils.CreateCurve(selAirbagParticle->GetName()+ "_curve_LCMi_" + to_string(lsdLCMiId), 
                                   (int)nPointsLCMi, crvPointsLCMi, fctHEdit, lsdSFA, lsdSFO, lsdOFFA ,lsdOFFO);
                        if (lsdLCMiId > 0) lsdLCMiIDList.push_back(fctHEdit.GetId(p_radiossModel));

                        sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                        sdiConvert::Convert::PushToConversionLog(std::make_pair(fctHEdit, sourceConVol));
                    }

                    //------------------
                    // fucntions: "LCTi"
                    //------------------
                    if (nPointsLCTi > 0)
                    {
                        sdiDoubleList crvPointsLCTi;
                        tempValue = sdiValue(crvPointsLCTi);
                        lsdLCTiHandle.GetValue(p_lsdynaModel, sdiIdentifier("points"), tempValue);
                        tempValue.GetValue(crvPointsLCTi);

                        tempValue = sdiValue(lsdSFA);
                        lsdLCMiHandle.GetValue(p_lsdynaModel, sdiIdentifier("SFA"), tempValue);
                        tempValue.GetValue(lsdSFA);
                        lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;

                        tempValue = sdiValue(lsdSFO);
                        lsdLCMiHandle.GetValue(p_lsdynaModel, sdiIdentifier("SFO"), tempValue);
                        tempValue.GetValue(lsdSFO);
                        lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

                        tempValue = sdiValue(lsdOFFA);
                        lsdLCMiHandle.GetValue(p_lsdynaModel, sdiIdentifier("OFFA"), tempValue);
                        tempValue.GetValue(lsdOFFA);

                        tempValue = sdiValue(lsdOFFO);
                        lsdLCMiHandle.GetValue(p_lsdynaModel, sdiIdentifier("OFFO"), tempValue);
                        tempValue.GetValue(lsdOFFO);

                        lsdOFFA = -TTF;

                        HandleEdit fctHEdit;
                        p_ConvertUtils.CreateCurve(selAirbagParticle->GetName()+ "_curve_LCTi_" + to_string(lsdLCTiId), 
                                   (int)nPointsLCTi, crvPointsLCTi, fctHEdit, lsdSFA, lsdSFO, lsdOFFA ,lsdOFFO);
                        if (lsdLCTiId > 0) lsdLCTiIDList.push_back(fctHEdit.GetId(p_radiossModel));

                        sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                        sdiConvert::Convert::PushToConversionLog(std::make_pair(fctHEdit, sourceConVol));
                    }

                    radAirbagEdit.SetValue(sdiIdentifier("Njet"), sdiValue(1));
                    radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("inject_ID"), sdiValue(sdiValueEntityList(radPropType, sdiUIntList(1, radPropInjectHEdit.GetId(p_radiossModel)))));

                } // for (int i = 0; i < lsd_NGAS; ++i)

                if (lsdLCMiIDList.size() > 0)
                     radPropInjectEdit.SetValue(sdiIdentifier("fun_ID_M"), sdiValue(sdiValueEntityList(radMatType, lsdLCMiIDList)));
                if (lsdLCTiIDList.size() > 0)
                     radPropInjectEdit.SetValue(sdiIdentifier("fun_ID_T"), sdiValue(sdiValueEntityList(radMatType, lsdLCTiIDList)));
            } // if(lsd_NGAS > 0)

            //---
            // Conversion of sensor corresponding to the injector property
            //---
            if (TTF > 0)
            {
                HandleEdit sensorTimeHedit;
                p_radiossModel->CreateEntity(sensorTimeHedit, "/SENSOR/TIME", selAirbagParticle->GetName());
                EntityEdit sensorTimeEntityEdit(p_radiossModel, sensorTimeHedit);
                sensorTimeEntityEdit.SetValue(sdiIdentifier("Tdelay"), sdiValue(TTF+OFFA_TTF));
                //radAirbagHEdit.SetEntityHandle(p_radiossModel, Identifier("sens_ID"), sensorTimeHedit);
                airbag_sensorID = sensorTimeHedit.GetId(p_radiossModel);
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("sens_ID"), sdiValue(sdiValueEntityList(radSensorType, sdiUIntList(1, sensorTimeHedit.GetId(p_radiossModel)))));

                sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(sensorTimeHedit, sourceConVol));
            }

            sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radPropInjectHEdit, sourceConVol));
        } // if (radPropInjectHEdit.IsValid())


        //  Create airbag surfaces: external/internal
//---------------------------------------------
        sdiUIntList PartSetList, PartSetListD;
        sdiString setType = "*SET_PART_LIST_TITLE";

        selAirbagParticle->GetValue(sdiIdentifier("SD1"), tempValue);
        tempValue.GetValue(surfSet);

        p_ConvertUtils.GetPartIdsFromPartSet(setType, surfSet.GetId(), PartSetList);
        sdiVectorSort(PartSetList);
        sdiVectorUnique(PartSetList);

        selAirbagParticle->GetValue(sdiIdentifier("SD2"), tempValue);
        tempValue.GetValue(surfSet);

        radAirbagEdit.SetValue(sdiIdentifier("surf_IDin"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"), surfSet.GetId())));

        p_ConvertUtils.GetPartIdsFromPartSet(setType, surfSet.GetId(), PartSetListD);
        sdiVectorSort(PartSetListD);
        sdiVectorUnique(PartSetListD);

        //---
        // extract parts of SID2 from SID1 and create a new SID1 = SID1-SID2, and insert into airbag SD1
        //---
        p_ConvertUtils.IdListRemove(PartSetList, PartSetListD);

        if(PartSetList.size() > 0)
        {
            HandleEdit partSetHedit;
            p_radiossModel->CreateEntity(partSetHedit, "/SET/GENERAL", "PART_SET_GENERAL_AIRBAG_PARTICLE_" + to_string(selAirbagParticle->GetId()));
            EntityEdit SetEntityEdit(p_radiossModel, partSetHedit);

            SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue((int) PartSetList.size()));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radPartType, PartSetList)));
            //radAirbagEdit.SetEntityHandle(sdiIdentifier("surf_IDex"), partSetHedit);

            if (SetEntityEdit.GetId())
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDex"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"), SetEntityEdit.GetId())));
            sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(partSetHedit, sourceConVol));
        }

        // Create injector input, Njet=1
        int lsdNorif = 0;
        tempValue = sdiValue(lsdNorif);
        selAirbagParticle->GetValue(sdiIdentifier("NORIF"), tempValue);
        tempValue.GetValue(lsdNorif);

        if(lsdNorif > 0)
        {
            sdiUIntList Sh4nPartSet,Sh3nPartSet;

            // check inflator input shell set list
            int lsdnegVDI = 0;
            tempValue = sdiValue(lsdnegVDI);
            selAirbagParticle->GetValue(sdiIdentifier("negVDIFlag"), tempValue);
            tempValue.GetValue(lsdnegVDI);

            if(lsdnegVDI == 0)
            {
              // error message "Inflator nozzles can be defined only by shells VID=-1 or VID=-2"
              DynaToRad::ShowMessage(sdiMessageHandler::Level::Error, 35,
                  selAirbagParticle->GetKeyword().c_str(), selAirbagParticle->GetId(), selAirbagParticle->GetName().c_str());
            }

            bool sh3n = false;
            bool sh4n = false;
            for(int k=0; k<lsdNorif; k=k+1)
            {
                int lsdLNID;
                sdiValueEntity lsdLNIDEntity;
                selAirbagParticle->GetValue(sdiIdentifier("NIDi",0,k),tempValue);
                tempValue.GetValue(lsdLNIDEntity);
                lsdLNID = lsdLNIDEntity.GetId();



                HandleRead ElementHRead;
                if(p_radiossModel->FindById("/SHELL", lsdLNID, ElementHRead)) sh4n = true;
                if(sh4n) Sh4nPartSet.push_back(lsdLNID);

                if(p_radiossModel->FindById("/SH3N", lsdLNID, ElementHRead)) sh3n = true;
                if(sh3n) Sh3nPartSet.push_back(lsdLNID);


            }

            if(sh4n)
            {
                HandleEdit Sh4nSetHedit;
                p_radiossModel->CreateEntity(Sh4nSetHedit, "/SET/GENERAL", "PART_SET_GENERAL_SHELL_AIRBAG_PARTICLE_" + to_string(selAirbagParticle->GetId()));
                EntityEdit Sh4nSetEntityEdit(p_radiossModel, Sh4nSetHedit);

                Sh4nSetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                Sh4nSetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("SHELL")));
                Sh4nSetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue((int) Sh4nPartSet.size()));
                Sh4nSetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radPartType, Sh4nPartSet)));
                //radAirbagEdit.SetEntityHandle(sdiIdentifier("surf_IDinj",0,0), Sh4nSetHedit);

                if (Sh4nSetEntityEdit.GetId())
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDinj"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/SET/GENERAL"),sdiUIntList(1, Sh4nSetEntityEdit.GetId()))));
                sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(Sh4nSetHedit, sourceConVol));
            }
            if(sh3n)
            {
                HandleEdit Sh3nSetHedit;
                p_radiossModel->CreateEntity(Sh3nSetHedit, "/SET/GENERAL", "PART_SET_GENERAL_SHELL_AIRBAG_PARTICLE_" + to_string(selAirbagParticle->GetId()));
                EntityEdit Sh3nSetEntityEdit(p_radiossModel, Sh3nSetHedit);

                Sh3nSetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                Sh3nSetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("SH3N")));
                Sh3nSetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue((int) Sh3nPartSet.size()));
                Sh3nSetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radPartType, Sh3nPartSet)));
                //radAirbagEdit.SetEntityHandle(sdiIdentifier("surf_IDinj",0,0), Sh3nSetHedit);

                if (Sh3nSetEntityEdit.GetId())
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDinj"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/SET/GENERAL"),sdiUIntList(1, Sh3nSetEntityEdit.GetId()))));
                sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(Sh3nSetHedit, sourceConVol));
            }
        }

        // Create vent input
        int lsdNvent = 0;
        tempValue = sdiValue(lsdNvent);
        selAirbagParticle->GetValue(sdiIdentifier("NVENT"), tempValue);
        tempValue.GetValue(lsdNvent);
        p_ConvertUtils.CopyValue(*selAirbagParticle, radAirbagEdit, "NVENT", "Nvent");

        sdiStringList VentNames;
        VentNames = { { "Vent1", "Vent2", "Vent3", "Vent4", "Vent5", "Vent6", "Vent7", "Vent8", "Vent9", "Vent10"} };
        if(lsdNvent > 0)
        {
//------
            for(int k=0; k<lsdNvent; k=k+1)
            {
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Iform",0,k), sdiValue(1));
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Tstart_arr_X",0,k), sdiValue(0.0));
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("ABG_dPdef",0,k), sdiValue(pow(10,30)));
            }
//------
            int lsd_stype_3_1 = 0;
            tempValue =sdiValue(lsd_stype_3_1);
            selAirbagParticle->GetValue(sdiIdentifier("STYPE3_1"), tempValue);
            tempValue.GetValue(lsd_stype_3_1);

            sdiValueEntity lsdID3Entity;
            tempValue = sdiValue(lsdID3Entity);
            selAirbagParticle->GetValue(sdiIdentifier("SID3_1"), tempValue);
            tempValue.GetValue(lsdID3Entity);

            if(lsd_stype_3_1 == 0)
            {
                // create set of defined part and extract surface from set
                HandleEdit NventPartSetHedit;
                p_radiossModel->CreateEntity(NventPartSetHedit, "/SET/GENERAL", "PART_SET_GENERAL_PART_AIRBAG_PARTICLE_" + to_string(selAirbagParticle->GetId()));
                EntityEdit NventPartSetEdit(p_radiossModel, NventPartSetHedit);

                NventPartSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
                NventPartSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radPartType, lsdID3Entity.GetId())));
                //radAirbagEdit.SetEntityHandle(sdiIdentifier("surf_IDv",0,0), NventPartSetHedit);

                if (NventPartSetEdit.GetId())
                    radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,0), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"),NventPartSetEdit.GetId())));
                sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(NventPartSetHedit, sourceConVol));
            }
            else if(lsd_stype_3_1 == 1)
            {
                // extract surface from set
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,0), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"), lsdID3Entity.GetId())));
            }

            double lsdC23_1 = 0;
            tempValue =sdiValue(lsdC23_1);
            selAirbagParticle->GetValue(sdiIdentifier("C23_1"), tempValue);
            tempValue.GetValue(lsdC23_1);
            //p_ConvertUtils.CopyValue(*selAirbagParticle, radAirbagEdit, "C23_1", "Avent");
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Avent",0,0), sdiValue(lsdC23_1));

            sdiValueEntity lsdLCTC23_1;
            tempValue = sdiValue(lsdLCTC23_1);
            selAirbagParticle->GetValue(sdiIdentifier("LCTC23_1"), tempValue);
            tempValue.GetValue(lsdLCTC23_1);
            unsigned int LCTC23_1Id=lsdLCTC23_1.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDt",0,0), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCTC23_1Id)));

            sdiValueEntity lsdLCPC23_1;
            tempValue = sdiValue(lsdLCPC23_1);
            selAirbagParticle->GetValue(sdiIdentifier("LCPC23_1"), tempValue);
            tempValue.GetValue(lsdLCPC23_1);
            unsigned int LCPC23_1Id=lsdLCPC23_1.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDP",0,0), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCPC23_1Id)));

            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("vent_title",0,0), sdiValue(VentNames[0]));
        } // if(lsdNvent > 0)

        if(lsdNvent > 1)
        {
            int lsd_stype_3_2 = 0;
            tempValue =sdiValue(lsd_stype_3_2);
            selAirbagParticle->GetValue(sdiIdentifier("STYPE3_2"), tempValue);
            tempValue.GetValue(lsd_stype_3_2);

            sdiValueEntity lsdID3Entity;
            tempValue = sdiValue(lsdID3Entity);
            selAirbagParticle->GetValue(sdiIdentifier("SID3_2"), tempValue);
            tempValue.GetValue(lsdID3Entity);

            if(lsd_stype_3_2 == 0)
            {
                // create set of defined part and extract surface from set
                HandleEdit NventPartSetHedit;
                p_radiossModel->CreateEntity(NventPartSetHedit, "/SET/GENERAL", "PART_SET_GENERAL_PART_AIRBAG_PARTICLE_" + to_string(selAirbagParticle->GetId()));
                EntityEdit NventPartSetEdit(p_radiossModel, NventPartSetHedit);

                NventPartSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
                NventPartSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radPartType, lsdID3Entity.GetId())));
                //radAirbagEdit.SetEntityHandle(sdiIdentifier("surf_IDv",0,1), NventPartSetHedit);

                if (NventPartSetEdit.GetId())
                    radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,1), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"),NventPartSetEdit.GetId())));
                sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(NventPartSetHedit, sourceConVol));
            }
            else if(lsd_stype_3_2 == 1)
            {
                // extract surface from set
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,1), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"), lsdID3Entity.GetId())));
            }

            double lsdC23_2 = 0;
            tempValue =sdiValue(lsdC23_2);
            selAirbagParticle->GetValue(sdiIdentifier("C23_2"), tempValue);
            tempValue.GetValue(lsdC23_2);
            //p_ConvertUtils.CopyValue(*selAirbagParticle, radAirbagEdit, "C23_2", "Avent");
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Avent",0,1), sdiValue(lsdC23_2));

            sdiValueEntity lsdLCTC23_2;
            tempValue = sdiValue(lsdLCTC23_2);
            selAirbagParticle->GetValue(sdiIdentifier("LCTC23_2"), tempValue);
            tempValue.GetValue(lsdLCTC23_2);
            unsigned int LCTC23_2Id=lsdLCTC23_2.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDt",0,1), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCTC23_2Id)));

            sdiValueEntity lsdLCPC23_2;
            tempValue = sdiValue(lsdLCPC23_2);
            selAirbagParticle->GetValue(sdiIdentifier("LCPC23_2"), tempValue);
            tempValue.GetValue(lsdLCPC23_2);
            unsigned int LCPC23_2Id=lsdLCPC23_2.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDP",0,1), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCPC23_2Id)));

            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("vent_title",0,1), sdiValue(VentNames[1]));
        } // if(lsdNvent > 1)

        if(lsdNvent > 2)
        {
            int lsd_stype_3_3 = 0;
            tempValue =sdiValue(lsd_stype_3_3);
            selAirbagParticle->GetValue(sdiIdentifier("STYPE3_3"), tempValue);
            tempValue.GetValue(lsd_stype_3_3);

            sdiValueEntity lsdID3Entity;
            tempValue = sdiValue(lsdID3Entity);
            selAirbagParticle->GetValue(sdiIdentifier("SID3_3"), tempValue);
            tempValue.GetValue(lsdID3Entity);

            if(lsd_stype_3_3 == 0)
            {
                // create set of defined part and extract surface from set
                HandleEdit NventPartSetHedit;
                p_radiossModel->CreateEntity(NventPartSetHedit, "/SET/GENERAL", "PART_SET_GENERAL_PART_AIRBAG_PARTICLE_" + to_string(selAirbagParticle->GetId()));
                EntityEdit NventPartSetEdit(p_radiossModel, NventPartSetHedit);

                NventPartSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
                NventPartSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radPartType, lsdID3Entity.GetId())));
                //radAirbagEdit.SetEntityHandle(sdiIdentifier("surf_IDv",0,2), NventPartSetHedit);

                if (NventPartSetEdit.GetId())
                    radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,2), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"),NventPartSetEdit.GetId())));
                sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(NventPartSetHedit, sourceConVol));
            }
            else if(lsd_stype_3_3 == 1)
            {
                // extract surface from set
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,2), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"), lsdID3Entity.GetId())));
            }

            double lsdC23_3 = 0;
            tempValue =sdiValue(lsdC23_3);
            selAirbagParticle->GetValue(sdiIdentifier("C23_3"), tempValue);
            tempValue.GetValue(lsdC23_3);
            //p_ConvertUtils.CopyValue(*selAirbagParticle, radAirbagEdit, "C23_3", "Avent");
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Avent",0,2), sdiValue(lsdC23_3));

            sdiValueEntity lsdLCTC23_3;
            tempValue = sdiValue(lsdLCTC23_3);
            selAirbagParticle->GetValue(sdiIdentifier("LCTC23_3"), tempValue);
            tempValue.GetValue(lsdLCTC23_3);
            unsigned int LCTC23_3Id=lsdLCTC23_3.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDt",0,2), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCTC23_3Id)));

            sdiValueEntity lsdLCPC23_3;
            tempValue = sdiValue(lsdLCPC23_3);
            selAirbagParticle->GetValue(sdiIdentifier("LCPC23_3"), tempValue);
            tempValue.GetValue(lsdLCPC23_3);
            unsigned int LCPC23_3Id=lsdLCPC23_3.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDP",0,2), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCPC23_3Id)));

            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("vent_title",0,2), sdiValue(VentNames[2]));
        } // if(lsdNvent > 2)

        if(lsdNvent > 3)
        {
            int lsd_stype_3_4 = 0;
            tempValue =sdiValue(lsd_stype_3_4);
            selAirbagParticle->GetValue(sdiIdentifier("STYPE3_4"), tempValue);
            tempValue.GetValue(lsd_stype_3_4);

            sdiValueEntity lsdID3Entity;
            tempValue = sdiValue(lsdID3Entity);
            selAirbagParticle->GetValue(sdiIdentifier("SID3_4"), tempValue);
            tempValue.GetValue(lsdID3Entity);

            if(lsd_stype_3_4 == 0)
            {
                // create set of defined part and extract surface from set
                HandleEdit NventPartSetHedit;
                p_radiossModel->CreateEntity(NventPartSetHedit, "/SET/GENERAL", "PART_SET_GENERAL_PART_AIRBAG_PARTICLE_" + to_string(selAirbagParticle->GetId()));
                EntityEdit NventPartSetEdit(p_radiossModel, NventPartSetHedit);

                NventPartSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
                NventPartSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radPartType, lsdID3Entity.GetId())));
                //radAirbagEdit.SetEntityHandle(sdiIdentifier("surf_IDv",0,3), NventPartSetHedit);

                if (NventPartSetEdit.GetId())
                    radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,3), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"),NventPartSetEdit.GetId())));
                sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(NventPartSetHedit, sourceConVol));
            }
            else if(lsd_stype_3_4 == 1)
            {
                // extract surface from set
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,3), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"), lsdID3Entity.GetId())));
            }

            double lsdC23_4 = 0;
            tempValue =sdiValue(lsdC23_4);
            selAirbagParticle->GetValue(sdiIdentifier("C23_4"), tempValue);
            tempValue.GetValue(lsdC23_4);
            //p_ConvertUtils.CopyValue(*selAirbagParticle, radAirbagEdit, "C23_4", "Avent");
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Avent",0,3), sdiValue(lsdC23_4));

            sdiValueEntity lsdLCTC23_4;
            tempValue = sdiValue(lsdLCTC23_4);
            selAirbagParticle->GetValue(sdiIdentifier("LCTC23_4"), tempValue);
            tempValue.GetValue(lsdLCTC23_4);
            unsigned int LCTC23_4Id=lsdLCTC23_4.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDt",0,3), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCTC23_4Id)));

            sdiValueEntity lsdLCPC23_4;
            tempValue = sdiValue(lsdLCPC23_4);
            selAirbagParticle->GetValue(sdiIdentifier("LCPC23_4"), tempValue);
            tempValue.GetValue(lsdLCPC23_4);
            unsigned int LCPC23_4Id=lsdLCPC23_4.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDP",0,3), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCPC23_4Id)));

            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("vent_title",0,3), sdiValue(VentNames[3]));
        } // if(lsdNvent > 3)

        if(lsdNvent > 4)
        {
            int lsd_stype_3_5 = 0;
            tempValue =sdiValue(lsd_stype_3_5);
            selAirbagParticle->GetValue(sdiIdentifier("STYPE3_5"), tempValue);
            tempValue.GetValue(lsd_stype_3_5);

            sdiValueEntity lsdID3Entity;
            tempValue = sdiValue(lsdID3Entity);
            selAirbagParticle->GetValue(sdiIdentifier("SID3_5"), tempValue);
            tempValue.GetValue(lsdID3Entity);

            if(lsd_stype_3_5 == 0)
            {
                // create set of defined part and extract surface from set
                HandleEdit NventPartSetHedit;
                p_radiossModel->CreateEntity(NventPartSetHedit, "/SET/GENERAL", "PART_SET_GENERAL_PART_AIRBAG_PARTICLE_" + to_string(selAirbagParticle->GetId()));
                EntityEdit NventPartSetEdit(p_radiossModel, NventPartSetHedit);

                NventPartSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
                NventPartSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radPartType, lsdID3Entity.GetId())));
                //radAirbagEdit.SetEntityHandle(sdiIdentifier("surf_IDv",0,4), NventPartSetHedit);

                if (NventPartSetEdit.GetId())
                    radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,4), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"),NventPartSetEdit.GetId())));
                sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(NventPartSetHedit, sourceConVol));
            }
            else if(lsd_stype_3_5 == 1)
            {
                // extract surface from set
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,4), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"), lsdID3Entity.GetId())));
            }

            double lsdC23_5 = 0;
            tempValue =sdiValue(lsdC23_5);
            selAirbagParticle->GetValue(sdiIdentifier("C23_5"), tempValue);
            tempValue.GetValue(lsdC23_5);
            //p_ConvertUtils.CopyValue(*selAirbagParticle, radAirbagEdit, "C23_5", "Avent");
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Avent",0,4), sdiValue(lsdC23_5));

            sdiValueEntity lsdLCTC23_5;
            tempValue = sdiValue(lsdLCTC23_5);
            selAirbagParticle->GetValue(sdiIdentifier("LCTC23_5"), tempValue);
            tempValue.GetValue(lsdLCTC23_5);
            unsigned int LCTC23_5Id=lsdLCTC23_5.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDt",0,4), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCTC23_5Id)));

            sdiValueEntity lsdLCPC23_5;
            tempValue = sdiValue(lsdLCPC23_5);
            selAirbagParticle->GetValue(sdiIdentifier("LCPC23_5"), tempValue);
            tempValue.GetValue(lsdLCPC23_5);
            unsigned int LCPC23_5Id=lsdLCPC23_5.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDP",0,4), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCPC23_5Id)));

            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("vent_title",0,4), sdiValue(VentNames[4]));
        } // if(lsdNvent > 4)

        if(lsdNvent > 5)
        {
            int lsd_stype_3_6 = 0;
            tempValue =sdiValue(lsd_stype_3_6);
            selAirbagParticle->GetValue(sdiIdentifier("STYPE3_6"), tempValue);
            tempValue.GetValue(lsd_stype_3_6);

            sdiValueEntity lsdID3Entity;
            tempValue = sdiValue(lsdID3Entity);
            selAirbagParticle->GetValue(sdiIdentifier("SID3_6"), tempValue);
            tempValue.GetValue(lsdID3Entity);

            if(lsd_stype_3_6 == 0)
            {
                // create set of defined part and extract surface from set
                HandleEdit NventPartSetHedit;
                p_radiossModel->CreateEntity(NventPartSetHedit, "/SET/GENERAL", "PART_SET_GENERAL_PART_AIRBAG_PARTICLE_" + to_string(selAirbagParticle->GetId()));
                EntityEdit NventPartSetEdit(p_radiossModel, NventPartSetHedit);

                NventPartSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
                NventPartSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radPartType, lsdID3Entity.GetId())));
                //radAirbagEdit.SetEntityHandle(sdiIdentifier("surf_IDv",0,5), NventPartSetHedit);

                if (NventPartSetEdit.GetId())
                    radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,5), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"),NventPartSetEdit.GetId())));
                sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(NventPartSetHedit, sourceConVol));
            }
            else if(lsd_stype_3_6 == 1)
            {
                // extract surface from set
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,5), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"), lsdID3Entity.GetId())));
            }

            double lsdC23_6 = 0;
            tempValue =sdiValue(lsdC23_6);
            selAirbagParticle->GetValue(sdiIdentifier("C23_6"), tempValue);
            tempValue.GetValue(lsdC23_6);
            //p_ConvertUtils.CopyValue(*selAirbagParticle, radAirbagEdit, "C23_6", "Avent");
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Avent",0,5), sdiValue(lsdC23_6));

            sdiValueEntity lsdLCTC23_6;
            tempValue = sdiValue(lsdLCTC23_6);
            selAirbagParticle->GetValue(sdiIdentifier("LCTC23_6"), tempValue);
            tempValue.GetValue(lsdLCTC23_6);
            unsigned int LCTC23_6Id=lsdLCTC23_6.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDt",0,5), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCTC23_6Id)));

            sdiValueEntity lsdLCPC23_6;
            tempValue = sdiValue(lsdLCPC23_6);
            selAirbagParticle->GetValue(sdiIdentifier("LCPC23_6"), tempValue);
            tempValue.GetValue(lsdLCPC23_6);
            unsigned int LCPC23_6Id=lsdLCPC23_6.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDP",0,5), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCPC23_6Id)));

            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("vent_title",0,5), sdiValue(VentNames[5]));
        } // if(lsdNvent > 5)

        if(lsdNvent > 6)
        {
            int lsd_stype_3_7 = 0;
            tempValue =sdiValue(lsd_stype_3_7);
            selAirbagParticle->GetValue(sdiIdentifier("STYPE3_7"), tempValue);
            tempValue.GetValue(lsd_stype_3_7);

            sdiValueEntity lsdID3Entity;
            tempValue = sdiValue(lsdID3Entity);
            selAirbagParticle->GetValue(sdiIdentifier("SID3_7"), tempValue);
            tempValue.GetValue(lsdID3Entity);

            if(lsd_stype_3_7 == 0)
            {
                // create set of defined part and extract surface from set
                HandleEdit NventPartSetHedit;
                p_radiossModel->CreateEntity(NventPartSetHedit, "/SET/GENERAL", "PART_SET_GENERAL_PART_AIRBAG_PARTICLE_" + to_string(selAirbagParticle->GetId()));
                EntityEdit NventPartSetEdit(p_radiossModel, NventPartSetHedit);

                NventPartSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
                NventPartSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radPartType, lsdID3Entity.GetId())));
                //radAirbagEdit.SetEntityHandle(sdiIdentifier("surf_IDv",0,6), NventPartSetHedit);

                if (NventPartSetEdit.GetId())
                    radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,6), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"),NventPartSetEdit.GetId())));
                sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(NventPartSetHedit, sourceConVol));
            }
            else if(lsd_stype_3_7 == 1)
            {
                // extract surface from set
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,6), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"), lsdID3Entity.GetId())));
            }

            double lsdC23_7 = 0;
            tempValue =sdiValue(lsdC23_7);
            selAirbagParticle->GetValue(sdiIdentifier("C23_7"), tempValue);
            tempValue.GetValue(lsdC23_7);
            //p_ConvertUtils.CopyValue(*selAirbagParticle, radAirbagEdit, "C23_7", "Avent");
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Avent",0,6), sdiValue(lsdC23_7));

            sdiValueEntity lsdLCTC23_7;
            tempValue = sdiValue(lsdLCTC23_7);
            selAirbagParticle->GetValue(sdiIdentifier("LCTC23_7"), tempValue);
            tempValue.GetValue(lsdLCTC23_7);
            unsigned int LCTC23_7Id=lsdLCTC23_7.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDt",0,6), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCTC23_7Id)));

            sdiValueEntity lsdLCPC23_7;
            tempValue = sdiValue(lsdLCPC23_7);
            selAirbagParticle->GetValue(sdiIdentifier("LCPC23_7"), tempValue);
            tempValue.GetValue(lsdLCPC23_7);
            unsigned int LCPC23_7Id=lsdLCPC23_7.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDP",0,6), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCPC23_7Id)));

            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("vent_title",0,6), sdiValue(VentNames[6]));
        } // if(lsdNvent > 6)

        if(lsdNvent > 7)
        {
            int lsd_stype_3_8 = 0;
            tempValue =sdiValue(lsd_stype_3_8);
            selAirbagParticle->GetValue(sdiIdentifier("STYPE3_8"), tempValue);
            tempValue.GetValue(lsd_stype_3_8);

            sdiValueEntity lsdID3Entity;
            tempValue = sdiValue(lsdID3Entity);
            selAirbagParticle->GetValue(sdiIdentifier("SID3_8"), tempValue);
            tempValue.GetValue(lsdID3Entity);

            if(lsd_stype_3_8 == 0)
            {
                // create set of defined part and extract surface from set
                HandleEdit NventPartSetHedit;
                p_radiossModel->CreateEntity(NventPartSetHedit, "/SET/GENERAL", "PART_SET_GENERAL_PART_AIRBAG_PARTICLE_" + to_string(selAirbagParticle->GetId()));
                EntityEdit NventPartSetEdit(p_radiossModel, NventPartSetHedit);

                NventPartSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
                NventPartSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radPartType, lsdID3Entity.GetId())));
                //radAirbagEdit.SetEntityHandle(sdiIdentifier("surf_IDv",0,7), NventPartSetHedit);

                if (NventPartSetEdit.GetId())
                    radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,7), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"),NventPartSetEdit.GetId())));
                sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(NventPartSetHedit, sourceConVol));
            }
            else if(lsd_stype_3_8 == 1)
            {
                // extract surface from set
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,7), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"), lsdID3Entity.GetId())));
            }

            double lsdC23_8 = 0;
            tempValue =sdiValue(lsdC23_8);
            selAirbagParticle->GetValue(sdiIdentifier("C23_8"), tempValue);
            tempValue.GetValue(lsdC23_8);
            //p_ConvertUtils.CopyValue(*selAirbagParticle, radAirbagEdit, "C23_8", "Avent");
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Avent",0,7), sdiValue(lsdC23_8));

            sdiValueEntity lsdLCTC23_8;
            tempValue = sdiValue(lsdLCTC23_8);
            selAirbagParticle->GetValue(sdiIdentifier("LCTC23_8"), tempValue);
            tempValue.GetValue(lsdLCTC23_8);
            unsigned int LCTC23_8Id=lsdLCTC23_8.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDt",0,7), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCTC23_8Id)));

            sdiValueEntity lsdLCPC23_8;
            tempValue = sdiValue(lsdLCPC23_8);
            selAirbagParticle->GetValue(sdiIdentifier("LCPC23_8"), tempValue);
            tempValue.GetValue(lsdLCPC23_8);
            unsigned int LCPC23_8Id=lsdLCPC23_8.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDP",0,7), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCPC23_8Id)));

            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("vent_title",0,7), sdiValue(VentNames[7]));
        } // if(lsdNvent > 7)

        if(lsdNvent > 8)
        {
            int lsd_stype_3_9 = 0;
            tempValue =sdiValue(lsd_stype_3_9);
            selAirbagParticle->GetValue(sdiIdentifier("STYPE3_9"), tempValue);
            tempValue.GetValue(lsd_stype_3_9);

            sdiValueEntity lsdID3Entity;
            tempValue = sdiValue(lsdID3Entity);
            selAirbagParticle->GetValue(sdiIdentifier("SID3_9"), tempValue);
            tempValue.GetValue(lsdID3Entity);

            if(lsd_stype_3_9 == 0)
            {
                // create set of defined part and extract surface from set
                HandleEdit NventPartSetHedit;
                p_radiossModel->CreateEntity(NventPartSetHedit, "/SET/GENERAL", "PART_SET_GENERAL_PART_AIRBAG_PARTICLE_" + to_string(selAirbagParticle->GetId()));
                EntityEdit NventPartSetEdit(p_radiossModel, NventPartSetHedit);

                NventPartSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
                NventPartSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radPartType, lsdID3Entity.GetId())));
                //radAirbagEdit.SetEntityHandle(sdiIdentifier("surf_IDv",0,8), NventPartSetHedit);

                if (NventPartSetEdit.GetId())
                    radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,8), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"),NventPartSetEdit.GetId())));
                sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(NventPartSetHedit, sourceConVol));
            }
            else if(lsd_stype_3_9 == 1)
            {
                // extract surface from set
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,8), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"), lsdID3Entity.GetId())));
            }

            double lsdC23_9 = 0;
            tempValue =sdiValue(lsdC23_9);
            selAirbagParticle->GetValue(sdiIdentifier("C23_9"), tempValue);
            tempValue.GetValue(lsdC23_9);
            //p_ConvertUtils.CopyValue(*selAirbagParticle, radAirbagEdit, "C23_9", "Avent");
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Avent",0,8), sdiValue(lsdC23_9));

            sdiValueEntity lsdLCTC23_9;
            tempValue = sdiValue(lsdLCTC23_9);
            selAirbagParticle->GetValue(sdiIdentifier("LCTC23_9"), tempValue);
            tempValue.GetValue(lsdLCTC23_9);
            unsigned int LCTC23_9Id=lsdLCTC23_9.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDt",0,8), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCTC23_9Id)));

            sdiValueEntity lsdLCPC23_9;
            tempValue = sdiValue(lsdLCPC23_9);
            selAirbagParticle->GetValue(sdiIdentifier("LCPC23_9"), tempValue);
            tempValue.GetValue(lsdLCPC23_9);
            unsigned int LCPC23_9Id=lsdLCPC23_9.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDP",0,8), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCPC23_9Id)));

            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("vent_title",0,8), sdiValue(VentNames[8]));
        } // if(lsdNvent > 8)

        if(lsdNvent > 9)
        {
            int lsd_stype_3_10 = 0;
            tempValue =sdiValue(lsd_stype_3_10);
            selAirbagParticle->GetValue(sdiIdentifier("STYPE3_10"), tempValue);
            tempValue.GetValue(lsd_stype_3_10);

            sdiValueEntity lsdID3Entity;
            tempValue = sdiValue(lsdID3Entity);
            selAirbagParticle->GetValue(sdiIdentifier("SID3_10"), tempValue);
            tempValue.GetValue(lsdID3Entity);

            if(lsd_stype_3_10 == 0)
            {
                // create set of defined part and extract surface from set
                HandleEdit NventPartSetHedit;
                p_radiossModel->CreateEntity(NventPartSetHedit, "/SET/GENERAL", "PART_SET_GENERAL_PART_AIRBAG_PARTICLE_" + to_string(selAirbagParticle->GetId()));
                EntityEdit NventPartSetEdit(p_radiossModel, NventPartSetHedit);

                NventPartSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
                NventPartSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                NventPartSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radPartType, lsdID3Entity.GetId())));
                //radAirbagEdit.SetEntityHandle(sdiIdentifier("surf_IDv",0,9), NventPartSetHedit);

                if (NventPartSetEdit.GetId())
                    radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,9), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"),NventPartSetEdit.GetId())));
                sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(NventPartSetHedit, sourceConVol));
            }
            else if(lsd_stype_3_10 == 1)
            {
                // extract surface from set
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDv",0,9), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"), lsdID3Entity.GetId())));
            }

            double lsdC23_10 = 0;
            tempValue =sdiValue(lsdC23_10);
            selAirbagParticle->GetValue(sdiIdentifier("C23_10"), tempValue);
            tempValue.GetValue(lsdC23_10);
            //p_ConvertUtils.CopyValue(*selAirbagParticle, radAirbagEdit, "C23_10", "Avent");
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Avent",0,9), sdiValue(lsdC23_10));

            sdiValueEntity lsdLCTC23_10;
            tempValue = sdiValue(lsdLCTC23_10);
            selAirbagParticle->GetValue(sdiIdentifier("LCTC23_10"), tempValue);
            tempValue.GetValue(lsdLCTC23_10);
            unsigned int LCTC23_10Id=lsdLCTC23_10.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDt",0,9), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCTC23_10Id)));

            sdiValueEntity lsdLCPC23_10;
            tempValue = sdiValue(lsdLCPC23_10);
            selAirbagParticle->GetValue(sdiIdentifier("LCPC23_10"), tempValue);
            tempValue.GetValue(lsdLCPC23_10);
            unsigned int LCPC23_10Id=lsdLCPC23_10.GetId();
            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDP",0,9), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCPC23_10Id)));

            radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("vent_title",0,9), sdiValue(VentNames[9]));
        } // if(lsdNvent > 9)

        //---
        // Create porosity
        //---
        sdiUIntList PartSetMAT58List;

        HandleRead partHread;
        int poro_count = 0;
        sdiString PoroSurfaceName;
        for(int k=0; k<PartSetList.size(); k=k+1)
        {
            int partID = PartSetList[k];
            HandleRead dynapartHread;
            p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), partID, dynapartHread);

            if(dynapartHread.IsValid())
            {
                HandleRead dynamatHRead;
                dynapartHread.GetEntityHandle(p_lsdynaModel, sdiIdentifier("MID"), dynamatHRead);
                EntityRead dynamatRead(p_lsdynaModel, dynamatHRead);

                HandleEdit radmatHEdit;
                p_radiossModel->FindById("/MAT", dynamatRead.GetId(), radmatHEdit);
                EntityEdit radmatEdit(p_radiossModel, radmatHEdit);
                if (airbag_sensorID > 0) 
                  radmatHEdit.SetValue(p_radiossModel, sdiIdentifier("sensor_ID"), sdiValue(sdiValueEntityList(radSensorType, sdiUIntList(1,airbag_sensorID))));

                if (dynamatHRead.IsValid())
                {
                    HandleRead crvLCIDHandle;
                    dynamatRead.GetEntityHandle(sdiIdentifier("LSD_LCID2"), crvLCIDHandle);
                    EntityRead crvLCIDRead(p_lsdynaModel, crvLCIDHandle);

                    double lsdFORM;
                    tempValue = sdiValue(lsdFORM);
                    dynamatRead.GetValue(sdiIdentifier("FORM"), tempValue);
                    tempValue.GetValue(lsdFORM);

                    if((lsdFORM == 14.0 || lsdFORM == -14.0) && crvLCIDHandle.IsValid())
                    {
                        // convert to /MAT/LAW58 with porosity
                        PartSetMAT58List.push_back(partID);

                        // create function "FUNCT_IDV" --> from "LEAK_FCT_IDLC"
                        int nPnts = 0;
                        sdiDoubleList crvPoints;

                        double lsdSFA = 1.0;
                        double lsdSFO = 1.0;
                        double lsdOFFA = 0.0;
                        double lsdOFFO = 0.0;

                        tempValue = sdiValue(crvPoints);
                        crvLCIDRead.GetValue(sdiIdentifier("points"), tempValue);
                        tempValue.GetValue(crvPoints);

                        tempValue = sdiValue(nPnts);
                        crvLCIDRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
                        tempValue.GetValue(nPnts);
                        crvPoints.reserve(2 * nPnts + 2);

                        vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
                        vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
                        p_ConvertUtils.GetAttribValues(crvLCIDRead, lsdQueryAttribs, attribVals);
                        lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
                        lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
                        //-----------------------
                        HandleEdit functHEdit;
                        p_ConvertUtils.CreateCurve("Porosity_shift_" + to_string(crvLCIDHandle.GetId(p_lsdynaModel)) + "_MatL34_" + to_string(dynamatRead.GetId()),
                                        (int)crvPoints.size() / 2, crvPoints, functHEdit, lsdSFA, lsdSFO, lsdOFFA-lsdPEXT ,lsdOFFO);

                        if (functHEdit.IsValid())
                        {
                            radAirbagHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("ABG_fct",0,poro_count), functHEdit);
                            sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
                            sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourceConVol));
                        }

                        //PoroSurfaceName = "Porous surface_" + to_string(poro_count+1)+
                        //                + "_from_function_" + to_string(crvLCIDHandle.GetId(p_lsdynaModel))+ "_MatL34_" + to_string(dynamatRead.GetId());
                        PoroSurfaceName = "Porous surface_" + to_string(poro_count+1);
                        radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surface_title",0,poro_count), sdiValue(PoroSurfaceName));
                        poro_count = poro_count + 1;
                        //-----------------------
                    }
                }
            }
        }  //

        // create set of part for each of the porous surfaces
        radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("ABG_Nporsurf"), sdiValue((int)PartSetMAT58List.size()));

        for(size_t k=0; k < PartSetMAT58List.size(); k=k+1)
        {
            int partID = PartSetMAT58List[k];

            HandleEdit NporPartSetHedit;
            p_radiossModel->CreateEntity(NporPartSetHedit, "/SET/GENERAL", "PART_SET_GENERAL_PART_AIRBAG_PARTICLE_" + to_string(selAirbagParticle->GetId()));
            EntityEdit NporPartSetEdit(p_radiossModel, NporPartSetHedit);

            NporPartSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
            NporPartSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
            NporPartSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
            NporPartSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radPartType, partID)));

            if (NporPartSetEdit.GetId())
                radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("surf_IDps",0,k), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET/GENERAL"),NporPartSetEdit.GetId())));
            sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(NporPartSetHedit, sourceConVol));

           radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Iformps",0,k), sdiValue(2));
           radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Iblockage",0,k), sdiValue(1));
           radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Tstart_arr_Y",0,k), sdiValue(0.0));
           radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("ABG_dPdef_ps",0,k), sdiValue(pow(10,30)));

           p_ConvertUtils.CopyValue(*selAirbagParticle, radAirbagEdit, "TSW", "Tswitch");
        }

        // Values by default:
        radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Ittf"), sdiValue(3));
        radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Cgmerg"), sdiValue(0.05));
        radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Dtsca"), sdiValue(0.9));
        if (lsdUNIT == 0)
          radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Dtmin"), sdiValue(1E-04));
        else if (lsdUNIT == 1 || lsdUNIT == 2)
          radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Dtmin"), sdiValue(1E-07));
//---
        // create "/TH/MONV"
        sdiUIntList monVolList;
        SelectionRead selMonvol(p_radiossModel, "/MONVOL/FVMBAG2");
        monVolList.reserve(selMonvol.Count());
        HandleEdit radThMonvHEdit;

        while (selMonvol.Next()) monVolList.push_back(selMonvol->GetId());

        if (!radThMonvHEdit.IsValid() && monVolList.size() > 0)
        {
            p_radiossModel->CreateEntity(radThMonvHEdit, "/TH/MONV", "TH-MONV");
            EntityEdit radThMonvEdit(p_radiossModel, radThMonvHEdit);

            radThMonvEdit.SetValue(sdiIdentifier("idsmax"), sdiValue((int)monVolList.size()));
            radThMonvEdit.SetValue(sdiIdentifier("Number_Of_Variables"), sdiValue(1));
            radThMonvEdit.SetValue(sdiIdentifier("Obj"), sdiValue(sdiValueEntityList(destEntityType, monVolList)));
            radThMonvEdit.SetValue(sdiIdentifier("var"), sdiValue(sdiStringList({ "DEF" })));

            sdiConvert::SDIHandlReadList sourceHandles = { {selAirbagParticle->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radThMonvHEdit, sourceHandles));
        }

//---------------------------------------------
        sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagParticle->GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(radAirbagHEdit, sourceConVol));
    } // while (selAirbagParticle.Next())
}
void sdiD2R::ConvertControlVolume::ConvertAirbagLinearFluid()
{
    SelectionRead selAirbagLinearFluid(p_lsdynaModel, "*AIRBAG_LINEAR_FLUID");
    while (selAirbagLinearFluid.Next())
    {
        HandleEdit radAirbagHEdit;
        p_radiossModel->CreateEntity(radAirbagHEdit, "/MONVOL/LFLUID", selAirbagLinearFluid->GetName(), selAirbagLinearFluid->GetId());
        EntityEdit radAirbagEdit(p_radiossModel, radAirbagHEdit);
        if (radAirbagHEdit.IsValid())
        {
            sdiValueEntity surfSet;
            sdiValue tempValue;

            selAirbagLinearFluid->GetValue(sdiIdentifier("SID"), tempValue);
            tempValue.GetValue(surfSet);
            if (surfSet.GetId())
                radAirbagEdit.SetValue(sdiIdentifier("surf_IDex"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SET"), surfSet.GetId())));

            sdiValueEntity lsdLCBULK = GetValue<sdiValueEntity>(*selAirbagLinearFluid, "LCBULK");
            unsigned int LCBULKID=lsdLCBULK.GetId();

            if(LCBULKID > 0)
              // read and convert into function
              radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_K"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCBULKID)));
            else if(LCBULKID == 0)
            {
              // create new fuction (from scalar)
              HandleEdit functHEdit;
              double lsdBULK = GetValue<double>(*selAirbagLinearFluid, "BULK");

              p_ConvertUtils.CreateCurve(selAirbagLinearFluid->GetName()+"_LCBULK", 2, { {0, lsdBULK, 1, lsdBULK} }, functHEdit);

              if (functHEdit.IsValid())
              {
                  radAirbagHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("fct_K"), functHEdit);
                  sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagLinearFluid->GetHandle()} };
                  sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourceConVol));
              }
            }

            p_ConvertUtils.CopyValue(*selAirbagLinearFluid, radAirbagEdit, "RO", "Rho");

            sdiValueEntity lsdLCINT = GetValue<sdiValueEntity>(*selAirbagLinearFluid, "LCINT");
            unsigned int LCINTID=lsdLCINT.GetId();

            if(LCINTID > 0)
              radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_Mtin"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCINTID)));

            sdiValueEntity lsdLCOUTT = GetValue<sdiValueEntity>(*selAirbagLinearFluid, "LCOUTT");
            unsigned int LCOUTTID=lsdLCOUTT.GetId();

            if(LCOUTTID > 0)
              radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Fct_Mtout"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCOUTTID)));

            sdiValueEntity lsdLCOUTP = GetValue<sdiValueEntity>(*selAirbagLinearFluid, "LCOUTP");
            unsigned int LCOUTPID=lsdLCOUTP.GetId();

            if(LCOUTPID > 0)
              radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Fct_Mpout"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCOUTPID)));

            sdiValueEntity lsdLCFIT = GetValue<sdiValueEntity>(*selAirbagLinearFluid, "LCFIT");
            unsigned int LCFITID=lsdLCFIT.GetId();

            if(LCFITID > 0)
              radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("Fct_Padd"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), LCFITID)));

            sdiValueEntity lsdPLIMLC = GetValue<sdiValueEntity>(*selAirbagLinearFluid, "P_LIMLC");
            unsigned int PLIMLCID=lsdPLIMLC.GetId();

            if(PLIMLCID > 0)
              // read and convert into function
              radAirbagHEdit.SetValue(p_radiossModel, sdiIdentifier("fct_Pmax"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), PLIMLCID)));
            else if(PLIMLCID == 0)
            {
              // create new fuction (from scalar)
              HandleEdit functHEdit;

              double lsdPLIMIT = GetValue<double>(*selAirbagLinearFluid, "P_LIMIT");

              if(lsdPLIMIT == 0.0) lsdPLIMIT = 1E+20;

              p_ConvertUtils.CreateCurve(selAirbagLinearFluid->GetName()+"_P_LIMLC", 2, { {0, lsdPLIMIT, 1, lsdPLIMIT} }, functHEdit);

              if (functHEdit.IsValid())
              {
                  radAirbagHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("fct_Pmax"), functHEdit);
                  sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagLinearFluid->GetHandle()} };
                  sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourceConVol));
              }
            }
            sdiConvert::SDIHandlReadList sourceConVol = { {selAirbagLinearFluid->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radAirbagHEdit, sourceConVol));
        } // if (radAirbagHEdit.IsValid())
//---
        // create "/TH/MONV"
        sdiUIntList monVolList;
        SelectionRead selMonvol(p_radiossModel, "/MONVOL/LFLUID");
        monVolList.reserve(selMonvol.Count());
        HandleEdit radThMonvHEdit;

        while (selMonvol.Next()) monVolList.push_back(selMonvol->GetId());

        if (!radThMonvHEdit.IsValid() && monVolList.size() > 0)
        {
            p_radiossModel->CreateEntity(radThMonvHEdit, "/TH/MONV", "TH-MONV");
            EntityEdit radThMonvEdit(p_radiossModel, radThMonvHEdit);

            radThMonvEdit.SetValue(sdiIdentifier("idsmax"), sdiValue((int)monVolList.size()));
            radThMonvEdit.SetValue(sdiIdentifier("Number_Of_Variables"), sdiValue(2));
            radThMonvEdit.SetValue(sdiIdentifier("Obj"), sdiValue(sdiValueEntityList(destEntityType, monVolList)));
            radThMonvEdit.SetValue(sdiIdentifier("var"), sdiValue(sdiStringList({ "DEF", "GAS" })));

            sdiConvert::SDIHandlReadList sourceHandles = { {selAirbagLinearFluid->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radThMonvHEdit, sourceHandles));
        }
    } // while (selAirbagLinearFluid.Next())
//---
}
