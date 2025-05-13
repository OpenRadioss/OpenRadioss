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
#include <dyna2rad/convertcards.h>
using namespace std;
using namespace sdi;
using namespace sdiConvert;

void sdiD2R::ConvertCard::ConvertCards()
{
    ConvertEntities();
}

void sdiD2R::ConvertCard::ConvertCtrlUnits()
{
    /* convert  CONTROL_UNITS */
    p_ConvertCtrlUnits();
}

void sdiD2R::ConvertCard::ConvertEntities()
{
    /*convert *DATABASE_OPTIONS*/
    p_ConvertDbOptions();

    /* convert  CONTROL_TIMESTEP */
    p_ConvertCtrlTimeStep();

    /* convert  CONTROL_TERMINATION */
    p_ConvertCtrlTermination();

    /*convert  *DATABASE_BINARY_CARDS*/
    p_ConvertDbBinaryCards();

    /* convert *DATABASE_EXTEND_BINARY */
    p_ConvertDbExtentBinaryCards();

    /* convert *DATABASE_BINARY_D3PLOT */
    p_ConvertDbBinaryD3Plot();

    /*convert control output*/
    p_ConvertControlOutput();
    
    /*convert control parallel*/
    p_ConvertControlParallel();

    /*convert *DATABASE_NODAL_FORCE_GROUP*/
    p_ConvertDbNodalForceGroup();
}


void sdiD2R::ConvertCard::p_ConvertDbOptions()
{
    vector<sdiString> dbCardList = { "*DATABASE_ABSTAT",
                                    "*DATABASE_BNDOUT",
                                    "*DATABASE_DEFORCE",
                                    "*DATABASE_ELOUT",
                                    "*DATABASE_GLSTAT",
                                    "*DATABASE_JNTFORC",
                                    "*DATABASE_MATSUM",
                                    "*DATABASE_NCFORC",
                                    "*DATABASE_NODFOR",
                                    "*DATABASE_NODOUT",
                                    "*DATABASE_RBDOUT",
                                    "*DATABASE_RCFORC",
                                    "*DATABASE_RWFORC",
                                    "*DATABASE_SBTOUT",
                                    "*DATABASE_SECFORC",
                                    "*DATABASE_SPCFORC",
                                    "*DATABASE_SWFORC" ,
                                    "*DATABASE_GLSTAT_MASS_PROPERTIES",
                                    "*DATABASE_SPHOUT",
                                    "*DATABASE_PLLYOUT",
                                    "*DATABASE_TPRINT"};
    double timeInterval = 1e30;
    SDIHandlReadList sourceCards;
    for (sdiString dbCard : dbCardList)
    {
        SelectionRead cardsSelect(p_lsdynaModel, dbCard);
        while (cardsSelect.Next())
        {
            double tempVal = 0.0;
            sdiValue tempValue(tempVal);
            cardsSelect->GetValue(sdiIdentifier("DT"), tempValue);
            tempValue.GetValue(tempVal);
            if (tempVal < timeInterval)
                timeInterval = tempVal;

            sourceCards.push_back(cardsSelect->GetHandle());

            sdiString solverCard = cardsSelect->GetKeyword();
            sdiString keyword = solverCard;
            HandleEdit radCard;
            if (keyword.find("*DATABASE_NCFORC") != keyword.npos)
            {
                p_radiossModel->CreateEntity(radCard, "/ANIM/VECT");
                radCard.SetValue(p_radiossModel, sdiIdentifier("CONT"), sdiValue(1));
                radCard.SetValue(p_radiossModel, sdiIdentifier("CONT2"), sdiValue(1));
            }

        }
    }
    if (timeInterval != 1e30)
    {
        HandleEdit cardHandleEdit;
        p_radiossModel->CreateEntity(cardHandleEdit, "/TFILE");
        cardHandleEdit.SetValue(p_radiossModel, sdiIdentifier("Type"), sdiValue(4));
        cardHandleEdit.SetValue(p_radiossModel, sdiIdentifier("DeltaThis"), sdiValue(timeInterval));

        sdiConvert::Convert::PushToConversionLog(std::make_pair(cardHandleEdit, sourceCards));
    }

}

void sdiD2R::ConvertCard::p_ConvertCtrlTimeStep()
{
    EntityType radSetType = p_radiossModel->GetEntityType("/SET");
    SelectionRead selectCtrlTS(p_lsdynaModel, "*CONTROL_TIMESTEP");
    while (selectCtrlTS.Next())
    {
        SDIHandlReadList sourceCards = { {selectCtrlTS->GetHandle()} };

        double lsdDT2MS;
        double lsdTSSFAC;
        double lsdDT2MSF;
        vector<reference_wrapper<double>> attrValList = { lsdDT2MS, lsdTSSFAC, lsdDT2MSF };
        vector<sdiString> attrNameList = { "DT2MS", "TSSFAC",  "DT2MSF" };
        p_ConvertUtils.GetAttribValues(*selectCtrlTS, attrNameList, attrValList);

        lsdTSSFAC = (lsdTSSFAC == 0.0) ? 0.9 : lsdTSSFAC;
        if (lsdDT2MS != 0.0)
        {
            HandleEdit dtNodaHandleEdit;
            p_radiossModel->CreateEntity(dtNodaHandleEdit, "/DT/NODA");
            EntityEdit dtNodaEdit(p_radiossModel, dtNodaHandleEdit);
            dtNodaEdit.SetValue(sdiIdentifier("ENG_DT_NODA_CST_0"), sdiValue(1));
            dtNodaEdit.SetValue(sdiIdentifier("FScale33"), sdiValue(lsdTSSFAC));
            dtNodaEdit.SetValue(sdiIdentifier("Tmin3"), sdiValue(abs(lsdDT2MS) * lsdTSSFAC));
            sdiConvert::Convert::PushToConversionLog(std::make_pair(dtNodaHandleEdit, sourceCards));
        }
        else
        {
            HandleEdit dtHandleEdit;
            p_radiossModel->CreateEntity(dtHandleEdit, "/DT");
            EntityEdit dtEdit(p_radiossModel, dtHandleEdit);
            dtEdit.SetValue(sdiIdentifier("SCALE"), sdiValue(lsdTSSFAC));
            dtEdit.SetValue(sdiIdentifier("Tmin"), sdiValue(0.0));
            sdiConvert::Convert::PushToConversionLog(std::make_pair(dtHandleEdit, sourceCards));
        }
        

        HandleRead lctmHandle;
        selectCtrlTS->GetEntityHandle(sdiIdentifier("LCTM"), lctmHandle);
        if (lctmHandle.IsValid())
        {
            int nPoints = 0;
            sdiValue tempValue(nPoints);
            lctmHandle.GetValue(p_lsdynaModel, sdiIdentifier("numberofpoints"), tempValue);
            tempValue.GetValue(nPoints);
            if (nPoints)
            {
                sdiDoubleList crvPoints;
                tempValue = sdiValue(crvPoints);
                lctmHandle.GetValue(p_lsdynaModel, sdiIdentifier("points"), tempValue);
                tempValue.GetValue(crvPoints);

                double firstOrd = crvPoints[1];
                double maxOrd = crvPoints[2 * nPoints - 1];

                HandleEdit dtixHandleEdit;
                p_radiossModel->CreateEntity(dtixHandleEdit, "/DTIX");
                if (dtixHandleEdit.IsValid())
                {
                    EntityEdit dtixEdit(p_radiossModel, dtixHandleEdit);
                    dtixEdit.SetValue(sdiIdentifier("ENG_DTIX_tIni"), sdiValue(firstOrd));
                    dtixEdit.SetValue(sdiIdentifier("ENG_DTIX_tMax"), sdiValue(maxOrd));
                }
                sdiConvert::Convert::PushToConversionLog(std::make_pair(dtixHandleEdit, sourceCards));
            }
        }

        HandleRead imsclHandle;
        selectCtrlTS->GetEntityHandle(sdiIdentifier("IMSCL"), imsclHandle);
        if (imsclHandle.IsValid())
        {
            EntityRead imsclRead(p_lsdynaModel, imsclHandle);
            unsigned int grPartId = imsclHandle.GetId(p_lsdynaModel);

            HandleEdit amsHandleEdit;
            p_radiossModel->CreateEntity(amsHandleEdit, "/AMS");
            amsHandleEdit.SetValue(p_radiossModel, sdiIdentifier("GRPART_ID"), sdiValue(sdiValueEntity(radSetType, 
                DynaToRad::GetRadiossSetIdFromLsdSet(imsclRead.GetId(), "*SET_PART" ))));

            HandleEdit dtAmsHandleEdit;
            p_radiossModel->CreateEntity(dtAmsHandleEdit, "/DT/AMS");

            dtAmsHandleEdit.SetValue(p_radiossModel, sdiIdentifier("SCALE"), sdiValue(0.67));
            dtAmsHandleEdit.SetValue(p_radiossModel, sdiIdentifier("Tmin"), sdiValue(abs(lsdDT2MS)));
        }
    }
}

void sdiD2R::ConvertCard::p_ConvertCtrlTermination()
{
    SelectionRead selectCtrlTerm(p_lsdynaModel, "*CONTROL_TERMINATION");
    while (selectCtrlTerm.Next())
    {
        HandleEdit runHandleEdit;
        SDIHandlReadList sourceCards = { {selectCtrlTerm->GetHandle()} };
        p_radiossModel->CreateEntity(runHandleEdit, "/RUN");
        if (runHandleEdit.IsValid())
        {
            EntityEdit runEdit(p_radiossModel, runHandleEdit);
            //runEdit.SetValue(sdiIdentifier("ENG_RUN_RunName"), sdiValue(string("")));
            runEdit.SetValue(sdiIdentifier("ENG_RUN_RunNum"), sdiValue(1));

            sdiValue tempValue(0.0);
            selectCtrlTerm->GetValue(sdiIdentifier("ENDTIM"), tempValue);
            runEdit.SetValue(sdiIdentifier("ENG_RUN_Tstop"), tempValue);

            sdiConvert::Convert::PushToConversionLog(std::make_pair(runHandleEdit, sourceCards));
        }

        HandleEdit stopHandleEdit;
        p_radiossModel->CreateEntity(stopHandleEdit, "/STOP");
        if (stopHandleEdit.IsValid())
        {
            EntityEdit stopEdit(p_radiossModel, stopHandleEdit);

            stopEdit.SetValue(sdiIdentifier("NTH"), sdiValue(1));
            stopEdit.SetValue(sdiIdentifier("NANIM"), sdiValue(1));

            sdiValue tempValue(0.0);
            selectCtrlTerm->GetValue(sdiIdentifier("ENDENG"), tempValue);
            stopEdit.SetValue(sdiIdentifier("Emax"), tempValue);

            tempValue = sdiValue(0.0);
            selectCtrlTerm->GetValue(sdiIdentifier("ENDMAS"), tempValue);
            stopEdit.SetValue(sdiIdentifier("DeltaMmax"), tempValue);
            stopEdit.SetValue(sdiIdentifier("DeltaNmax"), sdiValue(0.0));

            sdiConvert::Convert::PushToConversionLog(std::make_pair(stopHandleEdit, sourceCards));
        }
    }
}

void sdiD2R::ConvertCard::p_ConvertCtrlUnits()
{
    map<sdiString, sdiString> mapUnits;
    mapUnits["m"] = "m";
    mapUnits["mm"] = "mm";
    mapUnits["cm"] = "cm";
    mapUnits["inch"] = "0.0254";
    mapUnits["foot"] = "0.3048";

    mapUnits["s"] = "s";
    mapUnits["sec"] = "s";
    mapUnits["ms"] = "ms";
    mapUnits["micro_s"] = "mus";

    mapUnits["kg"] = "kg";
    mapUnits["g"] = "g";
    mapUnits["mg"] = "mg";
    mapUnits["mtrc_ton"] = "Mg";
    mapUnits["lb"] = "0.45359237";
    mapUnits["slug"] = "14.5939029372";
    mapUnits["slinch"] = "175.126835244";

    sdiString len  = "m";
    sdiString time = "sec";
    sdiString mass = "kg";

    HandleEdit beginHandleEdit;
    SDIHandlReadList sourceCards;
    p_radiossModel->CreateEntity(beginHandleEdit, "/BEGIN");

    if (beginHandleEdit.IsValid())
    {

        EntityEdit beginEdit(p_radiossModel, beginHandleEdit);

        //beginEdit.SetValue(sdiIdentifier("Runname"), sdiValue(string("")));
        beginEdit.SetValue(sdiIdentifier("Irun"), sdiValue(0));
        beginEdit.SetValue(sdiIdentifier("Invers"), sdiValue(2025));

        bool foundInMainFile = false;
        SelectionRead selCtrlUnits(p_lsdynaModel, "*CONTROL_UNITS");
        while (selCtrlUnits.Next() && foundInMainFile == false)
        {
            HandleRead include = selCtrlUnits->GetInclude();
            vector<reference_wrapper<sdiString>> attrValList = { len, time, mass };
            vector<sdiString> attrNameList = { "LENGTH","TIME", "MASS" };
            p_ConvertUtils.GetAttribValues(*selCtrlUnits, attrNameList, attrValList);
            sourceCards = { {selCtrlUnits->GetHandle()} };
            if(!include.IsValid()) foundInMainFile = true;
        }

        if (mapUnits[len].size() == 0) len = "m";
        if (mapUnits[time].size() == 0) time = "sec";
        if (mapUnits[mass].size() == 0) mass = "kg";

        beginEdit.SetValue(sdiIdentifier("Input_length_unit"), sdiValue(mapUnits[len]));
        beginEdit.SetValue(sdiIdentifier("Input_time_unit"), sdiValue(mapUnits[time]));
        beginEdit.SetValue(sdiIdentifier("Input_mass_unit"), sdiValue(mapUnits[mass]));


        beginEdit.SetValue(sdiIdentifier("Work_length_unit"), sdiValue(mapUnits[len]));
        beginEdit.SetValue(sdiIdentifier("Work_time_unit"), sdiValue(mapUnits[time]));
        beginEdit.SetValue(sdiIdentifier("Work_mass_unit"), sdiValue(mapUnits[mass]));

        sdiConvert::Convert::PushToConversionLog(std::make_pair(beginHandleEdit, sourceCards));
    }

}

void sdiD2R::ConvertCard::p_ConvertDbBinaryCards()
{
    SelectionRead selDbBin(p_lsdynaModel, "*DATABASE_BINARY");
    while (selDbBin.Next())
    {
        sdiString solverCard = selDbBin->GetKeyword();
        sdiString keyword = solverCard;
        sdiValue tempVal;
        HandleEdit radCard;

        if (keyword.find("RUNRSF") != keyword.npos || keyword.find("D3DUMP") != keyword.npos)
        {
            double lsdCYCL = 0.0;
            int lsdNR = 0;
            tempVal = sdiValue(lsdCYCL);
            selDbBin->GetValue(sdiIdentifier("LSD_DT"), tempVal);
            tempVal.GetValue(lsdCYCL);

            tempVal = sdiValue(lsdNR);
            selDbBin->GetValue(sdiIdentifier("LSD_NR"), tempVal);
            tempVal.GetValue(lsdNR);

            p_radiossModel->CreateEntity(radCard, "/RFILE");
            EntityEdit rfileEdit(p_radiossModel, radCard);

            rfileEdit.SetValue(sdiIdentifier("DEFAULT"), sdiValue(1));

            rfileEdit.SetValue(sdiIdentifier("RFILE_n"), sdiValue((lsdCYCL != 0.0) ? (int)lsdCYCL : (int)lsdNR));
        }
        else if (keyword.find("D3PLOT") != keyword.npos)
        {
            double lsdDtCycl = 0.0;
            tempVal = sdiValue(lsdDtCycl);
            selDbBin->GetValue(sdiIdentifier("LSD_DT"), tempVal);
            tempVal.GetValue(lsdDtCycl);

            p_radiossModel->CreateEntity(radCard, "/ANIM/DT");
            radCard.SetValue(p_radiossModel, sdiIdentifier("Tstart"), sdiValue(0.0));
            radCard.SetValue(p_radiossModel, sdiIdentifier("Tfreq"), sdiValue(lsdDtCycl));

            HandleEdit animVect;
            p_radiossModel->CreateEntity(animVect, "/ANIM/VECT");
            animVect.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_VECT_DISP"), sdiValue(1));
        }
        else if (keyword.find("INTFOR") != keyword.npos)
        {
            SelectionEdit selAnimDT(p_radiossModel, "/ANIM/DT");
            if (!selAnimDT.Count())
            {
                double lsdDt = 0.0;
                tempVal = sdiValue(lsdDt);
                selDbBin->GetValue(sdiIdentifier("DT"), tempVal);
                tempVal.GetValue(lsdDt);

                p_radiossModel->CreateEntity(radCard, "/ANIM/DT");
                radCard.SetValue(p_radiossModel, sdiIdentifier("Tstart"), sdiValue(0.0));
                radCard.SetValue(p_radiossModel, sdiIdentifier("Tfreq"), sdiValue(lsdDt));
            }
            p_radiossModel->CreateEntity(radCard, "/ANIM/VECT");

            radCard.SetValue(p_radiossModel, sdiIdentifier("CONT"), sdiValue(1));
            radCard.SetValue(p_radiossModel, sdiIdentifier("CONT2"), sdiValue(1));
            radCard.SetValue(p_radiossModel, sdiIdentifier("PCONT"), sdiValue(1));
            radCard.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_VECT_DISP"), sdiValue(1));
        }
        if (radCard.IsValid())
        {
            SDIHandlReadList sourceCards = { {selDbBin->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radCard, sourceCards));
        }
    }

}

void sdiD2R::ConvertCard::p_ConvertDbExtentBinaryCards()
{
    SelectionRead selDbExtBinComp(p_lsdynaModel, "*DATABASE_EXTENT_BINARY_COMP");
    if (selDbExtBinComp.Count() > 0)
    {
        while (selDbExtBinComp.Next())
        {
            int lsdIVEL;
            int lsdIACC;
            int lsdISTRS;
            int lsdISTRA;
            int lsdISED;

            HandleEdit animElem;
            HandleEdit animVect;
            HandleEdit brickTens;
            HandleEdit shellEpsp;
            HandleEdit shellTensStrain;
            HandleEdit shellTensStress;

            vector<sdiString> attribNames({ { "IVEL", "IACC", "ISTRS", "ISTRA", "ISED" } });
            vector<reference_wrapper<int>> attrVals({ { lsdIVEL, lsdIACC, lsdISTRS, lsdISTRA, lsdISED } });
            p_ConvertUtils.GetAttribValues(*selDbExtBinComp, attribNames, attrVals);

            if (lsdIVEL == 1)
            {
                if (!animVect.IsValid())
                    p_radiossModel->CreateEntity(animVect, "/ANIM/VECT");

                animVect.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_VECT_VEL"), sdiValue(1));
            }

            if (lsdIACC == 1)
            {
                if (!animVect.IsValid())
                    p_radiossModel->CreateEntity(animVect, "/ANIM/VECT");

                animVect.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_VECT_ACC"), sdiValue(1));
            }

            if (lsdISTRS == 1)
            {
                if (!shellTensStress.IsValid())
                    p_radiossModel->CreateEntity(shellTensStress, "/ANIM/SHELL/TENS/STRESS");

                shellTensStress.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHELL_TENS_STRESS_MEMB"), sdiValue(1));
                shellTensStress.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHEL_TENS_STRES_UPPER"), sdiValue(1));
                shellTensStress.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHEL_TENS_STRES_LOWER"), sdiValue(1));

                if (!brickTens.IsValid())
                    p_radiossModel->CreateEntity(brickTens, "/ANIM/BRICK/TENS");

                brickTens.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_BRICK_TENS_Type_STRESS"), sdiValue(1));

                if (!animElem.IsValid())
                    p_radiossModel->CreateEntity(animElem, "/ANIM/ELEM");

                animElem.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_ELEM_EPSP"), sdiValue(1));

                if (!shellEpsp.IsValid())
                    p_radiossModel->CreateEntity(shellEpsp, "/ANIM/SHELL/EPSP");

                shellEpsp.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHELL_EPSP_UPPER"), sdiValue(1));
                shellEpsp.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHELL_EPSP_LOWER"), sdiValue(1));
            }

            if (lsdISTRA == 1)
            {
                if (!shellTensStrain.IsValid())
                    p_radiossModel->CreateEntity(shellTensStrain, "/ANIM/SHELL/TENS/STRAIN");

                shellTensStrain.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHELL_TENS_STRAIN_MEMB"), sdiValue(1));
                shellTensStrain.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHELL_TENS_STRAIN_UPR"), sdiValue(1));
                shellTensStrain.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHELL_TENS_STRAIN_LWR"), sdiValue(1));

                if (!brickTens.IsValid())
                    p_radiossModel->CreateEntity(brickTens, "/ANIM/BRICK/TENS");

                brickTens.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_BRICK_TENS_Type_STRAIN"), sdiValue(1));
            }

            if (lsdISED == 1)
            {
                if (!animElem.IsValid())
                    p_radiossModel->CreateEntity(animElem, "/ANIM/ELEM");

                animElem.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_ELEM_ENERGY"), sdiValue(1));
            }

            sdiConvert::SDIHandlReadList sourceControlCards = { {selDbExtBinComp->GetHandle()} };
            for (HandleEdit tempHEdit : {shellTensStrain, brickTens, animElem, animVect, shellEpsp, shellTensStress })
            {
                if (tempHEdit.IsValid())
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(tempHEdit, sourceControlCards));
            }
        }
    }
    else
    {
        SelectionRead selDbExtBin(p_lsdynaModel, "*DATABASE_EXTENT_BINARY");
        while (selDbExtBin.Next())
        {
            int lsdSTRFLG;
            int lsdSIGFLG;
            int lsdEPSFLG;
            int lsdRLTFLG;
            int lsdENGFLG;
            int lsdDCOMP;
            int lsdSHGE;
            int lsdSTSSZ;
            int lsdPKP_SEN;
            int lsdMSSCL;
            int lsdTHERM;
            int lsdDTD;
            sdiString lsdNODOUT;
            HandleEdit animElem;
            HandleEdit animGzip;
            HandleEdit animNoda;
            HandleEdit animGps;
            HandleEdit animVect;
            HandleEdit brickTens;
            HandleEdit shellEpsp;
            HandleEdit shellTensStrain;
            HandleEdit shellTensStress;
            sdiValue tempVal;

            vector<sdiString> attribNames({ {"STRFLG", "SIGFLG", "EPSFLG", "RLTFLG", "ENGFLG", "DCOMP", "SHGE", "STSSZ", "PKP_SEN", "MSSCL", "THERM", "DTDT" } });
            vector<reference_wrapper<int>> attrVals({ { lsdSTRFLG, lsdSIGFLG, lsdEPSFLG, lsdRLTFLG, lsdENGFLG, lsdDCOMP, lsdSHGE, lsdSTSSZ, lsdPKP_SEN, lsdMSSCL, lsdTHERM, lsdDTD } });
            p_ConvertUtils.GetAttribValues(*selDbExtBin, attribNames, attrVals);

            if (lsdSTRFLG)
            {
                int LVal = lsdSTRFLG % 10;
                if (LVal == 1)
                {
                    if (!shellTensStrain.IsValid())
                        p_radiossModel->CreateEntity(shellTensStrain, "/ANIM/SHELL/TENS/STRAIN");

                    shellTensStrain.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHELL_TENS_STRAIN_MEMB"), sdiValue(1));
                    shellTensStrain.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHELL_TENS_STRAIN_UPR"), sdiValue(1));
                    shellTensStrain.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHELL_TENS_STRAIN_LWR"), sdiValue(1));

                    if (!brickTens.IsValid())
                        p_radiossModel->CreateEntity(brickTens, "/ANIM/BRICK/TENS");

                    brickTens.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_BRICK_TENS_Type_STRAIN"), sdiValue(1));
                }

                int MVal = lsdSTRFLG % 100 - LVal;
                if (MVal == 1)
                {
                    if (!animElem.IsValid())
                        p_radiossModel->CreateEntity(animElem, "/ANIM/ELEM");

                    animElem.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_ELEM_EPSP"), sdiValue(1));

                    if (!shellEpsp.IsValid())
                        p_radiossModel->CreateEntity(shellEpsp, "/ANIM/SHELL/EPSP");

                    shellEpsp.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHELL_EPSP_UPPER"), sdiValue(1));
                    shellEpsp.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHELL_EPSP_LOWER"), sdiValue(1));
                }
            }

            if (lsdSIGFLG == 0 || lsdSIGFLG == 1)
            {
                if (!shellTensStress.IsValid())
                    p_radiossModel->CreateEntity(shellTensStress, "/ANIM/SHELL/TENS/STRESS");

                shellTensStress.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHELL_TENS_STRESS_MEMB"), sdiValue(1));
                shellTensStress.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHEL_TENS_STRES_UPPER"), sdiValue(1));
                shellTensStress.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHEL_TENS_STRES_LOWER"), sdiValue(1));

                if (!brickTens.IsValid())
                    p_radiossModel->CreateEntity(brickTens, "/ANIM/BRICK/TENS");

                brickTens.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_BRICK_TENS_Type_STRESS"), sdiValue(1));
            }
            else if (lsdSIGFLG == 2)
            {
                if (!brickTens.IsValid())
                    p_radiossModel->CreateEntity(brickTens, "/ANIM/BRICK/TENS");

                brickTens.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_BRICK_TENS_Type_STRESS"), sdiValue(1));
            }

            if (lsdEPSFLG == 0 || lsdEPSFLG == 1)
            {
                if (!animElem.IsValid())
                    p_radiossModel->CreateEntity(animElem, "/ANIM/ELEM");

                animElem.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_ELEM_EPSP"), sdiValue(1));

                if (!shellEpsp.IsValid())
                    p_radiossModel->CreateEntity(shellEpsp, "/ANIM/SHELL/EPSP");

                shellEpsp.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHELL_EPSP_UPPER"), sdiValue(1));
                shellEpsp.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHELL_EPSP_LOWER"), sdiValue(1));
            }

            if (lsdRLTFLG == 0 || lsdRLTFLG == 1)
            {
                if (!animElem.IsValid())
                    p_radiossModel->CreateEntity(animElem, "/ANIM/ELEM");

                animElem.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_ELEM_VONM"), sdiValue(1));
            }

            if (lsdENGFLG == 0 || lsdENGFLG == 1)
            {
                if (!animElem.IsValid())
                    p_radiossModel->CreateEntity(animElem, "/ANIM/ELEM");

                animElem.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_ELEM_ENERGY"), sdiValue(1));
            }

            if (lsdDCOMP > 1)
            {
                if (!animGzip.IsValid())
                    p_radiossModel->CreateEntity(animGzip, "/ANIM/GZIP");
            }

            if (lsdSHGE == 2)
            {
                if (!animElem.IsValid())
                    p_radiossModel->CreateEntity(animElem, "/ANIM/ELEM");

                animElem.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_ELEM_HOURG"), sdiValue(1));
            }

            if (lsdSTSSZ == 2 || lsdSTSSZ == 3)
            {
                if (!animNoda.IsValid())
                    p_radiossModel->CreateEntity(animNoda, "/ANIM/NODA");

                animNoda.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_NODA_DT"), sdiValue(1));

                if (lsdSTSSZ == 3)
                    animNoda.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_NODA_DMAS"), sdiValue(1));
            }

            if (lsdPKP_SEN == 1)
            {
                if (!animVect.IsValid())
                    p_radiossModel->CreateEntity(animVect, "/ANIM/VECT");

                animVect.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_VECT_CONT"), sdiValue(1));
                animVect.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_VECT_CONT2"), sdiValue(1));
                animVect.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_VECT_PCONT"), sdiValue(1));
            }

            if (lsdMSSCL != 0)
            {
                if (!animNoda.IsValid())
                    p_radiossModel->CreateEntity(animNoda, "/ANIM/NODA");

                animNoda.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_NODA_DMAS"), sdiValue(1));
            }

            if (lsdTHERM != 0)
            {
                if (!animNoda.IsValid())
                    p_radiossModel->CreateEntity(animNoda, "/ANIM/NODA");

                animNoda.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_NODA_TEMP"), sdiValue(1));

                if (!animElem.IsValid())
                    p_radiossModel->CreateEntity(animElem, "/ANIM/ELEM");

                animElem.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_ELEM_TEMP"), sdiValue(1));
            }

            tempVal = sdiValue(lsdNODOUT);
            selDbExtBin->GetValue(sdiIdentifier("NODOUT"), tempVal);
            tempVal.GetValue(lsdNODOUT);
            if (lsdNODOUT == "STRESS" || lsdNODOUT == "STRESS_GL")
            {
                /* if (!animGps.IsValid())
                {
                    p_radiossModel->CreateEntity(animGps, "/ANIM/GPS");
                } */
            }
            else if (lsdNODOUT == "STRAIN" || lsdNODOUT == "STRAIN_GL")
            {
                /* if (!animGps.IsValid())
                {
                    p_radiossModel->CreateEntity(animGps, "/ANIM/GPS");
                } */
            }
            else if (lsdNODOUT == "ALL")
            {
                /* if (!animGps.IsValid())
                {
                    p_radiossModel->CreateEntity(animGps, "/ANIM/GPS");
                } */
            }

            if (lsdDTD == 1)
            {
                if (!animNoda.IsValid())
                    p_radiossModel->CreateEntity(animNoda, "/ANIM/NODA");

                animNoda.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_NODA_DT"), sdiValue(1));
                animNoda.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_NODA_TEMP"), sdiValue(1));
            }

            sdiConvert::SDIHandlReadList sourceControlCards = { {selDbExtBin->GetHandle()} };
            for (HandleEdit tempHEdit : {shellTensStrain, brickTens, animElem, animGzip, animNoda, animGps,
                shellEpsp, shellTensStress })
            {
                if (tempHEdit.IsValid())
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(tempHEdit, sourceControlCards));
            }
        } 
    }
}

void sdiD2R::ConvertCard::p_ConvertDbBinaryD3Plot()
{
    HandleEdit animVect;
    HandleEdit animShellStress;
    HandleEdit animBrickTens;
    HandleEdit animElem;
    HandleEdit animShellEPSP;

    SelectionRead selDbBinD3Plot(p_lsdynaModel, "*DATABASE_BINARY_D3PLOT");
    while (selDbBinD3Plot.Next())
    {
        sdiConvert::SDIHandlReadList sourceControlCards = { {selDbBinD3Plot->GetHandle()} };
        p_radiossModel->CreateEntity(animVect, "/ANIM/VECT");
        animVect.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_VECT_ACC"), sdiValue(1));
        animVect.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_VECT_VEL"), sdiValue(1));
        sdiConvert::Convert::PushToConversionLog(std::make_pair(animVect, sourceControlCards));

        SelectionEdit selAnimShellStress(p_radiossModel, "/ANIM/SHELL/TENS/STRESS");
        if (selAnimShellStress.Count() == 0)
        {
            p_radiossModel->CreateEntity(animShellStress, "/ANIM/SHELL/TENS/STRESS");
            animShellStress.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHEL_TENS_STRES_UPPER"), sdiValue(1));
            animShellStress.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHEL_TENS_STRES_LOWER"), sdiValue(1));
            sdiConvert::Convert::PushToConversionLog(std::make_pair(animShellStress, sourceControlCards));
        }
        else
        {
            /* TODO GetHandle() on selection. Set the values on the HandleEdit */
            selAnimShellStress->SetValue(sdiIdentifier("ENG_ANIM_SHEL_TENS_STRES_UPPER"), sdiValue(1));
            selAnimShellStress->SetValue(sdiIdentifier("ENG_ANIM_SHEL_TENS_STRES_LOWER"), sdiValue(1));
        }

        SelectionEdit selBrickTens(p_radiossModel, "/ANIM/BRICK/TENS");
        if (selBrickTens.Count() == 0)
        {
            p_radiossModel->CreateEntity(animBrickTens, "/ANIM/BRICK/TENS");
            sdiConvert::Convert::PushToConversionLog(std::make_pair(animBrickTens, sourceControlCards));
            animBrickTens.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_BRICK_TENS_Type_STRESS"), sdiValue(1));
        }
        else
        {
            /* TODO GetHandle() on selection. Set the values on the HandleEdit */
            selBrickTens->SetValue(sdiIdentifier("ENG_ANIM_BRICK_TENS_Type_STRESS"), sdiValue(1));
        }

        SelectionEdit selAnimElem(p_radiossModel, "/ANIM/ELEM");
        if (selAnimElem.Count() == 0)
        {
            p_radiossModel->CreateEntity(animElem, "/ANIM/ELEM");
            animElem.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_ELEM_EPSP"), sdiValue(1));
            animElem.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_ELEM_ENERGY"), sdiValue(1));
            sdiConvert::Convert::PushToConversionLog(std::make_pair(animElem, sourceControlCards));
        }
        else
        {
            /* TODO GetHandle() on selection. Set the values on the HandleEdit */
            selAnimElem->SetValue(sdiIdentifier("ENG_ANIM_ELEM_EPSP"), sdiValue(1));
            selAnimElem->SetValue(sdiIdentifier("ENG_ANIM_ELEM_ENERGY"), sdiValue(1));
        }

        SelectionEdit selAnimShell(p_radiossModel, "/ANIM/SHELL/EPSP");
        if (selAnimShell.Count() == 0)
        {
            p_radiossModel->CreateEntity(animShellEPSP, "/ANIM/SHELL/EPSP");
            animShellEPSP.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHELL_EPSP_UPPER"), sdiValue(1));
            animShellEPSP.SetValue(p_radiossModel, sdiIdentifier("ENG_ANIM_SHELL_EPSP_LOWER"), sdiValue(1));
            sdiConvert::Convert::PushToConversionLog(std::make_pair(animShellEPSP, sourceControlCards));
        }
        else
        {
            /* TODO GetHandle() on selection. Set the values on the HandleEdit */
            selAnimShell->SetValue(sdiIdentifier("ENG_ANIM_SHELL_EPSP_UPPER"), sdiValue(1));
            selAnimShell->SetValue(sdiIdentifier("ENG_ANIM_SHELL_EPSP_LOWER"), sdiValue(1));
        }
    }
}

void sdiD2R::ConvertCard::p_ConvertControlOutput()
{
    SelectionRead selCntOut(p_lsdynaModel, "*CONTROL_OUTPUT");
    while (selCntOut.Next())
    {
        int npOpt = 0;
        sdiValue tempVal(npOpt);
        selCntOut->GetValue(sdiIdentifier("NPOPT"), tempVal);
        tempVal.GetValue(npOpt);
        int ipri = 0;
        switch (npOpt)
        {
        case 11:
            ipri = 1;
            break;
        case 12:
            ipri = 2;
            break;
        case 13:
            ipri = 3;
            break;
        case 14:
            ipri = 4;
            break;
        case 15:
            ipri = 5;
            break;
        default:
            break;
        }
        HandleEdit ioflagHEdit;
        p_radiossModel->CreateEntity(ioflagHEdit, "/IOFLAG");
        if (ioflagHEdit.IsValid())
        {
            ioflagHEdit.SetValue(p_radiossModel, sdiIdentifier("Ipri"), sdiValue(ipri));

            SDIHandlReadList sourceCards = { {selCntOut->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(ioflagHEdit, sourceCards));
        }
    }
}

void sdiD2R::ConvertCard::p_ConvertControlParallel()
{
    HandleEdit parithHEdit;
    p_radiossModel->CreateEntity(parithHEdit, "/PARITH");
    parithHEdit.SetValue(p_radiossModel, sdiIdentifier("KEYWORD2"), sdiValue(sdiString("OFF")));

    int lsdConst = 0;
    SelectionRead selCntParallel(p_lsdynaModel, "*CONTROL_PARALLEL");
    while (selCntParallel.Next())
    {
        int lsdConstTmp = 0;
        sdiValue tempVal(lsdConst);
        selCntParallel->GetValue(sdiIdentifier("CONST"), tempVal);
        tempVal.GetValue(lsdConstTmp);
        if (lsdConstTmp == 1) 
            lsdConst = 1;
    }
    if (lsdConst == 1)
    {
        parithHEdit.SetValue(p_radiossModel, sdiIdentifier("KEYWORD2"), sdiValue(sdiString("ON")));
    }
}

void sdiD2R::ConvertCard::p_ConvertDbNodalForceGroup()
{
    SelectionRead seldbNodalForce(p_lsdynaModel, "*DATABASE_NODAL_FORCE_GROUP");

    while (seldbNodalForce.Next())
    {
        sdiValueEntity nsidEntity = GetValue<sdiValueEntity>(*seldbNodalForce, "NSID");
        unsigned int NSID = nsidEntity.GetId();

        sdiString optionName = seldbNodalForce->GetName();
        sdiString keyWord = seldbNodalForce->GetKeyword();
        sdiString TITLE;

        if(keyWord.find("DATABASE_NODAL_FORCE_GROUP_TITLE") != keyWord.npos)
            TITLE = optionName;
        else
            TITLE = "DATABASE NODAL FORCE GROUP NSET "+to_string(nsidEntity.GetId());

        sdiValueEntity cidEntity = GetValue<sdiValueEntity>(*seldbNodalForce, "CID");
        unsigned int CID = cidEntity.GetId();

        // create /TH/NODE card

        HandleEdit thHandleEdit;
        if (NSID)
        {
            sdiUIntList NSIDList;
            sdiUIntList allExtratedNodes;
            sdiUIntList skewList;
            HandleRead NsidHRead;
            p_radiossModel->FindById(p_radiossModel->GetEntityType("/SET/GENERAL"), NSID, NsidHRead);
            p_ConvertUtils.ExtractNodesFromRadiossSet(NsidHRead, NSIDList);
            if (!NSIDList.empty())
            {
                allExtratedNodes.reserve((int)NSIDList.size());
                skewList.reserve((int)NSIDList.size());
                for (unsigned int nodeId : NSIDList)
                {
                    allExtratedNodes.push_back(nodeId);
                    skewList.push_back(CID);
                }
            }

            int idArrSIze = (int)allExtratedNodes.size();
            if (idArrSIze)
            {
                p_radiossModel->CreateEntity(thHandleEdit, "/TH/NODE", TITLE);
                EntityEdit  thEdit(p_radiossModel, thHandleEdit);
                thEdit.SetValue(sdiIdentifier("idsmax"), sdiValue(idArrSIze));
                thEdit.SetValue(sdiIdentifier("Number_Of_Variables"), sdiValue(1));
                thEdit.SetValue(sdiIdentifier("elem_ID"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/NODE"), allExtratedNodes)));
                thEdit.SetValue(sdiIdentifier("skew_ID"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/SKEW"), skewList)));
                thEdit.SetValue(sdiIdentifier("var"), sdiValue(sdiStringList({ "DEF" })));
        
                sdiConvert::SDIHandlReadList sourceHandles = { {seldbNodalForce->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(thHandleEdit, sourceHandles));
            }
        }
    }
}