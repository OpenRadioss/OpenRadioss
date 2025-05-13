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
#include <dyna2rad/dynamatlawkeywordmap.h>
#include <dyna2rad/convertmats.h>
#include <dyna2rad/sdiUtils.h>
#include <typedef.h>

using namespace sdiD2R;
using namespace std;
using namespace sdi;
using namespace sdiConvert;

static size_t matToPropsReferenceCount = 0;

static void UpdateMatConvertingLoadCurves                       (const EntityRead& dynaMat,
                                                                ModelViewRead* lsdModel,
                                                                ModelViewEdit* radModel,
                                                                ConvertUtils& p_ConvertUtils,
                                                                EntityEdit& radmatEntityEdit,
                                                                int& lsdLCAoption);

static void RecomputeCurvesBasedOnFirstAbcissa(                        ModelViewRead* lsdModel,
                                                                const EntityRead dynaMat,
                                                                const ConvertUtils& convertUtils,
                                                                const vector<HandleRead>& curveHandleList,
                                                                map<EntityId, pair<bool, sdiDoubleList>>& containCrvIdVsCrvPnts);

static void UpdateShearLoadCurves                               (ModelViewRead* lsdModel,
                                                                const ConvertUtils& convertUtils,
                                                                map<EntityId, pair<bool, sdiDoubleList>>& containCrvIdVsCrvPnts,
                                                                const HandleRead& handleLCAB,
                                                                const HandleRead& handleLCBC,
                                                                const HandleRead& handleLCCA);

static void ConvertPlasticDispPointsTotalDisp                   (const double& stiff,
                                                                 sdiDoubleList& crvPoints);

static void ComputeCurvePtsForMatL240                                  (const double stressMode,
                                                                 const double logModel,
                                                                 const double cohesiveThick,
                                                                 const double strainRate,
                                                                 const sdiDoubleList& strainList,
                                                                 sdiDoubleList& tempList,
                                                                 sdiDoubleList& pointsList);

static void ComputeInitialYldCurveForMatL240                           (const double energyRelRate,
                                                                 const double upperEnergyRelRate,
                                                                 const double strainRate,
                                                                 const double cohesiveThick,
                                                                 const double fgVal,
                                                                 const double modVal,
                                                                 const sdiDoubleList strainList,
                                                                 const sdiDoubleList& tempList,
                                                                 sdiDoubleList& GList,
                                                                 sdiDoubleList& pointsList);

static void ComputeRuptureYldCurveForMAtL240                           (const double cohesiveThick, 
                                                                 const double fgVal, 
                                                                 const double modVal, 
                                                                 const sdiDoubleList& strainList,
                                                                 const sdiDoubleList& GList, 
                                                                 const sdiDoubleList& tempList, 
                                                                 sdiDoubleList& pointsList);

void ConvertMat::ConvertMaterials()
{
    ConvertEntities();
}

void ConvertMat::ConvertEntities()
{
    p_ConvertAllMatsAssociatedWithParts();
    p_ConvertMatAddErosion();
    p_ConvertMatAddDamageDiem();
    p_ConvertMatAddThermalExpansion();
    p_ConvertMatAddDamageGissmo();
}
void ConvertMat::p_ConvertAllMatsAssociatedWithParts()
{
    SelectionRead partsSelect(p_lsdynaModel, "*PART");
    while (partsSelect.Next())
    {
        sdiString partName = partsSelect->GetName();
        EntityId partId = partsSelect->GetId();

        HandleEdit radEditPart;
        EntityType radPartEntityType = p_radiossModel->GetEntityType("/PART");
        p_radiossModel->FindById(radPartEntityType, partId, radEditPart);

        p_PartBeingConverted = partsSelect->GetHandle();

        HandleRead partHRead;
        EntityRead partEntRead(p_lsdynaModel, p_PartBeingConverted);
        sdiString partCard = partEntRead.GetKeyword();

        if (radEditPart.IsValid())
        {
            HandleRead matHRead;
            if( partCard.find("COMPOSITE") != string::npos)
            {
                partsSelect->GetEntityHandle(sdiIdentifier("MID",0,0), matHRead);
            }
            else
            {
                partsSelect->GetEntityHandle(sdiIdentifier("MID"), matHRead);
            }

            HandleRead lsdEosHRead;
            partsSelect->GetEntityHandle(sdiIdentifier("EOSID"), lsdEosHRead);

            HandleEdit radMatEdit;
            if (matHRead.IsValid())
            {
                EntityRead matEntRead(p_lsdynaModel, matHRead);
                p_ConvertUtils.PropagateInclude(matEntRead);
                p_ConvertMatBasedOnCard(matEntRead, radMatEdit, lsdEosHRead);
            }
            if (radMatEdit.IsValid())
                radEditPart.SetEntityHandle(p_radiossModel, sdiIdentifier("mat_ID"), radMatEdit);
            else
                radEditPart.SetValue(p_radiossModel, sdiIdentifier("mat_ID"), sdiValue(sdiValueEntity(sdiValueEntityType(destEntityType), 0)));
            
            if( partCard.find("COMPOSITE") != string::npos)
            {
                int lsdNbPlies;
                sdiValue tempValue = sdiValue(lsdNbPlies);
                partEntRead.GetValue(sdiIdentifier("Number_of_Plies"), tempValue);
                tempValue.GetValue(lsdNbPlies); 

                for (int i = 1; i < lsdNbPlies; ++i)
                { 
                    HandleRead matHRead;
                    partsSelect->GetEntityHandle(sdiIdentifier("MID",0,i), matHRead);
                    HandleEdit radMatEdit;
                    if (matHRead.IsValid())
                    {
                        EntityRead matEntRead(p_lsdynaModel, matHRead);
                        p_ConvertUtils.PropagateInclude(matEntRead);
                        p_ConvertMatBasedOnCard(matEntRead, radMatEdit, lsdEosHRead);
                    }
                }
            }
        }
    }
}

void ConvertMat::p_ConvertMatBasedOnCard(const EntityRead& dynaMat, HandleEdit& radMat, HandleRead& lsdEosHRead)
{
    EntityId dynaMatId   = dynaMat.GetId();
    sdiString dynaMatName = dynaMat.GetName();
    sdiString sourceCard  = dynaMat.GetKeyword();
    SDIEntityReadList matToPropReferences;

    p_MatPropRelationDB.GetReferenceList(srcEntityType, dynaMatId, matToPropReferences);
    matToPropsReferenceCount = matToPropReferences.size();
    sdiVectorSort(p_convertedMats);

    HandleRead propHandle;
    p_PartBeingConverted.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHandle);
    EntityRead propRead(p_lsdynaModel, propHandle);
    if (mapMatIdVsMapPropIdNewMatHandle.find(dynaMatId) != mapMatIdVsMapPropIdNewMatHandle.end() &&
        mapMatIdVsMapPropIdNewMatHandle[dynaMatId].find(propRead.GetId()) != mapMatIdVsMapPropIdNewMatHandle[dynaMatId].end())
    {
        radMat = mapMatIdVsMapPropIdNewMatHandle[dynaMatId][propRead.GetId()];
        return;
    }
    else if (binary_search(p_convertedMats.begin(), p_convertedMats.end(), dynaMatId))
    {
        p_radiossModel->FindById(destEntityType, dynaMatId, radMat);
        return;
    }
    else
    {
        size_t pos = sourceCard.find("_TITLE");
        if (pos != string::npos)
        {
            sourceCard.erase(pos);
        }
        unsigned short int matLawNum = dynaMatLawMap[sourceCard];
        unsigned short int matLawNumChoice = 0;
        multimap<string, string> attribMap;
        map<string, double> attribDoubleValueMap;
        map<string, int> attribintValueMap;
        sdiString destCard;

        sdiString propKeyWord = propRead.GetKeyword();
        bool skipDuplicatingMat = false;

        int elform = GetValue<int>(propRead, "ELFORM");

        switch (matLawNum) 
        {
        case 1:
            if (sourceCard.find("FLUID") != sourceCard.npos)
            {
                destCard = "/MAT/LAW6";
                p_ConvertMatL1_FLUID(dynaMat, destCard, radMat);
                break;
            }
        case 20:
            if (propKeyWord.find("BEAM") != propKeyWord.npos)
            {
                destCard = "/MAT/LAW2";
                attribDoubleValueMap = { {"a", 1E20} };
                attribMap = { {"E","E"}, {"PR","Nu"}, {"RHO", "RHO_I"} };
                attribintValueMap["LAW2_ID"] = 1;
            }
            else
            {
                attribMap = { {"E", "E"}, {"PR","nu"}, {"RHO", "RHO_I"} };
                destCard = "/MAT/LAW1";
            }
            break;
        case 2:
        /*{
            destCard = "/MAT/COMPSH";
            attribMap = { {"RHO","RHO_I"}, {"EA","E11"}, {"EB","E22"}, {"EC", "E33"}, {"PRBA","NU12"},
                          {"LSDYNA_GAB","G12"}, {"LSDYNA_GBC","G23"}, {"LSDYNA_GCA", "G31"} };
            attribDoubleValueMap = { {"sig_1yt", 1E20}, { "sig_2yt", 1E20 }, { "sig_1yc", 1E20 }, {"sig_2yc", 1E20}, {"sig_12yc", 1E20}, { "sig_12yt", 1E20 } };
            break;
        }*/
            p_ConvertMatL2(dynaMat, destCard, attribMap, radMat);
            break;
        case 3:
            p_ConvertMatL3(dynaMat, destCard, attribMap, radMat);
            break;
        case 5:
            p_ConvertMatL5(dynaMat, destCard, attribMap, radMat);
            break;
        case 6:
        {
            p_ConvertMatL6(dynaMat, destCard, attribMap, radMat);
            break;
        }
        case 7:
        {
            destCard = "/MAT/LAW42";
            attribMap = { {"Rho", "RHO_I"}, {"G", "Mu_1"} };
            attribDoubleValueMap = { {"Nu", 0.463}, {"alpha_1", 2} };
            double ref = 0.0;
            sdiValue tempVal(ref);
            dynaMat.GetValue(sdiIdentifier("REF"), tempVal);
            tempVal.GetValue(ref);
            if (1.0 == ref)
            {
                HandleEdit xrefHandle;
                p_radiossModel->CreateEntity(xrefHandle, "/XREF", dynaMat.GetName());
                xrefHandle.SetValue(p_radiossModel, sdiIdentifier("Comp_Id"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/PART"), p_PartBeingConverted.GetId(p_lsdynaModel))));
                xrefHandle.SetValue(p_radiossModel, sdiIdentifier("Nitrs"), sdiValue(100));
            }
            break;
        }
        case 9:
            if(lsdEosHRead.IsValid())
            {
                destCard = "/MAT/LAW6";
                attribMap = { {"RHO", "RHO_I"}, {"PC","Pmin"} };
            }
            else if(propKeyWord.find("BEAM") != propKeyWord.npos && elform == 6)
            {
                // springs only
                    destCard = "/MAT/LAW108";
                    attribMap = { {"RHO", "RHO_I"} };
            }
            else
            {
                destCard = "/MAT/VOID";
                attribMap = { {"E","E"}, {"RHO", "Init.dens."}, {"PR","Nu"} };
            }
            if (matToPropsReferenceCount > 1)
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
            else
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);
            if(lsdEosHRead.IsValid())
            {
                EntityEdit radmatEntityEdit(p_radiossModel, radMat);
                p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "MU/RHO", "Nu");
            }
            else
            {
                radMat.SetValue(p_radiossModel, sdiIdentifier("Mat_RhoEOption"), sdiValue(1));
            }
            break;
        case 12:
        {
            destCard = "/MAT/PLAS_JOHNS";
            if (matToPropsReferenceCount > 1)
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
            else
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);
            attribMap = { {"RHO", "RHO_I"} };
            EntityEdit radmatEntityEdit(p_radiossModel, radMat);
            p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "9*BULK*G/(3*BULK + G)", "E");
            p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "(3*BULK - 2*G)/(6*BULK + 2*G)", "Nu");
            p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "SIGY", "a");
            p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "ETAN", "b");
            radmatEntityEdit.SetValue(sdiIdentifier("n"), sdiValue(1));
            break;
        }
        case 15:
            if(lsdEosHRead.IsValid())
            {
                p_ConvertMatL15EOS(dynaMat, destCard, attribMap, radMat);
            }
            else
            {
                p_ConvertMatL15(dynaMat, destCard, attribMap, radMat);
            }
            break;
        case 18:
            attribMap = { {"RHO","Init.dens."}, {"E","E"}, {"PR","nu"}, {"SRC","C"}, {"SRP","P"} };
            p_ConvertMatL18(dynaMat, destCard, radMat);
            break;
        case 19:
            p_ConvertMatL19(dynaMat, destCard, attribMap, radMat);
            break;
        case 24:
            p_ConvertMatL24(dynaMat, destCard, attribMap, radMat, matLawNum, matLawNumChoice);
            break;
        case 26:
            destCard = "/MAT/LAW50";
            attribMap = { {"RO", "RHO_I"}, {"TSEF", "Eps_max11"}, {"TSEF","Eps_max22"}, {"TSEF", "Eps_max33"}, {"SSEF" , "Eps_max12"},
                          {"SSEF" , "Eps_max23"}, {"SSEF" , "Eps_max31"} };
            p_ConvertMatL26(dynaMat, radMat);
            break;
        case 27:
            p_ConvertMatL27(dynaMat, destCard, attribMap, radMat);
            break;
        case 30:
            p_ConvertMatL30(dynaMat, destCard, attribMap, radMat);
            break;
        case 32:
            p_ConvertMatL32(dynaMat, destCard, attribMap, radMat);
            break;
        case 34:
            p_ConvertMatL34(dynaMat, destCard, attribMap, radMat);
            break;
        case 54:
            p_ConvertMatL54(dynaMat, destCard, attribMap, radMat);
            break;
        case 57:
            p_ConvertMatL57(dynaMat, destCard, attribMap, radMat);
            break;
        case 58:
            p_ConvertMatL58(dynaMat, destCard, attribMap, radMat);
            break;
        case 61:
            p_ConvertMatL61(dynaMat, destCard, attribMap, radMat);
            break;
        case 63:
            p_ConvertMatL63(dynaMat, destCard, attribMap, radMat);
            break;
        case 66:
            p_ConvertMatL66(dynaMat, destCard, attribMap, radMat);
            break;
        case 67:
            p_ConvertMatL67(dynaMat, destCard, attribMap, radMat);
            break;
        case 68:
            p_ConvertMatL68(dynaMat, destCard, attribMap, radMat);
            break;
        case 71:
            p_ConvertMatL71(dynaMat, destCard, attribMap, radMat);
            break;
        case 73:
            p_ConvertMatL73(dynaMat, destCard, attribMap, radMat);
            break;
        case 74:
            p_ConvertMatL74(dynaMat, destCard, attribMap, radMat);
            break;
        case 76:
            p_ConvertMatL76(dynaMat, destCard, attribMap, radMat);
            break;
        case 7701:
            p_ConvertMatL77(dynaMat, destCard, attribMap, radMat);
            break;
        case 77:
            p_ConvertMatL77H(dynaMat, destCard, attribMap, radMat);
            break;
        case 81:
            p_ConvertMatL81(dynaMat, destCard, attribMap, radMat, matLawNum, matLawNumChoice);
            break;
        case 83:
            p_ConvertMatL83(dynaMat, destCard, attribMap, radMat);
            break;
        case 91:
            p_ConvertMatL91_92(dynaMat, destCard, attribMap, radMat);
            break;
        case 98:
            destCard = "/MAT/PLAS_JOHNS";
            if (matToPropsReferenceCount > 1)
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
            else
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);
            radMat.SetValue(p_radiossModel, sdiIdentifier("LAW2_ID"), sdiValue(1));
            attribMap = { {"E","E"}, {"PR","Nu"}, {"RHO", "RHO_I"}, {"A", "a"} , {"B", "b"},
                          {"C" ,"c"}, {"N", "n"}, {"PSFAIL", "EPS_p_max"}, 
                          {"SIGMAX", "SIG_max0" }, {"EPSO", "EPS_DOT_0"} };
            break;
        case 99:
            p_ConvertMatL99(dynaMat, destCard, radMat);
            attribMap = { {"RHO","RHO_I"}, {"E","E"}, {"PR","Nu"}, {"EPPFR","EPS_p_max"},
                          {"A","a"}, {"B","b"}, {"N","n"}, {"C","c"} };
            break;
        case 100:
        {
            EntityRead compEntRead(p_lsdynaModel, p_PartBeingConverted);
            if (3 == p_ConvertUtils.GetComponentType(compEntRead))
            {
                p_ConvertMatL100(dynaMat, destCard, attribMap, radMat);
            }
            break;
        }
        case 105:
            p_ConvertMatL105(dynaMat, destCard, attribMap, radMat, matLawNum);
            break;
        case 111:
            p_ConvertMatL111(dynaMat, destCard, attribMap, radMat);
            break;
        case 119:
            p_ConvertMatL119(dynaMat, destCard, attribMap, radMat);
            break;
        case 120:
            p_ConvertMatL120(dynaMat, destCard, attribMap, radMat);
            break;
        case 121:
            p_ConvertMatL121(dynaMat, destCard, attribMap, radMat);
            break;
        case 122:
            p_ConvertMatL122(dynaMat, destCard, attribMap, radMat);
            break;
        case 123:
            p_ConvertMatL123(dynaMat, destCard, attribMap, radMat, matLawNum, matLawNumChoice);
            break;
        case 124:
            p_ConvertMatL124(dynaMat, destCard, attribMap, radMat);
            break;
        case 126:
            destCard = "/MAT/LAW50";
            attribMap = { {"RHO", "RHO_I"}, {"TSEF", "Eps_max11"}, {"TSEF","Eps_max22"}, {"TSEF", "Eps_max33"}, {"SSEF" , "Eps_max12"},
                          {"SSEF" , "Eps_max23"}, {"SSEF" , "Eps_max31"} };
            p_ConvertMatL26(dynaMat, radMat);
            break;
        case 138:
            p_ConvertMatL138(dynaMat, destCard, attribMap, radMat);
            break;
        case 154:
            p_ConvertMatL154(dynaMat, destCard, attribMap, radMat);
            break;
        case 169:
            p_ConvertMatL169(dynaMat, destCard, attribMap, radMat);
            break;
        case 177:
            p_ConvertMatL177H(dynaMat, destCard, attribMap, radMat);
            break;
        case 181:
            p_ConvertMatL181(dynaMat, destCard, attribMap, radMat);
            break;
        case 183:
            p_ConvertMatL183(dynaMat, destCard, attribMap, radMat);
            break;
        case 187:
            p_ConvertMatL187(dynaMat, destCard, attribMap, radMat);
            break;
        case 196:
            p_ConvertMatL196(dynaMat, destCard, attribMap, radMat);
            break;
        case 224:
            p_ConvertMatL224(dynaMat, destCard, attribMap, radMat);
            break;
        case 240:
            p_ConvertMatL240(dynaMat, destCard, attribMap, radMat);
            break;
        case 252:
            p_ConvertMatL252(dynaMat, destCard, attribMap, radMat);
            break;
        case 501:
        case 502:
        case 503:
        case 504:
        case 505:
        case 506:
        case 508:
            skipDuplicatingMat = true;
            destCard = "/MAT/LAW108";
            break;
        case 156: // full convertion is done in "convertprops.cxx"
            break;
        case 515: // full convertion is done in "convertprops.cxx"
            break;
        case 801:
            if (propKeyWord.find("SEATBELT") != propKeyWord.npos)
            {
                p_ConvertMatL801Seatbelt(dynaMat, destCard, attribMap, radMat);
            }
            else if (propKeyWord.find("SHELL") != propKeyWord.npos)
            {
                p_ConvertMatL801Shell(dynaMat, destCard, attribMap, radMat);
            }
            break;
        default:
            if(!sdiVectorContains(p_convertedMats,dynaMatId))
            {
                //DynaToRad::ShowMessage(sdiMessageHandler::Level::Error, 1,
                //    sourceCard.c_str(), dynaMatId, dynaMat.GetName().c_str());
                unsigned int newOptionId;
                if (matToPropsReferenceCount > 1)
                    newOptionId = p_ConvertUtils.GetDynaMaxEntityID(srcEntityType);
                else
                    newOptionId = dynaMat.GetId();

                ConvertUtils cnvrtUtil(p_lsdynaModel, p_radiossModel);

                
                //printf("selCard __%s__ newKeyWord __%s__ \n",Keyword.c_str(),newKeyWord.c_str());
                //cnvrtUtil.Convert1To1(dynaMat,newKeyWord,newOptionId);
                
                unsigned int matnum = matLawNum;
                sdiString optionKey = sdiString("*MAT");

                cnvrtUtil.Convert1To1(dynaMat, optionKey,  matnum, newOptionId);
                
                p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*MAT"), newOptionId, radMat);

                p_convertedMats.push_back(dynaMatId);
            }
            break;
        }
        if (!radMat.IsValid())
        {
            if (matToPropsReferenceCount > 1 && !skipDuplicatingMat)
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
            else
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);
        }

        if (matToPropsReferenceCount > 1 && !skipDuplicatingMat)
        {
            mapMatIdVsMapPropIdNewMatHandle[dynaMatId].insert(make_pair(propRead.GetId(), radMat));
            p_MatPropRelationDB.UpdateReferenceList(srcEntityType, dynaMatId, propRead);
        }
        else
            p_convertedMats.push_back(dynaMatId);
        sdiVectorUnique(p_convertedMats);

        if(lsdEosHRead.IsValid())
        {
            EntityRead lsdEosEntRead(p_lsdynaModel, lsdEosHRead);
            sdiString eosCard  = lsdEosEntRead.GetKeyword();

            size_t pos = eosCard.find("_TITLE");
            if (pos != string::npos)
            {
                eosCard.erase(pos);
            }
            unsigned short int eosNum = dynaEosMap[eosCard];

            switch (eosNum) 
            {
            case 1:
                p_ConvertEOS1(lsdEosEntRead, destCard, attribMap, dynaMat, radMat);
                break;
            case 4:
                p_ConvertEOS4(lsdEosEntRead, destCard, attribMap, radMat);
                break;

            default:
                break;
            }
        }

        if (radMat.IsValid())
        {
            SDIHandlReadList sourceMats = { {dynaMat.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radMat, sourceMats));
            if (!destCard.empty())
            {
                EntityEdit radMatEdit(p_radiossModel, radMat);
                for (const auto pair : attribMap)
                {
                    p_ConvertUtils.CopyValue(dynaMat, radMatEdit, pair.first, pair.second);
                }
                for (const auto valuePair : attribDoubleValueMap)
                {
                    radMatEdit.SetValue(sdiIdentifier(valuePair.first), sdiValue(valuePair.second));
                }
                for (const auto valuePair : attribintValueMap)
                {
                    radMatEdit.SetValue(sdiIdentifier(valuePair.first), sdiValue(valuePair.second));
                }
            }
        }
    }
}

void ConvertMat::p_ConvertMatL2(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();

    destCard = "/MAT/LAW93";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);

    attribMap = { {"Rho", "MAT_RHO"}, {"LSDYNA_EA", "LAW93_E11"}, {"LSDYNA_EB", "LAW93_E22"}, {"LSDYNA_EC", "LAW93_E33"},
                  {"LSDYNA_GAB", "LAW93_G12"}, {"LSDYNA_GBC", "LAW93_G23"}, {"LSDYNA_GCA", "LAW93_G13"}};
    
    if (radMat.IsValid())
    {
        EntityEdit radMatEntityEdit(p_radiossModel, radMat);
        p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "EA*PRBA/EB", "NU12");
        p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "EA*PRCA/EC", "NU13");
        p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "EB*PRCB/EC", "NU23");
    }
}

void ConvertMat::p_ConvertMatL3 (const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    destCard = "/MAT/LAW44";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    attribMap = { {"E","E"}, {"PR","nu"}, {"RHO", "Init.dens."}, {"SRC","C"}, {"SRP", "P"}, {"SIGY", "A"}, {"FS","EPS_MAX"}, {"VP","VP"} };
    
    //B = ETAN*E/(E-ETAN)
    p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "ETAN*E/(E-ETAN)", "B");
    p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "1-BETA", "C_HARD");

    radmatEntityEdit.SetValue(sdiIdentifier("ISMOOTH"), sdiValue(1));
    radmatEntityEdit.SetValue(sdiIdentifier("n"), sdiValue(1.0)); 

    //The rest of parameters are set to default 0 values in converson
    radmatEntityEdit.SetValue(sdiIdentifier("sigmax"), sdiValue(0.0)); 
    radmatEntityEdit.SetValue(sdiIdentifier("ICC"), sdiValue(0));
    radmatEntityEdit.SetValue(sdiIdentifier("F_CUT"), sdiValue(0.0));
    radmatEntityEdit.SetValue(sdiIdentifier("EPST1"), sdiValue(0.0));
    radmatEntityEdit.SetValue(sdiIdentifier("EPST2"), sdiValue(0.0));
}

void ConvertMat::p_ConvertMatL6(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    EntityRead compEntRead(p_lsdynaModel, p_PartBeingConverted);
    double lsdG0;
    double lsdGI;
    double lsdBULK;
    double lsdBETA;
    vector<reference_wrapper<double>> attrValList = { lsdG0, lsdGI, lsdBETA, lsdBULK };
    vector<sdiString> attrNameList = { "LSD_MAT_G0","LSD_MAT_GI","LSD_MAT_BETA","LSD_MAT_BULK" };
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);
    int compType = p_ConvertUtils.GetComponentType(compEntRead);
    destCard = "/MAT/LAW34";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    attribMap = { {"RHO","Init.dens."}, {"LSD_MAT_BULK", "K"} };
    for (const auto pair : map<sdiString, double>({ {"G0",lsdG0}, {"Gl", lsdGI}, { "Beta", lsdBETA} }))
        radmatEntityEdit.SetValue(sdiIdentifier(pair.first), sdiValue(pair.second));

    if (lsdG0 == lsdGI)
    {
        DynaToRad::ShowMessage(sdiMessageHandler::Level::Error, 3,
            dynaMat.GetKeyword().c_str(), dynaMat.GetId(), dynaMat.GetName().c_str());
    }
    vector<sdiString> crvAttNames{ {"G0_CURVES", "GI_CURVES", "BETA_CURVES"} };
    vector<sdiString> radAttNames{ {"G0", "Gl", "Beta"} };
    for (size_t i = 0; i < 3; ++i)
    {
        HandleRead curveHread;
        dynaMat.GetEntityHandle(sdiIdentifier(crvAttNames[i]), curveHread);
        if (curveHread.IsValid())
        {
            sdiDoubleList crvPnts;
            sdiValue tempVal(crvPnts);
            curveHread.GetValue(p_lsdynaModel, sdiIdentifier("points"), tempVal);
            tempVal.GetValue(crvPnts);
            if (!crvPnts.empty())
                radmatEntityEdit.SetValue(sdiIdentifier(radAttNames[i]), sdiValue(crvPnts[1]));
        }
    }
}

void ConvertMat::p_ConvertMatL5 (const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    destCard = "/MAT/LAW21";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    attribMap = { {"RHO", "RHO_I"} };
    
    sdiValue tempValue;
    double lsdG;
    double lsdBulk;
    double radNu;
    double radB;
    double radMumax = 0.0;
    double lsdVCR = 0.0;

    vector<reference_wrapper<double>> attrValList = { lsdG, lsdBulk, lsdVCR };
    vector<sdiString> attrNameList = { "G","BULK", "VCR" };
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);
    
    //E = 9*G*K/(3*K+G)
    p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "9*G*BULK/(3*BULK+G)", "E");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "A0", "A0");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "A1", "A1"); 
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "A2", "A2");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "PC", "P_min");

    radNu = (3*lsdBulk - 2*lsdG) / (6*lsdBulk + 2*lsdG);
    if(radNu < 0.0)
    {
        radNu = 0.0;
    }
    else if(radNu > 0.495)
    {
        radNu = 0.495;
    }
    radmatEntityEdit.SetValue(sdiIdentifier("Nu"), sdiValue(radNu));

    radB = lsdBulk;

    if(lsdVCR == 1.0)
    {
        radB = 0.0;
        radMumax = 0.0;

    }
    radmatEntityEdit.SetValue(sdiIdentifier("B"), sdiValue(radB));
    radmatEntityEdit.SetValue(sdiIdentifier("Mu_max"), sdiValue(radMumax));

    p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "BULK/100.0", "Kt");

    HandleRead lcidHandle;
    dynaMat.GetEntityHandle(sdiIdentifier("LCID"), lcidHandle);

    if (lcidHandle.IsValid())
    {
        int nPnts = 0;
        sdiDoubleList crvPoints;

        double lsdSFA = 1.0;
        double lsdSFO = 1.0;
        double lsdOFFA = 0.0;
        double lsdOFFO = 0.0;
        EntityRead lcidRead(p_lsdynaModel, lcidHandle);
        sdiValue tempValue(crvPoints);
        lcidRead.GetValue(sdiIdentifier("points"), tempValue);
        tempValue.GetValue(crvPoints);

        tempValue = sdiValue(nPnts);
        lcidRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
        tempValue.GetValue(nPnts);
        crvPoints.reserve(2 * nPnts + 2);

        vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
        vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
        p_ConvertUtils.GetAttribValues(lcidRead, lsdQueryAttribs, attribVals);
        lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
        lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

        int cptpos = 0;
        int cptneg = 0;
        for (size_t i = 0; i < crvPoints.size(); i += 2)
        {
            if( crvPoints[i] < 0.0 )
                cptneg++;
            else if( crvPoints[i] > 0.0 )
                cptpos++;
        }

        sdiDoubleList crvNegPoints,crvPosPoints;
        crvNegPoints.reserve(2 * cptneg + 2);
        crvPosPoints.reserve(2 * cptpos + 2);
        
        if(cptneg > 0.0 && cptpos > 0.0)
        {
            // keep only positive part of the curve

            for (size_t i = 0; i < crvPoints.size(); i += 2)
            {
                if( crvPoints[i] >= 0.0 )
                {
                    crvPosPoints.push_back(exp(crvPoints[i])-1.0);
                    crvPosPoints.push_back(crvPoints[i+1]);
                 }
            }

            HandleEdit functHEdit;
            p_ConvertUtils.CreateCurve("Recalculated_" + to_string(lcidRead.GetId()) + "_MatL121_" + to_string(dynaMat.GetId()),
                (int)crvPosPoints.size() / 2, crvPosPoints, functHEdit, abs(lsdSFA), abs(lsdSFO), lsdOFFA);
            if (functHEdit.IsValid())
            {
                radmatEntityEdit.SetEntityHandle(sdiIdentifier("func_IDf"), functHEdit);
                sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
            }
        }
        else if(cptneg > 0.0 && cptpos == 0.0)
        {
            // scale abscissas by -1 and sort the points

            for (size_t i = 0; i < crvPoints.size(); i += 2)
            {
                if( crvPoints[i] <= 0.0 )
                {
                    crvNegPoints.insert(crvNegPoints.begin(),  crvPoints[i+1]);
                    crvNegPoints.insert(crvNegPoints.begin(), exp(abs(crvPoints[i]))-1.0);
                }
            }

            HandleEdit functHEdit;
            p_ConvertUtils.CreateCurve("Recalculated_" + to_string(lcidRead.GetId()) + "_MatL121_" + to_string(dynaMat.GetId()),
                (int)crvNegPoints.size() / 2, crvNegPoints, functHEdit, abs(lsdSFA), abs(lsdSFO), lsdOFFA);
            if (functHEdit.IsValid())
            {
                radmatEntityEdit.SetEntityHandle(sdiIdentifier("func_IDf"), functHEdit);
                sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
            }
        }
    }
    else // use EPS ARRAY and EPS ARRAY
    {
        double lsdEPS1;
        double lsdEPS2;
        double lsdEPS3;
        double lsdEPS4;
        double lsdEPS5;
        double lsdEPS6;
        double lsdEPS7;
        double lsdEPS8;
        double lsdEPS9;
        double lsdEPS10;
        double lsdP1;
        double lsdP2;
        double lsdP3;
        double lsdP4;
        double lsdP5;
        double lsdP6;
        double lsdP7;
        double lsdP8;
        double lsdP9;
        double lsdP10;

        vector<reference_wrapper<double>> attrEPSValList = { lsdEPS1,lsdEPS2,lsdEPS3,lsdEPS4,lsdEPS5,
                                                             lsdEPS6,lsdEPS7,lsdEPS8,lsdEPS9,lsdEPS10,
                                                             lsdP1,lsdP2,lsdP3,lsdP4,lsdP5,
                                                             lsdP6,lsdP7,lsdP8,lsdP9,lsdP10 };
        vector<sdiString> attrEPSNameList = { "EPS1","EPS2","EPS3","EPS4","EPS5","EPS6","EPS7","EPS8","EPS9","EPS10",
                                              "P1","P2","P3","P4","P5","P6","P7","P8","P9","P10" };
        p_ConvertUtils.GetAttribValues(dynaMat, attrEPSNameList, attrEPSValList);

        sdiDoubleList crvPoints;
        crvPoints.reserve(2 * 10 + 2);
        crvPoints.clear();

        crvPoints.push_back(lsdEPS1);
        crvPoints.push_back(lsdP1);
        crvPoints.push_back(lsdEPS2);
        crvPoints.push_back(lsdP2);
        crvPoints.push_back(lsdEPS3);
        crvPoints.push_back(lsdP3);
        crvPoints.push_back(lsdEPS4);
        crvPoints.push_back(lsdP4);
        crvPoints.push_back(lsdEPS5);
        crvPoints.push_back(lsdP5);
        crvPoints.push_back(lsdEPS6);
        crvPoints.push_back(lsdP6);
        crvPoints.push_back(lsdEPS7);
        crvPoints.push_back(lsdP7);
        crvPoints.push_back(lsdEPS8);
        crvPoints.push_back(lsdP8);
        crvPoints.push_back(lsdEPS9);
        crvPoints.push_back(lsdP9);
        crvPoints.push_back(lsdEPS10);
        crvPoints.push_back(lsdP10);

        double lsdSFA = 1.0;
        double lsdSFO = 1.0;
        double lsdOFFA = 0.0;
        double lsdOFFO = 0.0;

        int cptpos = 0;
        int cptneg = 0;
        for (size_t i = 0; i < crvPoints.size(); i += 2)
        {
            if( crvPoints[i] < 0.0 )
                cptneg++;
            else if( crvPoints[i] > 0.0 )
                cptpos++;
        }

        sdiDoubleList crvNegPoints,crvPosPoints;
        crvNegPoints.reserve(2 * cptneg + 2);
        crvPosPoints.reserve(2 * cptpos + 2);
        
        if(cptneg > 0.0 && cptpos > 0.0)
        {
            // keep only positive part of the curve (EPS-P)

            for (size_t i = 0; i < crvPoints.size(); i += 2)
            {
                if( crvPoints[i] >= 0.0 )
                {
                    crvPosPoints.push_back(exp(crvPoints[i])-1.0);
                    crvPosPoints.push_back(crvPoints[i+1]);
                 }
            }

            HandleEdit functHEdit;
            p_ConvertUtils.CreateCurve("EPS-P_curve_MatL121_" + to_string(dynaMat.GetId()),
                (int)crvPosPoints.size() / 2, crvPosPoints, functHEdit, lsdSFA, lsdSFO, lsdOFFA);
            if (functHEdit.IsValid())
            {
                radmatEntityEdit.SetEntityHandle(sdiIdentifier("func_IDf"), functHEdit);
                sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
            }
        }
        else if(cptneg > 0.0 && cptpos == 0.0)
        {
            // scale abscissas by -1 and sort the points (EPS-P curve)

            for (size_t i = 0; i < crvPoints.size(); i += 2)
            {
                if( crvPoints[i] <= 0.0 )
                {
                    crvNegPoints.insert(crvNegPoints.begin(),  crvPoints[i+1]);
                    crvNegPoints.insert(crvNegPoints.begin(), exp(abs(crvPoints[i]))-1.0);
                }
            }

            HandleEdit functHEdit;
            p_ConvertUtils.CreateCurve("EPS-P_curve_MatL121_" + to_string(dynaMat.GetId()),
                (int)crvNegPoints.size() / 2, crvNegPoints, functHEdit, lsdSFA, lsdSFO, lsdOFFA);
            if (functHEdit.IsValid())
            {
                radmatEntityEdit.SetEntityHandle(sdiIdentifier("func_IDf"), functHEdit);
                sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
            }
        }
    }
}

void ConvertMat::p_ConvertMatL15(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    destCard = "/MAT/PLAS_JOHNS";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
    radMat.SetValue(p_radiossModel, sdiIdentifier("LAW2_ID"), sdiValue(1));
    attribMap = {{"RHO","RHO_I"}, {"PR","Nu"}, {"A", "a"}, {"B","b"},
                  {"C","c"}, {"N","n"}, {"M","m"}, {"TM","T_melt"}, {"TR","T_r"}, {"EPSO","EPS_DOT_0"}};
    
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);   
    sdiValue tempValue;
    double lsdRHO;
    double lsdCP;
    double lsdD1;
    double lsdD2;
    double lsdD3;
    double lsdD4;
    double lsdD5;
    double lsdEROD;
    double lsdEFMIN;
    double lsdNUMINT;

    vector<reference_wrapper<double>> attrValList = { lsdRHO, lsdCP, lsdD1, lsdD2, lsdD3, lsdD4, lsdD5, lsdEROD, lsdEFMIN, lsdNUMINT };
    vector<sdiString> attrNameList = { "RHO","CP","D1","D2","D3","D4","D5", "EROD", "EFMIN", "NUMINT" };
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);
    
    radmatEntityEdit.SetValue( sdiIdentifier("rhoC_p"), sdiValue(lsdRHO*lsdCP));
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "LSD_VP15", "MAT_VP");
    double lsdE = GetValue<double>(dynaMat, "E");
    double lsdG = GetValue<double>(dynaMat, "G");
    
    if(lsdE != 0.0 )
    {
        p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "E", "E");
    }
    else if(lsdE == 0.0 && lsdG != 0.0)
    {
        p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "2.0 * G * ( 1.0 + PR)", "E");
    }

    double lsdDTF = GetValue<double>(dynaMat, "DTF");

    if(lsdDTF > 0.0)
    {
        // "/FAIL/GENE1"

        HandleEdit failGene1HEdit;
        if (!failGene1HEdit.IsValid())
        {
            p_radiossModel->CreateEntity(failGene1HEdit, "/FAIL/GENE1", dynaMatName);
            failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
            if (failGene1HEdit.IsValid())
            {
                failGene1HEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("mat_id"), radMat);
                    EntityEdit failGene1Edit(p_radiossModel, failGene1HEdit);


                    p_ConvertUtils.CopyValue(dynaMat, failGene1Edit, "DTF", "dtmin");

                    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(failGene1HEdit, sourcemat));
            }
        }
    }
    else if (lsdD1 !=0 || lsdD2 != 0 || lsdD3 != 0 || lsdD4 != 0 || lsdD5 != 0)
    {
        HandleEdit failEdit;
        p_radiossModel->CreateEntity(failEdit, "/FAIL/JOHNSON", dynaMatName);
        failEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
        if (failEdit.IsValid())
        {
            failEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("mat_id"), radMat);

            EntityEdit failEntEdit(p_radiossModel, failEdit);
            map<string, double> attribDoubleValueFail = { {"D1", lsdD1},{"D2", lsdD2},{"D4", lsdD4},{"D5", lsdD5} };

            for (const auto valuePair : attribDoubleValueFail)
            {
                failEntEdit.SetValue(sdiIdentifier(valuePair.first), sdiValue(valuePair.second));
            }
            p_ConvertUtils.SetExpressionValue(dynaMat, failEntEdit, "-abs(D3)", "D3");

            failEntEdit.SetValue(sdiIdentifier("Ifail_so"), sdiValue((lsdEROD != 0) ? 2 : 1));

            failEntEdit.SetValue(sdiIdentifier("Ifail_sh"), sdiValue(2));
            
            p_ConvertUtils.CopyValue(dynaMat, failEntEdit, "EFMIN", "EPSF_MIN");

            sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(failEdit, sourcemat));
        }
    }
}

void ConvertMat::p_ConvertMatL15EOS(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    destCard = "/MAT/LAW4";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
    attribMap = {{"RHO","RHO_I"}, {"PR","nu"}, {"A", "A"}, {"B","B"},
                  {"C","C"}, {"N","n"}, {"M","M"}, {"TM","Tmelt"}, {"TR","Tmax"}, {"EPSO","EPS_DOT_0"}};
    
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);   
    sdiValue tempValue;
    double lsdRHO;
    double lsdCP;
    double lsdD1;
    double lsdD2;
    double lsdD3;
    double lsdD4;
    double lsdD5;
    double lsdEROD;
    double lsdEFMIN;
    double lsdNUMINT;

    vector<reference_wrapper<double>> attrValList = { lsdRHO, lsdCP, lsdD1, lsdD2, lsdD3, lsdD4, lsdD5, lsdEROD, lsdEFMIN, lsdNUMINT };
    vector<sdiString> attrNameList = { "RHO","CP","D1","D2","D3","D4","D5", "EROD", "EFMIN", "NUMINT" };
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);
    
    radmatEntityEdit.SetValue( sdiIdentifier("RHOCP"), sdiValue(lsdRHO*lsdCP));

    double lsdE = GetValue<double>(dynaMat, "E");
    double lsdG = GetValue<double>(dynaMat, "G");

    if(lsdE != 0.0 )
    {
        p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "E", "E");
    }
    else if(lsdE == 0.0 && lsdG != 0.0)
    {
        p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "2.0 * G * ( 1.0 + PR)", "E");
    }


    if (lsdD1 !=0 || lsdD2 != 0 || lsdD3 != 0 || lsdD4 != 0 || lsdD5 != 0)
    {
        HandleEdit failEdit;
        p_radiossModel->CreateEntity(failEdit, "/FAIL/JOHNSON", dynaMatName);
        failEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
        if (failEdit.IsValid())
        {
            failEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("mat_id"), radMat);

            EntityEdit failEntEdit(p_radiossModel, failEdit);
            map<string, double> attribDoubleValueFail = { {"D1", lsdD1},{"D2", lsdD2},{"D4", lsdD4},{"D5", lsdD5} };

            for (const auto valuePair : attribDoubleValueFail)
            {
                failEntEdit.SetValue(sdiIdentifier(valuePair.first), sdiValue(valuePair.second));
            }
            p_ConvertUtils.SetExpressionValue(dynaMat, failEntEdit, "-abs(D3)", "D3");
            
            failEntEdit.SetValue(sdiIdentifier("Ifail_so"), sdiValue((lsdEROD != 0) ? 2 : 1));

            failEntEdit.SetValue(sdiIdentifier("Ifail_sh"), sdiValue(2));

            p_ConvertUtils.CopyValue(dynaMat, failEntEdit, "EFMIN", "EPSF_MIN");

            sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(failEdit, sourcemat));
        }
    }
}

void ConvertMat::p_ConvertMatL18(const sdi::EntityRead& dynaMat, sdiString& destCard, sdi::HandleEdit& radMat)
{
    double lsdE; 
    double lsdK;
    double lsdN;
    double lsdSRC;
    double lsdSRP;
    double lsdSIGY;
    double lsdEPSF;
    double lsdVP;
    double radSIGYP = 0;
    HandleEdit failJohnsonEdit;
    HandleEdit functEdit;

    sdiString dynaMatName = dynaMat.GetName();
    vector<reference_wrapper<double>> attrValList = { lsdE, lsdK, lsdN, lsdSRC, lsdSRP, lsdSIGY, lsdEPSF, lsdVP };
    vector<sdiString> attrNameList = { "E","K","N","SRC","SRP","SIGY","EPSF","VP" };
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

    destCard = "/MAT/COWPER";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());

    if (lsdSIGY > 0.002 && lsdN != 0.0 && lsdK != 0.0)
        radSIGYP = pow((lsdSIGY / lsdK), (1 / lsdN));
    else if (lsdN - 1.0 != 0.0 && lsdK != 0.0)
        radSIGYP = pow(lsdE / lsdK, 1 / (lsdN - 1));

    if (lsdEPSF > 0.0)
    {
        p_radiossModel->CreateEntity(failJohnsonEdit, "/FAIL/JOHNSON", dynaMatName);
        failJohnsonEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));

        failJohnsonEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("mat_id"), radMat);
        failJohnsonEdit.SetValue(p_radiossModel, sdiIdentifier("D1"), sdiValue(lsdEPSF));
        failJohnsonEdit.SetValue(p_radiossModel, sdiIdentifier("D2"), sdiValue(0.0));
        failJohnsonEdit.SetValue(p_radiossModel, sdiIdentifier("D3"), sdiValue(0.0));
        failJohnsonEdit.SetValue(p_radiossModel, sdiIdentifier("D4"), sdiValue(0.0));
        failJohnsonEdit.SetValue(p_radiossModel, sdiIdentifier("D5"), sdiValue(0.0));
        failJohnsonEdit.SetValue(p_radiossModel, sdiIdentifier("Epsilon_Dot_0"), sdiValue(0.0));
        failJohnsonEdit.SetValue(p_radiossModel, sdiIdentifier("Ifail_sh"), sdiValue(2));
        failJohnsonEdit.SetValue(p_radiossModel, sdiIdentifier("Ifail_so"), sdiValue(1));
        failJohnsonEdit.SetValue(p_radiossModel, sdiIdentifier("Dadv"), sdiValue(0.0));
        failJohnsonEdit.SetValue(p_radiossModel, sdiIdentifier("Ixfem"), sdiValue(0));
    }

    sdiDoubleList abscissaList({ 0.0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01,
        0.015, 0.02, 0.025, 0.03, 0.035, 0.04, 0.045, 0.05, 0.055, 0.06, 0.065, 0.07, 0.075, 0.08, 0.085, 0.09, 0.095, 0.1,
        0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1.0 });
    sdiDoubleList pointsList;
    pointsList.reserve(2 * abscissaList.size());
    for (double abscissa : abscissaList)
    {
        pointsList.push_back(abscissa);
        pointsList.push_back(lsdK * pow(radSIGYP + abscissa, lsdN));
    }
    p_ConvertUtils.CreateCurve(dynaMatName, (int)abscissaList.size(), pointsList, functEdit);

    if (lsdSRC != 0 || lsdSRP != 0)
        radMat.SetValue(p_radiossModel, sdiIdentifier("Fsmooth"), sdiValue(1));
    radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue((int)lsdVP));
    radMat.SetEntityHandle(p_radiossModel, sdiIdentifier("fct_IDp"), functEdit);
    radMat.SetValue(p_radiossModel, sdiIdentifier("Optional_card"), sdiValue(1)); 

    sdiConvert::SDIHandlReadList sourcehandleList = { {dynaMat.GetHandle()} };
    if (failJohnsonEdit.IsValid())
        sdiConvert::Convert::PushToConversionLog(std::make_pair(failJohnsonEdit, sourcehandleList));
    if (functEdit.IsValid())
        sdiConvert::Convert::PushToConversionLog(std::make_pair(functEdit, sourcehandleList));
}

void ConvertMat::p_ConvertMatL19(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    destCard = "/MAT/LAW121";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);

    attribMap = { {"RHO", "RHO_I"} };
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "E", "E");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "PR", "Nu");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "VP", "Ivisc");

    HandleRead lcdiHandle;
    dynaMat.GetEntityHandle(sdiIdentifier("LC1"), lcdiHandle);
    if (lcdiHandle.IsValid())
    {
        radmatEntityEdit.SetValue(sdiIdentifier("Fct_SIG0"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), lcdiHandle.GetId(p_lsdynaModel))));
    }

    
    dynaMat.GetEntityHandle(sdiIdentifier("LC2"), lcdiHandle);
    if (lcdiHandle.IsValid())
    {
        radmatEntityEdit.SetValue(sdiIdentifier("Fct_YOUN"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), lcdiHandle.GetId(p_lsdynaModel))));
    }

    
    dynaMat.GetEntityHandle(sdiIdentifier("LC3"), lcdiHandle);
    if (lcdiHandle.IsValid())
    {
        radmatEntityEdit.SetValue(sdiIdentifier("Fct_TANG"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), lcdiHandle.GetId(p_lsdynaModel))));
    }
    else
    {
        p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "ETAN", "TANG");
    }

    
    dynaMat.GetEntityHandle(sdiIdentifier("LC4"), lcdiHandle);
    if (lcdiHandle.IsValid())
    {
        radmatEntityEdit.SetValue(sdiIdentifier("Fct_FAIL"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), lcdiHandle.GetId(p_lsdynaModel))));
    }

    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "TDEL", "DTMIN");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "RDEF", "Ifail");
}

void ConvertMat::p_ConvertMatL24(const EntityRead& dynaMat,sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat, unsigned short int &matLawNum, unsigned short int &matLawNumChoice)
{
    double lsdSIGY;
    double lsdETAN;
    double lsdTDEL;
    double lsdC;
    double lsdP;
    double lsdFAIL;
    double lsdEPS1;
    double lsdEPS2;
    double lsdEPS3;
    double lsdEPS4;
    double lsdEPS5;
    double lsdEPS6;
    double lsdEPS7;
    double lsdEPS8;
    double lsdES1;
    double lsdES2;
    double lsdES3;
    double lsdES4;
    double lsdES5;
    double lsdES6;
    double lsdES7;
    double lsdES8;
    double lsdVp;
    sdiString dynaMatName = dynaMat.GetName();
 
    attribMap = { {"E","E"}, {"PR","Nu"}, {"RHO", "Init.dens."} };

    vector<reference_wrapper<double>> attrValList = {   lsdSIGY, lsdETAN, lsdTDEL, lsdC, lsdP, lsdEPS1, lsdEPS2,lsdEPS3,
                                                        lsdEPS4, lsdEPS5, lsdEPS6, lsdEPS7, lsdEPS8, lsdES1,lsdES2,
                                                        lsdES3, lsdES4, lsdES5, lsdES6, lsdES7, lsdES8, lsdFAIL, lsdVp };

    vector<sdiString> attrNameList = {   "SIGY","ETAN","TDEL","C","P","EPS1","EPS2","EPS3",
                                        "EPS4","EPS5","EPS6","EPS7","EPS8","ES1","ES2",
                                        "ES3","ES4","ES5","ES6","ES7","ES8","FAIL", "VP"};

    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

    HandleRead lcssHandle;
    HandleRead lcsrHandle;
    dynaMat.GetEntityHandle(sdiIdentifier("LCSS"), lcssHandle);
    dynaMat.GetEntityHandle(sdiIdentifier("LCSR"),lcsrHandle);
    int lcssId = lcssHandle.GetId(p_lsdynaModel);
    int lcsrId = lcsrHandle.GetId(p_lsdynaModel);

    sdiValue curveListVal;
    sdiUIntList entList;
    sdiDoubleList scaleList;
    sdiDoubleList strainRatesList;
    sdiDoubleList strainRatesLogList;
    int nFunct = 1;
    sdiValue tempValue;
    bool negstrains = false;

    HandleEdit failJohnsonHEdit; /* for /FAIL/JOHNSON */

    destCard = "/MAT/LAW36";
    // check if "lcss" referenced curve is present in the model
    sdiValueEntity lcssEntity = GetValue<sdiValueEntity>(dynaMat, "LCSS");
    unsigned int lcssEntityId=lcssEntity.GetId();

    if(!lcssId && lcssEntityId > 0)
    {
            DynaToRad::ShowMessage(sdiMessageHandler::Level::Error, 42,
                dynaMat.GetKeyword().c_str(), dynaMat.GetId(), dynaMat.GetName().c_str());
    }

    if (lcssId)
    {
        EntityRead lcssEntityRead(p_lsdynaModel, lcssHandle);
        sdiString keyWord = lcssEntityRead.GetKeyword();
        sdiValueEntityList curveList;
        if (keyWord.find("TABLE") != keyWord.npos)
        {
            if (matToPropsReferenceCount > 1)
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
            else
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMat.GetId());
            int crvArrCount = 0;

            tempValue = sdiValue(crvArrCount);
            lcssEntityRead.GetValue(sdiIdentifier("ArrayCount"), tempValue);
            tempValue.GetValue(crvArrCount);
            nFunct = crvArrCount + 1;

            tempValue = sdiValue(curveList);
            lcssEntityRead.GetValue(sdiIdentifier("CurveIds"), tempValue);
            tempValue.GetValue(curveList);
            curveList.GetIdList(entList);
            entList.push_back(entList.back());

            scaleList = sdiDoubleList(nFunct, 1.0);

            tempValue = sdiValue(strainRatesList);
            lcssEntityRead.GetValue(sdiIdentifier("VALUE"), tempValue);
            tempValue.GetValue(strainRatesList);

            if (strainRatesList[0] >= 0)
                strainRatesList.push_back(2.0 * strainRatesList.back());
//-------------
            else if (strainRatesList[0] < 0)
            // natural logarithmic values f strain rates
            {
                negstrains = true;
                for (int i = 0; i < strainRatesList.size(); ++i)
                {
                   strainRatesLogList.push_back(exp(strainRatesList[i]));
                }
                strainRatesLogList.push_back(2.0 * strainRatesLogList.back());
                strainRatesList.clear();
                for (int i = 0; i < strainRatesLogList.size(); ++i)
                {
                    strainRatesList.push_back(strainRatesLogList[i]);
                }
                //-------
                // new table for logarithmic strain rate
                //-------
                HandleEdit table1HEdit;
                p_radiossModel->CreateEntity(table1HEdit, "/TABLE/1", "Duplicate_table_ID " + to_string(lcssEntityRead.GetId())+", " + "Mat name_"+dynaMatName+"_ ID _ " + to_string(dynaMat.GetId()), 
                                p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*DEFINE_TABLE")));
                EntityEdit table1Edit(p_radiossModel, table1HEdit);
                table1Edit.SetValue(sdiIdentifier("dimension"), sdiValue(2));
                table1Edit.SetValue(sdiIdentifier("curverows"), sdiValue(nFunct));
                table1Edit.SetValue(sdiIdentifier("fct_ID"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), entList)));
                table1Edit.SetValue(sdiIdentifier("A"), sdiValue(strainRatesList));
                table1Edit.SetValue(sdiIdentifier("Scale_y"), sdiValue(scaleList));
                SDIHandlReadList duplidynaTables = { {dynaMat.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(table1HEdit, duplidynaTables));
            }
            radMat.SetValue(p_radiossModel, sdiIdentifier("F_smooth"), sdiValue(2));
//-------------
        }
        else if (lcsrId)
        {
            if (matToPropsReferenceCount > 1)
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
            else
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMat.GetId());
            EntityRead lcsrEdit(p_lsdynaModel, lcsrHandle);
            int nPnts = 0;
            sdiDoubleList crvPoints;
            sdiValue tempValue(nPnts);
            lcsrEdit.GetValue(sdiIdentifier("numberofpoints"), tempValue);
            tempValue.GetValue(nPnts);

            tempValue = sdiValue(crvPoints);
            lcsrEdit.GetValue(sdiIdentifier("points"), tempValue);
            tempValue.GetValue(crvPoints);

            double lsdSFA = 1.0;
            double lsdSFO = 1.0;
            double lsdOFFA = 0.0;
            double lsdOFFO = 0.0;
            vector<reference_wrapper<double>> lsdDoubleAttrValues({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
            vector<sdiString> lsdDoubleAttrNames({ "SFA", "SFO", "OFFA", "OFFO" });
            p_ConvertUtils.GetAttribValues(lcsrEdit, lsdDoubleAttrNames, lsdDoubleAttrValues);

            lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
            lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

            int numVals = nPnts * 2;
            for (int i = 0; i < numVals; ++i)
            {
                if (i % 2 == 0)
                    strainRatesList.push_back(crvPoints[i] * lsdSFA + (lsdOFFA * lsdSFA));
                else
                    scaleList.push_back(crvPoints[i] * lsdSFO + (lsdOFFO * lsdSFO));
                if (i == numVals - 2)
                    strainRatesList.push_back(2 * (crvPoints[i] * lsdSFA + (lsdOFFA * lsdSFA)));
                if (i == numVals - 1)
                    scaleList.push_back(crvPoints[i] * lsdSFO + (lsdOFFO * lsdSFO));
            }
            nFunct = nPnts + 1;
            entList = sdiUIntList(nFunct, lcssId);
        }
        else if (lsdC > 0.0 && lsdP > 0.0)
        {
            p_ConvertToMatLAW44(dynaMat, destCard, attribMap, radMat);
            matLawNumChoice = 44;
//-------
            if(matLawNum != 24) radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue((int)lsdVp));

            if(lsdVp == -1.0f && matLawNum == 24)
            {
                radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(3));
                radMat.SetValue(p_radiossModel, sdiIdentifier("ISMOOTH"), sdiValue(1));
            }
            if((lsdVp == 0.f || lsdVp == 3.0f) && matLawNum == 24)
            {
                radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(0));
                radMat.SetValue(p_radiossModel, sdiIdentifier("ISMOOTH"), sdiValue(1));
            }
            if(lsdVp == 1.0f && matLawNum == 24)
            {
                radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(1));
                radMat.SetValue(p_radiossModel, sdiIdentifier("ISMOOTH"), sdiValue(0));
            }

            if (lsdFAIL > 0.0 && matLawNum == 24)
            {
                p_radiossModel->CreateEntity(failJohnsonHEdit, "/FAIL/JOHNSON", dynaMatName);
                //failJohnsonHEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
                EntityEdit failJohnsonEntEdit(p_radiossModel, failJohnsonHEdit);
                failJohnsonEntEdit.SetEntityHandle(sdiIdentifier("mat_id"), radMat);
                p_ConvertUtils.CopyValue(dynaMat, failJohnsonEntEdit, "FAIL", "D1");
                failJohnsonEntEdit.SetValue(sdiIdentifier("IFAIL_SH"), sdiValue(2));
                radMat.SetValue(p_radiossModel, sdiIdentifier("EPS_MAX"), sdiValue(0.0));
            }
//-------
            return;
        }
        else
        {
            if (matToPropsReferenceCount > 1)
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
            else
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMat.GetId());

            entList.push_back(lcssId);
            scaleList.push_back(1.0);
            strainRatesList.push_back(0.0);
            nFunct = 1;
        }

        if(matLawNum != 24) radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue((int)lsdVp));
        matLawNumChoice = 36;

        sdiString keyWordLog = dynaMat.GetKeyword();
        if((lsdVp == -1.0f || lsdVp == 0.f || lsdVp == 3.0f || lsdVp == 1.0f) && matLawNum == 24)
        {
            radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(0));
            if(lsdVp == 1.0f) radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(1));
            if (keyWordLog.find("LOG_INTERPOLATION") != keyWordLog.npos || negstrains)
            {
                radMat.SetValue(p_radiossModel, sdiIdentifier("F_smooth"), sdiValue(2));
            }
            else
            {
                radMat.SetValue(p_radiossModel, sdiIdentifier("F_smooth"), sdiValue(1));
            }
        }

        if (lsdFAIL > 0.0 && matLawNum == 24)
        {
            p_radiossModel->CreateEntity(failJohnsonHEdit, "/FAIL/JOHNSON", dynaMatName);
            //failJohnsonHEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
            EntityEdit failJohnsonEntEdit(p_radiossModel, failJohnsonHEdit);
            failJohnsonEntEdit.SetEntityHandle(sdiIdentifier("mat_id"), radMat);
            p_ConvertUtils.CopyValue(dynaMat, failJohnsonEntEdit, "FAIL", "D1");
            failJohnsonEntEdit.SetValue(sdiIdentifier("IFAIL_SH"), sdiValue(2));
        }
    }
    else
    {
        ContainDoubleVsDouble mapEPSVsES = { {lsdEPS1,lsdES1},{lsdEPS2,lsdES2},{lsdEPS3,lsdES3},{lsdEPS4,lsdES4},{lsdEPS5,lsdES5},{lsdEPS6,lsdES6},{lsdEPS7,lsdES7},{lsdEPS8,lsdES8}};

        sdiDoubleList lsdEPSList;
        sdiDoubleList lsdESList;
        int tempIndex = 0;
        for (const auto epsEsPair : mapEPSVsES)
        {
            const double lsdEps = epsEsPair.first;
            const double lsdEs = epsEsPair.second;

            if (tempIndex == 0 || (lsdEs != 0.0 && lsdEps != 0.0))
            {
                lsdESList.push_back(lsdEs);
                lsdEPSList.push_back(lsdEps);
            }
            ++tempIndex;
        }
        size_t epsEsCount = lsdESList.size();

        if (epsEsCount >= 2)
        {
            sdiDoubleList pntsToCreateCrv;
            pntsToCreateCrv.reserve(2 * epsEsCount + 2);
            if (lsdC > 0.0 && lsdP > 0.0 && lsdSIGY > 0.0 && lsdEPSList[0] > 0.0)
            {
                pntsToCreateCrv.push_back(0.0);
                pntsToCreateCrv.push_back(lsdSIGY);
            }
            for (size_t i = 0; i < epsEsCount; ++i)
            {
                pntsToCreateCrv.push_back(lsdEPSList[i]);
                pntsToCreateCrv.push_back(lsdESList[i]);
            }
            HandleEdit epsEsCurveHEdit;
            p_ConvertUtils.CreateCurve(dynaMatName, (int)pntsToCreateCrv.size() / 2, pntsToCreateCrv, epsEsCurveHEdit);

            if (epsEsCurveHEdit.IsValid())
            {
                SDIHandlReadList sourceMats = { {dynaMat.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(epsEsCurveHEdit, sourceMats));
                unsigned int epsEsCurveId = epsEsCurveHEdit.GetId(p_radiossModel);
                if (lsdC > 0.0 && lsdP > 0.0)
                {
                    p_ConvertToMatLAW44(dynaMat, destCard, attribMap, radMat);
                    matLawNumChoice = 44;
                    radMat.SetEntityHandle(p_radiossModel, sdiIdentifier("fct_IDp"), epsEsCurveHEdit);
                    radMat.SetValue(p_radiossModel, sdiIdentifier("Optional_card"), sdiValue(1)); 
//-------
                    if(matLawNum != 24) radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue((int)lsdVp));

                    if(lsdVp == -1.0f && matLawNum == 24)
                    {
                        radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(3));
                        radMat.SetValue(p_radiossModel, sdiIdentifier("ISMOOTH"), sdiValue(1));
                    }
                    if((lsdVp == 0.f || lsdVp == 3.0f) && matLawNum == 24)
                    {
                        radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(0));
                        radMat.SetValue(p_radiossModel, sdiIdentifier("ISMOOTH"), sdiValue(1));
                    }
                    if(lsdVp == 1.0f && matLawNum == 24)
                    {
                        radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(1));
                        radMat.SetValue(p_radiossModel, sdiIdentifier("ISMOOTH"), sdiValue(0));
                    }

                    if (lsdFAIL > 0.0 && matLawNum == 24)
                    {
                         p_radiossModel->CreateEntity(failJohnsonHEdit, "/FAIL/JOHNSON", dynaMatName);
                         //failJohnsonHEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
                         EntityEdit failJohnsonEntEdit(p_radiossModel, failJohnsonHEdit);
                         failJohnsonEntEdit.SetEntityHandle(sdiIdentifier("mat_id"), radMat);
                         p_ConvertUtils.CopyValue(dynaMat, failJohnsonEntEdit, "FAIL", "D1");
                         failJohnsonEntEdit.SetValue(sdiIdentifier("IFAIL_SH"), sdiValue(2));
                    }
//-------
                    return;
                }

                if (matToPropsReferenceCount > 1)
                    p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
                else
                    p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMat.GetId());
                if (lcsrId)
                {
                    int nPnts = 0;
                    sdiValue tempValue(nPnts);
                    lcsrHandle.GetValue(p_lsdynaModel, sdiIdentifier("numberofpoints"), tempValue);
                    tempValue.GetValue(nPnts);

                    sdiDoubleList crvPoints;
                    tempValue = sdiValue(crvPoints);
                    lcsrHandle.GetValue(p_lsdynaModel, sdiIdentifier("points"), tempValue);
                    tempValue.GetValue(crvPoints);
                    int numVals = nPnts * 2;
                    for (int i = 0; i < numVals; ++i)
                    {
                        if (i % 2 == 0)
                            strainRatesList.push_back(crvPoints[i]);
                        else
                            scaleList.push_back(crvPoints[i]);
                        if (i == numVals - 2)
                            strainRatesList.push_back(2 * crvPoints[i]);
                        if (i == numVals - 1)
                            scaleList.push_back(crvPoints[i]);
                    }
                    nFunct = nPnts + 1;
                    entList = sdiUIntList(nFunct, epsEsCurveId);
                }
                else
                {
                    strainRatesList.push_back(0.0);
                    scaleList.push_back(1.0);
                    entList.push_back(epsEsCurveId);
                    nFunct = 1;
                }
            }

            if(matLawNum != 24) radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue((int)lsdVp));
            matLawNumChoice = 36;

            sdiString keyWordLog = dynaMat.GetKeyword();
            if((lsdVp == -1.0f || lsdVp == 0.f || lsdVp == 3.0f || lsdVp == 1.0f) && matLawNum == 24)
            {
                radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(0));
                if(lsdVp == 1.0f) radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(1));
                if ((keyWordLog.find("LOG_INTERPOLATION") != keyWordLog.npos) || negstrains)
                {
                    radMat.SetValue(p_radiossModel, sdiIdentifier("F_smooth"), sdiValue(2));
                }
                else
                {
                    radMat.SetValue(p_radiossModel, sdiIdentifier("F_smooth"), sdiValue(1));
                }
            }

            if (lsdFAIL > 0.0 && matLawNum == 24)
            {
                p_radiossModel->CreateEntity(failJohnsonHEdit, "/FAIL/JOHNSON", dynaMatName);
                //failJohnsonHEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
                EntityEdit failJohnsonEntEdit(p_radiossModel, failJohnsonHEdit);
                failJohnsonEntEdit.SetEntityHandle(sdiIdentifier("mat_id"), radMat);
                p_ConvertUtils.CopyValue(dynaMat, failJohnsonEntEdit, "FAIL", "D1");
                failJohnsonEntEdit.SetValue(sdiIdentifier("IFAIL_SH"), sdiValue(2));
            }
        }
        else
        {
            p_ConvertToMatLAW44(dynaMat, destCard, attribMap, radMat);
            matLawNumChoice = 44;

            if(matLawNum != 24) radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue((int)lsdVp));

            if(lsdVp == -1.0f && matLawNum == 24)
            {
                radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(3));
                radMat.SetValue(p_radiossModel, sdiIdentifier("ISMOOTH"), sdiValue(1));
            }
            if((lsdVp == 0.f || lsdVp == 3.0f) && matLawNum == 24)
            {
                radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(0));
                radMat.SetValue(p_radiossModel, sdiIdentifier("ISMOOTH"), sdiValue(1));
            }
            if(lsdVp == 1.0f && matLawNum == 24)
            {
                radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(1));
                radMat.SetValue(p_radiossModel, sdiIdentifier("ISMOOTH"), sdiValue(0));
            }
//-------
            if (lsdFAIL > 0.0 && matLawNum == 24)
            {
                p_radiossModel->CreateEntity(failJohnsonHEdit, "/FAIL/JOHNSON", dynaMatName);
                //failJohnsonHEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
                EntityEdit failJohnsonEntEdit(p_radiossModel, failJohnsonHEdit);
                failJohnsonEntEdit.SetEntityHandle(sdiIdentifier("mat_id"), radMat);
                p_ConvertUtils.CopyValue(dynaMat, failJohnsonEntEdit, "FAIL", "D1");
                failJohnsonEntEdit.SetValue(sdiIdentifier("IFAIL_SH"), sdiValue(2));
                radMat.SetValue(p_radiossModel, sdiIdentifier("EPS_MAX"), sdiValue(0.0));
            }
//-------
            return;
        }
    }
    if (failJohnsonHEdit.IsValid())
    {
        sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(failJohnsonHEdit, sourcemat));
    }
    EntityEdit radMatEdit(p_radiossModel, radMat);
    radMatEdit.SetValue(sdiIdentifier("N_funct"), sdiValue((int)nFunct));
    if(entList.size())
        radMatEdit.SetValue(sdiIdentifier("func_ID"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), entList)));
    radMatEdit.SetValue(sdiIdentifier("Fscale"), sdiValue(scaleList));
    radMatEdit.SetValue(sdiIdentifier("Eps_dot"), sdiValue(strainRatesList));
    //if (nFunct > 1)
    //    radMatEdit.SetValue(sdiIdentifier("F_smooth"), sdiValue(1));
}

void ConvertMat::p_ConvertMatL26(const EntityRead& dynaMat, HandleEdit& radMat)
{
    double lsdE;
    double lsdPR;
    double lsdEAAU;
    double lsdEBBU;
    double lsdECCU;
    double lsdGABU;
    double lsdGBCU;
    double lsdGCAU;

    vector<reference_wrapper<double>> attribVals({ lsdE, lsdPR, lsdEAAU, lsdEBBU,
                                                   lsdECCU,lsdGABU, lsdGBCU, lsdGCAU });
    vector<sdiString> attribNames({ "E", "PR", "EAAU", "EBBU",
                                    "ECCU","GABU", "GBCU", "GCAU" });
    p_ConvertUtils.GetAttribValues(dynaMat, attribNames, attribVals);

    EntityId dynaMatId = dynaMat.GetId();
    sdiString dynaMatName = dynaMat.GetName();
    destCard = "/MAT/LAW50";

    int lsdLCAoption = 0;
    sdiValue tempValue;
    dynaMat.GetValue(sdiIdentifier("LCAoption"), tempValue);
    tempValue.GetValue(lsdLCAoption);


    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);

    vector<double> substitueVals({ lsdE, lsdE / 2 / (1 + lsdPR) });
    vector<vector<sdiString>> radttrNames({ { {"E11", "E22", "E33"} }, { {"G12", "G23", "G31"} } });

    vector<vector<double>> lsdVals;

    if (lsdLCAoption > 0)
    {
        lsdVals = { { {lsdEAAU, lsdEBBU, lsdEBBU} } , {lsdGBCU, lsdGABU, lsdGABU} };
    }
    else 
    { 
        lsdVals = { { {lsdEAAU, lsdEBBU, lsdECCU} } , {lsdGABU, lsdGBCU, lsdGCAU} };
    }

    for (size_t i = 0; i < 2; ++i)
    {
        for (size_t j = 0; j < 3; ++j)
        {
            double radValue = lsdVals[i][j];
            if (0.0 == radValue)
                radValue = substitueVals[i];
            radmatEntityEdit.SetValue(sdiIdentifier(radttrNames[i][j]), sdiValue(radValue));
        }
    }

    if (lsdLCAoption > 0)
    {
    
        radmatEntityEdit.SetValue(sdiIdentifier("Iflag1"), sdiValue(0));
        radmatEntityEdit.SetValue(sdiIdentifier("Iflag2"), sdiValue(1));
    }
    else
    {
        radmatEntityEdit.SetValue(sdiIdentifier("Iflag1"), sdiValue(-1));
        radmatEntityEdit.SetValue(sdiIdentifier("Iflag2"), sdiValue(-1));
    }

    UpdateMatConvertingLoadCurves(dynaMat, p_lsdynaModel, p_radiossModel, p_ConvertUtils, radmatEntityEdit, lsdLCAoption);
}


void ConvertMat::p_ConvertMatL27(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    HandleRead CrvHandle;
    dynaMat.GetEntityHandle(sdiIdentifier("LCID"), CrvHandle);
    
    if (!CrvHandle.IsValid())
    {
        destCard = "/MAT/LAW42";
        if (matToPropsReferenceCount > 1)
            p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
        else
            p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
        attribMap = { {"RHO", "RHO_I"},{"PR", "Nu"} };
        EntityEdit radmatEntityEdit(p_radiossModel, radMat);
        double lsdA, lsdB, Mu, lsdPR;
        vector<reference_wrapper<double>> attrValList = { lsdA, lsdB, lsdPR };
        vector<sdiString> attrNameList = { "A","B","PR"};
        p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

        Mu = lsdA*2;
        radmatEntityEdit.SetValue(sdiIdentifier("Mu_1"), sdiValue(Mu));
        Mu = lsdB*-2;
        radmatEntityEdit.SetValue(sdiIdentifier("Mu_2"), sdiValue(Mu));
        radmatEntityEdit.SetValue(sdiIdentifier("alpha_1"), sdiValue(2.0));
        radmatEntityEdit.SetValue(sdiIdentifier("alpha_2"), sdiValue(-2.0));
      
        double mu_p;
        mu_p = (2*lsdA*2 + -2*lsdB*-2)/2;

        double K;
        K = (2* mu_p *(1+lsdPR))/(3*(1-2* lsdPR));

        double D;
        D = (lsdA*(5*lsdPR-2)+lsdB*(11*lsdPR-5))/(2*(1-2*lsdPR));

        double j = 0.01;
        double fbulk;
        sdiDoubleList pointsList;

        for (int i = 0; i < 500 ; ++i)
        {
            fbulk=(2*lsdA*(pow(j,(-1/3))-pow(j,-5)) + 4*lsdB*(pow(j,(1/3))-pow(j,-5)) + 4*D*j*(pow(j,2)-1))/(K*(j-1));
            pointsList.push_back(j);
            pointsList.push_back(fbulk);
            j=(double)j+(double)0.01;
        }
        HandleEdit tempCrvEdit;
        p_ConvertUtils.CreateCurve(dynaMat.GetName(),500, pointsList, tempCrvEdit);
        radmatEntityEdit.SetEntityHandle(sdiIdentifier("funIDbulk"), tempCrvEdit);
        if (tempCrvEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(tempCrvEdit, sourcemat));
        }
    }
    else
    {
        destCard = "/MAT/LAW69";
        if (matToPropsReferenceCount > 1)
            p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
        else
            p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
        EntityEdit radmatEntityEdit(p_radiossModel, radMat);
        attribMap = { {"RHO", "RHO_I"},{"PR", "NU"} }; 
        radmatEntityEdit.SetValue(sdiIdentifier("LAW_ID"), sdiValue(2));

        HandleRead lcdiHandle;
        dynaMat.GetEntityHandle(sdiIdentifier("LCID"), lcdiHandle);
        if (lcdiHandle.IsValid())
        {
            radmatEntityEdit.SetValue(sdiIdentifier("FCT_ID1"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), lcdiHandle.GetId(p_lsdynaModel))));
        }
    }

}

void ConvertMat::p_ConvertMatL30(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    destCard = "/MAT/LAW71";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());

    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "RHO", "RHO_I");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "E", "E");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "PR", "nu");

    double lsdSIG_ASS = 0;
    sdiValue tempValue;
    dynaMat.GetValue(sdiIdentifier("SIG_ASS"), tempValue);
    tempValue.GetValue(lsdSIG_ASS);
    if (lsdSIG_ASS > 0.0) p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "SIG_ASS", "Sig_sas");

    double lsdSIG_ASF = 0;
    dynaMat.GetValue(sdiIdentifier("SIG_ASF"), tempValue);
    tempValue.GetValue(lsdSIG_ASF);
    if (lsdSIG_ASF > 0.0) p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "SIG_ASF", "Sig_fas");

    double lsdSIG_SAS = 0;
    dynaMat.GetValue(sdiIdentifier("SIG_SAS"), tempValue);
    tempValue.GetValue(lsdSIG_SAS);
    if (lsdSIG_SAS > 0.0) p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "SIG_SAS", "Sig_ssa");

    double lsdSIG_SAF = 0;
    dynaMat.GetValue(sdiIdentifier("SIG_SAF"), tempValue);
    tempValue.GetValue(lsdSIG_SAF);
    if (lsdSIG_SAF > 0.0) p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "SIG_SAF", "Sig_fsa");

    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "EPSL", "EpsL");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "YMTR", "E_mart");

    p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "sqrt(2/3)*ALPHA", "alpha");

    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
    sdiConvert::Convert::PushToConversionLog(std::make_pair(radMat, sourcemat));
}

void ConvertMat::p_ConvertMatL32(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radGlassMat)
{
    double lsdRho; 
    double lsdEG;
    double lsdPRG;
    double lsdSYG;
    double lsdETG;
    double lsdEFG;
    double lsdEP;
    double lsdPRP;
    double lsdSYP;
    double lsdETP;
    int arrayCount = 0;
    sdiDoubleList lsdFi;
    HandleEdit radPolymerMat;

    vector<reference_wrapper<double>> attrValList = { lsdRho, lsdEG, lsdPRG, lsdSYG, lsdETG, lsdEFG, lsdEP, lsdPRP, lsdSYP, lsdETP };
    vector<sdiString> attrNameList = { "RHO","EG", "PRG","SYG","ETG","EFG", "EP","PRP","SYP","ETP" };

    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

    sdiValue tempValue(arrayCount);
    dynaMat.GetValue(sdiIdentifier("ArrayCount"), tempValue);
    tempValue.GetValue(arrayCount);

    dynaMat.GetValue(sdiIdentifier("F"), tempValue);
    tempValue.GetValue(lsdFi);

    destCard = "/MAT/PLAS_BRIT";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radPolymerMat, destCard, dynaMatName + "- polymer", p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radPolymerMat, destCard, dynaMatName + "- polymer", dynaMatId);
    p_radiossModel->CreateEntity(radGlassMat, destCard, dynaMatName + "- Glass", p_ConvertUtils.GetRadiossMaxEntityID(p_radiossModel->GetEntityType("/MAT")));
    if (radGlassMat.IsValid())
    {

        // Glass Material
        EntityEdit radGlassMatEdit(p_radiossModel, radGlassMat);
        radGlassMatEdit.SetValue(sdiIdentifier("RHO_I"), sdiValue(lsdRho));
        radGlassMatEdit.SetValue(sdiIdentifier("E"), sdiValue(lsdEG));
        radGlassMatEdit.SetValue(sdiIdentifier("NU"), sdiValue(lsdPRG));
        radGlassMatEdit.SetValue(sdiIdentifier("a"), sdiValue(lsdSYG));
        radGlassMatEdit.SetValue(sdiIdentifier("b"), sdiValue(lsdETG));
        radGlassMatEdit.SetValue(sdiIdentifier("n"), sdiValue(1));
        radGlassMatEdit.SetValue(sdiIdentifier("EPS_t1"), sdiValue(lsdEFG));
        radGlassMatEdit.SetValue(sdiIdentifier("EPS_t2"), sdiValue(lsdEFG));
        radGlassMatEdit.SetValue(sdiIdentifier("EPS_m1"), sdiValue(lsdEFG + 0.05));
        radGlassMatEdit.SetValue(sdiIdentifier("EPS_m2"), sdiValue(lsdEFG + 0.05));
        radGlassMatEdit.SetValue(sdiIdentifier("EPS_f1"), sdiValue(lsdEFG + 0.1));
        radGlassMatEdit.SetValue(sdiIdentifier("EPS_f2"), sdiValue(lsdEFG + 0.1));
    }

    if (radPolymerMat.IsValid())
    {
        sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(radPolymerMat, sourcemats));
        // Polymer Material
        EntityEdit radPolymerMatEdit(p_radiossModel, radPolymerMat);
        radPolymerMatEdit.SetValue(sdiIdentifier("RHO_I"), sdiValue(lsdRho));
        radPolymerMatEdit.SetValue(sdiIdentifier("E"), sdiValue(lsdEP));
        radPolymerMatEdit.SetValue(sdiIdentifier("NU"), sdiValue(lsdPRP));
        radPolymerMatEdit.SetValue(sdiIdentifier("a"), sdiValue(lsdSYP));
        radPolymerMatEdit.SetValue(sdiIdentifier("b"), sdiValue(lsdETP));
        radPolymerMatEdit.SetValue(sdiIdentifier("n"), sdiValue(1));
    }
}

void ConvertMat::p_ConvertMatL34(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();

    EntityType radFunctEntType = p_radiossModel->GetEntityType("/FUNCT");

    sdiValue tempValue;
    double lsdFORM;
    tempValue = sdiValue(lsdFORM);
    dynaMat.GetValue(sdiIdentifier("FORM"), tempValue);
    tempValue.GetValue(lsdFORM);

    //---------------
    // check if curves LCA, LCB, LCAB, LCUA, LCUB, LCUAB are defined
    //---------------

    sdiValueEntity lsdLCAId = GetValue<sdiValueEntity>(dynaMat, "LCA");
    unsigned int LCA_ID=lsdLCAId.GetId();

    sdiValueEntity lsdLCBId = GetValue<sdiValueEntity>(dynaMat, "LCB");
    unsigned int LCB_ID=lsdLCBId.GetId();

    sdiValueEntity lsdLCABId = GetValue<sdiValueEntity>(dynaMat, "LCAB");
    unsigned int LCAB_ID=lsdLCABId.GetId();

    sdiValueEntity lsdLCUAId = GetValue<sdiValueEntity>(dynaMat, "LCUA");
    unsigned int LCUA_ID=lsdLCUAId.GetId();

    sdiValueEntity lsdLCUBId = GetValue<sdiValueEntity>(dynaMat, "LCUB");
    unsigned int LCUB_ID=lsdLCUBId.GetId();

    sdiValueEntity lsdLCUABId = GetValue<sdiValueEntity>(dynaMat, "LCUAB");
    unsigned int LCUAB_ID=lsdLCUABId.GetId();
    //---------------
    
    if((lsdFORM != 14.0 && lsdFORM != -14.0) || (lsdFORM == 14.0 || lsdFORM == -14.0) && 
       (LCA_ID == 0 && LCB_ID == 0 && LCAB_ID == 0 && LCUA_ID == 0 && LCUB_ID == 0 && LCUAB_ID == 0))
    {
      //---
      //--- convert to /MAT/FABRI ( /MAT/LAW19 )
      //---
      attribMap = {{"RO", "RHO_I"}, {"EA", "E11"}, {"LSD_MAT_PRBA", "NU12"}};
      destCard = "/MAT/FABRI";

      if (matToPropsReferenceCount > 1)
          p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
      else
          p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);

      double lsdGAB;
      double lsdGBC;
      double lsdGCA;
      double lsdCSE;
      // double lsdEA;
      double lsdEB;
      double lsdPRBA;
    
      vector<reference_wrapper<double>> attrValList = {/*lsdEA,*/ lsdEB,lsdPRBA, lsdGAB, lsdGBC, lsdGCA, lsdCSE};
      vector<sdiString> attrNameList = {/*"EA",*/"EB","LSD_MAT_PRBA", "GAB","LSD_MAT_GBC","LSD_MAT_GCA","CSE"};
      p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

      EntityEdit radmatEntityEdit(p_radiossModel, radMat);

      /* Legacy code without parameterization kept here for reference:
      if (lsdGAB == 0.0)
          lsdGAB = lsdEA * 2 * (1+ lsdPRBA);

      if (lsdGBC == 0.0)
          lsdGBC= lsdGAB;

      if (lsdGCA == 0.0)
          lsdGCA = lsdGAB;

      if (lsdEB == 0.0)
          lsdEB = lsdEA ;
      */

      radmatEntityEdit.SetValue(sdiIdentifier("R_E"), sdiValue((lsdCSE == 0) ? 0.0 : 0.01));

      /* TBD: parameterized GAB? To be exact, we should have this here, to be discussed with PM:
      if(dynaMat.IsParameterized(sdiIdentifier("GAB")))
      {
          p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit,
          "(GAB!=0)*GAB+(GAB==0)*EA*2*(1+LSD_MAT_PRBA)", "G12");
      }
      else */
      if(lsdGAB != 0.0) p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "GAB", "G12");
      else p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "EA/(2*(1+PRBA))", "G12");

      // TBD: parameterized GBC and/or GAB, cf. above?
      if(lsdGBC != 0.0) p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "LSD_MAT_GBC", "G23");
      else if(lsdGAB != 0.0) p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "GAB", "G23");
      else p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "EA/(2*(1+PRBA))", "G23");

      // TBD: parameterized GCA and/or GAB, cf. above?
      if(lsdGCA != 0.0) p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "LSD_MAT_GCA", "G31");
      else if(lsdGAB != 0.0) p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "GAB", "G31");
      else p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "EA/(2*(1+PRBA))", "G31");

      radmatEntityEdit.SetValue(sdiIdentifier("ZEROSTRESS"), sdiValue(1.0)); 

      // TBD: parameterized EB?
      if(lsdEB != 0.0) p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "EB", "E22");
      else p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "EA", "E22");

      //The rest of parameters are set to default 0 values in converson 
      radmatEntityEdit.SetValue(sdiIdentifier("FSCALE_POR"), sdiValue(0.0)); 
    }
    else if(lsdFORM == 14.0 || lsdFORM == -14.0)
    {
      //---
      //--- convert to /MAT/LAW58
      //---
      destCard = "/MAT/LAW58";

      if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
      else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);

      EntityEdit radmatEntityEdit(p_radiossModel, radMat);
      EntityType radPropType = p_radiossModel->GetEntityType("/PROP");

      double lsdEA;
      double lsdEB;
      double lsdGAB;
      double lsdCSE;

      //attribMap = { { "Rho", "RHO_I" } };
      vector<reference_wrapper<double>> attrValList = {/*lsdEA,*/ lsdEB, lsdGAB, lsdCSE};
      vector<sdiString> attrNameList = {/*"EA",*/"EB", "GAB","CSE"};
      p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

      p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "Rho", "RHO_I");
      p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "EA", "E1");
      // TBD: parameterized EB?
      if(lsdEB != 0.0) p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "EB", "E2");
      else p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "EA", "E1");

      if(lsdGAB != 0.0) p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "GAB", "G0");
      else
      {
         p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "EA/2", "G0"); // ??? to be checked (with spec)
         p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "EA", "GT");
         radmatEntityEdit.SetValue(sdiIdentifier("AlphaT"), sdiValue(18.0)); 
      }

      radmatEntityEdit.SetValue(sdiIdentifier("Flex"), sdiValue((lsdCSE == 0) ? 1.0 : 0.01));
      radmatEntityEdit.SetValue(sdiIdentifier("ZERO_STRESS"), sdiValue(1.0));
      radmatEntityEdit.SetValue(sdiIdentifier("S1"), sdiValue(0.0001));
      radmatEntityEdit.SetValue(sdiIdentifier("S2"), sdiValue(0.0001));


      sdiDoubleList temp2Points;
      // Functions 1-6:

      
      HandleRead lsdLCAHandle;
      dynaMat.GetEntityHandle(sdiIdentifier("LCA"), lsdLCAHandle);

      if (lsdLCAHandle.IsValid())
      {
          
          sdiUIntList lcaCrvList;
          EntityRead lsdLCAEntRead(p_lsdynaModel, lsdLCAHandle);
          sdiString lsdLCAType = lsdLCAEntRead.GetKeyword();
          EntityId lsdLCAId = lsdLCAEntRead.GetId();

          if(!lsdLCAType.compare(0, 13, "*DEFINE_CURVE"))
          {
              int nPnts = 0;
              sdiDoubleList crvPoints;

              double lsdSFA = 1.0;
              double lsdSFO = 1.0;
              double lsdOFFA = 0.0;
              double lsdOFFO = 0.0;

              tempValue = sdiValue(crvPoints);
              lsdLCAEntRead.GetValue(sdiIdentifier("points"), tempValue);
              tempValue.GetValue(crvPoints);

              tempValue = sdiValue(nPnts);
              lsdLCAEntRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
              tempValue.GetValue(nPnts);
              crvPoints.reserve(2 * nPnts + 2);

              vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
              vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
              p_ConvertUtils.GetAttribValues(lsdLCAEntRead, lsdQueryAttribs, attribVals);
              lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
              lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
              //-----------------------
              // Ignore negative part of curve, if it is available set FLEX1=0.01
              temp2Points.clear();
              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  temp2Points.push_back(crvPoints[j]);
                  temp2Points.push_back(crvPoints[j+1]);
              }
              crvPoints.clear();
              for (size_t j = 0; j < temp2Points.size(); j += 2)
              {
                  if (temp2Points[j] >= 0.0 && temp2Points[j+1] >= 0.0)
                  {
                       crvPoints.push_back(temp2Points[j]);
                       crvPoints.push_back(temp2Points[j+1]);
                  }
              }

              // abscisa:  STRAIN_RADIOSS=LN(1+STRAIN_DYNA)
              // ordonate: STRESS_RADIOSS=STRESS_DYNA*(1+STRAIN_DYNA)
              temp2Points.clear();
              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  temp2Points.push_back(crvPoints[j]);
                  temp2Points.push_back(crvPoints[j+1]);
              }
              crvPoints.clear();
              for (size_t j = 0; j < temp2Points.size(); j += 2)
              {
                 double ldsStrain,radStrain;
                 double ldsStress,radStress;
                 ldsStrain = temp2Points[j];
                 radStrain = log(1+ldsStrain);
                 ldsStress = temp2Points[j+1];
                 radStress = ldsStress*(1+ldsStrain);
                 crvPoints.push_back(radStrain);
                 crvPoints.push_back(radStress);
              }

              // cut monotonicaly function when it descends
              temp2Points.clear();
              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  temp2Points.push_back(crvPoints[j]);
                  temp2Points.push_back(crvPoints[j+1]);
              }
              crvPoints.clear();
              double minordonate = temp2Points[1];
              crvPoints.push_back(temp2Points[0]);
              crvPoints.push_back(temp2Points[1]);
              for (size_t j = 2; j < temp2Points.size(); j += 2)
              {
                  if (temp2Points[j+1] > minordonate)
                  {
                      minordonate = temp2Points[j+1];
                      crvPoints.push_back(temp2Points[j]);
                      crvPoints.push_back(temp2Points[j+1]);
                  }
                  else
                  {
                      break;
                  }
              }
              //-----------------------
              HandleEdit functHEdit;
              p_ConvertUtils.CreateCurve("Recalculated_" + to_string(lsdLCAId) + "_MatL34_" + to_string(dynaMatId),
                                        (int)crvPoints.size() / 2, crvPoints, functHEdit, lsdSFA, lsdSFO, lsdOFFA);
              if (functHEdit.IsValid())
              {
                 radmatEntityEdit.SetEntityHandle(sdiIdentifier("FUN_A1"), functHEdit);
                 sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
                 sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
              }

              radmatEntityEdit.SetValue(sdiIdentifier("FLEX1"), sdiValue(0.01));
          }
          else if (!lsdLCAType.compare(0, 13, "*DEFINE_TABLE"))
          {
          // take first table function

              sdiValueEntityList curveList;
              sdiValue tempValue(curveList);
              lsdLCAEntRead.GetValue(sdiIdentifier("CurveIds"), tempValue);
              tempValue.GetValue(curveList);
              curveList.GetIdList(lcaCrvList);
              lsdLCAId = lcaCrvList[0];

              HandleRead functLCAHRead;
              p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*DEFINE_CURVE"), lsdLCAId, functLCAHRead);
              EntityRead lsdLCAEntRead(p_lsdynaModel, functLCAHRead);

              int nPnts = 0;
              sdiDoubleList crvPoints;

              double lsdSFA = 1.0;
              double lsdSFO = 1.0;
              double lsdOFFA = 0.0;
              double lsdOFFO = 0.0;

              tempValue = sdiValue(crvPoints);
              lsdLCAEntRead.GetValue(sdiIdentifier("points"), tempValue);
              tempValue.GetValue(crvPoints);

              tempValue = sdiValue(nPnts);
              lsdLCAEntRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
              tempValue.GetValue(nPnts);
              crvPoints.reserve(2 * nPnts + 2);

              vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
              vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
              p_ConvertUtils.GetAttribValues(lsdLCAEntRead, lsdQueryAttribs, attribVals);
              lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
              lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
              //-----------------------
              // Ignore negative part of curve, if it is available set FLEX1=0.01
              temp2Points.clear();
              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  temp2Points.push_back(crvPoints[j]);
                  temp2Points.push_back(crvPoints[j+1]);
              }
              crvPoints.clear();
              for (size_t j = 0; j < temp2Points.size(); j += 2)
              {
                  if (temp2Points[j] >= 0.0 && temp2Points[j+1] >= 0.0)
                  {
                       crvPoints.push_back(temp2Points[j]);
                       crvPoints.push_back(temp2Points[j+1]);
                  }
              }

              // abscisa:  STRAIN_RADIOSS=LN(1+STRAIN_DYNA)
              // ordonate: STRESS_RADIOSS=STRESS_DYNA*(1+STRAIN_DYNA)
              temp2Points.clear();
              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  temp2Points.push_back(crvPoints[j]);
                  temp2Points.push_back(crvPoints[j+1]);
              }
              crvPoints.clear();
              for (size_t j = 0; j < temp2Points.size(); j += 2)
              {
                 double ldsStrain,radStrain;
                 double ldsStress,radStress;
                 ldsStrain = temp2Points[j];
                 radStrain = log(1+ldsStrain);
                 ldsStress = temp2Points[j+1];
                 radStress = ldsStress*(1+ldsStrain);
                 crvPoints.push_back(radStrain);
                 crvPoints.push_back(radStress);
              }

              // cut monotonicaly function when it descends
              temp2Points.clear();
              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  temp2Points.push_back(crvPoints[j]);
                  temp2Points.push_back(crvPoints[j+1]);
              }
              crvPoints.clear();
              double minordonate = temp2Points[1];
              crvPoints.push_back(temp2Points[0]);
              crvPoints.push_back(temp2Points[1]);
              for (size_t j = 2; j < temp2Points.size(); j += 2)
              {
                  if (temp2Points[j+1] > minordonate)
                  {
                      minordonate = temp2Points[j+1];
                      crvPoints.push_back(temp2Points[j]);
                      crvPoints.push_back(temp2Points[j+1]);
                  }
                  else
                  {
                      break;
                  }
              }
              //-----------------------
              HandleEdit functHEdit;
              p_ConvertUtils.CreateCurve("Recalculated_" + to_string(lsdLCAId) + "_MatL34_" + to_string(dynaMatId),
                                        (int)crvPoints.size() / 2, crvPoints, functHEdit, lsdSFA, lsdSFO, lsdOFFA);
              if (functHEdit.IsValid())
              {
                 radmatEntityEdit.SetEntityHandle(sdiIdentifier("FUN_A1"), functHEdit);
                 sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
                 sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
              }

              radmatEntityEdit.SetValue(sdiIdentifier("FLEX1"), sdiValue(0.01));
          }
      }
      else
      {
      // create a new one
          tempValue = sdiValue(lsdEA);
          dynaMat.GetValue(sdiIdentifier("EA"), tempValue);
          tempValue.GetValue(lsdEA);

          sdiDoubleList abscissaList({ -1.0, 0.0, 1.0 });
          sdiDoubleList ordonateList({ -lsdEA, 0.0, lsdEA });
          sdiDoubleList pointsList;
          pointsList.reserve(2 * abscissaList.size());
          for (int j = 0; j < abscissaList.size(); ++j)
          {
              pointsList.push_back(abscissaList[j]);
              pointsList.push_back(ordonateList[j]);
          }
          HandleEdit functHEdit;
          p_ConvertUtils.CreateCurve(dynaMatName, (int)abscissaList.size(), pointsList, functHEdit);
          if (functHEdit.IsValid())
          {
             radmatEntityEdit.SetEntityHandle(sdiIdentifier("FUN_A1"), functHEdit);
             sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
             sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
          }
      }

      
      HandleRead lsdLCBHandle;
      dynaMat.GetEntityHandle(sdiIdentifier("LCB"), lsdLCBHandle);

      if (lsdLCBHandle.IsValid())
      {
          
          sdiUIntList lcbCrvList;
          EntityRead lsdLCBEntRead(p_lsdynaModel, lsdLCBHandle);
          sdiString lsdLCBType = lsdLCBEntRead.GetKeyword();
          EntityId lsdLCBId = lsdLCBEntRead.GetId();
          if(!lsdLCBType.compare(0, 13, "*DEFINE_CURVE"))
          {
              int nPnts = 0;
              sdiDoubleList crvPoints;

              double lsdSFA = 1.0;
              double lsdSFO = 1.0;
              double lsdOFFA = 0.0;
              double lsdOFFO = 0.0;

              tempValue = sdiValue(crvPoints);
              lsdLCBEntRead.GetValue(sdiIdentifier("points"), tempValue);
              tempValue.GetValue(crvPoints);

              tempValue = sdiValue(nPnts);
              lsdLCBEntRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
              tempValue.GetValue(nPnts);
              crvPoints.reserve(2 * nPnts + 2);

              vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
              vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
              p_ConvertUtils.GetAttribValues(lsdLCBEntRead, lsdQueryAttribs, attribVals);
              lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
              lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
              //-----------------------
              // Ignore negative part of curve, if it is available set FLEX1=0.01
              temp2Points.clear();
              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  temp2Points.push_back(crvPoints[j]);
                  temp2Points.push_back(crvPoints[j+1]);
              }
              crvPoints.clear();
              for (size_t j = 0; j < temp2Points.size(); j += 2)
              {
                  if (temp2Points[j] >= 0.0 && temp2Points[j+1] >= 0.0)
                  {
                       crvPoints.push_back(temp2Points[j]);
                       crvPoints.push_back(temp2Points[j+1]);
                  }
              }

              // abscisa:  STRAIN_RADIOSS=LN(1+STRAIN_DYNA)
              // ordonate: STRESS_RADIOSS=STRESS_DYNA*(1+STRAIN_DYNA)
              temp2Points.clear();
              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  temp2Points.push_back(crvPoints[j]);
                  temp2Points.push_back(crvPoints[j+1]);
              }
              crvPoints.clear();
              for (size_t j = 0; j < temp2Points.size(); j += 2)
              {
                 double ldsStrain,radStrain;
                 double ldsStress,radStress;
                 ldsStrain = temp2Points[j];
                 radStrain = log(1+ldsStrain);
                 ldsStress = temp2Points[j+1];
                 radStress = ldsStress*(1+ldsStrain);
                 crvPoints.push_back(radStrain);
                 crvPoints.push_back(radStress);
              }

              // cut monotonicaly function when it descends
              temp2Points.clear();
              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  temp2Points.push_back(crvPoints[j]);
                  temp2Points.push_back(crvPoints[j+1]);
              }
              crvPoints.clear();
              double minordonate = temp2Points[1];
              crvPoints.push_back(temp2Points[0]);
              crvPoints.push_back(temp2Points[1]);
              for (size_t j = 2; j < temp2Points.size(); j += 2)
              {
                  if (temp2Points[j+1] > minordonate)
                  {
                      minordonate = temp2Points[j+1];
                      crvPoints.push_back(temp2Points[j]);
                      crvPoints.push_back(temp2Points[j+1]);
                  }
                  else
                  {
                      break;
                  }
              }
              //-----------------------
              HandleEdit functHEdit;
              p_ConvertUtils.CreateCurve("Recalculated_" + to_string(lsdLCBId) + "_MatL34_" + to_string(dynaMatId),
                                        (int)crvPoints.size() / 2, crvPoints, functHEdit, lsdSFA, lsdSFO, lsdOFFA);
              if (functHEdit.IsValid())
              {
                 radmatEntityEdit.SetEntityHandle(sdiIdentifier("FUN_A2"), functHEdit);
                 sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
                 sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
              }

              radmatEntityEdit.SetValue(sdiIdentifier("FLEX2"), sdiValue(0.01));
          }
          else if (!lsdLCBType.compare(0, 13, "*DEFINE_TABLE"))
          {
          // take first table function

              sdiValueEntityList curveList;
              sdiValue tempValue(curveList);
              lsdLCBEntRead.GetValue(sdiIdentifier("CurveIds"), tempValue);
              tempValue.GetValue(curveList);
              curveList.GetIdList(lcbCrvList);
              lsdLCBId = lcbCrvList[0];

              HandleRead functLCBHRead;
              p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*DEFINE_CURVE"), lsdLCBId, functLCBHRead);
              EntityRead lsdLCBEntRead(p_lsdynaModel, functLCBHRead);

              int nPnts = 0;
              sdiDoubleList crvPoints;

              double lsdSFA = 1.0;
              double lsdSFO = 1.0;
              double lsdOFFA = 0.0;
              double lsdOFFO = 0.0;

              tempValue = sdiValue(crvPoints);
              lsdLCBEntRead.GetValue(sdiIdentifier("points"), tempValue);
              tempValue.GetValue(crvPoints);

              tempValue = sdiValue(nPnts);
              lsdLCBEntRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
              tempValue.GetValue(nPnts);
              crvPoints.reserve(2 * nPnts + 2);

              vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
              vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
              p_ConvertUtils.GetAttribValues(lsdLCBEntRead, lsdQueryAttribs, attribVals);
              lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
              lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
              //-----------------------
              // Ignore negative part of curve, if it is available set FLEX1=0.01
              temp2Points.clear();
              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  temp2Points.push_back(crvPoints[j]);
                  temp2Points.push_back(crvPoints[j+1]);
              }
              crvPoints.clear();
              for (size_t j = 0; j < temp2Points.size(); j += 2)
              {
                  if (temp2Points[j] >= 0.0 && temp2Points[j+1] >= 0.0)
                  {
                       crvPoints.push_back(temp2Points[j]);
                       crvPoints.push_back(temp2Points[j+1]);
                  }
              }

              // abscisa:  STRAIN_RADIOSS=LN(1+STRAIN_DYNA)
              // ordonate: STRESS_RADIOSS=STRESS_DYNA*(1+STRAIN_DYNA)
              temp2Points.clear();
              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  temp2Points.push_back(crvPoints[j]);
                  temp2Points.push_back(crvPoints[j+1]);
              }
              crvPoints.clear();
              for (size_t j = 0; j < temp2Points.size(); j += 2)
              {
                 double ldsStrain,radStrain;
                 double ldsStress,radStress;
                 ldsStrain = temp2Points[j];
                 radStrain = log(1+ldsStrain);
                 ldsStress = temp2Points[j+1];
                 radStress = ldsStress*(1+ldsStrain);
                 crvPoints.push_back(radStrain);
                 crvPoints.push_back(radStress);
              }

              // cut monotonicaly function when it descends
              temp2Points.clear();
              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  temp2Points.push_back(crvPoints[j]);
                  temp2Points.push_back(crvPoints[j+1]);
              }
              crvPoints.clear();
              double minordonate = temp2Points[1];
              crvPoints.push_back(temp2Points[0]);
              crvPoints.push_back(temp2Points[1]);
              for (size_t j = 2; j < temp2Points.size(); j += 2)
              {
                  if (temp2Points[j+1] > minordonate)
                  {
                      minordonate = temp2Points[j+1];
                      crvPoints.push_back(temp2Points[j]);
                      crvPoints.push_back(temp2Points[j+1]);
                  }
                  else
                  {
                      break;
                  }
              }
              //-----------------------
              HandleEdit functHEdit;
              p_ConvertUtils.CreateCurve("Recalculated_" + to_string(lsdLCBId) + "_MatL34_" + to_string(dynaMatId),
                                        (int)crvPoints.size() / 2, crvPoints, functHEdit, lsdSFA, lsdSFO, lsdOFFA);
              if (functHEdit.IsValid())
              {
                 radmatEntityEdit.SetEntityHandle(sdiIdentifier("FUN_A2"), functHEdit);
                 sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
                 sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
              }

              radmatEntityEdit.SetValue(sdiIdentifier("FLEX2"), sdiValue(0.01));
          }
      }
      else
      {
      // create a new one
          tempValue = sdiValue(lsdEB);
          dynaMat.GetValue(sdiIdentifier("EB"), tempValue);
          tempValue.GetValue(lsdEB);

          sdiDoubleList abscissaList({ -1.0, 0.0, 1.0 });
          sdiDoubleList ordonateList({ -lsdEB, 0.0, lsdEB });
          sdiDoubleList pointsList;
          pointsList.reserve(2 * abscissaList.size());
          for (int j = 0; j < abscissaList.size(); ++j)
          {
              pointsList.push_back(abscissaList[j]);
              pointsList.push_back(ordonateList[j]);
          }
          HandleEdit functHEdit;
          p_ConvertUtils.CreateCurve(dynaMatName, (int)abscissaList.size(), pointsList, functHEdit);
          if (functHEdit.IsValid())
          {
             radmatEntityEdit.SetEntityHandle(sdiIdentifier("FUN_A2"), functHEdit);
             sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
             sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
          }
      }

      
      HandleRead lsdLCABHandle;
      dynaMat.GetEntityHandle(sdiIdentifier("LCAB"), lsdLCABHandle);

      if (lsdLCABHandle.IsValid())
      {
          int nPnts = 0;
          sdiDoubleList crvPoints;

          double lsdSFA = 1.0;
          double lsdSFO = 1.0;
          double lsdOFFA = 0.0;
          double lsdOFFO = 0.0;
          EntityRead lsdLCABEntRead(p_lsdynaModel, lsdLCABHandle);
          EntityId lsdLCABId = lsdLCABEntRead.GetId();

          tempValue = sdiValue(crvPoints);
          lsdLCABEntRead.GetValue(sdiIdentifier("points"), tempValue);
          tempValue.GetValue(crvPoints);

          tempValue = sdiValue(nPnts);
          lsdLCABEntRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
          tempValue.GetValue(nPnts);
          crvPoints.reserve(2 * nPnts + 2);

          vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
          vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
          p_ConvertUtils.GetAttribValues(lsdLCABEntRead, lsdQueryAttribs, attribVals);
          lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
          lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

          while (crvPoints[0] < 0.0)
          {
              crvPoints.erase(crvPoints.begin());
              crvPoints.erase(crvPoints.begin());
          }

          if (crvPoints[0] >= 0.0 && nPnts > 1)
          {
              // cut monotonicaly function when it descends
              temp2Points.clear();
              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  temp2Points.push_back(crvPoints[j]);
                  temp2Points.push_back(crvPoints[j+1]);
              }

              crvPoints.clear();
              double minordonate = temp2Points[1];
              crvPoints.push_back(temp2Points[0]);
              crvPoints.push_back(temp2Points[1]);
              for (size_t j = 2; j < temp2Points.size(); j += 2)
              {
                  if (temp2Points[j+1] > minordonate)
                  {
                      minordonate = temp2Points[j+1];
                      crvPoints.push_back(temp2Points[j]);
                      crvPoints.push_back(temp2Points[j+1]);
                  }
                  else
                  {
                      break;
                  }
              }

              // curve is modified (by mirroring) to the 3rd quadrant and scalled (abscisa, by 57 from radians to degrees)
              temp2Points.clear();
              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  temp2Points.push_back(crvPoints[j]);
                  temp2Points.push_back(crvPoints[j+1]);
              }
              for (size_t j = 3; j < temp2Points.size(); j += 2)
              {
                 crvPoints.insert(crvPoints.begin(), -(temp2Points[j]));
                 crvPoints.insert(crvPoints.begin(), -temp2Points[j - 1]);
              }
              //
              HandleEdit functHEdit;
              p_ConvertUtils.CreateCurve("Recalculated_" + to_string(lsdLCABId) + "_MatL34_" + to_string(dynaMatId),
                                        (int)crvPoints.size() / 2, crvPoints, functHEdit, lsdSFA*57, lsdSFO, lsdOFFA);
              if (functHEdit.IsValid())
              {
                 radmatEntityEdit.SetEntityHandle(sdiIdentifier("FUN_A3"), functHEdit);
                 sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
                 sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
              }
          }
      }
      else
      {
          // create a new function
          double lsdGAB = GetValue<double>(dynaMat, "GAB");
          double lsdEA = GetValue<double>(dynaMat, "EA");
          double lsdEB = GetValue<double>(dynaMat, "EB");
          if(lsdGAB == 0.0) lsdGAB = (lsdEA + lsdEB) / 2.0;

          sdiDoubleList abscissaList({ -57.0, 0.0, 57.0 });
          sdiDoubleList ordonateList({ -lsdGAB, 0.0, lsdGAB });
          sdiDoubleList pointsList;
          pointsList.reserve(2 * abscissaList.size());
          for (int j = 0; j < abscissaList.size(); ++j)
          {
              pointsList.push_back(abscissaList[j]);
              pointsList.push_back(ordonateList[j]);
          }
          HandleEdit functHEdit;
          p_ConvertUtils.CreateCurve(dynaMatName, (int)abscissaList.size(), pointsList, functHEdit);
          if (functHEdit.IsValid())
          {
             radmatEntityEdit.SetEntityHandle(sdiIdentifier("FUN_A3"), functHEdit);
             sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
             sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
          }
      }

      
      HandleRead lsdLCUAHandle;
      dynaMat.GetEntityHandle(sdiIdentifier("LCUA"), lsdLCUAHandle);

      if (lsdLCUAHandle.IsValid())
      {
          
          EntityRead lsdLCUAEntRead(p_lsdynaModel, lsdLCUAHandle);
          sdiString lsdLCUAType = lsdLCUAEntRead.GetKeyword();
          EntityId lsdLCUAId = lsdLCUAEntRead.GetId();
          if(!lsdLCUAType.compare(0, 13, "*DEFINE_CURVE"))
          {
              int nPnts = 0;
              sdiDoubleList crvPoints;

              double lsdSFA = 1.0;
              double lsdSFO = 1.0;
              double lsdOFFA = 0.0;
              double lsdOFFO = 0.0;
              EntityRead lsdLCUAEntRead(p_lsdynaModel, lsdLCUAHandle);

              tempValue = sdiValue(crvPoints);
              lsdLCUAEntRead.GetValue(sdiIdentifier("points"), tempValue);
              tempValue.GetValue(crvPoints);

              tempValue = sdiValue(nPnts);
              lsdLCUAEntRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
              tempValue.GetValue(nPnts);
              crvPoints.reserve(2 * nPnts + 2);

              vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
              vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
              p_ConvertUtils.GetAttribValues(lsdLCUAEntRead, lsdQueryAttribs, attribVals);
              lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
              lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

              // abscisa:  STRAIN_RADIOSS=LN(1+STRAIN_DYNA)
              // ordonate: STRESS_RADIOSS=STRESS_DYNA*(1+STRAIN_DYNA)
              temp2Points.clear();
              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  temp2Points.push_back(crvPoints[j]);
                  temp2Points.push_back(crvPoints[j+1]);
              }
              crvPoints.clear();
              for (size_t j = 0; j < temp2Points.size(); j += 2)
              {
                  double ldsStrain,radStrain;
                  double ldsStress,radStress;
                  ldsStrain = temp2Points[j];
                  radStrain = log(1+ldsStrain);
                  ldsStress = temp2Points[j+1];
                  radStress = ldsStress*(1+ldsStrain);
                  crvPoints.push_back(radStrain);
                  crvPoints.push_back(radStress);
              }
              HandleEdit functHEdit;
              p_ConvertUtils.CreateCurve("Recalculated_" + to_string(lsdLCUAId) + "_MatL34_" + to_string(dynaMatId),
                                        (int)crvPoints.size() / 2, crvPoints, functHEdit, lsdSFA, lsdSFO, lsdOFFA);
              if (functHEdit.IsValid())
              {
                 radmatEntityEdit.SetEntityHandle(sdiIdentifier("FUN_A4"), functHEdit);
                 sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
                 sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
              }
          }
      }
      else
      {
          // -- empty function --
          // "FUN_A4"  --> "FUN_A1"
          //HandleRead radFunA1Handle;
          //radMat.GetEntityHandle(p_radiossModel, sdiIdentifier("FUN_A1"), radFunA1Handle);
          //unsigned int radFunA1Id = radFunA1Handle.GetId(p_radiossModel);
          //if (radFunA1Handle.IsValid())
          //{
          //    radmatEntityEdit.SetValue(sdiIdentifier("FUN_A4"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), radFunA1Id)));
          //}
      }

      
      HandleRead lsdLCUBHandle;
      dynaMat.GetEntityHandle(sdiIdentifier("LCUB"), lsdLCUBHandle);

      if (lsdLCUBHandle.IsValid())
      {
          
          EntityRead lsdLCUBEntRead(p_lsdynaModel, lsdLCUBHandle);
          sdiString lsdLCUBType = lsdLCUBEntRead.GetKeyword();
          EntityId lsdLCUBId = lsdLCUBEntRead.GetId();
          if(!lsdLCUBType.compare(0, 13, "*DEFINE_CURVE"))
          {
              int nPnts = 0;
              sdiDoubleList crvPoints;

              double lsdSFA = 1.0;
              double lsdSFO = 1.0;
              double lsdOFFA = 0.0;
              double lsdOFFO = 0.0;
              EntityRead lsdLCUBEntRead(p_lsdynaModel, lsdLCUBHandle);

              tempValue = sdiValue(crvPoints);
              lsdLCUBEntRead.GetValue(sdiIdentifier("points"), tempValue);
              tempValue.GetValue(crvPoints);

              tempValue = sdiValue(nPnts);
              lsdLCUBEntRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
              tempValue.GetValue(nPnts);
              crvPoints.reserve(2 * nPnts + 2);

              vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
              vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
              p_ConvertUtils.GetAttribValues(lsdLCUBEntRead, lsdQueryAttribs, attribVals);
              lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
              lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

              // abscisa:  STRAIN_RADIOSS=LN(1+STRAIN_DYNA)
              // ordonate: STRESS_RADIOSS=STRESS_DYNA*(1+STRAIN_DYNA)
              temp2Points.clear();
              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  temp2Points.push_back(crvPoints[j]);
                  temp2Points.push_back(crvPoints[j+1]);
              }
              crvPoints.clear();
              for (size_t j = 0; j < temp2Points.size(); j += 2)
              {
                  double ldsStrain,radStrain;
                  double ldsStress,radStress;
                  ldsStrain = temp2Points[j];
                  radStrain = log(1+ldsStrain);
                  ldsStress = temp2Points[j+1];
                  radStress = ldsStress*(1+ldsStrain);
                  crvPoints.push_back(radStrain);
                  crvPoints.push_back(radStress);
              }
              HandleEdit functHEdit;
              p_ConvertUtils.CreateCurve("Recalculated_" + to_string(lsdLCUBId) + "_MatL34_" + to_string(dynaMatId),
                                        (int)crvPoints.size() / 2, crvPoints, functHEdit, lsdSFA, lsdSFO, lsdOFFA);
              if (functHEdit.IsValid())
              {
                 radmatEntityEdit.SetEntityHandle(sdiIdentifier("FUN_A5"), functHEdit);
                 sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
                 sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
              }
          }
      }
      else
      {
          // -- empty function --
          // "FUN_A5"  --> "FUN_A2"
          //HandleRead radFunA2Handle;
          //radMat.GetEntityHandle(p_radiossModel, sdiIdentifier("FUN_A2"), radFunA2Handle);
          //unsigned int radFunA2Id = radFunA2Handle.GetId(p_radiossModel);
          //if (radFunA2Handle.IsValid())
          //{
          //    radmatEntityEdit.SetValue(sdiIdentifier("FUN_A5"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), radFunA2Id)));
          //}
      }

      
      HandleRead lsdLCUABHandle;
      dynaMat.GetEntityHandle(sdiIdentifier("LCUAB"), lsdLCUABHandle);

      if (lsdLCUABHandle.IsValid())
      {
          int nPnts = 0;
          sdiDoubleList crvPoints;

          double lsdSFA = 1.0;
          double lsdSFO = 1.0;
          double lsdOFFA = 0.0;
          double lsdOFFO = 0.0;
          EntityRead lsdLCUABEntRead(p_lsdynaModel, lsdLCUABHandle);
          EntityId lsdLCUABId = lsdLCUABEntRead.GetId();

          tempValue = sdiValue(crvPoints);
          lsdLCUABEntRead.GetValue(sdiIdentifier("points"), tempValue);
          tempValue.GetValue(crvPoints);

          tempValue = sdiValue(nPnts);
          lsdLCUABEntRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
          tempValue.GetValue(nPnts);
          crvPoints.reserve(2 * nPnts + 2);

          vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
          vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
          p_ConvertUtils.GetAttribValues(lsdLCUABEntRead, lsdQueryAttribs, attribVals);
          lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
          lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

          while (crvPoints[0] < 0.0)
          {
              crvPoints.erase(crvPoints.begin());
              crvPoints.erase(crvPoints.begin());
          }

          if (crvPoints[0] >= 0.0 && nPnts > 1)
          {
              // curve is modified (by mirroring) to the 3rd quadrant and scalled (abscisa, by 57 from radians to degrees)
              temp2Points.clear();
              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  temp2Points.push_back(crvPoints[j]);
                  temp2Points.push_back(crvPoints[j+1]);
              }
              for (size_t j = 3; j < temp2Points.size(); j += 2)
              {
                  crvPoints.insert(crvPoints.begin(), -temp2Points[j]);
                  crvPoints.insert(crvPoints.begin(), -temp2Points[j - 1]);
              }
              HandleEdit functHEdit;
              p_ConvertUtils.CreateCurve("Recalculated_" + to_string(lsdLCUABId) + "_MatL34_" + to_string(dynaMatId),
                                        (int)crvPoints.size() / 2, crvPoints, functHEdit, lsdSFA*57, lsdSFO, lsdOFFA);
              if (functHEdit.IsValid())
              {
                  radmatEntityEdit.SetEntityHandle(sdiIdentifier("FUN_A6"), functHEdit);
                  sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
                  sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemat));
              }
          }
      }
      else
      {
          // -- empty function --
          // "FUN_A6"  --> "FUN_A3"
          //HandleRead radFunA3Handle;
          //radMat.GetEntityHandle(p_radiossModel, sdiIdentifier("FUN_A3"), radFunA3Handle);
          //unsigned int radFunA3Id = radFunA3Handle.GetId(p_radiossModel);
          //if (radFunA3Handle.IsValid())
          //{
          //    radmatEntityEdit.SetValue(sdiIdentifier("FUN_A6"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), radFunA3Id)));
          //}
      }
      // Create /LEAK/MAT

      int leakCoeffFlag = GetValue<int>(dynaMat, "leakCoeffFlag");

      if(leakCoeffFlag ==1)
      {
          HandleRead crvLCIDHandle;
          dynaMat.GetEntityHandle(sdiIdentifier("LSD_LCID"), crvLCIDHandle);

          if(crvLCIDHandle.IsValid())
          {
              EntityRead crvLCIDRead(p_lsdynaModel, crvLCIDHandle);
              EntityId crvLCIId = crvLCIDHandle.GetId(p_lsdynaModel);
              HandleEdit leakmatHEdit;
              p_radiossModel->CreateEntity(leakmatHEdit, "/LEAK/MAT", dynaMatName, radMat.GetId(p_radiossModel));
              EntityEdit leakmatEntEdit(p_radiossModel, leakmatHEdit);
              leakmatEntEdit.SetValue(sdiIdentifier("Ileakage"), sdiValue(2));
              p_ConvertUtils.CopyValue(dynaMat, leakmatEntEdit, "LSD_LCID", "Fct_ID_LC");

              sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
              sdiConvert::Convert::PushToConversionLog(std::make_pair(leakmatHEdit, sourcemat));
          }
      }
      else
      {
          // SCALAR(LSD_MAT_FLC)
          double lsdFLC = GetValue<double>(dynaMat, "LSD_MAT_FLC");
          if(lsdFLC > 0)
          {
              HandleEdit leakmatHEdit;
              p_radiossModel->CreateEntity(leakmatHEdit, "/LEAK/MAT", dynaMatName, radMat.GetId(p_radiossModel));
              EntityEdit leakmatEntEdit(p_radiossModel, leakmatHEdit);
              leakmatEntEdit.SetValue(sdiIdentifier("Ileakage"), sdiValue(1));
              p_ConvertUtils.CopyValue(dynaMat, leakmatEntEdit, "LSD_MAT_FLC", "LC");
              leakmatEntEdit.SetValue(sdiIdentifier("AC"), sdiValue(1.0));

              sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
              sdiConvert::Convert::PushToConversionLog(std::make_pair(leakmatHEdit, sourcemat));
          }
      }
    } // if(lsdFORM != 14.0 && lsdFORM != -14.0)
}

void ConvertMat::p_ConvertMatL54(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();

    destCard = "/MAT/LAW127";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);

    attribMap = { {"Rho", "MAT_RHO"},{"EA", "E1"},{"EB", "E2"},{"EC", "E3"},
                  {"GAB", "G12"},{"GCA", "G13"},{"GBC", "G23"},
                  {"PRBA", "Nu21"},{"PRCA", "Nu31"},{"PRCB", "Nu32"},
                  {"2WAY", "2WAY"},{"TI", "TI"},{"DFAILM", "DFAILM"},{"DFAILS", "DFAILS"},
                  {"TFAIL", "TFAIL"},{"ALPH", "ALPH"},{"FBRT", "FBRT"},
                  {"YCFAC", "YCFAC"},{"DFAILT", "DFAILT"},{"DFAILC", "DFAILC"},
                  {"EFS", "EFS"},{"XC", "XC"},{"XT", "XT"},{"YC", "YC"},{"YT", "YT"},{"SC", "SC"},
                  {"BETA", "BETA"},{"PFL", "PFL"},{"EPSF", "EPSF"},{"EPSR", "EPSR"},{"NCYRED", "NCYRED"},{"TSMD", "TSMD"},
                  {"SLIMT1", "SLIMT1"},{"SLIMT2", "SLIMT2"},{"SLIMC1", "SLIMC1"},{"SLIMC2", "SLIMC2"},{"SLIMS", "SLIMSC"}                  
                   };

    if (radMat.IsValid())
    {

        EntityEdit radMatEntityEdit(p_radiossModel, radMat);

        sdiStringList DynaRadAttrFunc({"LSD_LCXC","LSD_LCXT","LSD_LCYC","LSD_LCYT","LSD_LCSC"});

        sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
        EntityType radFunctEntType = p_radiossModel->GetEntityType("/FUNCT");
    
        for (int i = 0; i < 4; i++)
        {
            HandleRead lcHandle;
            dynaMat.GetEntityHandle(sdiIdentifier(DynaRadAttrFunc[i]), lcHandle);
            if (lcHandle.IsValid())
            {
                HandleEdit functHRead;
                EntityRead lcEntRead(p_lsdynaModel, lcHandle);
                if (lcHandle.IsValid())
                {
                    radMatEntityEdit.SetEntityHandle(sdiIdentifier(DynaRadAttrFunc[i]), lcHandle);
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(lcHandle, sourcemat));
                }
            }
        }
    }
}

void ConvertMat::p_ConvertMatL57(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    attribMap = { {"E","E0"}, {"RHO", "Rho_I"}, {"HU", "Hys"}, {"SHAPE", "Shape"} };
    destCard = "/MAT/LAW90"; 

    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);

    if (radMat.IsValid())
    {
        int nFunct = 0;
        sdiUIntList funcIdList;
        sdiDoubleList strainRatesList;
        sdiDoubleList scaleList;
        EntityEdit radMatEntityEdit(p_radiossModel, radMat);

        int curveOPt = 0;
        HandleRead sigCrvHandle;
        dynaMat.GetEntityHandle(sdiIdentifier("LSD_LCID"), sigCrvHandle);
        if (sigCrvHandle.IsValid())
        {
            EntityRead sigCrvEntRead(p_lsdynaModel, sigCrvHandle);
            sdiString crvType = sigCrvEntRead.GetKeyword();
            EntityId sigYcrvId = sigCrvEntRead.GetId();
            if (!crvType.compare(0, 13, "*DEFINE_CURVE"))
            {
                nFunct = 1;
                funcIdList.push_back(sigYcrvId);
                strainRatesList.push_back(0.0);
                scaleList.push_back(0.0);
            }
            else if (!crvType.compare(0, 13, "*DEFINE_TABLE"))
            {
                sdiValueEntityList curveList;
                sdiValue tempValue(curveList);
                sigCrvEntRead.GetValue(sdiIdentifier("CurveIds"), tempValue);
                tempValue.GetValue(curveList);
                curveList.GetIdList(funcIdList);
                nFunct = (int)funcIdList.size();

                tempValue = sdiValue(strainRatesList);
                sigCrvEntRead.GetValue(sdiIdentifier("VALUE"), tempValue);
                tempValue.GetValue(strainRatesList);
                scaleList = sdiDoubleList(nFunct, 0.0);
            }
        }
        radMatEntityEdit.SetValue(sdiIdentifier("fct_idL"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), funcIdList)));
        radMatEntityEdit.SetValue(sdiIdentifier("eps_dot"), sdiValue(strainRatesList));
        radMatEntityEdit.SetValue(sdiIdentifier("Fscale"), sdiValue(scaleList));

        radMatEntityEdit.SetValue(sdiIdentifier("NL"), sdiValue(nFunct));

        radMatEntityEdit.SetValue(sdiIdentifier("MAT_NU"), sdiValue(0.0));
        radMatEntityEdit.SetValue(sdiIdentifier("Ismooth"), sdiValue(1));
        radMatEntityEdit.SetValue(sdiIdentifier("Fcut"), sdiValue(0.0));
        radMatEntityEdit.SetValue(sdiIdentifier("TFLAG"), sdiValue(2));
    }
}

void ConvertMat::p_ConvertMatL58(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    attribMap = { {"RHO", "RHO_I"}, {"EA", "E11"}, {"EB", "E22"}, {"PRBA", "NU12"}, {"GAB", "G12"}, {"GBC", "G23"}, {"GCA", "G31"},
                  {"E11C", "EPS_1c1"}, {"E11T", "EPS_1t1"},{"E22C", "EPS_1c2"},{"E22T", "EPS_1t2"},};
    destCard = "/MAT/LAW25"; 

    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);

    if (radMat.IsValid())
    {
         EntityEdit radMatEntityEdit(p_radiossModel, radMat);
         radMatEntityEdit.SetValue(sdiIdentifier("Iform"), sdiValue(1));
         p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "SLIMT1 * XT ", "SIGMA_rst1");
         p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "SLIMC1 * XC ", "sig_rsc1");
         p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "SLIMT2 * YT ", "sig_rst2");
         p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "SLIMC2 * YC ", "sig_rsc2");
         p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "SLIMS * SC ", "sig_rst12");
         radMatEntityEdit.SetValue(sdiIdentifier("n_12t"), sdiValue(0.75));
         radMatEntityEdit.SetValue(sdiIdentifier("dmax"), sdiValue(0.9999));
         p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "E11C * 2 ", "EPS_2c1");
         p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "E11T * 2 ", "EPS_2t1");
         p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "E22C * 2 ", "EPS_2c2");
         p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "E22T * 2 ", "EPS_2t2");
         p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "ABS(ERODS)", "MAT_EPSF1");
         p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "ABS(ERODS)", "MAT_EPSF2");
         p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "ABS(ERODS) * 0.9 ", "MAT_EPST1");
         p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "ABS(ERODS) * 0.9 ", "MAT_EPST2");
         p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "ABS(ERODS) * 0.95", "MAT_EPSM1");
         p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "ABS(ERODS) * 0.95", "MAT_EPSM2");

         sdiValue tempValue;

         double lsdFS = 0.0;
         tempValue = sdiValue(lsdFS);
         dynaMat.GetValue(sdiIdentifier("FS"), tempValue);
         tempValue.GetValue(lsdFS);

         double lsdTAU1 = 0.0;
         tempValue = sdiValue(lsdTAU1);
         dynaMat.GetValue(sdiIdentifier("TAU1"), tempValue);
         tempValue.GetValue(lsdTAU1);

         double lsdGAMMA1 = 0.0;
         tempValue = sdiValue(lsdGAMMA1);
         dynaMat.GetValue(sdiIdentifier("GAMMA1"), tempValue);
         tempValue.GetValue(lsdGAMMA1);

         double lsdSC = 0.0;
         tempValue = sdiValue(lsdSC);
         dynaMat.GetValue(sdiIdentifier("SC"), tempValue);
         tempValue.GetValue(lsdSC);

         double lsdXC = 0.0;
         tempValue = sdiValue(lsdXC);
         dynaMat.GetValue(sdiIdentifier("XC"), tempValue);
         tempValue.GetValue(lsdXC);

         double lsdXT = 0.0;
         tempValue = sdiValue(lsdXT);
         dynaMat.GetValue(sdiIdentifier("XT"), tempValue);
         tempValue.GetValue(lsdXT);

         double lsdYC = 0.0;
         tempValue = sdiValue(lsdYC);
         dynaMat.GetValue(sdiIdentifier("YC"), tempValue);
         tempValue.GetValue(lsdYC);

         double lsdYT = 0.0;
         tempValue = sdiValue(lsdYT);
         dynaMat.GetValue(sdiIdentifier("YT"), tempValue);
         tempValue.GetValue(lsdYT);

         if((lsdFS == -1.0 || lsdFS == 1.0) &&  lsdTAU1 > 0.0 && lsdGAMMA1 > 0.0)
         {
           p_ConvertUtils.CopyValue(dynaMat, radMatEntityEdit, "TAU1", "sig_12yt");
           p_ConvertUtils.SetExpressionValue(dynaMat, radMatEntityEdit, "(SC/TAU1-1.0)/(((SC-TAU1)*(GMS-GAMMA1)) ^ 0.75) ", "b_12t");
         }
         else
         {
           p_ConvertUtils.CopyValue(dynaMat, radMatEntityEdit, "SC", "sig_12yt");
           radMatEntityEdit.SetValue(sdiIdentifier("b_12t"), sdiValue(0.0));
         }
         if(lsdSC == 0.0) radMatEntityEdit.SetValue(sdiIdentifier("sig_12yt"), sdiValue(1E10));
         if(lsdXC == 0.0) radMatEntityEdit.SetValue(sdiIdentifier("sig_1yc"), sdiValue(1E10));
         else p_ConvertUtils.CopyValue(dynaMat, radMatEntityEdit, "XC", "sig_1yc");
         if(lsdXT == 0.0) radMatEntityEdit.SetValue(sdiIdentifier("sig_1yt"), sdiValue(1E10));
         else p_ConvertUtils.CopyValue(dynaMat, radMatEntityEdit, "XT", "sig_1yt");
         if(lsdYC == 0.0) radMatEntityEdit.SetValue(sdiIdentifier("sig_2yc"), sdiValue(1E10));
         else p_ConvertUtils.CopyValue(dynaMat, radMatEntityEdit, "YC", "sig_2yc");
         if(lsdYT == 0.0) radMatEntityEdit.SetValue(sdiIdentifier("sig_2yt"), sdiValue(1E10));
         else p_ConvertUtils.CopyValue(dynaMat, radMatEntityEdit, "YT", "sig_2yt");

         radMatEntityEdit.SetValue(sdiIdentifier("Ioff"), sdiValue(6));
     }
}

void ConvertMat::p_ConvertMatL61(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    attribMap = { {"RHO", "RHO_I"}, {"BULK","K"}, {"GI","G_inf"}, {"DC","BETA1"} };
    destCard = "/MAT/KELVINMAX";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);
    double lsdG0;
    double lsdGI;
    vector<reference_wrapper<double>> attrValList = { lsdG0, lsdGI };
    vector<sdiString> attrNameList = { "G0","GI" };
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);
    lsdGI = lsdG0 - lsdGI;
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    radmatEntityEdit.SetValue(sdiIdentifier("G1"), sdiValue(lsdGI));

    //The rest of parameters are set to default 0 values in converson                                  
    radmatEntityEdit.SetValue(sdiIdentifier("Astass"), sdiValue(0.0));
    radmatEntityEdit.SetValue(sdiIdentifier("Bstass"), sdiValue(0.0));
    radmatEntityEdit.SetValue(sdiIdentifier("Kvm"), sdiValue(0.0));

    radmatEntityEdit.SetValue(sdiIdentifier("G2"), sdiValue(0.0));
    radmatEntityEdit.SetValue(sdiIdentifier("G3"), sdiValue(0.0));
    radmatEntityEdit.SetValue(sdiIdentifier("G4"), sdiValue(0.0));
    radmatEntityEdit.SetValue(sdiIdentifier("G5"), sdiValue(0.0));

    radmatEntityEdit.SetValue(sdiIdentifier("BETA2"), sdiValue(0.0));
    radmatEntityEdit.SetValue(sdiIdentifier("BETA3"), sdiValue(0.0));
    radmatEntityEdit.SetValue(sdiIdentifier("BETA4"), sdiValue(0.0));
    radmatEntityEdit.SetValue(sdiIdentifier("BETA5"), sdiValue(0.0));
}

void ConvertMat::p_ConvertMatL66(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat)
{
    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    attribMap = {{"RHO", "RHO_I"}, {"TKR","K1"}, {"TKS","K2"}, {"TKT","K3"}, {"RKR","K4"}, {"RKS","K5"}, {"RKT","K6"}, {"TDR","C1"}, {"TDS","C2"}, {"TDT","C3"}, {"RDR","C4"}, {"RDS","C5"}, {"RDT","C6"} };   
    destCard = "/MAT/LAW108";
    //---
    // destcard changes if in *SECTION_BEAM, SCOOR = +/-2, then convert it to destCard = "/MAT/LAW113";
    unsigned int partId = p_PartBeingConverted.GetId(p_lsdynaModel);
    HandleEdit parHEdit;
    p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), partId, parHEdit);
    if (parHEdit.IsValid())
    {
        HandleEdit propHread;
        parHEdit.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHread);
        if (propHread.IsValid())
        {
            double lsdSCOOR;
            sdiValue tempValue(lsdSCOOR);
            propHread.GetValue(p_lsdynaModel, sdiIdentifier("SCOOR"), tempValue);
            tempValue.GetValue(lsdSCOOR);                
            if(lsdSCOOR == -2.0 || lsdSCOOR == 2.0) destCard = "/MAT/LAW113";
        }
    }
    //---
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);

    double lsdTKR, lsdTKS, lsdTKT, lsdRKR, lsdRKS, lsdRKT;
    double lsdFOR, lsdFOS, lsdFOT, lsdMOR, lsdMOS, lsdMOT;

    vector<reference_wrapper<double>> attrValList = { lsdTKR, lsdTKS, lsdTKT, lsdRKR, lsdRKS, lsdRKT, lsdFOR, lsdFOS, lsdFOT, lsdMOR, lsdMOS, lsdMOT };
    vector<sdiString> attrNameList = {"TKR","TKS","TKT","RKR","RKS","RKT","FOR","FOS","FOT","MOR","MOS","MOT" };
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

    EntityEdit radmatEntityEdit(p_radiossModel, radMat);

    if (lsdFOR)
    {
        HandleEdit fctNEdit;
        p_ConvertUtils.CreateCurve("functN11", 2, { { 0.0, lsdFOR , 1.0, lsdTKR + lsdFOR } }, fctNEdit);
        if(fctNEdit.IsValid())
        {
            radmatEntityEdit.SetEntityHandle(sdiIdentifier("FUN_A1"), fctNEdit);
            sdiConvert::Convert::PushToConversionLog(std::make_pair(fctNEdit, sourcemat));
        }
    }
    if (lsdFOS)
    {
        HandleEdit fctNEdit;
        p_ConvertUtils.CreateCurve("functN12", 2, { { 0.0, lsdFOS , 1.0, lsdTKS + lsdFOS } }, fctNEdit);
        if (fctNEdit.IsValid())
        {
            radmatEntityEdit.SetEntityHandle(sdiIdentifier("FUN_A2"), fctNEdit);
            sdiConvert::Convert::PushToConversionLog(std::make_pair(fctNEdit, sourcemat));
        }
    }
    if (lsdFOT)
    {
        HandleEdit fctNEdit;
        p_ConvertUtils.CreateCurve("functN13", 2, { { 0.0, lsdFOT , 1.0, lsdTKT + lsdFOT } }, fctNEdit);
        if (fctNEdit.IsValid())
        {
            radmatEntityEdit.SetEntityHandle(sdiIdentifier("FUN_A3"), fctNEdit);
            sdiConvert::Convert::PushToConversionLog(std::make_pair(fctNEdit, sourcemat));
        }
    }
    if (lsdMOR)
    {
        HandleEdit fctNEdit;
        p_ConvertUtils.CreateCurve("functN14", 2, { { 0.0, lsdMOR , 1.0, lsdRKR + lsdMOR } }, fctNEdit);
        if (fctNEdit.IsValid())
        {
            radmatEntityEdit.SetEntityHandle(sdiIdentifier("FUN_A4"), fctNEdit);
            sdiConvert::Convert::PushToConversionLog(std::make_pair(fctNEdit, sourcemat));
        }
    }
    if (lsdMOS)
    {
        HandleEdit fctNEdit;
        p_ConvertUtils.CreateCurve("functN15", 2, { { 0.0, lsdMOS , 1.0, lsdRKS + lsdMOS } }, fctNEdit);
        if (fctNEdit.IsValid())
        {
            radmatEntityEdit.SetEntityHandle(sdiIdentifier("FUN_A5"), fctNEdit);
            sdiConvert::Convert::PushToConversionLog(std::make_pair(fctNEdit, sourcemat));
        }
    }
    if (lsdMOT)
    {
        HandleEdit fctNEdit;
        p_ConvertUtils.CreateCurve("functN16", 2, { { 0.0, lsdMOT , 1.0, lsdRKT + lsdMOT } }, fctNEdit);
        if (fctNEdit.IsValid())
        {
            radmatEntityEdit.SetEntityHandle(sdiIdentifier("FUN_A6"), fctNEdit);
            sdiConvert::Convert::PushToConversionLog(std::make_pair(fctNEdit, sourcemat));
        }
    }
    
    radmatEntityEdit.SetValue(sdiIdentifier("Mat_Name_OR_LawNo"), sdiValue(2));  

    //The rest of parameters are set to default 0 values in converson
    vector<sdiString>::iterator it;
    vector<sdiString> defaultAttrI({ "Ifail","Iequil","Ifail2",
                                    "H1","H2","H3","H4","H5","H6",
                                    "Fsmooth"});
    //Dcoeft1 and Dcoeft2 are used because D1 and D2 are variables in the cfg  
    vector<sdiString> defaultAttrD({ "A1","A2","A3","A4","A5","A6",
                                    "B1","B2","B3","B4","B5","B6",
                                    "Dcoeft1","Dcoeft2","D3","D4","D5","D6",
                                    "DeltaMin1","DeltaMin2","DeltaMin3","ThetaMin4","ThetaMin5","ThetaMin6",
                                    "DeltaMax1","DeltaMax2","DeltaMax3","ThetaMax4","ThetaMax5","ThetaMax6",
                                    "F1","F2","F3","F4","F5","F6",
                                    "E1","E2","E3","E4","E5","E6",
                                    "Ascale1","Ascale2","Ascale3","Ascale4","Ascale5","Ascale6",
                                    "Hscale1","Hscale2","Hscale3","Hscale4","Hscale5","Hscale6",
                                    "Fcut" });

    for (it = defaultAttrI.begin(); it < defaultAttrI.end(); it++)
    {
        radmatEntityEdit.SetValue(sdiIdentifier(*it), sdiValue(0));
    }
                           
    for (it = defaultAttrD.begin(); it < defaultAttrD.end(); it++)
    {
        radmatEntityEdit.SetValue(sdiIdentifier(*it), sdiValue(0.0));
    }
}

void ConvertMat::p_ConvertMatL67(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    attribMap = { {"RHO", "RHO_I"} };
    destCard = "/MAT/LAW108";
    //---
    // destcard changes if in *SECTION_BEAM, SCOOR = +/-2, then convert it to destCard = "/MAT/LAW113";
    unsigned int partId = p_PartBeingConverted.GetId(p_lsdynaModel);
    HandleEdit parHEdit;
    p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), partId, parHEdit);
    if (parHEdit.IsValid())
    {
        HandleEdit propHread;
        parHEdit.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHread);
        if (propHread.IsValid())
        {
            double lsdSCOOR;
            sdiValue tempValue(lsdSCOOR);
            propHread.GetValue(p_lsdynaModel, sdiIdentifier("SCOOR"), tempValue);
            tempValue.GetValue(lsdSCOOR);                
            if(lsdSCOOR == -2.0 || lsdSCOOR == 2.0) destCard = "/MAT/LAW113";
        }
    }
    //---
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);

    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    radmatEntityEdit.SetValue(sdiIdentifier("Mat_Name_OR_LawNo"), sdiValue(2));

    double lsdFOR;
    double lsdFOS;
    double lsdFOT;
    double lsdMOR;
    double lsdMOS;
    double lsdMOT;
    double lsdFFAILR;
    double lsdFFAILS;
    double lsdFFAILT;
    double lsdMFAILR;
    double lsdMFAILS;
    double lsdMFAILT;
    double lsdUFAILR;
    double lsdUFAILS;
    double lsdUFAILT;
    double lsdTFAILR;
    double lsdTFAILS;
    double lsdTFAILT;
    HandleRead handleLCIDTR;
    HandleRead handleLCIDTS;
    HandleRead handleLCIDTT;
    HandleRead handleLCIDRR;
    HandleRead handleLCIDRS;
    HandleRead handleLCIDRT;
    HandleRead handleLCIDTDR;
    HandleRead handleLCIDTDS;
    HandleRead handleLCIDTDT;
    HandleRead handleLCIDRDR;
    HandleRead handleLCIDRDS;
    HandleRead handleLCIDRDT;

    vector<reference_wrapper<HandleRead>> allLcidHandles({ handleLCIDTR, handleLCIDTS, handleLCIDTT, handleLCIDRR, handleLCIDRS, handleLCIDRT,
                                                           handleLCIDTDR, handleLCIDTDS, handleLCIDTDT, handleLCIDRDR, handleLCIDRDS, handleLCIDRDT });
    vector<sdiString> lcidAttrNames({ "LCIDTR","LCIDTS","LCIDTT","LCIDRR","LCIDRS","LCIDRT",
                                         "LCIDTDR","LCIDTDS","LCIDTDT","LCIDRDR","LCIDRDS","LCIDRDT" });
    p_ConvertUtils.GetEntityHandles(dynaMat, lcidAttrNames, allLcidHandles);

    vector<reference_wrapper<double>> attrValList = { lsdFOR, lsdFOS, lsdFOT, lsdMOR, lsdMOS, lsdMOT,
                                                      lsdFFAILR, lsdFFAILS, lsdFFAILT, lsdMFAILR, lsdMFAILS, lsdMFAILT,
                                                      lsdUFAILR, lsdUFAILS, lsdUFAILT, lsdTFAILR, lsdTFAILS, lsdTFAILT };
    vector<sdiString> attrNameList = { "FOR", "FOS", "FOT", "MOR", "MOS", "MOT",
                                      "FFAILR", "FFAILS", "FFAILT", "MFAILR", "MFAILS", "MFAILT",
                                      "UFAILR", "UFAILS", "UFAILT", "TFAILR", "TFAILS", "TFAILT" };
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

    vector<double> preLoadList({ lsdFOR, lsdFOS, lsdFOT, lsdMOR, lsdMOS, lsdMOT });

    EntityType radFunctEntType = p_radiossModel->GetEntityType("/FUNCT");
    sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
    vector<sdiConvert::SDIHandlReadList> lcidHReadsList({ { { handleLCIDTR, handleLCIDTS, handleLCIDTT, handleLCIDRR, handleLCIDRS, handleLCIDRT } },
                                                         { { handleLCIDTDR, handleLCIDTDS, handleLCIDTDT, handleLCIDRDR, handleLCIDRDS, handleLCIDRDT }} });
    sdiStringList functAttrStrList({ "fct_ID1", "fct_ID4" });
    for (size_t oi = 0; oi < 2; ++oi)
    {
        for (size_t i = 0; i < 6; ++i)
        {
            HandleRead lcidHread(lcidHReadsList[oi][i]);
            double preLoad = preLoadList[i];
            if (lcidHread.IsValid())
            {
                int nPnts = 0;
                sdiDoubleList crvPoints;

                double lsdSFA = 1.0;
                double lsdSFO = 1.0;
                double lsdOFFA = 0.0;
                double lsdOFFO = 0.0;
                EntityRead lcidRead(p_lsdynaModel, lcidHread);
                sdiValue tempValue(crvPoints);
                lcidRead.GetValue(sdiIdentifier("points"), tempValue);
                tempValue.GetValue(crvPoints);

                tempValue = sdiValue(nPnts);
                lcidRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
                tempValue.GetValue(nPnts);
                crvPoints.reserve(2 * nPnts + 2);

                vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
                vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
                p_ConvertUtils.GetAttribValues(lcidRead, lsdQueryAttribs, attribVals);
                lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
                lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
                if (oi == 1)
                {
                    while (crvPoints[0] < 0.0)
                    {
                        crvPoints.erase(crvPoints.begin());
                        crvPoints.erase(crvPoints.begin());
                    }
                    preLoad = 0;
                    radmatEntityEdit.SetValue(sdiIdentifier("Hscale" + to_string(i + 1)), sdiValue(1));
                }
                // curves N11- N16
                if (oi == 0)
                {
                    if (crvPoints[0] >= 0.0 && nPnts > 1)
                    {
                       // curve is modified (by mirroring) to the 3rd quadrant and shifted (if needed)
                       sdiDoubleList tempPoints(crvPoints);
                       for (size_t j = 3; j < tempPoints.size(); j += 2)
                       {
                           crvPoints.insert(crvPoints.begin(), -tempPoints[j]);
                           crvPoints.insert(crvPoints.begin(), -tempPoints[j - 1]);
                       }
                       HandleEdit functHEdit;
                       p_ConvertUtils.CreateCurve("Recalculated_" + to_string(lcidRead.GetId()) + "_MatL108_" + to_string(dynaMatId),
                           (int)crvPoints.size() / 2, crvPoints, functHEdit, lsdSFA, lsdSFO, lsdOFFA, preLoad);
                       if (functHEdit.IsValid())
                       {
                           radmatEntityEdit.SetEntityHandle(sdiIdentifier(functAttrStrList[oi] + to_string(i + 1)), functHEdit);
                           sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
                       }
                    }
                    else if (crvPoints[0] < 0.0 && preLoad != 0.0 && nPnts > 1)
                    {
                      // curve is only shifted (being already defined in the third quadrant)
                       HandleEdit functHEdit;
                       p_ConvertUtils.CreateCurve("Recalculated_" + to_string(lcidRead.GetId()) + "_MatL108_" + to_string(dynaMatId),
                           (int)crvPoints.size() / 2, crvPoints, functHEdit, lsdSFA, lsdSFO, lsdOFFA, preLoad);
                       if (functHEdit.IsValid())
                       {
                           radmatEntityEdit.SetEntityHandle(sdiIdentifier(functAttrStrList[oi] + to_string(i + 1)), functHEdit);
                           sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
                       }
                    }
                    else
                    {
                      // keep curve unmodified
                      radmatEntityEdit.SetEntityHandle(sdiIdentifier(functAttrStrList[oi] + to_string(i + 1)), lcidHread);
                    }
                }
                // curves N41-N46
                if (oi == 1)
                {
                    if (crvPoints[0] >= 0.0 && nPnts > 1)
                    {
                        // curve is modified (by mirroring) to the 3rd quadrant and shifted (if needed)
                        sdiDoubleList tempPoints(crvPoints);
                        for (size_t j = 3; j < tempPoints.size(); j += 2)
                        {
                            crvPoints.insert(crvPoints.begin(), -tempPoints[j]);
                            crvPoints.insert(crvPoints.begin(), -tempPoints[j - 1]);
                        }
                        HandleEdit functHEdit;
                        p_ConvertUtils.CreateCurve("Recalculated_" + to_string(lcidRead.GetId()) + "_MatL108_" + to_string(dynaMatId),
                            (int)crvPoints.size() / 2, crvPoints, functHEdit, lsdSFA, lsdSFO, lsdOFFA, lsdOFFO);
                        if (functHEdit.IsValid())
                        {
                            radmatEntityEdit.SetEntityHandle(sdiIdentifier(functAttrStrList[oi] + to_string(i + 1)), functHEdit);
                            sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
                        }
                    }
                    else
                    {
                      // keep curve unmodified
                      radmatEntityEdit.SetEntityHandle(sdiIdentifier(functAttrStrList[oi] + to_string(i + 1)), lcidHread);
                    }
                }
            }
        }
    }

    int iFail2 = -1;
    sdiDoubleList optFailParms1({ lsdUFAILR, lsdUFAILS, lsdUFAILT, lsdTFAILR, lsdTFAILS, lsdTFAILT });
    if (lsdUFAILR + lsdUFAILS + lsdUFAILT + lsdTFAILR + lsdTFAILS + lsdTFAILT > 0.0)
        iFail2 = 0;
    else if (lsdFFAILR + lsdFFAILS + lsdFFAILT + lsdMFAILR + lsdMFAILS + lsdMFAILT > 0.0)
    {
        optFailParms1 = { { lsdFFAILR, lsdFFAILS, lsdFFAILT, lsdMFAILR, lsdMFAILS, lsdMFAILT } };
        iFail2 = 1;
    }
    if (iFail2 != -1)
    {
        for (size_t i = 0; i < optFailParms1.size(); ++i)
        {
            double failParam = optFailParms1[i];
            if (failParam != 0.0 && i < 3)
            {
                radmatEntityEdit.SetValue(sdiIdentifier("DeltaMin" + to_string(i + 1)), sdiValue(-failParam));
                radmatEntityEdit.SetValue(sdiIdentifier("DeltaMax" + to_string(i + 1)), sdiValue(failParam));
            }
            else if (failParam != 0.0 && i < 6)
            {
                radmatEntityEdit.SetValue(sdiIdentifier("ThetaMin" + to_string(i + 1)), sdiValue(-failParam));
                radmatEntityEdit.SetValue(sdiIdentifier("ThetaMax" + to_string(i + 1)), sdiValue(failParam));
            }
        }
        radmatEntityEdit.SetValue(sdiIdentifier("Ifail2"), sdiValue(iFail2));
    }
    radmatEntityEdit.SetValue(sdiIdentifier("Ifail"), sdiValue(1));

}

void ConvertMat::p_ConvertMatL68(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    attribMap = { {"RHO", "RHO_I"}, {"LSD_MAT68_TKR","K1"}, {"LSD_MAT68_TKS","K2"}, {"LSD_MAT68_TKT","K3"}, {"LSD_MAT68_RKR","K4"},
        {"LSD_MAT68_RKS","K5"}, {"LSD_MAT68_RKT","K6"}, {"LSD_MAT68_TDR","C1"}, {"LSD_MAT68_TDS","C2"}, {"LSD_MAT68_TDT","C3"},
        {"LSD_MAT68_RDR","C4"}, {"LSD_MAT68_RDS","C5"}, {"LSD_MAT68_RDT","C6"} };
    destCard = "/MAT/LAW108";
    //---
    // destcard changes if in *SECTION_BEAM, SCOOR = +/-2, then convert it to destCard = "/MAT/LAW113";
    unsigned int partId = p_PartBeingConverted.GetId(p_lsdynaModel);
    HandleEdit parHEdit;
    p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), partId, parHEdit);
    if (parHEdit.IsValid())
    {
        HandleEdit propHread;
        parHEdit.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHread);
        if (propHread.IsValid())
        {
            double lsdSCOOR;
            sdiValue tempValue(lsdSCOOR);
            propHread.GetValue(p_lsdynaModel, sdiIdentifier("SCOOR"), tempValue);
            tempValue.GetValue(lsdSCOOR);                
            if(lsdSCOOR == -2.0 || lsdSCOOR == 2.0) destCard = "/MAT/LAW113";
        }
    }
    //---
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);

    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    radmatEntityEdit.SetValue(sdiIdentifier("Mat_Name_OR_LawNo"), sdiValue(2));

    double lsdTKR;
    double lsdTKS;
    double lsdTKT;
    double lsdRKR;
    double lsdRKS;
    double lsdRKT;
 
    double lsdFFAILR;
    double lsdFFAILS;
    double lsdFFAILT;
    double lsdMFAILR;
    double lsdMFAILS;
    double lsdMFAILT;
    
    double lsdUFAILR;
    double lsdUFAILS;
    double lsdUFAILT;
    double lsdTFAILR;
    double lsdTFAILS;
    double lsdTFAILT;

    double lsdFOR;
    double lsdFOS;
    double lsdFOT;
    double lsdMOR;
    double lsdMOS;
    double lsdMOT;

    vector<reference_wrapper<double>> attrValList = { lsdTKR, lsdTKS, lsdTKT, lsdRKR, lsdRKS, lsdRKT,
                                                      lsdFFAILR, lsdFFAILS, lsdFFAILT, lsdMFAILR, lsdMFAILS, lsdMFAILT, 
                                                      lsdUFAILR, lsdUFAILS, lsdUFAILT, lsdTFAILR, lsdTFAILS, lsdTFAILT,
                                                      lsdFOR, lsdFOS, lsdFOT, lsdMOR, lsdMOS, lsdMOT };
    vector<sdiString> attrNameList = { "LSD_MAT68_TKR", "LSD_MAT68_TKS", "LSD_MAT68_TKT", "LSD_MAT68_RKR", "LSD_MAT68_RKS", "LSD_MAT68_RKT",
                                      "FFAILR", "FFAILS", "FFAILT", "MFAILR", "MFAILS", "MFAILT", 
                                      "UFAILR", "UFAILS", "UFAILT", "TFAILR", "TFAILS", "TFAILT", 
                                      "FOR", "FOS", "FOT", "MOR", "MOS", "MOT"};
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

    vector<double> stiffList({ lsdTKR, lsdTKS, lsdTKT, lsdRKR, lsdRKS, lsdRKT });
    vector<double> preLoadList({ lsdFOR, lsdFOS, lsdFOT, lsdMOR, lsdMOS, lsdMOT });
    vector<sdiString> lsdLCPAttrNames({ "LCPDR","LCPDS","LCPDT","LCPMR","LCPMS","LCPMT" });
    int iFail2 = -1;
    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
    EntityType radFunctEntType = p_radiossModel->GetEntityType("/FUNCT");
    for (int i = 0; i < 6; i++)
    {
        double preLoad = preLoadList[i];
        double stiff = stiffList[i];
        if (stiff != 0.0)
        {
            HandleRead lcpHandle;
            dynaMat.GetEntityHandle(sdiIdentifier(lsdLCPAttrNames[i]), lcpHandle);
            if (lcpHandle.IsValid())
            {
                HandleEdit functHRead;
                EntityRead lcpEntRead(p_lsdynaModel, lcpHandle);
                if (lcpHandle.IsValid())
                {
                    int nPnts = 0;
                    sdiDoubleList crvPoints;
                    double lsdSFA = 1.0;
                    double lsdSFO = 1.0;
                    double lsdOFFA = 0.0;
                    double lsdOFFO = 0.0;
                    EntityRead lcpRead(p_lsdynaModel, lcpHandle);
                    sdiValue tempValue(crvPoints);
                    lcpRead.GetValue(sdiIdentifier("points"), tempValue);
                    tempValue.GetValue(crvPoints);

                    tempValue = sdiValue(nPnts);
                    lcpRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
                    tempValue.GetValue(nPnts);

                    vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
                    vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
                    p_ConvertUtils.GetAttribValues(lcpRead, lsdQueryAttribs, attribVals);
                    lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
                    lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

                    if (nPnts > 1)
                    {
                        ConvertPlasticDispPointsTotalDisp(stiff, crvPoints);
                        p_ConvertUtils.CreateCurve("Recaluclated_Curve_" + to_string(lcpRead.GetId()) + "_MatL68_" + to_string(dynaMatId),
                            (int)crvPoints.size() / 2, crvPoints, functHRead, lsdSFA, lsdSFO, lsdSFA * lsdOFFA + lsdSFO * lsdOFFO + preLoad);
                    }
                }
                else
                {
                    p_ConvertUtils.CreateCurve("NewCurve_MatL68_" + to_string(dynaMatId) + "_fct_ID1" + to_string(i+1),
                        3, { {-1,-preLoad -stiff,0.0, -preLoad, 1,preLoad + stiff } }, functHRead, 1.0, 1.0, 0.0 + preLoad);
                }
                if (functHRead.IsValid())
                {
                    radmatEntityEdit.SetEntityHandle(sdiIdentifier("fct_ID1" + to_string(i+1)), functHRead);
                    radmatEntityEdit.SetValue(sdiIdentifier("H" + to_string(i+1)), sdiValue(1));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(functHRead, sourcemat));
                }
            }
        }
    }
    
    sdiDoubleList optFailParms1({ lsdFFAILR, lsdFFAILS, lsdFFAILT, lsdMFAILR, lsdMFAILS, lsdMFAILT });
    auto itr = find_if(optFailParms1.begin(), optFailParms1.end(),
        [&](const double val) { return val != 0.0; });
    if (itr != optFailParms1.end())
        iFail2 = 1;
    else
    {
        optFailParms1 = { { lsdUFAILR, lsdUFAILS, lsdUFAILT, lsdTFAILR, lsdTFAILS, lsdTFAILT } };
        auto itr = find_if(optFailParms1.begin(), optFailParms1.end(),
            [](const double val) { return val != 0.0; });

        if (itr != optFailParms1.end())
            iFail2 = 0;
    }
    if (iFail2 != -1)
    {
        for (size_t i = 0; i < optFailParms1.size(); ++i)
        {
            double failParam = optFailParms1[i];
            if (failParam != 0.0 && i < 3)
            {
                radmatEntityEdit.SetValue(sdiIdentifier("DeltaMin" + to_string(i + 1)), sdiValue(-failParam));
                radmatEntityEdit.SetValue(sdiIdentifier("DeltaMax" + to_string(i + 1)), sdiValue(failParam));
            }
            else if (failParam != 0.0 && i < 6)
            {
                radmatEntityEdit.SetValue(sdiIdentifier("ThetaMin" + to_string(i + 1)), sdiValue(-failParam));
                radmatEntityEdit.SetValue(sdiIdentifier("ThetaMax" + to_string(i + 1)), sdiValue(failParam));
            }
        }
        radmatEntityEdit.SetValue(sdiIdentifier("Ifail2"), sdiValue(iFail2));
    }
    radmatEntityEdit.SetValue(sdiIdentifier("Ifail"), sdiValue(1));
    /* As there seem to be cases where the warning is wrong, we unplug it for 2021.1,
    * as requested by Bertrand and Erwan
    if (iFail2 == -1)
    {
        // warning
        DynaToRad::ShowMessage(sdiMessageHandler::Level::Warning, 4,
            dynaMat.GetKeyword().c_str(), dynaMatId, dynaMatName.c_str());
    }
    */
}

void ConvertMat::ConvertMatL181ToMatL70(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat)
{
    double initialMaxSlope = 0.0;
    double lsdRHO;
    double lsdKM;
    double lsdSGL;
    double lsdSW;
    double lsdST;
    double lsdHU;
    double lsdSHAPE;
    sdiValueEntity unLoadCurveId;
    HandleRead lsdLCTBIDHandle;
    HandleRead lsdLCUNLDHandle;
    sdiValueEntityList curveList;
    sdiUIntList funcIdList;
    sdiUIntList newFunctIdList;
    sdiValue tempValue;
    sdiDoubleList strainRatesList;
    sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };

    vector<reference_wrapper<double>> attribVals({ lsdRHO, lsdKM, lsdSGL, lsdSW, lsdST, lsdHU, lsdSHAPE });
    vector<sdiString> attribNames({ "RHO", "KM", "LSD_MAT_SGL", "LSD_MAT_SW", "LSD_MAT_ST", "HU", "SHAPE" });
    p_ConvertUtils.GetAttribValues(dynaMat, attribNames, attribVals);

    destCard = "/MAT/LAW70";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);

    strainRatesList.clear();
    dynaMat.GetEntityHandle(sdiIdentifier("LSD_MAT_TBID"), lsdLCTBIDHandle);
    if (lsdLCTBIDHandle.IsValid() && lsdSGL != 0.0 && lsdSW != 0.0 && lsdST != 0.0)
    {
        HandleEdit tempCrvEdit;

        EntityRead lsdLCTBIDRead(p_lsdynaModel, lsdLCTBIDHandle);
        sdiValue tempValue(curveList);
        lsdLCTBIDRead.GetValue(sdiIdentifier("CurveIds"), tempValue);
        tempValue.GetValue(curveList);
        curveList.GetIdList(funcIdList);
        int nFunct = (int)funcIdList.size();

        tempValue = sdiValue(strainRatesList);
        lsdLCTBIDRead.GetValue(sdiIdentifier("VALUE"), tempValue);
        tempValue.GetValue(strainRatesList);

        for (int i = 0; i < nFunct; ++i)
        {
            double lsdSFA = 1.0;
            double lsdSFO = 1.0;
            double lsdOFFA = 0.0;
            double lsdOFFO = 0.0;
            HandleRead lsdCurveRead;
            HandleEdit curveEdit;
            sdiDoubleList allPoints;
            int nPnts = 0;

            EntityType lsdCurveType = p_lsdynaModel->GetEntityType("*DEFINE_CURVE");
            p_lsdynaModel->FindById(lsdCurveType, funcIdList[i], lsdCurveRead);

            sdiValue tempValue(nPnts);
            lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("numberofpoints"), tempValue);
            tempValue.GetValue(nPnts);


            allPoints.clear();
            tempValue = sdiValue(allPoints);
            lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("points"), tempValue);
            tempValue.GetValue(allPoints);

            tempValue = sdiValue(lsdSFA);
            lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("SFA"), tempValue);
            tempValue.GetValue(lsdSFA);
            lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;

            tempValue = sdiValue(lsdSFO);
            lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("SFO"), tempValue);
            tempValue.GetValue(lsdSFO);
            lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

            tempValue = sdiValue(lsdOFFA);
            lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("OFFA"), tempValue);
            tempValue.GetValue(lsdOFFA);

            tempValue = sdiValue(lsdOFFO);
            lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("OFFO"), tempValue);
            tempValue.GetValue(lsdOFFO);

            vector<pair<double, double>> strainVsStressList;
            strainVsStressList.reserve(nPnts * 2);

            double slope = (allPoints[3] - allPoints[1]) / (allPoints[2] - allPoints[0]);
            if (initialMaxSlope == 0 || initialMaxSlope < slope)
                initialMaxSlope = slope;

            for (size_t i = 0; i < 2 * nPnts; i += 2)
            {
                strainVsStressList.push_back(make_pair(-1 * allPoints[i] / lsdSGL,
                    -1 * allPoints[i + 1] / (lsdSW * lsdST)));
            }

            sort(strainVsStressList.begin(), strainVsStressList.end(),
                [](const pair<double, double>& p1, const pair<double, double>& p2)
                {
                    return p1.first < p2.first;
                }
            );

            allPoints.clear();
            for (auto pair : strainVsStressList)
            {
                allPoints.push_back(pair.first);
                allPoints.push_back(pair.second);
            }

            sdiString curveName(lsdCurveRead.GetName(p_lsdynaModel) + to_string(lsdCurveRead.GetId(p_lsdynaModel)));
            p_ConvertUtils.CreateCurve(curveName, nPnts, allPoints, tempCrvEdit, lsdSFA, lsdSFO, lsdOFFA, lsdOFFO);
            if (tempCrvEdit.IsValid())
            {
                newFunctIdList.push_back(tempCrvEdit.GetId(p_radiossModel));
                sdiConvert::Convert::PushToConversionLog(std::make_pair(tempCrvEdit, sourcemats));
            }
        }
        if (tempCrvEdit.IsValid())
        {
            newFunctIdList.push_back(tempCrvEdit.GetId(p_radiossModel));
            strainRatesList.push_back(strainRatesList[strainRatesList.size() - 1] * 10.0);
        }

        radmatEntityEdit.SetValue(sdiIdentifier("NRATEP"), sdiValue((int)strainRatesList.size()));
        radmatEntityEdit.SetValue(sdiIdentifier("STRAINRATE_LOAD"), sdiValue(strainRatesList));
        radmatEntityEdit.SetValue(sdiIdentifier("FUN_LOAD"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), newFunctIdList)));
    }

    dynaMat.GetEntityHandle(sdiIdentifier("LCUNLD"), lsdLCUNLDHandle);
    if (lsdLCUNLDHandle.IsValid())
    {
        HandleEdit tempCrvEdit;
        EntityRead lsdLCUNLDRead(p_lsdynaModel, lsdLCUNLDHandle);
        HandleEdit curveEdit;
        sdiDoubleList allPoints;
        int nPnts = 0;

        sdiValue tempValue(nPnts);
        lsdLCUNLDRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
        tempValue.GetValue(nPnts);

        allPoints.clear();
        tempValue = sdiValue(allPoints);
        lsdLCUNLDRead.GetValue(sdiIdentifier("points"), tempValue);
        tempValue.GetValue(allPoints);

        double lsdSFA = 1.0;
        double lsdSFO = 1.0;
        double lsdOFFA = 0.0;
        double lsdOFFO = 0.0;
        vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
        vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
        p_ConvertUtils.GetAttribValues(lsdLCUNLDRead, lsdQueryAttribs, attribVals);

        lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
        lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

        vector<pair<double, double>> strainVsStressList;
        strainVsStressList.reserve(nPnts * 2);

        for (size_t i = 0; i < 2 * nPnts; i += 2)
        {
            strainVsStressList.push_back(make_pair(-1 * allPoints[i] / lsdSGL,
                -1 * allPoints[i + 1] / (lsdSW * lsdST)));
        }

        sort(strainVsStressList.begin(), strainVsStressList.end(),
            [](const pair<double, double>& p1, const pair<double, double>& p2)
            {
                return p1.first < p2.first;
            }
        );

        allPoints.clear();
        for (auto pair : strainVsStressList)
        {
            allPoints.push_back(pair.first);
            allPoints.push_back(pair.second);
        }

        sdiString curveName(lsdLCUNLDRead.GetName() + to_string(lsdLCUNLDRead.GetId()));
        p_ConvertUtils.CreateCurve(curveName, nPnts, allPoints, tempCrvEdit, lsdSFA, lsdSFO, lsdOFFA, lsdOFFO);

        if (tempCrvEdit.IsValid())
        {
            sdiConvert::Convert::PushToConversionLog(std::make_pair(tempCrvEdit, sourcemats));
        }

        newFunctIdList.clear();
        newFunctIdList.push_back(tempCrvEdit.GetId(p_radiossModel));
        radmatEntityEdit.SetValue(sdiIdentifier("NRATEN"), sdiValue(1));
        radmatEntityEdit.SetValue(sdiIdentifier("FUN_UNLOAD"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), newFunctIdList)));
        radmatEntityEdit.SetValue(sdiIdentifier("Iflag"), sdiValue(0));
    }
    else if (lsdHU > 0)
    {
        radmatEntityEdit.SetValue(sdiIdentifier("NRATEN"), sdiValue(0));
        radmatEntityEdit.SetValue(sdiIdentifier("Iflag"), sdiValue(4));
        radmatEntityEdit.SetValue(sdiIdentifier("Shape"), sdiValue(lsdSHAPE));
        if (lsdHU == 0)
            lsdHU = 1e-20;
        radmatEntityEdit.SetValue(sdiIdentifier("Hys"), sdiValue(lsdHU));
    }
    else if (lsdLCTBIDHandle.IsValid())
    {
        radmatEntityEdit.SetValue(sdiIdentifier("NRATEN"), sdiValue((int)strainRatesList.size()));
        radmatEntityEdit.SetValue(sdiIdentifier("STRAINRATE_UNLOAD"), sdiValue(strainRatesList));
        radmatEntityEdit.SetValue(sdiIdentifier("FUN_UNLOAD"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), newFunctIdList)));
        radmatEntityEdit.SetValue(sdiIdentifier("Iflag"), sdiValue(0));
    }

    radmatEntityEdit.SetValue(sdiIdentifier("RHO_I"), sdiValue(lsdRHO));
    radmatEntityEdit.SetValue(sdiIdentifier("NU"), sdiValue(0.0));
    radmatEntityEdit.SetValue(sdiIdentifier("E_max"), sdiValue(3.0 * lsdKM));
    radmatEntityEdit.SetValue(sdiIdentifier("EPS_max"), sdiValue(0.0));
    radmatEntityEdit.SetValue(sdiIdentifier("F_smooth"), sdiValue(1));
}

void ConvertMat::p_ConvertMatL71(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    attribMap = { {"RHO", "RHO_I"} };
    destCard = "/MAT/LAW113";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    radmatEntityEdit.SetValue(sdiIdentifier("Mat_Name_OR_LawNo"), sdiValue(2));

    sdiValue tempValue;
    double lsdE;
    double lsdF0;
    double lsdTMAXF0;
    double radK = 1.0;
    vector<reference_wrapper<double>> attrValList = { lsdE, lsdF0, lsdTMAXF0 };
    vector<sdiString> attrNameList = { "E", "F0", "TMAXF0"};
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);
    bool radKIsParamE = false; // not really straightforward, to be improved

    if (lsdE < 0)
    {
        p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "abs(E)", "K1");
        radKIsParamE = dynaMat.IsParameterized(sdiIdentifier("E"));
        radK=lsdE;
    }
    else
    {
        unsigned int partId = p_PartBeingConverted.GetId(p_lsdynaModel);
        HandleEdit parHEdit;
        p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), partId, parHEdit);
        if (parHEdit.IsValid())
        {
            HandleEdit propHread;
            parHEdit.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHread);
            if (propHread.IsValid())
            {
                double lsdCA;
                tempValue = sdiValue(lsdCA);
                propHread.GetValue(p_lsdynaModel, sdiIdentifier("CA"), tempValue);
                tempValue.GetValue(lsdCA);                
                radmatEntityEdit.SetValue(sdiIdentifier("K1"), sdiValue(lsdE*lsdCA));
                radK=lsdE*lsdCA;
            }
        }
    }

    HandleEdit fctNEdit;
    dynaMat.GetEntityHandle(sdiIdentifier("LCID"), fctNEdit);
    if (fctNEdit.IsValid())
    {
        EntityRead crvEntRead(p_lsdynaModel, fctNEdit);
        EntityId crvId = crvEntRead.GetId();
        sdiString curveName = crvEntRead.GetName();
        int crvLen = 0;               
        sdiValue tempVal(crvLen);
        crvEntRead.GetValue(sdiIdentifier("numberofpoints"), tempVal);
        tempVal.GetValue(crvLen);
        sdiDoubleList curvePnts;
        if (crvLen)
        {
            if (lsdF0 != 0.0)
            {
                double lsdSFA = 1.0;
                double lsdSFO = 1.0;
                double lsdOFFA = 0.0;
                double lsdOFFO = 0.0;
                vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
                vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
                p_ConvertUtils.GetAttribValues(crvEntRead, lsdQueryAttribs, attribVals);

                lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
                lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

                curvePnts.reserve(2 * crvLen);
                tempVal = sdiValue(curvePnts);
                crvEntRead.GetValue(sdiIdentifier("points"), tempVal);
                tempVal.GetValue(curvePnts);
                HandleEdit dupCurveHEdit;
                
                if (lsdTMAXF0 != 0.0)
                {
                    p_ConvertUtils.CreateCurve(curveName + "_Duplicate", (int)curvePnts.size() / 2, { curvePnts }, dupCurveHEdit, lsdSFA, lsdSFO, lsdSFA * lsdOFFA, lsdSFO * lsdOFFO); //used OFFO
                }
                else
                {
                    p_ConvertUtils.CreateCurve(curveName + "_Duplicate", (int)curvePnts.size() / 2, { curvePnts }, dupCurveHEdit, lsdSFA, lsdSFO, lsdSFA * lsdOFFA, lsdSFO * lsdOFFO + lsdF0); //used OFFO+F0
                }
                radmatEntityEdit.SetEntityHandle(sdiIdentifier("fct_ID11"), dupCurveHEdit);

                sdiConvert::Convert::PushToConversionLog(std::make_pair(dupCurveHEdit, sourcemat));
            }
            else
            {
                radmatEntityEdit.SetEntityHandle(sdiIdentifier("fct_ID11"), fctNEdit);
            }
        }
    }
    else
    {
        HandleEdit newCurveHEdit;
        if (lsdTMAXF0 != 0.0)
        {
            p_ConvertUtils.CreateCurve("New CURVE for MatId:"+to_string(dynaMatId), 3, { { -1.0, 0.0, 0.0, 0.0, 1.0, radK} }, newCurveHEdit,1.0,1.0,0.0,0.0);
        }
        else
        {
            p_ConvertUtils.CreateCurve("New CURVE for MatId:"+to_string(dynaMatId), 3, { { -1.0, 0.0, 0.0, 0.0, 1.0, radK} }, newCurveHEdit,1.0,1.0,0.0,lsdF0);
        }
        if (newCurveHEdit.IsValid())
        {
            radmatEntityEdit.SetEntityHandle(sdiIdentifier("fct_ID11"), newCurveHEdit);
            if(radKIsParamE)
            {
                EntityEdit newCurveEntEdit(p_radiossModel, newCurveHEdit);
                p_ConvertUtils.CopyValue(dynaMat, newCurveEntEdit, "E", "points", UINT_MAX, 5);
            }
            sdiConvert::Convert::PushToConversionLog(std::make_pair(newCurveHEdit, sourcemat));
        }
    }

    //The rest of parameters are set to default 0 values in converson
    radmatEntityEdit.SetValue(sdiIdentifier("Ileng"), sdiValue(1));
    vector<sdiString>::iterator item;
    vector<sdiString> defaultAttrI({ "Ifail","Ifail2", 
                                    "H1","H2","H3","H4","H5","H6",
                                    "Fsmooth" });
    
    vector<sdiString> defaultAttrD({ "K2","K3","K4","K5","K6",
                                    "DAMP1","DAMP2","DAMP3","DAMP4","DAMP5","DAMP6", //DAMP1...DAMP6 are used because C1...C6 are variables in the cfg                                
                                    "A1","A2","A3","A4","A5","A6",
                                    "B1","B2","B3","B4","B5","B6",
                                    "D1","D2","D3","D4","D5","D6",  
                                    "DeltaMin1","DeltaMin2","DeltaMin3","ThetaMin4","ThetaMin5","ThetaMin6",
                                    "DeltaMax1","DeltaMax2","DeltaMax3","ThetaMax4","ThetaMax5","ThetaMax6",
                                    "F1","F2","F3","F4","F5","F6",
                                    "E1","E2","E3","E4","E5","E6",
                                    "Ascale1","Ascale2","Ascale3","Ascale4","Ascale5","Ascale6",
                                    "Hscale1","Hscale2","Hscale3","Hscale4","Hscale5","Hscale6",
                                    "Vo","Wo","Fcut",
                                    "c1","c2","c3","c4","c5","c6",
                                    "n1","n2","n3","n4","n5","n6", 
                                    "alpha1","alpha2","alpha3","alpha4","alpha5","alpha6", 
                                    "beta1","beta2","beta3","beta4","beta5","beta6"});

    for (item = defaultAttrI.begin(); item < defaultAttrI.end(); item++)
    {
        radmatEntityEdit.SetValue(sdiIdentifier(*item), sdiValue(0));
    }

    for (item = defaultAttrD.begin(); item < defaultAttrD.end(); item++)
    {
        radmatEntityEdit.SetValue(sdiIdentifier(*item), sdiValue(0.0));
    }
}

void ConvertMat::p_ConvertMatL73(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    attribMap = { {"RHO", "Rho_I"}, {"E", "E0"}, {"HU", "Hys"}, {"SHAPE", "Shape"}};
    destCard = "/MAT/LAW90";
    
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    
    radmatEntityEdit.SetValue(sdiIdentifier("Ismooth"), sdiValue(1));
    radmatEntityEdit.SetValue(sdiIdentifier("Fcut"), sdiValue(0.0));
        
    HandleRead lcidHandle;
    dynaMat.GetEntityHandle(sdiIdentifier("LSD_LCID"), lcidHandle);
    unsigned int lcId = lcidHandle.GetId(p_lsdynaModel);

    // NL -> number of loading functions
    radmatEntityEdit.SetValue(sdiIdentifier("NL"), sdiValue(1));
    if (lcidHandle.IsValid())
        radmatEntityEdit.SetValue(sdiIdentifier("fct_IDL",0,0), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), lcId)));

    // Prony
    sdiString radMatName = radMat.GetName(p_radiossModel);
    unsigned int radMatId = radMat.GetId(p_radiossModel);
    
    HandleEdit ViscPronyHEdit;
    p_radiossModel->CreateEntity(ViscPronyHEdit, "/VISC/PRONY", radMatName, radMatId);
    sdiDoubleList lsdGI;
    sdiDoubleList lsdBETAI;
    sdiDoubleList radTimeRel;
    sdiDoubleList radGammaArr;
    vector<reference_wrapper<sdiDoubleList>> attrValList = { lsdGI, lsdBETAI };
    vector<sdiString> attrNameList = { "GI", "BETAI" };
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);
    radTimeRel.reserve(lsdBETAI.size());
    radGammaArr.reserve(lsdGI.size());

    int nprony=0;
    if (ViscPronyHEdit.IsValid())
    {
        EntityEdit ViscPronyEdit(p_radiossModel, ViscPronyHEdit);
        for (int i = 0; i < lsdBETAI.size(); i++)
        {
            if(lsdBETAI[i] > 0.0)
            {
                nprony++;
                radTimeRel.push_back(lsdGI[i]);
                radGammaArr.push_back(lsdBETAI[i]);
            }
        }

        ViscPronyEdit.SetValue(sdiIdentifier("M"), sdiValue(nprony));
        ViscPronyEdit.SetValue(sdiIdentifier("G_i"), sdiValue(radTimeRel));
        ViscPronyEdit.SetValue(sdiIdentifier("Beta_i"), sdiValue(radGammaArr));

        sdiConvert::SDIHandlReadList sourceList = { {dynaMat.GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(ViscPronyHEdit, sourceList));
    }
}

void ConvertMat::p_ConvertMatL74(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    attribMap = { {"RHO", "RHO_I"},{"D", "C1"}, {"TDF","DeltaMax1"} };
    destCard = "/MAT/LAW113";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    radmatEntityEdit.SetValue(sdiIdentifier("Mat_Name_OR_LawNo"), sdiValue(2));

    sdiValue tempValue;
    double lsdK;
    double lsdCDF;
    double lsdF0;
    double lsdC2;
    double lsdC1;
    double lsdDLE;

    vector<reference_wrapper<double>> attrValList = { lsdK, lsdCDF, lsdF0, lsdC2, lsdC1, lsdDLE};
    vector<sdiString> attrNameList = { "K","CDF", "F0", "C2", "C1", "DLE"};
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

    ContainStrVsDouble mapAttrNameVsAttVal({{"DeltaMin1" ,lsdCDF * -1 }, {"A1", 1.0 }, {"F1", 1.0}}); 
    for (auto tempPair : mapAttrNameVsAttVal)
        radmatEntityEdit.SetValue(sdiIdentifier(tempPair.first), sdiValue(tempPair.second));

    radmatEntityEdit.SetValue(sdiIdentifier("Hscale1"), sdiValue(1.0)); 

    HandleEdit fctNEdit;
    dynaMat.GetEntityHandle(sdiIdentifier("FLCID"), fctNEdit);

    if (fctNEdit.IsValid())
    {
        EntityRead crvEntRead(p_lsdynaModel, fctNEdit);
        EntityId crvId = crvEntRead.GetId();
        sdiString curveName = crvEntRead.GetName();
        int crvLen = 0;
        sdiValue tempVal(crvLen);
        crvEntRead.GetValue(sdiIdentifier("numberofpoints"), tempVal);
        tempVal.GetValue(crvLen);
        sdiDoubleList curvePnts;
        if (crvLen && lsdF0 != 0.0)
        {
            double lsdSFA = 1.0;
            double lsdSFO = 1.0;
            double lsdOFFA = 0.0;
            double lsdOFFO = 0.0;
            vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
            vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
            p_ConvertUtils.GetAttribValues(crvEntRead, lsdQueryAttribs, attribVals);

            lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
            lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
            lsdSFO = (lsdK != 0.0) ? lsdSFO *= lsdK : lsdSFO;

            curvePnts.reserve(2 * crvLen);
            tempVal = sdiValue(curvePnts);
            crvEntRead.GetValue(sdiIdentifier("points"), tempVal);
            tempVal.GetValue(curvePnts);
            HandleEdit dupCurveHEdit;
            p_ConvertUtils.CreateCurve(curveName + "_Duplicate", (int)curvePnts.size() / 2, curvePnts, dupCurveHEdit, lsdSFA, lsdSFO, lsdSFA * lsdOFFA, lsdSFO * lsdOFFO + lsdF0);
            radmatEntityEdit.SetEntityHandle(sdiIdentifier("fct_ID11"), dupCurveHEdit);

            sdiConvert::Convert::PushToConversionLog(std::make_pair(dupCurveHEdit, sourcemat));
        }



        sdiValueEntity glcidEntity;
        tempValue = sdiValue(glcidEntity);
        dynaMat.GetValue(sdiIdentifier("GLCID"), tempValue);
        tempValue.GetValue(glcidEntity);
        if (glcidEntity.GetId())
            radmatEntityEdit.SetValue(sdiIdentifier("fct_ID51"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), glcidEntity.GetId())));

        ContainStrVsDouble mapAttrNameVsAttVal({{"Dcoeft1", lsdDLE }, {"B1", lsdC2 }, {"E1", lsdC1 }});
        for (auto tempPair : mapAttrNameVsAttVal)
            radmatEntityEdit.SetValue(sdiIdentifier(tempPair.first), sdiValue(tempPair.second));

        //HandleEdit newCurveN2;
        //p_ConvertUtils.CreateCurve(dynaMatName, 3, { { -1.0, -1.0, 0.0, 0.0, 1.0, 1.0} }, newCurveN2);
        //if (newCurveN2.IsValid())
        //{
        //    radmatEntityEdit.SetEntityHandle(sdiIdentifier("fct_ID21"), newCurveN2);
        //    sdiConvert::Convert::PushToConversionLog(std::make_pair(newCurveN2, sourcemat));
        //}
    }
    else
    {
        if (lsdF0 != 0.0)
        {
            HandleEdit newCurveHEdit;
            p_ConvertUtils.CreateCurve(dynaMatName, 3, { { -1.0, -1.0, 0.0, 0.0, 1.0, 1.0} }, newCurveHEdit, 1.0, (lsdK == 0) ? 1.0 : lsdK, 0.0, lsdF0);
            if (newCurveHEdit.IsValid())
            {
                radmatEntityEdit.SetEntityHandle(sdiIdentifier("fct_ID11"), newCurveHEdit);
                sdiConvert::Convert::PushToConversionLog(std::make_pair(newCurveHEdit, sourcemat));
            }
        }
        radmatEntityEdit.SetValue(sdiIdentifier("K1"), sdiValue(lsdK));
    }

    sdiValueEntity hlcidEntity = GetValue<sdiValueEntity>(dynaMat, "HLCID");
    if (hlcidEntity.GetId())
        radmatEntityEdit.SetValue(sdiIdentifier("fct_ID21"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), hlcidEntity.GetId())));

}

void ConvertMat::p_ConvertMatL76(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    sdiValue tempValue;
    destCard = "/MAT/LAW42";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
    attribMap = { {"Rho", "RHO_I"} };
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);

    double lsdPR;
    dynaMat.GetValue(sdiIdentifier("PR"), tempValue);
    tempValue.GetValue(lsdPR);
    radmatEntityEdit.SetValue(sdiIdentifier("Nu"), sdiValue(0.495));
    p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "BULK * 0.01", "Mu_1");
    p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "-BULK * 0.01", "Mu_2");
    radmatEntityEdit.SetValue(sdiIdentifier("alpha_1"), sdiValue(2.0));
    radmatEntityEdit.SetValue(sdiIdentifier("alpha_2"), sdiValue(-2.0));

    HandleEdit ViscPronyHEdit;
    sdiString radMatName = radMat.GetName(p_radiossModel);
    unsigned int radMatId = radMat.GetId(p_radiossModel);

    sdiValue tempVal;
    sdiDoubleList lsdGi;
    sdiDoubleList lsdBeta;

    tempVal = sdiValue(lsdGi);
    dynaMat.GetValue(sdiIdentifier("GI"), tempVal);
    tempVal.GetValue(lsdGi);

    lsdBeta.clear();
    tempVal = sdiValue(lsdBeta);
    dynaMat.GetValue(sdiIdentifier("BETA"), tempVal);
    tempVal.GetValue(lsdBeta);

    HandleRead lcidHandle;
    dynaMat.GetEntityHandle(sdiIdentifier("LSD_LCID"), lcidHandle);
    unsigned int lcId = lcidHandle.GetId(p_lsdynaModel);

    HandleRead lcidkHandle;
    dynaMat.GetEntityHandle(sdiIdentifier("LSD_LCIDK"), lcidkHandle);
    unsigned int lcIdk = lcidkHandle.GetId(p_lsdynaModel);

    p_radiossModel->CreateEntity(ViscPronyHEdit, "/VISC/PRONY", radMatName, radMatId);
    if (ViscPronyHEdit.IsValid())
    {
        EntityEdit ViscPronyEdit(p_radiossModel, ViscPronyHEdit);

        if (lcId > 0 && lcIdk > 0)
        {
            ViscPronyEdit.SetValue(sdiIdentifier("Itab"), sdiValue(1));
            if (lcidHandle.IsValid())
            {
                radmatEntityEdit.SetValue(sdiIdentifier("Ifunc_G"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), lcidHandle.GetId(p_lsdynaModel))));
            }
            if (lcidkHandle.IsValid())
            {
                radmatEntityEdit.SetValue(sdiIdentifier("Ifunc_K"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), lcidkHandle.GetId(p_lsdynaModel))));
            }

            p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "min(max(NT,NTK),6)", "M");
        }
        else
        {
            ViscPronyEdit.SetValue(sdiIdentifier("Itab"), sdiValue(0));
            ViscPronyEdit.SetValue(sdiIdentifier("M"), sdiValue((int)lsdGi.size()));

            p_ConvertUtils.CopyValue(dynaMat, ViscPronyEdit, "GI", "G_i");
            p_ConvertUtils.CopyValue(dynaMat, ViscPronyEdit, "KI", "Ki");
            p_ConvertUtils.CopyValue(dynaMat, ViscPronyEdit, "BETAI", "Beta_i");
            p_ConvertUtils.CopyValue(dynaMat, ViscPronyEdit, "BETAK", "Beta_ki");


        }

        sdiConvert::SDIHandlReadList sourceList = { {dynaMat.GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(ViscPronyHEdit, sourceList));
    }

}

void ConvertMat::p_ConvertMatL77(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    sdiValue tempValue;
    int lsdN = 0;
    dynaMat.GetValue(sdiIdentifier("N"), tempValue);
    tempValue.GetValue(lsdN);
    
    double lsdG = 0.;
    tempValue = sdiValue(lsdG);
    dynaMat.GetValue(sdiIdentifier("G"), tempValue);
    tempValue.GetValue(lsdG);
    
    double lsdSIGF = 0.;
    tempValue = sdiValue(lsdSIGF);
    dynaMat.GetValue(sdiIdentifier("SIGF"), tempValue);
    tempValue.GetValue(lsdSIGF);

    
    if (lsdN==0)
    {
        destCard = "/MAT/LAW42";
        if (matToPropsReferenceCount > 1)
            p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
        else
            p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
        attribMap = { {"Rho", "RHO_I"}, {"MU1", "Mu_1"},{"MU2", "Mu_2"},{"MU3", "Mu_3"},{"MU4", "Mu_4"},
                      {"MU5", "Mu_5"},{"MU6", "Mu_6"},{"MU7", "Mu_7"},{"MU8", "Mu_8"},{"ALPHA1", "alpha_1"},
                      {"ALPHA2", "alpha_2"},{"ALPHA3", "alpha_3"},{"ALPHA4", "alpha_4"},{"ALPHA5", "alpha_5"},
                      {"ALPHA6", "alpha_6"},{"ALPHA7", "alpha_7"},{"ALPHA8", "alpha_8"} };
        EntityEdit radmatEntityEdit(p_radiossModel, radMat);

        double lsdPR;
        dynaMat.GetValue(sdiIdentifier("PR"), tempValue);
        tempValue.GetValue(lsdPR);
        radmatEntityEdit.SetValue(sdiIdentifier("Nu"), sdiValue(abs(lsdPR)));
        if (lsdPR < 0)
        {
            //To Do: Add warning message "the Mullins effect is not take into account" 
            DynaToRad::ShowMessage(sdiMessageHandler::Level::Warning, 28,dynaMat.GetKeyword().c_str(), dynaMatId, dynaMatName.c_str());
        }

        sdiDoubleList lsdGI;
        sdiDoubleList lsdBETAI;
        sdiDoubleList radTimeRel;
        sdiDoubleList radGammaArr;
        vector<reference_wrapper<sdiDoubleList>> attrValList = { lsdGI, lsdBETAI };
        vector<sdiString> attrNameList = { "GI", "BETAI" };
        p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);
        radTimeRel.reserve(lsdBETAI.size());
        radGammaArr.reserve(lsdGI.size());

        int nprony=0;
        for (int i = 0; i < lsdBETAI.size(); i++)
        {
            if(lsdBETAI[i] > 0.0)
            {
                nprony++;
                radTimeRel.push_back(1 / lsdBETAI[i]);
                radGammaArr.push_back(lsdGI[i]);
            }
        }

        radmatEntityEdit.SetValue(sdiIdentifier("M"), sdiValue(nprony));
        radmatEntityEdit.SetValue(sdiIdentifier("Tau_arr"), sdiValue(radTimeRel));
        radmatEntityEdit.SetValue(sdiIdentifier("Gamma_arr"), sdiValue(radGammaArr));

        //The rest of parameters are set to default 0 values in converson
        radmatEntityEdit.SetValue(sdiIdentifier("sigma_cut"), sdiValue(0.0));
        radmatEntityEdit.SetValue(sdiIdentifier("Fscale_bulk"), sdiValue(0.0));
        radmatEntityEdit.SetValue(sdiIdentifier("I_form"), sdiValue(2));
    }
    else if(lsdN > 0)
    {
        destCard = "/MAT/LAW69";
        if (matToPropsReferenceCount > 1)
            p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
        else
            p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
        EntityEdit radmatEntityEdit(p_radiossModel, radMat);
        attribMap = { {"Rho", "RHO_I"} , {"N","N_PAIR"} };

        double lsdPR;
        double lsdSGL;
        double lsdSW;
        double lsdST;
        double lsdDATA;
        EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");
        vector<reference_wrapper<double>> attrValList = { lsdPR, lsdSGL, lsdSW, lsdST, lsdDATA};
        vector<sdiString> attrNameList = { "PR", "SGL","SW", "ST", "DATA" };
        p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

        radmatEntityEdit.SetValue(sdiIdentifier("NU"), sdiValue(abs(lsdPR)));
        if (lsdPR < 0)
        {
            //To Do: Add warning message: the Mullins effect is not take into account 
            DynaToRad::ShowMessage(sdiMessageHandler::Level::Warning, 28,dynaMat.GetKeyword().c_str(), dynaMatId, dynaMatName.c_str());
        }
        radmatEntityEdit.SetValue(sdiIdentifier("LAW_ID"), sdiValue((int)lsdDATA));

        HandleRead curveLCID1;
        dynaMat.GetEntityHandle(sdiIdentifier("LCID1"), curveLCID1);
        if (curveLCID1.IsValid())
        {
            EntityRead crvEntRead(p_lsdynaModel, curveLCID1);
            double lsdSFA=1.0;
            double lsdSFO=1.0;
            double lsdOFFA=0.0;
            double lsdOFFO=0.0;

            lsdSGL = (lsdSGL == 0.0) ? 1.0 : lsdSGL;
            lsdSW = (lsdSW == 0.0) ? 1.0 : lsdSW;

            lsdSFA = 1 / lsdSGL; // scaling coefficient for abscissa 
            lsdSFO = 1 / (lsdSW * lsdST); // scaling coefficient for ordinate
            
            if (lsdSFA != 1.0 || lsdSFO != 1.0)
            {
                sdiString curveName = crvEntRead.GetName();
                int crvLen = 0;
                sdiValue tempVal(crvLen);
                crvEntRead.GetValue(sdiIdentifier("numberofpoints"), tempVal);
                tempVal.GetValue(crvLen);

                if (crvLen)
                {
                    sdiDoubleList curvePnts;
                    curvePnts.reserve(2 * crvLen);
                    tempVal = sdiValue(curvePnts);
                    crvEntRead.GetValue(sdiIdentifier("points"), tempVal);
                    tempVal.GetValue(curvePnts);
                    double lsdSFAOriginal = 1.0;
                    double lsdSFOOriginal = 1.0;
                    vector< reference_wrapper<double>> attribVals({ lsdSFAOriginal,lsdSFOOriginal, lsdOFFA, lsdOFFO });
                    vector<sdiString> lsdQueryAttribs = {"SFA", "SFO", "OFFA", "OFFO" };
                    p_ConvertUtils.GetAttribValues(crvEntRead, lsdQueryAttribs, attribVals);

                    lsdSFAOriginal = (lsdSFAOriginal == 0.0) ? 1.0 : lsdSFAOriginal;
                    lsdSFOOriginal = (lsdSFOOriginal == 0.0) ? 1.0 : lsdSFOOriginal;

                    HandleEdit duplicateCurve;
                    p_ConvertUtils.CreateCurve(curveName + "_Duplicate", (int)curvePnts.size() / 2, { curvePnts }, duplicateCurve, lsdSFAOriginal * lsdSFA, lsdSFOOriginal * lsdSFO, lsdSFAOriginal * lsdOFFA, lsdSFOOriginal * lsdOFFO);
                    radmatEntityEdit.SetEntityHandle(sdiIdentifier("FCT_ID1"), duplicateCurve);
                    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(duplicateCurve, sourcemat));
                }
            }
            else
            {
                radmatEntityEdit.SetValue(sdiIdentifier("FCT_ID1"), sdiValue(sdiValueEntity(radFunctType, crvEntRead.GetId()))); // when SFA and SFO are equal to 1 the curve is not duplicate
            }
        }
        radmatEntityEdit.SetValue(sdiIdentifier("FSCALE"), sdiValue(0.0));
        radmatEntityEdit.SetValue(sdiIdentifier("ICHECK"), sdiValue(0));
    }
    
 
    if(lsdG > 0 && lsdSIGF > 0)
    {
        sdiString matName = dynaMat.GetName();
        HandleEdit viscTabEdit;
        p_radiossModel->CreateEntity(viscTabEdit, "/VISC/PLAS", matName, radMat.GetId(p_radiossModel));
        if (viscTabEdit.IsValid() && radMat.IsValid())
        {
            EntityEdit viscTabEntEdit(p_radiossModel, viscTabEdit);
            p_ConvertUtils.CopyValue(dynaMat, viscTabEntEdit, "G", "LSD_G");
            p_ConvertUtils.CopyValue(dynaMat, viscTabEntEdit, "SIGF", "LSDYNA_SIGF");
            sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(viscTabEdit, sourcemat));
        }
    }
}

void ConvertMat::p_ConvertMatL81(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat, unsigned short int &matLawNum, unsigned short int &matLawNumChoice)
{
    double lsdEPPF;
    double lsdEPPFR;
    double lsdVP;
    HandleEdit failTab1Edit;
    HandleEdit table1Edit;
    HandleEdit table2Edit;

    /*convert the material */
    p_ConvertMatL24(dynaMat, destCard, attribMap, radMat, matLawNum, matLawNumChoice);

    sdiString matName = dynaMat.GetName();
    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
    EntityType lsdCurveType = p_lsdynaModel->GetEntityType("*DEFINE_CURVE");

    vector<reference_wrapper<double>> attribVals({ lsdEPPF, lsdEPPFR, lsdVP });
    vector<sdiString> attribNames({ "EPPF", "EPPFR", "VP" });
    p_ConvertUtils.GetAttribValues(dynaMat, attribNames, attribVals);
    if(lsdEPPF == 0.0) lsdEPPF =1e+14;
    if(lsdEPPFR == 0.0) lsdEPPFR =1e+14;

    if (destCard == "/MAT/LAW36")
    {
        sdiDoubleList strainRateList;
        sdiValue tempVal(strainRateList);
        EntityEdit radMatEdit(p_radiossModel, radMat);
        radMatEdit.GetValue(sdiIdentifier("Eps_dot"), tempVal);
        tempVal.GetValue(strainRateList);
        int numFucnt = (int)strainRateList.size();
        if (numFucnt > 1)
        {
            double lastFscale = strainRateList[numFucnt - 1];
            strainRateList.pop_back();
            strainRateList.push_back(lastFscale * 5.0);
            radMatEdit.SetValue(sdiIdentifier("Eps_dot"), sdiValue(strainRateList));
        }
        radMatEdit.SetValue(sdiIdentifier("F_smooth"), sdiValue(1));
    }
    else
        radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue((int)lsdVP));

    if (lsdEPPF > 0.0)
    {
        if (!failTab1Edit.IsValid())
        {
            p_radiossModel->CreateEntity(failTab1Edit, "/FAIL/TAB1", matName);
            failTab1Edit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
        }

        EntityEdit failTab1EntEdit(p_radiossModel, failTab1Edit);
        failTab1EntEdit.SetEntityHandle(sdiIdentifier("mat_id"), radMat);

        p_radiossModel->CreateEntity(table2Edit, "/TABLE/1", matName, p_ConvertUtils.GetDynaMaxEntityID(lsdCurveType));
        if (table2Edit.IsValid())
        {
            table2Edit.SetValue(p_radiossModel, sdiIdentifier("dimension"), sdiValue(1));
            table2Edit.SetValue(p_radiossModel, sdiIdentifier("curverows"), sdiValue(2));
            table2Edit.SetValue(p_radiossModel, sdiIdentifier("entry_size"), sdiValue(2 * 2));
            table2Edit.SetValue(p_radiossModel, sdiIdentifier("table2darray"), sdiValue(sdiDoubleList({ -1.0, lsdEPPF, 1, lsdEPPF })));

            failTab1EntEdit.SetEntityHandle(sdiIdentifier("TABLE2_ID"), table2Edit);
        }
    }

    if (lsdEPPFR > 0.0)
    {
        if (!failTab1Edit.IsValid())
        {
            p_radiossModel->CreateEntity(failTab1Edit, "/FAIL/TAB1", matName);
            failTab1Edit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
        }

        EntityEdit failTab1EntEdit(p_radiossModel, failTab1Edit);
        failTab1EntEdit.SetEntityHandle(sdiIdentifier("mat_id"), radMat);

        p_radiossModel->CreateEntity(table1Edit, "/TABLE/1", matName, p_ConvertUtils.GetDynaMaxEntityID(lsdCurveType));
        if (table1Edit.IsValid())
        {
            table1Edit.SetValue(p_radiossModel, sdiIdentifier("dimension"), sdiValue(1));
            table1Edit.SetValue(p_radiossModel, sdiIdentifier("curverows"), sdiValue(2));
            table1Edit.SetValue(p_radiossModel, sdiIdentifier("entry_size"), sdiValue(2 * 2));
            table1Edit.SetValue(p_radiossModel, sdiIdentifier("table2darray"), sdiValue(sdiDoubleList({ -1.0, lsdEPPFR, 1.0, lsdEPPFR })));

            failTab1EntEdit.SetEntityHandle(sdiIdentifier("TABLE1_ID"), table1Edit);
            failTab1EntEdit.SetValue(sdiIdentifier("IFAIL_SH"), sdiValue(2));
            failTab1EntEdit.SetValue(sdiIdentifier("IFAIL_SO"), sdiValue(1));
        }
    }

    if (failTab1Edit.IsValid())
    {
        sdiConvert::Convert::PushToConversionLog(std::make_pair(failTab1Edit, sourcemat));
    }
    if (table1Edit.IsValid())
    {
        sdiConvert::Convert::PushToConversionLog(std::make_pair(table1Edit, sourcemat));
    }
    if (table2Edit.IsValid())
    {
        sdiConvert::Convert::PushToConversionLog(std::make_pair(table2Edit, sourcemat));
    }
}

void ConvertMat::p_ConvertMatL83(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    attribMap = { {"E","E0"}, {"RHO", "Rho_I"}, {"HU", "Hys"}, {"SHAPE", "Shape"} };
    destCard = "/MAT/LAW90";

    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);

    if (radMat.IsValid())
    {
        int nFunct = 0;
        sdiUIntList funcIdList;
        sdiDoubleList strainRatesList;
        sdiDoubleList scaleList;
        EntityEdit radMatEntityEdit(p_radiossModel, radMat);
        double lsdTFLAG = 0;
        double lsdTC = 0.0, lsdE = 0.0;
        sdiValue tempValue;

        tempValue = sdiValue(lsdTFLAG);
        dynaMat.GetValue(sdiIdentifier("TFLAG"), tempValue);
        tempValue.GetValue(lsdTFLAG);

        tempValue = sdiValue(lsdTC);
        dynaMat.GetValue(sdiIdentifier("TC"), tempValue);
        tempValue.GetValue(lsdTC);

        tempValue = sdiValue(lsdE);
        dynaMat.GetValue(sdiIdentifier("E"), tempValue);
        tempValue.GetValue(lsdE);

        HandleRead sigCrvHandle;
        dynaMat.GetEntityHandle(sdiIdentifier("TBID"), sigCrvHandle);
        if (sigCrvHandle.IsValid())
        {
            EntityRead sigCrvEntRead(p_lsdynaModel, sigCrvHandle);
            sdiString crvType = sigCrvEntRead.GetKeyword();
            EntityId sigYcrvId = sigCrvEntRead.GetId();

            if (!crvType.compare(0, 13, "*DEFINE_TABLE"))
            {
                sdiValueEntityList curveList;
                sdiValue tempValue(curveList);
                sigCrvEntRead.GetValue(sdiIdentifier("CurveIds"), tempValue);
                tempValue.GetValue(curveList);
                curveList.GetIdList(funcIdList);
                nFunct = (int)funcIdList.size();

                tempValue = sdiValue(strainRatesList);
                sigCrvEntRead.GetValue(sdiIdentifier("VALUE"), tempValue);
                tempValue.GetValue(strainRatesList);
                scaleList = sdiDoubleList(nFunct, 0.0);
                if (funcIdList.size())
                {
                    // loop over table curves:
                    for (size_t i = 0; i < funcIdList.size(); ++i)
                    {
                        unsigned int lcid = funcIdList[i];

                        HandleEdit radfuncHEdit;
                        p_radiossModel->FindById("/FUNCT", lcid, radfuncHEdit);
                        EntityEdit radfuncEdit(p_radiossModel, radfuncHEdit);

                        if(radfuncHEdit.IsValid())
                        {
                            //-----------------------
                            int nPnts = 0;
                            sdiDoubleList crvPoints;

                            double lsdSFA = 1.0;
                            double lsdSFO = 1.0;
                            double lsdOFFA = 0.0;
                            double lsdOFFO = 0.0;

                            tempValue = sdiValue(nPnts);
                            radfuncEdit.GetValue(sdiIdentifier("numberofpoints"), tempValue);
                            tempValue.GetValue(nPnts);
                            crvPoints.reserve(2 * nPnts + 4);
                            //crvPoints.erase(crvPoints.begin(),crvPoints.end());
                            crvPoints.clear();

                            tempValue = sdiValue(crvPoints);
                            radfuncEdit.GetValue(sdiIdentifier("points"), tempValue);
                            tempValue.GetValue(crvPoints);

                            vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
                            vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
                            p_ConvertUtils.GetAttribValues(radfuncEdit, lsdQueryAttribs, attribVals);
                            lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
                            lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
                            //-----------------------
                            if(lsdTFLAG == 0.0)
                            {
                            // ignore part of stress strain curves in 3rd quadrant
                            // and create new curves with following points in the third quadrant:
                                // case (1)
                                if(lsdE*10.0 > lsdTC &&  lsdTC > 0.0)
                                {
                                    // if E*10>TC>0: (-TC/E-1, -TC); (-TC/E, -TC)
                                    while (crvPoints[0] < 0.0)
                                    {
                                        crvPoints.erase(crvPoints.begin());
                                        crvPoints.erase(crvPoints.begin());
                                    }

                                    crvPoints.insert(crvPoints.begin(), -lsdTC);
                                    crvPoints.insert(crvPoints.begin(), -lsdTC/lsdE);

                                    crvPoints.insert(crvPoints.begin(), -lsdTC);
                                    crvPoints.insert(crvPoints.begin(), -(lsdTC/lsdE)-1.0);

                                    HandleEdit newfunctHEdit;
                                    p_ConvertUtils.CreateCurve("Duplicate_" + to_string(lcid) + "_MatL83_" + to_string(dynaMatId),
                                        (int)crvPoints.size()/2, crvPoints, newfunctHEdit, lsdSFA, lsdSFO, lsdOFFA, lsdOFFO);

                                    sdiConvert::SDIHandlReadList sourceMat = { {dynaMat.GetHandle()} };
                                    if (newfunctHEdit.IsValid())
                                    {
                                        sdiConvert::Convert::PushToConversionLog(std::make_pair(newfunctHEdit, sourceMat));
                                        funcIdList[i] = newfunctHEdit.GetId(p_radiossModel);
                                    }
                                }
                                // case (2)
                                else if(lsdTC == 0.0 || lsdTC >= lsdE*10.0)
                                {
                                    // if TC=0 and lsdTC >= lsdE*10.0: (-1, -E), (0.0, 0.0)

                                    while (crvPoints[0] < 0.0)
                                    {
                                        crvPoints.erase(crvPoints.begin());
                                        crvPoints.erase(crvPoints.begin());
                                    }

                                    crvPoints.insert(crvPoints.begin(), -lsdE);
                                    crvPoints.insert(crvPoints.begin(), -1.0);

                                    HandleEdit newfunctHEdit;
                                    p_ConvertUtils.CreateCurve("Duplicate_" + to_string(lcid) + "_MatL83_" + to_string(dynaMatId),
                                        (int)crvPoints.size()/2, crvPoints, newfunctHEdit, lsdSFA, lsdSFO, lsdOFFA, lsdOFFO);

                                    sdiConvert::SDIHandlReadList sourceMat = { {dynaMat.GetHandle()} };
                                    if (newfunctHEdit.IsValid())
                                    {
                                        sdiConvert::Convert::PushToConversionLog(std::make_pair(newfunctHEdit, sourceMat));
                                        funcIdList[i] = newfunctHEdit.GetId(p_radiossModel);
                                    }
                                }
                            }
                            else if(lsdTFLAG == 1.0)
                            {
                                // case (1)
                                if(crvPoints[0] < 0.0 && crvPoints[1] < 0.0)
                                {
                                    // existing curve in third quadrant
                                    // Replace each points (xi, yi) in the 3rd quadrant by (xi, max(yi, -TC))
                                    double maxabscisa = 0.0;
                                    if(lsdTC != 0.0) maxabscisa = -lsdTC;

                                    for (size_t j = 0; j < crvPoints.size(); j += 2)
                                    {
                                        if(crvPoints[j] < 0.0 && crvPoints[j+1] < 0.0) crvPoints[j+1] = max(crvPoints[j+1],maxabscisa);
                                    }

                                    HandleEdit newfunctHEdit;
                                    p_ConvertUtils.CreateCurve("Duplicate_" + to_string(lcid) + "_MatL83_" + to_string(dynaMatId),
                                        (int)crvPoints.size()/2, crvPoints, newfunctHEdit, lsdSFA, lsdSFO, lsdOFFA, lsdOFFO);

                                    sdiConvert::SDIHandlReadList sourceMat = { {dynaMat.GetHandle()} };
                                    if (newfunctHEdit.IsValid())
                                    {
                                        sdiConvert::Convert::PushToConversionLog(std::make_pair(newfunctHEdit, sourceMat));
                                        funcIdList[i] = newfunctHEdit.GetId(p_radiossModel);
                                    }
                                }
                                else
                                {
                                    // case (2)
                                    // non existing curve in third quadrant
                                    // if TC>0: (-TC/Eslope-1, -TC); (-TC/Eslope, -TC)

                                    double Eslope = 0.0;
                                    Eslope = crvPoints[3]-crvPoints[1];
                                    if (crvPoints[2]-crvPoints[0] > 0.0)
                                        Eslope = (crvPoints[3]-crvPoints[1])/(crvPoints[2]-crvPoints[0] > 0.0);

                                    // case (2)-1
                                    if (Eslope*10.0 > lsdTC &&  lsdTC > 0.0)
                                    {
                                        crvPoints.insert(crvPoints.begin(), -lsdTC);
                                        crvPoints.insert(crvPoints.begin(), -lsdTC/Eslope);

                                        crvPoints.insert(crvPoints.begin(), -lsdTC);
                                        crvPoints.insert(crvPoints.begin(), -(lsdTC/Eslope)-1.0);

                                        HandleEdit newfunctHEdit;
                                        p_ConvertUtils.CreateCurve("Duplicate_" + to_string(lcid) + "_MatL83_" + to_string(dynaMatId),
                                            (int)crvPoints.size()/2, crvPoints, newfunctHEdit, lsdSFA, lsdSFO, lsdOFFA, lsdOFFO);

                                        sdiConvert::SDIHandlReadList sourceMat = { {dynaMat.GetHandle()} };
                                        if (newfunctHEdit.IsValid())
                                        {
                                            sdiConvert::Convert::PushToConversionLog(std::make_pair(newfunctHEdit, sourceMat));
                                            funcIdList[i] = newfunctHEdit.GetId(p_radiossModel);
                                        }
                                    }
                                    // case (2)-2
                                    else if (lsdTC == 0.0 || lsdTC >= Eslope*10.0)
                                    {
                                        crvPoints.insert(crvPoints.begin(), -Eslope);
                                        crvPoints.insert(crvPoints.begin(), -1.0);

                                        HandleEdit newfunctHEdit;
                                        p_ConvertUtils.CreateCurve("Duplicate_" + to_string(lcid) + "_MatL83_" + to_string(dynaMatId),
                                            (int)crvPoints.size()/2, crvPoints, newfunctHEdit, lsdSFA, lsdSFO, lsdOFFA, lsdOFFO);

                                        sdiConvert::SDIHandlReadList sourceMat = { {dynaMat.GetHandle()} };
                                        if (newfunctHEdit.IsValid())
                                        {
                                            sdiConvert::Convert::PushToConversionLog(std::make_pair(newfunctHEdit, sourceMat));
                                            funcIdList[i] = newfunctHEdit.GetId(p_radiossModel);
                                        }
                                    }
                                }
                            }
                        } // if(radfuncHEdit.IsValid())
                    } // for (size_t i = 0; i < funcIdList.size(); ++i)

                    funcIdList.push_back(funcIdList[nFunct - 1]);
                    strainRatesList.push_back(strainRatesList[nFunct - 1] * 100);
                    scaleList.push_back(0.0);
                    if(strainRatesList[0] != 0.0)
                    {
                        strainRatesList.insert(strainRatesList.begin(), 0.0);
                        funcIdList.insert(funcIdList.begin(),funcIdList[0]);
                        scaleList.insert(scaleList.begin(),0.0);
                        nFunct = nFunct + 1;
                    }
                    radMatEntityEdit.SetValue(sdiIdentifier("fct_idL"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), funcIdList)));
                    radMatEntityEdit.SetValue(sdiIdentifier("eps_dot"), sdiValue(strainRatesList));
                    radMatEntityEdit.SetValue(sdiIdentifier("Fscale"), sdiValue(scaleList));
                    nFunct = nFunct + 1;
                }
            }
        }
        radMatEntityEdit.SetValue(sdiIdentifier("NL"), sdiValue(nFunct));
        radMatEntityEdit.SetValue(sdiIdentifier("Ismooth"), sdiValue(1));
        radMatEntityEdit.SetValue(sdiIdentifier("Fcut"), sdiValue(0.0));
        p_ConvertUtils.CopyValue(dynaMat, radMatEntityEdit, "EXPON", "MAT_ALPHA");

        //The rest of parameters are set to default 0 values in converson
        radMatEntityEdit.SetValue(sdiIdentifier("MAT_NU"), sdiValue(0.0));
    }
}

void ConvertMat::ConvertMatL181ToMatL88(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat)
{
    double initialMaxSlope = 0.0;
    double lsdHU;
    double lsdG;
    double lsdKM;
    double lsdNu;
    double lsdRHO;
    double lsdSGL;
    double lsdSW;
    double lsdST;
    double lsdSHAPE;
    double lsdTENSION;
    double radSFA;
    double radSFO;
    HandleRead lsdLCTBIDHandle;
    HandleRead lsdLCUNLDHandle;
    HandleEdit tempCrvEdit;
    sdiValueEntityList curveList;
    sdiUIntList funcIdList;
    sdiUIntList newFunctIdList;
    sdiValue tempValue;
    sdiDoubleList strainRatesList;
    sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();

    vector<reference_wrapper<double>> attribVals({ lsdRHO, lsdKM, lsdG, lsdSGL, lsdSW, lsdST, lsdTENSION, lsdHU, lsdSHAPE });
    vector<sdiString> attribNames({ "RHO", "KM", "G", "LSD_MAT_SGL", "LSD_MAT_SW", "LSD_MAT_ST", "TENSION", "HU", "SHAPE" });
    p_ConvertUtils.GetAttribValues(dynaMat, attribNames, attribVals);

    lsdNu = 0.0;

    destCard = "/MAT/LAW88";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);

    strainRatesList.clear();
    dynaMat.GetEntityHandle(sdiIdentifier("LSD_MAT_TBID"), lsdLCTBIDHandle);

    if (lsdLCTBIDHandle.IsValid() && lsdSGL != 0.0 && lsdSW != 0.0 && lsdST != 0.0)
    {
        EntityRead lsdLCTBIDRead(p_lsdynaModel, lsdLCTBIDHandle);
        sdiValue tempValue(curveList);
        lsdLCTBIDRead.GetValue(sdiIdentifier("CurveIds"), tempValue);
        tempValue.GetValue(curveList);
        curveList.GetIdList(funcIdList);
        int nFunct = (int)funcIdList.size();

        tempValue = sdiValue(strainRatesList);
        lsdLCTBIDRead.GetValue(sdiIdentifier("VALUE"), tempValue);
        tempValue.GetValue(strainRatesList);

        if (nFunct!= 0.0)
        {
            for (int i = 0; i < nFunct; ++i)
            {
                double lsdSFA = 1.0;
                double lsdSFO = 1.0;
                double lsdOFFA = 0.0;
                double lsdOFFO = 0.0;
                HandleRead lsdCurveRead;
                HandleEdit curveEdit;
                sdiDoubleList allPoints;
                int nPnts = 0;

                EntityType lsdCurveType = p_lsdynaModel->GetEntityType("*DEFINE_CURVE");
                p_lsdynaModel->FindById(lsdCurveType, funcIdList[i], lsdCurveRead);

                sdiValue tempValue(nPnts);
                lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("numberofpoints"), tempValue);
                tempValue.GetValue(nPnts);

                allPoints.clear();
                tempValue = sdiValue(allPoints);
                lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("points"), tempValue);
                tempValue.GetValue(allPoints);

                tempValue = sdiValue(lsdSFA);
                lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("SFA"), tempValue);
                tempValue.GetValue(lsdSFA);
                lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
                radSFA = lsdSFA / lsdSGL;

                tempValue = sdiValue(lsdSFO);
                lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("SFO"), tempValue);
                tempValue.GetValue(lsdSFO);
                lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
                radSFO = lsdSFO / (lsdSW * lsdST);

                tempValue = sdiValue(lsdOFFA);
                lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("OFFA"), tempValue);
                tempValue.GetValue(lsdOFFA);

                tempValue = sdiValue(lsdOFFO);
                lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("OFFO"), tempValue);
                tempValue.GetValue(lsdOFFO);

                sdiString curveName(lsdCurveRead.GetName(p_lsdynaModel) + to_string(lsdCurveRead.GetId(p_lsdynaModel)));
                p_ConvertUtils.CreateCurve(curveName, nPnts, allPoints, tempCrvEdit, radSFA, radSFO, lsdOFFA, lsdOFFO);
                if (tempCrvEdit.IsValid())
                {
                    newFunctIdList.push_back(tempCrvEdit.GetId(p_radiossModel));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(tempCrvEdit, sourcemats));
                }
            }

            newFunctIdList.push_back(tempCrvEdit.GetId(p_radiossModel));
            strainRatesList.push_back(strainRatesList[strainRatesList.size() - 1] * 10.0);

            radmatEntityEdit.SetValue(sdiIdentifier("NL"), sdiValue((int)strainRatesList.size()));
            radmatEntityEdit.SetValue(sdiIdentifier("EPSI_LI"), sdiValue(strainRatesList));
            radmatEntityEdit.SetValue(sdiIdentifier("FCT_ID_LI"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), newFunctIdList)));
        }
        else
        {
            double lsdSFA = 1.0;
            double lsdSFO = 1.0;
            double lsdOFFA = 0.0;
            double lsdOFFO = 0.0;
            HandleEdit curveEdit;
            sdiDoubleList allPoints;
            int nPnts = 0;
            newFunctIdList.clear();

            sdiValue tempValue(nPnts);
            lsdLCTBIDRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
            tempValue.GetValue(nPnts);

            allPoints.clear();
            tempValue = sdiValue(allPoints);
            lsdLCTBIDRead.GetValue(sdiIdentifier("points"), tempValue);
            tempValue.GetValue(allPoints);

            tempValue = sdiValue(lsdSFA);
            lsdLCTBIDRead.GetValue(sdiIdentifier("SFA"), tempValue);
            tempValue.GetValue(lsdSFA);
            lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
            radSFA = lsdSFA / lsdSGL;

            tempValue = sdiValue(lsdSFO);
            lsdLCTBIDRead.GetValue(sdiIdentifier("SFO"), tempValue);
            tempValue.GetValue(lsdSFO);
            lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
            radSFO = lsdSFO / (lsdSW * lsdST);

            tempValue = sdiValue(lsdOFFA);
            lsdLCTBIDRead.GetValue(sdiIdentifier("OFFA"), tempValue);
            tempValue.GetValue(lsdOFFA);

            tempValue = sdiValue(lsdOFFO);
            lsdLCTBIDRead.GetValue(sdiIdentifier("OFFO"), tempValue);
            tempValue.GetValue(lsdOFFO);

            sdiString curveName(lsdLCTBIDRead.GetName() + to_string(lsdLCTBIDRead.GetId()));
            p_ConvertUtils.CreateCurve(curveName, nPnts, allPoints, tempCrvEdit, radSFA, radSFO, lsdOFFA, lsdOFFO);
            if (tempCrvEdit.IsValid())
            {
                newFunctIdList.push_back(tempCrvEdit.GetId(p_radiossModel));
                sdiConvert::Convert::PushToConversionLog(std::make_pair(tempCrvEdit, sourcemats));
            }

            strainRatesList.push_back(1.0);

            radmatEntityEdit.SetValue(sdiIdentifier("NL"), sdiValue(1));
            radmatEntityEdit.SetValue(sdiIdentifier("EPSI_LI"), sdiValue(strainRatesList[0]));
            radmatEntityEdit.SetValue(sdiIdentifier("FCT_ID_LI"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), newFunctIdList)));
        }
    }

    dynaMat.GetEntityHandle(sdiIdentifier("LCUNLD"), lsdLCUNLDHandle);
    if (lsdLCUNLDHandle.IsValid() && lsdSGL != 0.0 && lsdSW != 0.0 && lsdST != 0.0)
    {
        EntityRead lsdLCUNLDRead(p_lsdynaModel, lsdLCUNLDHandle);
        double lsdSFA = 1.0;
        double lsdSFO = 1.0;
        double lsdOFFA = 0.0;
        double lsdOFFO = 0.0;
        HandleEdit curveEdit;
        sdiDoubleList allPoints;
        int nPnts = 0;
        newFunctIdList.clear();

        sdiValue tempValue(nPnts);
        lsdLCUNLDRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
        tempValue.GetValue(nPnts);

        allPoints.clear();
        tempValue = sdiValue(allPoints);
        lsdLCUNLDRead.GetValue(sdiIdentifier("points"), tempValue);
        tempValue.GetValue(allPoints);

        tempValue = sdiValue(lsdSFA);
        lsdLCUNLDRead.GetValue(sdiIdentifier("SFA"), tempValue);
        tempValue.GetValue(lsdSFA);
        lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
        radSFA = lsdSFA / lsdSGL;

        tempValue = sdiValue(lsdSFO);
        lsdLCUNLDRead.GetValue(sdiIdentifier("SFO"), tempValue);
        tempValue.GetValue(lsdSFO);
        lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
        radSFO = lsdSFO / (lsdSW * lsdST);

        tempValue = sdiValue(lsdOFFA);
        lsdLCUNLDRead.GetValue(sdiIdentifier("OFFA"), tempValue);
        tempValue.GetValue(lsdOFFA);

        tempValue = sdiValue(lsdOFFO);
        lsdLCUNLDRead.GetValue(sdiIdentifier("OFFO"), tempValue);
        tempValue.GetValue(lsdOFFO);

        sdiString curveName(lsdLCUNLDRead.GetName() + to_string(lsdLCUNLDRead.GetId()));
        p_ConvertUtils.CreateCurve(curveName, nPnts, allPoints, tempCrvEdit, radSFA, radSFO, lsdOFFA, lsdOFFO);
        if (tempCrvEdit.IsValid())
        {
            newFunctIdList.push_back(tempCrvEdit.GetId(p_radiossModel));
            radmatEntityEdit.SetValue(sdiIdentifier("FCT_ID_UN"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), newFunctIdList[0])));
            sdiConvert::Convert::PushToConversionLog(std::make_pair(tempCrvEdit, sourcemats));
        }
    } 
    else if (lsdHU > 0.0)
    {
        radmatEntityEdit.SetValue(sdiIdentifier("SHAPE"), sdiValue(lsdSHAPE));
        radmatEntityEdit.SetValue(sdiIdentifier("HYS"), sdiValue(lsdHU));
        radmatEntityEdit.SetValue(sdiIdentifier("FCT_ID_UN"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), 0)));
    }
    else if (newFunctIdList.size())
    {
        radmatEntityEdit.SetValue(sdiIdentifier("FCT_ID_UN"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), newFunctIdList[0])));
    }

    radmatEntityEdit.SetValue(sdiIdentifier("RHO_I"), sdiValue(lsdRHO));
    radmatEntityEdit.SetValue(sdiIdentifier("NU"), sdiValue(lsdNu));
    radmatEntityEdit.SetValue(sdiIdentifier("K"), sdiValue(lsdKM));
    radmatEntityEdit.SetValue(sdiIdentifier("TENSIOM"), sdiValue(lsdTENSION));
    radmatEntityEdit.SetValue(sdiIdentifier("F_SMOOTH"), sdiValue(1));
}

void ConvertMat::p_ConvertMatL183(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiConvert::SDIHandlReadList sourcemats = { {dynaMat.GetHandle()} };
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    destCard = "/MAT/LAW88";
    attribMap = { {"RHO", "RHO_I"}, {"K", "K"}, {"TENSION", "TENSION"} };

    double lsdSGL;
    double lsdSW;
    double lsdST;
    double radSFA;
    double radSFO;
    vector<reference_wrapper<double>> attribVals({ lsdSGL, lsdSW, lsdST });
    vector<sdiString> attribNames({ "LSD_MAT_SGL", "LSD_MAT_SW", "LSD_MAT_ST" });
    p_ConvertUtils.GetAttribValues(dynaMat, attribNames, attribVals);

    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");

    if (radMat.IsValid())
    {
        radmatEntityEdit.SetValue(sdiIdentifier("F_SMOOTH"), sdiValue(1.0));

        sdiUIntList funcIdList;
        sdiDoubleList scaleList;
        int nFunct;
        sdiUIntList newFunctIdList;
        sdiValue tempValue;
        
        HandleRead lsdLCUNLDHandle;
        dynaMat.GetEntityHandle(sdiIdentifier("LCUNLD"), lsdLCUNLDHandle);

        if (lsdLCUNLDHandle.IsValid())
        {
            EntityRead lsdLCUNLDRead(p_lsdynaModel, lsdLCUNLDHandle);
            HandleEdit tempCrvEdit;
            double lsdSFA = 1.0;
            double lsdSFO = 1.0;
            double lsdOFFA = 0.0;
            double lsdOFFO = 0.0;
            HandleEdit curveEdit;
            sdiDoubleList allPoints;
            int nPnts = 0;
            newFunctIdList.clear();

            sdiValue tempValue(nPnts);
            lsdLCUNLDRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
            tempValue.GetValue(nPnts);

            allPoints.clear();
            tempValue = sdiValue(allPoints);
            lsdLCUNLDRead.GetValue(sdiIdentifier("points"), tempValue);
            tempValue.GetValue(allPoints);

            tempValue = sdiValue(lsdSFA);
            lsdLCUNLDRead.GetValue(sdiIdentifier("SFA"), tempValue);
            tempValue.GetValue(lsdSFA);
            lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;

            tempValue = sdiValue(lsdSFO);
            lsdLCUNLDRead.GetValue(sdiIdentifier("SFO"), tempValue);
            tempValue.GetValue(lsdSFO);
            lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

            if (lsdSGL != 0.0 && lsdSW != 0.0 && lsdST != 0.0)
            {
                radSFA = lsdSFA / lsdSGL;
                radSFO = lsdSFO / (lsdSW * lsdST);
            }

            tempValue = sdiValue(lsdOFFA);
            lsdLCUNLDRead.GetValue(sdiIdentifier("OFFA"), tempValue);
            tempValue.GetValue(lsdOFFA);

            tempValue = sdiValue(lsdOFFO);
            lsdLCUNLDRead.GetValue(sdiIdentifier("OFFO"), tempValue);
            tempValue.GetValue(lsdOFFO);

            sdiString curveName("Duplicate_curveID_"+to_string(lsdLCUNLDRead.GetId())+"_"+lsdLCUNLDRead.GetName());
            p_ConvertUtils.CreateCurve(curveName, nPnts, allPoints, tempCrvEdit, radSFA, radSFO, lsdOFFA, lsdOFFO);
            if (tempCrvEdit.IsValid())
            {
                newFunctIdList.push_back(tempCrvEdit.GetId(p_radiossModel));
                radmatEntityEdit.SetValue(sdiIdentifier("FCT_ID_UN"), sdiValue(sdiValueEntity(radFunctType, newFunctIdList[0])));
                sdiConvert::Convert::PushToConversionLog(std::make_pair(tempCrvEdit, sourcemats));
            }
        }

        HandleRead lsdLCTBIDHandle;
        dynaMat.GetEntityHandle(sdiIdentifier("LC"), lsdLCTBIDHandle);
        if (lsdLCTBIDHandle.IsValid())
        {
            EntityRead lsdLCTBIDRead(p_lsdynaModel, lsdLCTBIDHandle);
            sdiString crvType = lsdLCTBIDRead.GetKeyword();
            EntityId crvLCTBIDId = lsdLCTBIDRead.GetId();

            if (!crvType.compare(0, 13, "*DEFINE_CURVE"))
            {
                nFunct = 1;

                HandleEdit tempCrvEdit;
                double lsdSFA = 1.0;
                double lsdSFO = 1.0;
                double lsdOFFA = 0.0;
                double lsdOFFO = 0.0;
                               
                HandleEdit curveEdit;
                sdiDoubleList allPoints;
                int nPnts = 0;
                newFunctIdList.clear();

                sdiValue tempValue(nPnts);
                lsdLCTBIDRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
                tempValue.GetValue(nPnts);

                allPoints.clear();
                tempValue = sdiValue(allPoints);
                lsdLCTBIDRead.GetValue(sdiIdentifier("points"), tempValue);
                tempValue.GetValue(allPoints);

                tempValue = sdiValue(lsdSFA);
                lsdLCTBIDRead.GetValue(sdiIdentifier("SFA"), tempValue);
                tempValue.GetValue(lsdSFA);
                lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;

                tempValue = sdiValue(lsdSFO);
                lsdLCTBIDRead.GetValue(sdiIdentifier("SFO"), tempValue);
                tempValue.GetValue(lsdSFO);
                lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

                tempValue = sdiValue(lsdOFFA);
                lsdLCTBIDRead.GetValue(sdiIdentifier("OFFA"), tempValue);
                tempValue.GetValue(lsdOFFA);

                tempValue = sdiValue(lsdOFFO);
                lsdLCTBIDRead.GetValue(sdiIdentifier("OFFO"), tempValue);
                tempValue.GetValue(lsdOFFO);

                if (lsdSGL != 0.0 && lsdSW != 0.0 && lsdST != 0.0)
                {
                    radSFA = lsdSFA / lsdSGL;
                    radSFO = lsdSFO / (lsdSW * lsdST);
                }

                sdiString curveName("Duplicate_curveID_"+to_string(lsdLCTBIDRead.GetId())+"_"+lsdLCTBIDRead.GetName());
                p_ConvertUtils.CreateCurve(curveName, nPnts, allPoints, tempCrvEdit, radSFA, radSFO, lsdOFFA, lsdOFFO);
                if (tempCrvEdit.IsValid())
                {
                    newFunctIdList.push_back(tempCrvEdit.GetId(p_radiossModel));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(tempCrvEdit, sourcemats));
                }

                scaleList.push_back(1.0);
                radmatEntityEdit.SetValue(sdiIdentifier("LAW88_NL"), sdiValue(nFunct));
                radmatEntityEdit.SetValue(sdiIdentifier("FCT_ID_LI"), sdiValue(sdiValueEntityList(radFunctType, newFunctIdList)));
                radmatEntityEdit.SetValue(sdiIdentifier("F_SCALE_LI"), sdiValue(scaleList));
            }
            else if (!crvType.compare(0, 13, "*DEFINE_TABLE"))
            {
                HandleEdit tempCrvEdit;
                sdiValueEntityList curveList;
                sdiDoubleList strainRatesList;

                strainRatesList.clear();
                newFunctIdList.clear();

                EntityRead lsdLCTBIDRead(p_lsdynaModel, lsdLCTBIDHandle);
                sdiValue tempValue(curveList);
                lsdLCTBIDRead.GetValue(sdiIdentifier("CurveIds"), tempValue);
                tempValue.GetValue(curveList);
                curveList.GetIdList(funcIdList);
                int nFunct = (int)funcIdList.size();

                tempValue = sdiValue(strainRatesList);
                lsdLCTBIDRead.GetValue(sdiIdentifier("VALUE"), tempValue);
                tempValue.GetValue(strainRatesList);

                if (nFunct!= 0.0)
                {
                    for (int i = 0; i < nFunct; ++i)
                    {
                        double lsdSFA = 1.0;
                        double lsdSFO = 1.0;
                        double lsdOFFA = 0.0;
                        double lsdOFFO = 0.0;
                        HandleRead lsdCurveRead;
                        HandleEdit curveEdit;
                        sdiDoubleList allPoints;
                        int nPnts = 0;

                        EntityType lsdCurveType = p_lsdynaModel->GetEntityType("*DEFINE_CURVE");
                        p_lsdynaModel->FindById(lsdCurveType, funcIdList[i], lsdCurveRead);

                        sdiValue tempValue(nPnts);
                        lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("numberofpoints"), tempValue);
                        tempValue.GetValue(nPnts);

                        allPoints.clear();
                        tempValue = sdiValue(allPoints);
                        lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("points"), tempValue);
                        tempValue.GetValue(allPoints);

                        tempValue = sdiValue(lsdSFA);
                        lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("SFA"), tempValue);
                        tempValue.GetValue(lsdSFA);
                        lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
                        radSFA = lsdSFA / lsdSGL;

                        tempValue = sdiValue(lsdSFO);
                        lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("SFO"), tempValue);
                        tempValue.GetValue(lsdSFO);
                        lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
                        radSFO = lsdSFO / (lsdSW * lsdST);

                        if (lsdSGL != 0.0 && lsdSW != 0.0 && lsdST != 0.0)
                        {
                            radSFA = lsdSFA / lsdSGL;
                            radSFO = lsdSFO / (lsdSW * lsdST);
                        }

                        tempValue = sdiValue(lsdOFFA);
                        lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("OFFA"), tempValue);
                        tempValue.GetValue(lsdOFFA);

                        tempValue = sdiValue(lsdOFFO);
                        lsdCurveRead.GetValue(p_lsdynaModel, sdiIdentifier("OFFO"), tempValue);
                        tempValue.GetValue(lsdOFFO);

                        sdiString curveName(lsdCurveRead.GetName(p_lsdynaModel) + to_string(lsdCurveRead.GetId(p_lsdynaModel)));
                        p_ConvertUtils.CreateCurve(curveName, nPnts, allPoints, tempCrvEdit, radSFA, radSFO, lsdOFFA, lsdOFFO);
                        if (tempCrvEdit.IsValid())
                        {
                            newFunctIdList.push_back(tempCrvEdit.GetId(p_radiossModel));
                            sdiConvert::Convert::PushToConversionLog(std::make_pair(tempCrvEdit, sourcemats));
                        }
                    }

                    newFunctIdList.push_back(tempCrvEdit.GetId(p_radiossModel));
                    strainRatesList.push_back(strainRatesList[strainRatesList.size() - 1] * 10.0);

                    radmatEntityEdit.SetValue(sdiIdentifier("NL"), sdiValue((int)strainRatesList.size()));
                    radmatEntityEdit.SetValue(sdiIdentifier("EPSI_LI"), sdiValue(strainRatesList));
                    radmatEntityEdit.SetValue(sdiIdentifier("FCT_ID_LI"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), newFunctIdList)));
                }
                else
                {
                    double lsdSFA = 1.0;
                    double lsdSFO = 1.0;
                    double lsdOFFA = 0.0;
                    double lsdOFFO = 0.0;
                    HandleEdit curveEdit;
                    sdiDoubleList allPoints;
                    int nPnts = 0;
                    newFunctIdList.clear();

                    sdiValue tempValue(nPnts);
                    lsdLCTBIDRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
                    tempValue.GetValue(nPnts);

                    allPoints.clear();
                    tempValue = sdiValue(allPoints);
                    lsdLCTBIDRead.GetValue(sdiIdentifier("points"), tempValue);
                    tempValue.GetValue(allPoints);

                    tempValue = sdiValue(lsdSFA);
                    lsdLCTBIDRead.GetValue(sdiIdentifier("SFA"), tempValue);
                    tempValue.GetValue(lsdSFA);
                    lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;

                    tempValue = sdiValue(lsdSFO);
                    lsdLCTBIDRead.GetValue(sdiIdentifier("SFO"), tempValue);
                    tempValue.GetValue(lsdSFO);
                    lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

                    if (lsdSGL != 0.0 && lsdSW != 0.0 && lsdST != 0.0)
                    {
                        radSFA = lsdSFA / lsdSGL;
                        radSFO = lsdSFO / (lsdSW * lsdST);
                    }

                    tempValue = sdiValue(lsdOFFA);
                    lsdLCTBIDRead.GetValue(sdiIdentifier("OFFA"), tempValue);
                    tempValue.GetValue(lsdOFFA);

                    tempValue = sdiValue(lsdOFFO);
                    lsdLCTBIDRead.GetValue(sdiIdentifier("OFFO"), tempValue);
                    tempValue.GetValue(lsdOFFO);

                    sdiString curveName(lsdLCTBIDRead.GetName() + to_string(lsdLCTBIDRead.GetId()));
                    p_ConvertUtils.CreateCurve(curveName, nPnts, allPoints, tempCrvEdit, radSFA, radSFO, lsdOFFA, lsdOFFO);
                    if (tempCrvEdit.IsValid())
                    {
                        newFunctIdList.push_back(tempCrvEdit.GetId(p_radiossModel));
                        sdiConvert::Convert::PushToConversionLog(std::make_pair(tempCrvEdit, sourcemats));
                    }

                    strainRatesList.push_back(1.0);

                    radmatEntityEdit.SetValue(sdiIdentifier("NL"), sdiValue(1));
                    radmatEntityEdit.SetValue(sdiIdentifier("EPSI_LI"), sdiValue(strainRatesList[0]));
                    radmatEntityEdit.SetValue(sdiIdentifier("FCT_ID_LI"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), newFunctIdList)));
                }
            }       
        }
    }
}

void ConvertMat::p_ConvertMatL99(const EntityRead& dynaMat, sdiString& destCard, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();

    double lsdA;
    double lsdC;
    double lsdE;
    double lsdPSFAIL;
    double lsdSIGMAX;
    double lsdSIGSAT;
    double lsdEPSO;
    double sigValue = 0;
    HandleEdit failFldEdit;
    HandleEdit fctIdEdit;

    vector<reference_wrapper<double>> attrValList = { lsdA, lsdC, lsdE, lsdPSFAIL, lsdSIGMAX, lsdSIGSAT, lsdEPSO };
    vector<sdiString> attrNameList = { "A", "C", "E", "PSFAIL", "SIGMAX", "SIGSAT", "EPSO" };
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

    destCard = "/MAT/PLAS_JOHNS";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);
    if (radMat.IsValid())
    {
        if (lsdPSFAIL > 0.0)
        {
            p_radiossModel->CreateEntity(failFldEdit, "/FAIL/FLD", dynaMatName);
            failFldEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));

            double ordinateVal = lsdPSFAIL + (lsdA / lsdE);

            p_ConvertUtils.CreateCurve(dynaMatName, 2, { {-1, ordinateVal, 1, ordinateVal } }, fctIdEdit);

            failFldEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("FCT_ID"), fctIdEdit);
            failFldEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("mat_id"), radMat);
            failFldEdit.SetValue(p_radiossModel, sdiIdentifier("Ifail_sh"), sdiValue(2));
            failFldEdit.SetValue(p_radiossModel, sdiIdentifier("Ixfem"), sdiValue(0));
        }

        sigValue = min(lsdSIGSAT,lsdSIGMAX);


        radMat.SetValue(p_radiossModel, sdiIdentifier("SIG_max0"), sdiValue(sigValue));

        if (lsdC > 0 && lsdEPSO == 0.0)
            lsdEPSO = 1e-20;
        radMat.SetValue(p_radiossModel, sdiIdentifier("EPS_DOT_0"), sdiValue(lsdEPSO));
        radMat.SetValue(p_radiossModel, sdiIdentifier("Fsmooth"), sdiValue(1));

        sdiConvert::SDIHandlReadList sourcehandleList = { {dynaMat.GetHandle()} };
        if (failFldEdit.IsValid())
            sdiConvert::Convert::PushToConversionLog(std::make_pair(failFldEdit, sourcehandleList));
        if (fctIdEdit.IsValid())
            sdiConvert::Convert::PushToConversionLog(std::make_pair(fctIdEdit, sourcehandleList));
    }
}

void ConvertMat::p_ConvertMatL100(const EntityRead& dynaMat,sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    double lsdE;
    double lsdPR;
    double lsdSIGY;
    double lsdEH;
    double lsdEFAIL;

    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    unsigned int createdRadiossMatId = dynaMatId;
    attribMap = { {"E","E"}, {"Rho", "RHO_I"} };

    vector<reference_wrapper<double>> attrValList = { lsdE, lsdPR, lsdSIGY, lsdEH, lsdEFAIL };

    vector<sdiString> attrNameList = { "E", "LSD_MAT100_PR","LSD_MAT100_SIGY","LSD_MAT100_ET","EFAIL"};

    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);
    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };

    destCard = "/MAT/LAW59";
    if (matToPropsReferenceCount > 1)
        createdRadiossMatId = p_ConvertUtils.GetDynaMaxEntityID(srcEntityType);

    p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, createdRadiossMatId);

    if (radMat.IsValid())
    {

        // warning message "OPTION DAMAGE-FAILURE is ignored"
        sdiString keyWordLog = dynaMat.GetKeyword();
        if(keyWordLog.find("DAMAGE-FAILURE") != keyWordLog.npos)
        {
            DynaToRad::ShowMessage(sdiMessageHandler::Level::Warning, 32,
                dynaMat.GetKeyword().c_str(), dynaMat.GetId(), dynaMat.GetName().c_str());
        }
        //
        int nFunct = 0;
        sdiUIntList funcIdList;
        sdiDoubleList strainRatesList;
        sdiDoubleList scaleList;
        EntityEdit radMatEntityEdit(p_radiossModel, radMat);
        if (lsdSIGY == 0.0)
        {
            int curveOPt = 0;
            HandleRead sigCrvHandle;
            dynaMat.GetEntityHandle(sdiIdentifier("SIGY_AsCurve"), sigCrvHandle);
            if (sigCrvHandle.IsValid())
            {
                EntityRead sigCrvEntRead(p_lsdynaModel, sigCrvHandle);
                sdiString crvType = sigCrvEntRead.GetKeyword();
                EntityId sigYcrvId = sigCrvEntRead.GetId();
                HandleRead radCrvHandle;
                if (!crvType.compare(0, 13, "*DEFINE_CURVE"))
                {
                    nFunct = 1;
                    funcIdList.push_back(sigYcrvId);
                    strainRatesList.push_back(0.0);
                    scaleList.push_back(1.0);
                }
                else if (!crvType.compare(0, 13, "*DEFINE_TABLE"))
                {
                    sdiValueEntityList curveList;
                    sdiValue tempValue(curveList);
                    sigCrvEntRead.GetValue(sdiIdentifier("CurveIds"), tempValue);
                    tempValue.GetValue(curveList);
                    curveList.GetIdList(funcIdList);
                    nFunct = (int)funcIdList.size();

                    tempValue = sdiValue(strainRatesList);
                    sigCrvEntRead.GetValue(sdiIdentifier("VALUE"), tempValue);
                    tempValue.GetValue(strainRatesList);

                    scaleList = sdiDoubleList(nFunct, 1.0);
                }
            }
        }
        else if (lsdSIGY > 0.0)
        {
            HandleEdit crvHEdit;
            p_ConvertUtils.CreateCurve(dynaMatName, 2, { {0,lsdSIGY, 1, lsdSIGY + lsdEH } }, crvHEdit);
            if (crvHEdit.IsValid())
            {
                sdiConvert::Convert::PushToConversionLog(std::make_pair(crvHEdit, sourcemat));
            }
            nFunct = 1;
            strainRatesList.push_back(0.0);
            scaleList.push_back(1.0);
            funcIdList.push_back(crvHEdit.GetId(p_radiossModel));
        }
        if (nFunct)
        {
            radMatEntityEdit.SetValue(sdiIdentifier("NB_funct"), sdiValue(nFunct));
            radMatEntityEdit.SetValue(sdiIdentifier("YFun_IDN"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), funcIdList)));
            radMatEntityEdit.SetValue(sdiIdentifier("YFun_IDT"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), funcIdList)));
            radMatEntityEdit.SetValue(sdiIdentifier("SR_reference"), sdiValue(strainRatesList));
            radMatEntityEdit.SetValue(sdiIdentifier("Fscale_yield"), sdiValue(scaleList));
        }

        HandleEdit failHandlEdit;
        p_radiossModel->CreateEntity(failHandlEdit, "/FAIL/CONNECT", dynaMatName);
        failHandlEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
        failHandlEdit.SetValue(p_radiossModel, sdiIdentifier("EPSILON_MAXN"), sdiValue(lsdEFAIL));
        failHandlEdit.SetValue(p_radiossModel, sdiIdentifier("EPSILON_MAXT"), sdiValue(lsdEFAIL));
        failHandlEdit.SetValue(p_radiossModel, sdiIdentifier("mat_id"), sdiValue(sdiValueEntity(destEntityType, createdRadiossMatId)));
        radMatEntityEdit.SetValue(sdiIdentifier("G"), sdiValue(lsdE / (2 * (1 + lsdPR))));

        sdiConvert::Convert::PushToConversionLog(std::make_pair(failHandlEdit, sourcemat));
    }
}

void ConvertMat::p_ConvertMatL111(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    destCard = "/MAT/LAW126";
    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
    sdiString dynaMatName = dynaMat.GetName();
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());

    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    attribMap = { {"Rho","Init.dens."}, {"LSD_G","G"}, {"LSD_A", "A"}, {"LSD_B1", "B"}, {"LSD_C","C"}, {"LSD_MAT_N", "N"}, {"LSD_MAT_FC" , "FC"},
                  {"LSD_MAT52_T", "T"}, {"LSD_MAT_EPSO","EPS0"}, {"LSD_MAT111_EFMIN", "EFMIN"}, {"LSD_MAT110_SFMAX" , "SFMAX"},
                  {"LSD_MAT111_PC", "PC"}, {"LSD_MAT111_UC","MUC"}, {"LSD_MAT111_PL", "PL"}, {"LSD_MAT111_UL" , "MUL"},
                  {"LSDYNA_D1", "D1"}, {"LSDYNA_D2","D2"}, {"LSD_MATT2_K1", "K1"}, {"LSD_MATT2_K2" , "K2"},
                  {"LSD_MATT2_K3", "K3"}, {"LSD_FS","EPS_MAX"} };

    double lsdFS;
    sdiValue tempValue(lsdFS);
    dynaMat.GetValue(sdiIdentifier("LSD_FS"), tempValue);
    tempValue.GetValue(lsdFS);

    if(lsdFS < 0.0)
    {
        radmatEntityEdit.SetValue(sdiIdentifier("IDEL"), sdiValue(3));
    }
    else if(lsdFS > 0.0)
    {
        radmatEntityEdit.SetValue(sdiIdentifier("IDEL"), sdiValue(2));
    }
    else 
    {
        radmatEntityEdit.SetValue(sdiIdentifier("IDEL"), sdiValue(1));
    }
    
}

void ConvertMat::p_ConvertMatL119(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    double rho;
    double kt;
    double kr;
    double utfailr;
    double utfails;
    double utfailt;
    double wtfailr;
    double wtfails;
    double wtfailt;
    double ucfailr; 
    double ucfails; 
    double ucfailt; 
    double wcfailr; 
    double wcfails; 
    double wcfailt;
    int unldopt = -1;
    int iflag = -1;
    sdiValueEntity lcidTR;
    sdiValueEntity lcidTS;
    sdiValueEntity lcidTT;
    sdiValueEntity lcidRR;
    sdiValueEntity lcidRS;
    sdiValueEntity lcidRT;
    sdiValueEntity lcidTUR; 
    sdiValueEntity lcidTUS; 
    sdiValueEntity lcidTUT; 
    sdiValueEntity lcidRUR;
    sdiValueEntity lcidRUS;
    sdiValueEntity lcidRUT;
    sdiValueEntity lcidTDR;
    sdiValueEntity lcidTDS;
    sdiValueEntity lcidTDT;
    sdiValueEntity lcidRDR;
    sdiValueEntity lcidRDS;
    sdiValueEntity lcidRDT;
    destCard = "/MAT/LAW108";

    //---
    // destcard changes if in *SECTION_BEAM, SCOOR = +/-2, then convert it to destCard = "/MAT/LAW113";
    unsigned int partId = p_PartBeingConverted.GetId(p_lsdynaModel);
    HandleEdit parHEdit;
    p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), partId, parHEdit);
    if (parHEdit.IsValid())
    {
        HandleEdit propHread;
        parHEdit.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHread);
        if (propHread.IsValid())
        {
            double lsdSCOOR;
            sdiValue tempValue(lsdSCOOR);
            propHread.GetValue(p_lsdynaModel, sdiIdentifier("SCOOR"), tempValue);
            tempValue.GetValue(lsdSCOOR);                
            if(lsdSCOOR == -2.0 || lsdSCOOR == 2.0) destCard = "/MAT/LAW113";
        }
    }
    //---

    sdiValue value(unldopt);
    vector<reference_wrapper<double>> attrValList = { rho, kt, kr, 
                                                      utfailr, utfails, utfailt, wtfailr, wtfails, wtfailt, 
                                                      ucfailr, ucfails, ucfailt, wcfailr, wcfails, wcfailt };
    vector<sdiString> attrNameList = { "RHO","KT", "KR",
                                      "UTFAILR", "UTFAILS", "UTFAILT", "WTFAILR", "WTFAILS", "WTFAILT",
                                      "UCFAILR", "UCFAILS", "UCFAILT", "WCFAILR", "WCFAILS", "WCFAILT" };
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

    dynaMat.GetValue(sdiIdentifier("UNLDOPT"), value);
    value.GetValue(unldopt);

    value = sdiValue(iflag);
    dynaMat.GetValue(sdiIdentifier("IFLAG"), value);
    value.GetValue(iflag);

    int hValue = -1;
    if (unldopt == 0)
        hValue = 0;
    else if (unldopt == 1)
        hValue = 6;
    else if (unldopt == 2)
        hValue = 7;

    vector<reference_wrapper<sdiValueEntity>> attrCurveValueList = { lcidTR, lcidTS, lcidTT, lcidRR, lcidRS, lcidRT,
                                                             lcidTUR, lcidTUS, lcidTUT, lcidRUR, lcidRUS, lcidRUT, 
                                                             lcidTDR, lcidTDS, lcidTDT, lcidRDR, lcidRDS, lcidRDT };
    vector<sdiString> attrCurveNameList = { "LCIDTR","LCIDTS", "LCIDTT", "LCIDRR", "LCIDRS", "LCIDRT", 
                                           "LCIDTUR", "LCIDTUS", "LCIDTUT", "LCIDRUR", "LCIDRUS", "LCIDRUT",
                                           "LCIDTDR", "LCIDTDS", "LCIDTDT", "LCIDRDR", "LCIDRDS", "LCIDRDT" };
    p_ConvertUtils.GetAttribValues(dynaMat, attrCurveNameList, attrCurveValueList);
    
    vector<vector<EntityId>> lcidList{
        {lcidTR.GetId(), lcidTS.GetId(), lcidTT.GetId(), lcidRR.GetId(), lcidRS.GetId(), lcidRT.GetId()},
        {NULL, NULL, NULL, NULL, NULL, NULL},
        {lcidTUR.GetId(), lcidTUS.GetId(), lcidTUT.GetId(), lcidRUR.GetId(), lcidRUS.GetId(), lcidRUT.GetId()},
        {lcidTDR.GetId(), lcidTDS.GetId(), lcidTDT.GetId(), lcidRDR.GetId(), lcidRDS.GetId(), lcidRDT.GetId()}
    };

    vector< vector<double> > deltaList{
        {utfailr, utfails, utfailt, wtfailr, wtfails, wtfailt},
        {ucfailr, ucfails, ucfailt, wcfailr, wcfails, wcfailt}
    };

    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);

    radmatEntityEdit.SetValue(sdiIdentifier("RHO_I"), sdiValue(rho));
    radmatEntityEdit.SetValue(sdiIdentifier("Mat_Name_OR_LawNo"), sdiValue(2));
    radmatEntityEdit.SetValue(sdiIdentifier("Ifail"), sdiValue(1));
    if (iflag > -1)
        radmatEntityEdit.SetValue(sdiIdentifier("Iequil"), sdiValue(iflag));

    vector<sdiString> radAttribs = { "H1", "H2", "H3", "H4", "H5", "H6" };
    for (size_t i = 0; i < radAttribs.size(); ++i)
    {
        radmatEntityEdit.SetValue(sdiIdentifier("Hscale" + to_string(i + 1)), sdiValue(1.0));
        if (lcidList[0][i] == lcidList[2][i] || lcidList[2][i] == 0)
            radmatEntityEdit.SetValue(sdiIdentifier(radAttribs[i]), sdiValue((int)0));
        else if (hValue >= 0)
            radmatEntityEdit.SetValue(sdiIdentifier(radAttribs[i]), sdiValue(hValue));
    }

    double kValue = kt;
    for (size_t i = 0; i < 3; ++i)
    {
        radmatEntityEdit.SetValue(sdiIdentifier("K" + to_string(i + 1)), sdiValue(kValue));
        radmatEntityEdit.SetValue(sdiIdentifier("A" + to_string(i + 1)), sdiValue((double)1.0));
        radmatEntityEdit.SetValue(sdiIdentifier("DeltaMax" + to_string(i + 1)), sdiValue(deltaList[0][i]));
        radmatEntityEdit.SetValue(sdiIdentifier("DeltaMin" + to_string(i + 1)), sdiValue(deltaList[1][i] * -1.0));
    }
    kValue = kr;
    for (size_t i = 3; i < 6; ++i)
    {
        radmatEntityEdit.SetValue(sdiIdentifier("K" + to_string(i + 1)), sdiValue(kValue));
        radmatEntityEdit.SetValue(sdiIdentifier("A" + to_string(i + 1)), sdiValue((double)1.0));
        radmatEntityEdit.SetValue(sdiIdentifier("ThetaMax" + to_string(i + 1)), sdiValue(deltaList[0][i]));
        radmatEntityEdit.SetValue(sdiIdentifier("ThetaMin" + to_string(i + 1)), sdiValue(deltaList[1][i] * -1.0));
    }

    for (size_t j = 0; j < radAttribs.size(); ++j)
    {
        if ((lcidList[0][j] == lcidList[2][j] && lcidList[2][j] > 0) || lcidList[2][j] == 0)
        {
            // set only loading curve
            int i = 0;
            radmatEntityEdit.SetValue(sdiIdentifier("fct_ID" + to_string(i+1) + to_string(j + 1)), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), lcidList[i][j])));
            continue;
        }
        else
        {
            for (size_t i = 0; i < 4; ++i)
            {
                if (i == 1)
                {
                    continue;
                }
                else if(lcidList[i][j] > 0)
                {
                   radmatEntityEdit.SetValue(sdiIdentifier("fct_ID" + to_string(i + 1) + to_string(j + 1)), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), lcidList[i][j])));
                }
            }
        }
    }

}

void ConvertMat::p_ConvertMatL120(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    destCard = "/MAT/LAW52";
    sdiString dynaMatName = dynaMat.GetName();
    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };

    double lsdATYP = 0.0;
    double lsdN = 0.0;
    double lsdETAN = 0.0;
    double lsdEPS1 = 0.0;
    double lsdEPS2 = 0.0;
    double lsdEPS3 = 0.0;
    double lsdEPS4 = 0.0;
    double lsdEPS5 = 0.0;
    double lsdEPS6 = 0.0;
    double lsdEPS7 = 0.0;
    double lsdEPS8 = 0.0;
    double lsdES1 = 0.0;
    double lsdES2 = 0.0;
    double lsdES3 = 0.0;
    double lsdES4 = 0.0;
    double lsdES5 = 0.0;
    double lsdES6 = 0.0;
    double lsdES7 = 0.0;
    double lsdES8 = 0.0;
    double lsdFF1 = 0.0;
    double lsdFF2 = 0.0;
    double lsdFF3 = 0.0;
    double lsdFF4 = 0.0;

    vector<reference_wrapper<double>> attrValList = { lsdATYP, lsdN, lsdETAN, lsdEPS1,lsdEPS2, lsdEPS3,
                                                      lsdEPS4, lsdEPS5, lsdEPS6, lsdEPS7, lsdEPS8, lsdES1, lsdES2,
                                                      lsdES3, lsdES4, lsdES5, lsdES6, lsdES7, lsdES8 ,
                                                      lsdFF1, lsdFF2, lsdFF3, lsdFF4};
    vector<sdiString> attrNameList = { "ATYP", "N", "ETAN", "EPS1","EPS2", "EPS3",
                                       "EPS4", "EPS5", "EPS6", "EPS7", "EPS8", "ES1", "ES2",
                                       "ES3", "ES4", "ES5", "ES6", "ES7", "ES8" ,
                                       "FF1", "FF2", "FF3", "FF4"};
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());

    EntityEdit radmatEntityEdit(p_radiossModel, radMat);

    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "RHO", "RHO_I");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "E", "E");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "PR", "NU_12");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "SIGY", "A");
    //p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "N", "N");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "Q1", "alpha_1");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "Q2", "alpha_2");
    p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "Q1*Q1", "alpha_3");
    radmatEntityEdit.SetValue(sdiIdentifier("Iflag"), sdiValue(1));
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "FC", "Fc");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "F0", "Fi");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "EN", "EpsN");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "SN", "SN");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "FN", "FN");
    //p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "ETAN", "B");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "FF0", "FF");

    HandleRead lcssHandle;
    dynaMat.GetEntityHandle(sdiIdentifier("LCSS"), lcssHandle);
    int lcssId = 0;
    if (lcssHandle.IsValid()) lcssId = lcssHandle.GetId(p_lsdynaModel);

    if(lsdATYP == 0 && lcssId == 0)
    {
        radmatEntityEdit.SetValue(sdiIdentifier("B"), sdiValue(0.0));
        radmatEntityEdit.SetValue(sdiIdentifier("N"), sdiValue(1.0));
        radmatEntityEdit.SetValue(sdiIdentifier("Tab_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), 0)));
        radmatEntityEdit.SetValue(sdiIdentifier("Iyield"), sdiValue(0));
    }
    else if(lsdATYP == 1 && lcssId == 0)
    {
        // no conversion available
    }
    else if(lsdATYP == 2 && lcssId == 0)
    {
        p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "ETAN*E/(E-ETAN)", "B");
        radmatEntityEdit.SetValue(sdiIdentifier("N"), sdiValue(1.0));
        radmatEntityEdit.SetValue(sdiIdentifier("Tab_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), 0)));
        radmatEntityEdit.SetValue(sdiIdentifier("Iyield"), sdiValue(0));
    }
    else if(lsdATYP == 3 && lcssId == 0)
    {
        HandleEdit table1Edit;

        p_radiossModel->CreateEntity(table1Edit, "/TABLE/1", dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*DEFINE_CURVE")));
        if (table1Edit.IsValid())
        {
            table1Edit.SetValue(p_radiossModel, sdiIdentifier("dimension"), sdiValue(1));
            table1Edit.SetValue(p_radiossModel, sdiIdentifier("curverows"), sdiValue(8));
            table1Edit.SetValue(p_radiossModel, sdiIdentifier("entry_size"), sdiValue(8 * 2));

            table1Edit.SetValue(p_radiossModel, sdiIdentifier("table2darray"), 
                       sdiValue(sdiDoubleList({ lsdEPS1, lsdES1, lsdEPS2, lsdES2, lsdEPS3, lsdES3, lsdEPS4, lsdES4,
                                            lsdEPS5, lsdES5, lsdEPS6, lsdES6, lsdEPS7, lsdES7, lsdEPS8, lsdES8 })));

            radmatEntityEdit.SetEntityHandle(sdiIdentifier("Tab_ID"), table1Edit);
            sdiConvert::Convert::PushToConversionLog(std::make_pair(table1Edit, sourcemat));
        }

        radmatEntityEdit.SetValue(sdiIdentifier("Iyield"), sdiValue(1));
    }
    else
    {
        p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "N", "N");
        p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "ETAN", "B");
    }

    if(lcssId > 0)
    {
        radmatEntityEdit.SetValue(sdiIdentifier("Tab_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), lcssId)));
        radmatEntityEdit.SetValue(sdiIdentifier("Iyield"), sdiValue(1));
    }

    HandleRead lcffHRead;
    dynaMat.GetEntityHandle(sdiIdentifier("LSD_LCID2"), lcffHRead);

    if (lcffHRead.IsValid())
    {
        EntityRead lcffRead(p_lsdynaModel, lcffHRead);
        sdiDoubleList allCrvPnts;
        sdiValue tempValue(allCrvPnts);
        lcffRead.GetValue(sdiIdentifier("points"), tempValue);
        tempValue.GetValue(allCrvPnts);

        double meanCrvPnts = 0.0;
        int nPoints = 0;
        tempValue = sdiValue(nPoints);
        lcffRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
        tempValue.GetValue(nPoints);
        if (nPoints)
        {
            for (int i = 0; i < 2 * nPoints; i += 2)
            {
               meanCrvPnts = meanCrvPnts + allCrvPnts[i+1]/nPoints;
            }
        }
        radmatEntityEdit.SetValue(sdiIdentifier("FF"), sdiValue(meanCrvPnts));
    }
    else
    {
        radmatEntityEdit.SetValue(sdiIdentifier("FF"), sdiValue((lsdFF1+lsdFF2+lsdFF3+lsdFF4)/4.0));
    }

    HandleRead lcf0HRead;
    dynaMat.GetEntityHandle(sdiIdentifier("L1"), lcf0HRead);
    if (lcf0HRead.IsValid())
    {
        EntityRead lcf0Read(p_lsdynaModel, lcf0HRead);
        sdiDoubleList allCrvPnts;
        sdiValue tempValue(allCrvPnts);
        lcf0Read.GetValue(sdiIdentifier("points"), tempValue);
        tempValue.GetValue(allCrvPnts);

        radmatEntityEdit.SetValue(sdiIdentifier("Fi"), sdiValue(allCrvPnts [1]));
    }

    HandleRead lcfcHRead;
    dynaMat.GetEntityHandle(sdiIdentifier("LCFC"), lcfcHRead);
    if (lcfcHRead.IsValid())
    {
        EntityRead lcfcRead(p_lsdynaModel, lcfcHRead);
        sdiDoubleList allCrvPnts;
        sdiValue tempValue(allCrvPnts);
        lcfcRead.GetValue(sdiIdentifier("points"), tempValue);
        tempValue.GetValue(allCrvPnts);

        radmatEntityEdit.SetValue(sdiIdentifier("Fc"), sdiValue(allCrvPnts [1]));
    }

    HandleRead lcfnHRead;
    dynaMat.GetEntityHandle(sdiIdentifier("LCFN"), lcfnHRead);
    if (lcfnHRead.IsValid())
    {
        EntityRead lcfnRead(p_lsdynaModel, lcfnHRead);
        sdiDoubleList allCrvPnts;
        sdiValue tempValue(allCrvPnts);
        lcfnRead.GetValue(sdiIdentifier("points"), tempValue);
        tempValue.GetValue(allCrvPnts);

        radmatEntityEdit.SetValue(sdiIdentifier("FN"), sdiValue(allCrvPnts [1]));
    }
}

void ConvertMat::p_ConvertMatL121(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    attribMap = { {"RHO", "RHO_I"}, {"K","K1"}, {"UTFAIL", "DeltaMax1"} };
    destCard = "/MAT/LAW108";
    //double ucfail = -1;
    double ucfail = 0.0;
    int unldopt = -1;
    int iflag = -1;
    sdiValueEntity lcidT;
    sdiValueEntity lcidTU;
    sdiValueEntity lcidTD;

    sdiValue value(unldopt);
    dynaMat.GetValue(sdiIdentifier("UNLDOPT"), value);
    value.GetValue(unldopt);

    value = sdiValue(ucfail);
    dynaMat.GetValue(sdiIdentifier("UCFAIL"), value);
    value.GetValue(ucfail);

    int hValue = -1;
    if (unldopt == 0)
        hValue = 0;
    else if (unldopt == 1)
        hValue = 6;
    else if (unldopt == 2)
        hValue = 7;
    else if (unldopt == 3)
        hValue = 5;

    vector<reference_wrapper<sdiValueEntity>> attrCurveValueList = { lcidT, lcidTU, lcidTD };
    vector<sdiString> attrCurveNameList = { "LCIDT", "LCIDTU", "LCIDTD" };
    p_ConvertUtils.GetAttribValues(dynaMat, attrCurveNameList, attrCurveValueList);

    vector<EntityId> lcidList{ lcidT.GetId(), 0, lcidTU.GetId(), lcidTD.GetId() };

    if (unldopt > 0 && lcidTU.GetId() == 0) hValue = 0;
    //---
    // destcard changes if in *SECTION_BEAM, SCOOR = +/-2, then convert it to destCard = "/MAT/LAW113";
    double lsdSCOOR;
    unsigned int partId = p_PartBeingConverted.GetId(p_lsdynaModel);
    HandleEdit parHEdit;
    p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), partId, parHEdit);
    if (parHEdit.IsValid())
    {
        HandleEdit propHread;
        parHEdit.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHread);
        if (propHread.IsValid())
        {
            sdiValue tempValue(lsdSCOOR);
            propHread.GetValue(p_lsdynaModel, sdiIdentifier("SCOOR"), tempValue);
            tempValue.GetValue(lsdSCOOR);                
            if(lsdSCOOR == -2.0 || lsdSCOOR == 2.0) destCard = "/MAT/LAW113";
        }
    }
    //---

    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    
    radmatEntityEdit.SetValue(sdiIdentifier("Mat_Name_OR_LawNo"), sdiValue(2));


    if (hValue < 0)
       radmatEntityEdit.SetValue(sdiIdentifier("H1"), sdiValue((int)0));
    else if (hValue >= 0)
       radmatEntityEdit.SetValue(sdiIdentifier("H1"), sdiValue(hValue));
    radmatEntityEdit.SetValue(sdiIdentifier("A1"), sdiValue((double)1.0));
    radmatEntityEdit.SetValue(sdiIdentifier("DeltaMax1"), sdiValue(ucfail));
    radmatEntityEdit.SetValue(sdiIdentifier("DeltaMin1"), sdiValue(ucfail * -1.0));


    for (size_t i = 0; i < 4; ++i)
    {
        if (i == 1) continue;
        if (lcidList[i] > 0)
            radmatEntityEdit.SetValue(sdiIdentifier("fct_ID" + to_string(i + 1) + "1"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), lcidList[i])));
    }
}

void ConvertMat::p_ConvertMatL122(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    attribMap = { {"RHO", "RHO_I"}, {"E", "E"} , {"PR", "NU"}, {"R00", "r00"}, {"R45", "r45"}, {"R90", "r90"}};
    destCard = "/MAT/LAW43"; 

    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);

    if (radMat.IsValid())
    {
        EntityEdit radMatEntityEdit(p_radiossModel, radMat);
        double lsdHR = GetValue<double>(dynaMat, "HR");

        EntityType radFunctEntType = p_radiossModel->GetEntityType("/FUNCT");

        if(lsdHR == 1.0)
        {
            // create new curve with two points
            HandleEdit curveEdit;
            double lsdSFA = 1.0;
            double lsdSFO = 1.0;
            double lsdOFFA = 0.0;
            double lsdOFFO = 0.0;
            double lsdP1 = GetValue<double>(dynaMat, "P1");
            double lsdP2 = GetValue<double>(dynaMat, "P2");
            p_ConvertUtils.CreateCurve("New curve_MatL43_" + to_string(dynaMatId),
                                        2, { { 0.0, lsdP1, 1.0, lsdP1+lsdP2 } }, curveEdit, lsdSFA, lsdSFO, lsdOFFA, lsdOFFO);

            if (curveEdit.IsValid())
            {
                radMatEntityEdit.SetValue(sdiIdentifier("NUM_CURVES"), sdiValue(1));
                radMatEntityEdit.SetValue(sdiIdentifier("func_IDi",0,0), sdiValue(sdiValueEntity(radFunctEntType, curveEdit.GetId(p_radiossModel))));
            }
        }
        else if(lsdHR == 3.0)
        {
            
            HandleRead lcidHRead;
            dynaMat.GetEntityHandle(sdiIdentifier("LCID"), lcidHRead);
            radMatEntityEdit.SetValue(sdiIdentifier("NUM_CURVES"), sdiValue(1));
            radMatEntityEdit.SetValue(sdiIdentifier("func_IDi",0,0), sdiValue(sdiValueEntity(radFunctEntType, lcidHRead.GetId(p_lsdynaModel))));
        }
        sdiConvert::SDIHandlReadList sourceMat = { {dynaMat.GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(radMat, sourceMat));
    }
}

void ConvertMat::p_ConvertMatL123(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat, unsigned short int &matLawNum,unsigned short int &matLawNumChoice)
{
    p_ConvertMatL24(dynaMat, destCard, attribMap, radMat, matLawNum, matLawNumChoice);

    if(matLawNumChoice == 36)
        radMat.SetValue(p_radiossModel, sdiIdentifier("Eps_p_max"), sdiValue(0.0));
    else if(matLawNumChoice == 44)
        radMat.SetValue(p_radiossModel, sdiIdentifier("EPS_MAX"), sdiValue(0.0));

    double lsdFAIL;
    double lsdEPSTHIN;
    double lsdEPSMAJ;
    sdiString matName = dynaMat.GetName();
    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };

    vector<reference_wrapper<double>> attrValList = { lsdFAIL, lsdEPSTHIN, lsdEPSMAJ };

    vector<sdiString> attrNameList = { "FAIL", "EPSTHIN" , "EPSMAJ"};

    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

    if (lsdFAIL > 0.0 || (lsdEPSTHIN > 0.0 && lsdFAIL == 0.0))
    {
        HandleEdit failTab1Edit;
        HandleEdit table1Edit;
        lsdFAIL = (lsdFAIL == 0.0) ? 10.0 : lsdFAIL;

        p_radiossModel->CreateEntity(failTab1Edit, "/FAIL/TAB1", matName);
        failTab1Edit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
        if (failTab1Edit.IsValid())
        {
            EntityEdit failTab1EntEdit(p_radiossModel, failTab1Edit);
            failTab1EntEdit.SetValue(sdiIdentifier("IFAIL_SH"), sdiValue(2));
            failTab1EntEdit.SetEntityHandle(sdiIdentifier("mat_id"), radMat);
            if (lsdEPSTHIN > 0.0)
            {
                failTab1EntEdit.SetValue(sdiIdentifier("P_THICKFAIL"), sdiValue(lsdEPSTHIN));
            }
            sdiConvert::Convert::PushToConversionLog(std::make_pair(failTab1Edit, sourcemat));

            p_radiossModel->CreateEntity(table1Edit, "/TABLE/1", matName, p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*DEFINE_CURVE")));
            if (table1Edit.IsValid())
            {
                table1Edit.SetValue(p_radiossModel, sdiIdentifier("dimension"), sdiValue(1));
                table1Edit.SetValue(p_radiossModel, sdiIdentifier("curverows"), sdiValue(3));
                table1Edit.SetValue(p_radiossModel, sdiIdentifier("entry_size"), sdiValue(3 * 2));
                table1Edit.SetValue(p_radiossModel, sdiIdentifier("table2darray"), sdiValue(sdiDoubleList({ -0.3, lsdFAIL, 0, lsdFAIL, 0.3, lsdFAIL })));

                failTab1EntEdit.SetEntityHandle(sdiIdentifier("TABLE1_ID"), table1Edit);
                sdiConvert::Convert::PushToConversionLog(std::make_pair(table1Edit, sourcemat));
            }
        }
    }
    if (lsdEPSMAJ != 0.0)
    {
        HandleEdit failFldEdit;
        lsdEPSMAJ = abs(lsdEPSMAJ);
        p_radiossModel->CreateEntity(failFldEdit, "/FAIL/FLD", matName);
        failFldEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
        if (failFldEdit.IsValid())
        {
            failFldEdit.SetValue(p_radiossModel, sdiIdentifier("IFAIL_SH"), sdiValue(2));
            HandleEdit fctIdEdit;
            failFldEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("mat_id"), radMat);
            p_ConvertUtils.CreateCurve(matName, 3, { {-0.3,lsdEPSMAJ, 0, lsdEPSMAJ, 0.3, lsdEPSMAJ } }, fctIdEdit);

            sdiConvert::Convert::PushToConversionLog(std::make_pair(failFldEdit, sourcemat));

            if (fctIdEdit.IsValid())
            {
                failFldEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("FCT_ID"), fctIdEdit);
                sdiConvert::Convert::PushToConversionLog(std::make_pair(fctIdEdit, sourcemat));
            }
        }
    }
}

void ConvertMat::p_ConvertMatL138 (const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    destCard = "/MAT/LAW117";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);


    attribMap = { {"EN","MAT_E_ELAS_N"}, {"ET","MAT_E_ELAS_S"}, {"RO", "MAT_RHO"}, {"GIC", "MAT_GIC"}, {"GIIC", "MAT_GIIC"}, {"GAMMA", "MAT_GAMMA"}, {"INTFAIL", "MAT_IDEL"}};
   
    double lsdT ;
    double lsdS ;

    HandleRead lsdTHandle;
    HandleRead lsdSHandle;

    sdiValue value;
    dynaMat.GetValue(sdiIdentifier("T"), value);
    if(value.GetCompoundType() == sdiCompoundType::COMPOUND_TYPE_ENTITY)
    {
        sdiValueEntity lsdTEntity;
        value.GetValue(lsdTEntity);
        EntityId lsdTFunct = lsdTEntity.GetId();
        radmatEntityEdit.SetValue(sdiIdentifier("MAT_Fct_TN"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), lsdTFunct)));
        lsdT = 1.0;
        radmatEntityEdit.SetValue(sdiIdentifier("MAT_TMAX_N"), sdiValue(lsdT));
    }
    else
    {
        value.GetValue(lsdT);
        radmatEntityEdit.SetValue(sdiIdentifier("MAT_TMAX_N"), sdiValue(lsdT));
    }

    dynaMat.GetValue(sdiIdentifier("S"), value);
    if(value.GetCompoundType() == sdiCompoundType::COMPOUND_TYPE_ENTITY)
    {
        sdiValueEntity lsdSEntity;
        value.GetValue(lsdSEntity);
        EntityId lsdSFunct = lsdSEntity.GetId();
        radmatEntityEdit.SetValue(sdiIdentifier("MAT_Fct_TT"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), lsdSFunct)));
        lsdS = 1.0;
        radmatEntityEdit.SetValue(sdiIdentifier("MAT_TMAX_S"), sdiValue(lsdS));
    }
    else
    {
        value.GetValue(lsdS);
        radmatEntityEdit.SetValue(sdiIdentifier("MAT_TMAX_S"), sdiValue(lsdS));
    }


    double lsdXMU ;

    dynaMat.GetValue(sdiIdentifier("XMU"), value);
    value.GetValue(lsdXMU);

    if (lsdXMU > 0)
    {
        radmatEntityEdit.SetValue(sdiIdentifier("MAT_EXP_G"), sdiValue(lsdXMU));
        radmatEntityEdit.SetValue(sdiIdentifier("MAT_EXP_BK"), sdiValue(0));
        radmatEntityEdit.SetValue(sdiIdentifier("MAT_IRUPT"), sdiValue(1));
    }
    else if (lsdXMU < 0)
    {
        radmatEntityEdit.SetValue(sdiIdentifier("MAT_EXP_G"), sdiValue(0));
        radmatEntityEdit.SetValue(sdiIdentifier("MAT_EXP_BK"), sdiValue(abs(lsdXMU)));
        radmatEntityEdit.SetValue(sdiIdentifier("MAT_IRUPT"), sdiValue(2));
    }

    double lsdUND ;
    double lsdUTD ;
    double lsdGIC ;
    double lsdGIIC ;

    dynaMat.GetValue(sdiIdentifier("UND"), value);
    value.GetValue(lsdUND);

    dynaMat.GetValue(sdiIdentifier("UTD"), value);
    value.GetValue(lsdUTD);

    dynaMat.GetValue(sdiIdentifier("GIC"), value);
    value.GetValue(lsdGIC);

    dynaMat.GetValue(sdiIdentifier("GIIC"), value);
    value.GetValue(lsdGIIC);

    if (lsdT == 0 && lsdUND > 0)
        radmatEntityEdit.SetValue(sdiIdentifier("MAT_TMAX_N"), sdiValue(2*lsdGIC/lsdUND));

    if (lsdS == 0 && lsdUTD > 0)
        radmatEntityEdit.SetValue(sdiIdentifier("MAT_TMAX_S"), sdiValue(2*lsdGIIC/lsdUTD));

    double lsdROFLG ;

    dynaMat.GetValue(sdiIdentifier("ROFLG"), value);
    value.GetValue(lsdROFLG);

    if (lsdROFLG == 0)
        radmatEntityEdit.SetValue(sdiIdentifier("MAT_IMASS"), sdiValue(2));

    if (lsdROFLG == 1)
        radmatEntityEdit.SetValue(sdiIdentifier("MAT_IMASS"), sdiValue(1));

    double lsdGAMMMA ;

    dynaMat.GetValue(sdiIdentifier("GAMMA"), value);
    value.GetValue(lsdGAMMMA);

    if (lsdGAMMMA == 0)
        radmatEntityEdit.SetValue(sdiIdentifier("MAT_GAMMA"), sdiValue(2));

}


void ConvertMat::p_ConvertMatL154(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    destCard = "/MAT/LAW115";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());

    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "RHO", "RHO_I");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "E", "E");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "PR", "nu");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "ALPHA", "ALPHA");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "GAMMA", "GAMMA");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "EPSD", "EPSD");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "ALPHA2", "ALPHA2");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "BETA", "BETA");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "SIGP", "SIGP");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "CFAIL", "EPSVP_F");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "PFAIL", "SIGP_F");

    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
    sdiConvert::Convert::PushToConversionLog(std::make_pair(radMat, sourcemat));
}


void ConvertMat::p_ConvertToMatLAW44(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    destCard = "/MAT/LAW44";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    attribMap = { {"E","E"}, {"PR","nu"}, {"RHO", "Init.dens."}, {"C","C"}, {"P", "P"}, {"SIGY", "A"} };
    double lsdE;
    double lsdETAN;
    double lsdFAIL;
    double lsdVp;
    vector<reference_wrapper<double>> attrValList = { lsdE, lsdETAN, lsdFAIL, lsdVp };

    vector<sdiString> attrNameList = { "E","ETAN", "FAIL", "VP"};
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);
    HandleRead lcssHandle;
    dynaMat.GetEntityHandle(sdiIdentifier("LCSS"), lcssHandle);

    lsdETAN = lsdETAN * lsdE / (lsdE - lsdETAN);
    radmatEntityEdit.SetValue(sdiIdentifier("B"), sdiValue(lsdETAN));
    radmatEntityEdit.SetValue(sdiIdentifier("ISMOOTH"), sdiValue(1));
    radmatEntityEdit.SetValue(sdiIdentifier("n"), sdiValue(1.0));
    radmatEntityEdit.SetValue(sdiIdentifier("VP"), sdiValue((int)lsdVp));
    if (lsdFAIL > 0.0)
    {
        p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "FAIL", "EPS_MAX");
    }

    if (lcssHandle.IsValid())
    {
        radmatEntityEdit.SetValue(sdiIdentifier("fct_IDp"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), lcssHandle.GetId(p_lsdynaModel))));
        radmatEntityEdit.SetValue(sdiIdentifier("Optional_card"), sdiValue(1)); 
    }
}

void ConvertMat::p_ConvertMatL169(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    destCard = "/MAT/LAW169";
    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
    sdiString dynaMatName = dynaMat.GetName();
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    attribMap = { {"E","E"}, {"RHO", "Rho_I"}, {"Nu", "PR"}, {"LSD_MAT169_TENMAX","TENMAX"}, {"LSD_MAT169_GCTEN", "GCTEN"}, {"LSD_MAT169_SHRMAX" , "SHRMAX"},
                  {"LSD_MAT169_GCSHR", "GCSHR"}, {"LSD_MAT169_PWRT","PWRT"}, {"LSD_MAT169_PWRS", "PWRS"}, {"LSD_MAT169_SHRP" , "SHRP"}   };
    
}

void ConvertMat::p_ConvertMatL181(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat)
{
    double lsdKM = 0.0;
    double lsdG = 0.0;
    sdiDoubleList lsdGi;
    sdiDoubleList lsdBeta;

    sdiValue tempVal(lsdKM);
    dynaMat.GetValue(sdiIdentifier("KM"), tempVal);
    tempVal.GetValue(lsdKM);

    tempVal = sdiValue(lsdG);
    dynaMat.GetValue(sdiIdentifier("G"), tempVal);
    tempVal.GetValue(lsdG);

    ConvertMatL181ToMatL88(dynaMat, destCard, attribMap, radMat);

    // PRONY
    tempVal = sdiValue(lsdGi);
    dynaMat.GetValue(sdiIdentifier("Gi"), tempVal);
    tempVal.GetValue(lsdGi);

    lsdBeta.clear();
    tempVal = sdiValue(lsdBeta);
    dynaMat.GetValue(sdiIdentifier("BETA"), tempVal);
    tempVal.GetValue(lsdBeta);

    sdiString radMatName = radMat.GetName(p_radiossModel);
    unsigned int radMatId = radMat.GetId(p_radiossModel);

    if (lsdGi.size() > 0)
    {
        HandleEdit ViscPronyHEdit;
        p_radiossModel->CreateEntity(ViscPronyHEdit, "/VISC/PRONY", radMatName, radMatId);
        if (ViscPronyHEdit.IsValid())
        {
            EntityEdit ViscPronyEdit(p_radiossModel, ViscPronyHEdit);

            ViscPronyEdit.SetValue(sdiIdentifier("M"), sdiValue((int)lsdGi.size()));
            ViscPronyEdit.SetValue(sdiIdentifier("K_v"), sdiValue(0.0));
            ViscPronyEdit.SetValue(sdiIdentifier("G_i"), sdiValue(lsdGi));
            ViscPronyEdit.SetValue(sdiIdentifier("Beta_i"), sdiValue(lsdBeta));
            ViscPronyEdit.SetValue(sdiIdentifier("Ki"), sdiValue(sdiDoubleList(lsdGi.size(), 0.0)));
            ViscPronyEdit.SetValue(sdiIdentifier("Beta_ki"), sdiValue(sdiDoubleList(lsdGi.size(), 0.0)));

            sdiConvert::SDIHandlReadList sourceList = { {dynaMat.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(ViscPronyHEdit, sourceList));
        }
    }
}

void ConvertMat::p_ConvertMatL196(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    attribMap = { {"RHO", "RHO_I"}};
    destCard = "/MAT/LAW108";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    radmatEntityEdit.SetValue(sdiIdentifier("Mat_Name_OR_LawNo"), sdiValue(2));

    int nDOF = 0;
    sdiValue tempValue(nDOF);
    dynaMat.GetValue(sdiIdentifier("ArrayCount"), tempValue);
    tempValue.GetValue(nDOF);

    sdiDoubleList lsdKList;
    sdiDoubleList lsdDList;
    sdiDoubleList lsdC2List;
    sdiDoubleList lsdDLEList;
    sdiDoubleList lsdCDFList;
    sdiDoubleList lsdTDFList;
    sdiDoubleList lsdC1List;
  
    vector<reference_wrapper<sdiDoubleList>> attrValList = { lsdKList, lsdDList, lsdC2List, lsdDLEList, lsdCDFList, lsdTDFList, lsdC1List };
    vector<sdiString> attrNameList = { "K", "D", "C2", "DLE", "CDF", "TDF", "MAT196_C1" };
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

    sdiValueEntityList lsdHLCIDEntList;
    sdiUIntList hcidList;
    tempValue = sdiValue(lsdHLCIDEntList);
    dynaMat.GetValue(sdiIdentifier("HLCID"), tempValue);
    tempValue.GetValue(lsdHLCIDEntList);
    lsdHLCIDEntList.GetIdList(hcidList);

    sdiIntList lsdTYPEList;
    tempValue = sdiValue(lsdTYPEList);
    dynaMat.GetValue(sdiIdentifier("TYPE"), tempValue);
    tempValue.GetValue(lsdTYPEList);

    sdiStringList radAttrK({"K1","K2","K3","K4","K5","K6"});
    sdiStringList radAttrA({"A1","A2","A3","A4","A5","A6"});
    sdiStringList radAttrC({"C1","C2","C3","C4","C5","C6"});
    sdiStringList radAttrB({"B1","B2","B3","B4","B5","B6"});
    sdiStringList radAttrD({"Dcoeft1","Dcoeft2","D3","D4","D5","D6"}); //Dcoeft1 and Dcoeft2 are used because D1 and D2 are variables in the cfg 
    sdiStringList radAttrDeltaMin({"DeltaMin1","DeltaMin2","DeltaMin3","ThetaMin4","ThetaMin5","ThetaMin6"});
    sdiStringList radAttrDeltaMax({"DeltaMax1","DeltaMax2","DeltaMax3","ThetaMax4","ThetaMax5","ThetaMax6"});
    sdiStringList radAttrE({"E1","E2","E3","E4","E5","E6"});
    sdiStringList radAttrN1({"fct_ID11","fct_ID12","fct_ID13","fct_ID14","fct_ID15","fct_ID16"});
    sdiStringList radAttrN2({"fct_ID21","fct_ID22","fct_ID23","fct_ID24","fct_ID25","fct_ID26"});
    sdiStringList radAttrN4({"FUN_D1","fct_ID42","fct_ID43","fct_ID44","fct_ID45","fct_ID46"});

    EntityType radFunctEntType = p_radiossModel->GetEntityType("/FUNCT");
    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };

    for (size_t i = 0; i < nDOF; i++)
    {
        radmatEntityEdit.SetValue(sdiIdentifier(radAttrK[i]), sdiValue(lsdKList[i]));
        radmatEntityEdit.SetValue(sdiIdentifier(radAttrC[i]), sdiValue(lsdDList[i]));
        radmatEntityEdit.SetValue(sdiIdentifier(radAttrB[i]), sdiValue(lsdC2List[i]));
        radmatEntityEdit.SetValue(sdiIdentifier(radAttrD[i]), sdiValue(lsdDLEList[i]));
        radmatEntityEdit.SetValue(sdiIdentifier(radAttrDeltaMin[i]), sdiValue(-abs(lsdCDFList[i])));
        radmatEntityEdit.SetValue(sdiIdentifier(radAttrDeltaMax[i]), sdiValue(abs(lsdTDFList[i])));
        radmatEntityEdit.SetValue(sdiIdentifier(radAttrE[i]), sdiValue(lsdC1List[i]));
        radmatEntityEdit.SetValue(sdiIdentifier(radAttrA[i]), sdiValue(1.0));

        if (lsdC1List[i] > 0)
        {
            HandleEdit radN2FuncHEdit;
            p_ConvertUtils.CreateCurve("NewCurve_MatL108_" + to_string(dynaMatId), 2, { { 0.0, 1.0, 1.0, 1.0 } }, radN2FuncHEdit);
            if (radN2FuncHEdit.IsValid())
            {
                radmatEntityEdit.SetEntityHandle(sdiIdentifier(radAttrN2[i]), radN2FuncHEdit);
                sdiConvert::Convert::PushToConversionLog(std::make_pair(radN2FuncHEdit, sourcemat));
            }
        }
        radmatEntityEdit.SetValue(sdiIdentifier(radAttrN4[i]), sdiValue(sdiValueEntity(radFunctEntType, hcidList[i])));
        HandleRead flcidHRead;
        dynaMat.GetEntityHandle(sdiIdentifier("FLCID", 0, 0), flcidHRead);
        if (flcidHRead.IsValid())
        {
            EntityRead flcidRead(p_lsdynaModel, flcidHRead);
            if (lsdTYPEList[i])
            {

                sdiDoubleList allCrvPnts;
                tempValue = sdiValue(allCrvPnts);
                flcidRead.GetValue(sdiIdentifier("points"), tempValue);
                tempValue.GetValue(allCrvPnts);

                int nPnts;
                flcidRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
                tempValue.GetValue(nPnts);

                double lsdSFA = 1.0;
                double lsdSFO = 1.0;
                double lsdOFFA = 0.0;
                double lsdOFFO = 0.0;
                vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
                vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
                p_ConvertUtils.GetAttribValues(flcidRead, lsdQueryAttribs, attribVals);
                lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
                lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

                if (nPnts > 1)
                {
                    ConvertPlasticDispPointsTotalDisp(lsdKList[i], allCrvPnts);
                    HandleEdit functHEdit;
                    p_ConvertUtils.CreateCurve("Recaluclated_" + to_string(flcidRead.GetId()) + "_MatL108_" + to_string(dynaMatId),
                        (int)allCrvPnts.size() / 2, allCrvPnts, functHEdit, lsdSFA, lsdSFO, lsdSFA * lsdOFFA + lsdSFO * lsdOFFO);

                    if (functHEdit.IsValid())
                    {
                        radmatEntityEdit.SetEntityHandle(sdiIdentifier(radAttrN1[i]), functHEdit);
                        sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemat));
                    }
                }
            }
            else
                radmatEntityEdit.SetValue(sdiIdentifier(radAttrN1[i]), sdiValue(sdiValueEntity(radFunctEntType, flcidRead.GetId())));
        }

    }
}

void ConvertMat::p_ConvertMatL240(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat)
{
    double lsdRho;
    double lsdROFLAG;
    double lsdINTFAIL;
    double lsdEMOD;
    double lsdGMOD;
    double lsdG1C0;
    double lsdG1CINF;
    double lsdEDOTG1;
    double lsdT0;
    double lsdT1;
    double lsdEDOTT;
    double lsdG2C0;
    double lsdG2CINF;
    double lsdEDOTG2;
    double lsdS0;
    double lsdS1;
    double lsdEDOTS;
    double lsdTHICK;
    double lsdFG1;
    double lsdFG2;
    int lsdThermal;
    int lsd3Modes;
    sdiDoubleList G1List;
    sdiDoubleList G2List;
    sdiDoubleList pointsList;
    sdiDoubleList SList;
    sdiDoubleList TList;
    HandleEdit fctID1Edit;
    HandleEdit fctIDNEdit;
    HandleEdit fctIDSEdit;
    HandleEdit fctIDONEdit;
    HandleEdit fctIDFNEdit;
    HandleEdit fctIDOSEdit;
    HandleEdit fctIDFSEdit;
    HandleEdit failSConnectEdit;
    sdiConvert::SDIHandlReadList sourcehandleList = { {dynaMat.GetHandle()} };
    sdiDoubleList strainList({ 1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10 });
    
    sdiString dynaMatName = dynaMat.GetName();
    vector<reference_wrapper<int>> attrValList = { lsdThermal, lsd3Modes };
    vector<sdiString> attrNameList = { "Thermal","3Modes" };
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

    if (lsdThermal == 0 && lsd3Modes == 0)
    {
        vector<reference_wrapper<double>> attrValList = { lsdRho, lsdROFLAG, lsdINTFAIL, lsdEMOD, lsdGMOD,  lsdTHICK, lsdG1C0, lsdG1CINF, lsdEDOTG1, lsdT0, lsdT1, lsdEDOTT, lsdFG1, lsdG2C0, lsdG2CINF, lsdEDOTG2, lsdS0, lsdS1, lsdEDOTS, lsdFG2 };
        vector<sdiString> attrNameList = { "Rho", "ROFLG", "INTFAIL", "EMOD", "GMOD", "LSD_MAT240_THICK", "LSD_MAT240_G1C_0", "G1C_INF", "EDOT_G1", "T0", "T1", "EDOT_T", "LSD_MAT240_FG1", "LSD_MAT240_G2C_0", "G2C_INF", "EDOT_G2", "S0", "S1", "EDOT_S", "LSD_MAT240_FG2" };
        p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

        destCard = "/MAT/LAW116";
        if (matToPropsReferenceCount > 1)
            p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
        else
            p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
        EntityEdit radMatEdit(p_radiossModel, radMat);

        radMatEdit.SetValue(sdiIdentifier("MAT_RHO"), sdiValue(lsdRho));
        radMatEdit.SetValue(sdiIdentifier("E"), sdiValue(lsdEMOD));
        radMatEdit.SetValue(sdiIdentifier("G"), sdiValue(lsdGMOD));
        radMatEdit.SetValue(sdiIdentifier("Thick"), sdiValue(lsdTHICK));
        radMatEdit.SetValue(sdiIdentifier("MAT_GC1_ini"), sdiValue(abs(lsdG1C0)));

        if (lsdG1C0 < 0) 
            radMatEdit.SetValue(sdiIdentifier("MAT_GC1_inf"), sdiValue(abs(lsdG1CINF)));
        else
            radMatEdit.SetValue(sdiIdentifier("MAT_GC1_inf"), sdiValue(0.));

        if (lsdG1C0 < 0) 
            radMatEdit.SetValue(sdiIdentifier("MAT_SRATG1"), sdiValue(lsdEDOTG1));
        else
            radMatEdit.SetValue(sdiIdentifier("MAT_SRATG1"), sdiValue(0.));

        radMatEdit.SetValue(sdiIdentifier("MAT_SIGA1"), sdiValue(abs(lsdT0)));
        radMatEdit.SetValue(sdiIdentifier("MAT_SIGB1"), sdiValue(abs(lsdT1)));

        if (abs(lsdT1) > 0) 
            radMatEdit.SetValue(sdiIdentifier("MAT_SRATE1"), sdiValue(lsdEDOTT));
        else
            radMatEdit.SetValue(sdiIdentifier("MAT_SRATE1"), sdiValue(0.));

        radMatEdit.SetValue(sdiIdentifier("MAT_FG1"), sdiValue(abs(lsdFG1)));

        if (lsdFG1 > 0) 
            radMatEdit.SetValue(sdiIdentifier("MAT_FAIL1"), sdiValue(1));
        else if(lsdFG1 < 0) 
            radMatEdit.SetValue(sdiIdentifier("MAT_FAIL1"), sdiValue(2));

        radMatEdit.SetValue(sdiIdentifier("MAT_GC2_ini"), sdiValue(abs(lsdG2C0)));

        if (lsdG2C0 < 0) 
            radMatEdit.SetValue(sdiIdentifier("MAT_GC2_inf"), sdiValue(abs(lsdG2CINF)));
        else
            radMatEdit.SetValue(sdiIdentifier("MAT_GC2_inf"), sdiValue(0));

        if (lsdEDOTG2 < 0) 
            radMatEdit.SetValue(sdiIdentifier("MAT_SRATG2"), sdiValue(lsdEDOTG2));
        else
            radMatEdit.SetValue(sdiIdentifier("MAT_SRATG2"), sdiValue(0));

        radMatEdit.SetValue(sdiIdentifier("MAT_SIGA2"), sdiValue(abs(lsdS0)));
        radMatEdit.SetValue(sdiIdentifier("MAT_SIGB2"), sdiValue(abs(lsdS1)));

        if (abs(lsdS1) > 0) 
            radMatEdit.SetValue(sdiIdentifier("MAT_SRATE2"), sdiValue(lsdEDOTS));
        else
            radMatEdit.SetValue(sdiIdentifier("MAT_SRATE2"), sdiValue(0));

        radMatEdit.SetValue(sdiIdentifier("MAT_FG2"), sdiValue(abs(lsdFG2)));

        if (lsdFG2 > 0) 
            radMatEdit.SetValue(sdiIdentifier("MAT_FAIL2"), sdiValue(1));
        else if (lsdFG2 < 0) 
            radMatEdit.SetValue(sdiIdentifier("MAT_FAIL2"), sdiValue(2));

        if (lsdT0 < 0 && lsdT1 > 0) 
            radMatEdit.SetValue(sdiIdentifier("MAT_ORDER1"), sdiValue(2));

        if (lsdT0 < 0 && lsdT1 < 0) 
            radMatEdit.SetValue(sdiIdentifier("MAT_ORDER1"), sdiValue(1));

        if (lsdS0 < 0 && lsdS1 > 0) 
            radMatEdit.SetValue(sdiIdentifier("MAT_ORDER2"), sdiValue(2));

        if (lsdS0 < 0 && lsdS1 < 0) 
            radMatEdit.SetValue(sdiIdentifier("MAT_ORDER2"), sdiValue(1));


        if (lsdROFLAG == 1) 
            radMatEdit.SetValue(sdiIdentifier("Imass"), sdiValue(1));

        if (lsdROFLAG == 0) 
            radMatEdit.SetValue(sdiIdentifier("Imass"), sdiValue(2));

        if (lsdINTFAIL > 0) 
            radMatEdit.SetValue(sdiIdentifier("Idel"), sdiValue(1));
    }
}

void ConvertMat::p_ConvertMatL252(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    destCard = "/MAT/LAW120";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());

    int lsdFLG;
    int lsdJCFL;
    int lsdDOPT;
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    vector<reference_wrapper<int>> attrValList = { lsdFLG, lsdJCFL, lsdDOPT};
    vector<sdiString> attrNameList = { "FLG", "JCFL", "DOPT"};
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);


    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "RHO", "MAT_RHO");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "E", "E");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "PR", "nu");
    if (lsdFLG == 0) 
        radmatEntityEdit.SetValue(sdiIdentifier("Iform"), sdiValue(1));
    if (lsdFLG == 2) 
        radmatEntityEdit.SetValue(sdiIdentifier("Iform"), sdiValue(2));
    if (lsdJCFL == 0) 
        radmatEntityEdit.SetValue(sdiIdentifier("Itrx"), sdiValue(2));
    if (lsdJCFL == 2) 
        radmatEntityEdit.SetValue(sdiIdentifier("Itrx"), sdiValue(1));
    if (lsdDOPT == 0) 
        radmatEntityEdit.SetValue(sdiIdentifier("Idam"), sdiValue(2));
    if (lsdDOPT == 2) 
        radmatEntityEdit.SetValue(sdiIdentifier("Idam"), sdiValue(1));
        
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "LCSS", "Table_Id");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "TAUO", "T0");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "Q", "Q");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "B", "Beta");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "H", "H");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "C", "C");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "GAM0", "EPSD0");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "GAMM", "EPSDF");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "A10", "AF1");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "A20", "AF2");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "A1H", "AH1");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "A2H", "AH2");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "A2S", "AS");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "POW", "EXP_N");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "D1", "D1F");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "D2", "D2F");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "D3", "Dtrx");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "D4", "Djc");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "D1C", "D1C");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "D2C", "D2C");

    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
    sdiConvert::Convert::PushToConversionLog(std::make_pair(radMat, sourcemat));
}

void ConvertMat::p_ConvertMatAddErosion()
{

    SelectionRead selMatAddErosion(p_lsdynaModel, "*MAT_ADD_EROSION");
    while (selMatAddErosion.Next())
    {

        HandleRead lcsdgHandle;
        HandleRead matHandle;
        double lsdEXCL;
        double lsdMNEPS;
        double lsdEFFEPS;
        double lsdNUMFIP = 1; // by default
        double lsdDMGTYP;
        double lsdDCRIT;
        double lsdECRIT;
        double lsdFADEXP;
        double lsdREFSZ;
//
        double lsdSIGVM;
        double lsdFAILTM;
        double lsdNCS;
        double n_coef = 0.0;
        double radPthickfail = 0.0;
//
        int lsdIDAM = 0;
        int lsdECRITOpt = 0;
        int lsdMXEPSOpt = 0;
        int lsdFADEXPOpt = 0;
        sdiValueEntity lsdMIDEntity;
        sdiValue tempValue;

        tempValue = sdiValue(lsdMIDEntity);
        selMatAddErosion->GetValue(sdiIdentifier("MID"), tempValue);
        tempValue.GetValue(lsdMIDEntity);

        selMatAddErosion->GetEntityHandle(sdiIdentifier("MID"), matHandle);
        sdiConvert::SDIHandlReadList radMatConvertedHandles;

        sdiConvert::Convert::GetConvertedHandles(matHandle, radMatConvertedHandles, destEntityType);

        sdiString matAddErosionName = selMatAddErosion->GetName();
        EntityType dynaCurveType = p_lsdynaModel->GetEntityType("*DEFINE_CURVE");
        sdiConvert::SDIHandlReadList sourcehandleList = { {selMatAddErosion->GetHandle()} };

        vector<sdiString> attribNames({ { "EXCL", "MNEPS", "EFFEPS", "NUMFIP", "DMGTYP", "DCRIT", "LSD_ECRIT", "LSD_FADEXP", 
                                         "REFSZ", "LSD_SIGVM", "FAILTM", "NCS"} });
        vector<reference_wrapper<double>> attrVals({ { lsdEXCL, lsdMNEPS, lsdEFFEPS, lsdNUMFIP, lsdDMGTYP, lsdDCRIT, lsdECRIT, lsdFADEXP, lsdREFSZ,
                                                       lsdSIGVM, lsdFAILTM, lsdNCS} });

        p_ConvertUtils.GetAttribValues(*selMatAddErosion, attribNames, attrVals);

        //-----------------------
        // NIP weight factor map
        sdiDoubleList WeightsList({ 0.5, 0.25, 0.1667, 0.125, 0.1, 0.0833 });
        sdiIntList NipList({ 2, 3, 4, 5, 6, 7 });
        map<int, double> NipWeights;
        for (int i = 0; i < NipList.size(); ++i) // loop over NIp list
        {
          NipWeights.insert(pair<int, double>(NipList[i],WeightsList[i]));
        }
        //-----------------------
 
        for (int i=0; i < radMatConvertedHandles.size(); i=i+1)
        {

            selMatAddErosion->GetValue(sdiIdentifier("IDAM"), tempValue);
            tempValue.GetValue(lsdIDAM);
            selMatAddErosion->GetValue(sdiIdentifier("NCS"), tempValue);
            tempValue.GetValue(lsdNCS);

            if (lsdIDAM == 0)
            {
                //---------------------
                // -- /FAIL/GENE1/ -- //
                //---------------------
                HandleEdit failGene1HEdit;
                if (!failGene1HEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(failGene1HEdit, "/FAIL/GENE1", matAddErosionName);
                    failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));

                    EntityEdit failGene1Edit(p_radiossModel, failGene1HEdit);
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "MXPRES", "Pmax");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "MNEPS", "Eps_min");

                    if(lsdEFFEPS >= 0.0) p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "EFFEPS", "Eps_eff");
                    else failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Eps_eff"),sdiValue(abs(lsdEFFEPS)));

                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "VOLEPS", "Eps_vol");

                    unsigned int partId = p_PartBeingConverted.GetId(p_radiossModel);
                    // get elem part type (shell/solid)

                    int nbShell =0;
                    int nbSolid =0;

                    SelectionElementRead elemShellSelect(p_lsdynaModel, "*ELEMENT_SHELL");
                    nbShell = elemShellSelect.Count();

                    SelectionElementRead elemSolidSelect(p_lsdynaModel, "*ELEMENT_SOLID");
                    nbSolid = elemSolidSelect.Count();

                    // get NIP (nb of integration points through thickness) from *SECTION_SHELL
                    int Ishellsolid = 0;
                    HandleRead partHRead;
                    p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), partId, partHRead);

                    if (partHRead.IsValid())
                    {
                        HandleRead propHRead;
                        partHRead.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHRead);

                        EntityRead propEntityRead(p_lsdynaModel, propHRead);
                        int lsdelform;
                        tempValue = sdiValue(lsdelform);
                        propEntityRead.GetValue(sdiIdentifier("ELFORM"), tempValue);
                        tempValue.GetValue(lsdelform);

                        int lsdNIP = 0;
                        tempValue = sdiValue(lsdNIP);
                        propEntityRead.GetValue(sdiIdentifier("LSD_NIP"), tempValue);
                        tempValue.GetValue(lsdNIP);

                        if(nbShell > 0)
                        {
                            //Full-Integrated linear shell element: (elfrom = 9, 16, -16,17, 18, 20, 21, 22, 26, 30)
                            if(lsdelform == 9  || lsdelform == 16 || lsdelform == -16 || 
                               lsdelform == 17 || lsdelform == 18 || lsdelform ==  20 || 
                               lsdelform == 21 || lsdelform == 22  || lsdelform == 26 || 
                               lsdelform == 30)
                            {
                                if(lsdNUMFIP > 0)
                                {
                                    double lsdGauss = 4.0; // inplane shell integration points
                                    double radVOLFRAC;
                                    radVOLFRAC = abs(lsdNUMFIP)/(lsdGauss*lsdNIP);
                                    failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Volfrac"),sdiValue(radVOLFRAC));
                                }
                                else if(lsdNUMFIP < 0 && lsdNUMFIP >= -100)
                                {
                                    radPthickfail = abs(lsdNUMFIP)/100.0;
                                    failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Pthickfail"),sdiValue(radPthickfail));
                                }
                                else if(lsdNUMFIP < -100)
                                {
                                    radPthickfail = (abs(lsdNUMFIP)-100.0)/100.0;
                                    failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Pthickfail"),sdiValue(radPthickfail));
                                } // if(lsdNUMFIP > 0)
                            }
                            else if(lsdelform == 23  || lsdelform == 24)
                            {
                                // higher order shell element:
                                p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "VOLFRAC", "Volfrac");
                            }
                            else
                            {
                                // Reduced-Integrated linear shell element
                                if(lsdNUMFIP > 0)
                                {
                                    n_coef = lsdNUMFIP;
                                }
                                else if(lsdNUMFIP < 0 && lsdNUMFIP >= -100)
                                {
                                    n_coef = ceil((abs(lsdNUMFIP)/100.0)*lsdNIP);
                                }
                                else if(lsdNUMFIP < -100)
                                {
                                    n_coef = ceil((abs(lsdNUMFIP-100.0)/100.0)*lsdNIP);
                                } // if(lsdNUMFIP > 0)
 
                                if (lsdNIP > 1) radPthickfail = (2.0*n_coef-1)*NipWeights[lsdNIP];
                                failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Pthickfail"),sdiValue(radPthickfail));
                            }
                        } // if(nbShell > 0)

                        if(nbSolid > 0)
                        {
                            if(lsdelform == 2 || lsdelform == 3)
                            {
                                //Full-Integrated linear solid element: (elfrom = 2, 3)
                                double radVOLFRAC;
                                lsdNIP = 8;
                                radVOLFRAC = abs(lsdNUMFIP)/lsdNIP;
                                failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Volfrac"),sdiValue(radVOLFRAC));
                            }
                            else if(lsdelform == 24 || lsdelform == 25 || lsdelform == 26 || 
                                    lsdelform == 27 || lsdelform == 28 || lsdelform == 29)
                            {
                                // higher order solid element:
                                p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "VOLFRAC", "Volfrac");
                            }
                            else
                            {
                                // Reduced-Integrated linear solid element: (elform =1, 4, 0, 9, 10, 12, 13)
                                failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Volfrac"),sdiValue(0.5));
                            }
                        } // if(nbSolid > 0)
                    } // if (partHRead.IsValid())

                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "NCS", "NCS");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "MNPRES", "Pmin");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "SIGP1", "SigP1_max");

                    bool optSIGVMcurve;
                    tempValue = sdiValue(optSIGVMcurve);
                    selMatAddErosion->GetValue(sdiIdentifier("OPT_SIGVM_curve"), tempValue);
                    tempValue.GetValue(optSIGVMcurve);

                    if (optSIGVMcurve  == false)
                    {
                        p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "SIGVM", "Sig_max");
                    }
                    else
                    {
                        // LSD_SIGVM_CURVE --> function fct_Idps
                        HandleRead crvSIGVMHandle;
                        selMatAddErosion->GetEntityHandle(sdiIdentifier("SIGVM_CURVE"), crvSIGVMHandle);

                        if (crvSIGVMHandle.IsValid())
                        {
                            EntityRead crvSIGVMEntRead(p_lsdynaModel, crvSIGVMHandle);
                            EntityId crvSIGVMId = crvSIGVMEntRead.GetId();
                            //EntityId crvSIGVMId = crvSIGVMHandle.GetId(p_lsdynaModel);
                            failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDsm"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvSIGVMId)));
                        }
                    } // if (optSIGVMcurve  == false)

                    bool optMXEPScurve;
                    tempValue = sdiValue(optMXEPScurve);
                    selMatAddErosion->GetValue(sdiIdentifier("OPT_MXEPS_curve"), tempValue);
                    tempValue.GetValue(optMXEPScurve);

                    if (optMXEPScurve  == false)
                    {
                        p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "MXEPS", "Eps_max");
                    }
                    else
                    {
                        // LSD_MXEPS_CURVE --> function fct_Idps
                        HandleRead crvMXEPSHandle;
                        selMatAddErosion->GetEntityHandle(sdiIdentifier("MXEPS_CURVE"), crvMXEPSHandle);

                        if (crvMXEPSHandle.IsValid())
                        {
                            EntityRead crvMXEPSEntRead(p_lsdynaModel, crvMXEPSHandle);
                            EntityId crvMXEPSId = crvMXEPSEntRead.GetId();
                            //EntityId crvMXEPSId = crvMXEPSHandle.GetId(p_lsdynaModel);
                            failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDps"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvMXEPSId)));
                        }
                    } // if (optMXEPScurve  == false)

                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "EPSSH", "Eps_s");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "SIGTH", "Sigr");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "IMPULSE", "K");
                    if(lsdFAILTM >0) p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "FAILTM", "Time_max");

                    HandleRead crvLCREGDHandle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCREGD"), crvLCREGDHandle);
                    if (crvLCREGDHandle.IsValid())
                    {
                        EntityId crvLCREGId = crvLCREGDHandle.GetId(p_lsdynaModel);
                        failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDel"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCREGId)));
                    }

                    
                    HandleRead crvLCFLDHandle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCFLD"), crvLCFLDHandle);

                    if (crvLCFLDHandle.IsValid())
                    {
                        EntityRead crvLCFLDEntRead(p_lsdynaModel, crvLCFLDHandle);
                        sdiString crvLCFLDType = crvLCFLDEntRead.GetKeyword();
                        EntityId crvLCFLDId = crvLCFLDEntRead.GetId();
                        EntityEdit failGene1Edit(p_radiossModel, failGene1HEdit);

                        if(!crvLCFLDType.compare(0, 13, "*DEFINE_TABLE"))
                        {
                            failGene1Edit.SetValue(sdiIdentifier("tab_IDfld"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), crvLCFLDId)));
                            failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Itab"),sdiValue(1));
                            failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Istrain"),sdiValue(1));
                            failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Ismooth"),sdiValue(1));
                        }
                    } // if (crvLCFLDHandle.IsValid())

                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "NSFF", "Nstep");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "EPSTHIN", "Thinning");

                    HandleRead crvLCEPS12Handle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCEPS12"), crvLCEPS12Handle);
                    if (crvLCEPS12Handle.IsValid())
                    {
                        EntityId crvLCEPS12Id = crvLCEPS12Handle.GetId(p_lsdynaModel);
                        failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDg12"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCEPS12Id)));
                    }

                    HandleRead crvLCEPS13Handle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCEPS13"), crvLCEPS13Handle);
                    if (crvLCEPS13Handle.IsValid())
                    {
                        EntityId crvLCEPS13Id = crvLCEPS13Handle.GetId(p_lsdynaModel);
                        failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDg13"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCEPS13Id)));
                    }

                    HandleRead crvLCEPSMXHandle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCEPSMX"), crvLCEPSMXHandle);
                    if (crvLCEPSMXHandle.IsValid())
                    {
                        EntityId crvLCEPSMXId = crvLCEPSMXHandle.GetId(p_lsdynaModel);
                        failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDe1c"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCEPSMXId)));
                    }

                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "MXTMP", "Temp_max");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "DTMIN", "dtmin");

                }  // if (!failGene1HEdit.IsValid())
//---------------------
//---------------------
                if (failGene1HEdit.IsValid())
                {
                    failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("mat_id"), sdiValue(sdiValueEntity(destEntityType, radMatConvertedHandles[i].GetId(p_radiossModel))));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(failGene1HEdit, sourcehandleList));
                }
//---------------------
//---------------------
            }
            else if (lsdIDAM == 1 && (lsdNCS == 0.0 || lsdNCS == 1.0))
            {
                //---------------------
                // -- /FAIL/TAB2/ -- //
                //---------------------
                HandleEdit faiTab2HEdit;
                if (!faiTab2HEdit.IsValid())
                {
//---------------------
                    p_radiossModel->CreateEntity(faiTab2HEdit, "/FAIL/TAB2", matAddErosionName);
                    faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));

                    EntityEdit faiTab2Edit(p_radiossModel, faiTab2HEdit);

                    unsigned int partId = p_PartBeingConverted.GetId(p_radiossModel);
                    // get elem part type (shell/solid)

                    int nbShell =0;
                    int nbSolid =0;

                    SelectionElementRead elemShellSelect(p_lsdynaModel, "*ELEMENT_SHELL");
                    nbShell = elemShellSelect.Count();

                    SelectionElementRead elemSolidSelect(p_lsdynaModel, "*ELEMENT_SOLID");
                    nbSolid = elemSolidSelect.Count();

                    // get NIP (nb of integration points through thickness) from *SECTION_SHELL

                    HandleRead partHRead;
                    p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), partId, partHRead);

                    if (partHRead.IsValid())
                    {
                        HandleRead propHRead;
                        partHRead.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHRead);
                        EntityRead propEntityRead(p_lsdynaModel, propHRead);

                        int lsdelform;
                        tempValue = sdiValue(lsdelform);
                        propEntityRead.GetValue(sdiIdentifier("ELFORM"), tempValue);
                        tempValue.GetValue(lsdelform);

                        int lsdNIP = 0;
                        tempValue = sdiValue(lsdNIP);
                        propEntityRead.GetValue(sdiIdentifier("LSD_NIP"), tempValue);
                        tempValue.GetValue(lsdNIP);

                        if(nbShell > 0)
                        {
                            if(lsdNUMFIP == 0.0) lsdNUMFIP = 1.0 ; // by default
                           //Full-Integrated linear shell element: (elfrom = 9, 16, -16,17, 18, 20, 21, 22, 26, 30)
                            if(lsdelform == 9  || lsdelform == 16 || lsdelform == -16 || 
                               lsdelform == 17 || lsdelform == 18 || lsdelform ==  20 || 
                               lsdelform == 21 || lsdelform == 22  || lsdelform == 26 || 
                               lsdelform == 30)
                            {
                                if(lsdNUMFIP > 0)
                                {
                                    double lsdGauss = 4.0; // inplane shell integration points
                                    double radVOLFRAC;
                                    radVOLFRAC = abs(lsdNUMFIP)/(lsdGauss*lsdNIP);
                                    faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("Volfrac"),sdiValue(radVOLFRAC));
                                }
                                else if(lsdNUMFIP < 0 && lsdNUMFIP >= -100)
                                {
                                    radPthickfail = abs(lsdNUMFIP)/100.0;
                                    faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                                }
                                else if(lsdNUMFIP < -100)
                                {
                                    radPthickfail = (abs(lsdNUMFIP)-100.0)/100.0;
                                    faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                                } // if(lsdNUMFIP > 0)
                            }
                            else if(lsdelform == 23  || lsdelform == 24)
                            {
                                // higher order shell element:
                                p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "VOLFRAC", "Volfrac");
                            }
                            else
                            {
                                // Reduced-Integrated linear shell element
                                if(lsdNUMFIP > 0)
                                {
                                    n_coef = lsdNUMFIP;
                                }
                                else if(lsdNUMFIP < 0 && lsdNUMFIP >= -100)
                                {
                                    n_coef = ceil((abs(lsdNUMFIP)/100.0)*lsdNIP);
                                }
                                else if(lsdNUMFIP < -100)
                                {
                                    n_coef = ceil((abs(lsdNUMFIP-100.0)/100.0)*lsdNIP);
                                } // if(lsdNUMFIP > 0)
 
                                if (lsdNIP > 1) radPthickfail = (2.0*n_coef-1)*NipWeights[lsdNIP];
                                faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                            }
                        }

                        if(nbSolid > 0)
                        {
                            if(lsdNUMFIP > 0) p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "NUMFIP", "FAILIP");
                        }

                        
                        HandleRead crvLCSDGHandle;
                        selMatAddErosion->GetEntityHandle(sdiIdentifier("LCSDG"), crvLCSDGHandle);
                        if (crvLCSDGHandle.IsValid())
                        {
                            EntityId crvLCSDGId = crvLCSDGHandle.GetId(p_lsdynaModel);
                            faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("EPSF_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCSDGId)));
                        }


                    bool optECRITcurve;
                    tempValue = sdiValue(optECRITcurve);
                    selMatAddErosion->GetValue(sdiIdentifier("OPT_ECRIT_curve"), tempValue);
                    tempValue.GetValue(optECRITcurve);

                    if (optECRITcurve  == false)
                    {
                        p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "ECRIT", "ECRIT");
                    }
                    else
                    {
                        // LSD_ECRIT_CURVE --> function INST_ID
                        HandleRead crvECRITHandle;
                        selMatAddErosion->GetEntityHandle(sdiIdentifier("ECRIT_CURVE"), crvECRITHandle);

                        if (crvECRITHandle.IsValid())
                        {
                            EntityRead crvECRITEntRead(p_lsdynaModel, crvECRITHandle);
                            EntityId crvECRITId = crvECRITEntRead.GetId();
                            //EntityId crvECRITId = crvECRITHandle.GetId(p_lsdynaModel);
                            faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("INST_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvECRITId)));
                        }
                    } // if (optECRITcurve  == false)

                    p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "DMGEXP", "N");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "DCRIT", "DCRIT");

                    bool optFADEXPcurve;
                    tempValue = sdiValue(optFADEXPcurve);
                    selMatAddErosion->GetValue(sdiIdentifier("OPT_FADEXP_curve"), tempValue);
                    tempValue.GetValue(optFADEXPcurve);

                    if (optFADEXPcurve  == false)
                    {
                        p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "FADEXP", "EXP");
                    }
                    else
                    {
                        // LSD_FADEXP_CURVE --> function FCT_EXP
                        HandleRead crvFADEXPHandle;
                        selMatAddErosion->GetEntityHandle(sdiIdentifier("FADEXP_CURVE"), crvFADEXPHandle);

                        if (crvFADEXPHandle.IsValid())
                        {
                            EntityRead crvFADEXPEntRead(p_lsdynaModel, crvFADEXPHandle);
                            EntityId crvFADEXPId = crvFADEXPEntRead.GetId();
                            //EntityId crvFADEXPId = crvFADEXPHandle.GetId(p_lsdynaModel);
                            faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("FCT_EXP"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvFADEXPId)));
                        }
                    } // if (optFADEXPcurve  == false)

                    HandleRead crvLCREGDHandle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCREGD"), crvLCREGDHandle);
                    if (crvLCREGDHandle.IsValid())
                    {
                        EntityId crvLCREGId = crvLCREGDHandle.GetId(p_lsdynaModel);
                        faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("TAB_EL"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCREGId)));
                    }

                    p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "REFSZ", "EL_REF");

                    HandleRead crvLCSRSHandle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCSRS"), crvLCSRSHandle);
                    if (crvLCSRSHandle.IsValid())
                    {
                        EntityId crvLCSRSId = crvLCSRSHandle.GetId(p_lsdynaModel);
                        faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("FCT_SR"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCSRSId)));
                    }

                    p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "SHRF", "SHRF");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "BIAXF", "BIAXF");

                    } // if (partHRead.IsValid())
//---------------------
                } // if (!faiTab2HEdit.IsValid())
//---------------------
//---------------------
                if (faiTab2HEdit.IsValid())
                {
                    faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("mat_id"), sdiValue(sdiValueEntity(destEntityType, radMatConvertedHandles[i].GetId(p_radiossModel))));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(faiTab2HEdit, sourcehandleList));
                }
//---------------------
//---------------------
            }
            else if (lsdIDAM < 0 && (lsdNCS == 0.0 || lsdNCS == 1.0))
            {
                //---------------------
                // -- /FAIL/INIEVO/ -- //
                //---------------------
                HandleEdit failInievoHEdit;
                if (!failInievoHEdit.IsValid())
                {
//---------------------
                    sdiValue tempValue;
                    HandleRead matHandle;
                    sdiValueEntity lsdMIDEntity;

                    tempValue = sdiValue(lsdMIDEntity);
                    selMatAddErosion->GetValue(sdiIdentifier("MID"), tempValue);
                    tempValue.GetValue(lsdMIDEntity);

                    p_radiossModel->CreateEntity(failInievoHEdit, "/FAIL/INIEVO", matAddErosionName,lsdMIDEntity.GetId());
                    failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));

                    EntityEdit failInievoEdit(p_radiossModel, failInievoHEdit);

                    selMatAddErosion->GetEntityHandle(sdiIdentifier("MID"), matHandle);
                    sdiConvert::SDIHandlReadList radMatConvertedHandles;

                    sdiConvert::Convert::GetConvertedHandles(matHandle, radMatConvertedHandles, destEntityType);
                    sdiConvert::SDIHandlReadList sourcehandleList = { {selMatAddErosion->GetHandle()} };

                    failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("NINIEVO"),sdiValue(abs(lsdIDAM)));

                    unsigned int partId = p_PartBeingConverted.GetId(p_lsdynaModel);
                    // get elem part type (shell/solid)

                    int nbShell =0;
                    int nbSolid =0;

                    SelectionElementRead elemShellSelect(p_lsdynaModel, "*ELEMENT_SHELL");
                    nbShell = elemShellSelect.Count();
                    SelectionElementRead elemSolidSelect(p_lsdynaModel, "*ELEMENT_SOLID");
                    nbSolid = elemSolidSelect.Count();

                    // get NIP (nb of integration points through thickness) from *SECTION_SHELL
                    HandleRead partHRead;
                    p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), partId, partHRead);

                    double lsdNUMFIP;
                    tempValue = sdiValue(lsdNUMFIP);
                    selMatAddErosion->GetValue(sdiIdentifier("NUMFIP"), tempValue);
                    tempValue.GetValue(lsdNUMFIP);
                    
                    if (partHRead.IsValid())
                    {
                        HandleRead propHRead;
                        partHRead.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHRead);
                        EntityRead propEntityRead(p_lsdynaModel, propHRead);

                        int lsdelform;
                        tempValue = sdiValue(lsdelform);
                        propEntityRead.GetValue(sdiIdentifier("ELFORM"), tempValue);
                        tempValue.GetValue(lsdelform);

                        int lsdNIP = 0;
                        tempValue = sdiValue(lsdNIP);
                        propEntityRead.GetValue(sdiIdentifier("LSD_NIP"), tempValue);
                        tempValue.GetValue(lsdNIP);

                        if(nbShell > 0)
                        {
                            if(lsdNUMFIP == 0.0) lsdNUMFIP = 1.0 ; // by default
                           //Full-Integrated linear shell element: (elfrom = 9, 16, -16,17, 18, 20, 21, 22, 26, 30)
                            if(lsdelform == 9  || lsdelform == 16 || lsdelform == -16 || 
                               lsdelform == 17 || lsdelform == 18 || lsdelform ==  20 || 
                               lsdelform == 21 || lsdelform == 22  || lsdelform == 26 || 
                               lsdelform == 30)
                            {
                                if(lsdNUMFIP > 0)
                                {
                                    double lsdGauss = 4.0; // inplane shell integration points
                                    double radVOLFRAC;
                                    radVOLFRAC = abs(lsdNUMFIP)/(lsdGauss*lsdNIP);
                                    failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("Volfrac"),sdiValue(radVOLFRAC));
                                }
                                else if(lsdNUMFIP < 0 && lsdNUMFIP >= -100)
                                {
                                    radPthickfail = abs(lsdNUMFIP)/100.0;
                                    failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                                }
                                else if(lsdNUMFIP < -100)
                                {
                                    radPthickfail = (abs(lsdNUMFIP)-100.0)/100.0;
                                    failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                                } // if(lsdNUMFIP > 0)
                            }
                            else if(lsdelform == 23  || lsdelform == 24)
                            {
                                // higher order shell element:
                                p_ConvertUtils.CopyValue(*selMatAddErosion, failInievoEdit, "VOLFRAC", "Volfrac");
                            }
                            else
                            {
                                // Reduced-Integrated linear shell element
                                if(lsdNUMFIP > 0)
                                {
                                    n_coef = lsdNUMFIP;
                                }
                                else if(lsdNUMFIP < 0 && lsdNUMFIP >= -100)
                                {
                                    n_coef = ceil((abs(lsdNUMFIP)/100.0)*lsdNIP);
                                }
                                else if(lsdNUMFIP < -100)
                                {
                                    n_coef = ceil((abs(lsdNUMFIP-100.0)/100.0)*lsdNIP);
                                } // if(lsdNUMFIP > 0)
 
                                if (lsdNIP > 1) radPthickfail = (2.0*n_coef-1)*NipWeights[lsdNIP];
                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                            }
                        }

                        if(nbSolid > 0)
                        {
                            if(lsdNUMFIP > 0) p_ConvertUtils.CopyValue(*selMatAddErosion, failInievoEdit, "NUMFIP", "FAILIP");
                        }
                    }

                    HandleRead crvLCREGDHandle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCREGD"), crvLCREGDHandle);
                    if (crvLCREGDHandle.IsValid())
                    {
                        EntityId crvLCREGId = crvLCREGDHandle.GetId(p_lsdynaModel);
                        failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("TAB_EL"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCREGId)));
                    }

                    int Idamsize = abs(lsdIDAM);

                    sdiDoubleList lsdDITYP, lsdDETYP, lsdDCTYP, lsdP2, lsdP3, lsdQ1, lsdQ3, lsdQ1DAMAGE;
                    vector<reference_wrapper<sdiDoubleList>> attrValList = { lsdDITYP,lsdDETYP, lsdDCTYP, lsdP2, lsdP3, lsdQ1, lsdQ1DAMAGE};
                    vector<sdiString> attrNameList = { "DITYP", "DETYP", "DCTYP", "P2", "P3", "Q1","Q1_DAMAGE"};
                    p_ConvertUtils.GetAttribValues(*selMatAddErosion, attrNameList, attrValList);

                    if(Idamsize > 0)
                    {
                        for (int i = 0; i < Idamsize; ++i)
                        {
                            tempValue = sdiValue(lsdDITYP);
                            selMatAddErosion->GetValue(sdiIdentifier("DITYP",0,i), tempValue);
                            tempValue.GetValue(lsdDITYP);

                            if(lsdDITYP[i] == 0.0)
                              failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("INITYPE",0,i),sdiValue(1));
                            else if(lsdDITYP[i] == 1.0)
                              failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("INITYPE",0,i),sdiValue(2));
                            else if(lsdDITYP[i] == 2.0)
                              failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("INITYPE",0,i),sdiValue(3));
                            else if(lsdDITYP[i] == 3.0)
                              failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("INITYPE",0,i),sdiValue(4));
                            else if(lsdDITYP[i] == 4.0)
                              failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("INITYPE",0,i),sdiValue(5));

                            // P1 --> functions/table
                            HandleRead crvP1Handle;
                            selMatAddErosion->GetEntityHandle(sdiIdentifier("P1",0,i), crvP1Handle);

                            if (crvP1Handle.IsValid())
                            {
                                EntityRead crvP1EntRead(p_lsdynaModel, crvP1Handle);
                                sdiString crvP1Type = crvP1EntRead.GetKeyword();
                                EntityId crvP1Id = crvP1EntRead.GetId();
                                EntityEdit failInievoEdit(p_radiossModel, failInievoHEdit);
                                if(!crvP1Type.compare(0, 13, "*DEFINE_CURVE"))
                                {
                                    failInievoEdit.SetValue(sdiIdentifier("TAB_ID",0,i), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvP1Handle.GetId(p_lsdynaModel))));
                                }
                                    else if(!crvP1Type.compare(0, 13, "*DEFINE_TABLE"))
                                {
                                    failInievoEdit.SetValue(sdiIdentifier("TAB_ID",0,i), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), crvP1Handle.GetId(p_lsdynaModel))));
                                }
                            }

                            if(lsdDITYP[i] == 1.0 || lsdDITYP[i] == 4.0)
                            {
                            // P2
                                tempValue = sdiValue(lsdP2);
                                selMatAddErosion->GetValue(sdiIdentifier("P2",0,i), tempValue);
                                tempValue.GetValue(lsdP2);
                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("PARAM",0,i),sdiValue(lsdP2[i]));
                            }

                            if(lsdDITYP[i] == 2.0 || lsdDITYP[i] == 3.0)
                            {
                            // P3
                                tempValue = sdiValue(lsdP3);
                                selMatAddErosion->GetValue(sdiIdentifier("P3",0,i), tempValue);
                                tempValue.GetValue(lsdP3);
                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("PARAM",0,i),sdiValue(lsdP3[i]));
                            }

                            tempValue = sdiValue(lsdDETYP);
                            selMatAddErosion->GetValue(sdiIdentifier("DETYP",0,i), tempValue);
                            tempValue.GetValue(lsdDETYP);

                            if(lsdDETYP[i] == 0.0)
                            {
                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("EVOTYPE",0,i),sdiValue(1));
                            }
                            else if(lsdDETYP[i] == 1.0)
                            {
                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("EVOTYPE",0,i),sdiValue(2));
                            }

                            tempValue = sdiValue(lsdDCTYP);
                            selMatAddErosion->GetValue(sdiIdentifier("DCTYP",0,i), tempValue);
                            tempValue.GetValue(lsdDCTYP);
                            if(lsdDCTYP[i] == 0.0)
                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("COMPTYP",0,i),sdiValue(1));
                            else if(lsdDCTYP[i] == 1.0)
                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("COMPTYP",0,i),sdiValue(2));

                            // Q1

                            int lsdflagforQ1;
                            sdiValue tempVal(lsdflagforQ1);
                            selMatAddErosion->GetValue(sdiIdentifier("flagforQ1"), tempVal);
                            tempVal.GetValue(lsdflagforQ1);

                            if(lsdDETYP[i] == 0.0 && lsdflagforQ1 != 1)
                            {
                            // SCALAR(Q1)
                                tempValue = sdiValue(lsdQ1);
                                selMatAddErosion->GetValue(sdiIdentifier("Q1",0,i), tempValue);
                                tempValue.GetValue(lsdQ1);
                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("DISP",0,i),sdiValue(lsdQ1[i]));
                            }
                            else if(lsdDETYP[i] == 0.0 && lsdflagforQ1 == 1)
                            {
                                // DATA(Q1_CURVE)
                                HandleRead crvQ1Handle;
                                selMatAddErosion->GetEntityHandle(sdiIdentifier("Q1_CURVE",0,i), crvQ1Handle);

                                EntityRead crvQ1EntRead(p_lsdynaModel, crvQ1Handle);
                                sdiString crvQ1Type = crvQ1EntRead.GetKeyword();
                                EntityId crvQ1Id = crvQ1Handle.GetId(p_lsdynaModel);

                                int nPoints = 0;
                                sdiDoubleList crvPoints;
                                double min_DISP = 0.0, max_DISP = 0.0;
                                double SFA, SFO, OFFA, OFFO;

                                if(!crvQ1Type.compare(0, 13, "*DEFINE_CURVE"))
                                {
                                    //double SFA, SFO, OFFA, OFFO;
                                    tempValue = sdiValue(SFA);
                                    crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("SFA"), tempValue);
                                    tempValue.GetValue(SFA);
                                    SFA = (SFA == 0.0) ? 1.0 : SFA;

                                    tempValue = sdiValue(SFO);
                                    crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("SFO"), tempValue);
                                    tempValue.GetValue(SFO);
                                    SFO = (SFO == 0.0) ? 1.0 : SFO;

                                    tempValue = sdiValue(OFFA);
                                    crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("OFFA"), tempValue);
                                    tempValue.GetValue(OFFA);

                                    tempValue = sdiValue(OFFO);
                                    crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("OFFO"), tempValue);
                                    tempValue.GetValue(OFFO);

                                    //int nPoints = 0;
                                    tempValue = sdiValue(nPoints);
                                    crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("numberofpoints"), tempValue);
                                    tempValue.GetValue(nPoints);
                                    if (nPoints)
                                    {
                                        //sdiDoubleList crvPoints;
                                        tempValue = sdiValue(crvPoints);
                                        crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("points"), tempValue);
                                        tempValue.GetValue(crvPoints);

                                        //double min_DISP,max_DISP;
                                        // Assume first element as maximum and last element as minimum
                                        max_DISP = (crvPoints[0] + OFFO)*SFO;
                                        min_DISP = (crvPoints[nPoints-1] + OFFO)*SFO;

                                        // Find maximum and minimum in all array elements.
                                        for (int k = 0; k < 2 * nPoints; k += 2)
                                        {
                                        // If current element is greater than max
                                            if ((crvPoints[k+1] + OFFO)*SFO > max_DISP)
                                                max_DISP = (crvPoints[k+1] + OFFO)*SFO;

                                        // If current element is smaller than min
                                            if ((crvPoints[k+1] + OFFO)*SFO < min_DISP)
                                                min_DISP = (crvPoints[k+1] + OFFO)*SFO;
                                        }
                                    } // if (nPoints)
                                    failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("DISP",0,i),sdiValue(min_DISP));
                                }
                                else if(!crvQ1Type.compare(0, 13, "*DEFINE_TABLE"))
                                {
                                 // Loop over table's curves and get min_DISP
                                    int tableId = crvQ1Id;
                                    HandleEdit RadTableHandle;
                                    p_radiossModel->FindById(p_radiossModel->GetEntityType("/TABLE/1"), tableId, RadTableHandle);

                                    int nFunct = 0;
                                    sdiValue tempValue(nFunct);
                                    RadTableHandle.GetValue(p_radiossModel, sdiIdentifier("curverows"), tempValue);
                                    tempValue.GetValue(nFunct);

                                    for (int j = 0; j < nFunct; ++j)
                                    {
                                        sdiValueEntity functabEntity;
                                        RadTableHandle.GetValue(p_radiossModel, sdiIdentifier("fct_ID",0,j), tempValue);
                                        tempValue.GetValue(functabEntity);
                                        int functabId = functabEntity.GetId();

                                        HandleRead functabHRead;
                                        p_radiossModel->FindById(p_radiossModel->GetEntityType("/FUNCT"), functabId, functabHRead);

                                        if (functabHRead.IsValid())
                                        {
                                            //int nPoints;
                                            tempValue = sdiValue(nPoints);
                                            functabHRead.GetValue(p_radiossModel, sdiIdentifier("numberofpoints"), tempValue);
                                            tempValue.GetValue(nPoints);

                                            //sdiDoubleList crvPoints;
                                            tempValue = sdiValue(crvPoints);
                                            functabHRead.GetValue(p_radiossModel, sdiIdentifier("points"), tempValue);
                                            tempValue.GetValue(crvPoints);

                                            tempValue = sdiValue(SFA);
                                            functabHRead.GetValue(p_radiossModel, sdiIdentifier("A_SCALE_X"), tempValue);
                                            tempValue.GetValue(SFA);
                                            SFA = (SFA == 0.0) ? 1.0 : SFA;

                                            tempValue = sdiValue(SFO);
                                            functabHRead.GetValue(p_radiossModel, sdiIdentifier("F_SCALE_Y"), tempValue);
                                            tempValue.GetValue(SFO);
                                            SFO = (SFO == 0.0) ? 1.0 : SFO;

                                            tempValue = sdiValue(OFFA);
                                            functabHRead.GetValue(p_lsdynaModel, sdiIdentifier("A_SHIFT_X"), tempValue);
                                            tempValue.GetValue(OFFA);

                                            tempValue = sdiValue(OFFO);
                                            functabHRead.GetValue(p_lsdynaModel, sdiIdentifier("F_SHIFT_Y"), tempValue);
                                            tempValue.GetValue(OFFO);

                                            //double min_DISP,max_DISP;
                                            // Assume first element as maximum and last element as minimum
                                            if(j==0) // first table function initialization
                                            {
                                                max_DISP = (crvPoints[0] + OFFO)*SFO;
                                                min_DISP = (crvPoints[nPoints-1] + OFFO)*SFO;
                                            }

                                            // Find maximum and minimum in all array elements.
                                            for (int k = 0; k < 2 * nPoints; k += 2)
                                            {
                                                // If current element is greater than max
                                                if ((crvPoints[k+1] + OFFO)*SFO > max_DISP)
                                                    max_DISP = (crvPoints[k+1] + OFFO)*SFO;
                                                // If current element is smaller than min
                                                if ((crvPoints[k+1] + OFFO)*SFO < min_DISP)
                                                    min_DISP = (crvPoints[k+1] + OFFO)*SFO;
                                            }
                                        } // if (functabHRead.IsValid())
                                    } // for (int j = 0; j < nFunct; ++j)
                                    failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("DISP",0,i),sdiValue(min_DISP));
                                } // if(!crvQ1Type.compare(0, 13, "*DEFINE_CURVE"))
                            }
                            else if(lsdDETYP[i] == 1.0)
                            {
                                // ENER = SCALAR(Q1)
                                tempValue = sdiValue(lsdQ1DAMAGE);
                                selMatAddErosion->GetValue(sdiIdentifier("Q1_DAMAGE",0,i), tempValue);
                                tempValue.GetValue(lsdQ1DAMAGE);

                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("ENER",0,i),sdiValue(lsdQ1DAMAGE[i]));
                            } // if(lsdDETYP[i] == 0.0 && lsdflagforQ1 != 1)

                            // Q2 - ignored

                        } // for (int i = 0; i < Idamsize; ++i)
                    } // if(Idamsize > 0)

                } // if (!failInievoHEdit.IsValid())
//---------------------
//---------------------
                if (failInievoHEdit.IsValid())
                {
                    failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("mat_id"), sdiValue(sdiValueEntity(destEntityType, radMatConvertedHandles[i].GetId(p_radiossModel))));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(failInievoHEdit, sourcehandleList));
                }
//---------------------
//---------------------
            }
            else if (lsdIDAM == 1 && lsdNCS > 1.0)
            {
                //----------------------------------------------
                // -- convert to /FAIL/TAB2 + /FAIL/GENE1 -- //
                //----------------------------------------------
                //

                // -- /FAIL/TAB2 --
                HandleEdit faiTab2HEdit;
                if (!faiTab2HEdit.IsValid())
                {
//---------------------
                    p_radiossModel->CreateEntity(faiTab2HEdit, "/FAIL/TAB2", matAddErosionName);
                    faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));

                    EntityEdit faiTab2Edit(p_radiossModel, faiTab2HEdit);

                    unsigned int partId = p_PartBeingConverted.GetId(p_radiossModel);
                    // get elem part type (shell/solid)

                    int nbShell =0;
                    int nbSolid =0;

                    SelectionElementRead elemShellSelect(p_lsdynaModel, "*ELEMENT_SHELL");
                    nbShell = elemShellSelect.Count();

                    SelectionElementRead elemSolidSelect(p_lsdynaModel, "*ELEMENT_SOLID");
                    nbSolid = elemSolidSelect.Count();

                    // get NIP (nb of integration points through thickness) from *SECTION_SHELL

                    HandleRead partHRead;
                    p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), partId, partHRead);

                    if (partHRead.IsValid())
                    {
                        HandleRead propHRead;
                        partHRead.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHRead);
                        EntityRead propEntityRead(p_lsdynaModel, propHRead);

                        int lsdelform;
                        tempValue = sdiValue(lsdelform);
                        propEntityRead.GetValue(sdiIdentifier("ELFORM"), tempValue);
                        tempValue.GetValue(lsdelform);

                        int lsdNIP = 0;
                        tempValue = sdiValue(lsdNIP);
                        propEntityRead.GetValue(sdiIdentifier("LSD_NIP"), tempValue);
                        tempValue.GetValue(lsdNIP);

                        if(nbShell > 0)
                        {
                            if(lsdNUMFIP == 0.0) lsdNUMFIP = 1.0 ; // by default
                           //Full-Integrated linear shell element: (elfrom = 9, 16, -16,17, 18, 20, 21, 22, 26, 30)
                            if(lsdelform == 9  || lsdelform == 16 || lsdelform == -16 || 
                               lsdelform == 17 || lsdelform == 18 || lsdelform ==  20 || 
                               lsdelform == 21 || lsdelform == 22  || lsdelform == 26 || 
                               lsdelform == 30)
                            {
                                if(lsdNUMFIP > 0)
                                {
                                    double lsdGauss = 4.0; // inplane shell integration points
                                    double radVOLFRAC;
                                    radVOLFRAC = abs(lsdNUMFIP)/(lsdGauss*lsdNIP);
                                    faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("Volfrac"),sdiValue(radVOLFRAC));
                                }
                                else if(lsdNUMFIP < 0 && lsdNUMFIP >= -100)
                                {
                                    radPthickfail = abs(lsdNUMFIP)/100.0;
                                    faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                                }
                                else if(lsdNUMFIP < -100)
                                {
                                    radPthickfail = (abs(lsdNUMFIP)-100.0)/100.0;
                                    faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                                } // if(lsdNUMFIP > 0)
                            }
                            else if(lsdelform == 23  || lsdelform == 24)
                            {
                                // higher order shell element:
                                p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "VOLFRAC", "Volfrac");
                            }
                            else
                            {
                                // Reduced-Integrated linear shell element
                                if(lsdNUMFIP > 0)
                                {
                                    n_coef = lsdNUMFIP;
                                }
                                else if(lsdNUMFIP < 0 && lsdNUMFIP >= -100)
                                {
                                    n_coef = ceil((abs(lsdNUMFIP)/100.0)*lsdNIP);
                                }
                                else if(lsdNUMFIP < -100)
                                {
                                    n_coef = ceil((abs(lsdNUMFIP-100.0)/100.0)*lsdNIP);
                                } // if(lsdNUMFIP > 0)
 
                                if (lsdNIP > 1) radPthickfail = (2.0*n_coef-1)*NipWeights[lsdNIP];
                                faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                            }
                        }

                        if(nbSolid > 0)
                        {
                            if(lsdNUMFIP > 0) p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "NUMFIP", "FAILIP");
                        }

                        
                        HandleRead crvLCSDGHandle;
                        selMatAddErosion->GetEntityHandle(sdiIdentifier("LCSDG"), crvLCSDGHandle);
                        if (crvLCSDGHandle.IsValid())
                        {
                            EntityId crvLCSDGId = crvLCSDGHandle.GetId(p_lsdynaModel);
                            faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("EPSF_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCSDGId)));
                        }


                    bool optECRITcurve;
                    tempValue = sdiValue(optECRITcurve);
                    selMatAddErosion->GetValue(sdiIdentifier("OPT_ECRIT_curve"), tempValue);
                    tempValue.GetValue(optECRITcurve);

                    if (optECRITcurve  == false)
                    {
                        p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "ECRIT", "ECRIT");
                    }
                    else
                    {
                        // LSD_ECRIT_CURVE --> function INST_ID
                        HandleRead crvECRITHandle;
                        selMatAddErosion->GetEntityHandle(sdiIdentifier("ECRIT_CURVE"), crvECRITHandle);

                        if (crvECRITHandle.IsValid())
                        {
                            EntityRead crvECRITEntRead(p_lsdynaModel, crvECRITHandle);
                            EntityId crvECRITId = crvECRITEntRead.GetId();
                            //EntityId crvECRITId = crvECRITHandle.GetId(p_lsdynaModel);
                            faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("INST_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvECRITId)));
                        }
                    } // if (optECRITcurve  == false)

                    p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "DMGEXP", "N");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "DCRIT", "DCRIT");

                    bool optFADEXPcurve;
                    tempValue = sdiValue(optFADEXPcurve);
                    selMatAddErosion->GetValue(sdiIdentifier("OPT_FADEXP_curve"), tempValue);
                    tempValue.GetValue(optFADEXPcurve);

                    if (optFADEXPcurve  == false)
                    {
                        p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "FADEXP", "EXP");
                    }
                    else
                    {
                        // LSD_FADEXP_CURVE --> function FCT_EXP
                        HandleRead crvFADEXPHandle;
                        selMatAddErosion->GetEntityHandle(sdiIdentifier("FADEXP_CURVE"), crvFADEXPHandle);

                        if (crvFADEXPHandle.IsValid())
                        {
                            EntityRead crvFADEXPEntRead(p_lsdynaModel, crvFADEXPHandle);
                            EntityId crvFADEXPId = crvFADEXPEntRead.GetId();
                            //EntityId crvFADEXPId = crvFADEXPHandle.GetId(p_lsdynaModel);
                            faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("FCT_EXP"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvFADEXPId)));
                        }
                    } // if (optFADEXPcurve  == false)

                    HandleRead crvLCREGDHandle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCREGD"), crvLCREGDHandle);
                    if (crvLCREGDHandle.IsValid())
                    {
                        EntityId crvLCREGId = crvLCREGDHandle.GetId(p_lsdynaModel);
                        faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("TAB_EL"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCREGId)));
                    }

                    p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "REFSZ", "EL_REF");

                    HandleRead crvLCSRSHandle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCSRS"), crvLCSRSHandle);
                    if (crvLCSRSHandle.IsValid())
                    {
                        EntityId crvLCSRSId = crvLCSRSHandle.GetId(p_lsdynaModel);
                        faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("FCT_SR"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCSRSId)));
                    }

                    p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "SHRF", "SHRF");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, faiTab2Edit, "BIAXF", "BIAXF");

                    } // if (partHRead.IsValid())
//---------------------
                } // if (!faiTab2HEdit.IsValid())
//---------------------
//---------------------
                if (faiTab2HEdit.IsValid())
                {
                    faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("mat_id"), sdiValue(sdiValueEntity(destEntityType, radMatConvertedHandles[i].GetId(p_radiossModel))));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(faiTab2HEdit, sourcehandleList));
                }
//---------------------
//---------------------
                // -- /FAIL/GENE1 --
                HandleEdit failGene1HEdit;
                if (!failGene1HEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(failGene1HEdit, "/FAIL/GENE1", matAddErosionName);
                    failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));

                    EntityEdit failGene1Edit(p_radiossModel, failGene1HEdit);
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "MXPRES", "Pmax");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "MNEPS", "Eps_min");

                    if(lsdEFFEPS >= 0.0) p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "EFFEPS", "Eps_eff");
                    else failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Eps_eff"),sdiValue(abs(lsdEFFEPS)));

                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "VOLEPS", "Eps_vol");

                    unsigned int partId = p_PartBeingConverted.GetId(p_radiossModel);
                    // get elem part type (shell/solid)

                    int nbShell =0;
                    int nbSolid =0;

                    SelectionElementRead elemShellSelect(p_lsdynaModel, "*ELEMENT_SHELL");
                    nbShell = elemShellSelect.Count();

                    SelectionElementRead elemSolidSelect(p_lsdynaModel, "*ELEMENT_SOLID");
                    nbSolid = elemSolidSelect.Count();

                    // get NIP (nb of integration points through thickness) from *SECTION_SHELL
                    int Ishellsolid = 0;
                    HandleRead partHRead;
                    p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), partId, partHRead);

                    if (partHRead.IsValid())
                    {
                        HandleRead propHRead;
                        partHRead.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHRead);

                        EntityRead propEntityRead(p_lsdynaModel, propHRead);
                        int lsdelform;
                        tempValue = sdiValue(lsdelform);
                        propEntityRead.GetValue(sdiIdentifier("ELFORM"), tempValue);
                        tempValue.GetValue(lsdelform);

                        int lsdNIP = 0;
                        tempValue = sdiValue(lsdNIP);
                        propEntityRead.GetValue(sdiIdentifier("LSD_NIP"), tempValue);
                        tempValue.GetValue(lsdNIP);

                        if(nbShell > 0)
                        {
                            //Full-Integrated linear shell element: (elfrom = 9, 16, -16,17, 18, 20, 21, 22, 26, 30)
                            if(lsdelform == 9  || lsdelform == 16 || lsdelform == -16 || 
                               lsdelform == 17 || lsdelform == 18 || lsdelform ==  20 || 
                               lsdelform == 21 || lsdelform == 22  || lsdelform == 26 || 
                               lsdelform == 30)
                            {
                                if(lsdNUMFIP > 0)
                                {
                                    double lsdGauss = 4.0; // inplane shell integration points
                                    double radVOLFRAC;
                                    radVOLFRAC = abs(lsdNUMFIP)/(lsdGauss*lsdNIP);
                                    failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Volfrac"),sdiValue(radVOLFRAC));
                                }
                                else if(lsdNUMFIP < 0 && lsdNUMFIP >= -100)
                                {
                                    radPthickfail = abs(lsdNUMFIP)/100.0;
                                    failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Pthickfail"),sdiValue(radPthickfail));
                                }
                                else if(lsdNUMFIP < -100)
                                {
                                    radPthickfail = (abs(lsdNUMFIP)-100.0)/100.0;
                                    failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Pthickfail"),sdiValue(radPthickfail));
                                } // if(lsdNUMFIP > 0)
                            }
                            else if(lsdelform == 23  || lsdelform == 24)
                            {
                                // higher order shell element:
                                p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "VOLFRAC", "Volfrac");
                            }
                            else
                            {
                                // Reduced-Integrated linear shell element
                                if(lsdNUMFIP > 0)
                                {
                                    n_coef = lsdNUMFIP;
                                }
                                else if(lsdNUMFIP < 0 && lsdNUMFIP >= -100)
                                {
                                    n_coef = ceil((abs(lsdNUMFIP)/100.0)*lsdNIP);
                                }
                                else if(lsdNUMFIP < -100)
                                {
                                    n_coef = ceil((abs(lsdNUMFIP-100.0)/100.0)*lsdNIP);
                                } // if(lsdNUMFIP > 0)
 
                                if (lsdNIP > 1) radPthickfail = (2.0*n_coef-1)*NipWeights[lsdNIP];
                                failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Pthickfail"),sdiValue(radPthickfail));
                            }
                        } // if(nbShell > 0)

                        if(nbSolid > 0)
                        {
                            if(lsdelform == 2 || lsdelform == 3)
                            {
                                //Full-Integrated linear solid element: (elfrom = 2, 3)
                                double radVOLFRAC;
                                lsdNIP = 8;
                                radVOLFRAC = abs(lsdNUMFIP)/lsdNIP;
                                failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Volfrac"),sdiValue(radVOLFRAC));
                            }
                            else if(lsdelform == 24 || lsdelform == 25 || lsdelform == 26 || 
                                    lsdelform == 27 || lsdelform == 28 || lsdelform == 29)
                            {
                                // higher order solid element:
                                p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "VOLFRAC", "Volfrac");
                            }
                            else
                            {
                                // Reduced-Integrated linear solid element: (elform =1, 4, 0, 9, 10, 12, 13)
                                failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Volfrac"),sdiValue(0.5));
                            }
                        } // if(nbSolid > 0)
                    } // if (partHRead.IsValid())

                    //p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "NCS", "NCS");
                    p_ConvertUtils.SetExpressionValue(*selMatAddErosion, failGene1Edit, "(NCS-1)", "NCS");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "MNPRES", "Pmin");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "SIGP1", "SigP1_max");

                    bool optSIGVMcurve;
                    tempValue = sdiValue(optSIGVMcurve);
                    selMatAddErosion->GetValue(sdiIdentifier("OPT_SIGVM_curve"), tempValue);
                    tempValue.GetValue(optSIGVMcurve);

                    if (optSIGVMcurve  == false)
                    {
                        p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "SIGVM", "Sig_max");
                    }
                    else
                    {
                        // LSD_SIGVM_CURVE --> function fct_Idps
                        HandleRead crvSIGVMHandle;
                        selMatAddErosion->GetEntityHandle(sdiIdentifier("SIGVM_CURVE"), crvSIGVMHandle);

                        if (crvSIGVMHandle.IsValid())
                        {
                            EntityRead crvSIGVMEntRead(p_lsdynaModel, crvSIGVMHandle);
                            EntityId crvSIGVMId = crvSIGVMEntRead.GetId();
                            //EntityId crvSIGVMId = crvSIGVMHandle.GetId(p_lsdynaModel);
                            failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDsm"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvSIGVMId)));
                        }
                    } // if (optSIGVMcurve  == false)

                    bool optMXEPScurve;
                    tempValue = sdiValue(optMXEPScurve);
                    selMatAddErosion->GetValue(sdiIdentifier("OPT_MXEPS_curve"), tempValue);
                    tempValue.GetValue(optMXEPScurve);

                    if (optMXEPScurve  == false)
                    {
                        p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "MXEPS", "Eps_max");
                    }
                    else
                    {
                        // LSD_MXEPS_CURVE --> function fct_Idps
                        HandleRead crvMXEPSHandle;
                        selMatAddErosion->GetEntityHandle(sdiIdentifier("MXEPS_CURVE"), crvMXEPSHandle);
                        //selMatAddErosion->GetEntityHandle(sdiIdentifier("LSD_MXEPS_CURVE"), crvMXEPSHandle);

                        if (crvMXEPSHandle.IsValid())
                        {
                            EntityRead crvMXEPSEntRead(p_lsdynaModel, crvMXEPSHandle);
                            EntityId crvMXEPSId = crvMXEPSEntRead.GetId();
                            //EntityId crvMXEPSId = crvMXEPSHandle.GetId(p_lsdynaModel);
                            failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDps"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvMXEPSId)));
                        }
                    } // if (optMXEPScurve  == false)

                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "EPSSH", "Eps_s");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "SIGTH", "Sigr");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "IMPULSE", "K");
                    if(lsdFAILTM >0) p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "FAILTM", "Time_max");

                    HandleRead crvLCREGDHandle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCREGD"), crvLCREGDHandle);
                    if (crvLCREGDHandle.IsValid())
                    {
                        EntityId crvLCREGId = crvLCREGDHandle.GetId(p_lsdynaModel);
                        failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDel"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCREGId)));
                    }

                    
                    HandleRead crvLCFLDHandle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCFLD"), crvLCFLDHandle);

                    if (crvLCFLDHandle.IsValid())
                    {
                        EntityRead crvLCFLDEntRead(p_lsdynaModel, crvLCFLDHandle);
                        sdiString crvLCFLDType = crvLCFLDEntRead.GetKeyword();
                        EntityId crvLCFLDId = crvLCFLDEntRead.GetId();
                        EntityEdit failGene1Edit(p_radiossModel, failGene1HEdit);

                        if(!crvLCFLDType.compare(0, 13, "*DEFINE_TABLE"))
                        {
                            failGene1Edit.SetValue(sdiIdentifier("tab_IDfld"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), crvLCFLDId)));
                            failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Itab"),sdiValue(1));
                            failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Istrain"),sdiValue(1));
                            failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Ismooth"),sdiValue(1));
                        }
                    } // if (crvLCFLDHandle.IsValid())

                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "NSFF", "Nstep");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "EPSTHIN", "Thinning");

                    HandleRead crvLCEPS12Handle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCEPS12"), crvLCEPS12Handle);
                    if (crvLCEPS12Handle.IsValid())
                    {
                        EntityId crvLCEPS12Id = crvLCEPS12Handle.GetId(p_lsdynaModel);
                        failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDg12"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCEPS12Id)));
                    }

                    HandleRead crvLCEPS13Handle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCEPS13"), crvLCEPS13Handle);
                    if (crvLCEPS13Handle.IsValid())
                    {
                        EntityId crvLCEPS13Id = crvLCEPS13Handle.GetId(p_lsdynaModel);
                        failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDg13"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCEPS13Id)));
                    }

                    HandleRead crvLCEPSMXHandle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCEPSMX"), crvLCEPSMXHandle);
                    if (crvLCEPSMXHandle.IsValid())
                    {
                        EntityId crvLCEPSMXId = crvLCEPSMXHandle.GetId(p_lsdynaModel);
                        failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDe1c"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCEPSMXId)));
                    }

                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "MXTMP", "Temp_max");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "DTMIN", "dtmin");

                }  // if (!failGene1HEdit.IsValid())
//---------------------
//---------------------
                if (failGene1HEdit.IsValid())
                {
                    failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("mat_id"), sdiValue(sdiValueEntity(destEntityType, radMatConvertedHandles[i].GetId(p_radiossModel))));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(failGene1HEdit, sourcehandleList));
                }
//---------------------
//---------------------
            }
            else if (lsdIDAM < 0 && lsdNCS > 1.0)
            {
                //----------------------------------------------
                // -- convert to /FAIL/INIEVO + /FAIL/GENE1 -- //
                //----------------------------------------------
                //

                // -- /FAIL/INIEVO --
                HandleEdit failInievoHEdit;
                if (!failInievoHEdit.IsValid())
                {
//---------------------
                    sdiValue tempValue;
                    HandleRead matHandle;
                    sdiValueEntity lsdMIDEntity;

                    tempValue = sdiValue(lsdMIDEntity);
                    selMatAddErosion->GetValue(sdiIdentifier("MID"), tempValue);
                    tempValue.GetValue(lsdMIDEntity);

                    p_radiossModel->CreateEntity(failInievoHEdit, "/FAIL/INIEVO", matAddErosionName,lsdMIDEntity.GetId());
                    failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));

                    EntityEdit failInievoEdit(p_radiossModel, failInievoHEdit);

                    selMatAddErosion->GetEntityHandle(sdiIdentifier("MID"), matHandle);
                    sdiConvert::SDIHandlReadList radMatConvertedHandles;

                    sdiConvert::Convert::GetConvertedHandles(matHandle, radMatConvertedHandles, destEntityType);
                    sdiConvert::SDIHandlReadList sourcehandleList = { {selMatAddErosion->GetHandle()} };

                    failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("NINIEVO"),sdiValue(abs(lsdIDAM)));

                    unsigned int partId = p_PartBeingConverted.GetId(p_lsdynaModel);
                    // get elem part type (shell/solid)

                    int nbShell =0;
                    int nbSolid =0;

                    SelectionElementRead elemShellSelect(p_lsdynaModel, "*ELEMENT_SHELL");
                    nbShell = elemShellSelect.Count();
                    SelectionElementRead elemSolidSelect(p_lsdynaModel, "*ELEMENT_SOLID");
                    nbSolid = elemSolidSelect.Count();

                    // get NIP (nb of integration points through thickness) from *SECTION_SHELL
                    HandleRead partHRead;
                    p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), partId, partHRead);

                    double lsdNUMFIP;
                    tempValue = sdiValue(lsdNUMFIP);
                    selMatAddErosion->GetValue(sdiIdentifier("NUMFIP"), tempValue);
                    tempValue.GetValue(lsdNUMFIP);

                    if (partHRead.IsValid())
                    {
                        HandleRead propHRead;
                        partHRead.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHRead);
                        EntityRead propEntityRead(p_lsdynaModel, propHRead);

                        int lsdelform;
                        tempValue = sdiValue(lsdelform);
                        propEntityRead.GetValue(sdiIdentifier("ELFORM"), tempValue);
                        tempValue.GetValue(lsdelform);

                        int lsdNIP = 0;
                        tempValue = sdiValue(lsdNIP);
                        propEntityRead.GetValue(sdiIdentifier("LSD_NIP"), tempValue);
                        tempValue.GetValue(lsdNIP);

                        if(nbShell > 0)
                        {
                            if(lsdNUMFIP == 0.0) lsdNUMFIP = 1.0 ; // by default
                           //Full-Integrated linear shell element: (elfrom = 9, 16, -16,17, 18, 20, 21, 22, 26, 30)
                            if(lsdelform == 9  || lsdelform == 16 || lsdelform == -16 || 
                               lsdelform == 17 || lsdelform == 18 || lsdelform ==  20 || 
                               lsdelform == 21 || lsdelform == 22  || lsdelform == 26 || 
                               lsdelform == 30)
                            {
                                if(lsdNUMFIP > 0)
                                {
                                    double lsdGauss = 4.0; // inplane shell integration points
                                    double radVOLFRAC;
                                    radVOLFRAC = abs(lsdNUMFIP)/(lsdGauss*lsdNIP);
                                    failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("Volfrac"),sdiValue(radVOLFRAC));
                                }
                                else if(lsdNUMFIP < 0 && lsdNUMFIP >= -100)
                                {
                                    radPthickfail = abs(lsdNUMFIP)/100.0;
                                    failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                                }
                                else if(lsdNUMFIP < -100)
                                {
                                    radPthickfail = (abs(lsdNUMFIP)-100.0)/100.0;
                                    failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                                } // if(lsdNUMFIP > 0)
                            }
                            else if(lsdelform == 23  || lsdelform == 24)
                            {
                                // higher order shell element:
                                p_ConvertUtils.CopyValue(*selMatAddErosion, failInievoEdit, "VOLFRAC", "Volfrac");
                            }
                            else
                            {
                                // Reduced-Integrated linear shell element
                                if(lsdNUMFIP > 0)
                                {
                                    n_coef = lsdNUMFIP;
                                }
                                else if(lsdNUMFIP < 0 && lsdNUMFIP >= -100)
                                {
                                    n_coef = ceil((abs(lsdNUMFIP)/100.0)*lsdNIP);
                                }
                                else if(lsdNUMFIP < -100)
                                {
                                    n_coef = ceil((abs(lsdNUMFIP-100.0)/100.0)*lsdNIP);
                                } // if(lsdNUMFIP > 0)
 
                                if (lsdNIP > 1) radPthickfail = (2.0*n_coef-1)*NipWeights[lsdNIP];
                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                            }
                        }

                        if(nbSolid > 0)
                        {
                            if(lsdNUMFIP > 0) p_ConvertUtils.CopyValue(*selMatAddErosion, failInievoEdit, "NUMFIP", "FAILIP");
                        }
                    }

                    HandleRead crvLCREGDHandle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCREGD"), crvLCREGDHandle);
                    if (crvLCREGDHandle.IsValid())
                    {
                        EntityId crvLCREGId = crvLCREGDHandle.GetId(p_lsdynaModel);
                        failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("TAB_EL"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCREGId)));
                    }

                    int Idamsize = abs(lsdIDAM);

                    sdiDoubleList lsdDITYP, lsdDETYP, lsdDCTYP, lsdP2, lsdP3, lsdQ1, lsdQ3, lsdQ1DAMAGE;
                    vector<reference_wrapper<sdiDoubleList>> attrValList = { lsdDITYP,lsdDETYP, lsdDCTYP, lsdP2, lsdP3, lsdQ1, lsdQ1DAMAGE};
                    vector<sdiString> attrNameList = { "DITYP", "DETYP", "DCTYP", "P2", "P3", "Q1","Q1_DAMAGE"};
                    p_ConvertUtils.GetAttribValues(*selMatAddErosion, attrNameList, attrValList);

                    if(Idamsize > 0)
                    {
                        for (int i = 0; i < Idamsize; ++i)
                        {
                            tempValue = sdiValue(lsdDITYP);
                            selMatAddErosion->GetValue(sdiIdentifier("DITYP",0,i), tempValue);
                            tempValue.GetValue(lsdDITYP);

                            if(lsdDITYP[i] == 0.0)
                              failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("INITYPE",0,i),sdiValue(1));
                            else if(lsdDITYP[i] == 1.0)
                              failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("INITYPE",0,i),sdiValue(2));
                            else if(lsdDITYP[i] == 2.0)
                              failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("INITYPE",0,i),sdiValue(3));
                            else if(lsdDITYP[i] == 3.0)
                              failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("INITYPE",0,i),sdiValue(4));
                            else if(lsdDITYP[i] == 4.0)
                              failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("INITYPE",0,i),sdiValue(5));

                            // P1 --> functions/table
                            HandleRead crvP1Handle;
                            selMatAddErosion->GetEntityHandle(sdiIdentifier("P1",0,i), crvP1Handle);

                            if (crvP1Handle.IsValid())
                            {
                                EntityRead crvP1EntRead(p_lsdynaModel, crvP1Handle);
                                sdiString crvP1Type = crvP1EntRead.GetKeyword();
                                EntityId crvP1Id = crvP1EntRead.GetId();
                                EntityEdit failInievoEdit(p_radiossModel, failInievoHEdit);
                                if(!crvP1Type.compare(0, 13, "*DEFINE_CURVE"))
                                {
                                    failInievoEdit.SetValue(sdiIdentifier("TAB_ID",0,i), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvP1Handle.GetId(p_lsdynaModel))));
                                }
                                    else if(!crvP1Type.compare(0, 13, "*DEFINE_TABLE"))
                                {
                                    failInievoEdit.SetValue(sdiIdentifier("TAB_ID",0,i), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), crvP1Handle.GetId(p_lsdynaModel))));
                                }
                            }

                            if(lsdDITYP[i] == 1.0 || lsdDITYP[i] == 4.0)
                            {
                            // P2
                                tempValue = sdiValue(lsdP2);
                                selMatAddErosion->GetValue(sdiIdentifier("P2",0,i), tempValue);
                                tempValue.GetValue(lsdP2);
                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("PARAM",0,i),sdiValue(lsdP2[i]));
                            }

                            if(lsdDITYP[i] == 2.0 || lsdDITYP[i] == 3.0)
                            {
                            // P3
                                tempValue = sdiValue(lsdP3);
                                selMatAddErosion->GetValue(sdiIdentifier("P3",0,i), tempValue);
                                tempValue.GetValue(lsdP3);
                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("PARAM",0,i),sdiValue(lsdP3[i]));
                            }

                            tempValue = sdiValue(lsdDETYP);
                            selMatAddErosion->GetValue(sdiIdentifier("DETYP",0,i), tempValue);
                            tempValue.GetValue(lsdDETYP);

                            if(lsdDETYP[i] == 0.0)
                            {
                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("EVOTYPE",0,i),sdiValue(1));
                            }
                            else if(lsdDETYP[i] == 1.0)
                            {
                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("EVOTYPE",0,i),sdiValue(2));
                            }

                            tempValue = sdiValue(lsdDCTYP);
                            selMatAddErosion->GetValue(sdiIdentifier("DCTYP",0,i), tempValue);
                            tempValue.GetValue(lsdDCTYP);
                            if(lsdDCTYP[i] == 0.0)
                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("COMPTYP",0,i),sdiValue(1));
                            else if(lsdDCTYP[i] == 1.0)
                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("COMPTYP",0,i),sdiValue(2));

                            // Q1

                            int lsdflagforQ1;
                            sdiValue tempVal(lsdflagforQ1);
                            selMatAddErosion->GetValue(sdiIdentifier("flagforQ1"), tempVal);
                            tempVal.GetValue(lsdflagforQ1);

                            if(lsdDETYP[i] == 0.0 && lsdflagforQ1 != 1)
                            {
                            // SCALAR(Q1)
                                tempValue = sdiValue(lsdQ1);
                                selMatAddErosion->GetValue(sdiIdentifier("Q1",0,i), tempValue);
                                tempValue.GetValue(lsdQ1);
                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("DISP",0,i),sdiValue(lsdQ1[i]));
                            }
                            else if(lsdDETYP[i] == 0.0 && lsdflagforQ1 == 1)
                            {
                                // DATA(Q1_CURVE)
                                HandleRead crvQ1Handle;
                                selMatAddErosion->GetEntityHandle(sdiIdentifier("Q1_CURVE",0,i), crvQ1Handle);

                                EntityRead crvQ1EntRead(p_lsdynaModel, crvQ1Handle);
                                sdiString crvQ1Type = crvQ1EntRead.GetKeyword();
                                EntityId crvQ1Id = crvQ1Handle.GetId(p_lsdynaModel);

                                int nPoints = 0;
                                sdiDoubleList crvPoints;
                                double min_DISP = 0.0, max_DISP = 0.0;
                                double SFA, SFO, OFFA, OFFO;

                                if(!crvQ1Type.compare(0, 13, "*DEFINE_CURVE"))
                                {
                                    //double SFA, SFO, OFFA, OFFO;
                                    tempValue = sdiValue(SFA);
                                    crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("SFA"), tempValue);
                                    tempValue.GetValue(SFA);
                                    SFA = (SFA == 0.0) ? 1.0 : SFA;

                                    tempValue = sdiValue(SFO);
                                    crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("SFO"), tempValue);
                                    tempValue.GetValue(SFO);
                                    SFO = (SFO == 0.0) ? 1.0 : SFO;

                                    tempValue = sdiValue(OFFA);
                                    crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("OFFA"), tempValue);
                                    tempValue.GetValue(OFFA);

                                    tempValue = sdiValue(OFFO);
                                    crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("OFFO"), tempValue);
                                    tempValue.GetValue(OFFO);

                                    //int nPoints = 0;
                                    tempValue = sdiValue(nPoints);
                                    crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("numberofpoints"), tempValue);
                                    tempValue.GetValue(nPoints);
                                    if (nPoints)
                                    {
                                        //sdiDoubleList crvPoints;
                                        tempValue = sdiValue(crvPoints);
                                        crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("points"), tempValue);
                                        tempValue.GetValue(crvPoints);

                                        //double min_DISP,max_DISP;
                                        // Assume first element as maximum and last element as minimum
                                        max_DISP = (crvPoints[0] + OFFO)*SFO;
                                        min_DISP = (crvPoints[nPoints-1] + OFFO)*SFO;

                                        // Find maximum and minimum in all array elements.
                                        for (int k = 0; k < 2 * nPoints; k += 2)
                                        {
                                        // If current element is greater than max
                                            if ((crvPoints[k+1] + OFFO)*SFO > max_DISP)
                                                max_DISP = (crvPoints[k+1] + OFFO)*SFO;

                                        // If current element is smaller than min
                                            if ((crvPoints[k+1] + OFFO)*SFO < min_DISP)
                                                min_DISP = (crvPoints[k+1] + OFFO)*SFO;
                                        }
                                    } // if (nPoints)
                                    failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("DISP",0,i),sdiValue(min_DISP));
                                }
                                else if(!crvQ1Type.compare(0, 13, "*DEFINE_TABLE"))
                                {
                                 // Loop over table's curves and get min_DISP
                                    int tableId = crvQ1Id;
                                    HandleEdit RadTableHandle;
                                    p_radiossModel->FindById(p_radiossModel->GetEntityType("/TABLE/1"), tableId, RadTableHandle);

                                    int nFunct = 0;
                                    sdiValue tempValue(nFunct);
                                    RadTableHandle.GetValue(p_radiossModel, sdiIdentifier("curverows"), tempValue);
                                    tempValue.GetValue(nFunct);

                                    for (int j = 0; j < nFunct; ++j)
                                    {
                                        sdiValueEntity functabEntity;
                                        RadTableHandle.GetValue(p_radiossModel, sdiIdentifier("fct_ID",0,j), tempValue);
                                        tempValue.GetValue(functabEntity);
                                        int functabId = functabEntity.GetId();

                                        HandleRead functabHRead;
                                        p_radiossModel->FindById(p_radiossModel->GetEntityType("/FUNCT"), functabId, functabHRead);

                                        if (functabHRead.IsValid())
                                        {
                                            //int nPoints;
                                            tempValue = sdiValue(nPoints);
                                            functabHRead.GetValue(p_radiossModel, sdiIdentifier("numberofpoints"), tempValue);
                                            tempValue.GetValue(nPoints);

                                            //sdiDoubleList crvPoints;
                                            tempValue = sdiValue(crvPoints);
                                            functabHRead.GetValue(p_radiossModel, sdiIdentifier("points"), tempValue);
                                            tempValue.GetValue(crvPoints);

                                            tempValue = sdiValue(SFA);
                                            functabHRead.GetValue(p_radiossModel, sdiIdentifier("A_SCALE_X"), tempValue);
                                            tempValue.GetValue(SFA);
                                            SFA = (SFA == 0.0) ? 1.0 : SFA;

                                            tempValue = sdiValue(SFO);
                                            functabHRead.GetValue(p_radiossModel, sdiIdentifier("F_SCALE_Y"), tempValue);
                                            tempValue.GetValue(SFO);
                                            SFO = (SFO == 0.0) ? 1.0 : SFO;

                                            tempValue = sdiValue(OFFA);
                                            functabHRead.GetValue(p_lsdynaModel, sdiIdentifier("A_SHIFT_X"), tempValue);
                                            tempValue.GetValue(OFFA);

                                            tempValue = sdiValue(OFFO);
                                            functabHRead.GetValue(p_lsdynaModel, sdiIdentifier("F_SHIFT_Y"), tempValue);
                                            tempValue.GetValue(OFFO);

                                            //double min_DISP,max_DISP;
                                            // Assume first element as maximum and last element as minimum
                                            if(j==0) // first table function initialization
                                            {
                                                max_DISP = (crvPoints[0] + OFFO)*SFO;
                                                min_DISP = (crvPoints[nPoints-1] + OFFO)*SFO;
                                            }

                                            // Find maximum and minimum in all array elements.
                                            for (int k = 0; k < 2 * nPoints; k += 2)
                                            {
                                                // If current element is greater than max
                                                if ((crvPoints[k+1] + OFFO)*SFO > max_DISP)
                                                    max_DISP = (crvPoints[k+1] + OFFO)*SFO;
                                                // If current element is smaller than min
                                                if ((crvPoints[k+1] + OFFO)*SFO < min_DISP)
                                                    min_DISP = (crvPoints[k+1] + OFFO)*SFO;
                                            }
                                        } // if (functabHRead.IsValid())
                                    } // for (int j = 0; j < nFunct; ++j)
                                    failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("DISP",0,i),sdiValue(min_DISP));
                                } // if(!crvQ1Type.compare(0, 13, "*DEFINE_CURVE"))
                            }
                            else if(lsdDETYP[i] == 1.0)
                            {
                                // ENER = SCALAR(Q1)
                                tempValue = sdiValue(lsdQ1DAMAGE);
                                selMatAddErosion->GetValue(sdiIdentifier("Q1_DAMAGE",0,i), tempValue);
                                tempValue.GetValue(lsdQ1DAMAGE);

                                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("ENER",0,i),sdiValue(lsdQ1DAMAGE[i]));
                            } // if(lsdDETYP[i] == 0.0 && lsdflagforQ1 != 1)

                            // Q2 - ignored

                        } // for (int i = 0; i < Idamsize; ++i)
                    } // if(Idamsize > 0)

                } // if (!failInievoHEdit.IsValid())
//---------------------
//---------------------
                if (failInievoHEdit.IsValid())
                {
                    failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("mat_id"), sdiValue(sdiValueEntity(destEntityType, radMatConvertedHandles[i].GetId(p_radiossModel))));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(failInievoHEdit, sourcehandleList));
                }
//---------------------
//---------------------
                // -- /FAIL/GENE1 --
                HandleEdit failGene1HEdit;
                if (!failGene1HEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(failGene1HEdit, "/FAIL/GENE1", matAddErosionName);
                    failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));

                    EntityEdit failGene1Edit(p_radiossModel, failGene1HEdit);
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "MXPRES", "Pmax");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "MNEPS", "Eps_min");

                    if(lsdEFFEPS >= 0.0) p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "EFFEPS", "Eps_eff");
                    else failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Eps_eff"),sdiValue(abs(lsdEFFEPS)));

                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "VOLEPS", "Eps_vol");

                    unsigned int partId = p_PartBeingConverted.GetId(p_radiossModel);
                    // get elem part type (shell/solid)

                    int nbShell =0;
                    int nbSolid =0;

                    SelectionElementRead elemShellSelect(p_lsdynaModel, "*ELEMENT_SHELL");
                    nbShell = elemShellSelect.Count();

                    SelectionElementRead elemSolidSelect(p_lsdynaModel, "*ELEMENT_SOLID");
                    nbSolid = elemSolidSelect.Count();

                    // get NIP (nb of integration points through thickness) from *SECTION_SHELL
                    int Ishellsolid = 0;
                    HandleRead partHRead;
                    p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), partId, partHRead);

                    if (partHRead.IsValid())
                    {
                        HandleRead propHRead;
                        partHRead.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHRead);

                        EntityRead propEntityRead(p_lsdynaModel, propHRead);
                        int lsdelform;
                        tempValue = sdiValue(lsdelform);
                        propEntityRead.GetValue(sdiIdentifier("ELFORM"), tempValue);
                        tempValue.GetValue(lsdelform);

                        int lsdNIP = 0;
                        tempValue = sdiValue(lsdNIP);
                        propEntityRead.GetValue(sdiIdentifier("LSD_NIP"), tempValue);
                        tempValue.GetValue(lsdNIP);

                        if(nbShell > 0)
                        {
                            //Full-Integrated linear shell element: (elfrom = 9, 16, -16,17, 18, 20, 21, 22, 26, 30)
                            if(lsdelform == 9  || lsdelform == 16 || lsdelform == -16 || 
                               lsdelform == 17 || lsdelform == 18 || lsdelform ==  20 || 
                               lsdelform == 21 || lsdelform == 22  || lsdelform == 26 || 
                               lsdelform == 30)
                            {
                                if(lsdNUMFIP > 0)
                                {
                                    double lsdGauss = 4.0; // inplane shell integration points
                                    double radVOLFRAC;
                                    radVOLFRAC = abs(lsdNUMFIP)/(lsdGauss*lsdNIP);
                                    failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Volfrac"),sdiValue(radVOLFRAC));
                                }
                                else if(lsdNUMFIP < 0 && lsdNUMFIP >= -100)
                                {
                                    radPthickfail = abs(lsdNUMFIP)/100.0;
                                    failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Pthickfail"),sdiValue(radPthickfail));
                                }
                                else if(lsdNUMFIP < -100)
                                {
                                    radPthickfail = (abs(lsdNUMFIP)-100.0)/100.0;
                                    failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Pthickfail"),sdiValue(radPthickfail));
                                } // if(lsdNUMFIP > 0)
                            }
                            else if(lsdelform == 23  || lsdelform == 24)
                            {
                                // higher order shell element:
                                p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "VOLFRAC", "Volfrac");
                            }
                            else
                            {
                                // Reduced-Integrated linear shell element
                                if(lsdNUMFIP > 0)
                                {
                                    n_coef = lsdNUMFIP;
                                }
                                else if(lsdNUMFIP < 0 && lsdNUMFIP >= -100)
                                {
                                    n_coef = ceil((abs(lsdNUMFIP)/100.0)*lsdNIP);
                                }
                                else if(lsdNUMFIP < -100)
                                {
                                    n_coef = ceil((abs(lsdNUMFIP-100.0)/100.0)*lsdNIP);
                                } // if(lsdNUMFIP > 0)
 
                                if (lsdNIP > 1) radPthickfail = (2.0*n_coef-1)*NipWeights[lsdNIP];
                                failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Pthickfail"),sdiValue(radPthickfail));
                            }
                        } // if(nbShell > 0)

                        if(nbSolid > 0)
                        {
                            if(lsdelform == 2 || lsdelform == 3)
                            {
                                //Full-Integrated linear solid element: (elfrom = 2, 3)
                                double radVOLFRAC;
                                lsdNIP = 8;
                                radVOLFRAC = abs(lsdNUMFIP)/lsdNIP;
                                failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Volfrac"),sdiValue(radVOLFRAC));
                            }
                            else if(lsdelform == 24 || lsdelform == 25 || lsdelform == 26 || 
                                    lsdelform == 27 || lsdelform == 28 || lsdelform == 29)
                            {
                                // higher order solid element:
                                p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "VOLFRAC", "Volfrac");
                            }
                            else
                            {
                                // Reduced-Integrated linear solid element: (elform =1, 4, 0, 9, 10, 12, 13)
                                failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Volfrac"),sdiValue(0.5));
                            }
                        } // if(nbSolid > 0)
                    } // if (partHRead.IsValid())

                    //p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "NCS", "NCS");
                    p_ConvertUtils.SetExpressionValue(*selMatAddErosion, failGene1Edit, "(NCS-1)", "NCS");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "MNPRES", "Pmin");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "SIGP1", "SigP1_max");

                    bool optSIGVMcurve;
                    tempValue = sdiValue(optSIGVMcurve);
                    selMatAddErosion->GetValue(sdiIdentifier("OPT_SIGVM_curve"), tempValue);
                    tempValue.GetValue(optSIGVMcurve);

                    if (optSIGVMcurve  == false)
                    {
                        p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "SIGVM", "Sig_max");
                    }
                    else
                    {
                        // LSD_SIGVM_CURVE --> function fct_Idps
                        HandleRead crvSIGVMHandle;
                        selMatAddErosion->GetEntityHandle(sdiIdentifier("SIGVM_CURVE"), crvSIGVMHandle);

                        if (crvSIGVMHandle.IsValid())
                        {
                            EntityRead crvSIGVMEntRead(p_lsdynaModel, crvSIGVMHandle);
                            EntityId crvSIGVMId = crvSIGVMEntRead.GetId();
                            //EntityId crvSIGVMId = crvSIGVMHandle.GetId(p_lsdynaModel);
                            failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDsm"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvSIGVMId)));
                        }
                    } // if (optSIGVMcurve  == false)

                    bool optMXEPScurve;
                    tempValue = sdiValue(optMXEPScurve);
                    selMatAddErosion->GetValue(sdiIdentifier("OPT_MXEPS_curve"), tempValue);
                    tempValue.GetValue(optMXEPScurve);

                    if (optMXEPScurve  == false)
                    {
                        p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "MXEPS", "Eps_max");
                    }
                    else
                    {
                        // LSD_MXEPS_CURVE --> function fct_Idps
                        HandleRead crvMXEPSHandle;
                        selMatAddErosion->GetEntityHandle(sdiIdentifier("MXEPS_CURVE"), crvMXEPSHandle);
                        //selMatAddErosion->GetEntityHandle(sdiIdentifier("LSD_MXEPS_CURVE"), crvMXEPSHandle);

                        if (crvMXEPSHandle.IsValid())
                        {
                            EntityRead crvMXEPSEntRead(p_lsdynaModel, crvMXEPSHandle);
                            EntityId crvMXEPSId = crvMXEPSEntRead.GetId();
                            //EntityId crvMXEPSId = crvMXEPSHandle.GetId(p_lsdynaModel);
                            failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDps"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvMXEPSId)));
                        }
                    } // if (optMXEPScurve  == false)

                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "EPSSH", "Eps_s");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "SIGTH", "Sigr");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "IMPULSE", "K");
                    if(lsdFAILTM >0) p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "FAILTM", "Time_max");

                    HandleRead crvLCREGDHandle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCREGD"), crvLCREGDHandle);
                    if (crvLCREGDHandle.IsValid())
                    {
                        EntityId crvLCREGId = crvLCREGDHandle.GetId(p_lsdynaModel);
                        failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDel"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCREGId)));
                    }

                    
                    HandleRead crvLCFLDHandle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCFLD"), crvLCFLDHandle);

                    if (crvLCFLDHandle.IsValid())
                    {
                        EntityRead crvLCFLDEntRead(p_lsdynaModel, crvLCFLDHandle);
                        sdiString crvLCFLDType = crvLCFLDEntRead.GetKeyword();
                        EntityId crvLCFLDId = crvLCFLDEntRead.GetId();
                        EntityEdit failGene1Edit(p_radiossModel, failGene1HEdit);
                        if(!crvLCFLDType.compare(0, 13, "*DEFINE_TABLE"))
                        {
                            failGene1Edit.SetValue(sdiIdentifier("tab_IDfld"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), crvLCFLDId)));
                            failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Itab"),sdiValue(1));
                            failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Istrain"),sdiValue(1));
                            failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("Ismooth"),sdiValue(1));
                        }
                    } // if (crvLCFLDHandle.IsValid())

                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "NSFF", "Nstep");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "EPSTHIN", "Thinning");

                    HandleRead crvLCEPS12Handle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCEPS12"), crvLCEPS12Handle);
                    if (crvLCEPS12Handle.IsValid())
                    {
                        EntityId crvLCEPS12Id = crvLCEPS12Handle.GetId(p_lsdynaModel);
                        failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDg12"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCEPS12Id)));
                    }

                    HandleRead crvLCEPS13Handle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCEPS13"), crvLCEPS13Handle);
                    if (crvLCEPS13Handle.IsValid())
                    {
                        EntityId crvLCEPS13Id = crvLCEPS13Handle.GetId(p_lsdynaModel);
                        failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDg13"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCEPS13Id)));
                    }

                    HandleRead crvLCEPSMXHandle;
                    selMatAddErosion->GetEntityHandle(sdiIdentifier("LCEPSMX"), crvLCEPSMXHandle);
                    if (crvLCEPSMXHandle.IsValid())
                    {
                        EntityId crvLCEPSMXId = crvLCEPSMXHandle.GetId(p_lsdynaModel);
                        failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("fct_IDe1c"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCEPSMXId)));
                    }

                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "MXTMP", "Temp_max");
                    p_ConvertUtils.CopyValue(*selMatAddErosion, failGene1Edit, "DTMIN", "dtmin");

                }  // if (!failGene1HEdit.IsValid())
//---------------------
//---------------------
                if (failGene1HEdit.IsValid())
                {
                    failGene1HEdit.SetValue(p_radiossModel, sdiIdentifier("mat_id"), sdiValue(sdiValueEntity(destEntityType, radMatConvertedHandles[i].GetId(p_radiossModel))));
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(failGene1HEdit, sourcehandleList));
                }
//---------------------
//---------------------
            } // if (lsdIDAM == 0)
        } // for (int i=0; i < radMatConvertedHandles.size(); i=i+1)
    } // while (selMatAddErosion.Next())
}


void ConvertPlasticDispPointsTotalDisp(const double& stiff, sdiDoubleList& crvPoints)
{
    size_t numPoints = crvPoints.size();

    if (crvPoints[0] < 0.0)
    {
        /*curve is in 3rd quadrant */
        size_t halfCurveLen = numPoints / 2;
        if (crvPoints[numPoints / 2] >= 0.0)
        {
            sdiDoubleList tempDblList;
            tempDblList.reserve(halfCurveLen);
            for (size_t k = halfCurveLen; k < numPoints; ++k)
            {
                tempDblList.push_back(crvPoints[k]);
            }
            crvPoints.clear();
            crvPoints = tempDblList;
        }
        else
        {
            /*warning wrong curve input*/
            return;
        }
    }

    numPoints = crvPoints.size();
    for (size_t i = 0; i < numPoints; i += 2)
    {
            crvPoints[i] = crvPoints[i] + crvPoints[i+1] / stiff;
            if (i > 0 && crvPoints[i] < crvPoints[i - 2])
            {
                crvPoints[i] = crvPoints[i - 2] + 0.01;
            }
    }
    if (crvPoints[0] > 0.0 && crvPoints[1] == 0)
    {
        crvPoints[0] = 0.0;
    }
    if (crvPoints[1] > 0.0)
    {
        crvPoints.insert(crvPoints.begin(), 0.0);
        crvPoints.insert(crvPoints.begin(), 0.0);
        numPoints += 2;
    }
    /* if curve is descending add points to avoid extrapolation in negative force*/
    if (crvPoints[numPoints - 1] < crvPoints[numPoints - 3])
    {
        crvPoints.push_back(crvPoints[numPoints - 2] + 0.01);
        crvPoints.push_back(crvPoints[numPoints - 1]);
        numPoints += 2;
    }
    /*mirroring the curve to 3rd quadrant*/
    const sdiDoubleList tempList = crvPoints;
    for (size_t i = 3; i <= numPoints; i += 2)
    {
        crvPoints.insert(crvPoints.begin(), -tempList[i]);
        crvPoints.insert(crvPoints.begin(), -tempList[i -1]);
    }
}

void UpdateMatConvertingLoadCurves( const EntityRead& dynaMat,
                                    ModelViewRead* lsdModel, 
                                    ModelViewEdit* radModel, 
                                    ConvertUtils& convertUtils, 
                                    EntityEdit& radmatEntityEdit,
                                    int& lsdLCAoption)
{
    HandleRead handleLCA;
    HandleRead handleLCB;
    HandleRead handleLCC;
    HandleRead handleLCS;
    HandleRead handleLCAB;
    HandleRead handleLCBC;
    HandleRead handleLCCA;
    HandleRead handleLCSR;
    EntityId lsdLCA = 0;
    EntityId lsdLCB = 0;
    EntityId lsdLCC = 0;
    EntityId lsdLCS = 0;
    EntityId lsdLCAB = 0;
    EntityId lsdLCBC = 0;
    EntityId lsdLCCA = 0;
    EntityId lsdLCSR = 0;
    EntityId dynaMatId = dynaMat.GetId();
    sdiString dynaMatName = dynaMat.GetName();
    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };

    vector<reference_wrapper<HandleRead>> attribCurvVals({ handleLCA,  handleLCB,  handleLCC,  handleLCS,
                                                      handleLCAB, handleLCBC, handleLCCA, handleLCSR });

    if (lsdLCAoption > 0)
    {
        vector<sdiString> curveAttribNames({ "LCB", "LCC", "LCC", "LCS", "LCS", "LCS", "LCS", "LCSR" });
        convertUtils.GetEntityHandles(dynaMat, curveAttribNames, attribCurvVals);
    }
    else
    {
        vector<sdiString> curveAttribNames({ "LCA", "LCB", "LCC", "LCS", "LCAB", "LCBC", "LCCA", "LCSR" });
        convertUtils.GetEntityHandles(dynaMat, curveAttribNames, attribCurvVals);
    }


    map<EntityId, pair<bool,sdiDoubleList>> containCrvIdVsCrvPnts;
    vector<reference_wrapper<EntityId>> curveIdList({lsdLCA, lsdLCB, lsdLCC, lsdLCS, lsdLCAB, lsdLCBC, lsdLCCA, lsdLCSR});

    int tempI = -1;
    for (HandleRead curveHandle : {handleLCA, handleLCB, handleLCC, handleLCS,
        handleLCAB, handleLCBC, handleLCCA, handleLCSR })

//    for (HandleRead curveHandle : {handleLCB, handleLCC, handleLCC, handleLCS,
//        handleLCS, handleLCS, handleLCS, handleLCSR  })

    {
        EntityId& crvId = curveIdList[++tempI];
        if (curveHandle.IsValid())
        {
            EntityRead crvEntRead(lsdModel, curveHandle);
            crvId = crvEntRead.GetId();
            int crvLen = 0;
            sdiValue tempVal(crvLen);
            crvEntRead.GetValue(sdiIdentifier("numberofpoints"), tempVal);
            tempVal.GetValue(crvLen);
            sdiDoubleList curvePnts;
            if (crvLen)
            {
                curvePnts.reserve(2 * crvLen);
                tempVal = sdiValue(curvePnts);
                crvEntRead.GetValue(sdiIdentifier("points"), tempVal);
                tempVal.GetValue(curvePnts);
            }
            containCrvIdVsCrvPnts[crvId] = make_pair(false, curvePnts);
        }
    }
    sdiDoubleList strainRateList;
    sdiDoubleList fScaleList;
    strainRateList.reserve(5);
    fScaleList.reserve(5);
    if (handleLCSR.IsValid())
    {
        sdiDoubleList curvePnts = containCrvIdVsCrvPnts[lsdLCSR].second;
        size_t crvNumPoints = curvePnts.size();
        for (size_t i = 0; i < crvNumPoints; ++i)
        {
            sdiString identifier;
            int index = 0;
            if (i % 2 == 0)
                strainRateList.push_back(curvePnts[i]);
            else
                fScaleList.push_back(curvePnts[i]);
            if (i == 7)
                break;
        }
        if (crvNumPoints > 8)
        {
            if ((dynaMat.GetKeyword().find("MODIFIED") != string::npos))
            {
                strainRateList.push_back(curvePnts[8]);
                fScaleList.push_back(curvePnts[9]);
            }
            else
            {
                strainRateList.push_back(curvePnts[crvNumPoints-2]);
                fScaleList.push_back(curvePnts[crvNumPoints - 1]);
            }
        }

    }
    strainRateList.shrink_to_fit();
    fScaleList.shrink_to_fit();

    if (lsdLCAoption > 0)
    {
        sdiConvert::SDIHandlReadList curveHandleList({ handleLCB, handleLCC, handleLCC, handleLCS,
            handleLCS, handleLCS, handleLCS });

        
        RecomputeCurvesBasedOnFirstAbcissa(lsdModel, dynaMat, convertUtils, curveHandleList, containCrvIdVsCrvPnts);

        /* convert to constant ordinate curves */
        //UpdateShearLoadCurves(lsdModel, convertUtils, containCrvIdVsCrvPnts, handleLCAB, handleLCBC, handleLCCA);

        vector<sdiString> tempStrList({ "11", "22", "33","none", "12", "23", "31" });
        vector<pair<EntityId, HandleEdit>> mapCrvIDVsHandleEdit;
        for (int i = 0; i < 7; i++)
        {
            int queryHandleIndex = i;
            EntityId& crvId = curveIdList[i];
            if (i > 2 && crvId == 0)
            {
                if (curveIdList[3] == 0)
                {
                    crvId = curveIdList[0];
                    queryHandleIndex = 0;
                }
                else
                {
                    crvId = curveIdList[3];
                    queryHandleIndex = 3;
                }
            }
            else if (i < 3 && crvId == 0)
                crvId = curveIdList[0];
            if (i == 3)
                continue;
            if (crvId)
            {
                HandleEdit crveEdit;
                vector<pair<EntityId, HandleEdit>>::iterator itr = find_if(mapCrvIDVsHandleEdit.begin(), mapCrvIDVsHandleEdit.end(),
                    [&crvId](const pair<EntityId, HandleEdit> & tempPair)
                    {
                        return crvId == tempPair.first;
                    }
                );
                if (itr != mapCrvIDVsHandleEdit.end())
                    crveEdit = itr->second;
                else if (containCrvIdVsCrvPnts[crvId].first == true)
                {
                    sdiDoubleList crvPoints = containCrvIdVsCrvPnts[crvId].second;
                    sdiString crvName("Duplicate_" + to_string(crvId) + "_MatL50_" + to_string(dynaMatId) +  + "_funID" + tempStrList[i]);
                    EntityRead crvEntRead(lsdModel, attribCurvVals[queryHandleIndex].get());
                    double lsdSFA;
                    double lsdSFO;
                    double lsdOFFA;
                    double lsdOFFO;
                    vector<reference_wrapper<double>> attVals({ lsdSFA, lsdSFO, lsdOFFA , lsdOFFO });
                    vector<sdiString> attNames({ "SFA", "SFO", "OFFA", "OFFO" });
                    convertUtils.GetAttribValues(crvEntRead, attNames, attVals);

                    lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
                    lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

                    convertUtils.CreateCurve(crvName, (int) crvPoints.size() /2, crvPoints, crveEdit, lsdSFA, lsdSFO, lsdSFA * lsdOFFA, lsdSFO * lsdOFFO);
                    if (crveEdit.IsValid())
                    {
                        mapCrvIDVsHandleEdit.push_back(make_pair(crvId, crveEdit));
                        sdiConvert::Convert::PushToConversionLog(std::make_pair(crveEdit, sourcemat));
                    }
                }
                else
                {
                    radModel->FindById(radModel->GetEntityType("/FUNCT"), crvId, crveEdit);
                    mapCrvIDVsHandleEdit.push_back(make_pair(crvId, crveEdit));
                }
                if (crveEdit.IsValid())
                {
                    if(strainRateList.empty())
                        radmatEntityEdit.SetEntityHandle(sdiIdentifier("funID" + tempStrList[i] + "-" + to_string(1)), crveEdit);
                    else
                    {
                        for (size_t ii = 0; ii < strainRateList.size(); ++ii)
                        {
                            radmatEntityEdit.SetEntityHandle(sdiIdentifier("funID" + tempStrList[i] + "-" + to_string(ii+1)), crveEdit);
                            radmatEntityEdit.SetValue(sdiIdentifier("Eps_rate_" + tempStrList[i] + "-" + to_string(ii+1)), sdiValue(strainRateList[ii]));
                            radmatEntityEdit.SetValue(sdiIdentifier("Fscale_" + tempStrList[i] + "-" + to_string(ii+1)), sdiValue(fScaleList[ii]));
                        }
                    }
                }
            }
        }
    }
    else
    {
        sdiConvert::SDIHandlReadList curveHandleList({ handleLCA, handleLCB, handleLCC, handleLCS,
            handleLCAB, handleLCBC, handleLCCA });

        
        RecomputeCurvesBasedOnFirstAbcissa(lsdModel, dynaMat, convertUtils, curveHandleList, containCrvIdVsCrvPnts);

        /* convert to constant ordinate curves */
        //UpdateShearLoadCurves(lsdModel, convertUtils, containCrvIdVsCrvPnts, handleLCAB, handleLCBC, handleLCCA);

        vector<sdiString> tempStrList({ "11", "22", "33","none", "12", "23", "31" });
        vector<pair<EntityId, HandleEdit>> mapCrvIDVsHandleEdit;
        for (int i = 0; i < 7; i++)
        {
            int queryHandleIndex = i;
            EntityId& crvId = curveIdList[i];
            if (i > 2 && crvId == 0)
            {
                if (curveIdList[3] == 0)
                {
                    crvId = curveIdList[0];
                    queryHandleIndex = 0;
                }
                else
                {
                    crvId = curveIdList[3];
                    queryHandleIndex = 3;
                }
            }
            else if (i < 3 && crvId == 0)
                crvId = curveIdList[0];
            if (i == 3)
                continue;
            if (crvId)
            {
                HandleEdit crveEdit;
                vector<pair<EntityId, HandleEdit>>::iterator itr = find_if(mapCrvIDVsHandleEdit.begin(), mapCrvIDVsHandleEdit.end(),
                    [&crvId](const pair<EntityId, HandleEdit> & tempPair)
                    {
                        return crvId == tempPair.first;
                    }
                );
                if (itr != mapCrvIDVsHandleEdit.end())
                    crveEdit = itr->second;
                else if (containCrvIdVsCrvPnts[crvId].first == true)
                {
                    sdiDoubleList crvPoints = containCrvIdVsCrvPnts[crvId].second;
                    sdiString crvName("Duplicate_" + to_string(crvId) + "_MatL50_" + to_string(dynaMatId) +  + "_funID" + tempStrList[i]);
                    EntityRead crvEntRead(lsdModel, attribCurvVals[queryHandleIndex].get());
                    double lsdSFA;
                    double lsdSFO;
                    double lsdOFFA;
                    double lsdOFFO;
                    vector<reference_wrapper<double>> attVals({ lsdSFA, lsdSFO, lsdOFFA , lsdOFFO });
                    vector<sdiString> attNames({ "SFA", "SFO", "OFFA", "OFFO" });
                    convertUtils.GetAttribValues(crvEntRead, attNames, attVals);

                    lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
                    lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

                    convertUtils.CreateCurve(crvName, (int) crvPoints.size() /2, crvPoints, crveEdit, lsdSFA, lsdSFO, lsdSFA * lsdOFFA, lsdSFO * lsdOFFO);
                    if (crveEdit.IsValid())
                    {
                        mapCrvIDVsHandleEdit.push_back(make_pair(crvId, crveEdit));
                        sdiConvert::Convert::PushToConversionLog(std::make_pair(crveEdit, sourcemat));
                    }
                }
                else
                {
                    radModel->FindById(radModel->GetEntityType("/FUNCT"), crvId, crveEdit);
                    mapCrvIDVsHandleEdit.push_back(make_pair(crvId, crveEdit));
                }
                if (crveEdit.IsValid())
                {
                    if(strainRateList.empty())
                        radmatEntityEdit.SetEntityHandle(sdiIdentifier("funID" + tempStrList[i] + "-" + to_string(1)), crveEdit);
                    else
                    {
                        for (size_t ii = 0; ii < strainRateList.size(); ++ii)
                        {
                            radmatEntityEdit.SetEntityHandle(sdiIdentifier("funID" + tempStrList[i] + "-" + to_string(ii+1)), crveEdit);
                            radmatEntityEdit.SetValue(sdiIdentifier("Eps_rate_" + tempStrList[i] + "-" + to_string(ii+1)), sdiValue(strainRateList[ii]));
                            radmatEntityEdit.SetValue(sdiIdentifier("Fscale_" + tempStrList[i] + "-" + to_string(ii+1)), sdiValue(fScaleList[ii]));
                        }
                    }
                }
            }
        }
    }
}

void RecomputeCurvesBasedOnFirstAbcissa(        ModelViewRead* lsdModel,
                                                const EntityRead dynaMat,
                                                const ConvertUtils& convertUtils,
                                                const vector<HandleRead>& curveHandleList,
                                                map<EntityId, pair<bool, sdiDoubleList>>& containCrvIdVsCrvPnts)
{
    double lsdVF;
    double lsdSIGY;
    double lsdE;

    vector<reference_wrapper<double>> attVals({ lsdVF,  lsdSIGY, lsdE });
    vector<sdiString> atribNames({ "VF", "SIGY", "E" });
    convertUtils.GetAttribValues(dynaMat, atribNames, attVals);
    attVals.clear();
    atribNames.clear();

    for (HandleRead crveRead : curveHandleList)
    {
        if (!crveRead.IsValid())
            continue;
        EntityId crvId = crveRead.GetId(lsdModel);
        pair<bool, sdiDoubleList>& lookupVal = containCrvIdVsCrvPnts[crvId];
        int crvLen = (int)lookupVal.second.size() / 2;
        if (crvLen)
        {
            sdiDoubleList& curvePnts = lookupVal.second;
            if (curvePnts[0] > 0.0)
            {
                lookupVal.first = true;
                vector<pair<double, double>> strainVsStressList;
                strainVsStressList.reserve(crvLen);
                for (size_t i = 0; i < 2 * crvLen; i += 2)
                {
                    strainVsStressList.push_back(make_pair(1 - curvePnts[i], curvePnts[i + 1]));
                }

                sort(strainVsStressList.begin(), strainVsStressList.end(),
                    [](const pair<double, double>& p1, const pair<double, double>& p2)
                    {
                        return p1.first < p2.first;
                    }
                );
                curvePnts.clear();
                for (auto pair : strainVsStressList)
                {
                    curvePnts.push_back(pair.first);
                    curvePnts.push_back(pair.second);
                }
            }
        }
    }
}


void UpdateShearLoadCurves(         ModelViewRead* lsdModel,
                                    const ConvertUtils& convertUtils,
                                    map<EntityId, pair<bool, sdiDoubleList>>& containCrvIdVsCrvPnts,
                                    const HandleRead& handleLCAB,
                                    const HandleRead& handleLCBC,
                                    const HandleRead& handleLCCA)
{
    vector<EntityId> crveIdList;
    for (HandleRead crveRead : { handleLCAB , handleLCBC, handleLCCA })
    {
        if (!crveRead.IsValid())
            continue;
        EntityId crvId = crveRead.GetId(lsdModel);
        pair<bool, sdiDoubleList>& lookupVal = containCrvIdVsCrvPnts[crvId];

        int crvLen = (int) lookupVal.second.size() / 2;
        if (crvLen)
        {
            sdiDoubleList& curvePnts = lookupVal.second;
            for (size_t i = 0; i < 2 * crvLen; i += 2)
            {
                if (curvePnts[i] == 0)
                {
                    double constOrdinate = curvePnts[i + 1];
                    for (size_t ii = 1; ii < 2 * crvLen; ii += 2)
                    {
                        curvePnts[ii] = constOrdinate;
                    }
                    break;
                }
            }
        }
    }
}

void ComputeCurvePtsForMatL240(const double stressMode, const double logModel, const double cohesiveThick, 
                               const double strainRate, const sdiDoubleList & strainList,
                               sdiDoubleList &tempList, sdiDoubleList& pointsList)
{
    double temp;
    
    pointsList.clear();
    pointsList.reserve(2 * strainList.size());
    if (stressMode > 0)
    {
        for (auto abscissa : strainList)
        {
            pointsList.push_back(abscissa * cohesiveThick);
            pointsList.push_back(1.0);
            tempList.push_back(abs(stressMode));
        }
    }
    else if (stressMode < 0)
    {
        if (logModel > 0)
        {
            for (auto abscissa : strainList)
            {
                pointsList.push_back(abscissa * cohesiveThick);
                temp = abs(stressMode) + abs(logModel) * pow(max(0.0, log(abscissa / strainRate)), 2);
                pointsList.push_back(temp / abs(stressMode));
                tempList.push_back(temp);
            }
        }
        else if (logModel < 0)
        {
            for (auto abscissa : strainList)
            {
                pointsList.push_back(abscissa * cohesiveThick);
                temp = abs(stressMode) + abs(logModel) * pow(max(0.0, log(abscissa / strainRate)), 1);
                pointsList.push_back(temp / abs(stressMode));
                tempList.push_back(temp);
            }
        }
    }
}

void ComputeInitialYldCurveForMatL240(const double energyRelRate, const double upperEnergyRelRate, 
            const double strainRate, const double cohesiveThick, const double fgVal, const double modVal, 
            const sdiDoubleList strainList, const sdiDoubleList &tempList, sdiDoubleList &GList, sdiDoubleList &pointsList)
{
    if (GList.empty())
    {
        for (auto abscissa : strainList)
        {
            GList.push_back((energyRelRate > 0) ? abs(energyRelRate) : 
                       (abs(energyRelRate) + (abs(upperEnergyRelRate) - abs(energyRelRate)) * exp(-1 * strainRate / abscissa)));
        }
    }

    pointsList.clear();
    pointsList.reserve(2 * strainList.size());
    for (int i = 0; i < GList.size(); ++i)
    {
        pointsList.push_back(strainList[i] * cohesiveThick);
        pointsList.push_back((fgVal > 0) ? (abs(fgVal) * GList[i] / tempList[i]) :
                   ((abs(fgVal) / (1 + abs(fgVal))) * (2 * GList[i] / tempList[i] - tempList[i] / modVal)));
    }
}

void ComputeRuptureYldCurveForMAtL240(const double cohesiveThick, const double fgVal, const double modVal, 
            const sdiDoubleList &strainList, const sdiDoubleList &GList, const sdiDoubleList &tempList, 
            sdiDoubleList &pointsList )
{
    pointsList.clear();
    pointsList.reserve(2 * strainList.size());
    for (int i = 0; i < strainList.size(); ++i)
    {
        pointsList.push_back(strainList[i] * cohesiveThick);
        pointsList.push_back((fgVal > 0) ? (GList[i] * (2 - abs(fgVal)) / tempList[i] - tempList[i] / modVal) :
            ((2 * GList[i] / tempList[i] - tempList[i] / modVal) / (1 + abs(fgVal))));
    }
}


void ConvertMat::p_ConvertMatL801Seatbelt(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    destCard = "/MAT/LAW114";

    if (matToPropsReferenceCount > 1)
   	p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
   	p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());


    EntityEdit radMatEdit(p_radiossModel, radMat);
 
   double lsdMPUL;
   double lsdRHO;
   double lsdA;
   //=============================
    attribMap = { {"LMIN", "LMIN"}, {"E", "E"}, {"I", "I"},
                  {"J", "J"}, {"AS", "AS"}, {"F", "FMAX"}, {"M", "MMAX"}, {"R", "R"} };


    vector<reference_wrapper<double>> attribVals({ lsdMPUL, lsdA });
    vector<sdiString> attribNames({ "MPUL", "A"});
    p_ConvertUtils.GetAttribValues(dynaMat, attribNames, attribVals);

    if (lsdA != 0.0)
      {
        lsdRHO = lsdMPUL / lsdA;
      }
    else if (lsdA == 0.0)
      {
        lsdRHO = lsdMPUL;
      }

    radMatEdit.SetValue(sdiIdentifier("RHO_I"), sdiValue(lsdRHO));



    //----------
    // functions
    //----------

    sdiValue value;

    //int nFunct = 0;
    sdiUIntList funcIdList;
    HandleRead loadCrvHandle;
    dynaMat.GetEntityHandle(sdiIdentifier("LSD_LCID"), loadCrvHandle);
    if (loadCrvHandle.IsValid())
    {
        EntityRead loadCrvEntRead(p_lsdynaModel, loadCrvHandle);
        sdiString crvType = loadCrvEntRead.GetKeyword();
        EntityId loadcrvId = loadCrvEntRead.GetId();
        if (!crvType.compare(0, 13, "*DEFINE_CURVE"))
        {
            //nFunct = 1;
            radMatEdit.SetValue(sdiIdentifier("FUN_L"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), loadCrvHandle.GetId(p_lsdynaModel))));

        }
        else if (!crvType.compare(0, 13, "*DEFINE_TABLE"))
        {
           
            sdiValueEntityList curveList;
            sdiValue tempValue(curveList);
            loadCrvEntRead.GetValue(sdiIdentifier("CurveIds"), tempValue);
            tempValue.GetValue(curveList);
            curveList.GetIdList(funcIdList);
            //nFunct = (int)funcIdList.size();

            DynaToRad::ShowMessage(sdiMessageHandler::Level::Warning, 27,dynaMat.GetKeyword().c_str(), dynaMatId, dynaMatName.c_str());

            radMatEdit.SetValue(sdiIdentifier("FUN_L"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), funcIdList[0])));
        }
    }




        HandleRead unloadCrvHandle;
    // UNLOAD function
        dynaMat.GetEntityHandle(sdiIdentifier("LSD_LCID2"), unloadCrvHandle);
        if (unloadCrvHandle.IsValid())
        {
            radMatEdit.SetValue(sdiIdentifier("FUN_UL"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), unloadCrvHandle.GetId(p_lsdynaModel))));
        }
    //=============================

    SDIHandlReadList sourceMats = { {dynaMat.GetHandle()} };
    Convert::PushToConversionLog(std::make_pair(radMat, sourceMats));


}

void ConvertMat::p_ConvertMatL63(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    destCard = "/MAT/LAW33";

    if (matToPropsReferenceCount > 1)
   	p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
   	p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());


    EntityEdit radMatEdit(p_radiossModel, radMat);

    attribMap = { {"RHO", "MAT_RHO"}, {"E", "MAT_E"} ,{"TSC", "MAT_SIGT_CUTOFF"} };
    radMatEdit.SetValue(sdiIdentifier("Ka"), sdiValue(2));

    HandleRead lcidHandle;
    dynaMat.GetEntityHandle(sdiIdentifier("LSD_LCID"), lcidHandle);
    HandleEdit functEdit;

    if (lcidHandle.IsValid())
    {
        EntityRead loadCrvEntRead(p_lsdynaModel, lcidHandle);
        EntityId loadcrvId = loadCrvEntRead.GetId();

        int nPoints = 0;
        sdiValue tempValue;
        tempValue = sdiValue(nPoints);
        lcidHandle.GetValue(p_lsdynaModel, sdiIdentifier("numberofpoints"), tempValue);
        tempValue.GetValue(nPoints);

        if (nPoints)
        {
            sdiDoubleList radInvertFunc;
            sdiDoubleList crvPoints;
            tempValue = sdiValue(crvPoints);
            lcidHandle.GetValue(p_lsdynaModel, sdiIdentifier("points"), tempValue);
            tempValue.GetValue(crvPoints);

            // temporary storage for inversion:

            sdiDoubleList crvPoints_tmp;
            tempValue = sdiValue(crvPoints_tmp);
            crvPoints_tmp.reserve(2 * nPoints);

            // abscisa inverted and resorted (* -1.0)

            for (int i = 0; i < 2 * nPoints; i += 2)
            {
               int j=2*(nPoints-1)-i;
               crvPoints_tmp[i]   = - crvPoints[j];
               crvPoints_tmp[i+1] =   crvPoints[j+1];
            }

            // refill crvPoints after inversion:

            for (int i = 0; i < 2 * nPoints; ++i)
            {
               crvPoints[i] = crvPoints_tmp[i];
            }

            p_ConvertUtils.CreateCurve(dynaMat.GetName(), (int)nPoints, crvPoints, functEdit);
            radMatEdit.SetValue(sdiIdentifier("FUN_A1"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), functEdit.GetId(p_radiossModel))));


        }


        sdiConvert::SDIHandlReadList sourceMats = { {dynaMat.GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(radMat, sourceMats));


        if (functEdit.IsValid())
          sdiConvert::Convert::PushToConversionLog(std::make_pair(functEdit, sourceMats));

    }

}
void ConvertMat::p_ConvertMatL77H(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    sdiValue tempValue;
    int lsdN = -1;
    dynaMat.GetValue(sdiIdentifier("N"), tempValue);
    tempValue.GetValue(lsdN);

    if (lsdN==0)
    {
        destCard = "/MAT/LAW95";
        if (matToPropsReferenceCount > 1)
            p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
        else
            p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
        attribMap = { {"Rho", "Rho_I"}, {"C10", "C10"}, {"C01", "C01"}, {"C11", "C11"}, {"C20", "C20"}, {"C02", "C02"}, {"C30", "C30"} };
        EntityEdit radmatEntityEdit(p_radiossModel, radMat);

        double lsdPR;
        dynaMat.GetValue(sdiIdentifier("PR"), tempValue);
        tempValue.GetValue(lsdPR);

        if (lsdPR < 0)
        {
            //To Do: Add warning message "the Mullins effect is not take into account"
            DynaToRad::ShowMessage(sdiMessageHandler::Level::Warning, 28,dynaMat.GetKeyword().c_str(), dynaMatId, dynaMatName.c_str());
        }
        else
        {
          // If PR>=0 then Calculate G=2*(C01+C10), K=2G(1+PR)/3/(1-2PR), D1=2/K, use D1 in LAW95 definition
          double G;
          double K;
          double D1;
          double lsdC01;
          double lsdC10;
          dynaMat.GetValue(sdiIdentifier("C01"), tempValue);
          tempValue.GetValue(lsdC01);
          dynaMat.GetValue(sdiIdentifier("C10"), tempValue);
          tempValue.GetValue(lsdC10);
          G = 2*(lsdC01+lsdC10);
          K = 2*G*(1+lsdPR)/3/(1-2*lsdPR);
          D1 = 2/K;
          radmatEntityEdit.SetValue(sdiIdentifier("D1"), sdiValue(abs(D1)));
        }

        //The rest of parameters are set to default 0 values in converson
        radmatEntityEdit.SetValue(sdiIdentifier("C21"), sdiValue(0.0));
        radmatEntityEdit.SetValue(sdiIdentifier("C12"), sdiValue(0.0));
        radmatEntityEdit.SetValue(sdiIdentifier("C03"), sdiValue(0.0));
        radmatEntityEdit.SetValue(sdiIdentifier("Sb"), sdiValue(0.0));
        radmatEntityEdit.SetValue(sdiIdentifier("D2"), sdiValue(0.0));
        radmatEntityEdit.SetValue(sdiIdentifier("D3"), sdiValue(0.0));
        radmatEntityEdit.SetValue(sdiIdentifier("A"), sdiValue(0.0));
        radmatEntityEdit.SetValue(sdiIdentifier("C"), sdiValue(0.0));
        radmatEntityEdit.SetValue(sdiIdentifier("M"), sdiValue(0.0));
        radmatEntityEdit.SetValue(sdiIdentifier("KSI"), sdiValue(0.0));
        radmatEntityEdit.SetValue(sdiIdentifier("Tau_ref"), sdiValue(0.0));
    }
    else if(lsdN > 0)
    {
        destCard = "/MAT/LAW69";
        if (matToPropsReferenceCount > 1)
            p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
        else
            p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
        EntityEdit radmatEntityEdit(p_radiossModel, radMat);
        attribMap = { {"Rho", "RHO_I"} , {"N","N_PAIR"} };

        double lsdPR;
        double lsdSGL;
        double lsdSW;
        double lsdST;
        double lsdDATA;
        EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");
        vector<reference_wrapper<double>> attrValList = { lsdPR, lsdSGL, lsdSW, lsdST, lsdDATA};
        vector<sdiString> attrNameList = { "PR", "SGL","SW", "ST", "DATA" };
        p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

        radmatEntityEdit.SetValue(sdiIdentifier("NU"), sdiValue(abs(lsdPR)));
        if (lsdPR < 0)
        {
            //To Do: Add warning message: the Mullins effect is not take into account
            DynaToRad::ShowMessage(sdiMessageHandler::Level::Warning, 28,dynaMat.GetKeyword().c_str(), dynaMatId, dynaMatName.c_str());
        }
        radmatEntityEdit.SetValue(sdiIdentifier("LAW_ID"), sdiValue((int)lsdDATA));

        HandleRead curveLCID1;
        dynaMat.GetEntityHandle(sdiIdentifier("LCID1"), curveLCID1);
        if (curveLCID1.IsValid())
        {
            EntityRead crvEntRead(p_lsdynaModel, curveLCID1);
            double lsdSFA=1.0;
            double lsdSFO=1.0;
            double lsdOFFA=0.0;
            double lsdOFFO=0.0;

            lsdSGL = (lsdSGL == 0.0) ? 1.0 : lsdSGL;
            lsdSW = (lsdSW == 0.0) ? 1.0 : lsdSW;

            lsdSFA = 1 / lsdSGL; // scaling coefficient for abscissa 
            lsdSFO = 1 / (lsdSW * lsdST); // scaling coefficient for ordinate
            
            if (lsdSFA != 1.0 || lsdSFO != 1.0)
            {
                sdiString curveName = crvEntRead.GetName();
                int crvLen = 0;
                sdiValue tempVal(crvLen);
                crvEntRead.GetValue(sdiIdentifier("numberofpoints"), tempVal);
                tempVal.GetValue(crvLen);

                if (crvLen)
                {
                    sdiDoubleList curvePnts;
                    curvePnts.reserve(2 * crvLen);
                    tempVal = sdiValue(curvePnts);
                    crvEntRead.GetValue(sdiIdentifier("points"), tempVal);
                    tempVal.GetValue(curvePnts);
                    double lsdSFAOriginal = 1.0;
                    double lsdSFOOriginal = 1.0;
                    vector< reference_wrapper<double>> attribVals({ lsdSFAOriginal,lsdSFOOriginal, lsdOFFA, lsdOFFO });
                    vector<sdiString> lsdQueryAttribs = {"SFA", "SFO", "OFFA", "OFFO" };
                    p_ConvertUtils.GetAttribValues(crvEntRead, lsdQueryAttribs, attribVals);

                    lsdSFAOriginal = (lsdSFAOriginal == 0.0) ? 1.0 : lsdSFAOriginal;
                    lsdSFOOriginal = (lsdSFOOriginal == 0.0) ? 1.0 : lsdSFOOriginal;

                    HandleEdit duplicateCurve;
                    p_ConvertUtils.CreateCurve(curveName + "_Duplicate", (int)curvePnts.size() / 2, { curvePnts }, duplicateCurve, lsdSFAOriginal * lsdSFA, lsdSFOOriginal * lsdSFO, lsdSFAOriginal * lsdOFFA, lsdSFOOriginal * lsdOFFO);
                    radmatEntityEdit.SetEntityHandle(sdiIdentifier("FCT_ID1"), duplicateCurve);
                    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(duplicateCurve, sourcemat));
                }
            }
            else
            {
                radmatEntityEdit.SetValue(sdiIdentifier("FCT_ID1"), sdiValue(sdiValueEntity(radFunctType, crvEntRead.GetId()))); // when SFA and SFO are equal to 1 the curve is not duplicate
            }
        }
        radmatEntityEdit.SetValue(sdiIdentifier("FSCALE"), sdiValue(0.0));
        radmatEntityEdit.SetValue(sdiIdentifier("ICHECK"), sdiValue(0));
    }
    //-----------------------------------
    // check for writing /VISC/PRONY card
    //-----------------------------------
    sdiDoubleList lsdGi;
    sdiDoubleList lsdBeta;

    sdiValue tempVal;

    tempVal = sdiValue(lsdGi);
    dynaMat.GetValue(sdiIdentifier("Gi"), tempVal);
    tempVal.GetValue(lsdGi);

    lsdBeta.clear();
    tempVal = sdiValue(lsdBeta);
    dynaMat.GetValue(sdiIdentifier("BETAi"), tempVal);
    tempVal.GetValue(lsdBeta);

    sdiString radMatName = radMat.GetName(p_radiossModel);
    unsigned int radMatId = radMat.GetId(p_radiossModel);

    if (lsdGi.size() > 0)
    {
        HandleEdit ViscPronyHEdit;
        p_radiossModel->CreateEntity(ViscPronyHEdit, "/VISC/PRONY", radMatName, radMatId);

        if (ViscPronyHEdit.IsValid())
        {
            EntityEdit ViscPronyEdit(p_radiossModel, ViscPronyHEdit);

            ViscPronyEdit.SetValue(sdiIdentifier("M"), sdiValue((int)lsdGi.size()));
            ViscPronyEdit.SetValue(sdiIdentifier("K_v"), sdiValue(0.0));
            ViscPronyEdit.SetValue(sdiIdentifier("G_i"), sdiValue(lsdGi));
            ViscPronyEdit.SetValue(sdiIdentifier("Beta_i"), sdiValue(lsdBeta));
            ViscPronyEdit.SetValue(sdiIdentifier("Ki"), sdiValue(sdiDoubleList(lsdGi.size(), 0.0)));
            ViscPronyEdit.SetValue(sdiIdentifier("Beta_ki"), sdiValue(sdiDoubleList(lsdGi.size(), 0.0)));

            sdiConvert::SDIHandlReadList sourceList = { {dynaMat.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(ViscPronyHEdit, sourceList));
        }
    }
    //-----------------------------------
}
void ConvertMat::p_ConvertMatL177H(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();

    HandleRead lcidHandle;
    dynaMat.GetEntityHandle(sdiIdentifier("LSD_LCID"), lcidHandle);
    unsigned int lcId = lcidHandle.GetId(p_lsdynaModel);

    if (lcId == 0)
    {
      destCard = "/MAT/LAW62";

      if (matToPropsReferenceCount > 1)
          p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
      else
          p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());
      EntityEdit radMatEdit(p_radiossModel, radMat);

      attribMap = { {"Rho", "RHO_I"} };

      sdiValue tempValue;
      double lsdRHO;
      tempValue = sdiValue(lsdRHO);
      dynaMat.GetValue(sdiIdentifier("Rho"), tempValue);
      tempValue.GetValue(lsdRHO);

      double lsdN;
      tempValue = sdiValue(lsdN);
      dynaMat.GetValue(sdiIdentifier("N"), tempValue);
      tempValue.GetValue(lsdN);

      double radNU;
      radNU = lsdN/(1.0+2.0*lsdN);
      radMatEdit.SetValue(sdiIdentifier("Nu"),sdiValue(radNU));

      sdiDoubleList lsdCI;
      sdiDoubleList lsdBI;

      double lsdB1;
      tempValue = sdiValue(lsdB1);
      dynaMat.GetValue(sdiIdentifier("B1"), tempValue);
      tempValue.GetValue(lsdB1);
      if(lsdB1 != 0.0) lsdBI.push_back(lsdB1);

      double lsdB2;
      tempValue = sdiValue(lsdB2);
      dynaMat.GetValue(sdiIdentifier("B2"), tempValue);
      tempValue.GetValue(lsdB2);
      if(lsdB2 != 0.0) lsdBI.push_back(lsdB2);

      double lsdB3;
      tempValue = sdiValue(lsdB3);
      dynaMat.GetValue(sdiIdentifier("B3"), tempValue);
      tempValue.GetValue(lsdB3);
      if(lsdB3 != 0.0) lsdBI.push_back(lsdB3);

      double lsdB4;
      tempValue = sdiValue(lsdB4);
      dynaMat.GetValue(sdiIdentifier("B4"), tempValue);
      tempValue.GetValue(lsdB4);
      if(lsdB4 != 0.0) lsdBI.push_back(lsdB4);

      double lsdB5;
      tempValue = sdiValue(lsdB5);
      dynaMat.GetValue(sdiIdentifier("B5"), tempValue);
      tempValue.GetValue(lsdB5);
      if(lsdB5 != 0.0) lsdBI.push_back(lsdB5);

      double lsdB6;
      tempValue = sdiValue(lsdB6);
      dynaMat.GetValue(sdiIdentifier("B6"), tempValue);
      tempValue.GetValue(lsdB6);
      if(lsdB6 != 0.0) lsdBI.push_back(lsdB6);

      double lsdB7;
      tempValue = sdiValue(lsdB7);
      dynaMat.GetValue(sdiIdentifier("B7"), tempValue);
      tempValue.GetValue(lsdB7);
      if(lsdB7 != 0.0) lsdBI.push_back(lsdB7);

      double lsdB8;
      tempValue = sdiValue(lsdB8);
      dynaMat.GetValue(sdiIdentifier("B8"), tempValue);
      tempValue.GetValue(lsdB8);
      if(lsdB8 != 0.0) lsdBI.push_back(lsdB8);

      double lsdC1;
      tempValue = sdiValue(lsdC1);
      dynaMat.GetValue(sdiIdentifier("C1"), tempValue);
      tempValue.GetValue(lsdC1);
      if(lsdC1 != 0.0) lsdCI.push_back(lsdC1);

      double lsdC2;
      tempValue = sdiValue(lsdC2);
      dynaMat.GetValue(sdiIdentifier("C2"), tempValue);
      tempValue.GetValue(lsdC2);
      if(lsdC2 != 0.0) lsdCI.push_back(lsdC2);

      double lsdC3;
      tempValue = sdiValue(lsdC3);
      dynaMat.GetValue(sdiIdentifier("C3"), tempValue);
      tempValue.GetValue(lsdC3);
      if(lsdC3 != 0.0) lsdCI.push_back(lsdC3);

      double lsdC4;
      tempValue = sdiValue(lsdC4);
      dynaMat.GetValue(sdiIdentifier("C4"), tempValue);
      tempValue.GetValue(lsdC4);
      if(lsdC4 != 0.0) lsdCI.push_back(lsdC4);

      double lsdC5;
      tempValue = sdiValue(lsdC5);
      dynaMat.GetValue(sdiIdentifier("C5"), tempValue);
      tempValue.GetValue(lsdC5);
      if(lsdC5 != 0.0) lsdCI.push_back(lsdC5);

      double lsdC6;
      tempValue = sdiValue(lsdC6);
      dynaMat.GetValue(sdiIdentifier("C6"), tempValue);
      tempValue.GetValue(lsdC6);
      if(lsdC6 != 0.0) lsdCI.push_back(lsdC6);

      double lsdC7;
      tempValue = sdiValue(lsdC7);
      dynaMat.GetValue(sdiIdentifier("C7"), tempValue);
      tempValue.GetValue(lsdC7);
      if(lsdC7 != 0.0) lsdCI.push_back(lsdC7);

      double lsdC8;
      tempValue = sdiValue(lsdC8);
      dynaMat.GetValue(sdiIdentifier("C8"), tempValue);
      tempValue.GetValue(lsdC8);
      if(lsdC8 != 0.0) lsdCI.push_back(lsdC8);

      sdiDoubleList radMuI, radAlphaI;
      radMuI.reserve(lsdCI.size());
      radAlphaI.reserve(lsdCI.size());
      for (int i = 0; i < lsdCI.size(); i++)
      {
        radMuI.push_back(lsdCI[i]*lsdBI[i]/2.0);
        radAlphaI.push_back(lsdBI[i]);
      }
      radMatEdit.SetValue(sdiIdentifier("N"), sdiValue((int)lsdCI.size()));
      radMatEdit.SetValue(sdiIdentifier("mu_i"), sdiValue(radMuI));
      radMatEdit.SetValue(sdiIdentifier("alpha_i"), sdiValue(radAlphaI));

      // Default values
      radMatEdit.SetValue(sdiIdentifier("Flag_Visc"), sdiValue(0));
      radMatEdit.SetValue(sdiIdentifier("mu_max"), sdiValue(0.0));
      radMatEdit.SetValue(sdiIdentifier("M"), sdiValue(0));

      sdiConvert::SDIHandlReadList sourceMats = { {dynaMat.GetHandle()} };
      sdiConvert::Convert::PushToConversionLog(std::make_pair(radMat, sourceMats));
    } // if (lcId == 0)
}
void ConvertMat::p_ConvertMatL124(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();

    destCard = "/MAT/LAW66";
    if (matToPropsReferenceCount > 1)
   	p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
   	p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());

    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");

    double lsdPCUTC;
    double lsdPCUTT;
    double lsdPCUTF;
    vector<reference_wrapper<double>> attrValList = {lsdPCUTC,lsdPCUTT, lsdPCUTF};
    vector<sdiString> attrNameList = {"PCUTC","PCUTT", "PCUTF"};
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "RHO", "RHO_I");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "E", "E");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "PR", "Nu");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "PR", "Nu");
    radmatEntityEdit.SetValue(sdiIdentifier("Iyld_rate"), sdiValue(1));
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "P", "c");

    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "EC", "EC");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "RPCT", "RPCT");


    sdiValue tempValue;

    double lsdSRFLAG;
    dynaMat.GetValue(sdiIdentifier("SRFLAG"), tempValue);
    tempValue.GetValue(lsdSRFLAG);
    if(lsdSRFLAG == 2)
      radmatEntityEdit.SetValue(sdiIdentifier("VP"), sdiValue(1));
    else
      radmatEntityEdit.SetValue(sdiIdentifier("VP"), sdiValue(0));


    HandleRead curveLCIDC;
    dynaMat.GetEntityHandle(sdiIdentifier("LCIDC"), curveLCIDC);
    if (curveLCIDC.IsValid())
    {
    	EntityRead crvEntRead(p_lsdynaModel, curveLCIDC);
        radmatEntityEdit.SetValue(sdiIdentifier("funct_IDc"), sdiValue(sdiValueEntity(radFunctType, crvEntRead.GetId())));
        radmatEntityEdit.SetValue(sdiIdentifier("Fscalec"), sdiValue(0.0));
    }

    HandleRead curveLCIDT;
    dynaMat.GetEntityHandle(sdiIdentifier("LCIDT"), curveLCIDT);
    if (curveLCIDT.IsValid())
    {
    	EntityRead crvEntRead(p_lsdynaModel, curveLCIDT);
        radmatEntityEdit.SetValue(sdiIdentifier("funct_IDt"), sdiValue(sdiValueEntity(radFunctType, crvEntRead.GetId())));
        radmatEntityEdit.SetValue(sdiIdentifier("Fscalet"), sdiValue(0.0));
    }

    HandleRead curveLCSRC;
    dynaMat.GetEntityHandle(sdiIdentifier("LCSRC"), curveLCSRC);
    if (curveLCSRC.IsValid())
    {
    	EntityRead crvEntRead(p_lsdynaModel, curveLCSRC);
        radmatEntityEdit.SetValue(sdiIdentifier("fnYrt_IDc"), sdiValue(sdiValueEntity(radFunctType, crvEntRead.GetId())));
        radmatEntityEdit.SetValue(sdiIdentifier("Yrate_Fscalec"), sdiValue(0.0));
        radmatEntityEdit.SetValue(sdiIdentifier("Iyld_rate"), sdiValue(3));
    }

    HandleRead curveLCSRT;
    dynaMat.GetEntityHandle(sdiIdentifier("LCSRT"), curveLCSRT);
    if (curveLCSRT.IsValid())
    {
    	EntityRead crvEntRead(p_lsdynaModel, curveLCSRT);
        radmatEntityEdit.SetValue(sdiIdentifier("fnYrt_IDt"), sdiValue(sdiValueEntity(radFunctType, crvEntRead.GetId())));
        radmatEntityEdit.SetValue(sdiIdentifier("Yrate_Fscalet"), sdiValue(0.0));
        radmatEntityEdit.SetValue(sdiIdentifier("Iyld_rate"), sdiValue(3));
    }

    //-----------------------------------
    // check for writing /VISC/PRONY card
    //-----------------------------------
    int lsdArrayCount = 0;
    tempValue = sdiValue(lsdArrayCount);
    dynaMat.GetValue(sdiIdentifier("ArrayCount"), tempValue);
    tempValue.GetValue(lsdArrayCount);

    double lsdK;
    tempValue = sdiValue(lsdK);
    dynaMat.GetValue(sdiIdentifier("K"), tempValue);
    tempValue.GetValue(lsdK);

    double addG=0.0;
    double addBeta=0.0;
    sdiDoubleList lsdGi;
    sdiDoubleList lsdBetai;

    tempValue = sdiValue(lsdGi);
    dynaMat.GetValue(sdiIdentifier("GI"), tempValue);
    tempValue.GetValue(lsdGi);

    lsdBetai.clear();
    tempValue = sdiValue(lsdBetai);
    dynaMat.GetValue(sdiIdentifier("BETAI"), tempValue);
    tempValue.GetValue(lsdBetai);

    sdiString radMatName = radMat.GetName(p_radiossModel);
    unsigned int radMatId = radMat.GetId(p_radiossModel);

    if (lsdK > 0.0 && lsdArrayCount > 0)
    {
        HandleEdit ViscPronyHEdit;
        p_radiossModel->CreateEntity(ViscPronyHEdit, "/VISC/PRONY", radMatName, radMatId);

        if (ViscPronyHEdit.IsValid())
        {
            EntityEdit ViscPronyEdit(p_radiossModel, ViscPronyHEdit);

            ViscPronyEdit.SetValue(sdiIdentifier("M"), sdiValue((int)lsdGi.size()));
            p_ConvertUtils.CopyValue(dynaMat, ViscPronyEdit, "K", "K_v");
            p_ConvertUtils.CopyValue(dynaMat, ViscPronyEdit, "GI", "G_i");
            p_ConvertUtils.CopyValue(dynaMat, ViscPronyEdit, "BETAI", "Beta_i");
            ViscPronyEdit.SetValue(sdiIdentifier("Ki"), sdiValue(sdiDoubleList(lsdGi.size(), 0.0)));
            ViscPronyEdit.SetValue(sdiIdentifier("Beta_ki"), sdiValue(sdiDoubleList(lsdGi.size(), 0.0)));

            sdiConvert::SDIHandlReadList sourceList = { {dynaMat.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(ViscPronyHEdit, sourceList));
        }
    }

    double lsdFAIL = 0.0;
    dynaMat.GetValue(sdiIdentifier("FAIL"), tempValue);
    tempValue.GetValue(lsdFAIL);

    sdiValueEntity lsdLcfailEntity;
    tempValue = sdiValue(lsdLcfailEntity);
    dynaMat.GetValue(sdiIdentifier("LCFAIL"), tempValue);
    tempValue.GetValue(lsdLcfailEntity);
    unsigned int lcfailId = lsdLcfailEntity.GetId();

    if(lsdFAIL > 0 && lcfailId == 0)
    {

        HandleEdit failJohnsonHEdit;
        p_radiossModel->CreateEntity(failJohnsonHEdit, "/FAIL/JOHNSON", dynaMatName, radMatId);
        failJohnsonHEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
        if (failJohnsonHEdit.IsValid())
        {

            failJohnsonHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("mat_id"), radMat);
            EntityEdit failJohnsonEntEdit(p_radiossModel, failJohnsonHEdit);

            p_ConvertUtils.CopyValue(dynaMat, failJohnsonEntEdit, "FAIL", "D1");
            failJohnsonEntEdit.SetValue(sdiIdentifier("D2"), sdiValue(0.0));
            failJohnsonEntEdit.SetValue(sdiIdentifier("D3"), sdiValue(0.0));
            failJohnsonEntEdit.SetValue(sdiIdentifier("D4"), sdiValue(0.0));
            failJohnsonEntEdit.SetValue(sdiIdentifier("D5"), sdiValue(0.0));
            failJohnsonEntEdit.SetValue(sdiIdentifier("Epsilon_Dot_0"), sdiValue(0.0));
            failJohnsonEntEdit.SetValue(sdiIdentifier("Ifail_sh"), sdiValue(1));
            failJohnsonEntEdit.SetValue(sdiIdentifier("Ifail_so"), sdiValue(1));
            failJohnsonEntEdit.SetValue(sdiIdentifier("Dadv"), sdiValue(0.0));
            failJohnsonEntEdit.SetValue(sdiIdentifier("Ixfem"), sdiValue(0));

            sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(failJohnsonHEdit, sourcemat));
        }
    }
    else if(lcfailId > 0)
    {
      if(lsdK > 0.0)
      {
        for (int i = 0; i < lsdArrayCount; ++i)
        {
          addG = addG + lsdGi[i];
          addBeta = addBeta + lsdBetai[i];
        }
      }
      if(lsdSRFLAG == 2 || curveLCSRC.GetId(p_lsdynaModel) !=0 || curveLCSRT.GetId(p_lsdynaModel) !=0 || (addG !=0.0 && addBeta !=0.0))
      {
          HandleEdit failTensStrainHEdit;
          p_radiossModel->CreateEntity(failTensStrainHEdit, "/FAIL/TENSSTRAIN", dynaMatName, radMatId);
          failTensStrainHEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
          if (failTensStrainHEdit.IsValid())
          {
              failTensStrainHEdit.SetEntityHandle(p_radiossModel, sdiIdentifier("mat_id"), radMat);
              EntityEdit failTensStrainEntEdit(p_radiossModel, failTensStrainHEdit);

              failTensStrainEntEdit.SetValue(sdiIdentifier("EPSILON_T1"), sdiValue(1.0));
              failTensStrainEntEdit.SetValue(sdiIdentifier("EPSILON_T2"), sdiValue(1.1));
              failTensStrainEntEdit.SetValue(sdiIdentifier("FCT_ID"), sdiValue(sdiValueEntity(radFunctType, lcfailId)));

              sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
              sdiConvert::Convert::PushToConversionLog(std::make_pair(failTensStrainHEdit, sourcemat));
          }
      }
    }
    else if(lsdFAIL < 0)
    {
        DynaToRad::ShowMessage(sdiMessageHandler::Level::Warning, 30,dynaMat.GetKeyword().c_str(), dynaMatId, dynaMatName.c_str());
    }

    if(lsdPCUTF == 1.0) p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "PCUTC", "P_c");
    else radmatEntityEdit.SetValue(sdiIdentifier("P_c"), sdiValue(0.0));

    if(lsdPCUTT == 1.0) p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "PCUTT", "P_t");
    else radmatEntityEdit.SetValue(sdiIdentifier("P_t"), sdiValue(0.0));

    radmatEntityEdit.SetValue(sdiIdentifier("F_smooth"), sdiValue(1));
    radmatEntityEdit.SetValue(sdiIdentifier("F_cut"), sdiValue(0.0));
}
void ConvertMat::p_ConvertMatAddDamageDiem()
{

    SelectionRead selMatAddDamageDiem(p_lsdynaModel, "*MAT_ADD_DAMAGE_DIEM");
    while (selMatAddDamageDiem.Next())
    {
        sdiValue tempValue;
        HandleRead matHandle;
        sdiValueEntity lsdMIDEntity;

        tempValue = sdiValue(lsdMIDEntity);
        selMatAddDamageDiem->GetValue(sdiIdentifier("MID"), tempValue);
        tempValue.GetValue(lsdMIDEntity);

        sdiString matAddDamageDiemName = selMatAddDamageDiem->GetName();
        EntityId fail_id = selMatAddDamageDiem->GetId();

        HandleEdit failInievoHEdit; // /FAIL/INIEVO/ //
        p_radiossModel->CreateEntity(failInievoHEdit, "/FAIL/INIEVO", matAddDamageDiemName,lsdMIDEntity.GetId());
        failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));

        selMatAddDamageDiem->GetEntityHandle(sdiIdentifier("MID"), matHandle);
        sdiConvert::SDIHandlReadList radMatConvertedHandles;

        sdiConvert::Convert::GetConvertedHandles(matHandle, radMatConvertedHandles, destEntityType);
        sdiConvert::SDIHandlReadList sourcehandleList = { {selMatAddDamageDiem->GetHandle()} };

        int lsdNDIEMC;
        tempValue = sdiValue(lsdNDIEMC);
        selMatAddDamageDiem->GetValue(sdiIdentifier("NDIEMC"), tempValue);
        tempValue.GetValue(lsdNDIEMC);

        //failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("NINIEVO"),sdiValue(lsdNDIEMC));
        EntityEdit failInievoEdit(p_radiossModel, failInievoHEdit);
        p_ConvertUtils.CopyValue(*selMatAddDamageDiem, failInievoEdit, "NDIEMC", "NINIEVO");

        unsigned int partId = p_PartBeingConverted.GetId(p_lsdynaModel);
        // get elem part type (shell/solid)

        int nbShell =0;
        int nbSolid =0;

        SelectionElementRead elemShellSelect(p_lsdynaModel, "*ELEMENT_SHELL");
        nbShell = elemShellSelect.Count();

        SelectionElementRead elemSolidSelect(p_lsdynaModel, "*ELEMENT_SOLID");
        nbSolid = elemSolidSelect.Count();

        // get NIP (nb of integration points through thickness) from *SECTION_SHELL
        HandleRead partHRead;
        p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), partId, partHRead);

        HandleRead propHRead;
        partHRead.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHRead);

        double lsdNUMFIP;

        tempValue = sdiValue(lsdNUMFIP);
        selMatAddDamageDiem->GetValue(sdiIdentifier("NUMFIP"), tempValue);
        tempValue.GetValue(lsdNUMFIP);

        if(lsdNUMFIP > 0)
        {
            if(nbSolid > 0)
            {
                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("FAILIP"),sdiValue(int(lsdNUMFIP)));
            }
            else if(nbShell > 0)
            {
                int lsdNIP = 0;
                EntityRead propEntityRead(p_lsdynaModel, propHRead);
                tempValue = sdiValue(lsdNIP);
                propEntityRead.GetValue(sdiIdentifier("LSD_NIP"), tempValue);
                tempValue.GetValue(lsdNIP);

                if(lsdNIP > 0)
                {
                    double pthickFail;
                    pthickFail = lsdNUMFIP*100/lsdNIP;
                    failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("PTHICKFAIL"),sdiValue(pthickFail));
                }
            }
        }
        else if(lsdNUMFIP < 0)
        {
            if(nbShell > 0)
            {
                failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("PTHICKFAIL"),sdiValue(abs(lsdNUMFIP)));
            }
        }

        sdiDoubleList lsdDITYP, lsdDETYP, lsdDCTYP, lsdP2, lsdP3, lsdP4, lsdQ1, lsdQ3, lsdQ1DAMAGE;
        vector<reference_wrapper<sdiDoubleList>> attrValList = { lsdDITYP,lsdDETYP, lsdDCTYP };
        vector<sdiString> attrNameList = { "DITYP", "DETYP", "DCTYP", "P2", "P3", "P4", "Q1", "Q3", "Q1_DAMAGE"};
        p_ConvertUtils.GetAttribValues(*selMatAddDamageDiem, attrNameList, attrValList);

        if(lsdNDIEMC > 0)
        {
            for (int i = 0; i < lsdNDIEMC; ++i)
                {
                    tempValue = sdiValue(lsdDITYP);
                    selMatAddDamageDiem->GetValue(sdiIdentifier("DITYP",0,i), tempValue);
                    tempValue.GetValue(lsdDITYP);

                    if(lsdDITYP[i] == 0.0)
                      failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("INITYPE",0,i),sdiValue(1));
                    else if(lsdDITYP[i] == 1.0)
                      failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("INITYPE",0,i),sdiValue(2));
                    else if(lsdDITYP[i] == 2.0)
                      failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("INITYPE",0,i),sdiValue(3));
                    else if(lsdDITYP[i] == 3.0)
                      failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("INITYPE",0,i),sdiValue(4));
                    else if(lsdDITYP[i] == 4.0)
                      failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("INITYPE",0,i),sdiValue(5));

                    // P1 --> functions/table
                    HandleRead crvP1Handle;
                    selMatAddDamageDiem->GetEntityHandle(sdiIdentifier("P1",0,i), crvP1Handle);

                    if (crvP1Handle.IsValid())
                    {
                        EntityRead crvP1EntRead(p_lsdynaModel, crvP1Handle);
                        sdiString crvP1Type = crvP1EntRead.GetKeyword();
                        EntityId crvP1Id = crvP1EntRead.GetId();
                        EntityEdit failInievoEdit(p_radiossModel, failInievoHEdit);
                        if(!crvP1Type.compare(0, 13, "*DEFINE_CURVE"))
                        {
                            failInievoEdit.SetValue(sdiIdentifier("TAB_ID",0,i), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvP1Handle.GetId(p_lsdynaModel))));
                        }
                         else if(!crvP1Type.compare(0, 13, "*DEFINE_TABLE"))
                        {
                            failInievoEdit.SetValue(sdiIdentifier("TAB_ID",0,i), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), crvP1Handle.GetId(p_lsdynaModel))));
                        }
                    }

                    if(lsdDITYP[i] == 1.0 || lsdDITYP[i] == 4.0)
                    {
                        // P2
                        double lsd_P2;
                        tempValue = sdiValue(lsd_P2);
                        selMatAddDamageDiem->GetValue(sdiIdentifier("P2",0,i), tempValue);
                        tempValue.GetValue(lsd_P2);
                        failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("PARAM",0,i),sdiValue(lsd_P2));
                    }

                    if(lsdDITYP[i] == 2.0 || lsdDITYP[i] == 3.0)
                    {
                        // P3
                        double lsd_P3;
                        tempValue = sdiValue(lsd_P3);
                        selMatAddDamageDiem->GetValue(sdiIdentifier("P3",0,i), tempValue);
                        tempValue.GetValue(lsd_P3);
                        failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("PARAM",0,i),sdiValue(lsd_P3));
                    }

                    // P4
                    double lsd_P4;
                    tempValue = sdiValue(lsd_P4);
                    selMatAddDamageDiem->GetValue(sdiIdentifier("P4",0,i), tempValue);
                    tempValue.GetValue(lsd_P4);

                    if(lsd_P4 == 0.0)
                        failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("ISHEAR"),sdiValue(1));
                    else if(lsd_P4 == 1.0)
                        failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("ISHEAR"),sdiValue(0));

                    // P5 --> function
                    HandleRead crvP5Handle;
                    selMatAddDamageDiem->GetEntityHandle(sdiIdentifier("P5",0,i), crvP5Handle);

                    if (crvP5Handle.IsValid())
                    {
                        EntityRead crvP5EntRead(p_lsdynaModel, crvP5Handle);
                        sdiString crvP5Type = crvP5EntRead.GetKeyword();
                        EntityId crvP5Id = crvP5EntRead.GetId();
                        EntityEdit failInievoEdit(p_radiossModel, failInievoHEdit);
                        if(!crvP5Type.compare(0, 13, "*DEFINE_CURVE"))
                        {
                            failInievoEdit.SetValue(sdiIdentifier("TAB_EL",0,i), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvP5Handle.GetId(p_lsdynaModel))));
                        }
                         else if(!crvP5Type.compare(0, 13, "*DEFINE_TABLE"))
                        {
                            failInievoEdit.SetValue(sdiIdentifier("TAB_EL",0,i), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), crvP5Handle.GetId(p_lsdynaModel))));
                        }
                    }

                    tempValue = sdiValue(lsdDETYP);
                    selMatAddDamageDiem->GetValue(sdiIdentifier("DETYP",0,i), tempValue);
                    tempValue.GetValue(lsdDETYP);

                    if(lsdDETYP[i] == 0.0)
                    {
                        failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("EVOTYPE",0,i),sdiValue(1));
                    }
                    else if(lsdDETYP[i] == 1.0)
                    {
                        failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("EVOTYPE",0,i),sdiValue(2));
                    }

                    tempValue = sdiValue(lsdDCTYP);
                    selMatAddDamageDiem->GetValue(sdiIdentifier("DCTYP",0,i), tempValue);
                    tempValue.GetValue(lsdDCTYP);
                    if(lsdDCTYP[i] == 0.0)
                        failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("COMPTYP",0,i),sdiValue(1));
                    else if(lsdDCTYP[i] == 1.0)
                        failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("COMPTYP",0,i),sdiValue(2));

                    // Q1

                    int lsdflagforQ1;
                    sdiValue tempVal(lsdflagforQ1);
                    selMatAddDamageDiem->GetValue(sdiIdentifier("flagforQ1"), tempVal);
                    tempVal.GetValue(lsdflagforQ1);

                    if(lsdDETYP[i] == 0.0 && lsdflagforQ1 != 1)
                    {
                        // SCALAR(Q1)
                        double lsd_Q1;
                        tempValue = sdiValue(lsd_Q1);
                        selMatAddDamageDiem->GetValue(sdiIdentifier("Q1",0,i), tempValue);
                        tempValue.GetValue(lsd_Q1);
                        failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("DISP",0,i),sdiValue(lsd_Q1));
                    }
                    else if(lsdDETYP[i] == 0.0 && lsdflagforQ1 == 1)
                    {
                        // DATA(Q1_CURVE)
                        HandleRead crvQ1Handle;
                        selMatAddDamageDiem->GetEntityHandle(sdiIdentifier("Q1_CURVE",0,i), crvQ1Handle);

                        EntityRead crvQ1EntRead(p_lsdynaModel, crvQ1Handle);
                        sdiString crvQ1Type = crvQ1EntRead.GetKeyword();
                        EntityId crvQ1Id = crvQ1Handle.GetId(p_lsdynaModel);

                        int nPoints = 0;
                        sdiDoubleList crvPoints;
                        double min_DISP = 0.0, max_DISP = 0.0;
                        double SFA, SFO, OFFA, OFFO;

                        if(!crvQ1Type.compare(0, 13, "*DEFINE_CURVE"))
                        {
                            //double SFA, SFO, OFFA, OFFO;
                            tempValue = sdiValue(SFA);
                            crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("SFA"), tempValue);
                            tempValue.GetValue(SFA);
                            SFA = (SFA == 0.0) ? 1.0 : SFA;

                            tempValue = sdiValue(SFO);
                            crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("SFO"), tempValue);
                            tempValue.GetValue(SFO);
                            SFO = (SFO == 0.0) ? 1.0 : SFO;

                            tempValue = sdiValue(OFFA);
                            crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("OFFA"), tempValue);
                            tempValue.GetValue(OFFA);

                            tempValue = sdiValue(OFFO);
                            crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("OFFO"), tempValue);
                            tempValue.GetValue(OFFO);

                            //int nPoints = 0;
                            tempValue = sdiValue(nPoints);
                            crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("numberofpoints"), tempValue);
                            tempValue.GetValue(nPoints);
                            if (nPoints)
                            {
                                //sdiDoubleList crvPoints;
                                tempValue = sdiValue(crvPoints);
                                crvQ1Handle.GetValue(p_lsdynaModel, sdiIdentifier("points"), tempValue);
                                tempValue.GetValue(crvPoints);

                                //double min_DISP,max_DISP;
                                // Assume first element as maximum and last element as minimum
                                max_DISP = (crvPoints[0] + OFFO)*SFO;
                                min_DISP = (crvPoints[nPoints-1] + OFFO)*SFO;

                                // Find maximum and minimum in all array elements.
                                for (int k = 0; k < 2 * nPoints; k += 2)
                                {
                                // If current element is greater than max
                                    if ((crvPoints[k+1] + OFFO)*SFO > max_DISP)
                                        max_DISP = (crvPoints[k+1] + OFFO)*SFO;

                                // If current element is smaller than min
                                    if ((crvPoints[k+1] + OFFO)*SFO < min_DISP)
                                        min_DISP = (crvPoints[k+1] + OFFO)*SFO;
                                }
                            } // if (nPoints)
                            failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("DISP",0,i),sdiValue(min_DISP));
                        }
                        else if(!crvQ1Type.compare(0, 13, "*DEFINE_TABLE"))
                        {
                         // Loop over table's curves and get min_DISP
                            int tableId = crvQ1Id;
                            HandleEdit RadTableHandle;
                            p_radiossModel->FindById(p_radiossModel->GetEntityType("/TABLE/1"), tableId, RadTableHandle);

                            int nFunct = 0;
                            sdiValue tempValue(nFunct);
                            RadTableHandle.GetValue(p_radiossModel, sdiIdentifier("curverows"), tempValue);
                            tempValue.GetValue(nFunct);

                            for (int j = 0; j < nFunct; ++j)
                            {
                                sdiValueEntity functabEntity;
                                RadTableHandle.GetValue(p_radiossModel, sdiIdentifier("fct_ID",0,j), tempValue);
                                tempValue.GetValue(functabEntity);
                                int functabId = functabEntity.GetId();

                                HandleRead functabHRead;
                                p_radiossModel->FindById(p_radiossModel->GetEntityType("/FUNCT"), functabId, functabHRead);

                                if (functabHRead.IsValid())
                                {
                                    //int nPoints;
                                    tempValue = sdiValue(nPoints);
                                    functabHRead.GetValue(p_radiossModel, sdiIdentifier("numberofpoints"), tempValue);
                                    tempValue.GetValue(nPoints);

                                    //sdiDoubleList crvPoints;
                                    tempValue = sdiValue(crvPoints);
                                    functabHRead.GetValue(p_radiossModel, sdiIdentifier("points"), tempValue);
                                    tempValue.GetValue(crvPoints);

                                    tempValue = sdiValue(SFA);
                                    functabHRead.GetValue(p_radiossModel, sdiIdentifier("A_SCALE_X"), tempValue);
                                    tempValue.GetValue(SFA);
                                    SFA = (SFA == 0.0) ? 1.0 : SFA;

                                    tempValue = sdiValue(SFO);
                                    functabHRead.GetValue(p_radiossModel, sdiIdentifier("F_SCALE_Y"), tempValue);
                                    tempValue.GetValue(SFO);
                                    SFO = (SFO == 0.0) ? 1.0 : SFO;

                                    tempValue = sdiValue(OFFA);
                                    functabHRead.GetValue(p_lsdynaModel, sdiIdentifier("A_SHIFT_X"), tempValue);
                                    tempValue.GetValue(OFFA);

                                    tempValue = sdiValue(OFFO);
                                    functabHRead.GetValue(p_lsdynaModel, sdiIdentifier("F_SHIFT_Y"), tempValue);
                                    tempValue.GetValue(OFFO);

                                    //double min_DISP,max_DISP;
                                    // Assume first element as maximum and last element as minimum
                                    if(j==0) // first table function initialization
                                    {
                                        max_DISP = (crvPoints[0] + OFFO)*SFO;
                                        min_DISP = (crvPoints[nPoints-1] + OFFO)*SFO;
                                    }

                                    // Find maximum and minimum in all array elements.
                                    for (int k = 0; k < 2 * nPoints; k += 2)
                                    {
                                        // If current element is greater than max
                                        if ((crvPoints[k+1] + OFFO)*SFO > max_DISP)
                                            max_DISP = (crvPoints[k+1] + OFFO)*SFO;
                                        // If current element is smaller than min
                                        if ((crvPoints[k+1] + OFFO)*SFO < min_DISP)
                                            min_DISP = (crvPoints[k+1] + OFFO)*SFO;
                                    }
                                } // if (functabHRead.IsValid())
                            } // for (int j = 0; j < nFunct; ++j)
                            failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("DISP",0,i),sdiValue(min_DISP));
                        } // if(!crvQ1Type.compare(0, 13, "*DEFINE_CURVE"))
                    }
                    else if(lsdDETYP[i] == 1.0)
                    {
                        // ENER = SCALAR(Q1)
                        double lsd_Q1DAMAGE;
                        tempValue = sdiValue(lsdQ1DAMAGE);
                        selMatAddDamageDiem->GetValue(sdiIdentifier("Q1_DAMAGE",0,i), tempValue);
                        tempValue.GetValue(lsd_Q1DAMAGE);

                        failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("ENER",0,i),sdiValue(lsd_Q1DAMAGE));
                    } // if(lsdDETYP[i] == 0.0 && lsdflagforQ1 != 1)

                    // Q2 - ignored

                    // Q3
                    if(lsdDETYP[i] == 0.0 && lsdflagforQ1 != 1)
                    {
                        // SCALAR(Q3)
                        double lsd_Q1;
                        tempValue = sdiValue(lsd_Q1);
                        selMatAddDamageDiem->GetValue(sdiIdentifier("Q1",0,i), tempValue);
                        tempValue.GetValue(lsd_Q1);
                        double lsd_Q3;

                        tempValue = sdiValue(lsd_Q3);
                        selMatAddDamageDiem->GetValue(sdiIdentifier("Q3",0,i), tempValue);
                        tempValue.GetValue(lsd_Q3);

                        if(lsd_Q1 > 0.0 && lsd_Q3 > 0.0)
                        {
                            failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("ALPHA",0,i),sdiValue(lsd_Q3));
                            failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("EVOSHAP",0,i),sdiValue(2));
                        }
                    }
                } // for (int i = 0; i < lsdNDIEMC; ++i)
        } // if(lsdNDIEMC > 0)

        failInievoHEdit.SetValue(p_radiossModel, sdiIdentifier("mat_id"), sdiValue(sdiValueEntity(destEntityType, radMatConvertedHandles[0].GetId(p_radiossModel))));
        sdiConvert::Convert::PushToConversionLog(std::make_pair(failInievoHEdit, sourcehandleList));
    } // while (selMatAddDamageDiem.Next())
}
void ConvertMat::p_ConvertMatL105(const EntityRead& dynaMat,sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat, unsigned short int &matLawNum)
{
    double lsdSIGY;
    double lsdETAN;
    double lsdC;
    double lsdP;
    double lsdFAIL;
    double lsdEPS1;
    double lsdEPS2;
    double lsdEPS3;
    double lsdEPS4;
    double lsdEPS5;
    double lsdEPS6;
    double lsdEPS7;
    double lsdEPS8;
    double lsdES1;
    double lsdES2;
    double lsdES3;
    double lsdES4;
    double lsdES5;
    double lsdES6;
    double lsdES7;
    double lsdES8;
    sdiString dynaMatName = dynaMat.GetName();

    attribMap = { {"E","E"}, {"PR","Nu"}, {"RHO", "Init.dens."} };

    vector<reference_wrapper<double>> attrValList = {   lsdSIGY, lsdETAN, lsdC, lsdP, lsdEPS1, lsdEPS2,lsdEPS3,
                                                        lsdEPS4, lsdEPS5, lsdEPS6, lsdEPS7, lsdEPS8, lsdES1,lsdES2,
                                                        lsdES3, lsdES4, lsdES5, lsdES6, lsdES7, lsdES8, lsdFAIL};

    vector<sdiString> attrNameList = {   "SIGY","ETAN","C","P","EPS1","EPS2","EPS3",
                                        "EPS4","EPS5","EPS6","EPS7","EPS8","ES1","ES2",
                                        "ES3","ES4","ES5","ES6","ES7","ES8","FAIL"};

    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

    HandleRead lcssHandle;
    HandleRead lcsrHandle;
    dynaMat.GetEntityHandle(sdiIdentifier("LCSS"), lcssHandle);
    dynaMat.GetEntityHandle(sdiIdentifier("LCSR"),lcsrHandle);
    int lcssId = lcssHandle.GetId(p_lsdynaModel);
    int lcsrId = lcsrHandle.GetId(p_lsdynaModel);

    sdiValue curveListVal;
    sdiUIntList entList;
    sdiDoubleList scaleList;
    sdiDoubleList strainRatesList;
    sdiDoubleList strainRatesLogList;
    int nFunct = 1;
    sdiValue tempValue;
    bool negstrains = false;

    HandleEdit failJohnsonHEdit; /* for /FAIL/JOHNSON */

    destCard = "/MAT/LAW36";

    if (lcssId)
    {
        EntityRead lcssEntityRead(p_lsdynaModel, lcssHandle);
        sdiString keyWord = lcssEntityRead.GetKeyword();
        sdiValueEntityList curveList;
        if (keyWord.find("TABLE") != keyWord.npos)
        {
            if (matToPropsReferenceCount > 1)
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
            else
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMat.GetId());
            int crvArrCount = 0;

            tempValue = sdiValue(crvArrCount);
            lcssEntityRead.GetValue(sdiIdentifier("ArrayCount"), tempValue);
            tempValue.GetValue(crvArrCount);
            nFunct = crvArrCount + 1;

            tempValue = sdiValue(curveList);
            lcssEntityRead.GetValue(sdiIdentifier("CurveIds"), tempValue);
            tempValue.GetValue(curveList);
            curveList.GetIdList(entList);
            entList.push_back(entList.back());

            scaleList = sdiDoubleList(nFunct, 1.0);

            tempValue = sdiValue(strainRatesList);
            lcssEntityRead.GetValue(sdiIdentifier("VALUE"), tempValue);
            tempValue.GetValue(strainRatesList);

            if (strainRatesList[0] >= 0)
                strainRatesList.push_back(2.0 * strainRatesList.back());
//-------------
            else if (strainRatesList[0] < 0)
            // natural logarithmic values f strain rates
            {
                negstrains = true;
                for (int i = 0; i < strainRatesList.size(); ++i)
                {
                   strainRatesLogList.push_back(exp(strainRatesList[i]));
                }
                strainRatesLogList.push_back(2.0 * strainRatesLogList.back());
                strainRatesList.clear();
                for (int i = 0; i < strainRatesLogList.size(); ++i)
                {
                    strainRatesList.push_back(strainRatesLogList[i]);
                }
                //-------
                // new table for logarithmic strain rate
                //-------
                //HandleEdit table1HEdit;
                //p_radiossModel->CreateEntity(table1HEdit, "/TABLE/1", "Duplicate_table_ID " + to_string(lcssEntityRead.GetId())+", " + "Mat name_"+dynaMatName+"_ ID _ " + to_string(dynaMat.GetId()), 
                //                p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*DEFINE_TABLE")));
                //EntityEdit table1Edit(p_radiossModel, table1HEdit);
                //table1Edit.SetValue(sdiIdentifier("dimension"), sdiValue(2));
                //table1Edit.SetValue(sdiIdentifier("curverows"), sdiValue(nFunct));
                //table1Edit.SetValue(sdiIdentifier("fct_ID"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), entList)));
                //table1Edit.SetValue(sdiIdentifier("A"), sdiValue(strainRatesList));
                //table1Edit.SetValue(sdiIdentifier("Scale_y"), sdiValue(scaleList));
                //SDIHandlReadList duplidynaTables = { {dynaMat.GetHandle()} };
                //sdiConvert::Convert::PushToConversionLog(std::make_pair(table1HEdit, duplidynaTables));
            }
//-------------
        }
        else if (lcsrId)
        {
            if (matToPropsReferenceCount > 1)
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
            else
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMat.GetId());
            EntityRead lcsrEdit(p_lsdynaModel, lcsrHandle);
            int nPnts = 0;
            sdiDoubleList crvPoints;
            sdiValue tempValue(nPnts);
            lcsrEdit.GetValue(sdiIdentifier("numberofpoints"), tempValue);
            tempValue.GetValue(nPnts);

            tempValue = sdiValue(crvPoints);
            lcsrEdit.GetValue(sdiIdentifier("points"), tempValue);
            tempValue.GetValue(crvPoints);

            double lsdSFA = 1.0;
            double lsdSFO = 1.0;
            double lsdOFFA = 0.0;
            double lsdOFFO = 0.0;
            vector<reference_wrapper<double>> lsdDoubleAttrValues({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
            vector<sdiString> lsdDoubleAttrNames({ "SFA", "SFO", "OFFA", "OFFO" });
            p_ConvertUtils.GetAttribValues(lcsrEdit, lsdDoubleAttrNames, lsdDoubleAttrValues);

            lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
            lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;

            int numVals = nPnts * 2;
            for (int i = 0; i < numVals; ++i)
            {
                if (i % 2 == 0)
                    strainRatesList.push_back(crvPoints[i] * lsdSFA + (lsdOFFA * lsdSFA));
                else
                    scaleList.push_back(crvPoints[i] * lsdSFO + (lsdOFFO * lsdSFO));
                if (i == numVals - 2)
                    strainRatesList.push_back(2 * (crvPoints[i] * lsdSFA + (lsdOFFA * lsdSFA)));
                if (i == numVals - 1)
                    scaleList.push_back(crvPoints[i] * lsdSFO + (lsdOFFO * lsdSFO));
            }
            nFunct = nPnts + 1;
            entList = sdiUIntList(nFunct, lcssId);
        }
        else if (lsdC > 0.0 && lsdP > 0.0)
        {
            p_ConvertToMatLAW44(dynaMat, destCard, attribMap, radMat);
//-------
            if(matLawNum == 105)
            {
                radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(0));
                radMat.SetValue(p_radiossModel, sdiIdentifier("ISMOOTH"), sdiValue(1));
            }

            if (lsdFAIL > 0.0 && matLawNum == 105)
            {
                p_radiossModel->CreateEntity(failJohnsonHEdit, "/FAIL/JOHNSON", dynaMatName);
                //failJohnsonHEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
                EntityEdit failJohnsonEntEdit(p_radiossModel, failJohnsonHEdit);
                failJohnsonEntEdit.SetEntityHandle(sdiIdentifier("mat_id"), radMat);
                p_ConvertUtils.CopyValue(dynaMat, failJohnsonEntEdit, "FAIL", "D1");
                failJohnsonEntEdit.SetValue(sdiIdentifier("IFAIL_SH"), sdiValue(2));
                radMat.SetValue(p_radiossModel, sdiIdentifier("EPS_MAX"), sdiValue(0.0));
            }
//-------
            return;
        }
        else
        {
            if (matToPropsReferenceCount > 1)
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
            else
                p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMat.GetId());

            entList.push_back(lcssId);
            scaleList.push_back(1.0);
            strainRatesList.push_back(0.0);
            nFunct = 1;
        }

        if(matLawNum == 105)
        {
            radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(0));
            radMat.SetValue(p_radiossModel, sdiIdentifier("F_smooth"), sdiValue(1));
        }

        if (lsdFAIL > 0.0 && matLawNum == 105)
        {
            p_radiossModel->CreateEntity(failJohnsonHEdit, "/FAIL/JOHNSON", dynaMatName);
            //failJohnsonHEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
            EntityEdit failJohnsonEntEdit(p_radiossModel, failJohnsonHEdit);
            failJohnsonEntEdit.SetEntityHandle(sdiIdentifier("mat_id"), radMat);
            p_ConvertUtils.CopyValue(dynaMat, failJohnsonEntEdit, "FAIL", "D1");
            failJohnsonEntEdit.SetValue(sdiIdentifier("IFAIL_SH"), sdiValue(2));
        }
    }
    else
    {
        vector<pair<double, double>> mapEPSVsES = { {lsdEPS1,lsdES1},{lsdEPS2,lsdES2},{lsdEPS3,lsdES3},{lsdEPS4,lsdES4},{lsdEPS5,lsdES5},{lsdEPS6,lsdES6},{lsdEPS7,lsdES7},{lsdEPS8,lsdES8}};

        sdiDoubleList lsdEPSList;
        sdiDoubleList lsdESList;
        int tempIndex = 0;

        for (auto epsEsPair : mapEPSVsES)
        {
            double lsdEps = epsEsPair.first;
            double lsdEs  = epsEsPair.second;
 
            if (tempIndex == 0 || (lsdEs != 0.0 && lsdEps != 0.0))
            {
                lsdESList.push_back(lsdEs);
                lsdEPSList.push_back(lsdEps);
            }
            ++tempIndex;
        }
        size_t epsEsCount = lsdESList.size();
        if (epsEsCount >= 2)
        {
            sdiDoubleList pntsToCreateCrv;
            pntsToCreateCrv.reserve(2 * epsEsCount + 2);
            if (lsdEPSList[0] != 0.0)
            {
                pntsToCreateCrv.push_back(0.0);
                pntsToCreateCrv.push_back(lsdESList[0]);
            }
            for (size_t i = 0; i < epsEsCount; ++i)
            {
                pntsToCreateCrv.push_back(lsdEPSList[i]);
                pntsToCreateCrv.push_back(lsdESList[i]);
            }
            HandleEdit epsEsCurveHEdit;
            p_ConvertUtils.CreateCurve(dynaMatName, (int)pntsToCreateCrv.size() / 2, pntsToCreateCrv, epsEsCurveHEdit);

            if (epsEsCurveHEdit.IsValid())
            {
                SDIHandlReadList sourceMats = { {dynaMat.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(epsEsCurveHEdit, sourceMats));
                unsigned int epsEsCurveId = epsEsCurveHEdit.GetId(p_radiossModel);
                if (lsdC > 0.0 && lsdP > 0.0)
                {
                    p_ConvertToMatLAW44(dynaMat, destCard, attribMap, radMat);

                    radMat.SetEntityHandle(p_radiossModel, sdiIdentifier("fct_IDp"), epsEsCurveHEdit);
                    radMat.SetValue(p_radiossModel, sdiIdentifier("Optional_card"), sdiValue(1)); 
//-------
                    if(matLawNum == 105)
                    {
                        radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(0));
                        radMat.SetValue(p_radiossModel, sdiIdentifier("ISMOOTH"), sdiValue(1));
                    }

                    if (lsdFAIL > 0.0 && matLawNum == 105)
                    {
                         p_radiossModel->CreateEntity(failJohnsonHEdit, "/FAIL/JOHNSON", dynaMatName);
                         //failJohnsonHEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
                         EntityEdit failJohnsonEntEdit(p_radiossModel, failJohnsonHEdit);
                         failJohnsonEntEdit.SetEntityHandle(sdiIdentifier("mat_id"), radMat);
                         p_ConvertUtils.CopyValue(dynaMat, failJohnsonEntEdit, "FAIL", "D1");
                         failJohnsonEntEdit.SetValue(sdiIdentifier("IFAIL_SH"), sdiValue(2));
                         radMat.SetValue(p_radiossModel, sdiIdentifier("EPS_MAX"), sdiValue(0.0));
                    }
//-------
                    return;
                }

                if (matToPropsReferenceCount > 1)
                    p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
                else
                    p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMat.GetId());
                if (lcsrId)
                {
                    int nPnts = 0;
                    sdiValue tempValue(nPnts);
                    lcsrHandle.GetValue(p_lsdynaModel, sdiIdentifier("numberofpoints"), tempValue);
                    tempValue.GetValue(nPnts);

                    sdiDoubleList crvPoints;
                    tempValue = sdiValue(crvPoints);
                    lcsrHandle.GetValue(p_lsdynaModel, sdiIdentifier("points"), tempValue);
                    tempValue.GetValue(crvPoints);
                    int numVals = nPnts * 2;
                    for (int i = 0; i < numVals; ++i)
                    {
                        if (i % 2 == 0)
                            strainRatesList.push_back(crvPoints[i]);
                        else
                            scaleList.push_back(crvPoints[i]);
                        if (i == numVals - 2)
                            strainRatesList.push_back(2 * crvPoints[i]);
                        if (i == numVals - 1)
                            scaleList.push_back(crvPoints[i]);
                    }
                    nFunct = nPnts + 1;
                    entList = sdiUIntList(nFunct, epsEsCurveId);
                }
                else
                {
                    strainRatesList.push_back(0.0);
                    scaleList.push_back(1.0);
                    entList.push_back(epsEsCurveId);
                    nFunct = 1;
                }
            }

            if(matLawNum == 105)
            {
                radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(0));
                radMat.SetValue(p_radiossModel, sdiIdentifier("F_smooth"), sdiValue(1));
            }

            if (lsdFAIL > 0.0 && matLawNum == 105)
            {
                p_radiossModel->CreateEntity(failJohnsonHEdit, "/FAIL/JOHNSON", dynaMatName);
                //failJohnsonHEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
                EntityEdit failJohnsonEntEdit(p_radiossModel, failJohnsonHEdit);
                failJohnsonEntEdit.SetEntityHandle(sdiIdentifier("mat_id"), radMat);
                p_ConvertUtils.CopyValue(dynaMat, failJohnsonEntEdit, "FAIL", "D1");
                failJohnsonEntEdit.SetValue(sdiIdentifier("IFAIL_SH"), sdiValue(2));
            }
        }
        else
        {
            p_ConvertToMatLAW44(dynaMat, destCard, attribMap, radMat);

            if(matLawNum == 105)
            {
                radMat.SetValue(p_radiossModel, sdiIdentifier("VP"), sdiValue(0));
                radMat.SetValue(p_radiossModel, sdiIdentifier("ISMOOTH"), sdiValue(1));
            }
//-------
            if (lsdFAIL > 0.0 && matLawNum == 105)
            {
                p_radiossModel->CreateEntity(failJohnsonHEdit, "/FAIL/JOHNSON", dynaMatName);
                //failJohnsonHEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
                EntityEdit failJohnsonEntEdit(p_radiossModel, failJohnsonHEdit);
                failJohnsonEntEdit.SetEntityHandle(sdiIdentifier("mat_id"), radMat);
                p_ConvertUtils.CopyValue(dynaMat, failJohnsonEntEdit, "FAIL", "D1");
                failJohnsonEntEdit.SetValue(sdiIdentifier("IFAIL_SH"), sdiValue(2));
                radMat.SetValue(p_radiossModel, sdiIdentifier("EPS_MAX"), sdiValue(0.0));
            }
//-------
            return;
        }
    }
    if (failJohnsonHEdit.IsValid())
    {
        sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(failJohnsonHEdit, sourcemat));
    }
    EntityEdit radMatEdit(p_radiossModel, radMat);
    radMatEdit.SetValue(sdiIdentifier("N_funct"), sdiValue((int)nFunct));
    if(entList.size())
        radMatEdit.SetValue(sdiIdentifier("func_ID"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), entList)));
    radMatEdit.SetValue(sdiIdentifier("Fscale"), sdiValue(scaleList));
    radMatEdit.SetValue(sdiIdentifier("Eps_dot"), sdiValue(strainRatesList));
//---
    SDIHandlReadList sourceMats = { {dynaMat.GetHandle()} };
    sdiConvert::Convert::PushToConversionLog(std::make_pair(radMat, sourceMats));
}
void ConvertMat::p_ConvertMatL91_92(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    int lsdkeyOpt;
    sdiValue tempValue;
    tempValue = sdiValue(lsdkeyOpt);
    dynaMat.GetValue(sdiIdentifier("keyOpt"), tempValue);
    tempValue.GetValue(lsdkeyOpt);

    destCard = "/MAT/LAW42";
    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());

    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "Rho", "RHO_I");

    radmatEntityEdit.SetValue(sdiIdentifier("Nu"), sdiValue(0.495));

    p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "2.0*C1", "Mu_1");
    p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "-2.0*C2", "Mu_2");

    radmatEntityEdit.SetValue(sdiIdentifier("alpha_1"), sdiValue(2.0));
    radmatEntityEdit.SetValue(sdiIdentifier("alpha_2"), sdiValue(-2.0));

    if (lsdkeyOpt == 1)
    {
        sdiDoubleList lsdS,lsdT;
        lsdS.reserve(6);
        lsdT.reserve(6);
        int Mprony=0;

        vector<reference_wrapper<double>> attrValList = { lsdS[0],lsdS[1],lsdS[2],lsdS[3],lsdS[4],lsdS[5],
                                                          lsdT[0],lsdT[1],lsdT[2],lsdT[3],lsdT[4],lsdT[5] };
        vector<sdiString> attrNameList = { "S1","S2","S3","S4","S5","S6","T1","T2","T3","T4","T5","T6"};
        p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

        for (int i = 0; i < 6; ++i)
        {
            if(lsdS[i] != 0.0) Mprony++;
        }
        if(Mprony > 0)
        {
            radmatEntityEdit.SetValue(sdiIdentifier("M"), sdiValue(Mprony));
            for (int i = 0; i < Mprony; ++i)
            {
                radmatEntityEdit.SetValue(sdiIdentifier("Gamma_arr",0,i), sdiValue(lsdS[i]));
                radmatEntityEdit.SetValue(sdiIdentifier("Tau_arr",0,i), sdiValue(lsdT[i]));
            }
        }
    } // if (lsdkeyOpt == 1)
    sdiConvert::SDIHandlReadList sourcemat = { {dynaMat.GetHandle()} };
    sdiConvert::Convert::PushToConversionLog(std::make_pair(radMat, sourcemat));
}
void ConvertMat::p_ConvertMatL801Shell(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    destCard = "/MAT/LAW119";

    if (matToPropsReferenceCount > 1)
   	p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
   	p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());

    EntityEdit radMatEdit(p_radiossModel, radMat);

   //=============================
    double lsdCSE;
    
    vector<reference_wrapper<double>> attrValList = { lsdCSE };
    vector<sdiString> attrNameList = { "CSE" };
    p_ConvertUtils.GetAttribValues(dynaMat, attrNameList, attrValList);

    radMatEdit.SetValue(sdiIdentifier("RE"), sdiValue((lsdCSE == 0) ? 1.0 : 0.01));
    p_ConvertUtils.CopyValue(dynaMat, radMatEdit, "MPUL", "RHO_I");
    p_ConvertUtils.CopyValue(dynaMat, radMatEdit, "LMIN", "LMIN");
    p_ConvertUtils.CopyValue(dynaMat, radMatEdit, "ECOAT", "EC");
    p_ConvertUtils.CopyValue(dynaMat, radMatEdit, "TCOAT", "TC");
    p_ConvertUtils.CopyValue(dynaMat, radMatEdit, "EB", "E22");
    p_ConvertUtils.CopyValue(dynaMat, radMatEdit, "PRBA", "VC");
   //=============================


    //----------
    // functions
    //----------

    sdiValue value;

    //int nFunct = 0;
    sdiUIntList funcIdList;
    HandleRead loadCrvHandle;
    dynaMat.GetEntityHandle(sdiIdentifier("LSD_LCID"), loadCrvHandle);
    if (loadCrvHandle.IsValid())
    {
        EntityRead loadCrvEntRead(p_lsdynaModel, loadCrvHandle);
        sdiString crvType = loadCrvEntRead.GetKeyword();
        EntityId loadcrvId = loadCrvEntRead.GetId();
        if (!crvType.compare(0, 13, "*DEFINE_CURVE"))
        {
            //nFunct = 1;
            radMatEdit.SetValue(sdiIdentifier("FUN_L"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), loadCrvHandle.GetId(p_lsdynaModel))));

        }
        else if (!crvType.compare(0, 13, "*DEFINE_TABLE"))
        {
           
            sdiValueEntityList curveList;
            sdiValue tempValue(curveList);
            loadCrvEntRead.GetValue(sdiIdentifier("CurveIds"), tempValue);
            tempValue.GetValue(curveList);
            curveList.GetIdList(funcIdList);
            //nFunct = (int)funcIdList.size();

            DynaToRad::ShowMessage(sdiMessageHandler::Level::Warning, 27,dynaMat.GetKeyword().c_str(), dynaMatId, dynaMatName.c_str());

            radMatEdit.SetValue(sdiIdentifier("FUN_L"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), funcIdList[0])));
        }
    }




        HandleRead unloadCrvHandle;
    // UNLOAD function
        dynaMat.GetEntityHandle(sdiIdentifier("LSD_LCID2"), unloadCrvHandle);
        if (unloadCrvHandle.IsValid())
        {
            radMatEdit.SetValue(sdiIdentifier("FUN_UL"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), unloadCrvHandle.GetId(p_lsdynaModel))));
        }
    //=============================
    SDIHandlReadList sourceMats = { {dynaMat.GetHandle()} };
    Convert::PushToConversionLog(std::make_pair(radMat, sourceMats));
}
void ConvertMat::p_ConvertMatL224(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();

    destCard = "/MAT/LAW109";
    if (matToPropsReferenceCount > 1)
   	p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
   	p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());

    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");

    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "RHO", "RHO_I");
    //p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "E", "E");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "PR", "Nu");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "CP", "C_p");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "TR", "T_ref");
    //p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "LCK1", "tab_ID_h");
    //p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "LCKT", "tab_ID_t");

    sdiString keyWordLog = dynaMat.GetKeyword();
    if (keyWordLog.find("LOG_INTERPOLATION") != keyWordLog.npos)
        radmatEntityEdit.SetValue(sdiIdentifier("I_smooth"), sdiValue(2));

    sdiValue tempValue;

    int lsdTempDependYoungsMod;
    sdiValue tempVal(lsdTempDependYoungsMod);
    dynaMat.GetValue(sdiIdentifier("LSD_TempDependYoungsMod"), tempVal);
    tempVal.GetValue(lsdTempDependYoungsMod);

    if(lsdTempDependYoungsMod == 1)
    {
        // function
        HandleRead fctAsFunctOfTempHandle;
        dynaMat.GetEntityHandle(sdiIdentifier("E_AsFunctOfTemp"), fctAsFunctOfTempHandle);

        EntityRead fctAsFunctOfTempEntRead(p_lsdynaModel, fctAsFunctOfTempHandle);
        EntityId fctAsFunctOfTempId = fctAsFunctOfTempHandle.GetId(p_lsdynaModel);

        int nPoints = 0;
        sdiDoubleList crvPoints;
        tempVal = sdiValue(nPoints);
        fctAsFunctOfTempHandle.GetValue(p_lsdynaModel, sdiIdentifier("numberofpoints"), tempVal);
        tempVal.GetValue(nPoints);
        if (nPoints)
        {
            tempVal = sdiValue(crvPoints);
            fctAsFunctOfTempHandle.GetValue(p_lsdynaModel, sdiIdentifier("points"), tempVal);
            tempVal.GetValue(crvPoints);
            radmatEntityEdit.SetValue(sdiIdentifier("E"), sdiValue(crvPoints[1]));
        }
    }
    else
    {
        // scalar
        p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "E", "E");
    }
//---
    HandleRead lc0KTHandle;
    dynaMat.GetEntityHandle(sdiIdentifier("LCKT"), lc0KTHandle);
    int lc0KTId = lc0KTHandle.GetId(p_lsdynaModel);
    if (lc0KTId)
    {
        EntityRead lc0KTEntityRead(p_lsdynaModel, lc0KTHandle);
        sdiString keyWord = lc0KTEntityRead.GetKeyword();
        sdiValueEntityList curveList;
        if (keyWord.find("TABLE") != keyWord.npos)
        {
            radmatEntityEdit.SetValue(sdiIdentifier("tab_ID_t"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), lc0KTId)));
        }
    }

    sdiUIntList entList;
    sdiDoubleList scaleList;
    sdiDoubleList strainRatesList;
    sdiDoubleList strainRatesLogList;

    HandleRead lc0K1Handle;
    dynaMat.GetEntityHandle(sdiIdentifier("LCK1"), lc0K1Handle);
    int lc0K1Id = lc0K1Handle.GetId(p_lsdynaModel);
    if (lc0K1Id)
    {
        EntityRead lc0K1EntityRead(p_lsdynaModel, lc0K1Handle);
        sdiString keyWord = lc0K1EntityRead.GetKeyword();
        sdiValueEntityList curveList;
        if (keyWord.find("TABLE") != keyWord.npos)
        {
            int crvArrCount = 0;
            int nFunct = 0;

            tempValue = sdiValue(crvArrCount);
            lc0K1EntityRead.GetValue(sdiIdentifier("ArrayCount"), tempValue);
            tempValue.GetValue(crvArrCount);
            nFunct = crvArrCount + 1;

            tempValue = sdiValue(curveList);
            lc0K1EntityRead.GetValue(sdiIdentifier("CurveIds"), tempValue);
            tempValue.GetValue(curveList);
            curveList.GetIdList(entList);
            entList.push_back(entList.back());

            scaleList = sdiDoubleList(nFunct, 1.0);

            tempValue = sdiValue(strainRatesList);
            lc0K1EntityRead.GetValue(sdiIdentifier("VALUE"), tempValue);
            tempValue.GetValue(strainRatesList);

//-------------
            if (strainRatesList[0] < 0.0)
            // natural logarithmic values strain rates
            {
                for (int i = 0; i < strainRatesList.size(); ++i)
                {
                   strainRatesLogList.push_back(exp(strainRatesList[i]));
                }
                strainRatesList.clear();
                for (int i = 0; i < strainRatesLogList.size(); ++i)
                {
                    strainRatesList.push_back(strainRatesLogList[i]);
                }
                strainRatesList.push_back(strainRatesLogList[strainRatesLogList.size()-1]*10.0 + 1.0);
                //-------
                // new table for logarithmic strain rate
                //-------
                HandleEdit table1HEdit;
                p_radiossModel->CreateEntity(table1HEdit, "/TABLE/1", "Duplicate_table_ID_" + to_string(lc0K1EntityRead.GetId())+"_MatL224_ID_" + to_string(dynaMat.GetId()), 
                                             p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*DEFINE_CURVE")));
                EntityEdit table1Edit(p_radiossModel, table1HEdit);
                table1Edit.SetValue(sdiIdentifier("dimension"), sdiValue(2));
                table1Edit.SetValue(sdiIdentifier("curverows"), sdiValue(nFunct));
                table1Edit.SetValue(sdiIdentifier("fct_ID"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), entList)));
                table1Edit.SetValue(sdiIdentifier("A"), sdiValue(strainRatesList));
                table1Edit.SetValue(sdiIdentifier("Scale_y"), sdiValue(scaleList));

                SDIHandlReadList duplidynaTables = { {dynaMat.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(table1HEdit, duplidynaTables));

                radmatEntityEdit.SetValue(sdiIdentifier("tab_ID_h"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), table1HEdit.GetId(p_radiossModel))));
                radmatEntityEdit.SetValue(sdiIdentifier("I_smooth"), sdiValue(2));
            }
            else
                radmatEntityEdit.SetValue(sdiIdentifier("tab_ID_h"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), lc0K1Id)));
        }
    }
//---
    int lsdbetaopt;
    tempVal = sdiValue(lsdbetaopt);
    dynaMat.GetValue(sdiIdentifier("LSD_TempDependBeta"), tempVal);
    tempVal.GetValue(lsdbetaopt);

    EntityType lsdCurveType = p_lsdynaModel->GetEntityType("*DEFINE_CURVE");
    if(lsdbetaopt == 1)
    {
        // function
        HandleRead crvbetaHandle;
        dynaMat.GetEntityHandle(sdiIdentifier("BETA_AsFunctOfTemp"), crvbetaHandle);

        EntityRead crvbetaEntRead(p_lsdynaModel, crvbetaHandle);
        sdiString crvbetaType = crvbetaEntRead.GetKeyword();
        EntityId crvbetaId = crvbetaHandle.GetId(p_lsdynaModel);


        if (!crvbetaType.compare(0, 13, "*DEFINE_CURVE"))
        {
            // create a new table from function : dimension = 1
            HandleEdit RadTableEditHandle;
            p_radiossModel->CreateEntity(RadTableEditHandle, "/TABLE/1", "Table_from_fuction_" + to_string(crvbetaId) + "_MatL224_" + to_string(dynaMatId),
                                         p_ConvertUtils.GetDynaMaxEntityID(lsdCurveType));
            EntityEdit radTableEntEdit(p_radiossModel, RadTableEditHandle);

            radTableEntEdit.SetValue(sdiIdentifier("dimension"), sdiValue(1));

            HandleRead functabHRead;
            p_radiossModel->FindById(p_radiossModel->GetEntityType("/FUNCT"), crvbetaId, functabHRead);

            if (functabHRead.IsValid())
            {
                int nPoints = 0;
                sdiDoubleList crvPoints;
                double SFA, SFO, OFFA, OFFO;

                tempValue = sdiValue(nPoints);
                functabHRead.GetValue(p_radiossModel, sdiIdentifier("numberofpoints"), tempValue);
                tempValue.GetValue(nPoints);

                tempValue = sdiValue(crvPoints);
                functabHRead.GetValue(p_radiossModel, sdiIdentifier("points"), tempValue);
                tempValue.GetValue(crvPoints);

                tempValue = sdiValue(SFA);
                functabHRead.GetValue(p_radiossModel, sdiIdentifier("A_SCALE_X"), tempValue);
                tempValue.GetValue(SFA);
                SFA = (SFA == 0.0) ? 1.0 : SFA;

                tempValue = sdiValue(SFO);
                functabHRead.GetValue(p_radiossModel, sdiIdentifier("F_SCALE_Y"), tempValue);
                tempValue.GetValue(SFO);
                SFO = (SFO == 0.0) ? 1.0 : SFO;

                tempValue = sdiValue(OFFA);
                functabHRead.GetValue(p_lsdynaModel, sdiIdentifier("A_SHIFT_X"), tempValue);
                tempValue.GetValue(OFFA);

                tempValue = sdiValue(OFFO);
                functabHRead.GetValue(p_lsdynaModel, sdiIdentifier("F_SHIFT_Y"), tempValue);
                tempValue.GetValue(OFFO);

                for (int i = 0; i < 2 * nPoints; i += 2)
                {
                   if(crvPoints[i] < 0.0)
                   {
                       crvPoints[i] = exp(crvPoints[i]);
                       radmatEntityEdit.SetValue(sdiIdentifier("I_smooth"), sdiValue(2));
                   }
                   crvPoints[i]   = crvPoints[i]   * SFA + OFFA;
                   crvPoints[i+1] = crvPoints[i+1] * SFO + OFFO;
                }

                radTableEntEdit.SetValue(sdiIdentifier("curverows"), sdiValue(nPoints));
                radTableEntEdit.SetValue(sdiIdentifier("entry_size"), sdiValue(2 * nPoints));
                radTableEntEdit.SetValue(sdiIdentifier("table2darray"), sdiValue(sdiDoubleList(crvPoints)));

                radmatEntityEdit.SetValue(sdiIdentifier("TAB_ETA"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), RadTableEditHandle.GetId(p_radiossModel))));

                SDIHandlReadList dynaTables = { {dynaMat.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(RadTableEditHandle, dynaTables));

            } // if (functabHRead.IsValid())
        }
        else if (!crvbetaType.compare(0, 13, "*DEFINE_TABLE"))
        {
            radmatEntityEdit.SetValue(sdiIdentifier("TAB_ETA"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), crvbetaId)));
        }
    }
    else
    {
        // scalar
        double lsdbeta;
        tempValue = sdiValue(lsdbeta);
        dynaMat.GetValue(sdiIdentifier("LSD_MAT_BETA"), tempValue);
        tempValue.GetValue(lsdbeta);
        radmatEntityEdit.SetValue(sdiIdentifier("ETA"), sdiValue(lsdbeta));
    }

    //---------------------
    // -- /FAIL/TAB2/ -- //
    //---------------------

    HandleEdit faiTab2HEdit;

    p_radiossModel->CreateEntity(faiTab2HEdit, "/FAIL/TAB2", dynaMatName);
    faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));

    EntityEdit faiTab2Edit(p_radiossModel, faiTab2HEdit);

    double lsdNUMINT;
    tempValue = sdiValue(lsdNUMINT);
    dynaMat.GetValue(sdiIdentifier("NUMINT"), tempValue);
    tempValue.GetValue(lsdNUMINT);

    if(lsdNUMINT > 0.0) p_ConvertUtils.CopyValue(dynaMat, faiTab2Edit, "NUMINT", "FAILIP");
    else p_ConvertUtils.CopyValue(dynaMat, faiTab2Edit, "NUMINT", "PTHICKFAIL");

    p_ConvertUtils.CopyValue(dynaMat, faiTab2Edit, "LCG", "FCT_SR");
    p_ConvertUtils.CopyValue(dynaMat, faiTab2Edit, "LCH", "FCT_TEMP");

    // "LCI" table
    HandleRead lciHandle;
    dynaMat.GetEntityHandle(sdiIdentifier("LCI"), lciHandle);
    int lciId = lciHandle.GetId(p_lsdynaModel);
    if (lciId)
    {
        EntityRead lciEntityRead(p_lsdynaModel, lciHandle);
        sdiString keyWord = lciEntityRead.GetKeyword();
        sdiValueEntityList curveList;

        if (keyWord.find("TABLE_3D") != keyWord.npos)
        {
            // table 3D
            int n2DTab = 0;
            sdiUIntList tabIdList;
            sdiValueEntityList tabList;
            sdiValue tempValue(tabList);
            lciEntityRead.GetValue(sdiIdentifier("TableIds"), tempValue);
            tempValue.GetValue(tabList);
            tabList.GetIdList(tabIdList);
            n2DTab = (int)tabIdList.size();

            sdiDoubleList strainRates3DList;
            tempValue = sdiValue(strainRates3DList);
            lciEntityRead.GetValue(sdiIdentifier("VALUE"), tempValue);
            tempValue.GetValue(strainRates3DList);

            HandleEdit Rad3DTableEditHandle;
            //p_radiossModel->CreateEntity(Rad3DTableEditHandle, "/TABLE/1", readTable->GetName(), readTable->GetId());
            p_radiossModel->CreateEntity(Rad3DTableEditHandle, "/TABLE/1", "Duplicate_table_ID_" + to_string(lciEntityRead.GetId())+"_MatL224_ID_" + to_string(dynaMat.GetId()), 
                                         p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*DEFINE_CURVE")));
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
            sdiDoubleList scale3DList(nFunct3D, 1.0);

            for (int i = 0; i < n2DTab; i++)
            {
                // loop over functions of each table
 
                HandleRead tab2dHandle;
                p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*DEFINE_TABLE"), tabIdList[i], tab2dHandle);

                double lsdSFA = p_ConvertUtils.GetValue<double>(lciEntityRead, "SFA");
                double lsdOFFA = p_ConvertUtils.GetValue<double>(lciEntityRead, "OFFA");

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

                //-------------
                // (multiply LS-DYNA triaxiality entry by -1 to have RADIOSS triaxiality index)
                for (int j = 0; j < strainRates2DList.size(); ++j)
                {
                    strainRates2DList[j]=(strainRates2DList[j])*-1.0;
                }
                //-------------

                for (int j = 0; j < nFunct; j++)
                {
                    func3DIdList.push_back(funcIdList[j]);
                    A3List.push_back(strainRates2DList[j]);
                    B3List.push_back(strainRates3DList[i]);
                }
            }

            rad3DTableEntEdit.SetValue(sdiIdentifier("dimension"), sdiValue(3));
            rad3DTableEntEdit.SetValue(sdiIdentifier("curverows"), sdiValue(int(nFunct3D)));
            rad3DTableEntEdit.SetValue(sdiIdentifier("fct_ID"), sdiValue(sdiValueEntityList(destEntityType, func3DIdList)));
            rad3DTableEntEdit.SetValue(sdiIdentifier("A"), sdiValue(A3List));
            rad3DTableEntEdit.SetValue(sdiIdentifier("B"), sdiValue(B3List));
            rad3DTableEntEdit.SetValue(sdiIdentifier("Scale_y"), sdiValue(scale3DList));

            faiTab2Edit.SetEntityHandle(sdiIdentifier("TAB_EL"), Rad3DTableEditHandle);

            SDIHandlReadList duplidynaTables = { {dynaMat.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(Rad3DTableEditHandle, duplidynaTables));
        }
        else if (keyWord.find("TABLE_2D") != keyWord.npos)
        {
            // table 2D
            int nFunct = 0;

            tempValue = sdiValue(curveList);
            lciEntityRead.GetValue(sdiIdentifier("CurveIds"), tempValue);
            tempValue.GetValue(curveList);
            curveList.GetIdList(entList);
            //entList.push_back(entList.back());

            nFunct = (int)entList.size();

            scaleList = sdiDoubleList(nFunct, 1.0);

            sdiDoubleList strainRatesList;
            tempValue = sdiValue(strainRatesList);
            lciEntityRead.GetValue(sdiIdentifier("VALUE"), tempValue);
            tempValue.GetValue(strainRatesList);

            //-------------
            // (multiply LS-DYNA triaxiality entry by -1 to have RADIOSS triaxiality index)
            for (int i = 0; i < strainRatesList.size(); ++i)
            {
                strainRatesList[i]=(strainRatesList[i])*-1.0;
            }
            //-------------
            // new table for strain rate 
            HandleEdit tablelciHEdit;
            p_radiossModel->CreateEntity(tablelciHEdit, "/TABLE/1", "Duplicate_table_ID_" + to_string(lciEntityRead.GetId())+"_MatL224_ID_" + to_string(dynaMat.GetId()), 
                                         p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*DEFINE_CURVE")));
            EntityEdit tablelciEdit(p_radiossModel, tablelciHEdit);
            tablelciEdit.SetValue(sdiIdentifier("dimension"), sdiValue(2));
            tablelciEdit.SetValue(sdiIdentifier("curverows"), sdiValue(nFunct));
            tablelciEdit.SetValue(sdiIdentifier("fct_ID"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), entList)));
            tablelciEdit.SetValue(sdiIdentifier("A"), sdiValue(strainRatesList));
            tablelciEdit.SetValue(sdiIdentifier("Scale_y"), sdiValue(scaleList));

            faiTab2Edit.SetEntityHandle(sdiIdentifier("TAB_EL"), tablelciHEdit);

            SDIHandlReadList duplidynaTables = { {dynaMat.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(tablelciHEdit, duplidynaTables));
        }
    }

    // "LCF" table
    HandleRead lcfHandle;
    dynaMat.GetEntityHandle(sdiIdentifier("LCF"), lcfHandle);
    int lcfId = lcfHandle.GetId(p_lsdynaModel);
    if (lcfId)
    {
        EntityRead lcfEntityRead(p_lsdynaModel, lcfHandle);
        sdiString keyWord = lcfEntityRead.GetKeyword();
        sdiValueEntityList curveList;

        if (keyWord.find("TABLE_2D") != keyWord.npos)
        {
            int nFunct = 0;

            tempValue = sdiValue(curveList);
            lcfEntityRead.GetValue(sdiIdentifier("CurveIds"), tempValue);
            tempValue.GetValue(curveList);
            curveList.GetIdList(entList);

            nFunct = (int)entList.size();
            sdiUIntList DuplientList;
            DuplientList.reserve(nFunct);

            // (multiply LS-DYNA triaxiality entry by -1 to have RADIOSS triaxiality index)
            // SFA = SFA*-1.0;

            // duplicate entList function :
            for (int i = 0; i < nFunct; ++i)
            {
                HandleRead functLCFHRead;
                p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*DEFINE_CURVE"), entList[i], functLCFHRead);
                EntityRead functLCFEntRead(p_lsdynaModel, functLCFHRead);

                int nPnts = 0;
                sdiDoubleList crvPoints;

                double lsdSFA = 1.0;
                double lsdSFO = 1.0;
                double lsdOFFA = 0.0;
                double lsdOFFO = 0.0;

                tempValue = sdiValue(crvPoints);
                functLCFEntRead.GetValue(sdiIdentifier("points"), tempValue);
                tempValue.GetValue(crvPoints);

                tempValue = sdiValue(nPnts);
                functLCFEntRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
                tempValue.GetValue(nPnts);
                crvPoints.reserve(2 * nPnts + 2);

                vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
                vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
                p_ConvertUtils.GetAttribValues(functLCFEntRead, lsdQueryAttribs, attribVals);
                lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
                // (multiply LS-DYNA triaxiality entry by -1 to have RADIOSS triaxiality index)
                lsdSFA = lsdSFA*-1.0;
                lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
                //-----------------------
                HandleEdit functHEdit;
                p_ConvertUtils.CreateCurve("Duplicate_" + to_string(entList[i]) + "_MatL224_" + to_string(dynaMatId),
                                         (int)crvPoints.size() / 2, crvPoints, functHEdit, lsdSFA, lsdSFO, lsdOFFA);

                DuplientList.push_back(functHEdit.GetId(p_radiossModel));
            }

            scaleList = sdiDoubleList(nFunct, 1.0);

            sdiDoubleList strainRatesList;
            tempValue = sdiValue(strainRatesList);
            lcfEntityRead.GetValue(sdiIdentifier("VALUE"), tempValue);
            tempValue.GetValue(strainRatesList);

            //-------------
            // new table for strain rate 
            HandleEdit tablelcfHEdit;
            p_radiossModel->CreateEntity(tablelcfHEdit, "/TABLE/1", "Duplicate_table_ID_" + to_string(lcfEntityRead.GetId())+"_MatL224_ID_" + to_string(dynaMat.GetId()), 
                                         p_ConvertUtils.GetDynaMaxEntityID(p_lsdynaModel->GetEntityType("*DEFINE_CURVE")));
            EntityEdit tablelcfEdit(p_radiossModel, tablelcfHEdit);
            tablelcfEdit.SetValue(sdiIdentifier("dimension"), sdiValue(2));
            tablelcfEdit.SetValue(sdiIdentifier("curverows"), sdiValue(nFunct));
            //tablelcfEdit.SetValue(sdiIdentifier("fct_ID"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), entList)));
            tablelcfEdit.SetValue(sdiIdentifier("fct_ID"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/FUNCT"), DuplientList)));
            tablelcfEdit.SetValue(sdiIdentifier("A"), sdiValue(strainRatesList));
            tablelcfEdit.SetValue(sdiIdentifier("Scale_y"), sdiValue(scaleList));

            faiTab2Edit.SetEntityHandle(sdiIdentifier("EPSF_ID"), tablelcfHEdit);

            SDIHandlReadList duplidynaTables = { {dynaMat.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(tablelcfHEdit, duplidynaTables));
        }
    else if (keyWord.find("CURVE") != keyWord.npos) // function
       {
            int nPoints = 0;
            sdiDoubleList crvPoints;
            double SFA, SFO, OFFA, OFFO;

            tempValue = sdiValue(nPoints);
            lcfHandle.GetValue(p_radiossModel, sdiIdentifier("numberofpoints"), tempValue);
            tempValue.GetValue(nPoints);

            tempValue = sdiValue(crvPoints);
            lcfHandle.GetValue(p_radiossModel, sdiIdentifier("points"), tempValue);
            tempValue.GetValue(crvPoints);

            tempValue = sdiValue(SFA);
            lcfHandle.GetValue(p_radiossModel, sdiIdentifier("A_SCALE_X"), tempValue);
            tempValue.GetValue(SFA);
            SFA = (SFA == 0.0) ? 1.0 : SFA;
            // (multiply LS-DYNA triaxiality entry by -1 to have RADIOSS triaxiality index)
            SFA = SFA*-1.0;

            tempValue = sdiValue(SFO);
            lcfHandle.GetValue(p_radiossModel, sdiIdentifier("F_SCALE_Y"), tempValue);
            tempValue.GetValue(SFO);
            SFO = (SFO == 0.0) ? 1.0 : SFO;

            tempValue = sdiValue(OFFA);
            lcfHandle.GetValue(p_lsdynaModel, sdiIdentifier("A_SHIFT_X"), tempValue);
            tempValue.GetValue(OFFA);

            tempValue = sdiValue(OFFO);
            lcfHandle.GetValue(p_lsdynaModel, sdiIdentifier("F_SHIFT_Y"), tempValue);
            tempValue.GetValue(OFFO);

            HandleEdit duplicatelcffunctHEdit;
            p_ConvertUtils.CreateCurve("Duplicate_" + to_string(lcfId) + "_MatL224_" + to_string(dynaMatId),
                                        (int)crvPoints.size()/2, crvPoints, duplicatelcffunctHEdit, SFA, SFO, OFFA, OFFO);

            faiTab2Edit.SetEntityHandle(sdiIdentifier("EPSF_ID"), duplicatelcffunctHEdit);
            SDIHandlReadList duplidynaFuntion = { {dynaMat.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(duplicatelcffunctHEdit, duplidynaFuntion));
       }
    }

    faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("IREG"), sdiValue(2));
    faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("mat_id"), sdiValue(sdiValueEntity(destEntityType, radmatEntityEdit.GetId())));

    if (faiTab2HEdit.IsValid())
    {
        sdiConvert::SDIHandlReadList sourceMat = { {dynaMat.GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(faiTab2HEdit, sourceMat));
    } // if (!faiTab2HEdit.IsValid())
}



void ConvertMat::p_ConvertMatL187(const EntityRead& dynaMat, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    sdiString dynaMatName = dynaMat.GetName();
    unsigned int dynaMatId = dynaMat.GetId();
    sdiValue tempValue;
    destCard = "/MAT/LAW76";

    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMat.GetName(), dynaMat.GetId());

    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
    //=============================

        p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "RHO", "RHO_I");
        p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "EMOD", "E");
        p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "NUE", "nu");
        p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "NUEP", "Nu_p");
        p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "EPFAIL", "Epsilon_f_p");
        p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "EPFAIL + DEPRPT", "Epsilon_r_p");
        p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "ICONV", "ICONV");
        radmatEntityEdit.SetValue(sdiIdentifier("Fsmooth"), sdiValue(1));
        radmatEntityEdit.SetValue(sdiIdentifier("Fcut"), sdiValue(0.0));

        // curves-tables

        EntityType lsdCurveType = p_lsdynaModel->GetEntityType("*DEFINE_CURVE");

        //----
        
        //----
        HandleRead lcLCIDTHandle;
        dynaMat.GetEntityHandle(sdiIdentifier("LCID-T"), lcLCIDTHandle);
        int lcLCIDTId = lcLCIDTHandle.GetId(p_lsdynaModel);
    
        //if (lcLCIDTId)
        if (lcLCIDTHandle.IsValid())
        {
            EntityRead lcLCIDTEntityRead(p_lsdynaModel, lcLCIDTHandle);
            sdiString keyWord = lcLCIDTEntityRead.GetKeyword();
            sdiValueEntityList curveList;
            if (keyWord.find("TABLE") != keyWord.npos)
            {
                radmatEntityEdit.SetValue(sdiIdentifier("TAB_ID1"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), lcLCIDTHandle.GetId(p_lsdynaModel))));
            }
            else
            {
               
                HandleEdit RadTableEditHandle;
                p_radiossModel->CreateEntity(RadTableEditHandle, "/TABLE/1", "Table_from_fuction_" + to_string(lcLCIDTId) + "_MatL187_" + to_string(dynaMatId),
                                             p_ConvertUtils.GetDynaMaxEntityID(lsdCurveType));
                EntityEdit radTableEntEdit(p_radiossModel, RadTableEditHandle);

                radTableEntEdit.SetValue(sdiIdentifier("dimension"), sdiValue(1));

                HandleRead functabHRead;
                p_radiossModel->FindById(p_radiossModel->GetEntityType("/FUNCT"), lcLCIDTId, functabHRead);

                if (functabHRead.IsValid())
                {
                    int nPoints = 0;
                    sdiDoubleList crvPoints;
                    double SFA, SFO, OFFA, OFFO;

                    tempValue = sdiValue(nPoints);
                    functabHRead.GetValue(p_radiossModel, sdiIdentifier("numberofpoints"), tempValue);
                    tempValue.GetValue(nPoints);

                    tempValue = sdiValue(crvPoints);
                    functabHRead.GetValue(p_radiossModel, sdiIdentifier("points"), tempValue);
                    tempValue.GetValue(crvPoints);

                    tempValue = sdiValue(SFA);
                    functabHRead.GetValue(p_radiossModel, sdiIdentifier("A_SCALE_X"), tempValue);
                    tempValue.GetValue(SFA);
                    SFA = (SFA == 0.0) ? 1.0 : SFA;

                    tempValue = sdiValue(SFO);
                    functabHRead.GetValue(p_radiossModel, sdiIdentifier("F_SCALE_Y"), tempValue);
                    tempValue.GetValue(SFO);
                    SFO = (SFO == 0.0) ? 1.0 : SFO;

                    tempValue = sdiValue(OFFA);
                    functabHRead.GetValue(p_lsdynaModel, sdiIdentifier("A_SHIFT_X"), tempValue);
                    tempValue.GetValue(OFFA);

                    tempValue = sdiValue(OFFO);
                    functabHRead.GetValue(p_lsdynaModel, sdiIdentifier("F_SHIFT_Y"), tempValue);
                    tempValue.GetValue(OFFO);

                    for (int i = 0; i < 2 * nPoints; i += 2)
                    {
                       crvPoints[i]   = crvPoints[i]   * SFA + OFFA;
                       crvPoints[i+1] = crvPoints[i+1] * SFO + OFFO;
                    }

                    radTableEntEdit.SetValue(sdiIdentifier("curverows"), sdiValue(nPoints));
                    radTableEntEdit.SetValue(sdiIdentifier("entry_size"), sdiValue(2 * nPoints));
                    radTableEntEdit.SetValue(sdiIdentifier("table2darray"), sdiValue(sdiDoubleList(crvPoints)));

                    radmatEntityEdit.SetValue(sdiIdentifier("TAB_ID1"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), RadTableEditHandle.GetId(p_radiossModel))));

                    SDIHandlReadList dynaTables = { {dynaMat.GetHandle()} };
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(RadTableEditHandle, dynaTables));
                }
            }
        }

        //----
        
        //----
        HandleRead lcLCIDCHandle;
        dynaMat.GetEntityHandle(sdiIdentifier("LCID-C"), lcLCIDCHandle);
        int lcLCIDCId = lcLCIDCHandle.GetId(p_lsdynaModel);

        //if (lcLCIDCId)
        if (lcLCIDCHandle.IsValid())
        {
            EntityRead lcLCIDCEntityRead(p_lsdynaModel, lcLCIDCHandle);
            sdiString keyWord = lcLCIDCEntityRead.GetKeyword();
            sdiValueEntityList curveList;
            if (keyWord.find("CURVE") != keyWord.npos)
            {
                
                HandleEdit RadTableEditHandle;
                p_radiossModel->CreateEntity(RadTableEditHandle, "/TABLE/1", "Table_from_fuction_" + to_string(lcLCIDCId) + "_MatL187_" + to_string(dynaMatId),
                                             p_ConvertUtils.GetDynaMaxEntityID(lsdCurveType));
                EntityEdit radTableEntEdit(p_radiossModel, RadTableEditHandle);

                radTableEntEdit.SetValue(sdiIdentifier("dimension"), sdiValue(1));

                HandleRead functabHRead;
                p_radiossModel->FindById(p_radiossModel->GetEntityType("/FUNCT"), lcLCIDCId, functabHRead);

                if (functabHRead.IsValid())
                {
                    int nPoints = 0;
                    sdiDoubleList crvPoints;
                    double SFA, SFO, OFFA, OFFO;

                    tempValue = sdiValue(nPoints);
                    functabHRead.GetValue(p_radiossModel, sdiIdentifier("numberofpoints"), tempValue);
                    tempValue.GetValue(nPoints);

                    tempValue = sdiValue(crvPoints);
                    functabHRead.GetValue(p_radiossModel, sdiIdentifier("points"), tempValue);
                    tempValue.GetValue(crvPoints);

                    tempValue = sdiValue(SFA);
                    functabHRead.GetValue(p_radiossModel, sdiIdentifier("A_SCALE_X"), tempValue);
                    tempValue.GetValue(SFA);
                    SFA = (SFA == 0.0) ? 1.0 : SFA;

                    tempValue = sdiValue(SFO);
                    functabHRead.GetValue(p_radiossModel, sdiIdentifier("F_SCALE_Y"), tempValue);
                    tempValue.GetValue(SFO);
                    SFO = (SFO == 0.0) ? 1.0 : SFO;

                    tempValue = sdiValue(OFFA);
                    functabHRead.GetValue(p_lsdynaModel, sdiIdentifier("A_SHIFT_X"), tempValue);
                    tempValue.GetValue(OFFA);

                    tempValue = sdiValue(OFFO);
                    functabHRead.GetValue(p_lsdynaModel, sdiIdentifier("F_SHIFT_Y"), tempValue);
                    tempValue.GetValue(OFFO);

                    for (int i = 0; i < 2 * nPoints; i += 2)
                    {
                       crvPoints[i]   = crvPoints[i]   * SFA + OFFA;
                       crvPoints[i+1] = crvPoints[i+1] * SFO + OFFO;
                    }

                    radTableEntEdit.SetValue(sdiIdentifier("curverows"), sdiValue(nPoints));
                    radTableEntEdit.SetValue(sdiIdentifier("entry_size"), sdiValue(2 * nPoints));
                    radTableEntEdit.SetValue(sdiIdentifier("table2darray"), sdiValue(sdiDoubleList(crvPoints)));

                    radmatEntityEdit.SetValue(sdiIdentifier("TAB_ID2"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), RadTableEditHandle.GetId(p_radiossModel))));

                    SDIHandlReadList dynaTables = { {dynaMat.GetHandle()} };
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(RadTableEditHandle, dynaTables));
                }
            }
        }

        //----
        
        //----
        HandleRead lcLCIDSHandle;
        dynaMat.GetEntityHandle(sdiIdentifier("LCID-S"), lcLCIDSHandle);
        int lcLCIDSId = lcLCIDSHandle.GetId(p_lsdynaModel);

        //if (lcLCIDSId)
        if (lcLCIDSHandle.IsValid())
        {
            EntityRead lcLCIDSEntityRead(p_lsdynaModel, lcLCIDSHandle);
            sdiString keyWord = lcLCIDSEntityRead.GetKeyword();
            sdiValueEntityList curveList;
            if (keyWord.find("CURVE") != keyWord.npos)
            {
                
                HandleEdit RadTableEditHandle;
                p_radiossModel->CreateEntity(RadTableEditHandle, "/TABLE/1", "Table_from_fuction_" + to_string(lcLCIDSId) + "_MatL187_" + to_string(dynaMatId),
                                             p_ConvertUtils.GetDynaMaxEntityID(lsdCurveType));
                EntityEdit radTableEntEdit(p_radiossModel, RadTableEditHandle);

                radTableEntEdit.SetValue(sdiIdentifier("dimension"), sdiValue(1));

                HandleRead functabHRead;
                p_radiossModel->FindById(p_radiossModel->GetEntityType("/FUNCT"), lcLCIDSId, functabHRead);

                if (functabHRead.IsValid())
                {
                    int nPoints = 0;
                    sdiDoubleList crvPoints;
                    double SFA, SFO, OFFA, OFFO;

                    tempValue = sdiValue(nPoints);
                    functabHRead.GetValue(p_radiossModel, sdiIdentifier("numberofpoints"), tempValue);
                    tempValue.GetValue(nPoints);

                    tempValue = sdiValue(crvPoints);
                    functabHRead.GetValue(p_radiossModel, sdiIdentifier("points"), tempValue);
                    tempValue.GetValue(crvPoints);

                    tempValue = sdiValue(SFA);
                    functabHRead.GetValue(p_radiossModel, sdiIdentifier("A_SCALE_X"), tempValue);
                    tempValue.GetValue(SFA);
                    SFA = (SFA == 0.0) ? 1.0 : SFA;

                    tempValue = sdiValue(SFO);
                    functabHRead.GetValue(p_radiossModel, sdiIdentifier("F_SCALE_Y"), tempValue);
                    tempValue.GetValue(SFO);
                    SFO = (SFO == 0.0) ? 1.0 : SFO;

                    tempValue = sdiValue(OFFA);
                    functabHRead.GetValue(p_lsdynaModel, sdiIdentifier("A_SHIFT_X"), tempValue);
                    tempValue.GetValue(OFFA);

                    tempValue = sdiValue(OFFO);
                    functabHRead.GetValue(p_lsdynaModel, sdiIdentifier("F_SHIFT_Y"), tempValue);
                    tempValue.GetValue(OFFO);

                    for (int i = 0; i < 2 * nPoints; i += 2)
                    {
                       crvPoints[i]   = crvPoints[i]   * SFA + OFFA;
                       crvPoints[i+1] = crvPoints[i+1] * SFO + OFFO;
                    }

                    radTableEntEdit.SetValue(sdiIdentifier("curverows"), sdiValue(nPoints));
                    radTableEntEdit.SetValue(sdiIdentifier("entry_size"), sdiValue(2 * nPoints));
                    radTableEntEdit.SetValue(sdiIdentifier("table2darray"), sdiValue(sdiDoubleList(crvPoints)));

                    radmatEntityEdit.SetValue(sdiIdentifier("TAB_ID3"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/TABLE/1"), RadTableEditHandle.GetId(p_radiossModel))));

                    SDIHandlReadList dynaTables = { {dynaMat.GetHandle()} };
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(RadTableEditHandle, dynaTables));
                }
            }
        }

        //----
        
        //----
        HandleRead lcLCIDPHandle;
        dynaMat.GetEntityHandle(sdiIdentifier("LCID-P"), lcLCIDPHandle);
        int lcLCIDPId = lcLCIDPHandle.GetId(p_lsdynaModel);
    
        //if (lcLCIDPId)
        if (lcLCIDPHandle.IsValid())
        {
            EntityRead lcLCIDPEntityRead(p_lsdynaModel, lcLCIDPHandle);
            sdiString keyWord = lcLCIDPEntityRead.GetKeyword();
            sdiValueEntityList curveList;
            if (keyWord.find("CURVE") != keyWord.npos)
            {
                radmatEntityEdit.SetValue(sdiIdentifier("fct_IDpr"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), lcLCIDPHandle.GetId(p_lsdynaModel))));
            }
        }

        //----
        
        //----
        HandleRead lcLCIDDHandle;
        dynaMat.GetEntityHandle(sdiIdentifier("LCID-D"), lcLCIDDHandle);
        int lcLCIDDId = lcLCIDDHandle.GetId(p_lsdynaModel);
    
        //if (lcLCIDDId)
        if (lcLCIDDHandle.IsValid())
        {
            EntityRead lcLCIDDEntityRead(p_lsdynaModel, lcLCIDDHandle);
            sdiString keyWord = lcLCIDDEntityRead.GetKeyword();
            sdiValueEntityList curveList;
            if (keyWord.find("CURVE") != keyWord.npos)
            {
                radmatEntityEdit.SetValue(sdiIdentifier("fct_ID1"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), lcLCIDDHandle.GetId(p_lsdynaModel))));
            }
        }

        //----
        
        //----
        HandleRead lcLCIDLCHandle;
        dynaMat.GetEntityHandle(sdiIdentifier("LCID_LC"), lcLCIDLCHandle);
        int lcLCIDLCId = lcLCIDLCHandle.GetId(p_lsdynaModel);

        HandleRead lcLCIDTRIHandle;
        dynaMat.GetEntityHandle(sdiIdentifier("LCID-TRI"), lcLCIDTRIHandle);
        int lcLCIDTRIId = lcLCIDTRIHandle.GetId(p_lsdynaModel);
    
        //if (lcLCIDLCId)
        if (lcLCIDLCHandle.IsValid())
        {

            //if (lcLCIDTRIId)
            if (lcLCIDTRIHandle.IsValid())
            {
                //---
                // add /FAIL/TAB2
                //---
                HandleEdit faiTab2HEdit;

                p_radiossModel->CreateEntity(faiTab2HEdit, "/FAIL/TAB2", dynaMatName);
                faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));

                EntityEdit faiTab2Edit(p_radiossModel, faiTab2HEdit);

                int lsdNUMINT = GetValue<int>(dynaMat, "NUMINT");

                if(lsdNUMINT > 0) 
                {
                    p_ConvertUtils.CopyValue(dynaMat, faiTab2Edit, "NUMINT", "FAILIP");
                    p_ConvertUtils.CopyValue(dynaMat, faiTab2Edit, "NUMINT", "PTHICKFAIL");
                }
                else 
                {
                    faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("FAILIP"), sdiValue(1));
                    faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("PTHICKFAIL"), sdiValue(0.0));
                }
                
                p_ConvertUtils.CopyValue(dynaMat, faiTab2Edit, "EPFAIL", "FCRIT");
                p_ConvertUtils.CopyValue(dynaMat, faiTab2Edit, "LCID-TRI", "EPSF_ID");
                p_ConvertUtils.CopyValue(dynaMat, faiTab2Edit, "LCID_LC", "TAB_EL");
                faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("IREG"), sdiValue(2));


                faiTab2HEdit.SetValue(p_radiossModel, sdiIdentifier("mat_id"), sdiValue(sdiValueEntity(destEntityType, radmatEntityEdit.GetId())));

                if (faiTab2HEdit.IsValid())
                {
                  sdiConvert::SDIHandlReadList sourceMat = { {dynaMat.GetHandle()} };
                  sdiConvert::Convert::PushToConversionLog(std::make_pair(faiTab2HEdit, sourceMat));
                }
            }
            else
            {
                //---
                //  add /FAIL/TENSSTRAIN
                //---
                HandleEdit failtensstrainHEdit;

                p_radiossModel->CreateEntity(failtensstrainHEdit, "/FAIL/TENSSTRAIN", dynaMatName);
                failtensstrainHEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));

                EntityEdit failtensstrainEdit(p_radiossModel, failtensstrainHEdit);

                failtensstrainEdit.SetValue(sdiIdentifier("S_Flag"), sdiValue(2));
                p_ConvertUtils.CopyValue(dynaMat, failtensstrainEdit, "EPFAIL", "EPSILON_F1");
                p_ConvertUtils.SetExpressionValue(dynaMat, failtensstrainEdit, "EPFAIL + DEPRPT", "EPSILON_F2");
                p_ConvertUtils.CopyValue(dynaMat, failtensstrainEdit, "LCID_LC", "FCT_IDEL");

                failtensstrainHEdit.SetValue(p_radiossModel, sdiIdentifier("mat_id"), sdiValue(sdiValueEntity(destEntityType, radmatEntityEdit.GetId())));

                if (failtensstrainHEdit.IsValid())
                {
                  sdiConvert::SDIHandlReadList sourceMat = { {dynaMat.GetHandle()} };
                  sdiConvert::Convert::PushToConversionLog(std::make_pair(failtensstrainHEdit, sourceMat));
                }
            }
        }
}

void ConvertMat::p_ConvertMatL1_FLUID(const EntityRead& dynaMat, sdiString& destCard, HandleEdit& radMat)
{
    EntityId dynaMatId   = dynaMat.GetId();
    sdiString dynaMatName = dynaMat.GetName();

    if (matToPropsReferenceCount > 1)
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radMat, destCard, dynaMatName, dynaMatId);
                    
    EntityEdit radmatEntityEdit(p_radiossModel, radMat);
 
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "RHO", "RHO_I");
    p_ConvertUtils.CopyValue(dynaMat, radmatEntityEdit, "VC", "Nu");
    p_ConvertUtils.SetExpressionValue(dynaMat, radmatEntityEdit, "-CP", "Pmin");

    double lsdK;
    sdiValue tempValue = sdiValue(lsdK);
    dynaMat.GetValue(sdiIdentifier("K"), tempValue);
    tempValue.GetValue(lsdK);

    HandleEdit radEos;
    sdiString destEosCard = "/EOS/LINEAR";
    p_radiossModel->CreateEntity(radEos, destEosCard, dynaMat.GetName(), radmatEntityEdit.GetId());
    EntityEdit radEosEdit(p_radiossModel, radEos);

    radEos.SetValue(p_radiossModel, sdiIdentifier("EOS_Keyword"), sdiValue(sdiString("LINEAR")));
    radEos.SetValue(p_radiossModel, sdiIdentifier("EOS_Options"), sdiValue(13));

    if(lsdK > 0)
    {
        p_ConvertUtils.CopyValue(dynaMat, radEosEdit, "K", "B");
    }
    else if(lsdK == 0)
    {
        p_ConvertUtils.SetExpressionValue(dynaMat, radEosEdit, "E/(3.0*(1.0-2.0*NU))", "B");
    }

    if (radEos.IsValid())
    {
        sdiConvert::SDIHandlReadList sourceMat = { {dynaMat.GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(radEos, sourceMat));
    } 
}

void ConvertMat::p_ConvertEOS1(const EntityRead& lsdEosEntRead, sdiString& destCard, multimap<string, string>& attribMap, const EntityRead& dynaMat, HandleEdit& radMat)
{
    destCard = "/EOS/POLYNOMIAL";
    HandleEdit radEos;
    EntityEdit radMatEdit(p_radiossModel, radMat);
    p_radiossModel->CreateEntity(radEos, destCard, lsdEosEntRead.GetName(), radMatEdit.GetId());
    EntityEdit radEosEdit(p_radiossModel, radEos);

    radEos.SetValue(p_radiossModel, sdiIdentifier("EOS_Keyword"), sdiValue(sdiString("POLYNOMIAL")));
    radEos.SetValue(p_radiossModel, sdiIdentifier("EOS_Options"), sdiValue(3));

    p_ConvertUtils.CopyValue(lsdEosEntRead, radEosEdit, "C0", "C0");
    p_ConvertUtils.CopyValue(lsdEosEntRead, radEosEdit, "C1", "C1");
    p_ConvertUtils.CopyValue(lsdEosEntRead, radEosEdit, "C2", "C2");
    p_ConvertUtils.CopyValue(lsdEosEntRead, radEosEdit, "C3", "C3");
    p_ConvertUtils.CopyValue(lsdEosEntRead, radEosEdit, "C4", "C4");
    p_ConvertUtils.CopyValue(lsdEosEntRead, radEosEdit, "C5", "C5");
    p_ConvertUtils.CopyValue(lsdEosEntRead, radEosEdit, "E0", "MAT_EA");

    double lsdV0;
    sdiValue tempValue = sdiValue(lsdV0);
    lsdEosEntRead.GetValue(sdiIdentifier("V0"), tempValue);
    tempValue.GetValue(lsdV0);
    
    double lsdRHO;
    tempValue = sdiValue(lsdRHO);
    dynaMat.GetValue(sdiIdentifier("RHO"), tempValue);
    tempValue.GetValue(lsdRHO);
    
    if(lsdV0 > 0)
    {
        radEosEdit.SetValue(sdiIdentifier("V0"), sdiValue(lsdRHO*lsdV0));
    }

    if (radEos.IsValid())
    {
        sdiConvert::SDIHandlReadList sourceMat = { {lsdEosEntRead.GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(radEos, sourceMat));
    } 
}

void ConvertMat::p_ConvertEOS4(const EntityRead& lsdEosEntRead, sdiString& destCard, multimap<string, string>& attribMap, HandleEdit& radMat)
{
    destCard = "/EOS/GRUNEISEN";
    HandleEdit radEos;
    EntityEdit radMatEdit(p_radiossModel, radMat);
    p_radiossModel->CreateEntity(radEos, destCard, lsdEosEntRead.GetName(), radMatEdit.GetId());
    EntityEdit radEosEdit(p_radiossModel, radEos);

    radEos.SetValue(p_radiossModel, sdiIdentifier("EOS_Keyword"), sdiValue(sdiString("GRUNEISEN")));
    radEos.SetValue(p_radiossModel, sdiIdentifier("EOS_Options"), sdiValue(2));
    
    p_ConvertUtils.CopyValue(lsdEosEntRead, radEosEdit, "C", "C");
    p_ConvertUtils.CopyValue(lsdEosEntRead, radEosEdit, "S1", "S1");
    p_ConvertUtils.CopyValue(lsdEosEntRead, radEosEdit, "S2", "S2");
    p_ConvertUtils.CopyValue(lsdEosEntRead, radEosEdit, "S3", "S3");
    p_ConvertUtils.CopyValue(lsdEosEntRead, radEosEdit, "GAMAO", "Y0");
    p_ConvertUtils.CopyValue(lsdEosEntRead, radEosEdit, "A", "MAT_A");
    p_ConvertUtils.CopyValue(lsdEosEntRead, radEosEdit, "E0", "E0");

    double lsdV0;
    sdiValue tempValue = sdiValue(lsdV0);
    lsdEosEntRead.GetValue(sdiIdentifier("V0"), tempValue);
    tempValue.GetValue(lsdV0);
    
    if(lsdV0 > 0)
    {
        p_ConvertUtils.SetExpressionValue(lsdEosEntRead, radEosEdit, "1/V0", "RHO_0");
    }

    if (radEos.IsValid())
    {
        sdiConvert::SDIHandlReadList sourceMat = { {lsdEosEntRead.GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(radEos, sourceMat));
    } 
}


void ConvertMat::p_ConvertMatAddThermalExpansion()
{

    SelectionRead selMatAddThermalExpansion(p_lsdynaModel, "*MAT_ADD_THERMAL_EXPANSION");

    while (selMatAddThermalExpansion.Next())
    {
        HandleRead partHandle;
        selMatAddThermalExpansion->GetEntityHandle(sdiIdentifier("PID"), partHandle);
        
        EntityRead partEntRead(p_lsdynaModel, partHandle);
        sdiString partCard = partEntRead.GetKeyword();
        sdiString partName = partEntRead.GetName();
        EntityId partId = partEntRead.GetId();

        HandleEdit radEditPart;
        EntityType radPartEntityType = p_radiossModel->GetEntityType("/PART");
        p_radiossModel->FindById(radPartEntityType, partId, radEditPart);

        HandleRead partHRead;

        if (radEditPart.IsValid())
        {
            HandleRead matHRead;
            partEntRead.GetEntityHandle(sdiIdentifier("MID"), matHRead);

            HandleEdit radMatEdit;
            if (matHRead.IsValid())
            {
                EntityRead matEntRead(p_lsdynaModel, matHRead);
                p_ConvertUtils.PropagateInclude(matEntRead);
                
                HandleEdit thermStressHEdit;
                HandleEdit heatMatHEdit;
                if (!thermStressHEdit.IsValid())
                {
                    p_radiossModel->CreateEntity(thermStressHEdit, "/THERM_STRESS/MAT", selMatAddThermalExpansion->GetName(),matEntRead.GetId());
                    
                    p_radiossModel->CreateEntity(heatMatHEdit, "/HEAT/MAT", selMatAddThermalExpansion->GetName(),matEntRead.GetId());


                    EntityEdit thermStressEdit(p_radiossModel, thermStressHEdit);
                    thermStressHEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
                                 
                    p_ConvertUtils.CopyValue(*selMatAddThermalExpansion, thermStressEdit, "MULT", "Fscale_x");
                    p_ConvertUtils.CopyValue(*selMatAddThermalExpansion, thermStressEdit, "MULTY", "Fscale_y");
                    p_ConvertUtils.CopyValue(*selMatAddThermalExpansion, thermStressEdit, "MULTZ", "Fscale_z");
                    
                    p_ConvertUtils.CopyValue(*selMatAddThermalExpansion, thermStressEdit, "LCID", "Fct_ID_Tx");
                    p_ConvertUtils.CopyValue(*selMatAddThermalExpansion, thermStressEdit, "LCIDY", "Fct_ID_T");
                    p_ConvertUtils.CopyValue(*selMatAddThermalExpansion, thermStressEdit, "LCIDZ", "Fct_ID_Tz");

                    sdiConvert::SDIHandlReadList sourceMat = { {selMatAddThermalExpansion->GetHandle()} };
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(thermStressHEdit, sourceMat));

                }
            }
        }
    }
            

}
void ConvertMat::p_ConvertMatAddDamageGissmo()
{

    SelectionRead selMatAddDamageGissmo(p_lsdynaModel, "*MAT_ADD_DAMAGE_GISSMO");
    while (selMatAddDamageGissmo.Next())
    {
        HandleRead matHandle;
        selMatAddDamageGissmo->GetEntityHandle(sdiIdentifier("MID"), matHandle);

        sdiConvert::SDIHandlReadList radMatConvertedHandles;
        sdiConvert::Convert::GetConvertedHandles(matHandle, radMatConvertedHandles, destEntityType);
                
        sdiValueEntity lsdMIDEntity = GetValue<sdiValueEntity>(*selMatAddDamageGissmo, "MID");
        sdiString matAddDamageGissmoName = selMatAddDamageGissmo->GetName();

        //---
        // add /FAIL/TAB2
        //---
        HandleEdit failTAB2HEdit;
        p_radiossModel->CreateEntity(failTAB2HEdit, "/FAIL/TAB2", matAddDamageGissmoName,lsdMIDEntity.GetId());
        failTAB2HEdit.SetValue(p_radiossModel, sdiIdentifier("ID_CARD_EXIST"), sdiValue(true));
        EntityEdit failTAB2Edit(p_radiossModel, failTAB2HEdit);

        failTAB2Edit.SetValue(sdiIdentifier("mat_id"), sdiValue(sdiValueEntity(destEntityType, radMatConvertedHandles[0].GetId(p_radiossModel))));

        //-----------------------
        // NIP weight factor map
        sdiDoubleList WeightsList({ 0.5, 0.25, 0.1667, 0.125, 0.1, 0.0833 });
        sdiIntList NipList({ 2, 3, 4, 5, 6, 7 });
        map<int, double> NipWeights;
        for (int i = 0; i < NipList.size(); ++i) // loop over NIp list
        {
          NipWeights.insert(pair<int, double>(NipList[i],WeightsList[i]));
        }
        //-----------------------
        unsigned int partId = p_PartBeingConverted.GetId(p_radiossModel);
        // get elem type of part (shell/solid)

        int nbShell =0;
        int nbSolid =0;

        SelectionElementRead elemShellSelect(p_lsdynaModel, "*ELEMENT_SHELL");
        nbShell = elemShellSelect.Count();

        SelectionElementRead elemSolidSelect(p_lsdynaModel, "*ELEMENT_SOLID");
        nbSolid = elemSolidSelect.Count();

        // get NIP (nb of integration points through thickness) from *SECTION_SHELL

        HandleRead partHRead;
        p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*PART"), partId, partHRead);

        if (partHRead.IsValid())
        {
            HandleRead propHRead;
            partHRead.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propHRead);
            EntityRead propEntityRead(p_lsdynaModel, propHRead);

            int lsdelform = GetValue<int>(propEntityRead, "ELFORM");
            int lsdNIP = GetValue<int>(propEntityRead, "LSD_NIP");
            double lsdNUMFIP = GetValue<double>(*selMatAddDamageGissmo, "NUMFIP");

            if(nbShell > 0)
            {
                double radPthickfail = 0.0;
                if(lsdNUMFIP == 0.0) lsdNUMFIP = 1.0 ; // by default
                //Full-Integrated linear shell element: (elfrom = 9, 16, -16,17, 18, 20, 21, 22, 26, 30)
                if(lsdelform == 9  || lsdelform == 16 || lsdelform == -16 || 
                    lsdelform == 17 || lsdelform == 18 || lsdelform ==  20 || 
                    lsdelform == 21 || lsdelform == 22  || lsdelform == 26 || 
                    lsdelform == 30)
                {
                    if(lsdNUMFIP > 0)
                    {
                    /*    double lsdGauss = 4.0; // inplane shell integration points
                        double radVOLFRAC;
                        radVOLFRAC = abs(lsdNUMFIP)/(lsdGauss*lsdNIP);
                        failTAB2Edit.SetValue(sdiIdentifier("Volfrac"),sdiValue(radVOLFRAC));*/
                    }
                    else if(lsdNUMFIP < 0 && lsdNUMFIP >= -100)
                    {
                        radPthickfail = abs(lsdNUMFIP)/100.0;
                        failTAB2Edit.SetValue(sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                    }
                    else if(lsdNUMFIP < -100)
                    {
                        radPthickfail = (abs(lsdNUMFIP)-100.0)/100.0;
                        failTAB2Edit.SetValue(sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                    } // if(lsdNUMFIP > 0)
                }
                else if(lsdelform == 23  || lsdelform == 24)
                {
                    // higher order shell element:
                    // p_ConvertUtils.CopyValue(*selMatAddDamageGissmo, failTAB2Edit, "VOLFRAC", "Volfrac");
                }
                else
                {
                    // Reduced-Integrated linear shell element
                    double n_coef = 0.0;
                    if(lsdNUMFIP > 0)
                    {
                        n_coef = lsdNUMFIP;
                    }
                    else if(lsdNUMFIP < 0 && lsdNUMFIP >= -100)
                    {
                        n_coef = ceil((abs(lsdNUMFIP)/100.0)*lsdNIP);
                    }
                    else if(lsdNUMFIP < -100)
                    {
                        n_coef = ceil((abs(lsdNUMFIP-100.0)/100.0)*lsdNIP);
                    } // if(lsdNUMFIP > 0)
 
                    if (lsdNIP > 1) radPthickfail = (2.0*n_coef-1)*NipWeights[lsdNIP];
                    failTAB2Edit.SetValue(sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                }
            }

            if(nbSolid > 0)
            {
                if(lsdNUMFIP > 0) p_ConvertUtils.CopyValue(*selMatAddDamageGissmo, failTAB2Edit, "NUMFIP", "FAILIP");
            }

            
            HandleRead crvLCSDGHandle;
            selMatAddDamageGissmo->GetEntityHandle(sdiIdentifier("LCSDG"), crvLCSDGHandle);
            if (crvLCSDGHandle.IsValid())
            {
                EntityId crvLCSDGId = crvLCSDGHandle.GetId(p_lsdynaModel);
                failTAB2HEdit.SetValue(p_radiossModel, sdiIdentifier("EPSF_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCSDGId)));
            }
        }

        bool optECRITcurve = GetValue<bool>(*selMatAddDamageGissmo, "OPT_ECRIT_curve");

        if (optECRITcurve  == false)
        {
            p_ConvertUtils.CopyValue(*selMatAddDamageGissmo, failTAB2Edit, "ECRIT", "ECRIT");
        }
        else
        {
            // LSD_ECRIT_CURVE --> function INST_ID
             HandleRead crvECRITHandle;
            selMatAddDamageGissmo->GetEntityHandle(sdiIdentifier("ECRIT_CURVE"), crvECRITHandle);

            if (crvECRITHandle.IsValid())
            {
                EntityRead crvECRITEntRead(p_lsdynaModel, crvECRITHandle);
                EntityId crvECRITId = crvECRITEntRead.GetId();
                failTAB2HEdit.SetValue(p_radiossModel, sdiIdentifier("INST_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvECRITId)));
            }
        } // if (optECRITcurve  == false)
        
        p_ConvertUtils.CopyValue(*selMatAddDamageGissmo, failTAB2Edit, "DMGEXP", "N");
        p_ConvertUtils.CopyValue(*selMatAddDamageGissmo, failTAB2Edit, "DCRIT", "DCRIT");

        bool optFADEXPcurve = GetValue<bool>(*selMatAddDamageGissmo, "OPT_FADEXP_curve");

        if (optFADEXPcurve  == false)
        {
            p_ConvertUtils.CopyValue(*selMatAddDamageGissmo, failTAB2Edit, "FADEXP", "EXP");
        }
        else
        {
            // LSD_FADEXP_CURVE --> function FCT_EXP
            HandleRead crvFADEXPHandle;
            selMatAddDamageGissmo->GetEntityHandle(sdiIdentifier("FADEXP_CURVE"), crvFADEXPHandle);

            if (crvFADEXPHandle.IsValid())
            {
                EntityRead crvFADEXPEntRead(p_lsdynaModel, crvFADEXPHandle);
                EntityId crvFADEXPId = crvFADEXPEntRead.GetId();
                failTAB2HEdit.SetValue(p_radiossModel, sdiIdentifier("FCT_EXP"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvFADEXPId)));
            }
        } // if (optFADEXPcurve  == false)

        HandleRead crvLCREGDHandle;
        selMatAddDamageGissmo->GetEntityHandle(sdiIdentifier("LCREGD"), crvLCREGDHandle);
        if (crvLCREGDHandle.IsValid())
        {
            EntityId crvLCREGId = crvLCREGDHandle.GetId(p_lsdynaModel);
            failTAB2HEdit.SetValue(p_radiossModel, sdiIdentifier("TAB_EL"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCREGId)));
        }

        HandleRead crvLCSRSHandle;
        selMatAddDamageGissmo->GetEntityHandle(sdiIdentifier("LCSRS"), crvLCSRSHandle);
        if (crvLCSRSHandle.IsValid())
        {
            EntityId crvLCSRSId = crvLCSRSHandle.GetId(p_lsdynaModel);
            failTAB2HEdit.SetValue(p_radiossModel, sdiIdentifier("FCT_SR"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCSRSId)));
        }

        p_ConvertUtils.CopyValue(*selMatAddDamageGissmo, failTAB2Edit, "SHRF", "SHRF");
        p_ConvertUtils.CopyValue(*selMatAddDamageGissmo, failTAB2Edit, "BIAXF", "BIAXF");
        
                HandleRead crvLCDLIMHandle;
        selMatAddDamageGissmo->GetEntityHandle(sdiIdentifier("LCDLIM"), crvLCDLIMHandle);
        if (crvLCDLIMHandle.IsValid())
        {
            EntityId crvLCDLIMId = crvLCDLIMHandle.GetId(p_lsdynaModel);
            failTAB2HEdit.SetValue(p_radiossModel, sdiIdentifier("FCT_DLIM"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/FUNCT"), crvLCDLIMId)));
        }

        p_ConvertUtils.CopyValue(*selMatAddDamageGissmo, failTAB2Edit, "REFSZ", "EL_REF");
        p_ConvertUtils.CopyValue(*selMatAddDamageGissmo, failTAB2Edit, "VOLFRAC", "VOLFRAC");
        sdiConvert::SDIHandlReadList sourcehandleList = { {selMatAddDamageGissmo->GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(failTAB2HEdit, sourcehandleList));
    } // while (selMatAddDamageGissmo.Next())
}