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
#include <dyna2rad/convertparts.h>
#include <dyna2rad/convertmats.h>
#include <dyna2rad/convertprops.h>
#include <dyna2rad/propertymaterialrelation.h>
#include <dyna2rad/convertelements.h>
#include <dyna2rad/convertelementmasses.h>
#include <dyna2rad/convertcurves.h>
#include <dyna2rad/convertcards.h>
#include <dyna2rad/unitsystemdefaults.h>
#include <dyna2rad/convertsystems.h>
#include <dyna2rad/convertutils.h>
#include <dyna2rad/converttimehistory.h>
#include <dyna2rad/convertsets.h>
#include <dyna2rad/convertinivels.h>
#include <dyna2rad/convertrigids.h>
#include <dyna2rad/convertloads.h>
#include <dyna2rad/convertbcs.h>
#include <dyna2rad/convertcontrolvols.h>
#include <dyna2rad/convertboxes.h>
#include <dyna2rad/convertcontacts.h>
#include <dyna2rad/convertconstrainedjoints.h>
#include <dyna2rad/convertrwalls.h>
#include <dyna2rad/convertconstrainedspotwelds.h>
#include <dyna2rad/convertcrosssections.h>
#include <dyna2rad/convertconstrainedinterpolations.h>
#include <dyna2rad/convertconstrainednode.h>
#include <dyna2rad/convertdampings.h>
#include <dyna2rad/convertperturbations.h>
#include <dyna2rad/convertincludes.h>
#include <dyna2rad/convertnodetransforms.h>
#include <dyna2rad/convertnodes.h>
#include <dyna2rad/convertparameters.h>
#include <dyna2rad/convertfrictions.h>
#include <dyna2rad/sdiUtils.h>
#include <dyna2rad/convertinitialstresses.h>
#include <dyna2rad/convertinitialstrains.h>
#include <dyna2rad/convertdefineelementdeath.h>
#include <dyna2rad/convertinitialaxialforces.h>
#include <fstream>
#include <dyna2rad/convertdefinehexspotweldassembly.h>
#include <iostream>
#include <convertRuleParser.h>

using namespace sdiD2R;
using namespace std;
using namespace sdi;
using namespace sdiConvert;




convertRuleMap DynaToRad::p_conversionMap;

sdiConvert::ContainUIntVsUInt DynaToRad::storeLsdSDorientIdVsSkewId = {};

sdiConvert::ContainUIntVsUInt DynaToRad::storeRbodyPIDVsMasterNode = {};

sdiConvert::ContainUIntVsUInt DynaToRad::storeRigidPartVsMasterNode = {};

const sdiMessageHandler*    DynaToRad::p_pMessageHandler = nullptr;

std::map<unsigned int, sdiConvert::ContainStrVsUInt> DynaToRad::setsMappingDetails = {};

sdiConvert::ContainUIntVsUInt DynaToRad::storeLsdVIDVsRadSkewId = {};

std::vector<std::string> DynaToRad::p_parameterizedValues = {};

DynaToRad::DynaToRad(sdi::ModelViewRead* dynaModelViewSDI, sdi::ModelViewEdit* radiossModelViewSDI, sdiString& modelName,
    const sdiMessageHandler* pMessageHandler) :
    Convert("LsDyna", "Radioss", dynaModelViewSDI, radiossModelViewSDI),
    p_RunName(modelName),
    p_UseMapping(false)
{
    p_pMessageHandler = pMessageHandler;
}

void DynaToRad::SetUseSubmodelOffsets(bool useSubmodelOffsets)
{
    p_useSubmodelOffsets = useSubmodelOffsets;
}

void DynaToRad::SetDoConvertParameters(bool doConvertParameters)
{
    p_doConvertParameters = doConvertParameters;
}

void DynaToRad::SetDoCreateClassicGroups(bool doCreateClassicGroups,
    sdiConvert::ClientInfo* pClassicGroupIsUsed)
{
    p_doCreateClassicGroups = doCreateClassicGroups;
    p_pClassicGroupIsUsed = pClassicGroupIsUsed;
}

void DynaToRad::ShowMessage(const sdiMessageHandler::Level &level, int code,...)
{
    if(p_pMessageHandler)
    {
        va_list args;
        va_start(args,code);
        p_pMessageHandler->ShowMessage(level, code, args);
        va_end(args);
    }
}

void DynaToRad::GetConversionLog(LogQueryHandle& conversionLog) const
{
    conversionLog = this->conversionLog;
}

void sdiD2R::DynaToRad::PushToSetsMappingDetails(const unsigned int& LsdSetId, const sdiString& lsdKeyWord, const unsigned int& radSetId)
{
    setsMappingDetails[LsdSetId].insert(make_pair(lsdKeyWord, radSetId));
}

unsigned int sdiD2R::DynaToRad::GetRadiossSetIdFromLsdSet(const unsigned int& LsdSetId, const sdiString& keyWord)
{
    sdiConvert::ContainStrVsUInt mapingDetails = setsMappingDetails[LsdSetId];
    if (mapingDetails.size() > 1)
    {
        auto itr = find_if(mapingDetails.begin(), mapingDetails.end(),
            [&](const pair<sdiString, unsigned int>& tempPair) { return tempPair.first.find(keyWord) != string::npos; });
        if (itr != mapingDetails.end())
            return itr->second;
    }
    return LsdSetId;
}

void sdiD2R::DynaToRad::PushIntoStoreLsdVIDVsRadSkewId(const unsigned int& LsdVId, const unsigned int& radSkewId)
{
    storeLsdVIDVsRadSkewId[LsdVId] = radSkewId;
}

unsigned int sdiD2R::DynaToRad::GetRadiossSkewIdFromLsdVID(const unsigned int& LsdVId)
{
    auto itr = storeLsdVIDVsRadSkewId.find(LsdVId);
    if (itr != storeLsdVIDVsRadSkewId.end())
        return itr->second;
    return LsdVId;
}

void DynaToRad::BuildConversionMap()
{
    sdiString D2R_CFG_DIR= getenv("RAD_CFG_PATH");
    sdiString file_path = D2R_CFG_DIR + "/config/CFG/LsDynaToRad/LsDynaToRad.cfg";

    ConvertRuleParser ConversionRule(file_path);

    ConversionRule.fillMap(p_conversionMap);

    
}

const convertRuleMap&  DynaToRad::GetConversionMap()
{ 
    return p_conversionMap;
}


void DynaToRad::CallConvert()
{
    if (!p_lsdynaModel) 
        return;
    conversionLog.clear();

    /*build the conversion map */
    //BuildConversionMap();

    /*get the LsDyna unit system*/
    sdiString currentUnits;
    p_GetCurrentUnitSystem(currentUnits);
    if (currentUnits.empty())
        currentUnits = "m_s_kg";
    /*convert main unit system*/
    ConvertCard cnvrtCards(p_lsdynaModel, p_radiossModel);
    cnvrtCards.ConvertCtrlUnits();

    Units currentUnitSyst(currentUnits);
    currentUnitSyst.PopulateDefaultValues();

    /*convert includes*/
    ConvertInclude cnvrtIncludes(p_lsdynaModel, p_radiossModel, p_useSubmodelOffsets);
    cnvrtIncludes.ConvertIncludes();

    /* convert parameters if desired*/
    if(p_doConvertParameters)
    {
        ConvertParameter cnvrtParameters(p_lsdynaModel, p_radiossModel);
        cnvrtParameters.ConvertParameters();
    }

    /*convert node*/
    if(p_UseMapping == false)
    {
        ConvertNode cnvrtNodes(p_lsdynaModel, p_radiossModel);
        cnvrtNodes.ConvertNodes();
    }

    /*convert systems*/
    ConvertSystem cnvrtSysts(p_lsdynaModel, p_radiossModel);
    cnvrtSysts.ConvertSystems();

    /* convert boxes*/
    ConvertBox cnvrtBoxes(p_lsdynaModel, p_radiossModel);
    cnvrtBoxes.ConvertBoxes();

    /*convert parts*/
    ConvertPart cnvrtPrts(p_lsdynaModel, p_radiossModel);
    cnvrtPrts.ConvertParts();

    /*convert elements*/
    ConvertElem CnvrtElems(p_lsdynaModel, p_radiossModel);
    CnvrtElems.ConvertElems();

    /*convert constrained spotwelds*/
    ConvertSpotWeld cnvrtSpotWelds(p_lsdynaModel, p_radiossModel, currentUnitSyst);
    cnvrtSpotWelds.ConvertConstrainedSpotWelds();

    /*convert Sets*/
    ConvertSet convrtSets(p_lsdynaModel, p_radiossModel);
    convrtSets.ConvertSets();

    /*convert node_transform*/
    ConvertNodeTransform cnvrtNodeTransforms(p_lsdynaModel, p_radiossModel);
    cnvrtNodeTransforms.ConvertNodeTransforms();

    /*convert element_mass*/
    ConvertElemMass CnvrtElemMasses(p_lsdynaModel, p_radiossModel);
    CnvrtElemMasses.ConvertElemMasses();

    /*convert Curves*/
    ConvertCurve convrtCrvs(p_lsdynaModel, p_radiossModel);
    convrtCrvs.ConvertCurves();

    /*building material property relation*/
    PropertyMaterialRelation buildPropMatRefereces(p_lsdynaModel);
    buildPropMatRefereces.ClearMapPropMatsRelations();
    buildPropMatRefereces.ClearMapMatPropsRelations();
    buildPropMatRefereces.PopulateMapPropMatRelations();

    /*convert Materials*/
    ConvertMat convrtMats(p_lsdynaModel, p_radiossModel, buildPropMatRefereces, currentUnitSyst);
    convrtMats.ConvertMaterials();

    /*convert properties*/
    ConvertProp convrtProps(p_lsdynaModel, p_radiossModel, buildPropMatRefereces, currentUnitSyst);
    convrtProps.ConvertProperties();

    /*convert Cards*/
    cnvrtCards.ConvertCards();

    /*convert time history*/
    ConvertTimeHistory cnvrtHistory(p_lsdynaModel, p_radiossModel);
    cnvrtHistory.ConvertTimeHistories();

    /*Convert constrained nodal rigid bodies*/
    ConvertRigid cnvrtRigids(p_lsdynaModel, p_radiossModel);
    cnvrtRigids.ConvertAllRigids();

    /*convert initial velocities*/
    ConvertInitialVelocity cnvrtInivels(p_lsdynaModel, p_radiossModel);
    cnvrtInivels.ConvertInitialVelocities();

    /*convert loads*/
    ConvertLoad cnvrtLoads(p_lsdynaModel, p_radiossModel);
    cnvrtLoads.ConvertAllLoads();

    /*convert boundary conditions*/
    ConvertBcs cnvrtbcs(p_lsdynaModel, p_radiossModel);
    cnvrtbcs.ConvertAllBcs();

    /* convert control vols (airbags)*/
    ConvertControlVolume cnvrtContVols(p_lsdynaModel, p_radiossModel, currentUnitSyst);
    cnvrtContVols.ConvertControlVolumes();

    /* convert contacts*/
    ConvertContact cnvertContacts(p_lsdynaModel, p_radiossModel);
    cnvertContacts.ConvertContacts();

    /* convert frictions*/
    ConvertFriction cnvertFrictions(p_lsdynaModel, p_radiossModel);
    cnvertFrictions.ConvertAllFrictions();

    /*convert constrained joints*/
    ConvertJoint cnvrtJoints(p_lsdynaModel, p_radiossModel);
    cnvrtJoints.ConvertConstrainedJoints();

    /*convert rigidwalls*/
    ConvertRwall cnvrtRWall(p_lsdynaModel, p_radiossModel);
    cnvrtRWall.ConvertRigidwalls();

    /*convert database cross sections*/
    ConvertSection cnvrtSect(p_lsdynaModel, p_radiossModel);
    cnvrtSect.ConvertCrossSections();

    /*Convert constrained interpolations*/
    ConvertInterpolation cnvrtInterpolations(p_lsdynaModel, p_radiossModel);
    cnvrtInterpolations.ConvertConstrainedInterpolations();

    /*Convert constrained nodes*/
    ConvertConstrainedNode cnvrtConstrainedNodes(p_lsdynaModel, p_radiossModel);
    cnvrtConstrainedNodes.ConvertAllConstrainedNode();

    /*convert dampings*/
    ConvertDamping cnvrtDampings(p_lsdynaModel, p_radiossModel);
    cnvrtDampings.ConvertAllDampings();

    /*convert perturbations*/
    ConvertPerturbation cnvrtPerturbations(p_lsdynaModel, p_radiossModel);
    cnvrtPerturbations.ConvertAllPerturbations();

    /*convert initial stresses*/
    ConvertInitialStress cnvrtInitialStresses(p_lsdynaModel, p_radiossModel);
    cnvrtInitialStresses.ConvertAllInitialStresses();

    /*convert initial strains*/
    ConvertInitialStrain cnvrtInitialStrains(p_lsdynaModel, p_radiossModel);
    cnvrtInitialStrains.ConvertAllInitialStrains();

    /*convert define element death*/
    ConvertDefineElementDeath cnvrtDefineElementDeath(p_lsdynaModel, p_radiossModel);
    cnvrtDefineElementDeath.ConvertSelectedDefineElementDeath();
    
    /*convert initial axial force beam*/
    ConvertInitialAxialForce cnvrtInitialAxialForces(p_lsdynaModel, p_radiossModel);
    cnvrtInitialAxialForces.ConvertInitialAxialForces();

    p_CreateTHNodeBasedonLsdDatabaseCards();

    p_UpdateTHNodeBasedonLsdDatabaseCards();

    p_CreateTHCardsBasedonLsdDatabaseCards();

    /*automatic conversion*/
    p_AutoConvert();

    
    UpdateRunNameForCards();

    /* convert /SET to classic groups if desired*/
    if(p_doCreateClassicGroups)
    {
        ConvertRadiossSetsToGroups setConverter(p_radiossModel, p_pClassicGroupIsUsed);
        setConverter.ConvertSets();
    }
    
    /*convert define hex spotweld assembly*/
    ConvertHexSpotweld cnvrtHexSpotWelds(p_lsdynaModel, p_radiossModel);
    cnvrtHexSpotWelds.ConvertDefineHexSpotweldAssembly();
}

void DynaToRad::p_GetCurrentUnitSystem(sdiString& unitSyst)
{
    SelectionRead selCtrlUnits(p_lsdynaModel, "*CONTROL_UNITS");
    while (selCtrlUnits.Next())
    {
        sdiString len;
        sdiString time;
        sdiString mass;

        vector<reference_wrapper<sdiString>> attrValList = { len, time, mass };
        vector<sdiString> attrNameList = { "LENGTH","TIME", "MASS" };

        ConvertUtils cnvrtUtil(p_lsdynaModel, p_radiossModel);
        cnvrtUtil.GetAttribValues(*selCtrlUnits, attrNameList, attrValList);

        map<sdiString, sdiString> mapUnits;
        mapUnits["m"] = "m";
        mapUnits["mm"] = "mm";
        mapUnits["cm"] = "cm";
        mapUnits["in"] = "0.0254";
        mapUnits["foot"] = "0.3048";

        mapUnits["s"] = "s";
        mapUnits["sec"] = "s";
        mapUnits["ms"] = "ms";
        mapUnits["micro_s"] = "mus";

        mapUnits["kg"] = "kg";
        mapUnits["g"] = "g";
        mapUnits["mtrc_ton"] = "Mg";
        mapUnits["pound"] = "0.45359237";
        mapUnits["slug"] = "14.5939029372";
        mapUnits["slinch"] = "175.126835244";
        unitSyst = mapUnits[len] + "_" + mapUnits[time] + "_" + mapUnits[mass];
    }
}

void DynaToRad::UpdateRunNameForCards()
{
    size_t fPos = p_RunName.find(".");
    while (fPos != p_RunName.npos)
    {
        p_RunName.erase(fPos, 1);
        fPos = p_RunName.find(".");
    }

    vector<sdiString> cardsList({ {"/RUN"},{"/BEGIN"} });
    map <sdiString, sdiString> cardVsIdenfier;
    cardVsIdenfier["/RUN"] = "RunName";
    cardVsIdenfier["/BEGIN"] = "Runname";
    for (sdiString card : cardsList)
    {
        SelectionRead selCard(p_radiossModel, card);
        while (selCard.Next())
        {
            sdiString runName;
            sdiValue tempVal(runName);
            selCard->GetValue(sdiIdentifier(cardVsIdenfier[card]), tempVal);
            tempVal.GetValue(runName);

            if (runName.empty())
            {
                HandleEdit cardHEdit(p_radiossModel->GetEntityType(card), selCard->GetHandle().GetPointer());
                cardHEdit.SetValue(p_radiossModel, sdiIdentifier(cardVsIdenfier[card]), sdiValue(p_RunName));
            }
        }
    }
}

std::vector<std::string>& DynaToRad::GetParameterizedValues()
{
    return p_parameterizedValues;
}

void sdiD2R::DynaToRad::p_CreateTHNodeBasedonLsdDatabaseCards()
{
    /* DATABASE_BNDOUT /TH/NODE from IMPDISP, IMPACC, IMPVEL */
    /* DATABASE_SPCFORC /TH/NODE */
    ConvertUtils convertUtil(p_lsdynaModel, p_radiossModel);
    sdiStringList variables({ "REACX", "REACY", "REACZ", "REACXX", "REACYY", "REACZZ" });
    sdiStringList dbList({ "BNDOUT", "SPCFORC" });
    vector<sdiStringList> boundaryCardsList({ { {"/IMPDISP","/IMPVEL", "/IMPACC"} }, { {"/BCS"} } });

    for (size_t i = 0; i < 2; ++i)
    {
        SelectionRead selDbCard(p_lsdynaModel, "*DATABASE_" + dbList[i]);
        if (selDbCard.Count())
        {
            sdiUIntList allExtratedNodes;
            for (sdiString bndCard : boundaryCardsList[i])
            {
                sdiString grnodStr = (bndCard == "/IMPDISP") ? "Gnod_id" : "grnod_ID";
                SelectionRead selRadImpCard(p_radiossModel, bndCard);
                while (selRadImpCard.Next())
                {
                    HandleRead grnodHread;
                    selRadImpCard->GetEntityHandle(sdiIdentifier(grnodStr), grnodHread);
                    sdiUIntList nodeids;
                    convertUtil.ExtractNodesFromRadiossSet(grnodHread, nodeids);
                    if (!nodeids.empty())
                    {
                        for (unsigned int nodeId : nodeids)
                            allExtratedNodes.push_back(nodeId);
                    }
                }
            }
            sdiVectorSort(allExtratedNodes);
            sdiVectorUnique(allExtratedNodes);
            if (!allExtratedNodes.empty())
            {
                HandleEdit thNodeHEdit;
                p_radiossModel->CreateEntity(thNodeHEdit, "/TH/NODE", "TH_NODE_" + dbList[i]);
                EntityEdit  thNodeEdit(p_radiossModel, thNodeHEdit);
                thNodeEdit.SetValue(sdiIdentifier("idsmax"), sdiValue((int)allExtratedNodes.size()));
                thNodeEdit.SetValue(sdiIdentifier("elem_ID"), sdiValue(sdiValueEntityList(1, allExtratedNodes)));
                thNodeEdit.SetValue(sdiIdentifier("Number_Of_Variables"), sdiValue((int)variables.size()));
                thNodeEdit.SetValue(sdiIdentifier("var"), sdiValue(variables));
            }
        }
    }
}

void sdiD2R::DynaToRad::p_UpdateTHNodeBasedonLsdDatabaseCards()
{
    /* DATABASE_TPRINT --> /TH/NODE, /ANIM/ELEM, /ANIM/NODA --> insert "TEMP" variable*/
    ConvertUtils convertUtil(p_lsdynaModel, p_radiossModel);
    sdiString dboption = "TPRINT";

    SelectionRead selDbCard(p_lsdynaModel, "*DATABASE_TPRINT");
    HandleEdit radCard;
    while (selDbCard.Next())
    {
        // insert "TEMP" variable in engine ANIM output
        //---
        p_radiossModel->CreateEntity(radCard, "/ANIM/NODA");
        radCard.SetValue(p_radiossModel, sdiIdentifier("TEMP"), sdiValue(1));
        
        p_radiossModel->CreateEntity(radCard, "/ANIM/ELEM");
        radCard.SetValue(p_radiossModel, sdiIdentifier("TEMP"), sdiValue(1));

        // insert "TEMP" variable in engine TH output
        //---
        vector<sdiString> THCardsList({ {"/TH/NODE","/TH/BRIC","TH/SPHCEL","/TH/TRIA","/TH/QUAD"} });
        for (sdiString bndCard : THCardsList)
        {
            SelectionRead selRadTHCard(p_radiossModel, bndCard);
            if(selRadTHCard.Count())
            {
                while (selRadTHCard.Next())
                {
                    HandleEdit THradHEdit;
                    EntityType radTHEntityType = p_radiossModel->GetEntityType(bndCard);
                    p_radiossModel->FindById(radTHEntityType, selRadTHCard->GetId(), THradHEdit);
                    EntityEdit THradEdit(p_radiossModel, THradHEdit);
              
                    int nvar = GetValue<int>(*selRadTHCard, "Number_Of_Variables");
                    int counttemp = 0;
                    sdiString newvar;
                    sdiStringList varList;
                    for (size_t i = 0; i < nvar; ++i)
                    {
                        sdiString var = GetValue<sdiString>(*selRadTHCard, "var", i);
                        varList.push_back(var);
                        if(var == "TEMP") counttemp++;
                    }
                    if(counttemp == 0)
                    {
                        newvar = "TEMP";
                        varList.push_back(newvar);
                        THradHEdit.SetValue(p_radiossModel,sdiIdentifier("Number_Of_Variables"), sdiValue(nvar+1));
                        THradHEdit.SetValue(p_radiossModel,sdiIdentifier("var"), sdiValue(varList));
                    }
                }
            }
        }
    }
}

void sdiD2R::DynaToRad::p_CreateTHCardsBasedonLsdDatabaseCards()
{
    /* *DATABASE_DEFORC /TH/SPRING mat_S01 to mat_S08 */
    /* *DATABASE_DISBOUT /TH/SPRING MAT66, 67, 68, 74, 119, 121, 196 */
    /* *DATABASE_JNTFORC /TH/SPRING joints prop45 radioss */
    /* *DATABASE_SWFORC /TH/SPRING /spring prop45 radioss */
    EntityType radSprType = p_radiossModel->GetEntityType("/SPRING");
    sdiStringList lsdSpringMats
    (
        {
            "*MAT_SPRING_ELASTIC",
            "*MAT_DAMPER_VISCOUS",
            "*MAT_SPRING_ELASTOPLASTIC",
            "*MAT_SPRING_NONLINEAR_ELASTIC",
            "*MAT_DAMPER_NONLINEAR_VISCOUS",
            "*MAT_SPRING_GENERAL_NONLINEAR",
            "*MAT_SPRING_MAXWELL",
            "*MAT_SPRING_INELASTIC",
            "*MAT_S01",
            "*MAT_S02",
            "*MAT_S03",
            "*MAT_S04",
            "*MAT_S05",
            "*MAT_S06",
            "*MAT_S08"
        }
    );
    sdiStringList lsdDiscBeamMats
    (
        {
            "*MAT_LINEAR_ELASTIC_DISCRETE_BEAM",
            "*MAT_NONLINEAR_ELASTIC_DISCRETE_BEAM",
            "*MAT_NONLINEAR_PLASTIC_DISCRETE_BEAM",
            "*MAT_CABLE_DISCRETE_BEAM",
            "*MAT_ELASTIC_SPRING_DISCRETE_BEAM",
            "*MAT_GENERAL_NONLINEAR_6DOF_DISCRETE_BEAM",
            "*MAT_GENERAL_NONLINEAR_1DOF_DISCRETE_BEAM",
            "*MAT_066",
            "*MAT_067",
            "*MAT_068",
            "*MAT_071",
            "*MAT_074",
            "*MAT_119",
            "*MAT_121"
        }
    );
    sdiStringList radPropTye45
    (
        {
            "/PROP/TYPE45"
        }
    );
    sdiStringList lsdMatSpotWelds
    (
        {
            "*MAT_SPOTWELD",
            "*MATL100"
        }
    );

    vector<sdiStringList> vectStrList({ {lsdSpringMats} , {lsdDiscBeamMats}, {radPropTye45}, {lsdMatSpotWelds}, {lsdMatSpotWelds} });

    vector<sdiString> dbCardList({ "DEFORC" , "DISBOUT", "JNTFORC", "SWFORC", "SWFORC" });

    vector<ModelViewRead*> mvPointerList({ p_lsdynaModel , p_lsdynaModel, p_radiossModel, p_lsdynaModel, p_lsdynaModel });

    EntityType radPropEnityType = p_radiossModel->GetEntityType("/PROP");
    EntityType lsdMatEnityType = p_lsdynaModel->GetEntityType("*MAT");
    vector<EntityType> entityTypeList({ lsdMatEnityType , lsdMatEnityType , radPropEnityType, lsdMatEnityType, lsdMatEnityType });

    sdiStringList identifierNames({ "MID" , "MID" , "prop_ID", "MID", "MID" });

    sdiStringList partCardList({ "*PART" , "*PART" , "/PART", "*PART", "*PART" });

    ConvertUtils convertUtil(p_lsdynaModel, p_radiossModel);

    sdiString thCard = "/TH/SPRING";

    sdiString thName = "TH_SPRING_";

    for (size_t i = 0; i < dbCardList.size(); ++i)
    {
        SelectionRead selDbCard(p_lsdynaModel, "*DATABASE_" + dbCardList[i]);
        ModelViewRead*  mvPtr= mvPointerList[i];
        if (selDbCard.Count())
        {
            sdiUIntList elemList;
            for (sdiString entityCard : vectStrList[i])
            {
                SelectionRead selEntity(mvPtr, entityCard);
                while (selEntity.Next())
                {
                    SelectionRead compSelect(mvPtr, partCardList[i],
                        FilterValue(sdiIdentifier(identifierNames[i]), sdiValue(sdiValueEntity(entityTypeList[i], selEntity->GetId()))));
                    while (compSelect.Next())
                    {
                        if (i == 2)
                        {
                            SelectionElementRead sprElem(p_radiossModel, "/SPRING");
                            while (sprElem.Next())
                            {
                                if (sprElem->GetComponentId() == compSelect->GetId())
                                    elemList.push_back(sprElem->GetId());
                            }
                        }
                        else
                        {
                            SelectionElementRead selElem(*compSelect);
                            while (selElem.Next())
                            {
                                if (i == 3)
                                {
                                    sdiString elemKeyWord = selElem->GetKeyword();
                                    if (elemKeyWord == "*ELEMENT_DISCRETE" || elemKeyWord == "*ELEMENT_BEAM")
                                        elemList.push_back(selElem->GetId());
                                }
                                else if (i == 4)
                                {
                                    sdiString elemKeyWord = selElem->GetKeyword();
                                    if (elemKeyWord == "*ELEMENT_SOLID")
                                        elemList.push_back(selElem->GetId());
                                }
                                else
                                    elemList.push_back(selElem->GetId());
                            }
                        }
                    }
                }
            }
            if (!elemList.empty())
            {
                HandleEdit thSprHEdit;
                if (i == 4)
                {
                    thCard = "/TH/BRIC";
                    thName = "TH_BRIC_";
                }
                p_radiossModel->CreateEntity(thSprHEdit, thCard, thName + dbCardList[i]);
                EntityEdit  thSprEdit(p_radiossModel, thSprHEdit);
                thSprEdit.SetValue(sdiIdentifier("idsmax"), sdiValue((int)elemList.size()));
                thSprEdit.SetValue(sdiIdentifier("elem_ID"), sdiValue(sdiValueEntityList(radSprType, elemList)));
                thSprEdit.SetValue(sdiIdentifier("Number_Of_Variables"), sdiValue(1));
                thSprEdit.SetValue(sdiIdentifier("var"), sdiValue(sdiStringList({ "DEF" })));
            }
        }
    }

}


void DynaToRad::PrintLog(LogQueryHandle& conversionLog) const
{
    ofstream logFile;
    logFile.open("ConversionMessage_" + p_RunName + ".txt");
    logFile << "************************************************************************\n";
    logFile << "*KEYWORD MAPPING TO RADIOSS PROCESS \n";
    logFile << "************************************************************************\n\n";

    if (conversionLog.size())
    {
        for (auto readPair : conversionLog)
        {
            auto vectItrObj = readPair.second;
            for (auto itrObj : vectItrObj)
            {
                if (itrObj.IsValid())
                {
                    EntityRead itrEntityObj(p_lsdynaModel, itrObj);
                    logFile << itrEntityObj.GetKeyword();
                    logFile << " (" << itrEntityObj.GetId() << ")  " << itrEntityObj.GetName() << "\n";
                    logFile << "    is mapped to\n";
                    auto itrObj2 = readPair.first;
                    EntityRead itrEntityObj2(p_radiossModel, itrObj2);
                    logFile << "        ";
                    logFile << itrEntityObj2.GetKeyword();
                    logFile << " (" << itrEntityObj2.GetId() << ")  " << itrEntityObj2.GetName() << "\n";
                    logFile << endl;
                    break;
                }
            }
        }
    }
    logFile.close();
}


void sdiD2R::DynaToRad::p_AutoConvert()
{
    
    for (unsigned int i = 0; i < p_lsdynaModel->GetMaxEntityType(); ++i)
    {
        if(!(p_lsdynaModel->GetKeyword(i).size() == 0 || 
        p_lsdynaModel->GetKeyword(i).find("NODE") != p_lsdynaModel->GetKeyword(i).npos|| 
        p_lsdynaModel->GetKeyword(i).find("ELEMENT") != p_lsdynaModel->GetKeyword(i).npos|| 
        p_lsdynaModel->GetKeyword(i).find("SECTION") != p_lsdynaModel->GetKeyword(i).npos || 
        p_lsdynaModel->GetKeyword(i).find("CONTROL") != p_lsdynaModel->GetKeyword(i).npos ||
        p_lsdynaModel->GetKeyword(i).find("MAT") != p_lsdynaModel->GetKeyword(i).npos ||
        p_lsdynaModel->GetKeyword(i).find("EOS") != p_lsdynaModel->GetKeyword(i).npos ||
        p_lsdynaModel->GetKeyword(i).find("INCLUDE") != p_lsdynaModel->GetKeyword(i).npos ||
        p_lsdynaModel->GetKeyword(i).find("PARAMETER") != p_lsdynaModel->GetKeyword(i).npos ||
        p_lsdynaModel->GetKeyword(i).find("DATABASE") != p_lsdynaModel->GetKeyword(i).npos  ))
        {
            ConvertUtils cnvrtUtil(p_lsdynaModel, p_radiossModel);
            SelectionRead selCard(p_lsdynaModel, p_lsdynaModel->GetKeyword(i).c_str());

            while (selCard.Next())
            {
                sdiConvert::SDIHandlReadList radConvertedHandles;
                sdiConvert::Convert::GetConvertedHandles(selCard->GetHandle(), radConvertedHandles);
                sdiString elemKeyWord = selCard->GetKeyword();
                string Keyword = elemKeyWord.c_str();
                std::replace( Keyword.begin(), Keyword.end(), '*', '/'); 
            
                sdiString newKeyWord = Keyword;

                if(radConvertedHandles.size() == 0)
                {
                    
                    //printf("%d selCard __%s__ newKeyWord __%s__ \n",p_lsdynaModel->GetKeyword(i).npos,Keyword.c_str(),newKeyWord.c_str());
                    cnvrtUtil.Convert1To1(selCard,newKeyWord);
                }
            }
        }
    }


}