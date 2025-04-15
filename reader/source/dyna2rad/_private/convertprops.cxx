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
#include <dyna2rad/convertprops.h>
#include <dyna2rad/propertymaterialrelation.h>
#include <dyna2rad/dynamatlawkeywordmap.h>
#include <dyna2rad/sdiUtils.h>

#ifndef M_PI
#       define M_PI 3.14159265358979323846
#endif

using namespace sdiD2R;
using namespace std;
using namespace sdi;
using namespace sdiConvert;

static size_t matReferenceCount = 0;

static void HandleCurveLCFD(ModelViewRead*& dynaModel, const EntityRead& matEntityRead, sdiDoubleList& crvPoints);

static void UpdateSystemForOrthPropFromDynaMat(ModelViewRead* lsdModel, ModelViewEdit*radModel, const EntityRead& matEntityRead, ConvertUtils& convertUtil, EntityEdit& radPropEntEdit);

void ConvertProp::ConvertProperties()
{
    ConvertEntities();
    CreateDefSolidForRadSolidProp();
}

void ConvertProp::ConvertEntities()
{

    bool isControlHourglass = false;
    int lsdControlHourglassIHQ = 0;
    int lsdHourglassIHQ = 0;

    sdiValue queryValue(lsdControlHourglassIHQ);
    SelectionRead controlHourglassSelect(p_lsdynaModel, "*CONTROL_HOURGLASS");
    if (controlHourglassSelect.Count() > 0 )
    {
        isControlHourglass = true;
        controlHourglassSelect.Next();
        HandleRead controlHourglassHRead = controlHourglassSelect->GetHandle();
        EntityRead controlHourglassEntRead(p_lsdynaModel, controlHourglassHRead);
        controlHourglassEntRead.GetValue(sdiIdentifier("LSD_IHQ"), queryValue);
        queryValue.GetValue(lsdControlHourglassIHQ);
    }

    SelectionRead partsSelect(p_lsdynaModel, "*PART");
    while (partsSelect.Next())
    {
        sdiString partName = partsSelect->GetName();
        EntityId partId = partsSelect->GetId();

        HandleEdit radEditPart;
        HandleEdit radPropEdit;
        EntityType radPartEntityType = p_radiossModel->GetEntityType("/PART");
        p_radiossModel->FindById(radPartEntityType, partId, radEditPart);


        HandleRead partHRead;
        p_PartBeingConverted = partsSelect->GetHandle();

        EntityRead partEntRead(p_lsdynaModel, p_PartBeingConverted);
        sdiString partCard = partEntRead.GetKeyword();
        sdiString destCard;  
        if( partCard.find("COMPOSITE") != string::npos)
        {
            if (radEditPart.IsValid())
            {
                EntityRead partEntRead(p_lsdynaModel, p_PartBeingConverted);
                p_ConvertUtils.PropagateInclude(partEntRead);
                p_ConvertPartComposites(partEntRead, radPropEdit);

                if (radPropEdit.IsValid())
                    radEditPart.SetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), radPropEdit);
            }
        }
        else
        {
            if (radEditPart.IsValid())
            {
                HandleRead propHRead;
                partsSelect->GetEntityHandle(sdiIdentifier("SECID"), propHRead);

                if (propHRead.IsValid())
                {
                    EntityRead propEntRead(p_lsdynaModel, propHRead);
                    sdiString propCard = propEntRead.GetKeyword();
                    if( propCard.find("*SECTION") != string::npos)
                    {
                        p_ConvertUtils.PropagateInclude(propEntRead);
                        p_ConvertPropBasedOnCard(propEntRead, propCard, radPropEdit, destCard);
                    }
                    else
                    {
                        HandleRead propHRead1;
                        sdiValueEntity propEntity;
                        sdiValue tempVal(propEntity);
                        partsSelect->GetValue(sdiIdentifier("SECID"), tempVal);
                        tempVal.GetValue(propEntity);
                        p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*SECTION"), propEntity.GetId(), propHRead1);
                        if (propHRead1.IsValid())
                        {
                            EntityRead propEntRead1(p_lsdynaModel, propHRead1);
                            sdiString propCard1 = propEntRead1.GetKeyword();
                            p_ConvertUtils.PropagateInclude(propEntRead1);
                            p_ConvertPropBasedOnCard(propEntRead1, propCard1, radPropEdit, destCard);
                        }
                    }
                }

                if (radPropEdit.IsValid())
                {
                    EntityEdit radPartEntEdit(p_radiossModel, radEditPart);
                    radPartEntEdit.SetEntityHandle(sdiIdentifier("prop_ID"), radPropEdit);
                    //radEditPart.SetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), radPropEdit);
                }
                else
                    radEditPart.SetValue(p_radiossModel, sdiIdentifier("prop_ID"), sdiValue(sdiValueEntity(sdiValueEntityType(destEntityType), 0)));
            }

            bool isHourglass = false;
            int lsdHourglassIHQ = 0;

            HandleRead hourglassHRead;
            sdiValueEntity hourglassEntity;
            sdiValue tempVal(hourglassEntity);
            partsSelect->GetValue(sdiIdentifier("HGID"), tempVal);
            tempVal.GetValue(hourglassEntity);
            p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*HOURGLASS"), hourglassEntity.GetId(), hourglassHRead);
            if (hourglassHRead.IsValid())
            {
                EntityRead hourglassEntRead(p_lsdynaModel, hourglassHRead);
                hourglassEntRead.GetValue(sdiIdentifier("IHQ"), queryValue);
                queryValue.GetValue(lsdHourglassIHQ);
                isHourglass = true;
            }
                
            if(isHourglass || isControlHourglass)
            {
                sdiString matCard ;
                sdiString propCard = destCard.c_str();
                int elform = 0;
                HandleRead propHRead;
                partsSelect->GetEntityHandle(sdiIdentifier("SECID"), propHRead);

                if (propHRead.IsValid())
                {
                    EntityRead propEntRead(p_lsdynaModel, propHRead);
                    propEntRead.GetValue(sdiIdentifier("LSD_ELFORM"), queryValue);
                    queryValue.GetValue(elform);
                }
                
                unsigned int partId = p_PartBeingConverted.GetId(p_lsdynaModel);
                HandleEdit radParHEdit;
                p_radiossModel->FindById(p_radiossModel->GetEntityType("/PART"), partId, radParHEdit);
                if (radParHEdit.IsValid())
                {
                    HandleRead matHread;
                    radParHEdit.GetEntityHandle(p_radiossModel,sdiIdentifier("mat_ID"), matHread);
                    if (matHread.IsValid())
                    {
                         EntityRead radMat(p_radiossModel, matHread);
                         matCard = radMat.GetKeyword();
                    }
                }

                EntityEdit radPropEntityEdit(p_radiossModel, radPropEdit);
                if(isHourglass)
                {
                    HandleRead hourglassHRead;
                    sdiValueEntity hourglassEntity;
                    sdiValue tempVal(hourglassEntity);
                    partsSelect->GetValue(sdiIdentifier("HGID"), tempVal);
                    tempVal.GetValue(hourglassEntity);
                    p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*HOURGLASS"), hourglassEntity.GetId(), hourglassHRead);
                    if (hourglassHRead.IsValid())
                    {
                        EntityRead hourglassEntRead(p_lsdynaModel, hourglassHRead);
                        if (propCard.compare(0,9,"/PROP/SPH")) p_ConvertUtils.CopyValue(hourglassEntRead, radPropEntityEdit, "QM", "h");
                    }
                }
                else if (isControlHourglass)
                {
                    HandleRead controlHourglassHRead = controlHourglassSelect->GetHandle();
                    EntityRead controlHourglassEntRead(p_lsdynaModel, controlHourglassHRead);
                    if (propCard.compare(0,9,"/PROP/SPH")) p_ConvertUtils.CopyValue(controlHourglassEntRead, radPropEntityEdit, "QH", "h");
                }
                
                if (!propCard.compare(0,12,"/PROP/TYPE14") && elform != 2 )
                {
                    
                    if (isControlHourglass && (lsdControlHourglassIHQ == 1 ||
                                               lsdControlHourglassIHQ == 2 ||
                                               lsdControlHourglassIHQ == 3)) 
                        radPropEntityEdit.SetValue(sdiIdentifier("ISOLID"), sdiValue(1));

                    if (isControlHourglass && (lsdControlHourglassIHQ == 4 ||
                                               lsdControlHourglassIHQ == 5 ||
                                               lsdControlHourglassIHQ == 6 ||
                                               lsdControlHourglassIHQ == 7))
                        radPropEntityEdit.SetValue(sdiIdentifier("ISOLID"), sdiValue(5));
                    //* (*HOURGLASS if referenced in the *PART overwrites *CONTROL_HOURGLASS)
                    
                    if(isHourglass && (lsdHourglassIHQ == 1 ||
                                       lsdHourglassIHQ == 2 ||
                                       lsdHourglassIHQ == 3))
                        radPropEntityEdit.SetValue(sdiIdentifier("ISOLID"), sdiValue(1));

                    if(isHourglass && (lsdHourglassIHQ == 4 ||
                                       lsdHourglassIHQ == 5 ||
                                       lsdHourglassIHQ == 6 ||
                                       lsdHourglassIHQ == 7 ))
                        radPropEntityEdit.SetValue(sdiIdentifier("ISOLID"), sdiValue(5));
                }
                else
                {
                    // to be implemented (as new specifications for *CONTROL_HOURGLASS and *HOURGLASS)
                }
            }
        }
    }

}

void ConvertProp::p_ConvertPropBasedOnCard(const EntityRead& dynaProp, const sdiString& sourceCard, sdi::HandleEdit& radProp,sdiString& destCard)
{
    EntityId dynaPropId = dynaProp.GetId();
    SDIEntityReadList matReferences;
    p_PropMatRelationDB.GetReferenceList(srcEntityType, dynaPropId, matReferences);
    matReferenceCount = matReferences.size();

    if (find(p_convertedProps.begin(), p_convertedProps.end(), dynaPropId) != p_convertedProps.end())
    {
        p_radiossModel->FindById(destEntityType, dynaPropId, radProp);
        EntityEdit radPropEdit(p_radiossModel, radProp);
        destCard = radPropEdit.GetKeyword();
        return;
    }
    else
    {
        size_t pos = sourceCard.find("_TITLE");
        sdiString keyword = sourceCard;
        if (pos != string::npos)
        {
            keyword.erase(pos);
        }
        HandleRead matHandle;
        p_PartBeingConverted.GetEntityHandle(p_lsdynaModel, sdiIdentifier("MID"), matHandle);
        if (matHandle.IsValid())
        {
            sdiString dynaPropName = dynaProp.GetName();
            EntityRead matEntityRead(p_lsdynaModel, matHandle);
            sdiString matCard = matEntityRead.GetKeyword();
            size_t pos = matCard.find("_TITLE");
            if (pos != string::npos)
            {
                matCard.erase(pos);
            }
            unsigned short int matLawNum = dynaMatLawMap[matCard];
            if (keyword == "*SECTION_SHELL")
            {
                p_ConvertSectionShell(matEntityRead, dynaProp, sourceCard, destCard, radProp);
            }
            else if (keyword == "*SECTION_TSHELL")
            {
                p_ConvertSectionTShell(matEntityRead, dynaProp, sourceCard, destCard, radProp);
            }
            else if (keyword == "*SECTION_BEAM")
            {
                p_ConvertSectionBeam(matEntityRead, dynaProp, destCard, radProp);
            }
            else if (keyword == "*SECTION_DISCRETE")
            {
                if (matCard.find("*MAT_SPRING_MUSCLE") != string::npos || matCard.find("*MAT_S15") != string::npos)
                    p_ConvertSectionDiscreteMuscle(matEntityRead, dynaProp, destCard, radProp);
                else
                    p_ConvertSectionDiscrete(matEntityRead, dynaProp, destCard, radProp);
            }
            else if (keyword == "*SECTION_SOLID")
            {
                int isolid = 0;
                int elform = 0;
                sdiValue queryValue(elform);
                dynaProp.GetValue(sdiIdentifier("LSD_ELFORM"), queryValue);
                queryValue.GetValue(elform);

                if (matCard.find("*MAT_NULL") != string::npos || matCard.find("*MAT_009") != string::npos)
                {
                    destCard = "/PROP/TYPE14";
                    isolid = 1;
                }
                else if ((matCard.find("*MAT_SPOTWELD") != string::npos) || 
                         (matCard.find("*MAT_ARUP_ADHESIVE") != string::npos) ||
                         (matCard.find("*MAT_COHESIVE_MIXED_MODE_ELASTOPLASTIC_RATE") != string::npos) ||
                         (matCard.find("*MAT_COHESIVE_MIXED_MODE") != string::npos)|| 
                         (matCard.find("*MAT_100") != string::npos) ||
                         (matCard.find("*MAT_169") != string::npos) ||
                         (matCard.find("*MAT_240") != string::npos) ||
                         (matCard.find("*MAT_138") != string::npos))
                {
                    destCard = "/PROP/CONNECT";
                }
                else
                {
                    if (elform == 1 || elform == 0 || elform == -1)
                        isolid = 24;
                    else if (elform == 2 || elform == 3)
                    {
                        isolid = 18;
                    }
                    if (matCard.find("*MAT_HONEYCOMB") != string::npos ||
                        matCard.find("*MAT_ORTHOTROPIC_ELASTIC") != string::npos ||
                        matCard.find("*MAT_MODIFIED_HONEYCOMB") != string::npos ||
                        matCard.find("*MAT_LAMINATED_COMPOSITE_FABRIC_SOLID") != string::npos ||
                        matCard.find("*MAT_026") != string::npos ||
                        matCard.find("*MAT_002") != string::npos ||
                        matCard.find("*MAT_126") != string::npos ||
                        matCard.find("*MAT_058") != string::npos)
                    {
                        destCard = "/PROP/TYPE6";
                        isolid = 1;
                    }
                    else
                        destCard = "/PROP/TYPE14";
                }
                if (matReferenceCount > 1)
                    p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
                else
                    p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);
                EntityEdit radPropEdit(p_radiossModel, radProp);
                radPropEdit.SetValue(sdiIdentifier("ISOLID"), sdiValue(isolid));
                if(matLawNum != 26 && matLawNum != 126 && matLawNum != 2)
                   UpdateSystemForOrthPropFromDynaMat(p_lsdynaModel, p_radiossModel, matEntityRead, p_ConvertUtils, radPropEdit);

                size_t pos = matCard.find("_TITLE");
                if (pos != string::npos)
                {
                    matCard.erase(pos);
                }
                if (matLawNum == 181)
                {
                    double lsdKM = 0.0;
                    double lsdG = 0.0;
                    double nu;

                    sdiValue tempVal(lsdKM);
                    matEntityRead.GetValue(sdiIdentifier("KM"), tempVal);
                    tempVal.GetValue(lsdKM);

                    tempVal = sdiValue(lsdG);
                    matEntityRead.GetValue(sdiIdentifier("G"), tempVal);
                    tempVal.GetValue(lsdG);

                    if (lsdKM > 0.0 || lsdG > 0.0)
                    {
                        nu = (3 * lsdKM - 2 * lsdG) / (6 * lsdKM + 2 * lsdG);
                        if (nu >= 0.25)
                        {
                            radPropEdit.SetValue(sdiIdentifier("ISOLID"), sdiValue(5));
                            radPropEdit.SetValue(sdiIdentifier("Ihkt"), sdiValue(2));
                        }
                        else
                            radPropEdit.SetValue(sdiIdentifier("ISOLID"), sdiValue(1));
                    }

                    double lsdMU = 0.0;
                    tempVal = sdiValue(lsdMU);
                    matEntityRead.GetValue(sdiIdentifier("MU"), tempVal);
                    tempVal.GetValue(lsdMU);
                    lsdMU = (lsdMU == 0.0) ? 0.05 : lsdMU;

                    radPropEdit.SetValue(sdiIdentifier("Mu"), sdiValue(lsdMU));
                }
                else if (matLawNum == 138 || matLawNum == 240 || matLawNum == 2 || matLawNum == 26 || matLawNum == 126 || matLawNum == 58)
                {
                    radPropEdit.SetValue(sdiIdentifier("Ismstr"), sdiValue(1));
                    if (matLawNum == 26 || matLawNum == 126 || matLawNum == 2 || matLawNum == 58)
                        ConvertSolidOrthType6(matEntityRead, dynaProp, destCard, radProp);
                }
                else if (matLawNum == 57 || matLawNum == 83)
                {
                    double lsdDAMP = 0.0;
                    sdiValue tempVal(lsdDAMP);
                    matEntityRead.GetValue(sdiIdentifier("DAMP"), tempVal);
                    tempVal.GetValue(lsdDAMP);
                    lsdDAMP = (lsdDAMP == 0.0) ? 0.05 : lsdDAMP;

                    radPropEdit.SetValue(sdiIdentifier("Mu"), sdiValue(lsdDAMP));
                }
                if (matCard.find("*MAT_LOW_DENSITY_VISCOUS_FOAM") != string::npos || matCard.find("*MAT_073") != string::npos)
                {
                    double lsdDAMP = 0.0;
                    sdiValue tempVal(lsdDAMP);
                    matEntityRead.GetValue(sdiIdentifier("DAMP"), tempVal);
                    tempVal.GetValue(lsdDAMP);
                    lsdDAMP = (lsdDAMP == 0.0) ? 0.05 : lsdDAMP;

                    radPropEdit.SetValue(sdiIdentifier("Mu"), sdiValue(lsdDAMP));
                    radPropEdit.SetValue(sdiIdentifier("Lambda"), sdiValue(lsdDAMP/3.0));
                    radPropEdit.SetValue(sdiIdentifier("Ismstr"), sdiValue(10));
                }
            }
            else if (keyword == "*SECTION_SEATBELT")
            {
                //p_ConvertSectionSeatbelt(matEntityRead, dynaProp, destCard, radProp);
                p_ConvertSectionSeatbeltToProp23(matEntityRead, dynaProp, destCard, radProp);
            }
            else if (keyword == "*SECTION_SPH")
            {
                p_ConvertSectionSph(matEntityRead, dynaProp, destCard, radProp);
            }
            else if (keyword.find("*SECTION") != string::npos)
            {
                if(!sdiVectorContains(p_convertedProps,dynaPropId))
                {
                    DynaToRad::ShowMessage(sdiMessageHandler::Level::Error, 1,
                        sourceCard.c_str(), dynaPropId, dynaPropName.c_str());
                    p_convertedProps.push_back(dynaPropId);
                }
            }
            if (matReferenceCount > 1)
                p_PropMatRelationDB.UpdateReferenceList(srcEntityType, dynaPropId, EntityRead(p_lsdynaModel, matHandle));
            else if (matReferenceCount == 1)
                p_convertedProps.push_back(dynaPropId);
            if (radProp.IsValid())
            {
                SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(radProp, sourceProps));
            }
        }
    }
}

void ConvertProp::p_SetDefaultValuesForTYPE13(HandleEdit& radProp)
{
    EntityEdit radPropEntity(p_radiossModel, radProp);

    map<sdiString, double> mapAttribNameVsAttribVal =
    {
        {"Mass", p_CurrentUnitSyst.GetDefaultBeamMass()},
        {"Inertia", p_CurrentUnitSyst.GetDefaultBeamInertia()},
        {"Ifail2", 0.0},
        {"K1",0.0},
        {"K2",0.0},
        {"K3",0.0},
        {"K4",0.0},
        {"K5",0.0},
        {"K6",0.0},
        {"C1",0.00},
        {"C2",0.00},
        {"C3",0.00},
        {"C4",0.00},
        {"C5",0.00},
        {"C6",0.00},
        {"A1",1.00},
        {"A2",1.00},
        {"A3",1.00},
        {"A4",1.00},
        {"A5",1.00},
        {"A6",1.00},
        {"B1",0.00},
        {"B2",0.00},
        {"B3",0.00},
        {"B4",0.00},
        {"B5",0.00},
        {"B6",0.00},
        {"D1",0.00},
        {"D2",0.00},
        {"D3",0.00},
        {"D4",0.00},
        {"D5",0.00},
        {"D6",0.00},
        {"DeltaMax1",1.00E+30},
        {"DeltaMax2",1.00E+30},
        {"DeltaMax3",1.00E+30},
        {"DeltaMax4",1.00E+30},
        {"DeltaMax5",1.00E+30},
        {"DeltaMax6",1.00E+30},
        {"DeltaMin1",-1.00E+30},
        {"DeltaMin2",-1.00E+30},
        {"DeltaMin3",-1.00E+30},
        {"DeltaMin4",-1.00E+30},
        {"DeltaMin5",-1.00E+30},
        {"DeltaMin6",-1.00E+30},
        {"Hscale1",1.00},
        {"Hscale2",1.00},
        {"Hscale3",1.00},
        {"Hscale4",1.00},
        {"Hscale5",1.00},
        {"Hscale6",1.00},
        {"Vo",1.00},
        {"Wo",1.00},
        {"Fcut",1E+30},
        {"Fsmooth",0.00},
        {"c1",0.00},
        {"c2",0.00},
        {"c3",0.00},
        {"c4",0.00},
        {"c5",0.00},
        {"c6",0.00},
        {"n1",0.00},
        {"n2",0.00},
        {"n3",0.00},
        {"n4",0.00},
        {"n5",0.00},
        {"n6",0.00},
        {"alpha1",1.00},
        {"alpha2",1.00},
        {"alpha3",1.00},
        {"alpha4",1.00},
        {"alpha5",1.00},
        {"alpha6",1.00},
        {"beta1",2.00},
        {"beta2",2.00},
        {"beta3",2.00},
        {"beta4",2.00},
        {"beta5",2.00},
        {"beta6",2.00},
    };

    for (const auto valueStrPair : mapAttribNameVsAttribVal)
    {
        radPropEntity.SetValue(sdiIdentifier(valueStrPair.first), sdiValue(valueStrPair.second));
    }
}

void ConvertProp::p_ConvertSectionShell(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, const sdiString& sourceCard, sdiString& destCard, sdi::HandleEdit& radProp)
{
    double lsdT1 = 0.0;
    HandleRead IridHandle;
    sdiString matCard = matEntityRead.GetKeyword();
    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();

    sdiValue tempVal(lsdT1);
    dynaProp.GetValue(sdiIdentifier("T1"), tempVal);
    tempVal.GetValue(lsdT1);

    //int lsdQR = 0;
    //tempVal = sdiValue(lsdQR);
    //dynaProp.GetValue(sdiIdentifier("QR"), tempVal);
    //tempVal.GetValue(lsdQR);

    dynaProp.GetEntityHandle(sdiIdentifier("LSD_IRID"), IridHandle);

    //if (IridHandle.GetId(p_lsdynaModel) > 0 )  DynaToRad::ShowMessage(sdiMessageHandler::Level::Error, 24,
    //        sdiString("*SECTION_SHELL").c_str(), dynaPropId,dynaPropName.c_str() );

    if (IridHandle.IsValid())
    {
        EntityRead IridRead(p_lsdynaModel, IridHandle);
        if (IridHandle.GetId(p_lsdynaModel) > 0) ConvertSecShellsRelatedIntegrationShell(sourceCard,matEntityRead,IridRead,dynaProp, destCard, radProp);
    }
    else if (matCard.find("*MAT_NULL") != string::npos || matCard.find("*MAT_009") != string::npos)
    {
        destCard = "/PROP/TYPE0"; 
        if (matReferenceCount > 1)
            p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
        else
            p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);

        radProp.SetValue(p_radiossModel, sdiIdentifier("Prop_ThickOption"), sdiValue(1));
        radProp.SetValue(p_radiossModel, sdiIdentifier("THICK"), sdiValue(lsdT1));
    }
    else if (matCard.find("*MAT_FABRIC") != string::npos || matCard.find("*MAT_034") != string::npos)
    {
        sdiValue tempValue;
        double lsdFORM;
        tempValue = sdiValue(lsdFORM);
        matEntityRead.GetValue(sdiIdentifier("FORM"), tempValue);
        tempValue.GetValue(lsdFORM);

        if(lsdFORM != 14.0 && lsdFORM != -14.0)
        {
            destCard = "/PROP/TYPE9";
            ConvertSecShellsRelatedMatFabric(matEntityRead, dynaProp, destCard, radProp);
        }
        else
        {
            destCard = "/PROP/TYPE16";
            ConvertSecShells16RelatedMatFabric(matEntityRead, dynaProp, destCard, radProp);
        }
    }
    else if (matCard.find("*MAT_LAMINATED_GLASS") != string::npos || matCard.find("*MAT_032") != string::npos ||
             matCard.find("*MAT_LAMINATED_COMPOSITE_FABRIC") != string::npos || matCard.find("*MAT_058") != string::npos)
    {
        ConvertSecShellsRelatedMatLaminate(matEntityRead, dynaProp, destCard, radProp);
    }
    else if (matCard.find("*MAT_SEATBELT") != string::npos || matCard.find("*MAT_B01") != string::npos)
    {
        destCard = "/PROP/TYPE9";
        ConvertSecShellsRelatedMatSeatbelt2D(matEntityRead, dynaProp, destCard, radProp);
    }
    else if (matCard.find("*MAT_HILL_3R") != string::npos || matCard.find("*MAT_122") != string::npos)
    {
        destCard = "/PROP/TYPE9";
        ConvertSecShellsRelatedMatFabric(matEntityRead, dynaProp, destCard, radProp);
    }
    else
    {
        if (matCard.find("*MAT_ORTHOTROPIC_ELASTIC") != string::npos || matCard.find("*MAT_002") != string::npos)
        {
            /*
            destCard = "/PROP/TYPE9";
            if (p_radiossModel->IsIdAvailable(destEntityType, dynaPropId))
                p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);
            else
                p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));

            EntityEdit radPropEdit(p_radiossModel, radProp);
            UpdateSystemForOrthPropFromDynaMat(p_lsdynaModel, p_radiossModel, matEntityRead, p_ConvertUtils, radPropEdit);
            */
            destCard = "/PROP/SH_SANDW";  //type11
            ConvertSecShellsRelatedMatOrthotropic(matEntityRead, dynaProp, destCard, radProp);
        }
        else
        {
            destCard = "/PROP/TYPE1";
            if (matReferenceCount > 1)
                p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
            else
                p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);
        }

        int elform = 0;
        sdiValue tempValue(elform);
        dynaProp.GetValue(sdiIdentifier("LSD_ELFORM"), tempValue);
        tempValue.GetValue(elform);
        vector<int> unSupportedElformList = { 12, 13, 14, 15, 99 };
        if (find(unSupportedElformList.begin(), unSupportedElformList.end(), elform) != unSupportedElformList.end())
            /* post error message*/
            return;

        EntityEdit radPropEdit(p_radiossModel, radProp);
        radPropEdit.SetValue(sdiIdentifier("THICK"), sdiValue(lsdT1));

        int nip = 0;
        tempValue = sdiValue(nip);
        dynaProp.GetValue(sdiIdentifier("NIP"), tempValue);
        tempValue.GetValue(nip);

        if(nip > 10) nip = 10;

        int ishell = 24;
        int ish3n = 2;
        vector<int> category1 = { 9, -16, 20, 21, 26 };
        if (std::find(category1.begin(), category1.end(), elform) != category1.end())
            ishell = 12;
        else if (elform == 16)
            ishell = 24;
        else if (elform == 17)
            ish3n = 30;
        else if (elform == 18 || elform == 20)
            ish3n = 31;
        else if (elform == 5)
            nip = 1;
        else if (elform == 9)
        {
            nip = 1;
            ishell = 12;
        }
        if (!nip)
            nip = 2;
            
        if(destCard == "/PROP/TYPE1")
        {
            vector<int> intValList = { nip, ishell,ish3n, 2 , 1, 1 };
            vector<sdiString> radAttList = { "NIP" , "Ishell" , "Ish3n", "Ismstr", "ITHICK", "IPLAS" };
            for (int i = 0; i < intValList.size(); ++i)
            {
                radPropEdit.SetValue(sdiIdentifier(radAttList[i]), sdiValue(intValList[i]));
            }
        }
        else
        {
            vector<int> intValList = { nip, ishell,ish3n, 2 , 1, 1, 1 };
            vector<sdiString> radAttList = { "NIP" , "Ishell" , "Ish3n", "Ismstr", "ISTRAIN", "ITHICK", "IPLAS" };
            for (int i = 0; i < intValList.size(); ++i)
            {
                radPropEdit.SetValue(sdiIdentifier(radAttList[i]), sdiValue(intValList[i]));
            }
        }
 
        radPropEdit.SetValue(sdiIdentifier("AREA_SHEAR"), sdiValue(5.0 / 6));


        if (matCard.find("*MAT_PIECEWISE_LINEAR_PLASTICITY") != string::npos || matCard.find("*MAT_024") != string::npos)
        {
            p_CopyNumIntPtsFromMatAddErosion(matEntityRead, radProp, dynaProp, nip);
        }
        else if (matCard.find("*MAT_PLASTICITY_WITH_DAMAGE") != string::npos)
        {
            p_CopyNumIntPtsFromMatPlasticity(matEntityRead, radProp, nip);
        }
        if (nip > 0)
        {
            if (matCard.find("*MAT_SIMPLIFIED_JOHNSON_COOK_ORTHOTROPIC_DAMAGE") != string::npos || matCard.find("*MAT_099") != string::npos)
            {
                int lsdNumInt = 0;
                tempVal = sdiValue(lsdNumInt);
                matEntityRead.GetValue(sdiIdentifier("NUMINT"), tempVal);
                tempVal.GetValue(lsdNumInt);
                radProp.SetValue(p_radiossModel, sdiIdentifier("P_Thick_Fail"), sdiValue(lsdNumInt * 1.0 / nip));
            }
            if (matCard.find("*MAT_JOHNSON_COOK") != string::npos || matCard.find("*MAT_015") != string::npos)
            {
                double lsdNUMINT=0.0;
                tempValue = sdiValue(lsdNUMINT);
                matEntityRead.GetValue(sdiIdentifier("NUMINT"), tempValue);
                tempValue.GetValue(lsdNUMINT);
                radPropEdit.SetValue(sdiIdentifier("P_Thick_Fail"), sdiValue(lsdNUMINT / nip));
            }
        }
    }
}

void ConvertProp::p_ConvertSectionDiscrete(const EntityRead& matEntityRead, const EntityRead& dynaProp, sdiString& destCard, HandleEdit& radProp)
{
    sdiString matCard = matEntityRead.GetKeyword();
    EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");
    size_t pos = matCard.find("_TITLE");
    if (pos != string::npos)
    {
        matCard.erase(pos);
    }
    unsigned short int matLawNum = dynaMatLawMap[matCard];
    destCard = "/PROP/TYPE23";;
    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();
    if (matReferenceCount > 1)
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);

    sdiValue tempValue;
    int lsdDRO = 0;
    tempValue = sdiValue(lsdDRO);
    dynaProp.GetValue(sdiIdentifier("DRO"), tempValue);
    tempValue.GetValue(lsdDRO);

    sdiString lsdMatAtrib;
    sdiString lsdMatAtrib2;
    sdiString radMatAtrib;
    sdiString radMatAtrib2;
    sdiValue tempValue2;
    sdiValueEntity tempEntity;

    SDIEntityReadList propReferences;
    HandleEdit radmatHRead;;
    p_PropMatRelationDB.GetReferenceList(p_lsdynaModel->GetEntityType("*MAT"), matEntityRead.GetId(), propReferences);
    if (propReferences.size() > 1)
    {
        HandleEdit radPartHRead;
        if (p_radiossModel->FindById(p_radiossModel->GetEntityType("/PART"), p_PartBeingConverted.GetId(p_lsdynaModel), radPartHRead))
        {
            p_radiossModel->CreateEntity(radmatHRead, "/MAT/LAW108", "duplicate_" + matEntityRead.GetName());
            radPartHRead.SetEntityHandle(p_radiossModel, sdiIdentifier("mat_ID"), radmatHRead);
            p_PropMatRelationDB.UpdateReferenceList(p_lsdynaModel->GetEntityType("*MAT"), matEntityRead.GetId(), dynaProp);

            sdiConvert::SDIHandlReadList sourcemat = { {dynaProp.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radmatHRead, sourcemat));
        }
    }

    if (!radmatHRead.IsValid())
        p_radiossModel->FindById("/MAT/LAW108", matEntityRead.GetId(), radmatHRead);
    if (radmatHRead.IsValid())
    {
        EntityEdit radMatEdit(p_radiossModel, radmatHRead);
        switch (matLawNum)
        {
        case 501:
            lsdMatAtrib = "K";
            radMatAtrib = (lsdDRO == 0) ? "K1" : "K4";
            break;
        case 502:
            lsdMatAtrib = "DC";
            radMatAtrib = (lsdDRO == 0) ? "DAMP1" : "DAMP4";
            break;
        case 503:
        {
            double lsdK;
            double lsdKT;
            double lsdFY;
            vector<reference_wrapper<double >> attribVals({ lsdK, lsdKT, lsdFY });
            vector<sdiString> attriNames({ "K", "KT", "FY" });
            HandleEdit crvHanldeEdit;
            p_ConvertUtils.GetAttribValues(matEntityRead, attriNames, attribVals);
            sdiDoubleList points({ -(lsdFY / lsdK + 1), -(lsdFY + lsdKT), -lsdFY / lsdK, -lsdFY, 0, 0, lsdFY / lsdK, lsdFY, (lsdFY / lsdK + 1), (lsdFY + lsdKT) });
            p_ConvertUtils.CreateCurve(matEntityRead.GetName(), 5, points, crvHanldeEdit);
            if (crvHanldeEdit.IsValid())
            {
                sdiConvert::SDIHandlReadList sourcemat = { {dynaProp.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(crvHanldeEdit, sourcemat));
                radMatEdit.SetEntityHandle(sdiIdentifier((lsdDRO == 0) ? "fct_ID11" : "fct_ID14"), crvHanldeEdit);
            }

            lsdMatAtrib = "K";
            radMatAtrib = (lsdDRO == 0) ? "K1" : "K4";
            radMatEdit.SetValue(sdiIdentifier((lsdDRO == 0) ? "H1" : "H4"), sdiValue(1));
            break;
        }
        case 504:
            lsdMatAtrib = "LCD";
            lsdMatAtrib2 = "LCR";
            //radMatAtrib = (lsdDRO == 0) ? "fct_ID11" : "fct_ID14";
            //radMatAtrib2 = (lsdDRO == 0) ? "fct_ID21" : "fct_ID24";

            radMatEdit.SetValue(sdiIdentifier((lsdDRO == 0) ? "E1" : "E4"), sdiValue(1.0));
            //radMatEdit.SetValue(sdiIdentifier((lsdDRO == 0) ? "A1" : "A4"), sdiValue(1E-20)); // only if second curve is available / otherwise set 1.0

            //------
            {
                
                HandleRead LCDHandle;
                matEntityRead.GetEntityHandle(sdiIdentifier("LCD"), LCDHandle);
                EntityId LCDid = LCDHandle.GetId(p_lsdynaModel);

                if(LCDHandle.IsValid())
                {
                    if(lsdDRO == 0.0)
                    {
                        radMatAtrib = "fct_ID11";
                        radMatEdit.SetValue(sdiIdentifier("A1"), sdiValue(1.0));
                    }
                    else if(lsdDRO == 1.0)
                    {
                        radMatAtrib = "fct_ID14";
                        radMatEdit.SetValue(sdiIdentifier("A4"), sdiValue(1.0));
                    }
                }

                
                HandleRead LCRHandle;
                matEntityRead.GetEntityHandle(sdiIdentifier("LCR"), LCRHandle);
                EntityId LCRid = LCRHandle.GetId(p_lsdynaModel);

                if(LCRHandle.IsValid())
                {
                    if(lsdDRO == 0.0)
                    {
                        radMatAtrib2 = "fct_ID21";
                        radMatEdit.SetValue(sdiIdentifier("A1"), sdiValue(1E-20));
                    }
                    else if(lsdDRO == 1.0)
                    {
                        radMatAtrib2 = "fct_ID24";
                        radMatEdit.SetValue(sdiIdentifier("A4"), sdiValue(1E-20));
                    }
                }
            }
            //------
            break;
        case 505:
            destCard = "/PROP/TYPE8";
            lsdMatAtrib2 = "LCDR";
            radMatAtrib2 = (lsdDRO == 0) ? "fct_ID41" : "fct_ID44";
            break;
        case 506:
            radMatAtrib = (lsdDRO == 0) ? "fct_ID11" : "fct_ID14";
            radMatAtrib2 = (lsdDRO == 0) ? "fct_ID31" : "fct_ID34";
            lsdMatAtrib = "LCDL";
            lsdMatAtrib2 = "LCDU";
            radMatEdit.SetValue(sdiIdentifier((lsdDRO == 0) ? "H1" : "H4"), sdiValue(6));
            break;
        case 508:
        {
            sdiDoubleList crvPoints;
            HandleEdit modCrvHandle;
            sdiString crvAttrib;
            HandleCurveLCFD(p_lsdynaModel, matEntityRead, crvPoints);
            if (crvPoints.empty())
            {
                /*post warning*/
            }
            else
            {
                p_ConvertUtils.CreateCurve("modified_LCFD_MATS08:" + matEntityRead.GetName(), (int)crvPoints.size() / 2, crvPoints, modCrvHandle);
            }

            lsdMatAtrib = "KU";
            radMatAtrib = (lsdDRO == 0) ? "K1" : "K4";

            if (modCrvHandle.IsValid())
            {
                radMatEdit.SetEntityHandle(sdiIdentifier((lsdDRO == 0) ? "fct_ID11" : "fct_ID14"), modCrvHandle);
                if (modCrvHandle.IsValid())
                {
                    sdiConvert::SDIHandlReadList sourcemat = { {dynaProp.GetHandle()} };
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(modCrvHandle, sourcemat));
                }
            }
        }
        break;
        default:
            break;
        }

        if (radMatAtrib.find("fct") != string::npos)
        {
            HandleRead tempHandle;
            matEntityRead.GetEntityHandle(sdiIdentifier(lsdMatAtrib), tempHandle);
            EntityId tempid = tempHandle.GetId(p_lsdynaModel);

            radMatEdit.SetValue(sdiIdentifier(radMatAtrib), sdiValue(sdiValueEntity(radFunctType, tempid)));
        }
        else
        {
            tempValue = sdiValue(0.0);
            matEntityRead.GetValue(sdiIdentifier(lsdMatAtrib), tempValue);
            radMatEdit.SetValue(sdiIdentifier(radMatAtrib), tempValue);
        }

        if (radMatAtrib2.find("fct") != string::npos)
        {
            HandleRead tempHandle;
            matEntityRead.GetEntityHandle(sdiIdentifier(lsdMatAtrib2), tempHandle);
            EntityId tempid = tempHandle.GetId(p_lsdynaModel);

            radMatEdit.SetValue(sdiIdentifier(radMatAtrib2), sdiValue(sdiValueEntity(radFunctType, tempid)));
        }
        else
        {
            tempValue = sdiValue(0.0);
            matEntityRead.GetValue(sdiIdentifier(lsdMatAtrib2), tempValue);
            radMatEdit.SetValue(sdiIdentifier(radMatAtrib2), tempValue);
        }
    }
}


void HandleCurveLCFD(ModelViewRead*& dynaModel, const EntityRead& matEntityRead, sdiDoubleList& crvPoints)
{
    HandleRead crvHandle;
    matEntityRead.GetEntityHandle(sdiIdentifier("LCFD"), crvHandle);

    if (crvHandle.IsValid())
    {
        EntityRead crvEntityRead(dynaModel, crvHandle);
        sdiValue tempValue(0);
        crvEntityRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
        tempValue = sdiValue(crvPoints);
        crvEntityRead.GetValue(sdiIdentifier("points"), tempValue);
        tempValue.GetValue(crvPoints);
        if (crvPoints.size())
        {
            if (crvPoints[0] == 0.0 && crvPoints[1] == 0.0)
            {
                for (double point : crvPoints)
                {
                    if (point < 0.0)
                        return;
                }
                double lsdCTF = 0.0;
                tempValue = sdiValue(lsdCTF);
                matEntityRead.GetValue(sdiIdentifier("LSD_MAT_CTF"), tempValue);
                tempValue.GetValue(lsdCTF);
                if (lsdCTF == -1)
                {
                    crvPoints.insert(crvPoints.begin(), 0.0);
                    crvPoints.insert(crvPoints.begin(), -1.0);
                }
                else
                {
                    vector<double> xVals;
                    vector<double> yVals;
                    for (int i = 0; i < crvPoints.size(); ++i)
                    {
                        if (i % 2 == 0)
                            xVals.push_back(crvPoints[i]);
                        else
                            yVals.push_back(crvPoints[i]);
                    }
                    reverse(xVals.begin(), xVals.end());
                    reverse(yVals.begin(), yVals.end());
                    crvPoints.clear();
                    for (int i = 0; i < xVals.size(); ++i)
                    {
                        double abc = xVals[i];
                        double ord = yVals[i];
                        if (abc != 0.0)
                            abc = -1.0 * abc;
                        if (ord != 0.0)
                            ord = -1.0 * ord;
                        crvPoints.push_back(abc);
                        crvPoints.push_back(ord);
                    }
                    crvPoints.push_back(1.0);
                    crvPoints.push_back(0.0);
                }
            }
        }
    }
}

void UpdateSystemForOrthPropFromDynaMat(ModelViewRead* lsdModel, ModelViewEdit* radModel, const EntityRead& matEntityRead, ConvertUtils& convertUtils, EntityEdit& radPropEntEdit)
{
    sdiString keyWord = radPropEntEdit.GetKeyword();
    int axisOptFlag = 0;

    sdiValue tempValue(axisOptFlag);
    matEntityRead.GetValue(sdiIdentifier("axisOptFlag"), tempValue);
    tempValue.GetValue(axisOptFlag);

    if (axisOptFlag == 6)
    {
        HandleRead skewHandle;
        HandleRead radSkewHandle;
        matEntityRead.GetEntityHandle(sdiIdentifier("LSD_SYSTEM"), skewHandle);
        EntityRead skewEntityRead(lsdModel, skewHandle);
        EntityId skewId = skewEntityRead.GetId();

        radModel->FindById(radModel->GetEntityType("/SKEW"), skewId, radSkewHandle);
        if (radSkewHandle.IsValid())
        {
            if (keyWord.find("TYPE9") != keyWord.npos)
            {
                sdiValue tempVal;
                radSkewHandle.GetValue(radModel, sdiIdentifier("X1"), tempVal);
                radPropEntEdit.SetValue(sdiIdentifier("Vx"), tempVal);
                radSkewHandle.GetValue(radModel, sdiIdentifier("Y1"), tempVal);
                radPropEntEdit.SetValue(sdiIdentifier("Vy"), tempVal);
                radSkewHandle.GetValue(radModel, sdiIdentifier("Z1"), tempVal);
                radPropEntEdit.SetValue(sdiIdentifier("Vz"), tempVal);
                return;
            }
            else
                radPropEntEdit.SetEntityHandle(sdiIdentifier("skew_ID"), radSkewHandle);
        }
        return;
    }
    else if (axisOptFlag == 3)
    {
        double lsdA1;
        double lsdA2;
        double lsdA3;
        double lsdD1;
        double lsdD2;
        double lsdD3;

        vector<reference_wrapper<double>> attribVals({ lsdA1, lsdA2, lsdA3, lsdD1, lsdD2, lsdD3});
        vector <sdiString> attNames({ "LSDYNA_A1", "LSDYNA_A2", "LSDYNA_A3", "LSDYNA_D1", "LSDYNA_D2", "LSDYNA_D3"});
        convertUtils.GetAttribValues(matEntityRead, attNames, attribVals);
        sdiTriple planeCoords = sdiTriple(lsdA1, lsdA2, lsdA3) * sdiTriple(lsdD1, lsdD2, lsdD3);
        sdiTriple axisCoords = planeCoords * sdiTriple(lsdA1, lsdA2, lsdA3);
        if (keyWord.find("TYPE9") != keyWord.npos)
        {
            radPropEntEdit.SetValue(sdiIdentifier("Vx"), sdiValue(axisCoords[0]));
            radPropEntEdit.SetValue(sdiIdentifier("Vy"), sdiValue(axisCoords[1]));
            radPropEntEdit.SetValue(sdiIdentifier("Vz"), sdiValue(axisCoords[2]));
            return;
        }
        HandleEdit radSkewHandle;
        radModel->CreateEntity(radSkewHandle, "/SKEW/FIX", radPropEntEdit.GetName());
        if(radSkewHandle.IsValid())
        {
            EntityEdit radSkewEntEdit(radModel, radSkewHandle);
            radSkewEntEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(radPropEntEdit.GetName()));
            vector<sdiString> attribNames({ "Ox","Oy","Oz","X2", "Y2","Z2","X1","Y1","Z1" });
            vector<double> attribVals({ 0.0, 0.0, 0.0, planeCoords[0], planeCoords[1], planeCoords[2], axisCoords[0], axisCoords[1], axisCoords[2] });
            for (int i = 0; i < attribNames.size(); ++i)
            {
                radSkewEntEdit.SetValue(sdiIdentifier(attribNames[i]), sdiValue(attribVals[i]));
            }
            radPropEntEdit.SetEntityHandle(sdiIdentifier("skew_ID"), radSkewHandle);
            sdiConvert::SDIHandlReadList sourceInivel = { {matEntityRead.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radSkewHandle, sourceInivel));
        }
    }
}

void ConvertProp::p_ConvertSectionBeam(const EntityRead& matEntityRead, const EntityRead& dynaProp, sdiString& destCard, HandleEdit& radProp)
{
    sdiString matCard = matEntityRead.GetKeyword();
    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();

    int elform = 0;
    sdiValue queryValue(elform);
    dynaProp.GetValue(sdiIdentifier("ELFORM"), queryValue);
    queryValue.GetValue(elform);


    if ( (matCard.find("*MAT_NULL") != string::npos || matCard.find("*MAT_009") != string::npos ) && elform == 6)
    {
        destCard = "/PROP/TYPE23";
        if (matReferenceCount > 1)
            p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
        else
            p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);
    }
    else if ( (matCard.find("*MAT_NULL") != string::npos || matCard.find("*MAT_009") != string::npos ) 
               && elform != 0  && elform != 1 && elform != 2)
    {
        destCard = "/PROP/TYPE2";
        vector<double> defVals;
        p_CurrentUnitSyst.GetDefaultMatlaw2Vals(defVals);
        if (matReferenceCount > 1)
            p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
        else
            p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);

        EntityEdit radPropEntity(p_radiossModel, radProp);
        p_ConvertUtils.CopyValue(dynaProp, radPropEntity, "A", "AREA");
    }
    else if (matCard.find("*MAT_MUSCLE") != string::npos || matCard.find("*MAT_156") != string::npos)
    {
        ConvertSecBeamRelatedMatMuscle(matEntityRead, dynaProp, destCard, radProp);
    }
    else
    {
        if (!elform)
            elform = 1;
        switch (elform)
        {
        case 1:
        case 4:
        {
            double cSectType;
            double thickS1;
            double thickS2;
            double thickT1;
            double thickT2;
            vector<sdiString> attribNames({ {"LSD_CST", "LSD_THIC1s", "LSD_THIC2s", "LSD_THIC1t",  "LSD_THIC2t"} });
            vector<reference_wrapper<double>> attrVals({ {cSectType, thickS1, thickS2, thickT1, thickT2} });
            p_ConvertUtils.GetAttribValues(dynaProp, attribNames, attrVals);

            int cst = (int)cSectType;
            destCard = "/PROP/TYPE18";
            if (matReferenceCount > 1)
                p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
            else
                p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);

            EntityEdit radPropEntity(p_radiossModel, radProp);

            if (cst == 0)
            {
                if(thickS2 == 0) thickS2 = thickS1;
                radPropEntity.SetValue(sdiIdentifier("L1"), sdiValue(thickS1));
                radPropEntity.SetValue(sdiIdentifier("L2"), sdiValue(thickS2));
            }
            else if (cst == 1)
            {
                radPropEntity.SetValue(sdiIdentifier("L1"), sdiValue(max(thickS1 * 0.5, 0.5 * thickS2)));
            }

            radPropEntity.SetValue(sdiIdentifier("Ismstr"), sdiValue(4));
            radPropEntity.SetValue(sdiIdentifier("Dm"), sdiValue(0.0));
            radPropEntity.SetValue(sdiIdentifier("df"), sdiValue(0.0));

            int sectopt = 0;
            sdiValue tempValue(sectopt);
            dynaProp.GetValue(sdiIdentifier("Sect_Option"), tempValue);
            tempValue.GetValue(sectopt);
            // QR/IRID Quadrature rule, or rule number for user defined rule for integrated beams
            if(sectopt == 0)
            {
                //QR --> Quadrature rule
                double lsdQR = 0.0;
                tempValue = sdiValue(lsdQR);
                dynaProp.GetValue(sdiIdentifier("LSD_QR"), tempValue);
                tempValue.GetValue(lsdQR);

                if(lsdQR == 0.0)
                {
                    radPropEntity.SetValue(sdiIdentifier("NITRS"), sdiValue(2));
                    if(cst == 0)     radPropEntity.SetValue(sdiIdentifier("ISFLAG"), sdiValue(1));
                    else if(cst == 1)radPropEntity.SetValue(sdiIdentifier("ISFLAG"), sdiValue(2));
                }
                else if(lsdQR == 1.0)
                {
                    radPropEntity.SetValue(sdiIdentifier("NITRS"), sdiValue(1));
                    if(cst == 0)     radPropEntity.SetValue(sdiIdentifier("ISFLAG"), sdiValue(1));
                    else if(cst == 1)radPropEntity.SetValue(sdiIdentifier("ISFLAG"), sdiValue(2));
                }
                else if(lsdQR == 2.0)
                {
                    radPropEntity.SetValue(sdiIdentifier("NITRS"), sdiValue(2));
                    if(cst == 0)     radPropEntity.SetValue(sdiIdentifier("ISFLAG"), sdiValue(1));
                    else if(cst == 1)radPropEntity.SetValue(sdiIdentifier("ISFLAG"), sdiValue(2));
                }
                else if(lsdQR == 3.0)
                {
                    radPropEntity.SetValue(sdiIdentifier("NITRS"), sdiValue(3));
                    if(cst == 0)     radPropEntity.SetValue(sdiIdentifier("ISFLAG"), sdiValue(1));
                    else if(cst == 1)radPropEntity.SetValue(sdiIdentifier("ISFLAG"), sdiValue(2));
                }
                if(lsdQR == 4.0)
                {
                    if(cst == 0)
                    {
                      radPropEntity.SetValue(sdiIdentifier("NITRS"), sdiValue(3));
                      radPropEntity.SetValue(sdiIdentifier("ISFLAG"), sdiValue(3));
                    }
                    else if(cst == 1)
                    {
                      radPropEntity.SetValue(sdiIdentifier("NITRS"), sdiValue(25));
                      radPropEntity.SetValue(sdiIdentifier("ISFLAG"), sdiValue(4));
                    }
                }
                else if(lsdQR == 5.0)
                {
                    radPropEntity.SetValue(sdiIdentifier("NITRS"), sdiValue(3));
                    if(cst == 0)     radPropEntity.SetValue(sdiIdentifier("ISFLAG"), sdiValue(1));
                    else if(cst == 1)radPropEntity.SetValue(sdiIdentifier("ISFLAG"), sdiValue(2));
                }
            }
            else if(sectopt == 1)
            {
                // IRID --> rule number for user defined rule
                // NOT YET SUPPORTED (waits for "RD-6730" to be solved)
            }
            //---

            break;
        }
        case 2:
        {

            sdiValue tempValue;
            sdiString lsdSectType;
            dynaProp.GetValue(sdiIdentifier("SectType"), tempValue);
            tempValue.GetValue(lsdSectType);

            destCard = "/PROP/TYPE3";
            if (matReferenceCount > 1)
                p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
            else
                p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);

            if (lsdSectType == "SECTION_08")
            {
                double lsdD1;
                dynaProp.GetValue(sdiIdentifier("D1"), tempValue);
                tempValue.GetValue(lsdD1);

                radProp.SetValue(p_radiossModel, sdiIdentifier("Area"), sdiValue(M_PI * pow(lsdD1, 2.0f)));
                radProp.SetValue(p_radiossModel, sdiIdentifier("Iyy"), sdiValue(M_PI * pow(lsdD1, 4.0f) / 4.0f));
                radProp.SetValue(p_radiossModel, sdiIdentifier("Izz"), sdiValue(M_PI * pow(lsdD1, 4.0f)/4.0f));
                radProp.SetValue(p_radiossModel, sdiIdentifier("Ixx"), sdiValue(M_PI * pow(lsdD1, 4.0f) / 2.0f));

            }
            else if (lsdSectType == "SECTION_11")
            {
                double lsdD1;
                dynaProp.GetValue(sdiIdentifier("D1"), tempValue);
                tempValue.GetValue(lsdD1);
                double lsdD2;
                dynaProp.GetValue(sdiIdentifier("D2"), tempValue);
                tempValue.GetValue(lsdD2);
                double lsdD3;
                dynaProp.GetValue(sdiIdentifier("D3"), tempValue);
                tempValue.GetValue(lsdD3);

                radProp.SetValue(p_radiossModel, sdiIdentifier("Area"), sdiValue(lsdD1* lsdD2));
                radProp.SetValue(p_radiossModel, sdiIdentifier("Iyy"), sdiValue(lsdD1*pow(lsdD2,3.0f)/12.0f));
                radProp.SetValue(p_radiossModel, sdiIdentifier("Izz"), sdiValue(pow(lsdD1, 3.0f) * lsdD2 / 12.0f));
                radProp.SetValue(p_radiossModel, sdiIdentifier("Ixx"), sdiValue( (lsdD1* pow(lsdD2, 3.0f) / 12.0f) + (pow(lsdD1, 3.0f)* lsdD2 / 12.0f) ));
            }
            else
            {
                EntityEdit radPropEntity(p_radiossModel, radProp);
                p_ConvertUtils.CopyValue(dynaProp, radPropEntity, "ISS", "Iyy");
                p_ConvertUtils.CopyValue(dynaProp, radPropEntity, "ITT", "Izz");
                double lsdJ = p_ConvertUtils.GetValue<double>(dynaProp, "J");
                if(0 != lsdJ) p_ConvertUtils.CopyValue(dynaProp, radPropEntity, "J", "Ixx");
                else p_ConvertUtils.SetExpressionValue(dynaProp, radPropEntity, "ISS+ITT", "Ixx");
                p_ConvertUtils.CopyValue(dynaProp, radPropEntity, "A", "Area");
            }

            break;
        }
        case 3:
        {
            sdiValue tempValue;
            sdiString lsdSectType;
            dynaProp.GetValue(sdiIdentifier("SectType"), tempValue);
            tempValue.GetValue(lsdSectType);

            destCard = "/PROP/TRUSS";
            if (matReferenceCount > 1)
                p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
            else
                p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);

            if (lsdSectType == "SECTION_08")
            {
                double lsdD1;
                dynaProp.GetValue(sdiIdentifier("D1"), tempValue);
                tempValue.GetValue(lsdD1);

                radProp.SetValue(p_radiossModel, sdiIdentifier("AREA"), sdiValue(M_PI * pow(lsdD1, 2.0f)));

            }
            else if (lsdSectType == "SECTION_11")
            {
                double lsdD1;
                dynaProp.GetValue(sdiIdentifier("D1"), tempValue);
                tempValue.GetValue(lsdD1);
                double lsdD2;
                dynaProp.GetValue(sdiIdentifier("D2"), tempValue);
                tempValue.GetValue(lsdD2);
                radProp.SetValue(p_radiossModel, sdiIdentifier("AREA"), sdiValue(lsdD1* lsdD2));
            }
            else
            {
                double lsdA = 0.0;
                sdiValue tempVal(lsdA);
                dynaProp.GetValue(sdiIdentifier("A"), tempVal);
                radProp.SetValue(p_radiossModel, sdiIdentifier("AREA"), tempVal);
            }
            break;
        }
        case 6:
        {
            double lsdVOL;
            double lsdINER;
            double lsdCA;
            vector<sdiString> attribNames({ {"VOL", "INER", "LSD_CA",} });
            vector<reference_wrapper<double>> attrVals({ {lsdVOL, lsdINER, lsdCA, } });
            p_ConvertUtils.GetAttribValues(dynaProp, attribNames, attrVals);

            destCard = "/PROP/TYPE23";
            if (matReferenceCount > 1)
                p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
            else
                p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);

            EntityEdit radPropEdit(p_radiossModel, radProp);

            HandleRead cidHandle;
            dynaProp.GetEntityHandle(sdiIdentifier("CID"), cidHandle);

            HandleEdit radCidHandle;
            p_radiossModel->FindById(p_radiossModel->GetEntityType("/SKEW"), cidHandle.GetId(p_lsdynaModel), radCidHandle);
            if (radCidHandle.IsValid())
            {
                radPropEdit.SetEntityHandle(sdiIdentifier("skew_ID"), radCidHandle);
            }
            int iMass = 0;
            if (matCard.find("*MAT_CABLE_DISCRETE_BEAM") != string::npos)
            {
                iMass = 1;
                radPropEdit.SetValue(sdiIdentifier("AREA"), sdiValue(lsdCA));
            }
            else
            {
                iMass = 2;
                radPropEdit.SetValue(sdiIdentifier("Volume"), sdiValue(lsdVOL));
            }

            radPropEdit.SetValue(sdiIdentifier("Imass"), sdiValue(iMass));
            radPropEdit.SetValue(sdiIdentifier("Inertia"), sdiValue(lsdINER));
            break;
        }
        case 9:
            if (matCard.find("*MAT_SPOTWELD") != string::npos || matCard.find("*MAT_100") != string::npos)
            {
                double lsdCST = 0.0;
                sdiValue tempValue(lsdCST);
                dynaProp.GetValue(sdiIdentifier("CST"), tempValue);
                tempValue.GetValue(lsdCST);

                if (lsdCST != 1.0)
                {
                    
                    return;
                }

                // Check if *CONTROL_UNITS is in the model. If not, then add an error message
                SelectionRead selCtrlUnits(p_lsdynaModel, "*CONTROL_UNITS");
                if (selCtrlUnits.Count() == 0)
                {
                    DynaToRad::ShowMessage(sdiMessageHandler::Level::Error, 40,
                    matEntityRead.GetKeyword().c_str(), matEntityRead.GetId(), matEntityRead.GetName().c_str());
                }

                ConvertSectionBeamToSpringBeam(matEntityRead, dynaProp, destCard, radProp);
            }
            break;
        }
    }
}

void ConvertProp::ConvertSecShellsRelatedMatLaminate(const EntityRead& matEntityRead, const EntityRead& dynaProp, sdiString& destCard, HandleEdit& radProp)
{
    double thick = 0.0;
    sdiDoubleList lsdFi;
    sdiUIntList matIdList;
    int nip = 0;
    int lsdQRIRIDOpt = 0;
    int iPosValue = 0;
    HandleRead IridHandle;

    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();

    destCard = "/PROP/SH_SANDW";
    if (matReferenceCount > 1)
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);

    EntityEdit radShSandwEdit(p_radiossModel, radProp);

    sdiValue tempValue(thick);
    dynaProp.GetValue(sdiIdentifier("T1"), tempValue);
    tempValue.GetValue(thick);

    tempValue = sdiValue(nip);
    dynaProp.GetValue(sdiIdentifier("NIP"), tempValue);
    tempValue.GetValue(nip);
    if(nip == 0) nip = 2;

    tempValue = sdiValue(lsdQRIRIDOpt);
    dynaProp.GetValue(sdiIdentifier("Sect_Option"), tempValue);
    tempValue.GetValue(lsdQRIRIDOpt);

    sdiDoubleList thicknessList;
    sdiDoubleList layerPosList;

    radShSandwEdit.SetValue(sdiIdentifier("N"), sdiValue(nip));
    radShSandwEdit.SetValue(sdiIdentifier("THICK"), sdiValue(thick));
    if (lsdQRIRIDOpt == 0)
    {
        iPosValue = 0;

        if (nip > 0)
        {
            thicknessList = sdiDoubleList(nip, thick / nip);
            layerPosList = sdiDoubleList(nip, 0);
        }
    }
    else if (lsdQRIRIDOpt != 0)
    {
        int esop = 0;
        nip = 0;
        sdiDoubleList intS;
        sdiDoubleList intWF;

        iPosValue = 1;

        dynaProp.GetEntityHandle(sdiIdentifier("LSD_IRID"), IridHandle);

        if (IridHandle.IsValid())
        {
            // to be cmpleted when working to convert to prop 51 RD-11305
        }
        else
        {
            nip = 1;
            thicknessList.push_back(thick);
            layerPosList.push_back(0);
        }
    }

    radProp.SetValue(p_radiossModel, sdiIdentifier("Ipos"), sdiValue(iPosValue));
    if (!thicknessList.empty())
    {
        radShSandwEdit.SetValue(sdiIdentifier("Z"), sdiValue(layerPosList));
        radShSandwEdit.SetValue(sdiIdentifier("Prop_Thick"), sdiValue(thicknessList));
    }

    matEntityRead.GetValue(sdiIdentifier("F"), tempValue);
    tempValue.GetValue(lsdFi);

    unsigned int matId = matEntityRead.GetId();
    matIdList.clear();
    for (int i = 0; i < nip; ++i)
    {
        if (lsdFi.size() == 0)
        {
            unsigned int partId = p_PartBeingConverted.GetId(p_lsdynaModel);
            HandleEdit radParHEdit;
            p_radiossModel->FindById(p_radiossModel->GetEntityType("/PART"), partId, radParHEdit);
            int radMatId = matId;
            if (radParHEdit.IsValid())
            {
                HandleRead matHread;
                radParHEdit.GetEntityHandle(p_radiossModel,sdiIdentifier("mat_ID"), matHread);
                radMatId = matHread.GetId(p_radiossModel);
            }
            matIdList.push_back(radMatId);
        }
        else if (!lsdFi[i])
            matIdList.push_back(matId);
        else
        {
            unsigned int partId = p_PartBeingConverted.GetId(p_lsdynaModel);
            HandleEdit radParHEdit;
            p_radiossModel->FindById(p_radiossModel->GetEntityType("/PART"), partId, radParHEdit);
            if (radParHEdit.IsValid())
            {
                HandleRead matHread;
                radParHEdit.GetEntityHandle(p_radiossModel,sdiIdentifier("mat_ID"), matHread);
                if (matHread.IsValid())
                {
                    matIdList.push_back(matHread.GetId(p_radiossModel));
                    radParHEdit.SetValue(p_radiossModel,sdiIdentifier("mat_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/MAT"), matId)));
                }
                else
                    matIdList.push_back(0);
            }
            else
                matIdList.push_back(0);
        }
    }

    if (!matIdList.empty())
    {
        radShSandwEdit.SetValue(sdiIdentifier("m"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/MAT"), matIdList)));
    }
}

void ConvertProp::ConvertSecShellsRelatedMatFabric(const EntityRead& matEntityRead, const EntityRead& dynaProp, sdiString& destCard, HandleEdit& radProp)
{
    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();
    //destCard = "/PROP/TYPE9";

    if (p_radiossModel->IsIdAvailable(destEntityType, dynaPropId))
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);
    else
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));

    EntityEdit radPropEntityEdit(p_radiossModel, radProp);

    double thick = GetValue<double>(dynaProp, "T1");
    int lsdAOPT = GetValue<int>(matEntityRead, "axisOptFlag");
    double lsdBETA = GetValue<double>(matEntityRead, "BETA");

    sdiString matCard = matEntityRead.GetKeyword();

    if (matCard.find("*MAT_FABRIC") != string::npos || matCard.find("*MAT_034") != string::npos)
    {
        p_ConvertUtils.CopyValue(matEntityRead, radPropEntityEdit, "DAMP", "Dm");
    }

    if (lsdAOPT == 1)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(20));
        if(lsdBETA > 0.0)
          radProp.SetValue(p_radiossModel, sdiIdentifier("Phi"), sdiValue(lsdBETA));
        else
          radProp.SetValue(p_radiossModel, sdiIdentifier("Phi"), sdiValue(0.0));
    }
    else if (lsdAOPT == 2)
    {
        double lsdA1 = GetValue<double>(matEntityRead, "LSDYNA_A1");
        double lsdA2 = GetValue<double>(matEntityRead, "LSDYNA_A2");
        double lsdA3 = GetValue<double>(matEntityRead, "LSDYNA_A3");

        if (lsdA1 != 0.0 || lsdA2 != 0.0 || lsdA3 != 0.0)
        {
            // create a new /SKEW/FIX with OX=OY=OZ=0.0 and x-axis aligned with vector A(A1, A2, A3)
            sdiTriple origin(0.0, 0.0, 0.0);
            origin = origin.Normalize();
            sdiTriple inputDirVect(lsdA1, lsdA2, lsdA3);
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
                radSkewEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(sdiString(dynaPropName)));
                radProp.SetValue(p_radiossModel,sdiIdentifier("SKEW_CSID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SKEW"), radSkewHEdit.GetId(p_radiossModel))));
                sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(radSkewHEdit, sourceProps));
            }
        }
        else // if A(A1, A2, A3) is zero then create the /SKEW/FIX aligned with global axis
        {
            sdiTriple origin(0.0, 0.0, 0.0);
            origin = origin.Normalize();
            sdiTriple inputDirVect(0, 0, 0);
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
                radSkewEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(sdiString(dynaPropName)));
                radProp.SetValue(p_radiossModel,sdiIdentifier("SKEW_CSID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SKEW"), radSkewHEdit.GetId(p_radiossModel))));
                sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(radSkewHEdit, sourceProps));
            }
        }
        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(22));
    }
    else if (lsdAOPT == 3)
    {
        p_ConvertUtils.CopyValue(matEntityRead, radPropEntityEdit, "LSDYNA_V1", "Vx");
        p_ConvertUtils.CopyValue(matEntityRead, radPropEntityEdit, "LSDYNA_V2", "Vy");
        p_ConvertUtils.CopyValue(matEntityRead, radPropEntityEdit, "LSDYNA_V3", "Vz");

        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(23));

        if(lsdBETA > 0.0)
        {
          lsdBETA = lsdBETA + 90.0;
          radProp.SetValue(p_radiossModel, sdiIdentifier("Phi"), sdiValue(lsdBETA));
        }
        else
          radProp.SetValue(p_radiossModel, sdiIdentifier("Phi"), sdiValue(0.0));
    }
    else if (lsdAOPT == 4)
    {
        HandleRead defHandle;
        matEntityRead.GetEntityHandle(sdiIdentifier("LSD_SYSTEM"), defHandle);
        if (defHandle.IsValid())
        {
            EntityRead defEntityRead(p_lsdynaModel, defHandle);
            EntityId skew_Id = defEntityRead.GetId();
            radProp.SetValue(p_radiossModel,sdiIdentifier("SKEW_CSID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SKEW"), skew_Id)));
        }
        radProp.SetValue(p_radiossModel, sdiIdentifier("Phi"), sdiValue(0.0));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(0));
    }
    radProp.SetValue(p_radiossModel, sdiIdentifier("Thick"), sdiValue(thick));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Ishell"), sdiValue(12));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Ismstr"), sdiValue(4));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Ish3n"), sdiValue(2));
    radProp.SetValue(p_radiossModel, sdiIdentifier("N"), sdiValue(1));

    //The rest of parameters are set to default 0 values in converson
    radProp.SetValue(p_radiossModel, sdiIdentifier("P_Thick_Fail"), sdiValue(0.0));

    radProp.SetValue(p_radiossModel, sdiIdentifier("Hm"), sdiValue(0.0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Hf"), sdiValue(0.0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Hr"), sdiValue(0.0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Dn"), sdiValue(0.0));

    radProp.SetValue(p_radiossModel, sdiIdentifier("ISTRAIN"), sdiValue(0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Ashear"), sdiValue(0.0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("ITHICK"), sdiValue(0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("IPLAS"), sdiValue(0));
}

void ConvertProp::ConvertSecShellsRelatedIntegrationShell(const sdiString& sourceCard, const EntityRead& matEntityRead,const EntityRead& IridRead,const EntityRead& dynaProp, sdiString& destCard, HandleEdit& radProp)
{
    sdiString matCard = matEntityRead.GetKeyword();
    EntityType radPropType = p_radiossModel->GetEntityType("/PROP");
    EntityType radMatType = p_radiossModel->GetEntityType("/MAT");
    double lsdT1 = 0.0;
    double lsdShrf = 0.0;
    sdiDoubleList lsdFi;
    sdiUIntList matIdList;
    int lsdNip = 0;
    int lsdQRIRIDOpt = 0;
    int iPosValue = 0;
    int lsdEsop = 0;
    int lsdElform = 0;
    int isMat032 = 0;
    HandleRead IridHandle;

    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();

    destCard = "/PROP/TYPE51";
    if (matReferenceCount > 1)
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);

    EntityEdit radSh51Edit(p_radiossModel, radProp);


    sdiValue tempValue(lsdEsop);
    IridRead.GetValue(sdiIdentifier("ESOP"), tempValue);
    tempValue.GetValue(lsdEsop);

    tempValue = sdiValue(lsdElform);
    dynaProp.GetValue(sdiIdentifier("LSD_ELFORM"), tempValue);
    tempValue.GetValue(lsdElform);

    tempValue = sdiValue(lsdNip);
    IridRead.GetValue(sdiIdentifier("NIP"), tempValue);
    tempValue.GetValue(lsdNip);

    tempValue = sdiValue(lsdT1);
    dynaProp.GetValue(sdiIdentifier("T1"), tempValue);
    tempValue.GetValue(lsdT1);

    tempValue = sdiValue(lsdShrf);
    dynaProp.GetValue(sdiIdentifier("LSD_SHRF"), tempValue);
    tempValue.GetValue(lsdShrf);

    int ishell = 0;
    int ish3n = 0;

    vector<int> category1 = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 25, 27  };
    vector<int> category2 = { 9, 16, -16, 21, 21, 26  };
    if (std::find(category1.begin(), category1.end(), lsdElform) != category1.end())
    {
        ishell = 24;
        ish3n = 2;
    }
    else if (std::find(category2.begin(), category2.end(), lsdElform) != category2.end())
    {
        ishell = 12;
        ish3n = 2;
    }
    else if (lsdElform == 17)
    {
        ishell = 24;
        ish3n = 30;
    }
    else if (lsdElform == 18)
    {
        ishell = 24;
        ish3n = 31;
    }
    else if (lsdElform == 20)
    {
        ishell = 24;
        ish3n = 31;
    }
    else if (lsdElform == 5)
    {
        ishell = 24;
        ish3n = 2;
        lsdNip = 1;
    }
    else if (lsdElform == 9)
    {
        ishell = 12;
        ish3n = 2;
        lsdNip = 1;
    }
    else 
    {
        DynaToRad::ShowMessage(sdiMessageHandler::Level::Error, 26,
                        sourceCard.c_str(), dynaPropId, dynaPropName.c_str(), lsdElform);
    }

    destCard = "/PROP/TYPE19";

    radSh51Edit.SetValue(sdiIdentifier("plyidlistmax"), sdiValue(lsdNip));
    radSh51Edit.SetValue(sdiIdentifier("Istrain"), sdiValue(1));
    radSh51Edit.SetValue(sdiIdentifier("Ithick"), sdiValue(1));

    if (lsdShrf <= 1 )
    {
        radSh51Edit.SetValue(sdiIdentifier("Ashear"), sdiValue(lsdShrf));
    }
    else 
    {
        radSh51Edit.SetValue(sdiIdentifier("Ashear"), sdiValue(5.0/6.0));
    }

    matEntityRead.GetValue(sdiIdentifier("F"), tempValue);
    tempValue.GetValue(lsdFi);

    if (matCard.find("*MAT_LAMINATED_GLASS") != string::npos || matCard.find("*MAT_032") != string::npos)
        isMat032 = 1;


    switch (lsdEsop)
    {
        case 0:
        {
            sdiDoubleList lsdSList;
            sdiDoubleList lsdWfList;
            double lsdWfListSum = 0;

            lsdSList.reserve(lsdNip);
            lsdWfList.reserve(lsdNip);

            tempValue = sdiValue(lsdSList);
            IridRead.GetValue(sdiIdentifier("S"), tempValue);
            tempValue.GetValue(lsdSList);

            tempValue = sdiValue(lsdWfList);
            IridRead.GetValue(sdiIdentifier("WF"), tempValue);
            tempValue.GetValue(lsdWfList);

            for (int i = 0; i < lsdNip; i=i+1)
            {
                lsdWfListSum = lsdWfListSum + lsdWfList[i];
            }

            radSh51Edit.SetValue(sdiIdentifier("Ipos"), sdiValue(1));

            for (int i = 0; i < lsdNip; i=i+1)
            {
                HandleRead partHandle;
                HandleRead matHandle;
                IridRead.GetEntityHandle(sdiIdentifier("PID",0,i), partHandle);
                if (partHandle.IsValid())
                {
                    EntityRead partRead(p_lsdynaModel, partHandle);
                    partRead.GetEntityHandle(sdiIdentifier("MID"), matHandle);
                }

                HandleEdit radProperty19Handle;
                p_radiossModel->CreateEntity(radProperty19Handle,destCard, dynaPropName,p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
                if(radProperty19Handle.IsValid())
                {
                    EntityEdit radProperty19Edit(p_radiossModel, radProperty19Handle);
                    radSh51Edit.SetValue(sdiIdentifier("plyidlist",0,i), sdiValue(sdiValueEntity(radPropType,radProperty19Edit.GetId())));
                    radSh51Edit.SetValue(sdiIdentifier("Prop_Zi",0,i), sdiValue(lsdSList[i]*lsdT1 / 2.0 ));
                    radProperty19Edit.SetValue(sdiIdentifier("thickness1"), sdiValue( lsdT1 * lsdWfList[i] / lsdWfListSum ));
                    if (matHandle.IsValid()) 
                    {
                        EntityRead lsdMatRead(p_lsdynaModel, matHandle);
                        radProperty19Edit.SetValue(sdiIdentifier("material"), sdiValue(sdiValueEntity(radMatType,lsdMatRead.GetId())));
                    }
                    else
                    {
                        if (isMat032 == 0)
                        {
                            radProperty19Edit.SetValue(sdiIdentifier("material"), sdiValue(sdiValueEntity(radMatType,matEntityRead.GetId())));
                        }
                        else
                        {
                            if (lsdFi[i])
                            {
                                radProperty19Edit.SetValue(sdiIdentifier("material"), sdiValue(sdiValueEntity(radMatType,matEntityRead.GetId())));
                            }
                            else
                            {
                                unsigned int partId = p_PartBeingConverted.GetId(p_lsdynaModel);
                                HandleEdit radParHEdit;
                                p_radiossModel->FindById(p_radiossModel->GetEntityType("/PART"), partId, radParHEdit);
                                if (radParHEdit.IsValid())
                                {
                                    HandleRead matHread;
                                    radParHEdit.GetEntityHandle(p_radiossModel,sdiIdentifier("mat_ID"), matHread);
                                    if (matHread.IsValid())
                                    {
                                        matIdList.push_back(matHread.GetId(p_radiossModel));
                                        radProperty19Edit.SetValue(sdiIdentifier("material"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/MAT"), matHread.GetId(p_radiossModel))));
                                    }
                                }
                            }
                        }
                    }
                    if (radProperty19Handle.IsValid())
                    {
                        SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
                        sdiConvert::Convert::PushToConversionLog(std::make_pair(radProperty19Handle, sourceProps));
                    }
                }
            }
            break;
        }
        case 1:
        {
            for (int i = 0; i < lsdNip; i=i+1)
            {
                HandleRead partHandle;
                HandleRead matHandle;
                IridRead.GetEntityHandle(sdiIdentifier("PID",0,i), partHandle);
                if (partHandle.IsValid())
                {
                    EntityRead partRead(p_lsdynaModel, partHandle);
                    partRead.GetEntityHandle(sdiIdentifier("MID"), matHandle);
                }

                HandleEdit radProperty19Handle;
                p_radiossModel->CreateEntity(radProperty19Handle,destCard, dynaPropName,p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
                if(radProperty19Handle.IsValid())
                {
                    EntityEdit radProperty19Edit(p_radiossModel, radProperty19Handle);
                    radSh51Edit.SetValue(sdiIdentifier("plyidlist",0,i), sdiValue(sdiValueEntity(radPropType,radProperty19Edit.GetId())));
                    radProperty19Edit.SetValue(sdiIdentifier("thickness1"), sdiValue(lsdT1/(double)lsdNip));
                    if (matHandle.IsValid()) 
                    {
                        EntityRead lsdMatRead(p_lsdynaModel, matHandle);
                        radProperty19Edit.SetValue(sdiIdentifier("material"), sdiValue(sdiValueEntity(radMatType,lsdMatRead.GetId())));
                    }
                    else
                    {
                        if (isMat032 == 0)
                        {
                            radProperty19Edit.SetValue(sdiIdentifier("material"), sdiValue(sdiValueEntity(radMatType,matEntityRead.GetId())));
                        }
                        else
                        {
                            if (lsdFi[i])
                            {
                                radProperty19Edit.SetValue(sdiIdentifier("material"), sdiValue(sdiValueEntity(radMatType,matEntityRead.GetId())));
                            }
                            else
                            {
                                unsigned int partId = p_PartBeingConverted.GetId(p_lsdynaModel);
                                HandleEdit radParHEdit;
                                p_radiossModel->FindById(p_radiossModel->GetEntityType("/PART"), partId, radParHEdit);
                                if (radParHEdit.IsValid())
                                {
                                    HandleRead matHread;
                                    radParHEdit.GetEntityHandle(p_radiossModel,sdiIdentifier("mat_ID"), matHread);
                                    if (matHread.IsValid())
                                    {
                                        matIdList.push_back(matHread.GetId(p_radiossModel));
                                        radProperty19Edit.SetValue(sdiIdentifier("material"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/MAT"), matHread.GetId(p_radiossModel))));
                                    }
                                }
                            }
                        }
                    }
                }
            }
            break;
        }
    }
}


void ConvertProp::CreateDefSolidForRadSolidProp()
{
    int solPropCount{0};
    SelectionRead selPropSolid(p_radiossModel, "/PROP/TYPE14");
    solPropCount += selPropSolid.Count();
    SelectionRead selPropSolOrth(p_radiossModel, "/PROP/TYPE6");
    solPropCount += selPropSolOrth.Count();
     SelectionRead selPropVoid(p_radiossModel, "/PROP/TYPE0");
    solPropCount += selPropVoid.Count();
    if (solPropCount)
    {
        HandleEdit defSolHEdit;
        p_radiossModel->CreateEntity(defSolHEdit, "/DEF_SOLID", "DEF_SOLID");
        if (defSolHEdit.IsValid())
        {
            EntityEdit defSolEdit(p_radiossModel, defSolHEdit);
            defSolEdit.SetValue(sdiIdentifier("Ismstr"), sdiValue(-2));
            defSolEdit.SetValue(sdiIdentifier("Icpre"), sdiValue(-2));
            defSolEdit.SetValue(sdiIdentifier("Iframe"), sdiValue(-2));
        }
    }

}

void ConvertProp::ConvertSectionBeamToSpringBeam(const EntityRead& matEntityRead, const EntityRead& dynaProp, sdiString& destCard, HandleEdit& radProp)
{
    double lsdRHO;
    double lsdE;
    double lsdSIGY;
    double lsdET;
    double lsdTFAIL;
    double lsdEFAIL;
    double lsdNRR;
    double lsdNRS;
    double lsdNRT;
    double lsdMRR;
    double lsdMSS;
    double lsdMTT;
    double lsdPR;
    double lsdTS1;
    double lsdTS2;
    double lsdTT1;
    double lsdTT2;
    double meanTS;
    double meanTT;
    double piVal;

    double diam;
    double area;
    double Ixx;
    double Iyy;
    double Izz;
    double torsionMoment;
    double bendingMoment;
    double radMass;
    double radInertia;
    double radG;
    double radShear;
    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();

    destCard = "/PROP/TYPE13";
    sdiConvert::SDIHandlReadList sourcemat = { {matEntityRead.GetHandle()} };

    if (matReferenceCount > 1)
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);
    EntityEdit radPropEdit(p_radiossModel, radProp);

    vector<sdiString> attribNames({ { "Rho", "E", "LSD_MAT100_SIGY", "LSD_MAT100_ET", "TFAIL", "EFAIL", "LSD_MAT100_NRR", "LSD_MAT100_NRS", "LSD_MAT100_NRT", "LSD_MAT100_MRR", "LSD_MAT100_MSS", "LSD_MAT100_MTT", "LSD_MAT100_PR"} });
    vector<reference_wrapper<double>> attrVals({ { lsdRHO, lsdE, lsdSIGY, lsdET, lsdTFAIL, lsdEFAIL, lsdNRR, lsdNRS, lsdNRT, lsdMRR, lsdMSS, lsdMTT, lsdPR} });
    p_ConvertUtils.GetAttribValues(matEntityRead, attribNames, attrVals);

    vector<sdiString> propAttribNames({ {"TS1", "TS2", "TT1", "TT2"} });
    vector<reference_wrapper<double>> propAttrVals({ {lsdTS1, lsdTS2, lsdTT1, lsdTT2} });
    p_ConvertUtils.GetAttribValues(dynaProp, propAttribNames, propAttrVals);

    meanTS = (lsdTS1 + lsdTS2) / 2.0;
    meanTT = (lsdTT1 + lsdTT2) / 2.0;
    piVal = 3.14159265359;

    diam = meanTS - meanTT;
    area = piVal * (pow(meanTS, 2) - pow(meanTT, 2)) / 4.0;
    Ixx = piVal * (pow(meanTS, 4) - pow(meanTT, 4)) / 32.0;
    Iyy = piVal * piVal * (pow(meanTS, 4) - pow(meanTT, 4)) / 64.0;
    Izz = Iyy;
    torsionMoment = (pow(meanTS, 3) - pow(meanTT, 3)) / (2 * piVal);
    bendingMoment = (pow(meanTS, 3) - pow(meanTT, 3)) * piVal / 16.0;

    radMass = lsdRHO * area;
    radInertia = lsdRHO * Ixx;
    radShear = 7.0 / 8.0;
    radG = lsdE / (2 * (1.0 + lsdPR));

    if (lsdSIGY == 0.0)
        lsdSIGY = lsdE / 100.0;

    radPropEdit.SetValue(sdiIdentifier("Mass"), sdiValue(radMass));
    radPropEdit.SetValue(sdiIdentifier("Inertia"), sdiValue(radInertia));
    radPropEdit.SetValue(sdiIdentifier("K1"), sdiValue(area * lsdE));
    radPropEdit.SetValue(sdiIdentifier("K2"), sdiValue(area * lsdE / (2 * (1 + lsdPR))));
    radPropEdit.SetValue(sdiIdentifier("K3"), sdiValue(area * lsdE / (2 * (1 + lsdPR))));
    radPropEdit.SetValue(sdiIdentifier("K4"), sdiValue(lsdE * (pow(meanTS, 4) - pow(meanTT, 4)) / (9.0 * piVal)));
    radPropEdit.SetValue(sdiIdentifier("K5"), sdiValue(lsdE * (pow(meanTS, 4) - pow(meanTT, 4)) * 3.0 / 64.0));
    radPropEdit.SetValue(sdiIdentifier("K6"), sdiValue(lsdE * (pow(meanTS, 4) - pow(meanTT, 4)) * 3.0 / 64.0));

    if (lsdSIGY > 0)
    {
        HandleEdit curveEdit;

        double tempAbscissa = lsdSIGY / lsdE;
        p_ConvertUtils.CreateCurve(dynaPropName + "fct_ID11", 5, { { -tempAbscissa - 1, -1 * (lsdSIGY + lsdET) * area ,
                                                        -tempAbscissa, -1 * lsdSIGY * area,
                                                         0.0,  0.0,
                                                        tempAbscissa, lsdSIGY * area,
                                                        tempAbscissa + 1, (lsdSIGY + lsdET) * area} }, curveEdit);

        if (curveEdit.IsValid())
        {
            radPropEdit.SetEntityHandle(sdiIdentifier("fct_ID11"), curveEdit);
            sdiConvert::Convert::PushToConversionLog(std::make_pair(curveEdit, sourcemat));
        }

        tempAbscissa = (lsdSIGY * 0.5) / (radG * radShear);
        double tempOrdinate = (lsdSIGY * area * 0.5) / radShear;
        p_ConvertUtils.CreateCurve(dynaPropName + "fct_ID23", 5, { { -tempAbscissa - 1, -1 * tempOrdinate - lsdET * area * 5.0 / 16.0 ,
                                                        -tempAbscissa, -1 * tempOrdinate,
                                                         0.0,  0.0,
                                                        tempAbscissa, tempOrdinate,
                                                        tempAbscissa + 1, tempOrdinate + lsdET * area * 5.0 / 16.0} }, curveEdit);

        if (curveEdit.IsValid())
        {
            radPropEdit.SetEntityHandle(sdiIdentifier("fct_ID12"), curveEdit);
            radPropEdit.SetEntityHandle(sdiIdentifier("fct_ID13"), curveEdit);
            sdiConvert::Convert::PushToConversionLog(std::make_pair(curveEdit, sourcemat));
        }

        tempAbscissa = lsdSIGY / lsdE;
        tempOrdinate = lsdSIGY * torsionMoment;
        p_ConvertUtils.CreateCurve(dynaPropName + "fct_ID4", 5, { { (-tempAbscissa - 1) * 9.0 / (2.0 * diam), -1 * tempOrdinate - lsdET * torsionMoment ,
                                                        -tempAbscissa * 9.0 / (2 * diam), -tempOrdinate,
                                                         0.0,  0.0,
                                                        (tempAbscissa * 9.0) / (2 * diam), tempOrdinate,
                                                        (tempAbscissa + 1) * 9.0 / (2.0 * diam), tempOrdinate + lsdET * torsionMoment} }, curveEdit);

        if (curveEdit.IsValid())
        {
            radPropEdit.SetEntityHandle(sdiIdentifier("fct_ID14"), curveEdit);
            sdiConvert::Convert::PushToConversionLog(std::make_pair(curveEdit, sourcemat));
        }

        tempAbscissa = (lsdSIGY * 4.0 * piVal) / (lsdE * 3.0 * diam);
        tempOrdinate = lsdSIGY * bendingMoment;

        p_ConvertUtils.CreateCurve(dynaPropName + "fct_ID56", 5, { { -1 * tempAbscissa - (piVal / diam), -1 * tempOrdinate - lsdET * bendingMoment,
                                                        -1 * tempAbscissa, -1 * tempOrdinate,
                                                         0.0,  0.0,
                                                        tempAbscissa, tempOrdinate,
                                                        tempAbscissa + (piVal / diam), tempOrdinate + lsdET * bendingMoment } }, curveEdit);

        if (curveEdit.IsValid())
        {
            radPropEdit.SetEntityHandle(sdiIdentifier("fct_ID15"), curveEdit);
            radPropEdit.SetEntityHandle(sdiIdentifier("fct_ID16"), curveEdit);
            sdiConvert::Convert::PushToConversionLog(std::make_pair(curveEdit, sourcemat));
        }

        /*
        if (lsdTFAIL > 0.0)
        {
            HandleEdit sensorEdit;
            p_radiossModel->CreateEntity(sensorEdit, "/SENSOR/TIME", dynaPropName, dynaPropId);

            if (sensorEdit.IsValid())
            {
                sdiConvert::Convert::PushToConversionLog(std::make_pair(sensorEdit, sourcemat));
            }
        }
        */

        if (lsdEFAIL != 0.0)
        {
            for (int i = 1; i <= 6; ++i)
            {
                radPropEdit.SetValue(sdiIdentifier("DeltaMin" + to_string(i)), sdiValue(-lsdEFAIL));
                radPropEdit.SetValue(sdiIdentifier("DeltaMax" + to_string(i)), sdiValue(lsdEFAIL));
            }
        }

        if (lsdNRR != 0.0 || lsdNRS != 0.0 || lsdNRT != 0.0 || lsdMRR != 0.0 || lsdMSS != 0 || lsdMTT != 0.0)
            radPropEdit.SetValue(sdiIdentifier("Ifail2"), sdiValue(2));

        if (lsdNRR != 0)
        {
            radPropEdit.SetValue(sdiIdentifier("DeltaMin1"), sdiValue(-lsdNRR));
            radPropEdit.SetValue(sdiIdentifier("DeltaMax1"), sdiValue(lsdNRR));
        }

        if (lsdNRS != 0.0)
        {
            radPropEdit.SetValue(sdiIdentifier("DeltaMin2"), sdiValue(-lsdNRS));
            radPropEdit.SetValue(sdiIdentifier("DeltaMax2"), sdiValue(lsdNRS));
        }

        if (lsdNRT != 0.0)
        {
            radPropEdit.SetValue(sdiIdentifier("DeltaMin3"), sdiValue(-lsdNRT));
            radPropEdit.SetValue(sdiIdentifier("DeltaMax3"), sdiValue(lsdNRT));
        }

        if (lsdMRR != 0.0)
        {
            radPropEdit.SetValue(sdiIdentifier("DeltaMin4"), sdiValue(-lsdMRR));
            radPropEdit.SetValue(sdiIdentifier("DeltaMax4"), sdiValue(lsdMRR));
        }

        if (lsdMSS != 0.0)
        {
            radPropEdit.SetValue(sdiIdentifier("DeltaMin5"), sdiValue(-lsdMSS));
            radPropEdit.SetValue(sdiIdentifier("DeltaMax5"), sdiValue(lsdMSS));
        }

        if (lsdMTT != 0.0)
        {
            radPropEdit.SetValue(sdiIdentifier("DeltaMin6"), sdiValue(-lsdMTT));
            radPropEdit.SetValue(sdiIdentifier("DeltaMax6"), sdiValue(lsdMTT));
        }

        for (int i = 1; i <= 6; ++i)
        {
            radPropEdit.SetValue(sdiIdentifier("H" + to_string(i)), sdiValue(1));
        }
        radPropEdit.SetValue(sdiIdentifier("Ifail"), sdiValue(1));
        radPropEdit.SetValue(sdiIdentifier("Ileng"), sdiValue(1));
    }
}

void sdiD2R::ConvertProp::p_CopyNumIntPtsFromMatAddErosion(const EntityRead& matEntityRead, HandleEdit& radProp, const EntityRead& dynaProp, int nip)
{
    unsigned int matId = matEntityRead.GetId();
    HandleEdit FailEdit;
    double lsdNUMFIP = 0.0;
    double n_coef = 0.0;
    double radPthickfail = 0.0;
    sdiValue tempValue;

    SelectionRead selMatAddErosion(p_lsdynaModel, "*MAT_ADD_EROSION", FilterValue(sdiIdentifier("MID"), sdiValue(sdiValueEntity(p_lsdynaModel->GetEntityType("*MAT"), matId))));
    auto count = selMatAddErosion.Count();

    int lsdelform;
    dynaProp.GetValue(sdiIdentifier("ELFORM"), tempValue);
    tempValue.GetValue(lsdelform);

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

    SelectionEdit selFailTab2(p_radiossModel, "/FAIL/TAB2");

    while (selMatAddErosion.Next())
    {
        selMatAddErosion->GetValue(sdiIdentifier("NUMFIP"), tempValue);
        tempValue.GetValue(lsdNUMFIP);

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
                radVOLFRAC = abs(lsdNUMFIP)/(lsdGauss*nip);

                radProp.SetValue(p_radiossModel,sdiIdentifier("P_Thick_Fail"), sdiValue(radVOLFRAC));
                while (selFailTab2.Next())
                {
                    sdiValueEntity lsdMIDEntity;
                    tempValue = sdiValue(lsdMIDEntity);
                    selFailTab2->GetValue(sdiIdentifier("mat_id"), tempValue);
                    tempValue.GetValue(lsdMIDEntity);

                    if (matId == lsdMIDEntity.GetId())
                    {
                        selFailTab2->SetValue(sdiIdentifier("PTHICKFAIL"),sdiValue(radVOLFRAC));
                    }
                }
            }
            else if(lsdNUMFIP < 0 && lsdNUMFIP >= -100)
            {
                radPthickfail = abs(lsdNUMFIP)/100.0;
                radProp.SetValue(p_radiossModel,sdiIdentifier("P_Thick_Fail"), sdiValue(radPthickfail));
                while (selFailTab2.Next())
                {
                    sdiValueEntity lsdMIDEntity;
                    tempValue = sdiValue(lsdMIDEntity);
                    selFailTab2->GetValue(sdiIdentifier("mat_id"), tempValue);
                    tempValue.GetValue(lsdMIDEntity);

                    if (matId == lsdMIDEntity.GetId())
                    {
                        selFailTab2->SetValue(sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                    }
                }
            }
            else if(lsdNUMFIP < -100)
            {
                radPthickfail = (abs(lsdNUMFIP)-100.0)/100.0;
                radProp.SetValue(p_radiossModel,sdiIdentifier("P_Thick_Fail"), sdiValue(radPthickfail));
                while (selFailTab2.Next())
                {
                    sdiValueEntity lsdMIDEntity;
                    tempValue = sdiValue(lsdMIDEntity);
                    selFailTab2->GetValue(sdiIdentifier("mat_id"), tempValue);
                    tempValue.GetValue(lsdMIDEntity);

                    if (matId == lsdMIDEntity.GetId())
                    {
                        selFailTab2->SetValue(sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                    }
                }
            } // if(lsdNUMFIP > 0)
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
                n_coef = ceil((abs(lsdNUMFIP)/100.0)*nip);
            }
            else if(lsdNUMFIP < -100)
            {
                n_coef = ceil((abs(lsdNUMFIP-100.0)/100.0)*nip);
            } // if(lsdNUMFIP > 0)
 
            if (nip > 1) radPthickfail = (2.0*n_coef-1)*NipWeights[nip];
            radProp.SetValue(p_radiossModel,sdiIdentifier("P_Thick_Fail"), sdiValue(radPthickfail));
            while (selFailTab2.Next())
            {
                sdiValueEntity lsdMIDEntity;
                tempValue = sdiValue(lsdMIDEntity);
                selFailTab2->GetValue(sdiIdentifier("mat_id"), tempValue);
                tempValue.GetValue(lsdMIDEntity);

                if (matId == lsdMIDEntity.GetId())
                {
                    selFailTab2->SetValue(sdiIdentifier("PTHICKFAIL"),sdiValue(radPthickfail));
                }
            }
        }
    }
}

void sdiD2R::ConvertProp::p_CopyNumIntPtsFromMatPlasticity(const EntityRead& matEntityRead, HandleEdit& radProp, int nip)
{
    int lsdNUMINT = 0;
    unsigned int matId = matEntityRead.GetId();

    if (nip == 0.0)
        return;

    sdiValue tempValue(lsdNUMINT);
    matEntityRead.GetValue(sdiIdentifier("NUMINT"), tempValue);
    tempValue.GetValue(lsdNUMINT);

    SelectionEdit selFailTab(p_radiossModel, "/FAIL/TAB1");
    while (selFailTab.Next())
    {
        sdiValueEntity lsdMIDEntity;
        tempValue = sdiValue(lsdMIDEntity);
        selFailTab->GetValue(sdiIdentifier("mat_id"), tempValue);
        tempValue.GetValue(lsdMIDEntity);

        if (matId == lsdMIDEntity.GetId())
        {
            selFailTab->SetValue(sdiIdentifier("P_THICKFAIL"), sdiValue(lsdNUMINT * 1.0 / nip));
        }
    }
}


void ConvertProp::p_ConvertSectionSeatbeltToProp23(const EntityRead& matEntityRead, const EntityRead& dynaProp, sdiString& destCard, HandleEdit& radProp)
{
    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();

    destCard = "/PROP/TYPE23";

    if (matReferenceCount > 1)
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);
    EntityEdit radPropEdit(p_radiossModel, radProp);



    double lsdAREA = 0.0;

    sdiValue tempVal(lsdAREA);
    matEntityRead.GetValue(sdiIdentifier("LSD_MAT_SEATBELT_A"), tempVal);
    tempVal.GetValue(lsdAREA);

    if (lsdAREA != 0.0)
      {
        radProp.SetValue(p_radiossModel, sdiIdentifier("Imass"), sdiValue(1));
        radProp.SetValue(p_radiossModel, sdiIdentifier("AREA"), sdiValue(lsdAREA));
      }
    else if (lsdAREA == 0.0)
      {
      radProp.SetValue(p_radiossModel, sdiIdentifier("Imass"), sdiValue(2));
      lsdAREA = 1.0;
      radProp.SetValue(p_radiossModel, sdiIdentifier("Volume"), sdiValue(lsdAREA));
      }


    sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
    sdiConvert::Convert::PushToConversionLog(std::make_pair(radProp, sourceProps));

}



void ConvertProp::ConvertSecBeamRelatedMatMuscle(const EntityRead& matEntityRead, const EntityRead& dynaProp, sdiString& destCard, HandleEdit& radProp)
{
    unsigned int dynaMatId = matEntityRead.GetId();
    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();
    destCard = "/PROP/SPR_MUSCLE";

    EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");

    if (p_radiossModel->IsIdAvailable(destEntityType, dynaPropId))
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);
    else
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));

    sdiValue tempVal;
    double lsdArea = 0.0;
    tempVal = sdiValue(lsdArea);
    dynaProp.GetValue(sdiIdentifier("A"), tempVal);
    tempVal.GetValue(lsdArea);

    EntityEdit radPropEdit(p_radiossModel, radProp);
    HandleRead matHandle;
    p_PartBeingConverted.GetEntityHandle(p_lsdynaModel, sdiIdentifier("MID"), matHandle);
    if (matHandle.IsValid())
    {
        EntityRead matEntityRead(p_lsdynaModel, matHandle);

        double lsdSrm = 0.0;
        tempVal = sdiValue(lsdSrm);
        matEntityRead.GetValue(sdiIdentifier("SRM"), tempVal);
        tempVal.GetValue(lsdSrm);
        radProp.SetValue(p_radiossModel, sdiIdentifier("Vel_max"), tempVal);

        double lsdDmp = 0.0;
        tempVal = sdiValue(lsdDmp);
        matEntityRead.GetValue(sdiIdentifier("DMP"), tempVal);
        tempVal.GetValue(lsdDmp);
        radProp.SetValue(p_radiossModel, sdiIdentifier("Damp"), tempVal);

        double lsdRo = 0.0;
        tempVal = sdiValue(lsdRo);
        matEntityRead.GetValue(sdiIdentifier("RO"), tempVal);
        tempVal.GetValue(lsdRo);

        double Mass;
        Mass = lsdRo*lsdArea;
        radProp.SetValue(p_radiossModel, sdiIdentifier("Mass"), sdiValue(Mass));

        double lsdPis = 0.0, Force = 0.0, scaleF = 0.0;
        tempVal = sdiValue(lsdPis);
        matEntityRead.GetValue(sdiIdentifier("PIS"), tempVal);
        tempVal.GetValue(lsdPis);
        if(lsdPis > 0.0)
          Force = lsdPis*lsdArea;
        else
          Force = lsdArea;
        radProp.SetValue(p_radiossModel, sdiIdentifier("Force"), sdiValue(Force));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Scale_F"), sdiValue(Force));

        double lsdCER = 0.0;
        tempVal = sdiValue(lsdCER);
        matEntityRead.GetValue(sdiIdentifier("CER"), tempVal);
        tempVal.GetValue(lsdCER);

        double lsdSSM = 0.0;
        tempVal = sdiValue(lsdSSM);
        matEntityRead.GetValue(sdiIdentifier("SSM"), tempVal);
        tempVal.GetValue(lsdSSM);

        sdiValueEntity lcidEntity;
        unsigned int lcid = 0;

        int flagALM = 0;
        tempVal = sdiValue(flagALM);
        matEntityRead.GetValue(sdiIdentifier("FLAG_ALM"), tempVal);
        tempVal.GetValue(flagALM);

        if(flagALM == 0)
        {
          // SCALAR
          double lsdALM = 0.0;
          tempVal = sdiValue(lsdALM);
          matEntityRead.GetValue(sdiIdentifier("ALM"), tempVal);
          tempVal.GetValue(lsdALM);

          if(lsdALM >= 0.0)
          {
            // create new "fct_id1"
            // --------------------
            HandleEdit functEdit;
            p_ConvertUtils.CreateCurve("NewCurve_MatL156_ALM_" + to_string(dynaMatId), 2, 
                                       { { 0.0, lsdALM , 1.0, lsdALM } }, functEdit);

            if (functEdit.IsValid())
            {
                radProp.SetValue(p_radiossModel,sdiIdentifier("fct_id1"), sdiValue(sdiValueEntity(radFunctType, 
                                            functEdit.GetId(p_radiossModel))));
                sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(functEdit, sourceProps));
            }
          }
        }
        else
        {
          // FUNCTION
          tempVal = sdiValue(lcidEntity);
          matEntityRead.GetValue(sdiIdentifier("ALM"), tempVal);
          tempVal.GetValue(lcidEntity);
          radProp.SetValue(p_radiossModel, sdiIdentifier("fct_id1"), sdiValue(lcidEntity));
          lcid = lcidEntity.GetId();
        }

        int flagSVS = 0;
        tempVal = sdiValue(flagSVS);
        matEntityRead.GetValue(sdiIdentifier("FLAG_SVS"), tempVal);
        tempVal.GetValue(flagSVS);

        if(flagSVS == 0)
        {
          // SCALAR
          double lsdSVS = 0.0;
          tempVal = sdiValue(lsdSVS);
          matEntityRead.GetValue(sdiIdentifier("SVS"), tempVal);
          tempVal.GetValue(lsdSVS);

          if(lsdSVS >= 0.0)
          {
            /* convert to constant ordinate curves */
            // create new "fct_id2"
            // --------------------

            HandleEdit functEdit;
            p_ConvertUtils.CreateCurve("NewCurve_MatL156_SVS_" + to_string(dynaMatId), 2, 
                                       { { 0.0, 1.0 , 1.0, 1.0 } }, functEdit);

            if (functEdit.IsValid())
            {
                radProp.SetValue(p_radiossModel,sdiIdentifier("fct_id2"), sdiValue(sdiValueEntity(radFunctType, 
                                            functEdit.GetId(p_radiossModel))));
                sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(functEdit, sourceProps));
            }
          }
        }
        else
        {
          // FUNCTION
          //tempVal = sdiValue(lcidEntity);
          //matEntityRead.GetValue(sdiIdentifier("SVS"), tempVal);
          //tempVal.GetValue(lcidEntity);
          //radProp.SetValue(p_radiossModel, sdiIdentifier("fct_id2"), sdiValue(lcidEntity));
          

          /* convert to constant ordinate curves */
          // create new "fct_id2", with abscisa shifted by -1
          // --------------------

          HandleRead lcSVSHandle;
          matEntityRead.GetEntityHandle(sdiIdentifier("SVS"), lcSVSHandle);

          if (lcSVSHandle.IsValid())
          {
              // create a modified copy of SVS (abscisa shifted by -1)--> "fct_id2"

              EntityRead lcSVSEntRead(p_lsdynaModel, lcSVSHandle);
              EntityId lcSVSId = lcSVSEntRead.GetId();

              int nPnts = 0;
              sdiDoubleList crvPoints;

              double lsdSFA = 1.0;
              double lsdSFO = 1.0;
              double lsdOFFA = 0.0;
              double lsdOFFO = 0.0;

              tempVal = sdiValue(crvPoints);
              lcSVSEntRead.GetValue(sdiIdentifier("points"), tempVal);
              tempVal.GetValue(crvPoints);

              tempVal = sdiValue(nPnts);
              lcSVSEntRead.GetValue(sdiIdentifier("numberofpoints"), tempVal);
              tempVal.GetValue(nPnts);
              crvPoints.reserve(2 * nPnts + 2);

              vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
              vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
              p_ConvertUtils.GetAttribValues(lcSVSEntRead, lsdQueryAttribs, attribVals);
              lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
              lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
              //-----------------------
              // shift abscisa of SVS function, by -1

              for (size_t j = 0; j < crvPoints.size(); j += 2)
              {
                  crvPoints[j] = crvPoints[j]-1;
              }

              //-----------------------
              HandleEdit functHEdit;
              p_ConvertUtils.CreateCurve("Recalculated_" + to_string(lcSVSId) + "_MAT_MUSCLE_" + to_string(dynaMatId),
                                        (int)crvPoints.size() / 2, crvPoints, functHEdit, lsdSFA, lsdSFO, lsdOFFA);
              if (functHEdit.IsValid())
              {
                 //radProp.SetEntityHandle(p_radiossModel,sdiIdentifier("fct_id2"), functHEdit);

                 radProp.SetValue(p_radiossModel,sdiIdentifier("fct_id2"), sdiValue(sdiValueEntity(radFunctType, 
                                  functHEdit.GetId(p_radiossModel))));
                 sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
                 sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourceProps));
              }
          }
        }

        int flagSVR = 0;
        tempVal = sdiValue(flagSVR);
        matEntityRead.GetValue(sdiIdentifier("FLAG_SVR"), tempVal);
        tempVal.GetValue(flagSVR);

        if(flagSVR == 0)
        {
          // SCALAR
          double lsdSVR = 0.0;
          tempVal = sdiValue(lsdSVR);
          matEntityRead.GetValue(sdiIdentifier("SVR"), tempVal);
          tempVal.GetValue(lsdSVR);

          if(lsdSVR >= 0.0)
          {
            /* convert to constant ordinate curves */
            // create new "fct_id3"
            // --------------------

            HandleEdit functEdit;
            p_ConvertUtils.CreateCurve("NewCurve_MatL156_SVR_" + to_string(dynaMatId), 2, 
                                       { { 0.0, 1.0 , 1.0, 1.0 } }, functEdit);

            if (functEdit.IsValid())
            {
                radProp.SetValue(p_radiossModel,sdiIdentifier("fct_id3"), sdiValue(sdiValueEntity(radFunctType, 
                                            functEdit.GetId(p_radiossModel))));
                sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(functEdit, sourceProps));
            }
          }
        }
        else
        {
          // FUNCTION
          tempVal = sdiValue(lcidEntity);
          matEntityRead.GetValue(sdiIdentifier("SVR"), tempVal);
          tempVal.GetValue(lcidEntity);
          radProp.SetValue(p_radiossModel, sdiIdentifier("fct_id3"), sdiValue(lcidEntity));
          lcid = lcidEntity.GetId();
        }

        int flagSSP = 0;
        tempVal = sdiValue(flagSSP);
        matEntityRead.GetValue(sdiIdentifier("FLAG_SSP"), tempVal);
        tempVal.GetValue(flagSSP);


        if(flagSSP == 0)
        {
            // SCALAR
            double lsdSSP = 0.0;
            tempVal = sdiValue(lsdSSP);
            matEntityRead.GetValue(sdiIdentifier("SSP"), tempVal);
            tempVal.GetValue(lsdSSP);

            if(lsdSSP == 0.0)
            {
                // create new "fct_id4"
                // --------------------
                double ordY,C0,C1,C2;
                int nPoints = 102;
                sdiDoubleList crvPoints;
                crvPoints.reserve(2*nPoints);

                int j=0;
                vector<double> absX = { -1.0 ,0.0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,
                                             0.10,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,
                                             0.20,0.21,0.22,0.23,0.24,0.25,0.26,0.27,0.28,0.29,
                                             0.30,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,
                                             0.40,0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,
                                             0.50,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,
                                             0.60,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,
                                             0.70,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,
                                             0.80,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,
                                             0.90,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,
                                         1.0 };

                for (int i = 0; i < 2*nPoints; i += 2)
                {
                    if(absX[j] < 0.0)
                    {
                        ordY = 0.0;
                    }
                    else if(absX[j] >= 0.0 && (lsdCER < 0.0 || lsdCER > 0.0))
                    {
                        C0 = lsdCER*absX[j]/lsdSSM;
                        C1 = exp(C0)-1.0;
                        C2 = exp(lsdCER)-1.0;
                        ordY = C1/C2;
                    }
                    else if(absX[j] >= 0.0 && lsdCER == 0.0)
                    {
                        ordY = absX[j]/lsdSSM;
                    }
                    crvPoints.push_back(absX[j]);
                    crvPoints.push_back(ordY);
                    j=j+1;
                }

                HandleEdit functEdit;
                p_ConvertUtils.CreateCurve("NewCurve_MatL156_SSP_" + to_string(dynaMatId), (int)nPoints, crvPoints, functEdit);

                if (functEdit.IsValid())
                {
                    radProp.SetValue(p_radiossModel,sdiIdentifier("fct_id4"), sdiValue(sdiValueEntity(radFunctType, 
                                                functEdit.GetId(p_radiossModel))));
                    sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(functEdit, sourceProps));
                }
            }
            else if(lsdSSP > 0.0)
            {
            // F_4=0
            }
        }
        else
        {
            // FUNCTION
            tempVal = sdiValue(lcidEntity);
            matEntityRead.GetValue(sdiIdentifier("SSP"), tempVal);
            tempVal.GetValue(lcidEntity);
            radProp.SetValue(p_radiossModel, sdiIdentifier("fct_id4"), sdiValue(lcidEntity));
            lcid = lcidEntity.GetId();
        } // if(flagSSP == 0)

        // not translated, set by default:
        radProp.SetValue(p_radiossModel, sdiIdentifier("Idens"), sdiValue(0));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Epsi"), sdiValue(0));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Stiffness"), sdiValue(0.0));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Xk"), sdiValue(0.0));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Scale_t"), sdiValue(0.0));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Scale_x"), sdiValue(0.0));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Scale_v"), sdiValue(0.0));

        sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(radProp, sourceProps));
    }
}
void ConvertProp::p_ConvertSectionDiscreteMuscle(const EntityRead& matEntityRead, const EntityRead& dynaProp, sdiString& destCard, HandleEdit& radProp)
{
    sdiString matCard = matEntityRead.GetKeyword();
    unsigned int dynaMatId = matEntityRead.GetId();
    EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");
    size_t pos = matCard.find("_TITLE");

    if (pos != string::npos)
    {
        matCard.erase(pos);
    }

    unsigned short int matLawNum = dynaMatLawMap[matCard];
    destCard = "/PROP/TYPE46";;
    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();
    if (matReferenceCount > 1)
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);

//---------------
    sdiValue tempVal;
    double lsdL0 = 0.0;
    tempVal = sdiValue(lsdL0);
    matEntityRead.GetValue(sdiIdentifier("L0"), tempVal);
    tempVal.GetValue(lsdL0);

    double lsdVMAX = 0.0;
    tempVal = sdiValue(lsdVMAX);
    matEntityRead.GetValue(sdiIdentifier("VMAX"), tempVal);
    tempVal.GetValue(lsdVMAX);
//---------------

    EntityEdit radPropEntity(p_radiossModel, radProp);

    p_ConvertUtils.CopyValue(matEntityRead, radPropEntity, "VMAX", "Vel_max");
    p_ConvertUtils.CopyValue(matEntityRead, radPropEntity, "FMAX", "Force");
    radPropEntity.SetValue(sdiIdentifier("Epsi"), sdiValue(1));


    int Aoption = 0;
    matEntityRead.GetValue(sdiIdentifier("Aoption"), tempVal);
    tempVal.GetValue(Aoption);

    sdiValueEntity lcidEntity;
    if(Aoption <= 0)
    {
        // SCALAR --> crete NEW FUNCTION
        double lsdA = 0.0;
        tempVal = sdiValue(lsdA);
        matEntityRead.GetValue(sdiIdentifier("A"), tempVal);
        tempVal.GetValue(lsdA);

        HandleEdit functEdit;
        p_ConvertUtils.CreateCurve("New_curve_Mat_S15_A_" + to_string(dynaMatId), 2, 
                                   { { 0.0, lsdA , 1.0, lsdA } }, functEdit);

        if (functEdit.IsValid())
        {
            radProp.SetValue(p_radiossModel,sdiIdentifier("fct_id1"), sdiValue(sdiValueEntity(radFunctType, 
                                        functEdit.GetId(p_radiossModel))));
            sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(functEdit, sourceProps));
        }
    }
    else
    {
        // FUNCTION
        tempVal = sdiValue(lcidEntity);
        matEntityRead.GetValue(sdiIdentifier("LSD_AID"), tempVal);
        tempVal.GetValue(lcidEntity);
        radProp.SetValue(p_radiossModel, sdiIdentifier("fct_id1"), sdiValue(sdiValueEntity(radFunctType, lcidEntity.GetId())));
    }

    int TLoption = 0;
    matEntityRead.GetValue(sdiIdentifier("TLoption"), tempVal);
    tempVal.GetValue(TLoption);

    if(TLoption <= 0)
    {
        // SCALAR --> crete NEW FUNCTION
        double lsdTL = 0.0;
        tempVal = sdiValue(lsdTL);
        matEntityRead.GetValue(sdiIdentifier("TL"), tempVal);
        tempVal.GetValue(lsdTL);

        HandleEdit functEdit;
        p_ConvertUtils.CreateCurve("New_curve_Mat_S15_TL_" + to_string(dynaMatId), 2, 
                                   { { 0.0, 1.0 , 1.0, 1.0 } }, functEdit);

        if (functEdit.IsValid())
        {
            radProp.SetValue(p_radiossModel,sdiIdentifier("fct_id2"), sdiValue(sdiValueEntity(radFunctType, 
                                        functEdit.GetId(p_radiossModel))));
            sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(functEdit, sourceProps));
        }
    }
    else
    {
        // FUNCTION --> modify FUNCTION

        /* convert to constant ordinate curves */
        // create new "fct_id2", with abscissa_FctId2 = ( abscissa_TL - 1 ) * L0
        // --------------------

        HandleRead lcTLHandle;
        matEntityRead.GetEntityHandle(sdiIdentifier("LSD_TLID"), lcTLHandle);

        if (lcTLHandle.IsValid())
        {
            EntityRead lcTLEntRead(p_lsdynaModel, lcTLHandle);
            EntityId lcTLId = lcTLEntRead.GetId();

            int nPnts = 0;
            sdiDoubleList crvPoints;

            double lsdSFA = 1.0;
            double lsdSFO = 1.0;
            double lsdOFFA = 0.0;
            double lsdOFFO = 0.0;

            tempVal = sdiValue(crvPoints);
            lcTLEntRead.GetValue(sdiIdentifier("points"), tempVal);
            tempVal.GetValue(crvPoints);

            tempVal = sdiValue(nPnts);
            lcTLEntRead.GetValue(sdiIdentifier("numberofpoints"), tempVal);
            tempVal.GetValue(nPnts);
            crvPoints.reserve(2 * nPnts + 2);

            vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
            vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
            p_ConvertUtils.GetAttribValues(lcTLEntRead, lsdQueryAttribs, attribVals);
            lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
            lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
            //-----------------------

            for (size_t j = 0; j < crvPoints.size(); j += 2)
            {
                crvPoints[j] = (crvPoints[j]-1)*lsdL0;
            }

            //-----------------------
            HandleEdit functHEdit;
            p_ConvertUtils.CreateCurve("New_recalculated_curve_Mat_S15_TL_" + to_string(dynaMatId),
                                      (int)crvPoints.size() / 2, crvPoints, functHEdit, lsdSFA, lsdSFO, lsdOFFA);
            if (functHEdit.IsValid())
            {
               //radProp.SetEntityHandle(p_radiossModel,sdiIdentifier("fct_id2"), functHEdit);

               radProp.SetValue(p_radiossModel,sdiIdentifier("fct_id2"), sdiValue(sdiValueEntity(radFunctType, 
                                functHEdit.GetId(p_radiossModel))));
               sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
               sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourceProps));
            }
        }
    }

    int TVoption = 0;
    matEntityRead.GetValue(sdiIdentifier("TVoption"), tempVal);
    tempVal.GetValue(TVoption);

    if(TVoption <= 0)
    {
        // SCALAR --> crete NEW FUNCTION
        double lsdTV = 0.0;
        tempVal = sdiValue(lsdTV);
        matEntityRead.GetValue(sdiIdentifier("TV"), tempVal);
        tempVal.GetValue(lsdTV);

        HandleEdit functEdit;
        p_ConvertUtils.CreateCurve("New_curve_Mat_S15_TV_" + to_string(dynaMatId), 2, 
                                   { { 0.0, 1.0 , 1.0, 1.0 } }, functEdit);

        if (functEdit.IsValid())
        {
            radProp.SetValue(p_radiossModel,sdiIdentifier("fct_id3"), sdiValue(sdiValueEntity(radFunctType, 
                                        functEdit.GetId(p_radiossModel))));
            sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(functEdit, sourceProps));
        }
    }
    else
    {
        // FUNCTION --> modify FUNCTION

        /* convert to constant ordinate curves */
        // create new "fct_id3", with abscissa_FctId3 = abscissa_TV * VMAX
        // --------------------

        HandleRead lcTVHandle;
        matEntityRead.GetEntityHandle(sdiIdentifier("LSD_TVID"), lcTVHandle);

        if (lcTVHandle.IsValid())
        {
            EntityRead lcTVEntRead(p_lsdynaModel, lcTVHandle);
            EntityId lcTVId = lcTVEntRead.GetId();

            int nPnts = 0;
            sdiDoubleList crvPoints;

            double lsdSFA = 1.0;
            double lsdSFO = 1.0;
            double lsdOFFA = 0.0;
            double lsdOFFO = 0.0;

            tempVal = sdiValue(crvPoints);
            lcTVEntRead.GetValue(sdiIdentifier("points"), tempVal);
            tempVal.GetValue(crvPoints);

            tempVal = sdiValue(nPnts);
            lcTVEntRead.GetValue(sdiIdentifier("numberofpoints"), tempVal);
            tempVal.GetValue(nPnts);
            crvPoints.reserve(2 * nPnts + 2);

            vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
            vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
            p_ConvertUtils.GetAttribValues(lcTVEntRead, lsdQueryAttribs, attribVals);
            lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
            lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
            //-----------------------

            for (size_t j = 0; j < crvPoints.size(); j += 2)
            {
                crvPoints[j] = crvPoints[j]*lsdVMAX;
            }

            //-----------------------
            HandleEdit functHEdit;
            p_ConvertUtils.CreateCurve("New_recalculated_curve_Mat_S15_TL_" + to_string(dynaMatId),
                                      (int)crvPoints.size() / 2, crvPoints, functHEdit, lsdSFA, lsdSFO, lsdOFFA);
            if (functHEdit.IsValid())
            {
               //radProp.SetEntityHandle(p_radiossModel,sdiIdentifier("fct_id3"), functHEdit);

               radProp.SetValue(p_radiossModel,sdiIdentifier("fct_id3"), sdiValue(sdiValueEntity(radFunctType, 
                                functHEdit.GetId(p_radiossModel))));
               sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
               sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourceProps));
            }
        }
    }

    int FPEoption = 0;
    matEntityRead.GetValue(sdiIdentifier("FPEoption"), tempVal);
    tempVal.GetValue(FPEoption);

    if(FPEoption == 0)
    {
        // SCALAR --> crete NEW FUNCTION
        double lsdFPE = 0.0;
        tempVal = sdiValue(lsdFPE);
        matEntityRead.GetValue(sdiIdentifier("FPE"), tempVal);
        tempVal.GetValue(lsdFPE);

        if(lsdFPE > 0.0)
        {
            // constant value of 0.0 is used --> create new function Fct_Id4 = 0

            HandleEdit functEdit;
            p_ConvertUtils.CreateCurve("New_curve_Mat_S15_TV_" + to_string(dynaMatId), 2, 
                                       { { 0.0, 0.0 , 1.0, 0.0 } }, functEdit);

            if (functEdit.IsValid())
            {
                radProp.SetValue(p_radiossModel,sdiIdentifier("fct_id4"), sdiValue(sdiValueEntity(radFunctType, 
                                            functEdit.GetId(p_radiossModel))));
                sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(functEdit, sourceProps));
            }
        }
        else if (lsdFPE == 0.0)
        {
            //create a new function Fct_Id4: Abscissa: eps=(-1,0,0.01,0.02 ... 0.99,1) * L0

            double lsdLMAX = 0.0;
            tempVal = sdiValue(lsdLMAX);
            matEntityRead.GetValue(sdiIdentifier("LMAX"), tempVal);
            tempVal.GetValue(lsdLMAX);

            double lsdFMAX = 0.0;
            tempVal = sdiValue(lsdFMAX);
            matEntityRead.GetValue(sdiIdentifier("FMAX"), tempVal);
            tempVal.GetValue(lsdFMAX);

            double lsdKSH = 0.0;
            tempVal = sdiValue(lsdKSH);
            matEntityRead.GetValue(sdiIdentifier("KSH"), tempVal);
            tempVal.GetValue(lsdKSH);

            if(lsdLMAX != 0.0)
            {
                // create new "fct_id4"
                // --------------------
                double ordY,C1,C2;
                int nPoints = 102;
                sdiDoubleList crvPoints;
                crvPoints.reserve(2*nPoints);

                int j=0;
                vector<double> absX = { -1.0 ,0.0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,
                                             0.10,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,
                                             0.20,0.21,0.22,0.23,0.24,0.25,0.26,0.27,0.28,0.29,
                                             0.30,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,
                                             0.40,0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,
                                             0.50,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,
                                             0.60,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,
                                             0.70,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,
                                             0.80,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,
                                             0.90,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,
                                         1.0 };

                for (int i = 0; i < 2*nPoints; i += 2)
                {
                    absX[j] = absX[j]*lsdL0;

                    if(absX[j] <= 0.0)
                    {
                        ordY = 0.0;
                    }
                    else
                    {
                        C1 = 1.0/(exp(lsdKSH)-1.0);
                        C2 = exp((lsdKSH/lsdLMAX)*(absX[j]/lsdL0))-1.0;
                        ordY = lsdFMAX*C1*C2;
                    }

                    crvPoints.push_back(absX[j]);
                    crvPoints.push_back(ordY);
                    j=j+1;
                }

                HandleEdit functEdit;
                p_ConvertUtils.CreateCurve("New_curve_Mat_S15_FPE_" + to_string(dynaMatId), (int)nPoints, crvPoints, functEdit);

                if (functEdit.IsValid())
                {
                    radProp.SetValue(p_radiossModel,sdiIdentifier("fct_id4"), sdiValue(sdiValueEntity(radFunctType, 
                                                functEdit.GetId(p_radiossModel))));
                    sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
                    sdiConvert::Convert::PushToConversionLog(std::make_pair(functEdit, sourceProps));
                }
            }
        }
    }
    else if(FPEoption == 1)
    {
        // FUNCTION --> then Fct_Id4 = abs(FPE)

        tempVal = sdiValue(lcidEntity);
        matEntityRead.GetValue(sdiIdentifier("LSD_FPEID"), tempVal);
        tempVal.GetValue(lcidEntity);
        radProp.SetValue(p_radiossModel, sdiIdentifier("fct_id4"), sdiValue(sdiValueEntity(radFunctType, lcidEntity.GetId())));
    }
}
void ConvertProp::ConvertSecShells16RelatedMatFabric(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp)
{
    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();
    //destCard = "/PROP/TYPE16";

    if (p_radiossModel->IsIdAvailable(destEntityType, dynaPropId))
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);
    else
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));

    //----------------------------
    EntityRead partEntRead(p_lsdynaModel, p_PartBeingConverted);
    EntityId partId = partEntRead.GetId();

    HandleRead radpartHRead;
    EntityType radPartEntityType = p_radiossModel->GetEntityType("/PART");
    p_radiossModel->FindById(radPartEntityType, partId, radpartHRead);

    HandleRead radmatHRead;
    radpartHRead.GetEntityHandle(p_radiossModel, sdiIdentifier("mat_ID"), radmatHRead);
    //----------------------------

    EntityEdit radPropEntity(p_radiossModel, radProp);
    sdiValue tempVal;

    p_ConvertUtils.CopyValue(dynaProp, radPropEntity, "T1", "Thick");

    radProp.SetValue(p_radiossModel, sdiIdentifier("Ishell"), sdiValue(24));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Ismstr"), sdiValue(4));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Ish3n"), sdiValue(2));
    radProp.SetValue(p_radiossModel, sdiIdentifier("N"), sdiValue(1));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Dm"), sdiValue(0.05));

    int radIP = 1;  // "N"
    double lsdthick;
    dynaProp.GetValue(sdiIdentifier("T1"), tempVal);
    tempVal.GetValue(lsdthick);

    int lsdAOPT = 0;
    matEntityRead.GetValue(sdiIdentifier("axisOptFlag"), tempVal);
    tempVal.GetValue(lsdAOPT);

    if (lsdAOPT == 1)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(20));
        for (int i = 0; i < radIP; i=i+1)
        {
            double lsdBeta = 0.0;
            matEntityRead.GetValue(sdiIdentifier("BETA"), tempVal);
            tempVal.GetValue(lsdBeta);
            if(lsdBeta > 0.0)
                radProp.SetValue(p_radiossModel, sdiIdentifier("Phi_i",0,i), sdiValue(lsdBeta));
            else
                radProp.SetValue(p_radiossModel, sdiIdentifier("Phi_i",0,i), sdiValue(0.0));

            radProp.SetValue(p_radiossModel, sdiIdentifier("T_i",0,i), sdiValue(lsdthick));
            radProp.SetValue(p_radiossModel, sdiIdentifier("mat_IDi",0,i), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/MAT"), radmatHRead.GetId(p_radiossModel))));
        }
    }
    else if (lsdAOPT == 2)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(22));

        for (int i = 0; i < radIP; i=i+1)
        {
            radProp.SetValue(p_radiossModel, sdiIdentifier("Phi_i",0,i), sdiValue(0.0));
            radProp.SetValue(p_radiossModel, sdiIdentifier("T_i",0,i), sdiValue(lsdthick));
            radProp.SetValue(p_radiossModel, sdiIdentifier("mat_IDi",0,i), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/MAT"), radmatHRead.GetId(p_radiossModel))));
        }

        double lsdA1, lsdA2, lsdA3;
        matEntityRead.GetValue(sdiIdentifier("LSDYNA_A1"), tempVal);
        tempVal.GetValue(lsdA1);
        matEntityRead.GetValue(sdiIdentifier("LSDYNA_A2"), tempVal);
        tempVal.GetValue(lsdA2);
        matEntityRead.GetValue(sdiIdentifier("LSDYNA_A3"), tempVal);
        tempVal.GetValue(lsdA3);

        //matEntityRead.GetValue(sdiIdentifier("LSDYNA_A1"), tempVal);

        if (lsdA1 != 0.0 || lsdA2 != 0.0 || lsdA3 != 0.0)
        {
            // create a new /SKEW/FIX with OX=OY=OZ=0.0 and x-axis aligned with vector A(A1, A2, A3)
            sdiTriple origin(0.0, 0.0, 0.0);
            origin = origin.Normalize();
            sdiTriple inputDirVect(lsdA1, lsdA2, lsdA3);
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
                radSkewEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(sdiString(dynaPropName)));
                radProp.SetValue(p_radiossModel,sdiIdentifier("SKEW_CSID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SKEW"), radSkewHEdit.GetId(p_radiossModel))));
                sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(radSkewHEdit, sourceProps));
            }
        }
        else // if A(A1, A2, A3) is zero then create the /SKEW/FIX aligned with global axis
        {
            sdiTriple origin(0.0, 0.0, 0.0);
            origin = origin.Normalize();
            sdiTriple inputDirVect(0, 0, 0);
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
                radSkewEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(sdiString(dynaPropName)));
                radProp.SetValue(p_radiossModel,sdiIdentifier("SKEW_CSID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SKEW"), radSkewHEdit.GetId(p_radiossModel))));
                sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(radSkewHEdit, sourceProps));
            }
        }
    }
    else if (lsdAOPT == 3)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(23));

        for (int i = 0; i < radIP; i=i+1)
        {
            double lsdBeta = 0.0;
            matEntityRead.GetValue(sdiIdentifier("BETA"), tempVal);
            tempVal.GetValue(lsdBeta);
            if(lsdBeta > 0.0)
                radProp.SetValue(p_radiossModel, sdiIdentifier("Phi_i",0,i), sdiValue(lsdBeta));
            else
                radProp.SetValue(p_radiossModel, sdiIdentifier("Phi_i",0,i), sdiValue(0.0));

            radProp.SetValue(p_radiossModel, sdiIdentifier("T_i",0,i), sdiValue(lsdthick));
            radProp.SetValue(p_radiossModel, sdiIdentifier("mat_IDi",0,i), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/MAT"), radmatHRead.GetId(p_radiossModel))));
        }
        p_ConvertUtils.CopyValue(matEntityRead, radPropEntity, "LSDYNA_V1", "Vx");
        p_ConvertUtils.CopyValue(matEntityRead, radPropEntity, "LSDYNA_V2", "Vy");
        p_ConvertUtils.CopyValue(matEntityRead, radPropEntity, "LSDYNA_V3", "Vz");
    }
    else if(lsdAOPT == 4)
    // set skew_ID=abs(AOPT) (lsdAOPT < 0)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(0));

        for (int i = 0; i < radIP; i=i+1)
        {
            radProp.SetValue(p_radiossModel, sdiIdentifier("Phi_i",0,i), sdiValue(0.0));
            radProp.SetValue(p_radiossModel, sdiIdentifier("T_i",0,i), sdiValue(lsdthick));
            radProp.SetValue(p_radiossModel, sdiIdentifier("mat_IDi",0,i), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/MAT"), radmatHRead.GetId(p_radiossModel))));
        }

        HandleRead defHandle;
        matEntityRead.GetEntityHandle(sdiIdentifier("LSD_SYSTEM"), defHandle);
        if (defHandle.IsValid())
        {
            EntityRead defEntityRead(p_lsdynaModel, defHandle);
            EntityId skew_Id = defEntityRead.GetId();
            radProp.SetValue(p_radiossModel,sdiIdentifier("SKEW_CSID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SKEW"), skew_Id)));
        }
    }

    //The rest of parameters are set to default 0 values in conversion
    radProp.SetValue(p_radiossModel, sdiIdentifier("P_Thick_Fail"), sdiValue(0.0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Hm"), sdiValue(0.0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Hf"), sdiValue(0.0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Hr"), sdiValue(0.0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Dn"), sdiValue(0.0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("ISTRAIN"), sdiValue(0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Ashear"), sdiValue(0.0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("ITHICK"), sdiValue(0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Ipos"), sdiValue(0));

    sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
    sdiConvert::Convert::PushToConversionLog(std::make_pair(radProp, sourceProps));
}

void ConvertProp::p_ConvertPartComposites(const sdi::EntityRead& partEntRead, sdi::HandleEdit& radProp)
{
    HandleRead matHandle;
    partEntRead.GetEntityHandle(sdiIdentifier("MID"), matHandle);
    EntityRead matEntityRead(p_lsdynaModel, matHandle);
    int lsdAOPT = 0;
    sdiValue tempVal(lsdAOPT);
    matEntityRead.GetValue(sdiIdentifier("axisOptFlag"), tempVal);
    tempVal.GetValue(lsdAOPT);

    double piVal = 3.14159265359;
    EntityType radPropType = p_radiossModel->GetEntityType("/PROP");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    destCard = "/PROP/TYPE51";


    p_radiossModel->CreateEntity(radProp, destCard, partEntRead.GetName(), p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));


    EntityEdit radSh51Edit(p_radiossModel, radProp);

    int lsdElform = 0;
    sdiValue tempValue = sdiValue(lsdElform);
    partEntRead.GetValue(sdiIdentifier("ELFORM"), tempValue);
    tempValue.GetValue(lsdElform);

    int lsdNip = 0;
    tempValue = sdiValue(lsdNip);
    partEntRead.GetValue(sdiIdentifier("Number_of_Plies"), tempValue);
    tempValue.GetValue(lsdNip);

    double lsdNloc = 0.;
    tempValue = sdiValue(lsdNloc);
    partEntRead.GetValue(sdiIdentifier("NLOC"), tempValue);
    tempValue.GetValue(lsdNloc);

    sdiDoubleList lsdThickList;
    tempValue = sdiValue(lsdThickList);
    partEntRead.GetValue(sdiIdentifier("THICK"), tempValue);
    tempValue.GetValue(lsdThickList);

    sdiDoubleList lsdBList;
    tempValue = sdiValue(lsdBList);
    partEntRead.GetValue(sdiIdentifier("B"), tempValue);
    tempValue.GetValue(lsdBList);

    sdiUIntList lsdMatIdList;
    sdiValueEntityList lsdMatList;
    tempValue = sdiValue(lsdMatList);
    partEntRead.GetValue(sdiIdentifier("MID"), tempValue);
    tempValue.GetValue(lsdMatList);
    lsdMatList.GetIdList(lsdMatIdList);

    int lsdNipOk=0;
    double lsdThickMean = 0.;
    for (int i = 0; i < lsdNip; i=i+1)
    {
        if(lsdMatIdList[i] != 0 && lsdThickList[i] != 0.) lsdNipOk = lsdNipOk + 1;
        lsdThickMean = lsdThickMean + lsdThickList[i];
    }
    if (lsdNipOk != 0.) lsdThickMean = lsdThickMean / lsdNipOk;
    lsdNip = lsdNipOk;

    int ishell = 0;
    if (lsdElform == -16 || lsdElform == 9 || lsdElform == -16 )
    {
        ishell = 12;
    }
    else
    {
        ishell = 24;
    }
    radSh51Edit.SetValue(sdiIdentifier("Ishell"), sdiValue(ishell));
    radSh51Edit.SetValue(sdiIdentifier("Ithick"), sdiValue(1));

    if (lsdAOPT == 0)
    {
        radSh51Edit.SetValue( sdiIdentifier("Ip"), sdiValue(20));
    }
    else if (lsdAOPT == 1)
    {
        radSh51Edit.SetValue( sdiIdentifier("Ip"), sdiValue(20));
    }
    else if (lsdAOPT == 2)
    {
        radSh51Edit.SetValue( sdiIdentifier("Ip"), sdiValue(0));
        p_ConvertUtils.CopyValue(matEntityRead, radSh51Edit, "LSDYNA_A1", "Vx");
        p_ConvertUtils.CopyValue(matEntityRead, radSh51Edit, "LSDYNA_A2", "Vy");
        p_ConvertUtils.CopyValue(matEntityRead, radSh51Edit, "LSDYNA_A3", "Vz");
    }
    else if (lsdAOPT == 3)
    {
        radSh51Edit.SetValue( sdiIdentifier("Ip"), sdiValue(23));
        p_ConvertUtils.CopyValue(matEntityRead, radSh51Edit, "LSDYNA_V1", "Vx");
        p_ConvertUtils.CopyValue(matEntityRead, radSh51Edit, "LSDYNA_V2", "Vy");
        p_ConvertUtils.CopyValue(matEntityRead, radSh51Edit, "LSDYNA_V3", "Vz");
    }
    else if (lsdAOPT < 0)
    {
        radSh51Edit.SetValue( sdiIdentifier("Ip"), sdiValue(0));
        radSh51Edit.SetValue(sdiIdentifier("SKEW_CSID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SKEW"), -lsdAOPT)));
    }




    if (lsdNloc == 0.)
    {
        radSh51Edit.SetValue(sdiIdentifier("Ipos"), sdiValue(0));
    }
    /*
    else
    {
        radSh51Edit.SetValue(sdiIdentifier("Ipos"), sdiValue(2));
        radSh51Edit.SetValue(sdiIdentifier("Z0"), sdiValue(-lsdNloc*lsdThickMean/2.));
    }
    */
    else if(lsdNloc == -1.0)
    {
        radSh51Edit.SetValue(sdiIdentifier("Ipos"), sdiValue(4));
        radSh51Edit.SetValue(sdiIdentifier("Z0"), sdiValue(0.0));
    }
    else if(lsdNloc == 1.0)
    {
        radSh51Edit.SetValue(sdiIdentifier("Ipos"), sdiValue(3));
        radSh51Edit.SetValue(sdiIdentifier("Z0"), sdiValue(0.0));
    }

    destCard = "/PROP/TYPE19";
    radSh51Edit.SetValue(sdiIdentifier("plyidlistmax"), sdiValue(lsdNip));
        
    for (int i = 0; i < lsdNip; i=i+1)
    {
        HandleEdit radProperty19Handle;
        p_radiossModel->CreateEntity(radProperty19Handle,destCard,partEntRead.GetName() + " - ply " + to_string(i) );

        if(radProperty19Handle.IsValid())
        {
            EntityEdit radProperty19Edit(p_radiossModel, radProperty19Handle);
            radSh51Edit.SetValue(sdiIdentifier("plyidlist",0,i), sdiValue(sdiValueEntity(radPropType,radProperty19Edit.GetId())));
            radProperty19Edit.SetValue(sdiIdentifier("thickness1"), sdiValue(lsdThickList[i]));
            radProperty19Edit.SetValue(sdiIdentifier("delta_phi"), sdiValue(lsdBList[i]));
            radProperty19Edit.SetValue(sdiIdentifier("material"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/MAT"), lsdMatIdList[i])));

            SDIHandlReadList sourceParts = { {partEntRead.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radProperty19Handle, sourceParts));
        }
    }

    destCard = "/ADMAS";
        
    double lsdMArea = 0.;
    tempValue = sdiValue(lsdMArea);
    partEntRead.GetValue(sdiIdentifier("MAREA"), tempValue);
    tempValue.GetValue(lsdMArea);

    if(lsdMArea != 0.)
    {
        HandleEdit radAdmasHandle;
        p_radiossModel->CreateEntity(radAdmasHandle,destCard,partEntRead.GetName());

        if(radAdmasHandle.IsValid())
        {
            EntityEdit radAdmasEdit(p_radiossModel, radAdmasHandle);
            radAdmasEdit.SetValue(sdiIdentifier("type"), sdiValue(2));
            p_ConvertUtils.CopyValue(partEntRead, radAdmasEdit, "MAREA", "MASS");
   
            HandleEdit setHEdit;
            p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", partEntRead.GetName());
            EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

            SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
            SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
            SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/PART"), partEntRead.GetId())));

            radAdmasEdit.SetValue(sdiIdentifier("setid_type"), sdiValue(sdiString("/SETS/SURF")));
            radAdmasEdit.SetEntityHandle(sdiIdentifier("surf_ID"), setHEdit);

            SDIHandlReadList sourceParts = { {partEntRead.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radAdmasHandle, sourceParts));
        }
    }
}
void ConvertProp::ConvertSolidOrthType6(const EntityRead& matEntityRead, const EntityRead& dynaProp, sdiString& destCard, HandleEdit& radProp)
{
    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();
    //destCard = "/PROP/TYPE6";
    sdiValue tempVal;

    int lsdAOPT = 0;
    tempVal = sdiValue(lsdAOPT);
    matEntityRead.GetValue(sdiIdentifier("axisOptFlag"), tempVal);
    tempVal.GetValue(lsdAOPT);

    EntityEdit radPropEntity(p_radiossModel, radProp);
    if (lsdAOPT == 1)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(20));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Phi"), sdiValue(0.0));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Vx"), sdiValue(0.0));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Vy"), sdiValue(0.0));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Vz"), sdiValue(0.0));
        radProp.SetValue(p_radiossModel, sdiIdentifier("skew_ID"), sdiValue(0));
    }
    else if (lsdAOPT == 2)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(21));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Phi"), sdiValue(0.0));

        p_ConvertUtils.CopyValue(matEntityRead, radPropEntity, "XP", "Vx");
        p_ConvertUtils.CopyValue(matEntityRead, radPropEntity, "YP", "Vy");
        p_ConvertUtils.CopyValue(matEntityRead, radPropEntity, "ZP", "Vz");
    }
    else if (lsdAOPT == 3)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(0));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Phi"), sdiValue(0.0));

        double lsdA1;
        double lsdA2;
        double lsdA3;
        double lsdD1;
        double lsdD2;
        double lsdD3;

        vector<reference_wrapper<double>> attribVals({ lsdA1, lsdA2, lsdA3, lsdD1, lsdD2, lsdD3 });
        vector<sdiString> attribNames({ "A1", "A2", "A3", "D1", "D2", "D3"});
        p_ConvertUtils.GetAttribValues(matEntityRead, attribNames, attribVals);

        sdiTriple locXVect(lsdA1, lsdA2, lsdA3);
        sdiTriple locZVect(locXVect * sdiTriple(lsdD1, lsdD2, lsdD3));
        sdiTriple locYVect(locZVect * locXVect);
        //sdiTriple locZVect = sdiTriple(lsdA1, lsdA2, lsdA3) * sdiTriple(lsdD1, lsdD2, lsdD3);
        //sdiTriple locYVect = locZVect * sdiTriple(lsdA1, lsdA2, lsdA3);
        sdiTriple origin(0.0, 0.0, 0.0);

        HandleEdit radSkewHEdit;
        p_radiossModel->CreateEntity(radSkewHEdit, "/SKEW/FIX", "ORTH_" + radPropEntity.GetName());
        if (radSkewHEdit.IsValid())
        {
            EntityEdit radSkewEdit(p_radiossModel, radSkewHEdit);
            vector<sdiString> originAttr({ "Ox", "Oy", "Oz" });
            vector<sdiString> attribVect1({ "X1", "Y1", "Z1" });
            vector<sdiString> attribVect2({ "X2", "Y2", "Z2" });
            for (size_t i = 0; i < 3; ++i)
            {
                radSkewEdit.SetValue(sdiIdentifier(originAttr[i]), sdiValue(origin[i]));
                radSkewEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(locYVect[i]));
                radSkewEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(locZVect[i]));
            }
            radSkewEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(sdiString(dynaPropName)));
            radProp.SetValue(p_radiossModel,sdiIdentifier("SKEW_CSID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SKEW"), radSkewHEdit.GetId(p_radiossModel))));
            sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radSkewHEdit, sourceProps));
        }
/*
        double lsdA1;
        double lsdA2;
        double lsdA3;
        double lsdD1;
        double lsdD2;
        double lsdD3;

        vector<reference_wrapper<double>> attribVals({ lsdA1, lsdA2, lsdA3, lsdD1, lsdD2, lsdD3});
        vector <sdiString> attNames({ "LSDYNA_A1", "LSDYNA_A2", "LSDYNA_A3", "LSDYNA_D1", "LSDYNA_D2", "LSDYNA_D3"});
        p_ConvertUtils.GetAttribValues(matEntityRead, attNames, attribVals);

        sdiTriple planeCoords = sdiTriple(lsdA1, lsdA2, lsdA3) * sdiTriple(lsdD1, lsdD2, lsdD3);
        sdiTriple axisCoords = planeCoords * sdiTriple(lsdA1, lsdA2, lsdA3);

        //sdiTriple locXVect(lsdA1, lsdA2, lsdA3);
        //sdiTriple planeCoords(locXVect * sdiTriple(lsdD1, lsdD2, lsdD3));
        //sdiTriple axisCoords(planeCoords * locXVect);

        HandleEdit radSkewHEdit;
        p_radiossModel->CreateEntity(radSkewHEdit, "/SKEW/FIX", "ORTH_" + radPropEntity.GetName());
        if(radSkewHEdit.IsValid())
        {
            EntityEdit radSkewEdit(p_radiossModel, radSkewHEdit);
            radSkewEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(sdiString(dynaPropName)));
            vector<sdiString> attribNames({ "Ox","Oy","Oz","X2", "Y2","Z2","X1","Y1","Z1" });
            vector<double> attribVals({ 0.0, 0.0, 0.0, planeCoords[0], planeCoords[1], planeCoords[2], axisCoords[0], axisCoords[1], axisCoords[2] });
            for (int i = 0; i < attribNames.size(); ++i)
            {
                radSkewEdit.SetValue(sdiIdentifier(attribNames[i]), sdiValue(attribVals[i]));
            }
            radProp.SetValue(p_radiossModel,sdiIdentifier("SKEW_CSID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SKEW"), radSkewHEdit.GetId(p_radiossModel))));
            sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radSkewHEdit, sourceProps));
        }
*/
    }
    else if (lsdAOPT == 4)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(23));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Phi"), sdiValue(0.0));

        p_ConvertUtils.CopyValue(matEntityRead, radPropEntity, "V1", "Vx");
        p_ConvertUtils.CopyValue(matEntityRead, radPropEntity, "V2", "Vy");
        p_ConvertUtils.CopyValue(matEntityRead, radPropEntity, "V3", "Vz");
    }
    else if (lsdAOPT == 5)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(24));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Phi"), sdiValue(0.0));

        p_ConvertUtils.CopyValue(matEntityRead, radPropEntity, "XP", "Px");
        p_ConvertUtils.CopyValue(matEntityRead, radPropEntity, "YP", "Py");
        p_ConvertUtils.CopyValue(matEntityRead, radPropEntity, "ZP", "Pz");

        p_ConvertUtils.CopyValue(matEntityRead, radPropEntity, "V1", "Vx");
        p_ConvertUtils.CopyValue(matEntityRead, radPropEntity, "V2", "Vy");
        p_ConvertUtils.CopyValue(matEntityRead, radPropEntity, "V3", "Vz");
    }
    else if (lsdAOPT == 6)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(0));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Phi"), sdiValue(0.0));

        HandleRead defHandle;
        matEntityRead.GetEntityHandle(sdiIdentifier("LSD_SYSTEM"), defHandle);
        if (defHandle.IsValid())
        {
            EntityRead defEntityRead(p_lsdynaModel, defHandle);
            EntityId skew_Id = defEntityRead.GetId();
            radProp.SetValue(p_radiossModel,sdiIdentifier("skew_ID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SKEW"), skew_Id)));
        }
    }
}

void ConvertProp::ConvertSecShellsRelatedMatOrthotropic(const EntityRead& matEntityRead, const EntityRead& dynaProp, sdiString& destCard, HandleEdit& radProp)
{
    double thick = 0.0;
    sdiUIntList matIdList;
    int nip = 0;
    int lsdQRIRIDOpt = 0;
    int iPosValue = 0;
    HandleRead IridHandle;

    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();

    //destCard = "/PROP/SH_SANDW";
    if (matReferenceCount > 1)
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);

    EntityEdit radShSandwEdit(p_radiossModel, radProp);

    sdiValue tempValue(thick);
    dynaProp.GetValue(sdiIdentifier("T1"), tempValue);
    tempValue.GetValue(thick);

    p_ConvertUtils.CopyValue(dynaProp, radShSandwEdit, "T1", "Thick");

    tempValue = sdiValue(nip);
    dynaProp.GetValue(sdiIdentifier("NIP"), tempValue);
    tempValue.GetValue(nip);
    if(nip == 0) nip = 2;

    tempValue = sdiValue(lsdQRIRIDOpt);
    dynaProp.GetValue(sdiIdentifier("Sect_Option"), tempValue);
    tempValue.GetValue(lsdQRIRIDOpt);

    sdiDoubleList thicknessList;
    sdiDoubleList layerPosList;

    //radShSandwEdit.SetValue(sdiIdentifier("N"), sdiValue(nip));
    p_ConvertUtils.CopyValue(dynaProp, radShSandwEdit, "NIP", "N");

    if (lsdQRIRIDOpt == 0)
    {
        iPosValue = 0;

        if (nip > 0)
        {
            thicknessList = sdiDoubleList(nip, thick / nip);
            layerPosList = sdiDoubleList(nip, 0);
        }
    }
    else if (lsdQRIRIDOpt != 0)
    {
        int esop = 0;
        nip = 0;
        sdiDoubleList intS;
        sdiDoubleList intWF;

        iPosValue = 1;

        dynaProp.GetEntityHandle(sdiIdentifier("LSD_IRID"), IridHandle);

        if (IridHandle.IsValid())
        {
            // to be cmpleted when working to convert to prop 51 RD-11305
        }
        else
        {
            nip = 1;
            thicknessList.push_back(thick);
            layerPosList.push_back(0);
        }
    }
     radProp.SetValue(p_radiossModel, sdiIdentifier("Ipos"), sdiValue(iPosValue));
    if (!thicknessList.empty())
    {
        radShSandwEdit.SetValue(sdiIdentifier("Z"), sdiValue(layerPosList));
        radShSandwEdit.SetValue(sdiIdentifier("Prop_Thick"), sdiValue(thicknessList));
    }

    unsigned int matId = matEntityRead.GetId();

    matIdList.clear();
    for (int i = 0; i < nip; ++i)
    {
        matIdList.push_back(matId);
    }

    if (!matIdList.empty())
    {
        radShSandwEdit.SetValue(sdiIdentifier("m"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/MAT"), matIdList)));
    }

    //---
    // AOPT
    //---

    sdiValue tempVal;

    p_ConvertUtils.CopyValue(dynaProp, radShSandwEdit, "T1", "Thick");

    double lsdthick;
    dynaProp.GetValue(sdiIdentifier("T1"), tempVal);
    tempVal.GetValue(lsdthick);

    int lsdAOPT = 0;
    matEntityRead.GetValue(sdiIdentifier("axisOptFlag"), tempVal);
    tempVal.GetValue(lsdAOPT);

    if (lsdAOPT == 1 || lsdAOPT == 2)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(20));
        for (int i = 0; i < nip; i=i+1)
        {
            double lsdBeta = 0.0;
            matEntityRead.GetValue(sdiIdentifier("BETA"), tempVal);
            tempVal.GetValue(lsdBeta);
            if(lsdBeta > 0.0 && lsdAOPT == 1)
                radProp.SetValue(p_radiossModel, sdiIdentifier("Phi",0,i), sdiValue(lsdBeta));
            else
                radProp.SetValue(p_radiossModel, sdiIdentifier("Phi",0,i), sdiValue(0.0));
         }
    }
    else if (lsdAOPT == 3)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(22));

        for (int i = 0; i < nip; i=i+1)
        {
            double lsdBeta = 0.0;
            matEntityRead.GetValue(sdiIdentifier("BETA"), tempVal);
            tempVal.GetValue(lsdBeta);
            if(lsdBeta > 0.0)
                radProp.SetValue(p_radiossModel, sdiIdentifier("Phi",0,i), sdiValue(lsdBeta));
            else
                radProp.SetValue(p_radiossModel, sdiIdentifier("Phi",0,i), sdiValue(0.0));
        }
/*
        p_ConvertUtils.CopyValue(matEntityRead, radShSandwEdit, "LSDYNA_A1", "Vx");
        p_ConvertUtils.CopyValue(matEntityRead, radShSandwEdit, "LSDYNA_A2", "Vy");
        p_ConvertUtils.CopyValue(matEntityRead, radShSandwEdit, "LSDYNA_A3", "Vz");
*/

        double lsdA1, lsdA2, lsdA3;
        matEntityRead.GetValue(sdiIdentifier("LSDYNA_A1"), tempVal);
        tempVal.GetValue(lsdA1);
        matEntityRead.GetValue(sdiIdentifier("LSDYNA_A2"), tempVal);
        tempVal.GetValue(lsdA2);
        matEntityRead.GetValue(sdiIdentifier("LSDYNA_A3"), tempVal);
        tempVal.GetValue(lsdA3);

        //matEntityRead.GetValue(sdiIdentifier("LSDYNA_A1"), tempVal);

        if (lsdA1 != 0.0 || lsdA2 != 0.0 || lsdA3 != 0.0)
        {
            // create a new /SKEW/FIX with OX=OY=OZ=0.0 and x-axis aligned with vector A(A1, A2, A3)
            sdiTriple origin(0.0, 0.0, 0.0);
            origin = origin.Normalize();
            sdiTriple inputDirVect(lsdA1, lsdA2, lsdA3);
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
                radSkewEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(sdiString(dynaPropName)));
                radProp.SetValue(p_radiossModel,sdiIdentifier("SKEW_CSID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SKEW"), radSkewHEdit.GetId(p_radiossModel))));
                sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(radSkewHEdit, sourceProps));
            }
        }
        else // if A(A1, A2, A3) is zero then create the /SKEW/FIX aligned with global axis
        {
            sdiTriple origin(0.0, 0.0, 0.0);
            origin = origin.Normalize();
            sdiTriple inputDirVect(0, 0, 0);
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
                radSkewEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(sdiString(dynaPropName)));
                radProp.SetValue(p_radiossModel,sdiIdentifier("SKEW_CSID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SKEW"), radSkewHEdit.GetId(p_radiossModel))));
                sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(radSkewHEdit, sourceProps));
            }
        }
    }
    else if (lsdAOPT == 4)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(23));

        for (int i = 0; i < nip; i=i+1)
        {
            double lsdBeta = 0.0;
            matEntityRead.GetValue(sdiIdentifier("BETA"), tempVal);
            tempVal.GetValue(lsdBeta);
            if(lsdBeta > 0.0)
                radProp.SetValue(p_radiossModel, sdiIdentifier("Phi",0,i), sdiValue(lsdBeta));
            else
                radProp.SetValue(p_radiossModel, sdiIdentifier("Phi",0,i), sdiValue(0.0));
        }
        p_ConvertUtils.CopyValue(matEntityRead, radShSandwEdit, "LSDYNA_V1", "Vx");
        p_ConvertUtils.CopyValue(matEntityRead, radShSandwEdit, "LSDYNA_V2", "Vy");
        p_ConvertUtils.CopyValue(matEntityRead, radShSandwEdit, "LSDYNA_V3", "Vz");
    }
    else if(lsdAOPT == 6)
    // set skew_ID=abs(AOPT) (lsdAOPT < 0)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(0));

        for (int i = 0; i < nip; i=i+1)
        {
            radProp.SetValue(p_radiossModel, sdiIdentifier("Phi",0,i), sdiValue(0.0));
        }

        HandleRead defHandle;
        matEntityRead.GetEntityHandle(sdiIdentifier("LSD_SYSTEM"), defHandle);
        if (defHandle.IsValid())
        {
            EntityRead defEntityRead(p_lsdynaModel, defHandle);
            EntityId skew_Id = defEntityRead.GetId();
            radProp.SetValue(p_radiossModel,sdiIdentifier("SKEW_CSID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SKEW"), skew_Id)));
        }
    }
}
void ConvertProp::ConvertSecShellsRelatedMatSeatbelt2D(const EntityRead& matEntityRead, const EntityRead& dynaProp, sdiString& destCard, HandleEdit& radProp)
{
    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();
    //destCard = "/PROP/TYPE9";

    if (p_radiossModel->IsIdAvailable(destEntityType, dynaPropId))
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);
    else
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));

    EntityEdit radPropEdit(p_radiossModel, radProp);

    sdiValue tempVal;

    double thick{ 0.0 };
    tempVal = sdiValue(thick);
    dynaProp.GetValue(sdiIdentifier("T1"), tempVal);
    tempVal.GetValue(thick);
    //---
    double lsdECOAT = 0, lsdTCOAT = 0;
    tempVal = sdiValue(lsdECOAT);
    matEntityRead.GetValue(sdiIdentifier("ECOAT"), tempVal);
    tempVal.GetValue(lsdECOAT);

    tempVal = sdiValue(lsdTCOAT);
    matEntityRead.GetValue(sdiIdentifier("TCOAT"), tempVal);
    tempVal.GetValue(lsdTCOAT);

    int NIP = 1;
    if (lsdECOAT > 0.0 && lsdTCOAT > 0.0) NIP = 3;

    //---
    radProp.SetValue(p_radiossModel, sdiIdentifier("Ip"), sdiValue(24));
    p_ConvertUtils.CopyValue(dynaProp, radPropEdit, "T1", "Thick");
    radProp.SetValue(p_radiossModel, sdiIdentifier("Ishell"), sdiValue(12));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Ismstr"), sdiValue(11));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Ish3n"), sdiValue(3));
    radProp.SetValue(p_radiossModel, sdiIdentifier("N"), sdiValue(NIP));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Dm"), sdiValue(0.25));

    //The rest of parameters are set to default 0 values in converson
    radProp.SetValue(p_radiossModel, sdiIdentifier("P_Thick_Fail"), sdiValue(0.0));

    radProp.SetValue(p_radiossModel, sdiIdentifier("Hm"), sdiValue(0.0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Hf"), sdiValue(0.0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Hr"), sdiValue(0.0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Dn"), sdiValue(0.0));

    radProp.SetValue(p_radiossModel, sdiIdentifier("ISTRAIN"), sdiValue(0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("Ashear"), sdiValue(0.0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("ITHICK"), sdiValue(0));
    radProp.SetValue(p_radiossModel, sdiIdentifier("IPLAS"), sdiValue(0));
//
    HandleRead handleNodeSet;
    dynaProp.GetEntityHandle(sdiIdentifier("EDGSET"), handleNodeSet);
    if (handleNodeSet.IsValid())
    {
        sdiUIntList NodeSetList;
        p_ConvertUtils.GetNodeIdsFromNodeSet(handleNodeSet.GetId(p_lsdynaModel), NodeSetList);

        int N1,N2;
        N1 = NodeSetList[0];
        N2 = NodeSetList[1];

        HandleRead hReadNode1;
        HandleRead hReadNode2;
        EntityType radNodeEntityType = p_radiossModel->GetEntityType("/NODE");
        EntityType dynaNodeEntityType = p_lsdynaModel->GetEntityType("*NODE");
        p_lsdynaModel->FindById(dynaNodeEntityType, N1, hReadNode1);
        p_lsdynaModel->FindById(dynaNodeEntityType, N2, hReadNode2);

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
            HandleEdit radSkewHEdit;
            p_radiossModel->CreateEntity(radSkewHEdit, "/SKEW/MOV", radPropEdit.GetName());

            HandleNodeEdit node3HandleEdit;
            p_radiossModel->CreateNode(node3HandleEdit, "/NODE", node3pos);


            EntityEdit skewEntityEdit(p_radiossModel, radSkewHEdit);
            skewEntityEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(radPropEdit.GetName()));
            skewEntityEdit.SetValue(sdiIdentifier("N1"), sdiValue(sdiValueEntity(sdiValueEntityType(radNodeEntityType), node1Read.GetId())));
            skewEntityEdit.SetValue(sdiIdentifier("N2"), sdiValue(sdiValueEntity(sdiValueEntityType(radNodeEntityType), node2Read.GetId())));
            skewEntityEdit.SetValue(sdiIdentifier("N3"), sdiValue(sdiValueEntity(sdiValueEntityType(radNodeEntityType), node3HandleEdit.GetId(p_radiossModel))));

            radProp.SetValue(p_radiossModel,sdiIdentifier("SKEW_CSID"), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SKEW"), radSkewHEdit.GetId(p_radiossModel))));
            //radProp.SetEntityHandle(p_radiossModel,sdiIdentifier("SKEW_CSID"), radSkewHEdit);
            sdiConvert::SDIHandlReadList sourceProps = { {dynaProp.GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radSkewHEdit, sourceProps));
        }
    }
}

void ConvertProp::p_ConvertSectionSph(const EntityRead& matEntityRead, const EntityRead& dynaProp, sdiString& destCard, HandleEdit& radProp)
{
    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();
    destCard = "/PROP/SPH";

    if (p_radiossModel->IsIdAvailable(destEntityType, dynaPropId))
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);
    else
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));

    EntityEdit radPropEdit(p_radiossModel, radProp);
    sdiValue tempVal;

    double lsdCSLH;
    tempVal = sdiValue(lsdCSLH);
    dynaProp.GetValue(sdiIdentifier("CSLH"), tempVal);
    tempVal.GetValue(lsdCSLH);

    if(lsdCSLH > 0)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("h_1D"), sdiValue(0));
        p_ConvertUtils.CopyValue(dynaProp, radPropEdit, "SPHINI", "h");
    }
    else
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("h_1D"), sdiValue(2));
        p_ConvertUtils.CopyValue(dynaProp, radPropEdit, "HMIN", "hmin");
        p_ConvertUtils.CopyValue(dynaProp, radPropEdit, "HMAX", "hmax");
        p_ConvertUtils.CopyValue(dynaProp, radPropEdit, "CSLH", "hcst");
    }

    int lsdSPHKERN;
    tempVal = sdiValue(lsdSPHKERN);
    dynaProp.GetValue(sdiIdentifier("SPHKERN"), tempVal);
    tempVal.GetValue(lsdSPHKERN);

    if(lsdSPHKERN == 2)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("ORDER"), sdiValue(2));
    }
    else
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("ORDER"), sdiValue(0));
    }

}
void ConvertProp::p_ConvertSectionTShell(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, const sdiString& sourceCard, sdiString& destCard, sdi::HandleEdit& radProp)
{
    sdiString matCard = matEntityRead.GetKeyword();
    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();
    
    //  ICOMP=0, /PROP/TSHELL (TYPE20) with isotropic material
    //  ICOMP=0, /PROP/TSHELL (TYPE21) with orthotropic material
    //  ICOMP=1, /PROP/TSH_COMP (TYPE22)

    int ICOMP = GetValue<int>(dynaProp, "LSD_ICOMP");

    int lsdAOPT = GetValue<int>(matEntityRead, "axisOptFlag");

    //if (ICOMP == 0 & (matCard.find("MAT_HONEYCOMB") != string::npos || 
    //                  matCard.find("MAT_026") != string::npos ||
    //                  matCard.find("MAT_MODIFIED_HONEYCOMB") != string::npos || 
    //                  matCard.find("MAT_126") != string::npos))
    if (ICOMP == 0 && lsdAOPT > 0)   // materials orthoropic
    {
        destCard = "/PROP/TYPE21";
        ConvertSecTShellsRelatedMatOrthotropic(matEntityRead, dynaProp, destCard, radProp);
    }
    else if (ICOMP == 0)  // materials isotropic
    {
        destCard = "/PROP/TYPE20";
        ConvertSecTShellsRelatedMatIsotropic(matEntityRead, dynaProp, destCard, radProp);
    }
    else if (ICOMP == 1) //   // materials composite
    {
        destCard = "/PROP/TYPE22";
        ConvertSecTShellsRelatedMatComposite(matEntityRead, dynaProp, destCard, radProp);
    }
}
void ConvertProp::ConvertSecTShellsRelatedMatIsotropic(const EntityRead& matEntityRead, const EntityRead& dynaProp, sdiString& destCard, HandleEdit& radProp)
{
    //destCard = "/PROP/TYPE20";
    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();

    if (matReferenceCount > 1)
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);

    int elform = GetValue<int>(dynaProp, "LSD_ELFORM");
    
    int ICOMP = GetValue<int>(dynaProp, "LSD_ICOMP");

    int isolid = 0;
    if(elform == 1) isolid = 15;
    else isolid = 14;
    
    radProp.SetValue(p_radiossModel, sdiIdentifier("ISOLID"), sdiValue(isolid));

    int NIP = GetValue<int>(dynaProp, "LSD_NIP");

    int Inpts_R=0,Inpts_S=0,Inpts_T=0;

    if (isolid == 14)
    {
        Inpts_R = 2;
        Inpts_S = min(NIP,9);
        Inpts_S = max(1,Inpts_S);
        Inpts_T = 2;

        NIP = Inpts_R*100 + Inpts_S*10 + Inpts_T;
        
        radProp.SetValue(p_radiossModel, sdiIdentifier("Inpts_R"), sdiValue(Inpts_R));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Inpts_S"), sdiValue(Inpts_S));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Inpts_T"), sdiValue(Inpts_T));
        
        radProp.SetValue(p_radiossModel, sdiIdentifier("NBP"), sdiValue(NIP));
    }
    else if(isolid == 15)
    {
        Inpts_S = NIP;
        radProp.SetValue(p_radiossModel, sdiIdentifier("NBP"), sdiValue(Inpts_S));
    }
}
void ConvertProp::ConvertSecTShellsRelatedMatOrthotropic(const EntityRead& matEntityRead, const EntityRead& dynaProp, sdiString& destCard, HandleEdit& radProp)
{
    //destCard = "/PROP/TYPE21";
    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();

    if (matReferenceCount > 1)
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);

    int elform = GetValue<int>(dynaProp, "LSD_ELFORM");

    int ICOMP = GetValue<int>(dynaProp, "LSD_ICOMP");
    
    int isolid = 0;
    if(elform == 1) isolid = 15;
    else isolid = 14;
    
    radProp.SetValue(p_radiossModel, sdiIdentifier("ISOLID"), sdiValue(isolid));

    int NIP = GetValue<int>(dynaProp, "LSD_NIP");

    int Inpts_R=0,Inpts_S=0,Inpts_T=0;

    if (isolid == 14)
    {
        Inpts_R = 2;
        Inpts_S = min(NIP,9);
        Inpts_S = max(1,Inpts_S);
        Inpts_T = 2;

        NIP = Inpts_R*100 + Inpts_S*10 + Inpts_T;
        
        radProp.SetValue(p_radiossModel, sdiIdentifier("Inpts_R"), sdiValue(Inpts_R));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Inpts_S"), sdiValue(Inpts_S));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Inpts_T"), sdiValue(Inpts_T));
        
        radProp.SetValue(p_radiossModel, sdiIdentifier("NBP"), sdiValue(NIP));
    }
    else if(isolid == 15)
    {
        Inpts_S = NIP;
        radProp.SetValue(p_radiossModel, sdiIdentifier("NBP"), sdiValue(Inpts_S));
    }

    EntityEdit radTSHELLOrthEdit(p_radiossModel, radProp);

    //---
    // AOPT
    //---

    int lsdAOPT = GetValue<int>(matEntityRead, "axisOptFlag");

    if (lsdAOPT == 3)
    {
        p_ConvertUtils.CopyValue(matEntityRead, radTSHELLOrthEdit, "LSDYNA_A1", "Vx");
        p_ConvertUtils.CopyValue(matEntityRead, radTSHELLOrthEdit, "LSDYNA_A2", "Vy");
        p_ConvertUtils.CopyValue(matEntityRead, radTSHELLOrthEdit, "LSDYNA_A3", "Vz");
    }
    else if(lsdAOPT == 4)
    {
        p_ConvertUtils.CopyValue(matEntityRead, radTSHELLOrthEdit, "LSDYNA_V1", "Vx");
        p_ConvertUtils.CopyValue(matEntityRead, radTSHELLOrthEdit, "LSDYNA_V2", "Vy");
        p_ConvertUtils.CopyValue(matEntityRead, radTSHELLOrthEdit, "LSDYNA_V3", "Vz");
    }
}
void ConvertProp::ConvertSecTShellsRelatedMatComposite(const EntityRead& matEntityRead, const EntityRead& dynaProp, sdiString& destCard, HandleEdit& radProp)
{
    //destCard = "/PROP/TYPE22";
    sdiString dynaPropName = dynaProp.GetName();
    EntityId dynaPropId = dynaProp.GetId();

    if (matReferenceCount > 1)
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, p_ConvertUtils.GetDynaMaxEntityID(srcEntityType));
    else
        p_radiossModel->CreateEntity(radProp, destCard, dynaPropName, dynaPropId);

    EntityEdit radShSandwEdit(p_radiossModel, radProp);

    int elform = GetValue<int>(dynaProp, "LSD_ELFORM");
    
    int ICOMP = GetValue<int>(dynaProp, "LSD_ICOMP");

    int isolid = 0;
    if(elform == 1) isolid = 15;
    else isolid = 14;
    
    radProp.SetValue(p_radiossModel, sdiIdentifier("ISOLID"), sdiValue(isolid));

    int NIP = GetValue<int>(dynaProp, "LSD_NIP");
    int inpts_NIP = NIP;

    int Inpts_R=0,Inpts_S=0,Inpts_T=0;

    if (isolid == 14 && NIP <= 9)
    {
        Inpts_R = 2;
        Inpts_S = min(NIP,9);
        Inpts_S = max(1,Inpts_S);
        Inpts_T = 2;

        NIP = Inpts_R*100 + Inpts_S*10 + Inpts_T;

        radProp.SetValue(p_radiossModel, sdiIdentifier("Inpts_R"), sdiValue(Inpts_R));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Inpts_S"), sdiValue(Inpts_S));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Inpts_T"), sdiValue(Inpts_T));
        
        radProp.SetValue(p_radiossModel, sdiIdentifier("NBP"), sdiValue(NIP));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Iint"), sdiValue(0));
    }
    else if (isolid == 14 && NIP > 9)
    {
        radProp.SetValue(p_radiossModel, sdiIdentifier("Iint"), sdiValue(NIP));

        Inpts_R = 2;
        Inpts_S = 0;
        Inpts_T = 2;

        NIP = Inpts_R*100 + Inpts_S*10 + Inpts_T;
        
        radProp.SetValue(p_radiossModel, sdiIdentifier("Inpts_R"), sdiValue(Inpts_R));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Inpts_S"), sdiValue(Inpts_S));
        radProp.SetValue(p_radiossModel, sdiIdentifier("Inpts_T"), sdiValue(Inpts_T));
        
        radProp.SetValue(p_radiossModel, sdiIdentifier("NBP"), sdiValue(NIP));
    }
    else if(isolid == 15)
    {
        Inpts_S = NIP;
        radProp.SetValue(p_radiossModel, sdiIdentifier("NBP"), sdiValue(Inpts_S));
    }

    EntityEdit radTSHELLCompositeEdit(p_radiossModel, radProp);

    //---
    // AOPT
    //---

    int lsdAOPT = GetValue<int>(matEntityRead, "axisOptFlag");

    if (lsdAOPT == 3)
    {
        p_ConvertUtils.CopyValue(matEntityRead, radTSHELLCompositeEdit, "LSDYNA_A1", "Vx");
        p_ConvertUtils.CopyValue(matEntityRead, radTSHELLCompositeEdit, "LSDYNA_A2", "Vy");
        p_ConvertUtils.CopyValue(matEntityRead, radTSHELLCompositeEdit, "LSDYNA_A3", "Vz");
    }
    else if(lsdAOPT == 4)
    {
        p_ConvertUtils.CopyValue(matEntityRead, radTSHELLCompositeEdit, "LSDYNA_V1", "Vx");
        p_ConvertUtils.CopyValue(matEntityRead, radTSHELLCompositeEdit, "LSDYNA_V2", "Vy");
        p_ConvertUtils.CopyValue(matEntityRead, radTSHELLCompositeEdit, "LSDYNA_V3", "Vz");
    }

    unsigned int matId = matEntityRead.GetId();
    sdiUIntList matIdList;
    matIdList.clear();
    for (int i = 0; i < inpts_NIP; ++i)
    {
        matIdList.push_back(matId);
    }

    if (!matIdList.empty())
    {
        radTSHELLCompositeEdit.SetValue(sdiIdentifier("mat_IDi"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/MAT"), matIdList)));
    }


    sdiDoubleList PHIList;
    sdiValue tempValue;
    for (int i = 0; i < inpts_NIP; ++i)
    {
        double phi_i;
        sdiValue tempValue(phi_i);
        dynaProp.GetValue(sdiIdentifier("LSD_B",0,i), tempValue);
        tempValue.GetValue(phi_i);
        PHIList.push_back(phi_i);
    }

    if (!PHIList.empty())
    {
        radTSHELLCompositeEdit.SetValue(sdiIdentifier("Phi"), sdiValue(PHIList));
    }

    sdiDoubleList ThicknessRatioList;

    for (int i = 0; i < inpts_NIP; ++i)
    {
        ThicknessRatioList.push_back(1.0/inpts_NIP);
    }

    if (!ThicknessRatioList.empty())
    {
        radTSHELLCompositeEdit.SetValue(sdiIdentifier("ti/t"), sdiValue(ThicknessRatioList));
    }
}