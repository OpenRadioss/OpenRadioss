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

#include <dyna2rad/convertincludes.h>
#include <dyna2rad/convertdefinetransform.h>
#include <dyna2rad/dyna2rad.h>

using namespace std;
using namespace sdi;
using namespace sdiConvert;

void sdiD2R::ConvertInclude::ConvertIncludes()
{
    ConvertEntities();
}

void sdiD2R::ConvertInclude::ConvertEntities()
{
    EntityType radTransformType = p_radiossModel->GetEntityType("/TRANSFORM");

    map<sdiString, double> mapUnits;
    mapUnits["m"] = 1.;
    mapUnits["mm"] = 0.001;
    mapUnits["cm"] = 0.01;
    mapUnits["inch"] = 0.0254;
    mapUnits["foot"] = 0.3048;

    mapUnits["s"] = 1;
    mapUnits["sec"] = 1;
    mapUnits["ms"] = 0.001;
    mapUnits["micro_s"] = 0.000001;

    mapUnits["kg"] = 1.;
    mapUnits["g"] = 0.001;
    mapUnits["mg"] = 0.000001;
    mapUnits["mtrc_ton"] = 1000.;
    mapUnits["lb"] = 0.45359237;
    mapUnits["slug"] = 14.5939029372;
    mapUnits["slinch"] = 175.126835244;

    sdiString len  = "m";
    sdiString time = "sec";
    sdiString mass = "kg";

    bool foundInMainFile = false;
    SelectionRead selCtrlUnits(p_lsdynaModel, "*CONTROL_UNITS");
    while (selCtrlUnits.Next() && foundInMainFile == false)
    {
        HandleRead include = selCtrlUnits->GetInclude();
        vector<reference_wrapper<sdiString>> attrValList = { len, time, mass };
        vector<sdiString> attrNameList = { "LENGTH","TIME", "MASS" };
        p_ConvertUtils.GetAttribValues(*selCtrlUnits, attrNameList, attrValList);

        if(!include.IsValid()) 
        {
            foundInMainFile = true;
        }

        if (mapUnits[len] == 0.) len = "m";
        if (mapUnits[time] == 0.) time = "sec";
        if (mapUnits[mass] == 0.) mass = "kg";
    }


    SelectionRead selInclude(p_lsdynaModel, "*INCLUDE");

    while (selInclude.Next())
    {
        unsigned int id = selInclude->GetId();
        sdiString name = selInclude->GetName();
        name += "_0000.rad";

        HandleEdit includeHEdit;
        p_radiossModel->CreateEntity(includeHEdit, "#include" , name, id);
    }

    // For each *INCLUDE_TRANSFORM we need a //SUBMODEL, which contains the include.
    // We do this in a second iteration as a temporary workaround, because otherwise the
    // p_radiossModel gets confused with include ids
    EntityType includeType = p_radiossModel->GetEntityType("#include");
    selInclude.Restart();
    while (selInclude.Next())
    {
        if(selInclude->GetKeyword() == "*INCLUDE_TRANSFORM")
        {
            unsigned int id = selInclude->GetId();
            sdiString name = selInclude->GetName();

            HandleEdit submodelHEdit;
            p_radiossModel->CreateEntity(submodelHEdit, "//SUBMODEL",
                "Submodel " + to_string(id));
            EntityEdit submodelEntEdit(p_radiossModel, submodelHEdit);

            if(p_useSubmodelOffsets)
            {
                sdiValue tempVal;
                selInclude->GetValue(sdiIdentifier("IDNOFF"), tempVal);
                submodelEntEdit.SetValue(sdiIdentifier("nodeoffset"), tempVal);
                selInclude->GetValue(sdiIdentifier("IDEOFF"), tempVal);
                submodelEntEdit.SetValue(sdiIdentifier("elementoffset"), tempVal);
                selInclude->GetValue(sdiIdentifier("IDPOFF"), tempVal);
                submodelEntEdit.SetValue(sdiIdentifier("componentoffset"), tempVal);
                selInclude->GetValue(sdiIdentifier("IDMOFF"), tempVal);
                submodelEntEdit.SetValue(sdiIdentifier("materialoffset"), tempVal);
                selInclude->GetValue(sdiIdentifier("IDROFF"), tempVal);
                submodelEntEdit.SetValue(sdiIdentifier("alloptionoffset"), tempVal);
                submodelEntEdit.SetValue(sdiIdentifier("propertyoffset"), tempVal);
                /* maybe we should display an error if these are not equal to IDROFF,
                *  because we cannot propagate this to Radioss?
                selInclude->GetValue(sdiIdentifier("IDSOFF"), tempVal);
                selInclude->GetValue(sdiIdentifier("IDFOFF"), tempVal);
                selInclude->GetValue(sdiIdentifier("IDDOFF"), tempVal);
                */
            }

            HandleEdit includeHEdit;
            p_radiossModel->FindById(includeType, id, includeHEdit);
            if(!includeHEdit.IsValid()) continue; // ... but should always be valid

            EntityEdit includeEntEdit(p_radiossModel, includeHEdit);
            // TBD: Clean up SDI so that the cast isn't necessary in the following line
            HandleRead parentHRead = static_cast<const EntityRead&>(includeEntEdit).GetInclude();
            includeEntEdit.SetInclude(submodelHEdit);
            submodelEntEdit.SetInclude(parentHRead);

            // convert define transform, if present
            sdiValue tempVal;
            selInclude->GetValue(sdiIdentifier("TRANID"), tempVal);
            sdiValueEntity defineTransformEnt;
            tempVal.GetValue(defineTransformEnt);
            unsigned int TRANID = defineTransformEnt.GetId();
            if(0 < TRANID)
            {
                ConvertDefineTransform cnvrtDefineTransform(p_lsdynaModel, p_radiossModel);
                cnvrtDefineTransform.ConvertSelectedDefineTransform(TRANID, 0, submodelEntEdit.GetId());
            }
            // create and attach unit system, if necessary
            double FCTMAS=0, FCTTIM=0, FCTLEN=0;
            sdiValue fctValMas;
            sdiValue fctValTim;
            sdiValue fctValLen;
            selInclude->GetValue(sdiIdentifier("FCTMAS"), fctValMas);
            fctValMas.GetValue(FCTMAS);
            selInclude->GetValue(sdiIdentifier("FCTTIM"), fctValTim);
            fctValTim.GetValue(FCTTIM);
            selInclude->GetValue(sdiIdentifier("FCTLEN"), fctValLen);
            fctValLen.GetValue(FCTLEN);
            if(0 == FCTMAS) FCTMAS = 1;
            if(0 == FCTTIM) FCTTIM = 1;
            if(0 == FCTLEN) FCTLEN = 1;
            FCTMAS = FCTMAS * mapUnits[mass];
            FCTTIM = FCTTIM * mapUnits[time];
            FCTLEN = FCTLEN * mapUnits[len];
            char sFctMas[11] = {0};
            char sFctTim[11] = {0};
            char sFctLen[11] = {0};
            snprintf(sFctMas, 11, "%g", FCTMAS);
            snprintf(sFctTim, 11, "%g", FCTTIM);
            snprintf(sFctLen, 11, "%g", FCTLEN);

            p_radiossModel->SetCurrentCollector(submodelHEdit);
            HandleEdit beginHandleEdit;
            p_radiossModel->CreateEntity(beginHandleEdit, "/BEGIN");
            SDIHandlReadList sourceCards;
            sourceCards = { {selInclude->GetHandle()} };
            if (beginHandleEdit.IsValid())
            {
                EntityEdit beginEdit(p_radiossModel, beginHandleEdit);   
                beginEdit.SetValue(sdiIdentifier("Invers"), sdiValue(2025));
 
                beginEdit.SetValue(sdiIdentifier("Input_length_unit"), sdiValue(sdiString(sFctLen)));
                beginEdit.SetValue(sdiIdentifier("Input_time_unit"), sdiValue(sdiString(sFctTim)));
                beginEdit.SetValue(sdiIdentifier("Input_mass_unit"), sdiValue(sdiString(sFctMas)));

                sdiConvert::Convert::PushToConversionLog(std::make_pair(beginHandleEdit, sourceCards));
            }

            sdiValue fcttemVal;
            sdiString FCTTEM;
            selInclude->GetValue(sdiIdentifier("FCTTEM"), fcttemVal);
            fcttemVal.GetValue(FCTTEM);
            if(FCTTEM.size() > 0)
            {
                DynaToRad::ShowMessage(sdiMessageHandler::Level::Warning, 22,
                    "*INCLUDE_TRANSFORM", selInclude->GetName().c_str(), "FCTTEM");
            }
        }
    }
}
