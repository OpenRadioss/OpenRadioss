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

#include <dyna2rad/convertdampings.h>
#include <dyna2rad/dyna2rad.h>
#include <dyna2rad/sdiUtils.h>

using namespace sdi;
using namespace std;

void sdiD2R::ConvertDamping::ConvertAllDampings()
{
    ConvertEntities();
}

void sdiD2R::ConvertDamping::ConvertEntities()
{
    ConvertDampingGlobal();

    ConvertDampingPartStiffness();
    
    ConvertDampingRelative();

    ConvertDampingFrequencyRange();
}

void sdiD2R::ConvertDamping::ConvertDampingGlobal()
{
    EntityType radDampingType = p_radiossModel->GetEntityType("/DAMP");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    SelectionRead selDampingGlobal(p_lsdynaModel, "*DAMPING_GLOBAL");

    while (selDampingGlobal.Next())
    {
        sdiString dampingGlobalName = selDampingGlobal->GetName();

        unsigned int dampingGlobalId = selDampingGlobal->GetId();
        if (!p_radiossModel->IsIdAvailable(radDampingType, dampingGlobalId))
            dampingGlobalId = p_ConvertUtils.GetDynaMaxEntityID(srcEntityType);
        HandleEdit dampingHEdit;
        p_radiossModel->CreateEntity(dampingHEdit, "/DAMP", selDampingGlobal->GetName(), dampingGlobalId);
        EntityEdit dampingEdit(p_radiossModel, dampingHEdit);

        double VALDAMP = 0;
        sdiValue tempVal(VALDAMP);
        selDampingGlobal->GetValue(sdiIdentifier("VALDAMP"), tempVal);
        tempVal.GetValue(VALDAMP);

        double STX = 0;
        tempVal = sdiValue(STX);
        selDampingGlobal->GetValue(sdiIdentifier("STX"), tempVal);
        tempVal.GetValue(STX);

        double STY = 0;
        tempVal = sdiValue(STY);
        selDampingGlobal->GetValue(sdiIdentifier("STY"), tempVal);
        tempVal.GetValue(STY);

        double STZ = 0;
        tempVal = sdiValue(STZ);
        selDampingGlobal->GetValue(sdiIdentifier("STZ"), tempVal);
        tempVal.GetValue(STZ);

        double SRX = 0;
        tempVal = sdiValue(SRX);
        selDampingGlobal->GetValue(sdiIdentifier("SRX"), tempVal);
        tempVal.GetValue(SRX);

        double SRY = 0;
        tempVal = sdiValue(SRY);
        selDampingGlobal->GetValue(sdiIdentifier("SRY"), tempVal);
        tempVal.GetValue(SRY);

        double SRZ = 0;
        tempVal = sdiValue(SRZ);
        selDampingGlobal->GetValue(sdiIdentifier("SRZ"), tempVal);
        tempVal.GetValue(SRZ);

        dampingEdit.SetValue(sdiIdentifier("Mass_Damp_Factor_Option"), sdiValue(true));

        if (STX == 0 && STY == 0 && STZ == 0 && SRX == 0 && SRY == 0 && SRZ == 0)
        {
            dampingEdit.SetValue(sdiIdentifier("Alpha"), sdiValue(VALDAMP));
            dampingEdit.SetValue(sdiIdentifier("Alpha_y"), sdiValue(VALDAMP));
            dampingEdit.SetValue(sdiIdentifier("Alpha_z"), sdiValue(VALDAMP));
            dampingEdit.SetValue(sdiIdentifier("Alpha_xx"), sdiValue(VALDAMP));
            dampingEdit.SetValue(sdiIdentifier("Alpha_yy"), sdiValue(VALDAMP));
            dampingEdit.SetValue(sdiIdentifier("Alpha_zz"), sdiValue(VALDAMP));
        }
        else
        {
            dampingEdit.SetValue(sdiIdentifier("Alpha"), sdiValue(VALDAMP*STX));
            dampingEdit.SetValue(sdiIdentifier("Alpha_y"), sdiValue(VALDAMP*STY));
            dampingEdit.SetValue(sdiIdentifier("Alpha_z"), sdiValue(VALDAMP*STZ));
            dampingEdit.SetValue(sdiIdentifier("Alpha_xx"), sdiValue(VALDAMP*SRX));
            dampingEdit.SetValue(sdiIdentifier("Alpha_yy"), sdiValue(VALDAMP*SRY));
            dampingEdit.SetValue(sdiIdentifier("Alpha_zz"), sdiValue(VALDAMP*SRZ));
        }

        HandleEdit radSetHEdit;
        p_radiossModel->CreateEntity(radSetHEdit, "/SET/GENERAL", dampingGlobalName);
        EntityEdit SetEntityEdit(p_radiossModel, radSetHEdit);

        SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
        SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));

        dampingEdit.SetEntityHandle(sdiIdentifier("grnod_id"), radSetHEdit);


        if (dampingHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceDamping = { {selDampingGlobal->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(dampingHEdit, sourceDamping));
        }

        if (dampingHEdit.IsValid() && radSetHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceDamping = { {selDampingGlobal->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radSetHEdit, sourceDamping));
        }
    }
}


void sdiD2R::ConvertDamping::ConvertDampingPartStiffness()
{
    EntityType radDampingType = p_radiossModel->GetEntityType("/DAMP");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    SelectionRead selDampingPartStiffness(p_lsdynaModel, "*DAMPING_PART_STIFFNESS");

    while (selDampingPartStiffness.Next())
    {
        sdiString dampingPartStiffName = selDampingPartStiffness->GetName();
        sdiString keyWord = selDampingPartStiffness->GetKeyword();

        unsigned int dampingPartStiffId = selDampingPartStiffness->GetId();
        if (!p_radiossModel->IsIdAvailable(radDampingType, dampingPartStiffId))
            dampingPartStiffId = p_ConvertUtils.GetDynaMaxEntityID(srcEntityType);
        HandleEdit dampingHEdit;
        p_radiossModel->CreateEntity(dampingHEdit, "/DAMP", selDampingPartStiffness->GetName(), dampingPartStiffId);
        EntityEdit dampingEdit(p_radiossModel, dampingHEdit);

        double COEF = 0;
        sdiValue tempVal(COEF);
        selDampingPartStiffness->GetValue(sdiIdentifier("COEF"), tempVal);
        tempVal.GetValue(COEF);

        HandleEdit radSetHEdit;

        if (keyWord.find("_SET") != keyWord.npos)
        {
          HandleRead lsdPSIDHread;
          selDampingPartStiffness->GetEntityHandle(sdiIdentifier("PSID"), lsdPSIDHread);
          EntityRead psidRead(p_lsdynaModel, lsdPSIDHread);
          dampingEdit.SetValue(sdiIdentifier("grnod_id"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(psidRead.GetId(), "*SET_PART" ))));
        }
        else
        {
          HandleRead lsdPIDHread;
          selDampingPartStiffness->GetEntityHandle(sdiIdentifier("PID"), lsdPIDHread);
          unsigned int partEntityId = lsdPIDHread.GetId(p_lsdynaModel);

          p_radiossModel->CreateEntity(radSetHEdit, "/SET/GENERAL", dampingPartStiffName);
          EntityEdit SetEntityEdit(p_radiossModel, radSetHEdit);

          SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
          SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
          SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
          SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/PART"), partEntityId )));

          dampingEdit.SetEntityHandle(sdiIdentifier("grnod_id"), radSetHEdit);
        }

        double LSD_DT2MS = 0;
        tempVal = sdiValue(LSD_DT2MS);
        SelectionRead selControlTimeStep(p_lsdynaModel, "*CONTROL_TIMESTEP");
        if (COEF <= 0)
        {
            dampingEdit.SetValue(sdiIdentifier("Beta"), sdiValue(abs(COEF)));
        }
        else
        {
            if (selControlTimeStep.Next())
            {
                selControlTimeStep->GetValue(sdiIdentifier("LSD_DT2MS"), tempVal);
                tempVal.GetValue(LSD_DT2MS);
                dampingEdit.SetValue(sdiIdentifier("Beta"), sdiValue(abs(LSD_DT2MS)*COEF));
            }
        }

        if (dampingHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceDamping = { {selDampingPartStiffness->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(dampingHEdit, sourceDamping));
        }

        if (dampingHEdit.IsValid() && keyWord.find("_SET") == keyWord.npos && radSetHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceDamping = { {selDampingPartStiffness->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radSetHEdit, sourceDamping));
        }
    }
}

void sdiD2R::ConvertDamping::ConvertDampingRelative()
{
    EntityType radDampingType = p_radiossModel->GetEntityType("/DAMP");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    SelectionRead selDampingRelative(p_lsdynaModel, "*DAMPING_RELATIVE");

    while (selDampingRelative.Next())
    {
        sdiString dampingRelativeName = selDampingRelative->GetName();

        unsigned int dampingRelativelId = selDampingRelative->GetId();
        if (!p_radiossModel->IsIdAvailable(radDampingType, dampingRelativelId))
            dampingRelativelId = p_ConvertUtils.GetDynaMaxEntityID(srcEntityType);
        HandleEdit dampingHEdit;
        p_radiossModel->CreateEntity(dampingHEdit, "/DAMP/VREL", selDampingRelative->GetName(), dampingRelativelId);
        EntityEdit dampingEdit(p_radiossModel, dampingHEdit);
          
          

        double lsdCdamp = 0;
        sdiValue tempVal(lsdCdamp);
        selDampingRelative->GetValue(sdiIdentifier("CDAMP"), tempVal);
        tempVal.GetValue(lsdCdamp);
        dampingEdit.SetValue(sdiIdentifier("Alpha_x"), sdiValue(lsdCdamp));

        double lsdFreq = 0;
        sdiValue tempVal1(lsdFreq);
        selDampingRelative->GetValue(sdiIdentifier("FREQ"), tempVal1);
        tempVal1.GetValue(lsdFreq);
        dampingEdit.SetValue(sdiIdentifier("Freq"), sdiValue(lsdFreq));
       
        HandleRead lsdPidrbHread;
        selDampingRelative->GetEntityHandle(sdiIdentifier("PIDRB"), lsdPidrbHread);
        EntityRead lspPidRead(p_lsdynaModel, lsdPidrbHread);

        HandleRead lsdMidHread;
        lspPidRead.GetEntityHandle(sdiIdentifier("MID"), lsdMidHread);

        sdiConvert::SDIHandlReadList radMatConvertedHandles;
        sdiConvert::Convert::GetConvertedHandles(lsdMidHread, radMatConvertedHandles);

        EntityType radRbodyType = p_radiossModel->GetEntityType("/RBODY");

        for (int i=0; i < radMatConvertedHandles.size(); i=i+1)
        {
            if (radMatConvertedHandles[i].GetType() == radRbodyType)
            {
                EntityRead radRbodyRead(p_radiossModel, radMatConvertedHandles[i]);
                dampingEdit.SetValue(sdiIdentifier("RbodyID"), sdiValue(sdiValueEntity(radSetType,radRbodyRead.GetId())));
            }
        }

        
        HandleRead lsdPSIDHread;
        selDampingRelative->GetEntityHandle(sdiIdentifier("PSID"), lsdPSIDHread);
        EntityRead psidRead(p_lsdynaModel, lsdPSIDHread);
        dampingEdit.SetValue(sdiIdentifier("grnod_id"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(psidRead.GetId(), "*SET_PART" ))));

        sdiValueEntity lsdLcid;
        sdiValue tempValue1 = sdiValue(lsdLcid);
        selDampingRelative->GetValue(sdiIdentifier("LCID"), tempValue1);
        tempValue1.GetValue(lsdLcid);
        dampingEdit.SetValue(sdiIdentifier("FuncID"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(lsdLcid.GetId(), "*SET_PART" ))));


        if (dampingHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceDamping = { {selDampingRelative->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(dampingHEdit, sourceDamping));
        }

    }
}

void sdiD2R::ConvertDamping::ConvertDampingFrequencyRange()
{
    EntityType radDampingType = p_radiossModel->GetEntityType("/DAMP/FREQUENCY_RANGE");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    SelectionRead selDampingFrequencyRange(p_lsdynaModel, "*DAMPING_FREQUENCY_RANGE");

    sdiUIntList listSetPartFrequencyRange;
    while (selDampingFrequencyRange.Next())
    {
        sdiValueEntity lsdPSID = GetValue<sdiValueEntity>(*selDampingFrequencyRange, "PSID");
        unsigned PSID = lsdPSID.GetId();
        if(PSID)
        {
            listSetPartFrequencyRange.push_back(PSID);
        }
        sdiVectorSort(listSetPartFrequencyRange);
        sdiVectorUnique(listSetPartFrequencyRange);
    }
    selDampingFrequencyRange.Restart();

    while (selDampingFrequencyRange.Next())
    {
        sdiString dampingFrequencyRangeName = selDampingFrequencyRange->GetName();

        unsigned int dampingFrequencyRangeId = selDampingFrequencyRange->GetId();
        if (!p_radiossModel->IsIdAvailable(radDampingType, dampingFrequencyRangeId))
        dampingFrequencyRangeId = p_ConvertUtils.GetDynaMaxEntityID(srcEntityType);
        
        HandleEdit dampingHEdit;
        p_radiossModel->CreateEntity(dampingHEdit, "/DAMP/FREQUENCY_RANGE", selDampingFrequencyRange->GetName(), dampingFrequencyRangeId);
        EntityEdit dampingEdit(p_radiossModel, dampingHEdit);

        double lsdCDAMP = GetValue<double>(*selDampingFrequencyRange, "CDAMP");
        double lsdFLOW = GetValue<double>(*selDampingFrequencyRange, "FLOW");
        double lsdFHIGH = GetValue<double>(*selDampingFrequencyRange, "FHIGH");
        sdiValueEntity lsdPSID = GetValue<sdiValueEntity>(*selDampingFrequencyRange, "PSID");
        unsigned PSID = lsdPSID.GetId();

        p_ConvertUtils.CopyValue(*selDampingFrequencyRange, dampingEdit, "CDAMP", "Cdamp");
        p_ConvertUtils.CopyValue(*selDampingFrequencyRange, dampingEdit, "FLOW", "Freq_low");
        p_ConvertUtils.CopyValue(*selDampingFrequencyRange, dampingEdit, "FHIGH", "Freq_high");
       
        HandleEdit radSetHEdit;
        if(PSID == 0)
        {  
            p_radiossModel->CreateEntity(radSetHEdit, "/SET/GENERAL", dampingFrequencyRangeName);
            EntityEdit SetEntityEdit(p_radiossModel, radSetHEdit);
            SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1+(int)listSetPartFrequencyRange.size()));
            SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));

            /*
                - PSID -> Part set ID.
                The requested damping is applied only to the parts in
                the set. If PSID = 0, the damping is applied to all parts except
                those referred to by other *DAMPING_FREQUENCY_RANGE
                cards.
            */
           for (size_t i = 0; i < (int)listSetPartFrequencyRange.size(); i ++)
            {
                SetEntityEdit.SetValue(sdiIdentifier("opt_D", 0, i+1), sdiValue(1));
                SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, i+1), sdiValue(sdiString("SET")));
                SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, i+1), sdiValue(1));
                SetEntityEdit.SetValue(sdiIdentifier("ids", 0, i+1), sdiValue(sdiValueEntityList(radSetType, 
                sdiUIntList(1, DynaToRad::GetRadiossSetIdFromLsdSet(listSetPartFrequencyRange[i], "*SET_PART")))));
            }
            dampingEdit.SetEntityHandle(sdiIdentifier("grpart_id"), radSetHEdit);
        }
        else
            dampingEdit.SetValue(sdiIdentifier("grpart_id"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(PSID, "*SET_PART"))));


        if (dampingHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceDamping = { {selDampingFrequencyRange->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(dampingHEdit, sourceDamping));
        }

        if (dampingHEdit.IsValid() && radSetHEdit.IsValid())
        {
            sdiConvert::SDIHandlReadList sourceDamping = { {selDampingFrequencyRange->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radSetHEdit, sourceDamping));
        }
    }
}