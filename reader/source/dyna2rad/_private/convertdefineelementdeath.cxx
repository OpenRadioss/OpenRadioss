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

#include <dyna2rad/convertdefineelementdeath.h>
#include <dyna2rad/dyna2rad.h>

using namespace std;
using namespace sdi;

void sdiD2R::ConvertDefineElementDeath::ConvertSelectedDefineElementDeath()
{
    ConvertEntities();
}

void sdiD2R::ConvertDefineElementDeath::ConvertEntities()
{
    ConvertElementDeathSolid();
}

void sdiD2R::ConvertDefineElementDeath::ConvertElementDeathSolid()
{

    SelectionRead selElementDeath(p_lsdynaModel, "*DEFINE_ELEMENT_DEATH");

    EntityType radElementDeathType = p_radiossModel->GetEntityType("/ACTIV");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");

    while (selElementDeath.Next())
    {
        sdiString keyWord = selElementDeath->GetKeyword();

        sdiString defineElementDeathName = "Active";
        sdiValue tempValue;
        //-------------
        //--SOLID--
        //-------------
        if(keyWord.find("DEFINE_ELEMENT_DEATH_SOLID_SET") != keyWord.npos ||
           keyWord.find("DEFINE_ELEMENT_DEATH_THICK_SHELL_SET") != keyWord.npos)
        {
           sdiValueEntity sidId;
           tempValue = sdiValue(sidId);
           selElementDeath->GetValue(sdiIdentifier("SID"), tempValue);
           tempValue.GetValue(sidId);
           unsigned int sid = sidId.GetId();

           if (sid > 0)
           {
               HandleRead defineElementDeathHRead;
               EntityRead defineElementDeathEntityRead(p_lsdynaModel, defineElementDeathHRead);

               HandleEdit activHEdit;
               p_radiossModel->CreateEntity(activHEdit, "/ACTIV", defineElementDeathName);
               EntityEdit activEdit(p_radiossModel, activHEdit);

               activEdit.SetValue(sdiIdentifier("grbric_ID"), sdiValue(sdiValueEntity(radSetType, sid)));
               p_ConvertUtils.CopyValue(*selElementDeath, activEdit, "TIME", "Tstop");
               activEdit.SetValue(sdiIdentifier("Iform"), sdiValue(2));

               //---
               sdiConvert::SDIHandlReadList sourceDefineElementDeath = { {{selElementDeath->GetHandle()}} };
               sdiConvert::Convert::PushToConversionLog(std::make_pair(activHEdit, sourceDefineElementDeath));
           }
        }
        else if(keyWord.find("DEFINE_ELEMENT_DEATH_SOLID") != keyWord.npos ||
                keyWord.find("DEFINE_ELEMENT_DEATH_THICK_SHELL") != keyWord.npos)
        {
           sdiValueEntity eidId;
           tempValue = sdiValue(eidId);
           selElementDeath->GetValue(sdiIdentifier("EID"), tempValue);
           tempValue.GetValue(eidId);
           unsigned int eid = eidId.GetId();

           HandleRead elementHRead;
           p_radiossModel->FindById(p_radiossModel->GetEntityType("/BRICK"), eid, elementHRead);

           if (elementHRead.IsValid())
           {
               HandleRead defineElementDeathHRead;
               EntityRead defineElementDeathEntityRead(p_lsdynaModel, defineElementDeathHRead);

               HandleEdit activHEdit;
               p_radiossModel->CreateEntity(activHEdit, "/ACTIV", defineElementDeathName);
               EntityEdit activEdit(p_radiossModel, activHEdit);

               // new grbric SET from element_Id (eid)
               HandleEdit setHEdit;
               p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", "SET_GENERAL_ACTIVE_option");
               EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

               SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
               SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("SOLID")));
               SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
               SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/BRICK"),eid))); 
               //activEdit.SetEntityHandle(sdiIdentifier("grbric_ID"), setHEdit);
               activEdit.SetValue(sdiIdentifier("grbric_ID"), sdiValue(sdiValueEntity(radSetType, SetEntityEdit.GetId())));

               p_ConvertUtils.CopyValue(*selElementDeath, activEdit, "TIME", "Tstop");
               activEdit.SetValue(sdiIdentifier("Iform"), sdiValue(2));

               //---
               sdiConvert::SDIHandlReadList sourceDefineElementDeath = { {{selElementDeath->GetHandle()}} };
               sdiConvert::Convert::PushToConversionLog(std::make_pair(activHEdit, sourceDefineElementDeath));
           }
        }
        //-------------
        //--SHELL + SH3N--
        //-------------
        else if(keyWord.find("DEFINE_ELEMENT_DEATH_SHELL_SET") != keyWord.npos)
        {
           sdiValueEntity sidId;
           tempValue = sdiValue(sidId);
           selElementDeath->GetValue(sdiIdentifier("SID"), tempValue);
           tempValue.GetValue(sidId);
           unsigned int sid = sidId.GetId();

           // extract SHELLs and SH3Ns from set SID
           sdiUIntList shellList,sh3nList;
           int shellListSize=0;
           int sh3nListSize=0;
           HandleEdit radSetHread;
           if (p_radiossModel->FindById(radSetType, sid, radSetHread))
           p_ConvertUtils.ExtractShellsFromSet(radSetHread, shellList);
           shellListSize = (int)shellList.size();
           p_ConvertUtils.ExtractSh3nsFromSet(radSetHread, sh3nList);
           sh3nListSize = (int)sh3nList.size();

           if (sid > 0)
           {
               HandleRead defineElementDeathHRead;
               EntityRead defineElementDeathEntityRead(p_lsdynaModel, defineElementDeathHRead);

               HandleEdit activHEdit;
               p_radiossModel->CreateEntity(activHEdit, "/ACTIV", defineElementDeathName);
               EntityEdit activEdit(p_radiossModel, activHEdit);

               if(shellListSize >0 )activEdit.SetValue(sdiIdentifier("grshel_ID"), sdiValue(sdiValueEntity(radSetType, sid)));
              if(sh3nListSize >0 )activEdit.SetValue(sdiIdentifier("grsh3n_ID"), sdiValue(sdiValueEntity(radSetType, sid)));
               p_ConvertUtils.CopyValue(*selElementDeath, activEdit, "TIME", "Tstop");
               activEdit.SetValue(sdiIdentifier("Iform"), sdiValue(2));

               //---
               sdiConvert::SDIHandlReadList sourceDefineElementDeath = { {{selElementDeath->GetHandle()}} };
               sdiConvert::Convert::PushToConversionLog(std::make_pair(activHEdit, sourceDefineElementDeath));
           }
        }
        else if(keyWord.find("DEFINE_ELEMENT_DEATH_SHELL") != keyWord.npos)
        {
           sdiValueEntity eidId;
           tempValue = sdiValue(eidId);
           selElementDeath->GetValue(sdiIdentifier("EID"), tempValue);
           tempValue.GetValue(eidId);
           unsigned int eid = eidId.GetId();

           //HandleRead elementHRead;
           //p_radiossModel->FindById(p_radiossModel->GetEntityType("/BRICK"), eid, elementHRead);

           int nbSh3n=0;
           int nbShell=0;
           HandleRead elementsh4HRead;
           p_radiossModel->FindById(p_radiossModel->GetEntityType("/SHELL"), eid, elementsh4HRead);
           if ( elementsh4HRead.IsValid()) nbShell++;
           HandleRead elementsh3HRead;
           p_radiossModel->FindById(p_radiossModel->GetEntityType("/SH3N"), eid, elementsh3HRead);
           if ( elementsh3HRead.IsValid()) nbSh3n++;

           if (nbShell > 0)
           {
               HandleRead defineElementDeathHRead;
               EntityRead defineElementDeathEntityRead(p_lsdynaModel, defineElementDeathHRead);

               HandleEdit activHEdit;
               p_radiossModel->CreateEntity(activHEdit, "/ACTIV", defineElementDeathName);
               EntityEdit activEdit(p_radiossModel, activHEdit);

               // new grbric SET from element_Id (eid)
               HandleEdit setHEdit;
               p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", "SET_GENERAL_ACTIVE_option_SHELL");
               EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

               SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
               SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("SHELL")));
               SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
               SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SHELL"),eid))); 
               //activEdit.SetEntityHandle(sdiIdentifier("grshel_ID"), setHEdit);
               activEdit.SetValue(sdiIdentifier("grshel_ID"), sdiValue(sdiValueEntity(radSetType, SetEntityEdit.GetId())));

               p_ConvertUtils.CopyValue(*selElementDeath, activEdit, "TIME", "Tstop");
               activEdit.SetValue(sdiIdentifier("Iform"), sdiValue(2));

               //---
               sdiConvert::SDIHandlReadList sourceDefineElementDeath = { {{selElementDeath->GetHandle()}} };
               sdiConvert::Convert::PushToConversionLog(std::make_pair(activHEdit, sourceDefineElementDeath));
           }
           if (nbSh3n > 0)
           {
               HandleRead defineElementDeathHRead;
               EntityRead defineElementDeathEntityRead(p_lsdynaModel, defineElementDeathHRead);

               HandleEdit activHEdit;
               p_radiossModel->CreateEntity(activHEdit, "/ACTIV", defineElementDeathName);
               EntityEdit activEdit(p_radiossModel, activHEdit);

               // new grbric SET from element_Id (eid)
               HandleEdit setHEdit;
               p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", "SET_GENERAL_ACTIVE_option_SH3N");
               EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

               SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
               SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("SH3N")));
               SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
               SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/SH3N"),eid))); 
               //activEdit.SetEntityHandle(sdiIdentifier("grsh3n_ID"), setHEdit);
               activEdit.SetValue(sdiIdentifier("grsh3n_ID"), sdiValue(sdiValueEntity(radSetType, SetEntityEdit.GetId())));

               p_ConvertUtils.CopyValue(*selElementDeath, activEdit, "TIME", "Tstop");
               activEdit.SetValue(sdiIdentifier("Iform"), sdiValue(2));

               //---
               sdiConvert::SDIHandlReadList sourceDefineElementDeath = { {{selElementDeath->GetHandle()}} };
               sdiConvert::Convert::PushToConversionLog(std::make_pair(activHEdit, sourceDefineElementDeath));
           }
        }
        //-------------
        //--BEAM--
        //-------------
        if(keyWord.find("DEFINE_ELEMENT_DEATH_BEAM_SET") != keyWord.npos)
        {
           sdiValueEntity sidId;
           tempValue = sdiValue(sidId);
           selElementDeath->GetValue(sdiIdentifier("SID"), tempValue);
           tempValue.GetValue(sidId);
           unsigned int sid = sidId.GetId();

           if (sid > 0)
           {
               HandleRead defineElementDeathHRead;
               EntityRead defineElementDeathEntityRead(p_lsdynaModel, defineElementDeathHRead);

               HandleEdit activHEdit;
               p_radiossModel->CreateEntity(activHEdit, "/ACTIV", defineElementDeathName);
               EntityEdit activEdit(p_radiossModel, activHEdit);

               activEdit.SetValue(sdiIdentifier("grbeam_ID"), sdiValue(sdiValueEntity(radSetType, sid)));
               p_ConvertUtils.CopyValue(*selElementDeath, activEdit, "TIME", "Tstop");
               activEdit.SetValue(sdiIdentifier("Iform"), sdiValue(2));

               //---
               sdiConvert::SDIHandlReadList sourceDefineElementDeath = { {{selElementDeath->GetHandle()}} };
               sdiConvert::Convert::PushToConversionLog(std::make_pair(activHEdit, sourceDefineElementDeath));
           }
        }
        else if(keyWord.find("DEFINE_ELEMENT_DEATH_BEAM") != keyWord.npos)
        {
           sdiValueEntity eidId;
           tempValue = sdiValue(eidId);
           selElementDeath->GetValue(sdiIdentifier("EID"), tempValue);
           tempValue.GetValue(eidId);
           unsigned int eid = eidId.GetId();

           HandleRead elementHRead;
           p_radiossModel->FindById(p_radiossModel->GetEntityType("/BEAM"), eid, elementHRead);

           if (elementHRead.IsValid())
           {
               HandleRead defineElementDeathHRead;
               EntityRead defineElementDeathEntityRead(p_lsdynaModel, defineElementDeathHRead);

               HandleEdit activHEdit;
               p_radiossModel->CreateEntity(activHEdit, "/ACTIV", defineElementDeathName);
               EntityEdit activEdit(p_radiossModel, activHEdit);

               // new grbeam SET from element_Id (eid)
               HandleEdit setHEdit;
               p_radiossModel->CreateEntity(setHEdit, "/SET/GENERAL", "SET_GENERAL_ACTIVE_option_BEAM");
               EntityEdit SetEntityEdit(p_radiossModel, setHEdit);

               SetEntityEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
               SetEntityEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("SOLID")));
               SetEntityEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
               SetEntityEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(p_radiossModel->GetEntityType("/BEAM"),eid))); 
               //activEdit.SetEntityHandle(sdiIdentifier("grbeam_ID"), setHEdit);
               activEdit.SetValue(sdiIdentifier("grbeam_ID"), sdiValue(sdiValueEntity(radSetType, SetEntityEdit.GetId())));

               p_ConvertUtils.CopyValue(*selElementDeath, activEdit, "TIME", "Tstop");
               activEdit.SetValue(sdiIdentifier("Iform"), sdiValue(2));

               //---
               sdiConvert::SDIHandlReadList sourceDefineElementDeath = { {{selElementDeath->GetHandle()}} };
               sdiConvert::Convert::PushToConversionLog(std::make_pair(activHEdit, sourceDefineElementDeath));
           }
        }
    }
}

