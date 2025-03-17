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

#include "GlobalModelSDI.h"

#include <HCDI/hcdi_mec_pre_object.h>
#include <HCDI/hcdi_mv_descriptor.h>
#include <radiossblk.h>
#include <fstream>
#include <dll_settings.h>

#include <typedef.h>
#include <sdiModelView.h>
using namespace sdi;
using namespace std;

///////////////////////////////////////////////////////////////////////////////////////////////////
// Get Submodel From selectionRead
///////////////////////////////////////////////////////////////////////////////////////////////////
void GetSubmodelId(int& pValue,SelectionRead& entities)
{
    pValue = 0;
    HandleRead hInclude = entities->GetInclude();
    if(!hInclude.IsValid())
    {
        pValue = 0;
        return;
    }
    EntityRead include(g_pModelViewSDI, hInclude);
    sdiValue value;
    unsigned int submodelId=0;
    include.GetValue(sdiIdentifier("shadow_submodelid"), value);
    value.GetValue(submodelId);
    if(submodelId != 0)
    {
        pValue = (int) hInclude.GetId(g_pModelViewSDI);
        return;
    }
    else
    {
        hInclude = include.GetInclude();
        while(0 == submodelId)
        {
            if(!hInclude.IsValid())
            {
                pValue = 0;
                return;
            }
            else
            {
                EntityRead parent(g_pModelViewSDI, hInclude);
                parent.GetValue(sdiIdentifier("shadow_submodelid"), value);
                value.GetValue(submodelId);
                if(submodelId == 0) hInclude = parent.GetInclude();
            }
        }
        pValue = (int) hInclude.GetId(g_pModelViewSDI); 
    }
 }
//


///////////////////////////////////////////////////////////////////////////////////////////////////
// Print All Parameters
///////////////////////////////////////////////////////////////////////////////////////////////////
void printAllParameters(const char *outfilename)
{
//
    ModelViewEdit *g_pModelViewSDI = Get_ModelViewSDI();
    ofstream logFile;
    logFile.open(outfilename,std::ofstream::app);
//
    char keyword_prefix = '/';
    char comment_prefix = '*';
    sdiString keyword(&keyword_prefix, 1);
    keyword += "PARAMETER";
    SelectionRead entities(g_pModelViewSDI, keyword);//
    bool isOk = false;
    bool isParameterGlobal = false;
    bool isParameterLocal = false;
    int includeId;
//
    while(entities.Next())
    {
        GetSubmodelId(includeId,entities);

        if(includeId > 0) 
            isParameterLocal = true;
        else 
            isParameterGlobal = true;
    }
//
    if(0 < entities.Count())
    {
        logFile << "*\n";
        logFile << "*       PARAMETERS\n";
        logFile << "*       ----------\n";
        logFile << "*\n";
        logFile << "************************************************************************\n";
    }

    if(isParameterGlobal)
    {
        logFile << "*\n";
        logFile << "************************************************************************\n";
        logFile << "* GLOBAL PARAMETERS\n";
        logFile << "*\n";
        logFile << "*\n";
        logFile << "************************************************************************\n";
    }
//
    entities = SelectionRead(g_pModelViewSDI, keyword);
    while(entities.Next())
    {
        //GlobalModelSDISetCurrentEntity(entities);

        
        
        GetSubmodelId(includeId,entities);

        if (includeId == 0)
        {
            const sdiString& keyword = entities->GetKeyword();
            logFile << "*\n";
            logFile << "*      " << entities->GetName().c_str() << "\n";
            sdiValue val;
            entities->GetValue(sdiIdentifier("ParName"), val);
            sdiString ParName;
            val.GetValue(ParName);
            char valstr[500];
            int Ivalue = 0;
            if(keyword.find("REAL") != keyword.npos)
            {
                sdiValue val;
                entities->GetValue(sdiIdentifier("Rvalue"), val);
                double Rvalue;
                val.GetValue(Rvalue);
                sprintf(valstr, "%lg", Rvalue);
                logFile << "*              REFERENCE . . . = " << ParName.c_str() << "\n";
                logFile << "*              VALUE. .  . . . = " << valstr << "\n";
            }
            else if(keyword.find("INT") != keyword.npos)
            {
                sdiValue val;
                entities->GetValue(sdiIdentifier("Ivalue"), val);
                val.GetValue(Ivalue);
                sprintf(valstr, "%d", Ivalue);
                logFile << "*              REFERENCE . . . = " << ParName.c_str() << "\n";
                logFile << "*              VALUE. .  . . . = " << valstr << "\n";
            }
            else // should be TEXT
            {
                sdiValue valText, valLength;
                entities->GetValue(sdiIdentifier("Length"), valLength);
                int Length;
                valLength.GetValue(Length);
                entities->GetValue(sdiIdentifier("Text"), valText);
                sdiString Text;
                valText.GetValue(Text);
                sprintf(valstr, "%s", Text.c_str());
                logFile << "*              REFERENCE . . . = " << ParName.c_str() << "\n";
                logFile << "*              VALUE. .  . . . = " << valstr << "\n";
            }
        }
    }
//
    entities = SelectionRead(g_pModelViewSDI, keyword);
    while(entities.Next())
    {

        
        
        GetSubmodelId(includeId,entities);

        if (includeId == 0)
        {
            const sdiString& keyword = entities->GetKeyword();
            logFile << "*\n";
            sdiValue val;
            entities->GetValue(sdiIdentifier("ParName"), val);
            sdiString ParName;
            val.GetValue(ParName);
            char valstr[500];
            int Ivalue = 0;

            sdiValue idsmaxVal;
            entities->GetValue(sdiIdentifier("idsmax"), idsmaxVal);
            int idsmax = 0;
            idsmaxVal.GetValue(idsmax);

            if(keyword.find("REAL") != keyword.npos)
            {
                sdiValue val;
                entities->GetValue(sdiIdentifier("Rvalue"), val);
                double Rvalue;
                val.GetValue(Rvalue);
                sprintf(valstr, "%lg", Rvalue);
            }
            else if(keyword.find("INT") != keyword.npos)
            {
                sdiValue val;
                entities->GetValue(sdiIdentifier("Ivalue"), val);
                val.GetValue(Ivalue);
                sprintf(valstr, "%d", Ivalue);
            }
            else // should be TEXT
            {
                sdiValue valText, valLength;
                entities->GetValue(sdiIdentifier("Length"), valLength);
                int Length;
                valLength.GetValue(Length);
                entities->GetValue(sdiIdentifier("Text"), valText);
                sdiString Text;
                valText.GetValue(Text);
                sprintf(valstr, "%s", Text.c_str());
            }

            for(unsigned int i=0; i < (unsigned int) idsmax; ++i)
            {
                sdiValue idsVal, datanamesVal, rowsVal;
                entities->GetValue(sdiIdentifier("ids", 0, i), idsVal);
                sdiValueEntity ent;
                idsVal.GetValue(ent);
                sdiValueEntityType fullType = ent.GetEntityFullType();
                EntityType type;
                sdiString typeStr;
                if(fullType.IsTypeNumeric())
                {
                    type = fullType.GetTypeNumeric();
                    typeStr = g_pModelViewSDI->GetKeyword(type);
                }
                else
                {
                    typeStr = fullType.GetTypeNamed();
                    type = g_pModelViewSDI->GetEntityType(typeStr);
                }
                unsigned int id = ent.GetId();
                entities->GetValue(sdiIdentifier("datanames", 0, i), datanamesVal);
                sdiString dataname;
                datanamesVal.GetValue(dataname);
                bool isNodeBlock = false;
                bool isElementBlock = false;
                if(g_pModelViewSDI->GetSpecializationType(type) == SPECIALIZATION_TYPE_NODE &&
                    dataname.compare("unit_ID") == 0)
                {
                    isNodeBlock = true;
                }
                else if(g_pModelViewSDI->GetSpecializationType(type) == SPECIALIZATION_TYPE_ELEMENT &&
                    (dataname.compare("collector") == 0 || 
                        dataname.compare("unit_ID") == 0))
                {
                    isElementBlock = true;
                    const char *datanameLiteral = nullptr;
                    if(dataname.compare("collector") == 0)
                    {
                        id = (unsigned int)Ivalue;
                        datanameLiteral = "collector";
                    }
                    else
                    {
                        HandleRead hElem;
                        if(g_pModelViewSDI->FindById(type, id, hElem))
                        {
                            ElementRead elem(g_pModelViewSDI, hElem);
                            id = elem.GetOwnerId();
                        }
                        else
                        {
                            continue; // shouldn't happen
                        }
                        datanameLiteral = "unit_ID";
                    }
                }
                if(typeStr.compare(1, 4, "BRIC") == 0)
                {
                    // no distiction possible so far, because they are all in one id pool
                    // (except BRIC20 which for some reason have their own)
                    if(isElementBlock) typeStr = "solid elements";
                    else               typeStr = "solid element";
                }
                entities->GetValue(sdiIdentifier("rows", 0, i), rowsVal);
                unsigned int row;
                rowsVal.GetValue(row);
                if(!(typeStr.compare(1, 9, "PARAMETER") == 0))
                {                logFile << "* IN OPTION :\n";
                   if(isElementBlock)   logFile <<"* IN " << typeStr.c_str() << " ELEMENT OF PART " << id << "\n";
                   else if(isNodeBlock) logFile <<"* IN /NODE \n";
                   else                 logFile <<"* " << typeStr.c_str() << " WITH ID : "  << id << "\n";
//
                   logFile << "*     PARAMETER REFERENCE : \n";
                   logFile << "*             " << ParName.c_str() <<  "\n" ;
                   logFile << "*             REPLACED BY VALUE\n";
                   logFile << "*                 " << valstr <<  "\n" ;
                }
            }
        }
    }
//
    if(isParameterLocal)
    {
        logFile << "*\n";
        logFile << "*\n";
        logFile << "************************************************************************\n";
        logFile << "* LOCAL PARAMETERS\n";
        logFile << "*\n";
        logFile << "*\n";
        logFile << "************************************************************************\n";
    }

    entities = SelectionRead(g_pModelViewSDI, keyword);
    while(entities.Next())
    {

        
        GetSubmodelId(includeId,entities);

        if (includeId > 0)
        {
            const sdiString& keyword = entities->GetKeyword();
            logFile << "*\n";
            logFile << "*      " << entities->GetName().c_str() << "\n";
            sdiValue val;
            entities->GetValue(sdiIdentifier("ParName"), val);
            sdiString ParName;
            val.GetValue(ParName);
            char valstr[500];
            int Ivalue = 0;
            if(keyword.find("REAL") != keyword.npos)
            {
                sdiValue val;
                entities->GetValue(sdiIdentifier("Rvalue"), val);
                double Rvalue;
                val.GetValue(Rvalue);
                sprintf(valstr, "%lg", Rvalue);
                logFile << "*              REFERENCE . . . = " << ParName.c_str() << "\n";
                logFile << "*              VALUE. .  . . . = " << valstr << "\n";
            }
            else if(keyword.find("INT") != keyword.npos)
            {
                sdiValue val;
                entities->GetValue(sdiIdentifier("Ivalue"), val);
                val.GetValue(Ivalue);
                sprintf(valstr, "%d", Ivalue);
                logFile << "*              REFERENCE . . . = " << ParName.c_str() << "\n";
                logFile << "*              VALUE. .  . . . = " << valstr << "\n";
            }
            else // should be TEXT
            {
                sdiValue valText, valLength;
                entities->GetValue(sdiIdentifier("Length"), valLength);
                int Length;
                valLength.GetValue(Length);
                entities->GetValue(sdiIdentifier("Text"), valText);
                sdiString Text;
                valText.GetValue(Text);
                sprintf(valstr, "%s", Text.c_str());
                logFile << "*              REFERENCE . . . = " << ParName.c_str() << "\n";
                logFile << "*              VALUE. .  . . . = " << valstr << "\n";
            }
        }
    }

    entities = SelectionRead(g_pModelViewSDI, keyword);
    while(entities.Next())
    {
        GetSubmodelId(includeId,entities);

        if (includeId > 0)
        {
            const sdiString& keyword = entities->GetKeyword();
            logFile << "*\n";
            sdiValue val;
            entities->GetValue(sdiIdentifier("ParName"), val);
            sdiString ParName;
            val.GetValue(ParName);
            char valstr[500];
            int Ivalue = 0;

            sdiValue idsmaxVal;
            entities->GetValue(sdiIdentifier("idsmax"), idsmaxVal);
            int idsmax = 0;
            idsmaxVal.GetValue(idsmax);

            if(keyword.find("REAL") != keyword.npos)
            {
                sdiValue val;
                entities->GetValue(sdiIdentifier("Rvalue"), val);
                double Rvalue;
                val.GetValue(Rvalue);
                sprintf(valstr, "%lg", Rvalue);
            }
            else if(keyword.find("INT") != keyword.npos)
            {
                sdiValue val;
                entities->GetValue(sdiIdentifier("Ivalue"), val);
                val.GetValue(Ivalue);
                sprintf(valstr, "%d", Ivalue);
            }
            else // should be TEXT
            {
                sdiValue valText, valLength;
                entities->GetValue(sdiIdentifier("Length"), valLength);
                int Length;
                valLength.GetValue(Length);
                entities->GetValue(sdiIdentifier("Text"), valText);
                sdiString Text;
                valText.GetValue(Text);
                sprintf(valstr, "%s", Text.c_str());
            }

            for(unsigned int i=0; i < (unsigned int) idsmax; ++i)
            {
                sdiValue idsVal, datanamesVal, rowsVal;
                entities->GetValue(sdiIdentifier("ids", 0, i), idsVal);
                sdiValueEntity ent;
                idsVal.GetValue(ent);
                sdiValueEntityType fullType = ent.GetEntityFullType();
                EntityType type;
                sdiString typeStr;
                if(fullType.IsTypeNumeric())
                {
                    type = fullType.GetTypeNumeric();
                    typeStr = g_pModelViewSDI->GetKeyword(type);
                }
                else
                {
                    typeStr = fullType.GetTypeNamed();
                    type = g_pModelViewSDI->GetEntityType(typeStr);
                }
                unsigned int id = ent.GetId();
                entities->GetValue(sdiIdentifier("datanames", 0, i), datanamesVal);
                sdiString dataname;
                datanamesVal.GetValue(dataname);
                bool isNodeBlock = false;
                bool isElementBlock = false;
                if(g_pModelViewSDI->GetSpecializationType(type) == SPECIALIZATION_TYPE_NODE &&
                    dataname.compare("unit_ID") == 0)
                {
                    isNodeBlock = true;
                }
                else if(g_pModelViewSDI->GetSpecializationType(type) == SPECIALIZATION_TYPE_ELEMENT &&
                    (dataname.compare("collector") == 0 || 
                        dataname.compare("unit_ID") == 0))
                {
                    isElementBlock = true;
                    const char *datanameLiteral = nullptr;
                    if(dataname.compare("collector") == 0)
                    {
                        id = (unsigned int)Ivalue;
                        datanameLiteral = "collector";
                    }
                    else
                    {
                        HandleRead hElem;
                        if(g_pModelViewSDI->FindById(type, id, hElem))
                        {
                            ElementRead elem(g_pModelViewSDI, hElem);
                            id = elem.GetOwnerId();
                        }
                        else
                        {
                            continue; // shouldn't happen
                        }
                        datanameLiteral = "unit_ID";
                    }
                }
                if(typeStr.compare(1, 4, "BRIC") == 0)
                {
                    // no distiction possible so far, because they are all in one id pool
                    // (except BRIC20 which for some reason have their own)
                    if(isElementBlock) typeStr = "solid elements";
                    else               typeStr = "solid element";
                }
                entities->GetValue(sdiIdentifier("rows", 0, i), rowsVal);
                unsigned int row;
                rowsVal.GetValue(row);
                if(!(typeStr.compare(1, 9, "PARAMETER") == 0))
                {                logFile << "* IN OPTION :\n";
                   if(isElementBlock)   logFile <<"* IN " << typeStr.c_str() << " ELEMENT OF PART " << id << "\n";
                   else if(isNodeBlock) logFile <<"* IN /NODE \n";
                   else                 logFile << typeStr.c_str() << " WITH ID : "  << id << "\n";
//
                   logFile << "*     PARAMETER REFERENCE : \n";
                   logFile << "*             " << ParName.c_str() <<  "\n" ;
                   logFile << "*             REPLACED BY VALUE\n";
                   logFile << "*                 " << valstr <<  "\n" ;
                }
            }
        }
    }

    if(0 < entities.Count())
    {
        logFile << "*\n";
        logFile << "************************************************************************\n";
    }
    logFile.close();
}

extern "C" 
{

CDECL void cpp_print_parameters_(char *name, int *size)
{
    char *cname;
    int cname_len;
    int i;
    cname_len = *size + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*size;i++){
      cname[i] = name[i];
    }
    cname[*size]='\0';
    printAllParameters(cname);
}

CDECL void CPP_PRINT_PARAMETERS(char *name, int *size)
{cpp_print_parameters_ (name,size);}

CDECL void cpp_print_parameters__ (char *name, int *size)
{cpp_print_parameters_ (name,size);}

CDECL void cpp_print_parameters (char *name, int *size)
{cpp_print_parameters_ (name,size);}


}





