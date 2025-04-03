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
#include <buildmapping.h>



#include <boost/algorithm/string.hpp>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <radiossblk.h>
#include <dynamain.h>
#include <dyna2rad/dyna2rad.h>
#include <starter_lic.h>
#include <map>
#include <dll_settings.h>
#include <sdiSelectionObserver.h>


#include <typedef.h>
#include <sdiModelView.h>
using namespace sdi;
using namespace std;



//#include <vos/vosDynamicObject.h>
//extern "C" vosResult vosres;
//extern "C" vosDynamicObject loader;

//extern "C" bool SupplyReaderFunctionTable(mxReaderFunctionTable* rft);
//extern "C" ModelViewRead* RadiossblkReadModelEDIEDI(const char *filename);
//extern "C" const map<int, vector<string>>& RadiossblkGetMessages();
//extern "C" const char *RadiossblkGetVersion(unsigned int *pmajorVersion  = 0,
//                                            unsigned int *pminorVersion  = 0,
//                                            unsigned int *photfixVersion = 0,
//                                            unsigned int *pbuildNumber   = 0);


CDECL int cfgreader(char *modelfilename)
{
        // load deck into the reader
    ModelViewEdit *pModelViewSDI = NULL;
    try
    {
        pModelViewSDI = RadiossblkReadModelSDI(modelfilename);
        GlobalModelSDISetModel(pModelViewSDI);
        g_pModelViewSDI = pModelViewSDI;

    } catch(...) {}

    unsigned int majorVersion = 0, minorVersion = 0, hotfixVersion = 0, buildNumber = 0;

    return 0;
}


CDECL int cfgreader_inc(char *modelfilename, int *nbDynaIncludes,char *globalPath,int *globalPath_len)
{
//
// Dynamic load of library
//
        // load deck into the reader
    try
    {
        ModelViewEdit *pModelViewSDI = NULL;
        pModelViewSDI = RadiossblkReadModelSDI(modelfilename);
        RadiossblkApplyOffsets(pModelViewSDI);
        GlobalModelSDISetModel(pModelViewSDI);
        g_pModelViewSDI = pModelViewSDI;

        SelectionRead pSelection(pModelViewSDI, "/INCLUDE_LS-DYNA");

        sdiString dynaIncludeFileName;
        std::vector<sdiString> includeFileNameList;
            
        *nbDynaIncludes = pSelection.Count();

        if (pSelection.Count() > 0)
        {
            printf(" .. READING LS-DYNA INPUT FORMAT MODEL\n");
            while(pSelection.Next())
            {
                sdiString includeFileName;
                sdiValue value(includeFileName);
                pSelection->GetValue(sdiIdentifier("FileName"), value);
                value.GetValue(includeFileName);
                includeFileNameList.push_back(sdiString(globalPath)+includeFileName);
            }

            for(sdiString includeFileName : includeFileNameList)
            {
                ModelViewEdit* pDynaIncludeModelViewSDI = DynakeyReadModel(includeFileName.c_str());

                DynakeyMessages& messages = DynakeyGetMessages();
                messages.SetOffset(200000);

                sdiD2R::DynaToRad d2RObj(pDynaIncludeModelViewSDI, pModelViewSDI, includeFileName, &messages);
                d2RObj.CallConvert();

                sdiConvert::LogQueryHandle g_includeconversionLog;
                d2RObj.GetConversionLog(g_includeconversionLog);
                BuildMapping(g_includeconversionLog,pDynaIncludeModelViewSDI);

                if(pDynaIncludeModelViewSDI != NULL) delete pDynaIncludeModelViewSDI;
            }
        }

    } catch(...) {}

/*
    // Get and print reader messages
    const hwReaderMessageList& messages = RadiossblkGetMessages();
    // ... messages with id
    printf("Supported messages:\n");
    for(hwReaderMessageList::const_iterator it=messages.begin(); it!=messages.end(); it++)
    {
        const hwReaderMessage &message = *it;
        if(message.GetId() != 0)
        {
            switch(message.GetType())
            {
            case 0: printf("MESSAGE ID : %u\n", message.GetId()); break;
            case 1: printf("WARNING ID : %u\n", message.GetId()); break;
            default: printf("ERROR ID : %u\n", message.GetId()); break;
            }
            printf("%s\n", message.GetTitle().c_str());
            printf("line %u in file %s\n",
                       message.GetLinenumber(), message.GetFilename().c_str());
            printf("DESCRIPTION :\n");
            printf("%s\n", message.GetDescription().c_str());
            if(!message.GetSolution().empty())
            {
                printf("%s\n", message.GetSolution().c_str());
            }
        }
    }
*/
    // ... messages without id
/*
    printf("Unknown messages:\n");
    for(hwReaderMessageList::const_iterator it=messages.begin(); it!=messages.end(); it++)
    {
        const hwReaderMessage &message = *it;
        if(message.GetId() == 0)
        {
            printf("Message type %d: ", message.GetType());
            printf("%s\n", message.GetDescription().c_str());
            if(!message.GetSolution().empty())
            {
                printf("%s\n", message.GetSolution().c_str());
            }
        }
    }
*/

    unsigned int majorVersion = 0, minorVersion = 0, hotfixVersion = 0, buildNumber = 0;

    return 0;
}

extern "C" 
{

void CDECL cpp_build_model_(char *name, int *size, int *res)
{
    char *cname;
    int cname_len;
    int i;
    int ret_val;

    cname_len = *size + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*size;i++)  cname[i] = name[i];
    cname[*size]='\0'; 

#ifdef RD_COM

    ret_val = create_radflex_process();
#endif

    *res = cfgreader(cname);

#ifdef RD_COM

    ret_val = starter_lic_checkout(cname,IS_RD_MODEL);
#endif
}

void CDECL CPP_BUILD_MODEL(char *name, int *size, int *res)
{cpp_build_model_ (name, size, res);}

void CDECL cpp_build_model__ (char *name, int *size, int *res)
{cpp_build_model_ (name, size, res);}

void  CDECL cpp_build_model (char *name, int *size, int *res)
{cpp_build_model_ (name, size, res);}


void CDECL cpp_build_model_inc_(char *name, int *size, int *res, int *nbDynaIncludes, char *GLOBAL_PATH, int *SGLOBAL_PATH)
{
    char *cname,*globalPath;
    int cname_len,globalPath_len;
    int i;
    int ret_val;

    cname_len = *size + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*size;i++)  cname[i] = name[i];
    cname[*size]='\0'; 

    globalPath_len = *SGLOBAL_PATH+ 1;
    globalPath=(char*) malloc(sizeof(char)*globalPath_len);
#ifdef _WIN64
    strncpy_s(globalPath,globalPath_len,GLOBAL_PATH,*SGLOBAL_PATH);
#else
    strncpy(globalPath,GLOBAL_PATH,*SGLOBAL_PATH);
#endif
    globalPath[*SGLOBAL_PATH]='\0'; 

#ifdef RD_COM

    ret_val = create_radflex_process();
#endif

    *res = cfgreader_inc(cname,nbDynaIncludes,globalPath,&globalPath_len);

#ifdef RD_COM

    ret_val = starter_lic_checkout(cname,IS_RD_MODEL);
#endif
}

void CDECL CPP_BUILD_MODEL_INC(char *name, int *size, int *res, int *nbDynaIncludes, char *GLOBAL_PATH, int *SGLOBAL_PATH)
{cpp_build_model_inc_ (name, size, res,nbDynaIncludes,GLOBAL_PATH,SGLOBAL_PATH);}

void CDECL cpp_build_model_inc__ (char *name, int *size, int *res, int *nbDynaIncludes, char *GLOBAL_PATH, int *SGLOBAL_PATH)
{cpp_build_model_inc_ (name, size, res,nbDynaIncludes,GLOBAL_PATH,SGLOBAL_PATH);}

void CDECL cpp_build_model_inc (char *name, int *size, int *res, int *nbDynaIncludes, char *GLOBAL_PATH, int *SGLOBAL_PATH)
{cpp_build_model_inc_ (name, size, res,nbDynaIncludes,GLOBAL_PATH,SGLOBAL_PATH);}



}
