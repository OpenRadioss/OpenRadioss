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

#include <radiossblk.h>
#include <dynamain.h>

#include <HCDI/hcdi_mv_descriptor.h>

#include <boost/unordered_map.hpp>
#include <tuple>

#include <dyna2rad/dyna2rad.h>

#include <stdarg.h>
#include <string>
#include <fstream>
#include <string.h>
#include <dll_settings.h>


#include <sdiSelectionObserver.h>
#include <typedef.h>
#include <sdiModelView.h>
using namespace sdi;
using namespace std;

// Variables for Licensing
#define IS_RD_MODEL 1
#define IS_DYNA_MODEL 2



int create_radflex_process();
int starter_lic_checkout( char * filename,int model);



ModelViewEdit* pRadiossModelViewSDI = NULL;
ModelViewEdit* pDynaModelViewSDI = NULL;

typedef boost::unordered_map<HandleRead*, sdiVector<HandleRead*>> SourceDestQueryHandle;
//SourceDestQueryHandle g_conversionLog;
sdiConvert::LogQueryHandle g_conversionLog;


extern "C" int SdiD2RConvert(ModelViewRead* dynaModelViewSDI, ModelViewEdit* radModelViewSDI,
    boost::unordered_map<HandleRead*, sdiVector<HandleRead*>>&g_conversionLog);


sdiString GetFileNameFromPath(const sdiString& filePath)
{
    char delim = '/';
#ifdef OS_WIN
    delim = '\\';
#endif
    string retVal;
    size_t pos = filePath.find_last_of(delim);
    if (pos != string::npos) {
        retVal = filePath.substr(pos + 1);
        pos = retVal.find_last_of('.');
        if (pos != string::npos)
            retVal = retVal.substr(0, pos);
    }
    if (!retVal.size())
        return filePath;
    return retVal;
}



bool ReadDynaAndConvert(const char *modelfilename, const char *outfilename)
{
    bool isOk = true;
    std::string str_error;
    int ret_val;

#ifdef RD_COM

    ret_val = create_radflex_process();


    ret_val = starter_lic_checkout((char *)modelfilename,IS_DYNA_MODEL);
#endif

    // Read Dyna deck and populate model
    try
    {
        //printf("Reading Dyna... %s\n",modelfilename);
        printf(" .. READING LS-DYNA INPUT FORMAT MODEL\n");
        pDynaModelViewSDI = DynakeyReadModel(modelfilename);
        fflush(stdout);
    }
    catch(...)
    {
        str_error = "Unkown problem when reading Dyna deck.\n";
        isOk = false;
    }

    if(isOk)
    {
        /*Getting file name triming off extension*/
        sdiString radRunName = GetFileNameFromPath(sdiString(modelfilename));

        // Create empty Radioss model
        pRadiossModelViewSDI = RadiossblkNewModel();

        // Convertor error messages initialization
        DynakeyMessages& messages = DynakeyGetMessages();
        messages.SetOffset(200000);

        // activate automatic setting of include file in the p_radiossModel
        // from the currently selected entity in p_lsdynaModel
        SDIModelViewSelectionObservable *pModelObservable =
            dynamic_cast<SDIModelViewSelectionObservable*>(pDynaModelViewSDI);
        if(pModelObservable)
        {
            EntityType type = pRadiossModelViewSDI->GetEntityType("#include");
            SDICurrentIncludeFromSelectionSetter *pIncludeSetter =
                new SDICurrentIncludeFromSelectionSetter(*pRadiossModelViewSDI, type);
            pModelObservable->SetSelectionObserver(pIncludeSetter, true);
        }

        // Read Radioss includes
        SelectionRead selRadioss(pDynaModelViewSDI, "*INCLUDE_RADIOSS");

        if(selRadioss.Count() == 1)
        {
          while (selRadioss.Next())
          {
              sdiString includeFileName;
              sdiValue value(includeFileName);
              selRadioss->GetValue(sdiIdentifier("FileName"), value);
              value.GetValue(includeFileName);
              RadiossblkReadInclude(pRadiossModelViewSDI, includeFileName.c_str());
          }
        }
        else if(selRadioss.Count() > 1)
        {
            sdiD2R::DynaToRad::ShowMessage(sdiMessageHandler::Level::Error, 31,
                "*INCLUDE_RADIOSS");
            //messages.ShowMessage(sdiMessageHandler::Level::Error, 31,"*INCLUDE_RADIOSS");
        }


// Call converter
        sdiD2R::DynaToRad d2RObj(pDynaModelViewSDI, pRadiossModelViewSDI, radRunName, &messages);

        d2RObj.CallConvert();
//
        d2RObj.GetConversionLog(g_conversionLog);

        if(nullptr == pRadiossModelViewSDI) // only happens if environment isn't ok
        {
            RadiossblkApplyOffsets(pRadiossModelViewSDI);
            const hwReaderMessageList& messages = RadiossblkGetMessages();
            isOk = false;
            return isOk;
        }
//
        GlobalModelSDISetModel(pRadiossModelViewSDI);

//      Fill the Radioss2dyna mapping
        BuildMapping(g_conversionLog,pDynaModelViewSDI);

        // "unlink" the two models
        if(pModelObservable)
        {
            pModelObservable->SetSelectionObserver(nullptr);
        }


    }


    if(str_error.size() > 0) cout << str_error;
    return isOk;
}


extern "C" 
{

CDECL void cpp_read_dyna_and_convert_(char *name, int *size, int *res,char *name1, int *size1)
{
    char *cname;
    int cname_len;
    int i;
    
    cname_len = *size + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*size;i++){ cname[i] = name[i];}
    cname[*size]='\0';

    char *cname1;
    int cname_len1;
    
    cname_len1 = *size1 + 1;
    cname1=(char*) malloc(sizeof(char)*cname_len1);
    for(i=0;i<*size1;i++){ cname1[i] = name1[i];}
    cname1[*size1]='\0';
    ReadDynaAndConvert(cname,cname1);
}

CDECL void CPP_READ_DYNA_AND_CONVERT(char *name, int *size, int *res,char *name1, int *size1)
{cpp_read_dyna_and_convert_ (name, size, res, name1, size1);}

CDECL void cpp_read_dyna_and_convert__ (char *name, int *size, int *res,char *name1, int *size1)
{cpp_read_dyna_and_convert_ (name, size, res, name1, size1);}

CDECL void cpp_read_dyna_and_convert (char *name, int *size, int *res,char *name1, int *size1)
{cpp_read_dyna_and_convert_ (name, size, res, name1, size1);}


}




