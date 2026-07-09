//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2026 Altair Engineering Inc.
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.


#include "GlobalModelSDI.h"

//#include <hwSDIDefs.h>
//#include <hwSDIEntity.h>
//#include <hwSDIHandles.h>
//#include <hwSDIModelView.h>
//#include <hwSDISelection.h>
#include <typedef.h>
#include <sdiModelView.h>
#include <algorithm>
#include <array>
#include <unordered_map>
#include <unordered_set>
#include <stdexcept>
#include <limits>

#include <HCDI/hcdi_mec_pre_object.h>
#include <HCDI/hcdi_mv_descriptor.h>
#include <radiossblk.h>

using namespace sdi;

ModelViewEdit *g_pModelViewSDI = NULL;

static SelectionRead *g_pSelection = NULL; // pointer to current selection
static const EntityRead *g_pEntity = NULL; // pointer to current entity

std::vector<HandleRead> g_EntityHandles;             // Vector of entities - to read an option on arbitrary order
static const EntityRead *g_pEntity_single = NULL;    // pointer to that vector - to read an option on arbitrary order

static std::vector<const IDescriptor *> g_vpDescr;
FILE *outFile;
FILE *debugFile;
static bool isDebugFile = false;

static int i=0;



ModelViewEdit * Get_ModelViewSDI(){
 return g_pModelViewSDI ;
}

void GlobalModelSDISetModel(void *pModelView)
{
    g_pModelViewSDI = (ModelViewEdit *) pModelView;
    g_pEntity = NULL;
    g_vpDescr.resize(0);
}

static const IDescriptor * GlobalModelSDIGetDescriptor(unsigned int index, const char *kernel_ftype)
{
    if(g_vpDescr.size() <= index) g_vpDescr.resize(index + 1, NULL);
    if(NULL == g_vpDescr[index]) g_vpDescr[index] = HCDI_GetDescriptorHandle(kernel_ftype);
    return g_vpDescr[index];
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Start reading database of given option
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalModelSDISelectionStart(const sdiString& keyword)
{
    if(g_pSelection) delete g_pSelection;
    g_pSelection = new SelectionRead(g_pModelViewSDI, keyword);
    g_pEntity = &(*(*g_pSelection));
    g_vpDescr.resize(0);
}

void GlobalModelSDISelectionCount(unsigned int *pCount)
{
    if(g_pSelection && pCount) *pCount = g_pSelection->Count();
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Next option
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalModelSDISelectionNext(bool *pIsOk)
{
    if(g_pSelection && pIsOk)
    {
        *pIsOk = g_pSelection->Next();
        if(!(*pIsOk)) g_pEntity = NULL;
        g_vpDescr.resize(0);
    }
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Get Type
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalModelSDISelectionType(int *valueType)
{
     const sdiString& keyword = (*g_pSelection)->GetKeyword();
     if (keyword.compare(0,5,"/PART") == 0) *valueType = 3;
     else if (keyword.compare(0,4,"/MAT") == 0) *valueType = 4;
     else if (keyword.compare(0,5,"/PROP") == 0) *valueType = 5;
     else if (keyword.compare(0,7,"/SUBSET") == 0) *valueType = 7;
     else if (keyword.compare(0,10,"//SUBMODEL") == 0) *valueType = 6;
     else if (keyword.compare(0,5,"/FAIL") == 0) *valueType = 4;
     else if (keyword.compare(0,8,"/EOS/MAT") == 0) *valueType = 4;
     else if (keyword.compare(0,8,"/ALE/MAT") == 0) *valueType = 4;
     else if (keyword.compare(0,9,"/HEAT/MAT") == 0) *valueType = 4;
     else if (keyword.compare(0,9,"/LEAK/MAT") == 0) *valueType = 4;
     else if (keyword.compare(0,10,"/EULER/MAT") == 0) *valueType = 4;
     else if (keyword.compare(0,11,"/VISC/PRONY") == 0) *valueType = 4;
     else if (keyword.compare(0,17,"/THERM_STRESS/MAT") == 0) *valueType = 4;
}


void GlobalModelSDIFindById(const sdiString& keyword, int id, bool *pIsOk)
{
    if(0 < id) // id is considered as an id
    {
        EntityType type = g_pModelViewSDI->GetEntityType(keyword);
        HandleRead handle;
        *pIsOk = g_pModelViewSDI->FindById(type, (unsigned int) id, handle);
        static bool isFirst = true;
        static EntityRead s_Entity(g_pModelViewSDI, handle); // initialization
        if(!isFirst) s_Entity = EntityRead(g_pModelViewSDI, handle); // subsequent calls
        isFirst = false;
        g_pEntity = &s_Entity;
        g_vpDescr.resize(0);
    }
    else if(0 > id) // -id+1 is considered as an index in the selection (starting with -1 for the first)
    {
        GlobalModelSDISelectionStart(keyword);
        int index = 0;
        *pIsOk = true;
        while(index < -id && *pIsOk)
        {
            GlobalModelSDISelectionNext(pIsOk);
            ++index;
        }
    }
    else
    {
        *pIsOk = false;
    }
}




///////////////////////////////////////////////////////////////////////////////////////////////////
// Get float value without physical dimension
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIGetValueDoubleNoDim(const char *dataname, double *pValue, bool *pIsOk)
{
    if(NULL == g_pEntity) { *pIsOk = false; return; }
//
    *pValue = 0.;
    sdiValue tempValFloat(*pValue);
    *pIsOk =  g_pEntity->GetValue(sdiIdentifier(dataname), tempValFloat);
    if(*pIsOk) tempValFloat.GetValue(*pValue);
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Get float value & physical dimension
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIGetValueDouble(const char *dataname, double *pValue, bool *pIsOk, 
                                int *lengthDim, int *massDim, int *timeDim, unsigned int index, unsigned int index2)
{
    if(NULL == g_pEntity) { *pIsOk = false; return; }
    *pIsOk = RadiossblkGetValueDouble(*g_pEntity, dataname, pValue,
        lengthDim, massDim, timeDim, index, index2);
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Get float value & physical dimension
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIGetValueDoubleDimDouble(const char *dataname, double *pValue, bool *pIsOk, 
                                double *lengthDim, double *massDim, double *timeDim, unsigned int index, unsigned int index2)
{
    if(NULL == g_pEntity) { *pIsOk = false; return; }
    *pIsOk = RadiossblkGetValueDouble(*g_pEntity, dataname, pValue,
        lengthDim, massDim, timeDim, index, index2);
}
/*
void GlobalEntitySDIGetValueDouble(const char *dataname, double *pValue, bool *pIsOk, 
                                int *lengthDim, int *massDim, int *timeDim, unsigned int index)
{
    *pIsOk = false;
    if(NULL == g_pEntity) { *pIsOk = false; return; }
    sdiIdentifier identifier(dataname, 0, index);
    sdiValue value;
    bool found = g_pEntity->GetValue(identifier, value);
    if(!found) return;
    if(value.GetBasicType() != BASIC_TYPE_DOUBLE) return;
    value.GetValue(*pValue);
    *pIsOk = true;

    // dimension
// Attention: This must not be done for entities which aren't stored as PreObjects!
    const IMECPreObject *pObj = (const IMECPreObject *) g_pEntity->GetHandle().GetPointer();
    const IDescriptor *pDescr = GlobalModelSDIGetDescriptor(0, pObj->GetKernelFullType());
//
//
    if(!pDescr) return;

    bool isOk = GlobalEntitySDIGetDimensions(pDescr, identifier, lengthDim, massDim, timeDim);

    if(!isOk)
    {
        // if no success in pObj, try subobjects
        vector<IMECPreObject *> subobjects;
        pObj->GetSubobject(subobjects);
        for(unsigned int i = 0; i != subobjects.size(); ++i)
        {
            const IMECPreObject *pSubobject = subobjects[i];
            if(!pSubobject) continue;
            const IDescriptor *pSubobjectDescr = GlobalModelSDIGetDescriptor(i + 1, pSubobject->GetKernelFullType());
            if(!pSubobjectDescr) continue;
            isOk = GlobalEntitySDIGetDimensions(pSubobjectDescr, identifier, lengthDim, massDim, timeDim);
            if(isOk) break;
        }
    }
}
*/
///////////////////////////////////////////////////////////////////////////////////////////////////
// Get Integer value
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIGetValueInt(const char *dataname, int *pValue, bool *pIsOk, unsigned int index_1,
                             sdiString *p_objtype_str, unsigned int index_2)
{
    if(NULL == g_pEntity) { *pIsOk = false; return; }
    // special queries:
    *pIsOk = true;
    if(!strcmp(dataname, "id"))
    {
        GlobalEntitySDIGetId(pValue, pIsOk);
        return;
    }
    else if(!strcmp(dataname, "include"))
    {
        HandleRead hInclude = g_pEntity->GetInclude();
        if(!hInclude.IsValid())
        {
            *pValue = 0;
        }
        else
        {
            *pValue = (int) hInclude.GetId(g_pModelViewSDI);
            if(p_objtype_str) *p_objtype_str = g_pModelViewSDI->GetKeyword(hInclude.GetType());
        }
        return;
    }
    else if(!strcmp(dataname, "submodel"))
    {
        GlobalEntitySDIGetSubmodelId(pValue, pIsOk);
        return;
    }
    else if(!strcmp(dataname, "unitid"))
    {
        GlobalEntitySDIGetUnitId(pValue, pIsOk);
        return;
    }

    // generic
    *pIsOk = false;
    sdiIdentifier identifier(dataname, 0, index_1, index_2);
    sdiValue value;
    bool found = g_pEntity->GetValue(identifier, value);
    if(!found) return;
    if(value.GetCompoundType() == COMPOUND_TYPE_ENTITY)
    {
        sdiValueEntity value_loc;
        *pIsOk = value.GetValue(value_loc);
        if(p_objtype_str)
        {
            sdiValueEntityType type = value_loc.GetEntityFullType();
            if(type.IsTypeNumeric())
            {
                *p_objtype_str = g_pModelViewSDI->GetKeyword(type.GetTypeNumeric()).c_str();
            }
            else
            {
                *p_objtype_str = type.GetTypeNamed();
            }
            if(p_objtype_str->empty()) *p_objtype_str = "Id";
        }
        *pValue = value_loc.GetId();
    }
    else
    {
        if(value.GetBasicType() == BASIC_TYPE_INT)
        {
            *pIsOk = value.GetValue(*pValue);
        }
        else if(value.GetBasicType() == BASIC_TYPE_UINT)
        {
            unsigned int value_loc = 0;
            *pIsOk = value.GetValue(value_loc);
            *pValue = value_loc;
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Get Id
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIGetId(int *pValue, bool *pIsOk)
{
    if(NULL == g_pEntity) { *pIsOk = false; return; }
    *pIsOk = true;
    *pValue = (int) g_pEntity->GetId();
    return;
    
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Get Id
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIGetUnitId(int *pValue, bool *pIsOk)
{
    *pIsOk = false;
    unsigned int index = 0;
    sdiIdentifier identifier("unitid");
    sdiValue value;
    sdiValueEntity unit;
    bool found = g_pEntity->GetValue(identifier, value);
    if(!found) 
    {
        *pValue = 0;
        return;
    }
    value.GetValue(unit);
    *pValue = unit.GetId();
    *pIsOk = true;
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Get Submodel Id
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIGetSubmodelId(int *pValue, bool *pIsOk)
{
    if(NULL == g_pEntity) { *pIsOk = false; return; }
    *pIsOk = true;
    HandleRead hInclude = g_pEntity->GetInclude();
    if(!hInclude.IsValid())
    {
        *pValue = 0;
        return;
    }
    EntityRead include(g_pModelViewSDI, hInclude);
    sdiValue value;
    unsigned int submodelId=0;
    include.GetValue(sdiIdentifier("shadow_submodelid"), value);
    value.GetValue(submodelId);
    if(submodelId != 0)
    {
        *pValue = (int) hInclude.GetId(g_pModelViewSDI);
        return;
    }
    hInclude = include.GetInclude();
    while(0 == submodelId)
    {
        if(!hInclude.IsValid())
        {
            *pValue = 0;
            return;
        }
        EntityRead parent(g_pModelViewSDI, hInclude);
        parent.GetValue(sdiIdentifier("shadow_submodelid"), value);
        value.GetValue(submodelId);
        if(submodelId == 0) hInclude = parent.GetInclude();
    }
    // when we come out of the above while, hInclude is the submodel
    *pValue = (int) hInclude.GetId(g_pModelViewSDI);

    /*
    EntityRead include(g_pModelViewSDI, hInclude);
    bool isSubmodel = include.GetKeyword().find("SUBMODEL") != sdiString::npos;
    if(!isSubmodel) hInclude = include.GetInclude();
    while(!isSubmodel)
    {
        if(!hInclude.IsValid())
        {
            *pValue = 0;
            return;
        }
        EntityRead parent(g_pModelViewSDI, hInclude);
        isSubmodel = parent.GetKeyword().find("SUBMODEL") != sdiString::npos;
        if(!isSubmodel) hInclude = parent.GetInclude();
    }
    // when we come out of the above while, hInclude is the submodel
    *pValue = (int) hInclude.GetId(g_pModelViewSDI);
    */
    return;
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Get String
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIGetValueString(const char *dataname, char *buffer, int *size, bool *pIsOk, unsigned int index)
{
    if(NULL == g_pEntity) { *pIsOk = false; return; }

    // special queries:
    *pIsOk = true;
    int csize;

    if(!strcmp(dataname, "solverkeyword") || !strcmp(dataname, "keyword"))
    {
        csize= (int) strlen(g_pEntity->GetKeyword().c_str());
        strncpy(buffer,  g_pEntity->GetKeyword().c_str(), std::min(*size,csize));
        return;
    }
    else if(!strcmp(dataname, "name"))
    {
        csize= (int) strlen(g_pEntity->GetName().c_str());
        strncpy(buffer,  g_pEntity->GetName().c_str(), std::min(*size,csize));
        return;
    }

    // generic
    sdiIdentifier identifier(dataname, 0, index);
    sdiValue value;
    bool found =  g_pEntity->GetValue(identifier, value);
    if(!found) return;
    if(value.GetBasicType() == BASIC_TYPE_STRING)
    {
 
        sdiString valuestr;
        value.GetValue(valuestr);
        csize= (int) strlen(valuestr.c_str());
        strncpy(buffer,  valuestr.c_str(), std::min(*size,csize));
        return;
    }
    // BASIC_TYPE_CHAR, BASIC_TYPE_UCHAR to be handled here?

    *pIsOk = false;
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Get Boolean value
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIGetValueBool(const char *dataname, bool *pValue, bool *pIsOk, unsigned int index)
{
    *pIsOk = false;
    sdiIdentifier identifier(dataname, 0, index);
    sdiValue value;
    bool found = g_pEntity->GetValue(identifier, value);
    if(!found) return;
    *pIsOk = value.GetValue(*pValue);
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Get is crypted information for the entire option (at least one line crypted -> true)
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIIsCrypted(bool *pIsCrypted)
{
    *pIsCrypted = false;
    if(NULL == g_pEntity) return;
    // Attention: This must not be done for entities which aren't stored as PreObjects!
    const IMECPreObject *pObj = (const IMECPreObject *) g_pEntity->GetHandle().GetPointer();
    if(pObj)
    {
        const char *a_cryptingref=pObj->GetCryptingReference ();
        if(a_cryptingref) *pIsCrypted = true;
    }
}
///////////////////////////////////////////////////////////////////////////////////////////////////
void PrintEntity(FILE *file, ModelViewRead *pModelView, const EntityRead &entity,int *is_dyna);
///////////////////////////////////////////////////////////////////////////////////////////////////
// Print entity in a file
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIWrite(int *is_dyna)
{
    bool isOk=false;
    GlobalModelSDISelectionNext(&isOk);
    PrintEntity(outFile,g_pModelViewSDI,*g_pEntity,is_dyna);
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Open File outFile
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalModelOpenFile(char *fileName,int *s_fileName)
{
    
    char *cname;
    int cname_len;
    int i;
    cname_len = *s_fileName + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*s_fileName;i++)  cname[i] = fileName[i];
    cname[*s_fileName]='\0';
    outFile = fopen(cname,"w");

    if(cname) free(cname);
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Close File outFile
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalModelCloseFile()
{
    fclose(outFile);
}
///////////////////////////////////////////////////////////////////////////////////////////////////
void PrintPreObjectReport(FILE *file, ModelViewRead *pModelView, const EntityRead &entity);
///////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalDebugEntitySDIWrite()
{
    bool isOk=false;
    int *includeId = 0;
    int is_dyna = 0;
    GlobalModelSDISelectionNext(&isOk);
    fprintf(debugFile,"////////////////////////////////////////////////////////////////////////////////////////////////////\n");
    fprintf(debugFile,"////////////////////////////  PRE-OBJECT IN MEMORY  ////////////////////////////////////////////////\n");
    fprintf(debugFile,"////////////////////////////////////////////////////////////////////////////////////////////////////\n");
    PrintPreObjectReport(debugFile,g_pModelViewSDI,*g_pEntity);
    fprintf(debugFile,"////////////////////////////////////////////////////////////////////////////////////////////////////\n");
    fprintf(debugFile,"////////////////////////////////////////////////////////////////////////////////////////////////////\n");
    fprintf(debugFile,"////////////////////////////////////////////////////////////////////////////////////////////////////\n");
    fprintf(debugFile,"\n");
    fprintf(debugFile,"\n");
    fprintf(debugFile,"\n");
}
///////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalDebugModelOpenFile(char *fileName,int *s_fileName)
{
    
    char *cname;
    int cname_len;
    int i;
    cname_len = *s_fileName + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*s_fileName;i++)  cname[i] = fileName[i];
    cname[*s_fileName]='\0';
    if (isDebugFile == true) 
    {
        debugFile = fopen(cname,"a");
    }
    else if (isDebugFile == false) 
    {
        debugFile = fopen(cname,"w");
        isDebugFile = true;
    }

    if(cname) free(cname);
}
///////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalDebugModelCloseFile()
{
    fclose(debugFile);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Store an entity Vector
///////////////////////////////////////////////////////////////////////////////////////////////////

void GlobalModelSDISelectionStartList(const sdiString& keyword)
{

    SelectionRead entities(g_pModelViewSDI, keyword);
    unsigned int nbentities = entities.Count();
    g_EntityHandles.reserve(nbentities+1);

    g_EntityHandles.push_back(entities->GetHandle());
    while(entities.Next())
    {
      g_EntityHandles.push_back(entities->GetHandle());
    }
}

void GlobalModelSDISelectionClearList()
{
// std::vector<HandleRead>().swap(g_EntityHandles);
  g_EntityHandles.clear();
}


///////////////////////////////////////////////////////////////////////////////////////////////////
// Position g_pSelection to a given option
///////////////////////////////////////////////////////////////////////////////////////////////////


void GlobalModelPositionSelection(int pos)
{
     if (g_pEntity_single != NULL) delete g_pEntity_single ;
     g_pEntity_single = new EntityRead(g_pModelViewSDI, g_EntityHandles[pos] );
     g_pEntity = g_pEntity_single ;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Select option by Name & Sub_index
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalModelSDISelectOptionByName(const sdiString& keyword,const sdiString& optionName,int *subIndex)
{
    bool isOk = false;
    bool found = false;
    bool pIsOk = true;
    int includeId = 0;
    int id = 0;

    GlobalModelSDISelectionStart(keyword);
    while(pIsOk && !found)
    {
        GlobalModelSDISelectionNext(&pIsOk);
// Get Submodel Id
        GlobalEntitySDIGetSubmodelId(&includeId, &isOk);
// Get option Title
        char buffer[100] = "";
        int size = (int) sizeof(buffer);
        GlobalEntitySDIGetValueString("ParName", buffer, &size, &isOk);
// Option found
        if(!strcmp(buffer, optionName.c_str()) && (*subIndex == includeId) ) found = true;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Set Integer value
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDISetValueInt(const sdiString& dataname, int *pValue, bool *pIsOk)
{
    if(NULL == g_pEntity) { *pIsOk = false; return; }
    // special queries:

    // generic
    *pIsOk = false;
    sdiIdentifier identifier(dataname);

    HandleRead radEntityHRead = g_pEntity->GetHandle();
    HandleEdit radEntityHEdit(radEntityHRead.GetType(), radEntityHRead.GetPointer());
    EntityEdit paramEdit(g_pModelViewSDI, radEntityHEdit);
    const sdiString &keyword = g_pEntity->GetKeyword();
//
    if (keyword.find("PARAMETER") != keyword.npos) 
    {
         if (keyword.find("INTEGER") != keyword.npos) paramEdit.SetValue(identifier, sdiValue(*pValue));
    }
    else
    {
         paramEdit.SetValue(identifier, sdiValue(*pValue));
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Set Double value
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDISetValueDouble(const sdiString& dataname, double *pValue, bool *pIsOk)
{
    if(NULL == g_pEntity) { *pIsOk = false; return; }
    // special queries:

    // generic
    *pIsOk = false;
    sdiIdentifier identifier(dataname);

    HandleRead radEntityHRead = g_pEntity->GetHandle();
    HandleEdit radEntityHEdit(radEntityHRead.GetType(), radEntityHRead.GetPointer());
    EntityEdit paramEdit(g_pModelViewSDI, radEntityHEdit);
    const sdiString &keyword = g_pEntity->GetKeyword();
//
    if (keyword.find("PARAMETER") != keyword.npos) 
    {
         if (keyword.find("REAL") != keyword.npos) paramEdit.SetValue(identifier, sdiValue(*pValue));
    }
    else
    {
         paramEdit.SetValue(identifier, sdiValue(*pValue));
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Create /INTER/TYPE7 & /INTER/TYPE11 & /GRNOD & /LINE from /INTER/TYPE19
// same rules than old TRANSALL routine for /INTER/TYPE19
///////////////////////////////////////////////////////////////////////////////////////////////////

void GlobalEntitySDIConvertInterType19(int *INTER_MAXID,int *GRNOD_MAXID,int *LINE_MAXID,int *OFFSET,int *isFirst)
{           
    bool isOk = false;
    int id = 0;
    int uid = 0;
    int includeId = 0;
    int interId = 0;
    int grnodId = 0;
    int lineId = 0;
    bool found = false;

    EntityType radGrnodSurfType = g_pModelViewSDI->GetEntityType("/GRNOD/SURF");
    EntityType radSurfType = g_pModelViewSDI->GetEntityType("/SURF");
    EntityType radLineSurfType = g_pModelViewSDI->GetEntityType("/LINE/SURF");
    EntityType radSensorType = g_pModelViewSDI->GetEntityType("/SENSOR");
    EntityType radFrictionType = g_pModelViewSDI->GetEntityType("/FRICTION");
    EntityType radNodeType = g_pModelViewSDI->GetEntityType("/NODE");
    EntityType radFunctionType = g_pModelViewSDI->GetEntityType("/FUNCT");
    EntityType radSetType = g_pModelViewSDI->GetEntityType("/SET");

// Get option Id
    GlobalEntitySDIGetId(&id, &isOk);
// Get option Unit Id
    GlobalEntitySDIGetUnitId(&uid, &isOk);
// Get option Title
    char buffer[100] = "";
    int size = (int) sizeof(buffer);
    GlobalEntitySDIGetValueString("name", buffer, &size, &isOk);
// 
     interId = *INTER_MAXID - *OFFSET;
     grnodId = *GRNOD_MAXID - *OFFSET + 1;
     lineId = *LINE_MAXID - *OFFSET + 1;
// 
     HandleRead include = g_pEntity->GetInclude();
     g_pModelViewSDI->SetCurrentCollector(include);
/////////////////////////////////////////////////////////////////////////////////////
// Read /INTER/TYPE19 ATTRIBUTES
/////////////////////////////////////////////////////////////////////////////////////
//
/////////////////////////////////////////////////////////////////////////////////////
// Line1
//# surf_IDs  surf_IDm      Istf      Ithe      Igap     Iedge      Ibag      Idel     Icurv
/////////////////////////////////////////////////////////////////////////////////////
    int secondaryentityids=0;
    int mainentityids=0;
    int type7_Istf=0;
    int Ithe=0;
    int Igap=0;
    int Iedge_Type19=0;
    int Ibag=0;
    int Idel7=0;
    int Icurv=0;
    int ID_TYPE19 = id + *OFFSET;
//
    sdiString *p_objtype_str = NULL;
    GlobalEntitySDIGetValueInt("secondaryentityids", &secondaryentityids, &found, 0, p_objtype_str, 0);
    GlobalEntitySDIGetValueInt("mainentityids", &mainentityids, &found, 0, p_objtype_str, 0);
    
    bool isMainSet = false;
    bool isSecondarySet = false;
    HandleRead mainSetHRead;
    HandleRead secondarySetHRead;
    if (mainentityids > 0 )
        isMainSet = g_pModelViewSDI->FindById("/SET", mainentityids, mainSetHRead);
    if (secondaryentityids > 0 )
        isSecondarySet = g_pModelViewSDI->FindById("/SET", secondaryentityids, secondarySetHRead);

//
/////////////////////////////////////////////////////////////////////////////////////
//    ------   If one of the 2 surface is empty then type7 is symetric ------
/////////////////////////////////////////////////////////////////////////////////////
    if( (secondaryentityids == 0) && (mainentityids != 0))
    {
         secondaryentityids = mainentityids;
         isSecondarySet = isMainSet;
    }
    else if( (secondaryentityids != 0) && (mainentityids == 0))
    {
         mainentityids = secondaryentityids;
         isMainSet = isSecondarySet;
    }
/////////////////////////////////////////////////////////////////////////////////////
// Line1

/////////////////////////////////////////////////////////////////////////////////////
    sdiValue tempValInt(type7_Istf);
    found =  g_pEntity->GetValue(sdiIdentifier("type7_Istf"), tempValInt);
    if(found) tempValInt.GetValue(type7_Istf);
//
    found =  g_pEntity->GetValue(sdiIdentifier("Ithe"), tempValInt);
    if(found) tempValInt.GetValue(Ithe);
//
    found =  g_pEntity->GetValue(sdiIdentifier("Igap"), tempValInt);
    if(found) tempValInt.GetValue(Igap);
//
//    found =  g_pEntity->GetValue(sdiIdentifier("Iedge_Type19"), tempValInt);
//    if(found) tempValInt.GetValue(Iedge_Type19);
//
    found =  g_pEntity->GetValue(sdiIdentifier("Ibag"), tempValInt);
    if(found) tempValInt.GetValue(Ibag);
//
    found =  g_pEntity->GetValue(sdiIdentifier("Idel7"), tempValInt);
    if(found) tempValInt.GetValue(Idel7);
//
    found =  g_pEntity->GetValue(sdiIdentifier("Icurv"), tempValInt);
    if(found) tempValInt.GetValue(Icurv);
/////////////////////////////////////////////////////////////////////////////////////
// Line2
//#          Fscalegap             Gap_max
/////////////////////////////////////////////////////////////////////////////////////
    double GAPSCALE=0;
    double GAPMAX=0;
    double EDGE_SCALE_GAP=0;
//
    sdiValue tempValFloat(GAPSCALE);
    found =  g_pEntity->GetValue(sdiIdentifier("GAPSCALE"), tempValFloat);
    if(found) tempValFloat.GetValue(GAPSCALE);
//
    found =  g_pEntity->GetValue(sdiIdentifier("GAPMAX"), tempValFloat);
    if(found) tempValFloat.GetValue(GAPMAX);
//
    found =  g_pEntity->GetValue(sdiIdentifier("EDGE_SCALE_GAP"), tempValFloat);
    if(found) tempValFloat.GetValue(EDGE_SCALE_GAP);
/////////////////////////////////////////////////////////////////////////////////////
// Line3
//#              Stmin               Stmax          %mesh_size               dtmin  Irem_gap   Irem_i2
/////////////////////////////////////////////////////////////////////////////////////
    double STMIN=0;
    double STMAX=0;
    double PrMesh_Size=0;
    double Tmin=0;
    int Irem_Gap=0;
    int Irem_i2=0;
//
    found =  g_pEntity->GetValue(sdiIdentifier("STMIN"), tempValFloat);
    if(found) tempValFloat.GetValue(STMIN);
//
    found =  g_pEntity->GetValue(sdiIdentifier("STMAX"), tempValFloat);
    if(found) tempValFloat.GetValue(STMAX);
//
    found =  g_pEntity->GetValue(sdiIdentifier("PrMesh_Size"), tempValFloat);
    if(found) tempValFloat.GetValue(PrMesh_Size);
//
    found =  g_pEntity->GetValue(sdiIdentifier("Tmin"), tempValFloat);
    if(found) tempValFloat.GetValue(Tmin);
//
    found =  g_pEntity->GetValue(sdiIdentifier("Irem_Gap"), tempValInt);
    if(found) tempValInt.GetValue(Irem_Gap);
//
    found =  g_pEntity->GetValue(sdiIdentifier("Irem_i2"), tempValInt);
    if(found) tempValInt.GetValue(Irem_i2);
/////////////////////////////////////////////////////////////////////////////////////
// Line4 Optional if (Icurv==1 || Icurv==2)
//# node_ID1  node_ID2
/////////////////////////////////////////////////////////////////////////////////////
    int TYPE7_N1=0;
    int TYPE7_N2=0;
//
    GlobalEntitySDIGetValueInt("TYPE7_N1", &TYPE7_N1, &found, 0, p_objtype_str, 0);
    GlobalEntitySDIGetValueInt("TYPE7_N2", &TYPE7_N2, &found, 0, p_objtype_str, 0);
/////////////////////////////////////////////////////////////////////////////////////
// Line4 
//#              Stfac                Fric              Gapmin              Tstart               Tstop
/////////////////////////////////////////////////////////////////////////////////////
    double TYPE7_SCALE=0;
    double FRIC=0;
    double GAP=0;
    double TSTART=0;
    double TSTOP=0;
//
    found =  g_pEntity->GetValue(sdiIdentifier("TYPE7_SCALE"), tempValFloat);
    if(found) tempValFloat.GetValue(TYPE7_SCALE);
//
    found =  g_pEntity->GetValue(sdiIdentifier("FRIC"), tempValFloat);
    if(found) tempValFloat.GetValue(FRIC);
//
    found =  g_pEntity->GetValue(sdiIdentifier("GAP"), tempValFloat);
    if(found) tempValFloat.GetValue(GAP);
//
    found =  g_pEntity->GetValue(sdiIdentifier("TSTART"), tempValFloat);
    if(found) tempValFloat.GetValue(TSTART);
//
    found =  g_pEntity->GetValue(sdiIdentifier("TSTOP"), tempValFloat);
    if(found) tempValFloat.GetValue(TSTOP);
/////////////////////////////////////////////////////////////////////////////////////
// Line5 
//#      IBC                        Inacti                VISs                VISf              Bumult
/////////////////////////////////////////////////////////////////////////////////////
    int Deactivate_X_BC=0;
    int Deactivate_Y_BC=0;
    int Deactivate_Z_BC=0;
    int INACTIV=0;
    double STIFF_DC=0;
    double FRIC_DC=0;
    double SORT_FACT=0;
//
    found =  g_pEntity->GetValue(sdiIdentifier("Deactivate_X_BC"), tempValInt);
    if(found) tempValInt.GetValue(Deactivate_X_BC);
//
    found =  g_pEntity->GetValue(sdiIdentifier("Deactivate_Y_BC"), tempValInt);
    if(found) tempValInt.GetValue(Deactivate_Y_BC);
//
    found =  g_pEntity->GetValue(sdiIdentifier("Deactivate_Z_BC"), tempValInt);
    if(found) tempValInt.GetValue(Deactivate_Z_BC);
//
    found =  g_pEntity->GetValue(sdiIdentifier("INACTIV"), tempValInt);
    if(found) tempValInt.GetValue(INACTIV);
//
    found =  g_pEntity->GetValue(sdiIdentifier("STIFF_DC"), tempValFloat);
    if(found) tempValFloat.GetValue(STIFF_DC);
//
    found =  g_pEntity->GetValue(sdiIdentifier("FRIC_DC"), tempValFloat);
    if(found) tempValFloat.GetValue(FRIC_DC);
//
    found =  g_pEntity->GetValue(sdiIdentifier("SORT_FACT"), tempValFloat);
    if(found) tempValFloat.GetValue(SORT_FACT);
/////////////////////////////////////////////////////////////////////////////////////
// Line6
//#    Ifric    Ifiltr               Xfreq     Iform   sens_ID                                 fric_ID
/////////////////////////////////////////////////////////////////////////////////////
    int Ifric=0;
    int Ifiltr=0;
    double Xfreq=0;
    int IFORM=0;
    int ISENSOR=0;
    int Fric_ID=0;
//
    found =  g_pEntity->GetValue(sdiIdentifier("Ifric"), tempValInt);
    if(found) tempValInt.GetValue(Ifric);
//
    found =  g_pEntity->GetValue(sdiIdentifier("Ifiltr"), tempValInt);
    if(found) tempValInt.GetValue(Ifiltr);
//
    found =  g_pEntity->GetValue(sdiIdentifier("Xfreq"), tempValFloat);
    if(found) tempValFloat.GetValue(Xfreq);
//
    found =  g_pEntity->GetValue(sdiIdentifier("IFORM"), tempValInt);
    if(found) tempValInt.GetValue(IFORM);
//
//    found =  g_pEntity->GetValue(sdiIdentifier("ISENSOR"), tempValInt);
//    if(found) tempValInt.GetValue(ISENSOR);

    GlobalEntitySDIGetValueInt("ISENSOR", &ISENSOR, &found, 0, p_objtype_str, 0);
//
//    found =  g_pEntity->GetValue(sdiIdentifier("Fric_ID"), tempValInt);
//    if(found) tempValInt.GetValue(Fric_ID);
    GlobalEntitySDIGetValueInt("Fric_ID", &Fric_ID, &found, 0, p_objtype_str, 0);
/////////////////////////////////////////////////////////////////////////////////////
// Line7 (optionnal) if (Ifric > 0)
//#                 C1                  C2                  C3                  C4                  C5
/////////////////////////////////////////////////////////////////////////////////////
    double C1=0;
    double C2=0;
    double C3=0; 
    double C4=0; 
    double C5=0;
//
    found =  g_pEntity->GetValue(sdiIdentifier("C1"), tempValFloat);
    if(found) tempValFloat.GetValue(C1);
//
    found =  g_pEntity->GetValue(sdiIdentifier("C2"), tempValFloat);
    if(found) tempValFloat.GetValue(C2);
//
    found =  g_pEntity->GetValue(sdiIdentifier("C3"), tempValFloat);
    if(found) tempValFloat.GetValue(C3);
//
    found =  g_pEntity->GetValue(sdiIdentifier("C4"), tempValFloat);
    if(found) tempValFloat.GetValue(C4);
//
    found =  g_pEntity->GetValue(sdiIdentifier("C5"), tempValFloat);
    if(found) tempValFloat.GetValue(C5);
/////////////////////////////////////////////////////////////////////////////////////
// Line7 (optionnal) if ((Ifric > 1)
//#                 C6
/////////////////////////////////////////////////////////////////////////////////////
    double C6=0;
//
    found =  g_pEntity->GetValue(sdiIdentifier("C6"), tempValFloat);
    if(found) tempValFloat.GetValue(C6);
/////////////////////////////////////////////////////////////////////////////////////
// Line8 (optionnal) if ((Ithe==1)
//#               Kthe   fct_IDK                          Tint Ithe_form             AscaleK
/////////////////////////////////////////////////////////////////////////////////////
    double Kthe=0;
    int fct_ID_k=0;
    double T_Initial=0;
    int IFORM1=0; 
    double Crx=0;
//
    found =  g_pEntity->GetValue(sdiIdentifier("Kthe"), tempValFloat);
    if(found) tempValFloat.GetValue(Kthe);
//
//    found =  g_pEntity->GetValue(sdiIdentifier("fct_ID_k"), tempValInt);
//    if(found) tempValInt.GetValue(fct_ID_k);
    GlobalEntitySDIGetValueInt("fct_ID_k", &fct_ID_k, &found, 0, p_objtype_str, 0);
//
    found =  g_pEntity->GetValue(sdiIdentifier("T_Initial"), tempValFloat);
    if(found) tempValFloat.GetValue(T_Initial);
//
    found =  g_pEntity->GetValue(sdiIdentifier("IFORM1"), tempValInt);
    if(found) tempValInt.GetValue(IFORM1);
//
    found =  g_pEntity->GetValue(sdiIdentifier("Crx"), tempValFloat);
    if(found) tempValFloat.GetValue(Crx);
/////////////////////////////////////////////////////////////////////////////////////
// Line9 (optionnal) if ((Ithe==1)
//#               Frad                Drad              Fheats              Fheatm 
/////////////////////////////////////////////////////////////////////////////////////
    double F_RAD=0;
    double D_RAD=0;
    double Fmax=0;
    double HEAT_AL=0; 
//
    found =  g_pEntity->GetValue(sdiIdentifier("F_RAD"), tempValFloat);
    if(found) tempValFloat.GetValue(F_RAD);
//
    found =  g_pEntity->GetValue(sdiIdentifier("D_RAD"), tempValFloat);
    if(found) tempValFloat.GetValue(D_RAD);
//
    found =  g_pEntity->GetValue(sdiIdentifier("Fmax"), tempValFloat);
    if(found) tempValFloat.GetValue(Fmax);
//
    found =  g_pEntity->GetValue(sdiIdentifier("HEAT_AL"), tempValFloat);
    if(found) tempValFloat.GetValue(HEAT_AL);
//
/////////////////////////////////////////////////////////////////////////////////////
// Create /INTER/TYPE7
/////////////////////////////////////////////////////////////////////////////////////
    HandleEdit radInterHEdit;
    int interId7 = interId;
    
    if(*isFirst == 1) 
    {
        ID_TYPE19 = -1;
        *isFirst = 0;
        interId7 = id;
    }
    g_pModelViewSDI->CreateEntity(radInterHEdit, "/INTER/TYPE7", buffer ,interId7);
    EntityEdit radInterType7Edit(g_pModelViewSDI, radInterHEdit);
    radInterType7Edit.SetValue(sdiIdentifier("unitid"), sdiValue(uid));
/////////////////////////////////////////////////////////////////////////////////////
// Create /INTER/TYPE7 sym
/////////////////////////////////////////////////////////////////////////////////////
    int interId7_sym = 0;
    if( secondaryentityids !=  mainentityids)
    {
        interId = interId + 1;
        interId7_sym = interId;
        g_pModelViewSDI->CreateEntity(radInterHEdit, "/INTER/TYPE7", buffer ,interId7_sym);
    }
    EntityEdit radInterType7SymEdit(g_pModelViewSDI, radInterHEdit);
    if( secondaryentityids !=  mainentityids)
    {
        radInterType7SymEdit.SetValue(sdiIdentifier("unitid"), sdiValue(uid));
    }
/////////////////////////////////////////////////////////////////////////////////////
// Create /INTER/TYPE11
/////////////////////////////////////////////////////////////////////////////////////
    interId = interId + 1;
    int interId11 = interId;
    g_pModelViewSDI->CreateEntity(radInterHEdit, "/INTER/TYPE11", buffer ,interId11);
    EntityEdit radInterType11Edit(g_pModelViewSDI, radInterHEdit);
    radInterType11Edit.SetValue(sdiIdentifier("unitid"), sdiValue(uid));
//
/////////////////////////////////////////////////////////////////////////////////////
// Create /GRNOD/SURF
/////////////////////////////////////////////////////////////////////////////////////
    int grnodId1 = grnodId;
    int grnodId2 = 0;
    
     int lineId1 = lineId;
     int lineId2 = lineId;
     
    if(!isSecondarySet)
    {
        HandleEdit radGrnod1HEdit;
        g_pModelViewSDI->CreateEntity(radGrnod1HEdit, "/GRNOD/SURF", 
          "Type19 interface nb " + std::to_string(id) + 
          " Automatically generated node group from surface " + std::to_string(secondaryentityids) ,
          grnodId1);
        EntityEdit radGrnodSurf1Edit(g_pModelViewSDI, radGrnod1HEdit);
        radGrnodSurf1Edit.SetValue(sdiIdentifier("unitid"), sdiValue(uid));
//
// Create optional/GRNOD/SURF
//
        HandleEdit radGrnod2HEdit;
        if( secondaryentityids !=  mainentityids)
        {
            grnodId = grnodId + 1;
            grnodId2 = grnodId;
            g_pModelViewSDI->CreateEntity(radGrnod2HEdit, "/GRNOD/SURF", 
             "Type19 interface nb " + std::to_string(id) + 
             " Automatically generated node group from surface " + std::to_string(secondaryentityids) ,
             grnodId2);
        }
        EntityEdit radGrnodSurf2Edit(g_pModelViewSDI, radGrnod2HEdit);
        if( secondaryentityids !=  mainentityids)
        {
            radGrnodSurf2Edit.SetValue(sdiIdentifier("unitid"), sdiValue(uid));
        }
//
// Fill /GRNOD/SURF values
//
        radGrnodSurf1Edit.SetValue(sdiIdentifier("idsmax"), sdiValue(1));
        radGrnodSurf1Edit.SetValue(sdiIdentifier("ids",0), sdiValue(sdiValueEntity(radSurfType,secondaryentityids)));
//
// Fill /GRNOD/SURF for sym /INTER/TYPE7 values
//
        if( secondaryentityids !=  mainentityids)
        {
            radGrnodSurf2Edit.SetValue(sdiIdentifier("idsmax"), sdiValue(1));
            radGrnodSurf2Edit.SetValue(sdiIdentifier("ids",0), sdiValue(sdiValueEntity(radSurfType,mainentityids)));
        }
/////////////////////////////////////////////////////////////////////////////////////
// Create /LINE/SURF
/////////////////////////////////////////////////////////////////////////////////////
         HandleEdit radLineSurf1HEdit;
         g_pModelViewSDI->CreateEntity(radLineSurf1HEdit, "/LINE/SURF", 
              "Type19 interface nb " + std::to_string(id) + 
              " Automatically generated ext. edges line from surface " + std::to_string(secondaryentityids) ,
              lineId);
         EntityEdit radLineSurf1Edit(g_pModelViewSDI, radLineSurf1HEdit);
         radLineSurf1Edit.SetValue(sdiIdentifier("unitid"), sdiValue(uid));
/////////////////////////////////////////////////////////////////////////////////////
// Create /LINE/EDGE
/////////////////////////////////////////////////////////////////////////////////////
         lineId = lineId + 1;
         int lineId1b = lineId;
         HandleEdit radLineEdge1HEdit;
         g_pModelViewSDI->CreateEntity(radLineEdge1HEdit, "/LINE/EDGE", 
              "Type19 interface nb " + std::to_string(id) + 
              " Automatically generated line from surface " + std::to_string(secondaryentityids) ,
              lineId);
         EntityEdit radLineEdge1Edit(g_pModelViewSDI, radLineEdge1HEdit);
         radLineEdge1Edit.SetValue(sdiIdentifier("unitid"), sdiValue(uid));
/////////////////////////////////////////////////////////////////////////////////////
// Create opionnal /LINE/SURF
/////////////////////////////////////////////////////////////////////////////////////
        HandleEdit radLineSurf2HEdit;
        if( secondaryentityids !=  mainentityids)
        {
             lineId = lineId + 1;
             lineId2 = lineId;
             g_pModelViewSDI->CreateEntity(radLineSurf2HEdit, "/LINE/SURF", 
              "Type19 interface nb " + std::to_string(id) + 
              " Automatically generated ext. edges line from surface " + std::to_string(secondaryentityids) ,
             lineId);
        }
        EntityEdit radLineSurf2Edit(g_pModelViewSDI, radLineSurf2HEdit);
        if( secondaryentityids !=  mainentityids)
        {
             radLineSurf2Edit.SetValue(sdiIdentifier("unitid"), sdiValue(uid));
        }
/////////////////////////////////////////////////////////////////////////////////////
// Create optionnal/LINE/EDGE
/////////////////////////////////////////////////////////////////////////////////////
        HandleEdit radLineEdge2HEdit;
        int lineId2b;
        if( secondaryentityids !=  mainentityids)
        {
             lineId = lineId + 1; 
             lineId2b = lineId;
             g_pModelViewSDI->CreateEntity(radLineEdge2HEdit, "/LINE/EDGE",  
              "Type19 interface nb " + std::to_string(id) + 
              " Automatically generated line from surface " + std::to_string(secondaryentityids) ,
             lineId);
        }
         EntityEdit radLineEdge2Edit(g_pModelViewSDI, radLineEdge2HEdit);
        if( secondaryentityids !=  mainentityids)
        {
         radLineEdge2Edit.SetValue(sdiIdentifier("unitid"), sdiValue(uid));
        }
    
//
// Fill /LINE/SURF values
//
        radLineSurf1Edit.SetValue(sdiIdentifier("idsmax"), sdiValue(1));
        radLineSurf1Edit.SetValue(sdiIdentifier("ids",0), sdiValue(sdiValueEntity(radSurfType,secondaryentityids)));
//
// Fill /LINE/EDGE
//
//
        radLineEdge1Edit.SetValue(sdiIdentifier("idsmax"), sdiValue(1));
        radLineEdge1Edit.SetValue(sdiIdentifier("ids",0), sdiValue(sdiValueEntity(radSurfType,secondaryentityids)));
// Fill /LINE/SURF values if secondaryentityids !=  mainentityids
//
        if( secondaryentityids !=  mainentityids && !isMainSet)
        {
            radLineSurf2Edit.SetValue(sdiIdentifier("idsmax"), sdiValue(1));
            radLineSurf2Edit.SetValue(sdiIdentifier("ids",0), sdiValue(sdiValueEntity(radSurfType,mainentityids)));
        }
//
// Fill /LINE/EDGE values if secondaryentityids !=  mainentityids
//
        if( secondaryentityids !=  mainentityids)
        {
            radLineEdge2Edit.SetValue(sdiIdentifier("idsmax"), sdiValue(1));
            radLineEdge2Edit.SetValue(sdiIdentifier("ids",0), sdiValue(sdiValueEntity(radSurfType,mainentityids)));
        }

    }
/////////////////////////////////////////////////////////////////////////////////////



/////////////////////////////////////////////////////////////////////////////////////
// Fill /INTER/TYPE7 values
/////////////////////////////////////////////////////////////////////////////////////
    radInterType7Edit.SetValue(sdiIdentifier("IEDGE_TYPE19"), sdiValue(Iedge_Type19));
    if(isSecondarySet)
    {
        radInterType7Edit.SetValue(sdiIdentifier("secondaryentityids",0), sdiValue(sdiValueEntity(radSetType,secondaryentityids)));
    }
    else
    {
        radInterType7Edit.SetValue(sdiIdentifier("secondaryentityids",0), sdiValue(sdiValueEntity(radGrnodSurfType,grnodId1)));
    }
    radInterType7Edit.SetValue(sdiIdentifier("mainentityids",0), sdiValue(sdiValueEntity(radSurfType,mainentityids)));
    radInterType7Edit.SetValue(sdiIdentifier("type7_Istf"), sdiValue(type7_Istf));
    radInterType7Edit.SetValue(sdiIdentifier("I_TH"), sdiValue(Ithe));
    radInterType7Edit.SetValue(sdiIdentifier("Igap"), sdiValue(Igap));
    radInterType7Edit.SetValue(sdiIdentifier("Ibag"), sdiValue(Ibag));
    radInterType7Edit.SetValue(sdiIdentifier("Idel7"), sdiValue(Idel7));
    radInterType7Edit.SetValue(sdiIdentifier("Icurv"), sdiValue(Icurv));
//
    radInterType7Edit.SetValue(sdiIdentifier("GAPSCALE"), sdiValue(GAPSCALE));
    radInterType7Edit.SetValue(sdiIdentifier("GAPMAX"), sdiValue(GAPMAX));
//
    radInterType7Edit.SetValue(sdiIdentifier("STMIN"), sdiValue(STMIN));
    radInterType7Edit.SetValue(sdiIdentifier("STMAX"), sdiValue(STMAX));
    radInterType7Edit.SetValue(sdiIdentifier("PrMesh_Size"), sdiValue(PrMesh_Size));
    radInterType7Edit.SetValue(sdiIdentifier("Tmin"), sdiValue(Tmin));
    radInterType7Edit.SetValue(sdiIdentifier("IKREM"), sdiValue(Irem_Gap));
    radInterType7Edit.SetValue(sdiIdentifier("ICOG"), sdiValue(Irem_i2));
//
    if (Icurv==1 || Icurv==2)
    {
        radInterType7Edit.SetValue(sdiIdentifier("TYPE7_N1"), sdiValue(sdiValueEntity(radNodeType,TYPE7_N1)));
        radInterType7Edit.SetValue(sdiIdentifier("TYPE7_N2"), sdiValue(sdiValueEntity(radNodeType,TYPE7_N2)));
    }
//
    radInterType7Edit.SetValue(sdiIdentifier("TYPE7_SCALE"), sdiValue(TYPE7_SCALE));
    radInterType7Edit.SetValue(sdiIdentifier("FRIC"), sdiValue(FRIC));
    radInterType7Edit.SetValue(sdiIdentifier("GAP"), sdiValue(GAP));
    radInterType7Edit.SetValue(sdiIdentifier("EDGE_SCALE_GAP"), sdiValue(EDGE_SCALE_GAP));
    radInterType7Edit.SetValue(sdiIdentifier("TSTART"), sdiValue(TSTART));
    radInterType7Edit.SetValue(sdiIdentifier("TSTOP"), sdiValue(TSTOP));
//
    radInterType7Edit.SetValue(sdiIdentifier("Deactivate_X_BC"), sdiValue(Deactivate_X_BC));
    radInterType7Edit.SetValue(sdiIdentifier("Deactivate_Y_BC"), sdiValue(Deactivate_Y_BC));
    radInterType7Edit.SetValue(sdiIdentifier("Deactivate_Z_BC"), sdiValue(Deactivate_Z_BC));
    radInterType7Edit.SetValue(sdiIdentifier("ID_TYPE19"), sdiValue(ID_TYPE19));
    radInterType7Edit.SetValue(sdiIdentifier("INACTIV"), sdiValue(INACTIV));
    radInterType7Edit.SetValue(sdiIdentifier("STIFF_DC"), sdiValue(STIFF_DC));
    radInterType7Edit.SetValue(sdiIdentifier("FRIC_DC"), sdiValue(FRIC_DC));
    radInterType7Edit.SetValue(sdiIdentifier("SORT_FACT"), sdiValue(SORT_FACT));
//
    radInterType7Edit.SetValue(sdiIdentifier("Ifric"), sdiValue(Ifric));
    radInterType7Edit.SetValue(sdiIdentifier("Ifiltr"), sdiValue(Ifiltr));
    radInterType7Edit.SetValue(sdiIdentifier("Xfreq"), sdiValue(Xfreq));
    radInterType7Edit.SetValue(sdiIdentifier("IFORM"), sdiValue(IFORM));
    //radInterType7Edit.SetValue(sdiIdentifier("ISENSOR"), sdiValue(ISENSOR));
    //radInterType7Edit.SetValue(sdiIdentifier("Fric_ID"), sdiValue(Fric_ID));
    radInterType7Edit.SetValue(sdiIdentifier("ISENSOR",0), sdiValue(sdiValueEntity(radSensorType,ISENSOR)));
    radInterType7Edit.SetValue(sdiIdentifier("Fric_ID",0), sdiValue(sdiValueEntity(radFrictionType,Fric_ID)));
//
    if (Ifric > 0)
    {
        radInterType7Edit.SetValue(sdiIdentifier("C1"), sdiValue(C1));
        radInterType7Edit.SetValue(sdiIdentifier("C2"), sdiValue(C2));
        radInterType7Edit.SetValue(sdiIdentifier("C3"), sdiValue(C3));
        radInterType7Edit.SetValue(sdiIdentifier("C4"), sdiValue(C4));
        radInterType7Edit.SetValue(sdiIdentifier("C5"), sdiValue(C5));
    }
//
    if (Ifric > 1)
    {
        radInterType7Edit.SetValue(sdiIdentifier("C6"), sdiValue(C6));
    }
//
    if (Ithe == 1)
    {
        radInterType7Edit.SetValue(sdiIdentifier("R_TH"), sdiValue(Kthe));
        //radInterType7Edit.SetValue(sdiIdentifier("fct_ID_k"), sdiValue(fct_ID_k));
        radInterType7Edit.SetValue(sdiIdentifier("fct_ID_k",0), sdiValue(sdiValueEntity(radFunctionType,fct_ID_k)));
        radInterType7Edit.SetValue(sdiIdentifier("T_Initial"), sdiValue(T_Initial));
        radInterType7Edit.SetValue(sdiIdentifier("IFORM1"), sdiValue(IFORM1));
        radInterType7Edit.SetValue(sdiIdentifier("Crx"), sdiValue(Crx));
    }
    if (Ithe == 1)
    {
        radInterType7Edit.SetValue(sdiIdentifier("F_RAD"), sdiValue(F_RAD));
        radInterType7Edit.SetValue(sdiIdentifier("D_RAD"), sdiValue(D_RAD));
        radInterType7Edit.SetValue(sdiIdentifier("Fmax"), sdiValue(Fmax));
        radInterType7Edit.SetValue(sdiIdentifier("HEAT_AL"), sdiValue(HEAT_AL));
    }
/////////////////////////////////////////////////////////////////////////////////////
// Fill /INTER/TYPE7_sym values
/////////////////////////////////////////////////////////////////////////////////////
    ID_TYPE19 = id + *OFFSET;
    if( secondaryentityids !=  mainentityids)
    {
        radInterType7SymEdit.SetValue(sdiIdentifier("IEDGE_TYPE19"), sdiValue(Iedge_Type19));
        if(isMainSet)
        {
            radInterType7SymEdit.SetValue(sdiIdentifier("secondaryentityids",0), sdiValue(sdiValueEntity(radSetType,mainentityids)));
        }
        else
        {
            radInterType7SymEdit.SetValue(sdiIdentifier("secondaryentityids",0), sdiValue(sdiValueEntity(radGrnodSurfType,grnodId2)));
        }
        radInterType7SymEdit.SetValue(sdiIdentifier("mainentityids",0), sdiValue(sdiValueEntity(radSurfType,secondaryentityids)));
        radInterType7SymEdit.SetValue(sdiIdentifier("type7_Istf"), sdiValue(type7_Istf));
        radInterType7SymEdit.SetValue(sdiIdentifier("I_TH"), sdiValue(Ithe));
        radInterType7SymEdit.SetValue(sdiIdentifier("Igap"), sdiValue(Igap));
        radInterType7SymEdit.SetValue(sdiIdentifier("Ibag"), sdiValue(Ibag));
        radInterType7SymEdit.SetValue(sdiIdentifier("Idel7"), sdiValue(Idel7));
        radInterType7SymEdit.SetValue(sdiIdentifier("Icurv"), sdiValue(Icurv));
//
        radInterType7SymEdit.SetValue(sdiIdentifier("GAPSCALE"), sdiValue(GAPSCALE));
        radInterType7SymEdit.SetValue(sdiIdentifier("GAPMAX"), sdiValue(GAPMAX));
//
        radInterType7SymEdit.SetValue(sdiIdentifier("STMIN"), sdiValue(STMIN));
        radInterType7SymEdit.SetValue(sdiIdentifier("STMAX"), sdiValue(STMAX));
        radInterType7SymEdit.SetValue(sdiIdentifier("PrMesh_Size"), sdiValue(PrMesh_Size));
        radInterType7SymEdit.SetValue(sdiIdentifier("Tmin"), sdiValue(Tmin));
        radInterType7SymEdit.SetValue(sdiIdentifier("IKREM"), sdiValue(Irem_Gap));
        radInterType7SymEdit.SetValue(sdiIdentifier("ICOG"), sdiValue(Irem_i2));
//
        if (Icurv==1 || Icurv==2)
        {
            radInterType7SymEdit.SetValue(sdiIdentifier("TYPE7_N1"), sdiValue(sdiValueEntity(radNodeType,TYPE7_N1)));
            radInterType7SymEdit.SetValue(sdiIdentifier("TYPE7_N2"), sdiValue(sdiValueEntity(radNodeType,TYPE7_N2)));
        }
//
        radInterType7SymEdit.SetValue(sdiIdentifier("TYPE7_SCALE"), sdiValue(TYPE7_SCALE));
        radInterType7SymEdit.SetValue(sdiIdentifier("FRIC"), sdiValue(FRIC));
        radInterType7SymEdit.SetValue(sdiIdentifier("GAP"), sdiValue(GAP));
        radInterType7SymEdit.SetValue(sdiIdentifier("EDGE_SCALE_GAP"), sdiValue(EDGE_SCALE_GAP));
        radInterType7SymEdit.SetValue(sdiIdentifier("TSTART"), sdiValue(TSTART));
        radInterType7SymEdit.SetValue(sdiIdentifier("TSTOP"), sdiValue(TSTOP));
//
        radInterType7SymEdit.SetValue(sdiIdentifier("Deactivate_X_BC"), sdiValue(Deactivate_X_BC));
        radInterType7SymEdit.SetValue(sdiIdentifier("Deactivate_Y_BC"), sdiValue(Deactivate_Y_BC));
        radInterType7SymEdit.SetValue(sdiIdentifier("Deactivate_Z_BC"), sdiValue(Deactivate_Z_BC));
        radInterType7SymEdit.SetValue(sdiIdentifier("ID_TYPE19"), sdiValue(ID_TYPE19));
        radInterType7SymEdit.SetValue(sdiIdentifier("INACTIV"), sdiValue(INACTIV));
        radInterType7SymEdit.SetValue(sdiIdentifier("STIFF_DC"), sdiValue(STIFF_DC));
        radInterType7SymEdit.SetValue(sdiIdentifier("FRIC_DC"), sdiValue(FRIC_DC));
        radInterType7SymEdit.SetValue(sdiIdentifier("SORT_FACT"), sdiValue(SORT_FACT));
//
        radInterType7SymEdit.SetValue(sdiIdentifier("Ifric"), sdiValue(Ifric));
        radInterType7SymEdit.SetValue(sdiIdentifier("Ifiltr"), sdiValue(Ifiltr));
        radInterType7SymEdit.SetValue(sdiIdentifier("Xfreq"), sdiValue(Xfreq));
        radInterType7SymEdit.SetValue(sdiIdentifier("IFORM"), sdiValue(IFORM));
        //radInterType7SymEdit.SetValue(sdiIdentifier("ISENSOR"), sdiValue(ISENSOR));
        //radInterType7SymEdit.SetValue(sdiIdentifier("Fric_ID"), sdiValue(Fric_ID));
        radInterType7SymEdit.SetValue(sdiIdentifier("ISENSOR",0), sdiValue(sdiValueEntity(radSensorType,ISENSOR)));
        radInterType7SymEdit.SetValue(sdiIdentifier("Fric_ID",0), sdiValue(sdiValueEntity(radFrictionType,Fric_ID)));
//
        if (Ifric > 0)
        {
            radInterType7SymEdit.SetValue(sdiIdentifier("C1"), sdiValue(C1));
            radInterType7SymEdit.SetValue(sdiIdentifier("C2"), sdiValue(C2));
            radInterType7SymEdit.SetValue(sdiIdentifier("C3"), sdiValue(C3));
            radInterType7SymEdit.SetValue(sdiIdentifier("C4"), sdiValue(C4));
            radInterType7SymEdit.SetValue(sdiIdentifier("C5"), sdiValue(C5));
        }
// 
        if (Ifric > 1)
        {
            radInterType7SymEdit.SetValue(sdiIdentifier("C6"), sdiValue(C6));
        }
//
        if (Ithe == 1)
        {
            radInterType7SymEdit.SetValue(sdiIdentifier("R_TH"), sdiValue(Kthe));
            //radInterType7SymEdit.SetValue(sdiIdentifier("fct_ID_k"), sdiValue(fct_ID_k));
            radInterType7SymEdit.SetValue(sdiIdentifier("fct_ID_k",0), sdiValue(sdiValueEntity(radFunctionType,fct_ID_k)));
            radInterType7SymEdit.SetValue(sdiIdentifier("T_Initial"), sdiValue(T_Initial));
            radInterType7SymEdit.SetValue(sdiIdentifier("IFORM1"), sdiValue(IFORM1));
            radInterType7SymEdit.SetValue(sdiIdentifier("Crx"), sdiValue(Crx));
        }
        if (Ithe == 1)
        {
            radInterType7SymEdit.SetValue(sdiIdentifier("F_RAD"), sdiValue(F_RAD));
            radInterType7SymEdit.SetValue(sdiIdentifier("D_RAD"), sdiValue(D_RAD));
            radInterType7SymEdit.SetValue(sdiIdentifier("Fmax"), sdiValue(Fmax));
            radInterType7SymEdit.SetValue(sdiIdentifier("HEAT_AL"), sdiValue(HEAT_AL));
        }
    }

/////////////////////////////////////////////////////////////////////////////////////
// Fill /INTER/TYPE11 values
/////////////////////////////////////////////////////////////////////////////////////
//
    int Igap11 = Igap;
    if(Igap11 == 2) Igap11 = 1;
    radInterType11Edit.SetValue(sdiIdentifier("IEDGE_TYPE19"), sdiValue(Iedge_Type19));
    if(isSecondarySet)
    {
        radInterType11Edit.SetValue(sdiIdentifier("secondaryentityids",0), sdiValue(sdiValueEntity(radSetType,secondaryentityids)));
    }
    else
    {
        radInterType11Edit.SetValue(sdiIdentifier("secondaryentityids",0), sdiValue(sdiValueEntity(radLineSurfType,lineId1)));
    }
    
    if(isMainSet)
    {
        radInterType11Edit.SetValue(sdiIdentifier("mainentityids",0), sdiValue(sdiValueEntity(radSetType,mainentityids)));
    }
    else
    {
        radInterType11Edit.SetValue(sdiIdentifier("mainentityids",0), sdiValue(sdiValueEntity(radLineSurfType,lineId2)));
    }


    radInterType11Edit.SetValue(sdiIdentifier("Istf"), sdiValue(type7_Istf));
    radInterType11Edit.SetValue(sdiIdentifier("Ithe"), sdiValue(Ithe));
    radInterType11Edit.SetValue(sdiIdentifier("Igap"), sdiValue(Igap11));
    radInterType11Edit.SetValue(sdiIdentifier("IKREM"), sdiValue(Irem_Gap));
    radInterType11Edit.SetValue(sdiIdentifier("NodDel11"), sdiValue(Idel7));
//
    radInterType11Edit.SetValue(sdiIdentifier("STMIN"), sdiValue(STMIN));
    radInterType11Edit.SetValue(sdiIdentifier("STMAX"), sdiValue(STMAX));
    radInterType11Edit.SetValue(sdiIdentifier("PrMesh_Size"), sdiValue(PrMesh_Size));
    radInterType11Edit.SetValue(sdiIdentifier("Tmin"), sdiValue(Tmin));
    radInterType11Edit.SetValue(sdiIdentifier("IFORM"), sdiValue(IFORM));
    //radInterType11Edit.SetValue(sdiIdentifier("ISENSOR"), sdiValue(ISENSOR));
    radInterType11Edit.SetValue(sdiIdentifier("ISENSOR",0), sdiValue(sdiValueEntity(radSensorType,ISENSOR)));
//
    radInterType11Edit.SetValue(sdiIdentifier("TYPE11_SCALE"), sdiValue(TYPE7_SCALE));
    radInterType11Edit.SetValue(sdiIdentifier("FRIC"), sdiValue(FRIC));
    radInterType11Edit.SetValue(sdiIdentifier("GAP"), sdiValue(GAP));
    radInterType11Edit.SetValue(sdiIdentifier("EDGE_SCALE_GAP"), sdiValue(EDGE_SCALE_GAP));
    radInterType11Edit.SetValue(sdiIdentifier("TSTART"), sdiValue(TSTART));
    radInterType11Edit.SetValue(sdiIdentifier("TSTOP"), sdiValue(TSTOP));
//
    radInterType11Edit.SetValue(sdiIdentifier("Deactivate_X_BC"), sdiValue(Deactivate_X_BC));
    radInterType11Edit.SetValue(sdiIdentifier("Deactivate_Y_BC"), sdiValue(Deactivate_Y_BC));
    radInterType11Edit.SetValue(sdiIdentifier("Deactivate_Z_BC"), sdiValue(Deactivate_Z_BC));
    radInterType11Edit.SetValue(sdiIdentifier("ID_TYPE19"), sdiValue(ID_TYPE19));
    radInterType11Edit.SetValue(sdiIdentifier("INACTIV"), sdiValue(INACTIV));
    radInterType11Edit.SetValue(sdiIdentifier("STIFF_DC"), sdiValue(STIFF_DC));
    radInterType11Edit.SetValue(sdiIdentifier("FRIC_DC"), sdiValue(FRIC_DC));
    radInterType11Edit.SetValue(sdiIdentifier("SORT_FACT"), sdiValue(SORT_FACT));
//
//    radInterType11Edit.SetValue(sdiIdentifier("fric_ID"), sdiValue(Fric_ID));
    radInterType11Edit.SetValue(sdiIdentifier("Fric_ID",0), sdiValue(sdiValueEntity(radFrictionType,Fric_ID)));
//

    if (Ithe > 0)
    {
        radInterType11Edit.SetValue(sdiIdentifier("Kthe"), sdiValue(Kthe));
        //radInterType11Edit.SetValue(sdiIdentifier("fct_ID_k"), sdiValue(fct_ID_k));
        radInterType11Edit.SetValue(sdiIdentifier("fct_ID_k",0), sdiValue(sdiValueEntity(radFunctionType,fct_ID_k)));
        radInterType11Edit.SetValue(sdiIdentifier("A_scale_k"), sdiValue(Crx));
        radInterType11Edit.SetValue(sdiIdentifier("Tint"), sdiValue(T_Initial));
        radInterType11Edit.SetValue(sdiIdentifier("IFORM1"), sdiValue(IFORM1));
    }

    if (Ithe > 0)
    {
        radInterType11Edit.SetValue(sdiIdentifier("F_RAD"), sdiValue(F_RAD));
        radInterType11Edit.SetValue(sdiIdentifier("D_RAD"), sdiValue(D_RAD));
    }
/////////////////////////////////////////////////////////////////////////////////////


// Update INTER_MAXID value
    *INTER_MAXID = *INTER_MAXID + 2 ;
    if( secondaryentityids !=  mainentityids) *INTER_MAXID = *INTER_MAXID + 1;
    *GRNOD_MAXID = *GRNOD_MAXID + 1;
// optional 
    if( secondaryentityids !=  mainentityids) *GRNOD_MAXID = *GRNOD_MAXID + 1;
    *LINE_MAXID = *LINE_MAXID + 2;
// optional 
    if( secondaryentityids !=  mainentityids) *LINE_MAXID = *LINE_MAXID + 2;

}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Get float array & physical dimension
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIGetArrayDouble(const char *dataname, double *pValue, bool *pIsOk, 
                                double *lengthDim, double *massDim, double *timeDim, int *index)
{
    sdiIdentifier attributeName(dataname, 0, *index-1);
    sdiValue val_Attribute;
    double doublevalue=0;
    g_pEntity->GetValue(attributeName, val_Attribute);

    if(val_Attribute.GetArrayDimension() == 0)
    {
        val_Attribute.GetValue(pValue[0]);
    }
    else if(val_Attribute.GetArrayDimension() == 1)
    {
        unsigned int nbValues = val_Attribute.GetArrayDimensionExtent(1);
        for(unsigned int j = 0; j < nbValues; ++j)
        {
            val_Attribute.GetValue(pValue[j], j);
        }
    }
    else
    {
        assert(0);
    }

    RadiossblkGetDimensions(*g_pEntity, dataname, lengthDim, massDim, timeDim);
    //printf( "Dimension: LENGTH^%f * MASS^%f * TIME^%f\n", lengthDim, massDim, timeDim);

    return;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Create /FAIL/TAB1 & /TABLE from /FAIL/TAB
// same rules than old TRANSALL routine for /FAIL/TAB
///////////////////////////////////////////////////////////////////////////////////////////////////

void GlobalEntitySDIConvertFailTab(int *TABLE_MAXID,int *FAIL_MAXID,int *OFFSET)
{         
    bool isOk = false;
    int id = 0;
    int uid = 0;
    int includeId = 0;
    int tableId = 0;
    int failId = 0;
    int lineId = 0;
    bool found = false;

    EntityType radTableType = g_pModelViewSDI->GetEntityType("/TABLE");
    EntityType radFailTab1Type = g_pModelViewSDI->GetEntityType("/FAIL/TAB1");
    EntityType radFunctType = g_pModelViewSDI->GetEntityType("/FUNCT");
    EntityType radMatType = g_pModelViewSDI->GetEntityType("/MAT");

// Get option Id
    GlobalEntitySDIGetId(&id, &isOk);
// Get option Unit Id
    GlobalEntitySDIGetUnitId(&uid, &isOk);
// Get option Title
    char buffer[100] = "";
    int size = (int) sizeof(buffer);
    GlobalEntitySDIGetValueString("name", buffer, &size, &isOk);
// 
     tableId = *TABLE_MAXID - *OFFSET + 1;
     failId = *FAIL_MAXID - *OFFSET + 1;
// 
     HandleRead include = g_pEntity->GetInclude();
     g_pModelViewSDI->SetCurrentCollector(include);
/////////////////////////////////////////////////////////////////////////////////////
// Read /FAIL/TAB ATTRIBUTES
/////////////////////////////////////////////////////////////////////////////////////
//
    sdiValueEntity entityMat;
    sdiValue tempValEntity(entityMat);
    found =  g_pEntity->GetValue(sdiIdentifier("mat_id"), tempValEntity);
    if(found) tempValEntity.GetValue(entityMat);

/////////////////////////////////////////////////////////////////////////////////////
// Line1
//# IFAIL_SH  IFAIL_SO     NRATE                   P_THICKFAIL                                   IXFEM
/////////////////////////////////////////////////////////////////////////////////////
    int Ifail_sh=0;
    int Ifail_so=0;
    int N_rate=0;
    int Ixfem=0;
    double P_thickfail= 0;
    sdiValue tempValInt(Ifail_sh);
    found =  g_pEntity->GetValue(sdiIdentifier("Ifail_sh"), tempValInt);
    if(found) tempValInt.GetValue(Ifail_sh);
//
    found =  g_pEntity->GetValue(sdiIdentifier("Ifail_so"), tempValInt);
    if(found) tempValInt.GetValue(Ifail_so);
//
    found =  g_pEntity->GetValue(sdiIdentifier("N_rate"), tempValInt);
    if(found) tempValInt.GetValue(N_rate);
//
    found =  g_pEntity->GetValue(sdiIdentifier("Ixfem"), tempValInt);
    if(found) tempValInt.GetValue(Ixfem);
//
    sdiValue tempValFloat(P_thickfail);
    found =  g_pEntity->GetValue(sdiIdentifier("P_thickfail"), tempValFloat);
    if(found) tempValFloat.GetValue(P_thickfail);
/////////////////////////////////////////////////////////////////////////////////////
// Line2
//#              DCRIT                   D                   N                DADV
/////////////////////////////////////////////////////////////////////////////////////
    double Dcrit=0;
    double D=0;
    double n=0;
    double Dadv=0;
//
    found =  g_pEntity->GetValue(sdiIdentifier("Dcrit"), tempValFloat);
    if(found) tempValFloat.GetValue(Dcrit);
//
    found =  g_pEntity->GetValue(sdiIdentifier("D"), tempValFloat);
    if(found) tempValFloat.GetValue(D);
//
    found =  g_pEntity->GetValue(sdiIdentifier("n"), tempValFloat);
    if(found) tempValFloat.GetValue(n);
//
    found =  g_pEntity->GetValue(sdiIdentifier("Dadv"), tempValFloat);
    if(found) tempValFloat.GetValue(Dadv);
/////////////////////////////////////////////////////////////////////////////////////
// Line3
//#   FCT_ID              FSCALE              EPSDOT
//		CARD("%10d%20lg%20lg",fct_ID_TAB,Fscale,Epsdot);
/////////////////////////////////////////////////////////////////////////////////////
    sdiUIntList idList;
    sdiDoubleList Fscale;
    sdiDoubleList Epsdot;
    idList.reserve(N_rate);
    sdiValue tempVal;
    sdiValue tempVal1;
//    for (int j = 0; j < N_rate; ++j)
//    {
        sdiValueEntityList functEntityList;
        tempVal = sdiValue(functEntityList);
        g_pEntity->GetValue(sdiIdentifier("fct_ID_TAB"), tempVal);
        tempVal.GetValue(functEntityList);
        functEntityList.GetIdList(idList);

        tempVal1 = sdiValue(Fscale);
        g_pEntity->GetValue(sdiIdentifier("Fscale", 0), tempVal1);
        tempVal1.GetValue(Fscale);

        tempVal1 = sdiValue(Epsdot);
        g_pEntity->GetValue(sdiIdentifier("Epsdot", 0), tempVal1);
        tempVal1.GetValue(Epsdot);
//    }
/////////////////////////////////////////////////////////////////////////////////////
// Line4
//# FCT_IDEL           FSCALE_EL              EI_REF
//	CARD("%10d%20lg%20lg",fct_IDel,Fscale_el,EI_ref);
/////////////////////////////////////////////////////////////////////////////////////
    double Fscale_el=0;
    double EI_ref=0;
    sdiValueEntity functEntity1;
//
    tempVal = sdiValue(functEntity1);
    g_pEntity->GetValue(sdiIdentifier("fct_IDel"), tempVal);
    tempVal.GetValue(functEntity1);
//
    tempVal1 = sdiValue(Fscale_el);
    g_pEntity->GetValue(sdiIdentifier("Fscale_el", 0), tempVal1);
    tempVal1.GetValue(Fscale_el);
//
    tempVal1 = sdiValue(EI_ref);
    g_pEntity->GetValue(sdiIdentifier("EI_ref", 0), tempVal1);
    tempVal1.GetValue(EI_ref);
/////////////////////////////////////////////////////////////////////////////////////
// Line5
//# FCT_ID_T            FSCALE_T
//	CARD("%10d%20lg",fct_IDt,FscaleT);
/////////////////////////////////////////////////////////////////////////////////////
    double FscaleT=0;
    sdiValueEntity functEntity2;
//
    tempVal = sdiValue(functEntity2);
    g_pEntity->GetValue(sdiIdentifier("fct_IDt"), tempVal);
    tempVal.GetValue(functEntity2);
//
    tempVal1 = sdiValue(FscaleT);
    g_pEntity->GetValue(sdiIdentifier("FscaleT", 0), tempVal1);
    tempVal1.GetValue(FscaleT);
/////////////////////////////////////////////////////////////////////////////////////
// Create /FAIL/TAB1
/////////////////////////////////////////////////////////////////////////////////////
    HandleEdit radFailHEdit;
    g_pModelViewSDI->CreateEntity(radFailHEdit, "/FAIL/TAB1", buffer);
    EntityEdit radFailTab1Edit(g_pModelViewSDI, radFailHEdit);
    radFailTab1Edit.SetValue(sdiIdentifier("unitid"), sdiValue(uid));
/////////////////////////////////////////////////////////////////////////////////////
// Create /TABLE
/////////////////////////////////////////////////////////////////////////////////////
    HandleEdit radTableHEdit;
    g_pModelViewSDI->CreateEntity(radTableHEdit, "/TABLE/1", buffer ,tableId);
    EntityEdit radTableEdit(g_pModelViewSDI, radTableHEdit);
    radTableEdit.SetValue(sdiIdentifier("unitid"), sdiValue(uid));
/////////////////////////////////////////////////////////////////////////////////////
// Fill /FAIL/TAB1 values
/////////////////////////////////////////////////////////////////////////////////////
    radFailTab1Edit.SetValue(sdiIdentifier("mat_id",0), sdiValue(sdiValueEntity(radMatType,entityMat.GetId())));
//
    radFailTab1Edit.SetValue(sdiIdentifier("Ifail_sh"), sdiValue(Ifail_sh));
    radFailTab1Edit.SetValue(sdiIdentifier("Ifail_so"), sdiValue(Ifail_so));
    radFailTab1Edit.SetValue(sdiIdentifier("P_thickfail"), sdiValue(P_thickfail));
    radFailTab1Edit.SetValue(sdiIdentifier("Ixfem"), sdiValue(Ixfem));
//
    radFailTab1Edit.SetValue(sdiIdentifier("Dcrit"), sdiValue(Dcrit));
    radFailTab1Edit.SetValue(sdiIdentifier("D"), sdiValue(D));
    radFailTab1Edit.SetValue(sdiIdentifier("n"), sdiValue(n));
    radFailTab1Edit.SetValue(sdiIdentifier("Dadv"), sdiValue(Dadv));
//
    radFailTab1Edit.SetValue(sdiIdentifier("table1_ID",0), sdiValue(sdiValueEntity(radTableType,tableId)));
    radFailTab1Edit.SetValue(sdiIdentifier("Xscale1",0), sdiValue(1));
    radFailTab1Edit.SetValue(sdiIdentifier("Xscale2",0), sdiValue(1));
//
    radFailTab1Edit.SetValue(sdiIdentifier("fct_IDel",0), sdiValue(sdiValueEntity(radFunctType,functEntity1.GetId())));
    radFailTab1Edit.SetValue(sdiIdentifier("Fscale_el",0), sdiValue(Fscale_el));
    radFailTab1Edit.SetValue(sdiIdentifier("EI_ref",0), sdiValue(EI_ref));
//
    radFailTab1Edit.SetValue(sdiIdentifier("fct_IDt",0), sdiValue(sdiValueEntity(radFunctType,functEntity2.GetId())));
    radFailTab1Edit.SetValue(sdiIdentifier("FscaleT",0), sdiValue(FscaleT));
/////////////////////////////////////////////////////////////////////////////////////
// Fill /TABLE values
/////////////////////////////////////////////////////////////////////////////////////
    radTableEdit.SetValue(sdiIdentifier("ORDER"), sdiValue(2));
    if(N_rate == 1)
    {
        radTableEdit.SetValue(sdiIdentifier("curverows"), sdiValue(2));
        radTableEdit.SetValue(sdiIdentifier("tableentityarray", 0, 0),sdiValue(sdiValueEntity(radFunctType, idList[0])));
        radTableEdit.SetValue(sdiIdentifier("A", 0, 0), sdiValue(Epsdot[0]));
        radTableEdit.SetValue(sdiIdentifier("Fscale_array", 0, 0), sdiValue(Fscale[0]));
        radTableEdit.SetValue(sdiIdentifier("tableentityarray", 0, 1),sdiValue(sdiValueEntity(radFunctType, idList[0])));
        radTableEdit.SetValue(sdiIdentifier("A", 0, 1), sdiValue(Epsdot[0]+0.00001));
        radTableEdit.SetValue(sdiIdentifier("Fscale_array", 0, 1), sdiValue(Fscale[0]));

    }
    else
    {
        radTableEdit.SetValue(sdiIdentifier("curverows"), sdiValue(N_rate));
        for (int j = 0; j < N_rate; ++j)
        {
            radTableEdit.SetValue(sdiIdentifier("tableentityarray", 0, j),sdiValue(sdiValueEntity(radFunctType, idList[j])));
            radTableEdit.SetValue(sdiIdentifier("A", 0, j), sdiValue(Epsdot[j]));
            radTableEdit.SetValue(sdiIdentifier("Fscale_array", 0, j), sdiValue(Fscale[j]));
        }
    }
     *TABLE_MAXID = *TABLE_MAXID + 1;
     *FAIL_MAXID = *FAIL_MAXID + 1;

     bool pIsCrypted = false;

     GlobalEntitySDIIsCrypted(&pIsCrypted);

     if(pIsCrypted)
     {
         const char *a_cryptingref = "true";
         IMECPreObject *pObjFailTab1 = (IMECPreObject *) radFailTab1Edit.GetHandle().GetPointer();
         if(pObjFailTab1)
         {
             pObjFailTab1->SetCryptingReference(a_cryptingref);
         }
         IMECPreObject *pObjTable = (IMECPreObject *) radTableEdit.GetHandle().GetPointer();
         if(pObjTable)
         {
             pObjTable->SetCryptingReference(a_cryptingref);
         }
     }
}///////////////////////////////////////////////////////////////////////////////////////////////////
// Create /SPRING & /PART /MAT & /PROP  from /SHELL & /MAT119
// same rules than old TRANSALL routine for /FAIL/TAB
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIConvert2dElementSeatbelt(int *PART_MAT119,int *PART_MAXID,int *PROP_MAXID,int *MAT_MAXID,int *ELEM_MAXID,int *OFFSET)
{         
    bool isOk = false;
    int id = 0;
    int uid = 0;
    int includeId = 0;
    int partIncludeId = 0;
    int submodelId = 0;
    int elemId = 0;
    int partId = 0;
    int tableId = 0;
    int failId = 0;
    int lineId = 0;
    bool found = false;
    sdiUIntList aNodeId;
    sdiValue val;
    sdiUIntList elemNodes;
    elemNodes.resize(2);
//
    EntityType radPropType = g_pModelViewSDI->GetEntityType("/PROP");
    EntityType radMatType = g_pModelViewSDI->GetEntityType("/MAT");
    EntityType radPartType = g_pModelViewSDI->GetEntityType("/PART");
    EntityType radNodeType = g_pModelViewSDI->GetEntityType("/NODE");
    EntityType radFunctType = g_pModelViewSDI->GetEntityType("/FUNCT");
//
    sdiString destElem = "/SPRING";
/////////////////////////////////////////////////////////////////////////////////////
// READ ORIGINAL /PART INFORMATION
/////////////////////////////////////////////////////////////////////////////////////
// Get original Part Id
    partId = g_pEntity->GetId();
// Get Mat Entity associated with the original part
    HandleRead matHread;
    g_pEntity->GetEntityHandle(sdiIdentifier("materialid"), matHread);
// Get Selection of the elements of the original part
    SelectionElementRead elems(*g_pEntity);
// Submodel offset management
    int newPartId = *PART_MAXID - *OFFSET + 1;
    int newPropId = *PROP_MAXID - *OFFSET + 1;
    int newMatId = *MAT_MAXID - *OFFSET + 1;
    int newElemId = *ELEM_MAXID - *OFFSET + 1;
// Set current include for new option creation
     HandleRead include = g_pEntity->GetInclude();
     g_pModelViewSDI->SetCurrentCollector(include);
/////////////////////////////////////////////////////////////////////////////////////
// Read /MAT/LAW119 ATTRIBUTES
/////////////////////////////////////////////////////////////////////////////////////
//
    double mat119_RHO = 0;
    double mat119_LMIN = 0;
    double mat119_STIFF1 = 0;
    double mat119_DAMP1 = 0;
    double mat119_COEF1 = 0;
    double mat119_COEF2 = 0;
    sdiValueEntity functEntity1;
    sdiValueEntity functEntity2;

    if (matHread.IsValid())
    {
        EntityRead matRead(g_pModelViewSDI, matHread);
        sdiValue tempValFloat(mat119_RHO);
        found =  matRead.GetValue(sdiIdentifier("MAT_RHO"), tempValFloat);
        if(found) tempValFloat.GetValue(mat119_RHO);

        found =  matRead.GetValue(sdiIdentifier("LMIN"), tempValFloat);
        if(found) tempValFloat.GetValue(mat119_LMIN);

        found =  matRead.GetValue(sdiIdentifier("STIFF1"), tempValFloat);
        if(found) tempValFloat.GetValue(mat119_STIFF1);

        found =  matRead.GetValue(sdiIdentifier("DAMP1"), tempValFloat);
        if(found) tempValFloat.GetValue(mat119_DAMP1);

        found =  matRead.GetValue(sdiIdentifier("Fcoeft1"), tempValFloat);
        if(found) tempValFloat.GetValue(mat119_COEF1);

        found =  matRead.GetValue(sdiIdentifier("Fcoeft2"), tempValFloat);
        if(found) tempValFloat.GetValue(mat119_COEF2);

        sdiValue tempVal(functEntity1);
        found =  matRead.GetValue(sdiIdentifier("FUN_L"), tempVal);
        if(found) tempVal.GetValue(functEntity1);

        found =  matRead.GetValue(sdiIdentifier("FUN_UL"), tempVal);
        if(found) tempVal.GetValue(functEntity2);
    }
/////////////////////////////////////////////////////////////////////////////////////
// Create /PROP/TYPE23 & Fill attributes
/////////////////////////////////////////////////////////////////////////////////////
// 
    HandleEdit radpropHEdit;
    g_pModelViewSDI->CreateEntity(radpropHEdit, "/PROP/TYPE23", 
                          "Automatically generated property for 1D seatbelts from part : " + g_pEntity->GetName() , newPropId);
    EntityEdit radProp23Edit(g_pModelViewSDI, radpropHEdit);
//
    radProp23Edit.SetValue(sdiIdentifier("Volume"), sdiValue(1.0));
/////////////////////////////////////////////////////////////////////////////////////
// Create /MAT/LAW114 & Fill attributes
/////////////////////////////////////////////////////////////////////////////////////
//
    HandleEdit radmatHEdit;
    g_pModelViewSDI->CreateEntity(radmatHEdit, "/MAT/LAW114", 
                          "Automatically generated material for 1D seatbelts from part : " + g_pEntity->GetName() , newMatId);
    EntityEdit radMat114Edit(g_pModelViewSDI, radmatHEdit);
//
    radMat114Edit.SetValue(sdiIdentifier("MAT_RHO"), sdiValue(mat119_RHO));
    radMat114Edit.SetValue(sdiIdentifier("LMIN"), sdiValue(mat119_LMIN));
    radMat114Edit.SetValue(sdiIdentifier("STIFF1"), sdiValue(mat119_STIFF1));
    radMat114Edit.SetValue(sdiIdentifier("DAMP1"), sdiValue(mat119_DAMP1));
    radMat114Edit.SetValue(sdiIdentifier("Fcoeft1"), sdiValue(mat119_COEF1));
    radMat114Edit.SetValue(sdiIdentifier("FUN_L",0), sdiValue(sdiValueEntity(radFunctType,functEntity1.GetId())));
    radMat114Edit.SetValue(sdiIdentifier("FUN_UL",0), sdiValue(sdiValueEntity(radFunctType,functEntity2.GetId())));
/////////////////////////////////////////////////////////////////////////////////////
// Create /PART
/////////////////////////////////////////////////////////////////////////////////////
//
    HandleEdit radpartHEdit;
    g_pModelViewSDI->CreateEntity(radpartHEdit, "/PART", 
                          "Automatically generated part for 1D seatbelts from : " + g_pEntity->GetName() , newPartId);
    EntityEdit radPartEdit(g_pModelViewSDI, radpartHEdit);
//
    radPartEdit.SetValue(sdiIdentifier("materialid"), sdiValue(sdiValueEntity(radMatType, newMatId)));
    radPartEdit.SetValue(sdiIdentifier("propertyid"), sdiValue(sdiValueEntity(radPropType, newPropId)));
/////////////////////////////////////////////////////////////////////////////////////
// Elem loop to get connectivity of elements to create
///////////////////////////////////////////////////////////////////////////////////// 
    vector< pair <unsigned int,unsigned int> > tmpNodes;
    int nbElems = 0;
    while(elems.Next())
    {
        *ELEM_MAXID = *ELEM_MAXID + 1;
        nbElems = nbElems + 2;
// Get Elem Id
        elemId = elems->GetId();
// Get Elem Connectivity
        elems->GetNodeIds(aNodeId);   
        tmpNodes.push_back( make_pair(aNodeId[0],aNodeId[1]) );
        tmpNodes.push_back( make_pair(aNodeId[3],aNodeId[2]) );
// next     
        aNodeId.resize(0);   
        i++;
    }
/////////////////////////////////////////////////////////////////////////////////////
// Create elements deleting dupplicated connectivity
///////////////////////////////////////////////////////////////////////////////////// 
    sort(tmpNodes.begin(), tmpNodes.end());
    
    unsigned int first = tmpNodes[0].first;
    unsigned int second = tmpNodes[0].second;
// create first spring
    *ELEM_MAXID = *ELEM_MAXID + 1;
    HandleElementEdit radElem0;
    
    elemNodes[0] = first;
    elemNodes[1] = second;
    g_pModelViewSDI->CreateElement(radElem0,"/SPRING",elemNodes,radpartHEdit,0);

    for(int i = 1; i < nbElems ; i = i + 1)    
    { 
        if(first !=  tmpNodes[i].first || second !=  tmpNodes[i].second)  
        {  
// create other spring elements
            *ELEM_MAXID = *ELEM_MAXID + 1;
            HandleElementEdit radElem;
            elemNodes[0] = tmpNodes[i].first;
            elemNodes[1] = tmpNodes[i].second;
            g_pModelViewSDI->CreateElement(radElem,"/SPRING",elemNodes,radpartHEdit,*ELEM_MAXID);
        }
        first = tmpNodes[i].first;
        second = tmpNodes[i].second;
    }
/////////////////////////////////////////////////////////////////////////////////////
    *MAT_MAXID = *MAT_MAXID + 1 ;
    *PART_MAXID = *PART_MAXID + 1 ;
    *PROP_MAXID = *PROP_MAXID + 1 ;
    *ELEM_MAXID = *ELEM_MAXID + *OFFSET ;
/////////////////////////////////////////////////////////////////////////////////////
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Create /SPRING & /PART /MAT & /PROP  from /SHELL & /MAT119
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIConvert2dElementsSeatbelt(int *PART_MAT119,int *PART_MAXID,int *PROP_MAXID,int *MAT_MAXID,int *ELEM_MAXID,int *OFFSET,int *SEATBELT_CONVERTED_ELEMENTS,int *ELEM_INDEX)
{      
    bool isOk = false;
    int id = 0;
    int uid = 0;
    int includeId = 0;
    int partIncludeId = 0;
    int submodelId = 0;
    int elemId = 0;
    int partId = 0;
    int tableId = 0;
    int failId = 0;
    int lineId = 0;
    bool found = false;
    sdiUIntList aNodeId;
    sdiValue val;
    sdiUIntList elemNodes;
    elemNodes.resize(2);
//
    EntityType radPropType = g_pModelViewSDI->GetEntityType("/PROP");
    EntityType radMatType = g_pModelViewSDI->GetEntityType("/MAT");
    EntityType radPartType = g_pModelViewSDI->GetEntityType("/PART");
    EntityType radNodeType = g_pModelViewSDI->GetEntityType("/NODE");
    EntityType radFunctType = g_pModelViewSDI->GetEntityType("/FUNCT");
//
    sdiString destElem = "/SPRING";
/////////////////////////////////////////////////////////////////////////////////////
// READ ORIGINAL /PART INFORMATION
/////////////////////////////////////////////////////////////////////////////////////
// Get original Part Id
    HandleRead partHread;
    g_pModelViewSDI->FindById("/PART", *PART_MAT119, partHread);
    EntityRead partEntity = EntityRead(g_pModelViewSDI, partHread);
    g_pEntity = &partEntity;

    partId = g_pEntity->GetId();


// Get Mat Entity & Prop Entity associated with the original part
    HandleRead matHread;
    HandleRead propHread;
    HandleRead skewHread;
    HandleRead node1Hread;
    HandleRead node2Hread;
    HandleRead node3Hread;
    g_pEntity->GetEntityHandle(sdiIdentifier("materialid"), matHread);
    g_pEntity->GetEntityHandle(sdiIdentifier("propertyid"), propHread);
// Get Selection of the elements of the original part
    SelectionElementRead elems(*g_pEntity);
// Submodel offset management
    int newPartId = *PART_MAXID - *OFFSET + 1;
    int newPropId = *PROP_MAXID - *OFFSET + 1;
    int newMatId = *MAT_MAXID - *OFFSET + 1;
    int newElemId = *ELEM_MAXID - *OFFSET + 1;
// 
     HandleRead include = g_pEntity->GetInclude();
     g_pModelViewSDI->SetCurrentCollector(include);
/////////////////////////////////////////////////////////////////////////////////////
// Read /MAT/LAW119 ATTRIBUTES
/////////////////////////////////////////////////////////////////////////////////////
//
    double mat119_RHO = 0;
    double mat119_LMIN = 0;
    double mat119_STIFF1 = 0;
    double mat119_DAMP1 = 0;
    double mat119_COEF1 = 0;
    double mat119_COEF2 = 0;
    sdiValueEntity functEntity1;
    sdiValueEntity functEntity2;

    if (matHread.IsValid())
    {
        EntityRead matRead(g_pModelViewSDI, matHread);
        sdiValue tempValFloat(mat119_RHO);
        found =  matRead.GetValue(sdiIdentifier("MAT_RHO"), tempValFloat);
        if(found) tempValFloat.GetValue(mat119_RHO);

        found =  matRead.GetValue(sdiIdentifier("LMIN"), tempValFloat);
        if(found) tempValFloat.GetValue(mat119_LMIN);

        found =  matRead.GetValue(sdiIdentifier("STIFF1"), tempValFloat);
        if(found) tempValFloat.GetValue(mat119_STIFF1);

        found =  matRead.GetValue(sdiIdentifier("DAMP1"), tempValFloat);
        if(found) tempValFloat.GetValue(mat119_DAMP1);

        found =  matRead.GetValue(sdiIdentifier("Fcoeft1"), tempValFloat);
        if(found) tempValFloat.GetValue(mat119_COEF1);

        found =  matRead.GetValue(sdiIdentifier("Fcoeft2"), tempValFloat);
        if(found) tempValFloat.GetValue(mat119_COEF2);

        sdiValue tempVal(functEntity1);
        found =  matRead.GetValue(sdiIdentifier("FUN_L"), tempVal);
        if(found) tempVal.GetValue(functEntity1);

        found =  matRead.GetValue(sdiIdentifier("FUN_UL"), tempVal);
        if(found) tempVal.GetValue(functEntity2);
    }
/////////////////////////////////////////////////////////////////////////////////////
// Create /PROP/TYPE23 & Fill attributes
/////////////////////////////////////////////////////////////////////////////////////
// 
    HandleEdit radpropHEdit;
    g_pModelViewSDI->CreateEntity(radpropHEdit, "/PROP/TYPE23", 
                          "Automatically generated property for 1D seatbelts from part : " + g_pEntity->GetName() , newPropId);
    EntityEdit radProp23Edit(g_pModelViewSDI, radpropHEdit);
//
    radProp23Edit.SetValue(sdiIdentifier("Volume"), sdiValue(1.0));
/////////////////////////////////////////////////////////////////////////////////////
// Create /MAT/LAW114 & Fill attributes
/////////////////////////////////////////////////////////////////////////////////////
//
    HandleEdit radmatHEdit;
    g_pModelViewSDI->CreateEntity(radmatHEdit, "/MAT/LAW114", 
                          "Automatically generated material for 1D seatbelts from part : " + g_pEntity->GetName() , newMatId);
    EntityEdit radMat114Edit(g_pModelViewSDI, radmatHEdit);
//
    radMat114Edit.SetValue(sdiIdentifier("MAT_RHO"), sdiValue(mat119_RHO));
    radMat114Edit.SetValue(sdiIdentifier("LMIN"), sdiValue(mat119_LMIN));
    radMat114Edit.SetValue(sdiIdentifier("STIFF1"), sdiValue(mat119_STIFF1));
    radMat114Edit.SetValue(sdiIdentifier("DAMP1"), sdiValue(mat119_DAMP1));
    radMat114Edit.SetValue(sdiIdentifier("Fcoeft1"), sdiValue(mat119_COEF1));
    radMat114Edit.SetValue(sdiIdentifier("FUN_L",0), sdiValue(sdiValueEntity(radFunctType,functEntity1.GetId())));
    radMat114Edit.SetValue(sdiIdentifier("FUN_UL",0), sdiValue(sdiValueEntity(radFunctType,functEntity2.GetId())));
/////////////////////////////////////////////////////////////////////////////////////
// Create /PART
/////////////////////////////////////////////////////////////////////////////////////
//
    HandleEdit radpartHEdit;
    g_pModelViewSDI->CreateEntity(radpartHEdit, "/PART", 
                          "Automatically generated part for 1D seatbelts from : " + g_pEntity->GetName() , newPartId);
    EntityEdit radPartEdit(g_pModelViewSDI, radpartHEdit);
//
    radPartEdit.SetValue(sdiIdentifier("materialid"), sdiValue(sdiValueEntity(radMatType, newMatId)));
    radPartEdit.SetValue(sdiIdentifier("propertyid"), sdiValue(sdiValueEntity(radPropType, newPropId)));
/////////////////////////////////////////////////////////////////////////////////////
// Search for nodes of the skew in the orthotropic property 
///////////////////////////////////////////////////////////////////////////////////// 
    sdiValueEntity node1;
    sdiValueEntity node2;
    sdiValueEntity node3;
    unsigned int node1Id=0;
    unsigned int node2Id=0;
    unsigned int node3Id=0;
    int originElem = -1;
    vector< pair <unsigned int,unsigned int> > tmpNodes;
    vector< unsigned int > tmpIdShells;
    vector< unsigned int > index;
    int nbElems = 0;
    int idx = 0;
    if (propHread.IsValid()) propHread.GetEntityHandle(g_pModelViewSDI,sdiIdentifier("SKEW_CSID"), skewHread);
    if (skewHread.IsValid()) 
    {
        sdiValue tempVal(node1);
        EntityRead skewRead(g_pModelViewSDI, skewHread);
        found =  skewRead.GetValue(sdiIdentifier("N1"), tempVal);
        if(found) tempVal.GetValue(node1);
        found =  skewRead.GetValue(sdiIdentifier("N2"), tempVal);
        if(found) tempVal.GetValue(node2);
        found =  skewRead.GetValue(sdiIdentifier("N3"), tempVal);
        if(found) tempVal.GetValue(node3);
        node1Id = node1.GetId();
        node2Id = node2.GetId();
        node3Id = 0;
/////////////////////////////////////////////////////////////////////////////////////
// Elem loop to get inverse connectivity of elements 
///////////////////////////////////////////////////////////////////////////////////// 
        SelectionRead nodes(g_pModelViewSDI, "/NODE");
        int numnod = 0;
        numnod = nodes.Count();
        int *nod2shell,*knod2shell;
        nod2shell=(int*) malloc(sizeof(int)*4*elems.Count());
        int *elementNodes;
        elementNodes=(int*) malloc(sizeof(int)*4*elems.Count());
  
        for(int k=0; k<4*elems.Count(); k=k+1)
        {  
            nod2shell[k] = 0;
        }

        knod2shell=(int*) malloc(sizeof(int)*(numnod+1));
        for(int k=0; k<numnod+1; k=k+1)
        {  
            knod2shell[k] = 0;
        }

        map<int, int> nodeIndexes;
        map<int, int> elementIndexes;

        map<int, int> nodeIds;
        map<int, int> elementIds;

        int cpt = 0;
        int n = 0;
        int isSkewNodes = 0;
// Node Id Map
        while (nodes.Next())
        {  
            nodeIndexes.insert(pair<int, int>(nodes->GetId(),cpt));
            nodeIds.insert(pair<int, int>(cpt,nodes->GetId()));
            cpt++;
        }
// Elem Id Map
        cpt = 0;
        SelectionElementRead elems1(*g_pEntity);
        while(elems1.Next())
        {  
            elementIndexes.insert(pair<int, int>(elems1->GetId(),cpt));
            elementIds.insert(pair<int, int>(cpt,elems1->GetId()));
            cpt++;
        }
// Reversed Connectivity Creation
        for(int k=0; k<4; k=k+1)
        {  
            SelectionElementRead elems2(*g_pEntity);
            while(elems2.Next())
            {  
                elems2->GetNodeIds(aNodeId);   
                n = nodeIndexes[aNodeId[k]];
                knod2shell[n] = knod2shell[n] + 1;
                aNodeId.resize(0);  
            }
        }
//
        for(int k=0; k<numnod; k=k+1)
        {  
             knod2shell[k+1] = knod2shell[k+1] + knod2shell[k];
        }
//
        for(int k=numnod-1; k>=0; k=k-1)
        {  
             knod2shell[k+1] = knod2shell[k];
        }
        knod2shell[0]=0; 
//
        for(int k=0; k<4; k=k+1)
        {  
            SelectionElementRead elems3(*g_pEntity);
            cpt = 0;
            while(elems3.Next())
            {  
                elems3->GetNodeIds(aNodeId);   
                n = nodeIndexes[aNodeId[k]];
                nod2shell[knod2shell[n]] = cpt;
                knod2shell[n] = knod2shell[n] + 1;
                cpt++;
                aNodeId.resize(0);  
            }
        }
//
        for(int k=numnod-1; k>=0; k=k-1)
        {  
             knod2shell[k+1] = knod2shell[k];
        }
        knod2shell[0]=0; 
// Elements Nodes
        cpt = 0;
        SelectionElementRead elems4(*g_pEntity);
        while(elems4.Next())
        {  
            elems4->GetNodeIds(aNodeId);  
            for(int i=0; i<4; i=i+1)
            {   
                elementNodes[4*cpt+i] = nodeIndexes[aNodeId[i]];
            }
            aNodeId.resize(0);  
            cpt++;
        }
// Find Origin element for direction propagations
        SelectionElementRead elems5(*g_pEntity);
        cpt = 0;
        while(elems5.Next())
        {  
            elems5->GetNodeIds(aNodeId);
            isSkewNodes = 0;
            for(int k=0; k<=3; k=k+1)
            {  
                if ( (aNodeId[k] == node1Id) || (aNodeId[k] == node2Id) ) 
                {
                    isSkewNodes = isSkewNodes + 1;
                    if (isSkewNodes == 2) 
                    {
                        if(aNodeId[k] == node1Id) node1Id = node2Id;
                        if (k == 1) node3Id = aNodeId[3];
                        else if (k == 2) node3Id = aNodeId[0];
                        else if (k == 3) node3Id = aNodeId[1];
                        originElem = cpt;
                        isSkewNodes = 0;
                    }
                }
            }
            aNodeId.resize(0); 
            cpt++; 
        }
/////////////////////////////////////////////////////////////////////////////////////
// Elem loop to build element levels 
///////////////////////////////////////////////////////////////////////////////////// 
        int *levelElem;
        levelElem=(int*) malloc(sizeof(int)*elems.Count());
        for(int j=0;j<elems.Count();j++)
        {   
            levelElem[j] = 0;
        }
        int nbElem=1;
        int *listElement;
        listElement=(int*) malloc(sizeof(int)*4*elems.Count());
        for(int j=0;j<elems.Count();j++)
        {   
            listElement[j] = 0;
        }

        int *listElementTmp;
        listElementTmp=(int*) malloc(sizeof(int)*4*elems.Count());
        for(int j=0;j<elems.Count();j++)
        {   
            listElementTmp[j] = 0;
        }

        int *nodeTag;
        nodeTag=(int*) malloc(sizeof(int)*numnod);
        for(int j=0;j<numnod;j++)
        {   
            nodeTag[j] = 0;
        }
        int level;
        int currentElement;
        int nbNodesElements;
        int nbNodesElementsTmp;
        int nbElements;
        int elementTmp;

        int node1Tag;
        int node2Tag;
        int node3Tag;
        int node4Tag;
        int isNodeTag[4];
        int nbNodesTag;
        int sumNodesTag;
//
        level   = 1;
        currentElement = originElem; 
        levelElem[currentElement] = level;
        nbElements = 1;
        nbNodesElements = 4;
        nbNodesElementsTmp = 4;

        nodeTag[nodeIndexes[node1Id]] = 1;
        nodeTag[nodeIndexes[node3Id]] = 1;
        for(int j=0;j<4;j++)
        {   
            if(nodeTag[elementNodes[4*currentElement+j]] == 0) nodeTag[elementNodes[4*currentElement+j]] = 2; 
        }

        listElement[0]=currentElement;
        while(nbElements != 0)
        {   
            level = level + 1;
            cpt = 0;
            for(int i=0;i<nbElements;i++)
            {   
                currentElement  = listElement[i];
                nbNodesElements = 4;
                for(int j=0;j<nbNodesElements;j++)
                {   
                    for(int k=knod2shell[elementNodes[4*currentElement+j]];k<knod2shell[elementNodes[4*currentElement+j]+1];k++)
                    {  
                        elementTmp = nod2shell[k];
                        nbNodesElementsTmp = 4;
                        node1Tag = nodeTag[elementNodes[4*elementTmp]];
                        node2Tag = nodeTag[elementNodes[4*elementTmp+1]];
                        node3Tag = nodeTag[elementNodes[4*elementTmp+2]];
                        node4Tag = nodeTag[elementNodes[4*elementTmp+3]];
                        sumNodesTag = node1Tag+node2Tag+node3Tag+node4Tag;
                        nbNodesTag = 0;
                        for(int l=0;l<4;l++)
                        {   
                            isNodeTag[l] = 0;
                            if(nodeTag[elementNodes[4*elementTmp+l]]  != 0) isNodeTag[l] = 1;
                            nbNodesTag = nbNodesTag + isNodeTag[l];
                        }

                        if(levelElem[elementTmp] == 0 && nbNodesTag >= 2 && nbNodesTag < 4)
                        {   
                            cpt = cpt + 1;
                            listElementTmp[cpt]=elementTmp;

                            levelElem[elementTmp]=level;

                            for(int l=0;l<nbNodesElementsTmp;l++)
                            {   
                                int nPlusOne = l + 1;
                                if(nPlusOne == 4) nPlusOne = 0;
                                int nMinusOne = l - 1;
                                if(nMinusOne == -1) nMinusOne = 3;

                                if(nodeTag[elementNodes[4*elementTmp+l]] == 0 && nbNodesTag == 3 )
                                {   
                                    nodeTag[elementNodes[4*elementTmp+l]] = 6 - sumNodesTag;
                                    isNodeTag[nodeTag[elementNodes[4*elementTmp+l]]] = 1;
                                    nbNodesTag = nbNodesTag +1;
                                    sumNodesTag = sumNodesTag + nodeTag[elementNodes[4*elementTmp+l]] ;
                                }
                                else if(nodeTag[elementNodes[4*elementTmp+l]] == 0 && nbNodesTag == 2)
                                {   
                                    if(sumNodesTag == 2 ) nodeTag[elementNodes[4*elementTmp+l]] = 2;
                                    else if(sumNodesTag == 4 ) nodeTag[elementNodes[4*elementTmp+l]] = 1;
                                    else if(nodeTag[elementNodes[4*elementTmp+nMinusOne]] != 0 ) nodeTag[elementNodes[4*elementTmp+l]] = nodeTag[elementNodes[4*elementTmp+nMinusOne]];
                                    else if(nodeTag[elementNodes[4*elementTmp+nPlusOne]] != 0 ) nodeTag[elementNodes[4*elementTmp+l]] = nodeTag[elementNodes[4*elementTmp+nPlusOne]];
                                    isNodeTag[nodeTag[elementNodes[4*elementTmp+l]]] = 1;
                                    nbNodesTag = nbNodesTag +1;
                                    sumNodesTag = sumNodesTag + nodeTag[elementNodes[4*elementTmp+l]] ;
                                }
                            }
                        } 
                    }
                }
            }

            nbElements = cpt;
            for(int j=0;j<cpt;j++)
            {   
              listElement[j] =listElementTmp[j];
              listElementTmp[j] = 0;
            }
        }
/////////////////////////////////////////////////////////////////////////////////////
// Elem loop to get connectivity of elements to create in case skew defining direction
///////////////////////////////////////////////////////////////////////////////////// 
        while(elems.Next())
        {
            nbElems = nbElems + 2;
// Get Elem Id
            elemId = elems->GetId();
            tmpIdShells.push_back(elemId);
            tmpIdShells.push_back(elemId);
// Get Elem Connectivity
            elems->GetNodeIds(aNodeId);  
            if(originElem >= 0)
            {
                node1Tag = nodeTag[nodeIndexes[aNodeId[0]]];
                node2Tag = nodeTag[nodeIndexes[aNodeId[1]]];
                node3Tag = nodeTag[nodeIndexes[aNodeId[2]]];
                node4Tag = nodeTag[nodeIndexes[aNodeId[3]]];
                if (node1Tag == node2Tag)
                {
                    tmpNodes.push_back (make_pair(std::minmax(aNodeId[0], aNodeId[1]).first, std::minmax(aNodeId[0], aNodeId[1]).second));
                    tmpNodes.push_back (make_pair(std::minmax(aNodeId[3], aNodeId[2]).first, std::minmax(aNodeId[3], aNodeId[2]).second));
                }
                else
                {
                    tmpNodes.push_back (make_pair(std::minmax(aNodeId[1], aNodeId[2]).first, std::minmax(aNodeId[1], aNodeId[2]).second));
                    tmpNodes.push_back (make_pair(std::minmax(aNodeId[0], aNodeId[3]).first, std::minmax(aNodeId[0], aNodeId[3]).second));
                } 
            }
            else
            {
                tmpNodes.push_back (make_pair(std::minmax(aNodeId[0], aNodeId[1]).first, std::minmax(aNodeId[0], aNodeId[1]).second));
                tmpNodes.push_back (make_pair(std::minmax(aNodeId[3], aNodeId[2]).first, std::minmax(aNodeId[3], aNodeId[2]).second));
            }
// next     
            aNodeId.resize(0);  
            index.push_back(idx); 
            idx++;
            index.push_back(idx); 
            idx++;
        }
        free(nod2shell);
        free(elementNodes);
        free(knod2shell);
        free(levelElem);
        free(listElement);
        free(listElementTmp);
        free(nodeTag);
    }
    else 
/////////////////////////////////////////////////////////////////////////////////////
// Elem loop to get connectivity of elements to create in case no skew
/////////////////////////////////////////////////////////////////////////////////////       
    {
        while(elems.Next())
        {
            nbElems = nbElems + 2;
// Get Elem Id
            elemId = elems->GetId();
            tmpIdShells.push_back(elemId);
            tmpIdShells.push_back(elemId);
// Get Elem Connectivity
            elems->GetNodeIds(aNodeId); 
// push Spring Elem Connectivity 
            tmpNodes.push_back (make_pair(std::minmax(aNodeId[0], aNodeId[1]).first, std::minmax(aNodeId[0], aNodeId[1]).second));
            tmpNodes.push_back (make_pair(std::minmax(aNodeId[3], aNodeId[2]).first, std::minmax(aNodeId[3], aNodeId[2]).second));
// next     
            aNodeId.resize(0);  
            index.push_back(idx); 
            idx++;
            index.push_back(idx); 
            idx++;
        }
    }
/////////////////////////////////////////////////////////////////////////////////////
// Create elements deleting dupplicated connectivity
///////////////////////////////////////////////////////////////////////////////////// 
    sort(index.begin(), index.end(),[&tmpNodes](size_t i1, size_t i2){return tmpNodes[i1] < tmpNodes[i2];});

//
    vector< pair <unsigned int,unsigned int> > tmpShelltoSprings;
    unsigned int first = tmpNodes[index[0]].first;
    unsigned int second = tmpNodes[index[0]].second;
// create first spring
    *ELEM_MAXID = *ELEM_MAXID + 1;
    HandleElementEdit radElem0;
    elemNodes[0] = first;
    elemNodes[1] = second;
    g_pModelViewSDI->CreateElement(radElem0,"/SPRING",elemNodes,radpartHEdit,0);
//
    tmpShelltoSprings.push_back( make_pair(tmpIdShells[0] + *OFFSET,*ELEM_MAXID + *OFFSET) );
    int nbCreatedSprings = 1;
//
    if( nbElems > 1)
    {
      for(int i = 1; i < nbElems ; i = i + 1)    
      { 
          if(first !=  tmpNodes[index[i]].first || second !=  tmpNodes[index[i]].second)  
          {  
// create other spring elements 
                nbCreatedSprings = nbCreatedSprings + 1;
                *ELEM_MAXID = *ELEM_MAXID + 1;
                HandleElementEdit radElem;
                elemNodes[0] = tmpNodes[index[i]].first;
                elemNodes[1] = tmpNodes[index[i]].second;
                g_pModelViewSDI->CreateElement(radElem,"/SPRING",elemNodes,radpartHEdit,*ELEM_MAXID);
                //
                tmpShelltoSprings.push_back( make_pair(tmpIdShells[index[i]] + *OFFSET,*ELEM_MAXID + *OFFSET) );
                //
                first = tmpNodes[index[i]].first;
                second = tmpNodes[index[i]].second;
            }
        }
    }

    sort(tmpShelltoSprings.begin(), tmpShelltoSprings.end());
    SEATBELT_CONVERTED_ELEMENTS[*ELEM_INDEX*3] = tmpShelltoSprings[0].first;
    SEATBELT_CONVERTED_ELEMENTS[*ELEM_INDEX*3+1] = tmpShelltoSprings[0].second;
    if( nbElems > 1)
    {
        for(int i = 1; i < nbCreatedSprings ; i = i + 1)    
        {
            if( (tmpShelltoSprings[i].second != 0 )&& (tmpShelltoSprings[i].first !=  0) && (tmpShelltoSprings[i].first  != tmpShelltoSprings[i-1].first ) )
            {
                *ELEM_INDEX = *ELEM_INDEX + 1;
                SEATBELT_CONVERTED_ELEMENTS[*ELEM_INDEX*3] = tmpShelltoSprings[i].first;
                SEATBELT_CONVERTED_ELEMENTS[*ELEM_INDEX*3+1] = tmpShelltoSprings[i].second;
            }
            else if( (tmpShelltoSprings[i].second != 0) && (tmpShelltoSprings[i].first !=  0) && (tmpShelltoSprings[i].first  == tmpShelltoSprings[i-1].first ) )
            {
                SEATBELT_CONVERTED_ELEMENTS[*ELEM_INDEX*3+2] = tmpShelltoSprings[i].second;
            }
        }
    }
/////////////////////////////////////////////////////////////////////////////////////
fflush(stdout);
/////////////////////////////////////////////////////////////////////////////////////
    *MAT_MAXID = *MAT_MAXID + 1 ;
    *PART_MAXID = *PART_MAXID + 1 ;
    *PROP_MAXID = *PROP_MAXID + 1 ;
    *ELEM_MAXID = *ELEM_MAXID + *OFFSET ;
/////////////////////////////////////////////////////////////////////////////////////
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Create /TH AND TAKE TRANSLATED /SHELL -> /SPRIN INTO ACCOUNT
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIConvertTh2dElementSeatbelt(int *TH_MAXID,int *OFFSET,int *SEATBELT_CONVERTED_ELEMENTS,int *NB_SEATBELT_SHELLS)
{    
//
    EntityType radSpringType = g_pModelViewSDI->GetEntityType("/SPRING");
    bool found = false;
    int idsmax =0 ;
    sdiValue tempValInt(idsmax);
    found =  g_pEntity->GetValue(sdiIdentifier("idsmax"), tempValInt);
    if(found) tempValInt.GetValue(idsmax);
//
    sdiValueEntityList shellEntityList;
    sdiUIntList idList;
    sdiValue tempVal(shellEntityList);
    g_pEntity->GetValue(sdiIdentifier("ids"), tempVal);
    tempVal.GetValue(shellEntityList);
    shellEntityList.GetIdList(idList);
//
    sdiStringList VarList;
    sdiValue tempValString(VarList);
    g_pEntity->GetValue(sdiIdentifier("VAR", 0), tempValString);
    tempValString.GetValue(VarList);
//
    int idsmaxThSpring = 0;
    sdiUIntList idsThSpring;
    idsThSpring.resize(*NB_SEATBELT_SHELLS*2);

    for (int i = 0; i < idsmax; i++)
    {
        for (int j = 0; j < *NB_SEATBELT_SHELLS; j++)
        {
            if(SEATBELT_CONVERTED_ELEMENTS[3*j] == idList[i])
            {
                if (SEATBELT_CONVERTED_ELEMENTS[3*j+1] != 0) 
                {
                    idsThSpring[idsmaxThSpring]=SEATBELT_CONVERTED_ELEMENTS[3*j+1];
                    idsmaxThSpring = idsmaxThSpring + 1;
                }
                if (SEATBELT_CONVERTED_ELEMENTS[3*j+2] != 0) 
                {
                    idsThSpring[idsmaxThSpring]=SEATBELT_CONVERTED_ELEMENTS[3*j+2];
                    idsmaxThSpring = idsmaxThSpring + 1;
                }
            }
        }
    }
/////////////////////////////////////////////////////////////////////////////////////
// Create /TH/SPRING
/////////////////////////////////////////////////////////////////////////////////////
    *TH_MAXID = *TH_MAXID + 1;
    HandleEdit radThSpringHEdit;
    g_pModelViewSDI->CreateEntity(radThSpringHEdit, "/TH/SPRING", "/TH/SPRING Automatically generated from /TH/SPRING (Springs generated from 2d seatbelts)", *TH_MAXID);
    EntityEdit radThSpringEdit(g_pModelViewSDI, radThSpringHEdit);
//
    radThSpringEdit.SetValue(sdiIdentifier("idsmax"), sdiValue(idsmaxThSpring));
    for (int j = 0; j < idsmaxThSpring; ++j)
    {
        radThSpringEdit.SetValue(sdiIdentifier("ids", 0, j),sdiValue(sdiValueEntity(radSpringType, idsThSpring[j])));
    }
// 
    radThSpringEdit.SetValue(sdiIdentifier("Number_Of_Variables"), sdiValue(1));
    radThSpringEdit.SetValue(sdiIdentifier("VAR"), sdiValue(sdiStringList({"DEF"})));
//
}     

///////////////////////////////////////////////////////////////////////////////////////////////////
// Count number of translated 2D Seatbelts shells
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDICountElementsInPart(int *NB_ELEMS)
{         
    bool isOk = false;
    int id = 0;
    int uid = 0;
    int includeId = 0;
    int partIncludeId = 0;
    int submodelId = 0;
    int elemId = 0;
    int partId = 0;
    int tableId = 0;
    int failId = 0;
    int lineId = 0;
    bool found = false;
    sdiUIntList aNodeId;
    sdiValue val;
    sdiUIntList elemNodes;
    elemNodes.resize(2);
/////////////////////////////////////////////////////////////////////////////////////
// Get Selection of the elements of the original part
/////////////////////////////////////////////////////////////////////////////////////
    SelectionElementRead elems(*g_pEntity);
/////////////////////////////////////////////////////////////////////////////////////
// Count elements
///////////////////////////////////////////////////////////////////////////////////// 
    *NB_ELEMS = elems.Count();
/////////////////////////////////////////////////////////////////////////////////////
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Give Number of Entities that uses some other entities
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalModelSDIEntityReferencesNumber(char *type, int *id, int *refNumber)
{
    EntityType radType = g_pModelViewSDI->GetEntityType(type);
    std::vector<RadiossblkEntityReference>  entityReferences = RadiossblkGetEntityReferences(g_pModelViewSDI,radType,(unsigned int) *id);
    *refNumber=entityReferences.size();

    /*printf("***********************************************\n");
    printf(" type = %s  id=%d refNumber=%d \n",type,*id,*refNumber);
    printf("***********************************************\n");
    printf("%s %d referenced in:\n", type, *id);
    for(size_t i = 0; i < entityReferences.size(); ++i)
    {
        const RadiossblkEntityReference& ref = entityReferences[i];
        if(UINT_MAX == ref.row)
        {
            printf("  %s id %u: %s (%s)\n",
                ref.keyword, ref.id, ref.attributeName.c_str(), ref.attributeSolverLabel.c_str());
        }
        else
        {
            printf("  %s id %u: %s (%s) index %u\n",
                ref.keyword, ref.id, ref.attributeName.c_str(), ref.attributeSolverLabel.c_str(), ref.row);
        }
    }*/
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Apply Submodel Offsets
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalModelSDIApplyOffsets()
{
    RadiossblkApplyOffsets(g_pModelViewSDI,false);
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// UnApply Submodel Offsets
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalModelSDIUnapplyOffsets()
{
    RadiossblkApplyOffsets(g_pModelViewSDI,true);
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Count include files in global model
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalModelSDICountIncludeFiles(int *nbIncludes)
{
    SelectionRead entities(g_pModelViewSDI, "#include");
    // ...

    while(entities.Next())
    {
        unsigned int id = entities->GetId();
        const sdiString &solverkeyword = entities->GetKeyword();
        unsigned int parentId = 0;
        HandleRead hInclude = entities->GetInclude();
        if(hInclude.IsValid())
        {
            parentId = hInclude.GetId(g_pModelViewSDI);
        }
        sdiValue value;
        unsigned int submodelId=0;
        value.GetValue(submodelId);
        if(submodelId != 0)
        {
            HandleRead hSubmodel;
            entities->GetEntityHandle(sdiIdentifier("shadow_submodel"), hSubmodel);
        }
        else
        {
            *nbIncludes = *nbIncludes + 1;
        }
    }
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Find includes in global model
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalModelSDIGetIncludesList(char **includeFiles)
{
    int cptIncludeFiles=0;
    SelectionRead entities(g_pModelViewSDI, "#include");
    // ...
    while(entities.Next())
    {
        unsigned int id = entities->GetId();
        const sdiString &solverkeyword = entities->GetKeyword();
        unsigned int parentId = 0;
        HandleRead hInclude = entities->GetInclude();
        if(hInclude.IsValid())
        {
            parentId = hInclude.GetId(g_pModelViewSDI);
        }
        sdiValue value;
        unsigned int submodelId=0;
        value.GetValue(submodelId);
        if(submodelId != 0)
        {
            HandleRead hSubmodel;
            entities->GetEntityHandle(sdiIdentifier("shadow_submodel"), hSubmodel);
        }
        else
        {
            includeFiles[cptIncludeFiles] =(char*)malloc(sizeof(char*)* sizeof(entities->GetName().c_str()) );
            strcpy(includeFiles[cptIncludeFiles], entities->GetName().c_str());
            cptIncludeFiles = cptIncludeFiles + 1;
        }
    }
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Check if a group (with given type) is used
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalModelSDIIsGroupUsed(char *type, int *id, bool *isUsed)
{
    EntityType radType = g_pModelViewSDI->GetEntityType(type);
    *isUsed = RadiossblkIsGroupUsed(g_pModelViewSDI,radType,(unsigned int) *id);
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Delete Current Entity in Memory
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIdeleteEntity()
{
    HandleRead radEntityHRead = g_pEntity->GetHandle();
    
    HandleEdit radEntityHEdit(radEntityHRead.GetType(), radEntityHRead.GetPointer());

    g_pModelViewSDI->Delete(radEntityHEdit);
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Create main node in /RBODY when MAIN node is empty
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIRbodiesCreateMainNode(int *addedNodeId)
{    
    EntityType radNodeType = g_pModelViewSDI->GetEntityType("/NODE");
    sdiValueEntity node1;
    sdiValue tempVal(node1);
     bool found =  g_pEntity->GetValue(sdiIdentifier("RBID"), tempVal);        
    if(found) tempVal.GetValue(node1);

    if(node1.GetId() == 0 || !found)
    {
        HandleEdit radNodeHEdit;
        g_pModelViewSDI->CreateEntity(radNodeHEdit, "/NODE");
        EntityEdit radNodeEdit(g_pModelViewSDI, radNodeHEdit);
        radNodeEdit.SetValue(sdiIdentifier("X"), sdiValue(0.0));
        radNodeEdit.SetValue(sdiIdentifier("Y"), sdiValue(0.0));
        radNodeEdit.SetValue(sdiIdentifier("Z"), sdiValue(0.0));
        *addedNodeId = radNodeHEdit.GetId(g_pModelViewSDI);

        HandleRead rbodyHRead = g_pEntity->GetHandle();
        HandleEdit rbodyHEdit(rbodyHRead.GetType(), rbodyHRead.GetPointer());
        EntityEdit rbodyEdit(g_pModelViewSDI, rbodyHEdit);
        rbodyEdit.SetValue(sdiIdentifier("independentnode"), sdiValue(sdiValueEntity(radNodeType, *addedNodeId)));
        rbodyEdit.SetValue(sdiIdentifier("ICOG"), sdiValue(2));
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Create Node in Radioss Model
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDICreateNode(double *x, double *y, double *z, int *newNodeId)
{
    EntityType radNodeType = g_pModelViewSDI->GetEntityType("/NODE");
    HandleNodeEdit radnodeHEdit;
    sdiTriple pos(*x, *y, *z);
    g_pModelViewSDI->CreateNode(radnodeHEdit, "/NODE", pos , 0);
    *newNodeId = radnodeHEdit.GetId(g_pModelViewSDI);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// In case Itetra4=1 and /TETRA4 , change to /TETRA10
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIConvertTetra4ToTetra10(int *Itetra4ToConsider)
{    
    SelectionElementEdit tetElements(g_pModelViewSDI, "/TETRA4");
    
    // Define edge node pairs for TETRA4
    static const int edgeNodes[6][2] = {
        {0, 1},  // edge 1-2
        {1, 2},  // edge 2-3
        {2, 0},  // edge 3-1
        {0, 3},  // edge 1-4
        {1, 3},  // edge 2-4
        {2, 3}   // edge 3-4
    };

    EntityType radPartType = g_pModelViewSDI->GetEntityType("/PART");
    EntityType radNodeType = g_pModelViewSDI->GetEntityType("/NODE");
    EntityType radTetra10Type = g_pModelViewSDI->GetEntityType("/TETRA10");

    // Hash function for pair<unsigned int, unsigned int>
    struct PairHash {
        std::size_t operator()(const std::pair<unsigned int, unsigned int>& p) const {
            return std::hash<unsigned long long>{}(((unsigned long long)p.first << 32) | p.second);
        }
    };

    // Structure to cache element data
    struct ElementCache {
        int elemId;
        unsigned int partId;
        unsigned int cornerNodes[4];  // 4 corner nodes
        int midNodeIndices[6];        // Indices to unique mid-nodes
    };
    
    // Structure to cache unique mid-node data
    struct MidNodeCache {
        double x, y, z;
    };
    
    std::vector<ElementCache> elementsToCreate;
    elementsToCreate.reserve(10000);
    std::vector<MidNodeCache> midNodesToCreate;
    midNodesToCreate.reserve(30000);
    std::unordered_map<std::pair<unsigned int, unsigned int>, int, PairHash> edgeToMidNodeIndex;
    edgeToMidNodeIndex.reserve(30000);
    std::vector<int> elementsToDelete;
    elementsToDelete.reserve(10000);
    
    int Itetra4 = 0;
    HandleRead partHread;
    unsigned int partId_previous = 0;
    int elemCount = 0;
    
    // Cache for node positions
    std::unordered_map<unsigned int, sdiTriple> nodePositionCache;
    nodePositionCache.reserve(40000);

    // Phase 1: Collect all data
    
    while(tetElements.Next())
    {
        unsigned int partId = tetElements->GetComponentId();

        if(partId != partId_previous)
        {
            
            HandleRead include = tetElements->ElementRead::GetInclude();
            g_pModelViewSDI->SetCurrentCollector(include);

            partId_previous = partId;
            partHread = tetElements->GetComponent();
            
            HandleRead propHread;
            if(partHread.IsValid())
            {
                EntityRead partRead(g_pModelViewSDI, partHread);
                partRead.GetEntityHandle(sdiIdentifier("prop_ID"), propHread);
            }
                
            if(propHread.IsValid())
            {
                EntityRead propRead(g_pModelViewSDI, propHread);
                sdiValue tempValInt(Itetra4);
                bool found = propRead.GetValue(sdiIdentifier("Itetra4"), tempValInt);
                if(found) tempValInt.GetValue(Itetra4);
            }
        }
        
        if(Itetra4 == *Itetra4ToConsider)
        {
            sdiUIntList aNodeId;
            tetElements->GetNodeIds(aNodeId);
            
            if(aNodeId.size() >= 4)
            {
                ElementCache elemCache;
                elemCache.elemId = tetElements->GetId();
                elemCache.partId = partId;
                
                // Copy corner nodes
                for(int i = 0; i < 4; i++)
                    elemCache.cornerNodes[i] = aNodeId[i];
                
                // Get node coordinates (with caching)
                double x[4], y[4], z[4];
                for(int i = 0; i < 4; i++)
                {
                    unsigned int nodeId = aNodeId[i];
                    auto it = nodePositionCache.find(nodeId);
                    if(it != nodePositionCache.end())
                    {
                        x[i] = it->second.GetX();
                        y[i] = it->second.GetY();
                        z[i] = it->second.GetZ();
                    }
                    else
                    {
                        HandleRead nodeHread;
                        if(g_pModelViewSDI->FindById(radNodeType, nodeId, nodeHread))
                        {
                            NodeRead nodeRead(g_pModelViewSDI, nodeHread);
                            sdiTriple pos = nodeRead.GetPosition();
                            nodePositionCache[nodeId] = pos;
                            x[i] = pos.GetX();
                            y[i] = pos.GetY();
                            z[i] = pos.GetZ();
                        }
                    }
                }
                
                // Process 6 mid-edge nodes
                for(int i = 0; i < 6; i++)
                {
                    unsigned int n1 = elemCache.cornerNodes[edgeNodes[i][0]];
                    unsigned int n2 = elemCache.cornerNodes[edgeNodes[i][1]];
                    auto edgeKey = n1 < n2 ? std::make_pair(n1, n2) : std::make_pair(n2, n1);
                   
                    auto it = edgeToMidNodeIndex.find(edgeKey);
                    if(it != edgeToMidNodeIndex.end())
                    {
                        elemCache.midNodeIndices[i] = it->second;
                    }
                    else
                    {
                        MidNodeCache midNode;
                        int n1_idx = edgeNodes[i][0];
                        int n2_idx = edgeNodes[i][1];
                        midNode.x = (x[n1_idx] + x[n2_idx]) * 0.5;
                        midNode.y = (y[n1_idx] + y[n2_idx]) * 0.5;
                        midNode.z = (z[n1_idx] + z[n2_idx]) * 0.5;
                        
                        int newIndex = midNodesToCreate.size();
                        midNodesToCreate.push_back(midNode);
                        edgeToMidNodeIndex[edgeKey] = newIndex;
                        elemCache.midNodeIndices[i] = newIndex;
                    }
                }
                
                elementsToCreate.push_back(elemCache);
                elementsToDelete.push_back(elemCache.elemId);
                elemCount++;
            }
        }
    }
     
    // Phase 2: Create all mid-nodes
    
    std::vector<unsigned int> createdMidNodeIds(midNodesToCreate.size());
    for(size_t i = 0; i < midNodesToCreate.size(); i++)
    {
        HandleNodeEdit radnodeHEdit;
        sdiTriple pos(midNodesToCreate[i].x, midNodesToCreate[i].y, midNodesToCreate[i].z);
        g_pModelViewSDI->CreateNode(radnodeHEdit, "/NODE", pos, 0);
        createdMidNodeIds[i] = radnodeHEdit.GetId(g_pModelViewSDI);
    }
    
    // Phase 3: Create all TETRA10 elements
    sdiUIntList tetra10Nodes;
    tetra10Nodes.resize(10);
    
    for(size_t i = 0; i < elementsToCreate.size(); i++)
    {
        const ElementCache& elemCache = elementsToCreate[i];
        
        HandleElementEdit radTetra10HEdit;
        g_pModelViewSDI->CreateElement(radTetra10HEdit, "/TETRA10", elemCache.elemId);
        
        for(int j = 0; j < 4; j++)
            tetra10Nodes[j] = elemCache.cornerNodes[j];
        for(int j = 0; j < 6; j++)
            tetra10Nodes[j+4] = createdMidNodeIds[elemCache.midNodeIndices[j]];
        
        radTetra10HEdit.SetValue(g_pModelViewSDI, sdiIdentifier("node_ID"), 
                                 sdiValue(sdiValueEntityList(radNodeType, tetra10Nodes)));
        radTetra10HEdit.SetValue(g_pModelViewSDI, sdiIdentifier("part_ID"), 
                                 sdiValue(sdiValueEntity(radPartType, elemCache.partId)));
    }
    
    // Phase 4: Delete original TETRA4 elements

    SelectionElementEdit tetElementsDel(g_pModelViewSDI, "/TETRA4");
    std::unordered_set<int> deleteSet(elementsToDelete.begin(), elementsToDelete.end());
    while(tetElementsDel.Next())
    {
        if(deleteSet.count(tetElementsDel->GetId()))
        {
            tetElementsDel->SetId(0);
        }
    }
   }

///////////////////////////////////////////////////////////////////////////////////////////////////
// In case /PART with Irigid=1 create /RBODY and put part nodes in it
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalEntitySDIConvertRigidPartToRbody(int *NewRbodyToPart, int *NewRbodyId)
{
    SelectionRead selPart(g_pModelViewSDI, "/PART");
    EntityType radPartType = g_pModelViewSDI->GetEntityType("/PART");
    int cpt = 0;
    

    while(selPart.Next())
    {
        HandleRead include = selPart->GetInclude();
        g_pModelViewSDI->SetCurrentCollector(include);

        bool found = false;
        unsigned int partId = selPart->GetId();

        // Get Irigid value in /PART
        int Irigid = 0;
        sdiValue tempValInt(Irigid);
        found = selPart->GetValue(sdiIdentifier("Irigid"), tempValInt);
        if(found) tempValInt.GetValue(Irigid);
        
        // Check if part uses /MAT/RIGID
        int Imatrigid = 0;
        HandleRead matHread;
        selPart->GetEntityHandle(sdiIdentifier("materialid"), matHread);
            
        if(matHread.IsValid())
        {
            EntityRead matRead(g_pModelViewSDI, matHread);
            const sdiString& matKeyword = matRead.GetKeyword();
            size_t pos = matKeyword.find("/MAT/LAW13");
            if(matKeyword.find("/MAT/RIGID") != sdiString::npos || 
               (pos != sdiString::npos && (pos + 10 >= matKeyword.length() || !isdigit(matKeyword[pos + 10])))) {
                Imatrigid = 1;
            }
        }

        // Create /RBODY if Irigid=1 or if material is rigid, and put all part nodes in it as independent node set
        if(Irigid == 1|| Imatrigid == 1)
        {            
            // Create RBODY
            HandleEdit rbodyHEdit;
            g_pModelViewSDI->CreateEntity(rbodyHEdit, "/RBODY", "/RBODY Automatically generated from /PART Id: "+ std::to_string(selPart->GetId()) + " title: " + selPart->GetName() , 0);
            EntityEdit rbodyEdit(g_pModelViewSDI, rbodyHEdit);
            
            // Create Set of part 
            HandleEdit NodePartSetHedit;
            g_pModelViewSDI->CreateEntity(NodePartSetHedit, "/SET/GENERAL", "Rigid Part:" + std::to_string(partId));
            NodePartSetHedit.SetValue(g_pModelViewSDI,sdiIdentifier("clausesmax"), sdiValue(1));
            NodePartSetHedit.SetValue(g_pModelViewSDI,sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
            NodePartSetHedit.SetValue(g_pModelViewSDI,sdiIdentifier("idsmax", 0, 0), sdiValue(1));
            NodePartSetHedit.SetValue(g_pModelViewSDI,sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radPartType, partId)));
            

            // Set the created set as independent node set
            EntityType radSetType = g_pModelViewSDI->GetEntityType("/SET");
            unsigned int setId = NodePartSetHedit.GetId(g_pModelViewSDI);
            rbodyEdit.SetValue(sdiIdentifier("dependentnodeset"), sdiValue(sdiValueEntity(radSetType, setId)));
 
            sdiValue tempVal;
            bool found = selPart->GetValue(sdiIdentifier("Node_ID"), tempVal);
            if(found) rbodyEdit.SetValue(sdiIdentifier("node_ID"), tempVal); 
            found = selPart->GetValue(sdiIdentifier("Skew_ID"), tempVal);
            if(found) rbodyEdit.SetValue(sdiIdentifier("Skew_ID"), tempVal);
            found = selPart->GetValue(sdiIdentifier("Mass"), tempVal);
            if(found) rbodyEdit.SetValue(sdiIdentifier("Mass"), tempVal);
            found = selPart->GetValue(sdiIdentifier("Sens_ID"), tempVal);
            if(found) rbodyEdit.SetValue(sdiIdentifier("sens_ID"), tempVal);
            found = selPart->GetValue(sdiIdentifier("Jxx"), tempVal);
            if(found) rbodyEdit.SetValue(sdiIdentifier("Jxx"), tempVal);
            found = selPart->GetValue(sdiIdentifier("Jyy"), tempVal);
            if(found) rbodyEdit.SetValue(sdiIdentifier("Jyy"), tempVal);
            found = selPart->GetValue(sdiIdentifier("Jzz"), tempVal);
            if(found) rbodyEdit.SetValue(sdiIdentifier("Jzz"), tempVal);
            found = selPart->GetValue(sdiIdentifier("Jxy"), tempVal);
            if(found) rbodyEdit.SetValue(sdiIdentifier("Jxy"), tempVal);
            found = selPart->GetValue(sdiIdentifier("Jyz"), tempVal);
            if(found) rbodyEdit.SetValue(sdiIdentifier("Jyz"), tempVal);
            found = selPart->GetValue(sdiIdentifier("Jxz"), tempVal);
            if(found) rbodyEdit.SetValue(sdiIdentifier("Jxz"), tempVal);

            

            NewRbodyToPart[cpt] = partId;
            NewRbodyId[cpt] = rbodyHEdit.GetId(g_pModelViewSDI);
            cpt = cpt + 1;
        }
        // If Irigid=2, create one /RBODY per connected component of the part, and put nodes of each component in the corresponding /RBODY as independent node set
        if (Irigid == 2)
        {
            // Get all nodes and elements from the part
            SelectionElementRead elemSelection(*selPart);
            std::set<unsigned int> allPartNodes;
            std::map<unsigned int, unsigned int> nodeIdToIndex;
            std::map<unsigned int, unsigned int> indexToNodeId;
            std::vector<std::vector<unsigned int>> elementConnectivity;
            std::vector<unsigned int> elementIds;
            
            // Collect all nodes and elements
            unsigned int nodeIndex = 0;
            while(elemSelection.Next())
            {
                sdiUIntList nodeIds;
                elemSelection->GetNodeIds(nodeIds);
                elementIds.push_back(elemSelection->GetId());
                
                std::vector<unsigned int> elemNodes;
                for(unsigned int nodeId : nodeIds)
                {
                    if(allPartNodes.find(nodeId) == allPartNodes.end())
                    {
                        allPartNodes.insert(nodeId);
                        nodeIdToIndex[nodeId] = nodeIndex;
                        indexToNodeId[nodeIndex] = nodeId;
                        nodeIndex++;
                    }
                    elemNodes.push_back(nodeIdToIndex[nodeId]);
                }
                elementConnectivity.push_back(elemNodes);
            }
            
            unsigned int NodesNumber = allPartNodes.size();
            unsigned int ElementsNumber = elementConnectivity.size();
            
            if(NodesNumber > 0 && ElementsNumber > 0)
            {
                // Build adjacency graph
                std::vector<std::set<unsigned int>> adjacencyGraph(NodesNumber);
                
                for(const auto& element : elementConnectivity)
                {
                    for(size_t i = 0; i < element.size(); i++)
                    {
                        for(size_t j = i + 1; j < element.size(); j++)
                        {
                            unsigned int node1 = element[i];
                            unsigned int node2 = element[j];
                            adjacencyGraph[node1].insert(node2);
                            adjacencyGraph[node2].insert(node1);
                        }
                    }
                }
                
                // Find connected components using DFS
                std::vector<bool> visited(NodesNumber, false);
                std::vector<std::vector<unsigned int>> components;
                
                for(unsigned int i = 0; i < NodesNumber; i++)
                {
                    if(!visited[i])
                    {
                        std::vector<unsigned int> component;
                        std::stack<unsigned int> stack;
                        stack.push(i);
                        
                        while(!stack.empty())
                        {
                            unsigned int currentNode = stack.top();
                            stack.pop();
                            
                            if(visited[currentNode]) continue;
                            
                            visited[currentNode] = true;
                            component.push_back(currentNode);
                            
                            for(unsigned int neighbor : adjacencyGraph[currentNode])
                            {
                                if(!visited[neighbor])
                                {
                                    stack.push(neighbor);
                                }
                            }
                        }
                        
                        components.push_back(component);
                    }
                }
                
                // Create one RBODY per connected component
                EntityType radSetType = g_pModelViewSDI->GetEntityType("/SET");
                EntityType radNodeType = g_pModelViewSDI->GetEntityType("/NODE");
                
                for(size_t i = 0; i < components.size(); i++)
                {
                    if(!components[i].empty())
                    {
                        // Create RBODY
                        HandleEdit rbodyHEdit;
                        g_pModelViewSDI->CreateEntity(rbodyHEdit, "/RBODY", 
                            "/RBODY Automatically generated from /PART Id: " + std::to_string(partId) + 
                            " component: " + std::to_string(i+1) + "/" + std::to_string(components.size()), 0);
                        EntityEdit rbodyEdit(g_pModelViewSDI, rbodyHEdit);
                        
                        // Create Set with nodes
                        HandleEdit NodePartSetHedit;
                        g_pModelViewSDI->CreateEntity(NodePartSetHedit, "/SET/GENERAL", 
                            "Rigid Part:" + std::to_string(partId) + " Component:" + std::to_string(i+1));
                        NodePartSetHedit.SetValue(g_pModelViewSDI, sdiIdentifier("clausesmax"), sdiValue(1));
                        NodePartSetHedit.SetValue(g_pModelViewSDI, sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("NODE")));
                        NodePartSetHedit.SetValue(g_pModelViewSDI, sdiIdentifier("idsmax", 0, 0), sdiValue((int)components[i].size()));
                        
                        for(size_t j = 0; j < components[i].size(); j++)
                        {
                            unsigned int nodeId = indexToNodeId[components[i][j]];
                            NodePartSetHedit.SetValue(g_pModelViewSDI, sdiIdentifier("ids", 0, j), 
                                sdiValue(sdiValueEntity(radNodeType, nodeId)));
                        }
                        
                        unsigned int setId = NodePartSetHedit.GetId(g_pModelViewSDI);
                        rbodyEdit.SetValue(sdiIdentifier("dependentnodeset"), sdiValue(sdiValueEntity(radSetType, setId)));
                        
                        NewRbodyToPart[cpt] = partId;
                        NewRbodyId[cpt] = rbodyHEdit.GetId(g_pModelViewSDI);
                        cpt = cpt + 1;
                    }
                }
            }
        }
    }
}
///////////////////////////////////////////////////////////////////////////////////////////////////
// Evaluate connected components for all parts in the model
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalModelSDIEvaluateAllPartsConnectedComponents(int *nbComponentsPerPart)
{
        
    // Second pass: evaluate connected components for each part
    SelectionRead selPart(g_pModelViewSDI, "/PART");
    int partIndex = 0;
    
    while(selPart.Next())
    {
        unsigned int partId = selPart->GetId();
        SelectionElementRead elemSelection(*selPart);
        
        // Get Irigid value in /PART
        int Irigid = 0;
        sdiValue tempValInt(Irigid);
        bool found = selPart->GetValue(sdiIdentifier("Irigid"), tempValInt);
        if(found) tempValInt.GetValue(Irigid);
        // Check if part uses /MAT/RIGID
        int Imatrigid = 0;
        HandleRead matHread;
        selPart->GetEntityHandle(sdiIdentifier("materialid"), matHread);
            
        if(matHread.IsValid())
        {
            EntityRead matRead(g_pModelViewSDI, matHread);
            const sdiString& matKeyword = matRead.GetKeyword();
            size_t pos = matKeyword.find("/MAT/LAW13");
            if(matKeyword.find("/MAT/RIGID") != sdiString::npos || 
               (pos != sdiString::npos && (pos + 10 >= matKeyword.length() || !isdigit(matKeyword[pos + 10])))) {
                Imatrigid = 1;
            }
        }

        if(Irigid == 1 || Imatrigid == 1)
        {
            nbComponentsPerPart[partIndex] = 1;
            partIndex++;
        }
        else if(Irigid == 0)
        {
            nbComponentsPerPart[partIndex] = 0;
            partIndex++;
        }
        else if(Irigid == 2)
        {
        
          std::set<unsigned int> allPartNodes;
          std::map<unsigned int, unsigned int> nodeIdToIndex;
          std::vector<std::vector<unsigned int>> elementConnectivity;
        
        // Collect all nodes and elements
          unsigned int nodeIndex = 0;
          while(elemSelection.Next())
          {
              sdiUIntList nodeIds;
              elemSelection->GetNodeIds(nodeIds);
            
              std::vector<unsigned int> elemNodes;
              for(unsigned int nodeId : nodeIds)
              {
                  if(allPartNodes.find(nodeId) == allPartNodes.end())
                  {
                      allPartNodes.insert(nodeId);
                      nodeIdToIndex[nodeId] = nodeIndex;
                      nodeIndex++;
                  }
                  elemNodes.push_back(nodeIdToIndex[nodeId]);
              }
              elementConnectivity.push_back(elemNodes);
          }
        
          unsigned int NodesNumber = allPartNodes.size();
        
          if(NodesNumber == 0 || elementConnectivity.empty())
          {
              (nbComponentsPerPart)[partIndex] = 0;
              partIndex++;
              continue;
          }
        
          // Build adjacency graph
          std::vector<std::set<unsigned int>> adjacencyGraph(NodesNumber);
          
          for(const auto& element : elementConnectivity)
          {
              for(size_t i = 0; i < element.size(); i++)
              {
                  for(size_t j = i + 1; j < element.size(); j++)
                  {
                      unsigned int node1 = element[i];
                      unsigned int node2 = element[j];
                      adjacencyGraph[node1].insert(node2);
                      adjacencyGraph[node2].insert(node1);
                  }
              }
          }
          
          // Find connected components using DFS
          std::vector<bool> visited(NodesNumber, false);
          int componentCount = 0;
          
          for(unsigned int i = 0; i < NodesNumber; i++)
          {
              if(!visited[i])
              {
                  componentCount++;
                  std::stack<unsigned int> stack;
                  stack.push(i);
                  
                  while(!stack.empty())
                  {
                      unsigned int currentNode = stack.top();
                      stack.pop();
                      
                      if(visited[currentNode]) continue;
                      
                      visited[currentNode] = true;
                      
                      for(unsigned int neighbor : adjacencyGraph[currentNode])
                      {
                          if(!visited[neighbor])
                          {
                              stack.push(neighbor);
                          }
                      }
                  }
              }
          }
          
          (nbComponentsPerPart)[partIndex] = componentCount;
          partIndex++;
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Check if a part has elements
///////////////////////////////////////////////////////////////////////////////////////////////////
void GlobalModelSDIIsPartWithElements(unsigned int *partId, bool *hasElements)
{
    HandleRead partHread;
    if(g_pModelViewSDI->FindById(g_pModelViewSDI->GetEntityType("/PART"), *partId, partHread))
    {
        if (partHread.IsValid())
        {
            EntityRead partEntity(g_pModelViewSDI, partHread);
            SelectionElementRead elemSelection(partEntity);
            int nbElems = elemSelection.Count();
            if (nbElems > 0)
            {
                *hasElements = true;
            }
            else
            {
                *hasElements = false;
            }
        }
        else
        {
            *hasElements = false;
        }
    }
    else
    {
        *hasElements = false;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// S-ALE CARD BUILD MESH
///////////////////////////////////////////////////////////////////////////////////////////////////
static double GetCharacteristicLenght(int nb_node, double lenght, double q_ )
{
  if(q_==0.||q_==1.0||q_==-1.0)
  {
    return lenght / (nb_node - 1);
  }
  else
  {
    double geomAtLast = (1.0 - std::pow(q_, nb_node - 1)) / (1.0 - q_);
    if(std::fabs(geomAtLast) < 1e-18)
    {
        return lenght / (nb_node - 1);
    }
    return lenght / geomAtLast;
  }
}
    struct Bcs_Side
    {
      std::unordered_set<int> node_id ; // store 1 node id
      int seg_nb;
      std::vector<std::array<int,4>> facet_node_id ; // store 4 node ids

    };
    struct Bcs_Struct
    { // ale/bcs or nrf
      // ale/bcs --> grnod
      // ebcs/nrf --> surf/ext
      int bcs_nb;
      std::vector<int> avail;
      std::vector<Bcs_Side>  list;
    };
    Bcs_Struct bcs_data;
    
static void boundary_save(const int* node_list,int node_list_s,Bcs_Struct& bcs_data,int facet_id) //boundary_save(node_list,4,bcs_data,0) ;
{
  for(i=0;i<node_list_s;i++)
  {
    int node_id = node_list[i];    
    bcs_data.list[facet_id].node_id.insert(node_id) ;
  }
  std::array<int,4> local_array = {node_list[0],node_list[1],node_list[2],node_list[3]};
  bcs_data.list[facet_id].facet_node_id.push_back(local_array);  
}

// =============================================================================
// BVH (Bounding Volume Hierarchy) helper code for arbitrary surface trim (type 4)
//
// Purpose: given a trim surface defined by a list of 3-node or 4-node segments,
//          compute the signed distance from any query point P to that surface
//          efficiently using a spatial acceleration structure.
//
// Algorithm:
//   1. Each segment (3 or 4 nodes) is triangulated once at read time.
//   2. A recursive BVH is built on all triangles.  Each BVH node stores an AABB
//      (Axis-Aligned Bounding Box) covering a subset of triangles.
//   3. Nearest-point queries prune entire subtrees whose AABB is farther than
//      the current best distance (early-exit), giving O(log M) amortised cost
//      per query instead of O(M) brute-force.
//   4. The signed distance is computed with the normal of the closest triangle
//      (from segment node ordering): positive on normal side, negative opposite.
//
// Complexity: build O(M log M), query O(N log M), with N nodes, M triangles.
// =============================================================================

// --- Minimal 3D vector type used exclusively by the trim-BVH code ------------
struct TrimVec3
{
  double x, y, z;
};

static inline TrimVec3 TrimV3Add  (const TrimVec3& a, const TrimVec3& b) { return {a.x+b.x, a.y+b.y, a.z+b.z}; }
static inline TrimVec3 TrimV3Sub  (const TrimVec3& a, const TrimVec3& b) { return {a.x-b.x, a.y-b.y, a.z-b.z}; }
static inline TrimVec3 TrimV3Scale(const TrimVec3& a, double s)          { return {a.x*s,   a.y*s,   a.z*s};   }
static inline double   TrimV3Dot  (const TrimVec3& a, const TrimVec3& b) { return a.x*b.x + a.y*b.y + a.z*b.z; }
static inline double   TrimV3Norm2(const TrimVec3& v)                    { return TrimV3Dot(v, v);               }
static inline TrimVec3 TrimV3Cross(const TrimVec3& a, const TrimVec3& b)
{
    return { a.y*b.z - a.z*b.y,
                     a.z*b.x - a.x*b.z,
                     a.x*b.y - a.y*b.x };
}

// Normalize v; if almost zero, return the Z axis as a safe fallback.
static inline TrimVec3 TrimV3Normalize(const TrimVec3& v)
{
  const double n = std::sqrt(TrimV3Norm2(v));
  if (n < 1.0e-30) return {0.0, 0.0, 1.0};
  return TrimV3Scale(v, 1.0 / n);
}

// --- Triangle (3 vertices) ---------------------------------------------------
struct TrimTriangle
{
  TrimVec3 a, b, c;
  TrimVec3 barycenter; // centroid of the parent solid element; used to orient the normal outward
  bool     hasBary;    // true only for triangles coming from solid (/BRICK, /TETRA) elements
};

// --- Axis Aligned Bounding Box -----------------------------------------------
struct TrimAABB
{
  TrimVec3 bmin; // minimum corner (per axis)
  TrimVec3 bmax; // maximum corner (per axis)
};

// --- BVH node: either an internal node (two children) or a leaf --------------
struct TrimBVHNode
{
  TrimAABB box;      // bounding box of all triangles in this subtree
  int      left;     // index of left  child node (-1 if leaf)
  int      right;    // index of right child node (-1 if leaf)
  int      firstTri; // index of the first triangle in triIndices[] (leaf only)
  int      triCount; // number of triangles in this leaf             (leaf only)
  bool     isLeaf;   // true = leaf node, false = internal node
};

// --- Result of a nearest-point BVH query -------------------------------------
struct TrimClosestHit
{
  double   dist2;        // squared distance to the closest point on the surface
  TrimVec3 closestPoint; // coordinates of that closest point
  int      triIndex;     // index of the triangle that owns the closest point
};

// --- Full BVH structure ------------------------------------------------------
struct TrimSurfaceBVH
{
  std::vector<TrimTriangle> triangles;  // all triangles of the trim surface
  std::vector<int>          triIndices; // permutation array: BVH reorders triangles
                                        // without copying them
  std::vector<TrimBVHNode>  nodes;      // all BVH nodes; root is at index 0
};

// --- Squared distance from point P to AABB -----------------------------------
// Returns 0 if P is inside the box. Used to prune BVH branches that cannot
// improve the current best distance.
static double TrimAABBDist2(const TrimVec3& p, const TrimAABB& box)
{
  // For each axis: accumulate the squared gap if P lies outside the slab.
  double dx = 0.0;
  if      (p.x < box.bmin.x) dx = box.bmin.x - p.x;
  else if (p.x > box.bmax.x) dx = p.x - box.bmax.x;

  double dy = 0.0;
  if      (p.y < box.bmin.y) dy = box.bmin.y - p.y;
  else if (p.y > box.bmax.y) dy = p.y - box.bmax.y;

  double dz = 0.0;
  if      (p.z < box.bmin.z) dz = box.bmin.z - p.z;
  else if (p.z > box.bmax.z) dz = p.z - box.bmax.z;

  return dx*dx + dy*dy + dz*dz;
}

// --- Closest point on triangle T to query point P ----------------------------
// Uses the Eberly parametric-barycentric algorithm (Voronoi region classification).
// Handles all 7 regions: 3 vertices, 3 edges, 1 interior face.
static TrimVec3 TrimClosestPointOnTri(const TrimVec3& p, const TrimTriangle& t)
{
  const TrimVec3 ab = TrimV3Sub(t.b, t.a);
  const TrimVec3 ac = TrimV3Sub(t.c, t.a);
  const TrimVec3 ap = TrimV3Sub(p,   t.a);

  const double d1 = TrimV3Dot(ab, ap);
  const double d2 = TrimV3Dot(ac, ap);
  if (d1 <= 0.0 && d2 <= 0.0) return t.a; // Voronoi region of vertex A

  const TrimVec3 bp = TrimV3Sub(p, t.b);
  const double d3 = TrimV3Dot(ab, bp);
  const double d4 = TrimV3Dot(ac, bp);
  if (d3 >= 0.0 && d4 <= d3) return t.b; // Voronoi region of vertex B

  // Voronoi region of edge AB
  const double vc = d1*d4 - d3*d2;
  if (vc <= 0.0 && d1 >= 0.0 && d3 <= 0.0)
  {
    const double v = d1 / (d1 - d3);
    return TrimV3Add(t.a, TrimV3Scale(ab, v));
  }

  const TrimVec3 cp = TrimV3Sub(p, t.c);
  const double d5 = TrimV3Dot(ab, cp);
  const double d6 = TrimV3Dot(ac, cp);
  if (d6 >= 0.0 && d5 <= d6) return t.c; // Voronoi region of vertex C

  // Voronoi region of edge AC
  const double vb = d5*d2 - d1*d6;
  if (vb <= 0.0 && d2 >= 0.0 && d6 <= 0.0)
  {
    const double w = d2 / (d2 - d6);
    return TrimV3Add(t.a, TrimV3Scale(ac, w));
  }

  // Voronoi region of edge BC
  const double va = d3*d6 - d5*d4;
  if (va <= 0.0 && (d4 - d3) >= 0.0 && (d5 - d6) >= 0.0)
  {
    const TrimVec3 bc = TrimV3Sub(t.c, t.b);
    const double w = (d4 - d3) / ((d4 - d3) + (d5 - d6));
    return TrimV3Add(t.b, TrimV3Scale(bc, w));
  }

  // Voronoi region of the face (interior): barycentric projection
  const double denom = 1.0 / (va + vb + vc);
  const double v = vb * denom;
  const double w = vc * denom;
  return TrimV3Add(TrimV3Add(t.a, TrimV3Scale(ab, v)), TrimV3Scale(ac, w));
}

// --- Recursive BVH construction for one node ---------------------------------
// Processes sub-array triIndices[first .. first+count-1].
// Strategy: split on the longest axis of the centroid bounding box (median-split).
// Leaf threshold: <= 8 triangles (good balance between tree depth and leaf cost).
static int TrimBuildNode(TrimSurfaceBVH& bvh, int first, int count)
{
  TrimBVHNode node;
  node.left     = -1;
  node.right    = -1;
  node.firstTri = first;
  node.triCount = count;
  node.isLeaf   = false;

  // Compute (a) the vertex bounding box for this node's AABB, and
  //         (b) the centroid bounding box to choose the best split axis.
  TrimAABB box         = { { 1.0e30, 1.0e30, 1.0e30 }, {-1.0e30,-1.0e30,-1.0e30} };
  TrimAABB centroidBox = { { 1.0e30, 1.0e30, 1.0e30 }, {-1.0e30,-1.0e30,-1.0e30} };
  for (int i = 0; i < count; ++i)
  {
    const TrimTriangle& tri = bvh.triangles[bvh.triIndices[first + i]];
    const TrimVec3 pts[3]   = { tri.a, tri.b, tri.c };

    // Expand the vertex AABB to contain all 3 vertices
    for (int k = 0; k < 3; ++k)
    {
      if (pts[k].x < box.bmin.x) box.bmin.x = pts[k].x;
      if (pts[k].y < box.bmin.y) box.bmin.y = pts[k].y;
      if (pts[k].z < box.bmin.z) box.bmin.z = pts[k].z;
      if (pts[k].x > box.bmax.x) box.bmax.x = pts[k].x;
      if (pts[k].y > box.bmax.y) box.bmax.y = pts[k].y;
      if (pts[k].z > box.bmax.z) box.bmax.z = pts[k].z;
    }

    // Centroid of this triangle
    const TrimVec3 c = { (tri.a.x + tri.b.x + tri.c.x) / 3.0,
                         (tri.a.y + tri.b.y + tri.c.y) / 3.0,
                         (tri.a.z + tri.b.z + tri.c.z) / 3.0 };
    if (c.x < centroidBox.bmin.x) centroidBox.bmin.x = c.x;
    if (c.y < centroidBox.bmin.y) centroidBox.bmin.y = c.y;
    if (c.z < centroidBox.bmin.z) centroidBox.bmin.z = c.z;
    if (c.x > centroidBox.bmax.x) centroidBox.bmax.x = c.x;
    if (c.y > centroidBox.bmax.y) centroidBox.bmax.y = c.y;
    if (c.z > centroidBox.bmax.z) centroidBox.bmax.z = c.z;
  }
  node.box = box;

  // Add this node to the array BEFORE recursing so children can update its indices.
  // Note: indexing by integer (not by pointer) is safe even after vector reallocation.
  const int nodeIndex = (int)bvh.nodes.size();
  bvh.nodes.push_back(node);
  // Leaf condition: few enough triangles to test individually
  if (count <= 8)
  {
    bvh.nodes[nodeIndex].isLeaf = true;
    return nodeIndex;
  }

  // Choose split axis = largest dimension of the centroid bounding box
  const double extX = centroidBox.bmax.x - centroidBox.bmin.x;
  const double extY = centroidBox.bmax.y - centroidBox.bmin.y;
  const double extZ = centroidBox.bmax.z - centroidBox.bmin.z;
  int axis = 0;
  if (extY > extX && extY >= extZ) axis = 1;
  else if (extZ > extX && extZ >= extY) axis = 2;

    // Median split: keeps recursion depth bounded even for skewed centroid
    // distributions, unlike split-position partitioning which can become linear.
        const int half = count / 2;
        const int leftCount  = half;
        const int rightCount = count - half;
        if (leftCount <= 0 || rightCount <= 0)
        {
            // Defensive fallback: keep this node as a leaf if the split is degenerate.
            bvh.nodes[nodeIndex].isLeaf = true;
            return nodeIndex;
        }
    std::nth_element(
        bvh.triIndices.begin() + first,
        bvh.triIndices.begin() + first + half,
        bvh.triIndices.begin() + first + count,
        [&](int lhsIdx, int rhsIdx)
        {
            const TrimTriangle& lhs = bvh.triangles[lhsIdx];
            const TrimTriangle& rhs = bvh.triangles[rhsIdx];
            const double l = (axis == 0) ? (lhs.a.x + lhs.b.x + lhs.c.x) / 3.0 :
                                             (axis == 1) ? (lhs.a.y + lhs.b.y + lhs.c.y) / 3.0 :
                                                                         (lhs.a.z + lhs.b.z + lhs.c.z) / 3.0;
            const double r = (axis == 0) ? (rhs.a.x + rhs.b.x + rhs.c.x) / 3.0 :
                                             (axis == 1) ? (rhs.a.y + rhs.b.y + rhs.c.y) / 3.0 :
                                                                         (rhs.a.z + rhs.b.z + rhs.c.z) / 3.0;
            return l < r;
        });
        const int mid = first + half;
  // Recursively build children.
  // IMPORTANT: the two TrimBuildNode calls must be stored in local variables
  // BEFORE being assigned into bvh.nodes[nodeIndex].  If they were written as
  //   bvh.nodes[nodeIndex].left = TrimBuildNode(...);
  // the compiler (C++11/14) could evaluate the LHS address *before* the call,
  // capture a stale pointer, and then the push_back inside the recursive call
  // would reallocate the vector - writing the result to freed memory while
  // bvh.nodes[nodeIndex] itself (in the now-moved data) keeps its initial -1.
  const int leftIdx  = TrimBuildNode(bvh, first, leftCount);
  const int rightIdx = TrimBuildNode(bvh, mid,   rightCount);
  bvh.nodes[nodeIndex].left  = leftIdx;
  bvh.nodes[nodeIndex].right = rightIdx;
  return nodeIndex;
}

// --- Entry point: build the BVH from bvh.triangles ---------------------------
static void TrimBuildBVH(TrimSurfaceBVH& bvh)
{
  const int n = (int)bvh.triangles.size();
  // Initialize the permutation array to the identity permutation
  bvh.triIndices.resize(n);
  for (int i = 0; i < n; ++i) bvh.triIndices[i] = i;
  bvh.nodes.clear();
  // Reserve an upper bound (2*n nodes for n triangles) so that push_back
  // inside TrimBuildNode never triggers a reallocation - this is an extra
  // safety net on top of the local-variable fix above.
  bvh.nodes.reserve(2 * n + 1);
  if (n > 0) TrimBuildNode(bvh, 0, n);
}

// --- Recursive nearest-point query on the BVH --------------------------------
// Walks the tree depth-first, visiting the nearer child first for better pruning.
// Updates `best` whenever a closer point is found.
static void TrimQueryBVH(const TrimSurfaceBVH& bvh, int nodeIdx,
                         const TrimVec3& p, TrimClosestHit& best)
{
  const TrimBVHNode& node = bvh.nodes[nodeIdx];

  // Early exit: the closest possible point in this subtree is farther than
  // the current best; no triangle here can improve the answer.
  if (TrimAABBDist2(p, node.box) >= best.dist2) return;
  
  if (node.isLeaf)
  {
    // Leaf: test each triangle individually
    for (int i = 0; i < node.triCount; ++i)
    {
      const int      idx  = bvh.triIndices[node.firstTri + i];
      const TrimVec3 c    = TrimClosestPointOnTri(p, bvh.triangles[idx]);
      const double   d2   = TrimV3Norm2(TrimV3Sub(p, c));
      if (d2 < best.dist2)
      {
        best.dist2        = d2;
        best.closestPoint = c;
        best.triIndex     = idx;
      }
    }
    return;
  }

    if(node.left < 0 || node.right < 0)
    {
        return;
    }

  // Internal node: visit the nearer child first to tighten the bound earlier
  const double dL = TrimAABBDist2(p, bvh.nodes[node.left ].box);
  const double dR = TrimAABBDist2(p, bvh.nodes[node.right].box);
  if (dL < dR)
  {
    TrimQueryBVH(bvh, node.left,  p, best);
    TrimQueryBVH(bvh, node.right, p, best);
  }
  else
  {
    TrimQueryBVH(bvh, node.right, p, best);
    TrimQueryBVH(bvh, node.left,  p, best);
  }
}

// --- Compute signed distance from P to the arbitrary trim surface ------------
// Sign convention based on the user-defined segment orientation:
//   - For the closest triangle, compute its oriented normal from vertex order.
//   - sd > 0 when (P - projection) points to the same side as that normal.
//   - sd < 0 for the opposite side.
// This avoids any dependency on a global direction vector.
static double TrimSignedDist(const TrimSurfaceBVH& bvh, const TrimVec3& p)
{
  if (bvh.nodes.empty()) return 0.0;

  TrimClosestHit hit;
  hit.dist2        = 1.0e60; // initialise to a very large distance
  hit.triIndex     = -1;
  hit.closestPoint = p; // safe fallback
  TrimQueryBVH(bvh, 0, p, hit);
  if (hit.triIndex < 0) return 0.0;

    // Get the nearest triangle and derive its oriented normal from vertex order.
    // The orientation comes from the segment node ordering provided by user.
    const TrimTriangle& tri = bvh.triangles[hit.triIndex];
    const TrimVec3 edge1 = TrimV3Sub(tri.b, tri.a); // get the first edge
    const TrimVec3 edge2 = TrimV3Sub(tri.c, tri.a); // get the second edge
    const TrimVec3 normalUnit = TrimV3Normalize(TrimV3Cross(edge1, edge2)); // get the normal : edge1 ^ edge2

  const TrimVec3 delta = TrimV3Sub(p, hit.closestPoint); // node - closest point
  const double   dist  = std::sqrt(hit.dist2); // distance from P to closest point
    // Positive sign if P is on normal side; negative for opposite side.
    // For degenerate triangles (normal ~ 0), this falls back to +1 by design.
  const double proj = TrimV3Dot(delta, normalUnit);
  const double sign = (proj >= 0.0) ? 1.0 : -1.0;
  return sign * dist;
}

// --- Triangulate a 3-node or 4-node segment and append to a triangle list ----
// 3 nodes  ->  1 triangle.
// 4 nodes  ->  2 triangles; split along the shorter diagonal to avoid
//              degenerate triangles on non-planar quads.
static void TrimAddSegment(const TrimVec3 pts[], int nPts,
                            std::vector<TrimTriangle>& triangles)
{
  if (nPts == 3)
  {
    triangles.push_back({ pts[0], pts[1], pts[2] });
  }
  else if (nPts == 4)
  {
    // Compare diagonals 0-2 and 1-3; split along the shorter one.
    const double d02 = TrimV3Norm2(TrimV3Sub(pts[2], pts[0]));
    const double d13 = TrimV3Norm2(TrimV3Sub(pts[3], pts[1]));
    if (d02 <= d13)
    {
      // Split along diagonal 0-2: triangles (0,1,2) and (0,2,3)
      triangles.push_back({ pts[0], pts[1], pts[2] });
      triangles.push_back({ pts[0], pts[2], pts[3] });
    }
    else
    {
      // Split along diagonal 1-3: triangles (0,1,3) and (1,2,3)
      triangles.push_back({ pts[0], pts[1], pts[3] });
      triangles.push_back({ pts[1], pts[2], pts[3] });
    }
  }
}




    // Basic 3D dot product helpers used for local-axis projections.
    static double Dot3(const double a[3], const double b[3])
    {
      return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
    }

    static double Dot3(const sdiTriple &a, const double b[3])
    {
      return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
    }

    static double Dot3(const sdiTriple &a, const sdiTriple &b)
    {
      return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
    }

    // Normalize a vector in place and return its original norm.
    static double Normalize3(double v[3])
    {
      const double norm = std::sqrt(Dot3(v, v));
      if (norm > 0.0)
      {
        v[0] /= norm;
        v[1] /= norm;
        v[2] /= norm;
      }
      return norm;
    }

    static void Cross3(const double a[3], const double b[3], double out[3])
    {
      out[0] = a[1] * b[2] - a[2] * b[1];
      out[1] = a[2] * b[0] - a[0] * b[2];
      out[2] = a[0] * b[1] - a[1] * b[0];
    }

    // Bound check that is robust to reversed corner ordering and floating-point noise.
    static bool IsValueInsideBounds(double value, double bound0, double bound1, double tol)
    {
      const double lower = std::min(bound0, bound1) - tol;
      const double upper = std::max(bound0, bound1) + tol;
      return (value >= lower && value <= upper);
    }

    // Test if a point is outside an oriented 3D box defined by:
    // - two opposite corners (corner0, corner1)
    // - three box local axes coming from the skew system.
    static bool IsPointOutsideOrientedBox(const sdiTriple &point,
                                          const sdiTriple &corner0,
                                          const sdiTriple &corner1,
                                          const double axis_x_in[3],
                                          const double axis_y_in[3],
                                          const double axis_z_in[3],
                                          double edge_tol = 1.0e-8)
    {
      double axis_x[3] = {axis_x_in[0], axis_x_in[1], axis_x_in[2]};
      double axis_y[3] = {axis_y_in[0], axis_y_in[1], axis_y_in[2]};
      double axis_z[3] = {axis_z_in[0], axis_z_in[1], axis_z_in[2]};

      // Invalid basis: consider the point as outside to avoid false positives.
      if (Normalize3(axis_x) == 0.0 || Normalize3(axis_y) == 0.0 || Normalize3(axis_z) == 0.0)
      {
        return true;
      }

      sdiTriple diagonal(corner1[0] - corner0[0],
                         corner1[1] - corner0[1],
                         corner1[2] - corner0[2]);
      sdiTriple rel(point[0] - corner0[0],
                    point[1] - corner0[1],
                    point[2] - corner0[2]);

      // Box extents in the local frame.
      const double box_x = Dot3(diagonal, axis_x);
      const double box_y = Dot3(diagonal, axis_y);
      const double box_z = Dot3(diagonal, axis_z);

      // Point coordinates in the same local frame.
      const double point_x = Dot3(rel, axis_x);
      const double point_y = Dot3(rel, axis_y);
      const double point_z = Dot3(rel, axis_z);

      if (!IsValueInsideBounds(point_x, 0.0, box_x, edge_tol))
      {
        return true;
      }

      if (!IsValueInsideBounds(point_y, 0.0, box_y, edge_tol))
      {
        return true;
      }

      if (!IsValueInsideBounds(point_z, 0.0, box_z, edge_tol))
      {
        return true;
      }
      return false;
    }

    // Test if a point is outside a sphere defined by:
    // - a origin (origin)
    // - a radius (radius)
    static bool IsPointOutsideSphere(const sdiTriple &point,
                                     const sdiTriple &origin,
                                      const double radius_pow_2)
    {
      double dist2 = (point[0] - origin[0])*(point[0] - origin[0]) + 
                     (point[1] - origin[1])*(point[1] - origin[1]) +
                     (point[2] - origin[2])*(point[2] - origin[2]) ;
      dist2 = dist2 - radius_pow_2 ;
      if(dist2>0.) return true; // outside
      else return false; // inside or on the sphere
    }

        // Test if a point is outside a finite truncated cone ("cylinder" option)
        // defined by:
        // - two axis points (pos_1 -> pos_2)
        // - one radius at each end (radius_1, radius_2).
        static bool IsPointOutsideCylinder(const sdiTriple &point,
                                           const sdiTriple &pos_1,
                                           const sdiTriple &pos_2,
                                           double radius_1,
                                           double radius_2)
        {
            const double edge_tol = 1.0e-8;

            if (radius_1 < 0.0) radius_1 = -radius_1;
            if (radius_2 < 0.0) radius_2 = -radius_2;

            const sdiTriple axis(pos_2[0] - pos_1[0],pos_2[1] - pos_1[1],pos_2[2] - pos_1[2]);
            const double axis_len2 = Dot3(axis, axis);

            // Degenerate axis: conservative fallback to a sphere test around pos_1.
            if (axis_len2 <= 1.0e-20)
            {
                const double r = std::max(radius_1, radius_2);
                const double radius_pow_2 = (r + edge_tol) * (r + edge_tol);
                return IsPointOutsideSphere(point, pos_1, radius_pow_2);
            }

            const sdiTriple rel(point[0] - pos_1[0],point[1] - pos_1[1],point[2] - pos_1[2]);

            // Normalized curvilinear coordinate along the axis segment.
            const double t = Dot3(rel, axis) / axis_len2;
            if (t < -edge_tol || t > 1.0 + edge_tol)
            {
                return true;
            }

            const double t_clamped = std::max(0.0, std::min(1.0, t));
            const sdiTriple axis_point(pos_1[0] + t_clamped * axis[0],pos_1[1] + t_clamped * axis[1],pos_1[2] + t_clamped * axis[2]);

            const sdiTriple radial(point[0] - axis_point[0],point[1] - axis_point[1],point[2] - axis_point[2]);
            const double radial_dist2 = Dot3(radial, radial);

            const double radius = radius_1 + t_clamped * (radius_2 - radius_1);
            const double radius_with_tol = radius + edge_tol;
            const double radius_pow_2 = radius_with_tol * radius_with_tol;

            if(radial_dist2 > radius_pow_2) return true; // outside
            else return false; // inside or on the truncated cone
        }

    // Test if a point is outside a plane defined by:
    // - a origin (origin)
    // - a normal (normal)
    static bool IsPointOutsidePlane(const sdiTriple &point,
                                    const sdiTriple &origin,
                                    const sdiTriple normal)
    {
      double dist = (point[0] - origin[0])*normal[0] + 
                    (point[1] - origin[1])*normal[1] +
                    (point[2] - origin[2])*normal[2];
      if(dist>0.) return true; // outside
      else return false; // inside or on the plane
    }        



static double SolveGeometricQ_NewtonHybrid(double a, int nNodes)
{
    const int m = nNodes - 1; // element's number
    if (m <= 0) throw std::invalid_argument("nNodes must be >= 2");
    if (a <= 0.0 || a >= 1.0) throw std::invalid_argument("a must be in (0,1)");

    const double am = 1.0 / static_cast<double>(m);
    const double tol = 1e-12;
    const int maxNewton = 40;
    const int maxBisect = 80;

    // Cas uniforme
    if (std::fabs(a - am) < tol) return 1.0;

    auto g = [a, m](double q) -> double {
        return a * std::pow(q, m) - q + (1.0 - a);
    };
    auto gp = [a, m](double q) -> double {
        return a * static_cast<double>(m) * std::pow(q, m - 1) - 1.0;
    };

    // Bracket according to the regime
    double lo = 0.0, hi = 0.0;
    if (a < am) {
        // q > 1 (increasing size if u0 is the smallest)
        lo = 1.0 + 1e-10;
        hi = 2.0;
        double glo = g(lo), ghi = g(hi);
        while (glo * ghi > 0.0) {
            hi *= 2.0;
            ghi = g(hi);
            if (hi > 1e12) throw std::runtime_error("Cannot bracket root for q>1");
        }
    } else {
        // 0 < q < 1
        lo = 1e-12;
        hi = 1.0 - 1e-10;
        double glo = g(lo), ghi = g(hi);
        if (glo * ghi > 0.0) throw std::runtime_error("Cannot bracket root for q<1");
    }

    // Apply Newton's method within the bracket
    double q = 0.5 * (lo + hi);
    for (int it = 0; it < maxNewton; ++it) {
        const double val = g(q);
        if (std::fabs(val) < tol) return q;

        const double der = gp(q);
        double qNew = std::numeric_limits<double>::quiet_NaN();

        // No Newton step if derivative is too small
        if (std::fabs(der) > 1e-14) qNew = q - val / der;

        // If the step goes out of the bracket, fallback to bisection
        if (!(qNew > lo && qNew < hi) || !std::isfinite(qNew)) {
            qNew = 0.5 * (lo + hi);
        }

        // Update the bracket
        const double gLo = g(lo);
        const double gNew = g(qNew);
        if (gLo * gNew <= 0.0) hi = qNew;
        else lo = qNew;

        q = qNew;
        if (std::fabs(hi - lo) < tol * (1.0 + std::fabs(q))) return q;
    }

    // final dichotomy if Newton did not converge
    for (int it = 0; it < maxBisect; ++it) {
        const double mid = 0.5 * (lo + hi);
        const double gLo = g(lo);
        const double gMid = g(mid);

        if (std::fabs(gMid) < tol) return mid;
        if (gLo * gMid <= 0.0) hi = mid;
        else lo = mid;
    }

    return 0.5 * (lo + hi);
}
struct FaceKey
  {
      std::array<int, 4> ids{{0, 0, 0, 0}};
      int n = 0; // 3 ou 4

      bool operator==(const FaceKey& other) const
      {
          return n == other.n && ids == other.ids;
      }
  };

struct FaceKeyHash
  {
      std::size_t operator()(const FaceKey& k) const
      {
          // Combinaison simple et robuste
          std::size_t h = static_cast<std::size_t>(k.n);
          for (int i = 0; i < 4; ++i)
          {
              const std::size_t x = static_cast<std::size_t>(k.ids[i]);
              h ^= x + 0x9e3779b97f4a7c15ULL + (h << 6) + (h >> 2);
          }
          return h;
      }
  };

static FaceKey MakeFaceKey(const std::vector<int>& face)
  {
      FaceKey k;
      k.n = static_cast<int>(face.size());

      std::vector<int> tmp(face.begin(), face.end());
      std::sort(tmp.begin(), tmp.end());

      for (int i = 0; i < k.n; ++i) k.ids[i] = tmp[i];
      return k;
  }
    struct FaceCounter
    {
        int count = 0;
        std::vector<int> sample;
        TrimVec3 barycenter = {0.0, 0.0, 0.0}; // centroid of the solid element that owns this face
    };  

// **************************************************************************

void GlobalEntitySDISAleBuildMesh(int* message_value)
{
  // message_value (c convention): 
  // 0 : input --> if ==1, activation of debug messages
  // 0 : part id related to s-ale option
  // 1-->3 : control point id
  // 4-->6 : number of sub-area of control point
  // 7-->9: kind of mesh (regular of refined)
  // 10 : number of trimming
  // 11-->13 : number of element in each direction (without trimming)
  // 14 : total number of created elements (with trimming)
  // 15-->17 : number of node in each direction (without trimming)
  // 18 : total number of created nodes (with trimming)
  // 19-->24: boundary condition id
  // 25-->30: /BCS id
  // 31-->35: boundary condition type
  // 37 : not ok
  // 38 : material law type (6 for hydro or hydro-viscous, 151 for multifluid, 51 for multimaterial, -1 for not supported)
  // 39 : material law id
  // 40 : element offset
  // 41 : node offset
  // 42 : s-ale id
  // 43 : reference node id
  // 44 : /ALE/LINK/VEL id (if the reference node is != 0)

    bool isOk = false;
    
    sdiValueEntity part;
    sdiValue tempVal(part);

    EntityType radPartType = g_pModelViewSDI->GetEntityType("/PART");
    EntityType radPropType = g_pModelViewSDI->GetEntityType("/PROP");
    EntityType radMatType = g_pModelViewSDI->GetEntityType("/MAT");
    EntityType radNodeType = g_pModelViewSDI->GetEntityType("/NODE");
    EntityType radBrickType = g_pModelViewSDI->GetEntityType("/BRICK");
    EntityType radBrick16Type = g_pModelViewSDI->GetEntityType("/BRICK16");
    EntityType radBrick20Type = g_pModelViewSDI->GetEntityType("/BRICK20");        
    EntityType radTetra4Type = g_pModelViewSDI->GetEntityType("/TETRA4");
    EntityType radTetra10Type = g_pModelViewSDI->GetEntityType("/TETRA10");        
    EntityType radSegmentType = g_pModelViewSDI->GetEntityType("/SURF/SEG");
    EntityType radSkewType = g_pModelViewSDI->GetEntityType("/SKEW");
    EntityType radBoxType = g_pModelViewSDI->GetEntityType("/BOX/RECTA");
    EntityType radGrnodType = g_pModelViewSDI->GetEntityType("/GRNOD/NODE");

    // Get material and property from INITIAL_VOLUME_FRACTION
    double nodeX,nodeY,nodeZ;
    double u0_v,u0_w,u0_u;
    double q_v,q_w,q_u;
    int MatId = 0;
    int PropId = 0;
    sdiString law_type ;
    sdiValue tempValDouble ;
    int sale_id = 0;
    tempVal = sdiValue(part);
    sale_id = (int)g_pEntity->GetId();
    bool debug_activation = false;
    debug_activation = (message_value[0] == 1); // activation of debug messages if message_value[0] == 1
    message_value[42] = sale_id; // save the s-ale id
    

    int sale_part_id = -1 ;
    tempVal = sdiValue(part);
    bool found = g_pEntity->GetValue(sdiIdentifier("PART_ID"), tempVal);
    tempVal.GetValue(part);
    sale_part_id = part.GetId();
    bool law151 = false ;
    bool law51 = false ;
    bool law6 = false ;    
    if(debug_activation) printf ("S-ALE BUILD MESH PART ID = %d \n", sale_part_id);

    SelectionRead parts(g_pModelViewSDI, "/PART");
    message_value[37] = -1; // not ok --> invalid part id break
    while(parts.Next())
    {
        if ((int)parts->GetId() != sale_part_id) continue;
         message_value[37] = 0; // ok part id found

        HandleRead matHread;
        HandleRead propHread;
        parts->GetEntityHandle(sdiIdentifier("materialid"), matHread);
        parts->GetEntityHandle(sdiIdentifier("propertyid"), propHread);

        if(matHread.IsValid())
        {
          EntityRead matRead(g_pModelViewSDI, matHread);
          MatId = matHread.GetId(g_pModelViewSDI);          
          law_type = matRead.GetKeyword() ;
          if(law_type=="/MAT/LAW151"||law_type=="/MAT/MULTIFLUID")
          {
            law151 = true;
            message_value[39] = MatId;
          }
          if(law_type=="/MAT/LAW51"||law_type=="/MAT/MULTIMAT")
          {
            law51 = true;
            message_value[39] = MatId;
          }
          if(law_type=="/MAT/LAW6"||law_type=="/MAT/HYDRO"||law_type=="/MAT/HYD_VISC")
          {
            law6 = true;
            message_value[39] = MatId;
          }
          if(debug_activation) printf("Law Type %s \n",law_type.c_str());
        }
        if(propHread.IsValid()) PropId = propHread.GetId(g_pModelViewSDI);
        break;
    }
    message_value[0] = sale_part_id; // save the Structured ALE part id
    if(message_value[37]<0)
    {      
      if(debug_activation) printf ("S-ALE BUILD MESH PART ID not found, break \n");
      return;
    }
    if(debug_activation) printf ("S-ALE BUILD MESH PART ID = %d with mat %d & Prop %d \n",sale_part_id, MatId, PropId);
        
    if(law6)   {
      message_value[38] = 6; // BC type 6 for hydro or hydro-viscous material
    }
     else if(law151)
    {
      message_value[38] = 151; // BC type 151 for multifluid material
    }
    else if(law51)
    {
      message_value[38] = 51; // BC type 51 for multimaterial
    }
     else
    {
      message_value[38] = -1; // not supported material law for now
      message_value[37] = -39; // not ok --> -39 means non supported material law
      if(debug_activation) printf ("S-ALE BUILD MESH PART ID = %d with mat %d & Prop %d not supported for now, break \n",sale_part_id, MatId, PropId);
      return;
    }

    sdiUIntList control_point;
    control_point.resize(3);
    int control_id = 0 ;
    tempVal = sdiValue(control_id);
    found = g_pEntity->GetValue(sdiIdentifier("CONTROL_POINT_X"), tempVal);
    if(found) tempVal.GetValue(control_id);
    message_value[1] = control_id; // save the control point id x
    if(debug_activation) printf ("S-ALE BUILD MESH PART ID = %d with control point id x = %d \n",sale_part_id, control_id);
    control_point[0] = control_id;
    tempVal = sdiValue(control_id);
    found = g_pEntity->GetValue(sdiIdentifier("CONTROL_POINT_Y"), tempVal);
    if(found) tempVal.GetValue(control_id);
    message_value[2] = control_id; // save the control point id y
    if(debug_activation) printf ("S-ALE BUILD MESH PART ID = %d with control point id y = %d \n",sale_part_id, control_id);
    control_point[1] = control_id;
    
    tempVal = sdiValue(control_id);
    found = g_pEntity->GetValue(sdiIdentifier("CONTROL_POINT_Z"), tempVal);
    if(found) tempVal.GetValue(control_id);  
    message_value[3] = control_id; // save the control point id z
    if(debug_activation) printf ("S-ALE BUILD MESH PART ID = %d with control point id z = %d \n",sale_part_id, control_id);
    control_point[2] = control_id;
    if(debug_activation) printf("S-ALE BUILD MESH PART ID = %d with control points : (%d, %d, %d) \n",sale_part_id, control_point[0], control_point[1], control_point[2]);

    // Reference node
    message_value[43] = 0; // default value for reference node id (0 means no reference node)
    int ref_node_id = 0 ;
    tempVal = sdiValue(ref_node_id);
    found = g_pEntity->GetValue(sdiIdentifier("NODE_ID"), tempVal);
    if(found) tempVal.GetValue(ref_node_id);
    message_value[43] = ref_node_id; // reference node id
    if(debug_activation) printf ("S-ALE BUILD MESH PART ID = %d with reference node id = %d \n",sale_part_id, ref_node_id);
    
    int skew_id = 0;
    int element_offset = 0;
    int node_offset = 0;    
    tempVal = sdiValue(skew_id);
    found = g_pEntity->GetValue(sdiIdentifier("SKEW_ID"), tempVal);
    if(found) tempVal.GetValue(skew_id);
    tempVal = sdiValue(element_offset);
    found = g_pEntity->GetValue(sdiIdentifier("ELEM_OFFSET"), tempVal);
    if(found) tempVal.GetValue(element_offset);    
    tempVal = sdiValue(node_offset);
    found = g_pEntity->GetValue(sdiIdentifier("NODE_OFFSET"), tempVal);
    if(found) tempVal.GetValue(node_offset);        
    if(debug_activation) printf ("S-ALE BUILD MESH PART ID with skew id = %d , element offset = %d , node_offset = %d \n", skew_id, element_offset,node_offset);
    
    bool foundSkew = false;

    sdiValueEntity n1, n2, n3;
    SelectionRead skews(g_pModelViewSDI, "/SKEW");

    // find the skew & get its data (origin and two vectors to define local coordinate system)
    int origin_node_id;
    double  origin_x = 0.0, origin_y = 0.0, origin_z = 0.0;
    double  vector_y_x = 0.0, vector_y_y = 0.0, vector_y_z = 0.0;
    double  vector_z_x = 0.0, vector_z_y = 0.0, vector_z_z = 0.0;
    while (skews.Next())
    {
        if ((int)skews->GetId() != skew_id) continue;

        foundSkew = true;

        tempValDouble = sdiValue(origin_x);
        found = skews->GetValue(sdiIdentifier("globaloriginx"), tempValDouble);
        if(found) tempValDouble.GetValue(origin_x);
        tempValDouble = sdiValue(origin_y);
        found = skews->GetValue(sdiIdentifier("globaloriginy"), tempValDouble);
        if(found) tempValDouble.GetValue(origin_y);
        tempValDouble = sdiValue(origin_z);
        found = skews->GetValue(sdiIdentifier("globaloriginz"), tempValDouble);
        if(found) tempValDouble.GetValue(origin_z);                
        if(debug_activation) printf("SKEW origin : (%f,%f,%f) \n", origin_x, origin_y, origin_z);

        tempValDouble = sdiValue(vector_y_x);
        found = skews->GetValue(sdiIdentifier("globalyaxisx"), tempValDouble);
        if(found) tempValDouble.GetValue(vector_y_x);
        tempValDouble = sdiValue(vector_y_y);
        found = skews->GetValue(sdiIdentifier("globalyaxisy"), tempValDouble);
        if(found) tempValDouble.GetValue(vector_y_y);
        tempValDouble = sdiValue(vector_y_z);
        found = skews->GetValue(sdiIdentifier("globalyaxisz"), tempValDouble);
        if(found) tempValDouble.GetValue(vector_y_z);                
        if(debug_activation) printf("SKEW vector Y : (%f,%f,%f) \n", vector_y_x, vector_y_y, vector_y_z);  

        tempValDouble = sdiValue(vector_z_x);
        found = skews->GetValue(sdiIdentifier("globalzaxisx"), tempValDouble);
        if(found) tempValDouble.GetValue(vector_z_x);
        tempValDouble = sdiValue(vector_z_y);
        found = skews->GetValue(sdiIdentifier("globalzaxisy"), tempValDouble);
        if(found) tempValDouble.GetValue(vector_z_y);
        tempValDouble = sdiValue(vector_z_z);
        found = skews->GetValue(sdiIdentifier("globalzaxisz"), tempValDouble);
        if(found) tempValDouble.GetValue(vector_z_z);                
        if(debug_activation) printf("SKEW vector Z : (%f,%f,%f) \n", vector_z_x, vector_z_y, vector_z_z);          

        break;
    }
    
    bool foundtrim = false ;
    int s_ale_id = g_pEntity->GetId();
    int mesh_trim_option_nb = 0;
    struct TrimPlane // type 0
    {
      double origin[3];
      double normal[3]; 
    };
    struct TrimSphere // type 1
    {
      int origin_node_id;
      sdiTriple origin_pos;
      double radius;
    };
    struct TrimCylinder // type 2
    {
      int node_id_1;
      int node_id_2;
      double radius_1;
      double radius_2;
      sdiTriple pos_1;
      sdiTriple pos_2;      
    };
    struct TrimBox // type 3
    {
      int box_id;
      int skew_id;
      int node_id_1;
      int node_id_2;
      sdiTriple pos_1;
      sdiTriple pos_2;
      double vector_x[3];
      double vector_y[3];
      double vector_z[3];
    };
    // TrimArbitrary (type 4): trim surface defined by a list of 3-node or 4-node
    // segments. The segments are triangulated at read time and stored in a BVH
    // for fast nearest-point queries when evaluating node exclusion.
    struct TrimArbitrary
    {
            TrimSurfaceBVH bvh;      // BVH acceleration structure built from triangulated segments
            double distance; // absolute threshold applied on local signed distance
    };
    struct MeshTrimOption
    {
      int plane_nb;
      int sphere_nb;
      int cylinder_nb;
      int box_nb;
      int arbitrary_nb; // number of arbitrary-surface (SEGMENT) trim entries
      int option_nb;

      std::vector<TrimPlane>     plane;
      std::vector<TrimSphere>    sphere;
      std::vector<TrimCylinder>  cylinder;
      std::vector<TrimBox>       box;
      std::vector<TrimArbitrary> arbitrary; // arbitrary-surface (SEGMENT) trim entries
      std::vector<int> rsm_id ; // to store the RSM id of each trim option
      std::vector<int> operation; // 0 : trim , 1 keep
      std::vector<int> inout; // 0 : outside , 1 inside
      std::vector<int> index;
      std::vector<int> type;
    };
    MeshTrimOption mesh_trim_option;
    mesh_trim_option.plane_nb = 0;
    mesh_trim_option.sphere_nb = 0;
    mesh_trim_option.cylinder_nb = 0;
    mesh_trim_option.box_nb      = 0;
    mesh_trim_option.arbitrary_nb = 0; // initialise the arbitrary-surface (SEGMENT) trim counter

    SelectionRead mesh_trim(g_pModelViewSDI, "/ALE/STRUCTURED_MESH/TRIM");
    mesh_trim_option.option_nb=0;
    while (mesh_trim.Next())
    {
      foundtrim = true;
      sdiValue vCount(mesh_trim_option_nb);

      found = mesh_trim->GetValue(sdiIdentifier("table_count"), vCount);
      if (found){
        vCount.GetValue(mesh_trim_option_nb);
      }
      else
      {
        mesh_trim_option_nb =-1;
      }
      if(debug_activation) printf("TRIM option, line numbers: %d \n", mesh_trim_option_nb);
      
      for(int i = 0; i < mesh_trim_option_nb; i=i+1)
      {
        int rsm_id = -1;
        sdiValue v_rsm_id(rsm_id);
        found = mesh_trim->GetValue(sdiIdentifier("rsm_id_table", 0, i), v_rsm_id);
        if (found) v_rsm_id.GetValue(rsm_id);
        if(rsm_id!=s_ale_id) continue ; // get the trim option linked to the s_ale option
        mesh_trim_option.option_nb = mesh_trim_option.option_nb + 1;        
        mesh_trim_option.rsm_id.push_back(rsm_id);

        string option_table = "";
        sdiValue v_option(option_table);
        found = mesh_trim->GetValue(sdiIdentifier("option_table", 0, i), v_option);
        if (found) v_option.GetValue(option_table);
        int operation = 0;
        sdiValue v_operation(operation);
        found = mesh_trim->GetValue(sdiIdentifier("operation_table", 0, i), v_operation);
        if (found) v_operation.GetValue(operation);   
        mesh_trim_option.operation.push_back(operation);
        int inout = 0;
        sdiValue v_inout(inout);
        found = mesh_trim->GetValue(sdiIdentifier("inout_table", 0, i), v_inout);        
        if (found) v_inout.GetValue(inout);        
        mesh_trim_option.inout.push_back(inout);
        if(debug_activation) printf("TRIM option, line : %d , kind : %s , operation : %d , inout : %d \n",i,option_table.c_str(),operation,inout);      
        // ------------------------------------
        // Plane option
        if(option_table == "PLANE")
        {
          mesh_trim_option.index.push_back(mesh_trim_option.plane_nb);
          mesh_trim_option.type.push_back(0);
          mesh_trim_option.plane_nb++;
          TrimPlane new_plane;
          int int1 = 0, int2 = 0;
          double e3 = 0.0, e4 = 0.0;
          sdiValue vint1(int1), vint2(int2), vE3(e3), vE4(e4);
          bool okE1 = mesh_trim->GetValue(sdiIdentifier("int1_table", 0, i), vint1);
          if (okE1) vint1.GetValue(int1);             
          bool okE2 = mesh_trim->GetValue(sdiIdentifier("int2_table", 0, i), vint2);
          if (okE2) vint2.GetValue(int2);

          int node_id_1 = int1;
          int node_id_2 = int2; 
          sdiTriple pos_1 ;
          sdiTriple pos_2 ;
          HandleRead nodeHread;
          if(g_pModelViewSDI->FindById(radNodeType, node_id_1, nodeHread))
          {
            NodeRead nodeRead(g_pModelViewSDI, nodeHread);
            pos_1 = nodeRead.GetPosition();     
          }
          if(g_pModelViewSDI->FindById(radNodeType, node_id_2, nodeHread))
          {
            NodeRead nodeRead(g_pModelViewSDI, nodeHread);
            pos_2 = nodeRead.GetPosition();     
          }
          new_plane.origin[0] = pos_1[0];
          new_plane.origin[1] = pos_1[1];
          new_plane.origin[2] = pos_1[2];
          new_plane.normal[0] = pos_2[0] - pos_1[0];
          new_plane.normal[1] = pos_2[1] - pos_1[1];
          new_plane.normal[2] = pos_2[2] - pos_1[2];                      
          mesh_trim_option.plane.push_back(new_plane);
          if(debug_activation) printf("TRIM option, plane, id=%d, row=%d, int1=%d, int2=%d, normal=(%g, %g, %g)\n",(int)mesh_trim->GetId(), i, int1, int2,
                 new_plane.normal[0], new_plane.normal[1], new_plane.normal[2]);
        }
        // ------------------------------------
        // Sphere option        
        if(option_table == "SPHERE")
        {
          mesh_trim_option.index.push_back(mesh_trim_option.sphere_nb);
          mesh_trim_option.type.push_back(1);          
          mesh_trim_option.sphere_nb++;
          TrimSphere new_sphere;
          int int1 = 0, int2 = 0;
          double e3 = 0.0, e4 = 0.0;
          sdiValue vint1(int1), vint2(int2), vE3(e3), vE4(e4);
          bool okE1 = mesh_trim->GetValue(sdiIdentifier("int1_table", 0, i), vint1);
          if (okE1) vint1.GetValue(int1);             
          bool okE2 = mesh_trim->GetValue(sdiIdentifier("e3_table", 0, i), vE3);
          if (okE2) vE3.GetValue(e3);

          int node_id_1 = int1;
          double radius = e3; 
          sdiTriple pos_1 ;
          HandleRead nodeHread;
          if(g_pModelViewSDI->FindById(radNodeType, node_id_1, nodeHread))
          {
            NodeRead nodeRead(g_pModelViewSDI, nodeHread);
            pos_1 = nodeRead.GetPosition();     
          }
          new_sphere.origin_node_id = node_id_1;
          new_sphere.origin_pos = pos_1;
          new_sphere.radius = radius;
          mesh_trim_option.sphere.push_back(new_sphere);
          if(debug_activation) printf("TRIM option, sphere, id=%d, row=%d, int1=%d, e3=%g, origin=(%g, %g, %g), radius=%g \n",
            (int)mesh_trim->GetId(), i, int1, e3, new_sphere.origin_pos[0], new_sphere.origin_pos[1], new_sphere.origin_pos[2], new_sphere.radius);
        }
        // ------------------------------------
        // Cylinder option            
        if(option_table == "CYLINDER")
        {
          mesh_trim_option.index.push_back(mesh_trim_option.cylinder_nb);
          mesh_trim_option.type.push_back(2);          
          mesh_trim_option.cylinder_nb++;
          TrimCylinder new_cylinder;
          int int1 = 0, int2 = 0;
          double e3 = 0.0, e4 = 0.0;
          sdiValue vint1(int1), vint2(int2), vE3(e3), vE4(e4);
          bool okE1 = mesh_trim->GetValue(sdiIdentifier("int1_table", 0, i), vint1);
          if (okE1) vint1.GetValue(int1);             
          bool okE2 = mesh_trim->GetValue(sdiIdentifier("int2_table", 0, i), vint2);
          if (okE2) vint2.GetValue(int2);
          bool okE3 = mesh_trim->GetValue(sdiIdentifier("e3_table", 0, i), vE3);
          if (okE3) vE3.GetValue(e3);             
          bool okE4 = mesh_trim->GetValue(sdiIdentifier("e4_table", 0, i), vE4);
          if (okE4) vE4.GetValue(e4);

          int node_id_1 = int1;
          int node_id_2 = int2; 
          double radius_1 = e3;
          double radius_2 = e4; 
          new_cylinder.node_id_1 = node_id_1;
          new_cylinder.node_id_2 = node_id_2;
          new_cylinder.radius_1 = radius_1;
          new_cylinder.radius_2 = radius_2;
          sdiTriple pos_1 ;
          HandleRead nodeHread;
          if(g_pModelViewSDI->FindById(radNodeType, node_id_1, nodeHread))
          {
            NodeRead nodeRead(g_pModelViewSDI, nodeHread);
            pos_1 = nodeRead.GetPosition();     
          }
          sdiTriple pos_2 ;
          if(g_pModelViewSDI->FindById(radNodeType, node_id_2, nodeHread))
          {
            NodeRead nodeRead(g_pModelViewSDI, nodeHread);
            pos_2 = nodeRead.GetPosition();     
          }
          new_cylinder.pos_1 = pos_1;
          new_cylinder.pos_2 = pos_2;          
          mesh_trim_option.cylinder.push_back(new_cylinder);
          if(debug_activation) printf("TRIM option, cylinder, id=%d, row=%d, int1=%d, int2=%d, e3=%g, e4=%g, node1=%d, node2=%d, radius1=%g, radius2=%g\n",
            (int)mesh_trim->GetId(), i, int1, int2, e3, e4, node_id_1, node_id_2, radius_1, radius_2);
        }
        // ------------------------------------
        // Box option            
        if(option_table == "BOX")
        {
          mesh_trim_option.index.push_back(mesh_trim_option.box_nb);
          mesh_trim_option.type.push_back(3);          
          mesh_trim_option.box_nb++;
          TrimBox new_box;
          int int1 = 0, int2 = 0;
          double e3 = 0.0, e4 = 0.0;
          sdiValue vint1(int1), vint2(int2), vE3(e3), vE4(e4);
          bool okE1 = mesh_trim->GetValue(sdiIdentifier("int1_table", 0, i), vint1);
          if (okE1) vint1.GetValue(int1);
          int box_id = int1;

          SelectionRead box_recta(g_pModelViewSDI, "/BOX/RECTA");
          while (box_recta.Next())
          {
            if ((int)box_recta->GetId() != box_id) continue; // get the BOX/RECTA option

            int box_corner_node1 = 0;
            tempValDouble = sdiValue(box_corner_node1);
            found = box_recta->GetValue(sdiIdentifier("box_corner_node1"), tempValDouble);
            if(found) tempValDouble.GetValue(box_corner_node1);
            int box_corner_node2 = 0;
            tempValDouble = sdiValue(box_corner_node2);
            found = box_recta->GetValue(sdiIdentifier("box_corner_node2"), tempValDouble);
            if(found) tempValDouble.GetValue(box_corner_node2);

            int skew_box_id = 0;
            tempValDouble = sdiValue(skew_box_id);
            found = box_recta->GetValue(sdiIdentifier("box_system"), tempValDouble);
            if(found) tempValDouble.GetValue(skew_box_id);          

            double box_corner1_x = 0.0;
            tempValDouble = sdiValue(box_corner1_x);
            found = box_recta->GetValue(sdiIdentifier("box_corner1_x"), tempValDouble);
            if(found) tempValDouble.GetValue(box_corner1_x);        
            double box_corner1_y = 0.0;
            tempValDouble = sdiValue(box_corner1_y);
            found = box_recta->GetValue(sdiIdentifier("box_corner1_y"), tempValDouble);
            if(found) tempValDouble.GetValue(box_corner1_y);
            double box_corner1_z = 0.0;
            tempValDouble = sdiValue(box_corner1_z);
            found = box_recta->GetValue(sdiIdentifier("box_corner1_z"), tempValDouble);
            if(found) tempValDouble.GetValue(box_corner1_z);         
                
            double box_corner2_x = 0.0;
            tempValDouble = sdiValue(box_corner2_x);
            found = box_recta->GetValue(sdiIdentifier("box_corner2_x"), tempValDouble);
            if(found) tempValDouble.GetValue(box_corner2_x);        
            double box_corner2_y = 0.0;
            tempValDouble = sdiValue(box_corner2_y);
            found = box_recta->GetValue(sdiIdentifier("box_corner2_y"), tempValDouble);
            if(found) tempValDouble.GetValue(box_corner2_y);
            double box_corner2_z = 0.0;
            tempValDouble = sdiValue(box_corner2_z);
            found = box_recta->GetValue(sdiIdentifier("box_corner2_z"), tempValDouble);
            if(found) tempValDouble.GetValue(box_corner2_z);                  
            new_box.node_id_1=box_corner_node1;
            if(box_corner_node1==0)
            {
              new_box.pos_1[0] = box_corner1_x;
              new_box.pos_1[1] = box_corner1_y;
              new_box.pos_1[2] = box_corner1_z;
            }
            else
            {
              sdiTriple pos_1 ;
              HandleRead nodeHread;
              if(g_pModelViewSDI->FindById(radNodeType, box_corner_node1, nodeHread))
              {
                NodeRead nodeRead(g_pModelViewSDI, nodeHread);
                pos_1 = nodeRead.GetPosition();     
              }
              new_box.pos_1[0] = pos_1[0];
              new_box.pos_1[1] = pos_1[1];
              new_box.pos_1[2] = pos_1[2];
            }
            new_box.node_id_2=box_corner_node2;
            if(box_corner_node2==0)
            {
              new_box.pos_2[0] = box_corner2_x;
              new_box.pos_2[1] = box_corner2_y;
              new_box.pos_2[2] = box_corner2_z;
            }
            else
            {
              sdiTriple pos_2 ;
              HandleRead nodeHread;
              if(g_pModelViewSDI->FindById(radNodeType, box_corner_node2, nodeHread))
              {
                NodeRead nodeRead(g_pModelViewSDI, nodeHread);
                pos_2 = nodeRead.GetPosition();     
              }
              new_box.pos_2[0] = pos_2[0];
              new_box.pos_2[1] = pos_2[1];
              new_box.pos_2[2] = pos_2[2];
            }                

            SelectionRead local_skews(g_pModelViewSDI, "/SKEW");

            // find the skew & get its data (origin and two vectors to define local coordinate system)
            double  box_vector_x_x = 1.0, box_vector_x_y = 0.0, box_vector_x_z = 0.0;
            double  box_vector_y_x = 0.0, box_vector_y_y = 1.0, box_vector_y_z = 0.0;
            double  box_vector_z_x = 0.0, box_vector_z_y = 0.0, box_vector_z_z = 1.0;
            while (local_skews.Next())
            {
              if ((int)local_skews->GetId() != skew_box_id) continue;
              
              tempValDouble = sdiValue(box_vector_y_x);
              found = local_skews->GetValue(sdiIdentifier("globalyaxisx"), tempValDouble);
              if(found) tempValDouble.GetValue(box_vector_y_x);
              tempValDouble = sdiValue(box_vector_y_y);
              found = local_skews->GetValue(sdiIdentifier("globalyaxisy"), tempValDouble);
              if(found) tempValDouble.GetValue(box_vector_y_y);
              tempValDouble = sdiValue(box_vector_y_z);
              found = local_skews->GetValue(sdiIdentifier("globalyaxisz"), tempValDouble);
              if(found) tempValDouble.GetValue(box_vector_y_z);                

              tempValDouble = sdiValue(box_vector_z_x);
              found = local_skews->GetValue(sdiIdentifier("globalzaxisx"), tempValDouble);
              if(found) tempValDouble.GetValue(box_vector_z_x);
              tempValDouble = sdiValue(box_vector_z_y);
              found = local_skews->GetValue(sdiIdentifier("globalzaxisy"), tempValDouble);
              if(found) tempValDouble.GetValue(box_vector_z_y);
              tempValDouble = sdiValue(box_vector_z_z);
              found = local_skews->GetValue(sdiIdentifier("globalzaxisz"), tempValDouble);
              if(found) tempValDouble.GetValue(box_vector_z_z);                  
              box_vector_x_x = box_vector_y_y*box_vector_z_z - box_vector_y_z*box_vector_z_y;
              box_vector_x_y = box_vector_y_z*box_vector_z_x - box_vector_y_x*box_vector_z_z;
              box_vector_x_z = box_vector_y_x*box_vector_z_y - box_vector_y_y*box_vector_z_x;
              break;
            }

            new_box.vector_x[0] = box_vector_x_x;
            new_box.vector_x[1] = box_vector_x_y;
            new_box.vector_x[2] = box_vector_x_z;

            new_box.vector_y[0] = box_vector_y_x;
            new_box.vector_y[1] = box_vector_y_y;
            new_box.vector_y[2] = box_vector_y_z;

            new_box.vector_z[0] = box_vector_z_x;
            new_box.vector_z[1] = box_vector_z_y;
            new_box.vector_z[2] = box_vector_z_z;                                
            break;
          }
          mesh_trim_option.box.push_back(new_box);
        }
        // ------------------------------------
        // SEGMENT option (type 4): arbitrary trim surface defined by a list of
        // 3-node or 4-node segments.  Each segment is triangulated immediately
        // and the resulting triangles are stored in a BVH for efficient
        // nearest-point queries at evaluation time.
        //
        // Expected SDI field names for this row (adapt to your .cfg file):
        //   e1                : distance exclusion threshold
        //   seg_count         : number of segments in the sub-table
        //   seg_n1..seg_n4    : node IDs per segment (seg_n4=0 for 3-node triangles)
        if(option_table == "SEG" || option_table=="PART")
        {
          mesh_trim_option.index.push_back(mesh_trim_option.arbitrary_nb);
          mesh_trim_option.type.push_back(4);
          mesh_trim_option.arbitrary_nb++;

          TrimArbitrary new_arb;

          // --- Read the signed-distance exclusion threshold (e2) ---------
          int int1 = 0, int2 = 0;
          double e3 = 0.0, e4 = 0.0;
          sdiValue vint1(int1), vint2(int2), vE3(e3), vE4(e4);
          bool okE1 = mesh_trim->GetValue(sdiIdentifier("int1_table", 0, i), vint1);
          if (okE1) vint1.GetValue(int1);

          bool okDist = mesh_trim->GetValue(sdiIdentifier("e3_table", 0, i), vE3);
          if (okDist) vE3.GetValue(e3);
          new_arb.distance = e3;

          int surf_seg_id = 0;
          int part_id = 0;
          if(debug_activation) printf("TRIM option, SEG or PART, id=%d, sub-option:%s, int1=%d, distance=%g, \n",i,option_table.c_str(),int1,e3);
          if(option_table == "SEG") // /SET/GENERAL --> SEG
          {
            surf_seg_id = int1;
            SelectionRead set_seg(g_pModelViewSDI, "/SET/GENERAL");
            int segmax = 0;
            while (set_seg.Next())
            {
              if ((int)set_seg->GetId() != surf_seg_id) continue;
               string ids_type_key = "";
              sdiValue v_option_2(ids_type_key);
              found = set_seg->GetValue(sdiIdentifier("ids_type", 0, 0), v_option_2);
              if (found) v_option_2.GetValue(ids_type_key);      

              int idsmax = 0;
              tempValDouble = sdiValue(idsmax);
              found = set_seg->GetValue(sdiIdentifier("idsmax"), tempValDouble);
              if(found) tempValDouble.GetValue(idsmax); 

              int clausesmax = 0;
              tempValDouble = sdiValue(clausesmax);
              found = set_seg->GetValue(sdiIdentifier("clausesmax"), tempValDouble);
              if(found) tempValDouble.GetValue(clausesmax);

              for (int s = 0; s < clausesmax; ++s)
              {
                int opt_ = 0;
                sdiValue v_opt_(opt_);
                found = set_seg->GetValue(sdiIdentifier("opt_", 0, s), v_opt_);
                if (found) v_opt_.GetValue(opt_);
                if(opt_==1) continue; // option (add/remove/sub/ ... are not supported by TRIM)

                string set_key = "";
                sdiValue set_key_option(set_key);
                found = set_seg->GetValue(sdiIdentifier("KEY_type", 0, s), set_key_option);
                if (found) set_key_option.GetValue(set_key);
            
                if(set_key!="SEG") continue ; // get the SEG entry associated with this trim option
                

                int segmax =0;
                tempValDouble = sdiValue(segmax);
                found = set_seg->GetValue(sdiIdentifier("segmax", 0, s), tempValDouble);
                if(found) tempValDouble.GetValue(segmax);                  
                for(int seg = 0; seg < segmax; ++seg)
                {
                  int segid = 0;
                  tempValDouble = sdiValue(segid);
                  found = set_seg->GetValue(sdiIdentifier("segid", seg, s), tempValDouble);
                  if(found) tempValDouble.GetValue(segid); 

                  // Read the 3 or 4 node IDs for this segment.
                  // seg_n4 = 0 means a 3-node triangle; non-zero means a 4-node quad.
                  int n1_id = 0, n2_id = 0, n3_id = 0, n4_id = 0;
                  sdiValueEntity vN1, vN2, vN3, vN4;
                  sdiValue vN1_id , vN2_id, vN3_id, vN4_id ;
                  bool okN1 = set_seg->GetValue(sdiIdentifier("ids1", seg, s),vN1_id);
                  if (okN1)
                  { 
                    vN1_id.GetValue(vN1);
                    n1_id = vN1.GetId();
                  }
                  bool okN2 = set_seg->GetValue(sdiIdentifier("ids2", seg, s), vN2_id);
                  if (okN2)
                  {
                    vN2_id.GetValue(vN2);
                    n2_id = vN2.GetId();
                  }
                  bool okN3 = set_seg->GetValue(sdiIdentifier("ids3", seg, s), vN3_id);
                  if (okN3)
                  {
                    vN3_id.GetValue(vN3);
                    n3_id = vN3.GetId();
                  }
                  bool okN4 = set_seg->GetValue(sdiIdentifier("ids4", seg, s), vN4_id);
                  if (okN4)
                  {
                    vN4_id.GetValue(vN4);
                    n4_id = vN4.GetId();
                  }

                  // Helper lambda: resolve a node ID to its 3D position.
                  // Returns false if node_id == 0 or the node is not found.
                  auto getNodePos = [&](int node_id, TrimVec3& pos) -> bool
                  {
                    if (node_id == 0) return false;
                    HandleRead nh;
                    if (g_pModelViewSDI->FindById(radNodeType, node_id, nh))
                    {
                      NodeRead nr(g_pModelViewSDI, nh);
                      sdiTriple p3 = nr.GetPosition();
                      pos = { p3[0], p3[1], p3[2] };
                      return true;
                    }
                    return false;
                  };

                  // Resolve node positions from their IDs
                  TrimVec3 pts[4];
                  const bool ok1 = getNodePos(n1_id, pts[0]);
                  const bool ok2 = getNodePos(n2_id, pts[1]);
                  const bool ok3 = getNodePos(n3_id, pts[2]);
                  const bool ok4 = (n4_id != 0) && getNodePos(n4_id, pts[3]);
                  const int  nPts = ok4 ? 4 : 3;
                  if (ok1 && ok2 && ok3)
                  {
                    // Triangulate this segment (1 triangle for 3 nodes, 2 for 4 nodes)
                    // and append the result to the BVH triangle list.
                    TrimAddSegment(pts, nPts, new_arb.bvh.triangles);
                  }
                } // end for each segment of the current clauses
              } // end for each clause
            } // end of while set_seg.Next() 
          } // option == seg
          else // PART
          {
            int part_id = int1;          
            HandleRead partHread;
            if(g_pModelViewSDI->FindById(radPartType, part_id, partHread)) // get the /PART entry associated with this trim option
            {              
             EntityRead partRead(g_pModelViewSDI, partHread);
              SelectionElementRead selPartElem(partRead); // get a selection of elements associated with this part
              std:: vector <std::vector<int>> facet ; // to store the facet definition as a list of node IDs (key=node_id, value=node_id position in the facet)
              std::unordered_map<FaceKey, FaceCounter, FaceKeyHash> faceMap; // to store the number of occurrences of each face (key=sorted node IDs of the face, value=occurrence count)

              while(selPartElem.Next()) // loop over the selected elements of this part
              {
                const sdiString& elemKeyword = selPartElem->GetKeyword();            
                if(elemKeyword == "/SHELL" || elemKeyword == "/SH3N") // only for /SHELL & /SH3N elements
                {               
                  // Read the 3 or 4 node IDs for this shell
                  // node_ID4 = 0 means a 3-node triangle; non-zero means a 4-node shell.
                  int n1_id = 0, n2_id = 0, n3_id = 0, n4_id = 0;
                  sdiValue vN1(n1_id), vN2(n2_id), vN3(n3_id), vN4(n4_id);

                  sdiUIntList nodeIds;
                  selPartElem->GetNodeIds(nodeIds); // get the 3 or 4 nodes associated with this element
                  int node_nb = nodeIds.size();
                  n1_id = node_nb>0 ? nodeIds[0] : 0;
                  n2_id = node_nb>1 ? nodeIds[1] : 0;
                  n3_id = node_nb>2 ? nodeIds[2] : 0;
                  n4_id = node_nb>3 ? nodeIds[3] : 0;

                  bool okN1 = node_nb>0;
                  bool okN2 = node_nb>1;
                  bool okN3 = node_nb>2;
                  bool okN4 = node_nb>3;

                  int id = selPartElem->GetId(); // get the element id               

                  // Helper lambda: resolve a node ID to its 3D position.
                  // Returns false if node_id == 0 or the node is not found.
                  auto getNodePos = [&](int node_id, TrimVec3& pos) -> bool
                  {
                    if (node_id == 0) return false;
                    HandleRead nh;
                    if (g_pModelViewSDI->FindById(radNodeType, node_id, nh))
                    {
                      NodeRead nr(g_pModelViewSDI, nh);
                      sdiTriple p3 = nr.GetPosition();
                      pos = { p3[0], p3[1], p3[2] };
                      return true;
                    }
                    return false;
                  };

                  // Resolve node positions from their IDs
                  TrimVec3 pts[4];
                  const bool ok1 = getNodePos(n1_id, pts[0]);
                  const bool ok2 = getNodePos(n2_id, pts[1]);
                  const bool ok3 = getNodePos(n3_id, pts[2]);
                  const bool ok4 = (n4_id != 0) && getNodePos(n4_id, pts[3]);
                  const int  nPts = ok4 ? 4 : 3;

                  if (ok1 && ok2 && ok3)
                  {
                  // Triangulate this segment (1 triangle for 3 nodes, 2 for 4 nodes)
                  // and append the result to the BVH triangle list.
                    TrimAddSegment(pts, nPts, new_arb.bvh.triangles);
                  }
                } // if shell or sh3n
                else if(elemKeyword == "/BRICK" || elemKeyword == "/TETRA4" || elemKeyword == "/TETRA10" )
                {
                  int n1_id = 0, n2_id = 0, n3_id = 0, n4_id = 0, n5_id = 0, n6_id = 0, n7_id = 0, n8_id = 0;;
                  sdiValue vN1(n1_id), vN2(n2_id), vN3(n3_id), vN4(n4_id), vN5(n5_id), vN6(n6_id), vN7(n7_id), vN8(n8_id);
                  sdiUIntList nodeIds;
                  selPartElem->GetNodeIds(nodeIds); // get the 8 nodes associated with this element
                  int node_nb = nodeIds.size();
                  n1_id = node_nb>0 ? nodeIds[0] : 0;
                  n2_id = node_nb>1 ? nodeIds[1] : 0;
                  n3_id = node_nb>2 ? nodeIds[2] : 0;
                  n4_id = node_nb>3 ? nodeIds[3] : 0;
                  n5_id = node_nb>4 ? nodeIds[4] : 0;
                  n6_id = node_nb>5 ? nodeIds[5] : 0;
                  n7_id = node_nb>6 ? nodeIds[6] : 0;
                  n8_id = node_nb>7 ? nodeIds[7] : 0;
                  if(node_nb==4) // tetra4 or tetra10
                  {
                    facet.push_back({n1_id, n2_id, n3_id});  // opposite face to n4
                    facet.push_back({n4_id, n2_id, n1_id});  // opposite face to n3                    
                    facet.push_back({n1_id, n3_id, n4_id});  // opposite face to n2
                    facet.push_back({n4_id, n3_id, n2_id});  // opposite face to n1
                  }
                  else
                  {               
                    facet.push_back({n1_id, n5_id, n8_id,n4_id});//z-
                    facet.push_back({n2_id, n3_id, n7_id,n6_id});//z+
                    facet.push_back({n1_id, n2_id, n6_id,n5_id});// x-
                    facet.push_back({n3_id, n4_id, n8_id,n7_id});// x+
                    facet.push_back({n1_id,n4_id,n3_id,n2_id});//y-
                    facet.push_back({n5_id, n6_id,n7_id,n8_id});//y+
                  }
                }
              } // while selPartElem.Next()
              // loop over the facet of the elements to :
              //  * count the number of occurrence of each face in the part (using faceMap)
              //  * store the node IDs of one occurrence of each face (using counter.sample in
              for(auto &f : facet)
              {
                const FaceKey key = MakeFaceKey(f);
                FaceCounter& counter = faceMap[key];
                counter.count++;
                if(counter.count == 1)
                {
                  counter.sample = f; // store the first occurrence of the face
                }
              }
              // loop over the facet of the elements again to find the unique faces (count=1 in faceMap) that will define the surface of the trim option
              for(auto &f : facet)
              {
                const FaceKey key = MakeFaceKey(f);
                FaceCounter& counter = faceMap[key];
                if(counter.count == 1) // this face belongs to only one element, so it's part of the surface
                {
                  TrimVec3 pts[4];
                  int n1_id = counter.sample[0];
                  int n2_id = counter.sample[1];
                  int n3_id = counter.sample[2];
                  int n4_id = counter.sample.size()>3 ? counter.sample[3] : 0;
                  auto getNodePos = [&](int node_id, TrimVec3& pos) -> bool
                  {
                    if (node_id == 0) return false;
                    HandleRead nh;
                    if (g_pModelViewSDI->FindById(radNodeType, node_id, nh))
                    {
                      NodeRead nr(g_pModelViewSDI, nh);
                      sdiTriple p3 = nr.GetPosition();
                      pos = { p3[0], p3[1], p3[2] };
                      return true;
                    }
                    return false;
                  };
                  const bool ok1 = getNodePos(n1_id, pts[0]);
                  const bool ok2 = getNodePos(n2_id, pts[1]);
                  const bool ok3 = getNodePos(n3_id, pts[2]);
                  const bool ok4 = (n4_id != 0) && getNodePos(n4_id, pts[3]);
                  const int  nPts = ok4 ? 4 : 3;

                  if (ok1 && ok2 && ok3)
                  {       
                    TrimAddSegment(pts, nPts, new_arb.bvh.triangles);
                  }
                }
              } // end for loop over the facet of the elements
            } // /PART found                         
          }

          // Build the BVH acceleration structure once for this entire surface.
          // After this point, new_arb.bvh is ready for nearest-point queries.
          TrimBuildBVH(new_arb.bvh);
          // Move new_arb into the list to avoid deep-copying the BVH data.
          mesh_trim_option.arbitrary.push_back(std::move(new_arb));
          // ------------------------------------
        }
      }
    } // while mesh_trim

    Bcs_Struct bcs_data;
    bcs_data.list.resize(6); // 0--> x-, 1-->x+, 2-->y-, 3-->y+, 4-->z-, 5-->z+
    bcs_data.avail.resize(6,0); // -1--> no bcs, !=-1 --> bcs
    bcs_data.bcs_nb=0;
    for(i=0; i<6; i++)
    {
      bcs_data.avail[i] = -1; // default values --> no bcs
    }
    

    // Boundary conditions
    bool foundbcs = false;  
    SelectionRead bcs_sale(g_pModelViewSDI, "/ALE/STRUCTURED_MESH/BCS");      
    while (bcs_sale.Next())
    {      
      foundbcs = true;
      int table_count = 0;
      sdiValue vCount(table_count);

      found = bcs_sale->GetValue(sdiIdentifier("table_count"), vCount);
      if (found) vCount.GetValue(table_count);
      if(debug_activation) printf("BCS option, operation number= %d \n", table_count);
      for (int i = 0; i < table_count; i=i+1)
      {      
        int rsm_id = -1;
        sdiValue v_rsm_id(rsm_id);
        found = bcs_sale->GetValue(sdiIdentifier("RSM_id", 0, i), v_rsm_id);
        if (found) v_rsm_id.GetValue(rsm_id);      
        found = bcs_sale->GetValue(sdiIdentifier("rsm_id_table", 0, i), v_rsm_id);
        if (found) v_rsm_id.GetValue(rsm_id);        
        if(rsm_id!=s_ale_id) continue ; // get the trim option linked to the s_ale option 

        string option_table = "";
        sdiValue v_option(option_table);
        found = bcs_sale->GetValue(sdiIdentifier("option_table", 0, i), v_option);
        if (found) v_option.GetValue(option_table);
       
        
        int my_type = -1;
        if(option_table=="FIXED")
        {
           my_type = -1;
        }
        else if(option_table=="SLIPWALL")
        {
           my_type = 1;
        }

        else if(option_table=="NRF")
        {
           my_type = 2;
        }

        int negx = 0, posx = 0, negy = 0, posy = 0, negz = 0, posz = 0;
        sdiValue vNegx(negx), vPosx(posx), vNegy(negy), vPosy(posy), vNegz(negz), vPosz(posz);
        // X direction
        found = bcs_sale->GetValue(sdiIdentifier("face_x_minus", 0, i), vNegx);
        if (found) vNegx.GetValue(negx);        
        found = bcs_sale->GetValue(sdiIdentifier("face_x_plus", 0, i), vPosx);
        if (found) vPosx.GetValue(posx);
        if(negx==1) bcs_data.avail[0] = my_type;
        if(posx==1) bcs_data.avail[1] = my_type;
        // Y direction
        found = bcs_sale->GetValue(sdiIdentifier("face_y_minus", 0, i), vNegy);
        if (found) vNegy.GetValue(negy);
        found = bcs_sale->GetValue(sdiIdentifier("face_y_plus", 0, i), vPosy);
        if (found) vPosy.GetValue(posy);
        if(negy==1) bcs_data.avail[2] = my_type;
        if(posy==1) bcs_data.avail[3] = my_type;        

        // Z direction
        found = bcs_sale->GetValue(sdiIdentifier("face_z_minus", 0, i), vNegz);
        if (found) vNegz.GetValue(negz);
        found = bcs_sale->GetValue(sdiIdentifier("face_z_plus", 0, i), vPosz);
        if (found) vPosz.GetValue(posz);
        if(negz==1) bcs_data.avail[4] = my_type;
        if(posz==1) bcs_data.avail[5] = my_type;

        if(debug_activation)printf("BCS option, id=%d, row=%d, option=%s, Negx=%d, Posx=%d, Negy=%d, Posy=%d, Negz=%d, Posz=%d \n",
               (int)bcs_sale->GetId(), i, option_table.c_str(), negx, posx, negy, posy, negz, posz);
        
      }
    }
    if(debug_activation)printf("BCS option, x-=%d, x+=%d, y-=%d, y+=%d, z-=%d, z+=%d \n",
           bcs_data.avail[0], bcs_data.avail[1], bcs_data.avail[2], bcs_data.avail[3], bcs_data.avail[4], bcs_data.avail[5]);
    struct ControlPoint_Struct
    { 
      int icase;
      int node_nb;
      double offset;
      double scale;
      std::vector<int> node_ids;
      std::vector<double> coords;
      std::vector<double> ratio_from_input;
      std::vector<double> ratios;
    };
    ControlPoint_Struct ControlPoint_data[3]; // 3 directions : x, y, z
    for(int j = 0; j < 3; j++)
    {
      ControlPoint_data[j].icase = 0; 
      ControlPoint_data[j].node_nb = 0;
      ControlPoint_data[j].offset = 0.0;
      ControlPoint_data[j].scale = 1.0;
    }
  
    
    bool foundcontrol = false;
       
    for(int j = 0; j < 3; j++)
    {
      SelectionRead control_points(g_pModelViewSDI, "/ALE/STRUCTURED_MESH/CONTROL_POINTS");      
      int control_point_id = control_point[j];
      message_value[37] = -2-j; // error related to control points --> 2/-3/-4 control point not found
      while (control_points.Next())
      {        
        if ((int)control_points->GetId() != control_point_id) continue;
        foundcontrol = true;
        message_value[37] = 0; // control point found
        int table_count = 0;
        sdiValue vCount(table_count);

        found = control_points->GetValue(sdiIdentifier("table_count"), vCount);
        if (found) vCount.GetValue(table_count);

        int control_icase = 0 ;
        double control_offset = 0.0 ; 
        double control_scale = 1.0 ;
        sdiValue vControlIcase(control_icase);
        sdiValue vControlOffset(control_offset);
        sdiValue vControlScale(control_scale);
        bool okIcase = control_points->GetValue(sdiIdentifier("Icase"), vControlIcase);          
        bool okOffset = control_points->GetValue(sdiIdentifier("Offset"), vControlOffset);
        bool okScale = control_points->GetValue(sdiIdentifier("Scale"), vControlScale);
        if(okIcase) vControlIcase.GetValue(control_icase);
        if(okOffset) vControlOffset.GetValue(control_offset);
        if(okScale) vControlScale.GetValue(control_scale);
        ControlPoint_data[j].icase = control_icase;
        ControlPoint_data[j].offset = control_offset;
        ControlPoint_data[j].scale = control_scale;
        if(table_count<2) // not enough control points to define a pair, we stop here
        {
          message_value[37] = -5-j; // error related to control points --> 4/-5/-6 error in the control point definition
          if(debug_activation) printf("Error: ALE/STRUCTURED_MESH/CONTROL_POINTS id=%d has less than 2 control points defined, the mesh will be regular in this direction \n",(int)control_points->GetId());
          return;
        }
        if(debug_activation) printf("Control Points, Number of Point : %d \n",table_count);
        message_value[4+j] = table_count; // save the number of control points
        message_value[7+j] = 0; // default value for the type of mesh (0--> regular, 1--> refined)
        for (int i = 0; i < table_count; ++i)
        {
          int node_id = 0;
          double x_coord = 0.0;
          double ratio = 0.0;

          sdiValue vNode(node_id);
          sdiValue vX(x_coord);
          sdiValue vRatio(ratio);

          bool okNode = control_points->GetValue(sdiIdentifier("nodes_table_id", 0, i), vNode);
          if (okNode) vNode.GetValue(node_id);

          bool okX = control_points->GetValue(sdiIdentifier("nodes_table_x", 0, i), vX);
          if (okX) vX.GetValue(x_coord);

          bool okRatio = control_points->GetValue(sdiIdentifier("nodes_table_ratio", 0,i), vRatio);
          if (okRatio) vRatio.GetValue(ratio);

          if(debug_activation) printf("Control Points, id=%d, row=%d, node_id=%d, x=%g, ratio=%g \n",
                 (int)control_points->GetId(), i, node_id, x_coord, ratio);

          if(debug_activation) printf("Control Points, id=%d, row=%d, control_offset=%f, control_scale=%f \n",
                 (int)control_points->GetId(), i, control_offset, control_scale);
          // Apply offset and scale to the x_coord
          x_coord = control_offset + control_scale * x_coord;
          // Sauvegarder dans les tableaux selon la direction (j)     
          ControlPoint_data[j].node_nb++;         
          ControlPoint_data[j].node_ids.push_back(node_id);
          ControlPoint_data[j].coords.push_back(x_coord);
          if(ratio<=-1.0) ratio = 0.0; // if the user input a wrong value, we set the ratio to 0 to avoid negative values
          ControlPoint_data[j].ratio_from_input.push_back(ratio);
          if(debug_activation) printf("Control Points, Pair: %d &  %d , ratio_from_input=%g\n", j,i, ControlPoint_data[j].ratio_from_input[i]);
        }
        break; // point trouve
      }
      if(message_value[37] < 0) return ; // control point not found, we stop here
    } 
    
    // Define the number of pairs of control points 
    int pair_nb_x = ControlPoint_data[0].node_nb / 2 + ControlPoint_data[0].node_nb % 2; // if odd number of control points, the last one will form a pair with itself
    int pair_nb_y = ControlPoint_data[1].node_nb / 2 + ControlPoint_data[1].node_nb % 2;
    int pair_nb_z = ControlPoint_data[2].node_nb / 2 + ControlPoint_data[2].node_nb % 2;
    if(debug_activation) printf("Control points found: %d in X direction, %d in Y direction, %d in Z direction\n", ControlPoint_data[0].node_nb, ControlPoint_data[1].node_nb, ControlPoint_data[2].node_nb);
    if(debug_activation) printf("Number of pairs of control points: %d in X direction, %d in Y direction, %d in Z direction\n", pair_nb_x, pair_nb_y, pair_nb_z);
    if(pair_nb_x<1) message_value[37] = -5; // error related to control points --> not enough control points
    if(pair_nb_y<1) message_value[37] = -6; // error related to control points --> not enough control points
    if(pair_nb_z<1) message_value[37] = -7; // error related to control points --> not enough control points
    if(message_value[37] < 0)
    {
      if(debug_activation) printf("Error: at least one direction has less than 2 control points, the mesh will be regular in this direction \n");
      return ;
    }
    // Modification of the ratio for Icase = 1
    // Icase = 1 means that the user defines the lenght of the element at the control point
    for(int j=0;j<3;j++)
    {
      int next = 0 ;
      if(ControlPoint_data[j].node_ids[0]!=1)
      {
        message_value[37] = -5-j; // error related to control points, 1rst point must be 1 --> 4/-5/-6 error in the control point definition
        if(debug_activation) printf("Error: the first control point in direction %d has node_id %d, it must be 1, the mesh will be regular in this direction \n", j, ControlPoint_data[j].node_ids[0]);
        return ;
      }
      for(int i=0;i<ControlPoint_data[j].node_nb / 2 + ControlPoint_data[j].node_nb % 2;i++)
      {
        if(debug_activation) printf("Control Point, Pair %d in direction %d, ratio_from_input=%g\n", i, j, ControlPoint_data[j].ratio_from_input[i]);
        ControlPoint_data[j].ratios.push_back(ControlPoint_data[j].ratio_from_input[i]);
        if(ControlPoint_data[j].icase==1)
        {
          ControlPoint_data[j].ratios[i] = 0.0 ;// default value
          double pair_lenght = ControlPoint_data[j].coords[i+1] - ControlPoint_data[j].coords[i];
          int pair_element_nb = ControlPoint_data[j].node_ids[i+1] - ControlPoint_data[j].node_ids[i];          
          if(pair_element_nb<=0)
          {
            message_value[37] = -5-j; // error related to control points --> 4/-5/-6 error in the control point definition           
            if(debug_activation) printf("Error: the control points %d and %d in direction %d have node_ids %d and %d, the second node_id must be higher than the first one, the mesh will be regular in this direction \n", i, i+1, j, ControlPoint_data[j].node_ids[i], ControlPoint_data[j].node_ids[i+1]);
            return ;
          }
          double q = 0.;
          int my_index = -1;

          if(ControlPoint_data[j].ratio_from_input[i]>0.0 && ControlPoint_data[j].ratio_from_input[i+1]==0.0)
          {
            my_index = i;
          }
          else if(ControlPoint_data[j].ratio_from_input[i]==0.0&& ControlPoint_data[j].ratio_from_input[i+1]>0.0)
          {
            my_index = i+1;
          }
          if(my_index!=-1)
          {
            double dlbase = ControlPoint_data[j].ratio_from_input[my_index];
            double a = dlbase/pair_lenght;
            q = SolveGeometricQ_NewtonHybrid(a, pair_element_nb+1);
            if(my_index==i+1&&q>0.0)
            {
              q = 1.0 / q; // if the user defined the length at the second control point of the pair, we take the inverse of q to get the correct ratio
            }
            q = q - 1.0; // because in the formula we have (1+q)
            ControlPoint_data[j].ratios[i] = q ;
            if(debug_activation) printf("Control Point, Pair %d in direction %d, pair_length=%g, pair_element_nb=%d, dlbase=%g, computed q=%g\n", i, j, pair_lenght, pair_element_nb, dlbase, q);
          }
        }
      }
    }
    // Define the number of nodes in each direction
    int nx = ControlPoint_data[0].node_ids[ControlPoint_data[0].node_nb-1]-ControlPoint_data[0].node_ids[0]+1;
    int ny = ControlPoint_data[1].node_ids[ControlPoint_data[1].node_nb-1]-ControlPoint_data[1].node_ids[0]+1;
    int nz = ControlPoint_data[2].node_ids[ControlPoint_data[2].node_nb-1]-ControlPoint_data[2].node_ids[0]+1;
    for(int j=0;j<3;j++)
    {
      if((ControlPoint_data[j].node_ids[ControlPoint_data[j].node_nb-1]-ControlPoint_data[j].node_ids[0]+1)+1<2)
      {
        message_value[37] = -5-j; // error related to control points --> 5/-6/-7 not enough nodes to define the mesh in this direction
        if(debug_activation) printf("Error: not enough nodes to define the mesh in direction %d, the mesh will be regular in this direction \n", j);
        return ;
      }
    }

    // Point 1 is the origin of the box
    double x1 = ControlPoint_data[0].coords[0];
    double y1 = ControlPoint_data[1].coords[0];
    double z1 = ControlPoint_data[2].coords[0];
    // Vector V defines the first direction (local Y-axis)
    double vx = vector_y_x;
    double vy = vector_y_y;
    double vz = vector_y_z;
    // Vector W defines the second direction (local Z-axis)
    double wx = vector_z_x;
    double wy = vector_z_y;
    double wz = vector_z_z;
    // Normalize W & V to get unit direction vectors
    double v_length_raw = sqrt(vx * vx + vy * vy + vz * vz);    
    double w_length_raw = sqrt(wx * wx + wy * wy + wz * wz);
    if(v_length_raw > 0.0)
    {
        vx /= v_length_raw;
        vy /= v_length_raw;
        vz /= v_length_raw;
    }
    if(w_length_raw > 0.0)
    {
        wx /= w_length_raw;
        wy /= w_length_raw;
        wz /= w_length_raw;
    }    
    // Vector U defines the third direction (local X-axis)
    double ux = vy * wz - vz * wy;
    double uy = vz * wx - vx * wz;
    double uz = vx * wy - vy * wx;
    // Normalize U
    double u_length_raw = sqrt(ux * ux + uy * uy + uz * uz);
    if(u_length_raw > 0.0)
    {
        ux /= u_length_raw;
        uy /= u_length_raw;
        uz /= u_length_raw;
    }

    double length_x =  ControlPoint_data[0].coords[ControlPoint_data[0].node_nb - 1] - ControlPoint_data[0].coords[0];
    double length_y =  ControlPoint_data[1].coords[ControlPoint_data[1].node_nb - 1] - ControlPoint_data[1].coords[0];
    double length_z =  ControlPoint_data[2].coords[ControlPoint_data[2].node_nb - 1] - ControlPoint_data[2].coords[0];

    // Create nodes in a structured grid
    std::vector<int> nodeIdList,nodeIdList0,state,global_to_local_node_id;
    std::vector<sdiTriple> nodePos0;
    nodeIdList0.reserve(nx * ny * nz);
    nodeIdList.reserve(nx * ny * nz);
    global_to_local_node_id.reserve(nx * ny * nz);
    state.reserve(nx * ny * nz);
    std::vector <bool> elem_state;
    elem_state.resize((nx-1) * (ny-1) * (nz-1), true); // initialize all elements as active (not void)
    
    u0_u = 1. / (nx - 1);
    u0_v = 1. / (ny - 1);
    u0_w = 1. / (nz - 1);    
    message_value[11] = nx - 1 ; // save the number of elements in the X-direction (without trimming)
    message_value[12] = ny - 1 ; // save the number of elements in the Y-direction (without trimming)
    message_value[13] = nz - 1 ; // save the number of elements in the Z-direction (without trimming)

    message_value[15] = nx ; // save the number of nodes in the X-direction (without trimming)
    message_value[16] = ny ; // save the number of nodes in the Y-direction (without trimming)
    message_value[17] = nz ; // save the number of nodes in the Z-direction (without trimming)    
    int newNodeId = 0;
    for( int kk=1; kk<=pair_nb_z; kk++)
    {
      int k1 = ControlPoint_data[2].node_ids[kk-1];
      int k2 = ControlPoint_data[2].node_ids[kk];
      double local_lenght_z = ControlPoint_data[2].coords[kk] - ControlPoint_data[2].coords[kk-1];
      z1 = ControlPoint_data[2].coords[kk-1] ; // update the origin for each pair in z direction
      q_w = 1 + ControlPoint_data[2].ratios[kk-1]; // update q_w for each pair in z direction
      if(q_w!=0.&&q_w!=1.) message_value[9] = 1; // refined mesh is used
      int local_nz = k2 - k1 + 1 ;
      if(debug_activation) printf("Pair %d in Z direction: node ids (%d, %d) \n", kk, k1, k2);
      u0_w = GetCharacteristicLenght(local_nz, local_lenght_z, q_w);
      if(debug_activation) printf("u0_w for pair %d in Z direction: %f \n", kk, u0_w);      
      k2 = k2 - k1 ;    
      if(kk==pair_nb_z) k2 = k2 + 1; // to include the last node in the last pair
      for(int k = 0; k < k2; k++) 
      {
        double tk = (local_nz > 0) ? (double)k / (local_nz - 1) : 0.0;
        for(int jj=1 ; jj<=pair_nb_y; jj++)
        {
          int j1 = ControlPoint_data[1].node_ids[jj-1];
          int j2 = ControlPoint_data[1].node_ids[jj];
          double local_lenght_y = ControlPoint_data[1].coords[jj] - ControlPoint_data[1].coords[jj-1];
          y1 = ControlPoint_data[1].coords[jj-1] ; // update the origin for each pair in y direction
          q_v = 1 + ControlPoint_data[1].ratios[jj-1]; // update q_v for each pair in y direction
          if(q_v!=0.&&q_v!=1.) message_value[8] = 1; // refined mesh is used
          int local_ny = j2 - j1 + 1 ;
          if(debug_activation) printf("Pair %d in Y direction: node ids (%d, %d) \n", jj, j1, j2);
          u0_v = GetCharacteristicLenght(local_ny, local_lenght_y, q_v );
          if(debug_activation) printf("u0_v for pair %d in Y direction: %f \n", jj, u0_v);
          j2 = j2 - j1 ;
          if(jj==pair_nb_y) {j2 = j2 + 1; } // to include the last node in the last pair
          for(int j = 0; j < j2; j++)
          {
            double tj = (local_ny > 0) ? (double)j / (local_ny - 1) : 0.0;
            for(int ii=1 ; ii<=pair_nb_x; ii++)
            {
              int i1 = ControlPoint_data[0].node_ids[ii-1];
              int i2 = ControlPoint_data[0].node_ids[ii];
              double local_lenght_x = ControlPoint_data[0].coords[ii] - ControlPoint_data[0].coords[ii-1];              
              x1 = ControlPoint_data[0].coords[ii-1] ; // update the origin for each pair in x direction
              q_u = 1 + ControlPoint_data[0].ratios[ii-1]; // update q_v for each pair in x direction 
              if(q_u!=0.&&q_u!=1.) message_value[7] = 1; // refined mesh is used            
              int local_nx = i2 - i1 + 1 ;              
              if(debug_activation) printf("Pair %d in X direction: node ids (%d, %d) \n", ii, i1, i2);
              u0_u = GetCharacteristicLenght(local_nx, local_lenght_x, q_u);
              if(debug_activation) printf("u0_u for pair %d in X direction: %f \n", ii, u0_u);
              i2 = i2 - i1 ;
              if(ii==pair_nb_x) {i2 = i2 + 1; } // to include the last node in the last pair
              for(i = 0; i < i2; i++)
              {
                double ti = (local_nx > 0) ? (double)i / (local_nx - 1) : 0.0;
                
                computeNodePosition(x1,y1,z1,ux,uy,uz,
                                    vx,vy,vz,wx,wy,wz,
                                    i,j,k,
                                    i2,j2,k2,
                                    u0_u,q_u,u0_v,q_v,u0_w,q_w,
                                    nodeX,nodeY,nodeZ) ; 
                if(debug_activation) printf("NEW Compute position for node (%d,%d,%d) --> (%f,%f,%f) \n", i+i1, j+j1, k+k1, nodeX, nodeY, nodeZ);
                nodeIdList0.push_back(newNodeId);
                newNodeId++;
                nodePos0.push_back(sdiTriple(nodeX, nodeY, nodeZ));
                state.push_back(1);
                if(debug_activation) printf (" Created Node ID = %d at (%f,%f,%f) \n", newNodeId, nodeX, nodeY, nodeZ);
              }
            }
          }
        }
      }
    }

    if(debug_activation) printf("Trimming option, number : %d \n", mesh_trim_option.option_nb);
    message_value[10] = mesh_trim_option.option_nb ; // save the number of trimming option
    std::flush(std::cout); ;
    for(int i=0;i<mesh_trim_option.option_nb;i++)
    {
      int local_outside =  mesh_trim_option.inout[i] ; // 0 --> ouside, 1 --> inside
      int local_operation =  mesh_trim_option.operation[i] ; // 0 --> trim , 1 --> keep
      if(debug_activation) printf("Trimming option, line: %d, type = %d, index = %d, outside = %d, operation = %d \n", i, mesh_trim_option.type[i], mesh_trim_option.index[i], local_outside, local_operation);    

      if(mesh_trim_option.type[i]==0) // plane
      {
        int plane_id = mesh_trim_option.index[i];
        sdiTriple origin = mesh_trim_option.plane[plane_id].origin;
        sdiTriple normal = mesh_trim_option.plane[plane_id].normal;
        // check if the nodes are inside the plane
        for(int j=0;j<nodeIdList0.size();j++)
        {
          sdiTriple local_pos = sdiTriple(nodePos0[j].GetX(), nodePos0[j].GetY(), nodePos0[j].GetZ());
          bool local_state = IsPointOutsidePlane(local_pos,origin,normal);
          // Outside
          if(local_outside==0)
          {
            if(local_state) state[j] = local_operation; // mark the nodes with the local_operation value
            if(debug_activation) printf("Plane Outside Node ID %d at (%f,%f,%f) of plane id=%d is %d \n", nodeIdList0[j], nodePos0[j].GetX(), nodePos0[j].GetY(), nodePos0[j].GetZ(), plane_id,state[j]);
          }
          // Inside
          else
          {
            if(!local_state) state[j] = local_operation; // mark the nodes with the local_operation value
            if(debug_activation) printf("Plane Inside Node ID %d at (%f,%f,%f) of plane id=%d is %d \n", nodeIdList0[j], nodePos0[j].GetX(), nodePos0[j].GetY(), nodePos0[j].GetZ(), plane_id,state[j]);
          }
        }
      }
    
      else if(mesh_trim_option.type[i]==1) // sphere
      {
        int sphere_id = mesh_trim_option.index[i];
        sdiTriple origin = mesh_trim_option.sphere[sphere_id].origin_pos;
        double radius_pow_2 = mesh_trim_option.sphere[sphere_id].radius * mesh_trim_option.sphere[sphere_id].radius;
        // check if the nodes are inside the sphere
        for(int j=0;j<nodeIdList0.size();j++)
        {
          sdiTriple local_pos = sdiTriple(nodePos0[j].GetX(), nodePos0[j].GetY(), nodePos0[j].GetZ());
          // IsPointOutsideSphere returns true if the point is outside the sphere, false if it is inside or on the sphere
          bool local_state = IsPointOutsideSphere(local_pos,origin,radius_pow_2);
          // Outside
          if(local_outside==0)
          {
            if(local_state) state[j] = local_operation; // mark the nodes with the local_operation value
            if(debug_activation) printf("Sphere Outside Node ID %d at (%f,%f,%f) of sphere id=%d is %d \n", nodeIdList0[j], nodePos0[j].GetX(), nodePos0[j].GetY(), nodePos0[j].GetZ(), sphere_id,state[j]);
          }
          // Inside
          else
          {
            if(!local_state) state[j] = local_operation; // mark the nodes with the local_operation value
            if(debug_activation) printf("Sphere Inside Node ID %d at (%f,%f,%f) of sphere id=%d is %d \n", nodeIdList0[j], nodePos0[j].GetX(), nodePos0[j].GetY(), nodePos0[j].GetZ(), sphere_id,state[j]);
          }
        }        
      }
      else if(mesh_trim_option.type[i]==2) // cylinder
      {
        int cylinder_id = mesh_trim_option.index[i];
        double radius_1 = mesh_trim_option.cylinder[cylinder_id].radius_1;
        double radius_2 = mesh_trim_option.cylinder[cylinder_id].radius_2;
        sdiTriple pos_1 = mesh_trim_option.cylinder[cylinder_id].pos_1;
        sdiTriple pos_2 = mesh_trim_option.cylinder[cylinder_id].pos_2;
        // check if the nodes are inside the cylinder
        for(int j=0;j<nodeIdList0.size();j++)
        {
          sdiTriple local_pos = sdiTriple(nodePos0[j].GetX(), nodePos0[j].GetY(), nodePos0[j].GetZ());
          // IsPointOutsideCylinder returns true if the point is outside the cylinder, false if it is inside or on the cylinder
          bool local_state = IsPointOutsideCylinder(local_pos,pos_1,pos_2,radius_1,radius_2);
          // Outside
          if(local_outside==0)
          {
            if(local_state) 
            {
              state[j] = local_operation; // mark the nodes with the local_operation value
              if(debug_activation) printf("Cylinder Outside Node ID %d at (%f,%f,%f) of cylinder id=%d is %d \n", nodeIdList0[j], nodePos0[j].GetX(), nodePos0[j].GetY(), nodePos0[j].GetZ(), cylinder_id,state[j]);
            }
          }
          // Inside
          else
          {
            if(!local_state)
            {
              state[j] = local_operation; // mark the nodes with the local_operation value
              if(debug_activation) printf("Cylinder Inside Node ID %d at (%f,%f,%f) of cylinder id=%d is %d \n", nodeIdList0[j], nodePos0[j].GetX(), nodePos0[j].GetY(), nodePos0[j].GetZ(), cylinder_id,state[j]);
            }
          }
        }
      }
      else if(mesh_trim_option.type[i]==3) // box
      {
        int box_id = mesh_trim_option.index[i];
        double origin_box_x = mesh_trim_option.box[box_id].pos_1[0];
        double origin_box_y = mesh_trim_option.box[box_id].pos_1[1];
        double origin_box_z = mesh_trim_option.box[box_id].pos_1[2];
        sdiTriple corner0 = sdiTriple(mesh_trim_option.box[box_id].pos_1[0], mesh_trim_option.box[box_id].pos_1[1], mesh_trim_option.box[box_id].pos_1[2]);
        sdiTriple corner1 = sdiTriple(mesh_trim_option.box[box_id].pos_2[0], mesh_trim_option.box[box_id].pos_2[1], mesh_trim_option.box[box_id].pos_2[2]);
        // check if the nodes are inside the BOX/RECTA
        for(int j=0;j<nodeIdList0.size();j++)
        {
          sdiTriple local_pos = sdiTriple(nodePos0[j].GetX(), nodePos0[j].GetY(), nodePos0[j].GetZ());
          // IsPointOutsideOrientedBox returns true if the point is outside the box, false if it is inside or on the box
          bool local_state = IsPointOutsideOrientedBox(local_pos,corner0,corner1,
                                          mesh_trim_option.box[box_id].vector_x,
                                          mesh_trim_option.box[box_id].vector_y,
                                          mesh_trim_option.box[box_id].vector_z,
                                          1.0e-8);
          // Outside
          if(local_outside==0)
          {
            if(local_state) state[j] = local_operation; // mark the nodes with the local_operation value
            if(debug_activation) printf("Outside Node ID %d at (%f,%f,%f) of box id=%d is %d \n", nodeIdList0[j], nodePos0[j].GetX(), nodePos0[j].GetY(), nodePos0[j].GetZ(), box_id,state[j]);
          }
          // Inside
          else
          {
            if(!local_state) state[j] = local_operation; // mark the nodes with the local_operation value
            if(debug_activation) printf("Inside Node ID %d at (%f,%f,%f) of box id=%d is %d \n", nodeIdList0[j], nodePos0[j].GetX(), nodePos0[j].GetY(), nodePos0[j].GetZ(), box_id,state[j]);
          }
        }
      }

      else if(mesh_trim_option.type[i]==4) // arbitrary surface (SEGMENT option, BVH-based)
      {
        // Retrieve the TrimArbitrary entry that was built at read time.
        const int arb_id = mesh_trim_option.index[i];
        const TrimArbitrary& arb = mesh_trim_option.arbitrary[arb_id];

        // Loop over all candidate nodes and compute their signed distance
        // to the arbitrary trim surface.
        for(int j = 0; j<nodeIdList0.size(); j++)
        {
          if(debug_activation) printf("\n");
          if(debug_activation) printf("Trimming option, arbitrary surface, node :%d \n",j);
          // Query point = pre-computed position of node j
          const TrimVec3 p = { nodePos0[j].GetX(), nodePos0[j].GetY(), nodePos0[j].GetZ() };

          // TrimSignedDist now uses the oriented normal of the closest
          // triangle (derived from the user-provided segment node order).
          //   sd > 0 -> point on normal side (local "outside")
          //   sd < 0 -> point on opposite side (local "inside")
          const double sd = TrimSignedDist(arb.bvh, p);
          if(debug_activation) printf("Trimming option, arbitrary surface, node : %d (ID %d) at (%f,%f,%f) has signed distance sd=%f to arbitrary trim surface (option %d)\n",
              j, nodeIdList0[j], nodePos0[j].GetX(), nodePos0[j].GetY(), nodePos0[j].GetZ(),
              sd, i);
          // Apply the exclusion criterion using the InOut flag and distance
          // threshold, consistent with the plane/sphere/box convention.
          bool local_state = false;
          if (local_outside == 0) // InOut=0 "outside": exclude nodes beyond +distance
          {
            if(sd>0)
            {
              local_state = (sd >  arb.distance);
              if (local_state)
              {
                state[j] = local_operation; // mark node: 0=trim, 1=keep (follows operation flag)
              }
              else
              {
                state[j] = 1 - local_operation ; // mark node with the opposite value
              }            
              if(debug_activation) printf("Trimming option, arbitrary surface, Outside, Node %d at (%f,%f,%f) sd=%f -> state=%d\n",
                  nodeIdList0[j], nodePos0[j].GetX(), nodePos0[j].GetY(), nodePos0[j].GetZ(),
                  sd, state[j]);
            }            
          }
          else                    // InOut=1 "inside" : exclude nodes beyond -distance
          {          
            if(sd<=0)
            {
              local_state = (sd <= -arb.distance);
              if (local_state)
              {
                state[j] = local_operation; // mark node: 0=trim, 1=keep (follows operation flag)
              }
              else
              {
                state[j] = 1 - local_operation ; // mark node with the opposite value
              }            
              if(debug_activation) printf("Trimming option, arbitrary surface, Inside, Node %d at (%f,%f,%f) sd=%f -> state=%d\n",
                  nodeIdList0[j], nodePos0[j].GetX(), nodePos0[j].GetY(), nodePos0[j].GetZ(),
                  sd, state[j]);
            }            
          }
        }
      }
    }

    // loop over the elements and if at least 1 node is marked, then mark the other nodes of the element
    for(int k = 0; k < nz - 1; k++)
    {
      for(int j = 0; j < ny - 1; j++)
      {
        for(int i = 0; i < nx - 1; i++)
        {
          // Calculate node indices for this brick element
          // Bottom face (z level k)
          int node_index0[8];
          node_index0[0] = k * (nx * ny) + j * nx + i;
          node_index0[1] = k * (nx * ny) + j * nx + (i + 1);
          node_index0[2] = k * (nx * ny) + (j + 1) * nx + (i + 1);
          node_index0[3] = k * (nx * ny) + (j + 1) * nx + i;
          // Top face (z level k+1)
          node_index0[4] = (k + 1) * (nx * ny) + j * nx + i;
          node_index0[5] = (k + 1) * (nx * ny) + j * nx + (i + 1);
          node_index0[6] = (k + 1) * (nx * ny) + (j + 1) * nx + (i + 1);
          node_index0[7] = (k + 1) * (nx * ny) + (j + 1) * nx + i;
          bool element_marked = false;
          for(int n=0; n<8; n++)
          {
            if(state[node_index0[n]]==1) element_marked = true; // if at least 1 node is marked with 1
          }
          if(element_marked)
          {
            for(int n=0; n<8; n++)
            {
              if(state[node_index0[n]]==0) state[node_index0[n]] = 2; // mark all the nodes of the element with 2
            }
          }         
          elem_state[k * ((nx-1) * (ny-1)) + j * (nx-1) + i] =  element_marked;
        }
      }
    }

    int cpt_node = 0;
    int max_node_id = g_pModelViewSDI->GetNextAvailableId(radNodeType);
    if(max_node_id<=node_offset)
    {
      max_node_id = node_offset ;
      message_value[41] = node_offset;
    }
    else
    {
      message_value[41] = -max_node_id;
    }

    // ALE/LINK/VEL creation
    int grnod_id = 0;
    message_value[44] = 0; // initialize the message value for the /GRNOD/NODE id to 0, it will be updated if a reference node is defined
    if(ref_node_id!=0)
    {      
      // check if the reference node exists
      HandleRead nodeHread;
      if(g_pModelViewSDI->FindById(radNodeType,ref_node_id,nodeHread)==0)
      {
        message_value[43] = -ref_node_id;
        ref_node_id = 0; // if the reference node does not exist, we set it to 0 to avoid creating the /GRNOD/NODE and the /ALE/LINK/VEL
      }
    }

    for(int j=0;j<nodeIdList0.size();j++)
    {
      global_to_local_node_id[j]=0;        
      if(state[j]>0) // if the node is marked with 1 or 2 --> need to create it
      {
        HandleNodeEdit radnodeHEdit;
        g_pModelViewSDI->CreateNode(radnodeHEdit, "/NODE", sdiTriple(nodePos0[j].GetX(),  nodePos0[j].GetY(), nodePos0[j].GetZ()),max_node_id+cpt_node);
        sdiTriple local_pos = sdiTriple(nodePos0[j].GetX(), nodePos0[j].GetY(), nodePos0[j].GetZ());
        int newNodeId = radnodeHEdit.GetId(g_pModelViewSDI);
        nodeIdList.push_back(newNodeId);
        global_to_local_node_id[j]=cpt_node; // store the mapping between the local node index and the global node id 
        cpt_node++;
      }
    }

    HandleEdit radGrnodEdit ;    
    if(ref_node_id!=0)
    {        
      // if a reference node is defined, we create a /GRNOD/NODE entity to store the number of nodes and the id of the first node, this will allow to retrieve the correct node ids in the post-processing even if there are already nodes in the model before creating the mesh
      g_pModelViewSDI->CreateEntity(radGrnodEdit, "/GRNOD/NODE","Automatically generated /GRNOD/NODE for Structured ALE option : " + g_pEntity->GetName()) ;
      grnod_id = radGrnodEdit.GetId(g_pModelViewSDI); // get the id of the /GRNOD/NODE
      int node_nb = nodeIdList.size(); // get the number of nodes
      radGrnodEdit.SetValue(g_pModelViewSDI,sdiIdentifier("idsmax"),sdiValue(node_nb) ); // assign the number of nodes to the /GRNOD/NODE
      message_value[44] = grnod_id; // save the id of the /GRNOD/NODE related to the refrence node id
      
      HandleEdit radAleLinkVelEdit ;
      g_pModelViewSDI->CreateEntity(radAleLinkVelEdit, "/ALE/LINK/VEL","Automatically generated /ALE/LINK/VEL for Structured ALE option : " + g_pEntity->GetName()) ;      
      radAleLinkVelEdit.SetValue(g_pModelViewSDI,sdiIdentifier("node_ID1"),sdiValue(sdiValueEntity(radNodeType,ref_node_id)) ); // assign the reference node id to the /ALE/LINK/VEL
      radAleLinkVelEdit.SetValue(g_pModelViewSDI,sdiIdentifier("node_ID2"),sdiValue(sdiValueEntity(radNodeType,ref_node_id)) ); // assign the reference node id to the /ALE/LINK/VEL
      radAleLinkVelEdit.SetValue(g_pModelViewSDI,sdiIdentifier("grnod_ID"),sdiValue(sdiValueEntity(radGrnodType,grnod_id)) ); // assign the /GRNOD/NODE id to the /ALE/LINK/VEL
      int code_w = 1; // code for the Wx/y/z
      radAleLinkVelEdit.SetValue(g_pModelViewSDI,sdiIdentifier("Wx"),sdiValue(code_w) ); // assign the code for the grid velocity in X direction
      radAleLinkVelEdit.SetValue(g_pModelViewSDI,sdiIdentifier("Wy"),sdiValue(code_w) ); // assign the code for the grid velocity in Y direction
      radAleLinkVelEdit.SetValue(g_pModelViewSDI,sdiIdentifier("Wz"),sdiValue(code_w) ); // assign the code for the grid velocity in Z direction
      int iform = 1 ; // 1 for the standard ALE formulation, 2 for the non-conservative ALE formulation
      radAleLinkVelEdit.SetValue(g_pModelViewSDI,sdiIdentifier("Iform"),sdiValue(iform) ); // assign the ALE formulation code
    }
    if(grnod_id!=0)
    {
      for(int j=0;j<nodeIdList.size();j++)
      {
        int newNodeId = nodeIdList[j];
        // if a reference node is defined, save the nodes into the /GRNOD/NODE
        radGrnodEdit.SetValue(g_pModelViewSDI, sdiIdentifier("ids",0,j),sdiValue(sdiValueEntity(radNodeType, newNodeId))); // assign the id of the created node to the /GRNOD/NODE
      }
    }    
    
    int cpt = 0;    
    int max_brick_id = std::max({g_pModelViewSDI->GetNextAvailableId(radBrickType),
                                 g_pModelViewSDI->GetNextAvailableId(radBrick16Type),
                                 g_pModelViewSDI->GetNextAvailableId(radBrick20Type),                                 
                                 g_pModelViewSDI->GetNextAvailableId(radTetra4Type),
                                 g_pModelViewSDI->GetNextAvailableId(radTetra10Type)});
    if(max_brick_id<=element_offset)
    {
      max_brick_id = element_offset ;
      message_value[40] = element_offset;
    }
    else if(element_offset!=0)
    {
      message_value[40] = -max_brick_id;
    }
    else
    {
      message_value[40] = max_brick_id;
    }
    for(int k = 0; k < nz - 1; k++)
    {
      for(int j = 0; j < ny - 1; j++)
      {
        for(int i = 0; i < nx - 1; i++)
        {
          if(!elem_state[k * ((nx-1) * (ny-1)) + j * (nx-1) + i]) continue; // if the element is not marked, skip it
          // Calculate node indices for this brick element
          // Bottom face (z level k)
          int n0 = global_to_local_node_id[k * (nx * ny) + j * nx + i];
          int n1 = global_to_local_node_id[k * (nx * ny) + j * nx + (i + 1)];
          int n2 = global_to_local_node_id[k * (nx * ny) + (j + 1) * nx + (i + 1)];
          int n3 = global_to_local_node_id[k * (nx * ny) + (j + 1) * nx + i];
                
          // Top face (z level k+1)
          int n4 = global_to_local_node_id[(k + 1) * (nx * ny) + j * nx + i];
          int n5 = global_to_local_node_id[(k + 1) * (nx * ny) + j * nx + (i + 1)];
          int n6 = global_to_local_node_id[(k + 1) * (nx * ny) + (j + 1) * nx + (i + 1)];
          int n7 = global_to_local_node_id[(k + 1) * (nx * ny) + (j + 1) * nx + i];

          // check the state of the west element i-1
          bool need_it = false ;
          int node_list[4];
          if(i==0)
          {
            // save n0/n3/n4/n7 for the west face of the mesh
            need_it = true ;
          } 
          else if(!elem_state[k * ((nx-1) * (ny-1)) + j * (nx-1) + i-1])
          {
            // save n0/n3/n4/n7 for the west face of the mesh
            need_it = true ;
          }
          if(need_it)
          {
            node_list[0] = n0;
            node_list[1] = n3;
            node_list[2] = n4;
            node_list[3] = n7; 
            boundary_save(node_list,4,bcs_data,0) ; // x- --> save the nodes of the face in the boundary condition data structure              
          }
          // check the state of the east element i+1
          need_it = false ;
          if(i==nx-2)
          {
            // save n1/n2/n5/n6 for the east face of the mesh
            need_it = true ;        
          }
          else if(!elem_state[k * ((nx-1) * (ny-1)) + j * (nx-1) + i+1])
          {
            // save n1/n2/n5/n6 for the east face of the mesh
            need_it = true ;             
          }
          if(need_it)
          {
            node_list[0] = n1;
            node_list[1] = n2;
            node_list[2] = n5;
            node_list[3] = n6;   
            boundary_save(node_list,4,bcs_data,1) ; // x+ --> save the nodes of the face in the boundary condition data structure              
          }
          // check the state of the south element j-1
          need_it = false ;
          if(j==0)
          {
            // save n0/n1/n4/n5 for the south face of the mesh
            need_it = true ;              
          }
          else if(!elem_state[k * ((nx-1) * (ny-1)) + (j-1) * (nx-1) + i])
          {
            // save n0/n1/n4/n5 for the south face of the mesh
            need_it = true ;        
          }
          if(need_it)
          {
            node_list[0] = n0;
            node_list[1] = n1;
            node_list[2] = n4;
            node_list[3] = n5;   
            boundary_save(node_list,4,bcs_data,2) ; // y- --> save the nodes of the face in the boundary condition data structure              
          }
          // check the state of the north element j+1
          need_it = false ;
          if(j==ny-2)
          {
            // save n2/n3/n6/n7 for the north face of the mesh
            need_it = true ;                
          }
          else if(!elem_state[k * ((nx-1) * (ny-1)) + (j+1) * (nx-1) + i])
          {
            // save n2/n3/n6/n7 for the north face of the mesh
            need_it = true ;            
          }
          if(need_it)
          {
            node_list[0] = n2;
            node_list[1] = n3;
            node_list[2] = n6;
            node_list[3] = n7; 
            boundary_save(node_list,4,bcs_data,3) ; // y+ --> save the nodes of the face in the boundary condition data structure              
          }

          // check the state of the bottom element k-1
          need_it = false ;
          if(k==0) 
          {
            // save n0/n1/n2/n3 for the bottom face of the mesh
            need_it = true;
          }
          else if(!elem_state[(k-1) * ((nx-1) * (ny-1)) + j * (nx-1) + i])
          {
            // save n0/n1/n2/n3 for the bottom face of the mesh
            need_it = true;
          }
          if(need_it)
          {
            node_list[0] = n0;
            node_list[1] = n1;
            node_list[2] = n2;
            node_list[3] = n3; 
            boundary_save(node_list,4,bcs_data,4) ; // z- --> save the nodes of the face in the boundary condition data structure              
          }
          
          // check the state of the top element k+1
          need_it = false ;
          if(k==nz-2)
          {
            // save n4/n5/n6/n7 for the top face of the mesh
            need_it = true;
          }
          else if(!elem_state[(k+1) * ((nx-1) * (ny-1)) + j * (nx-1) + i])
          {
            // save n4/n5/n6/n7 for the top face of the mesh
            need_it = true;
          }
          if(need_it)
          {
            node_list[0] = n4;
            node_list[1] = n5;
            node_list[2] = n6;
            node_list[3] = n7; 
            boundary_save(node_list,4,bcs_data,5) ; // z+ --> save the nodes of the face in the boundary condition data structure              
          }

                
          // Create brick element
          HandleElementEdit radBrickElemHEdit;          
          g_pModelViewSDI->CreateElement(radBrickElemHEdit, "/BRICK",cpt+max_brick_id) ;
               
          // Set the 8 nodes for this brick
          sdiUIntList brickNodes;
          brickNodes.resize(8);
          brickNodes[0] = nodeIdList[n0];
          brickNodes[1] = nodeIdList[n1];
          brickNodes[2] = nodeIdList[n2];
          brickNodes[3] = nodeIdList[n3];
          brickNodes[4] = nodeIdList[n4];
          brickNodes[5] = nodeIdList[n5];
          brickNodes[6] = nodeIdList[n6];
          brickNodes[7] = nodeIdList[n7];
                 
          radBrickElemHEdit.SetValue(g_pModelViewSDI, sdiIdentifier("node_ID"), 
                                     sdiValue(sdiValueEntityList(radNodeType, brickNodes)));
          radBrickElemHEdit.SetValue(g_pModelViewSDI, sdiIdentifier("part_ID"), 
                                     sdiValue(sdiValueEntity(radPartType,sale_part_id)));
          if(debug_activation) printf("New element id = %d with nodes %d,%d,%d,%d,%d,%d,%d,%d \n", cpt+max_brick_id, brickNodes[0], brickNodes[1], brickNodes[2], brickNodes[3], brickNodes[4], brickNodes[5], brickNodes[6], brickNodes[7]);
          cpt++;
        }
      }
    }


    SelectionRead entities(g_pModelViewSDI, "/SURF/SEG");
    // Find the maximal segment id 
    int segidmax = -1;
    while(entities.Next())
    {
      int segmax = 0;
      sdiValue tempValInt(segmax);
      entities->GetValue(sdiIdentifier("segmax"), tempValInt);
      tempValInt.GetValue(segmax);
      for(int i=0; i<segmax; i++)
      {
        int segId = 0;
        sdiValue tempValIntSegId(segId);
        entities->GetValue(sdiIdentifier("SEGidArray",i), tempValIntSegId);
        tempValIntSegId.GetValue(segId);
        segidmax = std::max(segidmax, segId);
      }
    }

    segidmax++;
    if(!law151)
    {
      // Default boundary condition 
      for(int facet_id=0; facet_id<6; facet_id++)
      {
        if(bcs_data.avail[facet_id]==-1)
        {
          bcs_data.avail[facet_id] = 1; // assign slip wall boundary condition for the facets without boundary condition
        }
      }
    }
     
    // Boundary conditions for the faces of the mesh
    for(int facet_id=0; facet_id<6; facet_id++)
    {
      message_value[31+facet_id] = bcs_data.avail[facet_id] ; // save the boundary condition type
      if(bcs_data.avail[facet_id]!=-1)
      {
        if(debug_activation) printf("BCS option, facet id=%d, type=%d, number of nodes=%d \n", facet_id, bcs_data.avail[facet_id], bcs_data.list[facet_id].node_id.size());
        if(((bcs_data.avail[facet_id]==0||bcs_data.avail[facet_id]==1))&& !law151) // Fixed boundary condition --> /ALE/BCS & /BCS
                                                                                 // or No flow boundary condition --> /ALE/BCS
        {
          HandleEdit radGrnodEdit ;
          g_pModelViewSDI->CreateEntity(radGrnodEdit, "/GRNOD/NODE","Automatically generated /GRNOD/NODE for Structured ALE option : " + g_pEntity->GetName()) ;
          int grnod_id = radGrnodEdit.GetId(g_pModelViewSDI); // get the id of the /GRNOD/NODE
          int node_nb = bcs_data.list[facet_id].node_id.size(); // get the number of nodes
          if(debug_activation) printf("BCS option, /GRNOD/NODE creation, id=%d, number of nodes=%d \n", grnod_id, node_nb);
          radGrnodEdit.SetValue(g_pModelViewSDI,sdiIdentifier("idsmax"),sdiValue(node_nb) ); // assign the number of nodes to the /GRNOD/NODE
          int j=0;
          for(auto local_node_id : bcs_data.list[facet_id].node_id) 
          {
            radGrnodEdit.SetValue(g_pModelViewSDI, sdiIdentifier("ids",0,j),sdiValue(sdiValueEntity(radNodeType, nodeIdList[local_node_id])));
            j++;
            if(debug_activation) printf("BCS option, /GRNOD/NODE, facet id=%d, node id=%d \n", facet_id, nodeIdList[local_node_id]);
          }
          int bcs_id =-1;
          int dof[6] = {0};
          int dof_index = -1;
          if(facet_id==0||facet_id==1) // x- or x+ face
          {
            dof_index = 0; // block translation in x direction
          }
          else if(facet_id==2||facet_id==3) // y- or y+ face
          {
            dof_index = 1; // block translation in y direction
          }
          else if(facet_id==4||facet_id==5) // z- or z+ face
          {
            dof_index = 2; // block translation in z direction
          }
          if(true)
          {
          // /ALE/BCS creation
          HandleEdit radAleBcsdEdit ;
          g_pModelViewSDI->CreateEntity(radAleBcsdEdit, "/ALE/BCS","Automatically generated /ALE/BCS for Structured ALE option : " + g_pEntity->GetName()) ;
          bcs_id = radAleBcsdEdit.GetId(g_pModelViewSDI); // get the id of the /ALE/BCS
          message_value[19+facet_id] = bcs_id ; // save the /ALE/BCS bcs id
          radAleBcsdEdit.SetValue(g_pModelViewSDI, sdiIdentifier("entityid"), sdiValue(sdiValueEntity(radGrnodType,grnod_id))); //  assign the /GRNOD/NODE id to the /ALE/BCS
          radAleBcsdEdit.SetValue(g_pModelViewSDI, sdiIdentifier("inputsystem"), sdiValue(sdiValueEntity(radSkewType,skew_id))); //  assign the /SKEW id to the /ALE/BCS
           dof[6] = {0};
          if(bcs_data.avail[facet_id]==0)
          {
            dof[6] = {1}; // all directions are blocked for fixed boundary condition
          }
          else
          {
            dof[dof_index] = 1; // normal direction is blocked for No flow boundary condition
          }
          if(debug_activation) printf("BCS option, /ALE/BCS creation, id=%d, dof=%d,%d,%d,%d,%d,%d \n", bcs_id, dof[0], dof[1], dof[2], dof[3], dof[4], dof[5]);          
          radAleBcsdEdit.SetValue(g_pModelViewSDI, sdiIdentifier("LX"), sdiValue(dof[0])); // assign the dof for translation LX
          radAleBcsdEdit.SetValue(g_pModelViewSDI, sdiIdentifier("LY"), sdiValue(dof[1])); // assign the dof for translation LY
          radAleBcsdEdit.SetValue(g_pModelViewSDI, sdiIdentifier("LZ"), sdiValue(dof[2])); // assign the dof for translation LZ
          radAleBcsdEdit.SetValue(g_pModelViewSDI, sdiIdentifier("WX"), sdiValue(dof[3])); // assign the dof for rotation WX
          radAleBcsdEdit.SetValue(g_pModelViewSDI, sdiIdentifier("WY"), sdiValue(dof[4])); // assign the dof for rotation WY
          radAleBcsdEdit.SetValue(g_pModelViewSDI, sdiIdentifier("WZ"), sdiValue(dof[5])); // assign the dof for rotation WZ
          }
          // /BCS creation
          HandleEdit radBcsdEdit ;
          g_pModelViewSDI->CreateEntity(radBcsdEdit, "/BCS","Automatically generated /BCS for Structured ALE option : " + g_pEntity->GetName()) ;
          bcs_id = radBcsdEdit.GetId(g_pModelViewSDI); // get the id of the /BCS
          radBcsdEdit.SetValue(g_pModelViewSDI, sdiIdentifier("entityid"), sdiValue(sdiValueEntity(radGrnodType,grnod_id))); //  assign the /GRNOD/NODE id to the /BCS
          radBcsdEdit.SetValue(g_pModelViewSDI, sdiIdentifier("inputsystem"), sdiValue(sdiValueEntity(radSkewType,skew_id))); //  assign the /SKEW id to the /BCS
          int dof1 = 1;
          int dof2 = 1;
          int dof3 = 1;
          int dof4 = 0;
          int dof5 = 0;
          int dof6 = 0;
          dof[6] = {0};
          dof[dof_index] = 1;
          radBcsdEdit.SetValue(g_pModelViewSDI, sdiIdentifier("dof1"), sdiValue(dof[0])); // assign the dof for translation TX
          radBcsdEdit.SetValue(g_pModelViewSDI, sdiIdentifier("dof2"), sdiValue(dof[1])); // assign the dof for translation TY
          radBcsdEdit.SetValue(g_pModelViewSDI, sdiIdentifier("dof3"), sdiValue(dof[2])); // assign the dof for translation TZ
          radBcsdEdit.SetValue(g_pModelViewSDI, sdiIdentifier("dof4"), sdiValue(dof[3])); // assign the dof for rotation RX
          radBcsdEdit.SetValue(g_pModelViewSDI, sdiIdentifier("dof5"), sdiValue(dof[4])); // assign the dof for rotation RY
          radBcsdEdit.SetValue(g_pModelViewSDI, sdiIdentifier("dof6"), sdiValue(dof[5])); // assign the dof for rotation RZ
          message_value[25+facet_id] = bcs_id ; // save the /BCS bcs id
          if(debug_activation) printf("BCS option, /BCS creation, id=%d, dof=%d,%d,%d,%d,%d,%d \n", bcs_id, dof[0], dof[1], dof[2], dof[3], dof[4], dof[5]);          
        }
        else if(bcs_data.avail[facet_id]==2) // Non reflecting boundary condition --> /SURF/EXT
        {
          HandleEdit radSurfExtEdit ;
          g_pModelViewSDI->CreateEntity(radSurfExtEdit, "/SURF/SEG","Automatically generated /SURF/EXT for Structured ALE option : " + g_pEntity->GetName()) ;
          int surf_seg_id = radSurfExtEdit.GetId(g_pModelViewSDI); // get the id of the /SURF/SEG
          int segmax = bcs_data.list[facet_id].facet_node_id.size(); // get the number of segment
          radSurfExtEdit.SetValue(g_pModelViewSDI,sdiIdentifier("segmax"),sdiValue(segmax) ); // assigne the max number of segment

          int seg_count=0;
          for(int j=0; j<bcs_data.list[facet_id].facet_node_id.size(); j++) // loop over the segments
          {
            array <int,4> local_node_id = bcs_data.list[facet_id].facet_node_id[j]; // get the 4 nodes of the segment
            radSurfExtEdit.SetValue(g_pModelViewSDI, sdiIdentifier("SEGidArray",seg_count),sdiValue(segidmax)); // assign the semgent id      
            // assign the 4 nodes of the segment   
            radSurfExtEdit.SetValue(g_pModelViewSDI, sdiIdentifier("N1",0,seg_count),sdiValue(sdiValueEntity(radNodeType, nodeIdList[local_node_id[0]])));
            radSurfExtEdit.SetValue(g_pModelViewSDI, sdiIdentifier("N2",0,seg_count),sdiValue(sdiValueEntity(radNodeType, nodeIdList[local_node_id[1]])));
            radSurfExtEdit.SetValue(g_pModelViewSDI, sdiIdentifier("N3",0,seg_count),sdiValue(sdiValueEntity(radNodeType, nodeIdList[local_node_id[2]])));
            radSurfExtEdit.SetValue(g_pModelViewSDI, sdiIdentifier("N4",0,seg_count),sdiValue(sdiValueEntity(radNodeType, nodeIdList[local_node_id[3]])));                                    
            seg_count++;
            segidmax++;
          }

          HandleEdit radEbcsNrfEdit ;
          g_pModelViewSDI->CreateEntity(radEbcsNrfEdit, "/EBCS/NRF","Automatically generated /EBCS/NRF for Structured ALE option : " + g_pEntity->GetName()) ;
          int ebcs_id = radEbcsNrfEdit.GetId(g_pModelViewSDI); // get the id of the /EBCS/NRF
          radEbcsNrfEdit.SetValue(g_pModelViewSDI, sdiIdentifier("entityid"), sdiValue(sdiValueEntity(radSegmentType,surf_seg_id))); //  assign the /SURF/SEG id to the /EBCS/NRF
          double default_value = 0.0;
          radEbcsNrfEdit.SetValue(g_pModelViewSDI,sdiIdentifier("tcar_vf"),sdiValue(default_value) ); // default value for tcar_vf
          radEbcsNrfEdit.SetValue(g_pModelViewSDI,sdiIdentifier("tcar_p"),sdiValue(default_value) ); // default value for tcar_p
          message_value[19+facet_id] = ebcs_id ; // save the /EBCS/NRF bcs id
          if(debug_activation) printf("BCS option, /EBCS/NRF creation, facet id=%d,id=%d, segment number=%d \n", ebcs_id, facet_id,bcs_data.list[facet_id].facet_node_id.size());          
        }
      }        
    }

    message_value[14] = cpt; // save the number of created elements
    message_value[18] = cpt_node; // save the number of created nodes
  }    





  // ***************************************************************



void computeNodePosition(
    double x1, double y1, double z1, // Origine
    double ux, double uy, double uz, // U
    double vx, double vy, double vz, // V    
    double wx, double wy, double wz, // W
    int i, int j, int k,          // Index
    int n_u, int n_v, int n_w,    // Number of nodes in the segment
    double u0_u, double q_u,         // Initial size and factor U
    double u0_v, double q_v,         // Initial size and factor V
    double u0_w, double q_w,         // Initial size and factor W
    double& nodeX, double& nodeY, double& nodeZ)
{
            //double tj = (ny > 1) ? (double)j / (ny - 1) : 0.0;  
    double ti_disp = ((q_u == 1.0)||(q_u == 0.0)) ? i * u0_u :  u0_u * (1.0 - std::pow(q_u, i)) / (1.0 - q_u);
    double tj_disp = ((q_v == 1.0)||(q_v == 0.0)) ? j * u0_v :  u0_v * (1.0 - std::pow(q_v, j)) / (1.0 - q_v);
    double tk_disp = ((q_w == 1.0)||(q_w == 0.0)) ? k * u0_w :  u0_w * (1.0 - std::pow(q_w, k)) / (1.0 - q_w);

    nodeX = x1 + ti_disp * ux + tj_disp * vx + tk_disp * wx;
    nodeY = y1 + ti_disp * uy + tj_disp * vy + tk_disp * wy;
    nodeZ = z1 + ti_disp * uz + tj_disp * vz + tk_disp * wz;
}
