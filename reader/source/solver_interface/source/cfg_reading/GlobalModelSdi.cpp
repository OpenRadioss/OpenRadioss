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

//#include <hwSDIDefs.h>
//#include <hwSDIEntity.h>
//#include <hwSDIHandles.h>
//#include <hwSDIModelView.h>
//#include <hwSDISelection.h>
#include <typedef.h>
#include <sdiModelView.h>

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
    found =  g_pEntity->GetValue(sdiIdentifier("Iedge_Type19"), tempValInt);
    if(found) tempValInt.GetValue(Iedge_Type19);
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
    EntityType radnodeType = g_pModelViewSDI->GetEntityType("/NODE");
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
    g_pModelViewSDI->CreateElement(radElem0, destElem, *ELEM_MAXID); 
    elemNodes[0] = first;
    elemNodes[1] = second;
    radElem0.SetValue(g_pModelViewSDI, sdiIdentifier("node_ID"), sdiValue(sdiValueEntityList(radnodeType, elemNodes)));
    radElem0.SetValue(g_pModelViewSDI, sdiIdentifier("part_ID"), sdiValue(sdiValueEntity(radPartType, newPartId)));

    for(int i = 1; i < nbElems ; i = i + 1)    
    { 
        if(first !=  tmpNodes[i].first || second !=  tmpNodes[i].second)  
        {  
// create other spring elements
            *ELEM_MAXID = *ELEM_MAXID + 1;
            HandleElementEdit radElem;
            g_pModelViewSDI->CreateElement(radElem, destElem, *ELEM_MAXID); 
            elemNodes[0] = tmpNodes[i].first;
            elemNodes[1] = tmpNodes[i].second;
            radElem.SetValue(g_pModelViewSDI, sdiIdentifier("node_ID"), sdiValue(sdiValueEntityList(radnodeType, elemNodes)));
            radElem.SetValue(g_pModelViewSDI, sdiIdentifier("part_ID"), sdiValue(sdiValueEntity(radPartType, newPartId)));
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
    EntityType radnodeType = g_pModelViewSDI->GetEntityType("/NODE");
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
                    tmpNodes.push_back( make_pair(aNodeId[0],aNodeId[1]) );
                    tmpNodes.push_back( make_pair(aNodeId[3],aNodeId[2]) );
                }
                else
                {
                    tmpNodes.push_back( make_pair(aNodeId[1],aNodeId[2]) );
                    tmpNodes.push_back( make_pair(aNodeId[0],aNodeId[3]) );
                } 
            }
            else
            {
                tmpNodes.push_back( make_pair(aNodeId[0],aNodeId[1]) );
                tmpNodes.push_back( make_pair(aNodeId[3],aNodeId[2]) ); 
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
            tmpNodes.push_back( make_pair(aNodeId[0],aNodeId[1]) );
            tmpNodes.push_back( make_pair(aNodeId[3],aNodeId[2]) ); 
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
    g_pModelViewSDI->CreateElement(radElem0, destElem, *ELEM_MAXID); 
    elemNodes[0] = first;
    elemNodes[1] = second;
    radElem0.SetValue(g_pModelViewSDI, sdiIdentifier("node_ID"), sdiValue(sdiValueEntityList(radnodeType, elemNodes)));
    radElem0.SetValue(g_pModelViewSDI, sdiIdentifier("part_ID"), sdiValue(sdiValueEntity(radPartType, newPartId)));
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
                g_pModelViewSDI->CreateElement(radElem, destElem, *ELEM_MAXID); 
                elemNodes[0] = tmpNodes[index[i]].first;
                elemNodes[1] = tmpNodes[index[i]].second;
                radElem.SetValue(g_pModelViewSDI, sdiIdentifier("node_ID"), sdiValue(sdiValueEntityList(radnodeType, elemNodes)));
                radElem.SetValue(g_pModelViewSDI, sdiIdentifier("part_ID"), sdiValue(sdiValueEntity(radPartType, newPartId)));
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
