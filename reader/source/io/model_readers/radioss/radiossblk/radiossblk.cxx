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
#ifdef WIN32
#include <windows.h>
#else
#include <dirent.h>
#endif

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <set>

#include <HCDI/hcdi_utils.h>
#include <HCDI/hcdi_mv_descriptor.h>
#include <HCDI/hcdi_multicfgkernelmgr.h>

static void GetCurrentRadiossStrVersion(int version, std::string& rad_version)
{
    if (version >= 110)
    {
        rad_version = "radioss" + std::to_string(version);
    }
    else
        rad_version = "radioss110";
}

#include "radiossblk.h"

#include <sdiCFGTypeMapper.h>
#include <sdiModelViewPO.h>

namespace sdi
{

// forward declaration
class SDIModelViewRadiossblk;

class SDIIdManagerRadiossblk : public SDIIdManager
{
public:
    SDIIdManagerRadiossblk(const SDIModelViewRadiossblk& mv);

    virtual void GetOffsets(unsigned int includeId, std::vector<int>& offsets);

    virtual void Init() { SDIIdManager::Init("#include"); }
};


class SDIModelViewRadiossblk: public ModelViewPO
{
public:
    SDIModelViewRadiossblk(std::vector<IMECPreObject*> (&preobjects)[HCDI_OBJ_TYPE_HC_MAX]) :
        ModelViewPO(SDICFGTypeMapper(), preobjects, HCDI_OBJ_TYPE_HC_MAX)
    {
        p_pIdManager = new SDIIdManagerRadiossblk(*this);

        // we cache information about includes for direct access
        CacheIncludes();

        //myTypeElementOwner = GetEntityType("/PART");
        //myTypeNode = GetEntityType("/NODE");
        myTypeSkew = GetEntityType("/SKEW");
        myTypeGrnod = GetEntityType("/GRNOD");
        myTypeUnit = GetEntityType("/UNIT");
        myTypeInclude = GetEntityType("#include");
        //myTypeSubmodel = GetEntityType("//SUBMODEL");
        myTypeParameter = GetEntityType("/PARAMETER");
    }

    virtual ~SDIModelViewRadiossblk()
    {
    }

    void CacheIncludes()
    {
        // we cache information about includes for direct access
        p_includes.resize(0);

        // we cache the hierarchy of includes
        p_parentIncludes[0] = 0; // main file
    }

    /// Creation of a new entity
    virtual Status CreateEntity(
        HandleEdit&               handle,
        const sdiString&           keyword,
        const sdiString&           name = "",
        const unsigned int        id = 0)
    {
        unsigned int validId = id;
        if(id == 0 && !keyword.compare("/SET/GENERAL"))
        {
            unsigned int maxGroupId = GetNextAvailableId(GetEntityType("/GRBEAM"));
            if (GetNextAvailableId(GetEntityType("/GRBRIC")) > maxGroupId) 
                maxGroupId = GetNextAvailableId(GetEntityType("/GRBRIC"));
            if (GetNextAvailableId(GetEntityType("/GRNOD")) > maxGroupId) 
                maxGroupId = GetNextAvailableId(GetEntityType("/GRNOD"));
            if (GetNextAvailableId(GetEntityType("/GRPART")) > maxGroupId) 
                maxGroupId = GetNextAvailableId(GetEntityType("/GRPART"));
            if (GetNextAvailableId(GetEntityType("/GRQUAD")) > maxGroupId) 
                maxGroupId = GetNextAvailableId(GetEntityType("/GRQUAD"));
            if (GetNextAvailableId(GetEntityType("/GRSH3N")) > maxGroupId) 
                maxGroupId = GetNextAvailableId(GetEntityType("/GRSH3N"));
            if (GetNextAvailableId(GetEntityType("/GRSHEL")) > maxGroupId) 
                maxGroupId = GetNextAvailableId(GetEntityType("/GRSHEL"));
            if (GetNextAvailableId(GetEntityType("/GRSPRI")) > maxGroupId) 
                maxGroupId = GetNextAvailableId(GetEntityType("/GRSPRI"));
            if (GetNextAvailableId(GetEntityType("/GRTRIA")) > maxGroupId) 
                maxGroupId = GetNextAvailableId(GetEntityType("/GRTRIA"));
            if (GetNextAvailableId(GetEntityType("/GRTRUS")) > maxGroupId) 
                maxGroupId = GetNextAvailableId(GetEntityType("/GRTRUS"));

            unsigned int maxSetId = GetNextAvailableId(GetEntityType("/SET"));

            if (maxGroupId > maxSetId )
                validId = maxGroupId + 1;
        }
        bool isOk = ModelViewPO::CreateEntity(handle, keyword, name, validId);

        if(!isOk) return false;
        if(keyword.compare(1, 9, "PARAMETER") == 0)
        {
        }
        else if(keyword.compare(1, 7, "include") == 0)
        {
        }

        return isOk;
    }

    virtual unsigned int GetNextAvailableId(EntityType entities) const
    {
        bool hasChanged = UpdateSubmodelOffsets();
        if(hasChanged && nullptr != p_pIdManager)
        {
            p_pIdManager->InitOffsets();
        }

        return ModelViewPO::GetNextAvailableId(entities);
    }

    virtual bool P_FindById(const unsigned int id, EntityType entityType, HandleRead& handle) const
    {
        unsigned int CFGType = GetCFGType(entityType);
        bool isOk = sdi::ModelViewPO::P_FindById(id, entityType, handle);
        if(!isOk && HCDI_OBJ_TYPE_SETS == CFGType)
        {
            // we have to hard-code the fact that if searching for a "classic" group,
            // there may either be the classic group as such, or the /SET
            static EntityType entityTypeSet = GetEntityType("/SET");
            if(entityType != entityTypeSet)
            {
                isOk = sdi::ModelViewPO::P_FindById(
                    id, entityTypeSet, handle);
            }
        }
        return isOk;
    }

    virtual HandleRead SetCurrentCollector(const HandleRead handle) const
    {
        HandleRead oldCollector;
        FindById(myTypeInclude, p_currentIncludeId, oldCollector);
        if(handle.GetType() == myTypeInclude)
        {
            p_currentIncludeId = handle.GetId(this);
        }
        else
        {
            p_currentIncludeId = 0;
        }
        if(nullptr != p_pIdManager) p_pIdManager->SetCurrentInclude(p_currentIncludeId);
        return oldCollector;
    }

    virtual POKeywordSet GetUnsortableKeywords(unsigned int CFGType) const
    {
        POKeywordSet keywordSet;
        switch(CFGType)
        {
        case HCDI_OBJ_TYPE_LOADS:
            keywordSet.insert("/INIVEL");
            break;
        case HCDI_OBJ_TYPE_TRANSFORMATIONS:
            keywordSet.insert("/TRANSFORM");
            break;
        default:
            break;
        }
        return keywordSet;
    }

    // Include file hierarchy
    unsigned int GetParentIncludeId(unsigned int childIncludeId) const
    {
        if(p_parentIncludes.count(childIncludeId)) return p_parentIncludes.at(childIncludeId);
        else                                       return 0;
    }

    void RebuildSubmodelHierarchy() const
    {
        for(unsigned int childId = 1; childId < p_parentIncludes.size(); ++childId)
        {
            HandleRead childHandle;
            FindById(myTypeInclude, childId, childHandle);
            assert(childHandle.IsValid());
            EntityRead childEnt(this, childHandle);
            sdiValue val;
            childEnt.GetValue(sdiIdentifier("shadow_submodelid"), val);
            unsigned int childSubmodelId = 0;
            val.GetValue(childSubmodelId);

            unsigned int parentId = GetParentIncludeId(childId);
            unsigned int parentSubmodelId = 0;
            while(0 == parentSubmodelId && 0 < parentId)
            {
                HandleRead parentHandle;
                FindById(myTypeInclude, parentId, parentHandle);
                assert(parentHandle.IsValid());
                EntityRead parentEnt(this, parentHandle);
                parentEnt.GetValue(sdiIdentifier("shadow_submodelid"), val);
                val.GetValue(parentSubmodelId);
                if(0 == parentSubmodelId) parentId = GetParentIncludeId(parentId);
            }
        }
    }


    // handling submodel offsets
    void SetSubmodelOffsetsDirty(bool isDirty = true) const { p_isSubmodelOffsetsDirty = isDirty; }
    bool IsSubmodelOffsetsDirty() const { return p_isSubmodelOffsetsDirty; }
    bool UpdateSubmodelOffsets() const
    {
        if(p_isSubmodelOffsetsDirty)
        {
            p_isSubmodelOffsetsDirty = false;
            return true;
        }
        return false;
    }

// private: we keep everything public. Anyways, this class is not visible outside of this file
    mutable std::vector<void*>p_includes;
    mutable std::map<unsigned int, unsigned int> p_parentIncludes;
    mutable bool p_isSubmodelOffsetsDirty = true;

public: // for EntityData, should rather be friend
    EntityType myTypeSkew, myTypeGrnod;
};


SDIIdManagerRadiossblk::SDIIdManagerRadiossblk(const SDIModelViewRadiossblk& mv)
    : SDIIdManager(mv, mv.GetTypeMapper().GetIdPools())
{}

void SDIIdManagerRadiossblk::GetOffsets(unsigned int includeId, std::vector<int>& offsets)
{
    // no offsets in main, nothing to do, so return:
    if(0 == includeId) return;

}


} // namespace sdi

static void RadiossblkMovePreobjects(
    std::vector<IMECPreObject *>* pre_obj_lst,
    size_t source_type, const std::set<std::string>& keywords,
    size_t target_type)
{
    if(source_type >= HCDI_OBJ_TYPE_HC_MAX || target_type >= HCDI_OBJ_TYPE_HC_MAX) return;
    size_t i_new = 0;
    for(size_t i_old = 0; i_old < pre_obj_lst[source_type].size(); ++i_old)
    {
        assert(pre_obj_lst[source_type][i_old]);
        const char *inputfulltype = pre_obj_lst[source_type][i_old]->GetInputFullType();
        if(keywords.count(pre_obj_lst[source_type][i_old]->GetInputFullType()) > 0)
        { // move
            pre_obj_lst[target_type].push_back(pre_obj_lst[source_type][i_old]);
        }
        else
        { // keep
            pre_obj_lst[source_type][i_new] = pre_obj_lst[source_type][i_old];
            ++i_new;
        }
    }
    pre_obj_lst[source_type].resize(i_new);
}

static unsigned int sUserProfileLoadedVersion = 2024;
static string rad_version("radioss2024");

extern "C" RADIOSSBLK_DECLS
void RadiossblkSetUserProfileVersion(unsigned int version)
{
    sUserProfileLoadedVersion = version;
}

static hwReaderMessageList radiossblkmessages;

static
void RadiossblkReadMessageFile()
{
    if(!radiossblkmessages.IsInitialized())
    {
        std::string msgfilename = getenv("HM_MSG_DIR");
        msgfilename += "/CONFIG/msg_hw_radioss_reader.txt";

        // Initialize message pool
        radiossblkmessages.ReadMessageFile(msgfilename);
        radiossblkmessages.SetOffset(100000);
    }
}

static bool RadiossblkInitCFGKernel()
{
    if (sUserProfileLoadedVersion != 0)
        GetCurrentRadiossStrVersion(sUserProfileLoadedVersion, rad_version);

    vector<string>  allowable_vec{ "HM_SUPPORTED" };
    std::string str_error;
    MultiCFGKernelMgr::getInstance().InitCFGKernel("", "", rad_version, "", false, allowable_vec, str_error);
    if(str_error.size() > 0)
    {
        radiossblkmessages.Add(9001, 3, "", 0, str_error.c_str());
        return false;
    }
    return true;
}

static std::vector<IMECPreObject *> pre_obj_lst[HCDI_OBJ_TYPE_HC_MAX];

#include <MODEL_IO/solverCDR.h>

extern "C" RADIOSSBLK_DECLS
sdi::ModelViewEdit* RadiossblkReadModel(const char *filename)
{
    RadiossblkReadMessageFile();
    if(!RadiossblkInitCFGKernel()) return nullptr;

    // read file
    SolverSyntaxInfos syntaxSolverInfos;
    SolverInputInfo solverInf;
    CommonDataReaderCFG reader("radioss", rad_version, "", true);
    reader.ReadModel(filename, pre_obj_lst);

    // create and return SDI model
    sdi::SDIModelViewRadiossblk *pModelView = 
        new sdi::SDIModelViewRadiossblk(pre_obj_lst);
    /* Radioss AQA requires this: */
    pModelView->SetSelectionSortMethod(sdi::SDIModelViewRadiossblk::SortByFileAndId);

    int back = 0;
    bool isDir = false;
    #ifdef WIN32
        const char *char_temp = getenv("TEMP");
        if (char_temp == NULL) {
            char_temp = getenv("TMP");
        }
        std::string Schar_temp(char_temp);
        std::string return_string = Schar_temp + "\\hw_inistate_tmp";
        std::string command_line  = ("RMDIR /S /Q  " + return_string);

        HANDLE hFind;
        WIN32_FIND_DATA FindFile;
        std::string file_list=return_string + "\\*";
        if((hFind = FindFirstFile(file_list.c_str(), &FindFile)) != INVALID_HANDLE_VALUE){
                FindClose(hFind);
                isDir = true;
        }else{
            isDir = false;
        }
    #else
        const char *char_temp = getenv("TMPDIR");
        if (char_temp == NULL) {
            char_temp = "/tmp";
        }
        std::string Schar_temp(char_temp);
        std::string return_string = Schar_temp + "/hw_inistate_tmp";
        std::string command_line  = ("rm -rf " + return_string);
        DIR *tmpDir = opendir(return_string.c_str());
        if(tmpDir) 
        {
            isDir = true;
            closedir(tmpDir);
        }
    #endif
    if(isDir) back = system(command_line.c_str());

    return pModelView;
}

extern "C" RADIOSSBLK_DECLS
bool RadiossblkReadInclude(sdi::ModelViewEdit *pModelView, const char *filename, const char *directoryname)
{
    return false;
}

extern "C" RADIOSSBLK_DECLS
sdi::ModelViewEdit* RadiossblkReadModelSDI(const char *filename)
{
    return RadiossblkReadModel(filename);
}

extern "C" RADIOSSBLK_DECLS
sdi::ModelViewEdit* RadiossblkNewModel()
{
    RadiossblkReadMessageFile(); // in case there is a problem in InitCFGKernel
    if(!RadiossblkInitCFGKernel()) return nullptr;

    // Create root preobject
    IMECPreObject *pObj = HCDI_GetPreObjectHandle("", "#include", "", 0, 0);
    if(nullptr != pObj)
    {
        pObj->SetFileIndex(-1);
        pre_obj_lst[HCDI_OBJ_TYPE_INCLUDEFILES].push_back(pObj);
    }

    return new sdi::SDIModelViewRadiossblk(pre_obj_lst);
}

extern "C" RADIOSSBLK_DECLS
void RadiossblkApplyOffsets(sdi::ModelViewEdit *pModelView, bool doUnOffset)
{
    sdi::SDIModelViewRadiossblk* pModelViewRadiossblk = dynamic_cast<sdi::SDIModelViewRadiossblk*>(pModelView);
    if(nullptr != pModelViewRadiossblk)
    {
        pModelViewRadiossblk->ApplyIdOffsets("SUBMODEL", doUnOffset);
    }
}

extern "C" RADIOSSBLK_DECLS
void RadiossblkUnApplyOffsets(sdi::ModelViewEdit *pModelView)
{
    RadiossblkApplyOffsets(pModelView, true);
}

#include <sdiEntity.h>
#include <HCDI/hcdi_mv_descriptor.h>
#include <HCDI/hcdi_dimensions.h>
#include <MODEL_IO/mec_pre_object_expression_evaluator.h> // in hwcommon/cfgio
#include <boost/algorithm/string/replace.hpp>

static void RadiossblkGetDimension(double *pVal, const MuQuantity_t &quantity, MuEQuantity_e dim,
    const IMECPreObject *pPreobj, const IDescriptor *pDescr, vector<string>* pSkwds)
{
    if(nullptr == pVal) return;

    if(nullptr != pSkwds && pSkwds->size() && pSkwds->size() == quantity.getArgsVect().size())
    {
        const string& expr = quantity.getExpr(dim);
        if(!expr.empty())
        {
            string skwdExpr(expr);
            for(size_t i = 0; i < pSkwds->size(); ++i)
            {
                if(quantity.getArgsVect()[i] != pSkwds->at(i))
                {
                    boost::replace_all(skwdExpr, quantity.getArgsVect()[i], pSkwds->at(i));
                }
            }
            // We are using a utility class from hcio here. This would add another dependency to
            // this SDI implementation, but we have it anyways due to the reader
            ExpressionEvaluatorExprTk evaluator;
            MECPreObjectExpressionEvaluator pre_object_handler(pPreobj, pDescr, &evaluator);
            *pVal = pre_object_handler.Evaluate(skwdExpr.c_str());
            return;
        }
    }

    *pVal = quantity[dim];
}

static bool RadiossblkGetDimensions(
    const sdi::EntityRead &entity, const IMECPreObject *pPreobj, const IDescriptor *pDescr,
    const std::string &dataname, double *lengthDim, double *massDim, double *timeDim)
{
    int ikeyword = END_ARGS;

    ikeyword = pDescr->getIKeyword(dataname);
    if(0 == ikeyword)
    {
        // nothing stored under this "skeyword", caller might have set the "solver name"
        const string &skwd = pDescr->getSKeywordFromSolverName(dataname);
        ikeyword = pDescr->getIKeyword(skwd);
    }

    MvDimension_e dimension = UDI_UNKNOWN;
    vector<string>* pSkwds = nullptr;
    const MvDataFeature_t *pFeature = pDescr->getIkeywordDataFeature(DOM_COMMON, ikeyword);
    //MvDimension_e dimension = UDI_UNKNOWN;
    vector<pair<MvDimension_e, vector<string>>> dim_lst;
    HCDI_GetDimensionFromFeature<sdi::EntityRead, sdiIdentifier, sdiValue, sdiValueEntity, sdiString>
        (&entity, pDescr, pFeature,
        dim_lst, ikeyword, -1, DIMEN_IDEN_BY_SKEY);
    if(dim_lst.size() > 0)
    {
        dimension = dim_lst[0].first;
        if(dim_lst[0].second.size() > 0) pSkwds = &(dim_lst[0].second);
    }

    if(UDI_UNKNOWN != dimension)
    {
        //        printf( "  dataname %s , value = %f \n",dataname,*pValue);
        //        printf( "  dimension  %s \n",MV_get_dimension(dimension).c_str());

        const MuQuantity_t &quantity = MU_get_quantity(dimension);
        RadiossblkGetDimension(lengthDim, quantity, QTY_LENGTH, pPreobj, pDescr, pSkwds);
        RadiossblkGetDimension(massDim, quantity, QTY_MASS, pPreobj, pDescr, pSkwds);
        RadiossblkGetDimension(timeDim, quantity, QTY_TIME, pPreobj, pDescr, pSkwds);

        return true;
    }

    return false;
}

static const sdi::ModelViewPO *RadiossblkGetModelViewPO(
    const sdi::ModelViewRead *pModelView, sdi::EntityType type = sdi::ENTITY_TYPE_NONE)
{
    const sdi::ModelViewPO *pSDIModelViewPO = dynamic_cast<const sdi::ModelViewPO *>(pModelView);
    if(nullptr != pSDIModelViewPO &&
       !pSDIModelViewPO->IsContained(type))
    {
        pSDIModelViewPO = nullptr;
    }
    return pSDIModelViewPO;
}

RADIOSSBLK_DECLS
bool RadiossblkGetDimensions(
    const sdi::EntityRead &entity, const char *dataname, double *lengthDim, double *massDim, double *timeDim)
{
    // We cannot do anything for entities which aren't stored as PreObjects
    const sdi::ModelViewPO *pSDIModelViewPO = RadiossblkGetModelViewPO(
        entity.GetModelViewRead(), entity.GetType());
    if(nullptr == pSDIModelViewPO) return false;

    const IMECPreObject *pObj = (const IMECPreObject *) entity.GetHandle().GetPointer();
    const IDescriptor *pDescr = pSDIModelViewPO->GetDescriptor(pObj);
    if(!pDescr) return false;

    bool hasDim = RadiossblkGetDimensions(entity, pObj, pDescr, dataname, lengthDim, massDim, timeDim);

    if(!hasDim)
    {
        // if no success in pObj, try subobjects
        const vector<IMECPreObject *>& subobjects = pObj->GetSubobject();
        for(unsigned int i = 0; i != subobjects.size(); ++i)
        {
            const IMECPreObject *pSubobject = subobjects[i];
            if(!pSubobject) continue;
            const IDescriptor *pSubobjectDescr =
                pSDIModelViewPO->GetDescriptorFromKernelType(pSubobject->GetKernelFullType());
            if(!pSubobjectDescr) continue;
            // NB: We are passing entity, not the subobject, but GetValue calls on the entity should be passed
            // through to the subobjects by the SDI implementation
            hasDim = RadiossblkGetDimensions(entity, pSubobject, pSubobjectDescr,
                dataname, lengthDim, massDim, timeDim);
            if(hasDim) break;
        }
    }

    return hasDim;
}

RADIOSSBLK_DECLS
bool RadiossblkGetValueDouble(
    const sdi::EntityRead &entity, const char *dataname, double *pValue,
    double *lengthDim, double *massDim, double *timeDim, unsigned int index, unsigned int col)
{
    sdiIdentifier identifier(dataname, 0, index, col);
    sdiValue value;
    bool hasValue = entity.GetValue(identifier, value);
    if(pValue && hasValue)
    {
        if(value.GetBasicType() != BASIC_TYPE_DOUBLE) return false;
        if(value.GetArrayDimension() > 0)                           return false;
        hasValue = value.GetValue(*pValue);
    }

    // dimension
    RadiossblkGetDimensions(entity, dataname, lengthDim, massDim, timeDim);

    return hasValue;
}

RADIOSSBLK_DECLS
bool RadiossblkGetValueDouble(
    const sdi::EntityRead &entity,const char *dataname,double *pValue,
    int *lengthDim,int *massDim,int *timeDim,unsigned int index, unsigned int col)
{
    double lengthDim_d = 0, massDim_d = 0, timeDim_d = 0;
    bool isOk = RadiossblkGetValueDouble(
        entity,dataname,pValue,
        &lengthDim_d, &massDim_d, &timeDim_d, index, col);
    if(lengthDim) *lengthDim = (int) lengthDim_d;
    if(massDim)   *massDim   = (int) massDim_d;
    if(timeDim)   *timeDim   = (int) timeDim_d;
    return isOk;
}


static bool P_SDIGetGroupType(sdi::SDIModelViewRadiossblk* pSDIModelViewRadiossblk,
    IMECPreObject* pObj,
    const IDescriptor* pDescr, int ikwd, const string& skwd, sdi::EntityType& type)
{
    type = sdi::ENTITY_TYPE_NONE; // init to "not a group"

    // if it's neither set nor multiobject it's not a group
    obj_type_e cfgType = pDescr->getObjectType(ikwd);
    if(HCDI_OBJ_TYPE_SETS != cfgType && HCDI_OBJ_TYPE_MULTIOBJECT != cfgType) return true;

    // get and query "_type" attribute
    string typeSkwd = skwd + "_type";
    const char* typeValue = pObj->GetStringValue(typeSkwd.c_str());
    if(nullptr != typeValue && strlen(typeValue) > 1)
    { // there is a valid "_type" attribute
        const char* subtype = nullptr;
        if     (strncmp(typeValue, "SETS",  4) == 0) subtype = typeValue + 4;
        else if(strncmp(typeValue, "/SETS", 5) == 0) subtype = typeValue + 5;

        // if we can tell it's not a set, return ok
        if(nullptr == subtype) return true;

        // if we can get a valid subtype, return it
        if('/' == *subtype) ++subtype;
        sdiString keyword(sdiString("/") + subtype);
        if(keyword.size() > 7 && keyword.compare(keyword.size() - 7, 7, "_IDPOOL") == 0)
        { // remove "_IDPOOL" in the end
            keyword.resize(keyword.size() - 7);
        }
        type = pSDIModelViewRadiossblk->GetEntityType(keyword);
        if(sdi::ENTITY_TYPE_NONE != type) return true;

        // if we get here, it's a set but we cannot determine the subtype
        return false;
    }

    // second trial: check whether there is one possible /SETS subtype in the attribute
    bool isOk = false;
    // this is a bit of a hack into hcdi "private" stuff, a clean API would be better
    const descriptor_t* cdescr_p = pDescr->getDescriptorPtr();
    object_descriptor_t* objdescr_p = (object_descriptor_t*)(cdescr_p->attdescr_array[ikwd]);
    if(nullptr == objdescr_p) return false; // shouldn't happen
    int nbSubtypes = objdescr_p->num;
    for(int i = 0; i < nbSubtypes; ++i)
    {
        if( HCDI_OBJ_TYPE_SETS == objdescr_p->allowed_types[i] &&
            nullptr != objdescr_p->subtypes[i] &&
            strlen(objdescr_p->subtypes[i]) > 0)
        {
            if(isOk) // this is already the second one => not usable
            {
                isOk = false;
                break;
            }
            isOk = true;
            sdiString keyword(sdiString("/") + objdescr_p->subtypes[i]);
            if(keyword.size() > 7 && keyword.compare(keyword.size() - 7, 7, "_IDPOOL") == 0)
            { // remove "_IDPOOL" in the end
                keyword.resize(keyword.size() - 7);
            }
            type = pSDIModelViewRadiossblk->GetEntityType(keyword);
            assert(sdi::ENTITY_TYPE_NONE != type);
            if(sdi::ENTITY_TYPE_NONE == type) return false; // shouldn't happen
        }
        else if(HCDI_OBJ_TYPE_SETS == objdescr_p->allowed_types[i])
        {
            // if we get here, there is /SETS without subtype
            return false;
        }
    }

    return isOk;
}

static void P_PopulateUsedGroups(IMECPreObject* pObj,
    set<pair<sdi::EntityType, unsigned int>>& usedGroups,
    sdi::SDIModelViewRadiossblk* pSDIModelViewRadiossblk)
{
    if(nullptr == pObj) return; // shouldn't happen
    // other pointers are checked in calling function

    // populate for preobject itself
    const IDescriptor* pDescr = pSDIModelViewRadiossblk->GetDescriptor(pObj);
    if(nullptr == pDescr) return;
    int ikwd = pDescr->getFirstIKeyword();
    while(END_ARGS != ikwd)
    {
        if(VTYPE_OBJECT != pDescr->getValueType(ikwd)) // not an object => continue
        {ikwd = pDescr->getNextIKeyword(ikwd); continue;}

        string skwd = pDescr->getSKeyword(ikwd);
        IMECPreObject::MyAttributeType_e atype = 
            HCDIGetPATypeFromDAType(pDescr->getAttributeType(ikwd));

        int index = pObj->GetIndex(atype, IMECPreObject::VTY_OBJECT, skwd);
        if(0 > index) // value not present in pObj => continue
        {ikwd = pDescr->getNextIKeyword(ikwd); continue;}

        sdi::EntityType type = sdi::ENTITY_TYPE_NONE;
        bool isOk = P_SDIGetGroupType(pSDIModelViewRadiossblk,
            pObj, pDescr, ikwd, skwd, type);

        if(isOk && sdi::ENTITY_TYPE_NONE == type) // not a group => continue
        {ikwd = pDescr->getNextIKeyword(ikwd); continue;}

        if(IMECPreObject::ATY_SINGLE == atype)
        {
            unsigned int id = (unsigned int) pObj->GetObjectId(index);
            usedGroups.insert(make_pair(type, id));
        }
        else if(IMECPreObject::ATY_ARRAY == atype)
        {
            int nbObjs = pObj->GetNbValues(IMECPreObject::VTY_OBJECT, index);
            for(int i = 0; i < nbObjs; ++i)
            {
                unsigned int id = (unsigned int) pObj->GetObjectId(index, i);
                usedGroups.insert(make_pair(type, id));
            }
        }

        ikwd = pDescr->getNextIKeyword(ikwd);
    }

    // recursive call for subobjects
    const vector<IMECPreObject *>& subobjects = pObj->GetSubobject();
    for(size_t i = 0; i < subobjects.size(); ++i)
    {
        P_PopulateUsedGroups(subobjects[i], usedGroups,
            pSDIModelViewRadiossblk);
    }
}

RADIOSSBLK_DECLS
bool RadiossblkIsGroupUsed(
    sdi::ModelViewRead *pModelView, const sdi::EntityType type, const unsigned int id)
{
    static set<pair<sdi::EntityType, unsigned int>> usedGroups;

    // populate if necessary
    // NB: we have no possibility to clean and repopulate, to be added when needed!
    if(usedGroups.empty())
    {
        sdi::SDIModelViewRadiossblk* pSDIModelViewRadiossblk = 
            dynamic_cast<sdi::SDIModelViewRadiossblk*>(pModelView);
        assert(nullptr != pSDIModelViewRadiossblk);
        if(nullptr == pSDIModelViewRadiossblk) return false;

        // loop over all hcdi entity types, i.e. their corresponding containers
        // 1 and 2 are nodes and elements, so we start with 3
        for(int i = HCDI_OBJ_TYPE_ELEMS + 1; i < HCDI_OBJ_TYPE_GEOMETRY; i++)
        {
            if(pre_obj_lst[i].size() == 0) continue;
            if(HCDI_OBJ_TYPE_SETS == i) continue; // sets referenced by other sets are converted on the fly

            // loop over all preobjects in the container
            for(IMECPreObject* pObj : pre_obj_lst[i])
            {
                P_PopulateUsedGroups(pObj, usedGroups, pSDIModelViewRadiossblk);
            }
        }
    }

    return 
        usedGroups.count(make_pair(type, id)) > 0 || 
        usedGroups.count(make_pair(sdi::ENTITY_TYPE_NONE, id)) > 0;
}


static unsigned int P_GetEntityCFGType(IMECPreObject* pObj,
    const IDescriptor* pDescr, int ikwd, const string& skwd)
{
    obj_type_e cfgType = pDescr->getObjectType(ikwd);
    if(HCDI_OBJ_TYPE_SUBOBJECT == cfgType) return HCDI_OBJ_TYPE_NULL;

    if(HCDI_OBJ_TYPE_MULTIOBJECT != cfgType) return (unsigned int) cfgType;

    // for multiobject, get and query "_type" attribute
    string typeSkwd = skwd + "_type";
    const char* typeValue = pObj->GetStringValue(typeSkwd.c_str());
    if(nullptr != typeValue && strlen(typeValue) > 1)
    { // there is a valid "_type" attribute
        cfgType = HCDI_get_entitytype(typeValue);
        return (unsigned int) cfgType;
    }

    return HCDI_OBJ_TYPE_NULL;
}

static void P_PopulateEntityReferences(
    IMECPreObject* pObj,
    unsigned int refedType,
    map<unsigned int, vector<RadiossblkEntityReference>>& typeEntityReferences,
    sdi::SDIModelViewRadiossblk* pSDIModelViewRadiossblk)
{
    if(nullptr == pObj) return; // shouldn't happen
                                // other pointers are checked in calling function

    // populate for preobject itself
    const IDescriptor* pDescr = pSDIModelViewRadiossblk->GetDescriptor(pObj);
    if(nullptr == pDescr) return;
    int ikwd = pDescr->getFirstIKeyword();
    while(END_ARGS != ikwd)
    {
        if(VTYPE_OBJECT != pDescr->getValueType(ikwd)) // not an object => continue
        {ikwd = pDescr->getNextIKeyword(ikwd); continue;}

        string skwd = pDescr->getSKeyword(ikwd);
        IMECPreObject::MyAttributeType_e atype = 
            HCDIGetPATypeFromDAType(pDescr->getAttributeType(ikwd));

        int index = pObj->GetIndex(atype, IMECPreObject::VTY_OBJECT, skwd);
        if(0 > index) // value not present in pObj => continue
        {ikwd = pDescr->getNextIKeyword(ikwd); continue;}

        unsigned int type =
            P_GetEntityCFGType(pObj, pDescr, ikwd, skwd);

        if(refedType != type) // not the right type => continue
        {ikwd = pDescr->getNextIKeyword(ikwd); continue;}

        // if we come here, we have found
        string solvername = pDescr->getSolverName(ikwd);
        const char* inputType = pObj->GetInputFullType();
        unsigned int refingId = pObj->GetId();
        if(IMECPreObject::ATY_SINGLE == atype)
        {
            unsigned int refedId = (unsigned int) pObj->GetObjectId(index);
            vector<RadiossblkEntityReference>& entityReferences = typeEntityReferences[refedId];
            RadiossblkEntityReference entityReference(refingId, inputType, skwd, solvername);
            entityReferences.push_back(entityReference);
        }
        else if(IMECPreObject::ATY_ARRAY == atype)
        {
            int nbObjs = pObj->GetNbValues(IMECPreObject::VTY_OBJECT, index);
            for(int i = 0; i < nbObjs; ++i)
            {
                unsigned int refedId = (unsigned int) pObj->GetObjectId(index, i);
                vector<RadiossblkEntityReference>& entityReferences = typeEntityReferences[refedId];
                RadiossblkEntityReference entityReference(refingId, inputType, skwd, solvername,
                    (unsigned int) i);
                entityReferences.push_back(entityReference);
            }
        }

        ikwd = pDescr->getNextIKeyword(ikwd);
    }

    // recursive call for subobjects
    const vector<IMECPreObject *>& subobjects = pObj->GetSubobject();
    for(size_t i = 0; i < subobjects.size(); ++i)
    {
        P_PopulateEntityReferences(subobjects[i], refedType,
            typeEntityReferences, pSDIModelViewRadiossblk);
    }
}

RADIOSSBLK_DECLS
const std::vector<RadiossblkEntityReference>& RadiossblkGetEntityReferences(
    sdi::ModelViewRead *pModelView, const sdi::EntityType type, const unsigned int id)
{
    static map<
        sdi::EntityType,                            // type of referenced entity
        pair<bool,                                    // flag whether already populated
             map<unsigned int,                        // id of referenced entity vs
                 vector<RadiossblkEntityReference>>>> // list of referencing entities
        AllEntityReferences;

    static vector<RadiossblkEntityReference> nullReferences;

    pair<bool, map<unsigned int, vector<RadiossblkEntityReference>>>& typeEntityReferencesWithFlag =
        AllEntityReferences[type];
    bool& isPopulated = typeEntityReferencesWithFlag.first;
    map<unsigned int, vector<RadiossblkEntityReference>>& typeEntityReferences =
        typeEntityReferencesWithFlag.second;

    // populate if necessary
    // NB: we have no possibility to clean and repopulate, to be added when needed!
    if(!isPopulated)
    {
        isPopulated = true;
        sdi::SDIModelViewRadiossblk* pSDIModelViewRadiossblk = 
            dynamic_cast<sdi::SDIModelViewRadiossblk*>(pModelView);
        assert(nullptr != pSDIModelViewRadiossblk);
        if(nullptr == pSDIModelViewRadiossblk) return nullReferences;

        // For now, we get all matching references for the EDI type corresponding to the
        // requested SDI type.
        // This is only what the user expects in the case that the requested SDI type is
        // the only one mapped to "its" EDI type
        unsigned int CFGType = pSDIModelViewRadiossblk->GetCFGType(type);

        // loop over all hcdi entity types, i.e. their corresponding containers
        for(int i = 0; i < HCDI_OBJ_TYPE_GEOMETRY; i++)
        {
            if(pre_obj_lst[i].size() == 0) continue;
            if(HCDI_OBJ_TYPE_SETS == i) continue; // sets referenced by other sets are converted on the fly

                                                  // loop over all preobjects in the container
            for(IMECPreObject* pObj : pre_obj_lst[i])
            {
                P_PopulateEntityReferences(pObj, CFGType,
                    typeEntityReferences, pSDIModelViewRadiossblk);
            }
        }
    }

    if(typeEntityReferences.count(id) > 0) return typeEntityReferences[id];
    else                                   return nullReferences;
}


extern "C" RADIOSSBLK_DECLS
const hwReaderMessageList& RadiossblkGetMessages()
{
    return radiossblkmessages;
}

static unsigned int majorVersion  = 2020;
static unsigned int minorVersion  = 0;
static unsigned int hotfixVersion = 0;
static unsigned int buildNumber   = 14;
static char versionString[100] = "";

extern "C" RADIOSSBLK_DECLS
const char *RadiossblkGetVersion(unsigned int *pmajorVersion  = 0,
                                 unsigned int *pminorVersion  = 0,
                                 unsigned int *photfixVersion = 0,
                                 unsigned int *pbuildNumber   = 0)
{
    if(strlen(versionString) == 0)
    {
        sprintf(versionString, "%u.%u.%u.%u", majorVersion, minorVersion, hotfixVersion, buildNumber);
    }
    if(pmajorVersion)  *pmajorVersion  = majorVersion;
    if(pminorVersion)  *pminorVersion  = minorVersion;
    if(photfixVersion) *photfixVersion = hotfixVersion;
    if(pbuildNumber)   *pbuildNumber   = buildNumber;
    return versionString;
}
