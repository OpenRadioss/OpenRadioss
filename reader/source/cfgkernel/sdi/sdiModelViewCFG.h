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




////////////////////////////////////////////////////////////////////////////////////

#if !defined(SDIMODELVIEWCFG__INCLUDED_)
#define SDIMODELVIEWCFG__INCLUDED_


#include <_private/sdiModelViewPrivate.h>
#include <_private/sdiEntityData.h>
#include <_private/sdiElementData.h>
#include <_private/sdiNodeData.h>

#include <HCDI/hcdi_mv_descriptor.h>
#include <HCDI/hcdi_utils.h>
#include <HCDI/hcdi_multicfgkernelmgr.h>

#include <cfgio/MODEL_IO/meci_syntax_infos.h>
#include <cfgio/MODEL_IO/mv_solver_input_infos.h>

#include <sdi/tools/sdiTypeMapper.h>
#include <sdi/tools/sdiIdManager.h>
#include <sdi/tools/sdiSelectionObserver.h>

namespace sdi
{

// Template for CFG-based EntityData
template<sdi::SpecializationType SPECIALTYPE>
class TSDIEntityDataCFG : public SDITypeGuide<SPECIALTYPE>::data
{
public:
    TSDIEntityDataCFG(SDIModelViewPrivate* pModel) :
        SDITypeGuide<SPECIALTYPE>::data(pModel)
    {
    }

    virtual const IDescriptor *GetDescriptor() const;

    // derived classes should implement to populate p_keyword if empty
    virtual const sdiString& GetKeyword() const
    {
        return p_keyword;
    }

protected:
    sdiString p_keyword;
    mutable const IDescriptor *p_pDescr = nullptr;
};


// class ModelViewCFG : public SDIModelViewPrivate
// SelectionObserver hack:

/// ModelView for a DB using the CFG infrastructure.
class ModelViewCFG : public SDIModelViewSelectionObservable
{
public:

    //! Constructor
    ModelViewCFG(const SDITypeMapper& typemapper, const CFGKernel *pCFGKernel=nullptr) :
        p_typemapper(typemapper), p_pCFGKernel(pCFGKernel)
    {
        if(!p_pCFGKernel) p_pCFGKernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    }

    virtual ~ModelViewCFG()
    {
        if(nullptr != p_pIdManager) delete p_pIdManager;
    }

    // Type related API

    virtual EntityType GetEntityType(const sdiString& keyword) const
    {
        return p_typemapper.GetEntityType(keyword);
    }

    virtual const sdiString & GetKeyword(EntityType type) const
    {
        return p_typemapper.GetKeyword(type);
    }

    virtual SpecializationType GetSpecializationType(EntityType type) const
    {
        switch(GetCFGType(type))
        {
        case HCDI_OBJ_TYPE_NODES: return SPECIALIZATION_TYPE_NODE;
        case HCDI_OBJ_TYPE_ELEMS: return SPECIALIZATION_TYPE_ELEMENT;
        default:                  return SPECIALIZATION_TYPE_GENERAL;
        }
    }

    virtual EntityType GetMaxEntityType() const
    {
        return p_typemapper.GetMaxEntityType();
    }

    virtual unsigned int GetCFGType(EntityType type) const
    {
        return p_typemapper.GetDBType(type);
    }

    const SDITypeMapper& GetTypeMapper() const { return p_typemapper; }

    virtual unsigned int GetNextAvailableId(EntityType entities) const
    {
        // if there is an id manager, it takes care:
        if(nullptr != p_pIdManager) return p_pIdManager->GetNextAvailableId(entities);
        // if not successful, use base implementation
        return SDIModelViewPrivate::GetNextAvailableId(entities);
    }

    virtual HandleRead SetCurrentCollector(const HandleRead handle) const
    {
        HandleRead oldCollector;
        if(handle.GetType() == myTypeInclude)
        {
            FindById(myTypeInclude, p_currentIncludeId, oldCollector);
            p_currentIncludeId = handle.GetId(this);
            if(nullptr != p_pIdManager) p_pIdManager->SetCurrentInclude(p_currentIncludeId);
        }
        return oldCollector;
    }

public:

    virtual const IDescriptor* GetDescriptorFromKernelType(const char* kft) const
    {
        if(nullptr != p_pCFGKernel)
        {
            MultiCFGKernelMgr::CFGKernelSentinel sentinel(p_pCFGKernel->getSubUserProfile()); // workaround
            return p_pCFGKernel->GetDescriptorHandle(kft);
        }
        else return HCDI_GetDescriptorHandle(kft);
    }

    virtual const IDescriptor* GetDescriptor(const sdiString& keyword) const
    {
        const IDescriptor* pDescr = nullptr;
        EntityType type = GetEntityType(keyword);
        unsigned int cfgtype = GetCFGType(type);
        string header_str(p_syntaxSolverInfos.getNormalisedHeader(keyword.c_str()));
        auto headerdata = p_solverInf.GetKeywordSolverInfoFromHeaderLine(
            header_str, &p_syntaxSolverInfos, nullptr, (obj_type_e) cfgtype);
        if(nullptr != headerdata) pDescr = headerdata->pdescrp;
        return pDescr;
    }

    virtual sdiString GetKernelFullType(const char *keyword, const IDescriptor** ppDescr = nullptr) const
    {
        if(nullptr != ppDescr) *ppDescr = GetDescriptor(keyword);
        return sdiString();
    }

    const IDescriptor* GetDescriptor(const SDIEntityData* data) const
    {
        switch(GetSpecializationType(data->GetType()))
        {
        case SPECIALIZATION_TYPE_GENERAL:
            return static_cast<const TSDIEntityDataCFG<SPECIALIZATION_TYPE_GENERAL>*>(data)->GetDescriptor();
        case SPECIALIZATION_TYPE_ELEMENT:
            return static_cast<const TSDIEntityDataCFG<SPECIALIZATION_TYPE_ELEMENT>*>(data)->GetDescriptor();
        case SPECIALIZATION_TYPE_NODE:
            return static_cast<const TSDIEntityDataCFG<SPECIALIZATION_TYPE_NODE>*>(data)->GetDescriptor();
        default:
            return nullptr;
        }
    }

    EntityType GetEntityType(const IDescriptor *pDescr, int ikwd) const
    {
        if(nullptr == pDescr || END_ARGS == ikwd) return ENTITY_TYPE_NONE;
        // 1st trial: check for 1to1 mapping
        object_type_e CFGType = pDescr->getObjectType(ikwd);
        EntityType type = ENTITY_TYPE_NONE;
        if(HCDI_OBJ_TYPE_MULTIOBJECT != CFGType)
        {
            bool is1to1 = false;
            type = p_typemapper.GetEntityType(CFGType, is1to1);
            if(is1to1) return type;
        }

        /*
        // second trial: get and query "_type" attribute
        string typeSkwd = skwd + "_type";
        const char* typeValue = pObj->GetStringValue(typeSkwd.c_str());
        if(nullptr != typeValue && strlen(typeValue) > 1)
        { // there is a valid "_type" attribute
            if('/' == typeValue[0]) ++typeValue; // skip leading '/' if there is
            const char* subtype = strchr(typeValue, '/');
            if(nullptr != subtype)
            {
                // there is a subtype, it should be a type-keyword
                ++subtype; // go past the '/'
                sdiString keyword(p_typemapper.GetKeywordPrefix() + subtype);
                if(keyword.size() > 7 && keyword.compare(keyword.size() - 7, 7, "_IDPOOL") == 0)
                { // remove "_IDPOOL" in the end
                    keyword.resize(keyword.size() - 7);
                }
                type = p_typemapper.GetEntityType(keyword);
                if(ENTITY_TYPE_NONE != type) return type;
            }
            else
            {
                // there is no subtype, the type should be an hcdi-type-keyword
                bool is1to1 = false;
                CFGType = HCDI_get_entitytype(typeValue);
                type = p_typemapper.GetEntityType(CFGType, is1to1);
                if(is1to1) return type;
            }
        }
        */

        // third trial: check whether there is a subtype in the attribute
        // this is a bit of a hack into hcdi "private" stuff, a clean API would be better
        const descriptor_t* cdescr_p = pDescr->getDescriptorPtr();
        object_descriptor_t* objdescr_p = (object_descriptor_t*)(cdescr_p->attdescr_array[ikwd]);
        if(nullptr == objdescr_p) return type; // shouldn't happen
        int nbSubtypes = objdescr_p->num;
        for(int i = 0; i < nbSubtypes; ++i)
        {
            if(nullptr != objdescr_p->subtypes[i] &&
               strlen(objdescr_p->subtypes[i]) > 0)
            {
                // there is a subtype, it should be a type-keyword
                sdiString keyword(p_typemapper.GetKeywordPrefix() + objdescr_p->subtypes[i]);
                if(keyword.size() > 7 && keyword.compare(keyword.size() - 7, 7, "_IDPOOL") == 0)
                { // remove "_IDPOOL" in the end
                    keyword.resize(keyword.size() - 7);
                }
                type = p_typemapper.GetEntityType(keyword);
                if(ENTITY_TYPE_NONE != type) return type;
            }
            else
            {
                // there is no subtype, the type should be an hcdi-type-keyword
                bool is1to1 = false;
                type = p_typemapper.GetEntityType(objdescr_p->allowed_types[i], is1to1);
                if(is1to1) return type;
            }
        }

        /*
        // if we get here, there might be some sort of id-pooling in solver and
        // hcdi at the same time. Example: *DEFINE_CURVE and *DEFINE_TABLE,
        // which both are mapped to HCDI_OBJ_TYPE_CURVES. therefore we return
        // the type that exists in the model.
        std::vector<IMECPreObject*>::iterator itr = P_FindById(id, CFGType);
        if(p_preobjects[CFGType].end() != itr && (*itr))
        {
            const char* ift = (*itr)->GetInputFullType();
            if(nullptr != ift)
            {
                type = GetEntityType(sdiString(ift));
                return type;
            }
        }
        */

        // ideally we shouldn't come here
        return type;
    }

    //! Accessors
    const CFGKernel* GetCFGKernel() const { return p_pCFGKernel; }
    SDIIdManager* GetIdManager() const { return p_pIdManager; }

protected:
    const CFGKernel *p_pCFGKernel = nullptr;
    const SDITypeMapper p_typemapper;
    SDIIdManager* p_pIdManager = nullptr;
    mutable unsigned int p_currentIncludeId = 0;

private:
    // initialized from current cfg kernel at construction time:
    SolverSyntaxInfos p_syntaxSolverInfos;
    SolverInputInfo p_solverInf;

public: // for EntityData, should rather be friend
    EntityType myTypeInclude = HCDI_OBJ_TYPE_INCLUDEFILES,
        myTypeParameter = ENTITY_TYPE_NONE,
        myTypeUnit = ENTITY_TYPE_NONE;
}; // ModelViewCFG class


template<sdi::SpecializationType SPECIALTYPE>
const IDescriptor *TSDIEntityDataCFG<SPECIALTYPE>::GetDescriptor() const
{
    if(!p_pDescr)
    {
        const ModelViewCFG* mv = static_cast<const ModelViewCFG*>(this->GetModelView());
        assert(mv);
        p_pDescr=mv->GetDescriptor(GetKeyword());
    }
    return p_pDescr;
}


} // namespace sdi

#endif //! !defined(SDIMODELVIEWCFG__INCLUDED_)
