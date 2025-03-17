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

#if !defined(SDIMODELVIEWPO__INCLUDED_)
#define SDIMODELVIEWPO__INCLUDED_


#include "sdiModelViewCFG.h"

#include <_private/sdiElementData.h>
#include <_private/sdiNodeData.h>
#include <_private/sdiSelectionData.h>

// hcdi_cond_keyword_list.h indirectly includes hcdi_dimensions.h, which needs this:
#include <HCDI/hcdi_cond_keyword_list.h>
#include <HCDI/hcdi_mec_pre_object.h>
#include <KERNEL/mv_data_data_feature.h>

#include <MODEL_IO/hcioi_utils.h>

// *********************************************************************************
// Utilities for a PreObject based SDI implementation
// *********************************************************************************

// forward declaration
namespace sdi
{
class ModelViewPO;

static bool POCompareById(const IMECPreObject *pObj1, const IMECPreObject *pObj2)
{
    if     (!pObj1) return false; // invalid ones to the end, but this doesn't matter in fact
    else if(!pObj2) return true;
    unsigned int id1 = pObj1->GetId(), id2 = pObj2->GetId();
    if      (0 == id1)             return false; // id 0 in the end
    else if (0 != id1 && 0 == id2) return true;  // id 0 in the end
    else                           return id1 < id2;
}

class POCompareKeyword
{
public:
    bool operator()(const char* kwd1, const char* kwd2) const
    {
        size_t len  = strlen(kwd1);
        size_t len2 = strlen(kwd2);
        if(len > len2) len = len2;
        return strncmp(kwd1, kwd2, len) < 0;
    }
};

typedef set<const char*, POCompareKeyword> POKeywordSet;

class POCompareByFileAndId
{
public:
    inline POCompareByFileAndId(const ModelViewPO* pModel, unsigned int CFGType);
    bool operator()(const IMECPreObject *pObj1, const IMECPreObject *pObj2)
    {
        if(!pObj1)      return false; // invalid ones to the end, but this doesn't matter in fact
        else if(!pObj2) return true;
        if(!unsortableKeywords.empty())
        {
            if(IsUnsortable(pObj1)) return false; // unsortable ones to the end as well (before invalid ones)
            if(IsUnsortable(pObj2)) return true;
        }
        if(pObj1->GetFileIndex() != pObj2->GetFileIndex()) return pObj1->GetFileIndex() < pObj2->GetFileIndex();
        else            return pObj1->GetId() < pObj2->GetId();
    }
private:
    bool IsUnsortable(const IMECPreObject *pObj)
    {
        return unsortableKeywords.count(pObj->GetInputFullType()) > 0;
    }
    POKeywordSet unsortableKeywords;
};

class POCompareToId
{
public:
    bool operator()(const IMECPreObject *pObj1, unsigned int id)
    {
        if(!pObj1) return false; // invalid ones are at the end, cf POCompareById
        unsigned int id1 = pObj1->GetId();
        if(0 == id1) return false; // id 0 at the end
        else         return id1 < id;
    }
};

static int GetIndexFromPreobject(const IMECPreObject *pObj, const std::string &skwd,
                                 IMECPreObject::MyAttributeType_s *patype, IMECPreObject::MyValueType_s *pvtype)
{
    if(!pObj) return -1;
    for(int atype = IMECPreObject::ATY_UNKNOWN + 1;
        atype < IMECPreObject::ATY_LAST; ++atype)
    {
        for(int vtype = IMECPreObject::VTY_UNKNOWN + 1;
            vtype < IMECPreObject::VTY_LAST; ++vtype)
        {
            int index = pObj->GetIndex((IMECPreObject::MyAttributeType_s) atype,
                                       (IMECPreObject::MyValueType_s) vtype, skwd);
            if(0 <= index)
            {
                if(patype) *patype = (IMECPreObject::MyAttributeType_s) atype;
                if(pvtype) *pvtype = (IMECPreObject::MyValueType_s) vtype;
                return index;
            }
        }
    }
    return -1;
}


// *********************************************************************************
// Classes for a PreObject based SDI implementation
// *********************************************************************************

// forward declaration
template <sdi::SpecializationType SPECIALTYPE> class SDISelectionDataPO;
template <sdi::SpecializationType SPECIALTYPE> class SDISelectionDataPOSubobject;
template <sdi::SpecializationType SPECIALTYPE> class SDISelectionDataPOArray;

// Template for PreObject-based EntityData
template<sdi::SpecializationType SPECIALTYPE>
class TSDIEntityDataPO : public TSDIEntityDataCFG<SPECIALTYPE>
{
public:
    TSDIEntityDataPO(SDIModelViewPrivate* pModel, IMECPreObject* psource, unsigned int index = UINT_MAX) :
        TSDIEntityDataCFG<SPECIALTYPE>(pModel), p_ptr(psource), p_index(index)
    {
        Init();
    }

    virtual typename SDITypeGuide<SPECIALTYPE>::data * Clone() const
    {
        return new TSDIEntityDataPO<SPECIALTYPE>(*this);
    }

    virtual void Destroy()
    {
        delete this;
    }

    // Implemetations of BASE
    virtual bool IsValid() const { return p_ptr != 0; }
    virtual EntityType GetType() const { return p_entityType; }
    virtual const sdiString& GetKeyword() const { return p_keyword; }
    IMECPreObject* GetDataPointer() { return p_ptr; }
    const IMECPreObject* GetDataPointer() const { return p_ptr; }
    virtual HandleEdit GetHandle() const { return HandleEdit(p_entityType, p_ptr); }
    virtual unsigned int GetId() const
    {
        return p_ptr ? p_ptr->GetId(): 0;
    }
    virtual Status         SetId(const unsigned int id) const
    {
        if (p_ptr) { p_ptr->SetId((int) id); return true; }
        else return false;
    }
    virtual sdiString GetName() const
    {
        if(!p_ptr) return sdiString();
        sdiString name;

        const char *title = p_ptr->GetTitle();
        if (nullptr != title) name = title;

        if(name.empty())
        {
            // entity has no name, but some entities use the attribute "TITLE"
            sdiValue value;
            GetValue(sdiIdentifier("TITLE"), value);
            value.GetValue(name);
        }
        if(name.empty())
        {
            
            sdiValue value;
            GetValue(sdiIdentifier("displayname"), value);
            value.GetValue(name);
        }
        return name;
    }
    virtual Status SetName(const sdiString& name) const
    {
        if (p_ptr) { p_ptr->SetTitle(name.c_str()); return true; }
        else return false;
    }
    virtual HandleEdit GetInclude() const;
    virtual Status SetInclude(const HandleRead& hinc) const;
    inline virtual Status GetValue(const sdiIdentifier& identifier,
                                 sdiValue&            value) const;
    /* using base class, which does a GetValue plus FindById:
    virtual Status GetEntityHandle(const sdiIdentifier& identifier,
                                   HandleEdit&                     handle) const;
    */
    inline virtual Status SetValue(const sdiIdentifier& identifier,
                                 const sdiValue&      value) const;

    inline virtual Status GetAttributes(sdiVector<sdiIdentifier>& aIdentifier) const;

    inline virtual bool IsParameterized(const sdiIdentifier& identifier) const;
    inline virtual sdiString GetParameterName(const sdiIdentifier& identifier,
                                             bool*                           pIsNegated) const;
    inline virtual Status SetParameter(const sdiIdentifier& identifier,
                                     const sdiString&                 parameterName,
                                     bool                            isNegated) const;
    virtual const IDescriptor *GetDescriptor() const;

protected:
    friend class SDISelectionDataPO<SPECIALTYPE>;

    void SetDataPointer(IMECPreObject* ptr)
    {
        if(!ptr || !(p_ptr && !strcmp(ptr->GetKernelFullType(), p_ptr->GetKernelFullType()))) p_pDescr = 0;
        p_vpSubDescr.resize(0); /* we could check the subobjects one by one, but they might be different,
                                 * so this is a simple and safe approach */
        p_ptr = ptr;
        Init();
    }

    virtual void Init()
    {
        const SDIModelViewPrivate *pModelView = this->GetModelView();
        if(nullptr != p_ptr && nullptr != pModelView)
        {
            p_keyword = p_ptr->GetInputFullType();
            p_entityType = pModelView->GetEntityType(p_keyword);
        }
        else
        {
            p_keyword = "";
            p_entityType = ENTITY_TYPE_NONE;
        }
    }

    IMECPreObject *p_ptr;
    mutable unsigned int p_index = 0; // used for TSDIEntityDataPOArray
    sdiString p_keyword;
    EntityType p_entityType;
    mutable const IDescriptor         *p_pDescr = 0;
    mutable vector<const IDescriptor*> p_vpSubDescr;
};

typedef TSDIEntityDataPO<SPECIALIZATION_TYPE_GENERAL> SDIEntityDataPO;

class SDIEntityDataPOIncludeFile: public SDIEntityDataPO
{
public:
    SDIEntityDataPOIncludeFile(SDIModelViewPrivate* pModel, IMECPreObject* psource, EntityType type) :
        SDIEntityDataPO(pModel, psource)
    {
        p_entityType = type;
        if(nullptr != pModel) p_keyword = pModel->GetKeyword(type);
    }

    virtual unsigned int GetId() const
    {
        return GetModelView()->P_GetId(GetHandle());
    }

    virtual sdiString GetName() const
    {
        return GetModelView()->P_GetName(GetHandle());
    }

    virtual Status GetValue(const sdiIdentifier& identifier,
                            sdiValue&            value) const
    {
        if(identifier.GetNameKey().compare(0, 15, "shadow_submodel") == 0)
        {
            // submodels are stored as includes, but their type is different
            if(nullptr == p_ptr || nullptr == this->GetModelView()) return false;
            EntityType type = this->GetModelView()->GetEntityType(p_ptr->GetInputFullType());
            if(type != p_entityType)
            {
                if(identifier.GetNameKey() == "shadow_submodelid")
                {
                    value = sdiValue(p_ptr->GetId());
                }
                else
                {
                    assert(identifier.GetNameKey() == "shadow_submodel");
                    value = sdiValue(sdiValueEntity(sdiValueEntityType(
                        this->GetModelView()->GetEntityType(p_ptr->GetInputFullType())), p_ptr->GetId()));
                }
                return true;
            }
            else
            {
                value = sdiValue(0);
                return false;
            }
        }
        else
        {
            return SDIEntityDataPO::GetValue(identifier, value);
        }
    }

    virtual Status GetEntityHandle(const sdiIdentifier& identifier, HandleEdit& handle) const
    {
        if(identifier.GetNameKey() == "shadow_submodel")
        {
            // submodels are stored as includes, but their type is different
            if(nullptr == p_ptr || nullptr == this->GetModelView()) return false;
            EntityType type = this->GetModelView()->GetEntityType(p_ptr->GetInputFullType());
            if(type != p_entityType)
            {
                handle = HandleEdit(type, p_ptr);
                return true;
            }
            else
            {
                handle = HandleEdit();
                return false;
            }
        }
        else
        {
            return SDIEntityDataPO::GetEntityHandle(identifier, handle);
        }
    }

    // this is called indirectly when looping through a selection and must not change keyword and type
    virtual void Init() {}
};

// Template for PreObject-based EntityData, where multiple entities are stored in arrays of a PreObject
template<sdi::SpecializationType SPECIALTYPE>
class TSDIEntityDataPOArray : public TSDIEntityDataPO<SPECIALTYPE>
{
public:
    TSDIEntityDataPOArray(SDIModelViewPrivate* pModel, IMECPreObject* psource,
                          unsigned int index1 = 0, unsigned int index2 = 0) :
        TSDIEntityDataPO<SPECIALTYPE>(pModel, psource, index1), p_index2(index2)
    {}

    virtual HandleEdit GetHandle() const
    {
        return HandleEdit(this->p_entityType, this->p_index, p_index2);
    }

    friend class SDISelectionDataPOArray<SPECIALTYPE>;

protected:
    unsigned int p_index2 = 0;
};

class SDINodeDataPO : public TSDIEntityDataPOArray<SPECIALIZATION_TYPE_NODE>
{
public:
    SDINodeDataPO(SDIModelViewPrivate* pModel, IMECPreObject* psource,
                  unsigned int index1 = 0, unsigned int index2 = 0) :
        TSDIEntityDataPOArray(pModel, psource, index1, index2)
    {}

    virtual unsigned int GetId() const
    {
        int att_index_id = p_ptr->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "id");
        return p_ptr->GetIntValue(att_index_id, p_index2);
    }

    virtual sdiTriple GetPosition() const
    {
        int att_index_x = p_ptr->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, "globalx");
        double x = p_ptr->GetFloatValue(att_index_x, p_index2);
        int att_index_y = p_ptr->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, "globaly");
        double y = p_ptr->GetFloatValue(att_index_y, p_index2);
        int att_index_z = p_ptr->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, "globalz");
        double z = p_ptr->GetFloatValue(att_index_z, p_index2);
        return sdiTriple(x, y, z);
    }
    
    virtual Status SetPosition(const sdiTriple& position)const 
    {
        int att_index_x = p_ptr->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, "globalx");
        int att_index_y = p_ptr->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, "globaly");
        int att_index_z = p_ptr->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, "globalz");
        p_ptr->SetFloatValue(att_index_x, p_index2, position[0]);
        p_ptr->SetFloatValue(att_index_y, p_index2, position[1]);
        p_ptr->SetFloatValue(att_index_z, p_index2, position[2]);
        return true;
    }
    /*virtual Status findById(const sdiIdentifier& identifier, sdiValue& value) const
    {
        int index = GetIndexFromPreobject(p_ptr, identifier, 0, 0);
        if(index < 0) return false;
        value = p_ptr->GetValue(index, p_index2);
        return true;
    }*/
    
};

class SDIElementDataPO : public TSDIEntityDataPOArray<SPECIALIZATION_TYPE_ELEMENT>
{
public:
    SDIElementDataPO(SDIModelViewPrivate* pModel, IMECPreObject* psource,
                     unsigned int index1 = 0, unsigned int index2 = 0) :
        TSDIEntityDataPOArray(pModel, psource, index1, index2)
    {}

    
    virtual unsigned int GetId() const
    {
        int att_index_id = p_ptr->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "id");
        return p_ptr->GetIntValue(att_index_id, p_index2);
    }


    virtual HandleRead GetOwner() const
    {
        unsigned int id = GetOwnerId();
        HandleRead handle;
        this->GetModelView()->FindById(HCDI_OBJ_TYPE_COMPS, id, handle);
        return handle;
    }

    virtual unsigned int GetOwnerId() const
    {
        int att_index_id = p_ptr->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "collector");
        if(0 <= att_index_id) return p_ptr->GetIntValue(att_index_id, p_index2);

        att_index_id = p_ptr->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_OBJECT, "PART");
        if(0 <= att_index_id) return p_ptr->GetObjectId(att_index_id);

        // we shouldn't get here
        assert(0);
        return 0;
    }


    virtual unsigned int GetNodeCount() const
    {
        unsigned int node_count = 0;
        unsigned int node_max = 20;

        if(!p_ptr) return 0 ;
        std::vector<int> att_indices(node_max);
        for (unsigned int i = 0; i < node_max; ++i) {
            att_indices[i] = p_ptr->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "node" + std::to_string(i + 1));
        }
        

        for(unsigned int i = 0; i < node_max; ++i)
        {
            if (p_ptr->GetIntValue(att_indices[i], p_index2) > 0) node_count++;
        }

        if (node_count == 0)
        {
            for (unsigned int i = 0; i < node_max; ++i) {
                att_indices[i] = p_ptr->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "node_ID" + std::to_string(i + 1));
            }
            
            for(unsigned int i = 0; i < node_max; ++i)
            {
                if (p_ptr->GetIntValue(att_indices[i], p_index2) > 0) node_count++;
            }
        }

        return node_count;
    }


    virtual void GetNodeIds(sdiVector<unsigned int> &aNodeId) const
    {
        if(!p_ptr) return ;

        unsigned int node_count = 0;
        unsigned int node_max = 20;
        std::vector<int> att_indices(node_max);

        for (unsigned int i = 0; i < node_max; ++i) {
            att_indices[i] = p_ptr->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "node" + std::to_string(i + 1));
        }


        aNodeId.resize(node_max);
        
        for(unsigned int i = 0; i < aNodeId.size(); ++i)
        {
            aNodeId[i] = p_ptr->GetIntValue(att_indices[i], p_index2);
            if (p_ptr->GetIntValue(att_indices[i], p_index2) > 0) node_count++;
        }

        if (node_count == 0)
        {
            for (unsigned int i = 0; i < node_max; ++i) {
                att_indices[i] = p_ptr->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "node_ID" + std::to_string(i + 1));
            }
            
            for(unsigned int i = 0; i < node_max; ++i)
            {
                aNodeId[i] = p_ptr->GetIntValue(att_indices[i], p_index2);
                if (p_ptr->GetIntValue(att_indices[i], p_index2) > 0) node_count++;
            }
            aNodeId.resize(node_count);
        }

        return ;
    }
    
    
    virtual Status GetEntityHandle(const sdiIdentifier& identifier, HandleRead& handle) const
    {
        if(!p_ptr) return false;
        int att_index = p_ptr->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_OBJECT, identifier.GetNameKey());
        if(att_index < 0) return false;
        handle = HandleRead(p_ptr->GetIntValue(att_index, p_index2), p_ptr->GetInputFullType());
        return true;
    }
    
    virtual Status SetNodes(const sdiVector<HandleNodeRead> &ahNode) const
    {
        if(!p_ptr) return false;
        int att_index = p_ptr->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "node_ID");
        if(att_index < 0) return false;

        int idIndex = p_ptr->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "id");
        unsigned int NumberOfEntities = (unsigned int) p_ptr->GetNbValues(IMECPreObject::VTY_INT, idIndex);

        if(ahNode.size() != NumberOfEntities) return false;
        for(unsigned int i = 0; i < ahNode.size(); ++i)
        {
            NodeEdit *pNode = (NodeEdit *) ahNode[i].GetPointer();
            p_ptr->SetIntValue(att_index, i, pNode->GetId());
        }
        return true;
    }
};

template <sdi::SpecializationType SPECIALTYPE>
class SDISelectionDataPO: public SDISelectionData
{

public:
    inline SDISelectionDataPO(const sdiString& keyword,
                              const SDIModelViewPrivate* pModelView,
                              std::vector<IMECPreObject *> *pre_obj_lst,
                              const Filter* pFilter = 0,
                              bool testKeyword = true,
                              unsigned int p_indexFirst = 0);

    virtual ~SDISelectionDataPO()
    {
        if(nullptr != p_pCurrentEntityData) delete p_pCurrentEntityData;
        if(nullptr != p_pFilter) delete p_pFilter;

        // SelectionObserver hack:
        const SDIModelViewSelectionObservable *pModelViewSO = 
            static_cast<const SDIModelViewSelectionObservable*>(GetModelViewRead());
        if(pModelViewSO->GetSelectionObserver())
            pModelViewSO->GetSelectionObserver()->SelectionDestructed(*this);
    }

    //! Get pointer of current entity; will return 0 if if have advanced using
    //! Next() past last entity satisfying construction criteria.
    virtual IMECPreObject* GetCurrentEntityPtr() const
    {
        return p_pre_obj_lst && p_index < p_pre_obj_lst->size() ? (*p_pre_obj_lst)[p_index] : 0;
    }

    //! Retrieve entity data pointer used maintain reference of current value
    virtual SDIEntityData* GetReferenceToCurrentValue() const
    {
        assert(p_pCurrentEntityData);
        return p_pCurrentEntityData;
    }

    //! Advances to next entity, returning false if no such selectable entity
    inline virtual bool Next();


    //! Returns to just prior to first selectable entity
    virtual void Restart()
    {
        p_index = UINT_MAX;
        p_pCurrentEntityData->p_ptr = 0;
        p_pCurrentEntityData->p_index = UINT_MAX;
    }

    //! Can create (clone) another selection data using the same construction criteria, with ability to
    //! specify whether underlying implementation should do a deep or shallow copy.
    virtual SDISelectionData* Clone(const bool deepCopy) const
    {
        if(deepCopy) return 0; // TBD
        return new SDISelectionDataPO(
            p_keyword, (const SDIModelViewPrivate*) GetModelViewRead(),
            p_pre_obj_lst, p_pFilter);
    }

    //! Count of number of possible entities accessible by selection data.
    virtual unsigned int Count() const;

    //! Returns true if entity's pointer is accessible to selection data.
    //! This is the pointer encapsulated by a handle.
    virtual bool IsSelected(const void* ptr) const { return ptr == GetCurrentEntityPtr(); }

protected:
    void SetDataPointer(IMECPreObject* ptr) const
    {
        p_pCurrentEntityData->SetDataPointer(ptr);
    }

protected:
    std::vector <IMECPreObject *> *p_pre_obj_lst;
    unsigned int p_index;
    unsigned int p_indexFirst;
    mutable TSDIEntityDataPO<SPECIALTYPE>* p_pCurrentEntityData;
    sdiString p_keyword;
    const Filter* p_pFilter = nullptr;
    bool p_testKeyword = true;
};


template <sdi::SpecializationType SPECIALTYPE>
class SDISelectionDataPOArray: public SDISelectionDataPO<SPECIALTYPE>
{

public:
    SDISelectionDataPOArray(const sdiString& keyword,
                                const SDIModelViewPrivate* pModelView,
                                std::vector<IMECPreObject *> *pre_obj_lst,
                                const Filter* pFilter = 0) :
        SDISelectionDataPO<SPECIALTYPE>(keyword, pModelView, pre_obj_lst, pFilter)
    {}

    ~SDISelectionDataPOArray()
    {}

    //! Can create (clone) another selection data using the same construction criteria, with ability to
    //! specify whether underlying implementation should do a deep or shallow copy.
    virtual SDISelectionDataPOArray* Clone(const bool deepCopy) const
    {
        if(deepCopy) return 0; // TBD
        return new SDISelectionDataPOArray(
            this->p_keyword, (const SDIModelViewPrivate*) this->GetModelViewRead(),
            this->p_pre_obj_lst, this->p_pFilter);
    }

    //! Count of number of possible entities accessible by selection data.
    virtual unsigned int Count() const
    {
        unsigned int count = 0;
        if(!this->p_pre_obj_lst) return 0;
        for(unsigned int i = 0; i < this->p_pre_obj_lst->size(); ++i)
        {

            sdiString kernelFullType = this->p_pre_obj_lst->at(i)->GetKernelFullType();

            bool isSame = true;
            if (kernelFullType.find("/ELEMS") == 0) {
                kernelFullType.erase(0, 6); // Remove "/ELEMS"
                if (!(strcmp(this->p_keyword.c_str(), kernelFullType.c_str()) == 0)) {
                    isSame = false;
                }
            }

            if(this->p_pre_obj_lst->at(i) != nullptr && isSame)
            {
                int idIndex = this->p_pre_obj_lst->at(i)->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "id");
                count += (unsigned int) this->p_pre_obj_lst->at(i)->GetNbValues(IMECPreObject::VTY_INT, idIndex);
            }
        }
        return count;
    }

    //! Advances to next entity, returning false if no such selectable entity
    virtual bool Next()
    {
        // first call
        if(UINT_MAX == this->p_index) return SDISelectionDataPO<SPECIALTYPE>::Next();
        // subsequent calls
        TSDIEntityDataPOArray<SPECIALTYPE>* pCurrentEntityData = static_cast<TSDIEntityDataPOArray<SPECIALTYPE>*>(this->p_pCurrentEntityData);
        
        unsigned int NumberOfEntities = 0;

        int idIndex = pCurrentEntityData->p_ptr->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "id");
        NumberOfEntities = (unsigned int) pCurrentEntityData->p_ptr->GetNbValues(IMECPreObject::VTY_INT, idIndex);

        if(nullptr != pCurrentEntityData->p_ptr &&
           pCurrentEntityData->p_index2 + 1 < NumberOfEntities)
        { // there is another one in current preobject
            ++(pCurrentEntityData->p_index2);
            return true;
        }
        else
        { // switch to next preobject
            bool found = false;
            found = SDISelectionDataPO<SPECIALTYPE>::Next();
            pCurrentEntityData->p_index2 = 0;
            return found;
        }
    }

protected:
    sdiString p_inputtypeNode;
};




template <sdi::SpecializationType SPECIALTYPE>
class SDISelectionDataPOSubobject: public SDISelectionDataPO<SPECIALTYPE>
{

public:
    SDISelectionDataPOSubobject(const sdiString& keyword,
                                const SDIModelViewPrivate* pModelView,
                                std::vector<IMECPreObject *> *pre_obj_lst,
                                const sdiString& inputtypeSubobject,
                                const Filter* pFilter = 0) :
        SDISelectionDataPO<SPECIALTYPE>(keyword, pModelView, pre_obj_lst, pFilter),
        p_inputtypeSubobject(inputtypeSubobject)
    {}

    ~SDISelectionDataPOSubobject()
    {}

    IMECPreObject* GetEntityPtr(unsigned int index, bool doFilter = false) const;

    //! Get pointer of current entity; will return 0 if have advanced using
    //! Next() past last entity satisfying construction criteria.
    //! Also returns 0 if current preobject has no subobject, this is used by Next().
    virtual IMECPreObject* GetCurrentEntityPtr() const
    {
        return GetEntityPtr(this->p_index);
    }

    //! Advances to next entity, returning false if no such selectable entity
    inline virtual bool Next();


    //! Can create (clone) another selection data using the same construction criteria, with ability to
    //! specify whether underlying implementation should do a deep or shallow copy.
    virtual SDISelectionData* Clone(const bool deepCopy) const
    {
        if(deepCopy) return 0; // TBD
        return new SDISelectionDataPOSubobject(
            this->p_keyword, (const SDIModelViewPrivate*) this->GetModelViewRead(),
            this->p_pre_obj_lst, p_inputtypeSubobject, this->p_pFilter);
    }

    //! Count of number of possible entities accessible by selection data.
    virtual unsigned int Count() const
    {
        unsigned int count = 0;
        if(!this->p_pre_obj_lst) return 0;
        for(unsigned int i = 0; i < this->p_pre_obj_lst->size(); ++i)
        {
            if(nullptr != GetEntityPtr(i, true)) ++count;
        }
        return count;
    }

protected:
    sdiString p_inputtypeSubobject;
};


// This is a "hack" for LS-Dyna *DEFINE_CURVE, cf comments for p_typesMainAndSubobjects
class SDISelectionDataPOMainAndSubobjects: public SDISelectionDataPO<SPECIALIZATION_TYPE_GENERAL>
{

public:
    SDISelectionDataPOMainAndSubobjects(const sdiString& keyword,
                                        const SDIModelViewPrivate* pModelView,
                                        std::vector<IMECPreObject *> *pre_obj_lst,
                                        const Filter* pFilter = 0):
        SDISelectionDataPO<SPECIALIZATION_TYPE_GENERAL>(keyword, pModelView, pre_obj_lst, pFilter)
    {}

    ~SDISelectionDataPOMainAndSubobjects()
    {}

    inline IMECPreObject* GetEntityPtr(unsigned int index, unsigned int indexSubobject) const;

    //! Get pointer of current entity; will return 0 if have advanced using
    //! Next() past last entity satisfying construction criteria.
    virtual IMECPreObject* GetCurrentEntityPtr() const
    {
        return GetEntityPtr(this->p_index, p_indexSubobject);
    }

    // Advances to the next subobject of the current preobject, if there is one
    bool NextSubobject()
    {
        if(p_index >= p_pre_obj_lst->size()) return false;
        const IMECPreObject *pMainObj = (*p_pre_obj_lst)[p_index];
        if(nullptr == pMainObj) return false; // shouldn't happen
        const vector<IMECPreObject *>& subobjects = pMainObj->GetSubobject();
        do
        {
            p_indexSubobject = UINT_MAX == p_indexSubobject ? 0 : p_indexSubobject + 1;
        }
        while (p_indexSubobject < subobjects.size() &&
               nullptr != subobjects[p_indexSubobject] &&
               0 != strncmp(subobjects[p_indexSubobject]->GetInputFullType(),
                            p_keyword.c_str(), p_keyword.size()));

        if(p_indexSubobject < subobjects.size()) return true;
        // else
        p_indexSubobject = UINT_MAX;
        return false;
    }

    //! Advances to next entity, returning false if no such selectable entity
    inline virtual bool Next();

    //! Returns to just prior to first selectable entity
    virtual void Restart()
    {
        SDISelectionDataPO<SPECIALIZATION_TYPE_GENERAL>::Restart();
        p_indexSubobject = UINT_MAX;
    }

    //! Can create (clone) another selection data using the same construction criteria, with ability to
    //! specify whether underlying implementation should do a deep or shallow copy.
    virtual SDISelectionData* Clone(const bool deepCopy) const
    {
        if(deepCopy) return 0; // TBD
        return new SDISelectionDataPOMainAndSubobjects(
            this->p_keyword, (const SDIModelViewPrivate*) this->GetModelViewRead(),
            this->p_pre_obj_lst, this->p_pFilter);
    }

    //! Count of number of possible entities accessible by selection data.
    virtual unsigned int Count() const
    {
        return SDISelectionData::Count(); // default implementation, which uses Next()
    }

protected:
    unsigned int p_indexSubobject = UINT_MAX;
};


class EntityReadPO
{
public:
    EntityReadPO(const IMECPreObject* pObj, const ModelViewPO* mv, const IDescriptor** ppDescr = nullptr) :
        p_pObj(pObj), p_mv(mv), p_ppDescr(ppDescr)
    {}

    inline bool GetValue(const sdiIdentifier& identifier,
                         sdiValue&            value) const;

private:
    const IMECPreObject* p_pObj;
    const ModelViewPO* p_mv;
    const IDescriptor** p_ppDescr;
};


// *********************************************************************************
// ModelViewPO
// *********************************************************************************

// class ModelViewPO : public SDIModelViewPrivate
// SelectionObserver hack:

/// ModelView for a DB storing its entities in vectors of PreObjects.
class ModelViewPO : public ModelViewCFG
{
public:

    //! Constructor for empty model
    ModelViewPO(const SDITypeMapper& typemapper, const CFGKernel *pCFGKernel=nullptr) :
        ModelViewCFG(typemapper, pCFGKernel)
    {
        p_nbPOTypes = HCDI_OBJ_TYPE_HC_MAX;
        p_preobjects = new std::vector<IMECPreObject*>[p_nbPOTypes];
        owningPreobjects = true;
        for(size_t i=0; i < p_nbPOTypes; ++i) p_isSorted[i] = SortNone;
    }

    //! Constructor from existing objects
    ModelViewPO(const SDITypeMapper& typemapper, 
                std::vector<IMECPreObject*> *preobjects,
                unsigned int nbPOTypes,
                const CFGKernel *pCFGKernel=nullptr,
                const sdiVector<sdiString>& keywordsMainAndSubobjects = sdiVector<sdiString>()) :
        ModelViewCFG(typemapper, pCFGKernel),
        p_preobjects(preobjects),
        p_nbPOTypes(nbPOTypes),
        owningPreobjects(false)
    {
        assert(p_preobjects && 0 < p_nbPOTypes);
        for(size_t i=0; i < HCDI_OBJ_TYPE_HC_MAX; ++i) p_isSorted[i] = SortNone;
        for(auto keywordMainAndSubobjects : keywordsMainAndSubobjects)
        {
            p_typesMainAndSubobjects.insert(GetEntityType(keywordMainAndSubobjects));
        }
    }

    virtual ~ModelViewPO()
    {
        if(owningPreobjects)
        {
            for(unsigned int i = 0; i <= (unsigned int) p_nbPOTypes; ++i)
                for(unsigned int j=0; j < p_preobjects[i].size(); ++j)
                    delete p_preobjects[i][j];
            delete[] p_preobjects;
        }
    }

    enum SortState {
        SortNone,
        SortById,
        SortByFileAndId
    };

    void SetSelectionSortMethod (SortState selectionSortMethod) { p_selectionSortMethod = selectionSortMethod; }

    void SetUnsorted()
    {
        for(size_t i=0; i < HCDI_OBJ_TYPE_HC_MAX; ++i) p_isSorted[i] = SortNone;
    }

    void ApplyIdOffsets(const char* submodelsolkey, bool doUnOffset = false)
    {
        UpdateEntityIDOffsetCFG(p_preobjects, submodelsolkey, doUnOffset);

        // Clear cache, in case there is one
        //ClearElemsByCompsCache();

        // Sorting might get lost
        SetUnsorted();

        if(!doUnOffset)
        {
            // Init IdManager, in case anybody wants to add entities to the model
            sdi::SDIIdManager* pIdManager = GetIdManager();
            if(nullptr != pIdManager) pIdManager->Init();
        }
    }

    //
    // Implementations of selected methods
    //
    //! Create selection using filter
    virtual SDISelectionData* CreateSelectionData(const sdiString& keyword,
                                                  const Filter*   pselectionFilter = nullptr) const
    {
        EntityType type = GetEntityType(keyword);
        // assert(ENTITY_TYPE_NONE != type); // called with invalid keyword,
        // if(ENTITY_TYPE_NONE == type) return nullptr; // but we create a dummy data
        SpecializationType specialType = GetSpecializationType(type);
        sdiString inputtypeSubobject;

        // sort
        unsigned int CFGType = GetCFGType(type);
        if(HCDI_OBJ_TYPE_INCLUDEFILES != CFGType && // includes must keep their order
           SortByFileAndId != p_isSorted[CFGType] && SortByFileAndId == p_selectionSortMethod)
        {
            if(!p_preobjects[CFGType].empty())
            {
                POCompareByFileAndId comp(this, CFGType);
                stable_sort(p_preobjects[CFGType].begin(),
                            p_preobjects[CFGType].end(),
                            comp);
            }
            p_isSorted[CFGType] = SortByFileAndId;
        }
        else if(HCDI_OBJ_TYPE_INCLUDEFILES != CFGType && // includes must keep their order
                SortById != p_isSorted[CFGType] && SortById == p_selectionSortMethod)
        {
            if(!p_preobjects[CFGType].empty())
            {
                stable_sort(p_preobjects[CFGType].begin(),
                            p_preobjects[CFGType].end(),
                            POCompareById);
            }
            p_isSorted[CFGType] = SortById;
        }

        // create selection data
        if(p_typesMainAndSubobjects.count(type) > 0) // special case, cf comments for p_typesMainAndSubobjects
        { // call base implementation, which loops through a selection (to be specifically coded here if too slow)
            return new SDISelectionDataPOMainAndSubobjects(
                keyword, this, &(p_preobjects[CFGType]), pselectionFilter);
        }

        switch(specialType)
        {
        case SPECIALIZATION_TYPE_GENERAL:
            return TCreateSelectionData<SPECIALIZATION_TYPE_GENERAL>(
                keyword, pselectionFilter);
        case SPECIALIZATION_TYPE_NODE:
            return new SDISelectionDataPOArray<SPECIALIZATION_TYPE_NODE>(
                keyword, this, &(p_preobjects[CFGType]), pselectionFilter);
        case SPECIALIZATION_TYPE_ELEMENT:
            return new SDISelectionDataPOArray<SPECIALIZATION_TYPE_ELEMENT>(
                keyword, this, &(p_preobjects[CFGType]), pselectionFilter);
        default: return 0;
        }
    }

    virtual SDISelectionData* GetRelationSelection(
        const RelationSource&       relationSource,
        const EntityRelation        entityRelationTarget,
        const EntityTypeIdentifier& entityTypeIdentifierTarget,
        const Filter&               relationFilterTarget = nullFilter) const
    {
        if(entityTypeIdentifierTarget == EntityTypeIdentifier("elements"))
        {
            // dummy
            return CreateSelectionData("*ELEMENT_SHELL");
        }
        return 0;
    }


    virtual SDINodeData* Objectify(const HandleNodeRead& handle) const
    {
        if(handle.GetIndex1() < p_preobjects[HCDI_OBJ_TYPE_NODES].size())
        {
            return new SDINodeDataPO(
                const_cast<ModelViewPO*>(this),
                p_preobjects[HCDI_OBJ_TYPE_NODES][handle.GetIndex1()],
                handle.GetIndex1(),
                handle.GetIndex2());
        }
        else
        {
            return new SDINodeDataPO(const_cast<ModelViewPO*>(this), nullptr);
        }
    }

    virtual SDIElementData* Objectify(const HandleElementRead& handle) const
    {
        if(handle.GetIndex1() < p_preobjects[HCDI_OBJ_TYPE_ELEMS].size())
        {
            return new SDIElementDataPO(
                const_cast<ModelViewPO*>(this),
                p_preobjects[HCDI_OBJ_TYPE_ELEMS][handle.GetIndex1()],
                handle.GetIndex1(),
                handle.GetIndex2());
        }
        else
        {
            return new SDIElementDataPO(const_cast<ModelViewPO*>(this), nullptr);
        }
    }

    virtual SDIEntityData* Objectify(const HandleRead& handle) const
    {
        EntityType type = handle.GetType();
        // assert(ENTITY_TYPE_NONE != type); // called with invalid keyword,
        // if(ENTITY_TYPE_NONE == type) return nullptr; // but we create a dummy data
        SpecializationType specializationType = GetSpecializationType(type);
        switch(specializationType)
        {
        case SPECIALIZATION_TYPE_NODE:
            return Objectify(HandleNodeRead(type, handle.GetIndex1(), handle.GetIndex2()));
        case SPECIALIZATION_TYPE_ELEMENT:
            return Objectify(HandleElementRead(type, handle.GetIndex1(), handle.GetIndex2()));
        default:
            if(myTypeInclude == type)
            {
                return new SDIEntityDataPOIncludeFile(const_cast<ModelViewPO*>(this),
                                                      (IMECPreObject*)handle.GetPointer(), type);
            }
            else
            {
                return new SDIEntityDataPO(const_cast<ModelViewPO*>(this),
                                           (IMECPreObject*)handle.GetPointer());
            }
        }
    }

    /// Creation of a new entity
    virtual Status CreateEntity(HandleEdit&               handle,
                                const sdiString&          keyword,
                                const sdiString&          name = "",
                                const unsigned int        id = 0)
    {
        EntityType type = GetEntityType(keyword);
        if(ENTITY_TYPE_NONE == type) return false;
        SpecializationType specializationType = GetSpecializationType(type);
        unsigned int validId = id;
        if(0 == id) validId = GetNextAvailableId(type);
        if(SPECIALIZATION_TYPE_ELEMENT == specializationType)
        {
            // elements cannot be created without an owner
            assert(0);
            return false;
        }
        else if(SPECIALIZATION_TYPE_NODE == specializationType)
        {
            HandleNodeEdit hnode;
            bool isOk = CreateNode(hnode, keyword, sdiTriple(), validId);
            if(isOk) handle = HandleEdit(type, hnode.GetPointer());
            return isOk;
        }

        // SPECIALIZATION_TYPE_GENERAL:
        if(p_typemapper.GetEntityTypeParent(type) != ENTITY_TYPE_NONE)
        {
            // caller requests to create an "adhesive entity" / "subobject"
            HandleEdit hParent;
            bool isOk = FindById(p_typemapper.GetEntityTypeParent(type), id, hParent);
            if(isOk)
            {
                const sdiString &dataNameSubobject = p_typemapper.GetDataNameSubobject(type);
                handle = CreateSubobject(hParent, dataNameSubobject, keyword, name, id);
            }
            return isOk;
        }

        IMECPreObject *pObj = HCDI_GetPreObjectHandle("", keyword.c_str(), name.c_str(), (int) validId, 0);
        if(nullptr == pObj) return false;
        pObj->SetFileIndex(p_currentIncludeId);

        unsigned int CFGType = GetCFGType(type);

        if(nullptr == p_preobjects ||
           CFGType > p_nbPOTypes ||
           CFGType == HCDI_OBJ_TYPE_NULL)
        {
            return false; // shouldn't happen
        }

        p_preobjects[CFGType].push_back(pObj);
        p_isSorted[CFGType] = SortNone;

        handle = HandleEdit(type, pObj);

        if(nullptr != p_pIdManager) p_pIdManager->AddId(validId, type);
        return true;
    }

    // Specific method to create a subobject
    virtual HandleEdit CreateSubobject(HandleEdit hParent, const sdiString &dataNameSubobject,
        const sdiString &keyword, const sdiString &name = sdiString(), unsigned int id = 0)
    {
        // Get parent preobject
        IMECPreObject* pParent = (IMECPreObject*) hParent.GetPointer();
        if(nullptr == pParent) return (HandleEdit());
        if(0 == id) id = pParent->GetId();

        // Get type of subobject from CFG-descriptor of parent
        // NB: Other locations in this file where we need the subobjects are changed to use the
        
        // the code here as it is for the time being, because it is only used for "adhesive options"
        const IDescriptor *pDescr = GetDescriptor(pParent);
        if(nullptr == pDescr) return (HandleEdit());
        int ikwd = pDescr->getIKeyword(dataNameSubobject);
        int domains = HCDI_get_all_domains();
        const MvDataFeature_t *pFeature = pDescr->getIkeywordDataFeature(domains, ikwd);
        if(nullptr == pFeature) return (HandleEdit());
        MvDataFeatureType_e feature_type = pFeature->getType();
        MvFullType_t ftype;
        if (DFT_SUBOBJECT == feature_type || DFT_DATA == feature_type)
        {
            MvDataDataFeature_t* pSubobjFeature = (MvDataDataFeature_t*) pFeature;
            const MvFullTypeSet_t &ftypeset = pSubobjFeature->getAllowedObjectFullTypes();
            if(ftypeset.size() < 1) return (HandleEdit());
            ftype = *ftypeset.begin();
        }
        if(ftype.getType() == HCDI_OBJ_TYPE_NULL) return (HandleEdit());

        // Create subobject and add to parent
        IMECPreObject *pSubobj = HCDI_GetPreObjectHandle(string(ftype).c_str(), keyword.c_str(), name.c_str(), id, 0);
        if(nullptr == pSubobj) return (HandleEdit());
        if(nullptr != p_preobjects && (unsigned int)ftype.getType() < p_nbPOTypes) // should always be true
        {
            p_preobjects[ftype.getType()].push_back(pSubobj);
        }
        pSubobj->SetFileIndex(p_currentIncludeId);
        pParent->SetSubobject(pSubobj);
        SetValueToPreObject(pParent,
            sdiIdentifier(dataNameSubobject.c_str()),
            sdiValue(sdiValueEntity(sdiValueEntityType(ftype.getType()), id)));

        return HandleEdit((EntityType) ftype.getType(), pSubobj);
    }

    virtual Status CreateNode   (HandleNodeEdit&          handle,
                                 const sdiString&         keyword,
                                 const sdiTriple&         position,
                                 const unsigned int       id = 0)
    {
        IMECPreObject *pObj = nullptr;
        EntityType type = GetEntityType(keyword);

        unsigned int idNode = id;
        if (idNode == 0)  idNode = GetNextAvailableId(type);


        int idIndex, xIndex, yIndex, zIndex, arraySize = 0;
        if(p_preobjects[HCDI_OBJ_TYPE_NODES].size() > 0)
        {
            pObj = p_preobjects[HCDI_OBJ_TYPE_NODES][0];
            idIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "id");
            xIndex  = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, "globalx");
            yIndex  = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, "globaly");
            zIndex  = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, "globalz");
            arraySize = pObj->GetNbValues(IMECPreObject::VTY_INT, idIndex);
            pObj->resizeArray(IMECPreObject::VTY_INT,  idIndex, arraySize + 1);
            pObj->resizeArray(IMECPreObject::VTY_FLOAT,xIndex,  arraySize + 1);
            pObj->resizeArray(IMECPreObject::VTY_FLOAT,yIndex,  arraySize + 1);
            pObj->resizeArray(IMECPreObject::VTY_FLOAT,zIndex,  arraySize + 1);
        }
        else
        {
            pObj = HCDI_GetPreObjectHandle("", keyword.c_str(), "", (int) p_currentIncludeId, 0);
            p_preobjects[HCDI_OBJ_TYPE_NODES].push_back(pObj);
            idIndex = pObj->AddIntArray("id", 1);
            xIndex  = pObj->AddFloatArray("globalx", 1);
            yIndex  = pObj->AddFloatArray("globaly", 1);
            zIndex  = pObj->AddFloatArray("globalz", 1);
        }

        pObj->SetIntValue(idIndex, arraySize , idNode);
        pObj->SetFloatValue(xIndex, arraySize , position[0]);
        pObj->SetFloatValue(yIndex, arraySize , position[1]);
        pObj->SetFloatValue(zIndex, arraySize , position[2]);

        handle = HandleNodeEdit(GetEntityType(keyword), 0, arraySize);

        if(nullptr != p_pIdManager) p_pIdManager->AddId(idNode, type);

        return true;
    }



    virtual Status CreateElement(HandleElementEdit&       handle,
                                 const HandleElementRead& source,
                                 const HandleRead&        owner,
                                 const unsigned int       id = 0)
    {
        assert(0); // to be implemented
        return false;
    }

    virtual Status CreateElement(HandleElementEdit&       handle,
                                 const sdiString&          keyword,
                                 const sdiVector<HandleNodeRead>& ahNode,
                                 const HandleRead&        owner,
                                 const unsigned int       id = 0)
    {
        sdiUIntList aNodeId;
        aNodeId.reserve(ahNode.size());
        for(const HandleNodeRead& hNode : ahNode) aNodeId.push_back(P_GetId(hNode));
        return CreateElement(handle, keyword, aNodeId, owner, id);
        return false;
    }
    
    virtual Status CreateElement(HandleElementEdit&       handle,
                                 const sdiString&          keyword,
                                 const sdiUIntList& aNodeId,
                                 const HandleRead&        owner,
                                 const unsigned int       id = 0)
    {
        EntityType type = GetEntityType(keyword);
        unsigned int validId = id;

        IMECPreObject *pObj = nullptr;
        bool isNewPo = true;
        
        sdiString kernelFullType = "/ELEMS" + keyword;

        if (id == 0) return false;

        if(p_preobjects[HCDI_OBJ_TYPE_ELEMS].size() > 0)
        {
            pObj = p_preobjects[HCDI_OBJ_TYPE_ELEMS][p_preobjects[HCDI_OBJ_TYPE_ELEMS].size()-1];
            int att_index_componentId = pObj->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_OBJECT, "PART");
            if(pObj->GetObjectId(att_index_componentId) == owner.GetId(this)) isNewPo = false;
            if(strcmp(pObj->GetKernelFullType(), kernelFullType.c_str()) != 0) isNewPo = true;
        }

        if(isNewPo)
        {           
            pObj = HCDI_GetPreObjectHandle(kernelFullType.c_str(), keyword.c_str(), "", (int) p_currentIncludeId, 0);
            p_preobjects[HCDI_OBJ_TYPE_ELEMS].push_back(pObj);
            pObj->AddIntArray("id", 1);
            for (int i = 0; i < aNodeId.size(); ++i) 
            {
                sdiString nodeId = "node_ID" + std::to_string(i + 1);
                pObj->AddIntArray(nodeId.c_str(), 1);
            }
            pObj->AddObjectValue("PART", HCDI_get_entitystringtype(HCDI_OBJ_TYPE_COMPS).c_str(), owner.GetId(this));
        }
        
        int idIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "id");
        int arraySize = pObj->GetNbValues(IMECPreObject::VTY_INT, idIndex);
        if(!isNewPo) pObj->resizeArray(IMECPreObject::VTY_INT,idIndex, arraySize + 1);
        if(isNewPo) pObj->SetIntValue(idIndex, 0 , id);
        if(!isNewPo) pObj->SetIntValue(idIndex, arraySize , id);
        
        
        for (int i = 0; i < aNodeId.size(); ++i) {
            idIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "node_ID" + std::to_string(i + 1));
            arraySize = pObj->GetNbValues(IMECPreObject::VTY_INT, idIndex);
            if(!isNewPo) pObj->resizeArray(IMECPreObject::VTY_INT,idIndex, arraySize + 1);
            if(isNewPo) pObj->SetIntValue(idIndex, 0 , aNodeId[i]);
            if(!isNewPo) pObj->SetIntValue(idIndex, arraySize , aNodeId[i]);
        }

        handle = HandleElementEdit(type, (unsigned int) p_preobjects[HCDI_OBJ_TYPE_ELEMS].size()-1, arraySize);
        
        return handle.IsValid();
    }

    std::vector<IMECPreObject*>::iterator  P_FindById(const unsigned int id, unsigned int CFGType) const
    {
        if(SortByFileAndId == p_isSorted[CFGType] && SortByFileAndId == p_selectionSortMethod)
        {
            // if we are in "SortByFileAndId", we have to do a linear search
            for(std::vector<IMECPreObject*>::iterator itr = p_preobjects[CFGType].begin(); 
                itr != p_preobjects[CFGType].end();
                ++itr)
            {
                if((*itr) && (*itr)->GetId() == (int)id)
                {
                    return itr;
                }
            }
            return p_preobjects[CFGType].end();
        }

        if(SortById != p_isSorted[CFGType])
        {
            if(!p_preobjects[CFGType].empty())
            {
                stable_sort(p_preobjects[CFGType].begin(),
                    p_preobjects[CFGType].end(),
                    POCompareById);
            }
            p_isSorted[CFGType] = SortById;
        }
        POCompareToId finder;
        std::vector<IMECPreObject*>::iterator itr =
            lower_bound(p_preobjects[CFGType].begin(),
                p_preobjects[CFGType].end(),
                id, finder);
        if(p_preobjects[CFGType].end() != itr && (*itr) &&
            (*itr)->GetId() == (int)id)
        {
            return itr;
        }
        return p_preobjects[CFGType].end();
    }

    virtual void Delete(const HandleEdit& handle)
    {
        EntityType type = handle.GetType();
        unsigned int CFGType = GetCFGType(type);
        if(CFGType > p_nbPOTypes) return;
        unsigned int id = handle.GetId(this);
        // P_FindById gives the first one with this id, if there is ...
        std::vector<IMECPreObject*>::iterator itr = P_FindById(id, CFGType);
        // ... but there may be multiple entities with the same id right after,
        // and, with SortByFileAndId, even further up if in other includes
        void *ptr = handle.GetPointer();
        while(p_preobjects[CFGType].end() != itr && (*itr))
        {
            if((*itr) == ptr)
            {
                p_preobjects[CFGType].erase(itr);
                return;
            }
            ++itr;
        }
    }

    virtual unsigned int GetNextAvailableId(EntityType type) const
    {
        if(ENTITY_TYPE_NONE == type) return 0;
        if(myTypeInclude == type || HCDI_OBJ_TYPE_INCLUDEFILES == type)
        {
            return (unsigned int) p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES].size();
        }
        return ModelViewCFG::GetNextAvailableId(type);
    }

    virtual bool P_FindById(const unsigned int id, EntityType type, HandleRead& handle) const
    {
        if(ENTITY_TYPE_NONE == type) return false;
        unsigned int CFGType = GetCFGType(type);

        if(myTypeInclude == type || HCDI_OBJ_TYPE_INCLUDEFILES == type)
        {
            if(p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES].size() > id)
            {
                handle.SetPointer(p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES][id]);
                return true;
            }
            else return false;
        }
        else if(HCDI_OBJ_TYPE_INCLUDEFILES == CFGType)
        { // submodels are stored as includes, but their type is different, do a linear search
            for(std::vector<IMECPreObject*>::iterator itr = p_preobjects[CFGType].begin(); 
                itr != p_preobjects[CFGType].end();
                ++itr)
            {
                if((*itr) && 
                   (unsigned int)((*itr)->GetId()) == id &&
                   GetEntityType((*itr)->GetInputFullType()) != HCDI_OBJ_TYPE_INCLUDEFILES)
                {
                    handle.SetPointer(*itr);
                    return true;
                }
            }
            return false;
        }

        SpecializationType specializationType = GetSpecializationType(type);

        if(SPECIALIZATION_TYPE_NODE == specializationType)
        {
            // create map if not up to date
            if(p_isDirtyMapNodes)
            {
                p_MapNodes.clear();
                for (unsigned int i = 0; i < p_preobjects[HCDI_OBJ_TYPE_NODES].size(); ++i)
                {
                    IMECPreObject* pObj = p_preobjects[HCDI_OBJ_TYPE_NODES][i];
                    if(nullptr == pObj) continue; // shouldn't happen
                    int idIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "id");
                    if(0 > idIndex) continue; // shouldn't happen
                    unsigned int count = (unsigned int) pObj->GetNbValues(IMECPreObject::VTY_INT, idIndex);
                    for (unsigned int j = 0; j < count; ++j)
                    {
                        int nodeId = pObj->GetIntValue(idIndex, j);
                        p_MapNodes[nodeId] = std::make_pair(i, j);
                        
                    }
                }
                p_isDirtyMapNodes = false;
            }
            // search in the map
            auto it = p_MapNodes.find(id);
            if(it != p_MapNodes.end())
            {
                handle.SetIndex1(it->second.first);
                handle.SetIndex2(it->second.second);
                return true;
            }
            else return false;
        }

        if(SPECIALIZATION_TYPE_ELEMENT == specializationType)
        {
            // create map if not up to date
            if(p_isDirtyMapElems.count(type) == 0 || p_isDirtyMapElems[type])
            {
                p_MapELems[type].clear();
                for (unsigned int i = 0; i < p_preobjects[HCDI_OBJ_TYPE_ELEMS].size(); ++i)
                {
                    IMECPreObject* pObj = p_preobjects[HCDI_OBJ_TYPE_ELEMS][i];
                    if(nullptr == pObj) continue; // shouldn't happen
                    const char *keyword = pObj->GetInputFullType();
                    if(GetEntityType(keyword) != type) continue; // these are elements of another type, so skip
                    int idIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "id");
                    if(0 > idIndex) continue; // shouldn't happen
                    unsigned int count = (unsigned int) pObj->GetNbValues(IMECPreObject::VTY_INT, idIndex);
                    for (unsigned int j = 0; j < count; ++j)
                    {
                        int elemId = pObj->GetIntValue(idIndex, j);
                        p_MapELems[type][elemId] = std::make_pair(i, j);
                    }
                }
                p_isDirtyMapElems[type] = false;
            }
            // search in the map
            auto it = p_MapELems[type].find(id);
            if(it != p_MapELems[type].end())
            {
                handle.SetIndex1(it->second.first);
                handle.SetIndex2(it->second.second);
                return true;
            }
            else return false;
        }

        // SPECIALIZATION_TYPE_GENERAL:
        if(p_typesMainAndSubobjects.count(type) > 0) // special case, cf comments for p_typesMainAndSubobjects
        { // call base implementation, which loops through a selection (to be specifically coded here if too slow)
            return SDIModelViewPrivate::P_FindById(id, type, handle);
        }

        const sdiString& keyword = GetKeyword(type);
        if(CFGType > p_nbPOTypes) return false;
        std::vector<IMECPreObject*>::iterator itr = P_FindById(id, CFGType);
        // there may be multiple entities with the same id
        while(p_preobjects[CFGType].end() != itr && (*itr) &&
              (*itr)->GetId() == (int)id)
        {
            const char *ift = (*itr)->GetInputFullType();
            // if(ift && !keyword.compare(0, keyword.size(), ift, keyword.size())) should also work
            if(ift && !strncmp(keyword.c_str(), ift, keyword.size()))
            {
                handle.SetPointer(*itr);
                return true;
            }
            ++itr;
        }
        return false;
    }

public:

    ////////////////////////////////////////////////////////////////////////////////
    //! Functions used by handle
    ////////////////////////////////////////////////////////////////////////////////

    virtual unsigned int P_GetId(const HandleRead& handle) const
    {
        SpecializationType SpecializationType = GetSpecializationType(handle.GetType());
        switch(SpecializationType)
        {
        case SPECIALIZATION_TYPE_NODE: 
        {
            IMECPreObject* pObj = p_preobjects[HCDI_OBJ_TYPE_NODES][handle.GetIndex1()];
            int idIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "id");
            return pObj->GetIntValue(idIndex, handle.GetIndex2());
        }
        case SPECIALIZATION_TYPE_ELEMENT: 
        {
            IMECPreObject* pObj = p_preobjects[HCDI_OBJ_TYPE_ELEMS][handle.GetIndex1()];
            int idIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "id");
            return pObj->GetIntValue(idIndex, handle.GetIndex2());
        }
        default:
        {
            if(!handle.GetPointer()) return 0;
            switch(handle.GetType())
            {
            case ENTITY_TYPE_NONE: return 0;
            default:
                if(handle.GetType() == myTypeInclude)
                {
                    auto it = std::find(p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES].begin(),
                                        p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES].end(),
                                        handle.GetPointer());
                    if(it != p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES].end())
                    {
                        return (unsigned int) (it - p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES].begin());
                    }
                    else return 0;
                }
                else return static_cast<const IMECPreObject*>(handle.GetPointer())->GetId();
            }
        }
        }
    }

    virtual Status         P_SetId(const EntityType type, void* entityptr, const unsigned int id) const
    {
        if(!entityptr) return false;
        switch(type)
        {
        case ENTITY_TYPE_NONE: return false;
        default:               static_cast<IMECPreObject*>(entityptr)->SetId(id);
        }
        return false;
    }

    virtual sdiString P_GetName(const EntityType type, const void* entityptr) const
    {
        if(!entityptr) return sdiString();
        switch(type)
        {
        case ENTITY_TYPE_NONE:
            return sdiString();
        default:
        {
            const IMECPreObject* pObj = static_cast<const IMECPreObject*>(entityptr);
            if(type == myTypeInclude)
            {
                sdiString filename;
                sdiValue value;
                GetValueFromPreObject(pObj, sdiIdentifier("filename"), value);
                value.GetValue(filename);
                if(!filename.empty()) return filename;
            }
            sdiString name = pObj->GetTitle();
            if(name.empty())
            {
                // entity has no name, but some entities use the attribute "TITLE"
                sdiValue value;
                GetValueFromPreObject(pObj, sdiIdentifier("TITLE"), value);
                value.GetValue(name);
            }
            if(name.empty())
            {
                
                sdiValue value;
                GetValueFromPreObject(pObj, sdiIdentifier("displayname"), value);
                value.GetValue(name);
            }
            return name;
        }
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    //! Descriptior Values and Hierachy by identifier
    ////////////////////////////////////////////////////////////////////////////////
    virtual Status GetValue(const EntityType                type,
                          const void*                     entityptr,
                          const sdiIdentifier& identifier,
                          sdiValue&            value) const
    {
        switch(type)
        {
        case ENTITY_TYPE_NONE:
            return false;
        default:
            if(myTypeInclude == type)
            { // has a specific GetValue()
                EntityRead entity(this, HandleRead(type, entityptr));
                return entity.GetValue(identifier, value);
            }
            unsigned int CFGType = GetCFGType(type);
            if(CFGType > p_nbPOTypes) return false;
            EntityReadPO entityReadPO(static_cast<const IMECPreObject*>(entityptr), this);
            return GetValueFromPreObject(static_cast<const IMECPreObject*>(entityptr),
                identifier, value, &entityReadPO);
        }
    }
    virtual Status SetValue(const HandleEdit&    handle,
                            const sdiIdentifier& identifier,
                            const sdiValue&      value) const
    {
        SpecializationType SpecializationType = GetSpecializationType(handle.GetType());
        switch(SpecializationType)
        {
        case SPECIALIZATION_TYPE_NODE: 
        {
            IMECPreObject* pObj = p_preobjects[HCDI_OBJ_TYPE_NODES][handle.GetIndex1()];
            //pObj->SetIntValue(0, handle.GetIndex2(), value.GetInt());
            // creer un nouvel objet de type sdiIdentifier et d ajouter index2 dedans
            //SetValueToPreObject sur une seule value dans un tableau ( il me faut le index2)
            return 0;
        }
        case SPECIALIZATION_TYPE_ELEMENT: 
        {
            IMECPreObject* pObj = p_preobjects[HCDI_OBJ_TYPE_ELEMS][handle.GetIndex1()];
            return 0;
        }
        default:
        {
            unsigned int CFGType = GetCFGType(handle.GetType());
            if(CFGType> p_nbPOTypes) return false;
            EntityReadPO entityReadPO(static_cast<const IMECPreObject*>(handle.GetPointer()), this);
            return SetValueToPreObject(static_cast<IMECPreObject*>(handle.GetPointer()), identifier, value, &entityReadPO);
        }
        }
    }

    virtual bool IsParameterized(const EntityType                type,
                                 const void*                     entityptr,
                                 const sdiIdentifier& identifier) const
    {
        if(ENTITY_TYPE_NONE == type) return false;
        unsigned int CFGType = GetCFGType(type);
        if(CFGType > p_nbPOTypes) return false;
        EntityReadPO entityReadPO(static_cast<const IMECPreObject*>(entityptr), this);
        return IsPreObjectParameterized(static_cast<const IMECPreObject*>(entityptr),
            identifier, &entityReadPO);
    }

    ////////////////////////////////////////////////////////////////////////////////
    //! Utility functions
    ////////////////////////////////////////////////////////////////////////////////
    // Return whether any entity of a given type is contained in this model.
    // May confirm that this type is handled through PreObjects, rather than being
    // handled differently by a derived class
    bool IsContained(const EntityType type) const
    {
        unsigned int CFGType = GetCFGType(type);
        if( CFGType > p_nbPOTypes) return false;
        if(p_preobjects[CFGType].size() > 0) return true;
        return false;
    }

    bool GetValueFromPreObject(const IMECPreObject            *pObj,
                               const sdiIdentifier& identifier,
                               sdiValue&            value,
                               const IDescriptor             **ppDescr = 0,
                               vector<const IDescriptor*>     *pvpSubDescr = 0) const
    {
        return GetValueFromPreObject<EntityReadPO>(pObj, identifier, value,
            nullptr, ppDescr, pvpSubDescr);
    }

    template <class ENTITYREAD>
    bool GetValueFromPreObject(const IMECPreObject            *pObj,
        const sdiIdentifier& identifier,
        sdiValue&            value,
        const ENTITYREAD*               pEntityRead,
        const IDescriptor             **ppDescr = 0,
        vector<const IDescriptor*>     *pvpSubDescr = 0,
        bool*                           pIsParameterized = 0,
        sdiString*                       pParameterName = 0,
        bool*                           pParameterIsNegated = 0) const;

    bool SetValueToPreObject(IMECPreObject                  *pObj,
        const sdiIdentifier& identifier,
        const sdiValue&      value,
        const IDescriptor             **ppDescr = 0,
        vector<const IDescriptor*>     *pvpSubDescr = 0) const
    {
        return SetValueToPreObject<EntityReadPO>(pObj, identifier, value,
            nullptr, ppDescr, pvpSubDescr);
    }

    template <class ENTITYREAD>
    bool SetValueToPreObject(IMECPreObject                  *pObj,
        const sdiIdentifier& identifier,
        const sdiValue&      value,
        const ENTITYREAD*               pEntityRead,
        const IDescriptor             **ppDescr = 0,
        vector<const IDescriptor*>     *pvpSubDescr = 0,
        const char*                     parameterName = 0,
        bool                            parameterIsNegated = false) const;

    template <class ENTITYREAD>
    bool IsPreObjectParameterized(const IMECPreObject            *pObj,
        const sdiIdentifier& identifier,
        const ENTITYREAD*               pEntityRead,
        const IDescriptor             **ppDescr = 0,
        vector<const IDescriptor*>     *pvpSubDescr = 0) const;

    template <class ENTITYREAD>
    sdiString GetPreObjectParameterName(const IMECPreObject            *pObj,
        const sdiIdentifier& identifier,
        bool*                           pIsNegated,
        const ENTITYREAD*               pEntityRead,
        const IDescriptor             **ppDescr = 0,
        vector<const IDescriptor*>     *pvpSubDescr = 0) const;

    template <class ENTITYREAD>
    bool SetPreObjectParameter(const IMECPreObject            *pObj,
        const sdiIdentifier& identifier,
        const sdiString&                 parameterName,
        bool                            isNegated,
        const ENTITYREAD*               pEntityRead,
        const IDescriptor             **ppDescr = 0,
        vector<const IDescriptor*>     *pvpSubDescr = 0) const;

    virtual const IDescriptor* GetDescriptor(const IMECPreObject *pObj) const
    {
        if(nullptr == pObj) return nullptr;
        const char *kft = pObj->GetKernelFullType();
        const IDescriptor *pDescr = nullptr;
        if(strlen(kft) > 0)
        {
            pDescr = GetDescriptorFromKernelType(kft);
        }
        if(nullptr == pDescr)
        {
            const char *ift = pObj->GetInputFullType();
            if(strlen(ift) > 0)
            {
                string kftype = GetKernelFullType(ift, &pDescr);
                if(nullptr != pDescr) const_cast<IMECPreObject*>(pObj)->SetKernelFullType(kftype.c_str());
            }
        }
        return pDescr;
    }

    EntityType GetEntitytype(const string &keyword, unsigned int id, const IMECPreObject *pObj,
                             const string &skwd, const IDescriptor **ppDescr = nullptr,
                             int ikwd = END_ARGS) const
    {
        // 1st trial: check for 1to1 mapping
        object_type_e CFGType = HCDI_get_entitytype(keyword);
        EntityType type = ENTITY_TYPE_NONE;
        if(HCDI_OBJ_TYPE_MULTIOBJECT != CFGType)
        {
            bool is1to1 = false;
            type = p_typemapper.GetEntityType(CFGType, is1to1);
            if(is1to1) return type;
        }

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

        // third trial: check whether there is a subtype in the attribute
        const IDescriptor *pDescr = ppDescr ? *ppDescr : 0;
        if(nullptr == pDescr) pDescr = GetDescriptor(pObj);
        if(nullptr == pDescr) return type; // shouldn't happen
        if(ppDescr) *ppDescr = pDescr;
        bool isOk = false;
        if(END_ARGS == ikwd) ikwd = pDescr->getIKeyword(skwd);
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

        // ideally we shouldn't come here
        return type;
    }

    // Utility
    void PrintPreobject(const IMECPreObject *ptr)
    {
        
        if(ptr && (
#ifdef _DEBUG
            999 == p_doPrintDebug ||
#endif
            (p_doPrintDebug && !ptr->GetCryptingReference())))
        {
            const IDescriptor *pDescr = GetDescriptor(ptr);
            assert(GetCFGKernel() != nullptr);
            if(GetCFGKernel() == nullptr) return;
            MultiCFGKernelMgr::CFGKernelSentinel sentinel(GetCFGKernel()->getSubUserProfile());
            char *report = ptr->GetReport(pDescr);
            printf("===== Dump of PreObject: ========================================================\n");
            printf("%s\n", report);
            if (report != nullptr) free(report);
        }
    }

    // To be reimplemented if there are keywords that must not be sorted (SortByFileAndId only)
    virtual POKeywordSet GetUnsortableKeywords(EntityType type) const
    {
        return POKeywordSet();
    }

protected:

    template<sdi::SpecializationType SPECIALTYPE>
    SDISelectionData* TCreateSelectionData(const sdiString& keyword,
                                           const Filter* pselectionFilter) const
    {
        EntityType type = GetEntityType(keyword);
        unsigned int CFGType = GetCFGType(type);
        if(p_typemapper.GetEntityTypeParent(type) == ENTITY_TYPE_NONE)
        {
            bool testKeyword = true;
            unsigned int indexFirst = 0;
            if(myTypeInclude == type)
            {
                testKeyword = false; // selection of includes must not skip submodels
                indexFirst = 1;      // and skip the first PreObject (the root file)
            }
            return new SDISelectionDataPO<SPECIALTYPE>(
                keyword, this, &(p_preobjects[CFGType]), pselectionFilter, testKeyword, indexFirst);
        }
        else
        {
            const sdiString keywordParent = GetKeyword(p_typemapper.GetEntityTypeParent(type));
            return new SDISelectionDataPOSubobject<SPECIALTYPE>(
                keywordParent, this, &(p_preobjects[CFGType]), keyword, pselectionFilter);
        }
    }

    std::vector<IMECPreObject*> *p_preobjects;
    unsigned int p_nbPOTypes;
    bool owningPreobjects;
    mutable SortState p_isSorted[HCDI_OBJ_TYPE_HC_MAX];
    SortState p_selectionSortMethod = SortById;
    mutable std::map<unsigned int, std::pair<unsigned int, unsigned int>> p_MapNodes; 
    mutable std::map<EntityType, std::map<unsigned int, std::pair<unsigned int, unsigned int>>> p_MapELems; 
    mutable bool p_isDirtyMapNodes = true;
    mutable std::map<EntityType, bool> p_isDirtyMapElems;

    // This is a "hack" for LS-Dyna *DEFINE_CURVE, which can either be present as
    // - separate "main" preobjects or
    // - subobjects of *DEFINE_TABLE "parent" preobjects.
    // This logic could be used for similar cases, if the "main" and "parent" preobjects have the same
    // CFGType.
    std::set<EntityType> p_typesMainAndSubobjects;


public:
    int p_doPrintDebug = getenv("HW_PRINT_NEXT_PREOBJECT") ? atoi(getenv("HW_PRINT_NEXT_PREOBJECT")) : 0;
}; // ModelViewPO class


template<sdi::SpecializationType SPECIALTYPE>
HandleEdit TSDIEntityDataPO<SPECIALTYPE>::GetInclude() const
{
    HandleEdit parent;
    const ModelViewPO* mv = static_cast<const ModelViewPO*>(this->GetModelView());
    if(mv && p_ptr) mv->FindById(mv->myTypeInclude, p_ptr->GetFileIndex(), parent);
    return parent;
}

template<sdi::SpecializationType SPECIALTYPE>
Status TSDIEntityDataPO<SPECIALTYPE>::SetInclude(const HandleRead& hinc) const
{
    if(!p_ptr) return false;
    const ModelViewPO* mv = static_cast<const ModelViewPO*>(this->GetModelView());
    unsigned int includeId = 0;
    if(hinc.GetType() == mv->myTypeInclude) includeId = hinc.GetId(mv);
    else if(hinc.GetType() != ENTITY_TYPE_NONE) return false; // wrong type
    p_ptr->SetFileIndex((int) includeId);
    return true;
}

template<sdi::SpecializationType SPECIALTYPE>
Status TSDIEntityDataPO<SPECIALTYPE>::GetValue(const sdiIdentifier& identifier,
                                         sdiValue&            value) const
{
    const ModelViewPO* mv = static_cast<const ModelViewPO*>(this->GetModelView());
    assert(mv);
    return mv->GetValueFromPreObject(p_ptr, identifier, value, this, &p_pDescr, &p_vpSubDescr);
}

template<sdi::SpecializationType SPECIALTYPE>
Status TSDIEntityDataPO<SPECIALTYPE>::SetValue(const sdiIdentifier& identifier,
                                         const sdiValue&      value) const
{
    const ModelViewPO* mv = static_cast<const ModelViewPO*>(this->GetModelView());
    assert(mv);
    return mv->SetValueToPreObject(p_ptr, identifier, value, this, &p_pDescr, &p_vpSubDescr);
}

template<sdi::SpecializationType SPECIALTYPE>
Status TSDIEntityDataPO<SPECIALTYPE>::GetAttributes(sdiVector<sdiIdentifier>& aIdentifier) const
{
    if(nullptr == p_ptr) return false;

    static IMECPreObject::MyAttributeType_e atypes[] =
        {IMECPreObject::ATY_SINGLE, IMECPreObject::ATY_ARRAY};
    static IMECPreObject::MyValueType_e vtypes[] =
        {IMECPreObject::VTY_BOOL, IMECPreObject::VTY_INT, IMECPreObject::VTY_UINT,
         IMECPreObject::VTY_FLOAT, IMECPreObject::VTY_STRING, IMECPreObject::VTY_OBJECT};

    for(IMECPreObject::MyAttributeType_e atype : atypes)
    {
        for(IMECPreObject::MyValueType_e vtype : vtypes)
        {
            int nbAttributes = p_ptr->GetNbAttributes(atype, vtype);
            for(int i = 0; i < nbAttributes; ++i)
            {
                const char *kwd = p_ptr->GetKeyword(atype, vtype, i);
                aIdentifier.push_back(sdiIdentifier(kwd));
            }
        }
    }

    return true;
}

template<sdi::SpecializationType SPECIALTYPE>
bool TSDIEntityDataPO<SPECIALTYPE>::IsParameterized(const sdiIdentifier& identifier) const
{
    const ModelViewPO* mv = static_cast<const ModelViewPO*>(this->GetModelView());
    assert(mv);
    return mv->IsPreObjectParameterized(p_ptr, identifier, this, &p_pDescr, &p_vpSubDescr);
}

template<sdi::SpecializationType SPECIALTYPE>
sdiString TSDIEntityDataPO<SPECIALTYPE>::GetParameterName(const sdiIdentifier& identifier,
                                                     bool*                           pIsNegated) const
{
    const ModelViewPO* mv = static_cast<const ModelViewPO*>(this->GetModelView());
    assert(mv);
    return mv->GetPreObjectParameterName(p_ptr, identifier, pIsNegated, this, &p_pDescr, &p_vpSubDescr);
}

template<sdi::SpecializationType SPECIALTYPE>
Status TSDIEntityDataPO<SPECIALTYPE>::SetParameter(const sdiIdentifier& identifier,
                                             const sdiString&                 parameterName,
                                             bool                            isNegated) const
{
    const ModelViewPO* mv = static_cast<const ModelViewPO*>(this->GetModelView());
    assert(mv);
    return mv->SetPreObjectParameter(p_ptr, identifier, parameterName, isNegated, this, &p_pDescr, &p_vpSubDescr);
}


template<sdi::SpecializationType SPECIALTYPE>
const IDescriptor *TSDIEntityDataPO<SPECIALTYPE>::GetDescriptor() const
{
    if(!p_pDescr)
    {
        const ModelViewPO* mv = static_cast<const ModelViewPO*>(this->GetModelView());
        assert(mv);
        p_pDescr=mv->GetDescriptor(p_ptr);
    }
    return p_pDescr;
}


template <sdi::SpecializationType SPECIALTYPE>
SDISelectionDataPO<SPECIALTYPE>::SDISelectionDataPO(const sdiString& keyword,
                                                    const SDIModelViewPrivate* pModelView,
                                                    std::vector<IMECPreObject *> *pre_obj_lst,
                                                    const Filter* pFilter,
                                                    bool testKeyword, unsigned int indexFirst) :
    SDISelectionData(SPECIALTYPE, pModelView),
    p_index(UINT_MAX),
    p_pCurrentEntityData(0),
    p_pre_obj_lst(pre_obj_lst),
    p_keyword(keyword),
    p_testKeyword(testKeyword),
    p_indexFirst(indexFirst)
{
    assert(pModelView);
    if(pModelView)
    {
        EntityType type = pModelView->GetEntityType(keyword);
        p_pCurrentEntityData = (TSDIEntityDataPO<SPECIALTYPE>*)
            pModelView->Objectify(
                typename SDITypeGuide<SPECIALTYPE>::handleread(
                    type, nullptr));

        // SelectionObserver hack:
        const SDIModelViewSelectionObservable *pModelViewSO = 
            static_cast<const SDIModelViewSelectionObservable*>(pModelView);
        if(pModelViewSO->GetSelectionObserver())
            pModelViewSO->GetSelectionObserver()->SelectionConstructed(*this);
    }
    if(nullptr != pFilter) p_pFilter = pFilter->Clone();
}

template <sdi::SpecializationType SPECIALTYPE>
bool SDISelectionDataPO<SPECIALTYPE>::Next()
{
    if(!p_pre_obj_lst) return false; // shouldn't happen
    bool isOk = true;
    do
    {
        p_index = UINT_MAX == p_index ? p_indexFirst : p_index + 1;
        isOk = true;
        SetDataPointer(GetCurrentEntityPtr());

        if(p_pre_obj_lst->size() <= p_index) break;

        // skip entities that have a different keyword
        isOk = true;
        if(p_testKeyword)
        {
            size_t size = p_keyword.size();
            const sdiString& entKeyword = p_pCurrentEntityData->GetKeyword();
            if(p_keyword.compare(0, size, entKeyword, 0, size)) isOk = false;
        }

        // skip entities that do not pass the filter
        if(isOk && nullptr != p_pFilter)
        {
            if(!p_pFilter->EntityPassesFilter(p_pCurrentEntityData)) isOk = false;
        }

    } while(!isOk);

    p_pCurrentEntityData->p_index = p_index; // used for TSDIEntityDataPOArray

#ifdef PRINT_NEXT_PREOBJECT
    ((ModelViewPO*)GetModelViewRead())->PrintPreobject(p_pCurrentEntityData->p_ptr);
#endif

    // SelectionObserver hack:
    const SDIModelViewSelectionObservable *pModelViewSO = 
        static_cast<const SDIModelViewSelectionObservable*>(GetModelViewRead());
    if(pModelViewSO->GetSelectionObserver())
        pModelViewSO->GetSelectionObserver()->SelectionNext(*this);

    return p_index < p_pre_obj_lst->size() ? true : false;
}

//! Count of number of possible entities accessible by selection data.
template <sdi::SpecializationType SPECIALTYPE>
unsigned int SDISelectionDataPO<SPECIALTYPE>::Count() const
{
    if(!p_pre_obj_lst) return 0;

    unsigned int count = 0;
    for(size_t i = p_indexFirst; i < p_pre_obj_lst->size(); ++i)
    {
        SetDataPointer((*p_pre_obj_lst)[i]);

        // skip entities that have a different keyword
        bool isOk = true;
        if(p_testKeyword)
        {
            size_t size = p_keyword.size();
            const sdiString& entKeyword = p_pCurrentEntityData->GetKeyword();
            if(p_keyword.compare(0, size, entKeyword, 0, size)) isOk = false;
        }

        // skip entities that do not pass the filter
        if(isOk && nullptr != p_pFilter)
        {
            if(!p_pFilter->EntityPassesFilter(p_pCurrentEntityData)) isOk = false;
        }

        if(isOk) ++count;
    }

    // reset p_pCurrentEntityData
    SetDataPointer(GetCurrentEntityPtr());

    return count;
}


template <sdi::SpecializationType SPECIALTYPE>
IMECPreObject* SDISelectionDataPOSubobject<SPECIALTYPE>::GetEntityPtr(
    unsigned int index, bool doFilter) const
{
    std::vector <IMECPreObject *> *pre_obj_lst = this->p_pre_obj_lst;
    if(!pre_obj_lst || pre_obj_lst->size() <= index || !(*pre_obj_lst)[index]) return nullptr;

    // invalid if wrong keyword
    const char *entKeyword = (*pre_obj_lst)[index]->GetInputFullType();
    size_t size = this->p_keyword.size();
    if(strlen(entKeyword) < size || this->p_keyword.compare(0, size, entKeyword, size))
    {
        return nullptr;
    }

    const vector<IMECPreObject *>& subobjects = (*pre_obj_lst)[index]->GetSubobject();
    for(vector<IMECPreObject *>::const_iterator it = subobjects.begin(); it != subobjects.end(); ++it)
    {
        IMECPreObject *pSubobject = *it;
        if(!pSubobject) continue;
        size_t size = p_inputtypeSubobject.size();
        const char* inputtypeCurrentSubobject = pSubobject->GetInputFullType();
        if( strlen(inputtypeCurrentSubobject) >= size &&
           !p_inputtypeSubobject.compare(0, size, inputtypeCurrentSubobject, size))
        {
            if(doFilter && this->p_pFilter)
            {
                // We use p_pCurrentEntityData for the filter, so we change it temporarily.
                TSDIEntityDataPO<SPECIALTYPE>* pCurrentEntityData = this->p_pCurrentEntityData;
                IMECPreObject* dataPointer = pCurrentEntityData->GetDataPointer();
                if(dataPointer != pSubobject) this->SetDataPointer(pSubobject);
                bool isOk = this->p_pFilter->EntityPassesFilter(this->p_pCurrentEntityData);
                // reset p_pCurrentEntityData
                if(dataPointer != pSubobject) this->SetDataPointer(dataPointer);
                if(!isOk) return nullptr;
            }
            return pSubobject;
        }
    }
    return nullptr;
}

template <sdi::SpecializationType SPECIALTYPE>
bool SDISelectionDataPOSubobject<SPECIALTYPE>::Next()
{
    if(!this->p_pre_obj_lst) return false; // shouldn't happen
    this->p_index = UINT_MAX == this->p_index ? 0 : this->p_index + 1;
    IMECPreObject *pPreObj = GetEntityPtr(this->p_index, true);
    while(this->p_index < this->p_pre_obj_lst->size() && nullptr == pPreObj)
    {
        ++this->p_index;
        pPreObj = GetEntityPtr(this->p_index, true);
    }
    this->SetDataPointer(pPreObj);

#ifdef PRINT_NEXT_PREOBJECT
    ((ModelViewPO*)this->GetModelViewRead())->PrintPreobject(pPreObj);
#endif

    // SelectionObserver hack:
    const SDIModelViewSelectionObservable *pModelViewSO = 
        static_cast<const SDIModelViewSelectionObservable*>(this->GetModelViewRead());
    if(pModelViewSO->GetSelectionObserver())
        pModelViewSO->GetSelectionObserver()->SelectionNext(*this);

    return this->p_index < this->p_pre_obj_lst->size() ? true : false;
}


IMECPreObject* SDISelectionDataPOMainAndSubobjects::GetEntityPtr(
    unsigned int index, unsigned int indexSubobject) const
{
    if(!p_pre_obj_lst) return nullptr; // shouldn't happen
    if(index >= p_pre_obj_lst->size()) return nullptr; // already at the end
    if(nullptr == (*p_pre_obj_lst)[index]) return nullptr; // shouldn't happen
    if(UINT_MAX == indexSubobject)
    {
        return (*p_pre_obj_lst)[index];
    }
    else
    {
        const vector<IMECPreObject *>& subobjects = (*p_pre_obj_lst)[index]->GetSubobject();
        if(indexSubobject < subobjects.size()) return subobjects[indexSubobject];
        else                                   return nullptr; // shouldn't happen
    }
}

bool SDISelectionDataPOMainAndSubobjects::Next()
{
    if(!p_pre_obj_lst) return false; // shouldn't happen
    if(p_index == p_pre_obj_lst->size()) return false; // already at the end
    bool isSubobject = false;
    do
    {
        isSubobject = NextSubobject();
        if(!isSubobject) p_index = UINT_MAX == p_index ? p_indexFirst : p_index + 1;
    }
    while(!isSubobject &&
          p_index < p_pre_obj_lst->size() &&
          nullptr != (*p_pre_obj_lst)[p_index] &&
          0 != strncmp((*p_pre_obj_lst)[p_index]->GetInputFullType(),
                       p_keyword.c_str(),p_keyword.size()));

    IMECPreObject* pObj = GetCurrentEntityPtr();
    SetDataPointer(pObj);

    // skip entities that do not pass the filter TBD

#ifdef PRINT_NEXT_PREOBJECT
    ((ModelViewPO*)GetModelViewRead())->PrintPreobject(pObj);
#endif

    // SelectionObserver hack:
    const SDIModelViewSelectionObservable *pModelViewSO = 
        static_cast<const SDIModelViewSelectionObservable*>(GetModelViewRead());
    if(pModelViewSO->GetSelectionObserver())
        pModelViewSO->GetSelectionObserver()->SelectionNext(*this);

    return p_index < p_pre_obj_lst->size() ? true : false;
}


template <class ENTITYREAD>
bool ModelViewPO::GetValueFromPreObject(const IMECPreObject            *pObj,
    const sdiIdentifier& identifier,
    sdiValue&            value,
    const ENTITYREAD*               pEntityRead,
    const IDescriptor             **ppDescr,
    vector<const IDescriptor*>     *pvpSubDescr,
    bool*                           pIsParameterized,
    sdiString*                       pParameterName,
    bool*                           pParameterIsNegated) const
{
    if(!pObj) return false;
    if(identifier.GetNameKey() == "unitid")
    {
        unsigned int UnitId = (unsigned int) pObj->GetUnitId();
        value.SetValue(
            sdiValueEntity(sdiValueEntityType(myTypeUnit),
                UnitId));
        return true;
    }
    IMECPreObject::MyAttributeType_e atype = IMECPreObject::ATY_UNKNOWN;
    IMECPreObject::MyValueType_e     vtype = IMECPreObject::VTY_UNKNOWN;
    const IDescriptor *pDescr = ppDescr ? *ppDescr : 0;
    string skwd;
    int aindex = -1;
    unsigned int row = identifier.GetRow();
    if(identifier.UsesNumericKey())
    {
        if(!pDescr) pDescr=GetDescriptor(pObj);
        if(!pDescr) return false;
        if(ppDescr) *ppDescr = pDescr;
        int ikwd = END_ARGS;
        ikwd = identifier.GetNumericKey();
        ikwd = pDescr->getIKeywordFromIdentifierValue(DOM_COMMON, ikwd);
        if(0 >= ikwd) return false;
        skwd = pDescr->getSKeyword(ikwd);
        attribute_type_e datype = pDescr->getAttributeType(ikwd);
        value_type_e dvtype = pDescr->getValueType(ikwd);
        atype = HCDIGetPATypeFromDAType(datype);
        vtype = HCDIGetPVTypeFromDVType(dvtype);
        aindex = pObj->GetIndex(atype, vtype, skwd.c_str());
    }
    else
    {
        skwd = identifier.GetNameKey();
        aindex = GetIndexFromPreobject(pObj, skwd, &atype, &vtype);
        if(0 > aindex)
        {
            // nothing stored under this "skeyword", caller might have set the "solver name"
            if(!pDescr) pDescr=GetDescriptor(pObj);
            if(pDescr)
            {
                if(ppDescr) *ppDescr = pDescr;
                const string &skwd2 = HCDI_GetSKeywordFromSolverName
                    <ENTITYREAD, sdiIdentifier, sdiValue, sdiValueEntity, sdiString>
                    (pEntityRead, pDescr, skwd,
                     UINT_MAX == row ? -1 : row, DIMEN_IDEN_BY_SKEY);
                if(skwd2.size() > 0)
                {
                    aindex = GetIndexFromPreobject(pObj,skwd2, &atype, &vtype);
                    skwd = skwd2;
                }
            }
        }
        if(0 > aindex && pDescr)
        {
            // might also be the other way round: data is stored against the "solver name",
            // but caller is querying "skeyword"
            int ikwd = pDescr->getIKeyword(skwd);
            if(END_ARGS != ikwd)
            {
                aindex = GetIndexFromPreobject(pObj, pDescr->getSolverName(ikwd), &atype, &vtype);
            }
        }
        if(0 > aindex && pDescr)
        { // caller might use a DRAWABLE
            const MvDrawable_t *pDrawable = pDescr->getDrawablePtr(skwd);
            if(nullptr != pDrawable)
            {
                int ikwd = pDrawable->getIKeyword();
                if(END_ARGS != ikwd)
                {
                    aindex = GetIndexFromPreobject(pObj, pDescr->getSKeyword(ikwd), &atype, &vtype);
                }
            }
        }
    }
    if(0 > aindex)
    {
        // not found: try to find in subobjects via recursive call
        const vector<IMECPreObject *>& subobjects = pObj->GetSubobject();
        if(nullptr != pvpSubDescr)
        {
            if(pvpSubDescr->size() < subobjects.size()) pvpSubDescr->resize(subobjects.size(), nullptr);
        }

        size_t index = -1;
        size_t i = 0;
        while(i < subobjects.size())
        {
            const IMECPreObject *pSubobject = subobjects[i];
            if(!pSubobject) { ++i; continue; } // shouldn't happen
            const IDescriptor **ppSubDescr = (nullptr != pvpSubDescr) ? &(*pvpSubDescr)[i] : nullptr;
            bool isOk = false;
            if (UINT_MAX == row)
            {
                // the caller has requested a single value, we directly try to get it from the subobject
                EntityReadPO subEntityReadPO(pSubobject, this, ppSubDescr);
                isOk = GetValueFromPreObject(pSubobject, identifier, value, &subEntityReadPO,
                    ppSubDescr, nullptr, pIsParameterized, pParameterName, pParameterIsNegated);
            }
            else // UINT_MAX != row
            {
                // the caller has requested a value in an array, so we have to find out whether the caller wants
                // - a value in the row-th subobject in an array of subobjects or
                // - the row-th value in an array in the (single) subobject
                const IDescriptor *pSubDescr = (nullptr != ppSubDescr) ? *ppSubDescr : nullptr;
                if(nullptr == pSubDescr)
                {
                    pSubDescr = GetDescriptor(pSubobject);
                    if(nullptr != ppSubDescr) *ppSubDescr = pSubDescr;
                }
                if(nullptr != pSubDescr)
                {
                    int ikwd = pSubDescr->getIKeyword(identifier.GetNameKey());
                    if(END_ARGS == ikwd)
                    {
                        // nothing stored under this "skeyword", caller might have set the "solver name"
                        EntityReadPO subEntityReadPO(pSubobject, this, &pSubDescr);
                        const string& skwd2 = HCDI_GetSKeywordFromSolverName
                            <EntityReadPO, sdiIdentifier, sdiValue, sdiValueEntity, sdiString>
                            (&subEntityReadPO, pSubDescr,
                             identifier.GetNameKey(), (int)row, DIMEN_IDEN_BY_SKEY);
                        ikwd = pSubDescr->getIKeyword(skwd2);
                    }
                    if(END_ARGS == ikwd) { ++i; continue; } // not an attribute of this subobject, go on with next one
                }
                // else we go on without descriptor, which means the subobject doesn't know its type
                // (this is (until improvement) the case for some subobject created by the converter)

                int isArray = -1; // -1: unknown, 0: false, 1: true
                if(!pDescr) pDescr=GetDescriptor(pObj);
                if(pDescr)
                {
                    if(ppDescr) *ppDescr = pDescr;
                    const MvSubobjectsCardInfoList_t& cardInfoList = pDescr->GetSubobjectsCardInfoList();
                    for(MvSubobjectsCardInfoList_t::const_iterator it = cardInfoList.begin(); it != cardInfoList.end(); ++it)
                    {
                        const MvSubobjectsCardInfo_t& cardInfo =  *it;
                        if( strlen(pSubobject->GetKernelFullType()) == 0 ||
                            strcmp(pSubobject->GetKernelFullType(), cardInfo.full_type) == 0)
                        {
                            // the subobject seems to correspond to the given cardInfo
                            attribute_type_e atype = pDescr->getAttributeType(cardInfo.ikeyword);
                            if(ATYPE_STATIC_ARRAY == atype || ATYPE_DYNAMIC_ARRAY == atype) isArray = 1;
                            else                                                            isArray = 0;
                            break;
                        }
                    }
                }

                if(-1 == isArray)
                {
                    // no matching feature found in the cfg file, 2nd trial is to compare the types
                    if(0 != row && i + row < subobjects.size())
                    {
                        if(nullptr == subobjects[i + row]) return false; // shouldn't happen
                        const char* type0 = pSubobject->GetKernelFullType();
                        const char* type1 = subobjects[i + row]->GetKernelFullType();
                        if(strcmp(type0, type1)) isArray = 0;
                        else                     isArray = 1;
                    }
                    else
                    {
                        isArray = 0;
                    }
                }

                if(1 == isArray && i + row < subobjects.size())
                {
                    sdiIdentifier newIdentifier(identifier.GetNameKey(),
                        identifier.GetSolverIdx(), identifier.GetColumn());
                    EntityReadPO subEntityReadPO(subobjects[i + row], this,
                        nullptr != pvpSubDescr ? &(*pvpSubDescr)[i + row] : nullptr);
                    return GetValueFromPreObject(subobjects[i + row], newIdentifier, value, &subEntityReadPO,
                        nullptr != pvpSubDescr ? &(*pvpSubDescr)[i + row] : nullptr, nullptr,
                        pIsParameterized, pParameterName, pParameterIsNegated);
                }
                else
                {
                    EntityReadPO subEntityReadPO(pSubobject, this, &pSubDescr);
                    isOk = GetValueFromPreObject(pSubobject, identifier, value, &subEntityReadPO,
                        &pSubDescr, nullptr, pIsParameterized, pParameterName, pParameterIsNegated);
                }
            }
            if(isOk) return true;
            ++i;
        }
    }

    if(0 > aindex)
    {
        return false; // still not found => error
    }

    // check for parameter if desired
    if(nullptr != pIsParameterized || nullptr != pParameterName)
    {
        string paramName = pObj->GetParameterName(skwd.c_str(), row);
        if(paramName.empty())
        {
            if(nullptr != pIsParameterized) *pIsParameterized = false;
            if(nullptr != pParameterName) *pParameterName = "";
        }
        else
        {
            if(nullptr != pIsParameterized) *pIsParameterized = true;
            if(nullptr != pParameterName) *pParameterName = paramName;
            if(nullptr != pParameterIsNegated) *pParameterIsNegated = pObj->IsParameterNegated(skwd.c_str(), row);
        }
    }

    // now actually populate value
    switch(atype)
    {
    case IMECPreObject::ATY_SINGLE:
        switch(vtype)
        {
        case IMECPreObject::VTY_BOOL:   value.SetValue(pObj->GetBoolValue(aindex));   return true;
        case IMECPreObject::VTY_INT:    value.SetValue(pObj->GetIntValue(aindex));    return true;
        case IMECPreObject::VTY_UINT:   value.SetValue(pObj->GetUIntValue(aindex));   return true;
        case IMECPreObject::VTY_FLOAT:  value.SetValue(pObj->GetFloatValue(aindex));  return true;
        case IMECPreObject::VTY_STRING: value.SetValue(sdiString(pObj->GetStringValue(aindex))); return true;
        case IMECPreObject::VTY_OBJECT:
        {
            const char *otypestr = pObj->GetObjectType(aindex);
            if(nullptr != otypestr)
            {
                unsigned int id = (unsigned int) pObj->GetObjectId(aindex);
                unsigned int otype = GetEntitytype(otypestr, id, pObj, skwd, ppDescr);
                value.SetValue(
                    sdiValueEntity(
                        sdiValueEntityType(otype), id));
                return true;
            }
            else
            {
                value.SetValue(sdiValueEntity());
                return false;
            }
        }
        default: return false;
        }
    case IMECPreObject::ATY_ARRAY:
    {
        unsigned int row = identifier.GetRow();
        if(UINT_MAX != row) // single value queried
        {
            int nbValues = pObj->GetNbValues(vtype, aindex);
            if(((int) row) >= nbValues) return false;
            switch(vtype)
            {
            case IMECPreObject::VTY_BOOL:   value.SetValue(pObj->GetBoolValue(aindex, row));   return true;
            case IMECPreObject::VTY_INT:    value.SetValue(pObj->GetIntValue(aindex, row));    return true;
            case IMECPreObject::VTY_UINT:   value.SetValue(pObj->GetUIntValue(aindex, row));   return true;
            case IMECPreObject::VTY_FLOAT:  value.SetValue(pObj->GetFloatValue(aindex, row));  return true;
            case IMECPreObject::VTY_STRING: value.SetValue(sdiString(pObj->GetStringValue(aindex, row))); return true;
            case IMECPreObject::VTY_OBJECT:
            {
                string att_obj_type = "";
                const char *otypestr = pObj->GetObjectType(aindex, row);
                if(nullptr != otypestr)
                {
                    unsigned int id = (unsigned int) pObj->GetObjectId(aindex,row);
                    unsigned int otype = GetEntitytype(otypestr, id, pObj, skwd, ppDescr);
                    value.SetValue(
                        sdiValueEntity(
                            sdiValueEntityType(otype), id));
                    return true;
                }
                else
                {
                    value.SetValue(sdiValueEntity());
                    return false;
                }
            }
            default: return false;
            }
        }
        else // complete array queried
        {
            int nbValues = pObj->GetNbValues(vtype, aindex);
            if(0 >= nbValues) return false;
            switch(vtype)
            {
            case IMECPreObject::VTY_BOOL:
            {
                sdiBoolList value_loc(nbValues);
                for(int i = 0; i < nbValues; ++i) value_loc[i] = pObj->GetBoolValue(aindex, i);
                value = sdiValue(value_loc);
                return true;
            }
            case IMECPreObject::VTY_INT:
            {
                sdiIntList value_loc(nbValues);
                for(int i = 0; i < nbValues; ++i) value_loc[i] = pObj->GetIntValue(aindex, i);
                value = sdiValue(value_loc);
                return true;
            }
            case IMECPreObject::VTY_UINT:
            {
                sdiUIntList value_loc(nbValues);
                for(int i = 0; i < nbValues; ++i) value_loc[i] = pObj->GetUIntValue(aindex, i);
                value = sdiValue(value_loc);
                return true;
            }
            case IMECPreObject::VTY_FLOAT:
            {
                sdiDoubleList value_loc(nbValues);
                for(int i = 0; i < nbValues; ++i) value_loc[i] = pObj->GetFloatValue(aindex, i);
                value = sdiValue(value_loc);
                return true;
            }
            case IMECPreObject::VTY_STRING:
            {
                sdiStringList value_loc(nbValues);
                for(int i = 0; i < nbValues; ++i) value_loc[i] = pObj->GetStringValue(aindex, i);
                value = sdiValue(value_loc);
                return true;
            }
            case IMECPreObject::VTY_OBJECT:
            {
                int i = 0;
                const char *otypestr = nullptr;
                do
                {
                    otypestr = pObj->GetObjectType(aindex, i);
                    i++;
                } while (nullptr == otypestr && i < nbValues);
                if(nullptr != otypestr)
                {
                    unsigned int id = pObj->GetObjectId(aindex, 0);
                    unsigned int otype = GetEntitytype(otypestr, id, pObj, skwd, ppDescr);
                    sdiUIntList value_loc(nbValues);
                    for(i = 0; i < nbValues; ++i) value_loc[i] = pObj->GetObjectId(aindex, i);
                    value = sdiValue(sdiValueEntityList(sdiValueEntityType(otype), value_loc));
                    return true;
                }
                else
                {
                    value.SetValue(sdiValueEntityList());
                    return false;
                }
            }
            default: return false;
            }
        }
    }
    return false;
    default:
        return false;
    }
}

static void HCDIPreObjectSetSubobjectArraySize(IMECPreObject *pObj,
    const string &skwd, int val, const IDescriptor *pDescr, const CFGKernel *pCFGKernel)
{
    if(nullptr == pObj || nullptr == pDescr) return;

    const vector<IMECPreObject *>& subobjects = pObj->GetSubobject();

    int ikwd = pDescr->getIKeyword(skwd);
    if(END_ARGS == ikwd || ATYPE_SIZE != pDescr->getAttributeType(ikwd)) return;

    MvIKeywordSet_t ikw_set;
    pDescr->getSizeConnectedIKeywords(ikwd, &ikw_set);
    for(int ikwdConn : ikw_set)
    {
        if(VTYPE_OBJECT == pDescr->getValueType(ikwdConn))
        {
            string ftypestr;
            const char *typestring = "SUBOBJECT";
            if(HCDI_OBJ_TYPE_SUBOBJECT == pDescr->getObjectType(ikwdConn))
            {
                MvFullTypeSet_t fulltypeset;
                HCDIGetMultiObjectTypes(pDescr, ikwdConn, fulltypeset);
                if(!fulltypeset.empty())
                {
                    ftypestr = string(*(fulltypeset.begin()));
                    typestring = fulltypeset.begin()->getTypeStr().c_str();
                }
            }
            if(ftypestr.empty() && nullptr != pCFGKernel)
            {
                // haven't found in attribute definition, let's try to find in the subobject cards
                // (even if the type is other than HCDI_OBJ_TYPE_SUBOBJECT)
                const MvSubobjectsCardInfoList_t& cardInfoList = pDescr->GetSubobjectsCardInfoList();
                for(MvSubobjectsCardInfoList_t::const_iterator it = cardInfoList.begin(); it != cardInfoList.end(); ++it)
                {
                    const MvSubobjectsCardInfo_t& cardInfo =  *it;
                    if(cardInfo.ikeyword == ikwdConn)
                    {
                        ftypestr = cardInfo.full_type;
                        typestring = MvFullType_t(*pCFGKernel, ftypestr).getTypeStr().c_str();
                        break;
                    }
                }
            }
            if(ftypestr.empty()) continue;

            // resize or create array
            int sizeOld = 0;
            string skwdConn = pDescr->getSKeyword(ikwdConn);
            int ind = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, skwdConn);
            if(0 <= ind)
            {
                sizeOld = pObj->GetNbValues(IMECPreObject::VTY_OBJECT, ind);
                if(sizeOld < val) pObj->resizeArray(IMECPreObject::VTY_OBJECT, ind, val);
                // resizing to a smaller size TBD!
            }
            else if(0 < val)
            {
                pObj->AddObjectArray(skwdConn.c_str(), val);
                ind = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, skwdConn);
            }

            // create sub-preobjects and populate array
            // workaround
            MvFileFormat_e subUserProfile = FF_UNKNOWN;
            if(nullptr != pCFGKernel) subUserProfile = pCFGKernel->getSubUserProfile();
            MultiCFGKernelMgr::CFGKernelSentinel sentinel(subUserProfile); 

            for(int i = sizeOld; i < val; ++i)
            {
                unsigned int subId = (unsigned int) subobjects.size() + 1;
                IMECPreObject* pSubobj = HCDI_GetPreObjectHandle(ftypestr.c_str(), "", pObj->GetTitle(), subId, 0);
                pObj->SetSubobject(pSubobj);
                pObj->SetObjectValue(ind, i, typestring, subId);
            }
        }
    }
}

// The following is tricky, because there are many combinations of
// - row and column of the identifier (variables int "i" and "j" used here, int because in
//       HCDI world int is used rather than unsigned int, -1 meaning "not assigned")
// - dimension (0: single, 1: array) of the value (variable name "valDim")
// - dimension of the subobject attribute in the main object ("subDim")
// - dimension of the attribute in the subobject ("attDim"):
//
// |Indentifier| Value  | Main   | Subobj | Use case                                   |
// | row | col | valDim | subDim | attDim |                                            |
// | -   | -   | 0      | 0      | 0      | single att of single subobj                |
// | -   | -   | 1      | 0      | 1      | array att of single subobj                 |
// | i   | -   | 0      | 0      | 1      | val i in array att of single subobj        |
// | i   | -   | 0      | 1      | 0      | single att of i-th subobj of array         |
// | i   | -   | 1      | 1      | 1      | array att of i-th subobj of array          |
// | i   | j   | 0      | 1      | 1      | val j in array att of i-th subobj of array |
// | -   | -   | 1      | 1      | 0      | single att of all subobjs of array TBD!!!  |
//
template <class ENTITYREAD>
IMECPreObject * HCDIPreObjectGetSubobject(IMECPreObject* pObj, string& skeyword, int i, int j, unsigned int valDimExtent,
    vector<const IDescriptor*>* pvpSubDescr, unsigned int* pAttDim, value_type_e* pVtype, int* pIkwd,
    const CFGKernel *pCFGKernel, const ENTITYREAD* pEntityRead, enum IdenKeyType keytype, unsigned int* pSubIndex=nullptr)
{
    const vector<IMECPreObject *>& subobjects = pObj->GetSubobject();

    size_t index = -1;
    for(size_t k = 0; k < subobjects.size(); ++k)
    {
        const IMECPreObject *pSubobject = subobjects[k];
        if(!pSubobject) continue;
        const IDescriptor*pSubDescr = nullptr;
        if(pvpSubDescr && pvpSubDescr->size() > k) pSubDescr = pvpSubDescr->operator[](k);
        if(!pSubDescr)
        {
            // workaround
            MvFileFormat_e subUserProfile = FF_UNKNOWN;
            if(nullptr != pCFGKernel) subUserProfile = pCFGKernel->getSubUserProfile();
            MultiCFGKernelMgr::CFGKernelSentinel sentinel(subUserProfile); 

            pSubDescr = HCDI_GetDescriptorHandle(pSubobject->GetKernelFullType());
            if(pvpSubDescr)
            {
                if(pvpSubDescr->size() <= k) pvpSubDescr->resize(subobjects.size(), nullptr);
                pvpSubDescr->operator[](k) = pSubDescr;
            }
        }

        if(pSubDescr)
        {
            int ikwd = pSubDescr->getIKeyword(skeyword);
            if(END_ARGS == ikwd)
            { // not an skeyword / attribute name of this subobject
                // NB: We are passing pEntityRead, i.e. an interface to the parent entity here, but this should
                // be fine, because the GetValue() calls on the parent entity will be routed to the subobject
                const string& skwd = HCDI_GetSKeywordFromSolverName
                    <ENTITYREAD, sdiIdentifier, sdiValue, sdiValueEntity, sdiString>
                    (pEntityRead, pSubDescr, skeyword, i, DIMEN_IDEN_BY_SKEY);
                if(skwd.empty()) continue; // neither a solvername of this subobject
                skeyword = skwd;
                ikwd = pSubDescr->getIKeyword(skwd);
            }

            // if we get here, this is the first preobject whose descriptor has the skeyword,
            // so we count from here to get the one that we want
            // TBD: check whether this one is really the right one, because subobjects of different
            // attributes might be mixed!
            unsigned int attDim = 0;
            attribute_type_e atype = pSubDescr->getAttributeType(ikwd);
            if(ATYPE_STATIC_ARRAY == atype || ATYPE_DYNAMIC_ARRAY == atype) attDim = 1;
            if(0 > i)
            {
                i = 0;
                if(0 < valDimExtent && 0 == attDim)
                { // array value but single attribute => array of subobjects => get last one
                    i = valDimExtent - 1;
                }
            }
            else if(0 < i && 0 > j && 1 == attDim && 0 == valDimExtent)
            {
                // single value to be set in an array => must be a single preobject
                i = 0;
            }

            if(subobjects.size() > (k + i))
            {
                if(nullptr != pAttDim) *pAttDim = attDim;
                if(nullptr != pVtype) *pVtype = pSubDescr->getValueType(ikwd);
                if(nullptr != pIkwd) *pIkwd = ikwd;
                if(nullptr != pSubIndex) *pSubIndex = (unsigned int) k + i;
                return subobjects[k + i];
            }
            else                            return nullptr;
        }
    }
    return nullptr;
}

static value_type_e HCDI_ConvertValueTypeFromHWDescriptor(
    sdiBasicType hwBasicType, sdiCompoundType hwCompoundType)
{
    if(COMPOUND_TYPE_ENTITY == hwCompoundType)
    {
        return VTYPE_OBJECT;
    }
    else
    {
        switch(hwBasicType)
        {
        case BASIC_TYPE_BOOL:
            return VTYPE_BOOL;
        case BASIC_TYPE_INT:
            return VTYPE_INT;
        case BASIC_TYPE_UINT:
            return VTYPE_UINT;
        case BASIC_TYPE_DOUBLE:
            return VTYPE_FLOAT;
        case BASIC_TYPE_STRING:
            return VTYPE_STRING;
        default:
            return VTYPE_UNKNOWN;
        }
    }
}

static IMECPreObject* HCDIPreObjectCreateSubobject(IMECPreObject* pObj,
    string& skeyword, /* IMECPreObject::MyValueType_e or */ value_type_e vtype, int i, unsigned int valDimExtent,
    const IDescriptor* pDescr,
    vector<const IDescriptor*>* pvpSubDescr, unsigned int* pAttDim, int* pIkwd,
    const CFGKernel *pCFGKernel, unsigned int* pSubIndex=nullptr)
{
    // if no subobject found, it might not yet be created, let's have a look for possible sub-descriptors
    // TBD: This is quite a hack so far, a modified copy from SDIModelViewSDICFG::ConvertIdentifierToSDI,
    // to be cleaned up and componentized!
    const MvSubobjectsCardInfoList_t& cardInfoList = pDescr->GetSubobjectsCardInfoList();
    for(MvSubobjectsCardInfoList_t::const_iterator it = cardInfoList.begin(); it != cardInfoList.end(); ++it)
    {
        const MvSubobjectsCardInfo_t& cardInfo =  *it;
        const char* ftypestr = cardInfo.full_type;
        const IDescriptor* pSubDescr = pCFGKernel->GetDescriptorHandle(ftypestr);
        if(nullptr == pSubDescr) continue;

        int ikwd = pSubDescr->getIKeyword(skeyword);
        if(END_ARGS == ikwd)
        { // not an skeyword / attribute name of this subobject
          // to be replaced by code which handles conditional solver names
            // const string& skwd = pSubDescr->getSKeywordFromSolverName(skeyword);
            const MvCondKeywordList_t& kwdList = pSubDescr->getSKeywordsFromSolverName(skeyword);
            for(MvCondKeywordList_t::const_iterator kwdIt = kwdList.begin(); kwdIt != kwdList.end(); ++kwdIt)
            {
                const string& condSkwd = kwdIt->first;
                int condIkwd = pSubDescr->getIKeyword(condSkwd);
                if(pSubDescr->getValueType(condIkwd) == vtype)
                {
                    skeyword = condSkwd;
                    ikwd = condIkwd;
                    break;
                }
            }
            if(END_ARGS == ikwd) continue; // neither a solvername of this subobject
        }

        // if we get here, we have found, so create the subobjects
        const vector<IMECPreObject *>& subobjects = pObj->GetSubobject();
        size_t subobjectsCountOld = subobjects.size();
        int subobjIkwd = cardInfo.ikeyword;
        attribute_type_e atype = pDescr->getAttributeType(subobjIkwd);
        if(atype == ATYPE_DYNAMIC_ARRAY)
        {
            int sizeIkwd = pDescr->getSizeIKeyword(subobjIkwd);
            if(END_ARGS == sizeIkwd) continue; // shouldn't happen
            string sizeSkwd = pDescr->getSKeyword(sizeIkwd);
            unsigned int      nbVal = 1;
            if(0 < i)         nbVal = i + 1;
            if(0 < valDimExtent) nbVal = valDimExtent;

            HCDIPreObjectSetSubobjectArraySize(pObj, sizeSkwd, nbVal, pDescr, pCFGKernel);
        }
        else if(atype == ATYPE_VALUE)
        {
            unsigned int subId = (unsigned int) subobjectsCountOld + 1;
            IMECPreObject* pSubobj = HCDI_GetPreObjectHandle(ftypestr, "",
                pObj->GetTitle(), subId, 0);
            pObj->SetSubobject(pSubobj);
            MvFullType_t ftype(*pCFGKernel, ftypestr);
            pObj->AddObjectValue(pDescr->getSKeyword(subobjIkwd).c_str(), ftype.getTypeStr().c_str(), subId);
        }

        // the last preobjects should be the new ones
        if(nullptr != pvpSubDescr && pvpSubDescr->size() < subobjects.size())
        {
            pvpSubDescr->resize(subobjects.size(), nullptr);
            for(size_t iSubobject = subobjectsCountOld; iSubobject < pvpSubDescr->size(); ++iSubobject)
            {
                pvpSubDescr->at(iSubobject) = pSubDescr;
            }
        }

        if(nullptr != pAttDim)
        {
            attribute_type_e atype = pSubDescr->getAttributeType(ikwd);
            if(ATYPE_STATIC_ARRAY == atype || ATYPE_DYNAMIC_ARRAY == atype) *pAttDim = 1;
            else                                                            *pAttDim = 0;
        }
        if(nullptr != pIkwd) *pIkwd = ikwd;
        if(0 < subobjects.size())
        {
            if(nullptr != pSubIndex) *pSubIndex = (unsigned int) subobjects.size() - 1;
            return const_cast<IMECPreObject*>(subobjects[subobjects.size() - 1]);
        }
        else return nullptr; // shouldn't come here
    }

    return nullptr;
}

// utility functions for value type conversion, in order to be as permissive as possible
template <class VALUETYPE>
VALUETYPE GetValueDim0(const sdiValue& value)
{
    switch(value.GetBasicType())
    {
    case BASIC_TYPE_BOOL:
    {
        bool value_loc;
        value.GetValue(value_loc);
        return (VALUETYPE)value_loc;
    }
    case BASIC_TYPE_INT:
    {
        int value_loc;
        value.GetValue(value_loc);
        return (VALUETYPE)value_loc;
    }
    case BASIC_TYPE_UINT:
    {
        unsigned int value_loc;
        value.GetValue(value_loc);
        return (VALUETYPE)value_loc;
    }
    case BASIC_TYPE_DOUBLE:
    {
        double value_loc;
        value.GetValue(value_loc);
        return (VALUETYPE)value_loc;
    }
    default:
        return (VALUETYPE)0;
    }
}

template <class SOURCETYPE, class TARGETTYPE>
bool ConvertListDim1(const sdiValue& value, sdiVector<TARGETTYPE>& targetList)
{
    sdiVector<SOURCETYPE> sourceList;
    bool isOk = value.GetValue(sourceList);
    targetList.reserve(sourceList.size());
    for(size_t i = 0; i < sourceList.size(); ++i) targetList.push_back((TARGETTYPE)sourceList[i]);
    return isOk;
}

template <class TARGETTYPE>
void GetValueDim1(const sdiValue& value, sdiVector<TARGETTYPE>& targetList)
{
    switch(value.GetBasicType())
    {
    case BASIC_TYPE_BOOL:
        ConvertListDim1<bool>(value, targetList);
        break;
    case BASIC_TYPE_INT:
        ConvertListDim1<int>(value, targetList);
        break;
    case BASIC_TYPE_UINT:
        ConvertListDim1<unsigned int>(value, targetList);
        break;
    case BASIC_TYPE_DOUBLE:
        ConvertListDim1<double>(value, targetList);
        break;
    default:
        ;
    }
}

static int AssureArrayDimCompatibility(const IDescriptor* pDescr, int ikwd, unsigned int valDim, int i)
{
    if(ikwd == END_ARGS) return END_ARGS;
    attribute_type_e atype = pDescr->getAttributeType(ikwd);
    if( (0 == valDim && (ATYPE_VALUE == atype || ATYPE_SIZE == atype)) ||
        ((1 == valDim || i >= 0) && (ATYPE_STATIC_ARRAY == atype || ATYPE_DYNAMIC_ARRAY  == atype)))
    {
        return ikwd;
    }
    return END_ARGS;
}

template<class T>
sdiValue TCreateSubValue(const sdiValue& other, const unsigned int  i, const unsigned int  j)
{
    T val;
    bool isOk = other.GetValue(val, i, j);
    if(!isOk) return sdiValue();
    return sdiValue(val);
}

static sdiValue CreateSubValue(const sdiValue&     other,
                               const unsigned int  i,             //! index in first (outermost) dimension
                               const unsigned int  j = UINT_MAX)  //! index in second dimension
{
    switch(other.GetCompoundType())
    {
    case COMPOUND_TYPE_ENTITY:
        return TCreateSubValue<sdiValueEntity>(other, i, j);
    default:
        switch(other.GetBasicType())
        {
        case BASIC_TYPE_BOOL:   return TCreateSubValue<bool>        (other, i, j);
        case BASIC_TYPE_INT:    return TCreateSubValue<int>         (other, i, j);
        case BASIC_TYPE_UINT:   return TCreateSubValue<unsigned int>(other, i, j);
        case BASIC_TYPE_DOUBLE: return TCreateSubValue<double>      (other, i, j);
        default:                return sdiValue();
        }
    }
}

template <class ENTITYREAD>
bool ModelViewPO::SetValueToPreObject(IMECPreObject*              pObj,
                                      const sdiIdentifier&        identifier,
                                      const sdiValue&             value,
                                      const ENTITYREAD*           pEntityRead,
                                      const IDescriptor**         ppDescr,
                                      vector<const IDescriptor*>* pvpSubDescr,
                                      const char*                 parameterName,
                                      bool                        parameterIsNegated) const
{
    if(!pObj) return false;
    const IDescriptor *pDescr = ppDescr ? *ppDescr : 0;
    sdiString skeyword;
    if(identifier.UsesNumericKey())
    {
        if(!pDescr) pDescr=GetDescriptor(pObj);
        if(!pDescr) return false;
        if(ppDescr) *ppDescr = pDescr;
        int ikwd = END_ARGS;
        ikwd = identifier.GetNumericKey();
        ikwd = pDescr->getIKeywordFromIdentifierValue(DOM_COMMON, ikwd);
        if(0 >= ikwd) return false; // shouldn't happen
        skeyword = pDescr->getSKeyword(ikwd);
    }
    else
    {
        skeyword = identifier.GetNameKey();
    }
    if(skeyword.empty()) return false;
    bool isOk = true;
    if(skeyword == "solverkeyword")
    {
        sdiString value_loc;
        value.GetValue(value_loc);
        pObj->SetInputFullType(value_loc.c_str());
        return true;
    }
    else if(skeyword == "unitid")
    {
        int UnitId = 0;
        if(value.GetCompoundType() == COMPOUND_TYPE_ENTITY)
        {
            sdiValueEntity value_loc;
            isOk = value.GetValue(value_loc);
            UnitId = (int) value_loc.GetId();
        }
        else
        {
            switch(value.GetBasicType())
            {
            case BASIC_TYPE_INT:
                isOk = value.GetValue(UnitId);
                break;
            case BASIC_TYPE_UINT:
            {
                unsigned int value_loc;
                isOk = value.GetValue(value_loc);
                UnitId = (int) value_loc;
            }
            break;
            default:
                isOk = false; // shouldn't happen
            }
        }
        pObj->SetUnitId(UnitId);
        return true;
    }

    if(nullptr == pDescr)
    {
        pDescr = GetDescriptor(pObj);
        if(nullptr != ppDescr) *ppDescr = pDescr;
        if(nullptr == pDescr) return false; // shouldn't happen
    }

    // (hopefully) temporary quick and dirty hack for "type" of /ADMAS
    if(skeyword == "type" && !strcmp(pObj->GetInputFullType(), "/ADMAS"))
    {
        // set value to main object
        int type = GetValueDim0<int>(value);
        pObj->AddIntValue(skeyword.c_str(), type);
        // set value to subobject
        if(type == 5 || type == 6 || type == 7)
        {
            IMECPreObject *pSubObj = HCDIPreObjectGetSubobject(
                pObj, skeyword, -1, -1, 0, pvpSubDescr, nullptr, nullptr, nullptr, p_pCFGKernel,
                pEntityRead, DIMEN_IDEN_BY_SKEY);
            if(nullptr == pSubObj)
            {
                pSubObj = HCDIPreObjectCreateSubobject(pObj, skeyword, VTYPE_INT, -1, 0, pDescr,
                    pvpSubDescr, nullptr, nullptr, p_pCFGKernel);
            }
            if(nullptr != pSubObj) pSubObj->AddIntValue(skeyword.c_str(), type);
        }
        return true;
    }

    unsigned int valDim = value.GetArrayDimension();
    unsigned int valDimExtent = 0;
    if(1 == valDim) valDimExtent = value.GetArrayDimensionExtent();
    int i = -1, j = -1;
    if(identifier.GetRow() != UINT_MAX) i = (int) identifier.GetRow();
    if(identifier.GetColumn() != UINT_MAX) j = (int) identifier.GetColumn();

    int ikwd = pDescr->getIKeyword(skeyword.c_str());
    ikwd = AssureArrayDimCompatibility(pDescr, ikwd, valDim, i);
    value_type_e vtype = VTYPE_UNKNOWN;
    if(ikwd == END_ARGS)
    { // not an skeyword / attribute name of the main object
        const string& skwd = HCDI_GetSKeywordFromSolverName
            <ENTITYREAD, sdiIdentifier, sdiValue, sdiValueEntity, sdiString>
            (pEntityRead, pDescr, skeyword, i, DIMEN_IDEN_BY_SKEY);
        if(skwd.size() > 0)
        { // it's a solvername of the main object, but we still have to check array compatibility
            ikwd = pDescr->getIKeyword(skwd.c_str());
            ikwd = AssureArrayDimCompatibility(pDescr, ikwd, valDim, i);
            if(ikwd != END_ARGS)
            {
                skeyword = skwd;
                vtype = pDescr->getValueType(ikwd);
            }
        }
        if(ikwd == END_ARGS)
        { // it's not a solvername, so try DRAWABLE
            const MvDrawable_t *pDrawable = pDescr->getDrawablePtr(skeyword);
            if(nullptr != pDrawable)
            {
                ikwd = pDrawable->getIKeyword();
                if(END_ARGS != ikwd)
                { // it's a DRAWABLE of the main object, but we still have to check array compatibility
                    ikwd = AssureArrayDimCompatibility(pDescr, ikwd, valDim, i);
                    if(ikwd != END_ARGS)
                    {
                        skeyword = pDescr->getSKeyword(ikwd);
                        vtype = pDescr->getValueType(ikwd);
                    }
                }
            }
        }
        if(ikwd == END_ARGS)
        { // not in main object, must be in subobject
            unsigned int attDim = 0, subIndex = 0;
            IMECPreObject *pSubObj = HCDIPreObjectGetSubobject(
                pObj, skeyword, i, j, valDimExtent, pvpSubDescr, &attDim, &vtype, &ikwd, p_pCFGKernel,
                pEntityRead, DIMEN_IDEN_BY_SKEY, &subIndex);
            if(nullptr == pSubObj)
            {
                vtype = HCDI_ConvertValueTypeFromHWDescriptor(
                    value.GetBasicType(), value.GetCompoundType());
                pSubObj = HCDIPreObjectCreateSubobject(pObj, skeyword, vtype, i, valDimExtent, pDescr,
                    pvpSubDescr, &attDim, &ikwd, p_pCFGKernel, &subIndex);
            }
            // assert(nullptr != pSubObj); 
            if(nullptr != pSubObj)
            {
                if(0 <= i)
                {
                    pObj = pSubObj;
                    pDescr = nullptr != pvpSubDescr && pvpSubDescr->size() > subIndex ?
                        pvpSubDescr->at(subIndex) : nullptr;
                    // we will set the value to the subobject
                    // 0 <= i is equivalent to identifier.GetRow() != UINT_MAX
                    // The given row might be the index of the subobject in an array of subobjects,
                    // or the index in an array attribute in a single subobject.
                    // As we don't know here whether it's a single subobject or an array of
                    // subobjects, we conclude from the dimension (single or array) of the
                    // attribute of the subobject and the dimension of the value
                    if(0 == attDim && 0 == valDim)
                    { // it seems to be a single value in an array of subobjects
                        assert(identifier.GetColumn() == UINT_MAX);
                        i = -1;
                    }
                    else if(1 == attDim && 0 == valDim && identifier.GetColumn() != UINT_MAX)
                    { // it seems to be an element in an array value in an array of subobjects
                        i = (int) identifier.GetColumn();
                    }
                    else if(1 == attDim && 0 == valDim) // && identifier.GetColumn() == UINT_MAX)
                    { // it seems to be an element in an array value in a single subobject
                        ; // nothing to do, i is already assigned correctly
                    }
                    else if(1 == attDim && 1 == valDim)
                    { // it seems to be an element in an array value in an array of subobjects
                        assert(identifier.GetColumn() == UINT_MAX);
                        i = -1;
                    }
                    else
                    { 
                        assert(0);
                    }
                }
                else if(0 == attDim && 1 == valDim)
                { // it seems to be single values in an array of subobjects
                    const vector<IMECPreObject*>& subobjects = pObj->GetSubobject();
                    // pSubObj should point to the last one of this array, subIndex is the index
                    assert(subIndex < subobjects.size() && subIndex + 1 >= valDimExtent);
                    if(!(subIndex < subobjects.size() && subIndex + 1 >= valDimExtent)) return false;
                    unsigned int iFirst = subIndex + 1 - valDimExtent;
                    for(unsigned int k = 0; k < valDimExtent; ++k)
                    {
                        // sdiValue subvalue(value, k); // using "subvalue constructor"
                        sdiValue subvalue = CreateSubValue(value, k); // workaround for "subvalue constructor"
                        const IDescriptor* pSubDescr = nullptr != pvpSubDescr && pvpSubDescr->size() > k ?
                            pvpSubDescr->at(k) : nullptr;
                        SetValueToPreObject(subobjects[iFirst + k], identifier,
                            subvalue, pEntityRead, &pSubDescr, nullptr, parameterName, parameterIsNegated);
                    }
                    return true;
                }
                else
                {
                    pObj = pSubObj; // we will set the value to the subobject
                    pDescr = nullptr != pvpSubDescr && pvpSubDescr->size() > subIndex ?
                        pvpSubDescr->at(subIndex) : nullptr;
                }
            }
        }
    }
    else
    {
        vtype = pDescr->getValueType(ikwd);
    }

    if(valDim == 0)
    {
        if(value.GetCompoundType() == COMPOUND_TYPE_ENTITY)
        {
            sdiValueEntity value_loc;
            isOk = value.GetValue(value_loc);
            const char *otype = nullptr;
            if(value_loc.GetEntityFullType().IsTypeNumeric())
            {
                unsigned int CFGType = GetCFGType(value_loc.GetEntityFullType().GetTypeNumeric());
                otype = HCDI_get_entitystringtype((int) CFGType).c_str();
            }
            else
            {
                EntityType type = GetEntityType(value_loc.GetEntityFullType().GetTypeNamed());
                unsigned int CFGType = GetCFGType(type);
                otype = HCDI_get_entitystringtype((int) CFGType).c_str();
            }
            if(i == -1)
                pObj->AddObjectValue(skeyword.c_str(), otype, value_loc.GetId());
            else
            {
                int keywordIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, skeyword);
                if (keywordIndex == -1)
                {
                    pObj->AddObjectArray(skeyword.c_str(), i + 1);
                    keywordIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, skeyword);
                }
                else
                {
                    int arraySize = pObj->GetNbValues(IMECPreObject::VTY_OBJECT, keywordIndex);
                    if (arraySize <= i)
                        pObj->resizeArray(IMECPreObject::VTY_OBJECT, keywordIndex, i + 1);
                }
                pObj->SetObjectValue(keywordIndex, i,otype, value_loc.GetId());
            }
        }
        else
        {
            switch(vtype)
            {
            case VTYPE_BOOL:
            {
                bool value_loc = GetValueDim0<bool>(value);
                if (i == -1)
                    pObj->AddBoolValue(skeyword.c_str(), value_loc);
                else
                {
                    int keywordIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_BOOL, skeyword);
                    if (keywordIndex == -1)
                    {
                        pObj->AddIntArray(skeyword.c_str(), i + 1);
                        keywordIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_BOOL, skeyword);
                    }
                    else
                    {
                        int arraySize = pObj->GetNbValues(IMECPreObject::VTY_BOOL, keywordIndex);
                        if (arraySize <= i)
                            pObj->resizeArray(IMECPreObject::VTY_BOOL, keywordIndex, i + 1);
                    }
                    pObj->SetIntValue(keywordIndex, i, value_loc);
                }
            }
            break;
            case VTYPE_INT:
            {
                int value_loc = GetValueDim0<int>(value);
                if (i == -1)
                {
                    // if it is the size of an array of subobjects we create these here
                    if(!pDescr) pDescr=GetDescriptor(pObj);
                    if(pDescr)
                    {
                        HCDIPreObjectSetSubobjectArraySize(pObj, skeyword, value_loc, pDescr, p_pCFGKernel);
                    }
                    // now actually set the value
                    pObj->AddIntValue(skeyword.c_str(), value_loc);
                }
                else
                {
                    int keywordIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, skeyword);
                    if (keywordIndex == -1)
                    {
                        pObj->AddIntArray(skeyword.c_str(), i + 1);
                        keywordIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, skeyword);
                    }
                    else
                    {
                        int arraySize = pObj->GetNbValues(IMECPreObject::VTY_INT, keywordIndex);
                        if (arraySize <= i)
                            pObj->resizeArray(IMECPreObject::VTY_INT, keywordIndex, i + 1);
                    }
                    pObj->SetIntValue(keywordIndex, i, value_loc);
                }
            }
            break;
            case VTYPE_UINT:
            {
                unsigned int value_loc = GetValueDim0<unsigned int>(value);
                if (i == -1)
                    pObj->AddUIntValue(skeyword.c_str(), value_loc);
                else
                {
                    int keywordIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, skeyword);
                    if (keywordIndex == -1)
                    {
                        pObj->AddIntArray(skeyword.c_str(), i + 1);
                        keywordIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, skeyword);
                    }
                    else
                    {
                        int arraySize = pObj->GetNbValues(IMECPreObject::VTY_UINT, keywordIndex);
                        if (arraySize <= i)
                            pObj->resizeArray(IMECPreObject::VTY_UINT, keywordIndex, i + 1);
                    }
                    pObj->SetUIntValue(keywordIndex, i, value_loc);
                }
            }
            break;
            case VTYPE_FLOAT:
            {
                double value_loc = GetValueDim0<double>(value);
                if (i == -1)
                    pObj->AddFloatValue(skeyword.c_str(), value_loc);
                else
                {
                    int keywordIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, skeyword);
                    if (keywordIndex == -1)
                    {
                        pObj->AddFloatArray(skeyword.c_str(), i + 1);
                        keywordIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, skeyword);
                    }
                    else
                    {
                        int arraySize = pObj->GetNbValues(IMECPreObject::VTY_FLOAT, keywordIndex);
                        if (arraySize <= i)
                            pObj->resizeArray(IMECPreObject::VTY_FLOAT, keywordIndex, i + 1);
                    }
                    pObj->SetFloatValue(keywordIndex, i, value_loc);
                }
            }
            break;
            case VTYPE_STRING:
            {
                assert(BASIC_TYPE_STRING == value.GetBasicType());
                sdiString value_loc;
                isOk = value.GetValue(value_loc);
                if (i == -1)
                    pObj->AddStringValue(skeyword.c_str(), value_loc.c_str());
                else
                {
                    int keywordIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, skeyword);
                    if (keywordIndex == -1)
                    {
                        pObj->AddStringArray(skeyword.c_str(), i + 1);
                        keywordIndex = pObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, skeyword);
                    }
                    else
                    {
                        int arraySize = pObj->GetNbValues(IMECPreObject::VTY_STRING, keywordIndex);
                        if (arraySize <= i)
                            pObj->resizeArray(IMECPreObject::VTY_STRING, keywordIndex, i + 1);
                    }
                    pObj->SetStringValue(keywordIndex, i, value_loc.c_str());
                }
            }
            break;
            default: ;
            }
        }
        if(nullptr != parameterName)
        {
            pObj->SetParameterName(parameterName, skeyword.c_str(), i, parameterIsNegated);
        }
    }
    else if(valDim == 1)
    {
        if(value.GetCompoundType() == COMPOUND_TYPE_ENTITY)
        {
            sdiValueEntityList value_loc;
            isOk = value.GetValue(value_loc);
            const char *otype = nullptr;
            if(value_loc.GetEntityFullType().IsTypeNumeric())
            {
                unsigned int CFGType = GetCFGType(value_loc.GetEntityFullType().GetTypeNumeric());
                otype = HCDI_get_entitystringtype((int) CFGType).c_str();
            }
            else
            {
                EntityType type = GetEntityType(value_loc.GetEntityFullType().GetTypeNamed());
                unsigned int CFGType = GetCFGType(type);
                otype = HCDI_get_entitystringtype((int) CFGType).c_str();
            }
            pObj->AddObjectArray(skeyword.c_str(),value_loc.GetIdListCount());
            if(value_loc.GetIdListCount() > 0)
                pObj->AddObjectValues(skeyword.c_str(),0,otype,value_loc.GetIdListCount(),&(value_loc.GetList()[0]));

        }
        else
        {
            sdiBasicType btype = value.GetBasicType();
            switch(vtype)
            {
            case VTYPE_BOOL:
            {
                // TBD
                isOk = false;
            }
            break;
            case VTYPE_INT:
            {
                sdiIntList value_loc;
                if(BASIC_TYPE_INT == btype) value.GetValue(value_loc);
                else                                      GetValueDim1(value, value_loc);
                pObj->AddIntArray(skeyword.c_str(), (int)value_loc.size());
                pObj->AddIntValues(skeyword.c_str(), 0, (int)value_loc.size(), value_loc.data());
            }
            break;
            case VTYPE_UINT:
            {
                sdiUIntList value_loc;
                if(BASIC_TYPE_UINT == btype) value.GetValue(value_loc);
                else                                       GetValueDim1(value, value_loc);
                pObj->AddUIntArray(skeyword.c_str(), (int)value_loc.size());
                pObj->AddUIntValues(skeyword.c_str(), 0, (int)value_loc.size(), value_loc.data());
            }
            break;
            case VTYPE_FLOAT:
            {
                sdiDoubleList value_loc;
                if(BASIC_TYPE_DOUBLE == btype) value.GetValue(value_loc);
                else                                         GetValueDim1(value, value_loc);
                pObj->AddFloatArray(skeyword.c_str(), (int)value_loc.size());
                pObj->AddFloatValues(skeyword.c_str(), 0, (int)value_loc.size(), value_loc.data());
            }
            break;
            case VTYPE_STRING:
            {
                sdiStringList local_val;
                isOk = value.GetValue(local_val);
                size_t strListSize = local_val.size();
                const char** loc_charArray = (const char**)malloc(sizeof(const char*) * (strListSize));
                for (size_t i = 0; i < strListSize; ++i)
                {
                    loc_charArray[i] = local_val[i].c_str();
                }
                pObj->AddStringArray(skeyword.c_str(), (int)strListSize);
                pObj->AddStringValues(skeyword.c_str(), 0, (int)strListSize, (char**)loc_charArray);
                if (loc_charArray) {
                    free(loc_charArray);
                }
            }
            break;
            default: ;
            }
        }
    }
    else // array dimesion > 1 not yet supported
    {
        isOk = false;
    }
    return isOk;
}

template <class ENTITYREAD>
bool ModelViewPO::IsPreObjectParameterized(const IMECPreObject            *pObj,
    const sdiIdentifier& identifier,
    const ENTITYREAD*               pEntityRead,
    const IDescriptor             **ppDescr,
    vector<const IDescriptor*>     *pvpSubDescr) const
{
    sdiValue dummy;
    bool isParameterized = false;
    GetValueFromPreObject(pObj, identifier, dummy,
        pEntityRead, ppDescr, pvpSubDescr, &isParameterized);
    return isParameterized;
}

template <class ENTITYREAD>
sdiString ModelViewPO::GetPreObjectParameterName(const IMECPreObject            *pObj,
    const sdiIdentifier& identifier,
    bool*                           pIsNegated,
    const ENTITYREAD*               pEntityRead,
    const IDescriptor             **ppDescr,
    vector<const IDescriptor*>     *pvpSubDescr) const
{
    sdiValue dummy;
    sdiString parameterName;
    GetValueFromPreObject(pObj, identifier, dummy,
        pEntityRead, ppDescr, pvpSubDescr, nullptr,
        &parameterName, pIsNegated);
    return parameterName;
}

template <class ENTITYREAD>
bool ModelViewPO::SetPreObjectParameter(const IMECPreObject            *pObj,
    const sdiIdentifier& identifier,
    const sdiString&                 parameterName,
    bool                            isNegated,
    const ENTITYREAD*               pEntityRead,
    const IDescriptor             **ppDescr,
    vector<const IDescriptor*>     *pvpSubDescr) const
{
    HandleEdit hParam;
    bool isOk = FindByName(myTypeParameter, parameterName, hParam);
    if(!isOk) return false;
    // this is a bit quick and dirty, not following any convention
    sdiValue value;
    EntityEdit param(this, hParam);
    param.GetValue(sdiIdentifier("value"), value);
    isOk = SetValueToPreObject<ENTITYREAD>(
        const_cast<IMECPreObject*>(pObj), identifier, value, pEntityRead, ppDescr, pvpSubDescr,
        parameterName.c_str(), isNegated);
    if(!isOk)
    {
        // we would need some code here to set values from parameters which do not have the right
        // type, e.g. an int parameter to a double value
        assert(isOk);
    }
    if(isOk)
    {
        param.GetValue(sdiIdentifier("idsmax"), value);
        int idsmax = 0;
        value.GetValue(idsmax);
        param.SetValue(sdiIdentifier("idsmax"), sdiValue(idsmax + 1));
        assert(nullptr != pEntityRead);
        const char* keyword = pObj->GetInputFullType();
        unsigned int type = (unsigned int) pEntityRead->GetType(); // could also use pObj->GetKernelFullType
        param.SetValue(sdiIdentifier("ids", 0, (unsigned int) idsmax),
            sdiValue(sdiValueEntity(
                sdiValueEntityType(type, sdiString(keyword)), pObj->GetId())));
        param.SetValue(sdiIdentifier("datanames", 0, (unsigned int) idsmax),
            sdiValue(identifier.GetNameKey()));
        if(identifier.GetRow() != UINT_MAX)
        {
            param.SetValue(sdiIdentifier("rows", 0, (unsigned int) idsmax),
                sdiValue(identifier.GetRow()));

        }
    }
    return isOk;
}


bool EntityReadPO::GetValue(const sdiIdentifier& identifier,
                            sdiValue&            value) const
{
    assert(p_mv && p_pObj);
    if(!(p_mv && p_pObj)) return false;
    return p_mv->GetValueFromPreObject(p_pObj, identifier, value, this, p_ppDescr);
}


POCompareByFileAndId::POCompareByFileAndId(const ModelViewPO* pModel, unsigned int CFGType)
{
    unsortableKeywords = pModel->GetUnsortableKeywords(CFGType);
}

} // namespace sdi

#endif //! !defined(SDIMODELVIEWPO__INCLUDED_)
