/*Copyright>    OpenRadioss
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.*/




////////////////////////////////////////////////////////////////////////////////////

#if !defined(SDIMODELVIEWPOLSDYNA__INCLUDED_)
#define SDIMODELVIEWPOLSDYNA__INCLUDED_

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <set>

#include <HCDI/hcdi_utils.h>
#include <HCDI/hcdi_mv_descriptor.h>
#include <HCDI/hcdi_multicfgkernelmgr.h>

#include <sdiCFGTypeMapper.h>
#include <sdiModelViewPO.h>

namespace sdi
{

// forward declaration
class SDIModelViewPOLSDyna;

class SDISelectionDataPOLSDynaInclude: public SDISelectionDataPO<SPECIALIZATION_TYPE_GENERAL>
{
public:
    SDISelectionDataPOLSDynaInclude(const sdiString& keyword,
                                    const SDIModelViewPrivate* pModelView,
                                    std::vector<IMECPreObject *> *pre_obj_lst,
                                    const Filter* pFilter):
        SDISelectionDataPO<SPECIALIZATION_TYPE_GENERAL>(
            keyword, pModelView, pre_obj_lst, pFilter, false, 1)
    {}

    //! Advances to next entity, returning false if no such selectable entity
    virtual bool Next()
    {
        // the preobject following the one of a *INCLUDE_TRANSFORM is an internal one
        // and has to be skipped, so we call an additional Next() of the parent class
        if(p_index != UINT_MAX && // not first call
           p_pCurrentEntityData->GetKeyword() == "*INCLUDE_TRANSFORM")
        {
            SDISelectionDataPO<SPECIALIZATION_TYPE_GENERAL>::Next();
        }

        // now the "regular" call of Next() of the parent class
        return SDISelectionDataPO<SPECIALIZATION_TYPE_GENERAL>::Next();
    }

    //! Count of number of possible entities accessible by selection data.
    virtual unsigned int Count() const
    {
        return SDISelectionData::Count(); // default implementation, which uses Next()
    }
};

class SDIModelViewPOLSDyna: public ModelViewPO
{
public:
    SDIModelViewPOLSDyna(std::vector<IMECPreObject*> *preobjects) :
        ModelViewPO(SDICFGTypeMapper(), preobjects, HCDI_OBJ_TYPE_HC_MAX,
                    nullptr, {"*DEFINE_CURVE"})
    {
    }

    virtual ~SDIModelViewPOLSDyna()
    {
    }

    virtual SDISelectionData* CreateSelectionData(const sdiString& keyword,
                                                  const Filter*   pselectionFilter = nullptr) const
    {
        EntityType type = GetEntityType(keyword);
        unsigned int CFGType = GetCFGType(type);
        if(HCDI_OBJ_TYPE_INCLUDEFILES == CFGType)
        {
            return new SDISelectionDataPOLSDynaInclude(
                keyword, this, &(p_preobjects[CFGType]), pselectionFilter);
        }
        else
        {
            return ModelViewPO::CreateSelectionData(keyword, pselectionFilter);
        }
    }

    virtual bool P_FindById(const unsigned int id, EntityType type, HandleRead& handle) const
    {
        if(myTypeInclude == type || HCDI_OBJ_TYPE_INCLUDEFILES == type)
        {
            if(p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES].size() > id)
            {
                handle.SetPointer(p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES][id]);
                if(id > 1 &&
                   p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES][id-1] != nullptr &&
                   strcmp(p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES][id-1]->GetInputFullType(),
                          "*INCLUDE_TRANSFORM") == 0)
                {   // the preobject following the one of a *INCLUDE_TRANSFORM is an internal one,
                    // so in this case we need the preceding one
                    handle.SetPointer(p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES][id-1]);
                }
                return true;
            }
            else return false;
        }
        else
        {
            return ModelViewPO::P_FindById(id, type, handle);
        }
    }

    virtual unsigned int P_GetId(const HandleRead& handle) const
    {
        EntityType type = handle.GetType();
        if(myTypeInclude == type || HCDI_OBJ_TYPE_INCLUDEFILES == type)
        {
            auto it = std::find(p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES].begin(),
                                p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES].end(),
                                handle.GetPointer());
            if(it != p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES].end())
            {
                unsigned int id = (unsigned int) (it - p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES].begin());
                if(id > 0 &&
                   id < p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES].size() - 1 &&
                   p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES][id] != nullptr &&
                   strcmp(p_preobjects[HCDI_OBJ_TYPE_INCLUDEFILES][id]->GetInputFullType(),
                          "*INCLUDE_TRANSFORM") == 0)
                {   // preobjects of *INCLUDE_TRANSFORM are followed by an internal one, which is
                    // the owner of contained entities, so we need the "id" of that one
                    return id + 1;
                }
                return id;
            }
            return 0;
        }
        else
        {
            return ModelViewPO::P_GetId(handle);
        }
    }
};


} // namespace sdi


#endif // SDIMODELVIEWPOLSDYNA__INCLUDED_