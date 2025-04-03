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

#if !defined(SDICFGTYPEMAPPER__INCLUDED_)
#define SDICFGTYPEMAPPER__INCLUDED_

#include <sdi/tools/sdiTypeMapper.h>

#include <HCDI/hcdi_multicfgkernelmgr.h>
#include <KERNEL_BASE/Structure_types.h>
#include <KERNEL/mv_pre_datas_hierarchy.h>
#include <KERNEL/mv_type.h>
#include <cfgio/MODEL_IO/meci_syntax_infos.h>

namespace sdi
{

/// Class which describes the mapping between SDI and CFG types
class SDICFGTypeMapper : public SDITypeMapper
{

public:
    // Constructors and destructor
    SDICFGTypeMapper(const sdiString& keywordprefix = "", bool useCurrentCFGKernel = true) :
        SDITypeMapper(keywordprefix)
    {
        if(useCurrentCFGKernel)
        {
            if(keywordprefix.empty())
            {
                p_keywordprefix = SolverSyntaxInfos().getHeader();
            }
            Init(MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel());
        }
        else
        {
            InitDBTyes();
        }
    }

    SDICFGTypeMapper(const CFGKernel* cfgkernel,
                     const sdiString& keywordprefix = "",
                     char keywordseparator = '_') :
        SDITypeMapper(keywordprefix, keywordseparator)
    {
        Init(cfgkernel);
    }

    SDICFGTypeMapper(const SDICFGTypeMapper& other) :
        SDITypeMapper(other)
    {}

    // improvement TBD: move constructor

private:
    SDICFGTypeMapper& operator= (const SDICFGTypeMapper& other); // hidden until needed...

public:
    virtual ~SDICFGTypeMapper() {}

protected:
    void Init(const CFGKernel* cfgkernel)
    {
        if(nullptr != cfgkernel)
        {
            const MvPreDatasHierarchy_t* a_data_cfg_p = cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);
            if(NULL == a_data_cfg_p) return;
            AddKeywordsRecursively(a_data_cfg_p,HCDI_OBJ_TYPE_NULL);
        }

        Init();
    }

    void InitDBTyes()
    {
        p_elemDBtype = HCDI_OBJ_TYPE_ELEMS;
        p_subobjectDBtype = HCDI_OBJ_TYPE_SUBOBJECT;
        p_maxDBtype = HCDI_OBJ_TYPE_HC_MAX;
    }

    void Init()
    {
        InitDBTyes();

        SDITypeMapper::Init();
    }

    virtual void SortTmpKeywordList()
    {
        std::stable_sort(p_tmpkeywordlist.begin(), p_tmpkeywordlist.end(), [](const auto& lhs, const auto& rhs)
                         {
                             if(lhs.myDBtype == HCDI_OBJ_TYPE_CARDS && rhs.myDBtype == HCDI_OBJ_TYPE_CARDS)
                             { // control cards have to keep their order, to distinguish Starter and Engine
                                 return false;
                             }
                             return lhs.mykeyword < rhs.mykeyword;
                         });
    }

    virtual const char* GetDBTypeName(unsigned int DBType)
    {
        return MV_get_type((object_type_e) DBType).c_str();
    }


private: // used by constructor from CFGKernel
    void AddKeywordsRecursively(const MvPreDatasHierarchy_t* data_cfg_p,
                                object_type_e type)
    {
        if(nullptr == data_cfg_p) return; // shouldn't happen

        // first look for USER_NAMES (if not last level)...
        if(nullptr != data_cfg_p->getSubtypePtr() && data_cfg_p->getChildList().size() > 0)
        /*
        // first look for USER_NAMES of SUBOBJECT (i.e. adhesive options)...
        if(nullptr != data_cfg_p->getSubtypePtr() && data_cfg_p->getChildList().size() > 0 &&
           HCDI_OBJ_TYPE_SUBOBJECT == type)
        */
        {
            const MvStringList_t& list = data_cfg_p->getSubtypePtr()->getUserNameList();
            if(list.size() > 0)
            {
                int idpool = (int) data_cfg_p->getSubtypePtr()->getIdPool();
                // reverse loop, because we expect the first USER_NAME to be the default one,
                // and that one is needed here as last one
                for(int i = (int)list.size() - 1; i >= 0; --i)
                {
                    auto username = list[i];
                    p_tmpkeywordlist.push_back(
                        myKeywordInfo(username, (unsigned int) type, idpool));
                }
                return;
            }
        }

        // special case: for SUBOBJECT, only USER_NAMES at first level is supported
        if(HCDI_OBJ_TYPE_SUBOBJECT == type)
        {
            return;
        }

        // then get the first USER_NAMES in the last level
        if(data_cfg_p->getChildList().size() == 0 &&
           !data_cfg_p->getKeyword().empty() &&
           data_cfg_p->getKeyword() != "NO_KEYWORD")
        {
            auto pSubtype = data_cfg_p->getSubtypePtr();
            if(nullptr != pSubtype)
            {
                const MvStringList_t& list = pSubtype->getUserNameList();
                if(list.size() > 0)
                {
                    int idpool = (int)pSubtype->getIdPool();
                    auto username = list[0];
                    int configType = pSubtype->getHMConfigType();
                    int subType = pSubtype->getHMType();
                    p_tmpkeywordlist.push_back(
                        myKeywordInfo(username, (unsigned int)type, idpool, configType, subType));
                    return;
                }
            }
        }

        /*
        // ... then look for KEYWORD (in the last level)...
        if(data_cfg_p->getChildList().size() == 0 &&
           !data_cfg_p->getKeyword().empty() &&
           data_cfg_p->getKeyword() != "NO_KEYWORD")
        {
            string keyword = data_cfg_p->getKeyword();
            if(keyword.find("_IDPOOL") < keyword.size()) keyword.erase(keyword.find("_IDPOOL"), 7);
            int idpool = 0;
            if(nullptr != data_cfg_p->getSubtypePtr())
            {
                idpool = (int) data_cfg_p->getSubtypePtr()->getIdPool();
            }
            p_tmpkeywordlist.push_back(myKeywordInfo(keyword, (unsigned int) type, idpool));
            return;
        }
        */

        // ... then go down the hierarchy.
        MvPreDatasHierarchyList_t::const_iterator it_begin = data_cfg_p->getChildList().begin();
        MvPreDatasHierarchyList_t::const_iterator it_end = data_cfg_p->getChildList().end();
        MvPreDatasHierarchyList_t::const_iterator it;
        for(it = it_begin; it != it_end; ++it)
        {
            AddKeywordsRecursively(*it, (*it)->getType());
        }
    }

public:
    virtual unsigned int GetCFGType(EntityType type) const
    {
        return GetDBType(type);
    }

};

} // namespace sdi

#endif //! !defined(SDICFGTYPEMAPPER__INCLUDED_)
