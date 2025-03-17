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

#include <UTILS/win32_utils.h>  

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <ctime>
#include <map>
#include <vector>
#include <stdio.h>
#include <cmath>

#include <UTILS/mv_stl_various.h>
#include <UTILS/error.h>
#include <UTILS/set_utils.h>
#include <UTILS/memory_utils.h>
#include <UTILS/str_utils.h>

#include <UTILS/mv_cstring.h>
#include <KERNEL/Structure_fileformat_others.h>
#include <UTILS/file_utils.h>
#include <KERNEL/mv_descriptor.h>
#include <KERNEL/mv_kernel.h>
#include <KERNEL/mv_data_cfg.h>
#include <KERNEL/mv_data_cfg_parser.h>
#include <KERNEL/mv_pre_descriptor.h>
#include <KERNEL/mv_data_data_feature.h>
#include <KERNEL_BASE/keyword_map.h>

#include <KERNEL/mv_descriptor.h>
#include <KERNEL/mv_kernel.h>
#include <KERNEL/mv_data_cfg.h>
#include <KERNEL/mv_data_cfg_parser.h>
#include <KERNEL/mv_pre_descriptor.h>
#include <KERNEL/mv_data_data_feature.h>
#include "hcdi_multicfgkernelmgr.h"
#include "hcdi_mv_descriptor.h"
#include "hcdi_utils.h"

#include <UTILS/file_utils.h>
#include <UTILS/str_utils.h>
#include <algorithm>
#include<functional>
#include <regex>

#ifdef _WIN32
#include <windows.h>
#include <Psapi.h>
#include <direct.h>
#include <process.h>
#undef GetMessage
#define mkdir(dir, mode) _mkdir(dir)
#else
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#endif
#include <cstdio>
#include <ctime>

class CFGKernel;


static MvPreDatasHierarchy_t* loc_get_pdes_ptr(int option, string filename = "");

static void HCDIGetDiscreteListFromType(unsigned int etype, bool isuser, map < string, vector< string> >& mapDiscreteList);
static void HCDI_GetSolverFolderHierachy(map<ApplicationMode_e, string>& appfolderhierachy);
void loc_recursive_get_type_has_subtype_based_on_htype(int etype, const string& htype, const MvPreDatasHierarchy_t* data, leafmapmap_t& list, bool& do_for_first_time,
    leaf_t& a_sub_new_member, bool add_all_child);

void loc_recursive_get_type_has_subtype_based_on_htype_sub_htype_sub_htype_title(int etype,
    const string& htype, string& title_htype, string& sub_htype, string& sub_htype_title, string& sub_htype_child,
    const MvPreDatasHierarchy_t* data, bool& do_for_first_time,
    leaf_t& a_sub_new_member,
    child_t& result_vector, bool enable_do_firsttime = true);

static void loc_recursive_add_configs(int etype, vector< std::pair<unsigned int, string> >& aListConfig, const MvPreDatasHierarchy_t* data, bool withTitle = false);

static MvPreDatasHierarchy_t* loc_get_pdes_ptr(int option, string filename) {
    static MvPreDatasHierarchy_t* a_pdh_child_p = nullptr;
    //
    switch (option) {
    case 1:  // Creation
        if (a_pdh_child_p != NULL) delete a_pdh_child_p;
        if (filename != "")
        {
            MultiCFGKernelMgr& cfg_kernelmgr = MultiCFGKernelMgr::getInstance();
            CFGKernel* a_cfgkernel = const_cast<CFGKernel*>(cfg_kernelmgr.GetCurrentCFGKernel());
            if (!a_cfgkernel)
                return nullptr;
            a_pdh_child_p = MvDataCfgParser_t(filename).getDatasHierarchyPtr(*a_cfgkernel, "ROOT");
            return a_pdh_child_p;
        }
        break;
    case 0:
    {
        if (a_pdh_child_p != NULL)  return a_pdh_child_p;
        break;
    }
    case -1: // Destruction
        if (a_pdh_child_p != NULL) {
            delete a_pdh_child_p;
            a_pdh_child_p = NULL;
        }
        break;
    default:
        break;
    }
    //
    return a_pdh_child_p;
}

void HCDI_Close_UserDefined_Kernel()
{
    loc_get_pdes_ptr(-1);
}

void HCDI_Close_Kernel()
{
    MultiCFGKernelMgr::getInstance().DeleteCFGKernel();
    MU_close_dimensions();
    MV_delete_type_map();
}

HC_DATA_DLL_API IDESCRIPTORHANDLE HCDI_GetDescriptorHandle(const char* fulltype)
{
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel || !fulltype)
        return nullptr;

    return a_cfgkernel->GetDescriptorHandle(fulltype);
}

HC_DATA_DLL_API IDESCRIPTORHANDLE HCDI_GetDescriptorHandleFromKeyword(int type, const string& keyword)
{
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return nullptr;
    return a_cfgkernel->GetDescriptorHandleFromKeyword((object_type_e)type, keyword);
}
HC_DATA_DLL_API IDESCRIPTORHANDLE HCDI_GetDescriptorHandleUserKeywordFromKeyword(int type, int hm_config_type, int hm_type, const string& keyword, string& first_user_keyword)
{
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return nullptr;
    return a_cfgkernel->GetDescriptorHandleUserKeywordFromKeyword((object_type_e)type, hm_config_type, hm_type, keyword, first_user_keyword);
}

HC_DATA_DLL_API void HCDI_GetUserKeywordFromConfigHmType(int type, int config_type, int hm_type, string& first_user_keyword)
{
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;
    a_cfgkernel->GetUserKeywordFromConfigHmType((object_type_e)type, config_type, hm_type, first_user_keyword);
}

HC_DATA_DLL_API IDESCRIPTORHANDLE HCDI_GetDescriptorHandleFromUserID(int type, int user_id)
{
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return nullptr;
    return a_cfgkernel->GetDescriptorHandleFromUserID((object_type_e)type, user_id);
}

HC_DATA_DLL_API IDESCRIPTORHANDLE HCDI_GetDescriptorHandleFromHMConfigHMType(int type, int hm_config_type, int hm_type)
{
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return nullptr;
    return a_cfgkernel->GetDescriptorHandleFromHMConfigHMType((object_type_e)type, hm_config_type, hm_type);
}

HC_DATA_DLL_API IDESCRIPTORHANDLE HCDI_GetDescriptorHandleFromType(int type)
{
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return nullptr;
    return a_cfgkernel->GetDescriptorHandleFromType((object_type_e)type);
}
HC_DATA_DLL_API IDESCRIPTORHANDLE HCDI_GetDescriptorHandleFromFullType(const MvFullType_t& fulltype)
{
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return nullptr;
    return a_cfgkernel->GetDescriptorHandleFromFullType(fulltype);
}


HC_DATA_DLL_API object_type_e HCDI_get_entitytype(const string& keyword)
{
    return MV_get_type(keyword);
}
HC_DATA_DLL_API const string& HCDI_get_entitystringtype(int entity_type)
{
    return MV_get_type((object_type_e)entity_type);
}

HC_DATA_DLL_API int HCDI_get_all_domains()
{
    return MV_get_all_domains();
}

HC_DATA_DLL_API dir_type_e HCDI_MV_get_direction(const char* dir, int is_extended)
{
    return MV_get_direction(dir, is_extended);
}

HC_DATA_DLL_API const char* HCDI_MV_get_direction_str(dir_type_e dir, int is_extended)
{
    return MV_get_direction_str(dir, is_extended);
}
HC_DATA_DLL_API int  HCDI_MV_get_directions(const char* dir, int* xdir_p, int* ydir_p, int* zdir_p)
{
    return MV_get_directions(dir, xdir_p, ydir_p, zdir_p);
}

/** Gets a string from direction flags */
HC_DATA_DLL_API const char* HCDI_MV_get_directions_str(int xdir, int ydir, int zdir)
{
    return MV_get_directions_str(xdir, ydir, zdir);
}

void loc_recursive_add_configs(int etype, vector< std::pair<unsigned int,   string> > &aListConfig, const MvPreDatasHierarchy_t *data, bool withTitle)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        int nb_child = (*it)->getNbChildren();
        string skey = (*it)->getTitle();
        obj_type_e a_type = (*it)->getType();

        if (a_type == etype)
        {
            if (nb_child > 0)
            {
                loc_recursive_add_configs(etype, aListConfig, *it, withTitle);
            }
            else
            {
                string key;
                if(withTitle)
                    key = (*it)->getTitle();
                else
                    key = (*it)->getKeyword();

                const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
                if (!psubtype)
                    continue;
                int config_type = psubtype->getHMConfigType();

                if (config_type > 0)
                {
                    if (aListConfig.size() == 0)
                    {
                        aListConfig.push_back(std::make_pair(1, "Undefined"));
                    }
                    aListConfig.push_back(std::make_pair(config_type, key));
                }
            }
        }
    }
}

HC_DATA_DLL_API void HCDIgetAllKeywordConfigTypes(int etype, vector< std::pair<unsigned int,   string> > &aListConfig)
{
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;

    const MvPreDatasHierarchy_t* a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;

    loc_recursive_add_configs(etype, aListConfig, a_data_cfg_p);
}

HC_DATA_DLL_API void HCDIgetAllConfigTypes(int etype, vector< std::pair<unsigned int, string> >& aListConfig, bool withTitle)
{
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;

    const MvPreDatasHierarchy_t* a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;

    loc_recursive_add_configs(etype, aListConfig, a_data_cfg_p, withTitle);
}



void loc_recursive_add_config_type_descriptor(int etype, vector< std::pair<string,  const IDescriptor *> > &aListConfigIDescriptor, const MvPreDatasHierarchy_t *data)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        int nb_child = (*it)->getNbChildren();
        string skey = (*it)->getTitle();
        obj_type_e a_type = (*it)->getType();

        if (a_type == etype)
        {
            if (nb_child > 0)
            {
                loc_recursive_add_config_type_descriptor(etype, aListConfigIDescriptor, *it);
            }
            else
            {
                const MvFullType_t full_type = (*it)->getFullType();
                const IDescriptor* pdesp = HCDI_GetDescriptorHandleFromFullType(full_type);

                if (pdesp)
                {
                    string key = (*it)->getKeyword();
                    aListConfigIDescriptor.push_back(std::make_pair(key, pdesp));
                }
            }
        }
    }
}

HC_DATA_DLL_API void HCDIGetEntityDescriptors(int etype, vector< std::pair<string, const IDescriptor*> >& aListConfigIDescriptor)
{
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;
    const MvPreDatasHierarchy_t* a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;

    loc_recursive_add_config_type_descriptor(etype, aListConfigIDescriptor, a_data_cfg_p);
}

HC_DATA_DLL_API void HCDIGetAllTypesTitle(vector< std::pair<obj_type_e, string> >& vec_type_title)
{
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;

    const MvPreDatasHierarchy_t* a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;

    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        int nb_child = (*it)->getNbChildren();
        string skey = (*it)->getTitle();
        obj_type_e a_type = (*it)->getType();
        vec_type_title.push_back(std::make_pair(a_type, skey));
    }
}

HC_DATA_DLL_API void HCDIGetUserDiscreteListFromType(unsigned int etype, map < string, vector< string> >& mapDiscreteList)
{
    HCDIGetDiscreteListFromType(etype, true, mapDiscreteList);
}
HC_DATA_DLL_API void HCDIGetInternalDiscreteListFromType(unsigned int etype, map < string, vector< string> >& mapDiscreteList)
{
    HCDIGetDiscreteListFromType(etype, false, mapDiscreteList);
}

void HCDIGetDiscreteListFromType(unsigned int etype, bool isuser, map < string, vector< string> >& mapDiscreteList)
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = NULL;
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;

    if (isuser)
        a_data_cfg_p = a_cfgkernel->get_userdefined_hierarchy(0);
    else
        a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);

    if (NULL == a_data_cfg_p)
        return;
    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        string skey = (*it)->getTitle();
        obj_type_e a_type = (*it)->getType();

        if (a_type == etype)
        {
            MvPreDatasHierarchyList_t::const_iterator itc_begin = (*it)->getChildList().begin();
            MvPreDatasHierarchyList_t::const_iterator itc_end = (*it)->getChildList().end();
            MvPreDatasHierarchyList_t::const_iterator itc;

            for (itc = itc_begin; itc != itc_end; ++itc)
            {
                vector <string> usr_name_lst;
                string a_title = (*itc)->getTitle();
                string a_keyword = (*itc)->getKeyword();
                if (a_keyword != "NO_KEYWORD" && a_keyword != "")
                {
                    usr_name_lst.push_back(a_keyword);
                }
                else
                {
                    MvPreDatasHierarchyList_t::const_iterator itc_n_begin = (*itc)->getChildList().begin();
                    MvPreDatasHierarchyList_t::const_iterator itc_n_end = (*itc)->getChildList().end();
                    MvPreDatasHierarchyList_t::const_iterator itc_n;
                    for (itc_n = itc_n_begin; itc_n != itc_n_end; ++itc_n)
                    {
                        a_keyword = (*itc_n)->getKeyword();
                        if (a_keyword != "NO_KEYWORD" && a_keyword != "")
                        {
                            usr_name_lst.push_back(a_keyword);
                        }
                    }
                }
                auto a_it = mapDiscreteList.find(a_title);
                if (a_it != mapDiscreteList.end())
                {
                    vector<string>& a_set = a_it->second;
                    a_set.insert(std::end(a_set), std::begin(usr_name_lst), std::end(usr_name_lst));
                }
                else
                    mapDiscreteList[a_title] = usr_name_lst;
            }
        }
    }
}

HC_DATA_DLL_API void HCDIGetConfigHMTypeCardImageFromKeyword(unsigned int etype, string& keyword, int* configType, int* hmType, string& cardImage)
{
    MvSubtypePtrSet_t a_subtypes;
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;
    a_cfgkernel->get_subtypes((obj_type_e)etype, &a_subtypes);
    MvSubtypePtrSet_t::iterator a_it_begin = a_subtypes.begin();
    MvSubtypePtrSet_t::iterator a_it_end = a_subtypes.end();
    MvSubtypePtrSet_t::iterator a_it;
    for (a_it = a_it_begin; a_it != a_it_end; ++a_it) {

        const MvSubtype_t* a_subtype = *a_it;
        MvKeywordSet_t a_keyword_set;
        a_subtype->getKeywords((obj_type_e)etype, &a_keyword_set);
        MvKeywordSet_t::iterator a_key_it = a_keyword_set.find(keyword);
        if (a_key_it != a_keyword_set.end())
        {
            char* card_image = a_subtype->getCardImage();
            if (card_image)
                cardImage = card_image;
            *configType = a_subtype->getHMConfigType();
            *hmType = a_subtype->getHMType();
            break;
        }
    }
}

void loc_recursive_find_parent(int etype, bool isuser, const MvPreDatasHierarchy_t* data, string& keyword, vector< string>& parent, bool* if_found)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        int nb_child = (*it)->getNbChildren();
        string skey = (*it)->getTitle();
        obj_type_e a_type = (*it)->getType();

        if (a_type == etype)
        {
            if (nb_child > 0)
            {
                loc_recursive_find_parent(etype, isuser, *it, keyword, parent, if_found);
                if (*if_found)
                {
                    parent.push_back(skey);
                    *if_found = false; /*to continue searching*/
                }
            }
            else
            {
                string a_kew = (*it)->getKeyword();
                std::regex regeBoundry("([\\b\\_\\/])" + a_kew + "([\\b\\_\\/])");
                std::regex regeEnd("([\\b\\_\\/])" + a_kew + "$");
                std::smatch match;
                if (keyword == a_kew || std::regex_search(keyword, match, regeBoundry) || std::regex_search(keyword, match, regeEnd))
                {
                    *if_found = true;
                }
            }
        }
    }
}

HC_DATA_DLL_API void HCDIfindKeywordParent(int etype, bool isuser, string& keyword, vector< string>& parent)
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = NULL;
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;

    if (isuser)
        a_data_cfg_p = a_cfgkernel->get_userdefined_hierarchy(0);
    else
        a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);

    if (NULL == a_data_cfg_p)
        return;

    bool if_found = false;
    loc_recursive_find_parent(etype, isuser, a_data_cfg_p, keyword, parent, &if_found);
}



/*
HC_DATA_DLL_API const obj_type_e HCDIgetHCTypeFromIdentifer(string &hm_type_str, string &cardimage)
{

    const MvPreDatasHierarchy_t *a_data_cfg_p =MV_get_datashierarchy();
    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end   = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;



    for (it = it_begin; it != it_end; ++it)
    {
        string skey = (*it)->getTitle();

        if(skey==hm_type_str)
        {
            MvPreDatasHierarchyList_t::const_iterator itc_begin_prev;
            MvPreDatasHierarchyList_t::const_iterator it_cur = it_begin;
            while((*it_cur) && (*it_cur)->getNbChildren() > 0)
            {
                itc_begin_prev = (*it_cur)->getChildList().begin();
                it_cur++;
            }

            MvPreDatasHierarchyList_t::const_iterator itc_begin = (*itc_begin_prev)->getChildList().begin();
            MvPreDatasHierarchyList_t::const_iterator itc_end   = (*itc_begin_prev)->getChildList().end();
            MvPreDatasHierarchyList_t::const_iterator itc;

            for (itc = itc_begin; itc != itc_end; ++itc)
            {
                string skey = (*itc)->getTitle();
                if(skey==cardimage)
                {
                    return (*itc)->getType();
                }
            }
        }
    }
    return HCDI_OBJ_TYPE_NULL;
}
*/

void loc_recursive_get_idpool(int etype, int hm_config_type, int hm_type, string& cardimage, int* id_pool, const MvPreDatasHierarchy_t* data, bool* if_found)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        if (*if_found)
        {
            break;
        }
        int nb_child = (*it)->getNbChildren();
        string skey = (*it)->getKeyword();
        obj_type_e a_type = (*it)->getType();

        if (a_type == etype)
        {
            if (skey == cardimage)
            {
                if (nb_child)
                {
                    MvPreDatasHierarchyList_t::const_iterator child_it_begin = (*it)->getChildList().begin();
                    const MvSubtype_t* psubtype = (*child_it_begin)->getSubtypePtr();
                    if (psubtype)
                    {
                        *id_pool = psubtype->getIdPool();
                        *if_found = true;
                        break;
                    }
                }
            }

            if (nb_child > 0)
            {
                loc_recursive_get_idpool(etype, hm_config_type, hm_type, cardimage, id_pool, *it, if_found);
            }
            else
            {
                string key = (*it)->getKeyword();

                if (hm_type > 0 && hm_config_type > 0)
                {
                    const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
                    int a_hm_config_type = psubtype->getHMConfigType();
                    int a_hm_type = psubtype->getHMType();
                    if ((hm_type == a_hm_type) && (a_hm_config_type == hm_config_type))
                    {
                        *id_pool = psubtype->getIdPool();
                        *if_found = true;
                        break;
                    }
                }
                else if (hm_config_type > 0 && hm_type < 0)
                {
                    const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
                    *id_pool = psubtype->getIdPool();
                    *if_found = true;
                    break;
                }
                else
                {
                    const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
                    if (psubtype)
                    {
                        const MvStringList_t& list = psubtype->getUserNameList();
                        int size = (int)list.size();
                        bool found_in_list = false;
                        for (int i = 0; i < size; i++)
                        {
                            string elem = list.at(i);
                            if (cardimage == elem)
                            {
                                found_in_list = true;
                                break;
                            }
                        }

                        if (found_in_list)
                        {
                            *id_pool = psubtype->getIdPool();
                            *if_found = true;
                            break;
                        }
                    }
                }
            }
        }
    }
}


void loc_recursive_get_default_config(int etype, const MvPreDatasHierarchy_t* data, int id_pool, unsigned int &config)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        int nb_child = (*it)->getNbChildren();
        string skey = (*it)->getKeyword();
        obj_type_e a_type = (*it)->getType();
        if (nb_child > 0)
        {
            loc_recursive_get_default_config(etype, *it, id_pool, config);
        }
        else
        {
            const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
            if (psubtype)
            {
                int a_id_pool = psubtype->getIdPool();
                if (!id_pool || id_pool == a_id_pool)
                {
                    config = psubtype->getHMConfigType();
                    break;
                }
            }
        }
    }
}

HC_DATA_DLL_API void HCDIgetIdPool(int etype, int hm_config_type, int hm_type, string& cardimage, int* id_pool)
{
    MultiCFGKernelMgr& descrp_model = MultiCFGKernelMgr::getInstance();
    const CFGKernel* a_cfgkernel = descrp_model.GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;

    const MvPreDatasHierarchy_t* a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;

    bool if_found = false;
    loc_recursive_get_idpool(etype, hm_config_type, hm_type, cardimage, id_pool, a_data_cfg_p, &if_found);
    if (*id_pool == -1)
    {
        *id_pool = 0;
    }
}

HC_DATA_DLL_API unsigned int  HCDIGetDefaultConfigType(int etype, int id_pool)
{
    MultiCFGKernelMgr& descrp_model = MultiCFGKernelMgr::getInstance();
    const CFGKernel* a_cfgkernel = descrp_model.GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return 0;

    const MvPreDatasHierarchy_t* a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return 0;

    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        int nb_child = (*it)->getNbChildren();
        string skey = (*it)->getKeyword();
        obj_type_e a_type = (*it)->getType();

        if (a_type == etype)
        {
            unsigned int config = 0;
            loc_recursive_get_default_config(etype, *it, id_pool, config);
            return config;
        }
    }
    return 0;
}

void loc_recursive_get_keyword_list(int etype, string& query_string, string& cardimage, vector<string>& keyword_list, const MvPreDatasHierarchy_t* data, bool* if_found)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        if (*if_found)
        {
            break;
        }
        int nb_child = (*it)->getNbChildren();
        string skey = "";
        skey = (*it)->getKeyword();

        obj_type_e a_type = (*it)->getType();

        if (a_type == etype)
        {
            if (skey == cardimage)
            {
                if (nb_child)
                {
                    MvPreDatasHierarchyList_t::const_iterator child_it_begin = (*it)->getChildList().begin();
                    MvPreDatasHierarchyList_t::const_iterator child_it_end = (*it)->getChildList().end();
                    MvPreDatasHierarchyList_t::const_iterator child_it;
                    for (child_it = child_it_begin; child_it != child_it_end; ++child_it)
                    {
                        const MvSubtype_t* psubtype = (*child_it)->getSubtypePtr();
                        if (psubtype)
                        {
                            string child_key = "";
                            if (query_string == "KEYWORD")
                            {
                                child_key = psubtype->getKeyword();
                            }
                            else if (query_string == "CARDIMAGE")
                            {
                                char* cimage = psubtype->getCardImage();
                                child_key = (cimage == NULL) ? "" : cimage;
                                if (child_key == "")
                                {
                                    const MvStringList_t& user_names = psubtype->getUserNameList();
                                    int size = (int)user_names.size();
                                    if (size > 0)
                                        child_key = user_names.at((size_t)size - 1);
                                }
                            }

                            keyword_list.push_back(child_key);
                        }
                    }
                    *if_found = true;
                    break;
                }
            }

            if (nb_child > 0)
            {
                loc_recursive_get_keyword_list(etype, query_string, cardimage, keyword_list, *it, if_found);
            }
        }
    }
}

HC_DATA_DLL_API void HCDIgetChildKeywordListForGivenKeyword(int etype, string& keyword, vector<string>& keyword_list)
{
    MultiCFGKernelMgr& descrp_model = MultiCFGKernelMgr::getInstance();
    const CFGKernel* a_cfgkernel = descrp_model.GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;
    const MvPreDatasHierarchy_t* a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;

    bool if_found = false;
    string query_str = "KEYWORD";
    loc_recursive_get_keyword_list(etype, query_str, keyword, keyword_list, a_data_cfg_p, &if_found);
}

/* make sure etype has cardimage*/
HC_DATA_DLL_API void HCDIgetChildCardImageListForGivenKeyword(int etype, string& card_image, vector<string>& card_image_list)
{
    MultiCFGKernelMgr& descrp_model = MultiCFGKernelMgr::getInstance();
    const CFGKernel* a_cfgkernel = descrp_model.GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;

    const MvPreDatasHierarchy_t* a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;

    bool if_found = false;
    string query_str = "CARDIMAGE";
    loc_recursive_get_keyword_list(etype, query_str, card_image, card_image_list, a_data_cfg_p, &if_found);
}

HC_DATA_DLL_API void HCDI_StringTokenize(const string& str, vector<string>& pTokens, string delem)
{
    StringTokenize(str, pTokens, delem);
}

void loc_recursive_get_ikeyword_type_map(const MvPreDatasHierarchy_t* data_cfg_p, obj_type_e type, map<string, obj_type_e>& keyword_type_map)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        string skey = (*it)->getKeyword();

        const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
        if (psubtype)
        {
            const MvStringList_t& list = psubtype->getUserNameList();
            int size = (int)list.size();
            for (int i = 0; i < size; i++)
            {
                string username = list.at(i);
                keyword_type_map[username] = type;
            }
        }
        loc_recursive_get_ikeyword_type_map(*it, type, keyword_type_map);
    }
}

HC_DATA_DLL_API void HCDIgetKeywordTypeMap(map<string, obj_type_e> &keyword_type_map, bool add_first_parent)
{
    MultiCFGKernelMgr& descrp_model = MultiCFGKernelMgr::getInstance();
    const CFGKernel* a_cfgkernel = descrp_model.GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;

    const MvPreDatasHierarchy_t* a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;
    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        string skey = (*it)->getKeyword();
        obj_type_e a_type = (*it)->getType();

        if (add_first_parent)
        {
            keyword_type_map[skey] = a_type;
        }
        loc_recursive_get_ikeyword_type_map(*it, a_type, keyword_type_map);
    }
}

void loc_recursive_get_type_has_subtype(int etype, const MvPreDatasHierarchy_t* data, bool* if_found)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        if (*if_found)
        {
            break;
        }
        obj_type_e a_type = (*it)->getType();

        if (a_type == etype)
        {
            int nb_child = (int)(*it)->getChildList().size();
            if (nb_child)
            {
                *if_found = true;
            }
        }
    }
}

HC_DATA_DLL_API bool HCDIgetTypeHasSubtype(int etype)
{
    MultiCFGKernelMgr& descrp_model = MultiCFGKernelMgr::getInstance();
    const CFGKernel* a_cfgkernel = descrp_model.GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return false;
    const MvPreDatasHierarchy_t* a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return false;

    bool if_found = false;
    loc_recursive_get_type_has_subtype(etype, a_data_cfg_p, &if_found);
    return if_found;
}


HC_DATA_DLL_API bool HCDIGetFirstUserNameForGvnEtypeUserID(const string& etype, int userid, string& firstusername)
{
    MultiCFGKernelMgr& descrp_model = MultiCFGKernelMgr::getInstance();
    const CFGKernel* a_cfgkernel = descrp_model.GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return false;
    const MvSubtype_t* subtype_p = a_cfgkernel->get_user_subtype(etype, userid);

    if (subtype_p)
    {
        const MvStringList_t& usr_lst = subtype_p->getUserNameList();
        if (usr_lst.size())
        {
            firstusername = *(usr_lst.begin());
            return true;
        }
    }

    return false;
}

HC_DATA_DLL_API bool HCDIGetFirstUserNameForGvnEtypeConfigHMType(const string& etype, int config, int hmtype, string& firstusername)
{
    MultiCFGKernelMgr& descrp_model = MultiCFGKernelMgr::getInstance();
    const CFGKernel* a_cfgkernel = descrp_model.GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return false;
    const MvSubtype_t* subtype_p = a_cfgkernel->get_config_type_subtype(etype, config, hmtype);

    if (subtype_p)
    {
        const MvStringList_t& usr_lst = subtype_p->getUserNameList();
        if (usr_lst.size())
        {
            firstusername = *(usr_lst.begin());
            return true;
        }
    }
    return false;
}

HC_DATA_DLL_API bool HCDIGetFirstUserNameForGvnEtypeKeyword(int etype, string& cardimage, string& firstusername)
{
    MultiCFGKernelMgr& descrp_model = MultiCFGKernelMgr::getInstance();
    const CFGKernel* a_cfgkernel = descrp_model.GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return false;

    const MvSubtype_t* subtype_p = a_cfgkernel->get_subtype((object_type_e)etype, cardimage);

    if (subtype_p)
    {
        const MvStringList_t& usr_lst = subtype_p->getUserNameList();
        if (usr_lst.size())
        {
            firstusername = *(usr_lst.begin());
            return true;
        }
    }

    return false;
}

/*Responsibility of the user to deallocate the memory*/
HC_DATA_DLL_API MvDataUncondScalarFeature_t* HCDI_GetAllocatedUnCondScalarFeatureHandle(const string& title, int  ikeyword, MvDimension_e dimension, vector<string>& argVect)
{

    MvDataUncondScalarFeature_t* a_feature_p = NULL;
    a_feature_p = new MvDataUncondScalarFeature_t(title, ikeyword, dimension, &argVect);
    return a_feature_p;
}

void loc_check_recursive_id_pool(int etype, const MvPreDatasHierarchy_t* data, bool* has_id_pool)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        obj_type_e a_type = (*it)->getType();

        if (a_type == etype)
        {
            int nb_child = (int)(*it)->getChildList().size();
            if (nb_child > 0)
            {
                loc_check_recursive_id_pool(etype, *it, has_id_pool);
            }
            else
            {
                const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
                if (psubtype)
                {
                    int idpool = psubtype->getIdPool();
                    if (idpool > 0)
                    {
                        *has_id_pool = true;
                    }
                }
            }
            break;
        }
    }
}

HC_DATA_DLL_API bool HCDIhasIdPool(int etype)
{
    bool has_id_pool = false;
    MultiCFGKernelMgr& descrp_model = MultiCFGKernelMgr::getInstance();
    const CFGKernel* a_cfgkernel = descrp_model.GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return has_id_pool;
    const MvPreDatasHierarchy_t* a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return has_id_pool;


    loc_check_recursive_id_pool(etype, a_data_cfg_p, &has_id_pool);
    return has_id_pool;
}
HC_DATA_DLL_API bool HCDIgetInfo(const void* data_cfg_p, int etype, const string& htype, leafmapmap_t& list)
{
    if (NULL == data_cfg_p)
        return false;

    const MvPreDatasHierarchy_t* a_data_cfg_p = (const MvPreDatasHierarchy_t*)data_cfg_p;
    bool do_for_first_time = true;
    leaf_t a_sub_new_member;
    bool add_all_child = true;
    loc_recursive_get_type_has_subtype_based_on_htype(etype, htype, a_data_cfg_p, list, do_for_first_time, a_sub_new_member, add_all_child);

    if (list.empty())
        return false;
    return true;
}

HC_DATA_DLL_API bool HCDIgetInfoBasedOnHtypeTitle(const bool& isuser, int etype, string& htype, string& title_htype, string& sub_htype, string& title_sub_htype, string& sub_htype_child, child_t& result_vector)
{

    const MvPreDatasHierarchy_t* a_data_cfg_p = NULL;
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return false;

    if (isuser)
        a_data_cfg_p = a_cfgkernel->get_userdefined_hierarchy(0);
    else
        a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);

    if (NULL == a_data_cfg_p)
        return false;


    bool do_for_first_time = true;
    leaf_t a_sub_new_member;

    loc_recursive_get_type_has_subtype_based_on_htype_sub_htype_sub_htype_title(etype, htype, title_htype, sub_htype, title_sub_htype, sub_htype_child, a_data_cfg_p, do_for_first_time, a_sub_new_member, result_vector);

    if (result_vector.empty())
        return false;
    return true;
}
#if 0
/**/
/*ENGG,
DISCRETE, "Surface To Surface"
SOLVERTYPE
ENGG+DISCRETE
DISCRETE+SOLVERTYPE
etype GROUP, INTER*/
void loc_recursive_get_type_has_subtype_based_on_htype(int etype,
    /*vector<htype, title2>,*/
    string& htype, string& htype2, string& title2,
    const MvPreDatasHierarchy_t* data, leafmapmap_t& list, bool& do_for_first_time,
    leaf_t& a_sub_new_member, bool add_all_child)
#endif
    void loc_recursive_get_type_has_subtype_based_on_htype(int etype, const string& htype, const MvPreDatasHierarchy_t* data, leafmapmap_t& list,
        bool& do_for_first_time,
        leaf_t& a_sub_new_member, bool add_all_child)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        obj_type_e a_type = (*it)->getType();
        const string parent_title = (*it)->getTitle();
        string htype_str = (*it)->getHtype();
        string keyword = (*it)->getKeyword();
        if (a_type == etype)
        {
            MvPreDatasHierarchyList_t child_list = (*it)->getChildList();

            MvPreDatasHierarchyList_t::iterator ch_iter_b = child_list.begin();
            MvPreDatasHierarchyList_t::iterator ch_iter_e = child_list.end();
            MvPreDatasHierarchyList_t::iterator ch_iter;
            child_t last_item;
            for (ch_iter = ch_iter_b; ch_iter != ch_iter_e; ++ch_iter)
            {
                MvPreDatasHierarchy_t* child = (*ch_iter);
                htype_str = child->getHtype();
                if ((htype_str == htype) && (htype != ""))
                    add_all_child = true;
                MvPreDatasHierarchyList_t gchild_list = child->getChildList();
                int size = (int)gchild_list.size();
                if (size > 0)
                {
                    string title = child->getTitle();
                    loc_recursive_get_type_has_subtype_based_on_htype(etype, htype, child, list, do_for_first_time, a_sub_new_member, add_all_child);

                    leafmap_t a_map;
                    a_map[title] = a_sub_new_member;
                    list[parent_title] = a_map;
                }
                else
                {
                    string title = child->getTitle();
                    last_item.push_back(title);
                }
            }
            a_sub_new_member[parent_title] = last_item;
        }
    }
}

void loc_recursive_get_type_has_subtype_based_on_htype_sub_htype_sub_htype_title(int etype,
    const string& htype, string& title_htype, string& sub_htype, string& sub_htype_title, string& sub_htype_child,
    const MvPreDatasHierarchy_t* data, bool& do_for_first_time,
    leaf_t& a_sub_new_member, child_t& result_vector, bool enable_do_firsttime)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;
    for (it = it_begin; it != it_end; ++it)
    {
        obj_type_e a_type = (*it)->getType();
        const string parent_title = (*it)->getTitle();
        string htype_str = (*it)->getHtype();
        string keyword = (*it)->getKeyword();

        if (enable_do_firsttime == false && parent_title != sub_htype_title)
            continue;

        if (a_type == etype)
        {
            if (do_for_first_time)
            {
                do_for_first_time = false;
                MvPreDatasHierarchyList_t child_list = (*it)->getChildList(); // sliding tied

                MvPreDatasHierarchyList_t::iterator ch_iter_b = child_list.begin();
                MvPreDatasHierarchyList_t::iterator ch_iter_e = child_list.end();
                MvPreDatasHierarchyList_t::iterator ch_iter;
                for (ch_iter = ch_iter_b; ch_iter != ch_iter_e; ++ch_iter)
                {
                    MvPreDatasHierarchy_t* child = (*ch_iter);
                    htype_str = child->getHtype();
                    string child_title = child->getTitle();
                    if (title_htype == "")
                    {
                        result_vector.push_back(child_title);
                        continue;
                    }

                    bool title_match = false;

                    if ((htype_str == htype) && (htype != ""))
                    {
                        MvPreDatasHierarchyList_t gchild_list = child->getChildList();
                        string title = child->getTitle();
                        int size = (int)gchild_list.size();
                        if (title_htype == "" && sub_htype_title == "")
                            result_vector.push_back(child_title);
                        else if (title_htype == child_title)
                        {
                            loc_recursive_get_type_has_subtype_based_on_htype_sub_htype_sub_htype_title(etype, htype, title_htype, sub_htype, sub_htype_title, sub_htype_child, child, do_for_first_time, a_sub_new_member, result_vector, enable_do_firsttime);
                        }
                    }
                }
            }
            else
            {

                MvPreDatasHierarchy_t* child = (*it);
                MvPreDatasHierarchyList_t gchild_list = child->getChildList();
                int size = (int)gchild_list.size();
                string child_title = child->getTitle();
                if (sub_htype_title == "")
                    result_vector.push_back(child_title);
                else if (child_title == sub_htype_title)
                {
                    MvPreDatasHierarchyList_t::iterator ch_iter_b = gchild_list.begin();
                    MvPreDatasHierarchyList_t::iterator ch_iter_e = gchild_list.end();
                    MvPreDatasHierarchyList_t::iterator ch_iter;
                    for (ch_iter = ch_iter_b; ch_iter != ch_iter_e; ++ch_iter)
                    {
                        MvPreDatasHierarchy_t* child_last = (*ch_iter);
                        htype_str = child_last->getHtype();
                        string a_child_title = child_last->getTitle();
                        result_vector.push_back(a_child_title);
                    }
                }
            }
        }
    }
}

// type = HCDI_OBJ_TYPE_GROUPS
// htype = ENGG
// list will contain the TITLE hierarchies
bool HCDILoadDataHiearchyAtUserLocation(obj_type_e type, const string& htype, bool is_user, leafmapmap_t& list)
{
    bool result = false;
    char* fullpath_c = getenv("HM_CONTACT_CXT_MENU_FILE");
    string fullpath = (fullpath_c == NULL) ? "" : fullpath_c;
    const MvPreDatasHierarchy_t* a_data_cfg_p = NULL;
    CFGKernel* a_cfgkernel = const_cast<CFGKernel*>(MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel());
    if (!a_cfgkernel)
        return false;
    if (fullpath.empty())
    {
        if (is_user)
            a_data_cfg_p = a_cfgkernel->get_userdefined_hierarchy(0);
        else
            a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);

        result = HCDIgetInfo(a_data_cfg_p, type, htype, list);
    }
    else if (fullpath.size())
    {
        a_data_cfg_p = MvDataCfgParser_t(fullpath).getDatasHierarchyPtr(*a_cfgkernel, "included data");
        result = HCDIgetInfo(a_data_cfg_p, type, htype, list);

        delete a_data_cfg_p;
    }
    return result;
}

void loc_check_recursive_flag(obj_type_e type, const string& username, const MvPreDatasHierarchy_t* data_cfg_p, int flag, bool* if_found, bool* status)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        if (*if_found)
            break;

        obj_type_e a_type = (*it)->getType();

        if (a_type == type)
        {
            if (username.empty())
            {
                *status = (*it)->getFlagStatus(flag);
                *if_found = true;
            }
            else
            {
                string keyword = (*it)->getKeyword();
                if (keyword == username)
                {
                    *status = (*it)->getFlagStatus(flag);
                    *if_found = true;
                }
                if (*if_found == false)
                {
                    int nb_child = (int)(*it)->getChildList().size();
                    if (nb_child > 0)
                    {
                        loc_check_recursive_flag(type, username, *it, flag, if_found, status);
                    }
                    else
                    {
                        const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
                        if (psubtype)
                        {
                            const MvStringList_t& list = psubtype->getUserNameList();
                            int size = (int)list.size();
                            bool found_in_list = false;
                            for (int i = 0; i < size; i++)
                            {
                                string elem = list.at(i);
                                if (username == elem)
                                {
                                    found_in_list = true;
                                    break;
                                }
                            }

                            if (found_in_list)
                            {
                                *if_found = true;
                                *status = (*it)->getFlagStatus(flag);
                            }
                        }
                    }
                }
            }
        }
    }
}

void loc_get_recursive_flag(obj_type_e type, string& username, const MvPreDatasHierarchy_t* data_cfg_p, bool* found, int* flag)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        if (*found)
            break;
        obj_type_e a_type = (*it)->getType();

        if (a_type == type)
        {
            if (username.empty())
            {
                *flag = (*it)->getFlags();
                *found = true;
            }
            else
            {
                string keyword = (*it)->getKeyword();
                if (keyword == username)
                {
                    *flag = (*it)->getFlags();
                    *found = true;
                }
                if (*found == false)
                {
                    int nb_child = (int)(*it)->getChildList().size();
                    if (nb_child > 0)
                    {
                        loc_get_recursive_flag(type, username, *it, found, flag);
                    }
                    else
                    {
                        const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
                        if (psubtype)
                        {
                            const MvStringList_t& list = psubtype->getUserNameList();
                            int size = (int)list.size();
                            bool found_in_list = false;
                            for (int i = 0; i < size; i++)
                            {
                                string elem = list.at(i);
                                if (username == elem)
                                {
                                    found_in_list = true;
                                    break;
                                }
                            }

                            if (found_in_list)
                            {
                                *found = true;
                                *flag = (*it)->getFlags();
                            }
                        }
                    }
                }
            }
        }
    }
}

HC_DATA_DLL_API int HCDIGetObjTypeFlags(obj_type_e type, string& username)
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = NULL;
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return 0;

    a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);

    if (NULL == a_data_cfg_p)
        return 0;


    bool if_found = false;
    int flag = 0;
    loc_get_recursive_flag(type, username, a_data_cfg_p, &if_found, &flag);
    return flag;
}

HC_DATA_DLL_API bool HCDIHasObjTypeFlags(obj_type_e type, int flag)
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = NULL;
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return false;

    a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);

    if (NULL == a_data_cfg_p)
        return false;

    string username = "";
    bool if_found = false;
    bool status = false;
    loc_check_recursive_flag(type, username, a_data_cfg_p, flag, &if_found, &status);
    return status;
}

HC_DATA_DLL_API bool HCDIHasObjTypeWithUserNameFlags(obj_type_e type, const string& username, int flag)
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = NULL;
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return false;

    a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);

    if (NULL == a_data_cfg_p)
        return false;

    bool if_found = false;
    bool status = false;
    loc_check_recursive_flag(type, username, a_data_cfg_p, flag, &if_found, &status);
    return status;
}

void loc_fill_str_info(string& q_str, string& input_str, MvPreDatasHierarchy_t* data)
{
    if (data == NULL || input_str == "")
        return;

    char temp_var[200];
    if (input_str == "HTYPE")
    {
        q_str = data->getHtype();
    }
    else if (input_str == "KEYWORD")
    {
        q_str = data->getKeyword();
    }
    else if (input_str == "TITLE")
    {
        q_str = data->getTitle();
    }
    else if (input_str == "USER_ID")
    {
        const MvSubtype_t* subtype = data->getSubtypePtr();
        if (subtype)
        {
            int user_id = subtype->getUserId();
            sprintf(temp_var, "%d", user_id);
            q_str = temp_var;
        }
    }
    else if (input_str == "ID_POOL")
    {
        const MvSubtype_t* subtype = data->getSubtypePtr();
        if (subtype)
        {
            short int idpool = subtype->getIdPool();
            sprintf(temp_var, "%d", idpool);
            q_str = temp_var;
        }
    }
    else if (input_str == "CARD_IMAGE")
    {
        const MvSubtype_t* subtype = data->getSubtypePtr();
        if (subtype)
        {
            char* card_image = subtype->getCardImage();
            q_str = card_image;
        }
    }
    else if (input_str == "CONFIG_TYPE")
    {
        const MvSubtype_t* subtype = data->getSubtypePtr();
        if (subtype)
        {
            int config_type = subtype->getHMConfigType();
            sprintf(temp_var, "%d", config_type);
            q_str = temp_var;
        }
    }
    else if (input_str == "HM_TYPE")
    {
        const MvSubtype_t* subtype = data->getSubtypePtr();
        if (subtype)
        {
            int hm_type = subtype->getHMType();
            sprintf(temp_var, "%d", hm_type);
            q_str = temp_var;
        }
    }
}

void loc_query_hierarchy(int etype, const MvPreDatasHierarchy_t* data_cfg_p, vector< vector < pair<string, string>> >& input_vect, vector< pair<string, vector<string>>>& output_name_value_vect)
{
    int parent_size = (int)input_vect.size();

    const MvPreDatasHierarchy_t* data = (const MvPreDatasHierarchy_t*)data_cfg_p;
    MvPreDatasHierarchyList_t::const_iterator it_begin = data->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;
    for (it = it_begin; it != it_end; ++it)
    {
        obj_type_e a_type = (*it)->getType();
        if (a_type == etype)
        {
            string first_attrib_str = "";
            if (parent_size > 0)
            {
                vector <pair<string, string>>& a_item = input_vect[0];
                int size = (int)a_item.size();
                if (size > 0)
                {
                    loc_fill_str_info(first_attrib_str, a_item[0].first, *it);

                    if (first_attrib_str == a_item[0].second)
                    {
                        string sec_attrib_str = "";
                        if (size > 1)
                        {
                            loc_fill_str_info(sec_attrib_str, a_item[1].first, *it);
                            if (sec_attrib_str == a_item[1].second)
                            {
                                if (parent_size > 1)
                                {
                                    vector<vector <pair<string, string>>> t_vect;
                                    int temp_size = parent_size - 1;
                                    int i = 1;
                                    while (temp_size)
                                    {
                                        t_vect.push_back(input_vect[i]);
                                        i++;
                                        temp_size--;
                                    }

                                    loc_query_hierarchy(etype, *it, t_vect, output_name_value_vect);
                                }
                            }
                        }
                        else if (size == 1)
                        {
                            if (parent_size > 1)
                            {
                                vector<vector <pair<string, string>>> t_vect;
                                int temp_size = parent_size - 1;
                                int i = 1;
                                while (temp_size)
                                {
                                    t_vect.push_back(input_vect[i]);
                                    i++;
                                    temp_size--;
                                }

                                loc_query_hierarchy(etype, *it, t_vect, output_name_value_vect);
                            }
                            else
                            {
                                vector< pair<string, vector<string>>>::iterator o_iter_b = output_name_value_vect.begin();
                                vector< pair<string, vector<string>>>::iterator o_iter_e = output_name_value_vect.end();
                                vector< pair<string, vector<string>>>::iterator o_iter;
                                for (o_iter = o_iter_b; o_iter != o_iter_e; ++o_iter)
                                {
                                    string& output_tag = (*o_iter).first;
                                    vector<string>& output_values = (*o_iter).second;

                                    string value_str = "";
                                    loc_fill_str_info(value_str, output_tag, *it);
                                    output_values.push_back(value_str);
                                }
                            }
                        }
                    }
                    else
                    {
                        MvPreDatasHierarchyList_t child_list = (*it)->getChildList();
                        size = (int)child_list.size();
                        if (size > 0)
                        {
                            loc_query_hierarchy(etype, *it, input_vect, output_name_value_vect);
                        }
                    }
                }
            }
        }
    }
}

HC_DATA_DLL_API void HCDIQueryHierarchy(int etype, const bool& isuser, vector< vector < pair<string, string>> >& input_vect, vector< pair<string, vector<string>>>& output_name_value_vect)
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = NULL;
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;

    if (isuser)
        a_data_cfg_p = a_cfgkernel->get_userdefined_hierarchy(0);
    else
        a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);

    if (NULL == a_data_cfg_p)
        return;

    loc_query_hierarchy(etype, a_data_cfg_p, input_vect, output_name_value_vect);
}

HC_DATA_DLL_API void* HCDIGetCurrentCFGKernel()
{
    return (void*)MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
}

HC_DATA_DLL_API void HCDIGetAllLoadedCFGKernels(vector<string>& allLoadedCfgKernels)
{
    for (int i = 0; i < FF_LAST; i++)
    {
        CFGKernel* cfgkernel = MultiCFGKernelMgr::getInstance().GetCFGKernel(i);
        if (cfgkernel)
        {
            allLoadedCfgKernels.push_back(cfgkernel->getLatestVersion());
        }
    }
}

void loc_fill_flag_info_recursively(const MvPreDatasHierarchy_t* data_cfg_p, map<string, int>& type_username_str_bit_map)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        string skey = (*it)->getKeyword();
        int flags = (*it)->getFlags();
        type_username_str_bit_map[skey] = flags;

        loc_fill_flag_info_recursively(*it, type_username_str_bit_map);
        const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
        if (psubtype)
        {
            const MvStringList_t& list = psubtype->getUserNameList();
            int size = (int)list.size();
            for (int i = 0; i < size; i++)
            {
                string elem = list.at(i);
                type_username_str_bit_map[elem] = flags;
            }
        }
    }
}

HC_DATA_DLL_API void HCDIFillFlagsMap(map<string, int>& type_username_str_bit_map, bool add_child)
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = NULL;
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;

    a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;

    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        obj_type_e a_type = (*it)->getType();
        string type_str = MV_get_type(a_type);
        int flags = (*it)->getFlags();
        type_username_str_bit_map[type_str] = flags;

        if (add_child)
        {
            loc_fill_flag_info_recursively(*it, type_username_str_bit_map);
        }
    }
}

void loc_fill_hmunsupported_flag_info_recursively(const MvPreDatasHierarchy_t* data_cfg_p, set<string>& type_username_set, int bitmask)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        string skey = (*it)->getKeyword();
        int flags = (*it)->getFlags();
        bool if_hm_supported = (flags & bitmask) ? true : false;

        loc_fill_hmunsupported_flag_info_recursively(*it, type_username_set, bitmask);
        const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
        if (psubtype)
        {
            const MvStringList_t& list = psubtype->getUserNameList();
            int size = (int)list.size();
            for (int i = 0; i < size; i++)
            {
                string elem = list.at(i);
                if (if_hm_supported == false)
                {
                    type_username_set.insert(elem);
                }
            }
        }
    }
}

HC_DATA_DLL_API void HCDIFillHmUnsupportedObjectType(obj_type_e type, set<obj_type_e>& type_set)
{

    const MvPreDatasHierarchy_t* a_data_cfg_p = NULL;
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;

    int bitmask = a_cfgkernel->get_data_hierarchy_bitmask("HM_SUPPORTED");
    a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;

    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        obj_type_e a_type = (*it)->getType();
        if (a_type == type)
        {
            int flags = (*it)->getFlags();

            bool if_hm_supported = (flags & bitmask) ? true : false;
            if (if_hm_supported == false)
            {
                type_set.insert(a_type);
            }
        }
    }
}

HC_DATA_DLL_API void HCDIFillHmUnsupportedStrSetForObjectType(obj_type_e type, set<string>& type_username_set)
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = NULL;
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;

    int bitmask = a_cfgkernel->get_data_hierarchy_bitmask("HM_SUPPORTED");
    a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;

    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        obj_type_e a_type = (*it)->getType();
        if (a_type == type)
        {
            loc_fill_hmunsupported_flag_info_recursively(*it, type_username_set, bitmask);
        }
    }
}

HC_DATA_DLL_API void HCDIGetAllObjectTypesFlagged(const string& flag, set<obj_type_e>& type_set)
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = NULL;
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;

    int bitmask = a_cfgkernel->get_data_hierarchy_bitmask(flag);
    a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;

    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        obj_type_e a_type = (*it)->getType();
        int flags = (*it)->getFlags();

        bool if_supported = (flags & bitmask) ? true : false;
        if (if_supported == true)
        {
            type_set.insert(a_type);
        }
    }
}
void loc_fill_supported_flag_info_recursively(const MvPreDatasHierarchy_t* data_cfg_p, set<string>& type_username_set, int bitmask)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        string skey = (*it)->getKeyword();
        int flags = (*it)->getFlags();
        bool if_supported = (flags & bitmask) ? true : false;

        loc_fill_supported_flag_info_recursively(*it, type_username_set, bitmask);
        const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
        if (psubtype)
        {
            const MvStringList_t& list = psubtype->getUserNameList();
            int size = (int)list.size();
            for (int i = 0; i < size; i++)
            {
                string elem = list.at(i);
                if (if_supported == true)
                {
                    type_username_set.insert(elem);
                }
            }
        }
    }
}

HC_DATA_DLL_API void HCDIGetAllObjectUsernamesFlagged(obj_type_e type, const string& flag, set<string>& type_username_set)
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = NULL;
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;

    int bitmask = a_cfgkernel->get_data_hierarchy_bitmask(flag);
    a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;
    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        obj_type_e a_type = (*it)->getType();
        if (a_type == type)
        {
            loc_fill_supported_flag_info_recursively(*it, type_username_set, bitmask);
        }
    }
}
HC_DATA_DLL_API const MvSubtype_t* HCDIGetSubtypePtrFromFullType(const string& fulltype)
{
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return nullptr;

    return a_cfgkernel->get_subtype(fulltype);
}
void loc_get_recursive_child_flag(obj_type_e type, const MvPreDatasHierarchy_t* data_cfg_p, unsigned int config, int* flag)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        obj_type_e a_type = (*it)->getType();

        if (a_type == type)
        {
            int nb_child = (int)(*it)->getChildList().size();
            if (nb_child > 0)
            {
                loc_get_recursive_child_flag(type, *it, config, flag);
            }
            else
            {
                const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
                if (psubtype)
                {
                    int a_config = psubtype->getHMConfigType();
                    if (a_config == config)
                    {
                        *flag |= (*it)->getFlags();
                    }
                }
            }
        }
    }
}
HC_DATA_DLL_API string HCDIGetLatestUserSubProfile(unsigned int solvercode)
{
    map<ApplicationMode_e, string> appfolderhierachy;
    GetSolverFolderHierachy(appfolderhierachy);

    map<ApplicationMode_e, string>::iterator itr = appfolderhierachy.find((ApplicationMode_e)solvercode);

    if (itr != appfolderhierachy.end())
    {
        vector<string> vec_userprofile;
        HCDI_splitString(itr->second, ":", vec_userprofile);
        if (vec_userprofile.size())
            return vec_userprofile[0];
    }
    return "";
}


HC_DATA_DLL_API int HCDIGetObjTypeChildFlags(obj_type_e type, unsigned int config)
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = NULL;
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return 0;

    a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);

    if (NULL == a_data_cfg_p)
        return 0;

    int flag = 0;
    loc_get_recursive_child_flag(type, a_data_cfg_p, config, &flag);
    return flag;
}

void loc_get_recursive_max_child_bit(const MvPreDatasHierarchy_t* data_cfg_p, int* max_bit)
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        obj_type_e a_type = (*it)->getType();
        int flag = (*it)->getFlags();
        unsigned int r = 0;
        while (flag >>= 1)
        {
            r++;
        }
        if (r > 0)
            r = (unsigned int)pow(2, (double)r);
        if (*max_bit < (int)r)
            *max_bit = r;

        int nb_child = (int)(*it)->getChildList().size();
        if (nb_child > 0)
        {
            loc_get_recursive_max_child_bit(*it, max_bit);
        }
        else
        {
            const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
            if (psubtype)
            {
                int flag = (*it)->getFlags();
                unsigned int r = 0;
                while (flag >>= 1)
                {
                    r++;
                }
                if (r > 0)
                    r = (unsigned int)pow(2, (double)r);
                if (*max_bit < (int)r)
                    *max_bit = r;
            }
        }
    }
}

HC_DATA_DLL_API int HCDIGetMaxProfileBit()
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = NULL;
    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return 0;

    a_data_cfg_p = a_cfgkernel->get_datastreehierarchy(DTT_ASSIGNED);

    if (NULL == a_data_cfg_p)
        return 0;

    int max_bit = 0;
    loc_get_recursive_max_child_bit(a_data_cfg_p, &max_bit);
    return max_bit;
}

map<string, value_type_e>  g_skey_valtype;


int loc_update_skey_valtype(obj_type_e type, const IDescriptor* a_descr_p, vector<string>& vec_report);
int check_for_different_datatype(obj_type_e etype, vector<string>& vec_report)
{
    int ret = 0;
    bool has_conflict = false;
    vector< std::pair<string, const IDescriptor*> >  aListEntityIDescriptor;
    HCDIGetEntityDescriptors(etype, aListEntityIDescriptor);
    size_t nb_des = aListEntityIDescriptor.size();
    string etype_str = MV_get_type_str((obj_type_e)etype);
    //cout << etype_str << " " << nb_des << endl;
    for (int k = 0; k < nb_des; k++)
    {
        const IDescriptor* a_descr_p = aListEntityIDescriptor[k].second;
        if (!a_descr_p)
            continue;
        if (loc_update_skey_valtype(etype, a_descr_p, vec_report))
            has_conflict = true;
    }
    return has_conflict ? -1 : 0;
}

int loc_update_skey_valtype(obj_type_e type, const IDescriptor* a_descr_p, vector<string>& vec_report)
{
    if (!a_descr_p)
        return -1;
    bool has_conflict = false;
    string etype_str = MV_get_type_str(type);
    int  a_domains = HCDI_get_all_domains();
    MvIKeywordList_t* a_ikw_lst = NULL;
    a_ikw_lst = a_descr_p->getIKeywords(a_domains);
    if (!a_ikw_lst)
        return -1;

    bool first_time = true;
    MvDataFeatureList_t a_dfl;
    a_descr_p->getDataFeatures(a_domains, &a_dfl);
    MvIKeywordList_t    vec_ikey_idenval;
    MvIKeywordList_t* iden_lst = NULL;
    iden_lst = &vec_ikey_idenval;
    a_descr_p->getIdentifiers(iden_lst);

    //append array features ikws
    MvDataFeatureList_t::iterator iter_b = a_dfl.begin();
    MvDataFeatureList_t::iterator iter_e = a_dfl.end();
    MvDataFeatureList_t::iterator iter;
    for (iter = iter_b; iter != iter_e; ++iter)
    {
        MvDataFeatureType_e featuretype = (*iter)->getType();
        if (featuretype == DFT_SUBOBJECT)
        {
            int ikeyword = ((MvDataSingleFeature_t*)(*iter))->getIKeyword();
            const MvFullTypeSet_t& full_type_set = ((MvDataDataFeature_t*)(*iter))->getAllowedObjectFullTypes();

            MvFullType_t fulltype;
            if (!full_type_set.empty())
            {
                MvFullTypeSet_t::const_iterator it;
                for (it = full_type_set.begin(); it != full_type_set.end(); ++it)
                {
                    fulltype = (*it);
                    //In case of non-subtyped SUBOBJECT, we will have to change this.
                    if (NULL != fulltype.getSubtypePtr()) break;
                }
            }
            IDescriptor* sub_descrp = HCDI_GetDescriptorHandleFromFullType(fulltype);
            if (loc_update_skey_valtype(type, sub_descrp, vec_report))
                has_conflict = true;
        }
    }


    for (int i = 0; i < (*a_ikw_lst).size(); i++)
    {
        int a_ikeyword = (*a_ikw_lst)[i];
        int att_id = 0;
        if (iden_lst)
            att_id = (*iden_lst)[a_ikeyword];
        else
            att_id = a_descr_p->getIdentifierValue(a_domains, a_ikeyword);
        if (att_id < 0)
            continue;
        if (a_ikeyword > 0)
        {
            pair<map<string, value_type_e>::iterator, bool> ret;
            value_type_e a_val = a_descr_p->getValueType(a_ikeyword);
            string skey = a_descr_p->getSKeyword(a_ikeyword);
            ret = g_skey_valtype.insert(pair<string, value_type_e>(skey, a_val));
            if (ret.second == false)
            {
                value_type_e val = ret.first->second;
                if (val != a_val)
                {
                    if (first_time == true)
                    {
                        first_time = false;

                        string str = etype_str + string("/") + string(a_descr_p->getKeyword()) + string(" :Conflict in value type. Please check!\n");
                        vec_report.push_back(str);
                    }

                    string err = string(" Dataname: ") + skey + string("\n");
                    vec_report.push_back(err);
                    has_conflict = true;
                }
            }
        }
    }
    return has_conflict ? -1 : 0;
}

HC_DATA_DLL_API int HCDI_TestTool(const string& path_home, int testid, const string& userprofile, vector<string>& vec_report)
{
    std::string str_error;
    unsigned int memUsage = 0, memUsage_st = 0, memUsage_st_begin = 0;
    using namespace std;

    void outputtimedifference(clock_t begin_time, string str);
    void PrintResults();
    void UpdateTimeMemory(string version, clock_t begin_time, clock_t end_time, unsigned int begin_mem, unsigned int end_mem);
    multimap< string, vector<float> >   a_mapUserprofileTimeMemory;

    //check for current UP

    MvFileFormat_e fileformat_cur = MultiCFGKernelMgr::getInstance().GetActiveUserProfile();
    clock_t begin_time = clock();
    clock_t end_time = 0;

    map<ApplicationMode_e, string> appfolderhierachy;
    GetSolverFolderHierachy(appfolderhierachy);
    switch (testid)
    {
        case CONFIG_TEST_LOAD_CFG_KERNEL:
        {
            HCDI_Close_Kernel();
            for (auto it : appfolderhierachy)
            {
                //if(it.first != HCDI_SOLVER_LSDYNA)
                //    continue;
                vector<string> vec_userprofile;
                HCDI_splitString(it.second, ":", vec_userprofile);
                for (auto it : vec_userprofile)
                {
                    std::size_t found = it.find("common");
                    if (found != std::string::npos)
                        continue;

                    if (userprofile != "" && userprofile != it)
                        continue;
                    vector<string> flags{ "HM_SUPPORTED" };
                    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().InitCFGKernel(path_home, "", it, "", false, flags, str_error);
                    if (!a_cfgkernel)
                        continue;
                    if (str_error != "")
                    {
                        vec_report.push_back(str_error);
                        HCDI_Close_Kernel();
                        string fileformat_str = MV_get_file_format(fileformat_cur);
                        MultiCFGKernelMgr::getInstance().InitCFGKernel(path_home, "", fileformat_str, "", false, { "HM_SUPPORTED" }, str_error);
                        return -1;
                    }
                    else
                    {
                        // successfull
                        vec_report.push_back(it);
                        vec_report.push_back("Successful!!\n");
                    }
                    MvFileFormat_e ff = MV_get_file_format(it);
                    MultiCFGKernelMgr::getInstance().DeleteCFGKernel(ff);
                }
            }
            break;
        }
        case CONFIG_TEST_LOADING_TIME:
        {
            HCDI_Close_Kernel();
            vec_report.push_back("\nTest for Loading time (in secs)\n");
            for (auto it : appfolderhierachy)
            {
                vector<string> vec_userprofile;
                HCDI_splitString(it.second, ":", vec_userprofile);
                for (auto it : vec_userprofile)
                {
                    std::size_t found = it.find("common");
                    if (found != std::string::npos)
                        continue;

                    if (userprofile != "" && userprofile != it)
                        continue;

                    begin_time = clock();
                    vector<string> flags{ "HM_SUPPORTED" };
                    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().InitCFGKernel(path_home, "", it, "", false, flags, str_error);
                    if (!a_cfgkernel)
                        continue;
                    if (str_error != "")
                    {
                        vec_report.push_back(str_error);
                        HCDI_Close_Kernel();
                        string fileformat_str = MV_get_file_format(fileformat_cur);
                        MultiCFGKernelMgr::getInstance().InitCFGKernel(path_home, "", fileformat_str, "", false, { "HM_SUPPORTED" }, str_error);
                        return -1;
                    }
                    end_time = clock();
                    if (end_time - begin_time == 0)
                        continue;

                    float diff = float(end_time - begin_time) / CLOCKS_PER_SEC;

                    string up = it;
                    up.resize(20, ' ');

                    std::ostringstream ss;
                    ss << diff;
                    std::string s(ss.str());

                    string str = up + s + "\n";
                    vec_report.push_back(str);
                }
            }
            break;
        }
        case CONFIG_TEST_MEMORY_USAGE:
        {
#if defined(_WIN32) || defined(WIN32)
#include <Psapi.h>
#include <direct.h>
#include <process.h>
#ifdef _MEMORY_USAGE_CHECK
            typedef int   (WINAPI* PCTOR) (const char* path_home, const char* version, const string& filename, bool reload, string& str_error);
            typedef void   (WINAPI* HCDI_CLOSE) ();
            typedef TCHAR* (WINAPI* PGETLASTUSEDFUNC) ();
            HANDLE hProcess;
            DWORD processID = 0;
            //PROCESS_MEMORY_COUNTERS pmc;
            HCDI_Close_Kernel();
            vec_report.push_back("\nTest for Memory Usages (in MB)\n");
            processID = GetCurrentProcessId();
            DWORD error = GetLastError();

            hProcess = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, processID);
            if (nullptr == hProcess)
                return -1;
            for (auto it : appfolderhierachy)
            {
                vector<string> vec_userprofile;
                HCDI_splitString(it.second, ":", vec_userprofile);
                for (auto it : vec_userprofile)
                {
                    std::size_t found = it.find("common");
                    if (found != std::string::npos)
                        continue;

                    if (userprofile != "" && userprofile != it)
                        continue;

                    //if (GetProcessMemoryInfo(hProcess, &pmc, sizeof(pmc)))
                    //{
                    //    memUsage_st = (unsigned int)pmc.WorkingSetSize / 1024 / 1024;
                    //}
                    MEMORYSTATUSEX memInfo1;
                    memInfo1.dwLength = sizeof(MEMORYSTATUSEX);
                    GlobalMemoryStatusEx(&memInfo1);
                    DWORDLONG totalVirtualMem1 = memInfo1.ullTotalPageFile;
                    //Physical Memory currently used
                    DWORDLONG physMemUsed1 = memInfo1.ullTotalPhys - memInfo1.ullAvailPhys;

                    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().InitCFGKernel(path_home, "", it, "", false, { "HM_SUPPORTED" }, str_error);
                    if (!a_cfgkernel)
                        continue;
                    if (str_error != "")
                    {
                        HCDI_Close_Kernel();
                        string fileformat_str = MV_get_file_format(fileformat_cur);
                        const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().InitCFGKernel(path_home, "", fileformat_str, "", false, { "HM_SUPPORTED" }, str_error);
                        return -1;
                    }

                    //if (GetProcessMemoryInfo(hProcess, &pmc, sizeof(pmc)))
                    //{
                    //    memUsage = (unsigned int)pmc.WorkingSetSize / 1024 / 1024;
                    //}

                    MEMORYSTATUSEX memInfo2;
                    memInfo2.dwLength = sizeof(MEMORYSTATUSEX);
                    GlobalMemoryStatusEx(&memInfo2);
                    DWORDLONG totalVirtualMem2 = memInfo2.ullTotalPageFile;
                    //Physical Memory currently used
                    DWORDLONG physMemUsed2 = memInfo2.ullTotalPhys - memInfo2.ullAvailPhys;

                    float diff = float(physMemUsed2 - physMemUsed1) / 1024 / 1024;

                    if (!diff)
                        continue;
                    string temp = it;
                    temp.resize(20, ' ');

                    std::ostringstream ss;
                    ss << diff;
                    std::string s(ss.str());

                    string temp1 = temp + s + "\n";
                    vec_report.push_back(temp1);
                }
            }
            CloseHandle(hProcess);
#endif
#endif
            break;
        }
        case CONFIG_TEST_CONFLICTING_DATANAMES:
        {
            vec_report.push_back("\nTest for Conflicting datanames\n");
            for (auto it : appfolderhierachy)
            {
                vector<string> vec_userprofile;
                HCDI_splitString(it.second, ":", vec_userprofile);
                for (auto it : vec_userprofile)
                {
                    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().InitCFGKernel(path_home, "", it, "", false, { "HM_SUPPORTED" }, str_error);
                    if (!a_cfgkernel)
                        continue;
                    if (str_error != "")
                    {
                        return -1;
                    }
                    std::size_t found = it.find("common");
                    if (found != std::string::npos)
                        continue;

                    if (userprofile != "" && userprofile != it)
                        continue;

                    bool has_conflict = false;
                    vec_report.push_back((it + string("\n")));
                    for (int j = 0; j < HCDI_OBJ_TYPE_MAX; j++)
                    {
                        g_skey_valtype.clear();
                        if (check_for_different_datatype((obj_type_e)j, vec_report))
                        {
                            has_conflict = true;
                        }
                    }
                    if (!has_conflict)
                        vec_report.push_back("Successful!!\n\n");
                    else
                        vec_report.push_back("Not Successful!!\n\n");
                }
            }
            break;
        }
        case CONFIG_TEST_DATAHIERARCHY_EXPORT:
        {
            char* path = getenv("HW_CFG_TEST_TOOL_PATH");
            char* altair_home = getenv("ALTAIR_HOME");
            char* root_dir = getenv("HW_ROOTDIR");
            string filepath = "";
            if (path != NULL)
                filepath = path;
            else if (altair_home != NULL)
                filepath = altair_home;
            else if (root_dir != NULL)
            {
                filepath = root_dir;
                filepath += "/CFG_TESTTOOL_RESULTS/";
            }
            else
                return -1;

            int length = (int)filepath.size();
            if (length > 0)
            {
                // append '/' if not present
                if (filepath[length - 1] != '/')
                    filepath += "/";
            }
            for (auto it : appfolderhierachy)
            {
                vector<string> vec_userprofile;
                HCDI_splitString(it.second, ":", vec_userprofile);
                for (auto it : vec_userprofile)
                {
                    vector<string> flags{ "HM_SUPPORTED" };
                    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().InitCFGKernel(string(path_home), "", it, "", false, flags, str_error);
                    if (!a_cfgkernel)
                        continue;
                    if (str_error != "")
                    {
                        //MessageBoxA(NULL, str_error.c_str(), "", MB_OK);

                        return -1;
                    }
                    std::size_t found = it.find("common");
                    if (found != std::string::npos)
                        continue;

                    if (userprofile != "" && userprofile != it)
                        continue;

                    string a_filepath = filepath + "CFG/" + it;
                    a_filepath += "/";
                    string copy_filepath = a_filepath;

                    vector<string> pTokens;
                    string delem = "/";
                    StringTokenize(copy_filepath, pTokens, delem);
                    int size = (int)pTokens.size();
                    for (int i = 0; i < size - 2; i++)
                    {
                        string loc_path = "";
                        int j = 0;
                        while (j <= i + 1)
                        {
                            loc_path += pTokens[j];
                            loc_path += "/";
                            j++;
                        }

                        int check = mkdir(loc_path.c_str(), 0755);
                    }

                    int check = mkdir(a_filepath.c_str(), 0755);
                    //checking if directory is created
                    //if (!check)
                    //    printf("Directory created\n");
                    //else {
                      //  printf("Unable to create directory\n");
                      //  exit(1);
                    //}
                    a_cfgkernel->WriteDataHierarchyContents(a_filepath);

                    char output[500];
                    sprintf(output, "Data Hierarchy content written at location : %s\n", a_filepath.c_str());

                    string total_string = output;
                    vec_report.push_back(total_string);
                }
            }
            break;
        }
        case CONFIG_TEST_DATAHIERARCHY_UNIQUE_USERID:
        {
            for (auto it : appfolderhierachy)
            {
                vector<string> vec_userprofile;
                HCDI_splitString(it.second, ":", vec_userprofile);
                for (auto it : vec_userprofile)
                {
                    vector<string> flags{ "HM_SUPPORTED" };
                    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().InitCFGKernel(string(path_home), "", it, "", false, flags, str_error);
                    if (!a_cfgkernel)
                        continue;
                    if (str_error != "")
                    {
                        //MessageBoxA(NULL, str_error.c_str(), "", MB_OK);

                        return -1;
                    }
                    std::size_t found = it.find("common");
                    if (found != std::string::npos)
                        continue;

                    vector<string> errors;
                    int result = a_cfgkernel->CheckUniqueness("USERID", errors);

                    if (!result)
                    {
                        string loc_str = "For :" + it + "\n";
                        vec_report.push_back(loc_str);
                        vec_report.push_back("User Ids defined are unique!\n");
                    }
                    else
                    {
                        string loc_str = "Please correct:" + it + "\n";
                        vec_report.push_back(loc_str);
                        vector<string>::iterator it_b = errors.begin();
                        vector<string>::iterator it_e = errors.end();
                        vector<string>::iterator an_it;
                        for (an_it = it_b; an_it != it_e; ++an_it)
                        {
                            string a_error = *an_it;
                            vec_report.push_back(a_error);
                        }
                    }
                }
            }
            break;
        }
        case CONFIG_TEST_DATAHIERARCHY_UNIQUE_USERNAME:
        {
            for (auto it : appfolderhierachy)
            {
                vector<string> vec_userprofile;
                HCDI_splitString(it.second, ":", vec_userprofile);
                for (auto it : vec_userprofile)
                {
                    vector<string> flags{ "HM_SUPPORTED" };
                    const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().InitCFGKernel(string(path_home), "", it, "", false, flags, str_error);
                    if (!a_cfgkernel)
                        continue;
                    if (str_error != "")
                    {
                        //MessageBoxA(NULL, str_error.c_str(), "", MB_OK);

                        return -1;
                    }
                    std::size_t found = it.find("common");
                    if (found != std::string::npos)
                        continue;

                    vector<string> errors;
                    int result = a_cfgkernel->CheckUniqueness("USERNAME", errors);
                    if (!result)
                    {
                        string loc_str = "For :" + it + "\n";
                        vec_report.push_back(loc_str);
                        vec_report.push_back("User Names defined are unique!\n");
                    }
                    else
                    {
                        string loc_str = "Please correct:" + it + "\n";
                        vec_report.push_back(loc_str);
                        vector<string>::iterator it_b = errors.begin();
                        vector<string>::iterator it_e = errors.end();
                        vector<string>::iterator an_it;
                        for (an_it = it_b; an_it != it_e; ++an_it)
                        {
                            string a_error = *an_it;
                            vec_report.push_back(a_error);
                        }
                    }
                }
            }
            break;
        }
        default:
            break;
    }
    HCDI_Close_Kernel();
    string fileformat_str = MV_get_file_format(fileformat_cur);
    MultiCFGKernelMgr::getInstance().InitCFGKernel(path_home, "", fileformat_str, "", false, { "HM_SUPPORTED" }, str_error);
    return 0;
}

#ifdef  _HCDI_INTERNAL_TEST_TOOL

string GetCurrentFilePath()
{
    char pBuf[1024];
    int bytes = 0;
    size_t len = sizeof(pBuf);
#if defined(_WIN32) || defined(WIN32)
    bytes = GetModuleFileName(NULL, pBuf, (DWORD)len);
#else
    bytes = MIN(readlink("/proc/self/exe", pBuf, len), len - 1);
#endif
    if (bytes >= 0)
        pBuf[bytes] = '\0';

    return pBuf;
}

int main()
{
    string path_home = GetCurrentFilePath();
    vector<string> del;
    del.push_back("//");
    del.push_back("\\\\");
    del.push_back("\\");
    const std::string t = "/";
    for (int i = 0; i < del.size(); i++)
    {
        std::string::size_type n = 0;
        while ((n = path_home.find(del[i], n)) != std::string::npos)
        {
            path_home.replace(n, del[i].size(), t);
            n += t.size();
        }
    }
    size_t pos = path_home.find("hwcommon");
    if (pos != std::string::npos)
        path_home.erase(pos);

    vector<string> report_str;
    // HCDI_TestTool(path_home, 1, "", report_str);
     // HCDI_TestTool(path_home, 2, "", report_str);
    //  HCDI_TestTool(path_home, 3, "", report_str);

    HCDI_TestTool(path_home, 0, "EnggKernel", report_str);
    for (int i = 0; i < report_str.size(); i++)
        cout << report_str[i];
    return 0;
}
#endif //  _HCDI_INTERNAL_TEST_TOOL
