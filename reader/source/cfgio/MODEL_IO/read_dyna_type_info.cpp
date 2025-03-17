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

#include <algorithm>
#include <UTILS/mv_cstring.h>
#include <KERNEL/mv_descriptor.h>
#include <HCDI/hcdi_mv_descriptor.h>
#include <General/general_memory.h>
#include <KERNEL/mv_pre_datas_hierarchy.h>
#include <HCDI/hcdi_multicfgkernelmgr.h>
#include <KERNEL_BASE/utils.h>
#include "read_dyna_type_info.h"

static vector<pair<string, rdy_type_info_t >> g_current_string_type_info_vect; // later we can make it as part of SolverInputInfo 
static int g_current_type_info_format = 0; // later need to be part of SolverInputInfo
typedef std::pair<std::string, rdy_type_info_t> StringTypeInfoPair_t;
static rdy_type_info_t* type_info_by_keyword_a = NULL;
static size_t type_info_nb = 0;
bool do_once_fill_vect = true;
static bool myInHMFlag = true;
typedef vector<MvPreDatasHierarchy_t*> MvPreDatasHierarchyList_t;

static int loc_cmp_type_info_by_keyword (const rdy_type_info_t *info1, const rdy_type_info_t *info2)
{
    return strncmp(info1->keyword, info2->keyword, info2->keyword_size);
}

static int loc_cmp_type_info_by_type (const rdy_type_info_t *info1, const rdy_type_info_t *info2)
{
    return strcmp(info1->kernel_type, info2->kernel_type);

    //size_t strlen1 = strlen(info1->kernel_type);
    //size_t strlen2 = strlen(info2->kernel_type);
    //size_t strlen_min = strlen1<strlen2 ? strlen1 : strlen2;
    //return strncmp (info1->kernel_type, info2->kernel_type, strlen_min);
}


bool IsHmSupportedFlagCFG(int flag)
{
    static int bitmask = 0;
    if (bitmask == 0)
    {
        const CFGKernel* a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
        if (a_cfgkernel)
            bitmask = a_cfgkernel->get_data_hierarchy_bitmask("HM_SUPPORTED");
    }
    if (flag & bitmask)
        return true;
    return false;
}

const rdy_type_info_t *rdy_get_type_info_from_keyword
(const char *keyword, bool inHMFlag, int *format)
{
    int i = 0;
    if (NULL==keyword) return NULL;

    if (do_once_fill_vect)
    {
        myInHMFlag = inHMFlag;
        getUserNamesSolverInfo(g_current_string_type_info_vect, true, format, inHMFlag);

        sort(g_current_string_type_info_vect.begin(), g_current_string_type_info_vect.end(), [](const auto& lhs, const auto& rhs)
                {   return lhs.first > rhs.first; });

        do_once_fill_vect = false;
    }

    if('*' == keyword[0]) keyword++; // to skip from *
    const CFGKernel* a_cfg_kernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();

    if(myInHMFlag && !IsHmSupportedFlagCFG(myInHMFlag))
         return NULL;
    //Process header Line
    //const set<string>* p_opt_strings = nullptr;
    //a_cfg_kernel->getEntityOptionalStrings(i, &p_opt_strings);


    string  a_header_line(keyword);

    // 1. erase all optional strings if any
    //if (p_opt_strings && p_opt_strings->size())
    //{
    //    std::set<string>::iterator itr;// = p_type_info->myStripSubStrings->begin();
    //    //3. ERASE ALL BASED ON BITSheader_line DEFINED IN DATA_HIERARCHY FILE FROM HEADER IF ANY
    //    for (itr = p_opt_strings->begin(); itr != p_opt_strings->end(); itr++)
    //    {
    //        size_t pos = a_header_line.find(*itr);
    //        if (pos != string::npos) {
    //            a_header_line.erase(a_header_line.begin() + pos, a_header_line.begin() + pos + (*itr).length());
    //        }
    //    }
    //}
    // 2. CHECK FOR ENCRYPTED KEYWORD E.G. BEGIN_PGP....

    // 3. CONVERT SPACES TO '_'  FROM HEADER IF ANY
    // *INCIDENT WAVE INTRACTION PROPPERTY, ID=3

    // 4. REMOVE DIGITS FROM HEADER IF ANY
    //MAT/ELAST/1/2  

    auto it = std::find_if(g_current_string_type_info_vect.begin(), g_current_string_type_info_vect.end(), [&a_header_line](const pair< std::string, rdy_type_info_t >& x)
    {
        size_t sz = a_header_line.size();
        if (sz < x.first.size())
            return false;

        if (a_header_line.compare(x.first) == 0)
        {
            return true;
        }

        return false;
    });

    if (it != g_current_string_type_info_vect.end())
        return &(it->second);
/*
    if(MV_is_string_encrypted((char *)keyword))
    {
       return &encrypted_type;
    }
*/
    
    return NULL ;
}

const rdy_type_info_t *rdy_get_type_info_from_kernel_type
(const char *kernel_ftype, int format)
{
    /*Speacial handling for Unsupported card, as we still need to get the proper rdy_type_info_t,
    in case of unsupported subtype like *CONTROL_ME */
    

    if (!strncmp(kernel_ftype, "UNSUPPORTEDCARD", 10))
    {
        return NULL;
    }
    else if (!strncmp(kernel_ftype, "ENCRYPTED", 9))
    {
        return NULL;
    }
    static rdy_type_info_t* type_info_by_type_a = NULL;
    // Copy and sort array, if not yet done
    static bool is_sorted = false;
    if (g_current_type_info_format < format)
    {
        g_current_string_type_info_vect.clear();
        if (type_info_by_type_a)
        {
            free(type_info_by_type_a);
        }
        type_info_by_type_a = NULL;
        is_sorted = false;
    }
    if (false == is_sorted)
    {
        g_current_type_info_format = format;
        getUserNamesSolverInfo(g_current_string_type_info_vect, true, &format, true);

        int size = (int)g_current_string_type_info_vect.size();
        type_info_by_type_a = (rdy_type_info_t*)calloc(size, sizeof(rdy_type_info_t));

        is_sorted = true;

        vector<StringTypeInfoPair_t>::iterator iter_b = g_current_string_type_info_vect.begin();
        vector<StringTypeInfoPair_t>::iterator iter_e = g_current_string_type_info_vect.end();
        vector<StringTypeInfoPair_t>::iterator iter;
        int i = 0;
        for (iter = iter_b; iter != iter_e; ++iter, i++)
        {
            type_info_by_type_a[i] = iter->second;
        }

        qsort(type_info_by_type_a, size, sizeof(rdy_type_info_t),
            (int (*) (const void*, const void*)) loc_cmp_type_info_by_type);
    }

    // Search
    rdy_type_info_t dummy = { NULL, kernel_ftype, NULL, HCDI_OBJ_TYPE_NULL, 0, 0 };
    return (const rdy_type_info_t *) bsearch
        (&dummy, type_info_by_type_a, (int)g_current_string_type_info_vect.size(), sizeof(rdy_type_info_t),
         (int (*) (const void *, const void *)) loc_cmp_type_info_by_type);
}


extern "C"
const char *dyna_rw_get_id_keyword (const char *skeyword)
{
    
    static char *skeyword_id=NULL;
    static int length_alloc=0;
    int length=0;

    if (NULL!=skeyword) length =(int) strlen(skeyword);

    if ((length+4) > length_alloc)
    {
        length_alloc = length + 4;
        skeyword_id = (char*) my_malloc (length_alloc, sizeof(char));
        
    }
    if (NULL != skeyword_id)
    {
        if (NULL != skeyword) strcpy(skeyword_id, skeyword);
        else strcpy(skeyword_id, "");
        strcat(skeyword_id, "_ID");
    }
    return skeyword_id;
}

/* Builds an skeyword from two strings, similar to dyna_rw_get_id_keyword */
extern "C"
const char *dyna_rw_cat_keyword (const char *part1, const char *part2)
{
    static string skeyword="";

    if(NULL!=part1) skeyword = part1;
    if(NULL!=part2) skeyword.append(part2);

    return skeyword.c_str();
}

void fill_rdy_type_info()
{
    map<string, obj_type_e> a_keyword_type_map;
    static bool do_once = true;
    if (do_once)
    {
        bool add_first_parent = false;
        HCDIgetKeywordTypeMap(a_keyword_type_map, add_first_parent);

        int nb_types = (int)a_keyword_type_map.size();
        type_info_nb = nb_types;

        type_info_by_keyword_a = (rdy_type_info_t*)calloc(nb_types, sizeof(rdy_type_info_t));
        map<string, obj_type_e>::iterator iter_b = a_keyword_type_map.begin();
        map<string, obj_type_e>::iterator iter_e = a_keyword_type_map.end();
        map<string, obj_type_e>::iterator iter;
        int i = 0;
        for (iter = iter_b; iter != iter_e; ++iter, i++)
        {
            string keyword = iter->first;
            obj_type_e type = iter->second;

            rdy_type_info_t& data = type_info_by_keyword_a[i];
            data.keyword = (char*)calloc((int)keyword.size() + 2, sizeof(char));
            strcpy((char*)data.keyword, "*");
            strcat((char*)data.keyword, keyword.c_str());

            string type_str = MV_get_type(type);
            data.kernel_type = (char*)calloc((int)type_str.size() + 1, sizeof(char));
            strcat((char*)data.kernel_type, type_str.c_str());

            data.input_type = (char*)calloc((int)keyword.size() + 1, sizeof(char));
            strcat((char*)data.input_type, keyword.c_str());

            data.obj_type = type;

            data.format_info = HCDIGetObjTypeFlags(type, keyword);
            data.needs_posttreatment = 0;

            data.nb_cards_per_object = -1;

            data.keyword_size = (int)strlen(data.keyword);
        }
        do_once = false;
    }
}

HCIO_DATA_DLL_API void HCIOI_Free_Rdy_Type_Info()
{
    if (type_info_by_keyword_a)
    {
        for (int i = 0; i < type_info_nb; i++)
        {
            rdy_type_info_t& data = type_info_by_keyword_a[i];
            free((void*)data.keyword);
            data.keyword = NULL;

            free((void*)data.kernel_type);
            data.kernel_type = NULL;

            free((void*)data.input_type);
            data.input_type = NULL;
        }
        free((void *)type_info_by_keyword_a);
        type_info_by_keyword_a = NULL;
    }
}

void FillSolverInfoRecursively(obj_type_e etype, const MvPreDatasHierarchy_t* data_cfg_p, std::vector<StringTypeInfoPair_t>& objectsolverinfo, int *format, bool inHMFlag, bool from_import)
{
    const CFGKernel* a_cfg_kernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    MvPreDatasHierarchyList_t::const_iterator it_begin = data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        string skey = (*it)->getKeyword();
        int flags = (*it)->getFlags();

        int nb_child = (*it)->getNbChildren();
        if (nb_child > 0)
        {
            FillSolverInfoRecursively(etype, *it, objectsolverinfo, format, inHMFlag, from_import);
        }
        else
        {
            const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
            if (psubtype)
            {
                const MvStringList_t& list = psubtype->getUserNameList();
                int size = (int)list.size();
                int bitmask = (*it)->getFlags();
                for (int i = 0; i < size; i++)
                {
                    string elem = list.at(i);
                    rdy_type_info_t data;
                    if(inHMFlag)
                    {
                        // skip if etype is having !HM_SUPPORTED as a flag
                        static const string hm_supported_str = "HM_SUPPORTED";
                        int hm_supported_bitmask = a_cfg_kernel->get_data_hierarchy_bitmask(hm_supported_str);
                        if(!(bitmask & hm_supported_bitmask))
                            continue;

                        if (from_import)
                        {
                            static const string hm_export_only_str = "HM_EXPORT_ONLY";
                            int hm_export_only = a_cfg_kernel->get_data_hierarchy_bitmask(hm_export_only_str);
                            if (bitmask & hm_export_only)
                                continue;
                        }

                    }

                    data.obj_type = etype;

                    data.keyword = (char*)calloc((int)elem.size() + 2, sizeof(char));
                    strcpy((char*)data.keyword, "*");
                    strcat((char*)data.keyword, elem.c_str());

                    string type_str = MV_get_type(etype);
                    data.kernel_type = (char*)calloc((int)type_str.size() + (int)elem.size() + 3, sizeof(char));
                    strcpy((char*)data.kernel_type, "/");
                    strcat((char*)data.kernel_type, type_str.c_str());
                    strcat((char*)data.kernel_type, "/");
                    strcat((char*)data.kernel_type, elem.c_str());

                    data.input_type = (char*)calloc((int)elem.size() + 2, sizeof(char));
                    strcpy((char*)data.input_type, "/");
                    strcat((char*)data.input_type, elem.c_str());

                    data.format_info = bitmask;
                    data.needs_posttreatment = 0;

                    data.nb_cards_per_object = -1;
                    data.keyword_size = (int)strlen(data.keyword);
                    data.p_descr = a_cfg_kernel->GetDescriptorHandleFromKeyword(etype, list.at(i));
                    if (data.p_descr)
                    {
                        int a_format = *format;
                        const fileformat_t* a_format_p = data.p_descr->getDynaFileFormatPtr((MvFileFormat_e*)&a_format);
                        data.p_format = a_format_p;
                        data.format = a_format;
                    }
                    else
                    {
                        data.p_format = NULL;
                        data.format = 0;
                    }
                    objectsolverinfo.push_back(StringTypeInfoPair_t(elem, data));
                }
            }
        }
    }
}


void getUserNamesSolverInfo(vector<pair<string, rdy_type_info_t>>(&objectsolverinfo), bool add_user_subtype, int *format, bool inHMFlag, bool from_import)
{
    const CFGKernel* a_cfg_kernel = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (a_cfg_kernel == NULL)
    {
        return;
    }
    const MvPreDatasHierarchy_t* a_data_cfg_p = a_cfg_kernel->get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;
    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();

    MvPreDatasHierarchyList_t::const_iterator it_end = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        obj_type_e type = (*it)->getType();
        if(HCDI_OBJ_TYPE_SUBOBJECT == type) continue;
        if(inHMFlag)
        {
            // skip if etype is having !HM_SUPPORTED as a flag
            int bitmask = a_cfg_kernel->get_data_hierarchy_bitmask("HM_SUPPORTED");
            int flags = (*it)->getFlags();
            bool result = (flags & bitmask) ? true : false;
            if(result != true)
                continue;
        }

        int nb_child = (*it)->getNbChildren();

        if (!add_user_subtype || nb_child == 0)
        {
            const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
            if (psubtype)
            {
                const MvStringList_t& list = psubtype->getUserNameList();

                int size = (int)list.size();
                int bitmask = (*it)->getFlags();
                for (int i = 0; i < size; i++)
                {
                    std::pair<string, rdy_type_info_t>   keysolverinfo;
                    string elem = keysolverinfo.first;
                    if(inHMFlag)
                    {
                        // skip if etype is having !HM_SUPPORTED as a flag
                        int hm_supported_bitmask = a_cfg_kernel->get_data_hierarchy_bitmask("HM_SUPPORTED");
                        if(!(bitmask & hm_supported_bitmask))
                            continue;
                    }
                    keysolverinfo.first = list.at(i);

                    rdy_type_info_t data;
                    data.obj_type = type;
                    data.keyword = (char*)calloc((int)elem.size() + 2, sizeof(char));
                    strcpy((char*)data.keyword, "*");
                    strcat((char*)data.keyword, elem.c_str());

                    string type_str = MV_get_type(type);
                    data.kernel_type = (char*)calloc((int)type_str.size() + +(int)elem.size() + 3, sizeof(char));
                    strcpy((char*)data.kernel_type, "/");
                    strcat((char*)data.kernel_type, type_str.c_str());
                    strcat((char*)data.kernel_type, "/");
                    strcat((char*)data.kernel_type, elem.c_str());

                    data.input_type = (char*)calloc((int)elem.size() + 2, sizeof(char));
                    strcpy((char*)data.input_type, "/");
                    strcat((char*)data.input_type, elem.c_str());

                    data.format_info = bitmask;
                    data.needs_posttreatment = 0;

                    data.nb_cards_per_object = -1;
                    data.keyword_size = (int)strlen(data.keyword);
                    data.p_descr = a_cfg_kernel->GetDescriptorHandleFromKeyword(type, list.at(i));
                    if (data.p_descr)
                    {
                        int a_format = *format;
                        const fileformat_t* a_format_p = data.p_descr->getDynaFileFormatPtr((MvFileFormat_e*)&a_format);
                        data.p_format = a_format_p;
                        data.format = a_format;
                    }
                    else
                    {
                        data.p_format = NULL;
                        data.format = 0;
                    }
                    keysolverinfo.second = data;
                    objectsolverinfo.push_back(keysolverinfo);
                }
            }
        }
        else
        {
            FillSolverInfoRecursively(type, *it, objectsolverinfo, format, inHMFlag, from_import);
        }
    }
}
