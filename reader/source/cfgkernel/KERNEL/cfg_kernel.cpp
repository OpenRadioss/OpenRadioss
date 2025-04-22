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
#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>
#include <UTILS/file_utils.h>
#include <UTILS/system_utils.h>
#include <UTILS/error.h>
#include <UTILS/mv_cstring.h>
#include "mv_kernel_constants.h"
#include "cfg_kernel.h"
#include "mv_data_cfg_parser.h"
#include "mv_pre_datas_hierarchy.h"
#include "HCDI/hcdi_mv_descriptor.h"
#include "mv_kernel.h"
#include "mv_pre_descriptor.h"
#include <UTILS/mv_iostream.h>
#include <fstream>
#include <sstream>
#include <cctype>

#include <assert.h>
#define MSG_62 "ERROR: Environment variable %s is not set"

#ifdef OS_UNIX
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#endif

#if defined(_WIN32) || defined(WIN32)
#include <direct.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <windows.h>
#define mkdir(dir, mode) _mkdir(dir)
#endif

using namespace std;

typedef vector<MvPreDatasHierarchy_t*> MvPreDatasHierarchyList_t;

static void loc_recursive_add_configs(int etype, vector< std::pair<unsigned int, string> >& aListConfig, const MvPreDatasHierarchy_t* data);
static  void loc_recursive_add_config_type_descriptor(int etype, vector< std::pair<string, const IDescriptor*> >& aListConfigIDescriptor, const MvPreDatasHierarchy_t* data);
static void loc_recursive_get_idpool(int etype, int hm_config_type, int hm_type, string& cardimage, int* id_pool, const MvPreDatasHierarchy_t* data, bool* if_found);
static void loc_recursive_get_keyword_list(int etype, string& query_string, string& cardimage, vector<string>& keyword_list, const MvPreDatasHierarchy_t* data, bool* if_found);
static void loc_recursive_get_ikeyword_type_map(const MvPreDatasHierarchy_t* data_cfg_p, obj_type_e type, map<string, obj_type_e>& keyword_type_map);
static void loc_recursive_get_type_has_subtype(int etype, const MvPreDatasHierarchy_t* data, bool* if_found);
static void loc_check_recursive_id_pool(int etype, const MvPreDatasHierarchy_t* data, bool* has_id_pool);
static bool GetVerDirForGvnVersion(const string& profle, string& subversion, string& verdir_out, ApplicationMode_e& appmode_out);
static int CheckRecursively(const string& attrib, const MvPreDatasHierarchy_t* data, set<string>& namelist, set<int>& idlist, vector<string>& a_vect, obj_type_e otype);


class CUserNameTypeInfo;


// Single lookup table that supports both full and partial searches
map<ApplicationMode_e, map<string, MvFileFormat_e>> fileformat_lookupTable;


static string getFinalUsernameString(string username0, string usernamei, char sep, char hd) {
    // Handle empty strings
    if (username0.empty()) {
        return usernamei;
    }
    else if (usernamei.empty()) {
        return username0;
    }

    // Ensure username0 doesn't end with a double slash 
    if (!username0.empty() && username0.back() == sep) {
        username0.pop_back();
    }

    // Check if usernamei starts with a slash using a range-based for loop
    bool startsWithSlash = false;
    for (char c : usernamei) {
        if (c == sep) {
            startsWithSlash = true;
            break;
        }
        break; // Exit the loop if a non-slash character is found
    }
    if (!startsWithSlash)
        usernamei = string(1, hd) + usernamei;

    // Check if "username0" directory exists within "usernamei"
    size_t pos = usernamei.find(username0);
    if (pos != string::npos)
        return usernamei;
    usernamei[0] = sep;
    return username0 + usernamei;
}

static string extractNumericPart(const string& str)
{
    size_t pos = str.find_last_not_of("0123456789"); 
    if (pos == string::npos || pos == str.length() - 1)
        return ""; // No digits found
    return str.substr(pos + 1); // Extract numeric part
}

static void buildfileformatLookup(map<ApplicationMode_e, string>& appfolderhierachy, bool again=false);
void CFGKernel::getlKeywordListInfo(obj_type_e etype, const MvPreDatasHierarchy_t* data_cfg_p, std::map<std::string, CUserNameTypeInfo>& keywordlist) const
{
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
            getlKeywordListInfo(etype, *it, keywordlist);
        }
        else
        {
            const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
            if (psubtype)
            {
                const MvStringList_t& list = psubtype->getUserNameList();
                int idpool = (int)psubtype->getIdPool();
                int size = (int)list.size();
                SyntaxInfo* pinfo = getSyntaxInfo();
                set<string> sh_set;
                std::vector<cfgkernel::Variant> h_lst;
                if (pinfo)
                {
                    if (list.size())
                    {
                        h_lst = pinfo->getArrayAttribute("HEADER");
                    }
                }
                char hh;
                
                size_t h_size = h_lst.size();

                if(h_lst.size())
                    hh = h_lst[0].getStringValue()[0];

                int flags = (*it)->getFlags();

                for (int i = 0; i < size; i++)
                {
                    const IDescriptor* pdescrp = (const IDescriptor*)get_descriptor(etype, list.at(i));
                    //keywordlist.push_back(myKeywordInfo(hh + list.at(i), (unsigned int)etype, idpool));
                    
                    //keywordlist[hh + list.at(i)]  = myKeywordInfo(hh + list.at(i), (unsigned int)etype, idpool);
                    keywordlist[hh + list.at(i)] = CUserNameTypeInfo(etype, flags, (hh + list.at(i)).size(), sh_set, pdescrp, hh + list.at(i));
                }
            }
        }
    }
}

//loc_update_solverinfo_recursively
void CFGKernel::UpdateSolverInfoRecursively(obj_type_e etype, const MvPreDatasHierarchy_t* data_cfg_p, std::map<string, CUserNameTypeInfo>& objectsolverinfo, MvStringList_t& ulist) const
{
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
            const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
            int sz = 0;
            if (psubtype)
            {
                ulist.push_back(psubtype->getUserNameList()[0]);
                sz = (int)ulist.size();
            }
            UpdateSolverInfoRecursively(etype, *it, objectsolverinfo, ulist);

            if (psubtype && sz)
            {
                for (int i = sz - 1; i < ulist.size(); i++)
                {
                    ulist.pop_back();
                }
            }
        }
        else
        {
            const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
            if (psubtype)
            {
                const MvStringList_t& list = psubtype->getUserNameList();
                int size = (int)list.size();

                for (int i = 0; i < size; i++)
                {
                    string elem = list.at(i);
                    string keyword = elem;
                    SyntaxInfo* pinfo = getSyntaxInfo();
                    set<string> sh_set;
                    if (pinfo)
                    {
                        if (ulist.size() == 1)
                        {
                            std::vector<cfgkernel::Variant> h_lst = pinfo->getArrayAttribute("HEADER");
                            if (h_lst.size())
                            {
                                cfgkernel::Variant var = pinfo->getAttribute("SEPARATOR");
                                string sval = var.getStringValue();
                                char hh = h_lst[0].getStringValue()[0];
                                keyword = getFinalUsernameString(ulist[ulist.size() - 1], elem, sval[0], hh);
                                keyword.erase(0, 1);
                            }
                        }
                        std::vector<cfgkernel::Variant> sh_lst = pinfo->getArrayAttribute("STRIP_HEADER");

                        if (sh_lst.size())
                        {
                            for (int j = 0; j < sh_lst.size(); j++)
                            {
                                string sval = sh_lst[j].getStringValue();
                                sh_set.insert(sval);
                            }
                        }
                    }
                    const IDescriptor* pdescrp = (const IDescriptor*)get_descriptor(etype, elem);
                    objectsolverinfo[keyword] = CUserNameTypeInfo(etype, flags, keyword.length(), sh_set, pdescrp, elem);
                }
            }
        }
    }
}

void GetSolverFolderHierachy(map<ApplicationMode_e, string>& appfolderhierachy, map<ApplicationMode_e, string> *append_map)
{
    static map<ApplicationMode_e, string>  app_folder_hierachy;
    static bool first = false;
    if (false == first)
    {
        app_folder_hierachy[HCDI_SOLVER_LSDYNA]     = "Keyword971_R16.0:Keyword971_R15.0:Keyword971_R14.1:Keyword971_R14.0:Keyword971_R13.1:Keyword971_R13.0:Keyword971_R12.0:Keyword971_R11.2:Keyword971_R11.1:Keyword971_R11.0:Keyword971_R10.1:Keyword971_R9.3:Keyword971_R9.0:Keyword971_R8.0:Keyword971_R7.1:Keyword971_R6.1:Keyword971";
        app_folder_hierachy[HCDI_SOLVER_RADIOSS]    = "radioss2026:radioss2025:radioss2024:radioss2023:radioss2022:radioss2021:radioss2020:radioss2019:radioss2018:radioss2017:radioss140:radioss130:radioss120:radioss110:radioss100:radioss90:radioss51:radioss44:radioss42:radioss41";
        buildfileformatLookup(app_folder_hierachy);
        first = true;
    }
    if (append_map)
    {
        app_folder_hierachy.insert(append_map->begin(), append_map->end());
        buildfileformatLookup(app_folder_hierachy, true);
    }
    appfolderhierachy = app_folder_hierachy;
}


// Build lookup table only once
void buildfileformatLookup(map<ApplicationMode_e, string>& appfolderhierachy, bool again)
{
    static bool initialized = false;
    if (initialized && !again) return;
    initialized = true; //only one time...


    for (const auto& pair : appfolderhierachy)
    {
        istringstream ss(pair.second);
        string token;
        while (getline(ss, token, ':'))
        {
            MvFileFormat_e ff = MV_get_file_format(token);
            if (ff != FF_UNKNOWN)
            {
                fileformat_lookupTable[pair.first][token] = ff; // Full match
                string numericPart = extractNumericPart(token);
                if (!numericPart.empty())
                {
                    fileformat_lookupTable[pair.first][numericPart] = ff; // Numeric part match
                }
            }
        }
    }
}

// Retrieves the fileformat enum for a given applicationmode and numeric/alpha-numeric partial string
MvFileFormat_e CFGKernelGetFileFormatFromString(ApplicationMode_e app, const string& partial_ff)
{
    auto it = fileformat_lookupTable[app].find(partial_ff);
    if (it != fileformat_lookupTable[app].end())
    {
        return it->second;
    }
    return FF_UNKNOWN;
}






bool GetVerDirForGvnVersion(const string& profile, string& subversion, string& verdir_out, ApplicationMode_e& appmode_out)
{
    map<ApplicationMode_e, string>  app_folder_hierachy;
    GetSolverFolderHierachy(app_folder_hierachy);

    map<ApplicationMode_e, string>::iterator itr_b = app_folder_hierachy.begin();
    map<ApplicationMode_e, string>::iterator itr_e = app_folder_hierachy.end();
    map<ApplicationMode_e, string>::iterator itr;
    for (itr = itr_b; itr != itr_e; ++itr)
    {
        ApplicationMode_e appmode = (*itr).first;
        string  vers = (*itr).second;
        std::size_t found = vers.find(subversion);
        if (found != std::string::npos)
        {
            appmode_out = appmode;
            verdir_out = vers.substr(found);
            return true;
        }
    }
    return false;
}



static const char* loc_get_msg(int ind) { return MV_get_msg_array(MSGT_KERNEL)[ind]; }

static int
loc_setenv(const char* name, const char* value, int overwrite)
{
    char* name_and_value = NULL;
    int length = 0;

    if (overwrite == 0)
    {
        if (getenv(name) != NULL) return 0;
    }

    if (value == NULL) return -1;

    length = (int)strlen(name) + (int)strlen(value) + 2;
    name_and_value = (char*)calloc(length, sizeof(char));
    if (name_and_value == NULL) return -1;

    sprintf(name_and_value, "%s=%s", name, value);

    /*
        string str_name(name);
        string str_val(value);
        string strname_value = str_name + "=" + str_val;
    */
    return putenv(name_and_value);

    /* if I understand the man of putenv, we must not free name_and_value! */
}



static MvDirList_t* loc_split_dir_list(const string& dirs, char separator, MvDirList_t* dir_list_p) 
{
    MvDirList_t* a_dir_list_p = (dir_list_p == NULL ? new MvDirList_t() : dir_list_p);
    //
    int a_end_pos = (int)(dirs.size());
    int a_cur_pos = 0;
    while (a_cur_pos < a_end_pos) {
        string a_cur_dir = "";
        while (a_cur_pos < a_end_pos && dirs[a_cur_pos] != separator) a_cur_dir += dirs[a_cur_pos++];
        if (a_cur_pos < a_end_pos) ++a_cur_pos;
        if (!a_cur_dir.empty()) a_dir_list_p->push_back(a_cur_dir);
    }
    /*
    cout << "Splitting " << dirs << endl;
    MvDirList_t::iterator a_it_begin = a_dir_list_p->begin();
    MvDirList_t::iterator a_it_end   = a_dir_list_p->end();
    MvDirList_t::iterator a_it;
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) cout << "-> " << *a_it << endl;
    */
    return a_dir_list_p;
}

/* need_to_be_checked */
CFGKernel::CFGKernel()
{

}

CFGKernel::CFGKernel(const string& pathHome, const string& profile, const string& version, const string& userDefinedDHFilename) :
                     p_pathHome(pathHome), p_latestVersion(version), p_userDefinedDHFilename(userDefinedDHFilename), p_profile(profile)
                     
{
    LoadCFGKernel();
}
CFGKernel::CFGKernel(const string& pathHome, const string& profile, const string& version, const vector<string>& allowed_flags, const string& userDefinedDHFilename) :
    p_pathHome(pathHome), p_latestVersion(version), p_userDefinedDHFilename(userDefinedDHFilename), p_profile(profile)

{
    if (allowed_flags.size() == 0)
        assert(0);
    for (int i = 0; i < allowed_flags.size(); i++)
        set_default_data_hierarchy_flag(allowed_flags[i]);
    LoadCFGKernel();
}
CFGKernel::~CFGKernel() 
{
    close_kernel();
}

int CFGKernel::LoadCFGKernel()
{
    int error = 0;
    //"HM_MSG_DIR"
    if (p_pathHome.size() > 0)
    {
        string hm_msg_dir = p_pathHome + string("/messages");
        error = loc_setenv("HM_MSG_DIR", hm_msg_dir.c_str(), 0);
        if (error == -1)
        {
            throw "HM_MSG_DIR not set";
        }
    }
    //"HM_MV_CFG_DIR"
    if (p_pathHome.size() > 0)
    {
        p_cfgDirPath = p_pathHome + string("/config/CFG");
    }
    else
    {
        const char* envvar = getenv("HM_MV_CFG_DIR");
        if (envvar) p_cfgDirPath = envvar;
    }
    //"HM_MV_UNITS_DIR"
    if (p_pathHome.size() > 0)
    {
        p_unitsDirPath = p_pathHome + string("/config/CFG/UNITS");
    }
    else
    {
        const char* envvar = getenv("HM_MV_UNITS_DIR");
        if (envvar) p_unitsDirPath = envvar;
    }
    //folder hierachy 
    map<ApplicationMode_e, string>  app_folder_hierachy;
    GetSolverFolderHierachy(app_folder_hierachy);

    ApplicationMode_e   app_mode;

    string str_version(p_latestVersion);

    p_verDirVar = "";
    if (!GetVerDirForGvnVersion(p_profile, str_version, p_verDirVar, app_mode))
    {
        throw "Profile not supported";
    }
    if (p_verDirVar == "")
        throw "Sub-Profile not supported";

    loc_setenv("HM_MV_VER_DIR", p_verDirVar.c_str(), 1);

    p_subUserProfile = MV_get_file_format(str_version);
    p_userProfile = app_mode;

    mv_init_kernel();

    if (p_latestVersion != "")
    {
        char* path_usr = NULL;
        path_usr = getenv("HM_USER_DH_DIR");
        if (path_usr)
        {
            char a_path_usr[4096];
            strcpy(a_path_usr, path_usr);
            strcat(a_path_usr, "/tree_hierarchy_domain.cfg");
            FILE* file = fopen(a_path_usr, "r");
            if (NULL != file)
            {
                fclose(file);
                get_userdefined_hierarchy(1, a_path_usr);
            }
        }
    }
    if (p_userProfile ==  HCDI_SOLVER_RADIOSS || p_userProfile == HCDI_SOLVER_LSDYNA)
        InitAttributeSolverNames();

    LoadUserDHTree();

    return 0;
}

const MvDirList_t& CFGKernel::get_config_dirs()
{
    if (!p_cfg_dirs.size())
    {
        string a_usr_dir = mygetenv(USR_DIR_VAR);
        if (a_usr_dir != "") p_cfg_dirs.push_back(a_usr_dir);
        /*
        string a_script_dir = mygetenv("HC_SCRIPT_DIR");
        if (a_script_dir != "") p_cfg_dirs.push_back(a_script_dir);
        */
        p_cfg_dirs.push_back(p_cfgDirPath);
    }
    return p_cfg_dirs;
}

MvDirList_t& CFGKernel::get_version_dirs() 
{ 
    p_version_dirs.clear();
    loc_split_dir_list(p_verDirVar, ':', &p_version_dirs);
    return p_version_dirs;
}

void CFGKernel::mv_init_kernel() {
    try {
        //MV_add_memory_report("Before initializing kernel");
        //
        string a_var;
        //a_var = MV_get_config_dir();
        const MvDirList_t &a_cfg_dirs = get_config_dirs();
        a_var = a_cfg_dirs[a_cfg_dirs.size() - 1];

        if (a_var == "") throw MvError_t(MSG_62, CFG_DIR_VAR);

        MvDirList_t &a_versions = get_version_dirs();
        if(!a_versions.size())  throw MvError_t("Please check for versions not set correctly");

#if defined DEBUG || defined _DEBUG
        if ((strstr(a_var.c_str(), "config/CFG") == NULL) &&
            (strstr(a_var.c_str(), "config\\CFG") == NULL)) throw MvError_t("Please, \"HM_MV_CFG_DIR\" must map newly \"../config/CFG\" folder");
#endif
        setLatestVersion(a_versions[0]);
        a_var =  a_versions[0];  //MV_get_latest_version(false, true); /*multimodel need_to_change_member function*/
        if (a_var == "") throw MvError_t(loc_get_msg(50));

        if (p_unitsDirPath == "")
            throw MvError_t("HM_MV_UNITS_DIR environment variable is not set correctly. Please check.");
        string unit_dir = p_unitsDirPath + "/units.cfg";
        MU_init_dimensions(unit_dir);  /*multimodel need_to_change only once for all*/

        init_pre_descriptors();

        init_descriptors();
        init_model_descriptors();
        init_trees_cfg();

        
        //loc_load_key_file();         
        
        //
        //MV_init_adhesive_objects_list(); 
        
        //MV_add_memory_report("After initializing kernel");
        
    }
    catch (MvError_t& a_error) {
        throw a_error;
    }
}

void  CFGKernel::GetMultiObjectTypes(const IDescriptor* descrp, int ikeyword, MvFullTypeSet_t& set) const
{
    if (!descrp || ikeyword <= 0)
        return;
    const descriptor_t* cdescr_p = descrp->getDescriptorPtr();
    if (cdescr_p == NULL)
        return;
    object_descriptor_t* objdescr_p = (object_descriptor_t*)(cdescr_p->attdescr_array[ikeyword]);
    if (objdescr_p == NULL)
        return;

    for (int i = 0; i < objdescr_p->num; i++)
    {
        if (objdescr_p->subtypes[i])
        {
            const MvSubtype_t* subtype = get_subtype(objdescr_p->allowed_types[i], objdescr_p->subtypes[i]);
            if (subtype == NULL)
            {
                vector<string> key_vect;
                string string_val = objdescr_p->subtypes[i];
                getChildKeywordListForGivenKeyword(objdescr_p->allowed_types[i], string_val, key_vect);
                vector<string>::iterator iter_b = key_vect.begin();
                vector<string>::iterator iter_e = key_vect.end();
                vector<string>::iterator iter;
                for (iter = iter_b; iter != iter_e; ++iter)
                {
                    string keyw = *iter;
                    MvFullType_t ftype(*this, objdescr_p->allowed_types[i], keyw);
                    set.insert(ftype);
                }
            }
            else
            {
                // insert subtype if found
                MvFullType_t ftype(*this, objdescr_p->allowed_types[i], objdescr_p->subtypes[i]);
                set.insert(ftype);
            }
        }
        else
        {
            MvFullType_t ftype(objdescr_p->allowed_types[i]);
            set.insert(ftype);
        }
    }
}

MvPreDatasHierarchy_t* CFGKernel::get_userdefined_hierarchy(int option, string filename) const
{
    //
    switch (option) {
    case 1:  // Creation
        if (p_pUserDefinedHierarchy != NULL) delete p_pUserDefinedHierarchy;
        if (filename != "")
        {
            p_pUserDefinedHierarchy = MvDataCfgParser_t(filename).getDatasHierarchyPtr((CFGKernel&)*this, "ROOT");
            return p_pUserDefinedHierarchy;
        }
        break;
    case 0:
    {
        if (p_pUserDefinedHierarchy != NULL)  return p_pUserDefinedHierarchy;
        break;
    }
    case -1: // Destruction
        if (p_pUserDefinedHierarchy != NULL) {
            delete p_pUserDefinedHierarchy;
            p_pUserDefinedHierarchy = NULL;
        }
        break;
    default:
        break;
    }
    //
    return p_pUserDefinedHierarchy;
}


/****************************/





IDescriptor* CFGKernel::GetDescriptorHandle(const char* fulltype) const
{
    const MvDescriptor_t* descrp = get_descriptor(string(fulltype));
    return (IDescriptor*)descrp;
}

IDescriptor* CFGKernel::GetDescriptorHandleFromKeyword(int type, const string& keyword) const
{
    const MvDescriptor_t* descrp = get_user_descriptor((object_type_e)type, keyword);
    return (IDescriptor*)descrp;
}

IDescriptor* CFGKernel::GetDescriptorHandleUserKeywordFromKeyword(int type, int config_type, int hm_type, const string& keyword, string& first_user_keyword) const
{
    const MvDescriptor_t* descrp = get_user_descriptor_userkeyword((object_type_e)type, config_type, hm_type, keyword, first_user_keyword);
    return (IDescriptor*)descrp;
}

void CFGKernel::GetUserKeywordFromConfigHmType(int type, int config_type, int hm_type, string& first_user_keyword) const
{
    get_user_descriptor_userkeyword((object_type_e)type, config_type, hm_type, first_user_keyword);
}

IDescriptor* CFGKernel::GetDescriptorHandleFromUserID(int type, int user_id) const
{
    const MvDescriptor_t* descrp = get_user_descriptor((object_type_e)type, user_id);
    return (IDescriptor*)descrp;
}


IDescriptor* CFGKernel::GetDescriptorHandleFromHMConfigHMType(int type, int hm_config_type, int hm_type) const
{
    const MvDescriptor_t* descrp = get_user_descriptor((object_type_e)type, hm_config_type, hm_type);
    return (IDescriptor*)descrp;
}

IDescriptor* CFGKernel::GetDescriptorHandleFromType(int type) const
{
    const MvDescriptor_t* descrp = get_descriptor((object_type_e)type);
    return (IDescriptor*)descrp;
}
IDescriptor* CFGKernel::GetDescriptorHandleFromFullType(const MvFullType_t& fulltype) const
{
    const MvDescriptor_t* descrp = get_descriptor(fulltype);
    return (IDescriptor*)descrp;
}


object_type_e CFGKernel::get_entitytype(const string& keyword) const
{
    return MV_get_type(keyword);
}
const string& CFGKernel::get_entitystringtype(int entity_type) const
{
    return MV_get_type((object_type_e)entity_type);
}

int CFGKernel::get_all_domains() const
{
    return MV_get_all_domains();
}




void CFGKernel::getlUserNamesSolverInfo(map<string, CUserNameTypeInfo>(&objectsolverinfo)[HCDI_OBJ_TYPE_HC_MAX], bool add_user_subtype) const
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;
    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();

    MvPreDatasHierarchyList_t::const_iterator it_end = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;
    MvStringList_t ulist;
    for (it = it_begin; it != it_end; ++it)
    {
        obj_type_e type = (*it)->getType();
        int nb_child = (*it)->getNbChildren();

        //if (ulist.size() > 1)
        ulist.clear(); // pop_back();

        if (!add_user_subtype || nb_child == 0)
        {
            const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
            if (psubtype)
            {
                std::map<string, CUserNameTypeInfo>& a_obj_sol_info = objectsolverinfo[type];
                const MvStringList_t& list = psubtype->getUserNameList();
                const MvKeywordSet_t& list_opt = psubtype->getOptionalHeaderStringList();
                SyntaxInfo* pinfo = getSyntaxInfo();
                set<string> sh_set;
                if (pinfo)
                {
                    std::vector<cfgkernel::Variant> sh_lst = pinfo->getArrayAttribute("STRIP_HEADER");
                    if (sh_lst.size())
                    {
                        for (int j = 0; j < sh_lst.size(); j++)
                        {
                            string sval = sh_lst[j].getStringValue();
                            sh_set.insert(sval);
                        }
                    }
                }

                int size = (int)list.size();
                int flags = (*it)->getFlags();
                for (int i = 0; i < size; i++)
                {
                    string username_str = list.at(i);
                    const IDescriptor* pdescrp = (const IDescriptor*)get_descriptor(type, list.at(i));
                    //keysolverinfo.second = CUserNameTypeInfo(type, flags, keysolverinfo.first.size(), sh_set, pdescrp, keysolverinfo.first);
                    objectsolverinfo[type][username_str]=CUserNameTypeInfo(type, flags, username_str.size(), sh_set, pdescrp, username_str);
                }
            }
        }
        else
        {
            const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
            if (psubtype)
                ulist = psubtype->getUserNameList();

            UpdateSolverInfoRecursively(type, *it, objectsolverinfo[type], ulist);
        }
    }
}


/************************/

void CFGKernel::getDiscreteKeywordInfoMap(std::map<std::string, std::vector<CUserNameTypeInfo>> &descrete_lst) const
{
    std::map<std::string, CUserNameTypeInfo> originalMap;
    getlKeywordListInfo(originalMap);
    for (auto& itr : originalMap) 
    {
        std::string key = itr.first;
        CUserNameTypeInfo& value = itr.second;

        std::string baseString;
        size_t pos = 0;
        while (pos != std::string::npos) 
        {
            pos = key.find_first_of('_', pos); // get from syntax_info.cfg
            if (pos == std::string::npos) {
                vector<CUserNameTypeInfo>& vec = descrete_lst[key];
                vec.push_back(value);
                break;
            }
            baseString = key.substr(0, pos);
            auto it = descrete_lst.find(baseString);
            if (it != descrete_lst.end()) { // Check if key exists
                it->second.push_back(value);
            }
            else {
                vector<CUserNameTypeInfo> &vec = descrete_lst[baseString];
                vec.push_back(value);
            }
            ++pos;
        }
    }
}
// Function to recreate map based on base string (optimized)
void CFGKernel::getBaseStringKeywordSingleTypeMap(std::map<std::string, CUserNameTypeInfo>& resultMap) const
{
    std::map<std::string, std::vector<CUserNameTypeInfo>> descrete_lst;
    std::map<std::string, CUserNameTypeInfo> originalMap;
    getlKeywordListInfo(originalMap);
    getDiscreteKeywordInfoMap(descrete_lst);
    for (const auto& itr : originalMap) {
        // Access key and value using iterator
        const std::string& key = itr.first;
        //const MyEnum& value = itr.second;
        CUserNameTypeInfo value = itr.second;

        std::string baseString;
        size_t pos = 0;
        bool do_continue = true;
        while (do_continue) {
            pos = key.find_first_of('_', pos);
            if (pos == std::string::npos) {
                baseString = key;
                do_continue = false;
            }
            else
                baseString = key.substr(0, pos);

            // Check if all keys with this base string have the same value (optimized)
            bool allSameValue = true;
            auto it = descrete_lst.find(baseString);
            if (it != descrete_lst.end()) {
                const CUserNameTypeInfo& firstValue = *it->second.begin();
                for (const CUserNameTypeInfo& enumValue : it->second) {
                    if (enumValue.obj_type != firstValue.obj_type) {
                        allSameValue = false;
                        break;
                    }
                }
            }

            if (allSameValue) {
                resultMap[baseString] = value; // Add base string with same value
            }
            ++pos;
        }
        // If no base string found, add the full key
        if (baseString.empty()) {
            resultMap[key] = value;
        }
    }
}

void CFGKernel::getlKeywordListInfo(std::map<std::string, CUserNameTypeInfo>  &keywordlist) const
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;
    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();

    MvPreDatasHierarchyList_t::const_iterator it_end = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;
    MvStringList_t ulist;
    for (it = it_begin; it != it_end; ++it)
    {
        obj_type_e type = (*it)->getType();
        int nb_child = (*it)->getNbChildren();

        if (ulist.size() > 1)
            ulist.pop_back();

        if (nb_child == 0)
        {
            const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
            if (psubtype)
            {

                const MvStringList_t& list = psubtype->getUserNameList();
                const MvKeywordSet_t& list_opt = psubtype->getOptionalHeaderStringList();
                SyntaxInfo* pinfo = getSyntaxInfo();
                set<string> sh_set;
                if (pinfo)
                {
                    std::vector<cfgkernel::Variant> sh_lst = pinfo->getArrayAttribute("STRIP_HEADER");
                    if (sh_lst.size())
                    {
                        for (int j = 0; j < sh_lst.size(); j++)
                        {
                            string sval = sh_lst[j].getStringValue();
                            sh_set.insert(sval);
                        }
                    }
                }
                unsigned int idpool = (unsigned int)psubtype->getIdPool();
                int size = (int)list.size();
                int flags = (*it)->getFlags();
                for (int i = 0; i < size; i++)
                {
                    const IDescriptor* pdescrp = (const IDescriptor*)get_descriptor(type, list.at(i));
                    //keywordlist.push_back(myKeywordInfo(list.at(i), (unsigned int)type, idpool));

                    keywordlist[list.at(i)] = CUserNameTypeInfo(type, flags, list.at(i).size(), sh_set, pdescrp,
                                                                list.at(i), idpool);
                    //keywordlist[list.at(i)] = myKeywordInfo(list.at(i), (unsigned int)type, idpool);
                }
            }
        }
        else
        {
            getlKeywordListInfo(type, *it, keywordlist);
        }
    }
}



/**************************/

void CFGKernel::getEntityOptionalStrings(int etype, const set<string> **p_opt_strings) const
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;
    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();

    MvPreDatasHierarchyList_t::const_iterator it_end = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        obj_type_e type = (*it)->getType();
        if(type == etype)
        {
            const MvSubtype_t* psubtype = (*it)->getSubtypePtr();
            if (psubtype)
            {
                *p_opt_strings = &(psubtype->getOptionalHeaderStringList());
            }
        }
    }
}
//dir_type_e CFGKernel::get_direction(const char* dir, int is_extended)
//{
//    return MV_get_direction(dir, is_extended);
//}
//
//const char* CFGKernel::get_direction_str(dir_type_e dir, int is_extended)
//{
//    return MV_get_direction_str(dir, is_extended);
//}
//int  CFGKernel::get_directions(const char* dir, int* xdir_p, int* ydir_p, int* zdir_p)
//{
//    return MV_get_directions(dir, xdir_p, ydir_p, zdir_p);
//}
//
///** Gets a string from direction flags */
//const char* CFGKernel::get_directions_str(int xdir, int ydir, int zdir)
//{
//    return MV_get_directions_str(xdir, ydir, zdir);
//}

void loc_recursive_add_configs(int etype, vector< std::pair<unsigned int, string> >& aListConfig, const MvPreDatasHierarchy_t* data)
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
                loc_recursive_add_configs(etype, aListConfig, *it);
            }
            else
            {
                string key = (*it)->getKeyword();
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

void CFGKernel::getAllConfigTypes(int etype, vector< std::pair<unsigned int, string> >& aListConfig) const
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;

    loc_recursive_add_configs(etype, aListConfig, a_data_cfg_p);
}

void loc_recursive_add_config_type_descriptor(int etype, vector< std::pair<string, const IDescriptor*> >& aListConfigIDescriptor, const MvPreDatasHierarchy_t* data)
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

void CFGKernel::GetEntityDescriptors(int etype, vector< std::pair<string, const IDescriptor*> >& aListConfigIDescriptor) const
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;

    loc_recursive_add_config_type_descriptor(etype, aListConfigIDescriptor, a_data_cfg_p);
}

void CFGKernel::GetAllTypesTitle(vector< std::pair<obj_type_e, string> >& vec_type_title) const
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;

    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        string skey = (*it)->getTitle();
        obj_type_e a_type = (*it)->getType();
        vec_type_title.push_back(std::make_pair(a_type, skey));
    }
}

void CFGKernel::GetUserDiscreteListFromType(unsigned int etype, map < string, vector< string> >& mapDiscreteList) const
{
    GetDiscreteListFromType(etype, true, mapDiscreteList);
}
void CFGKernel::GetInternalDiscreteListFromType(unsigned int etype, map < string, vector< string> >& mapDiscreteList) const
{
    GetDiscreteListFromType(etype, false, mapDiscreteList);
}

void CFGKernel::GetDiscreteListFromType(unsigned int etype, bool isuser, map < string, vector< string> >& mapDiscreteList) const
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = NULL;

    if (isuser)
        a_data_cfg_p = get_userdefined_hierarchy(0);
    else
        a_data_cfg_p = get_datastreehierarchy(DTT_ASSIGNED);

    if (NULL == a_data_cfg_p)
        return;
    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        string skey = (*it)->getTitle();
        obj_type_e a_type = (*it)->getType();

        if ((unsigned int)a_type == etype)
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

void CFGKernel::GetConfigHMTypeCardImageFromKeyword(unsigned int etype, string& keyword, int* configType, int* hmType, string& cardImage) const
{
    MvSubtypePtrSet_t a_subtypes;
    get_subtypes((obj_type_e)etype, &a_subtypes);
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

MvKeywordSet_t* CFGKernel::getKeywords(object_type_e otype, const MvSubtype_t* subtype_p, MvKeywordSet_t* keywords_p) const
{
    if (!subtype_p)
        return nullptr;
    MvKeywordSet_t* a_keywords_p = (keywords_p == NULL ? new MvKeywordSet_t() : keywords_p);
    //


    const MvTypeKeywordSubtypeMap_t& a_tksm = p_subtype_map;// get_subtype_map(); /*mutimodel check for alternative*/
    MvTypeKeywordSubtypeMap_t::const_iterator  a_tksm_it = a_tksm.find(otype);
    if (a_tksm_it == a_tksm.end()) return NULL;
    //
    const MvKeywordSubtypeMap_t& a_ksm = (*a_tksm_it).second;
    MvKeywordSubtypeMap_t::const_iterator a_ksm_it_begin = a_ksm.begin();
    MvKeywordSubtypeMap_t::const_iterator a_ksm_it_end = a_ksm.end();
    MvKeywordSubtypeMap_t::const_iterator a_ksm_it;
    for (a_ksm_it = a_ksm_it_begin; a_ksm_it != a_ksm_it_end; ++a_ksm_it) {
        const string& a_keyword = (*a_ksm_it).first;
        const MvSubtype_t* a_subtype_p = (*a_ksm_it).second;
        //
        if (a_subtype_p == subtype_p) a_keywords_p->insert(a_keyword);
    }
    //
    return a_keywords_p;
}



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

void CFGKernel::getIdPool(int etype, int hm_config_type, int hm_type, string& cardimage, int* id_pool) const
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;

    bool if_found = false;
    loc_recursive_get_idpool(etype, hm_config_type, hm_type, cardimage, id_pool, a_data_cfg_p, &if_found);
    if (*id_pool == -1)
    {
        *id_pool = 0;
    }
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
                                    if(size > 0)
                                        child_key = user_names.at(size - 1);
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

void CFGKernel::getChildKeywordListForGivenKeyword(int etype, string& keyword, vector<string>& keyword_list) const
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;

    bool if_found = false;
    string query_str = "KEYWORD";
    loc_recursive_get_keyword_list(etype, query_str, keyword, keyword_list, a_data_cfg_p, &if_found);
}

/* make sure etype has cardimage*/
void CFGKernel::getChildCardImageListForGivenKeyword(int etype, string& card_image, vector<string>& card_image_list)
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;

    bool if_found = false;
    string query_str = "CARDIMAGE";
    loc_recursive_get_keyword_list(etype, query_str, card_image, card_image_list, a_data_cfg_p, &if_found);
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

void CFGKernel::getKeywordTypeMap(map<string, obj_type_e>& keyword_type_map) const
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return;
    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;

    for (it = it_begin; it != it_end; ++it)
    {
        string skey = (*it)->getKeyword();
        obj_type_e a_type = (*it)->getType();

        keyword_type_map[skey] = a_type;
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

bool CFGKernel::getTypeHasSubtype(int etype) const
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return false;

    bool if_found = false;
    loc_recursive_get_type_has_subtype(etype, a_data_cfg_p, &if_found);
    return if_found;
}


bool CFGKernel::GetFirstUserNameForGvnEtypeUserID(const string& etype, int userid, string& firstusername) const
{
    const MvSubtype_t* subtype_p = get_user_subtype(etype, userid);

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

bool CFGKernel::GetFirstUserNameForGvnEtypeConfigHMType(const string& etype, int config, int hmtype, string& firstusername) const
{

    const MvSubtype_t* subtype_p = get_config_type_subtype(etype, config, hmtype);

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

bool CFGKernel::GetFirstUserNameForGvnEtypeKeyword(int etype, string& cardimage, string& firstusername) const
{
    const MvSubtype_t* subtype_p = get_subtype((object_type_e)etype, cardimage);

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
MvDataUncondScalarFeature_t* CFGKernel::GetAllocatedUnCondScalarFeatureHandle(const string& title, int  ikeyword, MvDimension_e dimension)
{

    MvDataUncondScalarFeature_t* a_feature_p = NULL;
    a_feature_p = new MvDataUncondScalarFeature_t(title, ikeyword, dimension);
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

bool CFGKernel::hasIdPool(int etype) const
{
    const MvPreDatasHierarchy_t* a_data_cfg_p = get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == a_data_cfg_p)
        return false;

    bool has_id_pool = false;
    loc_check_recursive_id_pool(etype, a_data_cfg_p, &has_id_pool);
    return has_id_pool;
}

string CFGKernel::mv_get_cfg_file(const string& relative_path) const {
    const MvDirList_t& a_cfg_dirs = p_cfg_dirs;// MV_get_config_dirs();
    const MvDirList_t& a_ver_dirs = p_version_dirs;// MV_get_version_dirs();
    //const string& a_cur_version = p_latestVersion;
    string             a_path = "";

    bool IFound = false;
    MvDirList_t::const_iterator a_ver_it_begin = a_ver_dirs.begin();
    MvDirList_t::const_iterator a_ver_it_end = a_ver_dirs.end();
    MvDirList_t::const_iterator a_ver_it;
    for (a_ver_it = a_ver_it_begin; a_ver_it != a_ver_it_end && !IFound; ++a_ver_it) {
        const string& a_ver_dir = (*a_ver_it);
        //
        MvDirList_t::const_iterator a_cfg_it_begin = a_cfg_dirs.begin();
        MvDirList_t::const_iterator a_cfg_it_end = a_cfg_dirs.end();
        MvDirList_t::const_iterator a_cfg_it;
        for (a_cfg_it = a_cfg_it_begin; a_cfg_it != a_cfg_it_end; ++a_cfg_it) {
            const string& a_cfg_dir = (*a_cfg_it);
            //
          //  if (0 >= MV_comp_radioss_version(a_ver_dir, a_cur_version)) { /*need to check ??*/
                string a_cur_path = a_cfg_dir + "/" + a_ver_dir + "/" + relative_path;
                if (access(a_cur_path.c_str(), F_OK) == 0) {
              //if(stat(a_cur_path.c_str(), &buffer) == 0) {
              //if(std::experimental::filesystem::exists(a_cur_path)) {
                    a_path = a_cur_path;
                    IFound = true;
                    break;
                }
      //      }
        }
    }
    //
    return a_path;
}

string CFGKernel::mv_get_cfg_file_at_pos(const string& relative_path, int pos) const {
    const MvDirList_t& a_cfg_dirs = p_cfg_dirs;// MV_get_config_dirs();
    string             a_path = "";
    if (pos >(int)a_cfg_dirs.size())
        return a_path;
    const MvDirList_t& a_ver_dirs = p_version_dirs;// MV_get_version_dirs();
    //const string& a_cur_version = p_latestVersion;
    bool IFound = false;
    MvDirList_t::const_iterator a_ver_it_begin = a_ver_dirs.begin();
    MvDirList_t::const_iterator a_ver_it_end = a_ver_dirs.end();
    MvDirList_t::const_iterator a_ver_it;
    const string& a_cfg_dir = a_cfg_dirs.at(pos);
    for (a_ver_it = a_ver_it_begin; a_ver_it != a_ver_it_end && !IFound; ++a_ver_it) {
        const string& a_ver_dir = (*a_ver_it);
        //
        //  if (0 >= MV_comp_radioss_version(a_ver_dir, a_cur_version)) { /*need to check ??*/
        string a_cur_path = a_cfg_dir + "/" + a_ver_dir + "/" + relative_path;
        if (access(a_cur_path.c_str(), F_OK) == 0) {
            //if(stat(a_cur_path.c_str(), &buffer) == 0) {
            //if(std::experimental::filesystem::exists(a_cur_path)) {
            a_path = a_cur_path;
            IFound = true;
            break;
        }
    }
    //
    return a_path;
}

int CFGKernel::LoadUserDHTree()
{
    if ("" == p_userDefinedDHFilename)
        return -1;

    FILE* file = fopen(p_userDefinedDHFilename.c_str(), "r");
    if (NULL != file)
    {
        fclose(file);
        get_userdefined_hierarchy(1, p_userDefinedDHFilename);
    }
    return 0;
}

void CFGKernel::WriteDataHierarchyContentsRecursively(FILE* fp, const MvPreDatasHierarchy_t* data, int* tab_counter) const
{
    MvPreDatasHierarchyList_t::const_iterator it_begin = data->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;
    for (it = it_begin; it != it_end; ++it)
    {
        string keyword = (*it)->getKeyword();
        string title = (*it)->getTitle();
        obj_type_e a_type = (*it)->getType();
        string s_type = MV_get_type(a_type);
        int old_tab_counter = *tab_counter;
        string tab = "";
        for (int i = 0; i < old_tab_counter; i++)
        {
            tab += "\t";
        }

        fprintf(fp, "%sHIERARCHY {\n", tab.c_str());
        fprintf(fp, "%s\tKEYWORD = %s;\n", tab.c_str(), keyword.c_str());
        fprintf(fp, "%s\tTITLE   = \"%s\";\n", tab.c_str(), title.c_str());
        fprintf(fp, "%s\tTYPE    = %s;\n", tab.c_str(), s_type.c_str());

        const MvSubtype_t* subtype_p = (*it)->getSubtypePtr();
        if (subtype_p)
        {
            fprintf(fp, "%s\tSUBTYPE = USER;\n", tab.c_str());
            char* cimage = subtype_p->getCardImage();
            if (cimage)
            {
                fprintf(fp, "%s\tCARD_IMAGE = %s;\n", tab.c_str(), cimage);
            }
            int user_id = subtype_p->getUserId();
            fprintf(fp, "%s\tUSER_ID = %d;\n", tab.c_str(), user_id);
            short int idpool = subtype_p->getIdPool();
            if (idpool > 0)
            {
                fprintf(fp, "%s\tID_POOL = %d;\n", tab.c_str(), idpool);
            }

            int hm_conf_type = subtype_p->getHMConfigType();
            if (hm_conf_type > 0)
            {
                fprintf(fp, "%s\tHM_CONFIG_TYPE = %d;\n", tab.c_str(), hm_conf_type);
            }

            int hm_type = subtype_p->getHMType();
            if (hm_type > 0)
            {
                fprintf(fp, "%s\tHM_TYPE = %d;\n", tab.c_str(), hm_type);
            }

            const MvPreDescriptor_t* pre_descr_p = get_user_pre_descriptor(a_type, -1, -1, keyword);
            if (pre_descr_p)
            {
                string filename = pre_descr_p->getFileName();
                fprintf(fp, "%s\tFILE = \"%s\";\n", tab.c_str(), filename.c_str());
            }

            MvStringList_t user_names = subtype_p->getUserNameList();
            int size = (int)user_names.size();
            if (size > 0)
            {
                fprintf(fp, "%s\tUSER_NAMES = (", tab.c_str());
                for (int i = 0; i < size; i++)
                {
                    string elem = user_names.at(i);
                    fprintf(fp, "%s", elem.c_str());
                    if (i == size - 1)
                        fprintf(fp, ");\n");
                    else
                        fprintf(fp, ",");
                }
            }
        }

        // loop on the children
        int nb_child = (*it)->getNbChildren();
        if (nb_child)
        {
            (*tab_counter)++;
            WriteDataHierarchyContentsRecursively(fp, *it, tab_counter);
            (*tab_counter)--;
        }
        fprintf(fp, "%s}\n", tab.c_str());
    }
}

// it should end with separator / in above case
void CFGKernel::WriteDataHierarchyContents(string& filepath) const
{
    string filename = filepath + "data_hierarchy.cfg";
    FILE* file = fopen(filename.c_str(), "a+");
    if (!file)
        return;

    const MvPreDatasHierarchy_t* data = get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == data)
    {
        fclose(file);
        return;
    }
    int tab_counter = 0;
    WriteDataHierarchyContentsRecursively(file, data, &tab_counter);
    fclose(file);

    map<string, obj_type_e> keyword_type_map;
    getKeywordTypeMap(keyword_type_map);
    int domains = HCDI_get_all_domains();
    vector<string> left_file_vect, right_file_vect, different_file_vect;
    map<string, obj_type_e>::iterator it_b = keyword_type_map.begin();
    map<string, obj_type_e>::iterator it_e = keyword_type_map.end();
    map<string, obj_type_e>::iterator it;
    for (it = it_b; it != it_e; ++it)
    {
        string keyword = it->first;
        obj_type_e a_type = it->second;

        const MvPreDescriptor_t* a_predescp = get_user_pre_descriptor(a_type, -1, -1, keyword);
        if (!a_predescp)
            continue;

        const MvDescriptor_t* desc_p = a_predescp->getDescriptorPtr((void *)this);
        if (desc_p)
        {
            int aikw = desc_p->getIKeyword("solverkeyword");
            if (aikw > END_ARGS)
            {
                int value = desc_p->getIdentifierValue(domains, aikw);
                if (value != -1)
                {
                    cout << "solverkeyword = -1, not defined in <<" << keyword << " file\n";
                }
            }
        }
        string cfgfilename = a_predescp->getFileName();

        std::string delimiter = "/";
        std::string token = cfgfilename.substr(0, cfgfilename.find(delimiter));

        string total_path1 = filepath + token;
        int check = mkdir(total_path1.c_str(), 0755);
        
        string total_path = filepath + cfgfilename;

        ofstream outfile;
        outfile.open(total_path, std::ofstream::out);
        PrintToFileDescriptor(a_type, keyword, outfile);
        outfile.close();
    }
}

void CFGKernel::WriteDescrContent(obj_type_e a_type, string& keyword, string& filename) const
{
    FILE* file = fopen(filename.c_str(), "w+");
    if (file)
    {
        fprintf(file, "Sample file\n");
        fclose(file);
    }

    ofstream outfile;
    outfile.open(filename, std::ofstream::out);
    PrintToFileDescriptor(a_type, keyword, outfile);
    outfile.close();
}

void CFGKernel::PrintToFileDescriptor(obj_type_e etype, string& keyword, std::ostream& os) const
{
    const MvDescriptor_t* descr_p = get_user_descriptor((object_type_e)etype, keyword);
    if (descr_p == NULL)
        return;

    descr_p->display(os);
}

void CFGKernel::PrintToFileAllDescriptors(int etype, std::ostream& os) const
{
    const MvDescriptor_t* descr_p = get_descriptor((object_type_e)etype);
    if (descr_p == NULL)
        return;
    descr_p->display(os);
}

int CFGKernel::CheckUniqueness(const string& attrib, vector<string>& my_vect, obj_type_e otype) const
{
    const MvPreDatasHierarchy_t* data = get_datastreehierarchy(DTT_ASSIGNED);
    if (NULL == data)
    {
        return 0;
    }

    set<string> namelist;
    set<int> idlist;
    int result = CheckRecursively(attrib, data, namelist, idlist, my_vect, otype);
    return result;
}

static int CheckRecursively(const string& attrib, const MvPreDatasHierarchy_t* data, set<string> &namelist, set<int> &idlist, vector<string>& a_vect, obj_type_e otype)
{
    int result = 0;
    MvPreDatasHierarchyList_t::const_iterator it_begin = data->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end = data->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;
    for (it = it_begin; it != it_end; ++it)
    {
        string keyword = (*it)->getKeyword();
        string title = (*it)->getTitle();
        obj_type_e a_type = (*it)->getType();
        string s_type = MV_get_type(a_type);
        if (otype != HCDI_OBJ_TYPE_NULL)
        {
            // skip which donot match
            if (otype != a_type)
                continue;
        }
        const MvSubtype_t* subtype_p = (*it)->getSubtypePtr();
        if (subtype_p)
        {
            if (attrib == "USERNAME")
            {
                MvStringList_t user_names = subtype_p->getUserNameList();
                int size = (int)user_names.size();
                if (size > 0)
                {
                    for (int i = 0; i < size; i++)
                    {
                        string elem = user_names.at(i);
                        if (namelist.empty())
                            namelist.insert(elem);
                        else
                        {
                            set<string>::iterator it = namelist.find(elem);
                            if (it != namelist.end())
                            {
                                string a_mssg = elem + " is duplicated in " + keyword + " for type " + s_type + "\n";
                                a_vect.push_back(a_mssg);
                                result++;
                            }
                            else
                            {
                                namelist.insert(elem);
                            }
                        }
                    }
                }
            }
            else if (attrib == "USERID")
            {
                int auserid = subtype_p->getUserId();
                if (auserid == -1)
                    continue;
                if(idlist.empty())
                    idlist.insert(auserid);
                else
                {
                    set<int>::iterator it = idlist.find(auserid);
                    if (it != idlist.end())
                    {
                        char userid[500];
                        sprintf(userid, "%d", auserid);
                        string id_str = (string)userid;
                        string a_mssg = id_str + " is duplicated in " + keyword + " for type " + s_type + "\n";
                        a_vect.push_back(a_mssg);
                        result++;
                    }
                    else
                    {
                        idlist.insert(auserid);
                    }
                }
            }
        }

        // loop on the children
        int nb_child = (*it)->getNbChildren();
        if (nb_child)
        {
            result+=CheckRecursively(attrib, *it, namelist, idlist, a_vect, otype);
            namelist.clear();
            idlist.clear();
        }
    }
    return result;
}

int CFGKernel::get_data_hierarchy_bitmask(const string& bit_string) const
{
    map<string, int>::const_iterator a_iter = p_flag_bit_map.find(bit_string);
    if (a_iter != p_flag_bit_map.end())
    {
        return a_iter->second;
    }
    return 0;
}
int CFGKernel::set_default_data_hierarchy_flag(const string& bit_string)
{
    int bit = 0, value=0;// get_data_hierarchy_bitmask(bit_string);
    map<string, int>::iterator a_iter = p_flag_bit_map.find(bit_string);
    if (a_iter != p_flag_bit_map.end())
    {
        bit = a_iter->second;
    }
    if (!bit)
    {
        if (p_next_counter_bit >= 32)
        {
            assert(0);
            p_flag_bit_map[bit_string] = 0;
            return 0;
        }
        p_next_counter_bit++;

        value = 1 << p_next_counter_bit;
        p_flag_bit_map[bit_string] = value;
    }
    else
        value = bit;

    p_default_bit = p_default_bit & value;
    if (p_default_bit == 0 && value != 0)
    {
        /*initialize for the first time*/
        p_default_bit = value;
    }
    return value;
}

int CFGKernel::get_default_bit() const
{
    return p_default_bit;
}

const MvSubtype_t* CFGKernel::get_subtype(const string& fulltype) const
{
    if (fulltype == "" || fulltype[0] != '/') {
        return nullptr;
    }
    else {
        string a_str = "";
        int i = 1, n = (int)(fulltype.size());
        while (i < n && fulltype[i] != '/') a_str += fulltype[i++];
        obj_type_e a_type = MV_get_type(a_str);
        a_str = "";
        if (i < n) {
            ++i;
            a_str = fulltype.substr(i, n - i);
            return get_subtype(a_type, a_str);
        }
    }
    return nullptr;
}
/***************************/
