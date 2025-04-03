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

#include <MUNITS/mu_dimension.h>
#include "hcdi_multicfgkernelmgr.h"
#include "KERNEL/mv_type.h"
#include <UTILS/error.h>
#include <UTILS/file_utils.h>
#include <stdio.h>
#include "boost/filesystem.hpp"   // includes all needed Boost.Filesystem declarations
#include <iostream>               // for std::cout
namespace fs = boost::filesystem;

MvFileFormat_e MultiCFGKernelMgr::CFGKernelSentinel::s_effSolver = FF_UNKNOWN;
int MultiCFGKernelMgr::CFGKernelSentinel::count = 0;


bool find_file(const fs::path& dir_path,         // in this directory,
    const std::string& file_name, // search for this name,
    fs::path& path_found)             // placing path here if found
{
    if (!exists(dir_path))
        return false;

    fs::directory_iterator end_itr; // default construction yields past-the-end

    for (fs::directory_iterator itr(dir_path); itr != end_itr; ++itr)
    {
        if (fs::is_directory(itr->status()))
        {
            if (find_file(itr->path(), file_name, path_found))
                return true;
        }
    }
    return false;
}


MultiCFGKernelMgr::~MultiCFGKernelMgr()
{
    for (int i = 0; i < FF_LAST; i++)
    {
        if (MultiCFGKernelMgr::getInstance().p_pcfgkernel[i])
        {
            delete MultiCFGKernelMgr::getInstance().p_pcfgkernel[i];
            MultiCFGKernelMgr::getInstance().p_pcfgkernel[i] =  nullptr;
        }
    }
    MU_close_dimensions();
    MV_delete_type_map();
}

void MultiCFGKernelMgr::operator=(MultiCFGKernelMgr const&)
{
     
}

MvFileFormat_e MultiCFGKernelMgr::P_GetEffectiveSolver() const
{
    return CFGKernelSentinel::GetEffectiveSolver() != FF_UNKNOWN ? CFGKernelSentinel::GetEffectiveSolver() : p_current_solver;
}

const CFGKernel *MultiCFGKernelMgr::GetCurrentCFGKernel()
{
    MvFileFormat_e userprofile = P_GetEffectiveSolver();
    if(userprofile >= FF_UNKNOWN && userprofile <= FF_LAST)
       return p_pcfgkernel[userprofile];
    
    return nullptr;
}

void MultiCFGKernelMgr::SetCFGKernel(int index, CFGKernel  *pcfgkernel)
{
    assert(index >= FF_UNKNOWN && index <= FF_LAST);
    if(p_pcfgkernel[index] && p_pcfgkernel[index] != pcfgkernel)
        delete p_pcfgkernel[index];
    p_pcfgkernel[index] = pcfgkernel;
}

CFGKernel* MultiCFGKernelMgr::GetCFGKernel(int index)
{
    assert(index >= FF_UNKNOWN && index <= FF_LAST);
    return p_pcfgkernel[index];
}

const CFGKernel* MultiCFGKernelMgr::InitCFGKernel(const string &path_home, const string &profile, const string &version, const string& filename,
                                                  bool reload, const vector<string> &allowed_flags, string& str_error)
{
    if (version=="")
        return nullptr;

    CFGKernel* a_cfgkernel = nullptr;
    MvFileFormat_e ff_id = MV_get_file_format(version);

    if ( (profile != "" && version != "") && (ff_id <= FF_UNKNOWN || ff_id >= FF_LAST) )
    {
        string cfgDirPath = path_home + string("/templates/hc_descriptor/config/CFG/");

        cfgDirPath += profile;

        bool dir_exist = my_dir_exists(cfgDirPath);
        if (dir_exist)
        {
            string a_cfgDirPath = cfgDirPath + version;
            if (my_dir_exists(a_cfgDirPath))
            {
                map<ApplicationMode_e, string>  app_folder_hierachy;
                GetSolverFolderHierachy(app_folder_hierachy);
                ApplicationMode_e  last_solvercode =  app_folder_hierachy.rbegin()->first;

                string versions; bool first = true;
                fs::directory_iterator end_itr; // default construction yields past-the-end
                const fs::path dir_path{ cfgDirPath };
                for (fs::directory_iterator itr(dir_path); itr != end_itr; ++itr)
                {
                    if (fs::is_directory(itr->status()))
                    {
                        std::string filepath = string(itr->path().string());
                        std::string base_filename = filepath.substr(filepath.find_last_of("/\\") + 1);
                        if (first)
                        {
                            first = false;
                            versions += profile + string("/") + base_filename;
                        }
                        else
                        {
                            versions += string(":") + profile + string("/") + base_filename;
                        }
                    }
                }


                //const char* PATH = ".";
                //DIR* dir = opendir(cfgDirPath.c_str());

                //struct dirent* entry = readdir(dir);

                //while (entry != NULL)
                //{
                //    if (entry->d_type == DT_DIR)
                //    {
                //        //printf("%s\n", entry->d_name);
                //        if (first)
                //        {
                //            first = false;
                //            version += string(entry->d_name);
                //        }
                //        else
                //            versions += ":" + string(entry->d_name);
                //    }
                //    entry = readdir(dir);
                //}

                //closedir(dir);
                map<ApplicationMode_e, string>  app_folder_hierachy1, app_folder_hierachy2;
                app_folder_hierachy2[(ApplicationMode_e)(last_solvercode + 1)] = versions;
                GetSolverFolderHierachy(app_folder_hierachy1, &app_folder_hierachy2);

                if (app_folder_hierachy1.find((ApplicationMode_e)(last_solvercode + 1)) == app_folder_hierachy1.end())
                    assert(0);


                //app_folder_hierachy[(ApplicationMode_e)(last_solvercode + 1)] = versions; // should get all folders and append with :

                HCDI_SetFileFormat(version);
            }
        }
    }
    ff_id = MV_get_file_format(version);

    if (ff_id <= FF_UNKNOWN || ff_id >= FF_LAST)
    {
        MultiCFGKernelMgr::getInstance().SetActiveUserProfile(FF_UNKNOWN);
        return nullptr; /*Invalid sub-profile, No yet supported*/
        //throw MvError_t("Invalid sub-profile. Please check");
    }

    a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCFGKernel(ff_id);
    if (!a_cfgkernel || reload)
    {
        if (a_cfgkernel)
        {
            MultiCFGKernelMgr::getInstance().DeleteCFGKernel(ff_id);
        }
        try {
            a_cfgkernel = new CFGKernel(path_home, profile, version, allowed_flags, filename);
        }
        catch (MvError_t& a_error) {
            str_error = a_error.GetMessage();
            delete MultiCFGKernelMgr::getInstance().GetCFGKernel(ff_id);
            MultiCFGKernelMgr::getInstance().SetActiveUserProfile(ff_id);
            a_cfgkernel = nullptr;
            MultiCFGKernelMgr::getInstance().SetCFGKernel(ff_id, a_cfgkernel);
            return nullptr;
        }
        catch (...) {
            delete MultiCFGKernelMgr::getInstance().GetCFGKernel(ff_id);
            MultiCFGKernelMgr::getInstance().SetActiveUserProfile(ff_id);
            a_cfgkernel = nullptr;
            MultiCFGKernelMgr::getInstance().SetCFGKernel(ff_id, a_cfgkernel);
            return nullptr;
        }
        MultiCFGKernelMgr::getInstance().SetCFGKernel(ff_id, a_cfgkernel);
    }
    /*make sure that if sentinel is set from outside then just load the kernel model, dont set to p_current_solver*/
    if (a_cfgkernel && MultiCFGKernelMgr::CFGKernelSentinel::GetEffectiveSolver() == FF_UNKNOWN)
        MultiCFGKernelMgr::getInstance().SetActiveUserProfile(a_cfgkernel->getSubUserProfile());

    return a_cfgkernel; //sucessfull

}

int MultiCFGKernelMgr::InitCFGKernel(const string& path_home, const string& profile, const string& version, const vector<string>& allowed_flags, string& str_error_out)
{
    if (path_home.empty() || version.empty())
        return -1;

    CFGKernel* a_cfgkernel = nullptr;
    MvFileFormat_e ff_id = MV_get_file_format(version);

    if ((profile != "" || version != "") && (ff_id <= FF_UNKNOWN || ff_id >= FF_LAST))
    {
        string cfgDirPath = path_home + string("/templates/hc_descriptor/config/CFG");

        cfgDirPath += profile;

        bool dir_exist = my_dir_exists(cfgDirPath);
        if (dir_exist)
        {
            cfgDirPath += version;
            if (my_dir_exists(cfgDirPath))
            {
                map<ApplicationMode_e, string>  app_folder_hierachy;
                GetSolverFolderHierachy(app_folder_hierachy);
                ApplicationMode_e  last_solvercode = app_folder_hierachy.rbegin()->first;
                app_folder_hierachy[(ApplicationMode_e)(last_solvercode + 1)] = profile;

                HCDI_SetFileFormat(version);
            }
        }
    }

    ff_id = MV_get_file_format(version);

    if (ff_id <= FF_UNKNOWN || ff_id >= FF_LAST)
    {
        return -1; /*Invalid sub-profile, No yet supported*/
        //throw MvError_t("Invalid sub-profile. Please check");
    }

    a_cfgkernel = MultiCFGKernelMgr::getInstance().GetCFGKernel(ff_id);
    if (!a_cfgkernel)
    {
        try {
            a_cfgkernel = new CFGKernel(path_home, profile, version, allowed_flags, "");
        }
        catch (MvError_t& a_error) {
            str_error_out = a_error.GetMessage();
            delete MultiCFGKernelMgr::getInstance().GetCFGKernel(ff_id);
            MultiCFGKernelMgr::getInstance().SetActiveUserProfile(ff_id);
            a_cfgkernel = nullptr;
            MultiCFGKernelMgr::getInstance().SetCFGKernel(ff_id, a_cfgkernel);
            return -2;
        }
        catch (...) {
            delete MultiCFGKernelMgr::getInstance().GetCFGKernel(ff_id);
            MultiCFGKernelMgr::getInstance().SetActiveUserProfile(ff_id);
            a_cfgkernel = nullptr;
            MultiCFGKernelMgr::getInstance().SetCFGKernel(ff_id, a_cfgkernel);
            return -2;
        }
        MultiCFGKernelMgr::getInstance().SetCFGKernel(ff_id, a_cfgkernel);
    }
    return 0; //sucessfull
}
