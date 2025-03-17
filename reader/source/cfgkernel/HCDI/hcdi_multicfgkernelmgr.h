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
#ifndef HCDI_MULTICFGKERNELMGR_H
#define HCDI_MULTICFGKERNELMGR_H


#include "KERNEL/mv_file_format.h"
#include "assert.h"
#include <vector>
#include <KERNEL/cfg_kernel.h>
using namespace std;


class HC_DATA_DLL_API MultiCFGKernelMgr
{
public:

    class HC_DATA_DLL_API CFGKernelSentinel
    {
    public:
        CFGKernelSentinel(MvFileFormat_e userprofile)
        {
            assert(s_effSolver != -1);
            s_effSolver = userprofile;
            count++;
        }
        ~CFGKernelSentinel()
        {
            count--;
            if(!count)
              s_effSolver = FF_UNKNOWN;
        }
        static void SetEffectiveSolver(MvFileFormat_e curprofile) { s_effSolver = curprofile; }
        static MvFileFormat_e GetEffectiveSolver() { return s_effSolver; }

    private:
        static MvFileFormat_e s_effSolver;
        static int count;
    };

public:
    static MultiCFGKernelMgr& getInstance()
    {
        static MultiCFGKernelMgr    instance;
        return instance;
    }
private:
    MultiCFGKernelMgr(MvFileFormat_e userprofile = FF_UNKNOWN) : p_pcfgkernel(FF_LAST, nullptr)
    {
        p_current_solver = userprofile;
    }
    ~MultiCFGKernelMgr();

    MultiCFGKernelMgr(MultiCFGKernelMgr const&);
    void operator=(MultiCFGKernelMgr const&);
    MvFileFormat_e P_GetEffectiveSolver() const;

public:
    
    const CFGKernel* GetCurrentCFGKernel();

    const CFGKernel* InitCFGKernel(const string& path_home, const string& profile, const string& version, const string& filename, bool reload,
                                   const vector<string>& allowed_flags, string& str_error);

    int InitCFGKernel(const string& path_home, const string& profile, const string& version, const vector<string>& allowed_flags, string& str_error_out);

    vector<MvFileFormat_e> GetLoadedProfiles()
    {
        vector<MvFileFormat_e> loadedprofiles;
        for (int i = 0; i < FF_LAST; i++)
        {
            if (p_pcfgkernel[i])
                loadedprofiles.push_back((MvFileFormat_e)i);
        }
        return loadedprofiles;
    }

    /** get/set active user profile **/
    MvFileFormat_e GetActiveUserProfile()
    {
        return p_current_solver;
    }

    void SetActiveUserProfile(MvFileFormat_e activeuserprofile)
    {
        p_current_solver = activeuserprofile;
    }

    int DeleteCFGKernel(MvFileFormat_e userprofile = FF_UNKNOWN)
    {
        if (userprofile != FF_UNKNOWN)
        {
            if (userprofile > FF_UNKNOWN && userprofile < FF_LAST)
            {
                if (p_pcfgkernel[userprofile])
                {
                    delete p_pcfgkernel[userprofile];
                    p_pcfgkernel[userprofile] = nullptr;
                }
            }
        }
        else
        {
            for (int i = 0; i < FF_LAST; i++)
            {
                if (!p_pcfgkernel[i])
                    continue;
                delete p_pcfgkernel[i];
                p_pcfgkernel[i] = nullptr;
            }
        }
        return 0;
    }
    CFGKernel* GetCFGKernel(int index);

    void SetCFGKernel(int index, CFGKernel* pcfgkernel);

private:
    MvFileFormat_e                p_current_solver;
    vector< CFGKernel* >          p_pcfgkernel;
};
#endif //HCDI_MULTICFGKERNELMGR_H