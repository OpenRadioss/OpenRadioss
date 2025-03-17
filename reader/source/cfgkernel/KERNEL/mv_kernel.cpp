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



#include <UTILS/error.h>
#include <UTILS/file_utils.h> 
#include <UTILS/system_utils.h>
//#include <UTILS/mv_memory_report.h>
#include <MESSAGE/msg_manager.h>
#include <MESSAGE/mv_messages.h>
#include <MUNITS/mu_dimension.h>    



#include "mv_cstring.h"
#include "mv_data_cfg.h"
#include "mv_kernel_constants.h"
#include "mv_model_descriptors.h" 
#include "mv_kernel.h"
#include "mv_transformable_object_list.h"
#include "mv_pre_datas_hierarchy.h"

#include "cfg_kernel.h"
#define MSG_62 "ERROR: Environment variable %s is not set"

//static const char *loc_get_msg(int ind) { return MV_get_msg_array(MSGT_KERNEL)[ind]; }


/* --------- Kernel management --------- */

//void MV_init_kernel() {
//  try {
//    //MV_add_memory_report("Before initializing kernel");
//    //
//    string a_var;
//    a_var=MV_get_config_dir();
//    if(a_var=="") throw MvError_t(MSG_62,CFG_DIR_VAR);

//     if((strstr(a_var.c_str(),"config/CFG")==NULL) && 
//        (strstr(a_var.c_str(),"config\\CFG")==NULL) ) throw MvError_t("Please, \"HM_MV_CFG_DIR\" must map newly \"../config/CFG\" folder");

//
//    a_var=MV_get_latest_version(false, true);
//    if(a_var=="") throw MvError_t(loc_get_msg(50));
//
//    char *char_temp = getenv("HM_MV_UNITS_DIR");
//    if (char_temp == NULL)
//       throw MvError_t("HM_MV_UNITS_DIR environment variable is not set correctly. Please check.");
//    string unit_dir =  string(char_temp) + "/units.cfg";
//    MU_init_dimensions(unit_dir); 
//
//    MV_init_pre_descriptors();   
//    MV_init_descriptors();       
//    MV_init_model_descriptors(); 
//    MV_init_trees_cfg();         
//
//    
//    
//    //
//    //MV_init_adhesive_objects_list(); 
//    
//    //MV_add_memory_report("After initializing kernel");
//    
//  } catch(MvError_t &a_error) {
//    throw a_error;
//  }
//}

void CFGKernel::close_kernel() {

  close_trees_cfg();        
  close_model_descriptors(); 
  close_pre_descriptors();  
  close_descriptors();
  //MU_close_dimensions();       
  delete_all_subtypes();
  //MV_delete_type_map();
  get_userdefined_hierarchy(-1, "");
  if (p_syantaxInfo)
      delete p_syantaxInfo;
}


static void loc_init_attibute_solver_names(const CFGKernel &cfgkernel, const MvPreDatasHierarchy_t *a_data_cfg_p, MvFileFormat_e file_format)
{
    if(!a_data_cfg_p  || FF_UNKNOWN == file_format)
       return;

    MvPreDatasHierarchyList_t::const_iterator it_begin = a_data_cfg_p->getChildList().begin();
    MvPreDatasHierarchyList_t::const_iterator it_end   = a_data_cfg_p->getChildList().end();
    MvPreDatasHierarchyList_t::const_iterator it;
 
    for (it = it_begin; it != it_end; ++it) 
    {
        const MvPreDatasHierarchy_t *a_data_p = (*it);

        if (a_data_p->getNbChildren() > 0)
        {
            loc_init_attibute_solver_names(cfgkernel, a_data_p, file_format);
        }
        else
        {
            const MvFullType_t fulltype = a_data_p->getFullType();
            IDescriptor* descr_p = cfgkernel.GetDescriptorHandleFromFullType(fulltype); // (IDescriptor*)HCDI_GetDescriptorHandleFromFullType(fulltype);
            if (NULL != descr_p)
            {
                descr_p->postTreatFileFormat(file_format);
            }
        }
    }
}

void CFGKernel::InitAttributeSolverNames()
{
    if (FF_UNKNOWN == getSubUserProfile() ) return;

    const MvPreDatasHierarchy_t *a_data_cfg_p = get_tree_cfg(DTT_ASSIGNED, "Assigned data");
    if(a_data_cfg_p)
       loc_init_attibute_solver_names(*this, a_data_cfg_p, getSubUserProfile());
}
