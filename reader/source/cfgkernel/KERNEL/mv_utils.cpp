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

#ifdef WIN32
#include <windows.h>
#else
#include <dirent.h>
#endif

#include <UTILS/system_utils.h>
#include <UTILS/memory_utils.h>  
#include <UTILS/file_utils.h>
#include <UTILS/error.h>
#include <UTILS/set_utils.h>
#include <UTILS/str_utils.h>     
#include <UTILS/mv_cstring.h>
#include <KERNEL_BASE/type_API.h>   

#include <MESSAGE/msg_manager.h>
#include <MESSAGE/mv_messages.h>


#include "mv_full_type.h"
#include "mv_type.h"
#include "mv_subtype.h"          
#include "mv_kernel_constants.h"
#include "mv_utils.h"
#include "HCDI/hcdi_multicfgkernelmgr.h"



typedef map<object_type_e,const MvSubtype_t *> LocEConn2LConn_t;
typedef map<const MvSubtype_t *,object_type_e> LocLConn2EConn_t;


//static MvDirList_t *loc_split_dir_list(const string &dirs,char separator,MvDirList_t *dir_list_p=NULL);



//static double       loc_convert_memory_info(const string &info);




/* --------- Public but not documented --------- */

//*********************************************************************************
// MV_get_type_str
//*********************************************************************************
const char *MV_get_type_str(object_type_e obj_type) {
  return MV_get_type(obj_type).c_str();
}


/* --------- Set of object's types --------- */

//*********************************************************************************
// MV_get_object_type_set
//*********************************************************************************
/* Enitre Function need_to_be_checked. All the object types were added using a for loop */
const MvObjectTypeSet_t &HCDI_get_object_type_set() {
  static bool              a_first=true;
  static MvObjectTypeSet_t a_object_type_set;
  if(a_first) {
    //a_first=false;

  const CFGKernel* a_kernel_model = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
  if (!a_kernel_model)
      return a_object_type_set;
    for (int i = HCDI_OBJ_TYPE_NODES; i< HCDI_OBJ_TYPE_HC_MAX; i++) {
        object_type_e a_otype = object_type_e(i);
        if (a_kernel_model->MV_is_subtyped(a_otype)) a_object_type_set.insert(a_otype);
   }
  }
  return a_object_type_set;
}

const MvObjectTypeSet_t& CFGKernel::get_object_type_set() {
    if (!p_objectTypeSet.size())
    {
        for (int i = HCDI_OBJ_TYPE_NODES; i < HCDI_OBJ_TYPE_HC_MAX; i++) {
            object_type_e a_otype = object_type_e(i);
            if (MV_is_subtyped(a_otype)) p_objectTypeSet.insert(a_otype);
        }
    }
    return p_objectTypeSet;
}




//*********************************************************************************
// MV_get_option_type_set
//*********************************************************************************
/* Enitre Function need_to_be_checked. All the object types were added using a for loop by removing the individual addition of the object types*/
//const MvObjectTypeSet_t &MV_get_option_type_set() {
//    static bool              a_first=true;
//    static MvObjectTypeSet_t option_type_set;
//    if(a_first) {
//        a_first=false;
//        for (int i = HCDI_OBJ_TYPE_NODES; i< HCDI_OBJ_TYPE_HC_MAX; i++) {
//            object_type_e a_otype = object_type_e(i);
//            if (MV_is_subtyped(a_otype)) option_type_set.insert(a_otype);
//        }
//    }
//    return option_type_set;
//}

//*********************************************************************************
// MV_get_titled_type_set
//*********************************************************************************
/* Enitre Function need_to_be_checked. Commenting the complete function*/
const MvObjectTypeSet_t &MV_get_titled_radioss_type_set() {
  static bool              first=true;
  static MvObjectTypeSet_t object_with_title_set;
  if(first) {
    first=false;
//    const string &a_radioss_version=MV_get_latest_version(); 
//    //
      object_with_title_set.insert(HCDI_OBJ_TYPE_MATS);
//    object_with_title_set.insert(HCDI_OBJ_TYPE_PROPS);
//    //
//    object_with_title_set.insert(GRQUAD);
//    object_with_title_set.insert(GRBRIC);
//    object_with_title_set.insert(GRSHEL);
//    object_with_title_set.insert(GRSH3N);
//    object_with_title_set.insert(GRBEAM);
//    object_with_title_set.insert(GRSPRI);
//    object_with_title_set.insert(GRTRUS);
//    object_with_title_set.insert(GRNOD);
//    object_with_title_set.insert(SURF);
//    object_with_title_set.insert(LINE);
//	object_with_title_set.insert(GRPART);
//    //
//    object_with_title_set.insert(SENSOR);
//    object_with_title_set.insert(ACCELERO);
//    object_with_title_set.insert(FUNCTION);
//    object_with_title_set.insert(SKEW);
//    //
//    
//    object_with_title_set.insert(ADDED_MASS);
//    object_with_title_set.insert(BOUNDARY_CONDITION);
//    object_with_title_set.insert(INITIAL_VELOCITY);
//    object_with_title_set.insert(RIGID_BODY);
//    object_with_title_set.insert(RIGID_WALL);
//    object_with_title_set.insert(RIGID_LINK);
//    object_with_title_set.insert(PRESSURE_LOAD);
//    object_with_title_set.insert(GRAVITY);
//    object_with_title_set.insert(FIXED_DISPLACEMENT);
//    object_with_title_set.insert(FIXED_VELOCITY);
//    object_with_title_set.insert(CYLINDRICAL_JOINT);
    //
//    object_with_title_set.insert(TIME_HISTORY);
//    object_with_title_set.insert(CONCENTRATED_LOAD);
//    object_with_title_set.insert(SECTION);
//    object_with_title_set.insert(MONITORED_VOLUMES);
//    object_with_title_set.insert(MCDS_INTERFACE);
//    object_with_title_set.insert(LSD_INTERFACE);
//    object_with_title_set.insert(DRAPE);
//    object_with_title_set.insert(PLY_ENTITY);
//    object_with_title_set.insert(STACK);
//    object_with_title_set.insert(PARAMETER);
//    object_with_title_set.insert(PERTURB);
//    if(0<=MV_comp_radioss_version(a_radioss_version,"radioss42")) {
//      object_with_title_set.insert(WALL_SHAP);                
//      object_with_title_set.insert(WALL_PEN);                 
//    }
//    
//    if(0<=MV_comp_radioss_version(a_radioss_version,"radioss43") && 0>MV_comp_radioss_version(a_radioss_version,"radioss120")) {
//      object_with_title_set.insert(DAMP);
//    }
//    
//	
//	object_with_title_set.insert(MADYMO_LINK);                
//	if(0<=MV_comp_radioss_version(a_radioss_version,"radioss51")){
//		object_with_title_set.insert(MADYMO_EXFEM);
//	}
    //
//	
//  	
//	if(0<=MV_comp_radioss_version(a_radioss_version,"radioss52")){
//		object_with_title_set.insert(MCDS_TRANSFORM);
//	}
//    //
//    
//    if(0<=MV_comp_radioss_version(a_radioss_version,"radioss52")){
//        object_with_title_set.insert(MCDS_MOVE_FUNCT);
//    }
//    //
//	
//	
//	if(0<=MV_comp_radioss_version(a_radioss_version,"radioss52")){
//		object_with_title_set.insert(MCDS_CNODE);
//	}
//	
    //
    //
//    
//    object_with_title_set.insert(MADYMO_LINK);                
//    if(0<=MV_comp_radioss_version(a_radioss_version,"radioss51")){
//      object_with_title_set.insert(MADYMO_EXFEM);
//    }
//    
//    
//    if(0<=MV_comp_radioss_version(a_radioss_version,"radioss52")){
//      object_with_title_set.insert(MCDS_TRANSFORM);
//    }
//    
//    //
//    
//    if(0<=MV_comp_radioss_version(a_radioss_version,"radioss52")){
//      object_with_title_set.insert(MCDS_MOVE_FUNCT);
//    }
//    //
//    
//    if(0<=MV_comp_radioss_version(a_radioss_version,"radioss52")){
//      object_with_title_set.insert(MCDS_CNODE);
//    }
//    
    //
//    if(0<=MV_comp_radioss_version(a_radioss_version,"radioss44")) {
//      object_with_title_set.insert(FIXED_BODY_VELOCITY);    
//      object_with_title_set.insert(FIXED_ACCELERATION);     
//      object_with_title_set.insert(MULTI_POINT_CONSTRAINT); 
//      object_with_title_set.insert(ACTIVATION);             
//      
//      
//      object_with_title_set.insert(SPH_BCS);                
//      object_with_title_set.insert(SPH_INOUT);              
//      object_with_title_set.insert(GEAR_JOINT);             
//    }
//    
//    //
//    
//    if(0<MV_comp_radioss_version(a_radioss_version,"radioss50")) {
//      object_with_title_set.insert(EIGEN_MODES);
//      object_with_title_set.insert(FLEXIBLE_BODY);
//      object_with_title_set.insert(BEM_FLOW);               
//      object_with_title_set.insert(MCDS_EBCS);              
//    }
//     object_with_title_set.insert(THERMAL_LOAD);
//    object_with_title_set.insert(INITIAL_TEMPERATURE);
    //
//    
//    //
//    object_with_title_set.insert(PART1);
//    object_with_title_set.insert(SUBSET);
//    object_with_title_set.insert(JOINT_STIFFNESS);
//    object_with_title_set.insert(FE_JOINT);
//    object_with_title_set.insert(CONSTR_RBODY); 
//    
//    
//    object_with_title_set.insert(DEFINE_BOX);  
//    object_with_title_set.insert(ELEMENT_MASS1);  
//    // object_with_title_set.insert(INTEGRATION_BEAM); Is untitled! (CS#01_03_11)
//    // object_with_title_set.insert(INTEGRATION_SHELL); Is untitled! (CS#01_03_11)
    //
//    object_with_title_set.insert(ALE_MUL_MAT_GROUP);    //venkatk#S_96917#24_09_09 ALE_MULTI-MATERIAL_GROUP
//    object_with_title_set.insert(SET_MUL_MAT_GROUP);    //venkatk#S_96917#24_09_09 SET_MULTI-MATERIAL_GROUP_LIST
    //
//	object_with_title_set.insert(LSD_SENSOR_DEFINE); 
//	object_with_title_set.insert(LSD_SENSOR_CONTROL); 
//	object_with_title_set.insert(LSD_SENSOR_SWITCH); 
//	object_with_title_set.insert(LSD_PART_SENSOR); 
    //
//	object_with_title_set.insert(DEFINE_CONN_PROP);   //venkatk#S179198#27_10_09 DYNA *DEFINE_CONNECTION_PROPERTIES
//    object_with_title_set.insert(DEFINE_CONN_PROP_ADD);   //venkatk#S179198#27_10_09 DYNA *DEFINE_CONNECTION_PROPERTIES_ADD
//    object_with_title_set.insert(DEFINE_HEX_SW_ASSEM);   //venkatk#S_179200#02_11_09 *DEFINE_HEX_SPOTWELD_ASSEMBLY
//    
//    object_with_title_set.insert(DEFINE_AB_BAG);  //venkatk#S_179298#12DEC09#*DEFINE_ALEBAG_BAG
//    object_with_title_set.insert(DEFINE_AB_HOLE);  //venkatk#S_179301#12DEC09#*DEFINE_ALEBAG_HOLE
//    object_with_title_set.insert(DEFINE_AB_INF);  //venkatk#S_179302#12DEC09#*DEFINE_ALEBAG_INFLATOR
//    object_with_title_set.insert(AAA);    //venkatk#S_179297#12DEC09#*AIRBAG_ADVANCED_ALE(AAA)
//    object_with_title_set.insert(INITIAL_STRESS_SECTION);  
//    object_with_title_set.insert(RBE3_TYPE);  /*DJAY CONST_INTERPOL 03-23-2010*/  
//	object_with_title_set.insert(LOCAL_UNIT_SYSTEM); 
//    object_with_title_set.insert(MCDS_RBE2);  
    //
//    object_with_title_set.insert(INICONT); 
//    object_with_title_set.insert(ADMESH_SET); 
//  object_with_title_set.insert(GAUGE); 
//    object_with_title_set.insert(INICRACK);
//    object_with_title_set.insert(INIVOL); 
//    object_with_title_set.insert(SUBDOMAIN);
//    object_with_title_set.insert(RADIATION);
//    object_with_title_set.insert(LEAK);
//	object_with_title_set.insert(CONVEC);
//    object_with_title_set.insert(LOAD_VELOCITY_FIELD);
//    object_with_title_set.insert(HC_LOADS);
//    object_with_title_set.insert(EOS);
//    //object_with_title_set.insert(MCDS_ENGINE_FILE);
        //
        }
    return object_with_title_set;
}

//*********************************************************************************
// MV_get_user_type_set
//*********************************************************************************

//const MvObjectTypeSet_t &CFGKernel::MV_get_user_type_set() {
//    if(!p_userTypeSet.size()) {
//        for (int i = HCDI_OBJ_TYPE_NODES; i< HCDI_OBJ_TYPE_HC_MAX; i++) {
//            object_type_e a_otype = object_type_e(i);
//            if (MV_is_subtyped(a_otype)) p_userTypeSet.insert(a_otype);
//        }
//    }
//    return p_userTypeSet;
//}


//*********************************************************************************
// MV_get_cryptable_type_set
//*********************************************************************************


//*********************************************************************************
// MV_get_lagmul_type_set
//*********************************************************************************


const MvObjectTypeSet_t &MV_get_lagmul_type_set() {
  static bool              a_first=true;
  static MvObjectTypeSet_t a_otypes;
    /*
  if(a_first) {
   a_first=false;
   //
   a_otypes.insert(FIXED_VELOCITY);
   a_otypes.insert(RIGID_BODY);
   a_otypes.insert(RIGID_WALL);
   a_otypes.insert(MCDS_INTERFACE);
   a_otypes.insert(BOUNDARY_CONDITION);
  }
    */
  return a_otypes;
}


//*********************************************************************************
// MV_get_kernel_subtyped_type_set
//*********************************************************************************

const MvObjectTypeSet_t &MV_get_kernel_subtyped_type_set() {
  static bool              a_first=true;
  static MvObjectTypeSet_t a_otypes;
  if(a_first) {
    a_first=false;
    for (int i = HCDI_OBJ_TYPE_NODES; i< HCDI_OBJ_TYPE_HC_MAX; i++) {
      object_type_e a_otype= object_type_e(i);
      if(MV_is_subtyped(a_otype)) a_otypes.insert(a_otype);
    }
  }
  return a_otypes;
}


//*********************************************************************************
// MV_get_input_subtyped_type_set
//*********************************************************************************


const MvObjectTypeSet_t &MV_get_input_subtyped_type_set() {
  static bool              a_first=true;
  static MvObjectTypeSet_t a_otypes;
  /*
  if(a_first) {
    a_first=false;
    //
    a_otypes=MV_get_kernel_subtyped_type_set();
    a_otypes.erase(PART1);
    a_otypes.erase(SUBSET);
    a_otypes.insert(SKEW);
    a_otypes.insert(SENSOR);
    a_otypes.insert(INITIAL_VELOCITY);
    a_otypes.insert(ADDED_MASS);
    a_otypes.insert(FUNCTION);
  }
  */
  return a_otypes;
}


const MvObjectTypeSet_t& MV_get_has_no_id_type_set() {
    static bool              a_first = true;
    static MvObjectTypeSet_t a_otypes;
    
    if(a_first) {
      a_first=false;
      //
      a_otypes.insert(HCDI_OBJ_TYPE_INITIALGEOMETRIES);
    }
    
    return a_otypes;
}



//*********************************************************************************
// MV_is_titled
//*********************************************************************************
bool MV_is_titled(object_type_e obj_type, const string &solver, int version) 
{
    if (solver != "RADIOSS")
    {
        return false;
    }
    else  
    {
        static const MvObjectTypeSet_t    &a_object_with_title_set = MV_get_titled_radioss_type_set();
        MvObjectTypeSet_t::const_iterator  a_it                    = a_object_with_title_set.find(obj_type);
        return a_it!=a_object_with_title_set.end();
    }
}
//*********************************************************************************
// MV_is_user
//*********************************************************************************
bool CFGKernel::is_user(object_type_e obj_type) const {
  return true;
/* by default all types are user, in case of non-user we will need to implement as SUBTYPE=NON_USER; later */
/*
  const MvObjectTypeSet_t    &a_user_type_set = MV_get_user_type_set();
  MvObjectTypeSet_t::const_iterator  a_it            = a_user_type_set.find(obj_type);
  return a_it!=a_user_type_set.end();
*/
}

//*********************************************************************************
// MV_is_cryptable
//*********************************************************************************





//*********************************************************************************
// MV_is_kernel_subtyped
//*********************************************************************************

bool MV_is_kernel_subtyped(object_type_e otype) {
  static const MvObjectTypeSet_t    &a_otypes = MV_get_kernel_subtyped_type_set();
  MvObjectTypeSet_t::const_iterator  a_it     = a_otypes.find(otype);
  return a_it!=a_otypes.end();
}


//*********************************************************************************
// MV_is_input_subtyped
//*********************************************************************************

bool MV_is_input_subtyped(object_type_e otype) {
  static const MvObjectTypeSet_t    &a_otypes = MV_get_input_subtyped_type_set();
  MvObjectTypeSet_t::const_iterator  a_it     = a_otypes.find(otype);
  return a_it!=a_otypes.end();
}


bool MV_has_no_id(object_type_e obj_type) {
    static const MvObjectTypeSet_t& a_no_id_type_set = MV_get_has_no_id_type_set();
    MvObjectTypeSet_t::const_iterator  a_it = a_no_id_type_set.find(obj_type);
    return a_it != a_no_id_type_set.end();
}



//*********************************************************************************

//typedef map<object_type_e,MvObjectTypeSet_t> LocTypesSharingIds_t;



/* --------- Constants, directories, names, versions --------- */

//*********************************************************************************

//*********************************************************************************

const string &MV_get_helioss_version(bool do_strict) {
  static string a_version_large  = HELIOSS_VERSION;
  static string a_version_strict = "";
  //
  if(a_version_strict.empty()) {
     size_t a_pos=a_version_large.find(' ');
     if(a_pos==a_version_large.npos) a_version_strict=a_version_large;
     else                            a_version_strict=a_version_large.substr(0,a_pos);
  }
  //
  return do_strict ? a_version_strict : a_version_large;
}


//*********************************************************************************
// MV_get_latest_version
//*********************************************************************************

//*********************************************************************************
// MV_get_current_dir
//*********************************************************************************
string MV_get_current_dir() {
  static string a_cur_dir = my_get_current_dir();
  return a_cur_dir;
}

//*********************************************************************************
// MV_get_home_dir
//*********************************************************************************
const string &MV_get_home_dir() {
  static const string a_home_dir=my_get_home_dir();
  return a_home_dir;
}

//*********************************************************************************
// MV_get_root_dir
//*********************************************************************************
//*********************************************************************************
// MV_get_tmp_dir
//*********************************************************************************
const string &MV_get_tmp_dir() {
  static string a_tmp_dir=my_get_tmp_dir();
  return a_tmp_dir;
}

//*********************************************************************************
// MV_get_config_dirs
//*********************************************************************************

const MvDirList_t &MV_get_config_dirs() {
  static MvDirList_t a_cfg_dirs;
  static bool        a_first=true;
  //
  if(a_first) {
    
    
    string a_usr_dir=mygetenv(USR_DIR_VAR);
    if(a_usr_dir!="") a_cfg_dirs.push_back(a_usr_dir);

    string a_script_dir=mygetenv("HC_SCRIPT_DIR");
    if(a_script_dir!="") a_cfg_dirs.push_back(a_script_dir);

    
    a_cfg_dirs.push_back(mygetenv(CFG_DIR_VAR));
    
    a_first=false;
  }
  //
  return a_cfg_dirs;
}



// MV_get_version_dirs 
// get the first directory name
//*********************************************************************************



//*********************************************************************************
// MV_get_config_dir
//*********************************************************************************

const string &MV_get_config_dir() {
  const MvDirList_t &a_cfg_dirs=MV_get_config_dirs();
  return a_cfg_dirs[a_cfg_dirs.size()-1];
}


//*********************************************************************************
// MV_get_batch_file_name
//*********************************************************************************
const string &MV_get_batch_file_name(bool do_full) {
  static const string a_value=DEFAULT_BATCH_NAME;
  static const string a_full_value=MV_get_current_dir()+"/"+a_value;
  return do_full ? a_full_value : a_value;
}

//*********************************************************************************
// MV_get_groups_file_name
//*********************************************************************************
const string &MV_get_groups_file_name(bool do_full) {
  static const string a_value=DEFAULT_GROUPS_NAME;
  static const string a_full_value=MV_get_current_dir()+"/"+a_value;
  return do_full ? a_full_value : a_value;
}


//*********************************************************************************
// MV_get_mcrash_subdir
//*********************************************************************************

string MV_get_mcrash_subdir(const string &subdir) {
  static const MvDirList_t &a_cfg_dirs = MV_get_config_dirs();
  string             a_path     = "";
  //
  MvDirList_t::const_iterator a_cfg_it_begin = a_cfg_dirs.begin();
  MvDirList_t::const_iterator a_cfg_it_end   = a_cfg_dirs.end();
  MvDirList_t::const_iterator a_cfg_it;
  for(a_cfg_it=a_cfg_it_begin;a_cfg_it!=a_cfg_it_end && a_path.empty();++a_cfg_it) {
    const string &a_cfg_dir=(*a_cfg_it);
    //
    string a_cur_path=a_cfg_dir+"/MCRASH/"+subdir;
    if(my_dir_exists(a_cur_path)) a_path=a_cur_path;
  }
  //
  return a_path;
}



/* --------- Static functions --------- */
//*********************************************************************************
// loc_split_dir_list
//*********************************************************************************
//static MvDirList_t *loc_split_dir_list(const string &dirs,char separator,MvDirList_t *dir_list_p) {
//  MvDirList_t *a_dir_list_p=(dir_list_p==NULL ? new MvDirList_t() : dir_list_p);
//  //
//  int a_end_pos = (int)(dirs.size());
//  int a_cur_pos = 0;
//  while(a_cur_pos<a_end_pos) {
//    string a_cur_dir="";
//    while(a_cur_pos<a_end_pos && dirs[a_cur_pos]!=separator) a_cur_dir+=dirs[a_cur_pos++];
//    if(a_cur_pos<a_end_pos) ++a_cur_pos;
//    if(!a_cur_dir.empty()) a_dir_list_p->push_back(a_cur_dir);
//  }
//  /*
//  cout << "Splitting " << dirs << endl;
//  MvDirList_t::iterator a_it_begin = a_dir_list_p->begin();
//  MvDirList_t::iterator a_it_end   = a_dir_list_p->end();
//  MvDirList_t::iterator a_it;
//  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) cout << "-> " << *a_it << endl;
//  */
//  return a_dir_list_p;
//}

//*********************************************************************************
// loc_convert_memory_info
//*********************************************************************************


/*
double loc_convert_memory_info(const string &info) {
  double a_result = 0.;
  int    a_length = info.size();
  //
  if(info[a_length-1]=='M') {
    string a_info=info.substr(0,a_length-1);
    sscanf(a_info.c_str(),"%lf",&a_result);
  } else if(info[a_length-1]=='K') {
    string a_info=info.substr(0,a_length-1);
    sscanf(a_info.c_str(),"%lf",&a_result);
    a_result/=1024;
  } else {
    sscanf(info.c_str(),"%lf",&a_result);
    a_result/=1024;
  }
  //
  return a_result;
}
*/


int  MV_comp_radioss_version(string vers1, string vers2)
{
   static std::vector<std::string> versions = { "radioss2025", "radioss2024", "radioss2023", "radioss2022", "radioss2021","radioss2020","radioss2019","radioss2018", "radioss2017","radioss2016","radioss2015","radioss140","radioss130", "radioss120", "radioss110", "radioss100", "radioss90", "radioss52","radioss51", "radioss50","radioss48", "radioss44", "radioss43", "radioss42", "radioss41" };

   static int    nb_vers = static_cast<int> (versions.size());
   int    i, i1=-1, i2=-1 ;

   for (i=0 ; i<nb_vers ; i++)
   {
       if (vers1== versions.at(i))
       {
          i1 = i ;
          break ;
       }
   }

   for (i=0 ; i<nb_vers ; i++)
   {
       if (vers2== versions.at(i))
       {
          i2 = i ;
          break ;
       }
   }
   /*
    * if i2 greater than i1 then vers1 > vers 2 */
   return i2 - i1 ;
}


int splitString(const string & target, const string &delimiter, vector <string >& result)
{
   if(target.empty()||delimiter.empty())
        return 0;

   size_t step = delimiter.size();
   size_t prev_found = 0;
   size_t found = target.find(delimiter);
   while (found != string ::npos)
   {
      const string & tmp = target.substr(prev_found,found-prev_found);
      if( !tmp.empty())
        result.push_back(tmp);

      prev_found = found+step;

      found = target.find(delimiter,prev_found);

      if(found == string ::npos )
      {
        const string & tmp2 = target.substr(prev_found);
        if( !tmp2.empty())
          result.push_back(tmp2);
      }
   }
   return (int)(result.size());
}
