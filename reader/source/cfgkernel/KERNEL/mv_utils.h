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
#ifndef MV_UTILS_H
#define MV_UTILS_H


#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>
#include <KERNEL_BASE/Structure_descriptor.h>
#include <KERNEL_BASE/Structure_types.h>
#include <HCDI/hcdi.h>


#define object_t         header_t
#define object_type_e    obj_type_e
#define group_item_e     ctype_e
#define node_t           global_node_t
#define element_t        global_elem_t
#define MV_NB_MCDS_TYPES NB_CONTAINER_MODEL

class MvSubtype_t; 
class MvFullType_t;

const char *MV_get_type_str(object_type_e obj_type);


/** @name Utils */
//@{

/** @name Sets of types */
//@{

/// Set of object's types
typedef set<object_type_e> MvObjectTypeSet_t;

/// Gets object types
HC_DATA_DLL_API const MvObjectTypeSet_t& HCDI_get_object_type_set();

/// Gets option object types
//HC_DATA_DLL_API const MvObjectTypeSet_t &MV_get_option_type_set();


/// Returns types admitting lagrangian multiplyers formulation
HC_DATA_DLL_API const MvObjectTypeSet_t &MV_get_lagmul_type_set();
/// Returns kernel subtyped types
HC_DATA_DLL_API const MvObjectTypeSet_t &MV_get_kernel_subtyped_type_set();
/// Returns input subtyped types
HC_DATA_DLL_API const MvObjectTypeSet_t &MV_get_input_subtyped_type_set();

HC_DATA_DLL_API const MvObjectTypeSet_t& MV_get_has_no_id_type_set();


/// Returns true if the object type is a node or an element
bool MV_is_mesh(object_type_e otype);
/// Returns true if the object type is a hierarchy item (PART, SUBSET, CONNECTION, ROOT)
bool MV_is_hierarchy(object_type_e otype,bool generalized=false,bool admit_root=false);  
/// Returns true if the object type is a group
HC_DATA_DLL_API bool MV_is_group(object_type_e otype);

/// Returns true if the object has a title field
HC_DATA_DLL_API bool MV_is_titled(object_type_e obj_type, const string &solver, int version=0);


/// Returns true if the object type is an elementary connections
bool MV_is_elem_connection(object_type_e otype);    
/// Returns true if the object type is kernel subtyped
HC_DATA_DLL_API bool MV_is_kernel_subtyped(object_type_e otype);
/// Returns true if the object type is input subtyped
HC_DATA_DLL_API bool MV_is_input_subtyped(object_type_e otype);

HC_DATA_DLL_API bool MV_has_no_id(object_type_e obj_type);



/** @name Constants, directories, names, versions, ... */
//@{

/// List of directories
typedef vector<string> MvDirList_t;


const string &MV_get_helioss_version(bool do_strict=false); 

/// Converts the string version into integer version ("4.2" -> 42);

/// Gets the current directory
string MV_get_current_dir();
/// Gets the home directory
const string &MV_get_home_dir();
/// Gets the root directory
/// Gets the tmp directory
const string &MV_get_tmp_dir();

/// Gets the CONFIG directory
const MvDirList_t &MV_get_config_dirs();
/// Gets the CONFIG/version directory


/// Gets the non-user config directory
const string &MV_get_config_dir();
/// Get the title of batch file
const string &MV_get_batch_file_name(bool do_full=true);
/// Gets the title of groups file
const string &MV_get_groups_file_name(bool do_full=true);

/// Gets a sub-directory inside M-Crash directory
string MV_get_mcrash_subdir(const string &subdir);
/// Compares two versions
int MV_comp_radioss_version(string vers1, string vers2) ;



int splitString(const string & target, const string &delimiter, vector <string >& result);

//@}


//@}


#endif //MV_UTILS_H




