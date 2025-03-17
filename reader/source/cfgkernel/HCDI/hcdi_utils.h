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
#ifndef HCDI_UTILS_H
#define HCDI_UTILS_H

#include "hcdi.h"
#include "hcdi_mec_pre_object.h"
#include "hcdi_mv_descriptor.h"
#include <KERNEL_BASE/Structure_descriptor.h>

/// Config Test type
enum ConfigTestType_s {
    CONFIG_TEST_UNKNOWN                       = 1000,
    CONFIG_TEST_LOAD_CFG_KERNEL               = 1001,
    CONFIG_TEST_LOADING_TIME                  = 1002,
    CONFIG_TEST_MEMORY_USAGE                  = 1003,
    CONFIG_TEST_CONFLICTING_DATANAMES         = 1004,
    CONFIG_TEST_DATAHIERARCHY_EXPORT          = 1005,
    CONFIG_TEST_DATAHIERARCHY_UNIQUE_USERID   = 1006,
    CONFIG_TEST_DATAHIERARCHY_UNIQUE_USERNAME = 1007,
    CONFIG_TEST_LAST
};

enum cfglnkchildby_s
{
    CFG_LNK_CHILD_BY_NONE,
    CFG_LNK_CHILD_BY_ID,
    CFG_LNK_CHILD_BY_NAME,
    CFG_LNK_CHILD_BY_ATTRIBS
};
/// Config Test type
typedef enum ConfigTestType_s ConfigTestType_e;
typedef enum cfglnkchildby_s cfglnkchildby_e;

struct cfglnksubdescriptor {
    std::string         child_ktype;
    obj_type_e          parent_type;     // Parent type
    cfglnkchildby_e     link_by;      // "id" or "name" or "attribs"
    const char*         plnkatt;
    const char*         clnkatt;
};

HC_DATA_DLL_API string GetConfigTestDescrString(int index);
HC_DATA_DLL_API IMECPreObject::MyAttributeType_e HCDIGetPATypeFromDAType(attribute_type_e atype);
HC_DATA_DLL_API IMECPreObject::MyValueType_e HCDIGetPVTypeFromDVType(value_type_e vtype);
HC_DATA_DLL_API int HCDIGetIDPoolForGvnDatanameFromPreobject(const IDescriptor *descr_p, const IMECPreObject *preobj, const string &dataname, int &hm_type);
HC_DATA_DLL_API int HCDIGetIDPoolForGvnDataname(const IDescriptor *descr_p, const string &dataname, const string &dataname_type_val);
//HC_DATA_DLL_API void HCDI_set_default_data_hierarchy_flag(const string& bit_string);
HC_DATA_DLL_API int HCDI_get_data_hierarchy_bitmask(const string& bit_string);
HC_DATA_DLL_API int HCDI_get_default_bit();
HC_DATA_DLL_API int HCDIGetMaxProfileBit();
HC_DATA_DLL_API void HCDIGetMultiObjectTypes(const IDescriptor* descrp, int ikeyword, MvFullTypeSet_t& set);
HC_DATA_DLL_API int HCDI_splitString(const string& target, const string& delimiter, vector <string >& result);
HC_DATA_DLL_API bool HCDIIsAttributeTripleCFG(const IDescriptor& descrp, int ikeyword, bool& is_multi);
HC_DATA_DLL_API void CFGResolveEntitiesSubObjectReferences(std::map<string, CUserNameTypeInfo>& username_info, vector<IMECPreObject*>* p_preobjlst);

#endif 