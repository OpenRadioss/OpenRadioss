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
#include <UTILS/memory_utils.h>
#include <UTILS/set_utils.h>
#include <UTILS/map_utils.h>
#include <UTILS/mv_climits.h>
#include <KERNEL/mv_type.h>
#include <KERNEL/mv_keywords.h> 
#include <KERNEL/mv_full_type.h>
#include <HCDI/hcdi_mv_descriptor.h>
#include <HCDI/hcdi_multicfgkernelmgr.h>
#include <UTILS/file_utils.h>
#include <MODEL_IO/hcioi_utils.h>
#include "mec_msg_manager.h"
#include "mv_model_factory.h"



// Comparison functor with an additional string argument
struct ParameterComparator {
    std::string param_name;

    ParameterComparator(const std::string& arg) : param_name(arg) {}

    bool operator()(const IMECPreObject* param_obj1, const IMECPreObject* param_obj2) const {

        int att_indx1 = param_obj1->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, param_name.c_str());
        int att_indx2 = param_obj2->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_STRING, param_name.c_str());

        if (att_indx1 < 0 || att_indx2 < 0)
            return false;


        string param_name1 = param_obj1->GetStringValue(att_indx1);
        string param_name2 = param_obj2->GetStringValue(att_indx2);

        return param_name1 < param_name2;
    }
};


/* --------- Crypting keys --------- */



 
bool MvModelFactory_t::AddCryptingKey(const char* ref, const char* key, const char* cost, const char* feature, const char* module, const char* timeout, const char* interfaceid) {
  string a_ref(ref);
  if (a_ref < myCryptingKeys) return false;
  //
  myCryptingKeys[a_ref] = string(key);
  myCryptingKeysCost[a_ref] = string(cost);
  myCryptingKeysFeature[a_ref] = string(feature);
  myCryptingKeysModule[a_ref] = string(module);
  myCryptingKeysTimeOut[a_ref] = string(timeout);
  myCryptingKeysInterfaceID[a_ref] = string(interfaceid);
  //
  return true;
}



bool MvModelFactory_t::UpdateCryptingKeyCost(const char *ref,const char *key, const char *cost) {
  string a_ref(ref);
  if(a_ref<myCryptingKeys) {
	myCryptingKeysCost[a_ref]=string(cost);
	return true;
  }
  return false;
}


bool MvModelFactory_t::UpdateCryptingKeyFeature(const char *ref,const char *key, const char *feature) {
  string a_ref(ref);
  if(a_ref<myCryptingKeys) {
	myCryptingKeysFeature[a_ref]=string(feature); 
	return true;
  }
  return false;
}
bool MvModelFactory_t::UpdateCryptingKeyModule(const char *ref,const char *key, const char *module) {
  string a_ref(ref);
  if(a_ref<myCryptingKeys) {
	myCryptingKeysModule[a_ref]=string(module); 
	return true;
  }
  return false;
}


bool MvModelFactory_t::UpdateCryptingKeyTimeOut(const char* ref, const char* key, const char* timeout) {
  string a_ref(ref);
  if (a_ref < myCryptingKeys) {
      myCryptingKeysTimeOut[a_ref] = string(timeout);
      return true;
  }
  return false;
}
bool MvModelFactory_t::UpdateCryptingKeyInterfaceID(const char* ref, const char* key, const char* interfaceid) {
  string a_ref(ref);
  if (a_ref < myCryptingKeys) {
      myCryptingKeysInterfaceID[a_ref] = string(interfaceid);
      return true;
  }
  return false;
}




void MvModelFactory_t::PostTreatCryptingKeys() {

}

const char *MvModelFactory_t::GetCryptingKey(const char *ref) const {
  string a_ref(ref);
  //
  MyCryptingKeys_t::const_iterator a_it=myCryptingKeys.find(a_ref);
  //
  if(a_it==myCryptingKeys.end()) return NULL;
  return (*a_it).second.c_str();
}



const char *MvModelFactory_t::GetCryptingKeyCost(const char *ref) const {
  string a_ref(ref);
  //
  MyCryptingKeysCost_t::const_iterator a_it=myCryptingKeysCost.find(a_ref);
  //
  if(a_it==myCryptingKeysCost.end()) return NULL;
  return (*a_it).second.c_str();
}


const char *MvModelFactory_t::GetCryptingKeyFeature(const char *ref) const {
  string a_ref(ref);
  //
  MyCryptingKeysFeature_t::const_iterator a_it=myCryptingKeysFeature.find(a_ref);
  //
  if(a_it==myCryptingKeysFeature.end()) return NULL;
  return (*a_it).second.c_str();
}
const char *MvModelFactory_t::GetCryptingKeyModule(const char *ref) const {
  string a_ref(ref);
  //
  MyCryptingKeysModule_t::const_iterator a_it=myCryptingKeysModule.find(a_ref);
  //
  if(a_it==myCryptingKeysModule.end()) return NULL;
  return (*a_it).second.c_str();
}
const char* MvModelFactory_t::GetCryptingKeyTimeOut(const char* ref) const {
  string a_ref(ref);
  //
  MyCryptingKeysTimeOut_t::const_iterator a_it = myCryptingKeysTimeOut.find(a_ref);
  //
  if (a_it == myCryptingKeysTimeOut.end()) return NULL;
  return (*a_it).second.c_str();
}
const char* MvModelFactory_t::GetCryptingKeyInterfaceID(const char* ref) const {
  string a_ref(ref);
  //
  MyCryptingKeysInterfaceID_t::const_iterator a_it = myCryptingKeysInterfaceID.find(a_ref);
  //
  if (a_it == myCryptingKeysInterfaceID.end()) return NULL;
  return (*a_it).second.c_str();
}

static int loc_get_first_index(IParameter **MyWorkParamArray, int low, int high, string find_param)
{
    if (high >= low)
    {
        int mid = (high + low) / 2;
        if (mid == 0)
        {
            string param_name_mid;
            if (high == 1)
            {
                IParameter* a_mv_param_obj = MyWorkParamArray[1];
                param_name_mid = a_mv_param_obj->GetName();

                if (param_name_mid == find_param)
                    return 1;
            }

            IParameter* a_mv_param_obj = MyWorkParamArray[mid];
            param_name_mid = a_mv_param_obj->GetName();
            if(param_name_mid == find_param)
                return 0;
            else
                return -1;
        }
        IParameter  *a_mv_param_obj = MyWorkParamArray[mid - 1];
        string param_name_mid_minus = a_mv_param_obj->GetName();
        a_mv_param_obj = MyWorkParamArray[mid];
        string param_name_mid = a_mv_param_obj->GetName();

        if (find_param > param_name_mid_minus && param_name_mid == find_param)
            return mid;
        else if (find_param > param_name_mid)
            return loc_get_first_index(MyWorkParamArray, (mid + 1), high, find_param);
        else if (find_param == param_name_mid_minus && mid == 1)
            return mid - 1;
        else
            return loc_get_first_index(MyWorkParamArray, low, (mid-1), find_param);
    }
    return -1;
}

IParameter* MvModelFactory_t::GetParameterObject(const char *param_str, const int file_index, void **ent_ptr)
{
    IParameter* iparam_obj = NULL;
    if(myParameterData == NULL || !myParameterData->size())
        return iparam_obj;

    int first_index = loc_get_first_index(&(*myParameterData->begin()), 0, (int)myParameterData->size() - 1, param_str);
    if (first_index < 0 || first_index >= myParameterData->size())
        return iparam_obj;

    bool first_regular_param = true;
    for (unsigned int i = first_index; i < myParameterData->size(); i++)
    {
        IParameter *temp_iparam_obj = (*myParameterData)[i];
        if (temp_iparam_obj->GetName() == param_str)
        {
            if (temp_iparam_obj->GetKeywordType() == IParameter::LOCAL)
            {
                if (temp_iparam_obj->GetFileIndex() == file_index)
                {
                    iparam_obj = temp_iparam_obj;
                    break;
                }
            }
            else if (temp_iparam_obj->GetKeywordType() == IParameter::REGULAR && first_regular_param)
            {
                first_regular_param = false;
                iparam_obj = temp_iparam_obj;
            }
            else if (temp_iparam_obj->GetKeywordType() == IParameter::EXPRESSION)
            {
                iparam_obj = temp_iparam_obj;
            }
        }
        else
            break;
    }
    return iparam_obj;
}

bool MvModelFactory_t::SetParameterData(vector<IParameter*> *param_data_sorted)
{
    if (!param_data_sorted)
        return false;

    myParameterData = param_data_sorted;
    return true;
}

int MvModelFactory_t::GetIntParameter(const char *param_str, const int file_index)
{
    int a_value = (int) GetFloatParameter(param_str, file_index);
    return a_value;
}

double MvModelFactory_t::GetFloatParameter(const char *param_str, const int file_index, void** ent_ptr)
{
    double a_value = 0.0;
    IParameter *iparam_obj = GetParameterObject(param_str, file_index, ent_ptr);
    if (iparam_obj)
    {
        IParameter::Type type = iparam_obj->GetType();
        if(type == IParameter::TYPE_DOUBLE || type == IParameter::TYPE_DOUBLE_EXPRESSION)
            a_value = iparam_obj->GetDoubleValue();
        else if(type == IParameter::TYPE_INTEGER || type == IParameter::TYPE_INTEGER_EXPRESSION)
            a_value = (double) iparam_obj->GetIntValue();
    }
    return a_value;
}


string MvModelFactory_t::GetTextParameter(const char *param_str, const int file_index)
{
    string a_value;
    IParameter *iparam_obj = GetParameterObject(param_str, file_index);
    if (iparam_obj && iparam_obj->GetType() == IParameter::TYPE_STRING)
        a_value = iparam_obj->GetStringValue();
    return a_value;
}

IParameter::Type MvModelFactory_t::GetParameterValueType(const char *param_str, const int file_index)
{
    IParameter::Type a_value = IParameter::TYPE_UNKNOWN;
    IParameter *iparam_obj = GetParameterObject(param_str, file_index);
    if (iparam_obj)
        a_value = iparam_obj->GetType();
    return a_value;
}


IMECPreObject* MvModelFactory_t::FindByFullType(const string& fulltype)
{
    const CFGKernel* a_kernel_model = MultiCFGKernelMgr::getInstance().GetCurrentCFGKernel();
    if (!a_kernel_model)
        return nullptr;
    MvFullType_t a_fulltype(*a_kernel_model, fulltype.c_str());
    int entity_type = a_fulltype.getType();

    if (entity_type <= 0 || entity_type >= HCDI_OBJ_TYPE_MAX)
        return nullptr;

    vector<IMECPreObject *>  &a_vec =  myPreobjLst[entity_type];


    for (int i = 0; i < a_vec.size(); i++)
    {
        IMECPreObject* obj = a_vec[i];

        string str = obj->GetKernelFullType();
        if (str == fulltype)
            return obj;
    }
    return nullptr;
}

IMECPreObject* MvModelFactory_t::FindByObjectId(int entity_type, int id)
{
    if (entity_type <= 0 || entity_type >= HCDI_OBJ_TYPE_MAX || id <=0)
        return nullptr;

    vector<IMECPreObject*>& a_vec = myPreobjLst[entity_type];

    for (int i = 0; i < a_vec.size(); i++)
    {
        IMECPreObject* obj = a_vec[i];
        if (obj->GetId() == id)
            return obj;
    }
    return nullptr;
}

void MvModelFactory_t::SortPreObjectsByName(const char* otype)
{
    obj_type_e entity_type = HCDI_get_entitytype(string(otype));
    if (entity_type <= HCDI_OBJ_TYPE_NULL && entity_type >= HCDI_OBJ_TYPE_HC_MAX)
        return;

    vector<IMECPreObject*>& a_vec = myPreobjLst[entity_type];

    if (entity_type == HCDI_OBJ_TYPE_PARAMETERS)
    {
        if (a_vec.size())
        {
            IDescriptor* pdescrp = HCDI_GetDescriptorHandle(a_vec[0]->GetKernelFullType());
            string paramname_skey = GetAttribNameFromDrawable(pdescrp, "_PARAM_NAME");
            sort(a_vec.begin(), a_vec.end(), ParameterComparator(paramname_skey));
        }
    }
}

void MvModelFactory_t::StorePreObject(const int otype, IMECPreObject* pre_object, const InputInfos::IdentifierValuePairList* keywordData)
{
    if (myPreobjLst && otype <= HCDI_OBJ_TYPE_NULL && otype >= HCDI_OBJ_TYPE_HC_MAX)
        return; 

    //vector< MyPairPreobjString > &a_vec = myPreobjCommentLst[otype];
    //MyPairPreobjString a_pair;
    //a_pair.first = pre_object;
    //a_pair.second = (InputInfos::IdentifierValuePairList *)keywordData;

    if (keywordData)
    {
        const char *title_p = pre_object->GetTitle();
        for (int i = 0; i < keywordData->size(); i++)
        {
            const std::pair<std::string, cfgkernel::Variant>& item = (*keywordData)[i];
            string ss = item.first;
            if (!pre_object->GetId() && ss == "id")
            {
                unsigned int id = item.second.getUIntValue();;               
                pre_object->SetId(id);
            }
            else if ((!title_p || (title_p && title_p[0] == '\0')) && ss == "name")
            {
                std::string strval = item.second.getStringValue();
                pre_object->SetTitle(strval.c_str());
            }
            else if (ss == "color")
            {
                int col_id = item.second.getIntValue();
                pre_object->AddIntValue(ss.c_str(), col_id);
            }
        }
    }
    vector<IMECPreObject*>& a_vec = myPreobjLst[otype];
    a_vec.push_back(pre_object);
}

int MvModelFactory_t::AddObjects(const char* otype)
{
    obj_type_e entity_type = HCDI_get_entitytype(string(otype));
    if (entity_type <= HCDI_OBJ_TYPE_NULL && entity_type >= HCDI_OBJ_TYPE_HC_MAX)
        return -1;


    //vector<IMECPreObject*>& a_vec = myPreobjLst[otype];
    //for (int i = 0; i < a_vec.size(); i++)
    //{
    //    AddObject(*a_vec[i]);
    //}
   
    return 0;
}


