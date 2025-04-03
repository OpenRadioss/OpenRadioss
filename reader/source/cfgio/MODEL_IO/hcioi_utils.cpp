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
#include <stdio.h>
#include "string.h"
#include "hcioi_utils.h"

#include "KERNEL/mv_ikeyword_containers.h"
#include "HCDI/hcdi_mv_descriptor.h"
#include "HCDI/hcdi_mec_pre_object.h"
#include <HCDI/hcdi_drawableinf_pre_object.h>
#include <HCDI/hcdi_utils.h>


struct CumulativeSubmodelOffsetInfo {
    std::unordered_map<obj_type_e, int> cumulative_object_offsetMap;
    bool presence = false;
};
//store precomputed cumulative offsets  
std::unordered_map<int, CumulativeSubmodelOffsetInfo> cumulative_pindex_offsetMap;

void killBlanksEnd(char *buffer) {
	if(buffer==NULL)
		return;
	int a_nb_chars=(int)strlen(buffer);
    if(a_nb_chars == 0)
        return;
	char *a_char_p=buffer+a_nb_chars-1;
	while(a_char_p>=buffer && *a_char_p==' ') --a_char_p;
	*(++a_char_p)='\0';
}

bool IsValueDigitInRange(const unsigned int &value, const unsigned int &val_range)
{
    switch(val_range)
    {
    case 0:
        return true;
        break;
    case 1:
        {
            if(value < 10)
                return true;
            else
                return false;
        }
        break;
    case 2:
        {
            if(value < 100)
                return true;
            else
                return false;
        }
        break;
    case 3:
        {
            if(value < 1000)
                return true;
            else
                return false;
        }
        break;
    case 4:
        {
            if(value < 10000)
                return true;
            else
                return false;
        }
        break;
    case 5:
        {
            if(value < 100000)
                return true;
            else
                return false;
        }
        break;
    case 6:
        {
            if(value < 1000000)
                return true;
            else
                return false;
        }
        break;
    case 7:
        {
            if(value < 10000000)
                return true;
            else
                return false;
        }
        break;
    case 8:
        {
            if(value < 100000000)
                return true;
            else
                return false;
        }
        break;
    case 9:
        {
            if(value < 1000000000)
                return true;
            else
                return false;
        }
        break;
    default:
        break;
    }
    return false;
}

bool CopyPreObjectAttribFromPreObject(IMECPreObject& preobject_to, const string& attrib_to, const IDescriptor* descrp_to, const IMECPreObject& preobject_from,
                           const string& attrib_from, int index)
{
    const IDescriptor* a_from_descr_p = HCDI_GetDescriptorHandle(preobject_from.GetKernelFullType());
    if (!a_from_descr_p)
        return false;

    int a_ikeyword_from = a_from_descr_p->getIKeyword(attrib_from);
    if (a_ikeyword_from <= 0)
        return false;

    value_type_e a_vtype = descrp_to->getValueType(a_ikeyword_from);
    attribute_type_e a_atype = descrp_to->getAttributeType(a_ikeyword_from);
    bool result = false;
    if (a_atype == ATYPE_VALUE || a_atype == ATYPE_SIZE)
    {
        switch (a_vtype)
        {
        case VTYPE_BOOL:
        {
            preobject_to.AddBoolValue(attrib_to.c_str(), preobject_from.GetBoolValue(attrib_from.c_str()));
        }
        break;
        case VTYPE_INT:
        {
            preobject_to.AddIntValue(attrib_to.c_str(), preobject_from.GetIntValue(attrib_from.c_str()));
        }
        break;
        case VTYPE_UINT:
        {
            preobject_to.AddUIntValue(attrib_to.c_str(), preobject_from.GetUIntValue(attrib_from.c_str()));
        }
        break;
        case VTYPE_FLOAT:
        {
            preobject_to.AddFloatValue(attrib_to.c_str(), preobject_from.GetFloatValue(attrib_from.c_str()));
        }
        break;
        case VTYPE_STRING:
        {
            preobject_to.AddStringValue(attrib_to.c_str(), preobject_from.GetStringValue(attrib_from.c_str()));
        }
        break;
        default:
            break;
        }
    }
    else if (a_atype == ATYPE_SIZE)
    {
        int a_size = preobject_from.GetIntValue(attrib_from.c_str());

        int a_size_index = preobject_to.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, attrib_to.c_str());
        if(a_size_index >=0)
            preobject_to.SetIntValue(a_size_index, a_size);
        else
            preobject_to.AddIntValue(attrib_to.c_str(), a_size);

        int size_ikey = descrp_to->getIKeyword(attrib_to);
        MvIKeywordSet_t       a_array_ikws;
        descrp_to->getSizeConnectedIKeywords(size_ikey, &a_array_ikws);
        if (a_array_ikws.size())
        {
            MvIKeywordSet_t::iterator a_aikw_it_begin = a_array_ikws.begin();
            MvIKeywordSet_t::iterator a_aikw_it_end = a_array_ikws.end();
            MvIKeywordSet_t::iterator a_aikw_it;

            for (a_aikw_it = a_aikw_it_begin; a_aikw_it != a_aikw_it_end; ++a_aikw_it) {
                int    a_arr_ikw = (*a_aikw_it);
                ResizeArrayAttributesToPreObject(preobject_to, descrp_to, a_arr_ikw, a_size);
            }
        }
    }
    else if (a_atype == ATYPE_DYNAMIC_ARRAY) /*Not yet supported*/
    {
        //int a_size = 0;

        //int a_ikeyword_to = a_from_descr_p->getIKeyword(attrib_to);
        //int a_size_ikw_from = a_from_descr_p->getSizeIKeyword(a_ikeyword_from);
        //int a_size_ikw_to = descrp_to->getSizeIKeyword(a_ikeyword_to);

        //string a_size_skey_to = descrp_to->getSKeyword(a_size_ikw_to);
        //int a_size_ind_from = preobject_from.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_size_skey_from.c_str());

        //if (a_size_ind_from <= 0)
        //    return false;

        //int a_nb_values_from = preobject_from.GetIntValue(a_size_ind_from);

        //preobject_to.AddIntValue(a_size_skey_to.c_str(), a_nb_values_from);

        //switch (a_vtype)
        //{
        //case VTYPE_INT:
        //{
        //    int a_array_index = preobject_from.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, attrib_from.c_str());
        //    if (a_array_index >= 0)
        //    {
        //        int a_arr_size = preobject_from.GetNbValues(IMECPreObject::VTY_INT, a_array_index);
        //        if (a_arr_size < a_nb_values_from)
        //            preobject_to.resizeArray(IMECPreObject::VTY_INT, a_array_index, a_nb_values_from);
        //    }
        //    else
        //    {
        //        preobject_to.AddIntArray(attrib_from_update.c_str(), a_nb_values);
        //        a_array_index = preobject_to.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, attrib_from.c_str());
        //    }

        //   // preobject_to.AddIntValues();


        //    break;
        //}
        //default:
        //    break;
        //}
    }
    return true;
}

void MergeArrayAttributesToPreobject(const IDescriptor* pdescrp, IMECPreObject& preobject_base, IMECPreObject& preobject_to_merge)
{
    int i, j, k, l;
    int a_nb_atypes = IMECPreObject::ATY_LAST - IMECPreObject::ATY_UNKNOWN - 1;
    int a_nb_vtypes = IMECPreObject::VTY_LAST - IMECPreObject::VTY_UNKNOWN - 1;
    //
    for (i = 0; i < a_nb_atypes; ++i) {
        IMECPreObject::MyAttributeType_e a_atype = (IMECPreObject::MyAttributeType_e)(IMECPreObject::ATY_UNKNOWN + i + 1);
        //
        for (j = 0; j < a_nb_vtypes; ++j) {
            IMECPreObject::MyValueType_e a_vtype = (IMECPreObject::MyValueType_e)(IMECPreObject::VTY_UNKNOWN + j + 1);
            int           a_nb_attributes      = preobject_to_merge.GetNbAttributes(a_atype, a_vtype);
            int           a_nb_attributes_base = preobject_base.GetNbAttributes(a_atype, a_vtype);
            if (a_nb_attributes != a_nb_attributes_base)
                continue;
            //
            for (k = 0; k < a_nb_attributes; ++k) {
                const char* a_keyword = preobject_to_merge.GetKeyword(a_atype, a_vtype, k);
                const char* a_keyword_base = preobject_base.GetKeyword(a_atype, a_vtype, k);
                if (strcmp(a_keyword, a_keyword_base) != 0)
                    continue;

                //
                switch (a_atype) {
                case IMECPreObject::ATY_SINGLE:
                    break;
                case IMECPreObject::ATY_ARRAY:
                {
                    int a_nb_values = preobject_to_merge.GetNbValues(a_vtype, k);
                    if (!a_nb_values)
                        continue;

                    int a_nb_values_base = preobject_base.GetNbValues(a_vtype, k);
                    int total_size = a_nb_values_base + a_nb_values;


                    const IDescriptor* a_descr_p = (const IDescriptor*)pdescrp;

                    int arr_ikw = a_descr_p->getIKeyword(a_keyword);
                    //Getting the Current array size
                    int a_size_ikeyword = a_descr_p->getSizeIKeyword(arr_ikw);
                    if (a_size_ikeyword <= 0)
                        continue;
                    string a_size_skeyword = a_descr_p->getSKeyword(a_size_ikeyword);
                    int a_size_index = preobject_base.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_size_skeyword.c_str());
                    int a_size = 0;
                    if (a_size_index >= 0)
                        a_size = preobject_base.GetIntValue(a_size_index);
                    if (a_size < total_size)
                    {
                        if (a_size_index >= 0)
                            preobject_base.SetIntValue(a_size_index, total_size);
                        else preobject_base.AddIntValue(a_size_skeyword.c_str(), total_size);
                    }

                    //
                    switch (a_vtype) {
                    case IMECPreObject::VTY_BOOL:
                        preobject_base.AddBoolArray(a_keyword, total_size);
                        for (a_nb_values_base = 0; l < total_size; ++l) preobject_base.SetBoolValue(k, l, preobject_to_merge.GetBoolValue(k, l - a_nb_values_base));
                        break;
                    case IMECPreObject::VTY_INT:
                        preobject_base.AddIntArray(a_keyword, total_size);
                        for (l = a_nb_values_base; l < total_size; ++l) preobject_base.SetIntValue(k, l, preobject_to_merge.GetIntValue(k, l - a_nb_values_base));
                        break;
                    case IMECPreObject::VTY_UINT:
                        preobject_base.AddUIntArray(a_keyword, (unsigned int)total_size);
                        for (l = a_nb_values_base; l < total_size; ++l) preobject_base.SetUIntValue(k, l, preobject_to_merge.GetUIntValue(k, l - a_nb_values_base));
                        break;
                    case IMECPreObject::VTY_FLOAT:
                        preobject_base.AddFloatArray(a_keyword, total_size);
                        for (l = a_nb_values_base; l < total_size; ++l) preobject_base.SetFloatValue(k, l, preobject_to_merge.GetFloatValue(k, l- a_nb_values_base));
                        break;
                    case IMECPreObject::VTY_STRING:
                        preobject_base.AddStringArray(a_keyword, total_size);
                        for (l = a_nb_values_base; l < total_size; ++l) preobject_base.SetStringValue(k, l, preobject_to_merge.GetStringValue(k, l - a_nb_values_base));
                        break;
                    case IMECPreObject::VTY_OBJECT:
                        preobject_base.AddObjectArray(a_keyword, total_size);
                        for (l = a_nb_values_base; l < total_size; ++l) {
                            const char* a_otype = preobject_to_merge.GetObjectType(k, l - a_nb_values_base);
                            const char* a_name = preobject_to_merge.GetObjectName(k, l - a_nb_values_base);
                            int         a_id = preobject_to_merge.GetObjectId(k, l - a_nb_values_base);
                            int         a_index = preobject_to_merge.GetObjectIndex(k, l - a_nb_values_base);
                            if (a_name[0] != '\0')
                                preobject_base.SetObjectValue(k, l, a_otype, a_name, a_index);
                            else
                                preobject_base.SetObjectValue(k, l, a_otype, a_id, a_index);
                        }
                        break;
                    default:
                        // Wrong type of value
                        break;
                    }
                }
                break;
                default:
                    // Wrong type of attribute
                    break;
                }
            }
        }
    }
    //preobject_base.myParameters = preobject_to_merge.myParameters;
    //preobject_base.myParamIdName = preobject_to_merge.myParamIdName;
    //preobject_base.myIsNegatedParameters = preobject_to_merge.myIsNegatedParameters;

    const vector<IMECPreObject*>& a_subobj = preobject_to_merge.GetSubobject();

    size_t a_size = a_subobj.size();

    for (size_t ii = 0; ii < a_size; ii++)
    {
        preobject_base.SetSubobject(a_subobj[ii]);
    }
}



void ResizeArrayAttributesToPreObject(IMECPreObject& pre_object, const IDescriptor* descr_p, int arr_ikw, int size)
{
    const IDescriptor* a_descr_p = (const IDescriptor*)descr_p;
    string a_arr_skw = a_descr_p->getSKeyword(arr_ikw);
    //Getting the Current array size
    int a_size_ikeyword = a_descr_p->getSizeIKeyword(arr_ikw);
    if (a_size_ikeyword <= 0)
        return;

    string a_size_skeyword = a_descr_p->getSKeyword(a_size_ikeyword);
    int a_size_index = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_size_skeyword);
    int a_size = 0;
    if (a_size_index >= 0)
        a_size = pre_object.GetIntValue(a_size_index);
    if (a_size < size)
    {
        if (a_size_index >= 0)
            pre_object.SetIntValue(a_size_index, size);
        else pre_object.AddIntValue(a_size_skeyword.c_str(), size);
    }
    
    /* Allocating the array in to the preobject */
    switch (a_descr_p->getValueType(arr_ikw))
    {
    case VTYPE_BOOL:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_BOOL, a_arr_skw);
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_BOOL, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_BOOL, a_array_index, size);
                for (int i = a_nb_values; i < size; i++)
                    pre_object.AddBoolValue(a_arr_skw.c_str(), i, false);
            }
        }
        else
        {
            pre_object.AddBoolArray(a_arr_skw.c_str(), size);
        }
        break;
    }
    case VTYPE_INT:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_arr_skw);
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_INT, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_INT, a_array_index, size);
            }
        }
        else
        {
            pre_object.AddIntArray(a_arr_skw.c_str(), size);
        }
        break;
    }

    case VTYPE_UINT:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_arr_skw);
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_UINT, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_UINT, a_array_index, size);
            }
        }
        else
        {
            pre_object.AddUIntArray(a_arr_skw.c_str(), size);
        }
        break;
    }
    case VTYPE_FLOAT:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, a_arr_skw);
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_FLOAT, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_FLOAT, a_array_index, size);
            }
        }
        else
        {
            pre_object.AddFloatArray(a_arr_skw.c_str(), size);
        }
        break;
    }
    case VTYPE_STRING:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, a_arr_skw);
        string stringvalue = "";
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_STRING, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_STRING, a_array_index, size);
            }
        }
        else
        {
            pre_object.AddStringArray(a_arr_skw.c_str(), size);
        }
        break;
    }
    case VTYPE_OBJECT:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, a_arr_skw);
        int otype = a_descr_p->getObjectType(arr_ikw);
        string otype_str = HCDI_get_entitystringtype(otype);
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_OBJECT, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_OBJECT, a_array_index, size);
            }
        }
        else
        {
            pre_object.AddObjectArray(a_arr_skw.c_str(), size);
        }
        break;
    }
    default:
        break;
    }
    return;
}

std::string GetAttribNameFromDrawable(const IDescriptor* pdescrp, const string& name)
{
    if (!pdescrp)  return name;
    const MvDrawable_t* a_drawable_p1 = pdescrp->getDrawablePtr(name);
    if (a_drawable_p1) 
        return pdescrp->getSKeyword(a_drawable_p1->getIKeyword());

    return name;
}

//Function to calculate cumulative offset
void CalculateCumulativeOffset(std::vector<IMECPreObject*>* p_preobjlst, const char* submodelsolkey)
{
    int tot_includes = (int)p_preobjlst[HCDI_OBJ_TYPE_INCLUDEFILES].size();
    if (!tot_includes)
        return;
    cumulative_pindex_offsetMap.clear();
    for (int i = 0; i < tot_includes; i++)
    {
        CumulativeSubmodelOffsetInfo coff;
        coff.cumulative_object_offsetMap.reserve(HCDI_OBJ_TYPE_MAX);
        cumulative_pindex_offsetMap[i] = coff;
    }
    IMECPreObject* parent_obj = p_preobjlst[HCDI_OBJ_TYPE_INCLUDEFILES][0];
    int subtype = HCDI_OBJ_TYPE_SOLVERSUBMODELS;
    int current_index = 1;// parent_obj->GetFileIndex();
    int cumulative = 0;

    int index = 0;
    for (int i = 0; i < tot_includes; i++)
    {
        IMECPreObject* parent_obj = p_preobjlst[HCDI_OBJ_TYPE_INCLUDEFILES][i];
        if(nullptr == parent_obj) continue; // shouldn't happen
        index = parent_obj->GetFileIndex();
        bool has_submodel = false;
        bool only_once = false;
        while (0 <= index) // the root has index -1, no need to go through here
        {
            const char* kfulltype1 = parent_obj->GetKernelFullType();
            if (strstr(kfulltype1, submodelsolkey))
                has_submodel = true;
            if (has_submodel)
            {
                const IDescriptor* a_descr_p = HCDI_GetDescriptorHandle(parent_obj->GetKernelFullType());
                if (!a_descr_p) break; // shouldn't happen

                hwCFGDrawableInfPreObject drawable_inf(parent_obj, a_descr_p);

                const MvDrawable_t* def_drawable_p = a_descr_p->getDrawablePtr("_DEFAULT_IDOFFSET");
                int def_offset = 0;
                if(nullptr != def_drawable_p) def_offset = (int) def_drawable_p->evaluate(&drawable_inf);

                for(int inttype = HCDI_OBJ_TYPE_NULL + 1; inttype < HCDI_OBJ_TYPE_MAX; ++inttype)
                {
                    obj_type_e type = (obj_type_e) inttype;
                    char drawable_name[100];
                    sprintf(drawable_name, "_%s_IDOFFSET", MV_get_type(type).c_str());
                    const MvDrawable_t* drawable_p = a_descr_p->getDrawablePtr(drawable_name);
                    int offset = 0;
                    if(nullptr != drawable_p) offset = (int) drawable_p->evaluate(&drawable_inf);
                    else if(0 != def_offset)  offset = def_offset;
                    if (offset != 0)
                    {
                        CumulativeSubmodelOffsetInfo& csub = cumulative_pindex_offsetMap[i];
                        csub.cumulative_object_offsetMap[type] += offset; // Store value in the inner map
                        csub.presence = true; // Mark presence
                    }
                }
            }
            parent_obj = p_preobjlst[HCDI_OBJ_TYPE_INCLUDEFILES][index];
            index = parent_obj->GetFileIndex();
        }
    }
}
void TransformWithOffset(int submodelindex, IMECPreObject* preObj, bool doUnOffset, obj_type_e hm_type)
{
    if (preObj == nullptr || submodelindex < 0) return;


    //std::unordered_map<int, CumulativeSubmodelOffsetInfo> cumulative_pindex_offsetMap;
    CumulativeSubmodelOffsetInfo& subinfo = cumulative_pindex_offsetMap[submodelindex];
    if (!subinfo.presence)
        return;

    std::unordered_map<obj_type_e, int>& cumulobjinfo = subinfo.cumulative_object_offsetMap;

    // for all entities except /SET/COLLECT offset the own id
    if (preObj->GetId() > 0)
    {
        int offsetval = cumulobjinfo[hm_type];
        if (doUnOffset) preObj->SetId(preObj->GetId() - offsetval);
        else           preObj->SetId(preObj->GetId() + offsetval);
    }
    // offset the unit
    if (preObj->GetUnitId() > 0)
    {
        int offsetval = cumulobjinfo[HCDI_OBJ_TYPE_UNITS];
        if (doUnOffset) preObj->SetUnitId(preObj->GetUnitId() - offsetval);
        else            preObj->SetUnitId(preObj->GetUnitId() + offsetval);
    }

    // references
    int i, k, l;
    int a_nb_atypes = IMECPreObject::ATY_LAST - IMECPreObject::ATY_UNKNOWN - 1;
    for (i = 0; i < a_nb_atypes; ++i)
    {
        IMECPreObject::MyAttributeType_e a_atype = (IMECPreObject::MyAttributeType_e)(IMECPreObject::ATY_UNKNOWN + i + 1);
        //
        int           a_nb_attributes = preObj->GetNbAttributes(a_atype, IMECPreObject::VTY_OBJECT);
        //
        for (k = 0; k < a_nb_attributes; ++k)
        {
            switch (a_atype)
            {
            case IMECPreObject::ATY_SINGLE:
            {
                int         a_id = preObj->GetObjectId(k);
                if (0 < a_id)
                {
                    const char* a_otype = preObj->GetObjectType(k);
                    int         a_index = preObj->GetObjectIndex(k);
                    obj_type_e a_hm_type = HCDI_get_entitytype(a_otype);

                    int offsetval = 0;
                    if (hm_type == HCDI_OBJ_TYPE_MULTIOBJECT)
                    {
                        const char* kwd = preObj->GetKeyword(a_atype, IMECPreObject::VTY_OBJECT, k);
                        if (kwd)
                        {
                            string typekwd = string(kwd) + "_type";
                            const char* a_otype = preObj->GetStringValue(typekwd.c_str());
                            if (a_otype && strlen(a_otype) >= 1)
                            {
                                a_hm_type = HCDI_get_entitytype(a_otype + 1);
                                offsetval = cumulobjinfo[a_hm_type];
                            }
                        }
                    }
                    else
                        offsetval = cumulobjinfo[hm_type];

                    if (doUnOffset) a_id -= offsetval;
                    else            a_id += offsetval;

                    preObj->SetObjectValue(k, a_otype, a_id, a_index);
                }
            }
            break;
            case IMECPreObject::ATY_ARRAY:
            {
                int a_nb_values = preObj->GetNbValues(IMECPreObject::VTY_OBJECT, k);
                for (l = 0; l < a_nb_values; ++l)
                {
                    int         a_id = preObj->GetObjectId(k, l);
                    if (0 < a_id)
                    {
                        const char* a_otype = preObj->GetObjectType(k, l);
                        int         a_index = preObj->GetObjectIndex(k, l);
                        obj_type_e a_hm_type = HCDI_get_entitytype(a_otype);
                        int offsetval = 0;
                        if (hm_type == HCDI_OBJ_TYPE_MULTIOBJECT)
                        {
                            const char* kwd = preObj->GetKeyword(a_atype, IMECPreObject::VTY_OBJECT, k);
                            if (kwd)
                            {
                                string typekwd = string(kwd) + "_type";
                                const char* a_otype = preObj->GetStringValue(typekwd.c_str());
                                if (a_otype && strlen(a_otype) >= 1)
                                {
                                    a_hm_type = HCDI_get_entitytype(a_otype + 1);
                                    offsetval = cumulobjinfo[a_hm_type];
                                }
                            }
                        }
                        else
                            offsetval = cumulobjinfo[hm_type];

                        if (doUnOffset) a_id -= offsetval;
                        else           a_id += offsetval;

                        preObj->SetObjectValue(k, l, a_otype, a_id, a_index);
                    }
                }
            }
            break;
            default:
                break;
            }
        }
    }
    
    const std::vector<IMECPreObject*>& subobj = preObj->GetSubobject();

    for (int i = 0; i < subobj.size(); ++i)
    {
        int hm_type = (int)HCDI_GetHCObjectType(preObj->GetKernelFullType());
        TransformWithOffset(submodelindex, subobj[i], doUnOffset, (obj_type_e)hm_type);
    }
}

static void TransformWithOffsetIntArray(IMECPreObject* preObj,
                                        int k, // attribindex, naming for consistency with function above
                                        int offsetval, bool doUnOffset)
{
    if (0 == offsetval) return;
    int a_nb_values = preObj->GetNbValues(IMECPreObject::VTY_INT, k);
    for (int l = 0; l < a_nb_values; ++l)
    {
        int a_id = preObj->GetIntValue(k, l);
        if (0 < a_id)
        {
            if (doUnOffset) a_id -= offsetval;
            else            a_id += offsetval;
            preObj->SetIntValue(k, l, a_id);
        }
    }
}

void UpdateEntityIDOffsetCFG(std::vector<IMECPreObject*>* p_preobjlst, const char* submodelsolkey, bool doUnOffset)
{
    cumulative_pindex_offsetMap.clear();
    CalculateCumulativeOffset(p_preobjlst, submodelsolkey);

    if (!cumulative_pindex_offsetMap.size())
        return;
    int incl_totcount = (int)p_preobjlst[HCDI_OBJ_TYPE_INCLUDEFILES].size();

    // int ids in nodes
    for (auto* preObj : p_preobjlst[HCDI_OBJ_TYPE_NODES])
    {
        if (preObj == NULL) continue;
        int fileindex = preObj->GetFileIndex();
        if (fileindex <= 0) continue;
        int attribindex = preObj->GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, "id");
        if(0 > attribindex) continue;
        CumulativeSubmodelOffsetInfo& subinfo = cumulative_pindex_offsetMap[fileindex];
        if (!subinfo.presence) continue;
        std::unordered_map<obj_type_e, int>& cumulobjinfo = subinfo.cumulative_object_offsetMap;
        int nodeoffset = cumulobjinfo[HCDI_OBJ_TYPE_NODES];
        TransformWithOffsetIntArray(preObj, attribindex, nodeoffset, doUnOffset);
    }

    // int ids in elements
    for (auto* preObj : p_preobjlst[HCDI_OBJ_TYPE_ELEMS])
    {
        if (preObj == NULL) continue;
        int fileindex = preObj->GetFileIndex();
        if (fileindex <= 0) continue;
        int nb_attributes = preObj->GetNbAttributes(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT);
        if(0 == nb_attributes) continue;
        CumulativeSubmodelOffsetInfo& subinfo = cumulative_pindex_offsetMap[fileindex];
        if (!subinfo.presence) continue;
        std::unordered_map<obj_type_e, int>& cumulobjinfo = subinfo.cumulative_object_offsetMap;
        int nodeoffset = cumulobjinfo[HCDI_OBJ_TYPE_NODES];
        int elemoffset = cumulobjinfo[HCDI_OBJ_TYPE_ELEMS];
        int partoffset = cumulobjinfo[HCDI_OBJ_TYPE_COMPS];
        for(int k = 0; k < nb_attributes; ++k)
        {
            const char* skwd = preObj->GetKeyword(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, k);
            if (strcmp(skwd, "id") == 0)
            {
                TransformWithOffsetIntArray(preObj, k, elemoffset, doUnOffset);
            }
            else if (strncmp(skwd,"node", 4) == 0)
            {
                TransformWithOffsetIntArray(preObj, k, nodeoffset, doUnOffset);
            }
            else if (strcmp(skwd, "collector") == 0)
            {
                TransformWithOffsetIntArray(preObj, k, partoffset, doUnOffset);
            }
        }
    }

    for (int i = 0; i < (int)HCDI_OBJ_TYPE_MAX; ++i)
    {
        for (auto* preObj : p_preobjlst[i])
        {
            if (preObj == NULL) continue;

            int fileindex = preObj->GetFileIndex();
            if (fileindex <= 0)
                continue;

            CumulativeSubmodelOffsetInfo& subinfo = cumulative_pindex_offsetMap[fileindex];

            if (!subinfo.presence)
                continue;

            TransformWithOffset(fileindex, preObj, doUnOffset, (obj_type_e)i);
        }
    }
}
