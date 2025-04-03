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
#include <KERNEL/mv_descriptor.h>
#include <KERNEL/mv_expression.h>
#include <KERNEL_BASE/expression_API.h>

#include "mec_pre_object.h"
#include "hcdi_mec_pre_object.h"
#include "hcdi_mv_descriptor.h"
#include <KERNEL/mv_descriptor.h>
#include "hcdi.h"

HC_DATA_DLL_API IMECPREOBJECT HCDI_GetPreObjectHandle(const char *kernel_full_type, const char *input_full_type,const char *title,int id,int unit_id)
{
    IMECPreObject *pre_obj =  new MECPreObject(kernel_full_type, input_full_type, title, id, unit_id);
	return (IMECPREOBJECT)pre_obj;
}
HC_DATA_DLL_API IMECPREOBJECT HCDI_GetPreObjectHandleFromOtherPreObject(IMECPreObject *p_pre_obj)
{
    IMECPreObject *pre_obj =  new MECPreObject(*p_pre_obj);
	return (IMECPREOBJECT)pre_obj;
}
HC_DATA_DLL_API void HCDI_ReleasePreObjectHandle(IMECPreObject *p_pre_obj)
{
    if(p_pre_obj)
        p_pre_obj->Release();
}


HC_DATA_DLL_API obj_type_e HCDI_GetHCObjectType(string fulltype)
{
    obj_type_e etype = HCDI_OBJ_TYPE_NULL;
    if (fulltype[0] != '/') {
        etype = MV_get_type(fulltype);
    }
    else {
        string a_str = "";
        int i = 1, n = (int)(fulltype.size());
        while (i < n && fulltype[i] != '/') a_str += fulltype[i++];
        etype = MV_get_type(a_str);
    }
    return etype;
}

HC_DATA_DLL_API bool HCDI_UpdatePreObjectConnectedSizeIkeywords(IMECPreObject& pre_object, const PseudoDescriptor_t* descr_p, int size_ikey)
{
    const IDescriptor* a_descr_p = (const IDescriptor*)descr_p;
    if (a_descr_p == NULL)
        return false;
    MvIKeywordSet_t       a_array_ikws;
    a_descr_p->getSizeConnectedIKeywords(size_ikey, &a_array_ikws);
    if (a_array_ikws.size())
    {
        int a_attrib_index = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, (a_descr_p->getSKeyword(size_ikey)).c_str());
        int a_size = pre_object.GetIntValue(a_attrib_index);
        MvIKeywordSet_t::iterator a_aikw_it_begin = a_array_ikws.begin();
        MvIKeywordSet_t::iterator a_aikw_it_end = a_array_ikws.end();
        MvIKeywordSet_t::iterator a_aikw_it;

        for (a_aikw_it = a_aikw_it_begin; a_aikw_it != a_aikw_it_end; ++a_aikw_it) {
            int    a_arr_ikw = (*a_aikw_it);
            HCDI_AddArrayAttributesToPreObject(pre_object, descr_p, a_arr_ikw, a_size, false);
        }
    }
    return true;
}

HC_DATA_DLL_API bool HCDI_UpdatePreObjectValue(IMECPreObject& pre_object, const PseudoDescriptor_t* descr_p, int attrib_ikey, double value, std::string& str_val, int ind)
{
    const IDescriptor* a_descr_p = (const IDescriptor*)descr_p;
    value_type_e a_result = a_descr_p->getValueType(attrib_ikey);
    string skeyword = a_descr_p->getSKeyword(attrib_ikey);
    switch (a_result)
    {
    case VTYPE_BOOL:
    {
        bool a_val = false;
        if (value >= 1)
            a_val = true;
        if (ind < 0)
        {
            pre_object.AddBoolValue(skeyword.c_str(), a_val);
        }
        else
        {
            attribute_type_e a_atype = a_descr_p->getAttributeType(attrib_ikey);
            int a_cell_ind = -1;
            if (a_atype == ATYPE_VALUE)
            {
                pre_object.AddBoolValue(skeyword.c_str(), ind, a_val);
            }
            else if (a_atype == ATYPE_DYNAMIC_ARRAY)
            {
                a_cell_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_BOOL, skeyword);
                if (a_cell_ind >= 0)
                    pre_object.SetBoolValue(a_cell_ind, ind, a_val);
            }
        }
    }
    break;
    case VTYPE_INT:
    {
        attribute_type_e a_atype = a_descr_p->getAttributeType(attrib_ikey);
        if (ind < 0)
        {
            pre_object.AddIntValue(skeyword.c_str(), (int)value);
            
            if (a_atype == ATYPE_SIZE)
                HCDI_UpdatePreObjectConnectedSizeIkeywords(pre_object, descr_p, attrib_ikey);
        }
        else
        {
            int a_cell_ind = -1;
            if (a_atype == ATYPE_VALUE)
            {
                pre_object.AddIntValue(skeyword.c_str(), ind, (int)value);
            }
            else if (a_atype == ATYPE_DYNAMIC_ARRAY)
            {
                a_cell_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, skeyword);
                if (a_cell_ind >= 0)
                    pre_object.SetIntValue(a_cell_ind, ind, (int)value);
            }
        }
    }
    break;
    case VTYPE_UINT:
    {
        if (ind < 0)
        {
            pre_object.AddUIntValue(skeyword.c_str(), (unsigned int)value);
        }
        else
        {
            attribute_type_e a_atype = a_descr_p->getAttributeType(attrib_ikey);
            int a_cell_ind = -1;
            if (a_atype == ATYPE_VALUE)
            {
                pre_object.AddUIntValue(skeyword.c_str(), ind, (unsigned int)value);
            }
            else if (a_atype == ATYPE_DYNAMIC_ARRAY)
            {
                a_cell_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, skeyword);
                if (a_cell_ind >= 0)
                    pre_object.SetUIntValue(a_cell_ind, ind, (unsigned int)value);
            }
        }
    }
    break;
    case VTYPE_FLOAT:
    {
        if (ind < 0)
        {
            pre_object.AddFloatValue(skeyword.c_str(), value);
        }
        else
        {
            attribute_type_e a_atype = a_descr_p->getAttributeType(attrib_ikey);
            int a_cell_ind = -1;
            if (a_atype == ATYPE_VALUE)
            {
                pre_object.AddFloatValue(skeyword.c_str(), ind, value);
            }
            else if (a_atype == ATYPE_DYNAMIC_ARRAY)
            {
                a_cell_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, skeyword);
                if (a_cell_ind >= 0)
                    pre_object.SetFloatValue(a_cell_ind, ind, value);
            }
        }
    }
    break;
    case VTYPE_OBJECT:
    {
        if (ind < 0)
        {
            int otype = a_descr_p->getObjectType(attrib_ikey);
            string otype_str = HCDI_get_entitystringtype(otype);
            if (str_val != "")
                pre_object.AddObjectValue(skeyword.c_str(), otype_str.c_str(), str_val.c_str());
            else
                pre_object.AddObjectValue(skeyword.c_str(), otype_str.c_str(), (MYOBJ_INT)value);
        }
        else
        {
            int attrib_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, skeyword);
            if (attrib_ind >= 0)
            {
                const char* a_otype = pre_object.GetObjectType(attrib_ind);
                if (str_val != "")
                    pre_object.SetObjectValue(attrib_ind, ind, a_otype, str_val.c_str());
                else
                    pre_object.SetObjectValue(attrib_ind, ind, a_otype, (MYOBJ_INT)value);
            }
        }
    }
    break;
    default:
        break;
    }
    return true;
}

static void DescTypeToPreobjType(attribute_type_e desc_a_type, value_type_e desc_v_type, IMECPreObject::MyAttributeType_e& atype, IMECPreObject::MyValueType_e& vtype)
{
    if (desc_a_type == ATYPE_VALUE || desc_a_type == ATYPE_SIZE)
        atype = IMECPreObject::ATY_SINGLE;
    else if (desc_a_type == ATYPE_STATIC_ARRAY || desc_a_type == ATYPE_DYNAMIC_ARRAY)
        atype = IMECPreObject::ATY_ARRAY;

    switch (desc_v_type)
    {
    case VTYPE_BOOL:
        vtype = IMECPreObject::VTY_BOOL;
        break;
    case VTYPE_INT:
        vtype = IMECPreObject::VTY_INT;
        break;
    case VTYPE_UINT:
        vtype = IMECPreObject::VTY_UINT;
        break;
    case VTYPE_FLOAT:
        vtype = IMECPreObject::VTY_FLOAT;
        break;
    case VTYPE_STRING:
        vtype = IMECPreObject::VTY_STRING;
        break;
    case VTYPE_OBJECT:
        vtype = IMECPreObject::VTY_OBJECT;
        break;
    }
    return;
}

HC_DATA_DLL_API void HCDI_AddArrayAttributesToPreObject(IMECPreObject& pre_object, const PseudoDescriptor_t* descr_p, int arr_ikw, int size, bool update_coon_size_ikey)
{
    const IDescriptor* a_descr_p = (const IDescriptor*)descr_p;
    string a_arr_skw = a_descr_p->getSKeyword(arr_ikw);
    //Getting the Current array size
    int a_size_ikeyword = a_descr_p->getSizeIKeyword(arr_ikw);
    if (a_size_ikeyword > 0)
    {
        /* If this value is false it means that the size ikeyword is already set to the correct value i.e.,
           when the call comes from the HCDI_UpdatePreObjectConnectedSizeIkeywords and hence no need to update the size ikeyword value again */
        if (update_coon_size_ikey)
        {
            string a_size_skeyword = a_descr_p->getSKeyword(a_size_ikeyword);
            int a_size_index = pre_object.GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, a_size_skeyword.c_str());
            int a_size = 0;
            if (a_size_index >= 0)
                a_size = pre_object.GetIntValue(a_size_index);
            if (a_size < size)
            {
                if (a_size_index >= 0)
                    pre_object.SetIntValue(a_size_index, size);
                else pre_object.AddIntValue(a_size_skeyword.c_str(), size);
            }
        }
    }
    else
    {
        // Descriptor and Preobject Attribute and value type of attribute to which value has to be assigned(First Argument of Assign card)
        attribute_type_e des_atype = a_descr_p->getAttributeType(arr_ikw);
        value_type_e des_vtype = a_descr_p->getValueType(arr_ikw);
        IMECPreObject::MyAttributeType_e pre_obj_Atype = IMECPreObject::ATY_UNKNOWN;
        IMECPreObject::MyValueType_e pre_obj_Vtype = IMECPreObject::VTY_UNKNOWN;
        DescTypeToPreobjType(des_atype, des_vtype, pre_obj_Atype, pre_obj_Vtype);
        string assign_card_attrib_skey = a_descr_p->getSKeyword(arr_ikw);
        int a_array_index = pre_object.GetIndex(pre_obj_Atype, pre_obj_Vtype, assign_card_attrib_skey.c_str());
        if (a_array_index >= 0)
            return;
    }
    /* Allocating the array in to the preobject */
    switch (a_descr_p->getValueType(arr_ikw))
    {
    case VTYPE_BOOL:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_BOOL, a_arr_skw.c_str());
        bool boolvalue = false;
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_BOOL, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_BOOL, a_array_index, size);
                for (int i = a_nb_values; i < size; i++)
                    pre_object.AddBoolValue(a_arr_skw.c_str(), i, boolvalue);
            }
        }
        else
        {
            pre_object.AddBoolArray(a_arr_skw.c_str(), size);
            for (int i = 0; i < size; i++)
                pre_object.AddBoolValue(a_arr_skw.c_str(), i, boolvalue);
        }
        break;
    }
    case VTYPE_INT:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_INT, a_arr_skw.c_str());
        int intvalue = INT_MIN;
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_INT, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_INT, a_array_index, size);
                for (int i = a_nb_values; i < size; i++)
                    pre_object.AddIntValue(a_arr_skw.c_str(), i, intvalue);
            }
        }
        else
        {
            pre_object.AddIntArray(a_arr_skw.c_str(), size);
            for (int i = 0; i < size; i++)
                pre_object.AddIntValue(a_arr_skw.c_str(), i, intvalue);
        }
        break;
    }

    case VTYPE_UINT:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_UINT, a_arr_skw.c_str());
        unsigned int uintvalue = UINT_MAX;
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_UINT, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_UINT, a_array_index, size);
                for (int i = a_nb_values; i < size; i++)
                    pre_object.AddUIntValue(a_arr_skw.c_str(), i, uintvalue);
            }
        }
        else
        {
            pre_object.AddUIntArray(a_arr_skw.c_str(), size);
            for (int i = 0; i < size; i++)
                pre_object.AddUIntValue(a_arr_skw.c_str(), i, uintvalue);
        }
        break;
    }
    case VTYPE_FLOAT:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_FLOAT, a_arr_skw.c_str());
        double floatvalue = -DBL_MAX;
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_FLOAT, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_FLOAT, a_array_index, size);
                for (int i = a_nb_values; i < size; i++)
                    pre_object.AddFloatValue(a_arr_skw.c_str(), i, floatvalue);
            }
        }
        else
        {
            pre_object.AddFloatArray(a_arr_skw.c_str(), size);
            for (int i = 0; i < size; i++)
                pre_object.AddFloatValue(a_arr_skw.c_str(), i, floatvalue);
        }
        break;
    }
    case VTYPE_STRING:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, a_arr_skw.c_str());
        string stringvalue = "";
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_STRING, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_STRING, a_array_index, size);
                for (int i = a_nb_values; i < size; i++)
                    pre_object.AddStringValue(a_arr_skw.c_str(), i, stringvalue.c_str());
            }
        }
        else
        {
            pre_object.AddStringArray(a_arr_skw.c_str(), size);
            for (int i = 0; i < size; i++)
                pre_object.AddStringValue(a_arr_skw.c_str(), i, stringvalue.c_str());
        }
        break;
    }
    case VTYPE_OBJECT:
    {
        int a_array_index = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_OBJECT, a_arr_skw.c_str());
        int Obj_Id = UINT_MAX;
        int otype = a_descr_p->getObjectType(arr_ikw);
        string otype_str = HCDI_get_entitystringtype(otype);
        if (a_array_index >= 0)
        {
            int a_nb_values = pre_object.GetNbValues(IMECPreObject::VTY_OBJECT, a_array_index);
            if (a_nb_values < size)
            {
                pre_object.resizeArray(IMECPreObject::VTY_OBJECT, a_array_index, size);
                for (int i = a_nb_values; i < size; i++)
                    pre_object.AddObjectValue(a_arr_skw.c_str(), i, otype_str.c_str(), Obj_Id);
            }
        }
        else
        {
            pre_object.AddObjectArray(a_arr_skw.c_str(), size);
            for (int i = 0; i < size; i++)
                pre_object.AddObjectValue(a_arr_skw.c_str(), i, otype_str.c_str(), Obj_Id);
        }
        break;
    }
    default:
        break;
    }
    return;
}

HC_DATA_DLL_API void HCDI_DescTypeToPreobjType(attribute_type_e desc_a_type, value_type_e desc_v_type, IMECPreObject::MyAttributeType_e& atype, IMECPreObject::MyValueType_e& vtype)
{
    if (desc_a_type == ATYPE_VALUE || desc_a_type == ATYPE_SIZE)
        atype = IMECPreObject::ATY_SINGLE;
    else if (desc_a_type == ATYPE_STATIC_ARRAY || desc_a_type == ATYPE_DYNAMIC_ARRAY)
        atype = IMECPreObject::ATY_ARRAY;

    switch (desc_v_type)
    {
    case VTYPE_BOOL:
        vtype = IMECPreObject::VTY_BOOL;
        break;
    case VTYPE_INT:
        vtype = IMECPreObject::VTY_INT;
        break;
    case VTYPE_UINT:
        vtype = IMECPreObject::VTY_UINT;
        break;
    case VTYPE_FLOAT:
        vtype = IMECPreObject::VTY_FLOAT;
        break;
    case VTYPE_STRING:
        vtype = IMECPreObject::VTY_STRING;
        break;
    case VTYPE_OBJECT:
        vtype = IMECPreObject::VTY_OBJECT;
        break;
    }
    return;
}

HC_DATA_DLL_API bool HCDI_UpdatePreObjectStringValue(IMECPreObject& pre_object, const PseudoDescriptor_t* descr_p, int attrib_ikey, const std::string& str_val, int ind)
{
    const IDescriptor* a_descr_p = (const IDescriptor*)descr_p;
    string skeyword = a_descr_p->getSKeyword(attrib_ikey);
    if (ind < 0)
    {
        pre_object.AddStringValue(skeyword.c_str(), str_val.c_str());
    }
    else
    {
        attribute_type_e a_atype = a_descr_p->getAttributeType(attrib_ikey);
        int a_cell_ind = -1;
        if (a_atype == ATYPE_VALUE)
        {
            pre_object.AddStringValue(skeyword.c_str(), ind, str_val.c_str());
        }
        else if (a_atype == ATYPE_DYNAMIC_ARRAY)
        {
            a_cell_ind = pre_object.GetIndex(IMECPreObject::ATY_ARRAY, IMECPreObject::VTY_STRING, skeyword);
            if (a_cell_ind >= 0)
                pre_object.SetStringValue(a_cell_ind, ind, str_val.c_str());
        }
    }
    return true;
}

HC_DATA_DLL_API void HCDI_GetPreObjectValueAsString(IMECPreObject& pre_object, int att_index, std::string& str_val, IMECPreObject::MyValueType_e& vtype, int index)
{
    if (att_index >= 0)
    {
        switch (vtype)
        {
        case IMECPreObject::VTY_OBJECT:
        {
            int a_id = 0;
            //const char* a_value_str = "";
            //a_value_str = pre_object.GetObjectName(att_index, index);
            str_val = pre_object.GetObjectName(att_index, index);
            if (str_val == "")
            {
                a_id = pre_object.GetObjectId(att_index, index);
                str_val = std::to_string(a_id);
            }
        }
        break;
        case IMECPreObject::VTY_INT:
        {
            int a_val = pre_object.GetIntValue(att_index, index);
            str_val = std::to_string(a_val);
        }
        break;
        case IMECPreObject::VTY_FLOAT:
        {
            double a_val = pre_object.GetFloatValue(att_index, index);
            str_val = std::to_string(a_val);
        }
        break;
        case IMECPreObject::VTY_UINT:
        {
            unsigned int a_val = pre_object.GetUIntValue(att_index, index);
            str_val = std::to_string(a_val);
        }
        break;
        case IMECPreObject::VTY_BOOL:
        {
            bool a_val = pre_object.GetBoolValue(att_index, index);
            if (a_val == true)
                str_val = "true";
            else
                str_val = "false";
        }
        break;
        case IMECPreObject::VTY_STRING:
        {
            const char* a_str_val = pre_object.GetStringValue(att_index, index);
            str_val = a_str_val;
        }
        break;
        default:
            break;
        }
    }
    return;
}

HC_DATA_DLL_API void HCDI_GetPreObjectValue(IMECPreObject& pre_object, int att_index, double& val, std::string& val_str, IMECPreObject::MyValueType_e& vtype, int index)
{
    if (att_index >= 0)
    {
        switch (vtype)
        {
        case IMECPreObject::VTY_OBJECT:
        {
            val_str = pre_object.GetObjectName(att_index, index);
            MYOBJ_INT a_id = pre_object.GetObjectId(att_index, index);
            val = a_id;
        }
        break;
        case IMECPreObject::VTY_INT:
        {
            int a_val = pre_object.GetIntValue(att_index, index);
            val = a_val;
        }
        break;
        case IMECPreObject::VTY_FLOAT:
        {
            double a_val = pre_object.GetFloatValue(att_index, index);
            val = a_val;
        }
        break;
        case IMECPreObject::VTY_UINT:
        {
            unsigned int a_val = pre_object.GetUIntValue(att_index, index);
            val = a_val;
        }
        break;
        case IMECPreObject::VTY_BOOL:
        {
            bool a_val = pre_object.GetBoolValue(att_index, index);
            if (a_val == true)
                val = 1;
            else
                val = 0;
        }
        break;
        default:
            break;
        }
    }
    return;
}
