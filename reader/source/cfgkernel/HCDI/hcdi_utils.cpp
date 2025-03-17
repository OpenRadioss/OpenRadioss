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

#include "hcdi_utils.h"
#include "hcdi_multicfgkernelmgr.h"
#include <cstring>

#include <assert.h>


HC_DATA_DLL_API IMECPreObject::MyAttributeType_e HCDIGetPATypeFromDAType(attribute_type_e atype)
{
    switch (atype)
    {
    case ATYPE_UNKNOWN:
        return IMECPreObject::ATY_UNKNOWN;
        /** Value */
    case ATYPE_VALUE:
    case ATYPE_SIZE:
        return IMECPreObject::ATY_SINGLE;
        /** Array */
    case ATYPE_STATIC_ARRAY:
    case ATYPE_DYNAMIC_ARRAY:
        return IMECPreObject::ATY_ARRAY;
    case ATYPE_LAST:
        return IMECPreObject::ATY_LAST;
    }
    return IMECPreObject::ATY_UNKNOWN;
}

HC_DATA_DLL_API IMECPreObject::MyValueType_e HCDIGetPVTypeFromDVType(value_type_e vtype)
{
    switch (vtype)
    {
    case VTYPE_UNKNOWN:
        return IMECPreObject::VTY_UNKNOWN;
    case VTYPE_BOOL:
        return IMECPreObject::VTY_BOOL;
    case VTYPE_INT:
        return IMECPreObject::VTY_INT;
    case  VTYPE_UINT:
        return IMECPreObject::VTY_UINT;
    case VTYPE_FLOAT:
        return IMECPreObject::VTY_FLOAT;
    case VTYPE_STRING:
        return IMECPreObject::VTY_STRING;
    case VTYPE_OBJECT:
        return IMECPreObject::VTY_OBJECT;
    case VTYPE_LAST:
        return IMECPreObject::VTY_LAST;
    }
    return IMECPreObject::VTY_UNKNOWN;
}

HC_DATA_DLL_API int HCDIGetIDPoolForGvnDatanameFromPreobject(const IDescriptor *descr_p, const IMECPreObject *preobj, const string &dataname, int &hm_type)
{
    if (descr_p == NULL || preobj == NULL || dataname == "")
        return 0;

    int ikeyword = descr_p->getIKeyword(dataname);
    int nb_subtype = 0;
    obj_type_e att_hc_ent_type = HCDI_OBJ_TYPE_NULL;
    string sub_str = "";
    if (END_ARGS != ikeyword && descr_p->hasObjectAttribSubtype(ikeyword, &nb_subtype))
    {
        if (nb_subtype > 1)
        {
            string att_stype = dataname + "_type";
            int ikw = descr_p->getIKeyword(att_stype);
            if (ikw == END_ARGS)
            {
                return 0;
            }
            string att_obj_type = "";
            if (preobj->GetStringValue(att_stype.c_str()))
            {
                att_obj_type = preobj->GetStringValue(att_stype.c_str());
            }
            else
            {
                att_obj_type = descr_p->getStringDefaultValue(ikw, HCDI_get_all_domains());
            }

            vector<string> key_vect;
            HCDI_StringTokenize(att_obj_type, key_vect, "/");
            int size = (int)key_vect.size();
            if (!key_vect.empty())
            {
                string type_str = key_vect[0];
                att_hc_ent_type = HCDI_get_entitytype(type_str);
                if (size == 2)
                {
                    sub_str = key_vect[1];
                }
                else
                {
                    sub_str = type_str;
                }
            }
            if (att_hc_ent_type <= 0)
                return 0;
        }
        else
        {
            att_hc_ent_type = descr_p->getObjectType(ikeyword);
            if (att_hc_ent_type <= 0)
                return 0;

            MvFullTypeSet_t a_fulltypeset;
            //descr_p->getMultiObjectTypes(ikeyword, a_fulltypeset);
            HCDIGetMultiObjectTypes(descr_p, ikeyword, a_fulltypeset);

            if ((int)a_fulltypeset.size() != 0)
            {
                MvFullTypeSet_t::iterator iter_b = a_fulltypeset.begin();
                MvFullType_t ftype = *iter_b;
                att_hc_ent_type = ftype.getType();
                sub_str = ftype.getSubtypeStr();
            }
        }

        if (att_hc_ent_type > 0)
            hm_type = att_hc_ent_type;
        else
            return 0;

        if (sub_str != "")
        {
            int idpool_number = 0;
            HCDIgetIdPool(att_hc_ent_type, -1, -1, sub_str, &idpool_number);
            return idpool_number;
        }
    }

    return 0;
}

HC_DATA_DLL_API int HCDIGetIDPoolForGvnDataname(const IDescriptor *descr_p, const string &dataname, const string &dataname_type_val)
{
    if (descr_p == NULL || dataname == "")
        return 0;
    int a_att_hm_type = 0;
    int ikeyword = descr_p->getIKeyword(dataname);
    int nb_subtype = 0;
    obj_type_e att_hc_ent_type = HCDI_OBJ_TYPE_NULL;
    string sub_str = "";
    if (descr_p->hasObjectAttribSubtype(ikeyword, &nb_subtype))
    {
        if (nb_subtype > 1)
        {
            string att_stype = dataname + "_type";
            int ikw = descr_p->getIKeyword(att_stype);
            if (ikw == END_ARGS)
            {
                return 0;
            }

            vector<string> key_vect;
            HCDI_StringTokenize(dataname_type_val, key_vect, "/");
            int size = (int)key_vect.size();
            if (!key_vect.empty())
            {
                string type_str = key_vect[0];
                att_hc_ent_type = HCDI_get_entitytype(type_str);
                if (size == 2)
                {
                    sub_str = key_vect[1];
                }
                else
                {
                    sub_str = type_str;
                }
            }
            if (att_hc_ent_type <= 0)
                return 0;
        }
        else
        {
            att_hc_ent_type = descr_p->getObjectType(ikeyword);
            if (att_hc_ent_type <= 0)
                return 0;

            MvFullTypeSet_t a_fulltypeset;
            //descr_p->getMultiObjectTypes(ikeyword, a_fulltypeset);
            HCDIGetMultiObjectTypes(descr_p, ikeyword, a_fulltypeset);

            if ((int)a_fulltypeset.size() != 0)
            {
                MvFullTypeSet_t::iterator iter_b = a_fulltypeset.begin();
                MvFullType_t ftype = *iter_b;
                sub_str = ftype.getSubtypeStr();
            }
        }
    }

    a_att_hm_type = att_hc_ent_type;
    if (a_att_hm_type < 0)
        return 0;
    if (sub_str != "")
    {
        int idpool_number = 0;
        HCDIgetIdPool(att_hc_ent_type, -1, -1, sub_str, &idpool_number);
        return idpool_number;
    }
    return 0;
}

//HC_DATA_DLL_API void HCDI_set_default_data_hierarchy_flag(const string& bit_string)
//{
//    MultiCFGKernelMgr& kernel_mgr = MultiCFGKernelMgr::getInstance();
//    const CFGKernel* a_cfgkernel = kernel_mgr.GetCurrentCFGKernel();
//    if (!a_cfgkernel)
//        return;
//
//    a_cfgkernel->get_data_hierarchy_bitmask(bit_string);
//}

HC_DATA_DLL_API int HCDI_get_data_hierarchy_bitmask(const string& bit_string)
{
    MultiCFGKernelMgr& kernel_mgr = MultiCFGKernelMgr::getInstance();
    const CFGKernel* a_cfgkernel = kernel_mgr.GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return 0;

    return a_cfgkernel->get_data_hierarchy_bitmask(bit_string);
}

HC_DATA_DLL_API int HCDI_get_default_bit()
{
    MultiCFGKernelMgr& kernel_mgr = MultiCFGKernelMgr::getInstance();
    const CFGKernel* a_cfgkernel = kernel_mgr.GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return 0;
    return a_cfgkernel->get_default_bit();
}
HC_DATA_DLL_API void HCDIGetMultiObjectTypes(const IDescriptor *descrp, int ikeyword, MvFullTypeSet_t& set)
{
    if (!descrp || ikeyword <= 0)
        return;
    const descriptor_t* cdescr_p = descrp->getDescriptorPtr();
    if (cdescr_p == NULL)
        return;
    object_descriptor_t* objdescr_p = (object_descriptor_t*)(cdescr_p->attdescr_array[ikeyword]);
    if (objdescr_p == NULL)
        return;
    MultiCFGKernelMgr& descrp_model = MultiCFGKernelMgr::getInstance();
    const CFGKernel* a_cfgkernel = descrp_model.GetCurrentCFGKernel();
    if (!a_cfgkernel)
        return;
    for (int i = 0; i < objdescr_p->num; i++)
    {
        if (objdescr_p->subtypes[i])
        {
            const MvSubtype_t* subtype = a_cfgkernel->get_subtype(objdescr_p->allowed_types[i], objdescr_p->subtypes[i]);
            if (subtype == NULL)
            {
                vector<string> key_vect;
                string string_val = objdescr_p->subtypes[i];
                a_cfgkernel->getChildKeywordListForGivenKeyword(objdescr_p->allowed_types[i], string_val, key_vect);
                vector<string>::iterator iter_b = key_vect.begin();
                vector<string>::iterator iter_e = key_vect.end();
                vector<string>::iterator iter;
                for (iter = iter_b; iter != iter_e; ++iter)
                {
                    string keyw = *iter;
                    MvFullType_t ftype(*a_cfgkernel, objdescr_p->allowed_types[i], keyw);
                    set.insert(ftype);
                }
            }
            else
            {
                // insert subtype if found
                MvFullType_t ftype(*a_cfgkernel, objdescr_p->allowed_types[i], objdescr_p->subtypes[i]);
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

HC_DATA_DLL_API int HCDI_splitString(const string& target, const string& delimiter, vector <string >& result)
{
    return splitString(target, delimiter, result);
}


string GetConfigTestDescrString(int index)
{
    static string ConfigTestDescr_array[] = { "UNKNOWN", "LOADING OF ALL CFG KERNEL ", "DETAILS LOADING TIME", "SHOWING MEMORY USAGE", "CONFLICTING DATANAMES CHECK", "EXPORTING DATAHIERARCHY",
                                               "UNIQUE USERIDs GIVEN", "UNIQUE USERNAMES GIVEN"};

    // assert(index > ConfigTestDescr_array.size());

    return ConfigTestDescr_array[index];
}
bool HCDIIsAttributeTripleCFG(const IDescriptor& descrp, int ikeyword, bool &is_multi)
{
    if (ikeyword <= 0)
        return false;

    is_multi = false;
    bool a_multi = descrp.isMultiDimensionalArray(ikeyword);
    int a_size = 0;
    if (a_multi)
    {
        MvSizeVector sizeArrayVector;
        descrp.getDimensionSize(ikeyword, sizeArrayVector);
        size_t array_dimension = sizeArrayVector.size();
        if (array_dimension == 0 || array_dimension > 2)
            return false;

        is_multi = true;
        int size = 0;
        bool is_real_size = sizeArrayVector[1].isRealSize;
        if (is_real_size)
           a_size = sizeArrayVector[1].size;
    }
    else if (descrp.getAttributeType(ikeyword) == ATYPE_STATIC_ARRAY)
    {
        a_size = descrp.getSize(ikeyword);
    }
    if (a_size != 3)
        return false;

    const MvDataFeature_t* a_data_fea = descrp.getIkeywordDataFeature(DOM_COMMON, ikeyword);
    if (a_data_fea)
    {
        MvDataFeatureType_e  feat_type = a_data_fea->getType();
        if (feat_type == DFT_TRIPLE)
        {
            return true;
        }
    }
    return false;
}


void CFGLinkSubObjectsToParent(std::vector<IMECPreObject*>           parentlst[],   // Array of parent lists
                               const std::unordered_map<std::string, cfglnksubdescriptor>& descriptor_map,
                               const std::unordered_map<std::string, std::vector<IMECPreObject*>>& sorted_child_ids,
                               const std::unordered_map<std::string, std::vector<IMECPreObject*>>& sorted_child_names)
{

    for (const auto& entry : descriptor_map) {
        const std::string& child_ktype = entry.first;
        const cfglnksubdescriptor& descriptor = entry.second;

        if (descriptor.link_by == CFG_LNK_CHILD_BY_ID && sorted_child_ids.count(child_ktype)) {
            // Get the sorted list of child IDs for this ktype
            const auto& child_objs = sorted_child_ids.at(child_ktype);

            for (IMECPreObject* parent : parentlst[descriptor.parent_type]) {
                auto it = std::lower_bound(child_objs.begin(), child_objs.end(), parent->GetId(),
                    [](IMECPreObject* child, unsigned int id) { return child->GetId() < id; });
                if (it != child_objs.end() && (*it)->GetId() == parent->GetId()) {
                    parent->SetSubobject((*it));  // Link child to parent

                    IDescriptor* descrp = HCDI_GetDescriptorHandle(parent->GetKernelFullType());
                    if (descrp)
                    {
                        const MvSubobjectsCardInfoList_t& cardInfoList = descrp->GetSubobjectsCardInfoList();
                        for (MvSubobjectsCardInfoList_t::const_iterator it = cardInfoList.begin(); it != cardInfoList.end(); ++it)
                        {
                            const MvSubobjectsCardInfo_t& cardInfo = *it;
                            if (child_ktype != cardInfo.full_type)
                                continue;
                            string skey = descrp->getSKeyword(cardInfo.ikeyword);
                            obj_type_e ctype = descrp->getObjectType(cardInfo.ikeyword);

                            parent->AddObjectValue(skey.c_str(), HCDI_get_entitystringtype(ctype).c_str(), parent->GetId());

                            MvExpressionList_t exp_lst = cardInfo.conditions;
                            vector< const expression_t* >  ep_lst;
                            for (MvExpressionList_t::const_iterator expIt = exp_lst.begin(); expIt != exp_lst.end(); ++expIt)
                            {
                                const MvExpression_t* expr_p = &(*expIt);
                                const expression_t* ep = expr_p->getExpressionPtr();
                                //ep_lst.push_back(ep);

                                if (ep->my_type == EXPRT_ATTRIBUTE)
                                {
                                    attribute_expression_t* a_exp = (attribute_expression_t*)ep;
                                    comparator_e cmp = a_exp->my_comparator;
                                    if (cmp == CMPT_NE)
                                    {
                                        value_type_e val = descrp->getValueType(a_exp->my_ikeyword);
                                        if (val == VTYPE_INT)
                                        {
                                            int a_val = a_exp->my_rvalue.my_int_value;
                                            if (!a_val)
                                                a_val = 1;
                                            else
                                                a_val = 0;

                                            //int a_attribid_ind = p_node->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, descrp->getSKeyword(a_exp->my_ikeyword));
                                            parent->AddIntValue(descrp->getSKeyword(a_exp->my_ikeyword).c_str(), a_val);
                                        }
                                        else if (val == VTYPE_BOOL)
                                        {
                                            if (a_exp->my_rvalue_type == RVAT_INT)
                                            {
                                                int a_val = a_exp->my_rvalue.my_int_value;
                                                bool a_b_val = false;
                                                if (!a_val)
                                                    a_b_val = true;

                                                parent->AddBoolValue(descrp->getSKeyword(a_exp->my_ikeyword).c_str(), a_b_val);
                                            }
                                        }
                                    }
                                    else if (cmp == CMPT_EQ)
                                    {
                                        value_type_e val = descrp->getValueType(a_exp->my_ikeyword);
                                        if (val == VTYPE_INT)
                                        {
                                            int a_val = a_exp->my_rvalue.my_int_value;
                                            //int a_attribid_ind = p_node->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_INT, descrp->getSKeyword(a_exp->my_ikeyword));
                                            parent->AddIntValue(descrp->getSKeyword(a_exp->my_ikeyword).c_str(), a_val);
                                        }
                                        else if (val == VTYPE_BOOL)
                                        {
                                            if (a_exp->my_rvalue_type == RVAT_INT)
                                            {
                                                int a_val = a_exp->my_rvalue.my_int_value;
                                                bool a_b_val = false;
                                                if (a_val)
                                                    a_b_val = true;

                                                parent->AddBoolValue(descrp->getSKeyword(a_exp->my_ikeyword).c_str(), a_b_val);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        else if (descriptor.link_by == CFG_LNK_CHILD_BY_NAME && sorted_child_names.count(child_ktype)) {
            // Get the sorted list of child names for this ktype
            const auto& child_objs = sorted_child_names.at(child_ktype);

            for (IMECPreObject* parent : parentlst[descriptor.parent_type]) {
                auto it = std::lower_bound(child_objs.begin(), child_objs.end(), parent->GetTitle(),
                    [](IMECPreObject* child, const std::string& name) { return child->GetTitle() < name; });
                if (it != child_objs.end() && (*it)->GetTitle() == parent->GetTitle()) {
                    parent->SetSubobject((*it));// Link child to parent
                }
            }
        }
        else if (descriptor.link_by == CFG_LNK_CHILD_BY_ATTRIBS || !strcmp(descriptor.clnkatt, "_ID_") && sorted_child_ids.count(child_ktype)) {
            // Get the sorted list of child IDs for this ktype
            const auto& child_objs = sorted_child_ids.at(child_ktype);

            // Loop over parent objects of the given type
            for (IMECPreObject* parent : parentlst[descriptor.parent_type]) {

                IDescriptor* descrp = HCDI_GetDescriptorHandle(parent->GetKernelFullType());
                if (!descrp)
                    continue;
                int ikey_parent = descrp->getIKeyword(descriptor.plnkatt); //source
                if (ikey_parent <= 0)
                    continue;

                value_type_e valtype = descrp->getValueType(ikey_parent);

                if (valtype != VTYPE_OBJECT)
                    continue;

                int a_attribid_ind = parent->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_OBJECT, descriptor.plnkatt);

                if (a_attribid_ind < 0)
                    continue;

                unsigned int a_p_att_id = parent->GetObjectId(a_attribid_ind);

                auto it = std::lower_bound(child_objs.begin(), child_objs.end(), a_p_att_id,
                    [](IMECPreObject* child, unsigned int id) { return child->GetId() < id; });
                if (it != child_objs.end() && (*it)->GetId() == a_p_att_id) {
                    parent->SetSubobject((*it));  // Link child to parent
                }
            }
        }
        else if (descriptor.link_by == CFG_LNK_CHILD_BY_ATTRIBS || !strcmp(descriptor.clnkatt, "name") && sorted_child_names.count(child_ktype)) {
            // Get the sorted list of child IDs for this ktype
            const auto& child_objs = sorted_child_names.at(child_ktype);

            // Loop over parent objects of the given type
            for (IMECPreObject* parent : parentlst[descriptor.parent_type]) {

                IDescriptor* descrp = HCDI_GetDescriptorHandle(parent->GetKernelFullType());
                if (!descrp)
                    continue;
                int ikey_parent = descrp->getIKeyword(descriptor.plnkatt); //source
                if (ikey_parent <= 0)
                    continue;

                value_type_e valtype = descrp->getValueType(ikey_parent);

                if (valtype != VTYPE_OBJECT)
                    continue;

                int a_attribid_ind = parent->GetIndex(IMECPreObject::ATY_SINGLE, IMECPreObject::VTY_OBJECT, descriptor.plnkatt);

                if (a_attribid_ind < 0)
                    continue;

                const char* obj_name = parent->GetObjectName(a_attribid_ind);

                auto it = std::lower_bound(child_objs.begin(), child_objs.end(), obj_name,
                    [](IMECPreObject* child, const std::string& name) { return child->GetTitle() < name; });
                if (it != child_objs.end() && (*it)->GetTitle() == obj_name) {
                    parent->SetSubobject((*it));  // Link child to parent
                }
            }
        }
    }
}

static void CFGPreprocessSubobjects(vector<IMECPreObject*>* p_preobjlst,
    const std::unordered_map<std::string, cfglnksubdescriptor>& descriptor_map,
    std::unordered_map<std::string, std::vector<IMECPreObject*>>& sorted_child_ids,
    std::unordered_map<std::string, std::vector<IMECPreObject*>>& sorted_child_names)
{
    for (const auto& entry : descriptor_map) {
        const std::string& child_ktype = entry.first;
        const cfglnksubdescriptor& descriptor = entry.second;

        obj_type_e etype = HCDI_OBJ_TYPE_NULL;
        if (child_ktype[0] != '/') {
            etype = MV_get_type(child_ktype);
        }
        else {
            string a_str = "";
            int i = 1, n = (int)(child_ktype.size());
            while (i < n && child_ktype[i] != '/') a_str += child_ktype[i++];
            etype = MV_get_type(a_str);
        }
        if (etype <= HCDI_OBJ_TYPE_NULL || etype >= HCDI_OBJ_TYPE_HC_MAX)
            continue;

        const std::vector<IMECPreObject*>& childlst = p_preobjlst[etype];


        if (descriptor.link_by == CFG_LNK_CHILD_BY_ID || !strcmp(descriptor.clnkatt, "_ID_")) {
            // Process only children with the specified ktype and store their IDs
            for (IMECPreObject* child : childlst) {
                if (child && child->GetKernelFullType() == child_ktype) {
                    sorted_child_ids[child_ktype].push_back(child);
                }
            }
            // Sort IDs for this child ktype
            std::sort(sorted_child_ids[child_ktype].begin(), sorted_child_ids[child_ktype].end(),
                [](IMECPreObject* a, IMECPreObject* b) { return a->GetId() < b->GetId(); });
        }
        else if (descriptor.link_by == CFG_LNK_CHILD_BY_NAME || !strcmp(descriptor.clnkatt, "_ID_")) {
            // Process only children with the specified ktype and store their names
            for (IMECPreObject* child : childlst) {
                if (child && child->GetKernelFullType() == child_ktype) {
                    sorted_child_names[child_ktype].push_back(child);
                }
            }
            // Sort names for this child ktype
            std::sort(sorted_child_names[child_ktype].begin(), sorted_child_names[child_ktype].end(),
                [](IMECPreObject* a, IMECPreObject* b) { return a->GetTitle() < b->GetTitle(); });
        }
    }
}

void CFGResolveEntitiesSubObjectReferences(std::map<string, CUserNameTypeInfo>& username_info, vector<IMECPreObject*>* p_preobjlst)
{
    std::unordered_map<std::string, cfglnksubdescriptor>   cfglnksubdescriptor_map;
    vector<IMECPreObject*>& subobject_lst = p_preobjlst[HCDI_OBJ_TYPE_SUBOBJECT];

    for (auto& username_itr : username_info)
    {
        CUserNameTypeInfo& typeinfo = username_itr.second;
        const IDescriptor* descrp = typeinfo.pdescrp;
        if (descrp)
        {
            const MvSubobjectsCardInfoList_t& cardInfoList = descrp->GetSubobjectsCardInfoList();
            for (MvSubobjectsCardInfoList_t::const_iterator it = cardInfoList.begin(); it != cardInfoList.end(); ++it)
            {
                const MvSubobjectsCardInfo_t& cardInfo = *it;
                int obj_type = (int)descrp->getHmType();
                if (cardInfo.plnkatt[0] != '\0' && typeinfo.obj_type > 0)
                {
                    cfglnkchildby_e lnkby = CFG_LNK_CHILD_BY_ATTRIBS;
                    if (!strcmp(cardInfo.plnkatt, "_ID_") && !strcmp(cardInfo.clnkatt, "_ID_"))
                        lnkby = CFG_LNK_CHILD_BY_ID;
                    else if (!strcmp(cardInfo.plnkatt, "name") && !strcmp(cardInfo.clnkatt, "name"))
                        lnkby = CFG_LNK_CHILD_BY_NAME;

                    cfglnksubdescriptor_map[string(cardInfo.full_type)] = cfglnksubdescriptor{ string(cardInfo.full_type), typeinfo.obj_type, lnkby, cardInfo.plnkatt, cardInfo.clnkatt };
                }
            }
        }
    }

    // Preprocess children
    std::unordered_map<std::string, std::vector<IMECPreObject*>> sorted_child_ids;
    std::unordered_map<std::string, std::vector<IMECPreObject*>> sorted_child_names;
    CFGPreprocessSubobjects(p_preobjlst, cfglnksubdescriptor_map, sorted_child_ids, sorted_child_names);

    // Link parent and child objects
    CFGLinkSubObjectsToParent(p_preobjlst, cfglnksubdescriptor_map, sorted_child_ids, sorted_child_names);
}
