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
#pragma once
//#include <hwSDIEntity.h>
#include "hcdi_mv_descriptor.h"
#include <KERNEL/mv_data_feature.h>
#include <KERNEL/mv_data_data_feature.h>
#include <KERNEL/mv_data_cond_function_feature.h>
#include <KERNEL/mv_data_uncond_function_feature.h>
#include <KERNEL/mv_data_cond_scalar_feature.h>
#include <KERNEL/mv_data_uncond_scalar_feature.h>
#include <KERNEL/mv_data_triple_feature.h>
#include <KERNEL/mv_data_array_feature.h>
#include <KERNEL/mv_data_if_feature.h>
#include <KERNEL/mv_data_assign_feature.h>



enum IdenKeyType{
    DIMEN_IDEN_BY_SKEY,
    DIMEN_IDEN_BY_IKEY,
    DIMEN_IDEN_BY_SKEY_IDEN
};



template <class ENTITYREAD, class IDENTIFIER, class VALUE, class VALUEENTITY, class VALUESTRING>
static bool HCEvaluateExpression(
    const IDescriptor* descr_p, const expression_t* expr_p, int ind, IdenKeyType keytype, IMECPreObject* preobject = NULL);

template <class ENTITYREAD, class IDENTIFIER, class VALUE, class VALUEENTITY, class VALUESTRING>
static bool HCDI_GetDimensionFromFeature(
    const IDescriptor* pDescr, const MvDataFeature_t* pFeature, vector<pair<MvDimension_e, vector<string>>>& dim_lst, int ikeyword, int index, IdenKeyType keytype);

static bool HCDI_GetDimension(const IDescriptor* pDescr, map<const IDescriptor*, map<const string, vector<pair<MvDimension_e, vector<string>>>>> &desc_dim_map, IdenKeyType keytype);

static bool hcdi_eval_int(int lvalue, comparator_e op, int rvalue) {
    switch (op) {
    case CMPT_GT: return lvalue > rvalue;  break;
    case CMPT_GE: return lvalue >= rvalue; break;
    case CMPT_LT: return lvalue < rvalue;  break;
    case CMPT_LE: return lvalue <= rvalue; break;
    case CMPT_EQ: return lvalue == rvalue; break;
    case CMPT_NE: return lvalue != rvalue; break;
    default:      break;
    }
    return false;
}

static bool hcdi_eval_float(double lvalue, comparator_e op, double rvalue) {
    switch (op) {
    case CMPT_GT: return lvalue > rvalue;  break;
    case CMPT_GE: return lvalue >= rvalue; break;
    case CMPT_LT: return lvalue < rvalue;  break;
    case CMPT_LE: return lvalue <= rvalue; break;
    case CMPT_EQ: return lvalue == rvalue; break;
    case CMPT_NE: return lvalue != rvalue; break;
    default:      break;
    }
    return false;
}

static bool hcdi_eval_string(const char* lvalue, comparator_e op, const char* rvalue) {
    switch (op) {
    case CMPT_GT: return strcmp(lvalue, rvalue) > 0;  break;
    case CMPT_GE: return strcmp(lvalue, rvalue) >= 0; break;
    case CMPT_LT: return strcmp(lvalue, rvalue) < 0;  break;
    case CMPT_LE: return strcmp(lvalue, rvalue) <= 0; break;
    case CMPT_EQ: return strcmp(lvalue, rvalue) == 0; break;
    case CMPT_NE: return strcmp(lvalue, rvalue) != 0; break;
    default:      break;
    }
    return false;
}

static bool hcdi_eval_object(const int lvalue, comparator_e op, int rvalue) {
    switch (op) {
    case CMPT_EQ: return lvalue == rvalue; break;
    case CMPT_NE: return lvalue != rvalue; break;
    default:      break;
    }
    return false;
}


template <class ENTITYREAD, class IDENTIFIER, class VALUE, class VALUEENTITY, class VALUESTRING>
bool HCLocEvaluateAttributeExpression(
    const ENTITYREAD* entity,
    const IDescriptor* descr_p,
    const expression_t* expr_p,
    int                      ind,
    IdenKeyType                     keytype,
    IMECPreObject *preobject=NULL)
{
    int                     a_is_indexed = 0;   
    if (!descr_p) return false;

    attribute_expression_t* a_expr_p = (attribute_expression_t*)expr_p;

    IDENTIFIER a_iden;
    int att_id = 0;
    if (keytype == DIMEN_IDEN_BY_SKEY)
        a_iden.SetNameKey(descr_p->getSKeyword(a_expr_p->my_ikeyword));
    else if (keytype == DIMEN_IDEN_BY_SKEY_IDEN)
    {
        att_id = descr_p->getIdentifierValue(DOM_COMMON, a_expr_p->my_ikeyword);
        // for evaluating based on preobject data, a temp variable
        if (att_id < 0 && preobject)
        {
            return preobject->EvaluateMCDSExpression(a_expr_p, descr_p, ind);
        }
        else
        {
            if (att_id > 0)
                a_iden.SetNumericKey(att_id);
            else
                a_iden.SetNameKey(descr_p->getSKeyword(a_expr_p->my_ikeyword));
        }
    }
    else
        a_iden.SetNumericKey(a_expr_p->my_ikeyword);
    if (0 <= ind) a_iden.SetRow(ind);

    value_type_e dtype = descr_p->getValueType(a_expr_p->my_ikeyword);
    bool hasValue = false;
    if (!entity)
        return hasValue;
    VALUE value;
    hasValue = entity->GetValue(a_iden, value);

    if (hasValue) a_is_indexed = value.GetArrayDimensionExtent();
    if ((value == NULL) || (value.GetDisplayStatus() == DISPLAY_STATUS_OFF))
    {
        bool check_default = true;
        if (value != NULL)
        {
            switch (dtype)
            {
                case VTYPE_INT:
                {
                    int int_value = 0;
                    if (a_is_indexed)
                        value.GetValue(int_value, ind);
                    else
                        value.GetValue(int_value);

                    if (int_value != 0)
                        check_default = false;
                    break;
                }
                case VTYPE_UINT:
                {
                    unsigned int uint_value = 0;
                    if (value.GetCompoundType() == COMPOUND_TYPE_ENTITY)
                    {
                        VALUEENTITY entity;
                        if (a_is_indexed)
                        {
                            value.GetValue(entity, ind);
                            uint_value = entity.GetId();
                        }
                        else
                        {
                            value.GetValue(entity);
                            uint_value = entity.GetId();
                        }
                    }
                    else
                    {
                        if (a_is_indexed)
                            value.GetValue(uint_value, ind);
                        else
                            value.GetValue(uint_value);
                    }

                    if (uint_value != 0)
                        check_default = false;
                    break;
                }
                case VTYPE_FLOAT:
                {
                    double d_value = 0;
                    if (a_is_indexed)
                        value.GetValue(d_value, ind);
                    else
                        value.GetValue(d_value);

                    if (d_value != 0.)
                        check_default = false;
                    break;
                }
                case VTYPE_STRING:
                {
                    VALUESTRING str_value = "";
                    if (a_is_indexed)
                        value.GetValue(str_value, ind);
                    else
                        value.GetValue(str_value);

                    if (str_value != "")
                        check_default = false;
                    break;
                }
                case VTYPE_BOOL:
                {
                    bool b_value = false;
                    if (a_is_indexed)
                        value.GetValue(b_value, ind);
                    else
                        value.GetValue(b_value);

                    if (b_value != false)
                        check_default = false;
                    break;
                }
                default:
                    break;
            }
        }

        if (check_default)
        {
            int a_domains = HCDI_get_all_domains();
            const MvDataFeature_t* feature_p = descr_p->getIkeywordDataFeature(a_domains, a_expr_p->my_ikeyword);
            if (feature_p)
            {
                MvDataFeatureType_e feature_type = feature_p->getType();
                if (feature_type == DFT_FLAG)
                {
                    bool has_default = false;
                    switch (dtype)
                    {
                        case VTYPE_INT:
                        {
                            int a_lvalue = 0, a_rvalue = 0;
                            a_lvalue = descr_p->getIntDefaultValue(a_expr_p->my_ikeyword, a_domains, &has_default);
                            if (has_default == false)
                            {
                                a_lvalue = 0;
                            }

                            a_rvalue = a_expr_p->my_rvalue.my_int_value;
                            return hcdi_eval_int(a_lvalue, a_expr_p->my_comparator, a_rvalue);
                        }
                        break;
                        case VTYPE_BOOL:
                        {
                            int a_lvalue = 0, a_rvalue = 0;
                            a_lvalue = (int)descr_p->getBoolDefaultValue(a_expr_p->my_ikeyword, a_domains, &has_default);
                            if (has_default == false)
                            {
                                a_lvalue = 0;
                            }

                            a_rvalue = a_expr_p->my_rvalue.my_int_value;
                            return hcdi_eval_int(a_lvalue, a_expr_p->my_comparator, a_rvalue);
                        }
                        break;
                        default:
                            break;
                    }
                }
                else if (feature_type == DFT_RADIO)
                {
                    bool has_default = false;
                    switch (dtype)
                        {
                        case VTYPE_INT:
                        {
                            int a_lvalue = 0, a_rvalue = 0;
                            a_lvalue = descr_p->getIntDefaultValue(a_expr_p->my_ikeyword, a_domains, &has_default);
                            if (has_default == false)
                            {
                                a_lvalue = 0;
                            }

                            a_rvalue = a_expr_p->my_rvalue.my_int_value;
                            return hcdi_eval_int(a_lvalue, a_expr_p->my_comparator, a_rvalue);
                        }
                        break;
                        case VTYPE_UINT:
                        {
                            unsigned int a_lvalue = 0, a_rvalue = 0;
                            a_lvalue = descr_p->getUIntDefaultValue(a_expr_p->my_ikeyword, a_domains, &has_default);
                            if (has_default == false)
                            {
                                a_lvalue = 0;
                            }

                            a_rvalue = a_expr_p->my_rvalue.my_uint_value;
                            return hcdi_eval_int(a_lvalue, a_expr_p->my_comparator, a_rvalue);
                        }
                        break;
                        case VTYPE_FLOAT:
                        {
                            double a_lvalue = 0., a_rvalue = 0.;
                            a_lvalue = descr_p->getFloatDefaultValue(a_expr_p->my_ikeyword, a_domains, &has_default);
                            if (has_default == false)
                            {
                                a_lvalue = 0.;
                            }
                            a_rvalue = a_expr_p->my_rvalue.my_float_value;
                            return hcdi_eval_float(a_lvalue, a_expr_p->my_comparator, a_rvalue);
                        }
                        break;
                        case VTYPE_STRING:
                        {
                            const char* a_rvalue = NULL;
                            string a_lvalue = descr_p->getStringDefaultValue(a_expr_p->my_ikeyword, a_domains, &has_default);
                            if (has_default == false)
                            {
                                a_lvalue = "";
                            }
                            a_rvalue = a_expr_p->my_rvalue.my_string_value;
                            return hcdi_eval_string(a_lvalue.c_str(), a_expr_p->my_comparator, a_rvalue);
                        }
                        break;
                        default:
                            break;
                    }
                }
            }
        }
    }

    if (value == NULL)
    {
      return false;
    }

    if (a_is_indexed && ind < 0) return false;

    switch (dtype)
    {
    case VTYPE_BOOL:
    {
        bool a_lvalue = false;
        int a_rvalue = 0;
        if (a_is_indexed)
        {
            value.GetValue(a_lvalue, ind);
        }
        else
        {
            value.GetValue(a_lvalue);
        }

        a_rvalue = a_expr_p->my_rvalue.my_int_value;
        return hcdi_eval_int(a_lvalue, a_expr_p->my_comparator, a_rvalue);
    }
    break;
    case VTYPE_INT:
    {
        int a_lvalue = 0, a_rvalue = 0;
        if (a_is_indexed)
        {
            value.GetValue(a_lvalue, ind);
        }
        else
        {
            value.GetValue(a_lvalue);
        }

        a_rvalue = a_expr_p->my_rvalue.my_int_value;
        return hcdi_eval_int(a_lvalue, a_expr_p->my_comparator, a_rvalue);
    }
    break;
    case VTYPE_UINT:
    {
        unsigned int a_lvalue = 0, a_rvalue = 0;
        if (value.GetCompoundType() == COMPOUND_TYPE_ENTITY)
        {
            VALUEENTITY entity;
            if (a_is_indexed)
            {
                value.GetValue(entity, ind);
                a_lvalue = entity.GetId();
            }
            else
            {
                value.GetValue(entity);
                a_lvalue = entity.GetId();
            }
        }
        else
        {
            if (a_is_indexed)
            {
                value.GetValue(a_lvalue, ind);
            }
            else
            {
                value.GetValue(a_lvalue);
            }
        }
        a_rvalue = a_expr_p->my_rvalue.my_int_value;
        return hcdi_eval_int(a_lvalue, a_expr_p->my_comparator, a_rvalue);
    }
    break;
    /*
    case BASIC_TYPE_FLOAT:
    {
        float a_lvalue=0.,a_rvalue=0.;
        if(a_is_indexed)
        {
            value.GetValue(a_lvalue, ind);
        }
        else
        {
            value.GetValue(a_lvalue);
        }

        a_rvalue=(float) a_expr_p->my_rvalue.my_float_value;
        return hcdi_eval_float(a_lvalue, a_expr_p->my_comparator, a_rvalue);
    }
    break;
    */
    case VTYPE_FLOAT:
    {
        double a_lvalue = 0., a_rvalue = 0.;
        if (a_is_indexed)
        {
            value.GetValue(a_lvalue, ind);
        }
        else
        {
            value.GetValue(a_lvalue);
        }

        a_rvalue = a_expr_p->my_rvalue.my_float_value;
        return hcdi_eval_float(a_lvalue, a_expr_p->my_comparator, a_rvalue);
    }
    break;
    case VTYPE_STRING:
    {
        const char* a_rvalue = NULL;
        VALUESTRING a_lvalue;
        if (a_is_indexed)
        {
            value.GetValue(a_lvalue, ind);
        }
        else
        {
            value.GetValue(a_lvalue);
        }
        a_rvalue = a_expr_p->my_rvalue.my_string_value;
        return hcdi_eval_string(a_lvalue.c_str(), a_expr_p->my_comparator, a_rvalue);
    }
    break;
    case VTYPE_OBJECT:
    {
        int a_lvalue = 0, a_rvalue = 0;
        VALUEENTITY entity;
        if (a_is_indexed)
        {
            value.GetValue(entity, ind);
            a_lvalue = (int)entity.GetId();
        }
        else
        {
            value.GetValue(entity);
            a_lvalue = (int)entity.GetId();
        }
        a_rvalue = a_expr_p->my_rvalue.my_int_value;
        return hcdi_eval_object(a_lvalue, a_expr_p->my_comparator, a_rvalue);
    }
    break;
    default:
        break;
    }

    return false;
}

template <class ENTITYREAD, class IDENTIFIER, class VALUE, class VALUEENTITY, class VALUESTRING>
static bool HCLocEvaluateLogicalExpression(
    const ENTITYREAD* entity,
    const IDescriptor* descr_p,
    const expression_t* expr_p,
    int                      ind,
    IdenKeyType                     keytype,
    IMECPreObject *preobject=NULL)
{
    logical_expression_t* a_expr_p = (logical_expression_t*)expr_p;
    bool result1 = HCEvaluateExpression<ENTITYREAD, IDENTIFIER, VALUE, VALUEENTITY, VALUESTRING>
            (entity, descr_p, a_expr_p->my_first_expr_p, ind, keytype, preobject);

    switch (a_expr_p->my_operator) {
    case LGOP_AND:
        return result1 && HCEvaluateExpression<ENTITYREAD, IDENTIFIER, VALUE, VALUEENTITY, VALUESTRING>
            (entity, descr_p, a_expr_p->my_second_expr_p, ind, keytype, preobject);
    case LGOP_OR:
        return result1 || HCEvaluateExpression<ENTITYREAD, IDENTIFIER, VALUE, VALUEENTITY, VALUESTRING>
            (entity, descr_p, a_expr_p->my_second_expr_p, ind, keytype, preobject);
    case LGOP_NOT:
        return !result1;
    default:
        break;
    }

    return false;
}


template <class ENTITYREAD, class IDENTIFIER, class VALUE, class VALUEENTITY, class VALUESTRING>
static bool HCEvaluateExpression(
    const ENTITYREAD* entity,
    const IDescriptor* descr_p,
    const expression_t* expr_p,
    int                      ind,
    IdenKeyType                     keytype,
    IMECPreObject* preobject = NULL)
{
    if (expr_p == NULL)            return false;
    if (descr_p == NULL)           return false;

    switch (expr_p->my_type) {
    case EXPRT_ATTRIBUTE:
        return HCLocEvaluateAttributeExpression<ENTITYREAD, IDENTIFIER, VALUE, VALUEENTITY, VALUESTRING>
            (entity, descr_p, expr_p, ind, keytype, preobject);
    case EXPRT_LOGICAL:
        return HCLocEvaluateLogicalExpression<ENTITYREAD, IDENTIFIER, VALUE, VALUEENTITY, VALUESTRING>
            (entity, descr_p, expr_p, ind, keytype, preobject);
        break;
    default:
        break;
    }
    return false;
}

template <class ENTITYREAD, class IDENTIFIER, class VALUE, class VALUEENTITY, class VALUESTRING>
static bool HCEvaluateExpression(
    const ENTITYREAD* entity,
    const IDescriptor* descr_p,
    const MvExpression_t* expr_p,
    int                      ind,
    IdenKeyType                     keytype,
    IMECPreObject *preobject = NULL)
{
    if (expr_p == NULL)            return false;

    const expression_t* a_expr_p = expr_p->getExpressionPtr();
    return HCEvaluateExpression<ENTITYREAD, IDENTIFIER, VALUE, VALUEENTITY, VALUESTRING>
        (entity, descr_p, a_expr_p, ind, keytype, preobject);
}

//Gives the vector of dimensions corresponding to a particular feature.
template <class ENTITYREAD, class IDENTIFIER, class VALUE, class VALUEENTITY, class VALUESTRING>
static bool HCDI_GetDimensionFromFeature(
    const ENTITYREAD* entity, const IDescriptor* pDescr, const MvDataFeature_t* pFeature, vector<pair<MvDimension_e, vector<string>>>& dim_lst, int ikeyword, int index, IdenKeyType    keytype)
{
    if (!pFeature)
        return false;
    MvDimension_e dimension = UDI_UNKNOWN;
    vector<string> arg_vect;
    switch (pFeature->getType())
    {
        case DFT_SCALAR:
        {
            dimension = dynamic_cast<const MvDataDimensionFeature_t*>(pFeature)->getDimension();
            arg_vect = dynamic_cast<const MvDataDimensionFeature_t*>(pFeature)->getArgVect();
        }
        break;
        case DFT_COND_SCALAR:
        {
            const MvDataCondScalarFeature_t* pSFeature =
                dynamic_cast<const MvDataCondScalarFeature_t*>(pFeature);
            assert(pSFeature);
            if (!entity)
            {
                dimension = pSFeature->getDefaultDimension();
                arg_vect = pSFeature->getDefaultArgVect();
            }
            else
            {
                int a_ind = 0;
                bool a_found = false;
                while (a_ind < pSFeature->getNbTests() && !a_found)
                {
                    const MvExpression_t* pExpr = pSFeature->getExpressionPtr(a_ind);
                    a_found = HCEvaluateExpression<ENTITYREAD, IDENTIFIER, VALUE, VALUEENTITY, VALUESTRING>
                        (entity, pDescr, pExpr, index, keytype);
                    if (!a_found) ++a_ind;
                }
                dimension = a_found ?
                    pSFeature->getConditionalDimension(a_ind) :
                    pSFeature->getDefaultDimension();
                arg_vect = a_found?
                pSFeature->getConditionalArgVect(a_ind) :
                    pSFeature->getDefaultArgVect();
            }
        }
        break;
        case DFT_TRIPLE:
      //  case DFT_POINT:
            dimension = dynamic_cast<const MvDataTripleFeature_t*>(pFeature)->getDimension();
            arg_vect = dynamic_cast<const MvDataDimensionFeature_t*>(pFeature)->getArgVect();
            break;
        case DFT_FUNCTION:
        {
            const MvDataUncondFunctionFeature_t* pFFeature =
                dynamic_cast<const MvDataUncondFunctionFeature_t*>(pFeature);
            assert(pFFeature);
            if (pFFeature->getXScalingIKeyword() == ikeyword || pFFeature->getIKeyword() == ikeyword)
            {
                dimension = pFFeature->getFunctionXDimension();
                arg_vect = pFFeature->getFunctionXArgVect();
                if (dimension != UDI_UNKNOWN)
                    dim_lst.push_back(make_pair(dimension, arg_vect));
            }
            if (pFFeature->getYScalingIKeyword() == ikeyword || pFFeature->getIKeyword() == ikeyword)
            {
                dimension = pFFeature->getFunctionYDimension();
                arg_vect = pFFeature->getFunctionYArgVect();
                if (dimension != UDI_UNKNOWN)
                    dim_lst.push_back(make_pair(dimension, arg_vect));
            }
            if (pFFeature->getZScalingIKeyword() == ikeyword || pFFeature->getIKeyword() == ikeyword)
            {
                dimension = pFFeature->getFunctionZDimension();
                arg_vect = pFFeature->getFunctionZArgVect();
                if (dimension != UDI_UNKNOWN && dimension != UDI_DIMENSIONLESS)
                    dim_lst.push_back(make_pair(dimension, arg_vect));
            }
            return true;
        }
        break;
        case DFT_COND_FUNCTION:
        {
            const MvDataCondFunctionFeature_t* pFFeature =
                dynamic_cast<const MvDataCondFunctionFeature_t*>(pFeature);
            assert(pFFeature);
            if (!entity)
            {
                if (pFFeature->getXScalingIKeyword() == ikeyword || pFFeature->getFunctionIKeyword() == ikeyword)
                {
                    dimension = pFFeature->getDefaultFunctionXDimension();
                    arg_vect = pFFeature->getDefaultFunctionXArgVect();
                    if (dimension != UDI_UNKNOWN)
                        dim_lst.push_back(make_pair(dimension, arg_vect));
                }
                if (pFFeature->getYScalingIKeyword() == ikeyword || pFFeature->getFunctionIKeyword() == ikeyword)
                {
                    dimension = pFFeature->getDefaultFunctionYDimension();
                    arg_vect = pFFeature->getDefaultFunctionYArgVect();
                    if (dimension != UDI_UNKNOWN)
                        dim_lst.push_back(make_pair(dimension, arg_vect));
                }
                if (pFFeature->getZScalingIKeyword() == ikeyword || pFFeature->getFunctionIKeyword() == ikeyword)
                {
                    dimension = pFFeature->getDefaultFunctionZDimension();
                    arg_vect = pFFeature->getDefaultFunctionZArgVect();
                    if (dimension != UDI_UNKNOWN && dimension != UDI_DIMENSIONLESS)
                        dim_lst.push_back(make_pair(dimension, arg_vect));
                }
            }
            else
            {
                if (pFFeature->getXScalingIKeyword() == ikeyword || pFFeature->getFunctionIKeyword() == ikeyword)
                {
                    int a_ind = 0;
                    bool a_found = false;
                    while (a_ind < pFFeature->getNbTests() && !a_found)
                    {
                        const MvExpression_t* pExpr = pFFeature->getExpressionPtr(a_ind);
                        a_found = HCEvaluateExpression<ENTITYREAD, IDENTIFIER, VALUE, VALUEENTITY, VALUESTRING>
                            (entity, pDescr, pExpr, index, keytype);
                        if (!a_found) ++a_ind;
                    }
                    dimension = a_found ?
                        pFFeature->getConditionalFunctionXDimension(a_ind) :
                        pFFeature->getDefaultFunctionXDimension();
                    arg_vect = a_found ?
                        pFFeature->getConditionalFunctionXArgVect(a_ind) :
                        pFFeature->getDefaultFunctionXArgVect();
                    if (dimension != UDI_UNKNOWN)
                        dim_lst.push_back(make_pair(dimension, arg_vect));
                }
                if (pFFeature->getYScalingIKeyword() == ikeyword || pFFeature->getFunctionIKeyword() == ikeyword)
                {
                    int a_ind = 0;
                    bool a_found = false;
                    while (a_ind < pFFeature->getNbTests() && !a_found)
                    {
                        const MvExpression_t* pExpr = pFFeature->getExpressionPtr(a_ind);
                        a_found = HCEvaluateExpression<ENTITYREAD, IDENTIFIER, VALUE, VALUEENTITY, VALUESTRING>
                            (entity, pDescr, pExpr, index, keytype);
                        if (!a_found) ++a_ind;
                    }
                    dimension = a_found ?
                        pFFeature->getConditionalFunctionYDimension(a_ind) :
                        pFFeature->getDefaultFunctionYDimension();
                    arg_vect = a_found ?
                        pFFeature->getConditionalFunctionYArgVect(a_ind) :
                        pFFeature->getDefaultFunctionYArgVect();
                    if (dimension != UDI_UNKNOWN)
                        dim_lst.push_back(make_pair(dimension, arg_vect));
                }
                if (pFFeature->getZScalingIKeyword() == ikeyword || pFFeature->getFunctionIKeyword() == ikeyword)
                {
                    int a_ind = 0;
                    bool a_found = false;
                    while (a_ind < pFFeature->getNbTests() && !a_found)
                    {
                        const MvExpression_t* pExpr = pFFeature->getExpressionPtr(a_ind);
                        a_found = HCEvaluateExpression<ENTITYREAD, IDENTIFIER, VALUE, VALUEENTITY, VALUESTRING>
                            (entity, pDescr, pExpr, index, keytype);
                        if (!a_found) ++a_ind;
                    }
                    dimension = a_found ?
                        pFFeature->getConditionalFunctionZDimension(a_ind) :
                        pFFeature->getDefaultFunctionZDimension();
                    arg_vect = a_found ?
                        pFFeature->getConditionalFunctionZArgVect(a_ind) :
                        pFFeature->getDefaultFunctionZArgVect();
                    if (dimension != UDI_UNKNOWN && dimension != UDI_DIMENSIONLESS)
                        dim_lst.push_back(make_pair(dimension, arg_vect));
                }
            }
            return true;
        }
        break;
        default:
            break;
    }
    if (dimension != UDI_UNKNOWN)
        dim_lst.push_back(make_pair(dimension, arg_vect));
    return true;
}

static bool HCDI_GetDimensionFromFeature(
    const IDescriptor* pDescr, const MvDataFeature_t* pFeature, map<const IDescriptor *, map<const string,
    vector<pair<MvDimension_e, vector<string>>>>>& desc_dim_map, int ikeyword, IdenKeyType keytype)
{
    if (!pFeature)
        return false;
    MvDimension_e dimension = UDI_UNKNOWN;
    vector<string> arg_vect;
    vector<pair<MvDimension_e, vector<string>>> dim_lst;
    map<const string, vector<pair<MvDimension_e, vector<string>>>> dim_map;
    const string a_skeyword = pDescr->getSKeyword(ikeyword);
    switch (pFeature->getType())
    {
        case DFT_SCALAR:
        {
            dimension = dynamic_cast<const MvDataDimensionFeature_t*>(pFeature)->getDimension();
            arg_vect = dynamic_cast<const MvDataDimensionFeature_t*>(pFeature)->getArgVect();
        }
        break;
        case DFT_COND_SCALAR:
        {
            const MvDataCondScalarFeature_t* pSFeature = dynamic_cast<const MvDataCondScalarFeature_t*>(pFeature);
            assert(pSFeature);

            int a_ind = 0;
            bool a_found = false;
            while (a_ind <= pSFeature->getNbTests())
            {
                dim_lst.push_back(make_pair(pSFeature->getConditionalDimension(a_ind), pSFeature->getConditionalArgVect(a_ind)));
                a_ind++;
            }
            if (dim_lst.size())
            {
                dim_map.insert(make_pair(a_skeyword, dim_lst));
                if (desc_dim_map.empty())
                {
                    desc_dim_map[pDescr] = dim_map;
                }
                else
                {
                    auto a_iter = desc_dim_map.find(pDescr);
                    if (a_iter != desc_dim_map.end())
                    {
                        auto& a_dim_map = a_iter->second;
                        for (auto loc_iter = dim_map.begin(); loc_iter != dim_map.end(); ++loc_iter)
                        {
                            a_dim_map.insert(make_pair(loc_iter->first, loc_iter->second));
                        }
                    }
                    else
                    {
                        desc_dim_map[pDescr] = dim_map;
                    }
                }
            }
            return true;
        }
        break;
        case DFT_TRIPLE:
            //  case DFT_POINT:
            dimension = dynamic_cast<const MvDataTripleFeature_t*>(pFeature)->getDimension();
            arg_vect = dynamic_cast<const MvDataDimensionFeature_t*>(pFeature)->getArgVect();
            break;
        case DFT_FUNCTION:
        {
            const MvDataUncondFunctionFeature_t* pFFeature =
                dynamic_cast<const MvDataUncondFunctionFeature_t*>(pFeature);
            assert(pFFeature);
            if (pFFeature->getXScalingIKeyword() == ikeyword || pFFeature->getIKeyword() == ikeyword)
            {
                dimension = pFFeature->getFunctionXDimension();
                arg_vect = pFFeature->getFunctionXArgVect();
                dim_lst.push_back(make_pair(dimension, arg_vect));
            }
            if (pFFeature->getYScalingIKeyword() == ikeyword || pFFeature->getIKeyword() == ikeyword)
            {
                dimension = pFFeature->getFunctionYDimension();
                arg_vect = pFFeature->getFunctionYArgVect();
                dim_lst.push_back(make_pair(dimension, arg_vect));
            }
            if (pFFeature->getZScalingIKeyword() == ikeyword || pFFeature->getIKeyword() == ikeyword)
            {
                dimension = pFFeature->getFunctionZDimension();
                arg_vect = pFFeature->getFunctionZArgVect();
                dim_lst.push_back(make_pair(dimension, arg_vect));
            }
            if (dim_lst.size())
            {
                dim_map.insert(make_pair(a_skeyword, dim_lst));
                if (desc_dim_map.empty())
                {
                    desc_dim_map[pDescr] = dim_map;
                }
                else
                {
                    auto a_iter = desc_dim_map.find(pDescr);
                    if (a_iter != desc_dim_map.end())
                    {
                        auto& a_dim_map = a_iter->second;
                        for (auto loc_iter = dim_map.begin(); loc_iter != dim_map.end(); ++loc_iter)
                        {
                            a_dim_map.insert(make_pair(loc_iter->first, loc_iter->second));
                        }
                    }
                    else
                    {
                        desc_dim_map[pDescr] = dim_map;
                    }
                }
            }
            return true;
        }
        break;
        case DFT_COND_FUNCTION:
        {
            const MvDataCondFunctionFeature_t* pFFeature =
                dynamic_cast<const MvDataCondFunctionFeature_t*>(pFeature);
            assert(pFFeature);

            if (pFFeature->getXScalingIKeyword() == ikeyword || pFFeature->getFunctionIKeyword() == ikeyword)
            {
                int a_ind = 0;
                while (a_ind <= pFFeature->getNbTests())
                {
                    dim_lst.push_back(make_pair(pFFeature->getConditionalFunctionXDimension(a_ind), pFFeature->getConditionalFunctionXArgVect(a_ind)));
                    a_ind++;
                }
            }
            if (pFFeature->getYScalingIKeyword() == ikeyword || pFFeature->getFunctionIKeyword() == ikeyword)
            {
                int a_ind = 0;
                while (a_ind <= pFFeature->getNbTests())
                {
                    dim_lst.push_back(make_pair(pFFeature->getConditionalFunctionYDimension(a_ind), pFFeature->getConditionalFunctionYArgVect(a_ind)));
                    a_ind++;
                }
            }
            if (pFFeature->getZScalingIKeyword() == ikeyword || pFFeature->getFunctionIKeyword() == ikeyword)
            {
                int a_ind = 0;
                while (a_ind <= pFFeature->getNbTests())
                {
                    dim_lst.push_back(make_pair(pFFeature->getConditionalFunctionZDimension(a_ind), pFFeature->getConditionalFunctionZArgVect(a_ind)));
                    a_ind++;
                }
            }
            if (dim_lst.size())
            {
                dim_map.insert(make_pair(a_skeyword, dim_lst));
                if (desc_dim_map.empty())
                {
                    desc_dim_map[pDescr] = dim_map;
                }
                else
                {
                    auto a_iter = desc_dim_map.find(pDescr);
                    if (a_iter != desc_dim_map.end())
                    {
                        auto& a_dim_map = a_iter->second;
                        for (auto loc_iter = dim_map.begin(); loc_iter != dim_map.end(); ++loc_iter)
                        {
                            a_dim_map.insert(make_pair(loc_iter->first, loc_iter->second));
                        }
                    }
                    else
                    {
                        desc_dim_map[pDescr] = dim_map;
                    }
                }
            }
            return true;
        }
        break;
        case DFT_SUBOBJECT:
        {
            const MvFullTypeSet_t& full_type_set = ((MvDataDataFeature_t*)pFeature)->getAllowedObjectFullTypes();
            attribute_type_e atype = pDescr->getAttributeType(ikeyword);
            if (full_type_set.size() && (atype == ATYPE_VALUE))
            {
                MvFullTypeSet_t::const_iterator a_it_begin = full_type_set.begin();
                const MvFullType_t& a_full_type = (*a_it_begin);
                IDescriptor* subdescrp = HCDI_GetDescriptorHandleFromFullType(a_full_type);
                if (subdescrp)
                {
                    return HCDI_GetDimension(subdescrp, desc_dim_map, keytype);
                }
            }
            break;
        }
        default:
            break;
    }
    dim_lst.push_back(make_pair(dimension, arg_vect));

    if (dim_lst.size())
    {
        dim_map.insert(make_pair(a_skeyword, dim_lst));
        if (desc_dim_map.empty())
        {
            desc_dim_map[pDescr] = dim_map;
        }
        else
        {
            auto a_iter = desc_dim_map.find(pDescr);
            if (a_iter != desc_dim_map.end())
            {
                auto& a_dim_map = a_iter->second;
                for (auto loc_iter = dim_map.begin(); loc_iter != dim_map.end(); ++loc_iter)
                {
                    a_dim_map.insert(make_pair(loc_iter->first, loc_iter->second));
                }
            }
            else
            {
                desc_dim_map[pDescr] = dim_map;
            }
        }
    }
    return true;
}

static bool HCDI_GetDimension(const IDescriptor* pDescr, map<const IDescriptor *, map<const string, vector<pair<MvDimension_e, vector<string>>>>> &desc_dim_map, IdenKeyType keytype)
{
    bool ret_type = false;
    int domains = DOM_COMMON;
    int a_all_domains = HCDI_get_all_domains();
    MvDataFeatureList_t a_dfl;
    if (!pDescr)
        return ret_type;
    pDescr->getDataFeatures(a_all_domains, &a_dfl);
    MvDataFeatureList_t::iterator a_it_begin = a_dfl.begin();
    MvDataFeatureList_t::iterator a_it_end = a_dfl.end();
    MvDataFeatureList_t::iterator a_it;

    for (a_it = a_it_begin; a_it != a_it_end; ++a_it) {
        const MvDataFeature_t* pFeature = (*a_it);
        if (!pFeature)
            continue;
        MvDataFeatureType_e ftype = pFeature->getType();

        if ((ftype == DFT_DYNAMIC_ARRAY) || (ftype == DFT_STATIC_ARRAY))
        {
            MvDataArrayFeature_t* arr_fea = (MvDataArrayFeature_t *)pFeature;
            if (!arr_fea)
                continue;
            int nb = arr_fea->getNumber();
            for (int i = 0; i < nb; i++)
            {
                const MvDataFeature_t* a_feature = arr_fea->getDataFeature(i);
                if (!a_feature)
                    continue;
                MvDataFeatureType_e aa_type = a_feature->getType();
                if (DFT_IF == aa_type)
                {
                    MvDataIfFeature_t* dft_if = (MvDataIfFeature_t*)a_feature;
                    MvDataFeatureSet_t allFeaturesLst;
                    dft_if->getAllFeatures(allFeaturesLst);

                    MvDataFeatureSet_t::iterator a_ifbeg = allFeaturesLst.begin();
                    MvDataFeatureSet_t::iterator a_ifend = allFeaturesLst.end();
                    MvDataFeatureSet_t::iterator a_ifit;
                    for (a_ifit = a_ifbeg; a_ifit != a_ifend; ++a_ifit)
                    {
                        const MvDataFeature_t* d_fea = *a_ifit;
                        a_dfl.push_back(d_fea);
                    }
                }
                else
                    a_dfl.push_back(a_feature);
            }
        }
    }

    for (a_it = a_it_begin; a_it != a_it_end; ++a_it) {
        const MvDataFeature_t* pFeature = (*a_it);
        int a_ikeyword = pFeature->getIKeyword();
        if (pFeature && a_ikeyword > 0)
        {
            ret_type = HCDI_GetDimensionFromFeature(pDescr, pFeature, desc_dim_map, a_ikeyword, keytype);
        }
    }
    return ret_type;
}
