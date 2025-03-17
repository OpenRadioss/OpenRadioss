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
#include "mec_pre_object_expression_evaluator.h"

#include "HCDI/hcdi_mec_pre_object.h"
#include <HCDI/hcdi_mv_descriptor.h>

MECPreObjectExpressionEvaluator::MECPreObjectExpressionEvaluator(
    const IMECPreObject* preobject_p, const IDescriptor *descriptor_p,
    const IExpressionEvaluator* pEvaluator, int ind):
    myPreObjectPtr(preobject_p), myDescriptorPtr(descriptor_p),
    IValueExpressionEvaluator(pEvaluator), index(ind)
{}

bool MECPreObjectExpressionEvaluator::GetValue(const char* name, double& value) const
{
    if(myPreObjectPtr == NULL || myDescriptorPtr == NULL) return false;
    int ikey = myDescriptorPtr->getIKeyword(name);
    if(ikey <= 0)  return false;

    attribute_type_e a_att_type = myDescriptorPtr->getAttributeType(ikey);
    IMECPreObject::MyValueType_e attrib_vtype = IMECPreObject::VTY_UNKNOWN;

    if(a_att_type == ATYPE_VALUE || a_att_type == ATYPE_SIZE)
    {
        attrib_vtype = myPreObjectPtr->GetValueType(IMECPreObject::ATY_SINGLE,name);
        if(attrib_vtype == IMECPreObject::VTY_BOOL)
        {
            value = (double)myPreObjectPtr->GetBoolValue(name);
        }
        else if(attrib_vtype == IMECPreObject::VTY_INT)
        {
            value = (double)myPreObjectPtr->GetIntValue(name);
        }
        else if(attrib_vtype == IMECPreObject::VTY_UINT)
        {
            value = (double)myPreObjectPtr->GetUIntValue(name);
        }
        else if(attrib_vtype == IMECPreObject::VTY_FLOAT)
        {
            value = myPreObjectPtr->GetFloatValue(name);
        }
        else if(attrib_vtype == IMECPreObject::VTY_OBJECT)
        {
            int attrib_ind = myPreObjectPtr->GetIndex(IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_OBJECT,name);
            value = myPreObjectPtr->GetObjectId(attrib_ind);
        }
        else return false;
    } 
    else if(index >= 0 && (a_att_type == ATYPE_STATIC_ARRAY || a_att_type == ATYPE_DYNAMIC_ARRAY))
    {
        attrib_vtype = myPreObjectPtr->GetValueType(IMECPreObject::ATY_ARRAY,name);
        int att_ind = myPreObjectPtr->GetIndex(IMECPreObject::ATY_ARRAY, attrib_vtype, name);
        if(attrib_vtype == IMECPreObject::VTY_BOOL)
        {
            value = (double)myPreObjectPtr->GetBoolValue(att_ind, index);
        }
        else if(attrib_vtype == IMECPreObject::VTY_INT)
        {
            value = (double)myPreObjectPtr->GetIntValue(att_ind, index);
        }
        else if(attrib_vtype == IMECPreObject::VTY_UINT)
        {
            value = (double)myPreObjectPtr->GetUIntValue(att_ind, index);
        }
        else if(attrib_vtype == IMECPreObject::VTY_FLOAT)
        {
            value = myPreObjectPtr->GetFloatValue(att_ind, index);
        }
        else if(attrib_vtype == IMECPreObject::VTY_OBJECT)
        {
            value = myPreObjectPtr->GetObjectId(att_ind, index);
        }
        else return false;
    }
    else return false;
    return true;
}
