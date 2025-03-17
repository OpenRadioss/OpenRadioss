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

#include "hcdi_drawableinf_pre_object.h"
#include <assert.h>


hwCFGDrawableInfPreObject::hwCFGDrawableInfPreObject(const IMECPreObject *preobj_p, const IDescriptor *descr_p) :
    hwCFGDrawableInf(), p_preobj_p(preobj_p), p_descr_p(descr_p)
{
    if(nullptr == p_descr_p && nullptr != preobj_p)
    {
        p_descr_p = HCDI_GetDescriptorHandle(preobj_p->GetKernelFullType());
    }
}

int hwCFGDrawableInfPreObject::EvaluateDrawable(const std::string& drawable_name, double& value) const
{
    assert(0); // needs to be implemented!
    return -1;
}

double hwCFGDrawableInfPreObject::GetFloatAttribute(int ikeyword) const
{
    if(nullptr == p_preobj_p || nullptr == p_descr_p) return 0;
    const char *skeyword = nullptr;
    p_descr_p->getSKeyword(ikeyword, &skeyword);
    int ind = p_preobj_p->GetIndex(IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_FLOAT, skeyword);
    if(0 <= ind) return p_preobj_p->GetFloatValue(ind);
    ind = p_preobj_p->GetIndex(IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_INT, skeyword);
    if(0 <= ind) return p_preobj_p->GetIntValue(ind);
    ind = p_preobj_p->GetIndex(IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_UINT, skeyword);
    if(0 <= ind) return p_preobj_p->GetUIntValue(ind);
    ind = p_preobj_p->GetIndex(IMECPreObject::ATY_SINGLE,IMECPreObject::VTY_BOOL, skeyword);
    if(0 <= ind) return p_preobj_p->GetBoolValue(ind);
    // if needed, we could support OBJECT and STRING here?
    return 0;
}

bool hwCFGDrawableInfPreObject::GetFloatArrayAttribute(int ikeyword, std::vector<double>& val_array) const
{
    assert(0); // needs to be implemented!
    return false;
}

int hwCFGDrawableInfPreObject::EvaluateSubDrawableObjectAttrib(
    int ikeyword, const std::string& sub_drawable_name, double& value) const
{
    assert(0); // needs to be implemented!
    return -1;
}
