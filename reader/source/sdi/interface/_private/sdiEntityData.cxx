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




////////////////////////////////////////////////////////////////////////////////////

#include <_private/sdiEntityData.h>

using namespace sdi;

// *********************************************************************************
// SDIEntityData
// *********************************************************************************

    //! Specialized "GetValue" for entity references.
    //! This is at least a convenience API.
    //! Providing a base implementation here that uses p_pModelView->FindById().
    //! Implementer may redefine if desired, e.g. in order to have a performance gain.
Status SDIEntityData::GetEntityHandle(const sdiIdentifier& identifier,
                                      HandleEdit&                     handle) const
{
    if(!p_pModelView) { handle.SetPointer(0); return false; }

    // get generic sdiValue
    sdiValue value;
    bool isOk = GetValue(identifier,value);
    if(!isOk) { handle.SetPointer(0); return false; }

    // optional: test whether value has the right type
    if(value.GetCompoundType() != sdiCompoundType::COMPOUND_TYPE_ENTITY ||
        value.GetArrayDimension() != 0)
    {
        handle.SetPointer(0); return false;
    }

    // get sdiValueEntity
    sdiValueEntity descriptorentity;
    isOk = value.GetValue(descriptorentity);
    if(!isOk) { handle.SetPointer(0); return false; }

    // find in model by id and type
    unsigned int id = descriptorentity.GetId();
    unsigned int type = 0;
    sdiValueEntityType descriptortype = descriptorentity.GetEntityFullType();
    if(descriptortype.IsTypeNumeric())
    {
        type = descriptortype.GetTypeNumeric();
    }
    else
    {
        sdiString typenamed = descriptortype.GetTypeNamed();
        type = p_pModelView->GetEntityType(typenamed);
    }
    return p_pModelView->FindById(type,id,handle);
}

Status SDIEntityData::SetEntityHandle(const sdiIdentifier& identifier,
                                      const HandleRead& handle)
{
    if(!p_pModelView) return false;
    return SetValue(identifier, sdiValue(sdiValueEntity(sdiValueEntityType(handle.GetType()), handle.GetId(p_pModelView))));
}
