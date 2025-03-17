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

#include <sdiHandles.h>
#include <_private/sdiModelViewPrivate.h>


using namespace sdi;


// *********************************************************************************
// HandleRead inplementation
// *********************************************************************************

//! Retrieve specialization type
SpecializationType HandleRead::GetSpecializationType(const ModelViewRead* pModelView) const
{
    assert(nullptr != pModelView);
    if(nullptr != pModelView) return pModelView->GetSpecializationType(p_type);
    else                      return SPECIALIZATION_TYPE_GENERAL;
}

unsigned int HandleRead::GetId(const ModelViewRead* pModelView) const
{
    const SDIModelViewPrivate* pmv = static_cast<const SDIModelViewPrivate*>(pModelView);
    return (IsValid() && pmv) ? pmv->P_GetId(*this) : 0;
}


Status HandleEdit::SetId(const ModelViewEdit* pModelView,
                         const unsigned int id) const
{
    const SDIModelViewPrivate* pmv = static_cast<const SDIModelViewPrivate*>(pModelView);
    if(IsValid() && pmv) return pmv->P_SetId(*this, id);
    else                 return false;
}


sdiString HandleRead::GetName(const ModelViewRead* pModelView) const
{
    const SDIModelViewPrivate* pmv = static_cast<const SDIModelViewPrivate*>(pModelView);
    return (IsValid() && pmv) ? pmv->P_GetName(*this) : "";
}


Status HandleEdit::SetName(const ModelViewEdit* pModelView,
                           const sdiString& name) const
{
    const SDIModelViewPrivate* pmv = static_cast<const SDIModelViewPrivate*>(pModelView);
    if(IsValid() && pmv) return pmv->P_SetName(*this, name);
    else                 return false;
}



////////////////////////////////////////////////////////////////////////////////
// Descriptior Values and Hierachy by identifier
//     Return false if unable to identify.
////////////////////////////////////////////////////////////////////////////////
Status HandleRead::GetValue(const ModelViewRead* pModelView,
                            const sdiIdentifier&    identifier,
                            sdiValue&               value) const
{
    const SDIModelViewPrivate* pmv = static_cast<const SDIModelViewPrivate*>(pModelView);
    //Added to call SetValue for includeFile id 0
    if (IsValid() && pmv /* || type == SPECIALIZATION_TYPE_INCLUDEFILE */)
    {
        return pmv->GetValue(*this, identifier, value);
    }
    return false;
}

Status HandleRead::GetEntityHandle(const ModelViewRead*            pModelView,
                                   const sdiIdentifier& identifier,
                                   HandleRead&                     handle) const
{
    const SDIModelViewPrivate* pmv = static_cast<const SDIModelViewPrivate*>(pModelView);
    //Added to call SetValue for includeFile id 0
    if (IsValid() && pmv /* || type == SPECIALIZATION_TYPE_INCLUDEFILE */)
    {
        HandleEdit handleedit;
        Status status = pmv->GetEntityHandle(*this, identifier, handleedit);
        handle = handleedit;
        return status;
    }
    return false;
}

Status HandleEdit::GetEntityHandle(const ModelViewRead*            pModelView,
                                   const sdiIdentifier& identifier,
                                   HandleEdit&                     handle) const
{
    return GetEntityHandle(pModelView, identifier, (HandleRead&) handle);
}

Status HandleEdit::SetValue(const ModelViewEdit* pModelView,
                            const sdiIdentifier&    identifier,
                            const sdiValue&         value) const
{
    const SDIModelViewPrivate* pmv = static_cast<const SDIModelViewPrivate*>(pModelView);
    //Added to call SetValue for includeFile id 0
    if (IsValid() && pmv /* || type == SPECIALIZATION_TYPE_INCLUDEFILE */)
    {
        return pmv->SetValue(*this, identifier, value);
    }
    return false;
}

Status HandleEdit::SetEntityHandle(const ModelViewEdit* pModelView,
                                   const sdiIdentifier&    identifier,
                                   const HandleRead&    handle) const
{
    const SDIModelViewPrivate* pmv = static_cast<const SDIModelViewPrivate*>(pModelView);
    //Added to call SetValue for includeFile id 0
    if (IsValid() && pmv /* || type == SPECIALIZATION_TYPE_INCLUDEFILE */)
    {
        return pmv->SetEntityHandle(*this, identifier, handle);
    }
    return false;
}

bool HandleRead::IsParameterized(const ModelViewRead*            pModelView,
                                 const sdiIdentifier& identifier) const
{
    const SDIModelViewPrivate* pmv = static_cast<const SDIModelViewPrivate*>(pModelView);
    return (IsValid() && pmv) ? pmv->IsParameterized(*this, identifier) : false;
}

sdiString HandleRead::GetParameterName(const ModelViewRead*            pModelView,
                                      const sdiIdentifier& identifier,
                                      bool*                           pIsNegated) const
{
    const SDIModelViewPrivate* pmv = static_cast<const SDIModelViewPrivate*>(pModelView);
    return (IsValid() && pmv) ? pmv->GetParameterName(*this, identifier, pIsNegated) : sdiString();
}

Status HandleEdit::SetParameter(const ModelViewEdit*            pModelView,
                                const sdiIdentifier&            identifier,
                                const sdiString&                parameterName,
                                bool                            isNegated) const
{
    const SDIModelViewPrivate* pmv = static_cast<const SDIModelViewPrivate*>(pModelView);
    return (IsValid() && pmv) ? pmv->SetParameter(*this, identifier, parameterName, isNegated) : false;
}
