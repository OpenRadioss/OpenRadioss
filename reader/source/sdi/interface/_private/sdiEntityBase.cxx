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

#include <sdiNode.h>
#include <sdiModelView.h>
#include <sdiEntityBase.h>
#include <_private/sdiEntityData.h>
#include <_private/sdiModelViewPrivate.h>
#include <_private/sdiSelectionData.h>
#include <sdiEntity.h>


using namespace sdi;


// *********************************************************************************
//   EntityBaseRead
// *********************************************************************************

// Following functions are made virtual to ensure that all entities derived
// from this class have this functionality. Pure virtuals are used to force
// an implementation.
//
// : TODO think of throwing an exception if p_pData = 0

sdiString EntityBaseRead::GetName() const
{
    const SDIEntityData* p = GetData();
    return p ? p->GetName() : sdiString();
}

unsigned int EntityBaseRead::GetId() const
{
    const SDIEntityData* p = GetData();
    return p ? p->GetId() : 0;
}

bool EntityBaseRead::operator==(const EntityBaseRead& other) const
{
    const SDIEntityData* p_a = GetData();
    const SDIEntityData* p_b = other.GetData();
    return p_a ? p_a->IsEqualTo(p_b) : (p_b == 0);
}


const ModelViewRead* EntityBaseRead::GetModelViewRead() const
{
    const SDIEntityData* p = GetData();
    return p ? p->GetModelView() : 0;
}

EntityType EntityBaseRead::GetType() const
{
    const SDIEntityData* p = GetData();
    return p ? p->GetType() : 0;
}

const sdiString& EntityBaseRead::GetKeyword() const
{
    const SDIEntityData* p = GetData();
    return p ? p->GetKeyword() : EMPTYSTRING;
}


HandleRead EntityBaseRead::GetInclude() const
{
    const SDIEntityData* p = GetData();
    return p ? p->GetInclude() : HandleRead(0);
}


////////////////////////////////////////////////////////////////////////////////
// Descriptior Values and Hierachy by identifier
//     Return false if unable to identify.
////////////////////////////////////////////////////////////////////////////////
Status EntityBaseRead::GetValue(const sdiIdentifier& identifier,
                                sdiValue&            value) const
{
    assert(GetData());
    return GetData()->GetValue(identifier, value);
}

Status EntityBaseRead::GetEntityHandle(const sdiIdentifier& identifier,
                                       HandleRead&       handle) const
{
    assert(GetData());
    HandleEdit handleedit;
    Status status = GetData()->GetEntityHandle(identifier, handleedit);
    handle = handleedit;
    return status;
}

Status EntityBaseRead::GetAttributes(sdiVector<sdiIdentifier>& aIdentifier) const
{
    assert(GetData());
    return GetData()->GetAttributes(aIdentifier);
}

bool EntityBaseRead::IsParameterized(const sdiIdentifier& identifier) const
{
    assert(GetData());
    return GetData()->IsParameterized(identifier);
}

sdiString EntityBaseRead::GetParameterName(const sdiIdentifier& identifier, bool* pIsNegated) const
{
    assert(GetData());
    return GetData()->GetParameterName(identifier, pIsNegated);
}

// *********************************************************************************
//   EntityBaseEdit
// *********************************************************************************

Status EntityBaseEdit::SetName(const sdiString& name) const
{
    const SDIEntityData* p = GetData();
    if(p) return p->SetName(name);
    else  return false;
}


Status EntityBaseEdit::SetId(const unsigned int id) const  //! returns false if unable to assign id
{
    const SDIEntityData* p = GetData();
    if(p) return p->SetId(id);
    else  return false;
}

const ModelViewEdit* EntityBaseEdit::GetModelViewEdit() const
{
    const SDIEntityData* p = GetData();
    return p ? p->GetModelView() : 0;
}


Status EntityBaseEdit::SetInclude(const HandleRead& includefile) const
{
    const SDIEntityData* p = GetData();

    // No need to check if the includeFile is a valid handle, 
    // since sdiCommonDataHM will use validity to either set an Id or the 0 id. 
    // Thus if we need a way of SetInclude to include file zero, no IsValid check must be made.
    if(p) return p->SetInclude(includefile);
    else  return false;
}


HandleEdit EntityBaseEdit::GetInclude() const
{
    const SDIEntityData* p = GetData();
    return p ? p->GetInclude() : HandleEdit(0);
}


////////////////////////////////////////////////////////////////////////////////
// Descriptior Values by identifier
//     Return false if unable to identify.
////////////////////////////////////////////////////////////////////////////////
Status EntityBaseEdit::GetEntityHandle(const sdiIdentifier& identifier,
                                       HandleRead&       handle) const
{
    assert(GetData());
    HandleEdit handleedit;
    Status status = GetData()->GetEntityHandle(identifier, handleedit);
    handle = handleedit;
    return status;
}

Status EntityBaseEdit::GetEntityHandle(const sdiIdentifier& identifier,
                                       HandleEdit&       handle) const
{
    assert(GetData());
    return GetData()->GetEntityHandle(identifier, handle);
}

Status EntityBaseEdit::SetValue(const sdiIdentifier& identifier,
                              const sdiValue&      value) const
{
    assert(GetData());
    return GetData()->SetValue(identifier, value);
}

Status EntityBaseEdit::SetParameter(const sdiIdentifier& identifier,
                                  const sdiString& parameterName, bool isNegated) const
{
    assert(GetData());
    return GetData()->SetParameter(identifier, parameterName, isNegated);
}

Status EntityBaseEdit::SetEntityHandle(const sdiIdentifier& identifier,
                                       const HandleRead& handle)
{
    assert(GetData());
    return GetData()->SetEntityHandle(identifier, handle);
}


Status EntityBaseEdit::TransformEntity(double matrix[4][4],bool scale) const
{
    const SDIEntityData* p = GetData();
    if(p) return p->TransformEntity(matrix,scale);
    else  return false;
}

