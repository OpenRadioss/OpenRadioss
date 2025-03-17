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
#include <sdiElement.h>
#include <_private/sdiModelViewPrivate.h>
#include <_private/sdiNodeData.h>


using namespace sdi;


// *********************************************************************************
//   NodeRead
// *********************************************************************************

// The result of this function provides access to a permanent DB entity.
NodeRead::NodeRead(const ModelViewRead* pModelView,
                   const HandleNodeRead& handle) :
    EntityBaseRead()
{
    assert(handle.IsValid());                  //! assume that coming in with valid entity
    p_pData = static_cast<const SDIModelViewPrivate*>(pModelView)->Objectify(handle);
}


NodeRead::NodeRead(const ModelViewRead* pModelView,
                   const HandleRead& handle) :
    EntityBaseRead()
{
    assert(handle.IsValid());                  //! assume that coming in with valid entity
    assert(handle.GetSpecializationType(pModelView) == SPECIALIZATION_TYPE_NODE);

    if (handle.GetSpecializationType(pModelView) == SPECIALIZATION_TYPE_NODE)
    {
        SDIEntityData* d = static_cast<const SDIModelViewPrivate*>(pModelView)->Objectify(handle);
        p_pData = static_cast<SDINodeData*>(d);
    }
}


NodeRead::NodeRead(const NodeRead& other)
    : EntityBaseRead()
{
    p_pData = other.p_pData ? other.p_pData->Clone() : 0;
}


// = operator assigns clone of source as temporary entity in target
NodeRead& NodeRead::operator=(const NodeRead& other)
{
    if (other.p_pData == p_pData)
    {
        return *this;    //! copy onto itself does leaves things as they are
    }
    if (p_pData)
    {
        p_pData->Destroy();    //! remove its current data
    }
    p_pData = other.p_pData ? other.p_pData->Clone() : 0;
    return *this;
}


HandleNodeRead NodeRead::GetHandle() const
{
    assert(p_pData);
    if(p_pData) return HandleNodeRead(p_pData->GetHandle());
    else        return HandleNodeRead();
}


NodeRead::~NodeRead()
{
    if (p_pData)
    {
        p_pData->Destroy();
    }
}


sdiTriple NodeRead::GetPosition() const
{
    if (!p_pData)
    {
        return sdiTriple();
    }
    return p_pData->GetPosition();
}


// *********************************************************************************
//   NodeEdit
// *********************************************************************************

// The result of this function provides access to a permanent DB entity.
NodeEdit::NodeEdit(const ModelViewEdit* pModelView,
                   const HandleNodeEdit& handle) :
    NodeRead(), EntityBaseEdit()
{
    assert(handle.IsValid());                  //! assume that coming in with valid entity
    p_pData = static_cast<const SDIModelViewPrivate*>(pModelView)->Objectify(handle);
}


NodeEdit::NodeEdit(const ModelViewEdit* pModelView,
                   const HandleEdit& handle) :
    NodeRead(), EntityBaseEdit()
{
    assert(handle.IsValid());                  //! assume that coming in with valid entity
    assert(handle.GetSpecializationType(pModelView) == SPECIALIZATION_TYPE_NODE);

    if (handle.GetSpecializationType(pModelView) == SPECIALIZATION_TYPE_NODE)
    {
        SDIEntityData* d = static_cast<const SDIModelViewPrivate*>(pModelView)->Objectify(handle);
        p_pData = static_cast<SDINodeData*>(d);
    }
}


NodeEdit::NodeEdit(const NodeEdit& other)
    : NodeRead(), EntityBaseEdit()
{
    p_pData = other.p_pData ? other.p_pData->Clone() : 0;
}


// = operator assigns clone of source as temporary entity in target
NodeEdit& NodeEdit::operator=(const NodeEdit& other)
{
    if (other.p_pData == p_pData)
    {
        return *this;    //! copy onto itself does leaves things as they are
    }
    if (p_pData)
    {
        p_pData->Destroy();    //! remove its current data
    }
    p_pData = other.p_pData ? other.p_pData->Clone() : 0;
    return *this;
}


HandleNodeEdit NodeEdit::GetHandle() const
{
    assert(p_pData);
    if(p_pData) return HandleNodeEdit(p_pData->GetHandle());
    else        return HandleNodeEdit();
}


Status NodeEdit::SetPosition(const sdiTriple& position) const
{
    if(p_pData) return p_pData->SetPosition(position);
    else        return false;
}

