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

#include <sdiElement.h>
#include <_private/sdiModelViewPrivate.h>
#include <_private/sdiElementData.h>


using namespace sdi;


// *********************************************************************************
//   ElementRead
// *********************************************************************************

// The result of this function provides access to a permanent DB entity.
ElementRead::ElementRead(const ModelViewRead* pModelView,
                         const HandleElementRead& handle)
    : EntityBaseRead()
{
    assert(handle.IsValid());                  //! assume that coming in with valid entity
    p_pData = static_cast<const SDIModelViewPrivate*>(pModelView)->Objectify(handle);
}


ElementRead::ElementRead(const ModelViewRead* pModelView,
                         const HandleRead& handle)
    : EntityBaseRead()
{
    assert(handle.IsValid());                  //! assume that coming in with valid entity
    assert(handle.GetSpecializationType(pModelView) == SPECIALIZATION_TYPE_ELEMENT);

    if (handle.GetSpecializationType(pModelView) == SPECIALIZATION_TYPE_ELEMENT)
    {
        SDIEntityData* d = static_cast<const SDIModelViewPrivate*>(pModelView)->Objectify(handle);
        p_pData = static_cast<SDIElementData*>(d);
    }
}



ElementRead::ElementRead(const ElementRead& other)
    : EntityBaseRead()
{
    p_pData = other.p_pData ? other.p_pData->Clone() : 0;
}


ElementRead::~ElementRead()
{
    if (p_pData)
    {
        p_pData->Destroy();
    }
}


// = operator assigns clone of source as temporary entity in target
ElementRead& ElementRead::operator=(const ElementRead& other)
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


HandleElementRead ElementRead::GetHandle() const
{
    assert(p_pData);
    if(p_pData) return HandleElementRead(p_pData->GetHandle());
    else        return HandleElementRead();
}


////////////////////////////////////////////////////////////////////////////////
// Access to entity's data
////////////////////////////////////////////////////////////////////////////////

unsigned int ElementRead::GetNodeCount() const
{
    return p_pData ? p_pData->GetNodeCount() : 0;
}


void ElementRead::GetNodeHandles(sdiVector<HandleNodeRead> &ahNode) const
{
    if (p_pData)
    {
        p_pData->GetNodeHandles(ahNode);
    }
    else
    {
        ahNode.resize(0);
    }
}


void ElementRead::GetNodeIds(sdiVector<unsigned int> &aNodeId) const
{
    if (p_pData)
    {
        p_pData->GetNodeIds(aNodeId);
    }
    else
    {
        aNodeId.resize(0);
    }
}



HandleRead ElementRead::GetOwner() const
{
    return p_pData ? p_pData->GetOwner() : HandleRead();
}


unsigned int ElementRead::GetOwnerId() const
{
    return p_pData ? p_pData->GetOwnerId() : 0;
}


// *********************************************************************************
//   ElementEdit
// *********************************************************************************

// The result of this function provides access to a permanent DB entity.
ElementEdit::ElementEdit(const ModelViewEdit* pModelView,
                         const HandleElementEdit& handle)
    : ElementRead(), EntityBaseEdit()
{
    assert(handle.IsValid());                  //! assume that coming in with valid entity
    p_pData = static_cast<const SDIModelViewPrivate*>(pModelView)->Objectify(handle);
}


ElementEdit::ElementEdit(const ModelViewEdit* pModelView,
                         const HandleEdit& handle)
    : ElementRead(), EntityBaseEdit()
{
    assert(handle.IsValid());                  //! assume that coming in with valid entity
    assert(handle.GetSpecializationType(pModelView) == SPECIALIZATION_TYPE_ELEMENT);

    if (handle.GetSpecializationType(pModelView) == SPECIALIZATION_TYPE_ELEMENT)
    {
        SDIEntityData* d = static_cast<const SDIModelViewPrivate*>(pModelView)->Objectify(handle);
        p_pData = static_cast<SDIElementData*>(d);
    }
}


ElementEdit::ElementEdit(const ElementEdit& other)
    : ElementRead(), EntityBaseEdit()
{
    p_pData = other.p_pData ? other.p_pData->Clone() : 0;
}


// = operator assigns clone of source as temporary entity in target
ElementEdit& ElementEdit::operator=(const ElementEdit& other)
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


HandleElementEdit ElementEdit::GetHandle() const
{
    assert(p_pData);
    if(p_pData) return HandleElementEdit(p_pData->GetHandle());
    else        return HandleElementEdit();
}


////////////////////////////////////////////////////////////////////////////////
// Access to entity's data
////////////////////////////////////////////////////////////////////////////////

Status ElementEdit::ReplaceNode(const HandleNodeRead& hnode, unsigned int index) const
{
    if(p_pData) return p_pData->ReplaceNode(hnode, index);
    else        return false;
}


Status ElementEdit::SetNodes(const sdiVector<HandleNodeRead> &ahNode) const
{
    if(p_pData) return p_pData->SetNodes(ahNode);
    else        return false;
}


Status ElementEdit::SetNodes(const sdiVector<HandleNodeEdit> &ahNode) const
{
    if(p_pData) return p_pData->SetNodes(ahNode);
    else        return false;
}

Status ElementEdit::SetOwner(const HandleRead& hOwner) const
{
    if(p_pData) return p_pData->SetOwner(hOwner);
    else        return false;
}


// *********************************************************************************
//   Selections specific to Element
// *********************************************************************************

SelectionElementRead::SelectionElementRead(const EntityRead& single_component,
                                           const Filter&     selectionFilter)
    : SelectionBase((static_cast<const SDIModelViewPrivate*>(single_component.GetModelViewRead()))
                    ->GetRelationSelection(SDIModelViewPrivate::RelationSource(single_component.GetData()),
                                           ENTITY_RELATION_CONTAINMENT_DIRECT,
                                           EntityTypeIdentifier("elements"),
                                           selectionFilter)),
    p_currentEntity((SDIElementData*)P_GetReferenceToCurrentValue())
{
}


SelectionElementEdit::SelectionElementEdit(const EntityEdit& single_component,
                                           const Filter&        selectionFilter)
    : SelectionElementRead(single_component, selectionFilter)
{
}


SelectionElementRead::SelectionElementRead(const SelectionRead& components,
                                           const Filter&                 selectionFilter)
    : SelectionBase((static_cast<const SDIModelViewPrivate*>(components.GetModelViewRead()))
                    ->GetRelationSelection(SDIModelViewPrivate::RelationSource(components.GetData()),
                                           ENTITY_RELATION_CONTAINMENT_DIRECT,
                                           EntityTypeIdentifier("elements"),
                                           selectionFilter)),
    p_currentEntity((SDIElementData*)P_GetReferenceToCurrentValue())
{
}


SelectionElementEdit::SelectionElementEdit(const SelectionEdit& components,
                                           const Filter&                 selectionFilter)
    : SelectionElementRead(components, selectionFilter)
{
}
