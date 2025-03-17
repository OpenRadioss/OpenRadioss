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

#include <_private/sdiSelectionData.h>
#include <_private/sdiModelViewPrivate.h>
#include <_private/sdiEntityData.h>


using namespace sdi;



// *********************************************************************************
//   SelectionBase  (functionality of the abstract common base class)
// *********************************************************************************

// Navigation. These are not const, because they change the "current" pointer
bool SelectionBase::Next()                 //! returns false if no more entities can be selected
{
    assert(p_pSelectionData);
    p_pSelectionData->Next();
    SDIEntityData* data = P_GetReferenceToCurrentValue();
    return data->IsValid();
}


void SelectionBase::Restart()            //! returns to just prior to first selectable entity
{
    assert(p_pSelectionData);
    p_pSelectionData->Restart();
}



//
// Function referring to what is accessible by selection
//

unsigned int SelectionBase::Count() const
{
    assert(p_pSelectionData);
    return p_pSelectionData->Count();
}


bool         SelectionBase::IsEmpty() const
{
    assert(p_pSelectionData);
    return p_pSelectionData->IsEmpty();
}



// All construction/destructor access is through entity specific selections

//
// Create selection to entities in model view
//

SelectionBase::SelectionBase(const sdiString&      keyword,
                             const ModelViewRead* pModelView,
                             const Filter&        selectionFilter)
{
    const SDIModelViewPrivate* pModelViewPrivate = static_cast< const SDIModelViewPrivate* >(pModelView);
    const Filter* pfilter = (&selectionFilter == &nullFilter ? 0 : &selectionFilter);

    p_pSelectionData = pModelViewPrivate->CreateSelectionData(keyword, pfilter);
}

SelectionBase::SelectionBase(SpecializationType      engtype,
                             const ModelViewRead* pModelView)
{
    const SDIModelViewPrivate* pModelViewPrivate = static_cast< const SDIModelViewPrivate* >(pModelView);

    p_pSelectionData = pModelViewPrivate->CreateSelectionData(engtype);
}



// Create from another for copy constructors
SelectionBase::SelectionBase(const SDISelectionData* pSelectionData)
{
    //! shallow copy because membership does not change after construction
    p_pSelectionData = pSelectionData->Clone(false);
}


// Create from supplied data, taking ownership of that pointer
SelectionBase::SelectionBase(SDISelectionData* pSelectionData) :
        p_pSelectionData(pSelectionData)
{
    assert(pSelectionData != 0);
}

SelectionBase::~SelectionBase()
{
    assert(p_pSelectionData);
    delete p_pSelectionData;
}


void SelectionBase::P_CopyFrom(const SelectionBase& other)
{
    assert(p_pSelectionData);
    assert(other.p_pSelectionData);
    delete p_pSelectionData;
    p_pSelectionData = other.p_pSelectionData->Clone(false);   //! shallow copy because membership does not change after construction
}


// Retrieve entity data pointer used maintain reference of current value
SDIEntityData* SelectionBase::P_GetReferenceToCurrentValue() const
{
    assert(p_pSelectionData);
    return p_pSelectionData->GetReferenceToCurrentValue();
}


// Function referring to what is accessible by selection
bool SelectionBase::P_IsSelected(const void* pHandle) const
{
    assert(p_pSelectionData);
    return p_pSelectionData->IsSelected(pHandle);
}
