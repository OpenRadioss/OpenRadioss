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

#if !defined(SDISELECTIONDATA__INCLUDED_)
#define SDISELECTIONDATA__INCLUDED_

#include <sdiDefs.h>
#include <sdiHandles.h>
#include <sdiModelView.h>
#include <sdiSelection.h>


namespace sdi
{


// *********************************************************************************
// Foward defines
// *********************************************************************************
class SDIModelViewPrivate;



// *********************************************************************************
// SDISelectionData
//
// Used to selection data lists
//
// *********************************************************************************

/// Selection interface to be implemented by the application for its DB
class SDI_DECLS SDISelectionData
{
public:

    const ModelViewRead* GetModelViewRead() const
    {
        return p_pModelView;
    }


    //! Type of entity being selected
    SpecializationType GetSpecializationType() const
    {
        return p_specializationType;
    }


    //! Retrieve entity data pointer used maintain reference of current value
    virtual SDIEntityData* GetReferenceToCurrentValue() const { return 0; }


    //! Advances to next entity, returning false if no such selectable entity
    virtual bool Next() { return false; }


    //! Returns to just prior to first selectable entity
    virtual void Restart() { return; }


    //! Can create (clone) another selection data using the same construction criteria, with ability to
    //! specify whether underlying implementation should do a deep or shallow copy.
    virtual SDISelectionData* Clone(const bool deepCopy) const { return 0; }


    //! Destroy function is only means to delete ???
    virtual ~SDISelectionData() {}


    //!
    //! Function referring to what is accessible by selection data
    //!

    //! Count of number of possible entities accessible by selection data.
    // The default implementation counts Next(), but violates constness.
    virtual unsigned int Count() const
    {
        const_cast<SDISelectionData*>(this)->Restart();
        unsigned int count = 0;
        while(const_cast<SDISelectionData*>(this)->Next()) ++count;
        const_cast<SDISelectionData*>(this)->Restart();
        return count;
    }

    //! Are any entities accessible to selection data
    virtual bool IsEmpty() const { return Count() == 0; }

    //! Returns true if entity's pointer is accessible to selection data.
    //! This is the pointer encapsulated by a handle.
    virtual bool IsSelected(const void* ptr) const { return false; }


protected:

    //! Create selections:
    SDISelectionData(SpecializationType specializationType,
                     const ModelViewRead* p_pModelView)
        : p_specializationType(specializationType), p_pModelView(p_pModelView) {}


    SpecializationType p_specializationType;
    const ModelViewRead*  p_pModelView;


private:

    //! Hidden copy constructor and =operator
    SDISelectionData(const SDISelectionData& other);
    SDISelectionData& operator=(const SDISelectionData& other);
};

}

#endif //! !defined(SDISELECTIONDATA__INCLUDED_)
