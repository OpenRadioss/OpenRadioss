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

#if !defined(SDINODEDATA__INCLUDED_)
#define SDINODEDATA__INCLUDED_

#include <sdiDefs.h>
#include <_private/sdiEntityData.h>
#include <_private/sdiModelViewPrivate.h>

namespace sdi
{



// *********************************************************************************
// Foward defines
// *********************************************************************************




// *********************************************************************************
// SDINodeData
//
//
// *********************************************************************************
class SDI_DECLS SDINodeData : public SDIEntityData
{
public:

    //! Retrieve its type
    SpecializationType GetSpecializationType() const
    {
        return SPECIALIZATION_TYPE_NODE;
    }


    //! Creates clone of data
    virtual SDINodeData* Clone() const
    {
        return GetModelView()->Objectify(
            HandleNodeRead(GetHandle()));
    }


    ////////////////////////////////////////////////////////////////////////////////
    //! Access to specific entity's data
    ////////////////////////////////////////////////////////////////////////////////

    virtual sdiTriple GetPosition() const { return sdiTriple(); }
    virtual Status SetPosition(const sdiTriple& position)const { return false; }

protected:

    //! The underlying application constructs this entity as one associated
    //! with a permanent database entry, or associated to a temporary record.
    explicit SDINodeData(SDIModelViewPrivate* pModelView) : SDIEntityData(pModelView) {}

    virtual ~SDINodeData() {}
};

}


#endif //! !defined(SDINODEDATA__INCLUDED_)
