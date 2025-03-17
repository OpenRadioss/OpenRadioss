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

#if !defined(SDIELEMENTDATA__INCLUDED_)
#define SDIELEMENTDATA__INCLUDED_

#include <sdiDefs.h>
#include <sdiElement.h>
#include <_private/sdiEntityData.h>
#include <_private/sdiModelViewPrivate.h>



namespace sdi
{


// *********************************************************************************
// Foward defines
// *********************************************************************************




// *********************************************************************************
// SDIElementData
//
//
// *********************************************************************************
class SDI_DECLS SDIElementData : public SDIEntityData
{
public:

    //! Retrieve its type
    virtual SpecializationType GetSpecializationType() const
    {
        return SPECIALIZATION_TYPE_ELEMENT;
    }


    //! Creates clone of data
    virtual SDIElementData* Clone() const
    {
        return GetModelView()->Objectify(
            HandleElementRead(GetHandle()));
    }

    ////////////////////////////////////////////////////////////////////////////////
    //! Access to specific entity's data
    ////////////////////////////////////////////////////////////////////////////////
    virtual unsigned int        GetNodeCount() const { return 0; }

    // NB: One of the following pairs has to be implemented, otherwise infinite recursive call!
    // deprecated:
    virtual HandleRead          GetComponent() const { return GetOwner(); }
    virtual unsigned int        GetComponentId() const { return GetOwnerId(); }
    // Rename to "Owner":
    virtual HandleRead          GetOwner() const { return GetComponent(); }
    virtual unsigned int        GetOwnerId() const { return GetComponentId(); }

    virtual void   GetNodeHandles(sdiVector<HandleNodeRead> &ahNode) const { return; }
    virtual void   GetNodeIds(sdiVector<unsigned int> &aNodeId) const { return; }

    virtual Status ReplaceNode(const HandleNodeRead hnode, unsigned int index) const { return false; }

    virtual Status SetNodes(const sdiVector<HandleNodeRead> &ahNode) const { return false; }
    virtual Status SetNodes(const sdiVector<HandleNodeEdit> &ahNode) const { return false; }
    virtual Status SetOwner(const HandleRead& hOwner) const { return false; }

protected:

    //! The underlying application constructs this entity as one associated
    //! with a permanent database entry, or associated to a temporary record.
    explicit SDIElementData(SDIModelViewPrivate* pModelView)
        : SDIEntityData(pModelView) {}

    virtual ~SDIElementData() {}

private:

    SDIElementData() : SDIEntityData(0) {}

};

}


#endif //! !defined(SDIELEMENTDATA__INCLUDED_)
