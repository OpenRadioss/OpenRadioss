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

#if !defined(SDIELEMENT__INCLUDED_)
#define SDIELEMENT__INCLUDED_

#include <sdiDefs.h>
#include <sdiEntity.h>
#include <sdiHandles.h>
#include <sdiNode.h>
#include <set>


namespace sdi
{


#pragma warning(disable:4250)


// *********************************************************************************
// Foward defines
// *********************************************************************************
class ModelViewRead;
class ModelViewEdit;
class SDIModelViewPrivate;
class ElementEdit;
class SDIElementData;


// *********************************************************************************
// ElementRead
// *********************************************************************************
class SDI_DECLS ElementRead : public EntityBaseRead
{
public:

    //! Construct object to provide read-only access to a permanent DB entity.
    //! Note that forcing both the model view and the handle of "edit" type are
    //! derived from those of "read" type, can use "edit" type arguments.
    ElementRead(const ModelViewRead*     pModelView,
                const HandleElementRead& handle);

    ElementRead(const ModelViewRead*     pModelView,
                const HandleRead& handle);  //! must be of element type to construct valid object

    //! Copy constructors; note that can create a "read" entity object from an "edit".
    ElementRead(const ElementRead& other);

    //! Default destructor is exposed
    virtual ~ElementRead();

    //! = operator allows for assignment of "read" entity object from an "edit" object.
    ElementRead& operator=(const ElementRead& other);

    //! Correspoding handle
    HandleElementRead GetHandle() const;


    ////////////////////////////////////////////////////////////////////////////////
    //! Access to the Element-specific data members
    ////////////////////////////////////////////////////////////////////////////////

    //!!
    unsigned int        GetNodeCount() const;

    //!!
    void                GetNodeHandles(sdiVector<HandleNodeRead> &ahNode) const;
    void                GetNodeIds(sdiVector<unsigned int> &aNodeId) const;

    // deprecated:
    HandleRead          GetComponent() const { return GetOwner(); }
    unsigned int        GetComponentId() const { return GetOwnerId(); }
    // Rename to "Owner":
    HandleRead          GetOwner() const;
    unsigned int        GetOwnerId() const;

protected:

    //! Hold data for all "read", "edit", and "temp" entity objects.
    SDIElementData* p_pData;

    //! Functions used by selections to create references to entities
    ElementRead(SDIElementData* pData = 0) : EntityBaseRead(), p_pData(pData) {}


public:

    //! Access to private data; to be only used by internal infrastructure
    SDIEntityData*       GetData()
    {
        return (SDIEntityData*) p_pData;
    }
    const SDIEntityData* GetData() const
    {
        return (const SDIEntityData*) p_pData;
    }
};



// *********************************************************************************
// ElementEdit
// *********************************************************************************
class SDI_DECLS ElementEdit : public ElementRead, public EntityBaseEdit
{
public:

    //! Construct object to provide edit access to a permanent DB entity.
    //! Note that forcing both the model view and the handle arguments to be of
    //! "edit" type.
    ElementEdit(const ModelViewEdit*     pModelView,
                const HandleElementEdit& handle);

    ElementEdit(const ModelViewEdit*     pModelView,
                const HandleEdit& handle);  //! must be of element type to construct valid object


    //! Copy constructors; note that cannot create an "edit" entity object from a "read".
    ElementEdit(const ElementEdit& other);

    //! Default destructor is exposed
    virtual ~ElementEdit() {};

    //! = operator only allows for assignment of "edit" entity object into another.
    ElementEdit& operator=(const ElementEdit& other);

    //! Correspoding handle
    HandleElementEdit GetHandle() const;


    ////////////////////////////////////////////////////////////////////////////////
    //! Access to the Element-specific data members
    ////////////////////////////////////////////////////////////////////////////////

    Status   ReplaceNode(const HandleNodeRead& hnode, unsigned int index) const;
    Status   SetNodes(const sdiVector<HandleNodeRead> &ahNode) const;
    Status   SetNodes(const sdiVector<HandleNodeEdit> &ahNode) const;
    Status   SetOwner(const HandleRead& hOwner) const;


protected:

    friend class SelectionElementRead;

    //! Functions used by selections to create references to entities
    ElementEdit(SDIElementData* pData = 0) : ElementRead(pData), EntityBaseEdit() {}


public:

    //! Access to private data; to be only used by internal infrastructure
    SDIEntityData*       GetData()
    {
        return (SDIEntityData*) p_pData;
    }
    const SDIEntityData* GetData() const
    {
        return (const SDIEntityData*) p_pData;
    }
};



// *********************************************************************************
// Related Types Guides
// *********************************************************************************

class SelectionElementRead;
class SelectionElementEdit;

template <>
struct SDITypeGuide<SPECIALIZATION_TYPE_ELEMENT>
{
    typedef HandleElementRead      handleread;
    typedef HandleElementEdit      handleedit;
    typedef ElementRead            entityread;
    typedef ElementEdit            entityedit;
    typedef SDIElementData         data;
    typedef SelectionElementRead   selectionread;
    typedef SelectionElementEdit   selectionedit;
};



// *********************************************************************************
// Selections
// *********************************************************************************

class SDI_DECLS SelectionElementRead : public SelectionBase
{
    COMMON_SDISELECTIONREAD_MEMBERS(SPECIALIZATION_TYPE_ELEMENT, SelectionElementRead)

public:
    explicit SelectionElementRead(const EntityRead&    single_component,
                                  const Filter&        selectionFilter = nullFilter);
    explicit SelectionElementRead(const SelectionRead& some_components,
                                  const Filter&        selectionFilter = nullFilter);
};


class SDI_DECLS SelectionElementEdit : public SelectionElementRead
{
    COMMON_SDISELECTIONEDIT_MEMBERS(SPECIALIZATION_TYPE_ELEMENT, SelectionElementEdit)

public:
    explicit SelectionElementEdit(const EntityEdit&    single_component,
                                  const Filter&        selectionFilter = nullFilter);
    explicit SelectionElementEdit(const SelectionEdit& some_components,
                                  const Filter&        selectionFilter = nullFilter);
};


COMMON_SDISELECTION_BODIES(SPECIALIZATION_TYPE_ELEMENT)


}

#endif //! !defined(SDIELEMENT__INCLUDED_)
