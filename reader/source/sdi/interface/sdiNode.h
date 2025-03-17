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

#if !defined(SDINODE__INCLUDED_)
#define SDINODE__INCLUDED_

#include <sdiDefs.h>
#include <sdiEntityBase.h>
#include <sdiHandles.h>
#include <sdiSelection.h>


namespace sdi
{


// *********************************************************************************
// Foward defines
// *********************************************************************************
class ModelViewRead;
class ModelViewEdit;
class SDIModelViewPrivate;
class NodeEdit;
class SDINodeData;



// *********************************************************************************
// NodeRead
// *********************************************************************************
class SDI_DECLS NodeRead : public EntityBaseRead
{
public:

    //! Construct object to provide read-only access to a permanent DB entity.
    //! Note that forcing both the model view and the handle of "edit" type are
    //! derived from those of "read" type, can use "edit" type arguments.
    NodeRead(const ModelViewRead*  pModelView,
             const HandleNodeRead& handle);

    NodeRead(const ModelViewRead*     pModelView,
             const HandleRead& handle);  //! must be of node type to construct valid object


    //! Copy constructors; note that can create a "read" entity object from an "edit".
    NodeRead(const NodeRead& other);

    //! Default destructor is exposed
    virtual ~NodeRead();

    //! = operator allows for assignment of "read" entity object from an "edit" object.
    NodeRead& operator=(const NodeRead& other);

    //! Correspoding handle
    HandleNodeRead GetHandle() const;


    ////////////////////////////////////////////////////////////////////////////////
    //! Read only member functions
    ////////////////////////////////////////////////////////////////////////////////

    sdiTriple     GetPosition() const;


protected:

    //! Hold data for all "read", "edit", and "temp" entity objects.
    SDINodeData* p_pData;

    //! Functions used by selections to create references to entities
    NodeRead(SDINodeData* pData = 0) : EntityBaseRead(), p_pData(pData) {}


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
// NodeEdit
// *********************************************************************************
class SDI_DECLS NodeEdit :  public NodeRead, public EntityBaseEdit
{
public:

    //! Construct object to provide edit access to a permanent DB entity.
    //! Note that forcing both the model view and the handle arguments to be of
    //! "edit" type.
    NodeEdit(const ModelViewEdit*  pModelView,
             const HandleNodeEdit& handle);

    NodeEdit(const ModelViewEdit*     pModelView,
             const HandleEdit& handle);  //! must be of node type to construct valid object

    //! Copy constructors; note that cannot create an "edit" entity object from a "read".
    NodeEdit(const NodeEdit& other);

    //! Default destructor is exposed
    virtual ~NodeEdit() {};

    //! = operator only allows for assignment of "edit" entity object into another.
    NodeEdit& operator=(const NodeEdit& other);

    //! Correspoding handle
    HandleNodeEdit GetHandle() const;


    ////////////////////////////////////////////////////////////////////////////////
    //! Edit member functions
    ////////////////////////////////////////////////////////////////////////////////

    Status SetPosition(const sdiTriple& position) const;

protected:

    friend class SelectionNodeRead;

    //! Functions used by selections to create references to entities
    NodeEdit(SDINodeData* pData = 0) : NodeRead(pData), EntityBaseEdit() {}


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

class SelectionNodeRead;
class SelectionNodeEdit;

template <>
struct SDITypeGuide<SPECIALIZATION_TYPE_NODE>
{
    typedef HandleNodeRead      handleread;
    typedef HandleNodeEdit      handleedit;
    typedef NodeRead            entityread;
    typedef NodeEdit            entityedit;
    typedef SDINodeData         data;
    typedef SelectionNodeRead   selectionread;
    typedef SelectionNodeEdit   selectionedit;
};


// *********************************************************************************
// Selections
// *********************************************************************************

class ElementRead;
class ElementEdit;
class SelectionElementRead;
class SelectionElementEdit;


class SDI_DECLS SelectionNodeRead : public SelectionBase
{
    COMMON_SDISELECTIONREAD_MEMBERS(SPECIALIZATION_TYPE_NODE, SelectionNodeRead)
};


class SDI_DECLS SelectionNodeEdit : public SelectionNodeRead
{
    COMMON_SDISELECTIONEDIT_MEMBERS(SPECIALIZATION_TYPE_NODE, SelectionNodeEdit)
};


COMMON_SDISELECTION_BODIES(SPECIALIZATION_TYPE_NODE)


}

#endif //! !defined(SDINODE__INCLUDED_)
