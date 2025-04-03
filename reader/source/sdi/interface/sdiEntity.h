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

#if !defined(SDIENTITY__INCLUDED_)
#define SDIENTITY__INCLUDED_

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
class EntityEdit;
class SDIEntityData;




// *********************************************************************************
// EntityRead
// *********************************************************************************
class SDI_DECLS EntityRead : public EntityBaseRead
{
public:

    //! Construct invalid entity view
    EntityRead() : EntityBaseRead(), p_pData(nullptr) {}

    //! Construct object to provide read-only access to a permanent DB entity.
    //! Note that forcing both the model view and the handle of "edit" type are
    //! derived from those of "read" type, can use "edit" type arguments.
    EntityRead(const ModelViewRead*     pModelView,
               const HandleRead& handle);

    //! Copy constructors; note that can create a "read" entity object from an "edit".
    EntityRead(const EntityRead& other);

    //! Default destructor is exposed
    virtual ~EntityRead();

    //! = operator allows for assignment of "read" entity object from an "edit" object.
    EntityRead& operator=(const EntityRead& other);

    //! Correspoding handle
    HandleRead GetHandle() const;

    //! Retrieve its type
    EntityType GetType() const;
    const sdiString& GetKeyword() const;

    bool IsValid() const { return p_pData ? true : false; }

protected:

    //! Hold data for all "read", "edit", and "temp" entity objects.
    SDIEntityData* p_pData;

    //! Functions used by friend classes to create references to entities
    EntityRead(SDIEntityData* pData) : EntityBaseRead(), p_pData(pData) {}


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
// EntityEdit
// *********************************************************************************
class SDI_DECLS EntityEdit :  public EntityRead, public EntityBaseEdit
{
public:

    //! Construct invalid entity view
    EntityEdit() : EntityRead(), EntityBaseEdit() {}

    //! Construct object to provide edit access to a permanent DB entity.
    //! Note that forcing both the model view and the handle arguments to be of
    //! "edit" type.
    EntityEdit(const ModelViewEdit*     pModelView,
               const HandleEdit& handle);

    //! Copy constructor; note that cannot create an "edit" entity object from a "read".
    EntityEdit(const EntityEdit& other);

    //! Move constructor
    EntityEdit(EntityEdit&& other);

    //! Default destructor is exposed
    virtual ~EntityEdit() {};

    //! = operator only allows for assignment of "edit" entity object into another.
    EntityEdit& operator=(const EntityEdit& other);
    EntityEdit& operator=(EntityEdit&& other);

    //! Correspoding handle
    HandleEdit GetHandle() const;


protected:

    friend class SelectionRead;
    friend class SDIModelViewPrivate;

    //! Functions used by friend classes to create references to entities
    EntityEdit(SDIEntityData* pData) : EntityRead(pData), EntityBaseEdit() {}


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

class SelectionRead;
class SelectionEdit;

template <>
struct SDITypeGuide<SPECIALIZATION_TYPE_GENERAL>
{
    typedef HandleRead             handleread;
    typedef HandleEdit             handleedit;
    typedef EntityRead             entityread;
    typedef EntityEdit             entityedit;
    typedef SDIEntityData          data;
    typedef SelectionRead          selectionread;
    typedef SelectionEdit          selectionedit;
};


// *********************************************************************************
// Selections
// *********************************************************************************

class SDI_DECLS SelectionRead : public SelectionBase
{
    COMMON_SDISELECTIONREAD_MEMBERS(SPECIALIZATION_TYPE_GENERAL, SelectionRead)
};


class SDI_DECLS SelectionEdit : public SelectionRead
{
    COMMON_SDISELECTIONEDIT_MEMBERS(SPECIALIZATION_TYPE_GENERAL, SelectionEdit)
};


COMMON_SDISELECTION_BODIES(SPECIALIZATION_TYPE_GENERAL)

}

#endif //! !defined(SDIENTITY__INCLUDED_)
