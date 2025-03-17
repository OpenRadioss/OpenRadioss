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


// Solver Data Interface (SDI) overview:
// ------------------------------------------------------
//

#if !defined(SDIMODELVIEW__INCLUDED_)
#define SDIMODELVIEW__INCLUDED_

#include <sdiDefs.h>
#include <sdiHandles.h>
#include <sdiEntity.h>
#include <sdiElement.h>


namespace sdi
{


// *********************************************************************************
// ModelViewRead
//
///     Class provides "read" only access functionality of underlying DB - limited
///     to content of model view:
///         1. Its pointer is used in the creation of "read" type:
///                * EntityRead
///                * SelectionRead
///         2. Find "read" type handles by id and name, where applicable.
///         3. Obtain additional data from underlying DB:
///                * Keyword vs. EntityType
//
// *********************************************************************************
class SDI_DECLS ModelViewRead
{
public:

    /// Get the numerical (internal) type of a given solver keyword. DB is free to choose whatever
    /// EntityTypes for the solver keywords. These remain valid in the session and can be used as
    /// input for other functions.
    virtual EntityType GetEntityType(const sdiString& keyword) const { return ENTITY_TYPE_NONE; }
    /// Get the solver keyword from a numerical (internal) type
    virtual const sdiString & GetKeyword(EntityType type) const { return EMPTYSTRING; }
    /// Get the specialization type
    virtual SpecializationType GetSpecializationType(EntityType type) const { return SPECIALIZATION_TYPE_GENERAL; }
    /// Get the maximum entity type. May be used to iterate through all entity types of the model.
    /// NB: Not all entity types between 0 and max have to be valid!
    virtual EntityType GetMaxEntityType() const { return ENTITY_TYPE_NONE; }

    /// Find entity handle by id. Returns true if found.
    bool FindById(EntityType entities, const unsigned int id, HandleRead& handle) const
    {
        bool found = P_FindById(id, entities, handle);
        if(found) handle = HandleRead(entities, handle.GetIndex1(), handle.GetIndex2());
        else      handle = HandleRead(entities);
        return found;
    }

    /// Find entity handle by id. Returns true if found.
    bool FindById(const sdiString& keyword, const unsigned int id, HandleRead& handle) const
    {
        bool found = P_FindById(id, keyword, handle);
        EntityType entities = GetEntityType(keyword);
        if(found) handle = HandleRead(entities, handle.GetIndex1(), handle.GetIndex2());
        else      handle = HandleRead(entities);
        return found;
    }

    /// Find entity handle by name (if entity is named). Returns true if found.
    bool FindByName(EntityType entities, const sdiString& name,  HandleRead& handle, bool ignoreCase=false) const
    {
        bool found = P_FindByName(name, entities, handle, ignoreCase);
        if(found) handle = HandleRead(entities, handle.GetIndex1(), handle.GetIndex2());
        else      handle = HandleRead(entities);
        return found;
    }

    virtual bool IsUnresolvedId(const EntityType entity, const unsigned int id) const { return false; }


public:

    virtual ~ModelViewRead() {}


protected:

    // Constructed and destroyed by application
    ModelViewRead() {}

    // Base implementations construct selection and loop through it
    virtual bool P_FindById(const unsigned int id, EntityType type, HandleRead& handle) const
    { return P_FindById(id, GetKeyword(type), handle); }
    virtual bool P_FindById(const unsigned int id, const sdiString& keyword, HandleRead& handle) const;
    virtual bool P_FindByName(const sdiString& name, EntityType type, HandleRead& handle, bool ignoreCase=false) const;

}; // ModelViewRead class



// *********************************************************************************
// ModelViewEdit
//
///     Class provides "edit" access functionality of underlying DB - limited
///     to content of model view:
///         1. Its pointer is used in the creation of "edit" type:
///                * EntityEdit
///                * SelectionEdit
///         2. Find "edit" type handles by id and name, where applicable.
///         3. Delete from DB entity associated with "edit" handle.
//
// *********************************************************************************
class SDI_DECLS ModelViewEdit : public ModelViewRead
{
public:

    /// Set the current collector. The current collector is the "parent", in which new
    /// entities get created.
    /// Returns a Handle to the old current.
    /// If the supplied Handle is invalid, it will change that current collector to NULL.
    virtual HandleRead SetCurrentCollector(const HandleRead handle) const { return HandleRead(); }


    ////////////////////////////////////////////////////////////////////////////////
    //! Find entity handle by either id or name (if entity is named).
    //!     Functions return true if found in model view.
    ////////////////////////////////////////////////////////////////////////////////

    // note, these two have to be duplicated here, or else its awkward to get a
    // HandleRead from a ModelViewEdit.

    bool FindById(EntityType entities, const unsigned int id, HandleEdit& handle) const
    {
        bool found = P_FindById(id, entities, handle);
        if(found) handle = HandleEdit(entities, handle.GetIndex1(), handle.GetIndex2());
        else      handle = HandleEdit(entities);
        return found;
    }

    bool FindById(const sdiString& keyword, const unsigned int id, HandleEdit& handle) const
    {
        bool found = P_FindById(id, keyword, handle);
        EntityType entities = GetEntityType(keyword);
        if(found) handle = HandleEdit(entities, handle.GetIndex1(), handle.GetIndex2());
        else      handle = HandleEdit(entities);
        return found;
    }


    bool FindByName(EntityType entities, const sdiString& name,  HandleEdit& handle, bool ignoreCase=false) const
    {
        bool found = P_FindByName(name, entities, handle, ignoreCase);
        if(found) handle = HandleEdit(entities, handle.GetIndex1(), handle.GetIndex2());
        else      handle = HandleEdit(entities);
        return found;
    }

    bool FindById(EntityType entities, const unsigned int id, HandleRead& handle) const
    {
        bool found = P_FindById(id, entities, handle);
        if(found) handle = HandleRead(entities, handle.GetIndex1(), handle.GetIndex2());
        else      handle = HandleRead(entities);
        return found;
    }

    bool FindById(const sdiString& keyword, const unsigned int id, HandleRead& handle) const
    {
        bool found = P_FindById(id, keyword, handle);
        EntityType entities = GetEntityType(keyword);
        if(found) handle = HandleRead(entities, handle.GetIndex1(), handle.GetIndex2());
        else      handle = HandleRead(entities);
        return found;
    }

    bool FindByName(EntityType entities, const sdiString& name,  HandleRead& handle, bool ignoreCase=false) const
    {
        bool found = P_FindByName(name, entities, handle, ignoreCase);
        if(found) handle = HandleRead(entities, handle.GetIndex1(), handle.GetIndex2());
        else      handle = HandleRead(entities);
        return found;
    }


    ////////////////////////////////////////////////////////////////////////////////
    //! Create in DB entity
    //!     * Functions return handle of new entity, it will be invalid, if we
    //!       failed to create the new entity.  Perhaps we should throw an error.
    ////////////////////////////////////////////////////////////////////////////////

    /// Creation of a new entity. Better use the version with EntityEdit,
    /// if entity needs to be accessed after creation, e.g. to set values.
    virtual Status CreateEntity(HandleEdit&               handle,
                                const sdiString&          keyword,
                                const sdiString&          name = "",
                                const unsigned int        id = 0);

    /// Creation of a new entity. This version should be used if entity needs to be
    /// accessed after creation, e.g. to set values.
    virtual Status CreateEntity(EntityEdit&               entity,
                                const sdiString&          keyword,
                                const sdiString&          name = "",
                                const unsigned int        id = 0);

    /// Creation of a new entity as copy of an existing one
    virtual Status CreateEntity(HandleEdit&               handle,
                                const HandleRead&         source,
                                const sdiString&          name = "",
                                const unsigned int        id = 0) { return false; }

    /// Element creation
    
    virtual Status CreateElement(HandleElementEdit&       handle,
                                 const sdiString&         keyword,
                                 const unsigned int       id = 0) { return false; }
    virtual Status CreateElement(HandleElementEdit&       handle,
                                 const sdiString&         keyword,
                                 const sdiVector<HandleNodeRead>& ahNode,
                                 const HandleRead&        owner,
                                 const unsigned int       id = 0) { return false; }
    virtual Status CreateElement(HandleElementEdit&       handle,
                                 const sdiString&         keyword,
                                 const sdiVector<unsigned int>& aNodeId,
                                 const HandleRead&        owner,
                                 const unsigned int       id = 0) { return false; }

    /// Node creation
    virtual Status CreateNode   (HandleNodeEdit&          handle,
                                 const sdiString&         keyword,
                                 const sdiTriple&         position,
                                 const unsigned int       id = 0) { return false; }

    virtual bool IsIdAvailable(EntityType entities, unsigned int id) const { return false; }
    virtual unsigned int GetNextAvailableId(EntityType entities) const { return 0; }

    virtual void Delete(const EntityBaseEdit& entity) { return; }
    virtual void Delete(const HandleEdit& handle) { return; }

    virtual ~ModelViewEdit() {}

protected:

    // Constructed and destroyed by application
    ModelViewEdit() {}
}; // ModelViewEdit class


}

#endif //! !defined(SDIMODELVIEW__INCLUDED_)
