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

#if !defined(SDIMODELVIEWPRIVATE__INCLUDED_)
#define SDIMODELVIEWPRIVATE__INCLUDED_

#include <sdiModelView.h>
#include <sdiSelection.h>
#include <sdiElement.h>


namespace sdi
{


// *********************************************************************************
// Foward defines
// *********************************************************************************
class SDISelectionData;
class SDIEntityData;


// *********************************************************************************
// SDIModelViewPrivate
// *********************************************************************************

/// ModelView interface to be implemented by the application for its DB
class SDI_DECLS SDIModelViewPrivate : public ModelViewEdit
{
public:

    ////////////////////////////////////////////////////////////////////////////////
    //! Create selection data for specified keyword
    ////////////////////////////////////////////////////////////////////////////////

    //! Create selection on the whole model
    virtual SDISelectionData* CreateSelectionData(const sdiString& keyword,
                                                  const Filter*   pselectionFilter = nullptr) const { return 0; }

    //! Create "dummy" selection data
    virtual SDISelectionData* CreateSelectionData(SpecializationType engtype) const { return 0; }


    ////////////////////////////////////////////////////////////////////////////////
    //! Component
    ////////////////////////////////////////////////////////////////////////////////
    /*
    virtual SDIComponentData* Objectify(const HandleComponentRead& handle) const { return 0; }
    */

    ////////////////////////////////////////////////////////////////////////////////
    //! Node
    ////////////////////////////////////////////////////////////////////////////////

    virtual SDINodeData* Objectify(const HandleNodeRead& handle) const { return 0; }


    ////////////////////////////////////////////////////////////////////////////////
    //! Element
    ////////////////////////////////////////////////////////////////////////////////

    virtual SDIElementData* Objectify(const HandleElementRead& handle) const { return 0; }


    ////////////////////////////////////////////////////////////////////////////////
    //! General Entities
    ////////////////////////////////////////////////////////////////////////////////

    virtual SDIEntityData* Objectify(const HandleRead& handle) const { return 0; }

    EntityEdit GetEntityEdit(SDIEntityData* pData = 0);



    ////////////////////////////////////////////////////////////////////////////////
    //! Retrieve entity's relations: Descendents and Ascendents
    //!
    //!     The functions below are used to obtain entities related to the source,
    //!     where the source case be:
    //!
    //!     * a single entity
    //!     * a selection of entities (all of same entity type)
    //!
    //!     The public class SDIRelationsSource is used to specify the specific
    //!     source.
    //!
    //!     Note how there are several types of relations - specified by the value
    //!     of entityRelationTarget. This parameter can take OR'd combinations
    //!     of the corresponding enum's values.
    //!
    //!     * It is always possible to specify the target entity type, and
    //!       for most functions, specifying entityTypeIdentifierTarget = EntityTypeIdentifier()
    //!       implies to find all entities, of all types, that satisfy entityRelationTarget.
    //!
    //!     * It is always possible to use a filter to limit the number of found, target
    //!       relations.
    //!
    ////////////////////////////////////////////////////////////////////////////////

    class RelationSource
    {
    public:

        enum SourceType
        {
            SOURCE_TYPE_ENTITY,
            SOURCE_TYPE_SELECTION
        };

        RelationSource(const SDIEntityData* pEntity)
            : p_sourceType(SOURCE_TYPE_ENTITY), p_pEntity(pEntity), p_pSelection(0) {}

        RelationSource(const SDISelectionData* pSelection)
            : p_sourceType(SOURCE_TYPE_SELECTION), p_pEntity(0), p_pSelection(pSelection) {}

        RelationSource(const RelationSource& other)
            : p_sourceType(other.p_sourceType), p_pEntity(other.p_pEntity), p_pSelection(other.p_pSelection) {}

        ~RelationSource() {}


        SourceType GetType() const
        {
            return p_sourceType;
        }
        bool IsEntity() const
        {
            return p_pEntity ? true : false;
        }
        bool IsSelection() const
        {
            return p_pSelection ? true : false;
        }

        const SDIEntityData*     GetEntity() const
        {
            return p_pEntity;
        }
        const SDISelectionData*  GetSelection() const
        {
            return p_pSelection;
        }


    private:

        RelationSource() : p_sourceType(SOURCE_TYPE_ENTITY), p_pEntity(0), p_pSelection(0) {}

        RelationSource& operator=(const RelationSource& other)
        {
            return *this;
        }

        const SourceType         p_sourceType;
        const SDIEntityData*     p_pEntity;
        const SDISelectionData*  p_pSelection;
    };


    //! Get whether source is related to any other entity.
    virtual bool GetRelationAny(const RelationSource&       relationSource,
                                const EntityRelation        entityRelationTarget,
                                const EntityTypeIdentifier& entityTypeIdentifierTarget = EntityTypeIdentifier(),
                                const Filter&               relationFilterTarget = nullFilter) const { return false; }

    //! Get number of entities related to source. Function can be used to decide
    //! whether actual retrival of related entities should utilize selections/collection or
    //! vector of handles (for case of relatively few).
    virtual unsigned int GetRelationCount(const RelationSource&       relationSource,
                                          const EntityRelation        entityRelationTarget,
                                          const EntityTypeIdentifier& entityTypeIdentifierTarget = EntityTypeIdentifier(),
                                          const Filter&               relationFilterTarget = nullFilter) const { return 0; }

    //! Get list of types of entities related to source.
    virtual void GetRelationTypes(std::set<EntityType> &      sEntityTypeTarget,
                                  const RelationSource&       relationSource,
                                  const EntityRelation        entityRelationTarget,
                                  const EntityTypeIdentifier& entityTypeIdentifierTarget = EntityTypeIdentifier(),
                                  const Filter&               relationFilterTarget = nullFilter) const { return; }


    //! Get selection of specified entityTypeIdentifierTarget that are related to source
    //! per entityRelationTarget. It is the responsibility of the caller to delete the
    //! returned selection. For the case where there are no entities satisfying the input,
    //! an empty selection is returned. Note that entityTypeIdentifierTarget cannot of
    //! general type, EntityTypeIdentifier(), as Selections must have an entity type;
    //! if seek multiple entity type, use GetRelationsCollection...(...) below.
    virtual SDISelectionData* GetRelationSelection(const RelationSource&       relationSource,
                                                   const EntityRelation        entityRelationTarget,
                                                   const EntityTypeIdentifier& entityTypeIdentifierTarget,
                                                   const Filter&               relationFilterTarget = nullFilter) const { return 0; }


protected:

    SDIModelViewPrivate() {}
    virtual ~SDIModelViewPrivate();


public:

    // Functions used by handle.
    // NB: These methods should be implemented! Only in order not to break modelviews, which implement
    // only the deprecated ones, these are called here!
    // Exceptions: Get/SetEntityHandle have base implementations which call Get/SetValue, and only
    // need to be re-implemented if required.
    virtual unsigned int P_GetId(const HandleRead& handle) const
    { return P_GetId(handle.GetType(), handle.GetPointer()); }
    virtual Status       P_SetId(const HandleEdit& handle, const unsigned int id) const
    { return P_SetId(handle.GetType(), handle.GetPointer(), id); }
    virtual sdiString    P_GetName(const HandleRead& handle) const
    { return P_GetName(handle.GetType(), handle.GetPointer()); }
    virtual Status       P_SetName(const HandleRead& handle, const sdiString& name) const
    { return P_SetName(handle.GetType(), handle.GetPointer(), name); }

    // Descriptior Values and Hierachy by identifier
    //     Return false if unable to identify.
    virtual Status GetValue(const HandleRead&    handle,
                            const sdiIdentifier& identifier,
                            sdiValue&            value) const
    { return GetValue(handle.GetType(), handle.GetPointer(), identifier, value); }

    virtual Status GetEntityHandle(const HandleRead&    handle,
                                   const sdiIdentifier& identifier,
                                   HandleEdit&          valuehandle) const;

    virtual Status SetValue(const HandleEdit&    handle,
                            const sdiIdentifier& identifier,
                            const sdiValue&      value) const
    { return SetValue(handle.GetType(), handle.GetPointer(), identifier, value); }

    virtual Status SetEntityHandle(const HandleEdit&    handle,
                                   const sdiIdentifier& identifier,
                                   const HandleRead&    valuehandle) const;

    virtual bool IsParameterized(const HandleRead&    handle,
                                 const sdiIdentifier& identifier) const
    { return IsParameterized(handle.GetType(), handle.GetPointer(), identifier); }

    virtual sdiString GetParameterName(const HandleRead&    handle,
                                       const sdiIdentifier& identifier,
                                       bool*                pIsNegated) const
    { return GetParameterName(handle.GetType(), handle.GetPointer(), identifier, pIsNegated); }

    virtual Status SetParameter(const HandleEdit&    handle,
                                const sdiIdentifier& identifier,
                                const sdiString&     parameterName,
                                bool                 isNegated) const
    { return SetParameter(handle.GetType(), handle.GetPointer(), identifier, parameterName, isNegated); }

private:
    // deprectated
    virtual unsigned int P_GetId(const EntityType type, const void* entityptr) const { return 0; }
    virtual Status       P_SetId(const EntityType type, void* entityptr, const unsigned int id) const { return false; }
    virtual sdiString     P_GetName(const EntityType type, const void* entityptr) const { return sdiString(); }
    virtual Status       P_SetName(const EntityType type, void* entityptr, const sdiString& name) const { return false; }


    virtual Status GetValue(const EntityType                type,
                            const void*                     entityptr,
                            const sdiIdentifier& identifier,
                            sdiValue&            value) const { return false; }

    virtual Status GetEntityHandle(const EntityType                type,
                                   const void*                     entityptr,
                                   const sdiIdentifier& identifier,
                                   HandleEdit&                     handle) const;

    virtual Status SetValue(const EntityType                type,
                            void*                           entityptr,
                            const sdiIdentifier& identifier,
                            const sdiValue&      value) const { return false; }

    virtual Status SetEntityHandle(const EntityType                type,
                                   void*                           entityptr,
                                   const sdiIdentifier& identifier,
                                   const HandleRead&               handle) const;

    virtual bool IsParameterized(const EntityType                type,
                                 const void*                     entityptr,
                                 const sdiIdentifier& identifier) const { return false; }

    virtual sdiString GetParameterName(const EntityType                type,
                                      const void*                     entityptr,
                                      const sdiIdentifier& identifier,
                                      bool*                           pIsNegated) const { return sdiString(); }

    virtual Status SetParameter(const EntityType                type,
                              const void*                     entityptr,
                              const sdiIdentifier& identifier,
                              const sdiString&                 parameterName,
                              bool                            isNegated) const { return false; }

public:
    ////////////////////////////////////////////////////////////////////////////////
    //! Base implementations for selected methods of ModelViewRead/Edit
    //! These are not optimized, it will probably make sense to reimplement in
    //! derived classes
    ////////////////////////////////////////////////////////////////////////////////

    virtual bool IsIdAvailable(EntityType entities, unsigned int id) const;
    virtual unsigned int GetNextAvailableId(EntityType entities) const;

};

}

#endif //! !defined(SDIMODELVIEWPRIVATE__INCLUDED_)
