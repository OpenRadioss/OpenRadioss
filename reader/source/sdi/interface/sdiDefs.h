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

#if !defined(SDIDEFS__INCLUDED_)
#define SDIDEFS__INCLUDED_

#include <assert.h>


// *************************************************************************************
// include definitions of sdi utility classes
// *************************************************************************************
#include <sdi/utils/sdiUtilsDefs.h>
#include <sdi/utils/sdiValue.h>


// *************************************************************************************
// Windows export macro.
// *************************************************************************************
#if ( defined(OS_WIN) ) && (!defined(NO_DECLS) ) 
#ifdef SDI_EXPORT
#undef SDI_DECLS
#define SDI_DECLS __declspec(dllexport)
#else
#undef SDI_DECLS
#define SDI_DECLS __declspec(dllimport)
#endif  //! SDI_EXPORT
#else
#undef SDI_DECLS
#define SDI_DECLS
#endif //! OS_WIN


namespace sdi
{

// Groups of entity types with specialized behavior
enum SpecializationType
{
    SPECIALIZATION_TYPE_GENERAL,
    SPECIALIZATION_TYPE_NODE,
    SPECIALIZATION_TYPE_ELEMENT,
    SPECIALIZATION_TYPE_MAX
};

typedef unsigned int EntityType;
constexpr EntityType ENTITY_TYPE_NONE = 0;

/* We had explored the possibility to make return values more than a bool, like for example
* containing a message in case of an error return. However, in VS, if not used, this leads
* to the error:
* C26444 Avoid unnamed objects with custom construction and destruction
Therefore, at least for the time being, we replace by a bool.
class Status
{
public:
    Status() : p_status(1), p_message("") {}
    Status(bool flag) : p_status(flag ? 0 : 1), p_message("") {}
    Status(int status, const sdiString &message) : p_status(status), p_message(message) {}
    ~Status() {}

    // Not perfect style as per literature, but it allows to assign implicitely to a bool.
    operator bool() const { return p_status == 0; }

    int p_status;
    sdiString p_message;
};
*/
typedef bool Status;


// Used to specify the types of relations between entities.
enum EntityRelation
{
    //! Descendents
    ENTITY_RELATION_CONTAINMENT_DIRECT      = 1,       //! direct containment, such as elems by comps, or nodes by sets.
    ENTITY_RELATION_CONTAINMENT_EXTENDED    = 1 << 1,  //! only applicable to comps containing nodes, and recursive content of sets and assemblies
    ENTITY_RELATION_REFERENCES_DIRECT       = 1 << 2,
    ENTITY_RELATION_DESCENDENTS_DIRECT      = ENTITY_RELATION_CONTAINMENT_DIRECT | ENTITY_RELATION_REFERENCES_DIRECT,
    ENTITY_RELATION_DESCENDENTS_EXTENDED    = ENTITY_RELATION_CONTAINMENT_EXTENDED | ENTITY_RELATION_REFERENCES_DIRECT,

    //! Ascendents
    ENTITY_RELATION_CONTAINED_BY_DIRECT     = 1 << 10,
    ENTITY_RELATION_CONTAINED_BY_EXTENDED   = 1 << 11,
    ENTITY_RELATION_REFERENCED_BY_DIRECT    = 1 << 12,
    ENTITY_RELATION_ASCENDENTS_DIRECT       = ENTITY_RELATION_CONTAINED_BY_DIRECT | ENTITY_RELATION_REFERENCED_BY_DIRECT,
    ENTITY_RELATION_ASCENDENTS_EXTENDED     = ENTITY_RELATION_CONTAINED_BY_EXTENDED | ENTITY_RELATION_REFERENCED_BY_DIRECT,

    //! Descendents and Ascendents
    ENTITY_RELATION_ALL_DIRECT              = ENTITY_RELATION_DESCENDENTS_DIRECT | ENTITY_RELATION_ASCENDENTS_DIRECT,
    ENTITY_RELATION_ALL_EXTENDED            = ENTITY_RELATION_DESCENDENTS_EXTENDED | ENTITY_RELATION_ASCENDENTS_EXTENDED
};


// *********************************************************************************
// Related Types Guide
// *********************************************************************************

template <sdi::SpecializationType type>
struct SDITypeGuide
{
    typedef char handleread;
    typedef char handleedit;
    typedef char entityread;
    typedef char entityedit;
    typedef char data;
    typedef char selectionread;
    typedef char selectionedit;
};


// Class used to identify and differentiate entity types
class SDI_DECLS EntityTypeIdentifier
{
public:

    EntityTypeIdentifier()   //! implies that all entity types are to be identified
        : p_entityType(ENTITY_TYPE_NONE), p_genericTypeName(sdiString()) {}

    EntityTypeIdentifier(const EntityType entityType)
        : p_entityType(entityType), p_genericTypeName(sdiString()) {}

    EntityTypeIdentifier(const sdiString&  genericTypeName)
        : p_entityType(ENTITY_TYPE_NONE), p_genericTypeName(genericTypeName) {}

    EntityTypeIdentifier& operator=(const EntityType entityType)
    {
        p_genericTypeName = sdiString();  // clear string
        p_entityType = entityType;
        return *this;
    }

    bool operator==(const EntityTypeIdentifier& other) const
    {
        if (p_entityType != other.p_entityType)
        {
            return false;
        }
        if (p_genericTypeName != other.p_genericTypeName)
        {
            return false;
        }
        return true;
    }


    //! Get whether single entity type is identified
    bool IsSingleType() const
    {
        return (p_entityType != ENTITY_TYPE_NONE) ? true : false;
    }

    //! Get entity type: returns ENTITY_TYPE_NONE for case of all
    EntityType GetEntityType() const
    {
        return p_entityType;
    }

    //! Get generic type name; returns false if not generic type
    bool       GetGenericTypeName(sdiString&  genericTypeName) const
    {
        if (p_entityType != ENTITY_TYPE_NONE)
        {
            return false;
        }
        genericTypeName = p_genericTypeName;
        return true;
    }


private:

    EntityType p_entityType;
    sdiString   p_genericTypeName;    //! could be id pool name
};


extern SDI_DECLS sdiString EMPTYSTRING;  // definition in sdiEntity.cxx

}


#endif //! !defined(SDIDEFS__INCLUDED_)
