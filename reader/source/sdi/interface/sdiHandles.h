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

#if !defined(SDIHANDLES__INCLUDED_)
#define SDIHANDLES__INCLUDED_

#include <sdiDefs.h>


namespace sdi
{


// *********************************************************************************
// Foward declarations
// *********************************************************************************
class ModelViewRead;
class ModelViewEdit;
class CollectionRead;
class CollectionEdit;
class HandleRead;
class HandleEdit;

template<SpecializationType ETYPE> class HandleReadT;
template<SpecializationType ETYPE, class PARENT> class HandleEditT;

/**
* Entity handle for read only functionality of database.
* Handles are retrieved from the model or from other entities.
* They are lightweight objects that allow the DB to describe its storage location of an entity.
*/
class SDI_DECLS HandleRead
{
public:

    /// Default constructor, constructs an invalid handle.
    HandleRead()
        : p_type(ENTITY_TYPE_NONE), p_index1(UINT_MAX), p_index2(UINT_MAX) {}

    // Constructor using pointer (called by underlying DB)
    HandleRead(EntityType  type,
               const void* ptr  = 0)
        : p_type(type) { SetPointer(const_cast<void*>(ptr)); }

    // Constructor using index (called by underlying DB)
    HandleRead(EntityType   type,
               unsigned int index1,
               unsigned int index2 = UINT_MAX)
        : p_type(type), p_index1(index1), p_index2(index2) {}

    /// Copy constructor (also allows for creation of "read" entity handle from an "edit" handle).
    HandleRead(const HandleRead& other)
        : p_type(other.p_type), p_ptr(other.p_ptr) {}

    /// Copy-construct general handle from any other handle type
    template <SpecializationType ETYPE>
    HandleRead(const HandleReadT<ETYPE>& other) : p_type(other.p_type)
    { SetPointer(other.GetPointer()); }

    /// Assignment operator (also allows for assignemt of "read" entity handle from an "edit" handle).
    HandleRead& operator=(const HandleRead& other)
    { p_type = other.p_type; p_ptr  = other.p_ptr; return *this; }

    /// Assign general handle from any other handle type.
    template <SpecializationType ETYPE>
    HandleRead& operator=(const HandleReadT<ETYPE>& other)
    { p_type = other.p_type; SetPointer(other.GetPointer()); return *this; }


    //! Retrieve specialization type
    SpecializationType GetSpecializationType(const ModelViewRead* pModelView) const;

    /// Test whether handle is valid.
    bool  IsValid() const
    {
        return UINT_MAX != p_index1 || UINT_MAX != p_index2 ? true : false;
    }

    //! Retrieve entity type
    EntityType GetType() const
    {
        return p_type;
    }

    void* GetPointer() const
    {
        if(UINT_MAX == p_index1 && UINT_MAX == p_index2) return nullptr;
        else                                             return p_ptr;
    }
    void  SetPointer(void* ptr)
    {
        if(nullptr == ptr) SetIndex(UINT_MAX, UINT_MAX);
        else               p_ptr = ptr;
    }

    unsigned int GetIndex() const
    {
        return p_index1;
    }
    unsigned int GetIndex1() const
    {
        return p_index1;
    }
    unsigned int GetIndex2() const
    {
        return p_index2;
    }
    void  SetIndex(unsigned int index1, unsigned int index2 = UINT_MAX)
    {
        p_index1 = index1;
        p_index2 = index2;
    }
    void  SetIndex1(unsigned int index1)
    {
        p_index1 = index1;
    }
    void  SetIndex2(unsigned int index2)
    {
        p_index2 = index2;
    }

    bool operator==(const HandleRead& other) const
    {
        return (p_ptr == other.p_ptr && p_type == other.p_type);
    }
    bool operator!=(const HandleRead& other) const
    {
        return !operator==(other);
    }
    bool operator<(const HandleRead& other)  const
    {
        return (p_type != other.p_type ? p_type < other.p_type : p_ptr < other.p_ptr);
    }
    bool operator>(const HandleRead& other)  const
    {
        return (p_ptr > other.p_ptr);
    }

    // The > 3 is because the last 3 bits of pointers will always be zero.
    size_t hash() const
    {
        return ((size_t)(p_ptr) >> 3);
    }

    /// Retrieve id
    unsigned int GetId(const ModelViewRead* pModelView) const;
    /// Retrieve name. Returns empty string if unnamed entity.
    sdiString    GetName(const ModelViewRead* pModelView) const;


    /// Retrieve a value. Return false if unable to identify.
    Status GetValue(const ModelViewRead* pModelView,
                    const sdiIdentifier& identifier,
                    sdiValue&            value) const;

    /// Retrieve an "entity type" value as a handle.
    Status GetEntityHandle(const ModelViewRead* pModelView,
                           const sdiIdentifier& identifier,
                           HandleRead&          handle) const;

    //! Return whether particular identifier is parameterized for this entity
    bool IsParameterized(const ModelViewRead*            pModelView,
                         const sdiIdentifier& identifier) const;

    //! Return the parameter name if identifier is parameterized for this entity.
    //! If pIsNegated is passed as a valid pointer, *pIsNegated is populated with the info
    //!   whether the parameter is used as is (false) or negated (true).
    sdiString GetParameterName(const ModelViewRead*            pModelView,
                              const sdiIdentifier& identifier,
                              bool*                           pIsNegated = nullptr) const;

protected: // hidden
    Status GetEntityHandle(const ModelViewRead*            pModelView,
                           const sdiIdentifier& identifier,
                           HandleEdit&                     handle) const;

    // data members
    union
    {
        void* p_ptr;
        struct { unsigned int p_index1, p_index2; };
    };
    EntityType p_type;
};


/// Entity handle for read and edit functionality of database.
class SDI_DECLS HandleEdit : virtual public HandleRead
{
public:

    /// Default constructor, constructs an invalid handle.
    HandleEdit()
        : HandleRead() {}

    // Constructor using pointer (called by underlying DB)
    HandleEdit(EntityType type,
               void*      ptr  = 0)
        : HandleRead(type, ptr) {}

    // Constructor using index (called by underlying DB)
    HandleEdit(EntityType   type,
               unsigned int index1,
               unsigned int index2 = 0)
        : HandleRead(type, index1, index2) {}

    /// Copy constructor.
    HandleEdit(const HandleEdit& other)
        : HandleRead(other.GetType(), other.GetPointer()) {}

    /// Copy-construct general handle from any other handle type
    template <SpecializationType ETYPE, class PARENT>
    HandleEdit(const HandleEditT<ETYPE, PARENT>& other)
        : HandleRead(other.GetType(), other.GetPointer()) {}


    /// Assignment operator.
    HandleEdit& operator=(const HandleEdit& other)
    { p_type = other.p_type; p_ptr  = other.p_ptr; return *this; }

    /// Assign general handle from any other handle type.
    template <SpecializationType ETYPE, class PARENT>
    HandleEdit& operator=(const HandleEditT<ETYPE, PARENT>& other)
    { p_type = other.p_type; SetPointer(other.GetPointer()); return *this; }


    /// Set entity id.
    Status SetId(const ModelViewEdit* pModelView, const unsigned int id) const;
    /// Set entity name.
    Status SetName(const ModelViewEdit* pModelView, const sdiString& name) const;


    using HandleRead::GetEntityHandle;
    /// Retrieve an "entity type" value as a handle.
    Status GetEntityHandle(const ModelViewRead*            pModelView,
                           const sdiIdentifier& identifier,
                           HandleEdit&                     handle) const;

    /// Set a value.
    Status SetValue(const ModelViewEdit*            pModelView,
                           const sdiIdentifier& identifier,
                           const sdiValue&      value) const;

    /// Set an "entity type" value using a handle.
    Status SetEntityHandle(const ModelViewEdit*            pModelView,
                           const sdiIdentifier& identifier,
                           const HandleRead&               handle) const;

    //! Set a parameterized value.
    //! NB: The value of the parameter replaces the current value in the entity.
    Status SetParameter(const ModelViewEdit*            pModelView,
                        const sdiIdentifier& identifier,
                        const sdiString&                 parameterName,
                        bool                            isNegated = false) const;

};



// *********************************************************************************
// HandleBase that allows only for read only functionality of database
// *********************************************************************************
template<SpecializationType ETYPE>
class HandleReadT : virtual public HandleRead
{
public:

    HandleReadT() : HandleRead(0) {}
    //! with type checking (optional)
    HandleReadT(const HandleRead& other, const ModelViewRead* pModelView);  //! also will accept HandleEdit
    HandleReadT(EntityType type, const void* ptr, const ModelViewRead* pModelView);
    //! without type checking
    HandleReadT(const HandleRead& other)  //! also will accept HandleEdit
        : HandleRead(other) {}
    HandleReadT(EntityType type, const void* ptr)
        : HandleRead(type, ptr) {}
    HandleReadT(EntityType   type,
                unsigned int index1,
                unsigned int index2 = 0)
        : HandleRead(type, index1, index2) {}
};


// *********************************************************************************
// HandleBase that allows for read and edit functionality of database
// *********************************************************************************
template<SpecializationType ETYPE, class PARENT>
class HandleEditT : public PARENT, public HandleEdit
{
public:

    HandleEditT() : HandleRead(), HandleEdit(), PARENT() {}
    //! with type checking (optional)
    HandleEditT(const HandleEdit& other, const ModelViewRead* pModelView) :
        HandleRead((const HandleRead&) other), HandleEdit(other), PARENT(other, pModelView) {}
    HandleEditT(EntityType type, void* ptr, const ModelViewRead* pModelView) :
        HandleRead(type, ptr), HandleEdit(type, ptr), PARENT(type, ptr, pModelView) {}
    //! without type checking
    HandleEditT(const HandleEdit& other) :
        HandleRead((const HandleRead&) other), HandleEdit(other), PARENT(other) {}
    HandleEditT(EntityType type, void* ptr) :
        HandleRead(type, ptr), HandleEdit(type, ptr), PARENT(type, ptr) {}
    HandleEditT(EntityType   type,
                unsigned int index1,
                unsigned int index2 = 0) :
        HandleRead(type, index1, index2), HandleEdit(type, index1, index2), PARENT(type, index1, index2) {}

    using PARENT::IsValid;
    using PARENT::GetType;
    using PARENT::GetSpecializationType;
    using PARENT::GetPointer;
    using PARENT::SetPointer;
    using PARENT::operator=;
    using PARENT::operator==;
    using PARENT::operator!=;
    using PARENT::operator<;
    using PARENT::operator>;
    using PARENT::hash;
    using PARENT::GetId;
    using PARENT::GetName;
    using PARENT::GetValue;
    using PARENT::GetEntityHandle;
    using PARENT::IsParameterized;
    using PARENT::GetParameterName;
};



/** Handle for entities of "specialization type node" (read only). */
typedef HandleReadT<SPECIALIZATION_TYPE_NODE>         HandleNodeRead;
typedef HandleReadT<SPECIALIZATION_TYPE_ELEMENT>      HandleElementRead;


typedef HandleEditT<SPECIALIZATION_TYPE_NODE,          HandleNodeRead>          HandleNodeEdit;
typedef HandleEditT<SPECIALIZATION_TYPE_ELEMENT,       HandleElementRead>       HandleElementEdit;




////////////////////////////////////////////////////////////////////////////////////
// Function bodies
////////////////////////////////////////////////////////////////////////////////////


// *********************************************************************************
// Template handle that allows only for read only functionality of database
// *********************************************************************************

// also will accept HandleEdit
template<SpecializationType ETYPE>
HandleReadT<ETYPE>::HandleReadT(const HandleRead& other, const ModelViewRead* pModelView)
    : HandleRead(other.GetType(), other.GetPointer())
{
    if (nullptr != pModelView && other.GetSpecializationType(pModelView) != ETYPE)
    {
        p_ptr = 0;
    }
}

template<SpecializationType ETYPE>
HandleReadT<ETYPE>::HandleReadT(EntityType type, const void* ptr, const ModelViewRead* pModelView)
    : HandleRead(type, ptr)
{
    if (nullptr != pModelView && GetSpecializationType(pModelView) != ETYPE)
    {
        p_ptr = 0;
    }
}

}

#endif //! !defined(SDIHANDLES__INCLUDED_)
