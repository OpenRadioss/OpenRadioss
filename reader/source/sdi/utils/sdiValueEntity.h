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

#if !defined(SDIVALUEENTITY__INCLUDED_)
#define SDIVALUEENTITY__INCLUDED_

#include <sdiUtilsDefs.h>
#include <boost/iterator/iterator_facade.hpp>


class sdiValue;
class Attribute;
// *********************************************************************************
// sdiValueEntityType
// *********************************************************************************
class SDIUTILS_DECLS sdiValueEntityType
{
public:

    //! sdiValueEntityType is composed of primary and an optional secondary (sub) type. Both
    //! can be numeric or named (string).
    inline sdiValueEntityType(const unsigned int typeNumeric, const unsigned int subtypeNumeric = 0);
    inline sdiValueEntityType(const unsigned int typeNumeric, const sdiString& subtypeNamed);
    inline sdiValueEntityType(const sdiString& typeNamed, const unsigned int subtypeNumeric = 0);
    inline sdiValueEntityType(const sdiString& typeNamed, const sdiString& subtypeNamed);

    // Topology Type
    enum TopologyType
    {
    TOPOLOGY_TYPE_NONE,
    TOPOLOGY_TYPE_MIXED
    };

    //! Additional contracution that take topology type as argument. Above constructors all have TOPOLOGY_TYPE_NONE
    inline sdiValueEntityType(const TopologyType topoType, const unsigned int typeNumeric, const unsigned int subtypeNumeric = 0);
    inline sdiValueEntityType(const TopologyType topoType, const unsigned int typeNumeric, const sdiString& subtypeNamed);
    inline sdiValueEntityType(const TopologyType topoType, const sdiString& typeNamed, const unsigned int subtypeNumeric = 0);
    inline sdiValueEntityType(const TopologyType topoType, const sdiString& typeNamed, const sdiString& subtypeNamed);

    //! Exposed default constructor creates invalid entity type
    inline sdiValueEntityType();

    ~sdiValueEntityType() {}  // Thus, you shouldn't ever derive from this class.


    inline sdiValueEntityType& operator=(const sdiValueEntityType& other);

    inline bool operator==(const sdiValueEntityType& other) const;
    inline bool operator!=(const sdiValueEntityType& other) const;


    ////////////////////////////////////////////////////////////////////////////////////
    //! Query
    ////////////////////////////////////////////////////////////////////////////////////

    inline bool         IsValid() const;

    inline bool         IsTypeNumeric() const;        //! otherwise, it is named
    inline unsigned int GetTypeNumeric() const;       //! 0 if using named
    inline sdiString     GetTypeNamed() const;         //! "" if using numeric

    inline bool         HaveSubType() const;
    inline bool         IsSubTypeNumeric() const;     //! otherwise, it is named
    inline unsigned int GetSubtypeNumeric() const;    //! 0 if using named
    inline sdiString     GetSubtypeNamed() const;      //! "" if using numeric

    inline TopologyType GetTopologyType() const;      //! Topology type


private:

    unsigned int  p_typeNumeric, p_subtypeNumeric;
    sdiString      p_typeNamed, p_subtypeNamed;
    TopologyType  p_topologyType;
};

typedef sdiVector<sdiValueEntityType> EntityFullTypeList;
typedef std::pair<sdiValueEntityType, sdiValueEntityType> EntityFullTypePair;
typedef sdiVector<EntityFullTypePair> EntityFullTypePairList;

// *********************************************************************************
// sdiValueEntity
// *********************************************************************************
class SDIUTILS_DECLS sdiValueEntity
{
public:

    // Topology index only valid for entityFullType: TOPOLOGY_TYPE_MIXED
    inline sdiValueEntity(const sdiValueEntityType& entityFullType, const unsigned int id, const char topologyIndex = 0);

    //! Exposed default constructor creates "0" entity
    inline sdiValueEntity();

    //! Exposed destructor
    ~sdiValueEntity() {}  // Thus, you shouldn't ever derive from this class.

    inline sdiValueEntity& operator=(const sdiValueEntity& other);

    inline bool operator==(const sdiValueEntity& other) const;
    inline bool operator!=(const sdiValueEntity& other) const;
    inline bool operator<(const sdiValueEntity& other) const;


    ////////////////////////////////////////////////////////////////////////////////////
    //! Query
    ////////////////////////////////////////////////////////////////////////////////////

    inline sdiValueEntityType   GetEntityFullType() const;
    inline unsigned int     GetId() const;
    inline char             GetTopologyIndex() const;

    //! Provides direct access to id and topology index
    inline const unsigned int& GetIdRef() const;
    inline unsigned int&       GetIdRef();
    inline const char&         GetTopologyIndexRef() const;
    inline char&               GetTopologyIndexRef();

private:

    sdiValueEntityType    p_entityFullType;
    unsigned int      p_id;
    char              p_topologyIndex;
};



// *********************************************************************************
// sdiValueEntityList - homogeneous list of entities
// *********************************************************************************
class SDIUTILS_DECLS sdiValueEntityList
{
public:

    // Topology indeces only valid for entityFullType: TOPOLOGY_TYPE_MIXED

    inline sdiValueEntityList(const sdiValueEntityType& entityFullType, const sdiUIntList& aId, const sdiCharList& aTopologyIndex = sdiCharList());

    //! Exposed default constructor, with possible reserve for id list
    inline sdiValueEntityList(const unsigned int reserve = 0);

    //! Exposed destructor
    ~sdiValueEntityList() {}  // Thus, you shouldn't ever derive from this class.


    inline sdiValueEntityList& operator=(const sdiValueEntityList& other);


    bool        operator==(const sdiValueEntityList& other) const;
    inline bool operator!=(const sdiValueEntityList& other) const;


    ////////////////////////////////////////////////////////////////////////////////////
    //! Query
    ////////////////////////////////////////////////////////////////////////////////////

    inline sdiValueEntityType GetEntityFullType() const;

    inline void         GetIdList(sdiUIntList& aId) const;
    inline unsigned int GetIdListCount() const;
    inline bool         GetId(const unsigned int i, unsigned int& id) const;
    inline bool         GetIdAndTopologyIndex(const unsigned int i, unsigned int& id, char& topoIndex) const;

    //! [] operator provides direct access to id
    inline const unsigned int& operator [](const unsigned int i) const;

    //! Returns false if i > count, or no entity type.
    inline bool GetEntity(sdiValueEntity& entity, const unsigned int i) const;

    //! Return sdiUIntList const reference containing ids; can also access list of topology index list for entityFullType: TOPOLOGY_TYPE_MIXED
    inline const sdiUIntList& GetList() const;
    inline const sdiCharList& GetTopologyIndexList() const;

    inline bool IsEmpty() const;


    ////////////////////////////////////////////////////////////////////////////////////
    //! Modify
    ////////////////////////////////////////////////////////////////////////////////////

    //! Cannot change type once set
    inline bool SetEntityFullType(const sdiValueEntityType& entityFullType);

    inline void Reserve(const unsigned int reserve);
    inline void Resize(const unsigned int count);

    //! May change size of list
    inline void  SetId(const unsigned int i, const unsigned int id, const char topoIndex = 0);

    //! [] operator provides direct access to id; may change size of list
    inline unsigned int& operator [](const unsigned int i);

    //! Returns false if entity's type if different from that defined for list
    //! May change size of list.
    inline bool SetEntity(const sdiValueEntity& entity, const unsigned int i);

    //! Return sdiUIntList reference containing ids; can also access list of topology index list for entityFullType: TOPOLOGY_TYPE_MIXED
    inline sdiUIntList& GetList();               
    inline sdiCharList& GetTopologyIndexList();        // only valid for entityFullType: !TOPOLOGY_TYPE_NONE

    //! Set Lists simulataneously; aTopologyIndex only valid for entityFullType: TOPOLOGY_TYPE_MIXED
    //! Both lists must have the same size.
    inline bool SetLists(const sdiUIntList& aId, const sdiCharList& aTopologyIndex);

//HwConvert }}
    class const_iterator : public boost::iterator_facade<const_iterator, sdiValueEntity const, boost::forward_traversal_tag>
    {
    public:
        const_iterator(sdiValueEntityList const& el, unsigned int i) : _i(i), _el(&el) {}
        const_iterator& operator=(const const_iterator& input) { _el = input._el; _i = input._i; return *this; }
    private:

        friend class boost::iterator_core_access;
        void increment() { ++_i; }
        bool equal(const_iterator const& other) const { return &this->_el == &this->_el && this->_i == other._i; }
        sdiValueEntity const& dereference() const {
            _el->GetEntity(_e, _i);
            return _e;
        }
        unsigned int _i;
        sdiValueEntityList const* _el;
        mutable sdiValueEntity _e;
    };

    const_iterator begin() const noexcept { return const_iterator(*this, 0); }
    const_iterator end() const noexcept { return const_iterator(*this, (unsigned int)(p_aId.size())); }
//HwConvert {{

private:

    sdiValueEntityType  p_entityFullType;
    sdiUIntList      p_aId;
    sdiCharList      p_aTopologyIndex;
};


// *********************************************************************************
// sdiValueEntityList2 - homegeneous list of entities
// *********************************************************************************
class SDIUTILS_DECLS sdiValueEntityList2
{
public:
    
    

    inline sdiValueEntityList2(const sdiValueEntityType& entityFullType,
                       const sdiUIntList2&    aaId);

    //! Exposed default constructor, with possible allocation for id list
    inline sdiValueEntityList2(const unsigned int sizeDimFirst = 0,         //! size of first (outermost) dimension
                       const unsigned int sizeDimSecond = 0);

    //! Exposed destructor
    ~sdiValueEntityList2() {}  // Thus, you shouldn't ever derive from this class.


    inline sdiValueEntityList2& operator=(const sdiValueEntityList2& other);


    bool        operator==(const sdiValueEntityList2& other) const;
    inline bool operator!=(const sdiValueEntityList2& other) const;


    ////////////////////////////////////////////////////////////////////////////////////
    //! Query
    ////////////////////////////////////////////////////////////////////////////////////

    inline sdiValueEntityType GetEntityFullType() const;

    inline unsigned int GetIdListCount(const unsigned int dimensionIndex = 1) const;
    inline bool         GetId(const unsigned int i, const unsigned int j, unsigned int& id) const;

    //! Returns false if i,j outside populated range, or no entity type.
    inline bool GetEntity(sdiValueEntity& entity, const unsigned int i, const unsigned int j) const;

    //! Return sdiUIntList2 const reference containing ids
    inline const sdiUIntList2& GetList() const;


    ////////////////////////////////////////////////////////////////////////////////////
    //! Modify
    ////////////////////////////////////////////////////////////////////////////////////

    //! Cannot change type once set
    inline bool SetEntityFullType(const sdiValueEntityType& entityFullType);

    inline void Reserve(const unsigned int sizeDimFirst,            //! size of first (outermost) dimension
                        const unsigned int sizeDimSecond = 0);

    //! May change size of list
    inline void  SetId(const unsigned int i, const unsigned int j, const unsigned int id);

    //! Returns false if entity's type if different from that defined for list
    //! May change size of list.
    inline bool SetEntity(const sdiValueEntity& entity, const unsigned int i, const unsigned int j);

    //! Return sdiUIntList2 reference containing ids
    inline sdiUIntList2& GetList();


private:

    sdiValueEntityType  p_entityFullType;
    sdiUIntList2     p_aaId;
};



// *********************************************************************************
// sdiValueEntityType Implementation
// *********************************************************************************

// sdiValueEntityType is composed of primary and an optional secondary (sub) type. Both
// can be numeric or named (string).
inline sdiValueEntityType::sdiValueEntityType(const unsigned int typeNumeric, const unsigned int subtypeNumeric)
    : p_typeNumeric(typeNumeric), p_subtypeNumeric(subtypeNumeric), p_topologyType(TOPOLOGY_TYPE_NONE)
{
}
inline sdiValueEntityType::sdiValueEntityType(const unsigned int typeNumeric, const sdiString& subtypeNamed)
    : p_typeNumeric(typeNumeric), p_subtypeNumeric(0), p_subtypeNamed(subtypeNamed), p_topologyType(TOPOLOGY_TYPE_NONE)
{
}
inline sdiValueEntityType::sdiValueEntityType(const sdiString& typeNamed, const unsigned int subtypeNumeric)
    : p_typeNamed(typeNamed), p_typeNumeric(0), p_subtypeNumeric(subtypeNumeric), p_topologyType(TOPOLOGY_TYPE_NONE)
{
    assert(!typeNamed.empty());
}
inline sdiValueEntityType::sdiValueEntityType(const sdiString& typeNamed, const sdiString& subtypeNamed)
    : p_typeNamed(typeNamed), p_typeNumeric(0), p_subtypeNumeric(0), p_subtypeNamed(subtypeNamed), p_topologyType(TOPOLOGY_TYPE_NONE)
{
    assert(!typeNamed.empty());
}

inline sdiValueEntityType::sdiValueEntityType(const TopologyType topoType, const unsigned int typeNumeric, const unsigned int subtypeNumeric)
    : p_typeNumeric(typeNumeric), p_subtypeNumeric(subtypeNumeric), p_topologyType(topoType)
{
}
inline sdiValueEntityType::sdiValueEntityType(const TopologyType topoType, const unsigned int typeNumeric, const sdiString& subtypeNamed)
    : p_typeNumeric(typeNumeric), p_subtypeNumeric(0), p_subtypeNamed(subtypeNamed), p_topologyType(topoType)
{
}
inline sdiValueEntityType::sdiValueEntityType(const TopologyType topoType, const sdiString& typeNamed, const unsigned int subtypeNumeric)
    : p_typeNamed(typeNamed), p_typeNumeric(0), p_subtypeNumeric(subtypeNumeric), p_topologyType(topoType)
{
    assert(!typeNamed.empty());
}
inline sdiValueEntityType::sdiValueEntityType(const TopologyType topoType, const sdiString& typeNamed, const sdiString& subtypeNamed)
    : p_typeNamed(typeNamed), p_typeNumeric(0), p_subtypeNumeric(0), p_subtypeNamed(subtypeNamed), p_topologyType(topoType)
{
    assert(!typeNamed.empty());
}

// Exposed default constructor creates invalid entity type
inline sdiValueEntityType::sdiValueEntityType()
    : p_typeNumeric(0), p_subtypeNumeric(0), p_topologyType(TOPOLOGY_TYPE_NONE)
{
}

inline sdiValueEntityType& sdiValueEntityType::operator=(const sdiValueEntityType& other)
{
    p_typeNamed = other.p_typeNamed;
    p_typeNumeric = other.p_typeNumeric;
    p_subtypeNumeric = other.p_subtypeNumeric;
    p_subtypeNamed = other.p_subtypeNamed;
    p_topologyType = other.p_topologyType;
    return *this;
}

inline bool sdiValueEntityType::operator==(const sdiValueEntityType& other) const
{
    if (p_typeNumeric != other.p_typeNumeric)
    {
        return false;
    }
    if (p_typeNamed != other.p_typeNamed)
    {
        return false;
    }
    if (p_subtypeNumeric != other.p_subtypeNumeric)
    {
        return false;
    }
    if (p_subtypeNamed != other.p_subtypeNamed)
    {
        return false;
    }
    return true;
}
inline bool sdiValueEntityType::operator!=(const sdiValueEntityType& other) const
{
    return !operator==(other);
}


inline bool sdiValueEntityType::IsValid() const
{
    return (p_typeNumeric || !p_typeNamed.empty()) ? true : false;
}

inline bool         sdiValueEntityType::IsTypeNumeric() const
{
    return p_typeNumeric ? true : false;
}
inline unsigned int sdiValueEntityType::GetTypeNumeric() const
{
    return p_typeNumeric;    //! 0 if using named
}
inline sdiString     sdiValueEntityType::GetTypeNamed() const
{
    return p_typeNamed;    //! "" if using numeric
}

inline bool sdiValueEntityType::HaveSubType() const
{
    return (p_subtypeNumeric || !p_subtypeNamed.empty()) ? true : false;
}

inline bool         sdiValueEntityType::IsSubTypeNumeric() const
{
    return p_subtypeNumeric ? true : false;
}
inline unsigned int sdiValueEntityType::GetSubtypeNumeric() const
{
    return p_subtypeNumeric;    //! 0 if using named
}
inline sdiString     sdiValueEntityType::GetSubtypeNamed() const
{
    return p_subtypeNamed;    //! "" if using numeric
}

inline sdiValueEntityType::TopologyType sdiValueEntityType::GetTopologyType() const
{
    return p_topologyType;
}

// *********************************************************************************
// sdiValueEntity Implementation
// *********************************************************************************
inline sdiValueEntity::sdiValueEntity(const sdiValueEntityType& entityFullType, const unsigned int id, const char topologyIndex)
    : p_entityFullType(entityFullType), p_id(id), p_topologyIndex(topologyIndex)
{
}

// Exposed default constructor creates "0" entity
inline sdiValueEntity::sdiValueEntity()
    : p_id(0), p_topologyIndex(0)
{
}

inline sdiValueEntity& sdiValueEntity::operator=(const sdiValueEntity& other)
{
    p_entityFullType = other.p_entityFullType;
    p_id = other.p_id;
    p_topologyIndex = other.p_topologyIndex;
    return *this;
}

inline bool sdiValueEntity::operator==(const sdiValueEntity& other) const
{
    if (p_entityFullType != other.p_entityFullType)
    {
        return false;
    }
    if (p_id             != other.p_id)
    {
        return false;
    }
    if (p_topologyIndex  != other.p_topologyIndex)
    {
        return false;
    }
    return true;
}

inline bool sdiValueEntity::operator!=(const sdiValueEntity& other) const
{
    return !operator==(other);
}

inline bool sdiValueEntity::operator<(const sdiValueEntity& other) const
{
    // Only use id for comparison
    return (p_id < other.p_id);
}

inline sdiValueEntityType   sdiValueEntity::GetEntityFullType() const
{
    return p_entityFullType;
}
inline unsigned int     sdiValueEntity::GetId() const
{
    return p_id;
}
inline char             sdiValueEntity::GetTopologyIndex() const
{
    return p_topologyIndex;
}

inline const unsigned int& sdiValueEntity::GetIdRef() const
{
    return p_id;
}
inline unsigned int&       sdiValueEntity::GetIdRef()
{
    return p_id;
}
inline const char&         sdiValueEntity::GetTopologyIndexRef() const
{
    return p_topologyIndex;
}
inline char&               sdiValueEntity::GetTopologyIndexRef()
{
    return p_topologyIndex;
}


// *********************************************************************************
// sdiValueEntityList inline Implementation
// *********************************************************************************

inline sdiValueEntityList::sdiValueEntityList(const sdiValueEntityType& entityFullType, const sdiUIntList& aId, const sdiCharList& aTopologyIndex)
    : p_entityFullType(entityFullType), p_aId(aId), p_aTopologyIndex(aTopologyIndex)
{
    if (p_entityFullType.GetTopologyType() == sdiValueEntityType::TOPOLOGY_TYPE_NONE)
    {
        assert(!aTopologyIndex.size());
    }
    else
    {
        assert(aId.size() == aTopologyIndex.size());
    }
}

// Exposed default constructor, with possible reserve for id list
inline sdiValueEntityList::sdiValueEntityList(const unsigned int reserve)
{
    p_aId.reserve(reserve);
}

inline sdiValueEntityList& sdiValueEntityList::operator=(const sdiValueEntityList& other)
{
    p_entityFullType = other.p_entityFullType;
    p_aId = other.p_aId;
    p_aTopologyIndex = other.p_aTopologyIndex;
    return *this;
}

inline bool sdiValueEntityList::operator!=(const sdiValueEntityList& other) const
{
    return !operator==(other);
}


inline sdiValueEntityType sdiValueEntityList::GetEntityFullType() const
{
    return p_entityFullType;
}

inline unsigned int sdiValueEntityList::GetIdListCount() const
{
    return (unsigned int)p_aId.size();
}

inline void         sdiValueEntityList::GetIdList(sdiUIntList& aId) const
{
    aId = p_aId;
}

inline bool  sdiValueEntityList::GetId(const unsigned int i, unsigned int& id) const
{
    if (i >= p_aId.size())
    {
        assert(0);
        return false;
    }

    id = p_aId[i];
    return true;
}

inline bool sdiValueEntityList::GetIdAndTopologyIndex(const unsigned int i, unsigned int& id, char& topoIndex) const
{
    if (i >= p_aId.size())
    {
        assert(0);
        return false;
    }

    id = p_aId[i];
    
    if (p_entityFullType.GetTopologyType() == sdiValueEntityType::TOPOLOGY_TYPE_NONE)
    {
        topoIndex = 0;
        return true;
    }
    else
    {
        assert(p_aId.size() == p_aTopologyIndex.size());
        topoIndex = p_aTopologyIndex[i];
        return true;
    }

    assert(0);
    return false;
}

inline bool sdiValueEntityList::IsEmpty() const
{
    return( GetIdListCount() == 0 ? true : p_aId[0] == 0 );
} // IsEmpty

// [] operator provides direct access to id
inline const unsigned int& sdiValueEntityList::operator [](const int unsigned i) const
{
    if (i >= p_aId.size())
    {
        assert(0);
        static const unsigned int _value = 0;
        return _value;
    }

    return p_aId[i];
}


// Returns false if i > count, or no entity type.
inline bool sdiValueEntityList::GetEntity(sdiValueEntity& entity, const unsigned int i) const
{
    if (i >= p_aId.size())
    {
        return false;
    }

    if (!p_entityFullType.IsValid())
    {
        return false;
    }

    entity = sdiValueEntity(p_entityFullType, p_aId[i]);
    return true;
}

// Return sdiUIntList const reference containing ids
inline const sdiUIntList& sdiValueEntityList::GetList() const
{
    return p_aId;
}

inline const sdiCharList& sdiValueEntityList::GetTopologyIndexList() const
{
    assert(p_entityFullType.GetTopologyType() != sdiValueEntityType::TOPOLOGY_TYPE_NONE);
    return p_aTopologyIndex;
}

// Cannot change type once set
inline bool sdiValueEntityList::SetEntityFullType(const sdiValueEntityType& entityFullType)
{
    if (p_entityFullType.IsValid())
    {
        return false;
    }

    p_entityFullType = entityFullType;
    return true;
}

inline void sdiValueEntityList::Reserve(const unsigned int reserve)
{
    p_aId.reserve(reserve);
    if (p_entityFullType.GetTopologyType() != sdiValueEntityType::TOPOLOGY_TYPE_NONE)
    {
        p_aTopologyIndex.reserve(reserve);
    }
}

inline void sdiValueEntityList::Resize(const unsigned int count)
{
    p_aId.resize(count, 0);
    if (p_entityFullType.GetTopologyType() != sdiValueEntityType::TOPOLOGY_TYPE_NONE)
    {
        p_aTopologyIndex.resize(count, 0);
    }
}

// May change size of list
inline void  sdiValueEntityList::SetId(const unsigned int i, const unsigned int id, const char topoIndex)
{
    const bool haveTopo = (p_entityFullType.GetTopologyType() == sdiValueEntityType::TOPOLOGY_TYPE_NONE) ? false : true;

    if (i >= p_aId.size())
    {
        p_aId.resize(i + 1, 0);
        if (haveTopo)
        {
            p_aTopologyIndex.resize(i + 1, 0);
        }
    }

    p_aId[i] = id;
    if (haveTopo)
    {
        p_aTopologyIndex[i] = topoIndex;
    }
}

// [] operator provides direct access to id; may change size of list
inline unsigned int& sdiValueEntityList::operator [](const unsigned int i)
{
    if (i >= p_aId.size())
    {
        p_aId.resize(i + 1, 0);
    }

    return p_aId[i];
}

// Returns false if entity's type if different from that defined for list
// May change size of list.
inline bool sdiValueEntityList::SetEntity(const sdiValueEntity& entity, const unsigned int i)
{
    if (p_entityFullType.IsValid())
    {
        if (entity.GetEntityFullType() != p_entityFullType)
        {
            return false;
        }
    }
    else
    {
        p_entityFullType = entity.GetEntityFullType();
    }

    SetId(i, entity.GetId(), entity.GetTopologyIndex());
    return true;
}

// Return sdiUIntList reference containing ids
inline sdiUIntList& sdiValueEntityList::GetList()
{
    return p_aId;
}

inline sdiCharList& sdiValueEntityList::GetTopologyIndexList()
{
    assert(p_entityFullType.GetTopologyType() != sdiValueEntityType::TOPOLOGY_TYPE_NONE);
    return p_aTopologyIndex;
}

inline bool sdiValueEntityList::SetLists(const sdiUIntList& aId, const sdiCharList& aTopologyIndex)
{
    p_aId = aId;
    
    if (p_entityFullType.GetTopologyType() == sdiValueEntityType::TOPOLOGY_TYPE_NONE)
    {
        assert(!aTopologyIndex.size());
    }
    else
    {
        if (aId.size() != aTopologyIndex.size())
        {
            assert(0);
            return false;
        }
        p_aTopologyIndex = aTopologyIndex;
    }
    return true;
}



// *********************************************************************************
// sdiValueEntityList2 Implementation
// *********************************************************************************

inline sdiValueEntityList2::sdiValueEntityList2(const sdiValueEntityType& entityFullType, const sdiUIntList2& aaId)
    : p_entityFullType(entityFullType),
      p_aaId(aaId)
{
    assert(p_entityFullType.IsValid());
}

//! Exposed default constructor, with possible allocation for id list
inline sdiValueEntityList2::sdiValueEntityList2(const unsigned int sizeDimFirst,         //! size of first (outermost) dimension
                                const unsigned int sizeDimSecond)
{
    Reserve(sizeDimFirst, sizeDimSecond);
}

inline sdiValueEntityList2& sdiValueEntityList2::operator=(const sdiValueEntityList2& other)
{
    p_entityFullType = other.p_entityFullType;
    p_aaId = other.p_aaId;
    return *this;
}


inline bool sdiValueEntityList2::operator!=(const sdiValueEntityList2& other) const
{
    return !operator==(other);
}


////////////////////////////////////////////////////////////////////////////////////
//! Query
////////////////////////////////////////////////////////////////////////////////////

inline sdiValueEntityType sdiValueEntityList2::GetEntityFullType() const
{
    return p_entityFullType;
}

inline unsigned int sdiValueEntityList2::GetIdListCount(const unsigned int dimensionIndex) const
{
    if (!p_aaId.size())
    {
        return 0;
    }

    if (dimensionIndex == 1)
    {
        return (unsigned int)p_aaId.size();
    }

    unsigned int retval = (unsigned int)p_aaId[0].size();
    for (unsigned int i = 1; i < (unsigned int)p_aaId.size(); i++)
    {
        if ((unsigned int)p_aaId[i].size() > retval)
        {
            retval = (unsigned int)p_aaId[i].size();
        }
    }
    return retval;
}
inline bool         sdiValueEntityList2::GetId(const unsigned int i, const unsigned int j, unsigned int& id) const
{
    if (i < p_aaId.size())
    {
        if (j < p_aaId[i].size())
        {
            id = p_aaId[i][j];
            return true;
        }
    }
    return false;
}

// Returns false if i,j outside populated range, or no entity type.
inline bool sdiValueEntityList2::GetEntity(sdiValueEntity& entity, const unsigned int i, const unsigned int j) const
{
    unsigned int id;
    if (!GetId(i, j, id))
    {
        return false;
    }

    if (!p_entityFullType.IsValid())
    {
        return false;
    }

    entity = sdiValueEntity(p_entityFullType, id);
    return true;
}

// Return sdiUIntList2 const reference containing ids
inline const sdiUIntList2& sdiValueEntityList2::GetList() const
{
    return p_aaId;
}


////////////////////////////////////////////////////////////////////////////////////
// Modify
////////////////////////////////////////////////////////////////////////////////////

// Cannot change type once set
inline bool sdiValueEntityList2::SetEntityFullType(const sdiValueEntityType& entityFullType)
{
    if (p_entityFullType.IsValid())
    {
        return false;
    }

    p_entityFullType = entityFullType;
    return true;
}

inline void sdiValueEntityList2::Reserve(const unsigned int sizeDimFirst,            //! size of first (outermost) dimension
                                 const unsigned int sizeDimSecond)
{
    if (sizeDimFirst > p_aaId.size())
    {
        p_aaId.resize(sizeDimFirst);
    }
    for (unsigned int i = 0; i < p_aaId.size(); i++)
    {
        if (sizeDimSecond > p_aaId[i].size())
        {
            p_aaId[i].reserve(sizeDimSecond);
        }
    }
}

// May change size of list
inline void  sdiValueEntityList2::SetId(const unsigned int i, const unsigned int j, const unsigned int id)
{
    if (i >= p_aaId.size())
    {
        p_aaId.resize(i + 1);
    }
    if (j >= p_aaId[i].size())
    {
        p_aaId[i].resize(j + 1, 0);
    }
    p_aaId[i][j] = id;
}

// Returns false if entity's type if different from that defined for list
// May change size of list.
inline bool sdiValueEntityList2::SetEntity(const sdiValueEntity& entity, const unsigned int i, const unsigned int j)
{
    if (p_entityFullType.IsValid())
    {
        if (entity.GetEntityFullType() != p_entityFullType)
        {
            return false;
        }
    }
    else
    {
        p_entityFullType = entity.GetEntityFullType();
    }

    SetId(i, j, entity.GetId());
    return true;
}

// Return sdiUIntList2 reference containing ids
inline sdiUIntList2& sdiValueEntityList2::GetList()
{
    return p_aaId;
}


#endif //! !defined(SDIVALUEENTITY__INCLUDED_)
