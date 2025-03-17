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

#include <sdiValuePtr.h>

#include <regex>
#include <boost/lexical_cast.hpp>


#define _LARGE_FLOAT 1.0e+100
#define _SMALL_FLOAT 1.0e-100
#define DEFAULT_CONVERSION_PRECISION 14
#define SPRINTF_STRING_SIZE   50
#define ALLOC_INIT_SIZE       256

static const char _format_d[] = "%d";
static const char _format_c[] = "%c";
static const char _format_u[] = "%u";


// String utilities

// Remove bounding braces frm string. Returns false if not bounded using {}
inline bool RemoveBoundingBraces(sdiString& str)
{
    using namespace std;

    // test for bounding brackets 
    size_t foundOpen  = str.find_first_of('{');
    size_t foundClose = str.find_last_of('}');
    if (foundOpen == string::npos || foundClose == string::npos)
    {
        return false;
    }

    str.erase(foundOpen, 1);
    str.erase(foundClose - 1, 1);
    return true;
}

// Split string into tokens
template<class TYPE>
static bool SplitStringIntoTokens(const sdiString& str, sdiVector<TYPE> & aToken)
{
    using namespace std;

    aToken.resize(0);

    char* cstr = new char [str.size()+1];
    strcpy (cstr, str.c_str());

    
    bool returnVal = true;
    char*  p = strtok(cstr," ");
    while (p != NULL)
    {
        std::istringstream i(p);
        TYPE x;
        if (!(i >> x))
        {
            returnVal = false;
            break;
        }
        aToken.push_back(x);

        p = strtok(NULL, " ");
    }
    delete[] cstr;

    return returnVal;
}

static bool _SplitStringIntoTokens(const sdiString& str, sdiVector<sdiString> & aToken)
{    
    using namespace std;

    aToken.resize(0);

    char* cstr = new char [str.size()+1];
    strcpy (cstr, str.c_str());

    
    char*  p = strtok(cstr," ");
    while (p != NULL)
    {
        aToken.push_back(p);

        p = strtok(NULL, " ");
    }
    delete[] cstr;

    return true;
}

template<class TYPE>
bool ConvertToNumber(const sdiString& string, TYPE& number)
{
    std::istringstream i(string);
    TYPE x;
    if (!(i >> x))
    {
        return false;
    }
    number = x;
    return true;
}


//
// Conversion of sdiValueEntity to and from string
//
static bool PrintTypeAndSubtypeToBuffer(const sdiValueEntityType entityFullType, char* buffer, bool& haveSubtype)
{
    haveSubtype = true;

    if (entityFullType.GetTypeNumeric() && entityFullType.GetSubtypeNumeric())
    {
        if (sprintf(buffer, "%d %d", entityFullType.GetTypeNumeric(), entityFullType.GetSubtypeNumeric()) < 0)
        {
            return false;
        }
    }
    else if (!entityFullType.GetTypeNamed().empty() && entityFullType.GetSubtypeNumeric())
    {
        if (sprintf(buffer, "%s %d", entityFullType.GetTypeNamed().c_str(), entityFullType.GetSubtypeNumeric()) < 0)
        {
            return false;
        }
    }
    else if (entityFullType.GetTypeNumeric() && !entityFullType.GetSubtypeNamed().empty())
    {
        if (sprintf(buffer, "%d %s", entityFullType.GetTypeNumeric(), entityFullType.GetSubtypeNamed().c_str()) < 0)
        {
            return false;
        }
    }
    else if (!entityFullType.GetTypeNamed().empty() && !entityFullType.GetSubtypeNamed().empty())
    {
        if (sprintf(buffer, "%s %s", entityFullType.GetTypeNamed().c_str(), entityFullType.GetSubtypeNamed().c_str()) < 0)
        {
            return false;
        }
    }
    else if (entityFullType.GetTypeNumeric())
    {
        if (sprintf(buffer, "%d", entityFullType.GetTypeNumeric()) < 0)
        {
            return false;
        }
        haveSubtype = false;
    }
    else
    {
        assert(!entityFullType.GetTypeNamed().empty());
        if (sprintf(buffer, "%s", entityFullType.GetTypeNamed().c_str()) < 0)
        {
            return false;
        }
        haveSubtype = false;
    }

    return true;
}


bool _GetEntityAsString(const sdiValueEntity& entity,
                        sdiString&     value,
                        const char*   format)
{
    const sdiValueEntityType entityFullType = entity.GetEntityFullType();
    if (!entityFullType.IsValid())
    {
        return false;
    }
    const unsigned int _id = entity.GetId();

    char buffer[SPRINTF_STRING_SIZE];

    if (format)
    {
        if (entityFullType.GetTypeNumeric() && entityFullType.GetSubtypeNumeric())
        {
            if (sprintf(buffer, format, entityFullType.GetTypeNumeric(), entityFullType.GetSubtypeNumeric(), _id) < 0)
            {
                return false;
            }
        }
        else if (!entityFullType.GetTypeNamed().empty() && entityFullType.GetSubtypeNumeric())
        {
            if (sprintf(buffer, format, entityFullType.GetTypeNamed().c_str(), entityFullType.GetSubtypeNumeric(), _id) < 0)
            {
                return false;
            }
        }
        else if (entityFullType.GetTypeNumeric() && !entityFullType.GetSubtypeNamed().empty())
        {
            if (sprintf(buffer, format, entityFullType.GetTypeNumeric(), entityFullType.GetSubtypeNamed().c_str(), _id) < 0)
            {
                return false;
            }
        }
        else if (!entityFullType.GetTypeNamed().empty() && !entityFullType.GetSubtypeNamed().empty())
        {
            if (sprintf(buffer, format, entityFullType.GetTypeNamed().c_str(), entityFullType.GetSubtypeNamed().c_str(), _id) < 0)
            {
                return false;
            }
        }
        else if (entityFullType.GetTypeNumeric())
        {
            if (sprintf(buffer, format, entityFullType.GetTypeNumeric(), _id) < 0)
            {
                return false;
            }
        }
        else
        {
            assert(!entityFullType.GetTypeNamed().empty());
            if (sprintf(buffer, format, entityFullType.GetTypeNamed().c_str(), _id) < 0)
            {
                return false;
            }
        }

        value = buffer;
    }
    else
    {
        bool haveSubtype;
        if (!PrintTypeAndSubtypeToBuffer(entityFullType, buffer, haveSubtype))
        {
            return false;
        }
        if (_id)
        {
            std::ostringstream tempstr;
            tempstr << _id;
            value = sdiString(buffer) + sdiString(" ") + tempstr.str();
        }
        else
        {
            value = sdiString(buffer);
        }
    }

    return true;
}

bool _SetEntityFromString(sdiValueEntity&         entity,
                          const sdiString& value,
                          const char*     format)
{
    //! Input value needs to have one or two entries for the entity type; primary and optionally sub
    unsigned int typeNumeric, subtypeNumeric;
    char typeNamed[SPRINTF_STRING_SIZE];
    char subtypeNamed[SPRINTF_STRING_SIZE];
    unsigned int _id;
    sdiValueEntityType entityFullType;

    if (format)
    {
        if (sscanf(value.c_str(), format, &typeNumeric, &subtypeNumeric, &_id) == 3)
        {
            entityFullType = sdiValueEntityType(typeNumeric, subtypeNumeric);
        }
        else if (sscanf(value.c_str(), format, &typeNumeric, subtypeNamed, &_id) == 3)
        {
            entityFullType = sdiValueEntityType(typeNumeric, subtypeNamed);
        }
        else if (sscanf(value.c_str(), format, typeNamed, &subtypeNumeric, &_id) == 3)
        {
            entityFullType = sdiValueEntityType(typeNamed, subtypeNumeric);
        }
        else if (sscanf(value.c_str(), format, typeNamed, subtypeNamed, &_id) == 3)
        {
            entityFullType = sdiValueEntityType(typeNamed, subtypeNamed);
        }
        else if (sscanf(value.c_str(), format, &typeNumeric, &_id) == 2)
        {
            entityFullType = sdiValueEntityType(typeNumeric);
        }
        else if (sscanf(value.c_str(), format, typeNamed, &_id) == 2)
        {
            entityFullType = sdiValueEntityType(typeNamed);
        }
        else
        {
            return false;
        }
    }
    else
    {
        if (sscanf(value.c_str(), "%d %d %d", &typeNumeric, &subtypeNumeric, &_id) == 3)
        {
            entityFullType = sdiValueEntityType(typeNumeric, subtypeNumeric);
        }
        else if (sscanf(value.c_str(), "%d %s %d", &typeNumeric, subtypeNamed, &_id) == 3)
        {
            entityFullType = sdiValueEntityType(typeNumeric, subtypeNamed);
        }
        else if (sscanf(value.c_str(), "%s %d %d", typeNamed, &subtypeNumeric, &_id) == 3)
        {
            entityFullType = sdiValueEntityType(typeNamed, subtypeNumeric);
        }
        else if (sscanf(value.c_str(), "%s %s %d", typeNamed, subtypeNamed, &_id) == 3)
        {
            entityFullType = sdiValueEntityType(typeNamed, subtypeNamed);
        }
        else if (sscanf(value.c_str(), "%d %d", &typeNumeric, &_id) == 2)
        {
            entityFullType = sdiValueEntityType(typeNumeric);
        }
        else if (sscanf(value.c_str(), "%s %d", typeNamed, &_id) == 2)
        {
            entityFullType = sdiValueEntityType(typeNamed);
        }
        else
        {
            return false;
        }
    }

    entity = sdiValueEntity(entityFullType, _id);
    return true;
}


bool _GetEntityListAsString(const sdiValueEntityList& entityList,
                            sdiString&         value,
                            const char*       format)
{
    const sdiValueEntityType entityFullType = entityList.GetEntityFullType();
    if (!entityFullType.IsValid())
    {
        return false;
    }

    if (format)
    {
        assert(0);  
    }

    char buffer[SPRINTF_STRING_SIZE];
    bool haveSubtype;
    if (!PrintTypeAndSubtypeToBuffer(entityFullType, buffer, haveSubtype))
    {
        return false;
    }
    const sdiString eType = haveSubtype
        ? "{" + sdiString(buffer) + "}" 
        : sdiString(buffer);

    sdiString list;
    if (haveSubtype) list += "{";
    const unsigned int count = entityList.GetIdListCount();
    for (unsigned int i = 0; i != count; ++i)
    {
        const unsigned int _id = entityList[i];

        std::ostringstream tempstr;
        tempstr << _id;

        if (i) list += sdiString(" ");
        list += tempstr.str();
    }
    if (haveSubtype) list += "}";
    if (list == "0")
    {
        value = eType;
    }
    else
    {
        value = eType + " " + list;
    }
    return true;
}


bool _GetEntityList2AsString(const sdiValueEntityList2& entityList2,
                             sdiString&          value,
                             const char*        format)
{
    const sdiValueEntityType entityFullType = entityList2.GetEntityFullType();
    if (!entityFullType.IsValid())
    {
        return false;
    }

    if (format)
    {
        assert(0);  
    }

    char buffer[SPRINTF_STRING_SIZE];
    bool haveSubtype;
    if (!PrintTypeAndSubtypeToBuffer(entityFullType, buffer, haveSubtype))
    {
        assert(0);
        return false;
    }
    const sdiString eType = haveSubtype
        ? "{" + sdiString(buffer) + "}" 
        : sdiString(buffer);

    sdiString list;
    if (haveSubtype) list += "{";    
    const sdiUIntList2& list2 = entityList2.GetList();
    for (unsigned int i = 0; i != (unsigned int)list2.size(); ++i)
    {
        if (i) list += sdiString(" ");
        list += "{";
        for (unsigned int j = 0; j != (unsigned int)list2[i].size(); ++j)
        {
            std::ostringstream tempstr;
            tempstr << list2[i][j];

            if (j) list += sdiString(" ");
            list += tempstr.str();
        }
        list += "}";
    }
    if (haveSubtype) list += "}";
    if (list == "0")
    {
        value = eType;
    }
    else
    {
        value = eType + " " + list;
    }
    return true;
}


bool _SetEntityListFromString(sdiValueEntityList&     entityList,
                              const sdiString& value,
                              const char*     format)
{
#if 0 // to be implemented (this code is just copied from hwDescriptor
    sdiString str = value;
    RemoveBoundingBraces(str);

    hwStlVector< std::pair<size_t, size_t> > aBracePair;    // last pair bounds ids, first (optional) is entityType and sub
    bool haveBraces = false;
    if (FindMatchingBraces(str, aBracePair))
    {
        haveBraces = true;    
        if (aBracePair.size() < 1 || aBracePair.size() > 2)
        {
            assert(0);
            return false;
        }
    }
    else
    {
        // string contains only entity type or unbalanced pairs        
        hwStlVector<sdiString> aToken;
        if (SplitStringIntoTokens<sdiString>(str, aToken))
        {
            if (aToken.size() == 1)
            {
                sdiValueEntityType entityFullType;
                if (FormEntityFullTypeFromString(entityFullType, aToken[0]))
                {
                    entityList = sdiValueEntityList(entityFullType, hwUIntList());         
                    return true;
                }
            }
        }     

        //assert(0);
        //return false;
    }

    sdiString eTypeString;     // String containing etype
    sdiString idListString;    // String containing ids
    sdiValueEntityType entityFullType;
    hwStlVector<unsigned int> aId;

    if (!haveBraces)
    {
        hwStlVector<sdiString> aToken;
        if (!SplitStringIntoTokens<sdiString>(str, aToken))
        {
            assert(0);
            return false;
        }

        // Decode string containing etype. First decode it into strings. Then try to make any of the tokens into unsigned ints
        if (!FormEntityFullTypeFromString(entityFullType, aToken[0]))
        {
            assert(0);
            return false;
        }

        // ids
        aId.reserve(aToken.size());
        for (size_t i = 1; i < aToken.size(); i++)
        {
            unsigned int id = 0;
            ConvertToNumber<unsigned int>(aToken[i].c_str(),id);
            if (!id)
            {
                if (aToken[i] != "0")
                {
                    assert(0);
                    return false;
                }
            }
            aId.push_back(id);
        }        
    }
    else
    {
        if (aBracePair.size() == 1)
        {
            CopyPartOfString(str, eTypeString,  0, aBracePair[0].first - 1);
            CopyPartOfString(str, idListString, aBracePair[0].first + 1, aBracePair[0].second - 1);
        }
        else
        {
            CopyPartOfString(str, eTypeString,  aBracePair[0].first + 1, aBracePair[0].second - 1);
            CopyPartOfString(str, idListString, aBracePair[1].first + 1, aBracePair[1].second - 1);
        }

        // Decode string containing etype. First decode it into strings. Then try to make any of the tokens into unsigned ints
        if (!FormEntityFullTypeFromString(entityFullType, eTypeString))
        {
            assert(0);
            return false;
        }    

        // Decode string containing ids
        if (!SplitStringIntoTokens<unsigned int>(idListString, aId))
        {
            assert(0);
            return false;
        }
    }

    entityList = sdiValueEntityList(entityFullType, aId);         
    return true;
#else
    return false;
#endif
}


bool _SetEntityList2FromString(sdiValueEntityList2&    entityList,
                               const sdiString& value,
                               const char*     format)
{
#if 0 // to be implemented (this code is just copied from hwDescriptor
    sdiString str = value;
    RemoveBoundingBraces(str);  // remove outer braces


                                // Case with subtype:    {{type subtype} {{A B} {C D} {E F}}} - including outer braces
                                // Case without subtype: {{type} {{A B} {C D} {E F}}} or {type {{A B} {C D} {E F}}} - including outer braces

    hwStlVector< std::pair<size_t, size_t> > aBracePair;    // last pair bounds ids, first (optional) is entityType and sub
    bool haveBraces = false;
    if (FindMatchingBraces(str, aBracePair))
    {
        // Need at least 2 pairs of matching braces
        if (aBracePair.size() < 2)
        {
            assert(0);
            return false;
        }
    }
    else
    {
        // string contains only entity type or unbalanced pairs        
        hwStlVector<sdiString> aToken;
        if (SplitStringIntoTokens<sdiString>(str, aToken))
        {
            if (aToken.size() == 1)
            {
                sdiValueEntityType entityFullType;
                if (FormEntityFullTypeFromString(entityFullType, aToken[0]))
                {
                    entityList = sdiValueEntityList2(entityFullType, sdiUIntList2());         
                    return true;
                }
            }
        }     

        assert(0);
        return false;
    }

    // If first pair of braces ends before second starts, have braces for type (or optionally subtype).
    // Of not, cannot even have subtype
    const bool haveTypeBrances = aBracePair[0].second < aBracePair[1].first ? true : false;

    // Figure out type and subtype
    sdiString eTypeString;     // String containing etype
    if (haveTypeBrances)
    {
        if (!CopyPartOfString(str, eTypeString, aBracePair[0].first + 1, aBracePair[0].second - 1))
        {
            assert(0);
            return false;
        }
    }
    else
    {
        if (!CopyPartOfString(str, eTypeString, 0, aBracePair[0].first - 1))
        {
            assert(0);
            return false;
        }
    }
    // Decode string containing etype. First decode it into strings. Then try to make any of the tokens into unsigned ints
    sdiValueEntityType entityFullType;
    if (!FormEntityFullTypeFromString(entityFullType, eTypeString))
    {
        return false;
    }


    // Deal with id list

    sdiString idList2String;    // String containing 2-d list of ids
    if (!CopyPartOfString(str, idList2String, aBracePair[haveTypeBrances ? 1 : 0].first, str.size()-1))
    {
        assert(0);
        return false;
    }
    RemoveBoundingBraces(idList2String);  // remove outer braces

                                          // Decode string containing id 1-d lists
    hwStlVector<sdiString> aIdListString;
    if (!SplitStringIntoTokens<sdiString>(idList2String, aIdListString))
    {
        assert(0);
        return false;
    }
    sdiUIntList2 aaId;
    aaId.reserve(aIdListString.size());

    hwStlVector<unsigned int> aToken;
    for (size_t i = 0; i != aIdListString.size(); ++i)
    { 
        if (!SplitStringIntoTokens<unsigned int>(aIdListString[i], aToken))
        {
            assert(0);
            return false;
        }

        // All must be of same size
        if (i)
        {
            if (aToken.size() != aaId[0].size())
            {
                assert(0);
                return false;
            }
        }

        aaId.push_back(aToken);
    }


    entityList.SetEntityFullType(entityFullType);
    entityList.GetList() = aaId;
    return true;
#else
    return false;
#endif
}


// *********************************************************************************
// sdiValuePtrPrivateArray0T Definition
// *********************************************************************************
template <class VALUE, sdiBasicType BASIC_TYPE, sdiCompoundType COMPOUND_TYPE>
class sdiValuePtrPrivateArray0T : public sdiValuePtr
{
public:

    sdiValuePtrPrivateArray0T(const VALUE& value)
        : sdiValuePtr(), p_value(value)
    {
    }

    sdiValuePtrPrivateArray0T(const sdiString& value,
                           const char*     format)
        : sdiValuePtr()
    {
        this->SetValueFromString(value, format);
    }

    sdiValuePtrPrivateArray0T(const sdiValuePtrPrivateArray0T& other)
        : sdiValuePtr(),
          p_value(other.p_value)
    {
    }

    ~sdiValuePtrPrivateArray0T()
    {
    }

    //! Creates clone of value
    virtual sdiValuePtr* Clone() const
    {
        return new sdiValuePtrPrivateArray0T(*this);
    }


    ////////////////////////////////////////////////////////////////////////////////////
    //! Query
    ////////////////////////////////////////////////////////////////////////////////////

    //! Basic type
    virtual sdiBasicType GetBasicType() const
    {
        return BASIC_TYPE;
    }

    //! Compound type
    virtual sdiCompoundType GetCompoundType() const
    {
        return COMPOUND_TYPE;
    }

    //! Array dimension; 0 if not an array.
    virtual unsigned int GetArrayDimension() const
    {
        return 0;
    }

    //! Length of Array for specified dimension
    unsigned int GetArrayDimensionExtent(const unsigned int dimensionIndex) const
    {
        return 0;
    }

    //! Get sdiValueEntityType; returns an invalid object if sdiValue does not correspond
    //! to an sdiValueEntity, sdiValueEntityList, or Qualifier (not QualifierList).
    virtual sdiValueEntityType GetEntityFullType() const
    {
        return sdiValueEntityType();
    }

    //! Comparison with other
    virtual bool Equals(const sdiValuePtr* pOther) const;
    virtual bool LessThan(const sdiValuePtr* pOther) const;


    ////////////////////////////////////////////////////////////////////////////////////
    //! Get value(s)
    ////////////////////////////////////////////////////////////////////////////////////

    //! Get single values, including those stored in array. Funtion is limited to for
    //! scalar and 1-d (i, 0) and 2-d (i, j) dimensional arrays; i = j = 0 for scalars.
    //! Returns false if of inappropriate type, or if seeking value outside array bounds.
    virtual bool GetValue(VALUE&             value,
                          const unsigned int i = 0,
                          const unsigned int j = 0) const
    {
        value = p_value;
        return true;
    }

    //! Get values, including those stored in array (using same approach as above). sdiValue is returned
    //! as formatted string. The format argument is necessary if seek value formatted in particular form;
    //! otherwise, a default formating is provided: a space " " separate compound values, with parenthesis
    //! "{}" used to separate each row of matrices.
    //! For arrays, can get entire array per the format below if specify i = UINT_MAX.
    //!     1-d standard array:  { item1 item2 item3 ... }
    //!     2-d standard array:  {{item11 item12 item13 ...} {item21 item22 item23 ...} ...}
    //!     1-d entity array:    {{type with optional subtype} id1 id2 id3} Alternatively, {type id1 id2 id3} if not subtype specified  note skipping internal {}s
    //!     2-d entity array:    {{type with optional subtype} { id1 id2 id3} {id5 id4 id6}} Alternatively, {type { id1 id2 id3} {id5 id4 id6}} note skipping internal {}s
    //!         where type and sub-type can be numeric or named
    inline virtual bool GetValueAsString(sdiString&          value,
                                         const char*        format = 0,
                                         const unsigned int i = 0,
                                         const unsigned int j = 0) const;


    
    inline virtual std::size_t GetValueMemoryUsage() const;

    //! Can also retrive sdiValue if form of const pointer; note how null pointer is returned for
    //! type inconsistent with sdiValue

    inline const bool* GetValuePointerBool(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }

    inline const char* GetValuePointerChar(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }

    inline const int* GetValuePointerInt(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }

    inline const unsigned int* GetValuePointerUInt(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }

    inline const double* GetValuePointerDouble(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }

    inline const sdiString* GetValuePointerString(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }

    inline const sdiTriple* GetValuePointerTriple(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }

    inline const sdiValueEntity* GetValuePointerEntity(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }


    ////////////////////////////////////////////////////////////////////////////////////
    //! Set value(s)
    ////////////////////////////////////////////////////////////////////////////////////

    //! Set single values, including those stored in array. Funtion is limited to for
    //! scalar and 1-d (i, 0) and 2-d (i, j) dimensional arrays; i = j = 0 for scalars.
    //! Function returns false only for entity value type for case of inconsistent type
    //! that what was already in list.
    virtual bool SetValue(const VALUE&       value,
                          const unsigned int i = 0,
                          const unsigned int j = 0)
    {
        p_value = value;
        return false;
    }


    //! Set values, including those stored in array (using same approach as above).
    //! The format argument is necessary if have value formatted in particular form; otherwise,
    //! a default formating is expected: a space " " separate compound values, with parenthesis
    //! "{}" used to separate each row of matrices.
    //! For arrays, can get entire array per the format below if specify i = UINT_MAX.
    //!     1-d standard array:  { item1 item2 item3 ... }
    //!     2-d standard array:  {{item11 item12 item13 ...} {item21 item22 item23 ...} ...}
    //!     1-d entity array:    {{type with optional subtype} id1 id2 id3} Alternatively, {type id1 id2 id3} if not subtype specified  note skipping internal {}s
    //!     2-d entity array:    {{type with optional subtype} { id1 id2 id3} {id5 id4 id6}} Alternatively, {type { id1 id2 id3} {id5 id4 id6}}  note skipping internal {}s
    //!         where type and sub-type can be numeric or named
    inline virtual bool SetValueFromString(const sdiString&    value,
                                           const char*        format = 0,
                                           const unsigned int i = 0,
                                           const unsigned int j = 0);


    //! Location of memory location where data is stored.
    inline virtual const void* GetMemoryLocation(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return &p_value;
    }
    inline virtual       void* GetMemoryLocation(const unsigned int i = 0, const unsigned int j = 0)
    {
        return &p_value;
    }



private:

    VALUE p_value;
};



// *********************************************************************************
// sdiValuePtrPrivateArray0T Implementation
// *********************************************************************************

template <class VALUE, sdiBasicType BASIC_TYPE, sdiCompoundType COMPOUND_TYPE>
inline bool sdiValuePtrPrivateArray0T<VALUE, BASIC_TYPE, COMPOUND_TYPE>::Equals(const sdiValuePtr* pOther) const
{
    // Types must match
    if (!this->P_EqualTypes(pOther))
    {
        return false;
    }

    VALUE v1, v2;

    if (!(this->GetValue(v1)))
    {
        assert(0);
        return false;
    }

    if (!(pOther->GetValue(v2)))
    {
        assert(0);
        return false;
    }
    
    return (v1 == v2) ? true : false;
}

template <class VALUE, sdiBasicType BASIC_TYPE, sdiCompoundType COMPOUND_TYPE>
inline bool sdiValuePtrPrivateArray0T<VALUE, BASIC_TYPE, COMPOUND_TYPE>::LessThan(const sdiValuePtr* pOther) const
{
    // Types must match
    if (!this->P_EqualTypes(pOther))
    {
        return false;
    }

    VALUE v1, v2;

    if (!(this->GetValue(v1)))
    {
        assert(0);
        return false;
    }

    if (!(pOther->GetValue(v2)))
    {
        assert(0);
        return false;
    }

    return (v1 < v2) ? true : false;
}


// Some types do not support <operator
template <>
inline bool sdiValuePtrPrivateArray0T<sdiValueEntity,         BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::LessThan(const sdiValuePtr* pOther) const
{
    return false;
}

static inline bool GetDoubleAsString(const double doubleVal,
                                     const char*  format,
                                     char*        buffer)
{
    if ((int)doubleVal == doubleVal)
    {
        if (sprintf(buffer, "%d", (int)doubleVal) < 0)
        {
            return false;
        }
    }
    else
    {
        if (sprintf(buffer, format, doubleVal) < 0)
        {
            return false;
        }
    }
    return true;
}

// Get values, including those stored in array (using same approach as above). sdiValue is returned
// as formatted string. The format argument is necessary if seek value formatted in particular form;
// otherwise, a default formating is provided: a space " " separate compound values, with parenthesis
// "{}" used to separate each row of matrices.
//! For arrays, can get entire array per the format below if specify i = UINT_MAX.
//!     1-d standard array:  { item1 item2 item3 ... }
//!     2-d standard array:  {{item11 item12 item13 ...} {item21 item22 item23 ...} ...}
//!     1-d entity array:    {{type with optional subtype} id1 id2 id3} Alternatively, {type id1 id2 id3} if not subtype specified  note skipping internal {}s
//!     2-d entity array:    {{type with optional subtype} { id1 id2 id3} {id5 id4 id6}} Alternatively, {type { id1 id2 id3} {id5 id4 id6}} note skipping internal {}s
//!         where type and sub-type can be numeric or named
template <class VALUE, sdiBasicType BASIC_TYPE, sdiCompoundType COMPOUND_TYPE>
inline bool sdiValuePtrPrivateArray0T<VALUE, BASIC_TYPE, COMPOUND_TYPE>::GetValueAsString(
    sdiString&          value,
    const char*        format,
    const unsigned int i,
    const unsigned int j) const
{
    //! Need template specialization for all but some SINGLE types
    assert(COMPOUND_TYPE == COMPOUND_TYPE_SINGLE);

    char buffer[SPRINTF_STRING_SIZE];

    if (format)
    {
        if (sprintf(buffer, format, p_value) < 0)
        {
            return false;
        }
    }
    else
    {
        switch (BASIC_TYPE)
        {
        case BASIC_TYPE_INT:
            if (sprintf(buffer, _format_d, p_value) < 0)
            {
                return false;
            }
            break;

        case BASIC_TYPE_CHAR:
            if (sprintf(buffer, _format_c, p_value) < 0)
            {
                return false;
            }
            break;

        case BASIC_TYPE_UINT:
            if (sprintf(buffer, _format_u, p_value) < 0)
            {
                return false;
            }
            break;

        case BASIC_TYPE_DOUBLE:
            {
                char precisionFormat[128];
                sprintf(precisionFormat, "%%.%dg", DEFAULT_CONVERSION_PRECISION);
                if ( !GetDoubleAsString(p_value, precisionFormat, buffer) )
                {
                    return false;
                }
            }
            break;

        default:
            assert(0);
            return false;
        }
    }

    value = buffer;
    return true;
}

// Need template specialization for all but some SINGLE types

// Get sdiValueEntityType; returns an invalid object if sdiValue does not correspond
// to an sdiValueEntity, sdiValueEntityList, or Qualifier (not QualifierList).
template <>
inline sdiValueEntityType sdiValuePtrPrivateArray0T<sdiValueEntity, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetEntityFullType() const
{
    return p_value.GetEntityFullType();
}

template <>
inline bool sdiValuePtrPrivateArray0T<bool, BASIC_TYPE_BOOL, COMPOUND_TYPE_SINGLE>::GetValueAsString(
    sdiString&          value,
    const char*        format,
    const unsigned int i,
    const unsigned int j) const
{
    char buffer[SPRINTF_STRING_SIZE];
    const sdiString _value = p_value ? "true" : "false";

    if (format)
    {
        if (sprintf(buffer, format, _value.c_str()) < 0)
        {
            return false;
        }
        value = buffer;
    }
    else
    {
        value = _value;
    }
    return true;
}

template <>
inline bool sdiValuePtrPrivateArray0T<sdiString, BASIC_TYPE_STRING, COMPOUND_TYPE_SINGLE>::GetValueAsString(
    sdiString&          value,
    const char*        format,
    const unsigned int i,
    const unsigned int j) const
{
    value = p_value;
    const std::string::size_type zeroPos = value.find((char)0);
    if (zeroPos != std::string::npos)
    {
        if (!zeroPos)
        {
            value = "";
        }
        else
        {
            value.resize(zeroPos - 1);
        }
    }
    return true;
}

template <>
inline bool sdiValuePtrPrivateArray0T<sdiTriple, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>::GetValueAsString(
    sdiString&          value,
    const char*        format,
    const unsigned int i,
    const unsigned int j) const
{
    char buffer[128];
    if (format)
    {
        if (sprintf(buffer, format, p_value.GetX(), p_value.GetY(), p_value.GetZ()) < 0)
        {
            return false;
        }
    }
    else
    {
        value.clear();

        char precisionFormat[128];
        sprintf(precisionFormat, "%%.%dg", DEFAULT_CONVERSION_PRECISION);

        GetDoubleAsString(p_value.GetX(), precisionFormat, buffer);

        value += buffer;
        value += " ";

        GetDoubleAsString(p_value.GetY(), precisionFormat, buffer);
        value += buffer;
        value += " ";

        GetDoubleAsString(p_value.GetZ(), precisionFormat, buffer);
        value += buffer;
        return true;
    }
    value = buffer;
    return true;
}

template <>
inline bool sdiValuePtrPrivateArray0T<sdiValueEntity, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetValueAsString(
    sdiString&          value,
    const char*        format,
    const unsigned int i,
    const unsigned int j) const
{
    const sdiValueEntity& _entity = p_value;
    return _GetEntityAsString(_entity, value, format);
}

// Set values, including those stored in array (using same approach as above).
// The format argument is necessary if have value formatted in particular form; otherwise,
// a default formating is expected: a space " " separate compound values, with parenthesis
// "{}" used to separate each row of matrices.
//! For arrays, can get entire array per the format below if specify i = UINT_MAX.
//!     1-d standard array:  { item1 item2 item3 ... }
//!     2-d standard array:  {{item11 item12 item13 ...} {item21 item22 item23 ...} ...}
//!     1-d entity array:    {{type with optional subtype} id1 id2 id3} Alternatively, {type id1 id2 id3} if not subtype specified  note skipping internal {}s
//!     2-d entity array:    {{type with optional subtype} { id1 id2 id3} {id5 id4 id6}} Alternatively, {type { id1 id2 id3} {id5 id4 id6}}  note skipping internal {}s
//!         where type and sub-type can be numeric or named
template <class VALUE, sdiBasicType BASIC_TYPE, sdiCompoundType COMPOUND_TYPE>
inline bool sdiValuePtrPrivateArray0T<VALUE, BASIC_TYPE, COMPOUND_TYPE>::SetValueFromString(
    const sdiString&    value,
    const char*        format,
    const unsigned int i,
    const unsigned int j)
{
    //! Need template specialization for all but some SINGLE types
    assert(COMPOUND_TYPE == COMPOUND_TYPE_SINGLE);

    VALUE _value;

    if (format)
    {
        if (sscanf(value.c_str(), format, &_value) != 1)
        {
            return false;
        }
    }
    else
    {
        switch (BASIC_TYPE)
        {
        case BASIC_TYPE_CHAR:
        case BASIC_TYPE_INT:
        case BASIC_TYPE_UINT:
        case BASIC_TYPE_DOUBLE:
            if (!ConvertToNumber<VALUE>(value, _value))
            {
                return false;
            }
            break;

        default:
            assert(0);
            return false;
        }
    }
    this->SetValue(_value);
    return true;
}

// Need template specialization for all but some SINGLE types

template <>
inline bool sdiValuePtrPrivateArray0T<bool, BASIC_TYPE_BOOL, COMPOUND_TYPE_SINGLE>::SetValueFromString(
    const sdiString&    value,
    const char*        format,
    const unsigned int i,
    const unsigned int j)
{
    char str[SPRINTF_STRING_SIZE];

    if (format)
    {
        if (sscanf(value.c_str(), format, str) != 1)
        {
            return false;
        }
    }
    else
    {
        if (sscanf(value.c_str(), "%s", str) != 1)
        {
            return false;
        }
    }

    sdiString _stringValue(str);
    //_stringValue.tolower();
    const bool _value = (_stringValue == "true" || _stringValue == "1") ? true : false;
    this->SetValue(_value);
    return true;
}

template <>
inline bool sdiValuePtrPrivateArray0T<sdiString, BASIC_TYPE_STRING, COMPOUND_TYPE_SINGLE>::SetValueFromString(
    const sdiString&    value,
    const char*        format,
    const unsigned int i,
    const unsigned int j)
{
    this->SetValue(value);
    return true;
}

template <>
inline bool sdiValuePtrPrivateArray0T<sdiTriple, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>::SetValueFromString(
    const sdiString&    value,
    const char*        format,
    const unsigned int i,
    const unsigned int j)
{
    double _value[3];
    sdiString str = value;
    RemoveBoundingBraces(str);
    if (format)
    {
        if (sscanf(str.c_str(), format, &_value[0], &_value[1], &_value[2]) != 3)
        {
            return false;
        }
    }
    else
    {
        if (sscanf(str.c_str(), "%lf %lf %lf", &_value[0], &_value[1], &_value[2]) != 3)
        {
            return false;
        }
    }
    this->SetValue(sdiTriple(_value[0], _value[1], _value[2]));
    return true;
}

template <typename ElemT>
struct HexTo {
    ElemT value;
    operator ElemT() const { return value; }
    friend std::istream& operator >> (std::istream& in, HexTo& out) {
        in >> std::hex >> out.value;
        return in;
    }
};

template <>
inline bool sdiValuePtrPrivateArray0T<sdiValueEntity, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::SetValueFromString(
    const sdiString&    value,
    const char*        format,
    const unsigned int i,
    const unsigned int j)
{
    sdiValueEntity _entity;
    if (!_SetEntityFromString(_entity, value, format))
    {
        return false;
    }

    this->SetValue(_entity);
    return true;
}

template <>
inline const void* sdiValuePtrPrivateArray0T<sdiValueEntity, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetMemoryLocation(
    const unsigned int i, const unsigned int) const
{
    return &p_value.GetIdRef();
}

template <>
inline void* sdiValuePtrPrivateArray0T<sdiValueEntity, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetMemoryLocation(
    const unsigned int i, const unsigned int)
{
    return &p_value.GetIdRef();
}


template <>
inline const bool* sdiValuePtrPrivateArray0T<bool, BASIC_TYPE_BOOL, COMPOUND_TYPE_SINGLE>::GetValuePointerBool(const unsigned int i, const unsigned int j) const
{
    return &p_value;
}

template <>
inline const char* sdiValuePtrPrivateArray0T<char, BASIC_TYPE_CHAR, COMPOUND_TYPE_SINGLE>::GetValuePointerChar(const unsigned int i, const unsigned int j) const
{
    return &p_value;
}

template <>
inline const int* sdiValuePtrPrivateArray0T<int, BASIC_TYPE_INT, COMPOUND_TYPE_SINGLE>::GetValuePointerInt(const unsigned int i, const unsigned int j) const
{
    return &p_value;
}

template <>
inline const unsigned int* sdiValuePtrPrivateArray0T<unsigned int, BASIC_TYPE_UINT, COMPOUND_TYPE_SINGLE>::GetValuePointerUInt(const unsigned int i, const unsigned int j) const
{
    return &p_value;
}

template <>
inline const double* sdiValuePtrPrivateArray0T<double, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_SINGLE>::GetValuePointerDouble(const unsigned int i, const unsigned int j) const
{
    return &p_value;
}

template <>
inline const sdiString* sdiValuePtrPrivateArray0T<sdiString, BASIC_TYPE_STRING, COMPOUND_TYPE_SINGLE>::GetValuePointerString(const unsigned int i, const unsigned int j) const
{
    return &p_value;
}

template <>
inline const sdiTriple* sdiValuePtrPrivateArray0T<sdiTriple, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>::GetValuePointerTriple(const unsigned int i, const unsigned int j) const
{
    return &p_value;
}

template <>
inline const sdiValueEntity* sdiValuePtrPrivateArray0T<sdiValueEntity, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetValuePointerEntity(const unsigned int i, const unsigned int j) const
{
    return &p_value;
}

// *********************************************************************************
// sdiValuePtrPrivateArray1T Definition
// *********************************************************************************
template <class VALUE, class VALUE_LIST, sdiBasicType BASIC_TYPE, sdiCompoundType COMPOUND_TYPE>
class ValuePtrPrivateArray1T : public sdiValuePtr
{
public:

    ValuePtrPrivateArray1T(const VALUE_LIST&   aValue)
        : sdiValuePtr(),
          p_aValue(aValue)
    {
    }
    
    ValuePtrPrivateArray1T(const unsigned int  sizeDimFirst)
        : sdiValuePtr()
    {
        p_aValue.reserve(sizeDimFirst);
    }

    ValuePtrPrivateArray1T(const ValuePtrPrivateArray1T& other)
        : sdiValuePtr(),
          p_aValue(other.p_aValue)
    {
    }

    ValuePtrPrivateArray1T(VALUE_LIST&&   aValue)
        : sdiValuePtr(),
          p_aValue(std::move(aValue))
    {
    }

    ValuePtrPrivateArray1T(ValuePtrPrivateArray1T&& other)
        : sdiValuePtr()
    {
        if (other.p_aValue.IsMemoryExternallyManaged())
            p_aValue = other.p_aValue;            // copy
        else
            p_aValue = std::move(other.p_aValue); // move
    }

    ~ValuePtrPrivateArray1T()
    {
    }

    //! Creates clone of value
    virtual sdiValuePtr* Clone() const
    {
        return new ValuePtrPrivateArray1T(*this);
    }


    ////////////////////////////////////////////////////////////////////////////////////
    //! Query
    ////////////////////////////////////////////////////////////////////////////////////

    //! Basic type
    virtual sdiBasicType GetBasicType() const
    {
        return BASIC_TYPE;
    }

    //! Compound type
    virtual sdiCompoundType GetCompoundType() const
    {
        return COMPOUND_TYPE;
    }

    //! Array dimension; 0 if not an array.
    virtual unsigned int GetArrayDimension() const
    {
        return 1;
    }

    //! Length of Array for specified dimension
    unsigned int GetArrayDimensionExtent(const unsigned int dimensionIndex) const
    {
        return (unsigned int)p_aValue.size();
    }

    //! Get sdiValueEntityType; returns an invalid object if sdiValue does not correspond
    //! to an sdiValueEntity, sdiValueEntityList, or Qualifier (not QualifierList).
    virtual sdiValueEntityType GetEntityFullType() const
    {
        return sdiValueEntityType();
    }

    //! Comparison with other
    virtual bool Equals(const sdiValuePtr* pOther) const;


    ////////////////////////////////////////////////////////////////////////////////////
    //! Get value(s)
    ////////////////////////////////////////////////////////////////////////////////////

    //! Get single values, including those stored in array. Funtion is limited to for
    //! scalar and 1-d (i, 0) and 2-d (i, j) dimensional arrays; i = j = 0 for scalars.
    //! Returns false if of inappropriate type, or if seeking value outside array bounds.
    virtual bool GetValue(VALUE&             value,
                          const unsigned int i,
                          const unsigned int j = 0) const
    {
        if (i >= p_aValue.size())
        {
            assert(0);
            return false;
        }
        value = p_aValue[i];
        return true;
    }


        //! Get values, including those stored in array (using same approach as above). sdiValue is returned
    //! as formatted string. The format argument is necessary if seek value formatted in particular form;
    //! otherwise, a default formating is provided: a space " " separate compound values, with parenthesis
    //! "{}" used to separate each row of matrices.
    //! For arrays, can get entire array per the format below if specify i = UINT_MAX.
    //!     1-d standard array:  { item1 item2 item3 ... }
    //!     2-d standard array:  {{item11 item12 item13 ...} {item21 item22 item23 ...} ...}
    //!     1-d entity array:    {{type with optional subtype} id1 id2 id3} Alternatively, {type id1 id2 id3} if not subtype specified  note skipping internal {}s
    //!     2-d entity array:    {{type with optional subtype} { id1 id2 id3} {id5 id4 id6}} Alternatively, {type { id1 id2 id3} {id5 id4 id6}} note skipping internal {}s
    //!         where type and sub-type can be numeric or named
    inline virtual bool GetValueAsString(sdiString&          value,
                                         const char*        format,
                                         const unsigned int i,
                                         const unsigned int j = 0) const;

    
    inline virtual std::size_t GetValueMemoryUsage() const;

    //! For array
    virtual bool GetValue(VALUE_LIST& aValue) const
    {
        aValue = p_aValue;
        return true;
    }


    //! Can also retrive sdiValue if form of const pointer; note how null pointer is returned for
    //! type inconsistent with sdiValue

    inline const char* GetValuePointerChar(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }

    inline const int* GetValuePointerInt(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }

    inline const unsigned int* GetValuePointerUInt(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }

    inline const double* GetValuePointerDouble(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }

    inline const sdiString* GetValuePointerString(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }

    inline const sdiTriple* GetValuePointerTriple(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }

    inline const sdiValueEntity* GetValuePointerEntity(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }


    //! 1-d arrays
    virtual const sdiBoolList* GetValuePointerBoolList() const
    {
        return 0;
    }

    virtual const sdiCharList* GetValuePointerCharList() const
    {
        return 0;
    }

    virtual const sdiIntList* GetValuePointerIntList() const
    {
        return 0;
    }

    virtual const sdiUIntList* GetValuePointerUIntList() const
    {
        return 0;
    }

    virtual const sdiDoubleList* GetValuePointerDoubleList() const
    {
        return 0;
    }

    virtual const sdiStringList* GetValuePointerStringList() const
    {
        return 0;
    }

    virtual const sdiTripleList* GetValuePointerTripleList() const
    {
        return 0;
    }

    virtual const sdiValueEntityList* GetValuePointerEntityList() const
    {
        return 0;
    }


    ////////////////////////////////////////////////////////////////////////////////////
    //! Set value(s)
    ////////////////////////////////////////////////////////////////////////////////////

    //! Set single values, including those stored in array. Funtion is limited to for
    //! scalar and 1-d (i, 0) and 2-d (i, j) dimensional arrays; i = j = 0 for scalars.
    //! Function returns false only for entity value type for case of inconsistent type
    //! that what was already in list.
    virtual bool SetValue(const VALUE&          value, const unsigned int i, const unsigned int j = 0)
    {
        if (i >= p_aValue.size())
        {
            p_aValue.resize(i + 1);
        }
        p_aValue[i] = value;
        return true;
    }


        //! Set values, including those stored in array (using same approach as above).
    //! The format argument is necessary if have value formatted in particular form; otherwise,
    //! a default formating is expected: a space " " separate compound values, with parenthesis
    //! "{}" used to separate each row of matrices.
    //! For arrays, can get entire array per the format below if specify i = UINT_MAX.
    //!     1-d standard array:  { item1 item2 item3 ... }
    //!     2-d standard array:  {{item11 item12 item13 ...} {item21 item22 item23 ...} ...}
    //!     1-d entity array:    {{type �with optional subtype�} id1 id2 id3} Alternatively, {type id1 id2 id3} if not subtype specified � note skipping internal {}�s
    //!     2-d entity array:    {{type �with optional subtype�} { id1 id2 id3} {id5 id4 id6}} Alternatively, {type { id1 id2 id3} {id5 id4 id6}}  � note skipping internal {}�s
    //!         where type and sub-type can be numeric or named
    inline virtual bool SetValueFromString(const sdiString&    value,
                                           const char*        format,
                                           const unsigned int i,
                                           const unsigned int j = 0);

    //! For array
    virtual bool SetValue(const VALUE_LIST& aValue)
    {
        p_aValue = aValue;
        return true;
    }


    //! Location of memory location where data is stored.
    virtual const void* GetMemoryLocation(const unsigned int i, const unsigned int j = 0) const
    {
        if (i >= p_aValue.size())
        {
            assert(0);
            return 0;
        }
        return &p_aValue[i];
    }
    virtual       void* GetMemoryLocation(const unsigned int i, const unsigned int j = 0)
    {
        if (i >= p_aValue.size())
        {
            assert(0);
            return 0;
        }
        return &p_aValue[i];
    }



private:

    VALUE_LIST p_aValue;
};


// *********************************************************************************
// sdiValuePtrPrivateArray1T Implementation
// *********************************************************************************

template <class VALUE, class VALUE_LIST, sdiBasicType BASIC_TYPE, sdiCompoundType COMPOUND_TYPE>
inline bool ValuePtrPrivateArray1T<VALUE, VALUE_LIST, BASIC_TYPE, COMPOUND_TYPE>::Equals(const sdiValuePtr* pOther) const
{
    // Types must match
    if (!this->P_EqualTypes(pOther))
    {
        return false;
    }

    VALUE_LIST v1, v2;

    if (!(this->GetValue(v1)))
    {
        assert(0);
        return false;
    }

    if (!(pOther->GetValue(v2)))
    {
        assert(0);
        return false;
    }

    return (v1 == v2) ? true : false;
}


template <>
ValuePtrPrivateArray1T<sdiValueEntity, sdiValueEntityList, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::ValuePtrPrivateArray1T(
    const unsigned int  sizeDimFirst)
    : sdiValuePtr()
{
    p_aValue.Reserve(sizeDimFirst);
}

template <>
inline unsigned int ValuePtrPrivateArray1T<sdiValueEntity, sdiValueEntityList, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::
GetArrayDimensionExtent(const unsigned int dimensionIndex) const
{
    return p_aValue.GetIdListCount();
}

template <>
inline bool ValuePtrPrivateArray1T<sdiValueEntity, sdiValueEntityList, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetValue(
    sdiValueEntity&         value, const unsigned int i, const unsigned int j) const
{
    return p_aValue.GetEntity(value, i);
}

template <>
inline bool ValuePtrPrivateArray1T<sdiValueEntity, sdiValueEntityList, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::SetValue(
    const sdiValueEntity&   value, const unsigned int i, const unsigned int j)
{
    return p_aValue.SetEntity(value, i);
}


// Get values, including those stored in array (using same approach as above). sdiValue is returned
// as formatted string. The format argument is necessary if seek value formatted in particular form;
// otherwise, a default formating is provided: a space " " separate compound values, with parenthesis
// "{}" used to separate each row of matrices.
//! For arrays, can get entire array per the format below if specify i = UINT_MAX.
//!     1-d standard array:  { item1 item2 item3 ... }
//!     2-d standard array:  {{item11 item12 item13 ...} {item21 item22 item23 ...} ...}
//!     1-d entity array:    {{type �with optional subtype�} id1 id2 id3} Alternatively, {type id1 id2 id3} if not subtype specified � note skipping internal {}�s
//!     2-d entity array:    {{type �with optional subtype�} { id1 id2 id3} {id5 id4 id6}} Alternatively, {type { id1 id2 id3} {id5 id4 id6}}  � note skipping internal {}�s
//!         where type and sub-type can be numeric or named
template <class VALUE, class VALUE_LIST, sdiBasicType BASIC_TYPE, sdiCompoundType COMPOUND_TYPE>
inline bool ValuePtrPrivateArray1T<VALUE, VALUE_LIST, BASIC_TYPE, COMPOUND_TYPE>::GetValueAsString(
    sdiString&          value,
    const char*        format,
    const unsigned int i,
    const unsigned int j) const
{
    if (i != UINT_MAX && i >= p_aValue.size())
    {
        assert(0);
        return false;
    }
    char buffer[SPRINTF_STRING_SIZE];
    value.clear();

    // Get entire list bounded by {}
    if (i == UINT_MAX)
    {
        if (format)
        {
            for (unsigned int i = 0; i != p_aValue.size(); ++i)
            {
                sprintf(buffer, format, p_aValue[i]);

                if (i) value += sdiString(" ");
                value += buffer;
            }
        }
        else
        {
            switch(BASIC_TYPE)
            {
            case BASIC_TYPE_DOUBLE:
            {
                char precisionFormat[128];
                sprintf(precisionFormat, "%%.%dg", DEFAULT_CONVERSION_PRECISION);
                for (unsigned int i = 0; i != p_aValue.size(); ++i)
                {
                    if (! GetDoubleAsString(p_aValue[i], precisionFormat, buffer))
                    {
                        return false;
                    }
                    if (i) value += sdiString(" ");
                    value += buffer;
                }
                break;
            }
            default:
            {
                for (unsigned int i = 0; i != p_aValue.size(); ++i)
                {
                    std::ostringstream tempstr;
                    tempstr << p_aValue[i];

                    if (i) value += sdiString(" ");
                    value += tempstr.str();
                }
            }
            }
        }
        return true;
    }


    //! Need template specialization for all but some SINGLE types
    assert(COMPOUND_TYPE == COMPOUND_TYPE_SINGLE);

    if (format)
    {
        if (sprintf(buffer, format, p_aValue[i]) < 0)
        {
            return false;
        }
    }
    else
    {
        switch (BASIC_TYPE)
        {
        case BASIC_TYPE_CHAR:
            if (sprintf(buffer, _format_c, p_aValue[i]) < 0)
            {
                return false;
            }
            break;
        case BASIC_TYPE_INT:
            if (sprintf(buffer, _format_d, p_aValue[i]) < 0)
            {
                return false;
            }
            break;

        case BASIC_TYPE_UINT:
            if (sprintf(buffer, _format_u, p_aValue[i]) < 0)
            {
                return false;
            }
            break;

        case BASIC_TYPE_DOUBLE:
        {
            char temp[SPRINTF_STRING_SIZE];
            sprintf(temp, "%%.%dg", 
                DEFAULT_CONVERSION_PRECISION);
                if (sprintf(buffer, temp, p_aValue[i]) < 0)
                {
                    return false;
                }
                break;
        }
        default:
            assert(0);
            return false;
        }
    }

    value = buffer;
    return true;
}

// Need template specialization for all but some types

// Get sdiValueEntityType; returns an invalid object if sdiValue does not correspond
// to an sdiValueEntity, sdiValueEntityList, or Qualifier (not QualifierList).
template <>
inline sdiValueEntityType ValuePtrPrivateArray1T<sdiValueEntity, sdiValueEntityList, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetEntityFullType() const
{
    return p_aValue.GetEntityFullType();
}

template <>
inline bool ValuePtrPrivateArray1T<bool, sdiBoolList, BASIC_TYPE_BOOL, COMPOUND_TYPE_SINGLE>::GetValueAsString(
    sdiString&          value,
    const char*        format,
    const unsigned int i,
    const unsigned int j) const
{
    if (i == UINT_MAX)
    {
        value.clear();

        for (unsigned int i = 0; i != p_aValue.size(); ++i)
        {
            std::ostringstream tempstr;
            p_aValue[i] ? tempstr << "true" : tempstr << "false";

            if (i) value += sdiString(" ");
            value += tempstr.str();
        }

        return true;
    }

    char buffer[SPRINTF_STRING_SIZE];
    const sdiString _value = p_aValue[i] ? "true" : "false";

    if (format)
    {
        if (sprintf(buffer, format, _value.c_str()) < 0)
        {
            return false;
        }
        value = buffer;
    }
    else
    {
        value = _value;
    }
    return true;
}

template <>
inline bool ValuePtrPrivateArray1T<sdiString, sdiStringList, BASIC_TYPE_STRING, COMPOUND_TYPE_SINGLE>::GetValueAsString(
    sdiString&          value,
    const char*        format,
    const unsigned int i,
    const unsigned int j) const
{
    // Get entire list bounded by {}
    if (i == UINT_MAX)
    {
        value.clear();

        for (unsigned int i = 0; i != p_aValue.size(); ++i)
        {
            std::ostringstream tempstr;
            tempstr << p_aValue[i];

            if (i) value += sdiString(" ");

            const size_t strBegin = p_aValue[i].find_first_of(" \t");
            if (strBegin == std::string::npos)
            {
                value += tempstr.str();
            }
            else
            {
                value += "{" + tempstr.str() + "}";
            }
        }
        return true;
    }
 
 
    if (i >= p_aValue.size())
    {
        assert(0);
        return false;
    }

    value = p_aValue[i];
    return true;
}

template <>
inline bool ValuePtrPrivateArray1T<sdiTriple, sdiTripleList, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>::GetValueAsString(
    sdiString&          value,
    const char*        format,
    const unsigned int i,
    const unsigned int j) const
{
    char buffer[128];
    char temp[128];
    if (!format)
    {
        sprintf(temp, "%%.%dg %%.%dg %%.%dg", 
            DEFAULT_CONVERSION_PRECISION,DEFAULT_CONVERSION_PRECISION,DEFAULT_CONVERSION_PRECISION);
    }
    if (i == UINT_MAX)
    {
        value.clear();
        unsigned int size = (unsigned int)p_aValue.size();
        value.reserve(SPRINTF_STRING_SIZE*size);
        for (unsigned int k = 0; k != size; ++k)
        {
            if (k) value += " ";
            if (size > 1)
                value += "{";

            if (format)
            {
                if (sprintf(buffer, format, p_aValue[k].GetX(), p_aValue[k].GetY(), p_aValue[k].GetZ()) >= 0)
                {
                    value += buffer;
                }
            }
            else
            {
                if (sprintf(buffer, temp, p_aValue[k].GetX(), p_aValue[k].GetY(), p_aValue[k].GetZ()) >= 0)
                {
                    value += buffer;
                }
            }
            if (p_aValue.size() > 1)
                value += "}";
        }
        return true;
    }

    if (i >= p_aValue.size())
    {
        return false;
    }

    if (format)
    {
        if (sprintf(buffer, format, p_aValue[i].GetX(), p_aValue[i].GetY(), p_aValue[i].GetZ()) < 0)
        {
            return false;
        }
    }
    else
    {
        if (sprintf(buffer, temp, p_aValue[i].GetX(), p_aValue[i].GetY(), p_aValue[i].GetZ()) < 0)
        {
            return false;
        }
    }
    value = buffer;
    return true;
}

template <>
inline bool ValuePtrPrivateArray1T<sdiValueEntity, sdiValueEntityList, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetValueAsString(
    sdiString&          value,
    const char*        format,
    const unsigned int i,
    const unsigned int j) const
{
    if (i != UINT_MAX)
    {
        sdiValueEntity _entity;
        if (!p_aValue.GetEntity(_entity, i))
        {
            assert(0);
            return false;
        }

        return _GetEntityAsString(_entity, value, format);
    }

    return _GetEntityListAsString(p_aValue, value, format);
}


// Set values, including those stored in array (using same approach as above).
// The format argument is necessary if have value formatted in particular form; otherwise,
// a default formating is expected: a space " " separate compound values, with parenthesis
// "{}" used to separate each row of matrices.
//! For arrays, can get entire array per the format below if specify i = UINT_MAX.
//!     1-d standard array:  { item1 item2 item3 ... }
//!     2-d standard array:  {{item11 item12 item13 ...} {item21 item22 item23 ...} ...}
//!     1-d entity array:    {{type �with optional subtype�} id1 id2 id3} Alternatively, {type id1 id2 id3} if not subtype specified � note skipping internal {}�s
//!     2-d entity array:    {{type �with optional subtype�} { id1 id2 id3} {id5 id4 id6}} Alternatively, {type { id1 id2 id3} {id5 id4 id6}}  � note skipping internal {}�s
//!         where type and sub-type can be numeric or named
template <class VALUE, class VALUE_LIST, sdiBasicType BASIC_TYPE, sdiCompoundType COMPOUND_TYPE>
inline bool ValuePtrPrivateArray1T<VALUE, VALUE_LIST, BASIC_TYPE, COMPOUND_TYPE>::SetValueFromString(
    const sdiString&    value,
    const char*        format,
    const unsigned int i,
    const unsigned int j)
{
    //! Need template specialization for all but some SINGLE types
    assert(COMPOUND_TYPE == COMPOUND_TYPE_SINGLE);


    // Set entire list bounded by {}
    if (i == UINT_MAX)
    {
        sdiString str = value;
        RemoveBoundingBraces(str);

        sdiVector<VALUE> aToken;
        if (!SplitStringIntoTokens<VALUE>(str, aToken))
        {
            return false;
        }
     
        p_aValue.resize(0);
        p_aValue.reserve(aToken.size());
        for (unsigned int ii = 0; ii != (unsigned int)aToken.size(); ++ii)
        {
            this->SetValue(aToken[ii], ii);
        }

        return true;
    } 
 

    VALUE _value;

    if (format)
    {
        if (sscanf(value.c_str(), format, &_value) != 1)
        {
            return false;
        }
    }
    else
    {
        switch (BASIC_TYPE)
        {
        case BASIC_TYPE_CHAR:
        case BASIC_TYPE_INT:
        case BASIC_TYPE_UINT:
        case BASIC_TYPE_DOUBLE:
            if (!ConvertToNumber<VALUE>(value, _value))
            {
                return false;
            }
            break;

        default:
            assert(0);
            return false;
        }
    }

    return this->SetValue(_value, i);
}


// Need template specialization for all but some types

template <>
inline bool ValuePtrPrivateArray1T<bool, sdiBoolList, BASIC_TYPE_BOOL, COMPOUND_TYPE_SINGLE>::SetValueFromString(
    const sdiString&    value,
    const char*        format,
    const unsigned int i,
    const unsigned int j)
{
    char str[SPRINTF_STRING_SIZE];

    if (i == UINT_MAX)
    {
        sdiString str = value;
        RemoveBoundingBraces(str);

        sdiVector<sdiString> aToken;
        if (!SplitStringIntoTokens<sdiString>(str, aToken))
        {
            return false;
        }
     
        p_aValue.resize(0);
        p_aValue.reserve(aToken.size());
        for (unsigned int ii = 0; ii != (unsigned int)aToken.size(); ++ii)
        {
            //aToken[ii].tolower();
            const bool _value = (aToken[ii] == "true" || aToken[ii] == "1") ? true : false;
            this->SetValue(_value, ii);
        }

        return true;
    }

    if (format)
    {
        if (sscanf(value.c_str(), format, str) != 1)
        {
            return false;
        }
    }
    else
    {
        if (sscanf(value.c_str(), "%s", str) != 1)
        {
            return false;
        }
    }

    sdiString _stringValue(str);
    //_stringValue.tolower();
    const bool _value = (_stringValue == "true" || _stringValue == "1") ? true : false;

    this->SetValue(_value, i);
    return true;
}

template <>
inline bool ValuePtrPrivateArray1T<sdiString, sdiStringList, BASIC_TYPE_STRING, COMPOUND_TYPE_SINGLE>::SetValueFromString(
    const sdiString&    value,
    const char*        format,
    const unsigned int i,
    const unsigned int j)
{
    // Set entire list bounded by {}
    if (i == UINT_MAX)
    {
        sdiString str = value;
        RemoveBoundingBraces(str);

        sdiVector<sdiString> aToken;
        if (!SplitStringIntoTokens<sdiString>(str, aToken))
        {
            return false;
        }
     
        p_aValue.resize(0);
        p_aValue.reserve(aToken.size());
        for (unsigned int ii = 0; ii != (unsigned int)aToken.size(); ++ii)
        {
            this->SetValue(aToken[ii], ii);
        }

        return true;
    } 
 
 
    this->SetValue(value, i);
    return true;
}

template <>
inline bool ValuePtrPrivateArray1T<sdiTriple, sdiTripleList, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>::SetValueFromString(
    const sdiString&    value,
    const char*        format,
    const unsigned int i,
    const unsigned int j)
{
    if (i == UINT_MAX)
    {
        sdiString str = value;
        sdiVector<sdiString> aToken;

        const size_t firstBrace = str.find('{');
        if (firstBrace == std::string::npos) //! only one value
        {
            _SplitStringIntoTokens(str, aToken);
            assert(aToken.size() == 3); //! minimum should be three values
            if (aToken.size() == 3)
            {
                double _value[3];
                if (format)
                {
                    if (sscanf(str.c_str(), format, &_value[0], &_value[1], &_value[2]) != 3)
                    {
                        return false;
                    }
                }
                else
                {
                    if (sscanf(str.c_str(), "%lf %lf %lf", &_value[0], &_value[1], &_value[2]) != 3)
                    {
                        return false;
                    }
                }
                this->SetValue(sdiTriple(_value[0], _value[1], _value[2]), 0);
                return true;
            }
            return false;
        }
        else
        {
            if (!SplitStringIntoTokens<sdiString>(str, aToken))
            {
                return false;
            }
            for (unsigned int ii = 0; ii != (unsigned int)aToken.size(); ++ii)
            {
                double _value[3];
                sdiString str = aToken[ii];
                RemoveBoundingBraces(str);
                if (format)
                {
                    if (sscanf(str.c_str(), format, &_value[0], &_value[1], &_value[2]) != 3)
                    {
                        return false;
                    }
                }
                else
                {
                    if (sscanf(str.c_str(), "%lf %lf %lf", &_value[0], &_value[1], &_value[2]) != 3)
                    {
                        return false;
                    }
                }
                this->SetValue(sdiTriple(_value[0], _value[1], _value[2]), ii);
            }
            return true;
        }
    } 
 
    double _value[3];
    if (format)
    {
        if (sscanf(value.c_str(), format, &_value[0], &_value[1], &_value[2]) != 3)
        {
            return false;
        }
    }
    else
    {
        if (sscanf(value.c_str(), "%lf %lf %lf", &_value[0], &_value[1], &_value[2]) != 3)
        {
            return false;
        }
    }
    this->SetValue(sdiTriple(_value[0], _value[1], _value[2]), i);
    return true;
}

template <>
inline bool ValuePtrPrivateArray1T<sdiValueEntity, sdiValueEntityList, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::SetValueFromString(
    const sdiString&    value,
    const char*        format,
    const unsigned int i,
    const unsigned int j)
{
    if (i != UINT_MAX)
    {
        sdiValueEntity _entity;
        if (!_SetEntityFromString(_entity, value, format))
        {
            return false;
        }

        return this->SetValue(_entity, i);
    }

    return _SetEntityListFromString(p_aValue,
                                    value,
                                    format);
}


// Location of memory location where data is stored.
// sdiBoolList needs special treatment
template <>
const void* ValuePtrPrivateArray1T<bool, sdiBoolList, BASIC_TYPE_BOOL, COMPOUND_TYPE_SINGLE>::GetMemoryLocation(
    const unsigned int i,
    const unsigned int j) const
{
    assert(0);
    return 0;
}
template <>
void* ValuePtrPrivateArray1T<bool, sdiBoolList, BASIC_TYPE_BOOL, COMPOUND_TYPE_SINGLE>::GetMemoryLocation(
    const unsigned int i,
    const unsigned int j)
{
    assert(0);
    return 0;
}

// sdiValueEntityList needs special treatment
template <>
const void* ValuePtrPrivateArray1T<sdiValueEntity, sdiValueEntityList, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetMemoryLocation(
    const unsigned int i,
    const unsigned int j) const
{
    const sdiUIntList& list = p_aValue.GetList();

    if (i >= (unsigned int)list.size())
    {
        return 0;
    }
    return &list[i];
}

template <>
void* ValuePtrPrivateArray1T<sdiValueEntity, sdiValueEntityList, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetMemoryLocation(
    const unsigned int i,
    const unsigned int j)
{
    sdiUIntList& list = p_aValue.GetList();

    if (i >= (unsigned int)list.size())
    {
        assert(0);
        return 0;
    }
    return &list[i];
}

template <>
inline const char* ValuePtrPrivateArray1T<char, sdiCharList, BASIC_TYPE_CHAR, COMPOUND_TYPE_SINGLE>::GetValuePointerChar(const unsigned int i, const unsigned int j) const
{
    if (i >= p_aValue.size())
    {
        assert(0);
        return 0;
    }
    return &p_aValue[i];
}

template <>
inline const int* ValuePtrPrivateArray1T<int, sdiIntList, BASIC_TYPE_INT, COMPOUND_TYPE_SINGLE>::GetValuePointerInt(const unsigned int i, const unsigned int j) const
{
    if (i >= p_aValue.size())
    {
        assert(0);
        return 0;
    }
    return &p_aValue[i];
}

template <>
inline const unsigned int* ValuePtrPrivateArray1T<unsigned int, sdiUIntList, BASIC_TYPE_UINT, COMPOUND_TYPE_SINGLE>::GetValuePointerUInt(const unsigned int i, const unsigned int j) const
{
    if (i >= p_aValue.size())
    {
        assert(0);
        return 0;
    }
    return &p_aValue[i];
}

template <>
inline const double* ValuePtrPrivateArray1T<double, sdiDoubleList, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_SINGLE>::GetValuePointerDouble(const unsigned int i, const unsigned int j) const
{
    if (i >= p_aValue.size())
    {
        assert(0);
        return 0;
    }
    return &p_aValue[i];
}

template <>
inline const sdiString* ValuePtrPrivateArray1T<sdiString, sdiStringList, BASIC_TYPE_STRING, COMPOUND_TYPE_SINGLE>::GetValuePointerString(const unsigned int i, const unsigned int j) const
{
    if (i >= p_aValue.size())
    {
        assert(0);
        return 0;
    }
    return &p_aValue[i];
}

template <>
inline const sdiTriple* ValuePtrPrivateArray1T<sdiTriple, sdiTripleList, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>::GetValuePointerTriple(const unsigned int i, const unsigned int j) const
{
    if (i >= p_aValue.size())
    {
        assert(0);
        return 0;
    }
    return &p_aValue[i];
}


//! 1-d arrays
template <>
inline  const sdiBoolList* ValuePtrPrivateArray1T<bool, sdiBoolList, BASIC_TYPE_BOOL, COMPOUND_TYPE_SINGLE>::GetValuePointerBoolList() const
{
    return &p_aValue;
}

template <>
inline  const sdiCharList* ValuePtrPrivateArray1T<char, sdiCharList, BASIC_TYPE_CHAR, COMPOUND_TYPE_SINGLE>::GetValuePointerCharList() const
{
    return &p_aValue;
}

template <>
inline  const sdiIntList* ValuePtrPrivateArray1T<int, sdiIntList, BASIC_TYPE_INT, COMPOUND_TYPE_SINGLE>::GetValuePointerIntList() const
{
    return &p_aValue;
}

template <>
inline  const sdiUIntList* ValuePtrPrivateArray1T<unsigned int, sdiUIntList, BASIC_TYPE_UINT, COMPOUND_TYPE_SINGLE>::GetValuePointerUIntList() const
{
    return &p_aValue;
}

template <>
inline  const sdiDoubleList* ValuePtrPrivateArray1T<double, sdiDoubleList, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_SINGLE>::GetValuePointerDoubleList() const
{
    return &p_aValue;
}

template <>
inline  const sdiStringList* ValuePtrPrivateArray1T<sdiString, sdiStringList, BASIC_TYPE_STRING, COMPOUND_TYPE_SINGLE>::GetValuePointerStringList() const
{
    return &p_aValue;
}

template <>
inline  const sdiTripleList* ValuePtrPrivateArray1T<sdiTriple, sdiTripleList, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>::GetValuePointerTripleList() const
{
    return &p_aValue;
}

template <>
inline  const sdiValueEntityList* ValuePtrPrivateArray1T<sdiValueEntity, sdiValueEntityList, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetValuePointerEntityList() const
{
    return &p_aValue;
}


// *********************************************************************************
// sdiValuePtrPrivateArray2T Definition
// *********************************************************************************
template <class VALUE, class VALUE_LIST, sdiBasicType BASIC_TYPE, sdiCompoundType COMPOUND_TYPE>
class ValuePtrPrivateArray2T : public sdiValuePtr
{
public:

    ValuePtrPrivateArray2T(const VALUE_LIST&        aaValue)
        : sdiValuePtr(),
          p_aaValue(aaValue)
    {
    }

    ValuePtrPrivateArray2T(VALUE_LIST&&        aaValue)
        : sdiValuePtr(),
          p_aaValue(std::move(aaValue))
    {
    }

    ValuePtrPrivateArray2T(ValuePtrPrivateArray2T&& other)
        : sdiValuePtr()
    {
        if (other.p_aaValue.IsMemoryExternallyManaged())
            p_aaValue = other.p_aaValue;            // copy
        else
            p_aaValue = std::move(other.p_aaValue); // move
    }

    ValuePtrPrivateArray2T(const unsigned int  sizeDimFirst,         //! size of first (outermost) dimension
                           const unsigned int  sizeDimSecond)        //! size of second dimension
        : sdiValuePtr()
    {
        p_aaValue.resize(sizeDimFirst);
        for (unsigned int i = 0; i < sizeDimSecond; i++)
        {
            p_aaValue[i].reserve(sizeDimSecond);
        }
    }

    ValuePtrPrivateArray2T(const ValuePtrPrivateArray2T& other)
        : p_aaValue(other.p_aaValue)
    {
    }

    ~ValuePtrPrivateArray2T()
    {
    }

    //! Creates clone of value
    virtual sdiValuePtr* Clone() const
    {
        return new ValuePtrPrivateArray2T(*this);
    }


    ////////////////////////////////////////////////////////////////////////////////////
    //! Query
    ////////////////////////////////////////////////////////////////////////////////////

    //! Basic type
    virtual sdiBasicType GetBasicType() const
    {
        return BASIC_TYPE;
    }

    //! Compound type
    virtual sdiCompoundType GetCompoundType() const
    {
        return COMPOUND_TYPE;
    }

    //! Array dimension; 0 if not an array.
    virtual unsigned int GetArrayDimension() const
    {
        return 2;
    }

    //! Length of Array for specified dimension
    unsigned int GetArrayDimensionExtent(const unsigned int dimensionIndex) const
    {
        if (dimensionIndex == 1)
        {
            return (unsigned int)p_aaValue.size();
        }

        unsigned int retval = 0;
        if (p_aaValue.size())
        {
            retval = (unsigned int)p_aaValue[0].size();
            for (unsigned int i = 1; i < (unsigned int)p_aaValue.size(); i++)
            {
                if ((unsigned int)p_aaValue[i].size() > retval)
                {
                    retval = (unsigned int)p_aaValue[i].size();
                }
            }
        }
        return retval;
    }


    //! Comparison with other
    virtual bool Equals(const sdiValuePtr* pOther) const;


    ////////////////////////////////////////////////////////////////////////////////////
    //! Get value(s)
    ////////////////////////////////////////////////////////////////////////////////////

    //! Get single values, including those stored in array. Funtion is limited to for
    //! scalar and 1-d (i, 0) and 2-d (i, j) dimensional arrays; i = j = 0 for scalars.
    //! Returns false if of inappropriate type, or if seeking value outside array bounds.
    virtual bool GetValue(VALUE&          value, const unsigned int i, const unsigned int j) const
    {
        if (i >= p_aaValue.size())
        {
            assert(0);
            return false;
        }
        if (j >= p_aaValue[i].size())
        {
            assert(0);
            return false;
        }
        value = p_aaValue[i][j];
        return true;
    }


        //! Get values, including those stored in array (using same approach as above). sdiValue is returned
    //! as formatted string. The format argument is necessary if seek value formatted in particular form;
    //! otherwise, a default formating is provided: a space " " separate compound values, with parenthesis
    //! "{}" used to separate each row of matrices.
    //! For arrays, can get entire array per the format below if specify i = UINT_MAX.
    //!     1-d standard array:  { item1 item2 item3 ... }
    //!     2-d standard array:  {{item11 item12 item13 ...} {item21 item22 item23 ...} ...}
    //!     1-d entity array:    {{type with optional subtype} id1 id2 id3} Alternatively, {type id1 id2 id3} if not subtype specified  note skipping internal {}s
    //!     2-d entity array:    {{type with optional subtype} { id1 id2 id3} {id5 id4 id6}} Alternatively, {type { id1 id2 id3} {id5 id4 id6}}  note skipping internal {}s
    //!         where type and sub-type can be numeric or named
    inline virtual bool GetValueAsString(sdiString&          value,
                                         const char*        format,
                                         const unsigned int i,
                                         const unsigned int j) const;

    
    inline virtual std::size_t GetValueMemoryUsage() const;

    //! For entity array
    virtual bool GetValue(VALUE_LIST& aaValue) const
    {
        aaValue = p_aaValue;
        return true;
    }


    //! Can also retrive sdiValue if form of const pointer; note how null pointer is returned for
    //! type inconsistent with sdiValue

    inline const int* GetValuePointerInt(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }

    inline const unsigned int* GetValuePointerUInt(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }

    inline const double* GetValuePointerDouble(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }

    inline const sdiTriple* GetValuePointerTriple(const unsigned int i = 0, const unsigned int j = 0) const
    {
        return 0;
    }
    
    //! 2-d arrays
    virtual const sdiIntList2* GetValuePointerIntList2() const
    {
        return 0;
    }

    virtual const sdiUIntList2* GetValuePointerUIntList2() const
    {
        return 0;
    }

    virtual const sdiDoubleList2* GetValuePointerDoubleList2() const
    {
        return 0;
    }

    virtual const sdiTripleList2* GetValuePointerTripleList2() const
    {
        return 0;
    }

    virtual const sdiValueEntityList2* GetValuePointerEntityList2() const
    {
        return 0;
    }


    ////////////////////////////////////////////////////////////////////////////////////
    //! Set value(s)
    ////////////////////////////////////////////////////////////////////////////////////

    //! Set single values, including those stored in array. Funtion is limited to for
    //! scalar and 1-d (i, 0) and 2-d (i, j) dimensional arrays; i = j = 0 for scalars.
    //! Function returns false only for entity value type for case of inconsistent type
    //! that what was already in list.
    virtual bool SetValue(const VALUE&          value, const unsigned int i, const unsigned int j)
    {
        if (i >= p_aaValue.size())
        {
            p_aaValue.resize(i + 1);
        }
        if (j >= p_aaValue[i].size())
        {
            p_aaValue[i].resize(j + 1);
        }
        p_aaValue[i][j] = value;
        return true;
    }


    //! Set values, including those stored in array (using same approach as above).
    //! The format argument is necessary if have value formatted in particular form; otherwise,
    //! a default formating is expected: a space " " separate compound values, with parenthesis
    //! "{}" used to separate each row of matrices.
    //! For arrays, can get entire array per the format below if specify i = UINT_MAX.
    //!     1-d standard array:  { item1 item2 item3 ... }
    //!     2-d standard array:  {{item11 item12 item13 ...} {item21 item22 item23 ...} ...}
    //!     1-d entity array:    {{type �with optional subtype�} id1 id2 id3} Alternatively, {type id1 id2 id3} if not subtype specified � note skipping internal {}�s
    //!     2-d entity array:    {{type �with optional subtype�} { id1 id2 id3} {id5 id4 id6}} Alternatively, {type { id1 id2 id3} {id5 id4 id6}}  � note skipping internal {}�s
    //!         where type and sub-type can be numeric or named
    inline virtual bool SetValueFromString(const sdiString&    value,
                                           const char*        format,
                                           const unsigned int i,
                                           const unsigned int j);

    //! For entity array
    virtual bool SetValue(const VALUE_LIST& aaValue)
    {
        p_aaValue = aaValue;
        return true;
    }


    //! Location of memory location where data is stored.
    virtual const void* GetMemoryLocation(const unsigned int i, const unsigned int j) const
    {
        if (i >= p_aaValue.size())
        {
            assert(0);
            return 0;
        }
        if (j >= p_aaValue[i].size())
        {
            assert(0);
            return 0;
        }
        return &p_aaValue[i];
    }
    virtual       void* GetMemoryLocation(const unsigned int i, const unsigned int j)
    {
        if (i >= p_aaValue.size())
        {
            assert(0);
            return 0;
        }
        if (j >= p_aaValue[i].size())
        {
            assert(0);
            return 0;
        }
        return &p_aaValue[i];
    }



private:

    VALUE_LIST p_aaValue;
};


// *********************************************************************************
// sdiValuePtrPrivateArray2T Implementation
// *********************************************************************************

template <class VALUE, class VALUE_LIST, sdiBasicType BASIC_TYPE, sdiCompoundType COMPOUND_TYPE>
inline bool ValuePtrPrivateArray2T<VALUE, VALUE_LIST, BASIC_TYPE, COMPOUND_TYPE>::Equals(const sdiValuePtr* pOther) const
{
    // Types must match
    if (!this->P_EqualTypes(pOther))
    {
        return false;
    }

    VALUE_LIST v1, v2;

    if (!(this->GetValue(v1)))
    {
        assert(0);
        return false;
    }

    if (!(pOther->GetValue(v2)))
    {
        assert(0);
        return false;
    }

    return (v1 == v2) ? true : false;
}


// Get values, including those stored in array (using same approach as above). sdiValue is returned
// as formatted string. The format argument is necessary if seek value formatted in particular form;
// otherwise, a default formating is provided: a space " " separate compound values, with parenthesis
// "{}" used to separate each row of matrices.
//! For arrays, can get entire array per the format below if specify i = UINT_MAX.
//!     1-d standard array:  { item1 item2 item3 ... }
//!     2-d standard array:  {{item11 item12 item13 ...} {item21 item22 item23 ...} ...}
//!     1-d entity array:    {{type with optional subtype} id1 id2 id3} Alternatively, {type id1 id2 id3} if not subtype specified note skipping internal {}s
//!     2-d entity array:    {{type with optional subtype} { id1 id2 id3} {id5 id4 id6}} Alternatively, {type { id1 id2 id3} {id5 id4 id6}}  note skipping internal {}s
//!         where type and sub-type can be numeric or named
template <class VALUE, class VALUE_LIST, sdiBasicType BASIC_TYPE, sdiCompoundType COMPOUND_TYPE>
inline bool ValuePtrPrivateArray2T<VALUE, VALUE_LIST, BASIC_TYPE, COMPOUND_TYPE>::GetValueAsString(
    sdiString&          value,
    const char*        format,
    const unsigned int i,
    const unsigned int j) const
{
    char buffer[SPRINTF_STRING_SIZE];
    value.clear();

    char doublePrecision[SPRINTF_STRING_SIZE];
    sprintf(doublePrecision, "%%.%dg", DEFAULT_CONVERSION_PRECISION);

    // Get entire list bounded by {}
    if (i == UINT_MAX && j == UINT_MAX)
    {
        for (unsigned int i = 0; i != (unsigned int)p_aaValue.size(); ++i)
        {
            if (i)
            {
                value += sdiString(" ");
            }
            value += "{";
            for (unsigned int j = 0; j != (unsigned int)p_aaValue[i].size(); ++j)
            {
                std::ostringstream tempstr;
                if (format)
                {
                    sprintf(buffer, format, p_aaValue[i][j]);
                }
                else
                {
                    if (BASIC_TYPE == BASIC_TYPE_DOUBLE)
                    {
                        GetDoubleAsString(p_aaValue[i][j], doublePrecision, buffer);
                        tempstr << buffer; 
                    }
                    else
                    {
                        tempstr << p_aaValue[i][j];
                    }
                }
                if (j)
                {
                    value += sdiString(" ");
                }
                if (format)
                {
                    value += buffer;
                }
                else
                {
                    value += tempstr.str();
                }
            }
            value += "}";
        }
        return true;
    } 
    else if (i != UINT_MAX && j == UINT_MAX)
    {
        //! Get the entire row
        if (i >= p_aaValue.size())
        {
            assert(0);
            return false;
        }

        value += "{";
        for (unsigned int j = 0; j != (unsigned int)p_aaValue[i].size(); ++j)
        {
            std::ostringstream tempstr;
            if (format)
            {
                sprintf(buffer, format, p_aaValue[i][j]);
            }
            else
            {
                if (BASIC_TYPE == BASIC_TYPE_DOUBLE)
                {
                    GetDoubleAsString(p_aaValue[i][j], doublePrecision, buffer);
                    tempstr << buffer; 
                }
                else
                {
                    tempstr << p_aaValue[i][j];
                }
            }
            if (j)
            {
                value += sdiString(" ");
            }
            if (format)
            {
                value += buffer;
            }
            else
            {
                value += tempstr.str();
            }
        }
        value += "}";
        return true;
    }
    else
    {
        //! Get a single value at the particular row, column
        if (i >= p_aaValue.size())
        {
            assert(0);
            return false;
        }
        if (j >= p_aaValue[i].size())
        {
            assert(0);
            return false;
        }

        //! Need template specialization for all but some SINGLE types
        assert(COMPOUND_TYPE == COMPOUND_TYPE_SINGLE);

        if (format)
        {
            if (sprintf(buffer, format, p_aaValue[i][j]) < 0)
            {
                return false;
            }
        }
        else
        {
            switch (BASIC_TYPE)
            {
            case BASIC_TYPE_CHAR:
                if (sprintf(buffer, _format_c, p_aaValue[i][j]) < 0)
                {
                    return false;
                }
                break;
            case BASIC_TYPE_INT:
                if (sprintf(buffer, _format_d, p_aaValue[i][j]) < 0)
                {
                    return false;
                }
                break;

            case BASIC_TYPE_UINT:
                if (sprintf(buffer, _format_u, p_aaValue[i][j]) < 0)
                {
                    return false;
                }
                break;

            case BASIC_TYPE_DOUBLE:
                {
                    char temp[SPRINTF_STRING_SIZE];
                    sprintf(temp, "%%.%dg", DEFAULT_CONVERSION_PRECISION);
                    if (sprintf(buffer, temp , p_aaValue[i][j]) < 0)
                    {
                        return false;
                    }
                }
                break;

            default:
                assert(0);
                return false;
            }
        }

        value = buffer;
    }
    return true;
}

template <>
inline bool ValuePtrPrivateArray2T<sdiTriple, sdiTripleList2, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>::GetValueAsString(
    sdiString&          value,
    const char*        format,
    const unsigned int i,
    const unsigned int j) const
{
    char temp[128];
    if (!format)
    {
        sprintf(temp, "%%.%dg %%.%dg %%.%dg", 
            DEFAULT_CONVERSION_PRECISION,DEFAULT_CONVERSION_PRECISION,DEFAULT_CONVERSION_PRECISION);
    }
    // TODO: Get entire list bounded by {}
    assert(i != UINT_MAX);
 
    if (i >= p_aaValue.size())
    {
        assert(0);
        return false;
    }
    if (j >= p_aaValue[i].size())
    {
        assert(0);
        return false;
    }

    char buffer[128];
    if (format)
    {
        if (sprintf(buffer, format, p_aaValue[i][j].GetX(), p_aaValue[i][j].GetY(), p_aaValue[i][j].GetZ()) < 0)
        {
            return false;
        }
    }
    else
    {
        if (sprintf(buffer, temp, p_aaValue[i][j].GetX(), p_aaValue[i][j].GetY(), p_aaValue[i][j].GetZ()) < 0)
        {
            return false;
        }
    }
    value = buffer;
    return true;
}


// Set values, including those stored in array (using same approach as above).
// The format argument is necessary if have value formatted in particular form; otherwise,
// a default formating is expected: a space " " separate compound values, with parenthesis
// "{}" used to separate each row of matrices.
//! For arrays, can get entire array per the format below if specify i = UINT_MAX.
//!     1-d standard array:  { item1 item2 item3 ... }
//!     2-d standard array:  {{item11 item12 item13 ...} {item21 item22 item23 ...} ...}
//!     1-d entity array:    {{type �with optional subtype�} id1 id2 id3} Alternatively, {type id1 id2 id3} if not subtype specified � note skipping internal {}�s
//!     2-d entity array:    {{type �with optional subtype�} { id1 id2 id3} {id5 id4 id6}} Alternatively, {type { id1 id2 id3} {id5 id4 id6}}  � note skipping internal {}�s
//!         where type and sub-type can be numeric or named
template <class VALUE, class VALUE_LIST, sdiBasicType BASIC_TYPE, sdiCompoundType COMPOUND_TYPE>
inline bool ValuePtrPrivateArray2T<VALUE, VALUE_LIST, BASIC_TYPE, COMPOUND_TYPE>::SetValueFromString(
    const sdiString&    value,
    const char*        format,
    const unsigned int i,
    const unsigned int j)
{
    //! Need template specialization for all but some SINGLE types
    assert(COMPOUND_TYPE == COMPOUND_TYPE_SINGLE);


    // Set entire list bounded by {}
    if (i == UINT_MAX)
    {
        sdiString str = value;
        RemoveBoundingBraces(str);

        sdiVector<sdiString> aInternal;
        if (!SplitStringIntoTokens<sdiString>(str, aInternal))
        {
            return false;
        }

        // Make sure that can tokenize all internals
        sdiVector<sdiVector<VALUE> >aaToken;
        aaToken.resize(aInternal.size());
        for (unsigned int ii = 0; ii != (unsigned int)aInternal.size(); ++ii)
        {
            if (!SplitStringIntoTokens<VALUE>(aInternal[ii], aaToken[ii]))
            {
                return false;
            }
        }
        if (aaToken.size())
        {
            // All have same length internals
            for (unsigned int ii = 1; ii != (unsigned int)aaToken.size(); ++ii)
            {
                if (aaToken[ii].size() != aaToken[0].size())
                {
                    return false;
                }
            }
        }
        else
        {
            aaToken.resize(0);  // clearing data
            return true;
        }
    
        p_aaValue.resize(aaToken.size());
        for (unsigned int ii = 0; ii != (unsigned int)aaToken.size(); ++ii)
        {
            p_aaValue[ii].resize(aaToken[ii].size());
            for (unsigned int jj = 0; jj != (unsigned int)aaToken[ii].size(); ++jj)
            {
                this->SetValue(aaToken[ii][jj], ii, jj);
            }
        }

        return true;
    }


    VALUE _value;

    if (format)
    {
        if (sscanf(value.c_str(), format, &_value) != 1)
        {
            return false;
        }
    }
    else
    {
        switch (BASIC_TYPE)
        {
        case BASIC_TYPE_CHAR:
        case BASIC_TYPE_INT:
        case BASIC_TYPE_UINT:
        case BASIC_TYPE_DOUBLE:
            if (!ConvertToNumber<VALUE>(value, _value))
            {
                return false;
            }
            break;

        default:
            assert(0);
            return false;
        }
    }

    return this->SetValue(_value, i, j);
}

template <>
inline bool ValuePtrPrivateArray2T<sdiTriple, sdiTripleList2, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>::SetValueFromString(
    const sdiString&    value,
    const char*        format,
    const unsigned int i,
    const unsigned int j)
{
    double _value[3];
    if (format)
    {
        if (sscanf(value.c_str(), format, &_value[0], &_value[1], &_value[2]) != 3)
        {
            return false;
        }
    }
    else
    {
        if (sscanf(value.c_str(), "%lf %lf %lf", &_value[0], &_value[1], &_value[2]) != 3)
        {
            return false;
        }
    }
    this->SetValue(sdiTriple(_value[0], _value[1], _value[2]), i, j);
    return true;
}

template <>
ValuePtrPrivateArray2T<sdiValueEntity, sdiValueEntityList2, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::ValuePtrPrivateArray2T(
    const unsigned int  sizeDimFirst,         //! size of first (outermost) dimension
    const unsigned int  sizeDimSecond)        //! size of second dimension
    : sdiValuePtr()
{
    p_aaValue.Reserve(sizeDimFirst, sizeDimSecond);
}

template <>
inline unsigned int ValuePtrPrivateArray2T<sdiValueEntity, sdiValueEntityList2, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::
GetArrayDimensionExtent(const unsigned int dimensionIndex) const
{
    return p_aaValue.GetIdListCount(dimensionIndex);
}

template <>
inline bool ValuePtrPrivateArray2T<sdiValueEntity, sdiValueEntityList2, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetValue(
    sdiValueEntity&         value, const unsigned int i, const unsigned int j) const
{
    return p_aaValue.GetEntity(value, i, j);
}

template <>
inline bool ValuePtrPrivateArray2T<sdiValueEntity, sdiValueEntityList2, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetValueAsString(
    sdiString&          value,
    const char*        format,
    const unsigned int i,
    const unsigned int j) const
{
    if (i != UINT_MAX)
    { 
        sdiValueEntity _entity;
        if (!p_aaValue.GetEntity(_entity, i, j))
        {
            return false;
        }

        return _GetEntityAsString(_entity, value, format);
    }

    return _GetEntityList2AsString(p_aaValue, value, format);
}


template <>
inline bool ValuePtrPrivateArray2T<sdiValueEntity, sdiValueEntityList2, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::SetValue(
    const sdiValueEntity&   value, const unsigned int i, const unsigned int j)
{
    return p_aaValue.SetEntity(value, i, j);
}

template <>
inline bool ValuePtrPrivateArray2T<sdiValueEntity, sdiValueEntityList2, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::SetValueFromString(
    const sdiString&    value,
    const char*        format,
    const unsigned int i,
    const unsigned int j)
{
    if (i != UINT_MAX)
    { 
        sdiValueEntity _entity;
        if (!_SetEntityFromString(_entity, value, format))
        {
            return false;
        }

        return this->SetValue(_entity, i, j);
    }

    return _SetEntityList2FromString(p_aaValue, value, format);
}


template <>
const void* ValuePtrPrivateArray2T<sdiValueEntity, sdiValueEntityList2, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetMemoryLocation(
    const unsigned int i, const unsigned int j) const
{
    const sdiUIntList2& list = p_aaValue.GetList();

    if (i >= (unsigned int)list.size())
    {
        assert(0);
        return 0;
    }
    if (j >= (unsigned int)list[i].size())
    {
        assert(0);
        return 0;
    }
    return &list[i][j];
}

template <>
void* ValuePtrPrivateArray2T<sdiValueEntity, sdiValueEntityList2, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetMemoryLocation(
    const unsigned int i, const unsigned int j)
{
    sdiUIntList2& list = p_aaValue.GetList();

    if (i >= (unsigned int)list.size())
    {
        assert(0);
        return 0;
    }
    if (j >= (unsigned int)list[i].size())
    {
        assert(0);
        return 0;
    }
    return &list[i][j];
}


template <>
inline const int* ValuePtrPrivateArray2T<int, sdiIntList2, BASIC_TYPE_INT, COMPOUND_TYPE_SINGLE>::GetValuePointerInt(const unsigned int i, const unsigned int j) const
{
    if (i >= (unsigned int)p_aaValue.size())
    {
        assert(0);
        return 0;
    }
    if (j >= (unsigned int)p_aaValue[i].size())
    {
        assert(0);
        return 0;
    }
    return &p_aaValue[i][j];
}

template <>
inline const unsigned int* ValuePtrPrivateArray2T<unsigned int, sdiUIntList2, BASIC_TYPE_UINT, COMPOUND_TYPE_SINGLE>::GetValuePointerUInt(const unsigned int i, const unsigned int j) const
{
    if (i >= (unsigned int)p_aaValue.size())
    {
        assert(0);
        return 0;
    }
    if (j >= (unsigned int)p_aaValue[i].size())
    {
        assert(0);
        return 0;
    }
    return &p_aaValue[i][j];
}

template <>
inline const double* ValuePtrPrivateArray2T<double, sdiDoubleList2, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_SINGLE>::GetValuePointerDouble(const unsigned int i, const unsigned int j) const
{
    if (i >= (unsigned int)p_aaValue.size())
    {
        assert(0);
        return 0;
    }
    if (j >= (unsigned int)p_aaValue[i].size())
    {
        assert(0);
        return 0;
    }
    return &p_aaValue[i][j];
}


template <>
inline const sdiTriple* ValuePtrPrivateArray2T<sdiTriple, sdiTripleList2, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>::GetValuePointerTriple(const unsigned int i, const unsigned int j) const
{
    if (i >= (unsigned int)p_aaValue.size())
    {
        assert(0);
        return 0;
    }
    if (j >= (unsigned int)p_aaValue[i].size())
    {
        assert(0);
        return 0;
    }
    return &p_aaValue[i][j];
}

// 2-d arrays
template <>
inline const sdiIntList2* ValuePtrPrivateArray2T<int, sdiIntList2, BASIC_TYPE_INT, COMPOUND_TYPE_SINGLE>::GetValuePointerIntList2() const
{
    return &p_aaValue;
}

template <>
inline const sdiUIntList2* ValuePtrPrivateArray2T<unsigned int, sdiUIntList2, BASIC_TYPE_UINT, COMPOUND_TYPE_SINGLE>::GetValuePointerUIntList2() const
{
    return &p_aaValue;
}

template <>
inline const sdiDoubleList2* ValuePtrPrivateArray2T<double, sdiDoubleList2, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_SINGLE>::GetValuePointerDoubleList2() const
{
    return &p_aaValue;
}

template <>
inline const sdiTripleList2* ValuePtrPrivateArray2T<sdiTriple, sdiTripleList2, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>::GetValuePointerTripleList2() const
{
    return &p_aaValue;
}

template <>
inline const sdiValueEntityList2* ValuePtrPrivateArray2T<sdiValueEntity, sdiValueEntityList2, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetValuePointerEntityList2() const
{
    return &p_aaValue;
}



// *********************************************************************************
// sdiValuePtr Implementation
// *********************************************************************************

// Single values using typed values
sdiValuePtr* sdiValuePtr::Create(const bool&           value)
{
    return new sdiValuePtrPrivateArray0T<bool, BASIC_TYPE_BOOL, COMPOUND_TYPE_SINGLE>(value);
}

sdiValuePtr* sdiValuePtr::Create(const char&           value)
{
    return new sdiValuePtrPrivateArray0T<char, BASIC_TYPE_CHAR, COMPOUND_TYPE_SINGLE>(value);
}

sdiValuePtr* sdiValuePtr::Create(const int&            value)
{
    return new sdiValuePtrPrivateArray0T<int, BASIC_TYPE_INT, COMPOUND_TYPE_SINGLE>(value);
}

sdiValuePtr* sdiValuePtr::Create(const unsigned int&   value)
{
    return new sdiValuePtrPrivateArray0T<unsigned int, BASIC_TYPE_UINT, COMPOUND_TYPE_SINGLE>(value);
}

sdiValuePtr* sdiValuePtr::Create(const double&         value)
{
    return new sdiValuePtrPrivateArray0T<double, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_SINGLE>(value);
}

sdiValuePtr* sdiValuePtr::Create(const sdiString&       value)
{
    return new sdiValuePtrPrivateArray0T<sdiString, BASIC_TYPE_STRING, COMPOUND_TYPE_SINGLE>(value);
}

sdiValuePtr* sdiValuePtr::Create(const sdiTriple&       value)
{
    return new sdiValuePtrPrivateArray0T<sdiTriple, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>(value);
}

sdiValuePtr* sdiValuePtr::Create(const sdiValueEntity&         value)
{
    return new sdiValuePtrPrivateArray0T<sdiValueEntity, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>(value);
}

// Single values specifying types and optional string and format. If string is not
// specified, then unset value is created of specified type, with a value of 0
// for numeric types. The format argument is required for compound types, and is
// used internally to parse the input string. Function returns 0 if input arguments
// are inconsistent.
sdiValuePtr* sdiValuePtr::Create(const sdiBasicType          basicType,
                           const sdiCompoundType       compoundType,
                           const sdiString&          value,
                           const char*              format)
{
    switch (compoundType)
    {
    case COMPOUND_TYPE_SINGLE:
        switch (basicType)
        {
        case BASIC_TYPE_BOOL:
            return new sdiValuePtrPrivateArray0T<bool,           BASIC_TYPE_BOOL,   COMPOUND_TYPE_SINGLE>(value, format);
        case BASIC_TYPE_CHAR:
            return new sdiValuePtrPrivateArray0T<char,           BASIC_TYPE_CHAR,   COMPOUND_TYPE_SINGLE>(value, format);
        case BASIC_TYPE_INT:
            return new sdiValuePtrPrivateArray0T<int,            BASIC_TYPE_INT,    COMPOUND_TYPE_SINGLE>(value, format);
        case BASIC_TYPE_UINT:
            return new sdiValuePtrPrivateArray0T<unsigned int,   BASIC_TYPE_UINT,   COMPOUND_TYPE_SINGLE>(value, format);
        case BASIC_TYPE_DOUBLE:
            return new sdiValuePtrPrivateArray0T<double,         BASIC_TYPE_DOUBLE, COMPOUND_TYPE_SINGLE>(value, format);
        case BASIC_TYPE_STRING:
            return new sdiValuePtrPrivateArray0T<sdiString,       BASIC_TYPE_STRING, COMPOUND_TYPE_SINGLE>(value, format);
        default:
            assert(0);
            return 0;
        }
        break;

    case COMPOUND_TYPE_TRIPLE:
        switch (basicType)
        {
        case BASIC_TYPE_DOUBLE:
            return new sdiValuePtrPrivateArray0T<sdiTriple,       BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>(value, format);
        default:
            assert(0);
            return 0;
        }
        break;

    case COMPOUND_TYPE_ENTITY:
        switch (basicType)
        {
        case BASIC_TYPE_UINT:
            return new sdiValuePtrPrivateArray0T<sdiValueEntity,         BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>(value, format);
        default:
            assert(0);
            return 0;
        }
        break;
    }

    assert(0);
    return 0;
}


// 1-d arrays using typed values; the term "bag" is used for arrays where the lookup is by entityFullType.
sdiValuePtr* sdiValuePtr::Create(const sdiBoolList&     aValue)
{
    return new ValuePtrPrivateArray1T<bool, sdiBoolList, BASIC_TYPE_BOOL, COMPOUND_TYPE_SINGLE>(aValue);
}

sdiValuePtr* sdiValuePtr::Create(const sdiCharList&      aValue)
{
    return new ValuePtrPrivateArray1T<char, sdiCharList, BASIC_TYPE_CHAR, COMPOUND_TYPE_SINGLE>(aValue);
}

sdiValuePtr* sdiValuePtr::Create(const sdiIntList&      aValue)
{
    return new ValuePtrPrivateArray1T<int, sdiIntList, BASIC_TYPE_INT, COMPOUND_TYPE_SINGLE>(aValue);
}

sdiValuePtr* sdiValuePtr::Create(const sdiUIntList&     aValue)
{
    return new ValuePtrPrivateArray1T<unsigned int, sdiUIntList, BASIC_TYPE_UINT, COMPOUND_TYPE_SINGLE>(aValue);
}

sdiValuePtr* sdiValuePtr::Create(const sdiDoubleList&   aValue)
{
    return new ValuePtrPrivateArray1T<double, sdiDoubleList, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_SINGLE>(aValue);
}

sdiValuePtr* sdiValuePtr::Create(const sdiStringList&   aValue)
{
    return new ValuePtrPrivateArray1T<sdiString, sdiStringList, BASIC_TYPE_STRING, COMPOUND_TYPE_SINGLE>(aValue);
}

sdiValuePtr* sdiValuePtr::Create(const sdiTripleList&   aValue)
{
    return new ValuePtrPrivateArray1T<sdiTriple, sdiTripleList, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>(aValue);
}

sdiValuePtr* sdiValuePtr::Create(const sdiValueEntityList&     aValue)
{
    return new ValuePtrPrivateArray1T<sdiValueEntity, sdiValueEntityList, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>(aValue);
}

// move
// ----
// 1-d arrays using typed values; the term "bag" is used for arrays where the lookup is by entityFullType.
sdiValuePtr* sdiValuePtr::Create(sdiBoolList&&     aValue)
{
    return new ValuePtrPrivateArray1T<bool, sdiBoolList, BASIC_TYPE_BOOL, COMPOUND_TYPE_SINGLE>(std::move(aValue));
}

sdiValuePtr* sdiValuePtr::Create(sdiCharList&&      aValue)
{
    return new ValuePtrPrivateArray1T<char, sdiCharList, BASIC_TYPE_CHAR, COMPOUND_TYPE_SINGLE>(std::move(aValue));
}

sdiValuePtr* sdiValuePtr::Create(sdiIntList&&      aValue)
{
    return new ValuePtrPrivateArray1T<int, sdiIntList, BASIC_TYPE_INT, COMPOUND_TYPE_SINGLE>(std::move(aValue));
}

sdiValuePtr* sdiValuePtr::Create(sdiUIntList&&     aValue)
{
    return new ValuePtrPrivateArray1T<unsigned int, sdiUIntList, BASIC_TYPE_UINT, COMPOUND_TYPE_SINGLE>(std::move(aValue));
}

sdiValuePtr* sdiValuePtr::Create(sdiDoubleList&&   aValue)
{
    return new ValuePtrPrivateArray1T<double, sdiDoubleList, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_SINGLE>(std::move(aValue));
}

sdiValuePtr* sdiValuePtr::Create(sdiStringList&&   aValue)
{
    return new ValuePtrPrivateArray1T<sdiString, sdiStringList, BASIC_TYPE_STRING, COMPOUND_TYPE_SINGLE>(std::move(aValue));
}

sdiValuePtr* sdiValuePtr::Create(sdiTripleList&&   aValue)
{
    return new ValuePtrPrivateArray1T<sdiTriple, sdiTripleList, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>(std::move(aValue));
}

sdiValuePtr* sdiValuePtr::Create(sdiValueEntityList&&     aValue)
{
    return new ValuePtrPrivateArray1T<sdiValueEntity, sdiValueEntityList, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>(std::move(aValue));
}


// 2-d arrays using typed values; the term "bag" is used for arrays where the first lookup is by entityFullType.
sdiValuePtr* sdiValuePtr::Create(const sdiIntList2&     aaValue)
{
    return new ValuePtrPrivateArray2T<int, sdiIntList2, BASIC_TYPE_INT, COMPOUND_TYPE_SINGLE>(aaValue);
}

sdiValuePtr* sdiValuePtr::Create(const sdiUIntList2&    aaValue)
{
    return new ValuePtrPrivateArray2T<unsigned int, sdiUIntList2, BASIC_TYPE_UINT, COMPOUND_TYPE_SINGLE>(aaValue);
}

sdiValuePtr* sdiValuePtr::Create(const sdiDoubleList2&  aaValue)
{
    return new ValuePtrPrivateArray2T<double, sdiDoubleList2, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_SINGLE>(aaValue);
}

sdiValuePtr* sdiValuePtr::Create(const sdiTripleList2&  aaValue)
{
    return new ValuePtrPrivateArray2T<sdiTriple, sdiTripleList2, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>(aaValue);
}

sdiValuePtr* sdiValuePtr::Create(const sdiValueEntityList2&    aaValue)
{
    return new ValuePtrPrivateArray2T<sdiValueEntity, sdiValueEntityList2, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>(aaValue);
}

// 2-d arrays using typed values; move
sdiValuePtr* sdiValuePtr::Create(sdiIntList2&&     aaValue)
{
    return new ValuePtrPrivateArray2T<int, sdiIntList2, BASIC_TYPE_INT, COMPOUND_TYPE_SINGLE>(std::move(aaValue));
}

sdiValuePtr* sdiValuePtr::Create(sdiUIntList2&&    aaValue)
{
    return new ValuePtrPrivateArray2T<unsigned int, sdiUIntList2, BASIC_TYPE_UINT, COMPOUND_TYPE_SINGLE>(std::move(aaValue));
}

sdiValuePtr* sdiValuePtr::Create(sdiDoubleList2&&  aaValue)
{
    return new ValuePtrPrivateArray2T<double, sdiDoubleList2, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_SINGLE>(std::move(aaValue));
}

sdiValuePtr* sdiValuePtr::Create(sdiTripleList2&&  aaValue)
{
    return new ValuePtrPrivateArray2T<sdiTriple, sdiTripleList2, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>(std::move(aaValue));
}

sdiValuePtr* sdiValuePtr::Create(sdiValueEntityList2&&    aaValue)
{
    return new ValuePtrPrivateArray2T<sdiValueEntity, sdiValueEntityList2, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>(std::move(aaValue));
}





// Creates unset arrays of specified types, with reserved values for dimensions
sdiValuePtr* sdiValuePtr::Create(const sdiBasicType          basicType,
                           const sdiCompoundType       compoundType,
                           const unsigned int       sizeDimFirst,         //! size of first (outermost) dimension
                           const unsigned int       sizeDimSecond)        //! size of second dimension
{
    //! 2d array
    if (sizeDimFirst && sizeDimSecond)
    {
        if (compoundType == COMPOUND_TYPE_SINGLE)
        {
            switch (basicType)
            {
            case BASIC_TYPE_INT:
                return new ValuePtrPrivateArray2T<int, sdiIntList2, BASIC_TYPE_INT, COMPOUND_TYPE_SINGLE>(sizeDimFirst, sizeDimSecond);
            case BASIC_TYPE_UINT:
                return new ValuePtrPrivateArray2T<unsigned int, sdiUIntList2, BASIC_TYPE_UINT, COMPOUND_TYPE_SINGLE>(sizeDimFirst, sizeDimSecond);
            case BASIC_TYPE_DOUBLE:
                return new ValuePtrPrivateArray2T<double, sdiDoubleList2, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_SINGLE>(sizeDimFirst, sizeDimSecond);
            default:
                break;
            }
        }
        else if (compoundType == COMPOUND_TYPE_TRIPLE)
        {
            switch (basicType)
            {
            case BASIC_TYPE_DOUBLE:
                return new ValuePtrPrivateArray2T<sdiTriple, sdiTripleList2, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>(sizeDimFirst, sizeDimSecond);
            default:
                break;
            }
        }
        else if (compoundType == COMPOUND_TYPE_ENTITY)
        {
            return new ValuePtrPrivateArray2T<sdiValueEntity, sdiValueEntityList2, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>(sizeDimFirst, sizeDimSecond);
        }

        assert(0);
        return 0;
    }


    //! Not an array
    if (!sizeDimFirst)
    {
        assert(0);
        return 0;
    }


    //! 1d array
    switch (compoundType)
    {
    case COMPOUND_TYPE_SINGLE:
        switch (basicType)
        {
        case BASIC_TYPE_BOOL:
            return new ValuePtrPrivateArray1T<bool,           sdiBoolList,   BASIC_TYPE_BOOL,   COMPOUND_TYPE_SINGLE>(sizeDimFirst);
        case BASIC_TYPE_CHAR:
            return new ValuePtrPrivateArray1T<char,           sdiCharList,    BASIC_TYPE_CHAR,  COMPOUND_TYPE_SINGLE>(sizeDimFirst);
        case BASIC_TYPE_INT:
            return new ValuePtrPrivateArray1T<int,            sdiIntList,    BASIC_TYPE_INT,    COMPOUND_TYPE_SINGLE>(sizeDimFirst);
        case BASIC_TYPE_UINT:
            return new ValuePtrPrivateArray1T<unsigned int,   sdiUIntList,   BASIC_TYPE_UINT,   COMPOUND_TYPE_SINGLE>(sizeDimFirst);
        case BASIC_TYPE_DOUBLE:
            return new ValuePtrPrivateArray1T<double,         sdiDoubleList, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_SINGLE>(sizeDimFirst);
        case BASIC_TYPE_STRING:
            return new ValuePtrPrivateArray1T<sdiString,       sdiStringList, BASIC_TYPE_STRING, COMPOUND_TYPE_SINGLE>(sizeDimFirst);
        default:
            assert(0);
            return 0;
        }
        break;

    case COMPOUND_TYPE_TRIPLE:
        switch (basicType)
        {
        case BASIC_TYPE_DOUBLE:
            return new ValuePtrPrivateArray1T<sdiTriple,       sdiTripleList, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>(sizeDimFirst);
        default:
            assert(0);
            return 0;
        }
        break;

    case COMPOUND_TYPE_ENTITY:
        if      (sizeDimFirst == 1)
        {
            return new ValuePtrPrivateArray1T<sdiValueEntity,         sdiValueEntityList,   BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>(sizeDimFirst);
        }
        assert(0);
        return 0;
        break;

    }

    assert(0);
    return 0;
}

//! /////////////////////////////////////////////////////////////////////////////////
//! Values with associated strings
//! /////////////////////////////////////////////////////////////////////////////////

// *********************************************************************************
// sdiValuePtrPrivateArray0TStr Definition
// *********************************************************************************
template <class VALUE, sdiBasicType BASIC_TYPE, sdiCompoundType COMPOUND_TYPE>
class sdiValuePtrPrivateArray0TStr : public sdiValuePtrPrivateArray0T<VALUE, BASIC_TYPE, COMPOUND_TYPE>
{
public:

    sdiValuePtrPrivateArray0TStr(const VALUE&          value,
                              const sdiString&       associatedString)
        : sdiValuePtrPrivateArray0T<VALUE, BASIC_TYPE, COMPOUND_TYPE>(value), p_associatedString(associatedString)
    {
    }

    sdiValuePtrPrivateArray0TStr(const sdiString& value,
                              const sdiString& associatedString,
                              const char*     format)
        : sdiValuePtr(), p_associatedString(associatedString)
    {
        this->SetValueFromString(value, format);
    }

    sdiValuePtrPrivateArray0TStr(const sdiValuePtrPrivateArray0TStr& other)
        : sdiValuePtrPrivateArray0T<VALUE, BASIC_TYPE, COMPOUND_TYPE>(other), p_associatedString(other.p_associatedString)
    {
    }

    virtual ~sdiValuePtrPrivateArray0TStr()
    {
    }

    //! Creates clone of value
    virtual sdiValuePtr* Clone() const
    {
        return new sdiValuePtrPrivateArray0TStr(*this);
    }


    ////////////////////////////////////////////////////////////////////////////////////
    //! Query
    ////////////////////////////////////////////////////////////////////////////////////

    //! Comparison with other
    virtual bool Equals(const sdiValuePtr* pOther) const
    {
        if (!sdiValuePtrPrivateArray0T<VALUE, BASIC_TYPE, COMPOUND_TYPE>::Equals(pOther))
        {
            return false;
        }

        sdiString associatedString;
        if (!pOther->GetAssociatedString(associatedString))
        {
            return false;
        }
        return associatedString == p_associatedString ? true : false;
    }

    ////////////////////////////////////////////////////////////////////////////////////
    //! Get value(s)
    ////////////////////////////////////////////////////////////////////////////////////

    //! Retrieve associated string; returns false if not constructed as such.
    inline bool GetAssociatedString(sdiString& associatedString) const
    {
        associatedString = p_associatedString;
        return true;
    }

    
    inline virtual std::size_t GetValueMemoryUsage() const
    {
        return sdiValuePtrPrivateArray0T<VALUE, BASIC_TYPE, COMPOUND_TYPE>::GetValueMemoryUsage() + sizeof(p_associatedString);
    }


private:

    const sdiString      p_associatedString;
};


sdiValuePtr* sdiValuePtr::Create(const bool& value, const sdiString& associatedString)
{
    return new sdiValuePtrPrivateArray0TStr<bool, BASIC_TYPE_BOOL, COMPOUND_TYPE_SINGLE>(value, associatedString);
}

sdiValuePtr* sdiValuePtr::Create(const char& value, const sdiString& associatedString)
{
    return new sdiValuePtrPrivateArray0TStr<char, BASIC_TYPE_CHAR, COMPOUND_TYPE_SINGLE>(value, associatedString);
}

sdiValuePtr* sdiValuePtr::Create(const int& value, const sdiString& associatedString)
{
    return new sdiValuePtrPrivateArray0TStr<int, BASIC_TYPE_INT, COMPOUND_TYPE_SINGLE>(value, associatedString);
}

sdiValuePtr* sdiValuePtr::Create(const unsigned int& value, const sdiString& associatedString)
{
    return new sdiValuePtrPrivateArray0TStr<unsigned int, BASIC_TYPE_UINT, COMPOUND_TYPE_SINGLE>(value, associatedString);
}

sdiValuePtr* sdiValuePtr::Create(const double& value, const sdiString& associatedString)
{
    return new sdiValuePtrPrivateArray0TStr<double, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_SINGLE>(value, associatedString);
}

sdiValuePtr* sdiValuePtr::Create(const sdiString& value, const sdiString& associatedString)
{
    return new sdiValuePtrPrivateArray0TStr<sdiString, BASIC_TYPE_STRING, COMPOUND_TYPE_SINGLE>(value, associatedString);
}

sdiValuePtr* sdiValuePtr::Create(const sdiTriple& value, const sdiString& associatedString)
{
    return new sdiValuePtrPrivateArray0TStr<sdiTriple, BASIC_TYPE_DOUBLE, COMPOUND_TYPE_TRIPLE>(value, associatedString);
}

sdiValuePtr* sdiValuePtr::Create(const sdiValueEntity& value, const sdiString& associatedString)
{
    return new sdiValuePtrPrivateArray0TStr<sdiValueEntity, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>(value, associatedString);
}

//! /////////////////////////////////////////////////////////////////////////////////
//! END: Values with associated strings
//! /////////////////////////////////////////////////////////////////////////////////


// Only exposed means to destroy objectes created using above Create functions is
// using static Destroy function, which deals with ref counting.
void sdiValuePtr::Destroy(sdiValuePtr* ptr)
{
    if (!ptr)
    {
        return;
    }
    if (ptr->IsMemoryExternallyManaged())
    {
        return;
    }
    delete ptr;
}


// Get sdiValueEntityType; returns an invalid object if sdiValue does not correspond
// to an sdiValueEntity, sdiValueEntityList, or Qualifier (not QualifierList).
sdiValueEntityType sdiValuePtr::GetEntityFullType() const
{
    return sdiValueEntityType();
}


// Comparison with other
bool sdiValuePtr::Equals(const sdiValuePtr* pOther) const
{
    return false;
}
bool sdiValuePtr::LessThan(const sdiValuePtr* pOther) const
{
    return false;
}



template <class VALUE, sdiBasicType BASIC_TYPE, sdiCompoundType COMPOUND_TYPE>
inline std::size_t sdiValuePtrPrivateArray0T<VALUE, BASIC_TYPE, COMPOUND_TYPE>::GetValueMemoryUsage() const
{
    return sizeof(p_value);
}



template <class VALUE, class VALUE_LIST, sdiBasicType BASIC_TYPE, sdiCompoundType COMPOUND_TYPE>
inline std::size_t ValuePtrPrivateArray1T<VALUE, VALUE_LIST, BASIC_TYPE, COMPOUND_TYPE>::GetValueMemoryUsage() const
{
    return sizeof(p_aValue) + sizeof(typename VALUE_LIST::value_type) * p_aValue.size();
}



template <>
inline std::size_t ValuePtrPrivateArray1T<sdiValueEntity, sdiValueEntityList, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetValueMemoryUsage() const
{
    return sizeof(p_aValue) +
           sizeof(sdiUIntList) +
           sizeof(unsigned int) * p_aValue.GetIdListCount();
}



template <class VALUE, class VALUE_LIST, sdiBasicType BASIC_TYPE, sdiCompoundType COMPOUND_TYPE>
inline std::size_t ValuePtrPrivateArray2T<VALUE, VALUE_LIST, BASIC_TYPE, COMPOUND_TYPE>::GetValueMemoryUsage() const
{
    size_t memUsed = sizeof(p_aaValue);
    for (size_t i = 0; i < p_aaValue.size(); ++i)
    {
        memUsed += sizeof(typename VALUE_LIST::value_type) * p_aaValue[i].size();
    }
    return memUsed;
}



template <>
inline std::size_t ValuePtrPrivateArray2T<sdiValueEntity, sdiValueEntityList2, BASIC_TYPE_UINT, COMPOUND_TYPE_ENTITY>::GetValueMemoryUsage() const
{
    size_t memUsed = sizeof(p_aaValue);
    const sdiUIntList2& ids = p_aaValue.GetList();

    for (size_t i = 0; i < ids.size(); ++i)
    {
        memUsed += sizeof(unsigned int) * ids[i].size();
    }
    return memUsed;
}
