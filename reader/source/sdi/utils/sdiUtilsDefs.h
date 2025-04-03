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

#if !defined(SDIUTILSDEFS__INCLUDED_)
#define SDIUTILSDEFS__INCLUDED_

#include <assert.h>
#include <limits.h>
#include <algorithm>
#include <string>
#include <vector>

// *************************************************************************************
// Windows export macro.
// *************************************************************************************
#if defined(OS_WIN) && !defined(NO_DECLS)
#ifdef SDIUTILS_EXPORT
#undef SDIUTILS_DECLS
#define SDIUTILS_DECLS __declspec(dllexport)
#else
#undef SDIUTILS_DECLS
#define SDIUTILS_DECLS __declspec(dllimport)
#endif  //! DESCRIPTOR_EXPORT
#else
#undef SDIUTILS_DECLS
#define SDIUTILS_DECLS
#endif //! OS_WIN

#ifdef OS_WIN
// disable warnings on needs to have dll-interface to be used by
// clients of class xx.  windows stl template classes generate these.
#  pragma warning(disable:4251)
#endif  // OS_WIN


typedef std::string                  sdiString;
template<typename T> using           sdiVector = std::vector<T>;
typedef sdiVector<bool>              sdiBoolList;
typedef sdiVector<char>              sdiCharList;
typedef sdiVector<int>               sdiIntList;
typedef sdiVector<unsigned int>      sdiUIntList;
typedef sdiVector<double>            sdiDoubleList;
typedef sdiVector<sdiString>         sdiStringList;
typedef sdiVector<sdiBoolList>       sdiBoolList2;
typedef sdiVector<sdiCharList>       sdiCharList2;
typedef sdiVector<sdiIntList>        sdiIntList2;
typedef sdiVector<sdiUIntList>       sdiUIntList2;
typedef sdiVector<sdiDoubleList>     sdiDoubleList2;
typedef sdiVector<sdiStringList>     sdiStringList2;


// *********************************************************************************
// sdiValue types
// *********************************************************************************

// List of all supported value basic types
enum sdiBasicType
{
    BASIC_TYPE_UNDEFINED,
    BASIC_TYPE_BOOL,
    BASIC_TYPE_CHAR,
    BASIC_TYPE_INT,
    BASIC_TYPE_UINT,
    BASIC_TYPE_DOUBLE,
    BASIC_TYPE_STRING
};

// List of all supported value compound types
enum sdiCompoundType
{
    COMPOUND_TYPE_UNDEFINED,
    COMPOUND_TYPE_SINGLE,
    COMPOUND_TYPE_TRIPLE,
    COMPOUND_TYPE_ENTITY
};

// Display status (not yet used)
enum sdiDisplayStatus
{
    DISPLAY_STATUS_OFF,
    DISPLAY_STATUS_ON,
    DISPLAY_STATUS_ALWAYS_ON
};

#endif //! !defined(SDIUTILSDEFS__INCLUDED_)
