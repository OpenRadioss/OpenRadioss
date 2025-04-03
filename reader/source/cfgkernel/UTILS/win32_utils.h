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

/* Load once if possible */
#if _MSC_VER > 1000
#pragma once
#endif

/* We don't need that anymore */

#ifndef __WIN32_UTILS_H__
#define __WIN32_UTILS_H__
#if defined _WIN32 || defined WIN32

/*
  ctype is also defined in Microsoft header, so we need to redefine it here
  cgroup is here just for homogeneity
*/
#ifdef __cplusplus
extern "C" char *cgroup[];
extern "C" char *ctype[];
#else
extern char *cgroup[];
extern char *ctype[];
#endif


/*
  String manipulation functions are not the same for case-unsensitive operations
  Between Unix and Windows
*/
#if !defined(strcasecmp) && defined(_WIN32)
#define strcasecmp  _stricmp
#define strncasecmp _strnicmp
#endif


/* C stuffs */
#ifndef __cplusplus

/* Including some useful generic C stuff */
#include <stdio.h>
#include <stdlib.h>

/* C++ stuffs */
#else /*__cplusplus*/

/* Leaving warnings when compiling STL objects (Visual C++) */
#ifdef _MSC_VER
#pragma warning( disable : 4786)
#pragma warning( disable : 4503)
#endif /* MSC_VER */

#endif  /*__cplusplus*/


#endif /*defined _WIN32 || defined WIN32*/
#endif /* __WIN32_UTILS_H__ */




