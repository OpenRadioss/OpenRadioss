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
/* The following ifdef block is the standard way of creating macros which make exporting 
   from a DLL simpler. All files within this DLL are compiled with the HCIO_DATA_DLL_EXPORTS
   symbol defined on the command line. This symbol should not be defined on any project
   that uses this DLL. This way any other project whose source files include this file see 
   HCIO_DATA_DLL_API functions as being imported from a DLL, whereas this DLL sees symbols
   defined with this macro as being exported.
*/
#if defined(OS_WIN) && !defined(NO_DECLS)
    #ifdef HCIO_DATA_DLL_EXPORTS
        #undef HCIO_DATA_DLL_API
        #define HCIO_DATA_DLL_API __declspec(dllexport)
    #else
        #undef HCIO_DATA_DLL_API
        #define HCIO_DATA_DLL_API __declspec(dllimport)
    #endif
#else
    #undef HCIO_DATA_DLL_API
    #define HCIO_DATA_DLL_API
#endif 

#ifdef __cplusplus
#define EXTERNC extern "C"
#else
#define EXTERNC
#endif
