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
#ifndef __GENERAL_EXTERNAL_FUNCTIONS_H__
#define __GENERAL_EXTERNAL_FUNCTIONS_H__


#include <General/general_memory.h>

/* 
   This Print_* functions are in Message_Window/message_print.c
   They are declared here, due to old compatibility
*/


#ifdef __cplusplus
extern "C" {
#endif

extern void Print_Error(const char *message);
extern void Print_Error_level (const char *message, int level);
extern void Print_Error_level_set_ALWAYS_SAVE_AND_GO_ON (int always_save_and_go_on);
extern void Print_Message(const char *message);
extern void Print_Debug(const char *message);
extern void Print_Info(const char *message);


#ifdef __cplusplus
}
#endif



#ifdef _WIN32
#define mysnprintf _snprintf
#define myvsnprintf _vsnprintf
#else /* ! _WIN32 */
#define mysnprintf snprintf
#define myvsnprintf vsnprintf
#endif /* _WIN32 */

#endif
