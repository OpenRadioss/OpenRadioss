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
#ifndef DIRECTION_UTILS_H
#define DIRECTION_UTILS_H

#include <KERNEL_BASE/Structure_various.h>
#ifdef __cplusplus


#include <UTILS/mv_string.h>
#include <HCDI/hcdi.h>



dir_type_e MV_get_direction(const string &dir,bool is_extended=true);

string     MV_get_direction(dir_type_e dir,bool is_extended=true);

/** Gets direction flags from a string.<br>
    Returns true if the string was valid
*/
bool       MV_get_direction(const string &dir,bool *xdir_p,bool *ydir_p,bool *zdir_p);

/** Gets a string from direction flags */
string     MV_get_direction(bool xdir,bool ydir,bool zdir);


#endif /* __cplusplus */


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

dir_type_e MV_get_direction(const char *dir,int is_extended);

HC_DATA_DLL_API const char *MV_get_direction_str(dir_type_e dir,int is_extended);

/** Gets direction flags from a string.<br>
    Returns true if the string was valid
*/
HC_DATA_DLL_API int         MV_get_directions(const char *dir,int *xdir_p,int *ydir_p,int*zdir_p);

/** Gets a string from direction flags */
HC_DATA_DLL_API const char *MV_get_directions_str(int xdir,int ydir,int zdir);

#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /*DIRECTION_UTILS_H*/




