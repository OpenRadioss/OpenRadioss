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
#ifndef MV_TYPE_H
#define MV_TYPE_H


#include <UTILS/mv_string.h>

#include <KERNEL_BASE/Structure_types.h>
#include "mv_utils.h"
#include <HCDI/hcdi.h>

/**@name Types*/
//@{

/// Getting type from keyword
HC_DATA_DLL_API object_type_e  MV_get_type(const string &keyword);
/// Getting keyword from type
HC_DATA_DLL_API const string  &MV_get_type(object_type_e obj_type);

/// Gets object type from solver keyword
object_type_e  MV_get_solver_type(const string &keyword);
/// Gets solver keyword from object type
const string  &MV_get_solver_type(object_type_e obj_type);
void MV_delete_type_map();
extern "C"
const char *MV_get_solver_type_cstring(obj_type_e obj_type);
//@}


#endif /* MV_TYPE_H */




