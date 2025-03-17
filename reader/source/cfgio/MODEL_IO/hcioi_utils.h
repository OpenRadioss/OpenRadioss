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
#ifndef HCIOI_UTILS_H
#define HCIOI_UTILS_H

#include <stdio.h>
#include "hcio.h"
#include "mv_solver_input_infos.h"
#include <UTILS/mv_string.h>

class IDescriptor;
class IMECPreObject;
typedef enum cp_value_type_e
{
    CP_VALUE_NOT_ASSOCIATED = 0,
    CP_VALUE_DATANAMES,
    CP_VALUE_MAX
} cp_value_type_t;

void killBlanksEnd(char *buffer);
bool IsValueDigitInRange(const unsigned int &value, const unsigned int &val_range);
bool CopyPreObjectAttribFromPreObject(IMECPreObject& preobject_to, const string& attrib_to, const IDescriptor* descrp_to, const IMECPreObject& preobject_from,
                           const string& attrib_from, int index);
HCIO_DATA_DLL_API void MergeArrayAttributesToPreobject(const IDescriptor* pdescrp, IMECPreObject& preobject_base, IMECPreObject& preobject_to_merge);
void ResizeArrayAttributesToPreObject(IMECPreObject& pre_object, const IDescriptor* descr_p, int arr_ikw, int size);
HCIO_DATA_DLL_API std::string GetAttribNameFromDrawable(const IDescriptor* pdescrp, const string& name);

HCIO_DATA_DLL_API void UpdateEntityIDOffsetCFG(std::vector<IMECPreObject*>* p_preobjlst, const char* submodelsolkey, bool doUnOffset);

#endif 
