//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2026 Altair Engineering Inc.
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
#ifndef HCIOI_UTILS_H
#define HCIOI_UTILS_H

#include <stdio.h>
#include "hcio.h"
#include "mv_solver_input_infos.h"
#include <UTILS/mv_string.h>
#include <unordered_map>
#include <string>
#include <chrono>
#include <vector>

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


int cfgio_atoi_fast(const char* p) noexcept;
double  cfgio_atof_fast(const char* s) noexcept;
bool cfgio_parse_int_or_blank_fast(const char* p, int width, int& out, bool& is_blank, int& chars_read) noexcept;
bool cfgio_parse_double_or_blank_fast(const char* s, int len, double& val, bool& is_blank, int& chars_read) noexcept;

bool CopyPreObjectAttribFromPreObject(IMECPreObject& preobject_to, const string& attrib_to, const IDescriptor* descrp_to, const IMECPreObject& preobject_from,
                           const string& attrib_from, int index);
HCIO_DATA_DLL_API void MergeArrayAttributesToPreobject(const IDescriptor* pdescrp, IMECPreObject& preobject_base, IMECPreObject& preobject_to_merge);
void ResizeArrayAttributesToPreObject(IMECPreObject& pre_object, const IDescriptor* descr_p, int arr_ikw, int size);
HCIO_DATA_DLL_API std::string GetAttribNameFromDrawable(const IDescriptor* pdescrp, const string& name);

struct cfglnksubdescriptor;
HCIO_DATA_DLL_API void UpdateEntityIDOffsetCFG(
    std::vector<IMECPreObject*>* p_preobjlst, const char* submodelsolkey, bool doUnOffset,
    const std::unordered_map<std::string, cfglnksubdescriptor>* cfglnksubdescriptor_map = nullptr);


// PreObject lookup function declarations
/**
 * @brief Find a PreObject by ID within a specific object type
 *
 * More efficient version when the object type is known.
 * Provides O(log n) performance using binary search.
 *
 * @param preobj_list Array of vectors containing PreObjects for each type
 * @param type The specific object type to search in
 * @param id The ID to search for
 * @return Pointer to found PreObject or nullptr if not found
 */
HCIO_DATA_DLL_API IMECPreObject* FindPreObjectById(
    std::vector<IMECPreObject*>* preobj_list,
    obj_type_e type,
    int id);

/**
 * @brief Find all PreObjects within a specific ID range and type
 *
 * Efficiently finds all objects of a specific type within an ID range.
 * Uses binary search to find range bounds in O(log n) time.
 *
 * @param preobj_list Array of vectors containing PreObjects for each type
 * @param type The object type to search in
 * @param min_id Minimum ID in range (inclusive)
 * @param max_id Maximum ID in range (inclusive)
 * @param stats Optional statistics collector
 * @return Vector of all PreObjects in the specified range
 */
HCIO_DATA_DLL_API std::vector<IMECPreObject*> FindPreObjectsByIdRange(
    std::vector<IMECPreObject*>* preobj_list,
    obj_type_e type,
    int min_id,
    int max_id);

/**
 * @brief Get a PreObject from a reference attribute in another PreObject
 *
 * Resolves object references by extracting ID from various attribute types
 * (VTY_OBJECT, VTY_INT, VTY_UINT, VTY_STRING) and finding the referenced object.
 *
 * @param main_preobject The PreObject containing the reference
 * @param ref_obj_skeyword The attribute name containing the reference
 * @param preobj_list Array of vectors containing PreObjects for each type
 * @return Pointer to referenced PreObject or nullptr if not found
 */
HCIO_DATA_DLL_API IMECPreObject* GetPreObjectFromReference(
    IMECPreObject* main_preobject,
    const std::string& ref_obj_skeyword,
    const IDescriptor* descrp,
    std::vector<IMECPreObject*>* preobj_list);


HCIO_DATA_DLL_API bool VerifyPreObjectSorting(
    std::vector<IMECPreObject*>* preobj_list,
    obj_type_e type = static_cast<obj_type_e>(0)); // HCDI_OBJ_TYPE_NULL


HCIO_DATA_DLL_API void test_parse_string_fast_int_double();

HCIO_DATA_DLL_API void test_cfgio_atof_fast_precision();

#endif 
