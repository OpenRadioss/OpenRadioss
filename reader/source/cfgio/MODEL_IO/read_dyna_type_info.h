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
#ifndef READ_DYNA_TYPE_INFO_H
#define READ_DYNA_TYPE_INFO_H


#include <KERNEL_BASE/Structure_types.h>
#include "hcio.h"
#include <HCDI/hcdi_mv_descriptor.h>
#include <KERNEL_BASE/Structure_fileformat.h>

/** Contains all info necessary for reading and writing an object. */
struct rdy_type_info_s
{
    /** Keyword. <br>
     * Terminate with '_' for subtyped types! */
    const char* keyword;
    /** Kernel type. <br>
     * Terminate with '/' for subtyped types! */
    const char* kernel_type;
    /** Input type (only used for messages during reading up to now). */
    const char* input_type;
    /** Object type (has to correspond to kernel_type!). */
    obj_type_e obj_type;
    /** Bitmap containing info about the format.
        @see rd_format_info_e */
    int format_info;
    /** 1 of the object type needs posttreatment */
    int needs_posttreatment;
    /** Number of cards per object, used to count objects in a block. <br>
     * Special values: <br><ul><li>
     * 0: there can only be one object, no need to count </li><li>
     * -1: number of cards depends on subtype,
     *     use descriptor to find out automatically. </li></ul>
     * This means: <ul><li>
     * If LS-Dyna only allows a single object after a keyword card, use 0.
     *   Example: *CONTACT </li><li>
     * If you know the number of cards per object, use it. If the type is
     *   subtyped, the number of cards has to be the same for all subtypes,
     *   otherwise -1 has to be used. </li><li>
     * For subtyped object types with different number of cards for the
     *   different subtypes, give -1.
     *   <br> ATTENTION: This needs to be coded, it will do the same as "1"
     *        up to now, i.e. counts one object per line!!! </li></ul>
     */
    int nb_cards_per_object;
    
    /**size of the keyword*/
    size_t keyword_size;

    const IDescriptor* p_descr;

    const fileformat_t* p_format;

    int format;
};
typedef struct rdy_type_info_s rdy_type_info_t;


typedef enum dyna_set_type_s
{
    DYS_SET_NODE,
    DYS_SET_SHELL,
    DYS_SET_SEGMENT,
    DYS_SET_PART,
    DYS_SET_BEAM,
	DYS_SET_DISCRETE, 
	DYS_SET_SOLID, 
    DYS_PART,
    DYS_ALL,
    DYS_SET_TSHELL, 
    DYS_LAST
}dyna_set_type_e;

#ifdef __cplusplus

//const rdy_type_info_t unsupported_type = {"*UNSUPPORTEDCARD", "UNSUPPORTEDCARD", "UNSUPPORTEDCARD", UNSUPPORTEDCARD, 0, 0, 0, 16};
//const rdy_type_info_t encrypted_type = {"*ENCRYPTED", "ENCRYPTED", "ENCRYPTED", ENCRYPTED, 0, 0, 0, 10};
#include <HCDI/hcdi_mec_pre_object.h>

extern "C" {

#endif /* __cplusplus*/

HCIO_DATA_DLL_API const rdy_type_info_t *rdy_get_type_info_from_keyword
(const char *keyword, bool inHMFlag, int *format);

HCIO_DATA_DLL_API const rdy_type_info_t *rdy_get_type_info_from_kernel_type
(const char *kernel_type, int format);


const char *dyna_rw_get_id_keyword (const char *skeyword);

/** Builds an skeyword from two strings, similar to dyna_rw_get_id_keyword. */
const char *dyna_rw_cat_keyword (const char *kwd_part1, const char *kwd_part2);

void fill_rdy_type_info();
HCIO_DATA_DLL_API void HCIOI_Free_Rdy_Type_Info();

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
#endif

HCIO_DATA_DLL_API void getUserNamesSolverInfo(vector<pair<string, rdy_type_info_t>>(&objectsolverinfo), bool add_user_subtype, int* format, bool inHMFlag, bool from_import=true);
HCIO_DATA_DLL_API bool IsHmSupportedFlagCFG(int flag);
#endif /* READ_DYNA_TYPE_INFO_H */
