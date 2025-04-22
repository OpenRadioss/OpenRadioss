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
#ifndef MV_FILE_FORMAT_H
#define MV_FILE_FORMAT_H

#ifdef __cplusplus

#include <UTILS/mv_string.h>

#endif /* __cplusplus */
#include <HCDI/hcdi.h>

/**@name File formats*/
/*@{*/

/** Enum for file formats */
enum MvFileFormat_s {
  /** Unknown format */
  FF_UNKNOWN,
  /** RADIOSS subprofile starts */
  /** RADIOSS Starter 4.1 fixed */
  FF_D00_41,
  FF_D00_41F,
  /** RADIOSS Starter 4.1 block */
  FF_D00_41B, 
  /** RADIOSS Starter 4.2 */
  FF_D00_42,
  /** RADIOSS Starter 4.3 */
  FF_D00_43, 
  /** RADIOSS Starter 4.4 */
  FF_D00_44,
  /** RADIOSS Starter 4.8 */
  FF_D00_48,
  /** RADIOSS Starter 4.x (fixed or block) */
  FF_D00_4X,
  /** RADIOSS anim 4.x */
  FF_A00_4X,
  /** RADIOSS Starter 5.1 */
  FF_D00_51,
  FF_D00_52,
  /** RADIOSS Starter 5.x */
  FF_D00_5X,
  /** RADIOSS anim 5.x */
  FF_A00_5X,
  FF_D00_90,
  FF_D00_9X,
  FF_D00_100,
  FF_D00_10X,
  FF_D00_110,
  FF_D00_11X,
  FF_D00_120,
  FF_D00_12X,
  FF_D00_130,
  FF_D00_13X,
  FF_D00_140,
  FF_D00_14X,
  FF_D00_150,
  FF_D00_15X,
  FF_D00_160,
  FF_D00_16X,
  FF_D00_170,
  FF_D00_17X,
  FF_D00_180,
  FF_D00_18X,
  FF_D00_190,
  FF_D00_19X,
  FF_D00_20170,
  FF_D00_2017X,
  FF_D00_20180,
  FF_D00_2018X,
  FF_D00_20190,
  FF_D00_2019X,
  FF_D00_20200,
  FF_D00_2020X,
  FF_D00_2021,
  FF_D00_2022,
  FF_D00_2023,
  FF_D00_2024,
  FF_D00_2025,
  FF_D00_2026,
  FF_D00_LAST,
  /* Dyna subprofiles*/
  FF_DYNA,
  FF_971R4,
  FF_971R5,
  FF_971R6,
  FF_971R7,
  FF_971R8,
  FF_971R9,
  FF_971R93,
  FF_971R101,
  FF_971R11,
  FF_971R111,
  FF_971R112,
  FF_971R12,
  FF_971R13,
  FF_971R131,
  FF_971R14,
  FF_971R141,
  FF_971R15,
  FF_971R16,
  FF_CDRI = 1000,
  /** Last (for iterating) */
  FF_LAST = 2000
}; 

#ifdef __cplusplus

/// Enum for file formats
typedef enum MvFileFormat_s MvFileFormat_e;
/// Getting format from keyword
HC_DATA_DLL_API MvFileFormat_e  MV_get_file_format(const string &keyword);
/// Getting keyword from format
HC_DATA_DLL_API const string  &MV_get_file_format(MvFileFormat_e format);

void HCDI_SetFileFormat(string subprofile);
/// Returns true for a RADIOSS file format
bool MV_is_radioss_file_format(MvFileFormat_e format);
/// Returns true for a LSDYNA file format
bool MV_is_lsdyna_file_format(MvFileFormat_e format);
/// Returns true for a RADIOSS block file format
bool MV_is_radioss_block_file_format(MvFileFormat_e format);
/// Returns the file format for the given RADIOSS version
HC_DATA_DLL_API MvFileFormat_e MV_get_radioss_file_format(int version,int ifix);  
/// Returns the file format for the given RADIOSS version
MvFileFormat_e MV_get_radioss_file_format(const string &version); 
/// Returns the RADIOSS version of a file format


#endif /* __cplusplus */

/*@}*/


#endif /* MV_FILE_FORMAT_H */
