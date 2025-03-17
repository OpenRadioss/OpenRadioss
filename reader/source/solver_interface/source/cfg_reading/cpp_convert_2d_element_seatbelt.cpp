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

#include "GlobalModelSDI.h"

#include <stdio.h>
#include <string.h>
#include <dll_settings.h>

using namespace std;

extern "C" 
{

CDECL void cpp_convert_2d_element_seatbelt_(int *PART_MAT119,int *PART_MAXID,int *PROP_MAXID,int *MAT_MAXID,int *ELEM_MAXID,int *OFFSET)
{
// Convert entity
    GlobalEntitySDIConvert2dElementSeatbelt(PART_MAT119,PART_MAXID,PROP_MAXID,MAT_MAXID,ELEM_MAXID,OFFSET);
}

CDECL void CPP_CONVERT_2D_ELEMENT_SEATBELT(int *PART_MAT119,int *PART_MAXID,int *PROP_MAXID,int *MAT_MAXID,int *ELEM_MAXID,int *OFFSET)
{cpp_convert_2d_element_seatbelt_ (PART_MAT119,PART_MAXID,PROP_MAXID,MAT_MAXID,ELEM_MAXID,OFFSET);}

CDECL void cpp_convert_2d_element_seatbelt__(int *PART_MAT119,int *PART_MAXID,int *PROP_MAXID,int *MAT_MAXID,int *ELEM_MAXID,int *OFFSET)
{cpp_convert_2d_element_seatbelt_ (PART_MAT119,PART_MAXID,PROP_MAXID,MAT_MAXID,ELEM_MAXID,OFFSET);}

CDECL void cpp_convert_2d_element_seatbelt(int *PART_MAT119,int *PART_MAXID,int *PROP_MAXID,int *MAT_MAXID,int *ELEM_MAXID,int *OFFSET)
{cpp_convert_2d_element_seatbelt_ (PART_MAT119,PART_MAXID,PROP_MAXID,MAT_MAXID,ELEM_MAXID,OFFSET);}





CDECL void cpp_convert_2d_elements_seatbelt_(int *PART_MAT119,int *PART_MAXID,int *PROP_MAXID,int *MAT_MAXID,int *ELEM_MAXID,int *OFFSET,
                                      int *SEATBELT_CONVERTED_ELEMENTS,int *ELEM_INDEX)
{
// Convert entity
    GlobalEntitySDIConvert2dElementsSeatbelt(PART_MAT119,PART_MAXID,PROP_MAXID,MAT_MAXID,ELEM_MAXID,OFFSET,SEATBELT_CONVERTED_ELEMENTS,ELEM_INDEX);
}

CDECL void CPP_CONVERT_2D_ELEMENTS_SEATBELT(int *PART_MAT119,int *PART_MAXID,int *PROP_MAXID,int *MAT_MAXID,int *ELEM_MAXID,int *OFFSET,
                                     int *SEATBELT_CONVERTED_ELEMENTS,int *ELEM_INDEX)
{cpp_convert_2d_elements_seatbelt_ (PART_MAT119,PART_MAXID,PROP_MAXID,MAT_MAXID,ELEM_MAXID,OFFSET,SEATBELT_CONVERTED_ELEMENTS,ELEM_INDEX);}

CDECL void cpp_convert_2d_elements_seatbelt__(int *PART_MAT119,int *PART_MAXID,int *PROP_MAXID,int *MAT_MAXID,int *ELEM_MAXID,int *OFFSET,
                                       int *SEATBELT_CONVERTED_ELEMENTS,int *ELEM_INDEX)
{cpp_convert_2d_elements_seatbelt_ (PART_MAT119,PART_MAXID,PROP_MAXID,MAT_MAXID,ELEM_MAXID,OFFSET,SEATBELT_CONVERTED_ELEMENTS,ELEM_INDEX);}

CDECL void cpp_convert_2d_elements_seatbelt(int *PART_MAT119,int *PART_MAXID,int *PROP_MAXID,int *MAT_MAXID,int *ELEM_MAXID,int *OFFSET,
                                     int *SEATBELT_CONVERTED_ELEMENTS,int *ELEM_INDEX)
{cpp_convert_2d_elements_seatbelt_ (PART_MAT119,PART_MAXID,PROP_MAXID,MAT_MAXID,ELEM_MAXID,OFFSET,SEATBELT_CONVERTED_ELEMENTS,ELEM_INDEX);}




}
