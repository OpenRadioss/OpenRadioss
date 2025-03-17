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

CDECL void cpp_convert_th_2d_element_seatbelt_(int *TH_MAXID,int *OFFSET,int *SEATBELT_CONVERTED_ELEMENTS,int *NB_SEATBELT_SHELLS)
{
// Convert entity
    GlobalEntitySDIConvertTh2dElementSeatbelt(TH_MAXID,OFFSET,SEATBELT_CONVERTED_ELEMENTS,NB_SEATBELT_SHELLS);
}

CDECL void CPP_CONVERT_TH_2D_ELEMENT_SEATBELT(int *TH_MAXID,int *OFFSET,int *SEATBELT_CONVERTED_ELEMENTS,int *NB_SEATBELT_SHELLS)
{cpp_convert_th_2d_element_seatbelt_ (TH_MAXID,OFFSET,SEATBELT_CONVERTED_ELEMENTS,NB_SEATBELT_SHELLS);}

CDECL void cpp_convert_th_2d_element_seatbelt__(int *TH_MAXID,int *OFFSET,int *SEATBELT_CONVERTED_ELEMENTS,int *NB_SEATBELT_SHELLS)
{cpp_convert_th_2d_element_seatbelt_ (TH_MAXID,OFFSET,SEATBELT_CONVERTED_ELEMENTS,NB_SEATBELT_SHELLS);}

CDECL void cpp_convert_th_2d_element_seatbelt(int *TH_MAXID,int *OFFSET,int *SEATBELT_CONVERTED_ELEMENTS,int *NB_SEATBELT_SHELLS)
{cpp_convert_th_2d_element_seatbelt_ (TH_MAXID,OFFSET,SEATBELT_CONVERTED_ELEMENTS,NB_SEATBELT_SHELLS);}


}
