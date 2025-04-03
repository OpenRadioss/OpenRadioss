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

extern "C" 
{

CDECL void cpp_group_is_used_(char *attrib_key, int *s_attrib_key, int *id, bool *isUsed)
{
// Char fortran -> c++
    char *cname;
    int cname_len;
    int i;
    cname_len = *s_attrib_key + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*s_attrib_key;i++)  cname[i] = attrib_key[i];
    cname[*s_attrib_key]='\0'; 
//
    *isUsed = false;
    GlobalModelSDIIsGroupUsed(cname,id,isUsed);
}

CDECL void CPP_GROUP_IS_USED(char *attrib_key, int *s_attrib_key, int *id, bool *isUsed)
{cpp_group_is_used_ (attrib_key,s_attrib_key,id,isUsed);}

CDECL void cpp_group_is_used__(char *attrib_key, int *s_attrib_key, int *id, bool *isUsed)
{cpp_group_is_used_ (attrib_key,s_attrib_key,id,isUsed);}

CDECL void cpp_group_is_used(char *attrib_key, int *s_attrib_key, int *id, bool *isUsed)
{cpp_group_is_used_ (attrib_key,s_attrib_key,id,isUsed);}


}
