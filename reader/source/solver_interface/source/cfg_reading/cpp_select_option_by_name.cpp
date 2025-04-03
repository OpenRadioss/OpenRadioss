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
#include <stdarg.h>
#include <map>
#include <dll_settings.h>


extern "C" 
{

CDECL void cpp_select_option_by_name(char *ENTITY_TYPE, int *S_ENTITY_TYPE, char *attrib_key, int *s_attrib_key, int *subIndex)
{
    int i;
// Char fortran -> c++
    char *cname;
    int cname_len;
    cname_len = *S_ENTITY_TYPE + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*S_ENTITY_TYPE;i++)  cname[i] = ENTITY_TYPE[i];
    cname[*S_ENTITY_TYPE]='\0';
// Char fortran -> c++
    char *cname1;
    int cname_len1;
    cname_len1 = *s_attrib_key + 1;
    cname1=(char*) malloc(sizeof(char)*cname_len1);
    for(i=0;i<*s_attrib_key;i++)  cname1[i] = attrib_key[i];
    cname1[*s_attrib_key]='\0';
//
    GlobalModelSDISelectOptionByName(cname,cname1,subIndex);
    free(cname);
    free(cname1);
}

CDECL void CPP_SELECT_OPTION_BY_NAME(char *ENTITY_TYPE, int *S_ENTITY_TYPE, char *attrib_key, int *s_attrib_key, int *subIndex)
{cpp_select_option_by_name (ENTITY_TYPE,S_ENTITY_TYPE,attrib_key,s_attrib_key,subIndex);}

CDECL void cpp_select_option_by_name_(char *ENTITY_TYPE, int *S_ENTITY_TYPE, char *attrib_key, int *s_attrib_key, int *subIndex)
{cpp_select_option_by_name (ENTITY_TYPE,S_ENTITY_TYPE,attrib_key,s_attrib_key,subIndex);}

CDECL void cpp_select_option_by_name__(char *ENTITY_TYPE, int *S_ENTITY_TYPE, char *attrib_key, int *s_attrib_key, int *subIndex)
{cpp_select_option_by_name (ENTITY_TYPE,S_ENTITY_TYPE,attrib_key,s_attrib_key,subIndex);}


}
