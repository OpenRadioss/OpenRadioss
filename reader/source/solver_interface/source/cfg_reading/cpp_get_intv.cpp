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

CDECL void cpp_get_intv_(char *attrib_key, int *s_attrib_key,int *value_int,int *sub_id, bool *AVAILABLE, int *valueType)
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
    *AVAILABLE = false;
    unsigned int index1 = UINT_MAX;
    unsigned int index2 = UINT_MAX;
    sdiString objtype_str;
// Get Submodel Id
    bool isOk=false;
    GlobalEntitySDIGetSubmodelId(sub_id, &isOk);
// Get Value & type
    GlobalEntitySDIGetValueInt(cname, value_int, AVAILABLE, index1, &objtype_str, index2);
    if(*AVAILABLE == false) 
    {
        *value_int = 0;
        
    }
    //  search type of integer value
    *valueType = 7;
//    ..................................................................
//    if (strncmp(objtype_str.c_str(),"/SHELL",6) == 0 )*valueType = 1;
//    if (strncmp(objtype_str.c_str(),"/BRICK",6) == 0 )*valueType = 1;
//    if (strncmp(objtype_str.c_str(),"/SPRING",6) == 0 )*valueType = 1;
//    ..................................................................
    if (strncmp(objtype_str.c_str(),"/NODE",5) == 0 )*valueType = 2;
    if (strncmp(objtype_str.c_str(),"/PART",5) == 0 )*valueType = 3;
    if (strncmp(objtype_str.c_str(),"/MAT",4) == 0 )*valueType = 4;
    if (strncmp(objtype_str.c_str(),"/PROP",5) == 0 )*valueType = 5;
    if (strncmp(objtype_str.c_str(),"//SUBMODEL",10) == 0 )*valueType = 6;
    if (strncmp(objtype_str.c_str(),"/SUBSET",7) == 0 )*valueType = 7;
    if (strlen(objtype_str.c_str()) == 0 ) *valueType = 0;
    free(cname);
}

CDECL void CPP_GET_INTV(char *attrib_key, int *s_attrib_key,int *value_int,int *sub_id, bool *AVAILABLE, int *valueType)
{cpp_get_intv_ (attrib_key,s_attrib_key,value_int,sub_id,AVAILABLE,valueType);}

CDECL void cpp_get_intv__(char *attrib_key, int *s_attrib_key,int *value_int,int *sub_id, bool *AVAILABLE, int *valueType)
{cpp_get_intv_ (attrib_key,s_attrib_key,value_int,sub_id,AVAILABLE,valueType);}

CDECL void cpp_get_intv(char *attrib_key, int *s_attrib_key,int *value_int,int *sub_id, bool *AVAILABLE, int *valueType)
{cpp_get_intv_ (attrib_key,s_attrib_key,value_int,sub_id,AVAILABLE,valueType);}


CDECL void cpp_get_intv_index_(char *attrib_key, int *s_attrib_key,int *value_int,int *index1,int *sub_id, bool *AVAILABLE, int *valueType)
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
    *AVAILABLE = false;
    sdiString objtype_str;
    unsigned int index0 = 0;
// Get Submodel Id
    bool isOk=false;
    GlobalEntitySDIGetSubmodelId(sub_id, &isOk);
// Get Value & type
    unsigned int index_1 = *index1 - 1;
    unsigned int index_2 = UINT_MAX;
    GlobalEntitySDIGetValueInt(cname, value_int, AVAILABLE, index_1, &objtype_str,index_2);
    if(*AVAILABLE == false) 
    {
        *value_int = 0;
        
    }
    //  search type of integer value
    *valueType = 7;
//    ..................................................................
//    if (strncmp(objtype_str.c_str(),"/SHELL",6) == 0 )*valueType = 1;
//    if (strncmp(objtype_str.c_str(),"/BRICK",6) == 0 )*valueType = 1;
//    if (strncmp(objtype_str.c_str(),"/SPRING",6) == 0 )*valueType = 1;
//    ..................................................................
    if (strncmp(objtype_str.c_str(),"/NODE",5) == 0 )*valueType = 2;
    if (strncmp(objtype_str.c_str(),"/PART",5) == 0 )*valueType = 3;
    if (strncmp(objtype_str.c_str(),"/MAT",4) == 0 )*valueType = 4;
    if (strncmp(objtype_str.c_str(),"/PROP",5) == 0 )*valueType = 5;
    if (strncmp(objtype_str.c_str(),"//SUBMODEL",10) == 0 )*valueType = 6;
    if (strncmp(objtype_str.c_str(),"/SUBSET",7) == 0 )*valueType = 7;
    if (strlen(objtype_str.c_str()) == 0 ) *valueType = 0;
    free(cname);
}

CDECL void CPP_GET_INTV_INDEX(char *attrib_key, int *s_attrib_key,int *value_int,int *index1,int *sub_id, bool *AVAILABLE, int *valueType)
{cpp_get_intv_index_ (attrib_key,s_attrib_key,value_int,index1,sub_id,AVAILABLE,valueType);}

CDECL void cpp_get_intv_index__(char *attrib_key, int *s_attrib_key,int *value_int,int *index1,int *sub_id, bool *AVAILABLE, int *valueType)
{cpp_get_intv_index_ (attrib_key,s_attrib_key,value_int,index1,sub_id,AVAILABLE,valueType);}

CDECL void cpp_get_intv_index(char *attrib_key, int *s_attrib_key,int *value_int,int *index1,int *sub_id, bool *AVAILABLE, int *valueType)
{cpp_get_intv_index_ (attrib_key,s_attrib_key,value_int,index1,sub_id,AVAILABLE,valueType);}





CDECL void cpp_get_intv_2index_(char *attrib_key, int *s_attrib_key,int *value_int,int *index1,int *sub_id, bool *AVAILABLE, int *valueType,int *index2)
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
    *AVAILABLE = false;
    sdiString objtype_str;
    unsigned int index0 = 0;
// Get Submodel Id
    bool isOk=false;
    GlobalEntitySDIGetSubmodelId(sub_id, &isOk);
// Get Value & type
    unsigned int index_1 = *index1 - 1;
    unsigned int index_2 = *index2 - 1;
    GlobalEntitySDIGetValueInt(cname, value_int, AVAILABLE, index_1, &objtype_str,index_2);
    if(*AVAILABLE == false) 
    {
        *value_int = 0;
        
    }
    //  search type of integer value
    *valueType = 7;
//    ..................................................................
//    if (strncmp(objtype_str.c_str(),"/SHELL",6) == 0 )*valueType = 1;
//    if (strncmp(objtype_str.c_str(),"/BRICK",6) == 0 )*valueType = 1;
//    if (strncmp(objtype_str.c_str(),"/SPRING",6) == 0 )*valueType = 1;
//    ..................................................................
    if (strncmp(objtype_str.c_str(),"/NODE",5) == 0 )*valueType = 2;
    if (strncmp(objtype_str.c_str(),"/PART",5) == 0 )*valueType = 3;
    if (strncmp(objtype_str.c_str(),"/MAT",4) == 0 )*valueType = 4;
    if (strncmp(objtype_str.c_str(),"/PROP",5) == 0 )*valueType = 5;
    if (strncmp(objtype_str.c_str(),"//SUBMODEL",10) == 0 )*valueType = 6;
    if (strncmp(objtype_str.c_str(),"/SUBSET",7) == 0 )*valueType = 7;
    if (strlen(objtype_str.c_str()) == 0 ) *valueType = 0;
    free(cname);
}

CDECL void CPP_GET_INTV_2INDEX(char *attrib_key, int *s_attrib_key,int *value_int,int *index1,int *sub_id, bool *AVAILABLE, int *valueType,int *index2)
{cpp_get_intv_2index_ (attrib_key,s_attrib_key,value_int,index1,sub_id,AVAILABLE,valueType,index2);}

CDECL void cpp_get_intv_2index__(char *attrib_key, int *s_attrib_key,int *value_int,int *index1,int *sub_id, bool *AVAILABLE, int *valueType,int *index2)
{cpp_get_intv_2index_ (attrib_key,s_attrib_key,value_int,index1,sub_id,AVAILABLE,valueType,index2);}

CDECL void cpp_get_intv_2index(char *attrib_key, int *s_attrib_key,int *value_int,int *index1,int *sub_id, bool *AVAILABLE, int *valueType,int *index2)
{cpp_get_intv_2index_ (attrib_key,s_attrib_key,value_int,index1,sub_id,AVAILABLE,valueType,index2);}


}
