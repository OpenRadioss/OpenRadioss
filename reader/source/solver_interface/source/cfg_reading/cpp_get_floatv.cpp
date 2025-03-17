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

CDECL void cpp_get_floatv_only_(char *attrib_key, int *s_attrib_key,double *value_float, bool *AVAILABLE, 
                     int *lengthDim, int *massDim, int *timeDim)
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
    bool isOk=false;
// Get Value & dim
    GlobalEntitySDIGetValueDoubleNoDim(cname, value_float, AVAILABLE);
    if(*AVAILABLE == false) 
    {
        *value_float = 0.f;

    }
    free(cname);
//    printf("value %s available = %f \n",attrib_key,*value_float);
}

CDECL void CPP_GET_FLOATV_ONLY(char *attrib_key, int *s_attrib_key,double *value_float, bool *AVAILABLE, 
                                int *lengthDim, int *massDim, int *timeDim)
{cpp_get_floatv_only_ (attrib_key,s_attrib_key,value_float,AVAILABLE,lengthDim,massDim,timeDim);}

CDECL void cpp_get_floatv_only__(char *attrib_key, int *s_attrib_key,double *value_float, bool *AVAILABLE, 
                                int *lengthDim, int *massDim, int *timeDim)
{cpp_get_floatv_only_ (attrib_key,s_attrib_key,value_float,AVAILABLE,lengthDim,massDim,timeDim);}

CDECL void cpp_get_floatv_only(char *attrib_key, int *s_attrib_key,double *value_float, bool *AVAILABLE, 
                                int *lengthDim, int *massDim, int *timeDim)
{cpp_get_floatv_only_ (attrib_key,s_attrib_key,value_float,AVAILABLE,lengthDim,massDim,timeDim);}




CDECL void cpp_get_floatv_index_(char *attrib_key, int *s_attrib_key, double *value_float,int *index, bool *AVAILABLE, 
                     int *lengthDim, int *massDim, int *timeDim, int *uid, int *sub_id)
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
    bool isOk=false;
    unsigned int index1 = *index - 1;
    unsigned int index2 = 0;
// Get Submodel Id
    GlobalEntitySDIGetSubmodelId(sub_id, &isOk);
// Get Unit_id
    isOk=false;
    GlobalEntitySDIGetUnitId(uid, &isOk);
// Get Value & dim
    GlobalEntitySDIGetValueDouble(cname, value_float, AVAILABLE, 
                               lengthDim, massDim, timeDim, index1);
    if(*AVAILABLE == false) 
    {
        *value_float = 0.f;

    }
    free(cname);
//    printf("value %s available = %f \n",attrib_key,*value_float);
}

CDECL void CPP_GET_FLOATV_INDEX(char *attrib_key, int *s_attrib_key,double *value_float,int *index, bool *AVAILABLE, 
                                int *lengthDim, int *massDim, int *timeDim,int *uid,int *sub_id)
{cpp_get_floatv_index_ (attrib_key,s_attrib_key,value_float,index,AVAILABLE,lengthDim,massDim,timeDim,uid,sub_id);}

CDECL void cpp_get_floatv_index__(char *attrib_key, int *s_attrib_key,double *value_float,int *index, bool *AVAILABLE, 
                                int *lengthDim, int *massDim, int *timeDim,int *uid,int *sub_id)
{cpp_get_floatv_index_ (attrib_key,s_attrib_key,value_float,index,AVAILABLE,lengthDim,massDim,timeDim,uid,sub_id);}

CDECL void cpp_get_floatv_index(char *attrib_key, int *s_attrib_key,double *value_float,int *index, bool *AVAILABLE, 
                                int *lengthDim, int *massDim, int *timeDim,int *uid,int *sub_id)
{cpp_get_floatv_index_ (attrib_key,s_attrib_key,value_float,index,AVAILABLE,lengthDim,massDim,timeDim,uid,sub_id);}


CDECL void cpp_get_floatv_2index_(char *attrib_key, int *s_attrib_key, double *value_float,int *index1, bool *AVAILABLE, 
                     int *lengthDim, int *massDim, int *timeDim, int *uid, int *sub_id,int *index2)
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
    bool isOk=false;
    unsigned int index_1 = *index1 - 1;
    unsigned int index_2 = *index2 - 1;
// Get Submodel Id
    GlobalEntitySDIGetSubmodelId(sub_id, &isOk);
// Get Unit_id
    isOk=false;
    GlobalEntitySDIGetUnitId(uid, &isOk);
// Get Value & dim
    GlobalEntitySDIGetValueDouble(cname, value_float, AVAILABLE, 
                               lengthDim, massDim, timeDim, index_1, index_2);
    if(*AVAILABLE == false) 
    {
        *value_float = 0.f;

    }
    free(cname);
//    printf("value %s available = %f \n",attrib_key,*value_float);
}

CDECL void CPP_GET_FLOATV_2INDEX(char *attrib_key, int *s_attrib_key,double *value_float,int *index1, bool *AVAILABLE, 
                                int *lengthDim, int *massDim, int *timeDim,int *uid,int *sub_id,int *index2)
{cpp_get_floatv_2index_ (attrib_key,s_attrib_key,value_float,index1,AVAILABLE,lengthDim,massDim,timeDim,uid,sub_id,index2);}

CDECL void cpp_get_floatv_2index__(char *attrib_key, int *s_attrib_key,double *value_float,int *index1, bool *AVAILABLE, 
                                int *lengthDim, int *massDim, int *timeDim,int *uid,int *sub_id,int *index2)
{cpp_get_floatv_2index_ (attrib_key,s_attrib_key,value_float,index1,AVAILABLE,lengthDim,massDim,timeDim,uid,sub_id,index2);}

CDECL void cpp_get_floatv_2index(char *attrib_key, int *s_attrib_key,double *value_float,int *index1, bool *AVAILABLE, 
                                int *lengthDim, int *massDim, int *timeDim,int *uid,int *sub_id,int *index2)
{cpp_get_floatv_2index_ (attrib_key,s_attrib_key,value_float,index1,AVAILABLE,lengthDim,massDim,timeDim,uid,sub_id,index2);}



CDECL void cpp_get_floatv_floatd_(char *attrib_key, int *s_attrib_key,double *value_float,bool *AVAILABLE, 
                     double *lengthDim, double *massDim, double *timeDim, int *uid, int *sub_id)
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
    unsigned int index = 0;
    sdiString objtype_str;
    bool isOk=false;
// Get Submodel Id
    GlobalEntitySDIGetSubmodelId(sub_id, &isOk);
// Get Unit_id
    isOk=false;
    GlobalEntitySDIGetUnitId(uid, &isOk);
// Get Value & dim
    GlobalEntitySDIGetValueDoubleDimDouble(cname, value_float, AVAILABLE, 
                               lengthDim, massDim, timeDim, index);


//    printf("%s units : lengthDim: %f massDim : %f timeDim : %f \n",cname,*lengthDim,*massDim,*timeDim);
//    fflush(stdout);

    if(*AVAILABLE == false) 
    {
        *value_float = 0.f;

    }
    free(cname);
//    printf("value %s available = %f \n",attrib_key,*value_float);
}

CDECL void CPP_GET_FLOATV_FLOATD(char *attrib_key, int *s_attrib_key,double *value_float, bool *AVAILABLE, 
                                double *lengthDim, double *massDim, double *timeDim,int *uid,int *sub_id)
{cpp_get_floatv_floatd_ (attrib_key,s_attrib_key,value_float,AVAILABLE,lengthDim,massDim,timeDim,uid,sub_id);}

CDECL void cpp_get_floatv_floatd__(char *attrib_key, int *s_attrib_key,double *value_float, bool *AVAILABLE, 
                                double *lengthDim, double *massDim, double *timeDim,int *uid,int *sub_id)
{cpp_get_floatv_floatd_ (attrib_key,s_attrib_key,value_float,AVAILABLE,lengthDim,massDim,timeDim,uid,sub_id);}

CDECL void cpp_get_floatv_floatd(char *attrib_key, int *s_attrib_key,double *value_float, bool *AVAILABLE, 
                                double *lengthDim, double *massDim, double *timeDim,int *uid,int *sub_id)
{cpp_get_floatv_floatd_ (attrib_key,s_attrib_key,value_float,AVAILABLE,lengthDim,massDim,timeDim,uid,sub_id);}




CDECL void cpp_get_floatv_floatd_index_(char *attrib_key, int *s_attrib_key, double *value_float,int *index, bool *AVAILABLE, 
                     double *lengthDim, double *massDim, double *timeDim, int *uid, int *sub_id)
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
    bool isOk=false;
    unsigned int index1 = *index - 1;
// Get Submodel Id
    GlobalEntitySDIGetSubmodelId(sub_id, &isOk);
// Get Unit_id
    isOk=false;
    GlobalEntitySDIGetUnitId(uid, &isOk);
// Get Value & dim
    GlobalEntitySDIGetValueDoubleDimDouble(cname, value_float, AVAILABLE, 
                               lengthDim, massDim, timeDim, index1);

//    printf("%s units : lengthDim: %f massDim : %f timeDim : %f \n",cname,*lengthDim,*massDim,*timeDim);
//    fflush(stdout);

    if(*AVAILABLE == false) 
    {
        *value_float = 0.f;

    }
    free(cname);
//    printf("value %s available = %f \n",attrib_key,*value_float);
}

CDECL void CPP_GET_FLOATV_FLOATD_INDEX(char *attrib_key, int *s_attrib_key,double *value_float,int *index, bool *AVAILABLE, 
                                double *lengthDim, double *massDim, double *timeDim,int *uid,int *sub_id)
{cpp_get_floatv_floatd_index_ (attrib_key,s_attrib_key,value_float,index,AVAILABLE,lengthDim,massDim,timeDim,uid,sub_id);}

CDECL void cpp_get_floatv_floatd_index__(char *attrib_key, int *s_attrib_key,double *value_float,int *index, bool *AVAILABLE, 
                                double *lengthDim, double *massDim, double *timeDim,int *uid,int *sub_id)
{cpp_get_floatv_floatd_index_ (attrib_key,s_attrib_key,value_float,index,AVAILABLE,lengthDim,massDim,timeDim,uid,sub_id);}

CDECL void cpp_get_floatv_floatd_index(char *attrib_key, int *s_attrib_key,double *value_float,int *index, bool *AVAILABLE, 
                                double *lengthDim, double *massDim, double *timeDim,int *uid,int *sub_id)
{cpp_get_floatv_floatd_index_ (attrib_key,s_attrib_key,value_float,index,AVAILABLE,lengthDim,massDim,timeDim,uid,sub_id);}






}
