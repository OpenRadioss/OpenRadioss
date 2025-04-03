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

CDECL void cpp_get_max_id_(char *ENTITY_TYPE, int *S_ENTITY_TYPE,int *INTER_MAXID)
{
    bool isOk = false;
    unsigned int count=0;
    int cpt = 1;
    int id = 0;
    int includeId = 0;
    *INTER_MAXID = 0;

// Char fortran -> c++
    char *cname;
    int cname_len;
    int i;
    cname_len = *S_ENTITY_TYPE + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*S_ENTITY_TYPE;i++)  cname[i] = ENTITY_TYPE[i];
    cname[*S_ENTITY_TYPE]='\0';

    GlobalModelSDISelectionStart(cname);
    GlobalModelSDISelectionCount(&count);


    GlobalEntitySDIGetSubmodelId(&includeId, &isOk);
    while (isOk && cpt <= count)
    {
        GlobalModelSDISelectionNext(&isOk);
        GlobalEntitySDIGetId(&id, &isOk);
        GlobalEntitySDIGetSubmodelId(&includeId, &isOk);

        if( id > *INTER_MAXID ) *INTER_MAXID = id;
        cpt++;
    }
}

CDECL void CPP_GET_MAX_ID(char *ENTITY_TYPE, int *S_ENTITY_TYPE,int *INTER_MAXID)
{cpp_get_max_id_ (ENTITY_TYPE,S_ENTITY_TYPE,INTER_MAXID);}

CDECL void cpp_get_max_id__(char *ENTITY_TYPE, int *S_ENTITY_TYPE,int *INTER_MAXID)
{cpp_get_max_id_ (ENTITY_TYPE,S_ENTITY_TYPE,INTER_MAXID);}

CDECL void cpp_get_max_id(char *ENTITY_TYPE, int *S_ENTITY_TYPE,int *INTER_MAXID)
{cpp_get_max_id_ (ENTITY_TYPE,S_ENTITY_TYPE,INTER_MAXID);}


}
