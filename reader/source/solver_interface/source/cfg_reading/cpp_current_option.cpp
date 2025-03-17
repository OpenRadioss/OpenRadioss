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

CDECL void cpp_current_option_(int *ID, int *UID, int *includeId,char *KEY, int *SKEY, char *TITR, int *STITR)
{
    bool isOk=false;
    char  *v;
    bool found;
    unsigned int index;
    sdiString objtype_str;
// Get Submodel Id
    GlobalEntitySDIGetSubmodelId(includeId, &isOk);
// Get option Id
    GlobalEntitySDIGetId(ID, &isOk);
// Get option Unit Id
    GlobalEntitySDIGetUnitId(UID, &isOk);
// Get option Title
    char buffer[500] = "";
    int size = (int) sizeof(buffer);
    GlobalEntitySDIGetValueString("name", buffer, &size, &isOk);
    strcpy(TITR,buffer);
    *STITR = strlen(buffer);
// Get solverkeyword 
    char buffer1[100] = "";
    int size1 = (int) sizeof(buffer1);
    GlobalEntitySDIGetValueString("solverkeyword", buffer1, &size1, &isOk);
    strcpy(KEY,buffer1);
    *SKEY = strlen(buffer1);
}

CDECL void CPP_CURRENT_OPTION(int *ID, int *UID, int *includeId, char *KEY, int *SKEY, char *TITR, int *STITR)
{cpp_current_option_ (ID,UID,includeId,KEY,SKEY,TITR,STITR);}

CDECL void cpp_current_option__(int *ID, int *UID, int *includeId, char *KEY, int *SKEY, char *TITR, int *STITR)
{cpp_current_option_ (ID,UID,includeId,KEY,SKEY,TITR,STITR);}

CDECL void cpp_current_option(int *ID, int *UID, int *includeId, char *KEY, int *SKEY, char *TITR, int *STITR)
{cpp_current_option_ (ID,UID,includeId,KEY,SKEY,TITR,STITR);}


}
