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

CDECL void cpp_option_read_(int *ID, int *UID, int *includeId,char *KEY, int *SKEY, char *TITR, int *STITR, int *valueType,int * pos)
{
    bool isOk=false;
    if (*pos == 0){                                          // If position = 0 than we are in classical read modus, go through selection list
       GlobalModelSDISelectionNext(&isOk);
    }else{
       GlobalModelPositionSelection(*pos);                  // If position > 0 than we have set a vector of entities.
    }

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
//    printf("KEY : %s \n",KEY);
//    fflush(stdout);
// Get option Type
//    EntityType type;
    GlobalModelSDISelectionType(valueType);
//    if (type ==  ENTITY_TYPE_COMPONENT      ) *valueType = 3;
//    if (type ==  ENTITY_TYPE_MATERIAL       ) *valueType = 4;
//    if (type ==  ENTITY_TYPE_PROPERTY       ) *valueType = 5;
//    if (type ==  ENTITY_TYPE_SOLVERSUBMODEL ) *valueType = 6;
//    if (type ==  ENTITY_TYPE_ASSEMBLY       ) *valueType = 7;
}

CDECL void CPP_OPTION_READ(int *ID, int *UID, int *includeId, char *KEY, int *SKEY, char *TITR, int *STITR, int *valueType,int * pos)
{cpp_option_read_ (ID,UID,includeId,KEY,SKEY,TITR,STITR,valueType, pos);}

CDECL void cpp_option_read__(int *ID, int *UID, int *includeId, char *KEY, int *SKEY, char *TITR, int *STITR, int *valueType,int * pos)
{cpp_option_read_ (ID,UID,includeId,KEY,SKEY,TITR,STITR,valueType, pos);}

CDECL void cpp_option_read(int *ID, int *UID, int *includeId, char *KEY, int *SKEY, char *TITR, int *STITR, int *valueType,int * pos)
{cpp_option_read_ (ID,UID,includeId,KEY,SKEY,TITR,STITR,valueType, pos);}




CDECL void cpp_option_read_list_(int * pos,int *ID, int *UID, int *includeId,char *KEY, int *SKEY, char *TITR, int *STITR, int *valueType)
{
    bool isOk=false;
    int position = *pos;
    GlobalModelPositionSelection( position);

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
    char buffer[100] = "";
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
//    printf("KEY : %s \n",KEY);
//    fflush(stdout);
// Get option Type
//    EntityType type;
    GlobalModelSDISelectionType(valueType);
//    if (type ==  ENTITY_TYPE_COMPONENT      ) *valueType = 3;
//    if (type ==  ENTITY_TYPE_MATERIAL       ) *valueType = 4;
//    if (type ==  ENTITY_TYPE_PROPERTY       ) *valueType = 5;
//    if (type ==  ENTITY_TYPE_SOLVERSUBMODEL ) *valueType = 6;
//    if (type ==  ENTITY_TYPE_ASSEMBLY       ) *valueType = 7;
}



}
