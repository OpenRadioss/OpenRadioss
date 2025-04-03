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
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <dll_settings.h>

#define _FCALL


#include <chglt_id.h>

extern "C" {


// New library ID : Date_Git_Short_Sha1

CDECL void hm_build_id(char *build_id,int *build_id_size, int *size){
  // char* build_id:     output string recieving the Build_ID
  // int* build_id_size: input size of build_id_size
  // int *size : output size of copied string

  char * commit_id=BUILD_ID;
  int commit_id_size=strlen(commit_id);
  
  if ( *build_id_size < commit_id_size){
     *size = -1;
  }else{
     #ifdef _WIN64
       strcpy_s(build_id,*build_id_size,commit_id);
     #else
       strcpy(build_id,commit_id);  
     #endif
     *size=commit_id_size;
  }
}

CDECL void hm_build_id_(char *build_id,int *build_id_size, int *size){
  // char* build_id:     output string recieving the Build_ID
  // int* build_id_size: input size of build_id_size
  // int *size : output size of copied string

  char * commit_id=BUILD_ID;
  int commit_id_size=strlen(commit_id);
  
  if ( *build_id_size < commit_id_size){
     *size = -1;
  }else{
     #ifdef _WIN64
       strcpy_s(build_id,*build_id_size,commit_id);
     #else
       strcpy(build_id,commit_id);  
     #endif
     *size=commit_id_size;
  }
}

CDECL void hm_build_id__(char *build_id,int *build_id_size, int *size){
  // char* build_id:     output string recieving the Build_ID
  // int* build_id_size: input size of build_id_size
  // int *size : output size of copied string

  char * commit_id=BUILD_ID;
  int commit_id_size=strlen(commit_id);
  
  if ( *build_id_size < commit_id_size){
     *size = -1;
  }else{
     #ifdef _WIN64
       strcpy_s(build_id,*build_id_size,commit_id);
     #else
       strcpy(build_id,commit_id);  
     #endif
     *size=commit_id_size;
  }
}

CDECL void _FCALL HM_BUILD_ID (char *build_id,int *build_id_size, int *size){
  // char* build_id:     output string recieving the Build_ID
  // int* build_id_size: input size of build_id_size
  // int *size : output size of copied string

  char * commit_id=BUILD_ID;
  int commit_id_size=strlen(commit_id);
  
  if ( *build_id_size < commit_id_size){
     *size = -1;
  }else{
     #ifdef _WIN64
       strcpy_s(build_id,*build_id_size,commit_id);
     #else
       strcpy(build_id,commit_id);  
     #endif
     *size=commit_id_size;
  }
}

}

