//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2024 Altair Engineering Inc.
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <fcntl.h>

#ifdef _WIN64
#include <windows.h>
#include <process.h>
#include <io.h>
#include <sys\types.h>
#include <sys/stat.h>
#elif 1
#include <sys/resource.h>
#include <sys/types.h>
#include <time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dlfcn.h>
#endif

#define _FCALL 

void radioss_set_env_variable(char *variable,char * value, int *len_value)
{
 int result;
 char *path_name ;
  path_name = (char *) calloc(*len_value+1, sizeof (char));

#ifdef _WIN64
  char env_string[10192];
  size_t len=10192;
  
  env_string[0]='\0';
  strcat_s(env_string,len,variable);
  strcat_s(env_string,len,"=");
  strcat_s(env_string,len,value);
  _putenv(env_string);
  
#elif 1
  strncpy(path_name,value, *len_value);     
  result=setenv(variable,path_name,1);
#endif
}

void _FCALL RADIOSS_SET_ENV_VARIABLE(char *value , int *len_value)
{	
    char *variable="TMPDIR";
    radioss_set_env_variable(variable,value,len_value);
}

void radioss_set_env_variable_(char *value, int *len_value)
{	
    char *variable="TMPDIR";
    radioss_set_env_variable(variable,value,len_value);
}

void radioss_set_env_variable__(char *value, int *len_value)
{	
    char *variable="TMPDIR";
    radioss_set_env_variable(variable,value,len_value);
}


//void rad_test_env_(){
//  char *tmpdir="/work/roguic/sortie_tmp";
//  char *variable="TMPDIR";
//  radioss_set_env_variable(variable,tmpdir);
//}



