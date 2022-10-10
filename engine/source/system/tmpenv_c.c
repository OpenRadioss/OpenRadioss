//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2022 Altair Engineering Inc.
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
#include "hardware.inc"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

#define _FCALL 


#ifdef _WIN64

#include <Windows.h>
#include <direct.h>

#define my_getpid (int)GetCurrentProcessId()

#else
    
#include <sys/types.h>
#include <unistd.h>

#define my_getpid getpid()

#endif


#ifdef _WIN64


char* tmpenv_c(){
//  ---------------------------------------------------------------------------------------
//  tmpenv_c : give TMPDIR environment variable. If not set,give current working directory
//                 WINDOWS CODE
//  ---------------------------------------------------------------------------------------
//  INPUT
//  OUTPUT
//     char *       : returns tmpdir
//   ----------------------------------------------------------------------------
  char * tmpdir;
  int sz_tmpdir;
  
  tmpdir=(char *)malloc(sizeof(char)*2048);
  GetEnvironmentVariable("TMPDIR",tmpdir,2048);

  /* second trial get current working directory */
  if (sz_tmpdir == 0){
      
    sz_tmpdir = GetCurrentDirectory( 2048,tmpdir);
  }
  return tmpdir;

}

#elif 1
//  ---------------------------------------------------------------------------------------
//  tmpenv_c : give TMPDIR environment variable. If not set,give current working directory
//                 LINUX CODE
//  ---------------------------------------------------------------------------------------
//  INPUT
//  OUTPUT
//     char *       : returns tmpdir
//   ----------------------------------------------------------------------------
char* tmpenv_c(){

  char * tmpdir;
  tmpdir =  getenv("TMPDIR");
  
  /* second trial get current working directory */  
  if (tmpdir==NULL){
    tmpdir = (char *)calloc(200,sizeof(char));
    getcwd(tmpdir,200);
  }

  return tmpdir;
}

#endif



//  --------------------------------------------------------------------------------------------------
//  tmpenv_c : give TMPDIR environment variable to Fortran. If not set,give current working directory
//                 LINUX CODE
//  --------------------------------------------------------------------------------------------------
//  INPUT
//  OUTPUT
//     char *       : returns tmpdir
//   ----------------------------------------------------------------------------
void tmpenvf_(char* tmpdir,int *tmplen){

  char * tdir= tmpenv_c();
  int slen=strlen(tdir);
  int i;
  
  for (i=0;i<slen;i++) tmpdir[i]=tdir[i];
  tmpdir[slen]='\0';
  *tmplen = slen;
}

void tmpenvf(char* tmpdir,int *tmplen){
  tmpenvf_(tmpdir,tmplen);
}

void  tmpenvf__(char* tmpdir,int *tmplen){
  tmpenvf_(tmpdir,tmplen);
}

void _FCALL TMPENVF (char* tmpdir,int* tmplen){
  tmpenvf_(tmpdir,tmplen);
}


//  ---------------------------------------------------------------------------------
//  fgetpid : send processID back to Fortran
//                 WINDOWS CODE
//  ---------------------------------------------------------------------------------
//  INPUT
//  OUTPUT
//     int *pid       : returns process id of Radioss
//   ----------------------------------------------------------------------------
void fgetpid_(int * pid){
  *pid=my_getpid;
}

void fgetpid__(int * pid){
  *pid=my_getpid;
}

void fgetpid(int * pid){
  *pid=my_getpid;
}

void _FCALL FGETPID(int * pid){
  *pid=my_getpid;
}


#ifdef MAIN
int main()
{
  char * tmpenv =  tmpenv_c();
  printf("output: %s\n",tmpenv);

  return 1;
}

#endif