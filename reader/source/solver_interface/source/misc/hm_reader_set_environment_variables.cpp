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

#include <radiossblk.h>
#include <dll_settings.h>


extern "C" CDECL  const char *RadiossblkGetVersion(unsigned int *pmajorVersion  = 0,
                                                   unsigned int *pminorVersion  = 0,
                                                   unsigned int *photfixVersion = 0,
                                                   unsigned int *pbuildNumber   = 0);




#define _FCALL 
void seek_env_variable(char ** variable, char * varname);
void set_env_variable(char *variable,char *value);
int int_hm_reader_variables(int *codvers);
extern "C" {

     CDECL void hm_reader_variables(int * result,int *codvers)
     {   
      *result = int_hm_reader_variables(codvers);
     }

     CDECL void hm_reader_variables_(int * result,int *codvers)
     {
      *result = int_hm_reader_variables(codvers);
     }

     CDECL void hm_reader_variables__(int * result,int *codvers)
     {
      *result = int_hm_reader_variables(codvers);
     }

     CDECL void _FCALL HM_READER_VARIABLES(int * result,int *codvers)
     {
      *result = int_hm_reader_variables(codvers);
     }
}


int int_hm_reader_variables(int *codvers)
{
  char * HM_MV_CFG_DIR;
  char * HM_MV_UNITS_DIR;
  char * HM_MSG_DIR;
  char * RAD_CFG_PATH;
  char CFG_VERSION[10];
  char file_to_load[10192];
  int error;
  FILE * hm_data_hierarchy;
  unsigned int pmajorVersion;
  unsigned int pminorVersion;
  unsigned int photfixVersion;
  unsigned int pbuildNumber;

#ifdef _WIN64
  char *CFG_END="\\config\\CFG";
  char *UNITS_END="\\config\\CFG\\UNITS";
  char *MSG_END="\\messages";
#elif 1
  char *CFG_END="/config/CFG";
  char *UNITS_END="/config/CFG/UNITS";
  char *MSG_END="/messages";
#endif

pmajorVersion = *codvers;
RadiossblkSetUserProfileVersion(pmajorVersion);


//(*RadiossblkGetVersion)(&pmajorVersion,
//                        &pminorVersion,
//                        &photfixVersion,
//                        &pbuildNumber);
   sprintf(CFG_VERSION,"%i",pmajorVersion);

/*   printf("%i %i %i %i\n",pmajorVersion,pminorVersion,photfixVersion,pbuildNumber);  */

  HM_MV_CFG_DIR=NULL;
  HM_MV_UNITS_DIR=NULL;
  HM_MSG_DIR=NULL;
  RAD_CFG_PATH=NULL;
  error = 0;

  seek_env_variable(&RAD_CFG_PATH,"RAD_CFG_PATH");

  if (RAD_CFG_PATH != NULL){
    HM_MV_CFG_DIR   = (char *) malloc(10192);
    HM_MV_UNITS_DIR = (char *) malloc(10192);
    HM_MSG_DIR      = (char *) malloc(10192);

    HM_MV_CFG_DIR[0]='\0';
//    strcat(HM_MV_CFG_DIR,"HM_MV_CFG_DIR=");
    strcat(HM_MV_CFG_DIR,RAD_CFG_PATH);
    strcat(HM_MV_CFG_DIR,CFG_END);
    
    HM_MV_UNITS_DIR[0]='\0';
//    strcat(HM_MV_UNITS_DIR,"HM_MV_UNITS_DIR=");
    strcat(HM_MV_UNITS_DIR,RAD_CFG_PATH);
    strcat(HM_MV_UNITS_DIR,UNITS_END);

    HM_MSG_DIR[0]='\0';
//    strcat(HM_MSG_DIR,"HM_MSG_DIR=");
    strcat(HM_MSG_DIR,RAD_CFG_PATH);
    strcat(HM_MSG_DIR,MSG_END);

    set_env_variable("HM_MV_CFG_DIR",HM_MV_CFG_DIR);
    set_env_variable("HM_MV_UNITS_DIR",HM_MV_UNITS_DIR);
    set_env_variable("HM_MSG_DIR",HM_MSG_DIR);

    free(HM_MV_CFG_DIR);
    free(HM_MV_UNITS_DIR);
    free(HM_MSG_DIR);
  }

  HM_MV_CFG_DIR=NULL;
  HM_MV_UNITS_DIR=NULL;
  HM_MSG_DIR=NULL;

  seek_env_variable(&HM_MV_CFG_DIR,"HM_MV_CFG_DIR");
  seek_env_variable(&HM_MV_UNITS_DIR,"HM_MV_UNITS_DIR");
  seek_env_variable(&HM_MSG_DIR,"HM_MSG_DIR");


  if ( HM_MV_CFG_DIR != NULL && HM_MV_UNITS_DIR != NULL && HM_MSG_DIR ){

    
    file_to_load[0]='\0';
    strcat(file_to_load,HM_MV_CFG_DIR);
    strcat(file_to_load,"/radioss");
    strcat(file_to_load,CFG_VERSION);
    strcat(file_to_load,"/data_hierarchy.cfg");

    hm_data_hierarchy=fopen(file_to_load,"r");

    if (hm_data_hierarchy != NULL){

        fclose(hm_data_hierarchy);

    }else{
       error++;  
    }

    
    file_to_load[0]='\0';
    strcat(file_to_load,HM_MV_UNITS_DIR);
    strcat(file_to_load,"/units.cfg");

    hm_data_hierarchy=fopen(file_to_load,"r");

    if (hm_data_hierarchy != NULL){

        fclose(hm_data_hierarchy);

    }else{
       error++;  
    }

    
    file_to_load[0]='\0';
    strcat(file_to_load,HM_MSG_DIR);
    strcat(file_to_load,"/CONFIG/msg_hw_radioss_reader.txt");

    hm_data_hierarchy=fopen(file_to_load,"r");

    if (hm_data_hierarchy != NULL){

        fclose(hm_data_hierarchy);

    }else{
       error++;  
    }

  }else{
      error++;
  }
  return error;
}



/* ------------------------------------------------------------------------------------------------
    seek_env_variable : to read environement variable. Embeds Linux & Windows system calls
   ------------------------------------------------------------------------------------------------
    char ** variable : output - Read variable value
    char *  varname  : input  - Variable name
   ------------------------------------------------------------------------------------------------ */
void seek_env_variable(char ** variable, char * varname)
{
 char * value;
 int value_len;

 value = NULL ; 
 *variable = NULL;
 value_len=0;

  value = getenv(varname);

  if (value != NULL){ value_len=strlen(value)+1;}

  if (value_len > 0){
    value_len=strlen(value)+1;
    *variable = (char * )malloc(value_len);
    strcpy( *variable, value);
  }
}

/* ------------------------------------------------------------------------------------------------
    set_env_variable : to set environement variable. Embeds Linux & Windows system calls
   ------------------------------------------------------------------------------------------------
    char * variable : input  - Variable name
    char * value    : input  - Value
   ------------------------------------------------------------------------------------------------ */
void set_env_variable(char *variable,char * value)
{
 int result;
  /* printf("%s\n",variable); */
#ifdef _WIN64
  char env_string[10192];
  env_string[0]='\0';
  strcat(env_string,variable);
  strcat(env_string,"=");
  strcat(env_string,value);

  _putenv(env_string);
#elif 1
  result=setenv(variable,value,1);
#endif

}

