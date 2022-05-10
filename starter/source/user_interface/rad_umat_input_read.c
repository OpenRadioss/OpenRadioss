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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define _FCALL

FILE * userlib_input;
char * cfilename;

void rad_umat_open_(char * filename,int * length,int * result);
void rad_umat_input_read_(char * line,int * len,int *size_read );
void ad_umat_input_rewind_();
void rad_umat_close_input_();



#ifdef _WIN64
#define  rad_umat_input_open_ RAD_UMAT_INPUT_OPEN
#endif
void _FCALL rad_umat_input_open_(char * filename,int * length,int * result){
  int i;

  *result = 0;
  cfilename=malloc(sizeof(char)*(*length+1));
  for (i=0;i< *length;i++) { cfilename[i] = filename[i] ; } 
  cfilename[*length]='\0';

  userlib_input=fopen(cfilename,"r");

  if (userlib_input==NULL){
    *result=0;
  }else{
    *result=1;
  }

  return;
}


#ifdef _WIN64
#define rad_umat_input_read_ RAD_UMAT_INPUT_READ
#endif
void rad_umat_input_read_(char * line,int * len,int *size_read)
{
 int i;
 *size_read=0;
 for (i=0;i<*len;i++) line[i] = ' ' ;
 fgets(line,*len, userlib_input);
 line[strlen(line)-1]='\0';
 *size_read=strlen(line);
}

#ifdef _WIN64
#define rad_umat_input_rewind_ RAD_UMAT_INPUT_REWIND
#endif

void rad_umat_input_rewind_(){
  rewind (userlib_input);
}

#ifdef _WIN64
#define rad_umat_close_input_ RAD_UMAT_CLOSE_INPUT
#endif

void rad_umat_close_input_(){
 fclose( userlib_input);
  userlib_input=NULL;
  remove (cfilename);
}



