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
/* ---------------
    Global Headers 
   --------------- */

#include <hardware.inc>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <fcntl.h>

#define _FCALL 


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


/* ----------------------------------------------------------
    include containing the routine pointers from userlibrary 
   ---------------------------------------------------------- */

#ifdef MYREAL4
#define my_real float
#endif

#ifdef MYREAL8
#define my_real double
#endif


/*
   ------------------------------------------------ 
    Set libname / extension according to Arch name 
   ------------------------------------------------ 
*/


/* ------------ */
/* WIN64 Macros */
/* ------------ */
#ifdef _WIN64

#define ULIB_ARCH "_win64"
#define ULIB_EXT ".dll"



HINSTANCE mds_userlibhandler;

#endif
 

/* ------------ */
/* Linux Macros */
/* ------------ */

#if CPP_mach==CPP_p4linux964

   #define ULIB_EXT ".so"

   #if CPP_rel==70
     #define ULIB_ARCH "_linuxa64"
   #elif 1
     #define ULIB_ARCH "_linux64"
   #endif

#endif



#define ULIB_SP "_sp"

char * mds_userlib_name;

/* 
   -------------------------------
    Current user library routines
   -------------------------------
*/


void *mds_userlibhandle;

//void (*st_get_userbuf_variable)(int * id, char * title); 

void  (*mds_userlib_id)(int * info);

/* NEW MDS Starter Routine */
void  (*st_mds_lecm)  (my_real * uparam , int * maxuparam, int * nuparam,
                       int * nuvar, int * ifunc,int * maxfunc,int * nfunc,
                       my_real * stifint,my_real * young,  my_real * nu,  my_real * rho0,
                       char * files, char * label, int * ndepvar  );

void mds_array_init_(){
	  int i;
           st_mds_lecm    = NULL;
	   mds_userlibhandle = NULL;
	   mds_userlib_id     = NULL;
}

#ifdef _WIN64
#define mds_userlib_name_get_ MDS_USERLIB_NAME_GET 
#endif
void _FCALL  mds_userlib_name_get_(char * libname,int * length);

void mds_userlib_name_set(char * libname);


#ifdef _WIN64
 void mds_init_callback();
 void (*set_mds_callback) (void * callback_array[200]);
#endif


