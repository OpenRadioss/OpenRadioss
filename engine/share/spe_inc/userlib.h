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
    Set libname / extention according to Arch name 
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

void  (*mds_userlib_id)(int * info);

/* NEW MDS Starter Routine */
void  (*eng_mds_c)  (  int * NCYCLE, int * MID, int * ELEMENT_UID ,
                       int * NEL   , int * NPT, int * IT ,int * ILAY,int *  IPG   ,int *  IFLAG,
                       my_real *UPARAM ,int * NUPARAM  , my_real *UVAR ,int * NUVAR,
                       int *NFUNC  , int *IFUNC , my_real FUNCTION_ARRAY ,int * NPF ,
                       my_real * TIME, my_real *TIMESTEP , my_real * RHO0, my_real *AREA, my_real *EINT,
                       my_real * THKLY   ,my_real * THK     ,my_real * SHF     ,my_real * ETSE    ,
                       my_real * EPSPXX  ,my_real * EPSPYY  ,my_real * EPSPXY  ,my_real * EPSPYZ  ,my_real * EPSPZX  ,
                       my_real * DEPSXX  ,my_real * DEPSYY  ,my_real * DEPSXY  ,my_real * DEPSYZ  ,my_real * DEPSZX  ,
                       my_real * EPSXX   ,my_real * EPSYY   ,my_real * EPSXY   ,my_real * EPSYZ   ,my_real * EPSZX   ,
                       my_real * SOUNDSP ,my_real * VISCMAX  ,my_real * PLA     ,my_real * OFF     ,
                       my_real * TEMP    ,my_real * R11     ,my_real * R12     ,my_real * R13     ,my_real * R21     ,
                       my_real * R22     ,my_real * R23     ,my_real * R31     ,my_real * R32     ,my_real * R33     ,
                       my_real * SIGY    ,my_real * SIGOXX  ,my_real * SIGOYY  ,my_real * SIGOXY  ,my_real * SIGOYZ  ,
                       my_real * SIGOZX  ,my_real * SIGNXX  ,my_real * SIGNYY  ,my_real * SIGNXY  ,
                       my_real * SIGNYZ  ,my_real * SIGNZX  ,my_real * SIGVXX  ,my_real * SIGVYY  ,my_real * SIGVXY  ,
                       my_real * SIGVYZ  ,my_real * SIGVZX  ,my_real * DPLA     ,
                       my_real * ADDITIONAL_FLT_PARAMETERS,my_real * ADDITIONAL_INT_PARAMETERS   );

void  (*eng_mds_s)  (  int *  NCYCLE ,int * ID,int * IPTR,int * IPTS,int * IPTT,
                       int *  NEL    ,int * ELEMENT_UID,int * NUPARAM  ,int *NUVAR    ,int *NFUNC    ,int *IFUNC  ,
                       int * NPF     ,my_real * FUNCTION_ARRAY,my_real * TIME   ,my_real * TIMESTEP ,my_real * UPARAM   ,my_real * UVAR  ,
                       my_real * RHO0     ,my_real * RHO      ,my_real * VOLUME   ,my_real * EINT     ,my_real * TEMP     ,my_real * AMU      ,
                       my_real * EPSPXX   ,my_real * EPSPYY   ,my_real * EPSPZZ   ,my_real * EPSPXY   ,my_real * EPSPYZ   ,my_real * EPSPZX   ,
                       my_real * DEPSXX   ,my_real * DEPSYY   ,my_real * DEPSZZ   ,my_real * DEPSXY   ,my_real * DEPSYZ   ,my_real * DEPSZX   ,
                       my_real * EPSXX    ,my_real * EPSYY    ,my_real * EPSZZ    ,my_real * EPSXY    ,my_real * EPSYZ    ,my_real * EPSZX    ,
                       my_real * SIGOXX   ,my_real * SIGOYY   ,my_real * SIGOZZ   ,my_real * SIGOXY   ,my_real * SIGOYZ   ,my_real * SIGOZX   ,
                       my_real * SIGNXX   ,my_real * SIGNYY   ,my_real * SIGNZZ   ,my_real * SIGNXY   ,my_real * SIGNYZ   ,my_real * SIGNZX   ,
                       my_real * SIGVXX   ,my_real * SIGVYY   ,my_real * SIGVZZ   ,my_real * SIGVXY   ,my_real * SIGVYZ   ,my_real * SIGVZX   ,
                       my_real * SOUNDSP  ,my_real * VISCMAX  ,my_real * OFF      ,my_real * R11      ,my_real * R12      ,my_real * R13      ,
                       my_real * R21      ,my_real * R22      ,my_real * R23      ,my_real * R31      ,my_real * R32      ,my_real * R33      ,
                           int * ISMSTR   ,my_real * ISMSTR10_ARRAYS    ,my_real * PLA      ,my_real * DPLA     ,my_real * SIGY     ,
                       my_real * ADDITIONAL_FLT_PARAMETERS, int * ADDITIONAL_INT_PARAMETERS ) ;    

void (*mds_eng_user_check) ( int *MY_RANK, double *TSTOP, int *NCYCLE, double *TT, int *MSTOP ) ;

void (*mds_eng_user_finalize) ( int *MY_RANK ) ;

void (*mds_eng_user_initialize) (int *NSPMD,     int *NTHREAD,    int *MY_RANK,double *TSTOP,
                                 int * MDS_NMAT, int * MDS_MATID, char * MDS_FILES, char * MDS_LABEL, int * MDS_NDEPSVAR,int * MDS_MAXDEPVAR,int*MDS_OUTPUT_TABLE) ;

void mds_array_init_(){
	  int i;
       eng_mds_c          = NULL;
       eng_mds_s          = NULL;
       mds_userlibhandle  = NULL;
       mds_userlib_id     = NULL;
       mds_eng_user_check = NULL;
       mds_eng_user_finalize = NULL;
       mds_eng_user_initialize = NULL;
}

void mds_userlib_name_set(char * libname);
void mds_userlib_name_get_(char * libname,int * length);



#ifdef _WIN64
 void mds_init_callback();
 void (*set_mds_callback) (void * callback_array[200]);
#endif


