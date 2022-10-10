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
#define _FCALL 

#elif 1

#include <dlfcn.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <time.h>
#include <sys/stat.h>
#include <unistd.h>
#define _FCALL

#endif

#ifdef MYREAL4
#define my_real float
#endif

#ifdef MYREAL8
#define my_real double
#endif


void *userlibhandle;

void  (*userlib_id)(int * info);

/* ------------ */
/* User Springs */
/* ------------ */
void  (*eng_ruser)(int *ITYP, int *NEL     ,int *IPROP       ,my_real *UVAR   ,int *NUVAR  ,
             my_real *FX      ,my_real *FY      ,my_real *FZ     ,my_real *XMOM   ,my_real *YMOM   ,
             my_real *ZMOM    ,my_real *E        ,my_real *OFF    ,my_real *STIFM  ,my_real *STIFR  ,
             my_real *VISCM   ,my_real *VISCR   ,my_real *MASS   ,my_real *XINER  ,my_real *DT     ,
             my_real *XL      ,my_real *VX      ,my_real *RY1    ,my_real *RZ1    ,my_real *RX     ,
             my_real *RY2     ,my_real *RZ2     ,my_real *FR_WAVE);

/* ------------------------- */
/* User Law 29-30-31 Solids  */
/* ------------------------- */
void  (*eng_sigeps)(int* ilaw ,
      int *NEL ,int* NUPARAM ,int* NUVAR ,int* NFUNC ,int* IFUNC ,
      int* NPF ,my_real *TF  ,my_real *TIME , my_real *TIMESTEP, my_real *UPARAM ,my_real *RHO0 ,
      my_real *RHO ,my_real *VOLUME ,my_real *EINT ,my_real *EPSPXX ,my_real *EPSPYY ,my_real *EPSPZZ  ,
      my_real *EPSPXY  ,my_real *EPSPYZ  ,my_real *EPSPZX ,my_real *DEPSXX ,my_real *DEPSYY ,my_real *DEPSZZ  ,
      my_real *DEPSXY  ,my_real *DEPSYZ  ,my_real *DEPSZX ,my_real *EPSXX  ,my_real *EPSYY  ,my_real *EPSZZ   ,
      my_real *EPSXY   ,my_real *EPSYZ   ,my_real *EPSZX  ,
      my_real *IGOXX  ,my_real *SIGOYY ,my_real *SIGOZZ  ,my_real *SIGOXY  ,my_real *SIGOYZ  ,my_real *SIGOZX ,
      my_real *SIGNXX ,my_real *SIGNYY ,my_real *SIGNZZ  ,my_real *SIGNXY  ,my_real *SIGNYZ  ,my_real *SIGNZX ,
      my_real *SIGVXX ,my_real *SIGVYY ,my_real *SIGVZZ  ,my_real *SIGVXY  ,my_real *SIGVYZ  ,my_real *SIGVZX ,
      my_real *SOUNDSP,my_real *VISCMAX,my_real *UVAR    ,my_real *OFF     );

/* ------------------------- */
/* User Law 29-30-31 Shells  */
/* ------------------------- */
void  (*eng_sigepsc)(int* ilaw ,
      int* NEL ,int*NUPARAM ,int*NUVAR ,int*NFUNC ,int*IFUNC ,
      int*NPF  ,int*NPT     ,int*IPT   ,int*IFLAG ,
      my_real *TF ,my_real *TIME ,my_real *TIMESTEP , my_real *UPARAM , my_real *RHO0   ,
      my_real *AREA   ,my_real *EINT   ,my_real *THKLY   ,
      my_real *EPSPXX ,my_real *EPSPYY ,my_real *EPSPXY  ,my_real *EPSPYZ  ,my_real *EPSPZX ,
      my_real *DEPSXX ,my_real *DEPSYY ,my_real *DEPSXY  ,my_real *DEPSYZ  ,my_real *DEPSZX ,
      my_real *EPSXX  ,my_real *EPSYY  ,my_real *EPSXY   ,my_real *EPSYZ   ,my_real *EPSZX  ,
      my_real *SIGOXX ,my_real *SIGOYY ,my_real *SIGOXY  ,my_real *SIGOYZ  ,my_real *SIGOZX ,
      my_real *SIGNXX ,my_real *SIGNYY ,my_real *SIGNXY  ,my_real *SIGNYZ  ,my_real *SIGNZX ,
      my_real *SIGVXX ,my_real *SIGVYY ,my_real *SIGVXY  ,my_real *SIGVYZ  ,my_real *SIGVZX ,
      my_real *SOUNDSP,my_real *VISCMAX,my_real *THK     ,my_real *PLA     ,my_real *UVAR   ,
      my_real *OFF    ,int *NGL    ,int *SHF);

/* ----------------------- */
/* User Law Rupture BRICK  */
/* ----------------------- */
void (*eng_flaw)(int*IRUP,
           int*NEL    ,int*NUPARAM,int*NUVAR   ,int*NFUNC   ,int*IFUNC   ,
           int*NPF    ,my_real *TF     ,my_real *TIME   ,my_real *TIMESTEP ,my_real *UPARAM  ,
           int*NGL    ,int*IPM     ,int*NPROPMI,int*MAT,int*IDEL7NOK,
           my_real *EPSPXX ,my_real *EPSPYY ,my_real *EPSPZZ ,my_real *EPSPXY,my_real *EPSPYZ,my_real *EPSPZX ,
           my_real *EPSXX  ,my_real *EPSYY  ,my_real *EPSZZ  ,my_real *EPSXY ,my_real *EPSYZ ,my_real *EPSZX  ,
           my_real *SIGNXX ,my_real *SIGNYY ,my_real *SIGNZZ ,my_real *SIGNXY,my_real *SIGNYZ,my_real *SIGNZX ,
           my_real *PLA    ,my_real *DPLA   ,my_real *EPSP   ,my_real *UVAR  ,my_real *OFF   ,
           my_real *BIDON1 ,my_real *BIDON2 ,my_real *BIDON3 ,my_real *BIDON4,my_real *BIDON5);

/* ------------------------ */
/* User Law Rupture SHELL  */
/* ------------------------ */
void (*eng_flawc)( int *IRUP,
       int *NEL   ,int *NUPARAM,int *NUVAR   ,int *NFUNC   ,int *IFUNC  ,int *NPF    ,
       my_real *TF    ,my_real *TIME    ,my_real *TIMESTEP,my_real *UPARAM  , int *NGL   ,int *IPT    ,
       int *NPT0  ,int *IPM    ,int *NPROPMI ,int *MAT   ,
       my_real *SIGNXX ,my_real *SIGNYY ,my_real *SIGNXY  ,my_real *SIGNYZ  ,my_real *SIGNZX ,
       my_real *EPSPXX ,my_real *EPSPYY ,my_real *EPSPXY  ,my_real *EPSPYZ  ,my_real *EPSPZX ,
       my_real *EPSXX  ,my_real *EPSYY  ,my_real *EPSXY   ,my_real *EPSYZ   ,my_real *EPSZX  ,
       my_real *PLA    ,my_real *DPLA   ,my_real *EPSP    ,my_real *UVAR    ,my_real *UEL    , 
       my_real *OFF    ,my_real *BIDON1  ,my_real *BIDON2   ,my_real *BIDON3  ,my_real *BIDON4  ,my_real *BIDON5   );


/* ---------------------------- */
/* User Laws sigeps99c (Shells) */
/* ---------------------------- */
void (*eng_sigeps99c)(int*NEL      ,int*NUPARAM ,int*NUVAR   ,int*ILAW_USER ,int*NFUNC  ,
         int*IFUNC    ,int *NPF     ,int*NGL    ,my_real *TF       ,my_real *TIME   ,
         my_real *TIMESTEP ,my_real *UPARAM  ,my_real *RHO    ,my_real *AREA      ,my_real *EINT   ,
         my_real *SHF       ,my_real *SOUNDSP ,my_real *VISCMAX ,my_real *PLA       ,my_real *UVAR   , 
         my_real *OFF       ,my_real *SIGY  );

/* ------------------------------- */
/* User Law sigeps99c get variables */
/* ------------------------------- */
void (*eng_get_lawc_user_var)( int*NCYCLE, int*IMAT, int*ILAYER, int*NPTA, int*IFLAG,
                     my_real* R11,     my_real*R12,    my_real*R13,    my_real*R21,    my_real*R22,
                     my_real* R23,     my_real*R31,    my_real*R32,    my_real*R33,    my_real*SIGOXX,
                     my_real* SIGOYY, my_real*SIGOXY, my_real*SIGOYZ, my_real*SIGOZX, my_real*EPSPXX,
                     my_real* EPSPYY, my_real*EPSPXY, my_real*EPSPYZ, my_real*EPSPZX, my_real*EPSXX,
                     my_real* EPSYY,  my_real*EPSXY,  my_real*EPSYZ,  my_real*EPSZX,  my_real*DEPSXX,
                     my_real* DEPSYY, my_real*DEPSXY, my_real*DEPSYZ, my_real*DEPSZX, my_real*THKLYL,      
                     my_real* THKN,   my_real*SIGNXX, my_real*SIGNYY, my_real*SIGNXY, my_real*SIGNYZ,
                     my_real* SIGNZX, my_real*SIGVXX, my_real*SIGVYY, my_real*SIGVXY, my_real*SIGVYZ,
                     my_real* SIGVZX, my_real*DPLA );
                    
                    
void (*eng_get_lawc_user_var_2) (     my_real* VAR01,int *SIZVAR01,my_real* VAR02,int *SIZVAR02,
         my_real* VAR03,int *SIZVAR03,my_real* VAR04,int *SIZVAR04,my_real* VAR05,int *SIZVAR05,my_real* VAR06,int * SIZVAR06,
         my_real* VAR07,int *SIZVAR07,my_real* VAR08,int *SIZVAR08,my_real* VAR09,int *SIZVAR09,my_real* VAR10,int * SIZVAR10,
         my_real* VAR11,int *SIZVAR11,my_real* VAR12,int *SIZVAR12,my_real* VAR13,int *SIZVAR13,my_real* VAR14,int * SIZVAR14,
         my_real* VAR15,int *SIZVAR15,my_real* VAR16,int *SIZVAR16,my_real* VAR17,int *SIZVAR17,my_real* VAR18,int * SIZVAR18,    
         my_real* VAR19,int *SIZVAR19,my_real* VAR20,int *SIZVAR20,my_real* VAR21,int *SIZVAR21,my_real* VAR22,int * SIZVAR22,
         my_real* VAR23,int *SIZVAR23,my_real* VAR24,int *SIZVAR24,my_real* VAR25,int *SIZVAR25,my_real* VAR26,int * SIZVAR26,    
         my_real* VAR27,int *SIZVAR27,my_real* VAR28,int *SIZVAR28,my_real* VAR29,int *SIZVAR29,my_real* VAR30,int * SIZVAR30,
         my_real* VAR31,int *SIZVAR31,my_real* VAR32,int *SIZVAR32,my_real* VAR33,int *SIZVAR33,my_real* VAR34,int * SIZVAR34,    
         my_real* VAR35,int *SIZVAR35,my_real* VAR36,int *SIZVAR36,my_real* VAR37,int *SIZVAR37,my_real* VAR38,int * SIZVAR38,
         my_real* VAR39,int *SIZVAR39,my_real* VAR40,int *SIZVAR40,my_real* VAR41,int *SIZVAR41,my_real* VAR42,int * SIZVAR42,    
         my_real* VAR43,int *SIZVAR43,my_real* VAR44,int *SIZVAR44,my_real* VAR45,int *SIZVAR45,my_real* VAR46,int * SIZVAR46,
         my_real* VAR47,int *SIZVAR47,my_real* VAR48,int *SIZVAR48,my_real* VAR49,int *SIZVAR49,my_real* VAR50,int * SIZVAR50,    
         my_real* VAR51,int *SIZVAR51,my_real* VAR52,int *SIZVAR52,my_real* VAR53,int *SIZVAR53,my_real* VAR54,int * SIZVAR54,
         my_real* VAR55,int *SIZVAR55,my_real* VAR56,int *SIZVAR56,my_real* VAR57,int *SIZVAR57,my_real* VAR58,int * SIZVAR58,    
         my_real* VAR59,int *SIZVAR59,my_real* VAR60,int *SIZVAR60,my_real* VAR61,int *SIZVAR61,my_real* VAR62,int * SIZVAR62,
         my_real* VAR63,int *SIZVAR63,my_real* VAR64,int *SIZVAR64,my_real* VAR65,int *SIZVAR65,my_real* VAR66,int * SIZVAR66,    
         my_real* VAR67,int *SIZVAR67,my_real* VAR68,int *SIZVAR68,my_real* VAR69,int *SIZVAR69,my_real* VAR70,int * SIZVAR70,
         my_real* VAR71,int *SIZVAR72,my_real* VAR73,int *SIZVAR73,my_real* VAR74,int *SIZVAR74,my_real* VAR75,int * SIZVAR75,
         my_real* VAR76,int *SIZVAR76,my_real* VAR77,int *SIZVAR77,my_real* VAR78,int *SIZVAR78,my_real* VAR79,int * SIZVAR79,
         my_real* VAR80,int *SIZVAR80,my_real* VAR81,int *SIZVAR81,my_real* VAR82,int *SIZVAR82,my_real* VAR83,int * SIZVAR83,    
         my_real* VAR84,int *SIZVAR84,my_real* VAR85,int *SIZVAR85,my_real* VAR86,int *SIZVAR86,my_real* VAR87,int * SIZVAR87,
         my_real* VAR88,int *SIZVAR88,my_real* VAR89,int *SIZVAR89,my_real* VAR90,int *SIZVAR90,my_real* VAR91,int * SIZVAR91,    
         my_real* VAR92,int *SIZVAR92,my_real* VAR93,int *SIZVAR93,my_real* VAR94,int *SIZVAR94,my_real* VAR95,int * SIZVAR95,
         my_real* VAR96,int *SIZVAR96,my_real* VAR97,int *SIZVAR97,my_real* VAR98,int *SIZVAR98,my_real* VAR99,int * SIZVAR99);

/* ----------------------------------- */
/* User Law sigeps99 copy back results */
/* ----------------------------------- */
void (*eng_set_lawc_user_var) ( my_real*SIGNXX,  my_real*SIGNYY, my_real*SIGNXY, my_real* SIGNYZ,  my_real*SIGNZX,
                     my_real* SIGVXX, my_real*SIGVYY, my_real*SIGVXY, my_real*SIGVYZ,   my_real*SIGVZX,
                     my_real* DPLA,   my_real*ETSE,   my_real* THKN ) ;

void (*eng_set_lawc_user_var_2) (my_real* VAR01,int*SIZVAR01,my_real* VAR02,int*SIZVAR02,
         my_real* VAR03,int*SIZVAR03,my_real* VAR04,int*SIZVAR04,my_real* VAR05,int*SIZVAR05,my_real* VAR06,int*SIZVAR06,
         my_real* VAR07,int*SIZVAR07,my_real* VAR08,int*SIZVAR08,my_real* VAR09,int*SIZVAR09,my_real* VAR10,int*SIZVAR10,
         my_real* VAR11,int*SIZVAR11,my_real* VAR12,int*SIZVAR12,my_real* VAR13,int*SIZVAR13,my_real* VAR14,int*SIZVAR14,
         my_real* VAR15,int*SIZVAR15,my_real* VAR16,int*SIZVAR16,my_real* VAR17,int*SIZVAR17,my_real* VAR18,int*SIZVAR18,    
         my_real* VAR19,int*SIZVAR19,my_real* VAR20,int*SIZVAR20,my_real* VAR21,int*SIZVAR21,my_real* VAR22,int*SIZVAR22,
         my_real* VAR23,int*SIZVAR23,my_real* VAR24,int*SIZVAR24,my_real* VAR25,int*SIZVAR25,my_real* VAR26,int*SIZVAR26,    
         my_real* VAR27,int*SIZVAR27,my_real* VAR28,int*SIZVAR28,my_real* VAR29,int*SIZVAR29,my_real* VAR30,int*SIZVAR30,
         my_real* VAR31,int*SIZVAR31,my_real* VAR32,int*SIZVAR32,my_real* VAR33,int*SIZVAR33,my_real* VAR34,int*SIZVAR34,    
         my_real* VAR35,int*SIZVAR35,my_real* VAR36,int*SIZVAR36,my_real* VAR37,int*SIZVAR37,my_real* VAR38,int*SIZVAR38,
         my_real* VAR39,int*SIZVAR39,my_real* VAR40,int*SIZVAR40,my_real* VAR41,int*SIZVAR41,my_real* VAR42,int*SIZVAR42,    
         my_real* VAR43,int*SIZVAR43,my_real* VAR44,int*SIZVAR44,my_real* VAR45,int*SIZVAR45,my_real* VAR46,int*SIZVAR46,
         my_real* VAR47,int*SIZVAR47,my_real* VAR48,int*SIZVAR48,my_real* VAR49,int*SIZVAR49,my_real* VAR50,int*SIZVAR50,    
         my_real* VAR51,int*SIZVAR51,my_real* VAR52,int*SIZVAR52,my_real* VAR53,int*SIZVAR53,my_real* VAR54,int*SIZVAR54,
         my_real* VAR55,int*SIZVAR55,my_real* VAR56,int*SIZVAR56,my_real* VAR57,int*SIZVAR57,my_real* VAR58,int*SIZVAR58,    
         my_real* VAR59,int*SIZVAR59,my_real* VAR60,int*SIZVAR60,my_real* VAR61,int*SIZVAR61,my_real* VAR62,int*SIZVAR62,
         my_real* VAR63,int*SIZVAR63,my_real* VAR64,int*SIZVAR64,my_real* VAR65,int*SIZVAR65,my_real* VAR66,int*SIZVAR66,    
         my_real* VAR67,int*SIZVAR67,my_real* VAR68,int*SIZVAR68,my_real* VAR69,int*SIZVAR69,my_real* VAR70,int*SIZVAR70,
         my_real* VAR71,int*SIZVAR72,my_real* VAR73,int*SIZVAR73,my_real* VAR74,int*SIZVAR74,my_real* VAR75,int*SIZVAR75,
         my_real* VAR76,int*SIZVAR76,my_real* VAR77,int*SIZVAR77,my_real* VAR78,int*SIZVAR78,my_real* VAR79,int*SIZVAR79,
         my_real* VAR80,int*SIZVAR80,my_real* VAR81,int*SIZVAR81,my_real* VAR82,int*SIZVAR82,my_real* VAR83,int*SIZVAR83,    
         my_real* VAR84,int*SIZVAR84,my_real* VAR85,int*SIZVAR85,my_real* VAR86,int*SIZVAR86,my_real* VAR87,int*SIZVAR87,
         my_real* VAR88,int*SIZVAR88,my_real* VAR89,int*SIZVAR89,my_real* VAR90,int*SIZVAR90,my_real* VAR91,int*SIZVAR91,    
         my_real* VAR92,int*SIZVAR92,my_real* VAR93,int*SIZVAR93,my_real* VAR94,int*SIZVAR94,my_real* VAR95,int*SIZVAR95,
         my_real* VAR96,int*SIZVAR96,my_real* VAR97,int*SIZVAR97,my_real* VAR98,int*SIZVAR98,my_real* VAR99,int*SIZVAR99);
/* ---------------------------- */
/* User Laws sigeps99  (Solids) */
/* ---------------------------- */
void (*eng_sigeps99) (
         int*NEL     ,int*NUPARAM     ,int*NUVAR      ,int*ILAW_USER  ,int*NFUNC   ,
         int*IFUNC       ,int*NPF      ,my_real*TF      ,my_real*TIME   ,my_real*TIMESTEP,
         my_real*UPARAM  ,my_real*RHO      ,my_real*VOLUME ,my_real*EINT   ,int*NGL    ,
         my_real*SOUNDSP ,my_real*VISCMAX ,my_real*UVAR   ,my_real*OFF      ,my_real*SIGY   ,
         my_real*PLA  );

void (*eng_get_law_user_var)(  int*NCYCLE, int*IMAT, int*IPTR, int*IPTS, int*IPTT,
                               my_real*UR11, my_real*R12,  my_real*R13,  my_real*R21,  my_real*R22,  my_real*R23,   my_real*R31,
                               my_real*UR32, my_real*R33,  my_real*SO1,  my_real*SO2,  my_real*SO3,  my_real*SO4,   my_real*SO5,
                               my_real*SO6,  my_real*EP1,  my_real*EP2,  my_real*EP3,  my_real*EP4,  my_real*EP5,   my_real*EP6,
                               my_real*ES1,  my_real*ES2,  my_real*ES3,  my_real*ES4,  my_real*ES5,  my_real*ES6,   my_real*DE1,
                               my_real*DE2,  my_real*DE3,  my_real*DE4,  my_real*DE5,  my_real*DE6,  my_real*RHO0,  my_real*S1,
                               my_real*S2,   my_real*S3,   my_real*S4,   my_real*S5,   my_real*S6,   my_real*SV1,   my_real*SV2,
                               my_real*SV3,  my_real*SV4,  my_real*SV5,  my_real*SV6  );


void (*eng_set_law_user_var)(  my_real*S1,  my_real*S2,   my_real*S3,  my_real*S4,  my_real*S5,  my_real*S6,
                               my_real*SV1, my_real*SV2,  my_real*SV3, my_real*SV4, my_real*SV5, my_real*SV6,
                               my_real*DPLA );

void (*eng_get_law_user_var_2) ( my_real*FPSXX,int*SIZFPSXX,my_real*FPSYY,int *SIZFPSYY,
         my_real*FPSZZ,int *SIZFPSZZ,my_real*FPSXY,int *SIZFPSXY,my_real*FPSYZ,int *SIZFPSYZ,my_real*FPSZX,int *SIZFPSZX,
         my_real*FPSYX,int *SIZFPSYX,my_real*FPSZY,int *SIZFPSZY,my_real*FPSXZ,int *SIZFPSXZ,my_real*VAR10,int *SIZVAR10,
         my_real*VAR11,int *SIZVAR11,my_real*VAR12,int *SIZVAR12,my_real*VAR13,int *SIZVAR13,my_real*VAR14,int *SIZVAR14,
         my_real*VAR15,int *SIZVAR15,my_real*VAR16,int *SIZVAR16,my_real*VAR17,int *SIZVAR17,my_real*VAR18,int *SIZVAR18,    
         my_real*VAR19,int *SIZVAR19,my_real*VAR20,int *SIZVAR20,my_real*VAR21,int *SIZVAR21,my_real*VAR22,int *SIZVAR22,
         my_real*VAR23,int *SIZVAR23,my_real*VAR24,int *SIZVAR24,my_real*VAR25,int *SIZVAR25,my_real*VAR26,int *SIZVAR26,    
         my_real*VAR27,int *SIZVAR27,my_real*VAR28,int *SIZVAR28,my_real*VAR29,int *SIZVAR29,my_real*VAR30,int *SIZVAR30,
         my_real*VAR31,int *SIZVAR31,my_real*VAR32,int *SIZVAR32,my_real*VAR33,int *SIZVAR33,my_real*VAR34,int *SIZVAR34,    
         my_real*VAR35,int *SIZVAR35,my_real*VAR36,int *SIZVAR36,my_real*VAR37,int *SIZVAR37,my_real*VAR38,int *SIZVAR38,
         my_real*VAR39,int *SIZVAR39,my_real*VAR40,int *SIZVAR40,my_real*VAR41,int *SIZVAR41,my_real*VAR42,int *SIZVAR42,    
         my_real*VAR43,int *SIZVAR43,my_real*VAR44,int *SIZVAR44,my_real*VAR45,int *SIZVAR45,my_real*VAR46,int *SIZVAR46,
         my_real*VAR47,int *SIZVAR47,my_real*VAR48,int *SIZVAR48,my_real*VAR49,int *SIZVAR49,my_real*VAR50,int *SIZVAR50,    
         my_real*VAR51,int *SIZVAR51,my_real*VAR52,int *SIZVAR52,my_real*VAR53,int *SIZVAR53,my_real*VAR54,int *SIZVAR54,
         my_real*VAR55,int *SIZVAR55,my_real*VAR56,int *SIZVAR56,my_real*VAR57,int *SIZVAR57,my_real*VAR58,int *SIZVAR58,    
         my_real*VAR59,int *SIZVAR59,my_real*VAR60,int *SIZVAR60,my_real*VAR61,int *SIZVAR61,my_real*VAR62,int *SIZVAR62,
         my_real*VAR63,int *SIZVAR63,my_real*VAR64,int *SIZVAR64,my_real*VAR65,int *SIZVAR65,my_real*VAR66,int *SIZVAR66,    
         my_real*VAR67,int *SIZVAR67,my_real*VAR68,int *SIZVAR68,my_real*VAR69,int *SIZVAR69,my_real*VAR70,int *SIZVAR70,
         my_real*VAR71,int *SIZVAR72,my_real*VAR73,int *SIZVAR73,my_real*VAR74,int *SIZVAR74,my_real*VAR75,int *SIZVAR75,
         my_real*VAR76,int *SIZVAR76,my_real*VAR77,int *SIZVAR77,my_real*VAR78,int *SIZVAR78,my_real*VAR79,int *SIZVAR79,
         my_real*VAR80,int *SIZVAR80,my_real*VAR81,int *SIZVAR81,my_real*VAR82,int *SIZVAR82,my_real*VAR83,int *SIZVAR83,    
         my_real*VAR84,int *SIZVAR84,my_real*VAR85,int *SIZVAR85,my_real*VAR86,int *SIZVAR86,my_real*VAR87,int *SIZVAR87,
         my_real*VAR88,int *SIZVAR88,my_real*VAR89,int *SIZVAR89,my_real*VAR90,int *SIZVAR90,my_real*VAR91,int *SIZVAR91,    
         my_real*VAR92,int *SIZVAR92,my_real*VAR93,int *SIZVAR93,my_real*VAR94,int *SIZVAR94,my_real*VAR95,int *SIZVAR95,
         my_real*VAR96,int *SIZVAR96,my_real*VAR97,int *SIZVAR97,my_real*VAR98,int *SIZVAR98,my_real*VAR99,int *SIZVAR99);

void (*eng_set_law_user_var_2) (     my_real*VAR01,int *SIZVAR01,my_real*VAR02,int *SIZVAR02,
         my_real*VAR03,int *SIZVAR03,my_real*VAR04,int *SIZVAR04,my_real*VAR05,int *SIZVAR05,my_real*VAR06,int *SIZVAR06,
         my_real*VAR07,int *SIZVAR07,my_real*VAR08,int *SIZVAR08,my_real*VAR09,int *SIZVAR09,my_real*VAR10,int *SIZVAR10,
         my_real*VAR11,int *SIZVAR11,my_real*VAR12,int *SIZVAR12,my_real*VAR13,int *SIZVAR13,my_real*VAR14,int *SIZVAR14,
         my_real*VAR15,int *SIZVAR15,my_real*VAR16,int *SIZVAR16,my_real*VAR17,int *SIZVAR17,my_real*VAR18,int *SIZVAR18,    
         my_real*VAR19,int *SIZVAR19,my_real*VAR20,int *SIZVAR20,my_real*VAR21,int *SIZVAR21,my_real*VAR22,int *SIZVAR22,
         my_real*VAR23,int *SIZVAR23,my_real*VAR24,int *SIZVAR24,my_real*VAR25,int *SIZVAR25,my_real*VAR26,int *SIZVAR26,    
         my_real*VAR27,int *SIZVAR27,my_real*VAR28,int *SIZVAR28,my_real*VAR29,int *SIZVAR29,my_real*VAR30,int *SIZVAR30,
         my_real*VAR31,int *SIZVAR31,my_real*VAR32,int *SIZVAR32,my_real*VAR33,int *SIZVAR33,my_real*VAR34,int *SIZVAR34,    
         my_real*VAR35,int *SIZVAR35,my_real*VAR36,int *SIZVAR36,my_real*VAR37,int *SIZVAR37,my_real*VAR38,int *SIZVAR38,
         my_real*VAR39,int *SIZVAR39,my_real*VAR40,int *SIZVAR40,my_real*VAR41,int *SIZVAR41,my_real*VAR42,int *SIZVAR42,    
         my_real*VAR43,int *SIZVAR43,my_real*VAR44,int *SIZVAR44,my_real*VAR45,int *SIZVAR45,my_real*VAR46,int *SIZVAR46,
         my_real*VAR47,int *SIZVAR47,my_real*VAR48,int *SIZVAR48,my_real*VAR49,int *SIZVAR49,my_real*VAR50,int *SIZVAR50,    
         my_real*VAR51,int *SIZVAR51,my_real*VAR52,int *SIZVAR52,my_real*VAR53,int *SIZVAR53,my_real*VAR54,int *SIZVAR54,
         my_real*VAR55,int *SIZVAR55,my_real*VAR56,int *SIZVAR56,my_real*VAR57,int *SIZVAR57,my_real*VAR58,int *SIZVAR58,    
         my_real*VAR59,int *SIZVAR59,my_real*VAR60,int *SIZVAR60,my_real*VAR61,int *SIZVAR61,my_real*VAR62,int *SIZVAR62,
         my_real*VAR63,int *SIZVAR63,my_real*VAR64,int *SIZVAR64,my_real*VAR65,int *SIZVAR65,my_real*VAR66,int *SIZVAR66,    
         my_real*VAR67,int *SIZVAR67,my_real*VAR68,int *SIZVAR68,my_real*VAR69,int *SIZVAR69,my_real*VAR70,int *SIZVAR70,
         my_real*VAR71,int *SIZVAR72,my_real*VAR73,int *SIZVAR73,my_real*VAR74,int *SIZVAR74,my_real*VAR75,int *SIZVAR75,
         my_real*VAR76,int *SIZVAR76,my_real*VAR77,int *SIZVAR77,my_real*VAR78,int *SIZVAR78,my_real*VAR79,int *SIZVAR79,
         my_real*VAR80,int *SIZVAR80,my_real*VAR81,int *SIZVAR81,my_real*VAR82,int *SIZVAR82,my_real*VAR83,int *SIZVAR83,    
         my_real*VAR84,int *SIZVAR84,my_real*VAR85,int *SIZVAR85,my_real*VAR86,int *SIZVAR86,my_real*VAR87,int *SIZVAR87,
         my_real*VAR88,int *SIZVAR88,my_real*VAR89,int *SIZVAR89,my_real*VAR90,int *SIZVAR90,my_real*VAR91,int *SIZVAR91,    
         my_real*VAR92,int *SIZVAR92,my_real*VAR93,int *SIZVAR93,my_real*VAR94,int *SIZVAR94,my_real*VAR95,int *SIZVAR95,
         my_real*VAR96,int *SIZVAR96,my_real*VAR97,int *SIZVAR97,my_real*VAR98,int *SIZVAR98,my_real*VAR99,int *SIZVAR99);

         
/* ------------ */
/* User strings */
/* ------------ */
void  (*eng_suser)(int*ITYP,
      int*NEL     ,int*NUVAR   ,int*IPROP  ,int*IMAT  ,int*SOLID_ID,my_real *TIME  ,my_real *TIMESTEP,
      my_real *EINT   ,my_real *VOL    ,my_real *UVAR   ,my_real *FR_WAVE,my_real *OFF    ,my_real *RHO    ,my_real *SIG    ,
      my_real *XX1    ,my_real *XX2    ,my_real *XX3    ,my_real *XX4    ,my_real *XX5    ,my_real *XX6    ,my_real *XX7    ,my_real *XX8    ,     
      my_real *YY1    ,my_real *YY2    ,my_real *YY3    ,my_real *YY4    ,my_real *YY5    ,my_real *YY6    ,my_real *YY7    ,my_real *YY8    ,  
      my_real *ZZ1    ,my_real *ZZ2    ,my_real *ZZ3    ,my_real *ZZ4    ,my_real *ZZ5    ,my_real *ZZ6    ,my_real *ZZ7    ,my_real *ZZ8    ,
      my_real *UX1    ,my_real *UX2    ,my_real *UX3    ,my_real *UX4    ,my_real *UX5    ,my_real *UX6    ,my_real *UX7    ,my_real *UX8    ,
      my_real *UY1    ,my_real *UY2    ,my_real *UY3    ,my_real *UY4    ,my_real *UY5    ,my_real *UY6    ,my_real *UY7    ,my_real *UY8    ,
      my_real *UZ1    ,my_real *UZ2    ,my_real *UZ3    ,my_real *UZ4    ,my_real *UZ5    ,my_real *UZ6    ,my_real *UZ7    ,my_real *UZ8    ,
      my_real *VX1    ,my_real *VX2    ,my_real *VX3    ,my_real *VX4    ,my_real *VX5    ,my_real *VX6    ,my_real *VX7    ,my_real *VX8    ,
      my_real *VY1    ,my_real *VY2    ,my_real *VY3    ,my_real *VY4    ,my_real *VY5    ,my_real *VY6    ,my_real *VY7    ,my_real *VY8    ,
      my_real *VZ1    ,my_real *VZ2    ,my_real *VZ3    ,my_real *VZ4    ,my_real *VZ5    ,my_real *VZ6    ,my_real *VZ7    ,my_real *VZ8    ,
      my_real *VRX1   ,my_real *VRX2   ,my_real *VRX3   ,my_real *VRX4   ,my_real *VRX5   ,my_real *VRX6   ,my_real *VRX7   ,my_real *VRX8   ,
      my_real *VRY1   ,my_real *VRY2   ,my_real *VRY3   ,my_real *VRY4   ,my_real *VRY5   ,my_real *VRY6   ,my_real *VRY7   ,my_real *VRY8   ,
      my_real *VRZ1   ,my_real *VRZ2   ,my_real *VRZ3   ,my_real *VRZ4   ,my_real *VRZ5   ,my_real *VRZ6   ,my_real *VRZ7   ,my_real *VRZ8   ,
      my_real *FX1    ,my_real *FX2    ,my_real *FX3    ,my_real *FX4    ,my_real *FX5    ,my_real *FX6    ,my_real *FX7    ,my_real *FX8    ,
      my_real *FY1    ,my_real *FY2    ,my_real *FY3    ,my_real *FY4    ,my_real *FY5    ,my_real *FY6    ,my_real *FY7    ,my_real *FY8    ,
      my_real *FZ1    ,my_real *FZ2    ,my_real *FZ3    ,my_real *FZ4    ,my_real *FZ5    ,my_real *FZ6    ,my_real *FZ7    ,my_real *FZ8    ,
      my_real * MX1    ,my_real *MX2   ,my_real *MX3    ,my_real *MX4    ,my_real *MX5    ,my_real *MX6    ,my_real *MX7    ,my_real *MX8    ,
      my_real *MY1    ,my_real *MY2    ,my_real *MY3    ,my_real *MY4    ,my_real *MY5    ,my_real *MY6    ,my_real *MY7    ,my_real *MY8    ,
      my_real *MZ1    ,my_real *MZ2    ,my_real *MZ3    ,my_real *MZ4    ,my_real *MZ5    ,my_real *MZ6    ,my_real *MZ7    ,my_real *MZ8    ,
      my_real *STIFM  ,my_real *STIFR  ,my_real *VISCM  ,my_real *VISCR  );


/* ------------------------- */
/* T2 INTERFACE USER RUPTURE */
/* ------------------------- */
void (*eng_userint)(int *IGTYP,
      int *NSN       ,int *II       ,int *PID      ,int *NUVAR     ,  
      my_real * UVAR  );

void (*eng_get_uintbuf_var)(int *ISLAVE, my_real *AREA, my_real *DT,my_real *DXN,my_real *DXT,my_real *SIGN,my_real *SIGT,
                           my_real *RUPT, my_real *FACN, my_real *FACT );

void (*eng_userwi)(char *ROOTN ,int *ROOTLEN ,
             int *NUVAR  ,int *NUVARI ,int *NUMNOD ,
             int *NCYCLE ,int *LENWA  ,int *IUVAR  ,int *ITAB   ,my_real *TT     ,
             my_real *DT1    ,my_real *DT2     ,my_real *USREINT,my_real *EXWORK ,my_real *UVAR   ,
             my_real *D    ,my_real *X     ,my_real *V      ,my_real *VR     ,my_real *MASS   ,
             my_real *INER    ,my_real *STIFN  ,my_real *STIFR  ,my_real *A       ,my_real *AR     ,
             my_real *WA    );


void (*eng_user_sens) (int *TYP,int *ID);

/* ----------------------------------------------- */
void (*engine_user_check) ( int *MY_RANK, double *TSTOP, int *NCYCLE, double *TT, int *MSTOP ) ;
void (*engine_user_finalize) ( int *MY_RANK ) ;
void (*engine_user_initialize) (int *NSPMD, int *NTHREAD, int *MY_RANK) ;
/* ----------------------------------------------- */

void eng_array_init_(){
       userlibhandle         = NULL;
       userlib_id            = NULL;
           eng_ruser             = NULL;
       eng_sigeps            = NULL;
       eng_sigepsc           = NULL;
           eng_flaw              = NULL;
           eng_flawc             = NULL;
           eng_sigeps99c         = NULL;
           eng_get_lawc_user_var = NULL;
           eng_set_lawc_user_var = NULL;
           eng_sigeps99          = NULL;
           eng_get_law_user_var  = NULL;
           eng_set_law_user_var  = NULL;
           eng_suser             = NULL;
           eng_userint           = NULL;
           eng_get_uintbuf_var   = NULL;
           eng_userwi            = NULL; 
           eng_user_sens         = NULL;
           engine_user_check     = NULL;
           engine_user_finalize  = NULL;
           engine_user_initialize= NULL;

}

#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
void init_callback(void ** callback_array);
void userlib_init_callback();

HINSTANCE userlibhandler;
void (*set_library_callback) (void * callback_array[200]);

void _FCALL DYN_USERLIB_INIT (char * libname, int *size, int * userlib_avail, int * userlib_ver,int * iresp,int * userlib_altname, int * dlib_array){

     char* libn,current_dir;
     int i;     
     char rname[256];
     char dllpath[10240];
     char *dllname;
     int sz_dllname;
     int result,dllpath_size;
     int err;
     int has_path;
     err=0;
     *userlib_avail=0;
     eng_array_init_();

    if(*userlib_altname==0){
     libname[*size]='\0';
     
     if(*iresp==1){
       strcat_s(libname,2048,"win64_sp.dll");   // libname size set as 2048 in userlib.inc / Fortran
       *size = *size + 12;
     }else{
       strcat_s(libname,2048,"win64.dll");   // libname size set as 2048 in userlib.inc / Fortran
       *size = *size + 9;
     }
    }
     libn = (char * )malloc(sizeof(char)* *size+1);
     strcpy_s(libn,*size+1,libname);


/* it is possible to set a path when using alternate library names*/
     has_path=0;
     for (i=0;i<*size;i++){ 
       if (libn[i]==47 || libn[i]==92)has_path=1;
     }
     
     userlibhandler=NULL;
     if (has_path==1){
        userlibhandler = LoadLibrary(TEXT(libn));
     }else{

       sz_dllname=15360;
       dllname=(char*)malloc(sizeof(char)*15360);
       /* first trial find Environment variable RAD_USERLIB_LIBPATH */
       dllpath_size=GetEnvironmentVariable("RAD_USERLIB_LIBPATH",dllpath,10240);
       
       if (dllpath_size > 0){
         strcpy_s(dllname,sz_dllname,dllpath);
         strcat_s(dllname,sz_dllname,"\\");
         strcat_s(dllname,sz_dllname,libn);
         userlibhandler = LoadLibrary(TEXT(dllname));
       }
       
       if (!userlibhandler){
       /* second trial find Environment variable in local directory */
         dllpath_size=GetCurrentDirectory(10240,dllpath);
         strcpy_s(dllname,sz_dllname,dllpath);
         strcat_s(dllname,sz_dllname,"\\");
         strcat_s(dllname,sz_dllname,libn);     
       userlibhandler = LoadLibrary(TEXT(dllname));
       }
            
       if (!userlibhandler){
       /* third trial find Environment variable in %PATH% */
       dllpath_size=GetEnvironmentVariable("PATH",dllpath,10240);
       SetDllDirectory(dllpath);
       userlibhandler = LoadLibrary(TEXT(libn));
       }

       free(dllname);
     }


     if (userlibhandler){

/* Routine ENG_RUSER*/
         sprintf(rname,"ENG_RUSER");
         eng_ruser=(void*)GetProcAddress(userlibhandler,rname);
     if (!eng_ruser) err=err+1;

/* Routine ENG_SIGEPS*/
         sprintf(rname,"ENG_SIGEPS");
         eng_sigeps=(void*)GetProcAddress(userlibhandler,rname);
     if (!eng_sigeps) err=err+1;

/* Routine ENG_SIGEPSC*/
         sprintf(rname,"ENG_SIGEPSC");
         eng_sigepsc=(void*)GetProcAddress(userlibhandler,rname);
     if (!eng_sigepsc) err=err+1;

/* Routine ENG_FLAW*/
         sprintf(rname,"ENG_FLAW");
         eng_flaw=(void*)GetProcAddress(userlibhandler,rname);
     if (!eng_flaw) err=err+1;


/* Routine ENG_FLAWC*/
         sprintf(rname,"ENG_FLAWC");
         eng_flawc=(void*)GetProcAddress(userlibhandler,rname);
     if (!eng_flawc) err=err+1;

/* Routine ENG_SUSER*/
         sprintf(rname,"ENG_SUSER");
         eng_suser=(void*)GetProcAddress(userlibhandler,rname);
     if (!eng_suser) err=err+1;

/* Routine ENG_USERINT*/
         sprintf(rname,"ENG_USERINT");
         eng_userint=(void*)GetProcAddress(userlibhandler,rname);
     if (!eng_userint) err=err+1;

/* Routine ENG_GET_UINTBUF_VAR*/
         sprintf(rname,"ENG_GET_UINTBUF_VAR");
         eng_get_uintbuf_var=(void*)GetProcAddress(userlibhandler,rname);
     if (!eng_get_uintbuf_var) err=err+1;

/* Routine ENG_USERWI*/
         sprintf(rname,"ENG_USERWI");
         eng_userwi=(void*)GetProcAddress(userlibhandler,rname);
     if (!eng_userwi) err=err+1;

/* Routine ENG_USER_SENS*/
         sprintf(rname,"ENG_USER_SENS");
         eng_user_sens=(void*)GetProcAddress(userlibhandler,rname);
     if (!eng_user_sens) err=err+1;

/* Routine ENG_GET_LAWC_USERVAR*/
         sprintf(rname,"ENG_GET_LAWC_USER_VAR");
         eng_get_lawc_user_var=(void*)GetProcAddress(userlibhandler,rname);
     if (!eng_get_lawc_user_var) err=err+1;

/* Routine ENG_SIGEPS99C*/
         sprintf(rname,"ENG_SIGEPS99C");
         eng_sigeps99c=(void*)GetProcAddress(userlibhandler,rname);
     if (!eng_sigeps99c) err=err+1;

/* Routine ENG_SET_LAWC_USER_VAR*/
         sprintf(rname,"ENG_SET_LAWC_USER_VAR");
         eng_set_lawc_user_var=(void*)GetProcAddress(userlibhandler,rname);
     if (!eng_set_lawc_user_var) err=err+1;

/* Routine ENG_GET_LAW_USERVAR*/
         sprintf(rname,"ENG_GET_LAW_USER_VAR");
         eng_get_law_user_var=(void*)GetProcAddress(userlibhandler,rname);
     if (!eng_get_law_user_var) err=err+1;

/* Routine ENG_SIGEPS99C*/
         sprintf(rname,"ENG_SIGEPS99");
         eng_sigeps99=(void*)GetProcAddress(userlibhandler,rname);
     if (!eng_sigeps99) err=err+1;

/* Routine ENG_SET_LAW_USER_VAR*/
         sprintf(rname,"ENG_SET_LAW_USER_VAR");
         eng_set_law_user_var=(void*)GetProcAddress(userlibhandler,rname);
     if (!eng_get_law_user_var) err=err+1;

/* System - Library ID Version */
         sprintf(rname,"userlib_id");
         userlib_id=(void*)GetProcAddress(userlibhandler,rname);
     if(userlib_id) {
        userlib_id(userlib_ver);
        }else{
        err = err+1; 
     }
     
     
    
/* System - Windows Callback Routine */
         sprintf(rname,"set_callback");
         set_library_callback=(void*)GetProcAddress(userlibhandler,rname);
     if(set_library_callback) {
        userlib_init_callback();
        }else{
        err = err+1; 
     }
     
/* Addtionnal interface calls with Newer SDKs */
    if  (*userlib_ver >=1301501260){
/* Routine ENG_GET_LAW_USER_VAR*/
         sprintf(rname,"ENG_GET_LAW_USER_VAR_2");
         eng_get_law_user_var_2=(void*)GetProcAddress(userlibhandler,rname);
         if (!eng_get_law_user_var_2) err=err+1;
    
/* Routine ENG_SET_LAW_USER_VAR*/
         sprintf(rname,"ENG_SET_LAW_USER_VAR_2");
         eng_set_law_user_var_2=(void*)GetProcAddress(userlibhandler,rname);
//         if (!eng_set_law_user_var_2) err=err+1;
         
/* Routine ENG_GET_LAWC_USER_VAR*/
         sprintf(rname,"ENG_GET_LAWC_USER_VAR_2");
         eng_get_lawc_user_var_2=(void*)GetProcAddress(userlibhandler,rname);
         if (!eng_get_lawc_user_var_2) err=err+1;
    
/* Routine ENG_SET_LAWC_USER_VAR*/
         sprintf(rname,"ENG_SET_LAWC_USER_VAR_2");
         eng_set_lawc_user_var_2=(void*)GetProcAddress(userlibhandler,rname);
         if (!eng_set_lawc_user_var_2) err=err+1;
    
    }

/* Addtionnal interface calls with Newer SDKs */
//    if  (*userlib_ver >=210yymmddx){
//    if  (*userlib_ver >=2102011230){
    if  (*userlib_ver >=2102011230){
/* Routine ENGINE_USER_CHECK*/
         sprintf(rname,"ENGINE_USER_CHECK");
         engine_user_check=(void*)GetProcAddress(userlibhandler,rname);
         if(engine_user_check) dlib_array[0]=1;
//         if (!engine_user_check) err=err+1;
/* Routine ENGINE_USER_FINALIZE*/
         sprintf(rname,"ENGINE_USER_FINALIZE");
         engine_user_finalize=(void*)GetProcAddress(userlibhandler,rname);
         if(engine_user_finalize) dlib_array[1]=1;
//         if (!engine_user_finalize) err=err+1;
/* Routine ENGINE_USER_INITIALIZE*/
         sprintf(rname,"ENGINE_USER_INITIALIZE");
         engine_user_initialize=(void*)GetProcAddress(userlibhandler,rname);
         if(engine_user_initialize) dlib_array[2]=1;
//         if (!engine_user_initialize) err=err+1;
     }
    


     if (err==0)*userlib_avail = 1;
     
    }else{
/*        printf("load unsuccessfull\n");*/
    }
}

void userlib_init_callback()
{
void * callback_array[200];

init_callback(callback_array);
set_library_callback(callback_array);
}

#elif 1
void *userlibhandle;


#if CPP_mach==CPP_macosx64

void dyn_userlib_init_(char * libname, int *size, int * userlib_avail, int * userlib_ver ,int * iresp,int * userlib_altname, int * dlib_array){
     char* libn;
     int i;     
     char rname[256];
     int err;
     err=0;
     *userlib_avail=0;
     *userlib_ver=0;
}

#elif 1

void dyn_userlib_init_(char * libname, int *size, int * userlib_avail, int * userlib_ver ,int * iresp,int * userlib_altname, int * dlib_array){
     char* libn;
     int i,has_path,sz;     
     char rname[256],load_libname[15000];
     char libname_c[256];
     char * rad_userlib_libpath;
     int err;
     err=0;

     eng_array_init_();

    if(*userlib_altname==0){

#ifdef __aarch64__
      if(*iresp==1){
       strcpy(libname_c,"linuxa64_sp.so");
      }else{
       strcpy(libname_c,"linuxa64.so");
      }
       sz=strlen(libname_c);
       for (i=0;i<sz;i++) { libname[*size + i ] = libname_c[i]; }
       *size = *size + sz;
      
     
#else
     if (*iresp ==0){     
       libname[*size  ]='l';
       libname[*size+1]='i';
       libname[*size+2]='n';
       libname[*size+3]='u';
       libname[*size+4]='x';
       libname[*size+5]='6';
       libname[*size+6]='4';
       libname[*size+7]='.';
       libname[*size+8]='s';
       libname[*size+9]='o';
       *size = *size + 10;
     }else{
       libname[*size   ]='l';
       libname[*size+ 1]='i';
       libname[*size+ 2]='n';
       libname[*size+ 3]='u';
       libname[*size+ 4]='x';
       libname[*size+ 5]='6';
       libname[*size+ 6]='4';
       libname[*size+ 7]='_';
       libname[*size+ 8]='s';
       libname[*size+ 9]='p';
       libname[*size+10]='.';
       libname[*size+11]='s';
       libname[*size+12]='o';
       *size = *size + 13; 
     }
#endif
    }
     libn = (char * )malloc(sizeof(char)* *size+1);
     for (i=0;i<*size;i++)libn[i]=libname[i];
     libn[*size]='\0';

/* it is possible to set a path when using alternate library names*/
     has_path=0;
     for (i=0;i<*size;i++){ 
       if (libn[i]==47 || libn[i]==92)has_path=1;
     }

     userlibhandle = NULL;
     if (has_path==1){
        userlibhandle = dlopen(libn,RTLD_LAZY|RTLD_GLOBAL|RTLD_DEEPBIND);
     }else{

       /* first trial find Environment variable RAD_USERLIB_LIBPATH */
       rad_userlib_libpath=getenv("RAD_USERLIB_LIBPATH");
       if (rad_userlib_libpath){
         strcpy(load_libname,rad_userlib_libpath);
         strcat(load_libname,"/");
         strcat(load_libname,libn);
         userlibhandle = dlopen(load_libname,RTLD_LAZY|RTLD_GLOBAL|RTLD_DEEPBIND);
       }

       if(userlibhandle==NULL){
         /* second trial search lib in Current Working directory */
         getcwd(load_libname,15000);
         strcat(load_libname,"/");
         strcat(load_libname,libn);
         userlibhandle = dlopen(load_libname,RTLD_LAZY|RTLD_GLOBAL|RTLD_DEEPBIND);
       }

       if(userlibhandle==NULL){
         /* Third Trial : Default - LD_LRARY_PATH */
         userlibhandle = dlopen(libn,RTLD_LAZY|RTLD_GLOBAL|RTLD_DEEPBIND);
       }

     }

     /* DLSYM Engine routines in the Dynlib */
     if (userlibhandle) {

/* Routine ENG_RUSER*/
         sprintf(rname,"eng_ruser_");
         eng_ruser=dlsym(userlibhandle,rname);
         if (!eng_ruser) err=err+1;

/* Routine ENG_SIGEPS*/
         sprintf(rname,"eng_sigeps_");
         eng_sigeps=dlsym(userlibhandle,rname);
         if (!eng_sigeps) err=err+1;

/* Routine ENG_SIGEPSC*/
         sprintf(rname,"eng_sigepsc_");
         eng_sigepsc=dlsym(userlibhandle,rname);
         if (!eng_sigepsc) err=err+1;

/* Routine ENG_FLAW*/
         sprintf(rname,"eng_flaw_");
         eng_flaw=dlsym(userlibhandle,rname);
         if (!eng_flaw) err=err+1;


/* Routine ENG_FLAWC*/
         sprintf(rname,"eng_flawc_");
         eng_flawc=dlsym(userlibhandle,rname);
         if (!eng_flawc) err=err+1;

/* Routine ENG_SUSER*/
         sprintf(rname,"eng_suser_");
         eng_suser=dlsym(userlibhandle,rname);
         if (!eng_suser) err=err+1;

/* Routine ENG_USERINT*/
         sprintf(rname,"eng_userint_");
         eng_userint=dlsym(userlibhandle,rname);
         if (!eng_userint) err=err+1;

/* Routine ENG_GET_UINTBUF_VAR*/
         sprintf(rname,"eng_get_uintbuf_var_");
         eng_get_uintbuf_var=dlsym(userlibhandle,rname);
     if (!eng_get_uintbuf_var) err=err+1;

/* Routine ENG_USERWI*/
         sprintf(rname,"eng_userwi_");
         eng_userwi=dlsym(userlibhandle,rname);
     if (!eng_userwi) err=err+1;

/* Routine ENG_USER_SENS*/
         sprintf(rname,"eng_user_sens_");
         eng_user_sens=dlsym(userlibhandle,rname);
     if (!eng_user_sens) err=err+1;

/* Routine ENG_GET_LAWC_USERVAR*/
         sprintf(rname,"eng_get_lawc_user_var_");
         eng_get_lawc_user_var=dlsym(userlibhandle,rname);
     if (!eng_get_lawc_user_var) err=err+1;

/* Routine ENG_SIGEPS99C*/
         sprintf(rname,"eng_sigeps99c_");
         eng_sigeps99c=dlsym(userlibhandle,rname);
     if (!eng_sigeps99c) err=err+1;

/* Routine ENG_GET_LAWC_USERVAR*/
         sprintf(rname,"eng_set_lawc_user_var_");
         eng_set_lawc_user_var=dlsym(userlibhandle,rname);
     if (!eng_set_lawc_user_var) err=err+1;

/* Routine ENG_GET_LAW_USERVAR*/
         sprintf(rname,"eng_get_law_user_var_");
         eng_get_law_user_var=dlsym(userlibhandle,rname);
     if (!eng_get_law_user_var) err=err+1;

/* Routine ENG_SIGEPS99*/
         sprintf(rname,"eng_sigeps99_");
         eng_sigeps99=dlsym(userlibhandle,rname);
     if (!eng_sigeps99) err=err+1;

/* Routine ENG_SET_LAW_USERVAR*/
         sprintf(rname,"eng_set_law_user_var_");
         eng_set_law_user_var=dlsym(userlibhandle,rname);
     if (!eng_set_law_user_var) err=err+1;

/* System - Library ID Version */
         sprintf(rname,"userlib_id");
         userlib_id=dlsym(userlibhandle,rname);

     if(userlib_id) {
        userlib_id(userlib_ver);
        }else{err = err+1; }


         if  (*userlib_ver >=1301501260){

/* Routine ENG_SET_LAW_USER_VAR_2*/
         sprintf(rname,"eng_get_law_user_var_2_");
         eng_get_law_user_var_2=dlsym(userlibhandle,rname);
     if (!eng_get_law_user_var_2) err=err+1;

/* Routine ENG_SET_LAW_USER_VAR_2*/
         sprintf(rname,"eng_set_law_user_var_2_");
         eng_set_law_user_var_2=dlsym(userlibhandle,rname);
     if (!eng_set_law_user_var_2) err=err+1;

/* Routine ENG_GET_LAWC_USER_VAR_2*/
         sprintf(rname,"eng_get_lawc_user_var_2_");
         eng_get_lawc_user_var_2=dlsym(userlibhandle,rname);
     if (!eng_get_lawc_user_var_2) err=err+1;

/* Routine ENG_SET_LAWC_USER_VAR_2*/
         sprintf(rname,"eng_set_lawc_user_var_2_");
         eng_set_lawc_user_var_2=dlsym(userlibhandle,rname);
     if (!eng_set_lawc_user_var_2) err=err+1;


         }

/* Addtionnal interface calls with Newer SDKs */
      if  (*userlib_ver >=2102011230){
/* Routine ENGINE_USER_CHECK*/
         sprintf(rname,"engine_user_check_");
         engine_user_check=dlsym(userlibhandle,rname);
         if(engine_user_check) dlib_array[0]=1;
/* Routine ENGINE_USER_FINALIZE*/
         sprintf(rname,"engine_user_finalize_");
         engine_user_finalize=dlsym(userlibhandle,rname);
         if(engine_user_finalize) dlib_array[1]=1;
/* Routine ENGINE_USER_INITIALIZE*/
         sprintf(rname,"engine_user_initialize_");
         engine_user_initialize=dlsym(userlibhandle,rname);
         if(engine_user_initialize) dlib_array[2]=1;
     }

/*         printf("err:%i\n",err);*/
         if (err==0)*userlib_avail = 1;
 
         
     }else{
       *userlib_avail = 0;
     }
    
}
#endif

#endif

/* --------------------------------- */
/* User Material Laws 29-30-31 Brick */
/* --------------------------------- */

/* WINDOWS */
void _FCALL ENG_USERLIB_SIGEPS(int* ilaw ,
        int *NEL ,int* NUPARAM ,int* NUVAR ,int* NFUNC ,int* IFUNC ,
        int* NPF ,my_real *TF  ,my_real *TIME , my_real *TIMESTEP, my_real *UPARAM ,my_real *RHO0 ,
        my_real *RHO ,my_real *VOLUME ,my_real *EINT ,my_real *EPSPXX ,my_real *EPSPYY ,my_real *EPSPZZ  ,
        my_real *EPSPXY  ,my_real *EPSPYZ  ,my_real *EPSPZX ,my_real *DEPSXX ,my_real *DEPSYY ,my_real *DEPSZZ  ,
        my_real *DEPSXY  ,my_real *DEPSYZ  ,my_real *DEPSZX ,my_real *EPSXX  ,my_real *EPSYY  ,my_real *EPSZZ   ,
        my_real *EPSXY   ,my_real *EPSYZ   ,my_real *EPSZX  ,
        my_real *IGOXX ,my_real *SIGOYY ,my_real *SIGOZZ  ,my_real *SIGOXY  ,my_real *SIGOYZ  ,my_real *SIGOZX ,
        my_real *SIGNXX ,my_real *SIGNYY ,my_real *SIGNZZ  ,my_real *SIGNXY  ,my_real *SIGNYZ  ,my_real *SIGNZX ,
        my_real *SIGVXX ,my_real *SIGVYY ,my_real *SIGVZZ  ,my_real *SIGVXY  ,my_real *SIGVYZ  ,my_real *SIGVZX ,
        my_real *SOUNDSP,my_real *VISCMAX,my_real *UVAR    ,my_real *OFF     )
{
    if(eng_sigeps){
        (*eng_sigeps)(ilaw ,
                NEL     ,NUPARAM ,NUVAR  ,NFUNC    ,IFUNC          ,
                NPF     ,TF      ,TIME   ,TIMESTEP ,UPARAM ,RHO0   ,
                RHO     ,VOLUME  ,EINT   ,EPSPXX   ,EPSPYY ,EPSPZZ ,
                EPSPXY  ,EPSPYZ  ,EPSPZX ,DEPSXX   ,DEPSYY ,DEPSZZ  ,
                DEPSXY  ,DEPSYZ  ,DEPSZX ,EPSXX    ,EPSYY  ,EPSZZ   ,
                EPSXY   ,EPSYZ   ,EPSZX  ,
                IGOXX   ,SIGOYY  ,SIGOZZ ,SIGOXY   ,SIGOYZ ,SIGOZX ,
                SIGNXX  ,SIGNYY  ,SIGNZZ ,SIGNXY   ,SIGNYZ ,SIGNZX ,
                SIGVXX  ,SIGVYY  ,SIGVZZ ,SIGVXY   ,SIGVYZ ,SIGVZX ,
                SOUNDSP ,VISCMAX ,UVAR   ,OFF     );
    }

}


/* LINUX */
void  eng_userlib_sigeps_(int* ilaw ,
        int *NEL ,int* NUPARAM ,int* NUVAR ,int* NFUNC ,int* IFUNC ,
        int* NPF ,my_real *TF  ,my_real *TIME , my_real *TIMESTEP, my_real *UPARAM ,my_real *RHO0 ,
        my_real *RHO ,my_real *VOLUME ,my_real *EINT ,my_real *EPSPXX ,my_real *EPSPYY ,my_real *EPSPZZ  ,
        my_real *EPSPXY  ,my_real *EPSPYZ  ,my_real *EPSPZX ,my_real *DEPSXX ,my_real *DEPSYY ,my_real *DEPSZZ  ,
        my_real *DEPSXY  ,my_real *DEPSYZ  ,my_real *DEPSZX ,my_real *EPSXX  ,my_real *EPSYY  ,my_real *EPSZZ   ,
        my_real *EPSXY   ,my_real *EPSYZ   ,my_real *EPSZX  ,
        my_real *IGOXX ,my_real *SIGOYY ,my_real *SIGOZZ  ,my_real *SIGOXY  ,my_real *SIGOYZ  ,my_real *SIGOZX ,
        my_real *SIGNXX ,my_real *SIGNYY ,my_real *SIGNZZ  ,my_real *SIGNXY  ,my_real *SIGNYZ  ,my_real *SIGNZX ,
        my_real *SIGVXX ,my_real *SIGVYY ,my_real *SIGVZZ  ,my_real *SIGVXY  ,my_real *SIGVYZ  ,my_real *SIGVZX ,
        my_real *SOUNDSP,my_real *VISCMAX,my_real *UVAR    ,my_real *OFF     )
{
    if(eng_sigeps){
        (*eng_sigeps)(ilaw ,
                NEL     ,NUPARAM ,NUVAR  ,NFUNC    ,IFUNC          ,
                NPF     ,TF      ,TIME   ,TIMESTEP ,UPARAM ,RHO0   ,
                RHO     ,VOLUME  ,EINT   ,EPSPXX   ,EPSPYY ,EPSPZZ ,
                EPSPXY  ,EPSPYZ  ,EPSPZX ,DEPSXX   ,DEPSYY ,DEPSZZ  ,
                DEPSXY  ,DEPSYZ  ,DEPSZX ,EPSXX    ,EPSYY  ,EPSZZ   ,
                EPSXY   ,EPSYZ   ,EPSZX  ,
                IGOXX   ,SIGOYY  ,SIGOZZ ,SIGOXY   ,SIGOYZ ,SIGOZX ,
                SIGNXX  ,SIGNYY  ,SIGNZZ ,SIGNXY   ,SIGNYZ ,SIGNZX ,
                SIGVXX  ,SIGVYY  ,SIGVZZ ,SIGVXY   ,SIGVYZ ,SIGVZX ,
                SOUNDSP ,VISCMAX ,UVAR   ,OFF     );
    }

}



/* --------------------------------- */
/* User Material Laws 29-30-31 Shell */
/* --------------------------------- */

/* WINDOWS */
void  _FCALL ENG_USERLIB_SIGEPSC(int* ilaw ,
      int* NEL ,int*NUPARAM ,int*NUVAR ,int*NFUNC ,int*IFUNC ,
      int*NPF  ,int*NPT     ,int*IPT   ,int*IFLAG ,
      my_real *TF ,my_real *TIME ,my_real *TIMESTEP , my_real *UPARAM , my_real *RHO0   ,
      my_real *AREA   ,my_real *EINT   ,my_real *THKLY   ,
      my_real *EPSPXX ,my_real *EPSPYY ,my_real *EPSPXY  ,my_real *EPSPYZ  ,my_real *EPSPZX ,
      my_real *DEPSXX ,my_real *DEPSYY ,my_real *DEPSXY  ,my_real *DEPSYZ  ,my_real *DEPSZX ,
      my_real *EPSXX  ,my_real *EPSYY  ,my_real *EPSXY   ,my_real *EPSYZ   ,my_real *EPSZX  ,
      my_real *SIGOXX ,my_real *SIGOYY ,my_real *SIGOXY  ,my_real *SIGOYZ  ,my_real *SIGOZX ,
      my_real *SIGNXX ,my_real *SIGNYY ,my_real *SIGNXY  ,my_real *SIGNYZ  ,my_real *SIGNZX ,
      my_real *SIGVXX ,my_real *SIGVYY ,my_real *SIGVXY  ,my_real *SIGVYZ  ,my_real *SIGVZX ,
      my_real *SOUNDSP,my_real *VISCMAX,my_real *THK     ,my_real *PLA     ,my_real *UVAR   ,
      my_real *OFF    ,int *NGL    ,int *SHF){

      if(eng_sigepsc){
          (*eng_sigepsc)(ilaw ,
                  NEL    ,NUPARAM ,NUVAR    ,NFUNC  ,IFUNC ,
                NPF    ,NPT     ,IPT      ,IFLAG  ,
                TF     ,TIME    ,TIMESTEP ,UPARAM ,RHO0   ,
                AREA   ,EINT    ,THKLY    ,
                EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
                DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
                EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
                SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
                SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
                SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
                SOUNDSP,VISCMAX,THK     ,PLA     ,UVAR   ,
                OFF    ,NGL    ,SHF);
      }

}


/* LINUX */
void  eng_userlib_sigepsc_(int* ilaw ,
      int* NEL ,int*NUPARAM ,int*NUVAR ,int*NFUNC ,int*IFUNC ,
      int*NPF  ,int*NPT     ,int*IPT   ,int*IFLAG ,
      my_real *TF ,my_real *TIME ,my_real *TIMESTEP , my_real *UPARAM , my_real *RHO0   ,
      my_real *AREA   ,my_real *EINT   ,my_real *THKLY   ,
      my_real *EPSPXX ,my_real *EPSPYY ,my_real *EPSPXY  ,my_real *EPSPYZ  ,my_real *EPSPZX ,
      my_real *DEPSXX ,my_real *DEPSYY ,my_real *DEPSXY  ,my_real *DEPSYZ  ,my_real *DEPSZX ,
      my_real *EPSXX  ,my_real *EPSYY  ,my_real *EPSXY   ,my_real *EPSYZ   ,my_real *EPSZX  ,
      my_real *SIGOXX ,my_real *SIGOYY ,my_real *SIGOXY  ,my_real *SIGOYZ  ,my_real *SIGOZX ,
      my_real *SIGNXX ,my_real *SIGNYY ,my_real *SIGNXY  ,my_real *SIGNYZ  ,my_real *SIGNZX ,
      my_real *SIGVXX ,my_real *SIGVYY ,my_real *SIGVXY  ,my_real *SIGVYZ  ,my_real *SIGVZX ,
      my_real *SOUNDSP,my_real *VISCMAX,my_real *THK     ,my_real *PLA     ,my_real *UVAR   ,
      my_real *OFF    ,int *NGL    ,int *SHF){

      if(eng_sigepsc){
          (*eng_sigepsc)(ilaw ,
                  NEL    ,NUPARAM ,NUVAR    ,NFUNC  ,IFUNC ,
                NPF    ,NPT     ,IPT      ,IFLAG  ,
                TF     ,TIME    ,TIMESTEP ,UPARAM ,RHO0   ,
                AREA   ,EINT    ,THKLY    ,
                EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
                DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
                EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
                SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
                SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
                SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
                SOUNDSP,VISCMAX,THK     ,PLA     ,UVAR   ,
                OFF    ,NGL    ,SHF);
      }
}

/* --------------------------------- */
/* User Material law 1-99  SHELL     */
/* --------------------------------- */

/* ------------------- */
/* WINDOWS - SIGEPS99C */
/* ------------------- */
void _FCALL ENG_USERLIB_SIGEPS99C(int*NEL      ,int*NUPARAM ,int*NUVAR   ,int*ILAW_USER ,int*NFUNC  ,
         int*IFUNC    ,int *NPF     ,int*NGL    ,my_real *TF       ,my_real *TIME   ,
         my_real *TIMESTEP ,my_real *UPARAM  ,my_real *RHO    ,my_real *AREA      ,my_real *EINT   ,
         my_real *SHF       ,my_real *SOUNDSP ,my_real *VISCMAX ,my_real *PLA       ,my_real *UVAR   , 
         my_real *OFF       ,my_real *SIGY ){

       if (eng_sigeps99c){
           (eng_sigeps99c)( NEL ,NUPARAM ,NUVAR   ,ILAW_USER ,NFUNC  ,
         IFUNC    ,NPF     ,NGL    ,TF       ,TIME   ,
         TIMESTEP ,UPARAM  ,RHO    ,AREA     ,EINT   ,
         SHF      ,SOUNDSP ,VISCMAX,PLA      ,UVAR     , 
         OFF      ,SIGY   );
 }
}

/* ----------------- */
/* LINUX - SIGEPS99C */
/* ----------------- */
void eng_userlib_sigeps99c_(int*NEL      ,int*NUPARAM ,int*NUVAR   ,int*ILAW_USER ,int*NFUNC  ,
         int*IFUNC    ,int*NPF     ,int*NGL    ,my_real *TF       ,my_real *TIME   ,
         my_real *TIMESTEP ,my_real *UPARAM  ,my_real *RHO    ,my_real *AREA      ,my_real *EINT   ,
         my_real *SHF       ,my_real *SOUNDSP ,my_real *VISCMAX ,my_real *PLA       ,my_real *UVAR   , 
         my_real *OFF       ,my_real *SIGY ){

       if (eng_sigeps99c){
           (eng_sigeps99c)( NEL ,NUPARAM ,NUVAR   ,ILAW_USER ,NFUNC  ,
         IFUNC    ,NPF     ,NGL    ,TF       ,TIME   ,
         TIMESTEP ,UPARAM  ,RHO    ,AREA     ,EINT   ,
         SHF      ,SOUNDSP ,VISCMAX,PLA      ,UVAR     , 
         OFF      ,SIGY   );
 }
}


void  _FCALL ENG_USERLIB_GET_LAWC_VAR( int*NCYCLE, int*IMAT, int*ILAYER, int*NPTA, int*IFLAG,
                     my_real* R11,     my_real*R12,    my_real*R13,    my_real*R21,    my_real*R22,
                     my_real* R23,     my_real*R31,    my_real*R32,    my_real*R33,    my_real*SIGOXX,
                     my_real* SIGOYY, my_real*SIGOXY, my_real*SIGOYZ, my_real*SIGOZX, my_real*EPSPXX,
                     my_real* EPSPYY, my_real*EPSPXY, my_real*EPSPYZ, my_real*EPSPZX, my_real*EPSXX,
                     my_real* EPSYY,  my_real*EPSXY,  my_real*EPSYZ,  my_real*EPSZX,  my_real*DEPSXX,
                     my_real* DEPSYY, my_real*DEPSXY, my_real*DEPSYZ, my_real*DEPSZX, my_real*THKLYL,      
                     my_real* THKN,   my_real*SIGNXX, my_real*SIGNYY, my_real*SIGNXY, my_real*SIGNYZ,
                     my_real* SIGNZX, my_real*SIGVXX, my_real*SIGVYY, my_real*SIGVXY, my_real*SIGVYZ,
                     my_real* SIGVZX, my_real*DPLA ){
          if (eng_get_lawc_user_var){
                     (eng_get_lawc_user_var)( NCYCLE, IMAT, ILAYER, NPTA, IFLAG,
                      R11,     R12,    R13,    R21,    R22,
                      R23,     R31,    R32,    R33,    SIGOXX,
                      SIGOYY, SIGOXY, SIGOYZ, SIGOZX, EPSPXX,
                      EPSPYY, EPSPXY, EPSPYZ, EPSPZX, EPSXX,
                      EPSYY,  EPSXY,  EPSYZ,  EPSZX,  DEPSXX,
                      DEPSYY, DEPSXY, DEPSYZ, DEPSZX, THKLYL,      
                      THKN,   SIGNXX, SIGNYY, SIGNXY, SIGNYZ,
                      SIGNZX, SIGVXX, SIGVYY, SIGVXY, SIGVYZ,
                      SIGVZX, DPLA ); }
}


void  eng_userlib_get_lawc_var_( int*NCYCLE, int*IMAT, int*ILAYER, int*NPTA, int*IFLAG,
                     my_real* R11,     my_real*R12,    my_real*R13,    my_real*R21,    my_real*R22,
                     my_real* R23,     my_real*R31,    my_real*R32,    my_real*R33,    my_real*SIGOXX,
                     my_real* SIGOYY, my_real*SIGOXY, my_real*SIGOYZ, my_real*SIGOZX, my_real*EPSPXX,
                     my_real* EPSPYY, my_real*EPSPXY, my_real*EPSPYZ, my_real*EPSPZX, my_real*EPSXX,
                     my_real* EPSYY,  my_real*EPSXY,  my_real*EPSYZ,  my_real*EPSZX,  my_real*DEPSXX,
                     my_real* DEPSYY, my_real*DEPSXY, my_real*DEPSYZ, my_real*DEPSZX, my_real*THKLYL,      
                     my_real* THKN,   my_real*SIGNXX, my_real*SIGNYY, my_real*SIGNXY, my_real*SIGNYZ,
                     my_real* SIGNZX, my_real*SIGVXX, my_real*SIGVYY, my_real*SIGVXY, my_real*SIGVYZ,
                     my_real* SIGVZX, my_real*DPLA ){
          if (eng_get_lawc_user_var){
                     (eng_get_lawc_user_var)( NCYCLE, IMAT, ILAYER, NPTA, IFLAG,
                      R11,     R12,    R13,    R21,    R22,
                      R23,     R31,    R32,    R33,    SIGOXX,
                      SIGOYY, SIGOXY, SIGOYZ, SIGOZX, EPSPXX,
                      EPSPYY, EPSPXY, EPSPYZ, EPSPZX, EPSXX,
                      EPSYY,  EPSXY,  EPSYZ,  EPSZX,  DEPSXX,
                      DEPSYY, DEPSXY, DEPSYZ, DEPSZX, THKLYL,      
                      THKN,   SIGNXX, SIGNYY, SIGNXY, SIGNYZ,
                      SIGNZX, SIGVXX, SIGVYY, SIGVXY, SIGVYZ,
                      SIGVZX, DPLA ); }
}



void  eng_userlib_get_lawc_var_2_(my_real* VAR01,int*SIZVAR01,my_real* VAR02,int*SIZVAR02,
         my_real* VAR03,int*SIZVAR03,my_real* VAR04,int*SIZVAR04,my_real* VAR05,int*SIZVAR05,my_real* VAR06,int*SIZVAR06,
         my_real* VAR07,int*SIZVAR07,my_real* VAR08,int*SIZVAR08,my_real* VAR09,int*SIZVAR09,my_real* VAR10,int*SIZVAR10,
         my_real* VAR11,int*SIZVAR11,my_real* VAR12,int*SIZVAR12,my_real* VAR13,int*SIZVAR13,my_real* VAR14,int*SIZVAR14,
         my_real* VAR15,int*SIZVAR15,my_real* VAR16,int*SIZVAR16,my_real* VAR17,int*SIZVAR17,my_real* VAR18,int*SIZVAR18,    
         my_real* VAR19,int*SIZVAR19,my_real* VAR20,int*SIZVAR20,my_real* VAR21,int*SIZVAR21,my_real* VAR22,int*SIZVAR22,
         my_real* VAR23,int*SIZVAR23,my_real* VAR24,int*SIZVAR24,my_real* VAR25,int*SIZVAR25,my_real* VAR26,int*SIZVAR26,    
         my_real* VAR27,int*SIZVAR27,my_real* VAR28,int*SIZVAR28,my_real* VAR29,int*SIZVAR29,my_real* VAR30,int*SIZVAR30,
         my_real* VAR31,int*SIZVAR31,my_real* VAR32,int*SIZVAR32,my_real* VAR33,int*SIZVAR33,my_real* VAR34,int*SIZVAR34,    
         my_real* VAR35,int*SIZVAR35,my_real* VAR36,int*SIZVAR36,my_real* VAR37,int*SIZVAR37,my_real* VAR38,int*SIZVAR38,
         my_real* VAR39,int*SIZVAR39,my_real* VAR40,int*SIZVAR40,my_real* VAR41,int*SIZVAR41,my_real* VAR42,int*SIZVAR42,    
         my_real* VAR43,int*SIZVAR43,my_real* VAR44,int*SIZVAR44,my_real* VAR45,int*SIZVAR45,my_real* VAR46,int*SIZVAR46,
         my_real* VAR47,int*SIZVAR47,my_real* VAR48,int*SIZVAR48,my_real* VAR49,int*SIZVAR49,my_real* VAR50,int*SIZVAR50,    
         my_real* VAR51,int*SIZVAR51,my_real* VAR52,int*SIZVAR52,my_real* VAR53,int*SIZVAR53,my_real* VAR54,int*SIZVAR54,
         my_real* VAR55,int*SIZVAR55,my_real* VAR56,int*SIZVAR56,my_real* VAR57,int*SIZVAR57,my_real* VAR58,int*SIZVAR58,    
         my_real* VAR59,int*SIZVAR59,my_real* VAR60,int*SIZVAR60,my_real* VAR61,int*SIZVAR61,my_real* VAR62,int*SIZVAR62,
         my_real* VAR63,int*SIZVAR63,my_real* VAR64,int*SIZVAR64,my_real* VAR65,int*SIZVAR65,my_real* VAR66,int*SIZVAR66,    
         my_real* VAR67,int*SIZVAR67,my_real* VAR68,int*SIZVAR68,my_real* VAR69,int*SIZVAR69,my_real* VAR70,int*SIZVAR70,
         my_real* VAR71,int*SIZVAR72,my_real* VAR73,int*SIZVAR73,my_real* VAR74,int*SIZVAR74,my_real* VAR75,int*SIZVAR75,
         my_real* VAR76,int*SIZVAR76,my_real* VAR77,int*SIZVAR77,my_real* VAR78,int*SIZVAR78,my_real* VAR79,int*SIZVAR79,
         my_real* VAR80,int*SIZVAR80,my_real* VAR81,int*SIZVAR81,my_real* VAR82,int*SIZVAR82,my_real* VAR83,int*SIZVAR83,    
         my_real* VAR84,int*SIZVAR84,my_real* VAR85,int*SIZVAR85,my_real* VAR86,int*SIZVAR86,my_real* VAR87,int*SIZVAR87,
         my_real* VAR88,int*SIZVAR88,my_real* VAR89,int*SIZVAR89,my_real* VAR90,int*SIZVAR90,my_real* VAR91,int*SIZVAR91,    
         my_real* VAR92,int*SIZVAR92,my_real* VAR93,int*SIZVAR93,my_real* VAR94,int*SIZVAR94,my_real* VAR95,int*SIZVAR95,
         my_real* VAR96,int*SIZVAR96,my_real* VAR97,int*SIZVAR97,my_real* VAR98,int*SIZVAR98,my_real* VAR99,int*SIZVAR99){
         
         if (eng_get_lawc_user_var_2){
             (eng_get_lawc_user_var_2) ( VAR01, SIZVAR01, VAR02, SIZVAR02,
         VAR03,SIZVAR03,VAR04,SIZVAR04,VAR05,SIZVAR05,VAR06,SIZVAR06,
         VAR07,SIZVAR07,VAR08,SIZVAR08,VAR09,SIZVAR09,VAR10,SIZVAR10,
         VAR11,SIZVAR11,VAR12,SIZVAR12,VAR13,SIZVAR13,VAR14,SIZVAR14,
         VAR15,SIZVAR15,VAR16,SIZVAR16,VAR17,SIZVAR17,VAR18,SIZVAR18,    
         VAR19,SIZVAR19,VAR20,SIZVAR20,VAR21,SIZVAR21,VAR22,SIZVAR22,
         VAR23,SIZVAR23,VAR24,SIZVAR24,VAR25,SIZVAR25,VAR26,SIZVAR26,    
         VAR27,SIZVAR27,VAR28,SIZVAR28,VAR29,SIZVAR29,VAR30,SIZVAR30,
         VAR31,SIZVAR31,VAR32,SIZVAR32,VAR33,SIZVAR33,VAR34,SIZVAR34,    
         VAR35,SIZVAR35,VAR36,SIZVAR36,VAR37,SIZVAR37,VAR38,SIZVAR38,
         VAR39,SIZVAR39,VAR40,SIZVAR40,VAR41,SIZVAR41,VAR42,SIZVAR42,    
         VAR43,SIZVAR43,VAR44,SIZVAR44,VAR45,SIZVAR45,VAR46,SIZVAR46,
         VAR47,SIZVAR47,VAR48,SIZVAR48,VAR49,SIZVAR49,VAR50,SIZVAR50,    
         VAR51,SIZVAR51,VAR52,SIZVAR52,VAR53,SIZVAR53,VAR54,SIZVAR54,
         VAR55,SIZVAR55,VAR56,SIZVAR56,VAR57,SIZVAR57,VAR58,SIZVAR58,    
         VAR59,SIZVAR59,VAR60,SIZVAR60,VAR61,SIZVAR61,VAR62,SIZVAR62,
         VAR63,SIZVAR63,VAR64,SIZVAR64,VAR65,SIZVAR65,VAR66,SIZVAR66,    
         VAR67,SIZVAR67,VAR68,SIZVAR68,VAR69,SIZVAR69,VAR70,SIZVAR70,
         VAR71,SIZVAR72,VAR73,SIZVAR73,VAR74,SIZVAR74,VAR75,SIZVAR75,
         VAR76,SIZVAR76,VAR77,SIZVAR77,VAR78,SIZVAR78,VAR79,SIZVAR79,
         VAR80,SIZVAR80,VAR81,SIZVAR81,VAR82,SIZVAR82,VAR83,SIZVAR83,    
         VAR84,SIZVAR84,VAR85,SIZVAR85,VAR86,SIZVAR86,VAR87,SIZVAR87,
         VAR88,SIZVAR88,VAR89,SIZVAR89,VAR90,SIZVAR90,VAR91,SIZVAR91,    
         VAR92,SIZVAR92,VAR93,SIZVAR93,VAR94,SIZVAR94,VAR95,SIZVAR95,
         VAR96,SIZVAR96,VAR97,SIZVAR97,VAR98,SIZVAR98,VAR99,SIZVAR99);
         }
    }     


void _FCALL ENG_USERLIB_GET_LAWC_VAR_2(my_real* VAR01,int*SIZVAR01,my_real* VAR02,int*SIZVAR02,
         my_real* VAR03,int*SIZVAR03,my_real* VAR04,int*SIZVAR04,my_real* VAR05,int*SIZVAR05,my_real* VAR06,int*SIZVAR06,
         my_real* VAR07,int*SIZVAR07,my_real* VAR08,int*SIZVAR08,my_real* VAR09,int*SIZVAR09,my_real* VAR10,int*SIZVAR10,
         my_real* VAR11,int*SIZVAR11,my_real* VAR12,int*SIZVAR12,my_real* VAR13,int*SIZVAR13,my_real* VAR14,int*SIZVAR14,
         my_real* VAR15,int*SIZVAR15,my_real* VAR16,int*SIZVAR16,my_real* VAR17,int*SIZVAR17,my_real* VAR18,int*SIZVAR18,    
         my_real* VAR19,int*SIZVAR19,my_real* VAR20,int*SIZVAR20,my_real* VAR21,int*SIZVAR21,my_real* VAR22,int*SIZVAR22,
         my_real* VAR23,int*SIZVAR23,my_real* VAR24,int*SIZVAR24,my_real* VAR25,int*SIZVAR25,my_real* VAR26,int*SIZVAR26,    
         my_real* VAR27,int*SIZVAR27,my_real* VAR28,int*SIZVAR28,my_real* VAR29,int*SIZVAR29,my_real* VAR30,int*SIZVAR30,
         my_real* VAR31,int*SIZVAR31,my_real* VAR32,int*SIZVAR32,my_real* VAR33,int*SIZVAR33,my_real* VAR34,int*SIZVAR34,    
         my_real* VAR35,int*SIZVAR35,my_real* VAR36,int*SIZVAR36,my_real* VAR37,int*SIZVAR37,my_real* VAR38,int*SIZVAR38,
         my_real* VAR39,int*SIZVAR39,my_real* VAR40,int*SIZVAR40,my_real* VAR41,int*SIZVAR41,my_real* VAR42,int*SIZVAR42,    
         my_real* VAR43,int*SIZVAR43,my_real* VAR44,int*SIZVAR44,my_real* VAR45,int*SIZVAR45,my_real* VAR46,int*SIZVAR46,
         my_real* VAR47,int*SIZVAR47,my_real* VAR48,int*SIZVAR48,my_real* VAR49,int*SIZVAR49,my_real* VAR50,int*SIZVAR50,    
         my_real* VAR51,int*SIZVAR51,my_real* VAR52,int*SIZVAR52,my_real* VAR53,int*SIZVAR53,my_real* VAR54,int*SIZVAR54,
         my_real* VAR55,int*SIZVAR55,my_real* VAR56,int*SIZVAR56,my_real* VAR57,int*SIZVAR57,my_real* VAR58,int*SIZVAR58,    
         my_real* VAR59,int*SIZVAR59,my_real* VAR60,int*SIZVAR60,my_real* VAR61,int*SIZVAR61,my_real* VAR62,int*SIZVAR62,
         my_real* VAR63,int*SIZVAR63,my_real* VAR64,int*SIZVAR64,my_real* VAR65,int*SIZVAR65,my_real* VAR66,int*SIZVAR66,    
         my_real* VAR67,int*SIZVAR67,my_real* VAR68,int*SIZVAR68,my_real* VAR69,int*SIZVAR69,my_real* VAR70,int*SIZVAR70,
         my_real* VAR71,int*SIZVAR72,my_real* VAR73,int*SIZVAR73,my_real* VAR74,int*SIZVAR74,my_real* VAR75,int*SIZVAR75,
         my_real* VAR76,int*SIZVAR76,my_real* VAR77,int*SIZVAR77,my_real* VAR78,int*SIZVAR78,my_real* VAR79,int*SIZVAR79,
         my_real* VAR80,int*SIZVAR80,my_real* VAR81,int*SIZVAR81,my_real* VAR82,int*SIZVAR82,my_real* VAR83,int*SIZVAR83,    
         my_real* VAR84,int*SIZVAR84,my_real* VAR85,int*SIZVAR85,my_real* VAR86,int*SIZVAR86,my_real* VAR87,int*SIZVAR87,
         my_real* VAR88,int*SIZVAR88,my_real* VAR89,int*SIZVAR89,my_real* VAR90,int*SIZVAR90,my_real* VAR91,int*SIZVAR91,    
         my_real* VAR92,int*SIZVAR92,my_real* VAR93,int*SIZVAR93,my_real* VAR94,int*SIZVAR94,my_real* VAR95,int*SIZVAR95,
         my_real* VAR96,int*SIZVAR96,my_real* VAR97,int*SIZVAR97,my_real* VAR98,int*SIZVAR98,my_real* VAR99,int*SIZVAR99){
         
         if (eng_get_lawc_user_var_2){
             (eng_get_lawc_user_var_2) ( VAR01, SIZVAR01, VAR02, SIZVAR02,
         VAR03,SIZVAR03,VAR04,SIZVAR04,VAR05,SIZVAR05,VAR06,SIZVAR06,
         VAR07,SIZVAR07,VAR08,SIZVAR08,VAR09,SIZVAR09,VAR10,SIZVAR10,
         VAR11,SIZVAR11,VAR12,SIZVAR12,VAR13,SIZVAR13,VAR14,SIZVAR14,
         VAR15,SIZVAR15,VAR16,SIZVAR16,VAR17,SIZVAR17,VAR18,SIZVAR18,    
         VAR19,SIZVAR19,VAR20,SIZVAR20,VAR21,SIZVAR21,VAR22,SIZVAR22,
         VAR23,SIZVAR23,VAR24,SIZVAR24,VAR25,SIZVAR25,VAR26,SIZVAR26,    
         VAR27,SIZVAR27,VAR28,SIZVAR28,VAR29,SIZVAR29,VAR30,SIZVAR30,
         VAR31,SIZVAR31,VAR32,SIZVAR32,VAR33,SIZVAR33,VAR34,SIZVAR34,    
         VAR35,SIZVAR35,VAR36,SIZVAR36,VAR37,SIZVAR37,VAR38,SIZVAR38,
         VAR39,SIZVAR39,VAR40,SIZVAR40,VAR41,SIZVAR41,VAR42,SIZVAR42,    
         VAR43,SIZVAR43,VAR44,SIZVAR44,VAR45,SIZVAR45,VAR46,SIZVAR46,
         VAR47,SIZVAR47,VAR48,SIZVAR48,VAR49,SIZVAR49,VAR50,SIZVAR50,    
         VAR51,SIZVAR51,VAR52,SIZVAR52,VAR53,SIZVAR53,VAR54,SIZVAR54,
         VAR55,SIZVAR55,VAR56,SIZVAR56,VAR57,SIZVAR57,VAR58,SIZVAR58,    
         VAR59,SIZVAR59,VAR60,SIZVAR60,VAR61,SIZVAR61,VAR62,SIZVAR62,
         VAR63,SIZVAR63,VAR64,SIZVAR64,VAR65,SIZVAR65,VAR66,SIZVAR66,    
         VAR67,SIZVAR67,VAR68,SIZVAR68,VAR69,SIZVAR69,VAR70,SIZVAR70,
         VAR71,SIZVAR72,VAR73,SIZVAR73,VAR74,SIZVAR74,VAR75,SIZVAR75,
         VAR76,SIZVAR76,VAR77,SIZVAR77,VAR78,SIZVAR78,VAR79,SIZVAR79,
         VAR80,SIZVAR80,VAR81,SIZVAR81,VAR82,SIZVAR82,VAR83,SIZVAR83,    
         VAR84,SIZVAR84,VAR85,SIZVAR85,VAR86,SIZVAR86,VAR87,SIZVAR87,
         VAR88,SIZVAR88,VAR89,SIZVAR89,VAR90,SIZVAR90,VAR91,SIZVAR91,    
         VAR92,SIZVAR92,VAR93,SIZVAR93,VAR94,SIZVAR94,VAR95,SIZVAR95,
         VAR96,SIZVAR96,VAR97,SIZVAR97,VAR98,SIZVAR98,VAR99,SIZVAR99);
         }
    }     

void  _FCALL ENG_USERLIB_SET_LAWC( my_real *SIGNXX,  my_real *SIGNYY, my_real *SIGNXY, my_real *SIGNYZ,  my_real *SIGNZX,
                     my_real *SIGVXX, my_real *SIGVYY, my_real *SIGVXY, my_real *SIGVYZ,   my_real *SIGVZX,
                     my_real* DPLA,   my_real *ETSE,   my_real* THKN ){
          if (eng_set_lawc_user_var){
                     (eng_set_lawc_user_var)(  SIGNXX,  SIGNYY, SIGNXY, SIGNYZ,  SIGNZX,
                                    SIGVXX,  SIGVYY, SIGVXY, SIGVYZ,  SIGVZX,
                                    DPLA,    ETSE,    THKN  ); }
}

void  eng_userlib_set_lawc_ (   my_real*SIGNXX,  my_real*SIGNYY, my_real*SIGNXY, my_real* SIGNYZ,  my_real*SIGNZX,
                     my_real* SIGVXX, my_real*SIGVYY, my_real*SIGVXY, my_real*SIGVYZ,   my_real*SIGVZX,
                     my_real* DPLA,   my_real*ETSE,   my_real* THKN ){
          if (eng_set_lawc_user_var){
                     (eng_set_lawc_user_var)(  SIGNXX,  SIGNYY, SIGNXY, SIGNYZ,  SIGNZX,
                                    SIGVXX,  SIGVYY, SIGVXY, SIGVYZ,  SIGVZX,
                                    DPLA,    ETSE,   THKN  ); }
}

void  _FCALL ENG_USERLIB_SET_LAWC_VAR_2(my_real* VAR01,int*SIZVAR01,my_real* VAR02,int*SIZVAR02,
         my_real* VAR03,int*SIZVAR03,my_real* VAR04,int*SIZVAR04,my_real* VAR05,int*SIZVAR05,my_real* VAR06,int*SIZVAR06,
         my_real* VAR07,int*SIZVAR07,my_real* VAR08,int*SIZVAR08,my_real* VAR09,int*SIZVAR09,my_real* VAR10,int*SIZVAR10,
         my_real* VAR11,int*SIZVAR11,my_real* VAR12,int*SIZVAR12,my_real* VAR13,int*SIZVAR13,my_real* VAR14,int*SIZVAR14,
         my_real* VAR15,int*SIZVAR15,my_real* VAR16,int*SIZVAR16,my_real* VAR17,int*SIZVAR17,my_real* VAR18,int*SIZVAR18,    
         my_real* VAR19,int*SIZVAR19,my_real* VAR20,int*SIZVAR20,my_real* VAR21,int*SIZVAR21,my_real* VAR22,int*SIZVAR22,
         my_real* VAR23,int*SIZVAR23,my_real* VAR24,int*SIZVAR24,my_real* VAR25,int*SIZVAR25,my_real* VAR26,int*SIZVAR26,    
         my_real* VAR27,int*SIZVAR27,my_real* VAR28,int*SIZVAR28,my_real* VAR29,int*SIZVAR29,my_real* VAR30,int*SIZVAR30,
         my_real* VAR31,int*SIZVAR31,my_real* VAR32,int*SIZVAR32,my_real* VAR33,int*SIZVAR33,my_real* VAR34,int*SIZVAR34,    
         my_real* VAR35,int*SIZVAR35,my_real* VAR36,int*SIZVAR36,my_real* VAR37,int*SIZVAR37,my_real* VAR38,int*SIZVAR38,
         my_real* VAR39,int*SIZVAR39,my_real* VAR40,int*SIZVAR40,my_real* VAR41,int*SIZVAR41,my_real* VAR42,int*SIZVAR42,    
         my_real* VAR43,int*SIZVAR43,my_real* VAR44,int*SIZVAR44,my_real* VAR45,int*SIZVAR45,my_real* VAR46,int*SIZVAR46,
         my_real* VAR47,int*SIZVAR47,my_real* VAR48,int*SIZVAR48,my_real* VAR49,int*SIZVAR49,my_real* VAR50,int*SIZVAR50,    
         my_real* VAR51,int*SIZVAR51,my_real* VAR52,int*SIZVAR52,my_real* VAR53,int*SIZVAR53,my_real* VAR54,int*SIZVAR54,
         my_real* VAR55,int*SIZVAR55,my_real* VAR56,int*SIZVAR56,my_real* VAR57,int*SIZVAR57,my_real* VAR58,int*SIZVAR58,    
         my_real* VAR59,int*SIZVAR59,my_real* VAR60,int*SIZVAR60,my_real* VAR61,int*SIZVAR61,my_real* VAR62,int*SIZVAR62,
         my_real* VAR63,int*SIZVAR63,my_real* VAR64,int*SIZVAR64,my_real* VAR65,int*SIZVAR65,my_real* VAR66,int*SIZVAR66,    
         my_real* VAR67,int*SIZVAR67,my_real* VAR68,int*SIZVAR68,my_real* VAR69,int*SIZVAR69,my_real* VAR70,int*SIZVAR70,
         my_real* VAR71,int*SIZVAR72,my_real* VAR73,int*SIZVAR73,my_real* VAR74,int*SIZVAR74,my_real* VAR75,int*SIZVAR75,
         my_real* VAR76,int*SIZVAR76,my_real* VAR77,int*SIZVAR77,my_real* VAR78,int*SIZVAR78,my_real* VAR79,int*SIZVAR79,
         my_real* VAR80,int*SIZVAR80,my_real* VAR81,int*SIZVAR81,my_real* VAR82,int*SIZVAR82,my_real* VAR83,int*SIZVAR83,    
         my_real* VAR84,int*SIZVAR84,my_real* VAR85,int*SIZVAR85,my_real* VAR86,int*SIZVAR86,my_real* VAR87,int*SIZVAR87,
         my_real* VAR88,int*SIZVAR88,my_real* VAR89,int*SIZVAR89,my_real* VAR90,int*SIZVAR90,my_real* VAR91,int*SIZVAR91,    
         my_real* VAR92,int*SIZVAR92,my_real* VAR93,int*SIZVAR93,my_real* VAR94,int*SIZVAR94,my_real* VAR95,int*SIZVAR95,
         my_real* VAR96,int*SIZVAR96,my_real* VAR97,int*SIZVAR97,my_real* VAR98,int*SIZVAR98,my_real* VAR99,int*SIZVAR99){
         
         if (eng_set_lawc_user_var_2){
             (eng_set_lawc_user_var_2) ( VAR01, SIZVAR01, VAR02, SIZVAR02,
         VAR03,SIZVAR03,VAR04,SIZVAR04,VAR05,SIZVAR05,VAR06,SIZVAR06,
         VAR07,SIZVAR07,VAR08,SIZVAR08,VAR09,SIZVAR09,VAR10,SIZVAR10,
         VAR11,SIZVAR11,VAR12,SIZVAR12,VAR13,SIZVAR13,VAR14,SIZVAR14,
         VAR15,SIZVAR15,VAR16,SIZVAR16,VAR17,SIZVAR17,VAR18,SIZVAR18,    
         VAR19,SIZVAR19,VAR20,SIZVAR20,VAR21,SIZVAR21,VAR22,SIZVAR22,
         VAR23,SIZVAR23,VAR24,SIZVAR24,VAR25,SIZVAR25,VAR26,SIZVAR26,    
         VAR27,SIZVAR27,VAR28,SIZVAR28,VAR29,SIZVAR29,VAR30,SIZVAR30,
         VAR31,SIZVAR31,VAR32,SIZVAR32,VAR33,SIZVAR33,VAR34,SIZVAR34,    
         VAR35,SIZVAR35,VAR36,SIZVAR36,VAR37,SIZVAR37,VAR38,SIZVAR38,
         VAR39,SIZVAR39,VAR40,SIZVAR40,VAR41,SIZVAR41,VAR42,SIZVAR42,    
         VAR43,SIZVAR43,VAR44,SIZVAR44,VAR45,SIZVAR45,VAR46,SIZVAR46,
         VAR47,SIZVAR47,VAR48,SIZVAR48,VAR49,SIZVAR49,VAR50,SIZVAR50,    
         VAR51,SIZVAR51,VAR52,SIZVAR52,VAR53,SIZVAR53,VAR54,SIZVAR54,
         VAR55,SIZVAR55,VAR56,SIZVAR56,VAR57,SIZVAR57,VAR58,SIZVAR58,    
         VAR59,SIZVAR59,VAR60,SIZVAR60,VAR61,SIZVAR61,VAR62,SIZVAR62,
         VAR63,SIZVAR63,VAR64,SIZVAR64,VAR65,SIZVAR65,VAR66,SIZVAR66,    
         VAR67,SIZVAR67,VAR68,SIZVAR68,VAR69,SIZVAR69,VAR70,SIZVAR70,
         VAR71,SIZVAR72,VAR73,SIZVAR73,VAR74,SIZVAR74,VAR75,SIZVAR75,
         VAR76,SIZVAR76,VAR77,SIZVAR77,VAR78,SIZVAR78,VAR79,SIZVAR79,
         VAR80,SIZVAR80,VAR81,SIZVAR81,VAR82,SIZVAR82,VAR83,SIZVAR83,    
         VAR84,SIZVAR84,VAR85,SIZVAR85,VAR86,SIZVAR86,VAR87,SIZVAR87,
         VAR88,SIZVAR88,VAR89,SIZVAR89,VAR90,SIZVAR90,VAR91,SIZVAR91,    
         VAR92,SIZVAR92,VAR93,SIZVAR93,VAR94,SIZVAR94,VAR95,SIZVAR95,
         VAR96,SIZVAR96,VAR97,SIZVAR97,VAR98,SIZVAR98,VAR99,SIZVAR99);
         }
    }     


void  eng_userlib_set_lawc_var_2_(my_real* VAR01,int*SIZVAR01,my_real* VAR02,int*SIZVAR02,
         my_real* VAR03,int*SIZVAR03,my_real* VAR04,int*SIZVAR04,my_real* VAR05,int*SIZVAR05,my_real* VAR06,int*SIZVAR06,
         my_real* VAR07,int*SIZVAR07,my_real* VAR08,int*SIZVAR08,my_real* VAR09,int*SIZVAR09,my_real* VAR10,int*SIZVAR10,
         my_real* VAR11,int*SIZVAR11,my_real* VAR12,int*SIZVAR12,my_real* VAR13,int*SIZVAR13,my_real* VAR14,int*SIZVAR14,
         my_real* VAR15,int*SIZVAR15,my_real* VAR16,int*SIZVAR16,my_real* VAR17,int*SIZVAR17,my_real* VAR18,int*SIZVAR18,    
         my_real* VAR19,int*SIZVAR19,my_real* VAR20,int*SIZVAR20,my_real* VAR21,int*SIZVAR21,my_real* VAR22,int*SIZVAR22,
         my_real* VAR23,int*SIZVAR23,my_real* VAR24,int*SIZVAR24,my_real* VAR25,int*SIZVAR25,my_real* VAR26,int*SIZVAR26,    
         my_real* VAR27,int*SIZVAR27,my_real* VAR28,int*SIZVAR28,my_real* VAR29,int*SIZVAR29,my_real* VAR30,int*SIZVAR30,
         my_real* VAR31,int*SIZVAR31,my_real* VAR32,int*SIZVAR32,my_real* VAR33,int*SIZVAR33,my_real* VAR34,int*SIZVAR34,    
         my_real* VAR35,int*SIZVAR35,my_real* VAR36,int*SIZVAR36,my_real* VAR37,int*SIZVAR37,my_real* VAR38,int*SIZVAR38,
         my_real* VAR39,int*SIZVAR39,my_real* VAR40,int*SIZVAR40,my_real* VAR41,int*SIZVAR41,my_real* VAR42,int*SIZVAR42,    
         my_real* VAR43,int*SIZVAR43,my_real* VAR44,int*SIZVAR44,my_real* VAR45,int*SIZVAR45,my_real* VAR46,int*SIZVAR46,
         my_real* VAR47,int*SIZVAR47,my_real* VAR48,int*SIZVAR48,my_real* VAR49,int*SIZVAR49,my_real* VAR50,int*SIZVAR50,    
         my_real* VAR51,int*SIZVAR51,my_real* VAR52,int*SIZVAR52,my_real* VAR53,int*SIZVAR53,my_real* VAR54,int*SIZVAR54,
         my_real* VAR55,int*SIZVAR55,my_real* VAR56,int*SIZVAR56,my_real* VAR57,int*SIZVAR57,my_real* VAR58,int*SIZVAR58,    
         my_real* VAR59,int*SIZVAR59,my_real* VAR60,int*SIZVAR60,my_real* VAR61,int*SIZVAR61,my_real* VAR62,int*SIZVAR62,
         my_real* VAR63,int*SIZVAR63,my_real* VAR64,int*SIZVAR64,my_real* VAR65,int*SIZVAR65,my_real* VAR66,int*SIZVAR66,    
         my_real* VAR67,int*SIZVAR67,my_real* VAR68,int*SIZVAR68,my_real* VAR69,int*SIZVAR69,my_real* VAR70,int*SIZVAR70,
         my_real* VAR71,int*SIZVAR72,my_real* VAR73,int*SIZVAR73,my_real* VAR74,int*SIZVAR74,my_real* VAR75,int*SIZVAR75,
         my_real* VAR76,int*SIZVAR76,my_real* VAR77,int*SIZVAR77,my_real* VAR78,int*SIZVAR78,my_real* VAR79,int*SIZVAR79,
         my_real* VAR80,int*SIZVAR80,my_real* VAR81,int*SIZVAR81,my_real* VAR82,int*SIZVAR82,my_real* VAR83,int*SIZVAR83,    
         my_real* VAR84,int*SIZVAR84,my_real* VAR85,int*SIZVAR85,my_real* VAR86,int*SIZVAR86,my_real* VAR87,int*SIZVAR87,
         my_real* VAR88,int*SIZVAR88,my_real* VAR89,int*SIZVAR89,my_real* VAR90,int*SIZVAR90,my_real* VAR91,int*SIZVAR91,    
         my_real* VAR92,int*SIZVAR92,my_real* VAR93,int*SIZVAR93,my_real* VAR94,int*SIZVAR94,my_real* VAR95,int*SIZVAR95,
         my_real* VAR96,int*SIZVAR96,my_real* VAR97,int*SIZVAR97,my_real* VAR98,int*SIZVAR98,my_real* VAR99,int*SIZVAR99){
         
         if (eng_set_lawc_user_var_2){
             (eng_set_lawc_user_var_2) ( VAR01, SIZVAR01, VAR02, SIZVAR02,
         VAR03,SIZVAR03,VAR04,SIZVAR04,VAR05,SIZVAR05,VAR06,SIZVAR06,
         VAR07,SIZVAR07,VAR08,SIZVAR08,VAR09,SIZVAR09,VAR10,SIZVAR10,
         VAR11,SIZVAR11,VAR12,SIZVAR12,VAR13,SIZVAR13,VAR14,SIZVAR14,
         VAR15,SIZVAR15,VAR16,SIZVAR16,VAR17,SIZVAR17,VAR18,SIZVAR18,    
         VAR19,SIZVAR19,VAR20,SIZVAR20,VAR21,SIZVAR21,VAR22,SIZVAR22,
         VAR23,SIZVAR23,VAR24,SIZVAR24,VAR25,SIZVAR25,VAR26,SIZVAR26,    
         VAR27,SIZVAR27,VAR28,SIZVAR28,VAR29,SIZVAR29,VAR30,SIZVAR30,
         VAR31,SIZVAR31,VAR32,SIZVAR32,VAR33,SIZVAR33,VAR34,SIZVAR34,    
         VAR35,SIZVAR35,VAR36,SIZVAR36,VAR37,SIZVAR37,VAR38,SIZVAR38,
         VAR39,SIZVAR39,VAR40,SIZVAR40,VAR41,SIZVAR41,VAR42,SIZVAR42,    
         VAR43,SIZVAR43,VAR44,SIZVAR44,VAR45,SIZVAR45,VAR46,SIZVAR46,
         VAR47,SIZVAR47,VAR48,SIZVAR48,VAR49,SIZVAR49,VAR50,SIZVAR50,    
         VAR51,SIZVAR51,VAR52,SIZVAR52,VAR53,SIZVAR53,VAR54,SIZVAR54,
         VAR55,SIZVAR55,VAR56,SIZVAR56,VAR57,SIZVAR57,VAR58,SIZVAR58,    
         VAR59,SIZVAR59,VAR60,SIZVAR60,VAR61,SIZVAR61,VAR62,SIZVAR62,
         VAR63,SIZVAR63,VAR64,SIZVAR64,VAR65,SIZVAR65,VAR66,SIZVAR66,    
         VAR67,SIZVAR67,VAR68,SIZVAR68,VAR69,SIZVAR69,VAR70,SIZVAR70,
         VAR71,SIZVAR72,VAR73,SIZVAR73,VAR74,SIZVAR74,VAR75,SIZVAR75,
         VAR76,SIZVAR76,VAR77,SIZVAR77,VAR78,SIZVAR78,VAR79,SIZVAR79,
         VAR80,SIZVAR80,VAR81,SIZVAR81,VAR82,SIZVAR82,VAR83,SIZVAR83,    
         VAR84,SIZVAR84,VAR85,SIZVAR85,VAR86,SIZVAR86,VAR87,SIZVAR87,
         VAR88,SIZVAR88,VAR89,SIZVAR89,VAR90,SIZVAR90,VAR91,SIZVAR91,    
         VAR92,SIZVAR92,VAR93,SIZVAR93,VAR94,SIZVAR94,VAR95,SIZVAR95,
         VAR96,SIZVAR96,VAR97,SIZVAR97,VAR98,SIZVAR98,VAR99,SIZVAR99);
         }
    }     
   
    
/* --------------------------------- */
/* User Material law 1-99  SOLID     */
/* --------------------------------- */

/* ------------------- */
/* WINDOWS - SIGEPS99 */
/* ------------------- */
void _FCALL ENG_USERLIB_SIGEPS99( int*NEL     ,int*NUPARAM     ,int*NUVAR      ,int*ILAW_USER  ,int*NFUNC   ,
         int*IFUNC       ,int*NPF      ,my_real*TF      ,my_real*TIME   ,my_real*TIMESTEP,
         my_real*UPARAM  ,my_real*RHO      ,my_real*VOLUME ,my_real*EINT   ,int*NGL    ,
         my_real*SOUNDSP ,my_real*VISCMAX ,my_real*UVAR   ,my_real*OFF      ,my_real*SIGY   ,
         my_real*PLA ){

       if (eng_sigeps99){
           (eng_sigeps99)( NEL     ,NUPARAM     ,NUVAR      ,ILAW_USER  ,NFUNC   ,
                           IFUNC  ,NPF          ,TF         ,TIME   ,TIMESTEP,
                        UPARAM  ,RHO      ,VOLUME ,EINT   ,NGL    ,
                         SOUNDSP ,VISCMAX ,UVAR   ,OFF      ,SIGY   ,
                           PLA  );
 }
}
/* ----------------- */
/* LINUX - SIGEPS99C */
/* ----------------- */
void  eng_userlib_sigeps99_( int*NEL     ,int*NUPARAM     ,int*NUVAR      ,int*ILAW_USER  ,int*NFUNC   ,
         int*IFUNC       ,int*NPF      ,my_real*TF      ,my_real*TIME   ,my_real*TIMESTEP,
         my_real*UPARAM  ,my_real*RHO      ,my_real*VOLUME ,my_real*EINT   ,int*NGL    ,
         my_real*SOUNDSP ,my_real*VISCMAX ,my_real*UVAR   ,my_real*OFF      ,my_real*SIGY   ,
         my_real*PLA ){

       if (eng_sigeps99){
           (eng_sigeps99)( NEL     ,NUPARAM     ,NUVAR      ,ILAW_USER  ,NFUNC   ,
                           IFUNC  ,NPF          ,TF         ,TIME   ,TIMESTEP,
                        UPARAM  ,RHO      ,VOLUME ,EINT   ,NGL    ,
                         SOUNDSP ,VISCMAX ,UVAR   ,OFF      ,SIGY   ,
                           PLA  );
 }
}

/* ------------------------------ */
/* WINDOWS - eng_get_law_user_var */
/* ------------------------------ */
void _FCALL ENG_USERLIB_GET_LAW_VAR(  int*NCYCLE, int*IMAT, int*IPTR, int*IPTS, int*IPTT,
                               my_real*R11,my_real*R12, my_real*R13, my_real*R21, my_real*R22, my_real*R23,  my_real*R31,
                               my_real*R32,my_real*R33, my_real*SO1, my_real*SO2, my_real*SO3, my_real*SO4,  my_real*SO5,
                               my_real*SO6, my_real*EP1, my_real*EP2, my_real*EP3, my_real*EP4, my_real*EP5,  my_real*EP6,
                               my_real*ES1, my_real*ES2, my_real*ES3, my_real*ES4, my_real*ES5, my_real*ES6,  my_real*DE1,
                               my_real*DE2, my_real*DE3, my_real*DE4, my_real*DE5, my_real*DE6, my_real*RHO0, my_real*S1,
                               my_real*S2,  my_real*S3,  my_real*S4,  my_real*S5,  my_real*S6,  my_real*SV1,  my_real*SV2,
                       my_real*SV3, my_real*SV4, my_real*SV5, my_real*SV6 ){
              if (eng_get_law_user_var){
                 (eng_get_law_user_var)( NCYCLE, IMAT,IPTR, IPTS,IPTT,
                               R11,R12, R13, R21, R22, R23,  R31,
                               R32,R33, SO1, SO2, SO3, SO4,  SO5,
                               SO6, EP1, EP2, EP3, EP4, EP5,  EP6,
                               ES1, ES2, ES3, ES4, ES5, ES6,  DE1,
                               DE2, DE3, DE4, DE5, DE6, RHO0, S1,
                               S2,  S3,  S4,  S5,  S6,  SV1,  SV2,
                       SV3, SV4, SV5, SV6 );
                               }

}

/* ---------------------------- */
/* LINUX - eng_get_law_user_var */
/* ---------------------------- */
void eng_userlib_get_law_var_(  int*NCYCLE, int*IMAT, int*IPTR, int*IPTS, int*IPTT,
                        my_real*R11,my_real*R12, my_real*R13, my_real*R21, my_real*R22, my_real*R23,  my_real*R31,
                        my_real*R32,my_real*R33, my_real*SO1, my_real*SO2, my_real*SO3, my_real*SO4,  my_real*SO5,
                        my_real*SO6, my_real*EP1, my_real*EP2, my_real*EP3, my_real*EP4, my_real*EP5,  my_real*EP6,
                        my_real*ES1, my_real*ES2, my_real*ES3, my_real*ES4, my_real*ES5, my_real*ES6,  my_real*DE1,
                        my_real*DE2, my_real*DE3, my_real*DE4, my_real*DE5, my_real*DE6, my_real*RHO0, my_real*S1,
                        my_real*S2,  my_real*S3,  my_real*S4,  my_real*S5,  my_real*S6,  my_real*SV1,  my_real*SV2,
                my_real*SV3, my_real*SV4, my_real*SV5, my_real*SV6 ){

              if (eng_get_law_user_var){
                 (eng_get_law_user_var)( NCYCLE, IMAT,IPTR, IPTS,IPTT,
                               R11,R12, R13, R21, R22, R23,  R31,
                               R32,R33, SO1, SO2, SO3, SO4,  SO5,
                               SO6, EP1, EP2, EP3, EP4, EP5,  EP6,
                               ES1, ES2, ES3, ES4, ES5, ES6,  DE1,
                               DE2, DE3, DE4, DE5, DE6, RHO0, S1,
                               S2,  S3,  S4,  S5,  S6,  SV1,  SV2,
                       SV3, SV4, SV5, SV6 );
                               }

}
/* ---------------------------- */
/* LINUX - eng_userlib_get_law_user_var_2 */
/* ---------------------------- */
void eng_userlib_get_law_var_2_ ( my_real*FPSXX,int*SIZFPSXX,my_real*FPSYY,int *SIZFPSYY,
         my_real*FPSZZ,int *SIZFPSZZ,my_real*FPSXY,int *SIZFPSXY,my_real*FPSYZ,int *SIZFPSYZ,my_real*FPSZX,int *SIZFPSZX,
         my_real*FPSYX,int *SIZFPSYX,my_real*FPSZY,int *SIZFPSZY,my_real*FPSXZ,int *SIZFPSXZ,my_real*VAR10,int *SIZVAR10,
         my_real*VAR11,int *SIZVAR11,my_real*VAR12,int *SIZVAR12,my_real*VAR13,int *SIZVAR13,my_real*VAR14,int *SIZVAR14,
         my_real*VAR15,int *SIZVAR15,my_real*VAR16,int *SIZVAR16,my_real*VAR17,int *SIZVAR17,my_real*VAR18,int *SIZVAR18,    
         my_real*VAR19,int *SIZVAR19,my_real*VAR20,int *SIZVAR20,my_real*VAR21,int *SIZVAR21,my_real*VAR22,int *SIZVAR22,
         my_real*VAR23,int *SIZVAR23,my_real*VAR24,int *SIZVAR24,my_real*VAR25,int *SIZVAR25,my_real*VAR26,int *SIZVAR26,    
         my_real*VAR27,int *SIZVAR27,my_real*VAR28,int *SIZVAR28,my_real*VAR29,int *SIZVAR29,my_real*VAR30,int *SIZVAR30,
         my_real*VAR31,int *SIZVAR31,my_real*VAR32,int *SIZVAR32,my_real*VAR33,int *SIZVAR33,my_real*VAR34,int *SIZVAR34,    
         my_real*VAR35,int *SIZVAR35,my_real*VAR36,int *SIZVAR36,my_real*VAR37,int *SIZVAR37,my_real*VAR38,int *SIZVAR38,
         my_real*VAR39,int *SIZVAR39,my_real*VAR40,int *SIZVAR40,my_real*VAR41,int *SIZVAR41,my_real*VAR42,int *SIZVAR42,    
         my_real*VAR43,int *SIZVAR43,my_real*VAR44,int *SIZVAR44,my_real*VAR45,int *SIZVAR45,my_real*VAR46,int *SIZVAR46,
         my_real*VAR47,int *SIZVAR47,my_real*VAR48,int *SIZVAR48,my_real*VAR49,int *SIZVAR49,my_real*VAR50,int *SIZVAR50,    
         my_real*VAR51,int *SIZVAR51,my_real*VAR52,int *SIZVAR52,my_real*VAR53,int *SIZVAR53,my_real*VAR54,int *SIZVAR54,
         my_real*VAR55,int *SIZVAR55,my_real*VAR56,int *SIZVAR56,my_real*VAR57,int *SIZVAR57,my_real*VAR58,int *SIZVAR58,    
         my_real*VAR59,int *SIZVAR59,my_real*VAR60,int *SIZVAR60,my_real*VAR61,int *SIZVAR61,my_real*VAR62,int *SIZVAR62,
         my_real*VAR63,int *SIZVAR63,my_real*VAR64,int *SIZVAR64,my_real*VAR65,int *SIZVAR65,my_real*VAR66,int *SIZVAR66,    
         my_real*VAR67,int *SIZVAR67,my_real*VAR68,int *SIZVAR68,my_real*VAR69,int *SIZVAR69,my_real*VAR70,int *SIZVAR70,
         my_real*VAR71,int *SIZVAR72,my_real*VAR73,int *SIZVAR73,my_real*VAR74,int *SIZVAR74,my_real*VAR75,int *SIZVAR75,
         my_real*VAR76,int *SIZVAR76,my_real*VAR77,int *SIZVAR77,my_real*VAR78,int *SIZVAR78,my_real*VAR79,int *SIZVAR79,
         my_real*VAR80,int *SIZVAR80,my_real*VAR81,int *SIZVAR81,my_real*VAR82,int *SIZVAR82,my_real*VAR83,int *SIZVAR83,    
         my_real*VAR84,int *SIZVAR84,my_real*VAR85,int *SIZVAR85,my_real*VAR86,int *SIZVAR86,my_real*VAR87,int *SIZVAR87,
         my_real*VAR88,int *SIZVAR88,my_real*VAR89,int *SIZVAR89,my_real*VAR90,int *SIZVAR90,my_real*VAR91,int *SIZVAR91,    
         my_real*VAR92,int *SIZVAR92,my_real*VAR93,int *SIZVAR93,my_real*VAR94,int *SIZVAR94,my_real*VAR95,int *SIZVAR95,
         my_real*VAR96,int *SIZVAR96,my_real*VAR97,int *SIZVAR97,my_real*VAR98,int *SIZVAR98,my_real*VAR99,int *SIZVAR99){
         
           if(eng_get_law_user_var_2){
             eng_get_law_user_var_2 ( FPSXX,SIZFPSXX,FPSYY,SIZFPSYY,
                                      FPSZZ,SIZFPSZZ,FPSXY,SIZFPSXY,FPSYZ,SIZFPSYZ,FPSZX,SIZFPSZX,
                                      FPSYX,SIZFPSYX,FPSZY,SIZFPSZY,FPSXZ,SIZFPSXZ,VAR10,SIZVAR10,
                                      VAR11,SIZVAR11,VAR12,SIZVAR12,VAR13,SIZVAR13,VAR14,SIZVAR14,
                                      VAR15,SIZVAR15,VAR16,SIZVAR16,VAR17,SIZVAR17,VAR18,SIZVAR18,    
                                      VAR19,SIZVAR19,VAR20,SIZVAR20,VAR21,SIZVAR21,VAR22,SIZVAR22,
                                      VAR23,SIZVAR23,VAR24,SIZVAR24,VAR25,SIZVAR25,VAR26,SIZVAR26,    
                                      VAR27,SIZVAR27,VAR28,SIZVAR28,VAR29,SIZVAR29,VAR30,SIZVAR30,
                                      VAR31,SIZVAR31,VAR32,SIZVAR32,VAR33,SIZVAR33,VAR34,SIZVAR34,    
                                      VAR35,SIZVAR35,VAR36,SIZVAR36,VAR37,SIZVAR37,VAR38,SIZVAR38,
                                      VAR39,SIZVAR39,VAR40,SIZVAR40,VAR41,SIZVAR41,VAR42,SIZVAR42,    
                                      VAR43,SIZVAR43,VAR44,SIZVAR44,VAR45,SIZVAR45,VAR46,SIZVAR46,
                                      VAR47,SIZVAR47,VAR48,SIZVAR48,VAR49,SIZVAR49,VAR50,SIZVAR50,    
                                      VAR51,SIZVAR51,VAR52,SIZVAR52,VAR53,SIZVAR53,VAR54,SIZVAR54,
                                      VAR55,SIZVAR55,VAR56,SIZVAR56,VAR57,SIZVAR57,VAR58,SIZVAR58,    
                                      VAR59,SIZVAR59,VAR60,SIZVAR60,VAR61,SIZVAR61,VAR62,SIZVAR62,
                                      VAR63,SIZVAR63,VAR64,SIZVAR64,VAR65,SIZVAR65,VAR66,SIZVAR66,    
                                      VAR67,SIZVAR67,VAR68,SIZVAR68,VAR69,SIZVAR69,VAR70,SIZVAR70,
                                      VAR71,SIZVAR72,VAR73,SIZVAR73,VAR74,SIZVAR74,VAR75,SIZVAR75,
                                      VAR76,SIZVAR76,VAR77,SIZVAR77,VAR78,SIZVAR78,VAR79,SIZVAR79,
                                      VAR80,SIZVAR80,VAR81,SIZVAR81,VAR82,SIZVAR82,VAR83,SIZVAR83,    
                                      VAR84,SIZVAR84,VAR85,SIZVAR85,VAR86,SIZVAR86,VAR87,SIZVAR87,
                                      VAR88,SIZVAR88,VAR89,SIZVAR89,VAR90,SIZVAR90,VAR91,SIZVAR91,    
                                      VAR92,SIZVAR92,VAR93,SIZVAR93,VAR94,SIZVAR94,VAR95,SIZVAR95,
                                      VAR96,SIZVAR96,VAR97,SIZVAR97,VAR98,SIZVAR98,VAR99,SIZVAR99);}
           }

/* ---------------------------- */
/* WINDOWS - eng_userlib_get_law_user_var_2 */
/* ---------------------------- */
void _FCALL ENG_USERLIB_GET_LAW_VAR_2 ( my_real*FPSXX,int*SIZFPSXX,my_real*FPSYY,int *SIZFPSYY,
         my_real*FPSZZ,int *SIZFPSZZ,my_real*FPSXY,int *SIZFPSXY,my_real*FPSYZ,int *SIZFPSYZ,my_real*FPSZX,int *SIZFPSZX,
         my_real*FPSYX,int *SIZFPSYX,my_real*FPSZY,int *SIZFPSZY,my_real*FPSXZ,int *SIZFPSXZ,my_real*VAR10,int *SIZVAR10,
         my_real*VAR11,int *SIZVAR11,my_real*VAR12,int *SIZVAR12,my_real*VAR13,int *SIZVAR13,my_real*VAR14,int *SIZVAR14,
         my_real*VAR15,int *SIZVAR15,my_real*VAR16,int *SIZVAR16,my_real*VAR17,int *SIZVAR17,my_real*VAR18,int *SIZVAR18,    
         my_real*VAR19,int *SIZVAR19,my_real*VAR20,int *SIZVAR20,my_real*VAR21,int *SIZVAR21,my_real*VAR22,int *SIZVAR22,
         my_real*VAR23,int *SIZVAR23,my_real*VAR24,int *SIZVAR24,my_real*VAR25,int *SIZVAR25,my_real*VAR26,int *SIZVAR26,    
         my_real*VAR27,int *SIZVAR27,my_real*VAR28,int *SIZVAR28,my_real*VAR29,int *SIZVAR29,my_real*VAR30,int *SIZVAR30,
         my_real*VAR31,int *SIZVAR31,my_real*VAR32,int *SIZVAR32,my_real*VAR33,int *SIZVAR33,my_real*VAR34,int *SIZVAR34,    
         my_real*VAR35,int *SIZVAR35,my_real*VAR36,int *SIZVAR36,my_real*VAR37,int *SIZVAR37,my_real*VAR38,int *SIZVAR38,
         my_real*VAR39,int *SIZVAR39,my_real*VAR40,int *SIZVAR40,my_real*VAR41,int *SIZVAR41,my_real*VAR42,int *SIZVAR42,    
         my_real*VAR43,int *SIZVAR43,my_real*VAR44,int *SIZVAR44,my_real*VAR45,int *SIZVAR45,my_real*VAR46,int *SIZVAR46,
         my_real*VAR47,int *SIZVAR47,my_real*VAR48,int *SIZVAR48,my_real*VAR49,int *SIZVAR49,my_real*VAR50,int *SIZVAR50,    
         my_real*VAR51,int *SIZVAR51,my_real*VAR52,int *SIZVAR52,my_real*VAR53,int *SIZVAR53,my_real*VAR54,int *SIZVAR54,
         my_real*VAR55,int *SIZVAR55,my_real*VAR56,int *SIZVAR56,my_real*VAR57,int *SIZVAR57,my_real*VAR58,int *SIZVAR58,    
         my_real*VAR59,int *SIZVAR59,my_real*VAR60,int *SIZVAR60,my_real*VAR61,int *SIZVAR61,my_real*VAR62,int *SIZVAR62,
         my_real*VAR63,int *SIZVAR63,my_real*VAR64,int *SIZVAR64,my_real*VAR65,int *SIZVAR65,my_real*VAR66,int *SIZVAR66,    
         my_real*VAR67,int *SIZVAR67,my_real*VAR68,int *SIZVAR68,my_real*VAR69,int *SIZVAR69,my_real*VAR70,int *SIZVAR70,
         my_real*VAR71,int *SIZVAR72,my_real*VAR73,int *SIZVAR73,my_real*VAR74,int *SIZVAR74,my_real*VAR75,int *SIZVAR75,
         my_real*VAR76,int *SIZVAR76,my_real*VAR77,int *SIZVAR77,my_real*VAR78,int *SIZVAR78,my_real*VAR79,int *SIZVAR79,
         my_real*VAR80,int *SIZVAR80,my_real*VAR81,int *SIZVAR81,my_real*VAR82,int *SIZVAR82,my_real*VAR83,int *SIZVAR83,    
         my_real*VAR84,int *SIZVAR84,my_real*VAR85,int *SIZVAR85,my_real*VAR86,int *SIZVAR86,my_real*VAR87,int *SIZVAR87,
         my_real*VAR88,int *SIZVAR88,my_real*VAR89,int *SIZVAR89,my_real*VAR90,int *SIZVAR90,my_real*VAR91,int *SIZVAR91,    
         my_real*VAR92,int *SIZVAR92,my_real*VAR93,int *SIZVAR93,my_real*VAR94,int *SIZVAR94,my_real*VAR95,int *SIZVAR95,
         my_real*VAR96,int *SIZVAR96,my_real*VAR97,int *SIZVAR97,my_real*VAR98,int *SIZVAR98,my_real*VAR99,int *SIZVAR99){
         
           if(eng_get_law_user_var_2){
             (eng_get_law_user_var_2) ( FPSXX,SIZFPSXX,FPSYY,SIZFPSYY,
                                      FPSZZ,SIZFPSZZ,FPSXY,SIZFPSXY,FPSYZ,SIZFPSYZ,FPSZX,SIZFPSZX,
                                      FPSYX,SIZFPSYX,FPSZY,SIZFPSZY,FPSXZ,SIZFPSXZ,VAR10,SIZVAR10,
                                      VAR11,SIZVAR11,VAR12,SIZVAR12,VAR13,SIZVAR13,VAR14,SIZVAR14,
                                      VAR15,SIZVAR15,VAR16,SIZVAR16,VAR17,SIZVAR17,VAR18,SIZVAR18,    
                                      VAR19,SIZVAR19,VAR20,SIZVAR20,VAR21,SIZVAR21,VAR22,SIZVAR22,
                                      VAR23,SIZVAR23,VAR24,SIZVAR24,VAR25,SIZVAR25,VAR26,SIZVAR26,    
                                      VAR27,SIZVAR27,VAR28,SIZVAR28,VAR29,SIZVAR29,VAR30,SIZVAR30,
                                      VAR31,SIZVAR31,VAR32,SIZVAR32,VAR33,SIZVAR33,VAR34,SIZVAR34,    
                                      VAR35,SIZVAR35,VAR36,SIZVAR36,VAR37,SIZVAR37,VAR38,SIZVAR38,
                                      VAR39,SIZVAR39,VAR40,SIZVAR40,VAR41,SIZVAR41,VAR42,SIZVAR42,    
                                      VAR43,SIZVAR43,VAR44,SIZVAR44,VAR45,SIZVAR45,VAR46,SIZVAR46,
                                      VAR47,SIZVAR47,VAR48,SIZVAR48,VAR49,SIZVAR49,VAR50,SIZVAR50,    
                                      VAR51,SIZVAR51,VAR52,SIZVAR52,VAR53,SIZVAR53,VAR54,SIZVAR54,
                                      VAR55,SIZVAR55,VAR56,SIZVAR56,VAR57,SIZVAR57,VAR58,SIZVAR58,    
                                      VAR59,SIZVAR59,VAR60,SIZVAR60,VAR61,SIZVAR61,VAR62,SIZVAR62,
                                      VAR63,SIZVAR63,VAR64,SIZVAR64,VAR65,SIZVAR65,VAR66,SIZVAR66,    
                                      VAR67,SIZVAR67,VAR68,SIZVAR68,VAR69,SIZVAR69,VAR70,SIZVAR70,
                                      VAR71,SIZVAR72,VAR73,SIZVAR73,VAR74,SIZVAR74,VAR75,SIZVAR75,
                                      VAR76,SIZVAR76,VAR77,SIZVAR77,VAR78,SIZVAR78,VAR79,SIZVAR79,
                                      VAR80,SIZVAR80,VAR81,SIZVAR81,VAR82,SIZVAR82,VAR83,SIZVAR83,    
                                      VAR84,SIZVAR84,VAR85,SIZVAR85,VAR86,SIZVAR86,VAR87,SIZVAR87,
                                      VAR88,SIZVAR88,VAR89,SIZVAR89,VAR90,SIZVAR90,VAR91,SIZVAR91,    
                                      VAR92,SIZVAR92,VAR93,SIZVAR93,VAR94,SIZVAR94,VAR95,SIZVAR95,
                                      VAR96,SIZVAR96,VAR97,SIZVAR97,VAR98,SIZVAR98,VAR99,SIZVAR99);}
           }


/* ------------------------------ */
/* WINDOWS - eng_set_law_user_var */
/* ------------------------------ */
void _FCALL ENG_USERLIB_SET_LAW_VAR(  my_real*S1,  my_real*S2,   my_real*S3,  my_real*S4,  my_real*S5,  my_real*S6,
                                      my_real*SV1, my_real*SV2,  my_real*SV3, my_real*SV4, my_real*SV5, my_real*SV6,
                                      my_real*DPLA ) {
              if(eng_set_law_user_var){
       (eng_set_law_user_var)( S1,  S2,   S3, S4,  S5,  S6,
                               SV1, SV2,  SV3,SV4, SV5, SV6,
                               DPLA );
   }
}

/* ------------------------------ */
/* LINUX - eng_set_law_user_var */
/* ------------------------------ */
void eng_userlib_set_law_var_(  my_real*S1,  my_real*S2,   my_real*S3,  my_real*S4,  my_real*S5,  my_real*S6,
                                      my_real*SV1, my_real*SV2,  my_real*SV3, my_real*SV4, my_real*SV5, my_real*SV6,
                                      my_real*DPLA ) {
              if(eng_set_law_user_var){
       (eng_set_law_user_var)( S1,  S2,   S3, S4,  S5,  S6,
                               SV1, SV2,  SV3,SV4, SV5, SV6,
                               DPLA );
   }
}

/* ------------------------------ */
/* WINDOWS - eng_set_law_user_var_2 */
/* ------------------------------ */
void _FCALL ENG_USERLIB_SET_LAW_VAR_2(     my_real*VAR01,int *SIZVAR01,my_real*VAR02,int *SIZVAR02,
         my_real*VAR03,int *SIZVAR03,my_real*VAR04,int *SIZVAR04,my_real*VAR05,int *SIZVAR05,my_real*VAR06,int *SIZVAR06,
         my_real*VAR07,int *SIZVAR07,my_real*VAR08,int *SIZVAR08,my_real*VAR09,int *SIZVAR09,my_real*VAR10,int *SIZVAR10,
         my_real*VAR11,int *SIZVAR11,my_real*VAR12,int *SIZVAR12,my_real*VAR13,int *SIZVAR13,my_real*VAR14,int *SIZVAR14,
         my_real*VAR15,int *SIZVAR15,my_real*VAR16,int *SIZVAR16,my_real*VAR17,int *SIZVAR17,my_real*VAR18,int *SIZVAR18,    
         my_real*VAR19,int *SIZVAR19,my_real*VAR20,int *SIZVAR20,my_real*VAR21,int *SIZVAR21,my_real*VAR22,int *SIZVAR22,
         my_real*VAR23,int *SIZVAR23,my_real*VAR24,int *SIZVAR24,my_real*VAR25,int *SIZVAR25,my_real*VAR26,int *SIZVAR26,    
         my_real*VAR27,int *SIZVAR27,my_real*VAR28,int *SIZVAR28,my_real*VAR29,int *SIZVAR29,my_real*VAR30,int *SIZVAR30,
         my_real*VAR31,int *SIZVAR31,my_real*VAR32,int *SIZVAR32,my_real*VAR33,int *SIZVAR33,my_real*VAR34,int *SIZVAR34,    
         my_real*VAR35,int *SIZVAR35,my_real*VAR36,int *SIZVAR36,my_real*VAR37,int *SIZVAR37,my_real*VAR38,int *SIZVAR38,
         my_real*VAR39,int *SIZVAR39,my_real*VAR40,int *SIZVAR40,my_real*VAR41,int *SIZVAR41,my_real*VAR42,int *SIZVAR42,    
         my_real*VAR43,int *SIZVAR43,my_real*VAR44,int *SIZVAR44,my_real*VAR45,int *SIZVAR45,my_real*VAR46,int *SIZVAR46,
         my_real*VAR47,int *SIZVAR47,my_real*VAR48,int *SIZVAR48,my_real*VAR49,int *SIZVAR49,my_real*VAR50,int *SIZVAR50,    
         my_real*VAR51,int *SIZVAR51,my_real*VAR52,int *SIZVAR52,my_real*VAR53,int *SIZVAR53,my_real*VAR54,int *SIZVAR54,
         my_real*VAR55,int *SIZVAR55,my_real*VAR56,int *SIZVAR56,my_real*VAR57,int *SIZVAR57,my_real*VAR58,int *SIZVAR58,    
         my_real*VAR59,int *SIZVAR59,my_real*VAR60,int *SIZVAR60,my_real*VAR61,int *SIZVAR61,my_real*VAR62,int *SIZVAR62,
         my_real*VAR63,int *SIZVAR63,my_real*VAR64,int *SIZVAR64,my_real*VAR65,int *SIZVAR65,my_real*VAR66,int *SIZVAR66,    
         my_real*VAR67,int *SIZVAR67,my_real*VAR68,int *SIZVAR68,my_real*VAR69,int *SIZVAR69,my_real*VAR70,int *SIZVAR70,
         my_real*VAR71,int *SIZVAR72,my_real*VAR73,int *SIZVAR73,my_real*VAR74,int *SIZVAR74,my_real*VAR75,int *SIZVAR75,
         my_real*VAR76,int *SIZVAR76,my_real*VAR77,int *SIZVAR77,my_real*VAR78,int *SIZVAR78,my_real*VAR79,int *SIZVAR79,
         my_real*VAR80,int *SIZVAR80,my_real*VAR81,int *SIZVAR81,my_real*VAR82,int *SIZVAR82,my_real*VAR83,int *SIZVAR83,    
         my_real*VAR84,int *SIZVAR84,my_real*VAR85,int *SIZVAR85,my_real*VAR86,int *SIZVAR86,my_real*VAR87,int *SIZVAR87,
         my_real*VAR88,int *SIZVAR88,my_real*VAR89,int *SIZVAR89,my_real*VAR90,int *SIZVAR90,my_real*VAR91,int *SIZVAR91,    
         my_real*VAR92,int *SIZVAR92,my_real*VAR93,int *SIZVAR93,my_real*VAR94,int *SIZVAR94,my_real*VAR95,int *SIZVAR95,
         my_real*VAR96,int *SIZVAR96,my_real*VAR97,int *SIZVAR97,my_real*VAR98,int *SIZVAR98,my_real*VAR99,int *SIZVAR99){
           if(eng_set_law_user_var_2){
           (eng_set_law_user_var_2) (  VAR01,SIZVAR01,VAR02,SIZVAR02,
                                       VAR03,SIZVAR03,VAR04,SIZVAR04,VAR05,SIZVAR05,VAR06,SIZVAR06,
                                       VAR07,SIZVAR07,VAR08,SIZVAR08,VAR09,SIZVAR09,VAR10,SIZVAR10,
                                       VAR11,SIZVAR11,VAR12,SIZVAR12,VAR13,SIZVAR13,VAR14,SIZVAR14,
                                       VAR15,SIZVAR15,VAR16,SIZVAR16,VAR17,SIZVAR17,VAR18,SIZVAR18,    
                                       VAR19,SIZVAR19,VAR20,SIZVAR20,VAR21,SIZVAR21,VAR22,SIZVAR22,
                                       VAR23,SIZVAR23,VAR24,SIZVAR24,VAR25,SIZVAR25,VAR26,SIZVAR26,    
                                       VAR27,SIZVAR27,VAR28,SIZVAR28,VAR29,SIZVAR29,VAR30,SIZVAR30,
                                       VAR31,SIZVAR31,VAR32,SIZVAR32,VAR33,SIZVAR33,VAR34,SIZVAR34,    
                                       VAR35,SIZVAR35,VAR36,SIZVAR36,VAR37,SIZVAR37,VAR38,SIZVAR38,
                                       VAR39,SIZVAR39,VAR40,SIZVAR40,VAR41,SIZVAR41,VAR42,SIZVAR42,    
                                       VAR43,SIZVAR43,VAR44,SIZVAR44,VAR45,SIZVAR45,VAR46,SIZVAR46,
                                       VAR47,SIZVAR47,VAR48,SIZVAR48,VAR49,SIZVAR49,VAR50,SIZVAR50,    
                                       VAR51,SIZVAR51,VAR52,SIZVAR52,VAR53,SIZVAR53,VAR54,SIZVAR54,
                                       VAR55,SIZVAR55,VAR56,SIZVAR56,VAR57,SIZVAR57,VAR58,SIZVAR58,    
                                       VAR59,SIZVAR59,VAR60,SIZVAR60,VAR61,SIZVAR61,VAR62,SIZVAR62,
                                       VAR63,SIZVAR63,VAR64,SIZVAR64,VAR65,SIZVAR65,VAR66,SIZVAR66,    
                                       VAR67,SIZVAR67,VAR68,SIZVAR68,VAR69,SIZVAR69,VAR70,SIZVAR70,
                                       VAR71,SIZVAR72,VAR73,SIZVAR73,VAR74,SIZVAR74,VAR75,SIZVAR75,
                                       VAR76,SIZVAR76,VAR77,SIZVAR77,VAR78,SIZVAR78,VAR79,SIZVAR79,
                                       VAR80,SIZVAR80,VAR81,SIZVAR81,VAR82,SIZVAR82,VAR83,SIZVAR83,    
                                       VAR84,SIZVAR84,VAR85,SIZVAR85,VAR86,SIZVAR86,VAR87,SIZVAR87,
                                       VAR88,SIZVAR88,VAR89,SIZVAR89,VAR90,SIZVAR90,VAR91,SIZVAR91,    
                                       VAR92,SIZVAR92,VAR93,SIZVAR93,VAR94,SIZVAR94,VAR95,SIZVAR95,
                                       VAR96,SIZVAR96,VAR97,SIZVAR97,VAR98,SIZVAR98,VAR99,SIZVAR99);
                                     }
         }
 

void eng_userlib_set_law_var_2_(     my_real*VAR01,int *SIZVAR01,my_real*VAR02,int *SIZVAR02,
         my_real*VAR03,int *SIZVAR03,my_real*VAR04,int *SIZVAR04,my_real*VAR05,int *SIZVAR05,my_real*VAR06,int *SIZVAR06,
         my_real*VAR07,int *SIZVAR07,my_real*VAR08,int *SIZVAR08,my_real*VAR09,int *SIZVAR09,my_real*VAR10,int *SIZVAR10,
         my_real*VAR11,int *SIZVAR11,my_real*VAR12,int *SIZVAR12,my_real*VAR13,int *SIZVAR13,my_real*VAR14,int *SIZVAR14,
         my_real*VAR15,int *SIZVAR15,my_real*VAR16,int *SIZVAR16,my_real*VAR17,int *SIZVAR17,my_real*VAR18,int *SIZVAR18,    
         my_real*VAR19,int *SIZVAR19,my_real*VAR20,int *SIZVAR20,my_real*VAR21,int *SIZVAR21,my_real*VAR22,int *SIZVAR22,
         my_real*VAR23,int *SIZVAR23,my_real*VAR24,int *SIZVAR24,my_real*VAR25,int *SIZVAR25,my_real*VAR26,int *SIZVAR26,    
         my_real*VAR27,int *SIZVAR27,my_real*VAR28,int *SIZVAR28,my_real*VAR29,int *SIZVAR29,my_real*VAR30,int *SIZVAR30,
         my_real*VAR31,int *SIZVAR31,my_real*VAR32,int *SIZVAR32,my_real*VAR33,int *SIZVAR33,my_real*VAR34,int *SIZVAR34,    
         my_real*VAR35,int *SIZVAR35,my_real*VAR36,int *SIZVAR36,my_real*VAR37,int *SIZVAR37,my_real*VAR38,int *SIZVAR38,
         my_real*VAR39,int *SIZVAR39,my_real*VAR40,int *SIZVAR40,my_real*VAR41,int *SIZVAR41,my_real*VAR42,int *SIZVAR42,    
         my_real*VAR43,int *SIZVAR43,my_real*VAR44,int *SIZVAR44,my_real*VAR45,int *SIZVAR45,my_real*VAR46,int *SIZVAR46,
         my_real*VAR47,int *SIZVAR47,my_real*VAR48,int *SIZVAR48,my_real*VAR49,int *SIZVAR49,my_real*VAR50,int *SIZVAR50,    
         my_real*VAR51,int *SIZVAR51,my_real*VAR52,int *SIZVAR52,my_real*VAR53,int *SIZVAR53,my_real*VAR54,int *SIZVAR54,
         my_real*VAR55,int *SIZVAR55,my_real*VAR56,int *SIZVAR56,my_real*VAR57,int *SIZVAR57,my_real*VAR58,int *SIZVAR58,    
         my_real*VAR59,int *SIZVAR59,my_real*VAR60,int *SIZVAR60,my_real*VAR61,int *SIZVAR61,my_real*VAR62,int *SIZVAR62,
         my_real*VAR63,int *SIZVAR63,my_real*VAR64,int *SIZVAR64,my_real*VAR65,int *SIZVAR65,my_real*VAR66,int *SIZVAR66,    
         my_real*VAR67,int *SIZVAR67,my_real*VAR68,int *SIZVAR68,my_real*VAR69,int *SIZVAR69,my_real*VAR70,int *SIZVAR70,
         my_real*VAR71,int *SIZVAR72,my_real*VAR73,int *SIZVAR73,my_real*VAR74,int *SIZVAR74,my_real*VAR75,int *SIZVAR75,
         my_real*VAR76,int *SIZVAR76,my_real*VAR77,int *SIZVAR77,my_real*VAR78,int *SIZVAR78,my_real*VAR79,int *SIZVAR79,
         my_real*VAR80,int *SIZVAR80,my_real*VAR81,int *SIZVAR81,my_real*VAR82,int *SIZVAR82,my_real*VAR83,int *SIZVAR83,    
         my_real*VAR84,int *SIZVAR84,my_real*VAR85,int *SIZVAR85,my_real*VAR86,int *SIZVAR86,my_real*VAR87,int *SIZVAR87,
         my_real*VAR88,int *SIZVAR88,my_real*VAR89,int *SIZVAR89,my_real*VAR90,int *SIZVAR90,my_real*VAR91,int *SIZVAR91,    
         my_real*VAR92,int *SIZVAR92,my_real*VAR93,int *SIZVAR93,my_real*VAR94,int *SIZVAR94,my_real*VAR95,int *SIZVAR95,
         my_real*VAR96,int *SIZVAR96,my_real*VAR97,int *SIZVAR97,my_real*VAR98,int *SIZVAR98,my_real*VAR99,int *SIZVAR99){
           if(eng_set_law_user_var_2){
           (eng_set_law_user_var_2) (  VAR01,SIZVAR01,VAR02,SIZVAR02,
                                       VAR03,SIZVAR03,VAR04,SIZVAR04,VAR05,SIZVAR05,VAR06,SIZVAR06,
                                       VAR07,SIZVAR07,VAR08,SIZVAR08,VAR09,SIZVAR09,VAR10,SIZVAR10,
                                       VAR11,SIZVAR11,VAR12,SIZVAR12,VAR13,SIZVAR13,VAR14,SIZVAR14,
                                       VAR15,SIZVAR15,VAR16,SIZVAR16,VAR17,SIZVAR17,VAR18,SIZVAR18,    
                                       VAR19,SIZVAR19,VAR20,SIZVAR20,VAR21,SIZVAR21,VAR22,SIZVAR22,
                                       VAR23,SIZVAR23,VAR24,SIZVAR24,VAR25,SIZVAR25,VAR26,SIZVAR26,    
                                       VAR27,SIZVAR27,VAR28,SIZVAR28,VAR29,SIZVAR29,VAR30,SIZVAR30,
                                       VAR31,SIZVAR31,VAR32,SIZVAR32,VAR33,SIZVAR33,VAR34,SIZVAR34,    
                                       VAR35,SIZVAR35,VAR36,SIZVAR36,VAR37,SIZVAR37,VAR38,SIZVAR38,
                                       VAR39,SIZVAR39,VAR40,SIZVAR40,VAR41,SIZVAR41,VAR42,SIZVAR42,    
                                       VAR43,SIZVAR43,VAR44,SIZVAR44,VAR45,SIZVAR45,VAR46,SIZVAR46,
                                       VAR47,SIZVAR47,VAR48,SIZVAR48,VAR49,SIZVAR49,VAR50,SIZVAR50,    
                                       VAR51,SIZVAR51,VAR52,SIZVAR52,VAR53,SIZVAR53,VAR54,SIZVAR54,
                                       VAR55,SIZVAR55,VAR56,SIZVAR56,VAR57,SIZVAR57,VAR58,SIZVAR58,    
                                       VAR59,SIZVAR59,VAR60,SIZVAR60,VAR61,SIZVAR61,VAR62,SIZVAR62,
                                       VAR63,SIZVAR63,VAR64,SIZVAR64,VAR65,SIZVAR65,VAR66,SIZVAR66,    
                                       VAR67,SIZVAR67,VAR68,SIZVAR68,VAR69,SIZVAR69,VAR70,SIZVAR70,
                                       VAR71,SIZVAR72,VAR73,SIZVAR73,VAR74,SIZVAR74,VAR75,SIZVAR75,
                                       VAR76,SIZVAR76,VAR77,SIZVAR77,VAR78,SIZVAR78,VAR79,SIZVAR79,
                                       VAR80,SIZVAR80,VAR81,SIZVAR81,VAR82,SIZVAR82,VAR83,SIZVAR83,    
                                       VAR84,SIZVAR84,VAR85,SIZVAR85,VAR86,SIZVAR86,VAR87,SIZVAR87,
                                       VAR88,SIZVAR88,VAR89,SIZVAR89,VAR90,SIZVAR90,VAR91,SIZVAR91,    
                                       VAR92,SIZVAR92,VAR93,SIZVAR93,VAR94,SIZVAR94,VAR95,SIZVAR95,
                                       VAR96,SIZVAR96,VAR97,SIZVAR97,VAR98,SIZVAR98,VAR99,SIZVAR99);
                                     }
         }
 

/* ------------------------ */
/* User Rupture Model Shell */
/* ------------------------ */

/* WINDOWS */
void  _FCALL ENG_USERLIB_FLAWC(int *IRUP,
       int *NEL   ,int *NUPARAM,int *NUVAR   ,int *NFUNC   ,int *IFUNC  ,int *NPF    ,
       my_real *TF    ,my_real *TIME    ,my_real *TIMESTEP,my_real *UPARAM  , int *NGL   ,int *IPT    ,
       int *NPT0  ,int *IPM    ,int *NPROPMI ,int *MAT   ,
       my_real *SIGNXX ,my_real *SIGNYY ,my_real *SIGNXY  ,my_real *SIGNYZ  ,my_real *SIGNZX ,
       my_real *EPSPXX ,my_real *EPSPYY ,my_real *EPSPXY  ,my_real *EPSPYZ  ,my_real *EPSPZX ,
       my_real *EPSXX  ,my_real *EPSYY  ,my_real *EPSXY   ,my_real *EPSYZ   ,my_real *EPSZX  ,
       my_real *PLA    ,my_real *DPLA   ,my_real *EPSP    ,my_real *UVAR    ,my_real *UEL    , 
       my_real *OFF    ,my_real *BIDON1  ,my_real *BIDON2   ,my_real *BIDON3  ,my_real *BIDON4  ,my_real *BIDON5  ){

      if(eng_flawc){
          (eng_flawc)(IRUP,
       NEL   ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC  ,NPF    ,
       TF    ,TIME    ,TIMESTEP,UPARAM  , NGL   ,IPT    ,
       NPT0  ,IPM    ,NPROPMI ,MAT   ,
       SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
       EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
       EPSXX  ,EPSYY  ,EPSXY    ,EPSYZ   ,EPSZX  ,
       PLA    ,DPLA   ,EPSP    ,UVAR     ,UEL     , 
       OFF    ,BIDON1 ,BIDON2  ,BIDON3  ,BIDON4  ,BIDON5);
      }

}

/* LINUX */
void  eng_userlib_flawc_(int *IRUP,
       int *NEL   ,int *NUPARAM,int *NUVAR   ,int *NFUNC   ,int *IFUNC  ,int *NPF    ,
       my_real *TF    ,my_real *TIME    ,my_real *TIMESTEP,my_real *UPARAM  , int *NGL   ,int *IPT    ,
       int *NPT0  ,int *IPM    ,int *NPROPMI ,int *MAT   ,
       my_real *SIGNXX ,my_real *SIGNYY ,my_real *SIGNXY  ,my_real *SIGNYZ  ,my_real *SIGNZX ,
       my_real *EPSPXX ,my_real *EPSPYY ,my_real *EPSPXY  ,my_real *EPSPYZ  ,my_real *EPSPZX ,
       my_real *EPSXX  ,my_real *EPSYY  ,my_real *EPSXY   ,my_real *EPSYZ   ,my_real *EPSZX  ,
       my_real *PLA    ,my_real *DPLA   ,my_real *EPSP    ,my_real *UVAR    ,my_real *UEL    , 
       my_real *OFF    ,my_real *BIDON1  ,my_real *BIDON2   ,my_real *BIDON3  ,my_real *BIDON4  ,my_real *BIDON5  ){

      if(eng_flawc){
          (*eng_flawc)(IRUP,
       NEL   ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC  ,NPF    ,
       TF    ,TIME    ,TIMESTEP,UPARAM  , NGL   ,IPT    ,
       NPT0  ,IPM    ,NPROPMI ,MAT   ,
       SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
       EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
       EPSXX  ,EPSYY  ,EPSXY    ,EPSYZ   ,EPSZX  ,
       PLA    ,DPLA   ,EPSP    ,UVAR     ,UEL     , 
       OFF    ,BIDON1 ,BIDON2  ,BIDON3  ,BIDON4  ,BIDON5);  }

}


/* ------------------------- */
/* User Rupture Model Solids */
/* ------------------------- */

/* WINDOWS */
void  _FCALL ENG_USERLIB_FLAW(int*IRUP,
           int*NEL    ,int*NUPARAM,int*NUVAR   ,int*NFUNC   ,int*IFUNC   ,
           int*NPF    ,my_real *TF     ,my_real *TIME   ,my_real *TIMESTEP ,my_real *UPARAM  ,
           int*NGL    ,int*IPM     ,int*NPROPMI,int*MAT,int*IDEL7NOK,
           my_real *EPSPXX ,my_real *EPSPYY ,my_real *EPSPZZ ,my_real *EPSPXY,my_real *EPSPYZ,my_real *EPSPZX ,
           my_real *EPSXX  ,my_real *EPSYY  ,my_real *EPSZZ  ,my_real *EPSXY ,my_real *EPSYZ ,my_real *EPSZX  ,
           my_real *SIGNXX ,my_real *SIGNYY ,my_real *SIGNZZ ,my_real *SIGNXY,my_real *SIGNYZ,my_real *SIGNZX ,
           my_real *PLA    ,my_real *DPLA   ,my_real *EPSP   ,my_real *UVAR  ,my_real *OFF   ,
           my_real *BIDON1 ,my_real *BIDON2 ,my_real *BIDON3 ,my_real *BIDON4,my_real *BIDON5){

      if(eng_flaw){
          (*eng_flaw)(IRUP,
          NEL    , NUPARAM,NUVAR  ,NFUNC    ,IFUNC,
          NPF    , TF     ,TIME   ,TIMESTEP ,UPARAM  ,
          NGL    , IPM    ,NPROPMI,MAT      ,IDEL7NOK,
          EPSPXX , EPSPYY ,EPSPZZ ,EPSPXY,EPSPYZ,EPSPZX ,
          EPSXX  , EPSYY  ,EPSZZ  ,EPSXY ,EPSYZ ,EPSZX  ,
          SIGNXX , SIGNYY ,SIGNZZ ,SIGNXY,SIGNYZ,SIGNZX ,
          PLA    , DPLA     ,EPSP   ,UVAR  ,OFF   ,
          BIDON1 , BIDON2 ,BIDON3 ,BIDON4,BIDON5);
      }

}
/* Linux */
void  eng_userlib_flaw_(int*IRUP,
           int*NEL    ,int*NUPARAM,int*NUVAR   ,int*NFUNC   ,int*IFUNC   ,
           int*NPF    ,my_real *TF     ,my_real *TIME   ,my_real *TIMESTEP ,my_real *UPARAM  ,
           int*NGL    ,int*IPM     ,int*NPROPMI,int*MAT,int*IDEL7NOK,
           my_real *EPSPXX ,my_real *EPSPYY ,my_real *EPSPZZ ,my_real *EPSPXY,my_real *EPSPYZ,my_real *EPSPZX ,
           my_real *EPSXX  ,my_real *EPSYY  ,my_real *EPSZZ  ,my_real *EPSXY ,my_real *EPSYZ ,my_real *EPSZX  ,
           my_real *SIGNXX ,my_real *SIGNYY ,my_real *SIGNZZ ,my_real *SIGNXY,my_real *SIGNYZ,my_real *SIGNZX ,
           my_real *PLA    ,my_real *DPLA   ,my_real *EPSP   ,my_real *UVAR  ,my_real *OFF   ,
           my_real *BIDON1 ,my_real *BIDON2 ,my_real *BIDON3 ,my_real *BIDON4,my_real *BIDON5){

      if(eng_flaw){
          (*eng_flaw)(IRUP,
          NEL    , NUPARAM,NUVAR  ,NFUNC    ,IFUNC,
          NPF    , TF     ,TIME   ,TIMESTEP ,UPARAM  ,
          NGL    , IPM    ,NPROPMI,MAT      ,IDEL7NOK,
          EPSPXX , EPSPYY ,EPSPZZ ,EPSPXY,EPSPYZ,EPSPZX ,
          EPSXX  , EPSYY  ,EPSZZ  ,EPSXY ,EPSYZ ,EPSZX  ,
          SIGNXX , SIGNYY ,SIGNZZ ,SIGNXY,SIGNYZ,SIGNZX ,
          PLA    , DPLA     ,EPSP   ,UVAR  ,OFF   ,
          BIDON1 , BIDON2 ,BIDON3 ,BIDON4,BIDON5);
      }

}


/* --------------------------------- */
/* User Spring 29-30-31 Shell */
/* --------------------------------- */

/* WINDOWS */
void  _FCALL ENG_USERLIB_RUSER(int *ITYP,
                    int * NEL     ,int *IPROP       ,my_real *UVAR   ,int *NUVAR  ,
             my_real *FX      ,my_real *FY      ,my_real *FZ     ,my_real *XMOM   ,my_real *YMOM   ,
             my_real *ZMOM    ,my_real *E        ,my_real *OFF    ,my_real *STIFM  ,my_real *STIFR  ,
             my_real *VISCM   ,my_real *VISCR   ,my_real *MASS   ,my_real *XINER  ,my_real *DT     ,
             my_real *XL      ,my_real *VX      ,my_real *RY1    ,my_real *RZ1    ,my_real *RX     ,
             my_real *RY2      ,my_real *RZ2     ,my_real *FR_WAVE){

      if(eng_ruser){
          (*eng_ruser)(ITYP,NEL   ,IPROP  ,UVAR    ,NUVAR  ,
                FX    ,FY      ,FZ      ,XMOM   ,YMOM   ,
                ZMOM  ,E      ,OFF     ,STIFM  ,STIFR  ,
                VISCM ,VISCR   ,MASS   ,XINER  ,DT     ,
                XL      ,VX       ,RY1    ,RZ1    ,RX     ,
                    RY2      ,RZ2       ,FR_WAVE);
      }

}

/* LINUX */
void   eng_userlib_ruser_(int *ITYP,
                    int * NEL     ,int *IPROP       ,my_real *UVAR   ,int *NUVAR  ,
             my_real *FX      ,my_real *FY      ,my_real *FZ     ,my_real *XMOM   ,my_real *YMOM   ,
             my_real *ZMOM    ,my_real *E        ,my_real *OFF    ,my_real *STIFM  ,my_real *STIFR  ,
             my_real *VISCM   ,my_real *VISCR   ,my_real *MASS   ,my_real *XINER  ,my_real *DT     ,
             my_real *XL      ,my_real *VX      ,my_real *RY1    ,my_real *RZ1    ,my_real *RX     ,
             my_real *RY2      ,my_real *RZ2     ,my_real *FR_WAVE){

      if(eng_ruser){
          (*eng_ruser)(ITYP,
                    NEL   ,IPROP  ,UVAR    ,NUVAR  ,
                FX    ,FY      ,FZ      ,XMOM   ,YMOM   ,
                ZMOM  ,E      ,OFF     ,STIFM  ,STIFR  ,
                VISCM ,VISCR   ,MASS   ,XINER  ,DT     ,
                XL      ,VX       ,RY1    ,RZ1    ,RX     ,
                    RY2      ,RZ2       ,FR_WAVE);
      }
}


void _FCALL ENG_USERLIB_SUSER(int*ITYP,
      int*NEL     ,int*NUVAR   ,int*IPROP  ,int*IMAT  ,int*SOLID_ID,my_real *TIME  ,my_real *TIMESTEP,
      my_real *EINT   ,my_real *VOL    ,my_real *UVAR   ,my_real *FR_WAVE,my_real *OFF    ,my_real *RHO    ,my_real *SIG    ,
      my_real *XX1    ,my_real *XX2    ,my_real *XX3    ,my_real *XX4    ,my_real *XX5    ,my_real *XX6    ,my_real *XX7    ,my_real *XX8    ,     
      my_real *YY1    ,my_real *YY2    ,my_real *YY3    ,my_real *YY4    ,my_real *YY5    ,my_real *YY6    ,my_real *YY7    ,my_real *YY8    ,  
      my_real *ZZ1    ,my_real *ZZ2    ,my_real *ZZ3    ,my_real *ZZ4    ,my_real *ZZ5    ,my_real *ZZ6    ,my_real *ZZ7    ,my_real *ZZ8    ,
      my_real *UX1    ,my_real *UX2    ,my_real *UX3    ,my_real *UX4    ,my_real *UX5    ,my_real *UX6    ,my_real *UX7    ,my_real *UX8    ,
      my_real *UY1    ,my_real *UY2    ,my_real *UY3    ,my_real *UY4    ,my_real *UY5    ,my_real *UY6    ,my_real *UY7    ,my_real *UY8    ,
      my_real *UZ1    ,my_real *UZ2    ,my_real *UZ3    ,my_real *UZ4    ,my_real *UZ5    ,my_real *UZ6    ,my_real *UZ7    ,my_real *UZ8    ,
      my_real *VX1    ,my_real *VX2    ,my_real *VX3    ,my_real *VX4    ,my_real *VX5    ,my_real *VX6    ,my_real *VX7    ,my_real *VX8    ,
      my_real *VY1    ,my_real *VY2    ,my_real *VY3    ,my_real *VY4    ,my_real *VY5    ,my_real *VY6    ,my_real *VY7    ,my_real *VY8    ,
      my_real *VZ1    ,my_real *VZ2    ,my_real *VZ3    ,my_real *VZ4    ,my_real *VZ5    ,my_real *VZ6    ,my_real *VZ7    ,my_real *VZ8    ,
      my_real *VRX1   ,my_real *VRX2   ,my_real *VRX3   ,my_real *VRX4   ,my_real *VRX5   ,my_real *VRX6   ,my_real *VRX7   ,my_real *VRX8   ,
      my_real *VRY1   ,my_real *VRY2   ,my_real *VRY3   ,my_real *VRY4   ,my_real *VRY5   ,my_real *VRY6   ,my_real *VRY7   ,my_real *VRY8   ,
      my_real *VRZ1   ,my_real *VRZ2   ,my_real *VRZ3   ,my_real *VRZ4   ,my_real *VRZ5   ,my_real *VRZ6   ,my_real *VRZ7   ,my_real *VRZ8   ,
      my_real *FX1    ,my_real *FX2    ,my_real *FX3    ,my_real *FX4    ,my_real *FX5    ,my_real *FX6    ,my_real *FX7    ,my_real *FX8    ,
      my_real *FY1    ,my_real *FY2    ,my_real *FY3    ,my_real *FY4    ,my_real *FY5    ,my_real *FY6    ,my_real *FY7    ,my_real *FY8    ,
      my_real *FZ1    ,my_real *FZ2    ,my_real *FZ3    ,my_real *FZ4    ,my_real *FZ5    ,my_real *FZ6    ,my_real *FZ7    ,my_real *FZ8    ,
      my_real * MX1    ,my_real *MX2   ,my_real *MX3    ,my_real *MX4    ,my_real *MX5    ,my_real *MX6    ,my_real *MX7    ,my_real *MX8    ,
      my_real *MY1    ,my_real *MY2    ,my_real *MY3    ,my_real *MY4    ,my_real *MY5    ,my_real *MY6    ,my_real *MY7    ,my_real *MY8    ,
      my_real *MZ1    ,my_real *MZ2    ,my_real *MZ3    ,my_real *MZ4    ,my_real *MZ5    ,my_real *MZ6    ,my_real *MZ7    ,my_real *MZ8    ,
      my_real *STIFM  ,my_real *STIFR  ,my_real *VISCM  ,my_real *VISCR  ){
      
      if (eng_suser){
         (*eng_suser)(ITYP,
      NEL    ,NUVAR  ,IPROP  ,IMAT  ,SOLID_ID,TIME  ,TIMESTEP,
      EINT   ,VOL    ,UVAR   ,FR_WAVE,OFF    ,RHO    ,SIG    ,
      XX1    ,XX2    ,XX3    ,XX4    ,XX5    ,XX6    ,XX7    ,XX8    ,     
      YY1    ,YY2    ,YY3    ,YY4    ,YY5    ,YY6    ,YY7    ,YY8    ,  
      ZZ1    ,ZZ2    ,ZZ3    ,ZZ4    ,ZZ5    ,ZZ6    ,ZZ7    ,ZZ8    ,
      UX1    ,UX2    ,UX3    ,UX4    ,UX5    ,UX6    ,UX7    ,UX8    ,
      UY1    ,UY2    ,UY3    ,UY4    ,UY5    ,UY6    ,UY7    ,UY8    ,
      UZ1    ,UZ2    ,UZ3    ,UZ4    ,UZ5    ,UZ6    ,UZ7    ,UZ8    ,
      VX1    ,VX2    ,VX3    ,VX4    ,VX5    ,VX6    ,VX7    ,VX8    ,
      VY1    ,VY2    ,VY3    ,VY4    ,VY5    ,VY6    ,VY7    ,VY8    ,
      VZ1    ,VZ2    ,VZ3    ,VZ4    ,VZ5    ,VZ6    ,VZ7    ,VZ8    ,
      VRX1   ,VRX2   ,VRX3   ,VRX4   ,VRX5   ,VRX6   ,VRX7   ,VRX8   ,
      VRY1   ,VRY2   ,VRY3   ,VRY4   ,VRY5   ,VRY6   ,VRY7   ,VRY8   ,
      VRZ1   ,VRZ2   ,VRZ3   ,VRZ4   ,VRZ5   ,VRZ6   ,VRZ7   ,VRZ8   ,
      FX1    ,FX2    ,FX3    ,FX4    ,FX5    ,FX6    ,FX7    ,FX8    ,
      FY1    ,FY2    ,FY3    ,FY4    ,FY5    ,FY6    ,FY7    ,FY8    ,
      FZ1    ,FZ2    ,FZ3    ,FZ4    ,FZ5    ,FZ6    ,FZ7    ,FZ8    ,
      MX1    ,MX2    ,MX3    ,MX4    ,MX5    ,MX6    ,MX7    ,MX8    ,
      MY1    ,MY2    ,MY3    ,MY4    ,MY5    ,MY6    ,MY7    ,MY8    ,
      MZ1    ,MZ2    ,MZ3    ,MZ4    ,MZ5    ,MZ6    ,MZ7    ,MZ8    ,
      STIFM  ,STIFR  ,VISCM  ,VISCR  );
      }
}


void eng_userlib_suser_(int*ITYP,
      int*NEL     ,int*NUVAR   ,int*IPROP  ,int*IMAT  ,int*SOLID_ID,my_real *TIME  ,my_real *TIMESTEP,
      my_real *EINT   ,my_real *VOL    ,my_real *UVAR   ,my_real *FR_WAVE,my_real *OFF    ,my_real *RHO    ,my_real *SIG    ,
      my_real *XX1    ,my_real *XX2    ,my_real *XX3    ,my_real *XX4    ,my_real *XX5    ,my_real *XX6    ,my_real *XX7    ,my_real *XX8    ,     
      my_real *YY1    ,my_real *YY2    ,my_real *YY3    ,my_real *YY4    ,my_real *YY5    ,my_real *YY6    ,my_real *YY7    ,my_real *YY8    ,  
      my_real *ZZ1    ,my_real *ZZ2    ,my_real *ZZ3    ,my_real *ZZ4    ,my_real *ZZ5    ,my_real *ZZ6    ,my_real *ZZ7    ,my_real *ZZ8    ,
      my_real *UX1    ,my_real *UX2    ,my_real *UX3    ,my_real *UX4    ,my_real *UX5    ,my_real *UX6    ,my_real *UX7    ,my_real *UX8    ,
      my_real *UY1    ,my_real *UY2    ,my_real *UY3    ,my_real *UY4    ,my_real *UY5    ,my_real *UY6    ,my_real *UY7    ,my_real *UY8    ,
      my_real *UZ1    ,my_real *UZ2    ,my_real *UZ3    ,my_real *UZ4    ,my_real *UZ5    ,my_real *UZ6    ,my_real *UZ7    ,my_real *UZ8    ,
      my_real *VX1    ,my_real *VX2    ,my_real *VX3    ,my_real *VX4    ,my_real *VX5    ,my_real *VX6    ,my_real *VX7    ,my_real *VX8    ,
      my_real *VY1    ,my_real *VY2    ,my_real *VY3    ,my_real *VY4    ,my_real *VY5    ,my_real *VY6    ,my_real *VY7    ,my_real *VY8    ,
      my_real *VZ1    ,my_real *VZ2    ,my_real *VZ3    ,my_real *VZ4    ,my_real *VZ5    ,my_real *VZ6    ,my_real *VZ7    ,my_real *VZ8    ,
      my_real *VRX1   ,my_real *VRX2   ,my_real *VRX3   ,my_real *VRX4   ,my_real *VRX5   ,my_real *VRX6   ,my_real *VRX7   ,my_real *VRX8   ,
      my_real *VRY1   ,my_real *VRY2   ,my_real *VRY3   ,my_real *VRY4   ,my_real *VRY5   ,my_real *VRY6   ,my_real *VRY7   ,my_real *VRY8   ,
      my_real *VRZ1   ,my_real *VRZ2   ,my_real *VRZ3   ,my_real *VRZ4   ,my_real *VRZ5   ,my_real *VRZ6   ,my_real *VRZ7   ,my_real *VRZ8   ,
      my_real *FX1    ,my_real *FX2    ,my_real *FX3    ,my_real *FX4    ,my_real *FX5    ,my_real *FX6    ,my_real *FX7    ,my_real *FX8    ,
      my_real *FY1    ,my_real *FY2    ,my_real *FY3    ,my_real *FY4    ,my_real *FY5    ,my_real *FY6    ,my_real *FY7    ,my_real *FY8    ,
      my_real *FZ1    ,my_real *FZ2    ,my_real *FZ3    ,my_real *FZ4    ,my_real *FZ5    ,my_real *FZ6    ,my_real *FZ7    ,my_real *FZ8    ,
      my_real * MX1    ,my_real *MX2   ,my_real *MX3    ,my_real *MX4    ,my_real *MX5    ,my_real *MX6    ,my_real *MX7    ,my_real *MX8    ,
      my_real *MY1    ,my_real *MY2    ,my_real *MY3    ,my_real *MY4    ,my_real *MY5    ,my_real *MY6    ,my_real *MY7    ,my_real *MY8    ,
      my_real *MZ1    ,my_real *MZ2    ,my_real *MZ3    ,my_real *MZ4    ,my_real *MZ5    ,my_real *MZ6    ,my_real *MZ7    ,my_real *MZ8    ,
      my_real *STIFM  ,my_real *STIFR  ,my_real *VISCM  ,my_real *VISCR  ){
      
      if (eng_suser){
         (*eng_suser)(ITYP,
      NEL    ,NUVAR  ,IPROP  ,IMAT  ,SOLID_ID,TIME  ,TIMESTEP,
      EINT   ,VOL    ,UVAR   ,FR_WAVE,OFF    ,RHO    ,SIG    ,
      XX1    ,XX2    ,XX3    ,XX4    ,XX5    ,XX6    ,XX7    ,XX8    ,     
      YY1    ,YY2    ,YY3    ,YY4    ,YY5    ,YY6    ,YY7    ,YY8    ,  
      ZZ1    ,ZZ2    ,ZZ3    ,ZZ4    ,ZZ5    ,ZZ6    ,ZZ7    ,ZZ8    ,
      UX1    ,UX2    ,UX3    ,UX4    ,UX5    ,UX6    ,UX7    ,UX8    ,
      UY1    ,UY2    ,UY3    ,UY4    ,UY5    ,UY6    ,UY7    ,UY8    ,
      UZ1    ,UZ2    ,UZ3    ,UZ4    ,UZ5    ,UZ6    ,UZ7    ,UZ8    ,
      VX1    ,VX2    ,VX3    ,VX4    ,VX5    ,VX6    ,VX7    ,VX8    ,
      VY1    ,VY2    ,VY3    ,VY4    ,VY5    ,VY6    ,VY7    ,VY8    ,
      VZ1    ,VZ2    ,VZ3    ,VZ4    ,VZ5    ,VZ6    ,VZ7    ,VZ8    ,
      VRX1   ,VRX2   ,VRX3   ,VRX4   ,VRX5   ,VRX6   ,VRX7   ,VRX8   ,
      VRY1   ,VRY2   ,VRY3   ,VRY4   ,VRY5   ,VRY6   ,VRY7   ,VRY8   ,
      VRZ1   ,VRZ2   ,VRZ3   ,VRZ4   ,VRZ5   ,VRZ6   ,VRZ7   ,VRZ8   ,
      FX1    ,FX2    ,FX3    ,FX4    ,FX5    ,FX6    ,FX7    ,FX8    ,
      FY1    ,FY2    ,FY3    ,FY4    ,FY5    ,FY6    ,FY7    ,FY8    ,
      FZ1    ,FZ2    ,FZ3    ,FZ4    ,FZ5    ,FZ6    ,FZ7    ,FZ8    ,
      MX1    ,MX2    ,MX3    ,MX4    ,MX5    ,MX6    ,MX7    ,MX8    ,
      MY1    ,MY2    ,MY3    ,MY4    ,MY5    ,MY6    ,MY7    ,MY8    ,
      MZ1    ,MZ2    ,MZ3    ,MZ4    ,MZ5    ,MZ6    ,MZ7    ,MZ8    ,
      STIFM  ,STIFR  ,VISCM  ,VISCR  );
      }
}

/* ------------------------------ */
/* Interface type 2 / USERRUPTURE */
/* ------------------------------ */

void _FCALL ENG_USERLIB_USERINT(int *IGTYP,
      int *NSN       ,int *II       ,int *PID      ,int *NUVAR     ,  
      my_real * UVAR  ){

       if (eng_userint){
           (eng_userint)(IGTYP,
            NSN       ,II       ,PID      ,NUVAR     ,  
            UVAR  );
 }
}

void eng_userlib_userint_(int *IGTYP,
      int *NSN       ,int *II       ,int *PID      ,int *NUVAR     ,  
      my_real * UVAR  ){

       if (eng_userint){
           (eng_userint)(IGTYP,
            NSN       ,II       ,PID      ,NUVAR     ,  
            UVAR  );
 }
}


void _FCALL ENG_USERLIB_UINTBUF_VAR(int *ISLAVE, my_real *AREA, my_real *DT,my_real *DXN,my_real *DXT,my_real *SIGN,my_real *SIGT,
                           my_real *RUPT, my_real *FACN, my_real *FACT ){
       if (*eng_get_uintbuf_var){
           (eng_get_uintbuf_var)( ISLAVE, AREA, DT,DXN,DXT,SIGN,SIGT,
                                  RUPT, FACN, FACT  );
 }
}


void eng_userlib_uintbuf_var_(int *ISLAVE, my_real *AREA, my_real *DT,my_real *DXN,my_real *DXT,my_real *SIGN,my_real *SIGT,
                           my_real *RUPT, my_real *FACN, my_real *FACT ){
       if (*eng_get_uintbuf_var){
           (eng_get_uintbuf_var)( ISLAVE, AREA, DT,DXN,DXT,SIGN,SIGT,
                                  RUPT, FACN, FACT  );
 }
}

void _FCALL ENG_USERLIB_USER_SENS(int *TYP,int *ID ){
       if (*eng_user_sens){
           (eng_user_sens)( TYP,ID );
 }
}

void _FCALL eng_userlib_user_sens_(int *TYP,int *ID ){
       if (*eng_user_sens){
           (eng_user_sens)( TYP,ID );
 }
}



void _FCALL ENG_USERLIB_USERWI(char *ROOTN ,int *ROOTLEN ,
             int *NUVAR  ,int *NUVARI ,int *NUMNOD ,
             int *NCYCLE ,int *LENWA  ,int *IUVAR  ,int *ITAB   ,my_real *TT     ,
             my_real *DT1    ,my_real *DT2     ,my_real *USREINT,my_real *EXWORK ,my_real *UVAR   ,
             my_real *D    ,my_real *X     ,my_real *V      ,my_real *VR     ,my_real *MASS   ,
             my_real *INER    ,my_real *STIFN  ,my_real *STIFR  ,my_real *A       ,my_real *AR     ,
             my_real *WA    ){
       if (*eng_userwi){
           (eng_userwi)(ROOTN ,ROOTLEN ,
             NUVAR  ,NUVARI ,NUMNOD ,
             NCYCLE ,LENWA  ,IUVAR  ,ITAB   ,TT     ,
             DT1    ,DT2    ,USREINT,EXWORK ,UVAR   ,
             D      ,X      ,V      ,VR     ,MASS   ,
             INER   ,STIFN  ,STIFR  ,A      ,AR     ,
             WA     );
 }
}

void eng_userlib_userwi_(char *ROOTN ,int *ROOTLEN ,
             int *NUVAR  ,int *NUVARI ,int *NUMNOD ,
             int *NCYCLE ,int *LENWA  ,int *IUVAR  ,int *ITAB   ,my_real *TT     ,
             my_real *DT1    ,my_real *DT2     ,my_real *USREINT,my_real *EXWORK ,my_real *UVAR   ,
             my_real *D    ,my_real *X     ,my_real *V      ,my_real *VR     ,my_real *MASS   ,
             my_real *INER    ,my_real *STIFN  ,my_real *STIFR  ,my_real *A       ,my_real *AR     ,
             my_real *WA    ){
       if (*eng_userwi){
           (eng_userwi)(ROOTN ,ROOTLEN ,
             NUVAR  ,NUVARI ,NUMNOD ,
             NCYCLE ,LENWA  ,IUVAR  ,ITAB   ,TT     ,
             DT1    ,DT2    ,USREINT,EXWORK ,UVAR   ,
             D      ,X      ,V      ,VR     ,MASS   ,
             INER   ,STIFN  ,STIFR  ,A      ,AR     ,
             WA     );
 }
}
/* ----------------------------------------------- */
/* Routine ENGINE_USER_CHECK*/
//  Windows
void _FCALL ENGINE_USER_CHECK(int *MY_RANK,double *TSTOP,int *NCYCLE,double *TT,int *MSTOP){

       if (*engine_user_check){
           (engine_user_check)( MY_RANK,TSTOP,NCYCLE,TT,MSTOP );
 }
}

//  Linux
void engine_user_check_(int *MY_RANK,double *TSTOP,int *NCYCLE,double *TT,int *MSTOP){

       if (*engine_user_check){
           (engine_user_check)( MY_RANK,TSTOP,NCYCLE,TT,MSTOP );
 }
}
/* ----------------------------------------------- */
/* Routine ENGINE_USER_FINALIZE*/
//  Windows
void _FCALL ENGINE_USER_FINALIZE( int *MY_RANK ){

       if (*engine_user_finalize){
           (engine_user_finalize)( MY_RANK );
 }
}

//  Linux
void engine_user_finalize_( int *MY_RANK ){

       if (*engine_user_finalize){
           (engine_user_finalize)( MY_RANK );
 }
}
/* ----------------------------------------------- */
/* Routine ENGINE_USER_INITIALIZE*/
//  Windows
void _FCALL ENGINE_USER_INITIALIZE(int *NSPMD, int *NTHREAD, int *MY_RANK){

       if (*engine_user_initialize){
           (engine_user_initialize)(NSPMD,NTHREAD,MY_RANK);
 }
}

//  Linux
void engine_user_initialize_(int *NSPMD, int *NTHREAD, int *MY_RANK){

       if (*engine_user_initialize){
           (engine_user_initialize)(NSPMD,NTHREAD,MY_RANK);
 }
}
/* ----------------------------------------------- */


void delete_user_file_(char * filnam,int * len)
{
  int i;
  char * filn;
  filn = (char*)malloc(sizeof(char)* 1+ *len);
  for (i=0;i< *len ; i++){
    filn[i]=filnam[i];
  }
  filn[*len]='\0';

  remove(filn);
}

void _FCALL DELETE_USER_FILE(char * filnam,int * len)
{
  int i;
  char * filn;
  filn = (char*)malloc(sizeof(char)* 1+ *len);
  for (i=0;i< *len ; i++){
    filn[i]=filnam[i];
  }
  filn[*len]='\0';

  remove(filn);
}
