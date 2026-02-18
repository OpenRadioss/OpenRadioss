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



// Pointer routines to rad_userlib library 
// ------------------------------------------

// Pointers to the user library function
void  (*userlib_id)(int * info);

//  ------------
//  User Springs 
//  ------------
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

         
/* ----------- */
/* User solids */
/* ----------- */
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

// ----------------
// User shells
// ----------------
void  (*eng_cuser29)(
      int *NEL ,int* nnod    ,int*NUVAR   ,int*IPROP  ,int*IMAT  ,int*SOLID_ID,my_real *TIME  ,my_real *TIMESTEP,
      my_real *EINT   ,my_real *VOL    ,my_real *UVAR   ,my_real *FR_WAVE ,my_real *OFF   ,my_real *RHO    ,my_real *SIG    ,
      my_real *XX     ,my_real *YY     ,my_real *ZZ     ,my_real *UX      ,my_real *UY    ,my_real *UZ     ,
      my_real *VX     ,my_real *VY     ,my_real *VZ     ,my_real *VRX     ,my_real *VRY   ,my_real *VRZ    ,
      my_real *FX     ,my_real *FY     ,my_real *FZ     ,my_real *MX      ,my_real *MY    ,my_real *MZ     ,
      my_real *STIFM  ,my_real *STIFR  ,my_real *VISCM  ,my_real *VISCR  );

void  (*eng_cuser30)(
      int *NEL ,int* nnod    ,int*NUVAR   ,int*IPROP  ,int*IMAT  ,int*SOLID_ID,my_real *TIME  ,my_real *TIMESTEP,
      my_real *EINT   ,my_real *VOL    ,my_real *UVAR   ,my_real *FR_WAVE ,my_real *OFF   ,my_real *RHO    ,my_real *SIG    ,
      my_real *XX     ,my_real *YY     ,my_real *ZZ     ,my_real *UX      ,my_real *UY    ,my_real *UZ     ,
      my_real *VX     ,my_real *VY     ,my_real *VZ     ,my_real *VRX     ,my_real *VRY   ,my_real *VRZ    ,
      my_real *FX     ,my_real *FY     ,my_real *FZ     ,my_real *MX      ,my_real *MY    ,my_real *MZ     ,
      my_real *STIFM  ,my_real *STIFR  ,my_real *VISCM  ,my_real *VISCR  );

void  (*eng_cuser31)(
      int *NEL ,int* nnod    ,int*NUVAR   ,int*IPROP  ,int*IMAT  ,int*SOLID_ID,my_real *TIME  ,my_real *TIMESTEP,
      my_real *EINT   ,my_real *VOL    ,my_real *UVAR   ,my_real *FR_WAVE ,my_real *OFF   ,my_real *RHO    ,my_real *SIG    ,
      my_real *XX     ,my_real *YY     ,my_real *ZZ     ,my_real *UX      ,my_real *UY    ,my_real *UZ     ,
      my_real *VX     ,my_real *VY     ,my_real *VZ     ,my_real *VRX     ,my_real *VRY   ,my_real *VRZ    ,
      my_real *FX     ,my_real *FY     ,my_real *FZ     ,my_real *MX      ,my_real *MY    ,my_real *MZ     ,
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


// Interface routines
// ------------------

#ifdef _WIN64
// User materials
#define eng_userlib_sigeps_         ENG_USERLIB_SIGEPS
#define eng_userlib_sigepsc_        ENG_USERLIB_SIGEPSC
#define eng_userlib_sigeps99c_      ENG_USERLIB_SIGEPS99C
#define eng_userlib_get_lawc_var_   ENG_USERLIB_GET_LAWC_VAR
#define eng_userlib_get_lawc_var_2_ ENG_USERLIB_GET_LAWC_VAR_2
#define eng_userlib_set_lawc_       ENG_USERLIB_SET_LAWC
#define eng_userlib_set_lawc_var_2_ ENG_USERLIB_SET_LAWC_VAR_2
#define eng_userlib_sigeps99_       ENG_USERLIB_SIGEPS99
#define eng_userlib_get_law_var_    ENG_USERLIB_GET_LAW_VAR
#define eng_userlib_get_law_var_2_  ENG_USERLIB_GET_LAW_VAR_2
#define eng_userlib_set_law_var_    ENG_USERLIB_SET_LAW_VAR
#define eng_userlib_set_law_var_2_  ENG_USERLIB_SET_LAW_VAR_2
// User failure models
#define eng_userlib_flawc_          ENG_USERLIB_FLAWC
#define eng_userlib_flaw_           ENG_USERLIB_FLAW
// User properties
#define eng_userlib_ruser_          ENG_USERLIB_RUSER
#define eng_userlib_suser_          ENG_USERLIB_SUSER

#endif

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
        my_real *SOUNDSP,my_real *VISCMAX,my_real *UVAR    ,my_real *OFF     );

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
      my_real *OFF    ,int *NGL    ,int *SHF);

void eng_userlib_sigeps99c_( int*NEL           ,int*NUPARAM      ,int*NUVAR         ,int*ILAW_USER  ,int*NFUNC,
                             int*IFUNC         ,int*NPF          ,int*NGL           ,my_real *TF    ,my_real *TIME,
                             my_real *TIMESTEP ,my_real *UPARAM  ,my_real *RHO      ,my_real *AREA  ,my_real *EINT,
                             my_real *SHF      ,my_real *SOUNDSP ,my_real *VISCMAX  ,my_real *PLA   ,my_real *UVAR,
                             my_real *OFF      ,my_real *SIGY );

void eng_userlib_get_lawc_var_( int*NCYCLE, int*IMAT, int*ILAYER, int*NPTA, int*IFLAG,
                     my_real* R11,     my_real*R12,    my_real*R13,    my_real*R21,    my_real*R22,
                     my_real* R23,     my_real*R31,    my_real*R32,    my_real*R33,    my_real*SIGOXX,
                     my_real* SIGOYY, my_real*SIGOXY, my_real*SIGOYZ, my_real*SIGOZX, my_real*EPSPXX,
                     my_real* EPSPYY, my_real*EPSPXY, my_real*EPSPYZ, my_real*EPSPZX, my_real*EPSXX,
                     my_real* EPSYY,  my_real*EPSXY,  my_real*EPSYZ,  my_real*EPSZX,  my_real*DEPSXX,
                     my_real* DEPSYY, my_real*DEPSXY, my_real*DEPSYZ, my_real*DEPSZX, my_real*THKLYL,      
                     my_real* THKN,   my_real*SIGNXX, my_real*SIGNYY, my_real*SIGNXY, my_real*SIGNYZ,
                     my_real* SIGNZX, my_real*SIGVXX, my_real*SIGVYY, my_real*SIGVXY, my_real*SIGVYZ,
                     my_real* SIGVZX, my_real*DPLA );

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
         my_real* VAR96,int*SIZVAR96,my_real* VAR97,int*SIZVAR97,my_real* VAR98,int*SIZVAR98,my_real* VAR99,int*SIZVAR99);

void  eng_userlib_set_lawc_ (my_real*SIGNXX,  my_real*SIGNYY, my_real*SIGNXY, my_real* SIGNYZ,  my_real*SIGNZX,
                             my_real* SIGVXX, my_real*SIGVYY, my_real*SIGVXY, my_real*SIGVYZ,   my_real*SIGVZX,
                             my_real* DPLA,   my_real*ETSE,   my_real* THKN );

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
         my_real* VAR96,int*SIZVAR96,my_real* VAR97,int*SIZVAR97,my_real* VAR98,int*SIZVAR98,my_real* VAR99,int*SIZVAR99);

void  eng_userlib_sigeps99_( int*NEL         ,int*NUPARAM      ,int*NUVAR       ,int*ILAW_USER   ,int*NFUNC,
                             int*IFUNC       ,int*NPF          ,my_real*TF      ,my_real*TIME    ,my_real*TIMESTEP,
                             my_real*UPARAM  ,my_real*RHO      ,my_real*VOLUME  ,my_real*EINT    ,int*NGL,
                             my_real*SOUNDSP ,my_real*VISCMAX  ,my_real*UVAR    ,my_real*OFF     ,my_real*SIGY,
                             my_real*PLA );

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
         my_real*VAR96,int *SIZVAR96,my_real*VAR97,int *SIZVAR97,my_real*VAR98,int *SIZVAR98,my_real*VAR99,int *SIZVAR99);

void eng_userlib_set_law_var_(  my_real*S1,  my_real*S2,   my_real*S3,  my_real*S4,  my_real*S5,  my_real*S6,
                                my_real*SV1, my_real*SV2,  my_real*SV3, my_real*SV4, my_real*SV5, my_real*SV6,
                                my_real*DPLA  );

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
         my_real*VAR96,int *SIZVAR96,my_real*VAR97,int *SIZVAR97,my_real*VAR98,int *SIZVAR98,my_real*VAR99,int *SIZVAR99);

void  eng_userlib_flaw_(int*IRUP,
           int *NEL        ,int*NUPARAM     ,int *NUVAR      ,int*NFUNC         ,int *IFUNC        ,
           int *NPF        ,my_real *TF     ,my_real *TIME   ,my_real *TIMESTEP ,my_real *UPARAM  ,
           int *NGL        ,int*IPM         ,int *NPROPMI    ,int *MAT          ,int *IDEL7NOK,
           my_real *EPSPXX ,my_real *EPSPYY ,my_real *EPSPZZ ,my_real *EPSPXY   ,my_real *EPSPYZ ,my_real *EPSPZX ,
           my_real *EPSXX  ,my_real *EPSYY  ,my_real *EPSZZ  ,my_real *EPSXY    ,my_real *EPSYZ  ,my_real *EPSZX  ,
           my_real *SIGNXX ,my_real *SIGNYY ,my_real *SIGNZZ ,my_real *SIGNXY   ,my_real *SIGNYZ ,my_real *SIGNZX ,
           my_real *PLA    ,my_real *DPLA   ,my_real *EPSP   ,my_real *UVAR     ,my_real *OFF    ,
           my_real *BIDON1 ,my_real *BIDON2 ,my_real *BIDON3 ,my_real *BIDON4   ,my_real *BIDON5);


void _FCALL eng_userlib_suser_(int*ITYP,
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

void _FCALL engine_userlib_cuser(
      int *igtyp      ,int*NEL         ,int * NNOD      ,
      int*NUVAR       ,int*IPROP       ,int*IMAT        ,int*SOLID_ID    ,my_real *TIME    ,my_real *TIMESTEP,
      my_real *EINT   ,my_real *VOL    ,my_real *UVAR   ,my_real *FR_WAVE,my_real *OFF    ,my_real *RHO    ,my_real *SIG    ,
      my_real *XX     ,my_real *YY     ,my_real *ZZ     ,my_real *UX     ,my_real *UY     ,my_real *UZ     ,
      my_real *VX     ,my_real *VY     ,my_real *VZ     ,my_real *VRX    ,my_real *VRY    ,my_real *VRZ    ,
      my_real *FX     ,my_real *FY     ,my_real *FZ     ,my_real *MX     ,my_real *MY     ,my_real *MZ     ,
      my_real *STIFM  ,my_real *STIFR  ,my_real *VISCM  ,my_real *VISCR  ,int * api_return);
