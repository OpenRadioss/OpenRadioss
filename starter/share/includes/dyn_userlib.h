//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2026 Altair Engineering Inc.
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
#define _FCALL 
#elif 1
#include <sys/resource.h>
#include <sys/types.h>
#include <time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dlfcn.h>
#define _FCALL
#endif

#ifdef MYREAL4
#define my_real float
#endif

#ifdef MYREAL8
#define my_real double
#endif


void *userlibhandle;

void  (*st_lecg)        (int * igtyp,char * rootn,int* rootlen, int * nuvar,my_real * pargeo);

void  (*st_lecguser)    (int * igtyp,char * rootn,int* rootlen, int * nuvar,my_real * pargeo);

void  (*st_lecm)        (int* ilaw ,char * rootn, int* rootlen,my_real* uparam ,int* maxuparam ,
                         int* nuparam,int* nuvar,int* ifunc ,int* maxfunc ,int* mfunc ,
                         my_real* parmat,int* unitab);

void  (*st_lecr)        (int* irupt ,char * rootn, int* rootlen,my_real* uparam ,int* maxuparam ,
                         int* nuparam,int* nuvar,int* ifunc ,int* maxfunc ,int* mfunc ,int* id);

void  (*st_lecm99)      (int * ilaw,char * rootn,int* rootlen, int * iuser_law,my_real * uparam , int * maxuparam,
                         int * nuparam,int * nuvar, int * ifunc,int * maxfunc,int * nfunc, my_real * parmat);

void  (*st_lecsen)      (int * typ,char * rootn,int* rootlen);

void  (*st_riniuser)    (int *igtyp,char * rootn,int* rootlen,int* nel,int* iprop,int * ix,my_real *xl,
                         my_real *mass,my_real *xiner,my_real *stifn,my_real *stifr,my_real *vism  ,
                         my_real *visr,my_real *uvar,int *nuvar);

void  (*st_userwis)     (char * rootn,int* rootlen,int*numnod,int * itab,my_real *x ,my_real *v,
                         my_real *vr,my_real *mass,my_real *iner,int *nuvar ,int *nuvari  ,
                         my_real *uvar ,int *iuvar );

void  (*st_userwis_ini) (char * rootn,int* rootlen,int*iuparam,int*numnod,int * itab,my_real *x ,
                         my_real *v,my_real *vr,my_real *mass,my_real *iner,int *nuvar ,int *nuvari,
                         my_real *uvar ,int *iuvar );

void  (*st_siniusr)     (int *ITYP,char *ROOTN,int *ROOTLEN,
    int *NEL	,int *NUVAR  ,int *IPROP  ,int *IMAT  ,int *SOLID_ID,my_real *TIME  ,my_real * TIMESTEP,
    my_real *EINT   ,my_real *VOL    ,my_real *UVAR   ,my_real *FR_WAVE,my_real *OFF    ,my_real *RHO    ,my_real *SIG    ,
    my_real *XX1    ,my_real *XX2    ,my_real *XX3    ,my_real *XX4    ,my_real *XX5    ,my_real *XX6    ,my_real *XX7    ,my_real *XX8    ,	 
    my_real *YY1    ,my_real *YY2    ,my_real *YY3    ,my_real *YY4    ,my_real *YY5    ,my_real *YY6    ,my_real *YY7    ,my_real *YY8    ,  
    my_real *ZZ1    ,my_real *ZZ2    ,my_real *ZZ3    ,my_real *ZZ4    ,my_real *ZZ5    ,my_real *ZZ6    ,my_real *ZZ7    ,my_real *ZZ8    ,
    my_real *UX1    ,my_real *UX2    ,my_real *UX3    ,my_real *UX4    ,my_real *UX5    ,my_real *UX6    ,my_real *UX7    ,my_real *UX8    ,
    my_real *UY1    ,my_real *UY2    ,my_real *UY3    ,my_real *UY4    ,my_real *UY5    ,my_real *UY6    ,my_real *UY7    ,my_real *UY8    ,
    my_real *UZ1    ,my_real *UZ2    ,my_real *UZ3    ,my_real *UZ4    ,my_real *UZ5    ,my_real *UZ6    ,my_real *UZ7    ,my_real *UZ8    ,
    my_real *VX1    ,my_real *VX2    ,my_real *VX3    ,my_real *VX4    ,my_real *VX5    ,my_real *VX6    ,my_real *VX7    ,my_real *VVX8    ,
    my_real *VY1    ,my_real *VY2    ,my_real *VY3    ,my_real *VY4    ,my_real *VY5    ,my_real *VY6    ,my_real *VY7    ,my_real *VVY8    ,
    my_real *VZ1    ,my_real *VZ2    ,my_real *VZ3    ,my_real *VZ4    ,my_real *VZ5    ,my_real *VZ6    ,my_real *VZ7    ,my_real *VVZ8    ,
    my_real *VRX1   ,my_real *VRX2   ,my_real *VRX3   ,my_real *VRX4   ,my_real *VRX5   ,my_real *VRX6   ,my_real *VRX7   ,my_real *VVRX8   ,
    my_real *VRY1   ,my_real *VRY2   ,my_real *VRY3   ,my_real *VRY4   ,my_real *VRY5   ,my_real *VRY6   ,my_real *VRY7   ,my_real *VVRY8   ,
    my_real *VRZ1   ,my_real *VRZ2   ,my_real *VRZ3   ,my_real *VRZ4   ,my_real *VRZ5   ,my_real *VRZ6   ,my_real *VRZ7   ,my_real *VVRZ8   ,
    my_real *FX1    ,my_real *FX2    ,my_real *FX3    ,my_real *FX4    ,my_real *FX5    ,my_real *FX6    ,my_real *FX7    ,my_real *VFX8    ,
    my_real *FY1    ,my_real *FY2    ,my_real *FY3    ,my_real *FY4    ,my_real *FY5    ,my_real *FY6    ,my_real *FY7    ,my_real *VFY8    ,
    my_real *FZ1    ,my_real *FZ2    ,my_real *FZ3    ,my_real *FZ4    ,my_real *FZ5    ,my_real *FZ6    ,my_real *FZ7    ,my_real *VFZ8    ,
    my_real *MX1    ,my_real *MX2    ,my_real *MX3    ,my_real *MX4    ,my_real *MX5    ,my_real *MX6    ,my_real *MX7    ,my_real *VMX8    ,
    my_real *MY1    ,my_real *MY2    ,my_real *MY3    ,my_real *MY4    ,my_real *MY5    ,my_real *MY6    ,my_real *MY7    ,my_real *MY8    ,
    my_real *MZ1    ,my_real *MZ2    ,my_real *MZ3    ,my_real *MZ4    ,my_real *MZ5    ,my_real *MZ6    ,my_real *MZ7    ,my_real *MZ8    ,
    my_real *STIFM  ,my_real *STIFR  ,my_real *VISCM  ,my_real *VISCR  );


    void  (*cini29)     (
    int *NEL	,int * nnod,int *NUVAR  ,int *IPROP  ,int *IMAT  ,int *SOLID_ID,
    my_real *EINT   ,my_real *VOL    ,my_real *UVAR   ,my_real *OFF    ,my_real *RHO    ,my_real *SIG    ,
    my_real *XX     ,my_real *YY     , my_real *ZZ,    my_real *VX     ,my_real *VY     ,my_real *VZ   ,
    my_real *VRX     ,my_real*VRY    ,my_real *VRZ    ,my_real *MAS     ,my_real *INN    ,
    my_real *STIFM  ,my_real *STIFR  ,my_real *VISCM  ,my_real *VISCR  );

    void  (*cini30)     (
    int *NEL	,int * nnod,int *NUVAR  ,int *IPROP  ,int *IMAT  ,int *SOLID_ID,
    my_real *EINT   ,my_real *VOL    ,my_real *UVAR   ,my_real *OFF    ,my_real *RHO    ,my_real *SIG    ,
    my_real *XX     ,my_real *YY     , my_real *ZZ,    my_real *VX     ,my_real *VY     ,my_real *VZ   ,
    my_real *VRX     ,my_real*VRY    ,my_real *VRZ    ,my_real *MAS     ,my_real *INN    ,
    my_real *STIFM  ,my_real *STIFR  ,my_real *VISCM  ,my_real *VISCR  );

    void  (*cini31)     (
    int *NEL	,int * nnod,int *NUVAR  ,int *IPROP  ,int *IMAT  ,int *SOLID_ID,
    my_real *EINT   ,my_real *VOL    ,my_real *UVAR   ,my_real *OFF    ,my_real *RHO    ,my_real *SIG    ,
    my_real *XX     ,my_real *YY     , my_real *ZZ,    my_real *VX     ,my_real *VY     ,my_real *VZ   ,
    my_real *VRX     ,my_real*VRY    ,my_real *VRZ    ,my_real *MAS     ,my_real *INN    ,
    my_real *STIFM  ,my_real *STIFR  ,my_real *VISCM  ,my_real *VISCR  );

    void (*st_get_userbuf_variable)(int * id, char * title);

void  (*userlib_id)(int * info);


// Interface routines
#ifdef _WIN64
#define st_userlib_lecg_         ST_USERLIB_LECG
#define st_userlib_lecguser_     ST_USERLIB_LECGUSER
#define st_userlib_usermat_      ST_USERLIB_USERMAT
#define st_userlib_lecm99_       ST_USERLIB_LECM99
#define st_userlib_lecr_         ST_USERLIB_LECR
#define st_userlib_lecsen_       ST_USERLIB_LECSEN
#define st_userlib_riniuser_     ST_USERLIB_RINIUSER
#define st_userlib_siniusr_      ST_USERLIB_SINIUSR
#define st_userlib_userwis_      ST_USERLIB_USERWIS
#define st_userlib_userwis_ini_  ST_USERLIB_USERWIS_INI
#define st_get_userbuf_var_      ST_GET_USERBUF_VAR
#define delete_user_file_        DELETE_USER_FILE
#endif

// lecg29-30-31
void st_userlib_lecg_(int * igtyp,char * rootn,int* rootlen, int * nuvar,my_real * pargeo);
// lecguser29-30-31
void st_userlib_lecguser_(int * igtyp,char * rootn,int* rootlen, int * nuvar,my_real * pargeo);
// lecm 29-30-31
void st_userlib_usermat_(int *MATNUM,  char *ROOTN, int *ROOTLEN, my_real *UPARAM, int * MAXUPARAM,
                         int *NUPARAM, int *NUVAR,  int *IFUNC,   int * MAXFUNC,   int * MFUNC,
                         my_real *PARMAT,int *UNITAB);
//lecmuser 01-99
void st_userlib_lecm99_(int *ilaw,       char *rootn,    int* rootlen, int *iuser_law,
                        my_real *uparam, int *maxuparam, int *nuparam, int *nuvar, 
                        int     *ifunc,  int *maxfunc,   int *nfunc,   my_real *parmat);

// lecr29-31
void st_userlib_lecr_(int *RUPTID,  char *ROOTN, int *ROOTLEN, my_real *UPARAM, int *MAXUPARAM,
                      int *NUPARAM, int *NUVAR,  int *IFUNC,   int *MAXFUNC,    int *MFUNC, 
                      int *ID);

// lecsen29-30-31
void st_userlib_lecsen_(int * typ,char * rootn,int* rootlen);

// rini29-31
void st_userlib_riniuser_(int *igtyp,char * rootn,int* rootlen,int* nel,int* iprop,
                         int * ix,my_real *xl,my_real *mass,my_real *xiner,my_real *stifn,
                         my_real *stifr,my_real *vism  ,my_real *visr,my_real *uvar,int *nuvar);

// sini29-31
void st_userlib_siniusr_(int *ITYP,char *ROOTN,int *ROOTLEN,
    int *NEL	,int *NUVAR  ,int *IPROP  ,int *IMAT  ,int *SOLID_ID,my_real *TIME  ,my_real * TIMESTEP,
    my_real *EINT   ,my_real *VOL    ,my_real *UVAR   ,my_real *FR_WAVE,my_real *OFF    ,my_real *RHO    ,my_real *SIG    ,
    my_real *XX1    ,my_real *XX2    ,my_real *XX3    ,my_real *XX4    ,my_real *XX5    ,my_real *XX6    ,my_real *XX7    ,my_real *XX8    ,	 
    my_real *YY1    ,my_real *YY2    ,my_real *YY3    ,my_real *YY4    ,my_real *YY5    ,my_real *YY6    ,my_real *YY7    ,my_real *YY8    ,  
    my_real *ZZ1    ,my_real *ZZ2    ,my_real *ZZ3    ,my_real *ZZ4    ,my_real *ZZ5    ,my_real *ZZ6    ,my_real *ZZ7    ,my_real *ZZ8    ,
    my_real *UX1    ,my_real *UX2    ,my_real *UX3    ,my_real *UX4    ,my_real *UX5    ,my_real *UX6    ,my_real *UX7    ,my_real *UX8    ,
    my_real *UY1    ,my_real *UY2    ,my_real *UY3    ,my_real *UY4    ,my_real *UY5    ,my_real *UY6    ,my_real *UY7    ,my_real *UY8    ,
    my_real *UZ1    ,my_real *UZ2    ,my_real *UZ3    ,my_real *UZ4    ,my_real *UZ5    ,my_real *UZ6    ,my_real *UZ7    ,my_real *UZ8    ,
    my_real *VX1    ,my_real *VX2    ,my_real *VX3    ,my_real *VX4    ,my_real *VX5    ,my_real *VX6    ,my_real *VX7    ,my_real *VVX8    ,
    my_real *VY1    ,my_real *VY2    ,my_real *VY3    ,my_real *VY4    ,my_real *VY5    ,my_real *VY6    ,my_real *VY7    ,my_real *VVY8    ,
    my_real *VZ1    ,my_real *VZ2    ,my_real *VZ3    ,my_real *VZ4    ,my_real *VZ5    ,my_real *VZ6    ,my_real *VZ7    ,my_real *VVZ8    ,
    my_real *VRX1   ,my_real *VRX2   ,my_real *VRX3   ,my_real *VRX4   ,my_real *VRX5   ,my_real *VRX6   ,my_real *VRX7   ,my_real *VVRX8   ,
    my_real *VRY1   ,my_real *VRY2   ,my_real *VRY3   ,my_real *VRY4   ,my_real *VRY5   ,my_real *VRY6   ,my_real *VRY7   ,my_real *VVRY8   ,
    my_real *VRZ1   ,my_real *VRZ2   ,my_real *VRZ3   ,my_real *VRZ4   ,my_real *VRZ5   ,my_real *VRZ6   ,my_real *VRZ7   ,my_real *VVRZ8   ,
    my_real *FX1    ,my_real *FX2    ,my_real *FX3    ,my_real *FX4    ,my_real *FX5    ,my_real *FX6    ,my_real *FX7    ,my_real *VFX8    ,
    my_real *FY1    ,my_real *FY2    ,my_real *FY3    ,my_real *FY4    ,my_real *FY5    ,my_real *FY6    ,my_real *FY7    ,my_real *VFY8    ,
    my_real *FZ1    ,my_real *FZ2    ,my_real *FZ3    ,my_real *FZ4    ,my_real *FZ5    ,my_real *FZ6    ,my_real *FZ7    ,my_real *VFZ8    ,
    my_real *MX1    ,my_real *MX2    ,my_real *MX3    ,my_real *MX4    ,my_real *MX5    ,my_real *MX6    ,my_real *MX7    ,my_real *VMX8    ,
    my_real *MY1    ,my_real *MY2    ,my_real *MY3    ,my_real *MY4    ,my_real *MY5    ,my_real *MY6    ,my_real *MY7    ,my_real *MY8    ,
    my_real *MZ1    ,my_real *MZ2    ,my_real *MZ3    ,my_real *MZ4    ,my_real *MZ5    ,my_real *MZ6    ,my_real *MZ7    ,my_real *MZ8    ,
    my_real *STIFM  ,my_real *STIFR  ,my_real *VISCM  ,my_real *VISCR );


// User Shell initialization
void _FCALL st_userlib_cini(int * igtyp,
    int *nel	,int * nnod, int *nuvar  ,int *iprop  ,int *imat  ,int *solid_id,
    my_real *eint   ,my_real *vol    ,my_real *uvar   ,my_real *off    ,my_real *rho    ,my_real *sig    ,
    my_real *xx     ,my_real *yy     ,my_real *zz ,    my_real *vx     ,my_real *vy     ,my_real *vz ,
    my_real *vrx    ,my_real *vry    ,my_real *vrz  ,  my_real *mas    ,my_real *inn    ,
    my_real *stifm  ,my_real *stifr  ,my_real *viscm  ,my_real *viscr  , int * return_code);


// user windows
void st_userlib_userwis_(char * rootn,int* rootlen,int*numnod,int * itab,my_real *x ,my_real *v,
                               my_real *vr,my_real *mass,my_real *iner,int *nuvar ,int *nuvari  ,
                               my_real *uvar ,int *iuvar);


void st_userlib_userwis_ini_(char * rootn,int* rootlen,int * iuparam,int*numnod,int * itab,my_real *x ,my_real *v,
                               my_real *vr,my_real *mass,my_real *iner,int *nuvar ,int *nuvari  ,
                               my_real *uvar ,int *iuvar);

// userbuf
void st_get_userbuf_var_(int * id, char * title);

// delete file
void delete_user_file_(char * filnam,int * len);