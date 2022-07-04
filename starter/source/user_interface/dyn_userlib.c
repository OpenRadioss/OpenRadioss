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

#if CPP_mach != CPP_macosx64
#include <malloc.h>
#endif


#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
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
void  (*st_lecm)        (int* ilaw ,char * rootn, int* rootlen,my_real* uparam ,int* maxuparam ,int* nuparam,int* nuvar,int* ifunc ,int* maxfunc ,int* mfunc ,my_real* parmat,int* unitab);
void  (*st_lecr)        (int* irupt ,char * rootn, int* rootlen,my_real* uparam ,int* maxuparam ,int* nuparam,int* nuvar,int* ifunc ,int* maxfunc ,int* mfunc ,int* id);
void  (*st_lecm99)      (int * ilaw,char * rootn,int* rootlen, int * iuser_law,my_real * uparam , int * maxuparam, int * nuparam,int * nuvar, int * ifunc,int * maxfunc,int * nfunc, my_real * parmat);
void  (*st_lecsen)      (int * typ,char * rootn,int* rootlen);
void  (*st_riniuser)    (int *igtyp,char * rootn,int* rootlen,int* nel,int* iprop,int * ix,my_real *xl,my_real *mass,my_real *xiner,my_real *stifn,my_real *stifr,my_real *vism  ,my_real *visr,my_real *uvar,int *nuvar);
void  (*st_userwis)     (char * rootn,int* rootlen,int*numnod,int * itab,my_real *x ,my_real *v,my_real *vr,my_real *mass,my_real *iner,int *nuvar ,int *nuvari  ,my_real *uvar ,int *iuvar );
void  (*st_userwis_ini) (char * rootn,int* rootlen,int*iuparam,int*numnod,int * itab,my_real *x ,my_real *v,my_real *vr,my_real *mass,my_real *iner,int *nuvar ,int *nuvari  ,my_real *uvar ,int *iuvar );
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

void (*st_get_userbuf_variable)(int * id, char * title);

void  (*userlib_id)(int * info);

void st_array_init_(){
	  int i;
           st_lecg        = NULL;
           st_lecguser    = NULL;
           st_lecm        = NULL;
           st_lecm99      = NULL;
           st_lecsen      = NULL;
           st_riniuser    = NULL;
           st_userwis     = NULL;
           st_userwis_ini = NULL;

	   userlibhandle = NULL;
	   userlib_id = NULL;
}
#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
void init_callback(void ** callback_array);
void dll_init_callback();

HINSTANCE userlibhandler;
void (*set_library_callback) (void * callback_array[200]);
void _FCALL DYN_USERLIB_INIT (char * libname, int *size, int * userlib_avail, int * userlib_ver ,int* iresp,int * userlib_altname){

     char* libn,current_dir;
     int i;     
     char rname[256];
     char dllpath[10240],dllname[15000];
     int result,dllpath_size;
     int err;
     int has_path;
     err=0;
     *userlib_avail=0;
     st_array_init_();

    if(*userlib_altname==0){
     if(*iresp==1){
       libname[*size   ]='w';  
       libname[*size+1 ]='i';
       libname[*size+2 ]='n';
       libname[*size+3 ]='6'; 
       libname[*size+4 ]='4';
       libname[*size+5 ]='_';
       libname[*size+6 ]='s';
       libname[*size+7 ]='p';
       libname[*size+8 ]='.';
       libname[*size+9 ]='d';
       libname[*size+10]='l';
       libname[*size+11]='l';
       *size = *size + 12;
     }else{     
       libname[*size  ]='w';  
       libname[*size+1]='i';
       libname[*size+2]='n';
       libname[*size+3]='6'; 
       libname[*size+4]='4';
       libname[*size+5]='.';
       libname[*size+6]='d';
       libname[*size+7]='l';
       libname[*size+8]='l';
       *size = *size + 9;
     }
    }
     libn = (char * )malloc(sizeof(char)* *size+1);
     for (i=0;i<*size;i++)libn[i]=libname[i];
     libn[*size]='\0';
     
/* it is possible to set a path when using alternate library names*/
     has_path=0;
     for (i=0;i<*size;i++){ 
       if (libn[i]==47 || libn[i]==92)has_path=1;
     }
     
     userlibhandler=NULL;
     if (has_path==1){
        userlibhandler = LoadLibrary(TEXT(libn));
     }else{
     
       /* first trial find Environment variable RAD_USERLIB_LIBPATH */
       dllpath_size=GetEnvironmentVariable("RAD_USERLIB_LIBPATH",dllpath,10240);
       if (dllpath_size > 0){
         strcpy(dllname,dllpath);
         strcat(dllname,"\\");
         strcat(dllname,libn);
         userlibhandler = LoadLibrary(TEXT(dllname));
       }
       
       if (!userlibhandler){
       /* second trial find Environment variable in local directory */
         dllpath_size=GetCurrentDirectory(10240,dllpath);
         strcpy(dllname,dllpath);
         strcat(dllname,"\\");
         strcat(dllname,libn);     
       userlibhandler = LoadLibrary(TEXT(dllname));
       }
            
       if (!userlibhandler){
       /* third trial find Environment variable in %PATH% */
       dllpath_size=GetEnvironmentVariable("PATH",dllpath,10240);
       SetDllDirectory(dllpath);
       userlibhandler = LoadLibrary(TEXT(libn));
       }
     
     }
     if (userlibhandler){

/* Routine ST_LECG */
         sprintf(rname,"ST_LECG");
         st_lecg=GetProcAddress(userlibhandler,rname);
	 if (!st_lecg) err=err+1;

/* Routine ST_LECGUSER */
         sprintf(rname,"ST_LECGUSER");
         st_lecguser=GetProcAddress(userlibhandler,rname);
	 if (!st_lecguser) err=err+1;

/* Routine ST_LECM */
         sprintf(rname,"ST_LECM");
         st_lecm=GetProcAddress(userlibhandler,rname);
	 if (!st_lecm) err=err+1;

/* Routine ST_LECR */
         sprintf(rname,"ST_LECR");
         st_lecr=GetProcAddress(userlibhandler,rname);
	 if (!st_lecr) err=err+1;

/* Routine ST_LECM99 */
         sprintf(rname,"ST_LECM99");
         st_lecm99=GetProcAddress(userlibhandler,rname);
	 if (!st_lecm99) err=err+1;

/* Routine ST_LECSEN */
         sprintf(rname,"ST_LECSEN");
         st_lecsen=GetProcAddress(userlibhandler,rname);
	 if (!st_lecsen) err=err+1;

/* Routine ST_RINIUSER */
         sprintf(rname,"ST_RINIUSER");
         st_riniuser=GetProcAddress(userlibhandler,rname);
	 if (!st_riniuser) err=err+1;

/* Routine ST_SINIUSR */
         sprintf(rname,"ST_SINIUSR");
         st_siniusr=GetProcAddress(userlibhandler,rname);
	 if (!st_siniusr) err=err+1;

/* Routine ST_USERWIS */
         sprintf(rname,"ST_USERWIS");
         st_userwis=GetProcAddress(userlibhandler,rname);
	 if (!st_userwis) err=err+1;

/* Routine ST_USERWIS_INI */
         sprintf(rname,"ST_USERWIS_INI");
         st_userwis_ini=GetProcAddress(userlibhandler,rname);
	 if (!st_userwis_ini) err=err+1;

/* Routine ST_GET_USERBUF_VARIABLE */
         sprintf(rname,"ST_GET_USERBUF_VAR");
         st_get_userbuf_variable=GetProcAddress(userlibhandler,rname);
	 if (!st_get_userbuf_variable) err=err+1;

/* System - Library ID Version */
         sprintf(rname,"userlib_id");
         userlib_id=GetProcAddress(userlibhandler,rname);
	 if(userlib_id) {
	    userlib_id(userlib_ver);
	    }else{
	    err = err+1; 
	 }

/* System - Windows Callback Routine */
         sprintf(rname,"set_callback");
         set_library_callback=GetProcAddress(userlibhandler,rname);
	 if(set_library_callback) {
	    dll_init_callback();
	    }else{
	    err = err+1; 
	 }
         if (err==0){  *userlib_avail = 1;}
            else{
                /*  printf("ko : %i \n",err); */
                }
	 
    }else{
      /*   printf("load unsuccessfull\n"); */
    }
}


void  dll_init_callback(){
 void * callback_array[200];
 init_callback(callback_array);
 fflush(stdout);
 set_library_callback(callback_array);
}

#elif 1

#if CPP_mach==CPP_macosx64

void dyn_userlib_init_(char * libname, int *size, int * userlib_avail, int * userlib_ver ,int* iresp,int * userlib_altname){
     char* libn;
     int i;     
     char rname[256];
     int err;
     err=0;
     *userlib_avail=0;
     *userlib_ver=0;
}

#elif 1
void dyn_userlib_init_(char * libname, int *size, int * userlib_avail, int * userlib_ver  ,int* iresp,int * userlib_altname){
     char* libn;
     int i,has_path,sz;     
     char rname[256],load_libname[15000];
     char libname_c[256];
     char * rad_userlib_libpath;
     int err;
     struct stat buffer;
     err=0;
     *userlib_avail=0;
     *userlib_ver=0;

     st_array_init_();

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
      
     }
#else
      if(*iresp==1){
       libname[*size   ]='l';
       libname[*size+1 ]='i';
       libname[*size+2 ]='n';
       libname[*size+3 ]='u';
       libname[*size+4 ]='x';
       libname[*size+5 ]='6';
       libname[*size+6 ]='4';
       libname[*size+7 ]='_';
       libname[*size+8 ]='s';
       libname[*size+9 ]='p';
       libname[*size+10]='.';
       libname[*size+11]='s';
       libname[*size+12]='o';
       *size = *size + 13;
      }else{
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
      }
     }
#endif
     
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
             if(stat(libn,&buffer) == 0)
             { 
                     userlibhandle = dlopen(libn,RTLD_LAZY|RTLD_GLOBAL|RTLD_DEEPBIND);
             }
     }else{

             /* first trial find Environment variable RAD_USERLIB_LIBPATH */
             rad_userlib_libpath=getenv("RAD_USERLIB_LIBPATH");
             if (rad_userlib_libpath){
                     strcpy(load_libname,rad_userlib_libpath);
                     strcat(load_libname,"/");
                     strcat(load_libname,libn);
                     if(stat(libn,&buffer) == 0)
                     {
                             userlibhandle = dlopen(load_libname,RTLD_LAZY|RTLD_GLOBAL|RTLD_DEEPBIND);
                     }
             }

             if(userlibhandle==NULL){
                     /* second trial search lib in Current Working directory */
                     getcwd(load_libname,15000);
                     strcat(load_libname,"/");
                     strcat(load_libname,libn);
                     if(stat(libn,&buffer) == 0)
                     {
                             userlibhandle = dlopen(load_libname,RTLD_LAZY|RTLD_GLOBAL|RTLD_DEEPBIND);
                     }
             }

             if(userlibhandle==NULL){
                     /* Third Trial : Default - LD_LRARY_PATH */
                     if(stat(libn,&buffer) == 0)
                     {
                             userlibhandle = dlopen(libn,RTLD_LAZY|RTLD_GLOBAL|RTLD_DEEPBIND);
                     }
             }

     }

     if (userlibhandle) {

/* Routine ST_LECG */
         sprintf(rname,"st_lecg_");
         st_lecg=dlsym(userlibhandle,rname);
	 if (!st_lecg) err=err+1;

/* Routine ST_LECGUSER */
         sprintf(rname,"st_lecguser_");
         st_lecguser=dlsym(userlibhandle,rname);
	 if (!st_lecguser) err=err+1;

/* Routine ST_LECM */
         sprintf(rname,"st_lecm_");
         st_lecm=dlsym(userlibhandle,rname);
	 if (!st_lecm) err=err+1;

/* Routine ST_LECR */
         sprintf(rname,"st_lecr_");
         st_lecr=dlsym(userlibhandle,rname);
	 if (!st_lecr) err=err+1;

/* Routine ST_LECM99 */
         sprintf(rname,"st_lecm99_");
         st_lecm99=dlsym(userlibhandle,rname);
	 if (!st_lecm99) err=err+1;

/* Routine ST_LECSEN */
         sprintf(rname,"st_lecsen_");
         st_lecsen=dlsym(userlibhandle,rname);
	 if (!st_lecsen) err=err+1;

/* Routine ST_RINIUSER */
         sprintf(rname,"st_riniuser_");
         st_riniuser=dlsym(userlibhandle,rname);
	 if (!st_riniuser) err=err+1;

/* Routine ST_SINIUSR */
         sprintf(rname,"st_siniusr_");
         st_siniusr=dlsym(userlibhandle,rname);
	 if (!st_siniusr) err=err+1;

/* Routine ST_USERWIS */
         sprintf(rname,"st_userwis_");
         st_userwis=dlsym(userlibhandle,rname);
	 if (!st_userwis) err=err+1;

/* Routine ST_USERWIS_INI */
         sprintf(rname,"st_userwis_ini_");
         st_userwis_ini=dlsym(userlibhandle,rname);
	 if (!st_userwis_ini) err=err+1;


/* System - Library ID Version */
         sprintf(rname,"userlib_id");
         userlib_id=dlsym(userlibhandle,rname);
	 if(userlib_id) {
	    userlib_id(userlib_ver);
	    }else{
	    err = err+1; 
	 }
       
         if (err==0){  *userlib_avail = 1;}

     }else{
       *userlib_avail = 0;
/*        fputs (dlerror(), stderr);*/
/*        printf("\n");*/
     }
    
}
#endif

#endif

/* --------------------------------- */
/* User properties lecg 29-30-31     */
/* --------------------------------- */

/* WINDOWS */
void _FCALL ST_USERLIB_LECG(int * igtyp,char * rootn,int* rootlen, int * nuvar,my_real * pargeo)
{
	  if (st_lecg) {
	    (*st_lecg)(igtyp,rootn,rootlen,nuvar,pargeo);
	  }
}


/* LINUX */
void st_userlib_lecg_(int * igtyp,char * rootn,int* rootlen, int * nuvar,my_real * pargeo)
{
	  if (st_lecg) {
	    (*st_lecg)(igtyp,rootn,rootlen,nuvar,pargeo);
	  }
}

/* ------------------------------------- */
/* User properties lecguser 29-30-31     */
/* ------------------------------------- */

/* WINDOWS */
void _FCALL ST_USERLIB_LECGUSER(int * igtyp,char * rootn,int* rootlen, int * nuvar,my_real * pargeo)
{
	  if (st_lecguser) {
	    (*st_lecguser)(igtyp,rootn,rootlen,nuvar,pargeo);
	  }
}


/* LINUX */
void st_userlib_lecguser_(int * igtyp,char * rootn,int* rootlen, int * nuvar,my_real * pargeo)
{
	  if (st_lecguser) {
	    (*st_lecguser)(igtyp,rootn,rootlen,nuvar,pargeo);
	  }
}


/* --------------------------------- */
/* User Material Laws 29-30-31       */
/* --------------------------------- */

/* WINDOWS */
void  _FCALL ST_USERLIB_USERMAT(MATNUM,ROOTN, ROOTLEN,UPARAM, MAXUPARAM, NUPARAM, NUVAR, IFUNC, MAXFUNC, MFUNC, PARMAT,UNITAB)
     int *MATNUM, *ROOTLEN,*MAXUPARAM, *NUPARAM, *NUVAR, *IFUNC, *MAXFUNC, *MFUNC, * UNITAB;
char * ROOTN;
my_real*UPARAM, *PARMAT;
{
	  if (st_lecm) {
	    (*st_lecm)(MATNUM, ROOTN, ROOTLEN,UPARAM, MAXUPARAM, NUPARAM, NUVAR, IFUNC, MAXFUNC, MFUNC, PARMAT,UNITAB);
	  }
}


/* LINUX */
void st_userlib_usermat_(MATNUM,ROOTN, ROOTLEN,UPARAM, MAXUPARAM, NUPARAM, NUVAR, IFUNC, MAXFUNC, MFUNC, PARMAT,UNITAB)
     int *MATNUM, *ROOTLEN,*MAXUPARAM, *NUPARAM, *NUVAR, *IFUNC, *MAXFUNC, *MFUNC, * UNITAB;
char * ROOTN;
my_real*UPARAM, *PARMAT;
{
	  if (st_lecm) {
	    (*st_lecm)(MATNUM, ROOTN, ROOTLEN,UPARAM, MAXUPARAM, NUPARAM, NUVAR, IFUNC, MAXFUNC, MFUNC, PARMAT,UNITAB);
	  }
}

/* --------------------------- */
/* Extended User Material Laws */
/* --------------------------- */

/* WINDOWS */
void _FCALL ST_USERLIB_LECM99(int * ilaw,char * rootn,int* rootlen, int * iuser_law,my_real * uparam , int * maxuparam, int * nuparam,int * nuvar, int * ifunc,int * maxfunc,int * nfunc, my_real * parmat)
{
	if (st_lecm99){
             (*st_lecm99)(ilaw,rootn,rootlen,iuser_law,uparam,maxuparam,nuparam,nuvar,ifunc,maxfunc,nfunc,parmat);
	}
}

/* LINUX */
void st_userlib_lecm99_(int * ilaw,char * rootn,int* rootlen, int * iuser_law,my_real * uparam , int * maxuparam, int * nuparam,int * nuvar, int * ifunc,int * maxfunc,int * nfunc, my_real * parmat)
{
        if (st_lecm99){
             (*st_lecm99)(ilaw,rootn,rootlen,iuser_law,uparam,maxuparam,nuparam,nuvar,ifunc,maxfunc,nfunc,parmat);
        }
}

/* ------------------ */
/* User Rupture       */
/* ------------------ */

/* WINDOWS */
void _FCALL ST_USERLIB_LECR(RUPTID,ROOTN, ROOTLEN,UPARAM, MAXUPARAM, NUPARAM, NUVAR, IFUNC, MAXFUNC, MFUNC, ID)
     int *RUPTID, *ROOTLEN,*MAXUPARAM, *NUPARAM, *NUVAR, *IFUNC, *MAXFUNC, *MFUNC, *ID;
char * ROOTN;
my_real*UPARAM;
{
	  if (st_lecr) {
	    (*st_lecr)(RUPTID, ROOTN, ROOTLEN,UPARAM, MAXUPARAM, NUPARAM, NUVAR, IFUNC, MAXFUNC, MFUNC, ID);
	  }
}

/* LINUX */
void st_userlib_lecr_(RUPTID,ROOTN, ROOTLEN,UPARAM, MAXUPARAM, NUPARAM, NUVAR, IFUNC, MAXFUNC, MFUNC, ID)
     int *RUPTID, *ROOTLEN,*MAXUPARAM, *NUPARAM, *NUVAR, *IFUNC, *MAXFUNC, *MFUNC, *ID;
char * ROOTN;
my_real*UPARAM;
{
	  if (st_lecr) {
	    (*st_lecr)(RUPTID, ROOTN, ROOTLEN,UPARAM, MAXUPARAM, NUPARAM, NUVAR, IFUNC, MAXFUNC, MFUNC, ID);
	  }
}


/* ------------ */
/* User Sensors */
/* ------------ */

/* WINDOWS */
void _FCALL ST_USERLIB_LECSEN(int * typ,char * rootn,int* rootlen)
{
	if (st_lecsen){
             (*st_lecsen)(typ,rootn,rootlen);
	}
}

/* LINUX */
void st_userlib_lecsen_(int * typ,char * rootn,int* rootlen)
{
        if (st_lecsen){
             (*st_lecsen)(typ,rootn,rootlen);
        }
}


/* ------------ */
/* User Springs */
/* ------------ */
/* WINDOWS */
void _FCALL ST_USERLIB_RINIUSER(int *igtyp,char * rootn,int* rootlen,int* nel,int* iprop,
                                int * ix,my_real *xl,my_real *mass,my_real *xiner,my_real *stifn,
                                my_real *stifr,my_real *vism  ,my_real *visr,my_real *uvar,int *nuvar)
{
	if (st_riniuser){
             (*st_riniuser)(igtyp,rootn,rootlen,nel,iprop,
                            ix,xl,mass,xiner,stifn,
                            stifr,vism  ,visr,uvar,nuvar );
	}
}

/* LINUX */
void st_userlib_riniuser_(int *igtyp,char * rootn,int* rootlen,int* nel,int* iprop,
                         int * ix,my_real *xl,my_real *mass,my_real *xiner,my_real *stifn,
                         my_real *stifr,my_real *vism  ,my_real *visr,my_real *uvar,int *nuvar)
{
	if (st_riniuser){
             (*st_riniuser)(igtyp,rootn,rootlen,nel,iprop,
                            ix,xl,mass,xiner,stifn,
                            stifr,vism  ,visr,uvar,nuvar );
	}
}

/* ------------ */
/* User Solids  */
/* ------------ */
/* WINDOWS */
void _FCALL ST_USERLIB_SINIUSR(int *ITYP,char *ROOTN,int *ROOTLEN,
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
    my_real *STIFM  ,my_real *STIFR  ,my_real *VISCM  ,my_real *VISCR )
{
	if (st_siniusr){
             (*st_siniusr)( ITYP,ROOTN,ROOTLEN,
    NEL	,NUVAR  ,IPROP  ,IMAT  ,SOLID_ID,TIME  ,TIMESTEP,
    EINT   ,VOL    ,UVAR   ,FR_WAVE,OFF    ,RHO    ,SIG    ,
    XX1    ,XX2    ,XX3    ,XX4    ,XX5    ,XX6    ,XX7    ,XX8    ,	 
    YY1    ,YY2    ,YY3    ,YY4    ,YY5    ,YY6    ,YY7    ,YY8    ,  
    ZZ1    ,ZZ2    ,ZZ3    ,ZZ4    ,ZZ5    ,ZZ6    ,ZZ7    ,ZZ8    ,
    UX1    ,UX2    ,UX3    ,UX4    ,UX5    ,UX6    ,UX7    ,UX8    ,
    UY1    ,UY2    ,UY3    ,UY4    ,UY5    ,UY6    ,UY7    ,UY8    ,
    UZ1    ,UZ2    ,UZ3    ,UZ4    ,UZ5    ,UZ6    ,UZ7    ,UZ8    ,
    VX1    ,VX2    ,VX3    ,VX4    ,VX5    ,VX6    ,VX7    ,VVX8    ,
    VY1    ,VY2    ,VY3    ,VY4    ,VY5    ,VY6    ,VY7    ,VVY8    ,
    VZ1    ,VZ2    ,VZ3    ,VZ4    ,VZ5    ,VZ6    ,VZ7    ,VVZ8    ,
    VRX1   ,VRX2   ,VRX3   ,VRX4   ,VRX5   ,VRX6   ,VRX7   ,VVRX8   ,
    VRY1   ,VRY2   ,VRY3   ,VRY4   ,VRY5   ,VRY6   ,VRY7   ,VVRY8   ,
    VRZ1   ,VRZ2   ,VRZ3   ,VRZ4   ,VRZ5   ,VRZ6   ,VRZ7   ,VVRZ8   ,
    FX1    ,FX2    ,FX3    ,FX4    ,FX5    ,FX6    ,FX7    ,VFX8    ,
    FY1    ,FY2    ,FY3    ,FY4    ,FY5    ,FY6    ,FY7    ,VFY8    ,
    FZ1    ,FZ2    ,FZ3    ,FZ4    ,FZ5    ,FZ6    ,FZ7    ,VFZ8    ,
    MX1    ,MX2    ,MX3    ,MX4    ,MX5    ,MX6    ,MX7    ,VMX8    ,
    MY1    ,MY2    ,MY3    ,MY4    ,MY5    ,MY6    ,MY7    ,MY8    ,
    MZ1    ,MZ2    ,MZ3    ,MZ4    ,MZ5    ,MZ6    ,MZ7    ,MZ8    ,
    STIFM  ,STIFR  ,VISCM  ,VISCR);
	}
}

/* LINUX */
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
    my_real *STIFM  ,my_real *STIFR  ,my_real *VISCM  ,my_real *VISCR )
{
	if (st_siniusr){
             (*st_siniusr)( ITYP,ROOTN,ROOTLEN,
    NEL	,NUVAR  ,IPROP  ,IMAT  ,SOLID_ID,TIME  ,TIMESTEP,
    EINT   ,VOL    ,UVAR   ,FR_WAVE,OFF    ,RHO    ,SIG    ,
    XX1    ,XX2    ,XX3    ,XX4    ,XX5    ,XX6    ,XX7    ,XX8    ,	 
    YY1    ,YY2    ,YY3    ,YY4    ,YY5    ,YY6    ,YY7    ,YY8    ,  
    ZZ1    ,ZZ2    ,ZZ3    ,ZZ4    ,ZZ5    ,ZZ6    ,ZZ7    ,ZZ8    ,
    UX1    ,UX2    ,UX3    ,UX4    ,UX5    ,UX6    ,UX7    ,UX8    ,
    UY1    ,UY2    ,UY3    ,UY4    ,UY5    ,UY6    ,UY7    ,UY8    ,
    UZ1    ,UZ2    ,UZ3    ,UZ4    ,UZ5    ,UZ6    ,UZ7    ,UZ8    ,
    VX1    ,VX2    ,VX3    ,VX4    ,VX5    ,VX6    ,VX7    ,VVX8    ,
    VY1    ,VY2    ,VY3    ,VY4    ,VY5    ,VY6    ,VY7    ,VVY8    ,
    VZ1    ,VZ2    ,VZ3    ,VZ4    ,VZ5    ,VZ6    ,VZ7    ,VVZ8    ,
    VRX1   ,VRX2   ,VRX3   ,VRX4   ,VRX5   ,VRX6   ,VRX7   ,VVRX8   ,
    VRY1   ,VRY2   ,VRY3   ,VRY4   ,VRY5   ,VRY6   ,VRY7   ,VVRY8   ,
    VRZ1   ,VRZ2   ,VRZ3   ,VRZ4   ,VRZ5   ,VRZ6   ,VRZ7   ,VVRZ8   ,
    FX1    ,FX2    ,FX3    ,FX4    ,FX5    ,FX6    ,FX7    ,VFX8    ,
    FY1    ,FY2    ,FY3    ,FY4    ,FY5    ,FY6    ,FY7    ,VFY8    ,
    FZ1    ,FZ2    ,FZ3    ,FZ4    ,FZ5    ,FZ6    ,FZ7    ,VFZ8    ,
    MX1    ,MX2    ,MX3    ,MX4    ,MX5    ,MX6    ,MX7    ,VMX8    ,
    MY1    ,MY2    ,MY3    ,MY4    ,MY5    ,MY6    ,MY7    ,MY8    ,
    MZ1    ,MZ2    ,MZ3    ,MZ4    ,MZ5    ,MZ6    ,MZ7    ,MZ8    ,
    STIFM  ,STIFR  ,VISCM  ,VISCR);
	}
}

/* -----------*/
/* UserWindow */
/* ---------- */
/* WINDOWS */
void _FCALL ST_USERLIB_USERWIS(char * rootn,int* rootlen,int*numnod,int * itab,my_real *x ,my_real *v,
                               my_real *vr,my_real *mass,my_real *iner,int *nuvar ,int *nuvari  ,
                               my_real *uvar ,int *iuvar)
{
	if (st_userwis){
             (*st_userwis)(rootn,rootlen,numnod,itab, x ,v,
                               vr,mass,iner,nuvar ,nuvari  ,
                               uvar ,iuvar);
	}
}
/* LINUX */
void st_userlib_userwis_(char * rootn,int* rootlen,int*numnod,int * itab,my_real *x ,my_real *v,
                               my_real *vr,my_real *mass,my_real *iner,int *nuvar ,int *nuvari  ,
                               my_real *uvar ,int *iuvar)
{
	if (st_userwis){
             (*st_userwis)(rootn,rootlen,numnod,itab, x ,v,
                               vr,mass,iner,nuvar ,nuvari  ,
                               uvar ,iuvar);
	}
}

/* --------------- */
/* UserWindow init */
/* --------------- */
/* WINDOWS */
void _FCALL ST_USERLIB_USERWIS_INI(char * rootn,int* rootlen,int *iuparam,int*numnod,int * itab,my_real *x ,my_real *v,
                               my_real *vr,my_real *mass,my_real *iner,int *nuvar ,int *nuvari  ,
                               my_real *uvar ,int *iuvar)
{
	if (st_userwis_ini){
             (*st_userwis_ini)(rootn,rootlen,iuparam,numnod,itab, x ,v,
                               vr,mass,iner,nuvar ,nuvari  ,
                               uvar ,iuvar);
	}
}
/* LINUX */
void st_userlib_userwis_ini_(char * rootn,int* rootlen,int * iuparam,int*numnod,int * itab,my_real *x ,my_real *v,
                               my_real *vr,my_real *mass,my_real *iner,int *nuvar ,int *nuvari  ,
                               my_real *uvar ,int *iuvar){
	if (st_userwis_ini){
             (*st_userwis_ini)(rootn,rootlen,iuparam,numnod,itab, x ,v,
                               vr,mass,iner,nuvar ,nuvari  ,
                               uvar ,iuvar);
	}
}

/* ----------------------------- */
/* Starter User Module info seek */
/* ----------------------------- */
/* WINDOWS */
void _FCALL ST_GET_USERBUF_VAR(int * id, char * title)
{
	if (st_get_userbuf_variable){
             (*st_get_userbuf_variable)(id,title);
	}
}
/* LINUX */
void st_get_userbuf_var_(int * id, char * title)
{
	if (st_get_userbuf_variable){
             (*st_get_userbuf_variable)(id,title);
	}
}




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
