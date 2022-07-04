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
#include <userlib.h>



#ifdef _WIN64

/* -----------------------------------------------------------------------------------------
   MDS_USERLIB_INIT load MDS library & initialize routines
   -----------------------------------------------------------------------------------------
input  : int * iresp     - Singe Precision flag
output : int * mds_avail - MDS availibility 1 (ok) or 0  (error/no)
output : int * mds_ver   - MDS lib interface version or -1 if error
----------------------------------------------------------------------------------------- */
void _FCALL MDS_USERLIB_INIT (int * iresp,
                int * mds_avail, 
                int * mds_ver,
                char *mds_path,
                int  *mds_path_len  ) {

        char libname[4096];
        char dllpath[10240],dllname[15000];
        char *arch="win64";
        char altair_home[10240];
        int libname_size;
        int altair_home_size,arch_size;

        int i;     
        char rname[256];
        int result,dllpath_size;
        int err;
        int has_path;
        err=0;
        *mds_avail=0;
        mds_array_init_();

        libname[0]='\0';
        strcat(libname,"librad_mds");
        strcat(libname,ULIB_ARCH);
        strcat(libname,ULIB_EXT);

        if(*iresp==1){
                strcat(libname,ULIB_SP);
        }

        libname_size=strlen(libname);

        mds_userlib_name_set(libname);

        /* 
           --------------
           Load Library 
           --------------
         */

        mds_userlibhandler=NULL;

        /* First Command line argument -mds_libpath
           ------------------------------- */
        if(mds_userlibhandler==NULL && *mds_path_len !=0 ){


                for (i=0;i<*mds_path_len;i++) { dllname[i]=mds_path[i] ; }
                dllname[*mds_path_len]='\0';

                if ( dllname[*mds_path_len-1]!='\\') strcat(dllname,"\\");
                strcat(dllname,libname);

                mds_userlibhandler = LoadLibrary(TEXT(dllname));
        }

        /* Second Look in RAD_MDS_LIBPATH 
           ---------------------------- */
        if (!mds_userlibhandler){

                dllpath_size=GetEnvironmentVariable("RAD_MDS_LIBPATH",dllpath,10240);
                if (dllpath_size > 0){
                        strcpy(dllname,dllpath);
                        strcat(dllname,"\\");
                        strcat(dllname,libname);
                        mds_userlibhandler = LoadLibrary(TEXT(dllname));
                }
        }

        /* Third : find in local directory 
           ------------------------------- */
        if (!mds_userlibhandler){
                dllpath_size=GetCurrentDirectory(10240,dllpath);
                dllname[0]='\0';
                strcpy(dllname,dllpath);
                strcat(dllname,"\\");
                strcat(dllname,libname);
                mds_userlibhandler = LoadLibrary(TEXT(dllname));
        }

        /* Fourth find in $ALTAIR_HOME/hwsolvers/MultiscaleDesigner 
           ---------------------------------------------------------- */
        if(!mds_userlibhandler){

                altair_home_size=GetEnvironmentVariable("ALTAIR_TOP",altair_home,10240);

                if (altair_home_size > 0 && arch_size > 0){

                        strcpy(dllname,altair_home);

                        if ( dllname[altair_home_size-1]!='\\') strcat(dllname,"\\");
                        strcat(dllname,"hwsolvers\\MultiscaleDesigner\\");
                        strcat(dllname,arch);
                        strcat(dllname,"\\plugins\\radioss\\");
                        strcat(dllname,libname);
                        mds_userlibhandler = LoadLibrary(TEXT(dllname));
                }
        }


        if (!mds_userlibhandler){
                /* Fail to load library */
                *mds_avail=0;
                *mds_ver=-1;
                return;
        }   


        if (mds_userlibhandler){


                /* Routine MAT_MDS_C */
                sprintf(rname,"MAT_MDS_C");
                eng_mds_c=(void*)GetProcAddress(mds_userlibhandler,rname);
                if (!eng_mds_c) err=err+1;

                /* Routine MAT_MDS_S */
                sprintf(rname,"MAT_MDS_S");
                eng_mds_s=(void*)GetProcAddress(mds_userlibhandler,rname);
                if (!eng_mds_s) err=err+1;

                sprintf(rname,"MDS_ENGINE_USER_INITIALIZE");
                mds_eng_user_initialize=GetProcAddress(mds_userlibhandler,rname);
                if (!mds_eng_user_initialize) err=err+1;

                sprintf(rname,"MDS_ENGINE_USER_FINALIZE");
                mds_eng_user_finalize=GetProcAddress(mds_userlibhandler,rname);
                if (!mds_eng_user_finalize) err=err+1;

                sprintf(rname,"MDS_ENGINE_USER_CHECK");
                mds_eng_user_check=GetProcAddress(mds_userlibhandler,rname);
                if (!mds_eng_user_check) err=err+1;/* System - Library ID Version */

                sprintf(rname,"userlib_id");
                mds_userlib_id=(void*)GetProcAddress(mds_userlibhandler,rname);
                if(mds_userlib_id) {
                        mds_userlib_id(mds_ver);
                }else{
                        err = err+1; 
                }


                /* System - Windows Callback Routine */
                sprintf(rname,"set_callback");
                set_mds_callback=(void*)GetProcAddress(mds_userlibhandler,rname);
                if(set_mds_callback) {
                        mds_init_callback();
                }else{
                        err = err+1; 
                }
                if (err==0){  *mds_avail = 1;}
                else{
                        mds_avail = 0;
                }

        }else{
                mds_avail = 0;
        }
}


void mds_init_callback()
{
        void * callback_array[200];

        init_callback(callback_array);
        set_mds_callback(callback_array);
}


#elif 1

/* -----------------------------------------------------------------------------------------
   mds_userlib_init_ load MDS library & initialize routines / Linux
   -----------------------------------------------------------------------------------------
input  : int * iresp     - Singe Precision flag
output : int * mds_avail - MDS availibility 1 (ok) or 0  (error/no)
output : int * mds_ver   - MDS lib interface version or -1 if error
----------------------------------------------------------------------------------------- */
void _FCALL mds_userlib_init_ (int * iresp,
                int * mds_avail, 
                int * mds_ver,
                char* mds_path,
                int * mds_path_len )
{
        char *rad_userlib_libpath,*altair_home;
        char *arch="linux64";
        char libname[4096];
        char dllpath[10240],load_libname[15000];
        int libname_size,altair_home_len;
        struct stat buffer;
        int i;     
        char rname[256];
        int err;

        err=0;
        *mds_avail=0;
        *mds_ver=0;

        mds_array_init_();

        libname[0]='\0';
        strcat(libname,"librad_mds");
        strcat(libname,ULIB_ARCH);
        strcat(libname,ULIB_EXT);

        if(*iresp==1){
                strcat(libname,ULIB_SP);
        }

        libname_size=strlen(libname);

        mds_userlib_name_set(libname);

        mds_userlibhandle = NULL;



        /* First Command line argument -mds_libpath
           ------------------------------- */
        if(mds_userlibhandle==NULL && *mds_path_len !=0 ){
                for (i=0;i<*mds_path_len;i++) { load_libname[i]=mds_path[i] ; }
                load_libname[*mds_path_len]='\0';
                if ( load_libname[*mds_path_len-1]!='/') strcat(load_libname,"/");
                strcat(load_libname,libname);
                if(stat(load_libname,&buffer) == 0) 
                {
                        mds_userlibhandle = dlopen(load_libname,RTLD_LAZY|RTLD_GLOBAL|RTLD_DEEPBIND);
                }
        }

        /* Second find in RAD_USERLIB_LIBPATH
           ------------------------------- */
        if(mds_userlibhandle==NULL){
                rad_userlib_libpath=getenv("RAD_MDS_LIBPATH");
                if (rad_userlib_libpath){
                        strcpy(load_libname,rad_userlib_libpath);
                        strcat(load_libname,"/");
                        strcat(load_libname,libname);
                        if(stat(load_libname,&buffer) == 0) 
                        {
                                mds_userlibhandle = dlopen(load_libname,RTLD_LAZY|RTLD_GLOBAL|RTLD_DEEPBIND);
                        }
                }
        }

        /* Third find in local directory 
           ------------------------------- */

        if(mds_userlibhandle==NULL){
                getcwd(load_libname,15000);
                strcat(load_libname,"/");
                strcat(load_libname,libname);
                if(stat(load_libname,&buffer) == 0) 
                {
                        mds_userlibhandle = dlopen(load_libname,RTLD_LAZY|RTLD_GLOBAL|RTLD_DEEPBIND);
                }
        }


        /* Fourth find in $ALTAIR_HOME/hwsolvers/MultiscaleDesigner 
           ---------------------------------------------------------- */
        if(mds_userlibhandle==NULL){
                altair_home=getenv("ALTAIR_TOP");

                if (altair_home != NULL && arch != NULL){
                        strcpy(load_libname,altair_home);
                        altair_home_len=strlen(altair_home);
                        if ( load_libname[altair_home_len-1]!='/') strcat(load_libname,"/");
                        strcat(load_libname,"hwsolvers/MultiscaleDesigner/");
                        strcat(load_libname,arch);
                        strcat(load_libname,"/plugins/radioss/");
                        strcat(load_libname,libname);
                        if(stat(load_libname,&buffer) == 0) 
                        {
                                mds_userlibhandle = dlopen(load_libname,RTLD_LAZY|RTLD_GLOBAL|RTLD_DEEPBIND);
                        }
                }
        }


        if (!mds_userlibhandle){
                /* Fail to load library */
                *mds_avail=0;
                *mds_ver=-1;
                return;
        }


        if (mds_userlibhandle) {

                sprintf(rname,"mat_mds_c_");
                eng_mds_c=dlsym(mds_userlibhandle,rname);
                if (!eng_mds_c) err=err+1;

                sprintf(rname,"mat_mds_s_");
                eng_mds_s=dlsym(mds_userlibhandle,rname);
                if (!eng_mds_s) err=err+1;

                sprintf(rname,"mds_engine_user_initialize_");
                mds_eng_user_initialize=dlsym(mds_userlibhandle,rname);
                if (!mds_eng_user_initialize) err=err+1;

                sprintf(rname,"mds_engine_user_finalize_");
                mds_eng_user_finalize=dlsym(mds_userlibhandle,rname);
                if (!mds_eng_user_finalize) err=err+1;

                sprintf(rname,"mds_engine_user_check_");
                mds_eng_user_check=dlsym(mds_userlibhandle,rname);
                if (!mds_eng_user_check) err=err+1;

                /* System - Library ID Version */
                sprintf(rname,"userlib_id");
                mds_userlib_id=dlsym(mds_userlibhandle,rname);
                if(mds_userlib_id) {
                        mds_userlib_id(mds_ver);
                }else{
                        err = err+1; 
                }

                if (err==0){  *mds_avail = 1;}

        }else{
                *mds_avail = 0;
                /*        fputs (dlerror(), stderr);*/
                /*        printf("\n");*/
        }

}
#endif


/* -------------
   MDS Userlaw  
   ------------- */

/* WINDOWS */
#ifdef _WIN64
#define  eng_mds_sigeps_c_  ENG_MDS_SIGEPS_C
#endif
/* LINUX */
void _FCALL eng_mds_sigeps_c_(  int * NCYCLE, int * MID, int * ELEMENT_UID ,
                int * NEL   , int * NPT, int * IT ,int * ILAY,int *  IPG   ,int *  IFLAG,
                my_real *UPARAM , int *NUPARAM  , my_real *UVAR ,int *NUVAR,
                int *NFUNC  , int *IFUNC , my_real FUNCTION_ARRAY ,int * NPF ,
                my_real * TIME, my_real *TIMESTEP , my_real * RHO0, my_real *AREA, my_real *EINT,
                my_real * THKLY   ,my_real * THK     ,my_real * SHF     ,my_real * ETSE    ,
                my_real * EPSPXX  ,my_real * EPSPYY  ,my_real * EPSPXY  ,my_real * EPSPYZ  ,my_real * EPSPZX  ,
                my_real * DEPSXX  ,my_real * DEPSYY  ,my_real * DEPSXY  ,my_real * DEPSYZ  ,my_real * DEPSZX  ,
                my_real * EPSXX   ,my_real * EPSYY   ,my_real * EPSXY   ,my_real * EPSYZ   ,
                my_real * EPSZX   ,my_real * SOUNDSP ,my_real * VISCMAX ,my_real * PLA     ,my_real * OFF     ,
                my_real * TEMP    ,my_real * R11     ,my_real * R12     ,my_real * R13     ,my_real * R21     ,
                my_real * R22     ,my_real * R23     ,my_real * R31     ,my_real * R32     ,my_real * R33     ,
                my_real * SIGY    ,my_real * SIGOXX  ,my_real * SIGOYY  ,my_real * SIGOXY  ,my_real * SIGOYZ  ,
                my_real * SIGOZX  ,my_real * SIGNXX  ,my_real * SIGNYY  ,my_real * SIGNXY  ,
                my_real * SIGNYZ  ,my_real * SIGNZX  ,my_real * SIGVXX  ,my_real * SIGVYY  ,my_real * SIGVXY  ,
                my_real * SIGVYZ  ,my_real * SIGVZX  ,my_real * DPLA     ,
                my_real * ADDITIONAL_FLT_PARAMETERS,my_real * ADDITIONAL_INT_PARAMETERS)
{
        if (eng_mds_c){
                (*eng_mds_c)  ( NCYCLE  , MID     , ELEMENT_UID ,
                                NEL     , NPT     , IT      , ILAY , IPG , IFLAG,
                                UPARAM  , NUPARAM , UVAR    , NUVAR,
                                NFUNC   , IFUNC   , FUNCTION_ARRAY    , NPF ,
                                TIME    , TIMESTEP, RHO0    , AREA    , EINT,
                                THKLY   , THK     , SHF     , ETSE    ,
                                EPSPXX  , EPSPYY  , EPSPXY  , EPSPYZ  , EPSPZX  ,
                                DEPSXX  , DEPSYY  , DEPSXY  , DEPSYZ  , DEPSZX  ,
                                EPSXX   , EPSYY   , EPSXY   , EPSYZ   ,
                                EPSZX   , SOUNDSP , VISCMAX , PLA     , OFF     ,
                                TEMP    , R11     , R12     , R13     , R21     ,
                                R22     , R23     , R31     , R32     , R33     ,
                                SIGY    , SIGOXX  , SIGOYY  , SIGOXY  , SIGOYZ  ,
                                SIGOZX  , SIGNXX  , SIGNYY  , SIGNXY  ,
                                SIGNYZ  , SIGNZX  , SIGVXX  , SIGVYY  , SIGVXY  ,
                                SIGVYZ  , SIGVZX  , DPLA,
                                ADDITIONAL_FLT_PARAMETERS, ADDITIONAL_INT_PARAMETERS  );
        }
}


#ifdef _WIN64
#define  eng_mds_sigeps_s_  ENG_MDS_SIGEPS_S
#endif
void _FCALL eng_mds_sigeps_s_ (  int *  NCYCLE      ,int * ID   ,int * ELEMENT_UID, int *  NEL    ,
                int * IPTR         ,int * IPTS ,int * IPTT, int * NUPARAM  ,int *NUVAR    ,int *NFUNC    ,int *IFUNC  ,
                int * NPF          ,my_real * FUNCTION_ARRAY,my_real * TIME   ,my_real * TIMESTEP ,my_real * UPARAM   ,my_real * UVAR  ,
                my_real * RHO0     ,my_real * RHO      ,my_real * VOLUME   ,my_real * EINT     ,my_real * TEMP     ,my_real * AMU      ,
                my_real * EPSPXX   ,my_real * EPSPYY   ,my_real * EPSPZZ   ,my_real * EPSPXY   ,my_real * EPSPYZ   ,my_real * EPSPZX   ,
                my_real * DEPSXX   ,my_real * DEPSYY   ,my_real * DEPSZZ   ,my_real * DEPSXY   ,my_real * DEPSYZ   ,my_real * DEPSZX   ,
                my_real * EPSXX    ,my_real * EPSYY    ,my_real * EPSZZ    ,my_real * EPSXY    ,my_real * EPSYZ    ,my_real * EPSZX    ,
                my_real * SIGOXX   ,my_real * SIGOYY   ,my_real * SIGOZZ   ,my_real * SIGOXY   ,my_real * SIGOYZ   ,my_real * SIGOZX   ,
                my_real * SIGNXX   ,my_real * SIGNYY   ,my_real * SIGNZZ   ,my_real * SIGNXY   ,my_real * SIGNYZ   ,my_real * SIGNZX   ,
                my_real * SIGVXX   ,my_real * SIGVYY   ,my_real * SIGVZZ   ,my_real * SIGVXY   ,my_real * SIGVYZ   ,my_real * SIGVZX   ,
                my_real * SOUNDSP  ,my_real * VISCMAX  ,my_real * OFF      ,my_real * R11      ,my_real * R12      ,my_real * R13      ,
                my_real * R21      ,my_real * R22      ,my_real * R23      ,my_real * R31      ,my_real * R32      ,my_real * R33      ,
                int * ISMSTR   ,my_real * ISMSTR10_ARRAYS              ,my_real * PLA      ,my_real * DPLA     ,my_real * SIGY     ,
                my_real * ADDITIONAL_FLT_PARAMETERS, int * ADDITIONAL_INT_PARAMETERS ) {

        if (eng_mds_s ) {
                (*eng_mds_s ) (  NCYCLE ,ID, ELEMENT_UID,NEL    ,
                                IPTR     , IPTS     , IPTT     , NUPARAM  ,NUVAR     ,NFUNC     ,IFUNC  ,
                                NPF      , FUNCTION_ARRAY      , TIME     , TIMESTEP , UPARAM   , UVAR  ,
                                RHO0     , RHO      , VOLUME   , EINT     , TEMP     , AMU      ,
                                EPSPXX   , EPSPYY   , EPSPZZ   , EPSPXY   , EPSPYZ   , EPSPZX   ,
                                DEPSXX   , DEPSYY   , DEPSZZ   , DEPSXY   , DEPSYZ   , DEPSZX   ,
                                EPSXX    , EPSYY    , EPSZZ    , EPSXY    , EPSYZ    , EPSZX    ,
                                SIGOXX   , SIGOYY   , SIGOZZ   , SIGOXY   , SIGOYZ   , SIGOZX   ,
                                SIGNXX   , SIGNYY   , SIGNZZ   , SIGNXY   , SIGNYZ   , SIGNZX   ,
                                SIGVXX   , SIGVYY   , SIGVZZ   , SIGVXY   , SIGVYZ   , SIGVZX   ,
                                SOUNDSP  , VISCMAX  , OFF      , R11      , R12      , R13      ,
                                R21      , R22      , R23      , R31      , R32      , R33      ,
                                ISMSTR   , ISMSTR10_ARRAYS     ,PLA      , DPLA      , SIGY     ,
                                ADDITIONAL_FLT_PARAMETERS,  ADDITIONAL_INT_PARAMETERS ) ;

        } 
}

void mds_userlib_name_set(char * libname){ 
        int len_libname=strlen(libname);
        mds_userlib_name=malloc(sizeof(char)*(len_libname+1)) ;
        strcpy(mds_userlib_name, libname);
}

#ifdef _WIN64
#define mds_userlib_name_get_ MDS_USERLIB_NAME_GET
#endif
void _FCALL mds_userlib_name_get_(char * libname,int * length){
        *length = strlen(mds_userlib_name);
        strcpy(libname,mds_userlib_name);
}


#ifdef _WIN64
#define mds_engine_user_check_  MDS_ENGINE_USER_CHECK
#endif
void _FCALL mds_engine_user_check_(int *MY_RANK,double *TSTOP,int *NCYCLE,double *TT,int *MSTOP){

        if (*mds_eng_user_check){
                (mds_eng_user_check)( MY_RANK,TSTOP,NCYCLE,TT,MSTOP );
        }
}


#ifdef _WIN64
#define mds_engine_user_finalize_  MDS_ENGINE_USER_FINALIZE
#endif
void _FCALL mds_engine_user_finalize_( int *MY_RANK ){

        if (*mds_eng_user_finalize){
                (mds_eng_user_finalize)( MY_RANK );
        }
}


#ifdef _WIN64
#define mds_engine_user_initialize_  MDS_ENGINE_USER_INITIALIZE
#endif
void mds_engine_user_initialize_(int *NSPMD, int *NTHREAD, int *MY_RANK,double *TSTOP,
                int * MDS_NMAT, int * MDS_MATID, char * MDS_FILES, char * MDS_LABEL, int * MDS_NDEPSVAR,int * MDS_MAXDEPVAR,int *MDS_OUTPUT_TABLE){

        if (*mds_eng_user_initialize){
                (mds_eng_user_initialize)(NSPMD,NTHREAD,MY_RANK,TSTOP,
                                MDS_NMAT, MDS_MATID, MDS_FILES,  MDS_LABEL, MDS_NDEPSVAR,MDS_MAXDEPVAR,MDS_OUTPUT_TABLE);
        }
}
/* ----------------------------------------------- */
