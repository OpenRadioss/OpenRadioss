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
                char* mds_path,
                int * mds_path_len ) {

        char libname[4096];
        char dllpath[10240],dllname[15000];
        char altair_home[10240];
        char *arch="win64";
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

        mds_userlibhandle=NULL;


        /* First Command line argument -mds_libpath
           ------------------------------- */
        if(mds_userlibhandle==NULL && *mds_path_len !=0 ){

                for (i=0;i<*mds_path_len;i++) { dllname[i]=mds_path[i] ; }
                dllname[*mds_path_len]='\0';

                if ( dllname[*mds_path_len-1]!='/') strcat(dllname,"/");
                strcat(dllname,libname);

                mds_userlibhandle = LoadLibrary(TEXT(dllname));
        }

        /* Second : Look in RAD_MDS_LIBPATH 
           ----------------------------------- */
        if (!mds_userlibhandle){
                dllpath_size=GetEnvironmentVariable("RAD_MDS_LIBPATH",dllpath,10240);
                if (dllpath_size > 0){
                        strcpy(dllname,dllpath);
                        strcat(dllname,"\\");
                        strcat(dllname,libname);
                        mds_userlibhandle = LoadLibrary(TEXT(dllname));
                }
        }

        /* Third : find in local directory 
           ------------------------------- */
        if(!mds_userlibhandle){
                dllpath_size=GetCurrentDirectory(10240,dllpath);

                dllname[0]='\0';
                strcpy(dllname,dllpath);
                strcat(dllname,"\\");
                strcat(dllname,libname);
                mds_userlibhandle = LoadLibrary(TEXT(dllname));
        }


        /* Fourth find in $ALTAIR_HOME/hwsolvers/MultiscaleDesigner 
           ---------------------------------------------------------- */
        if(!mds_userlibhandle){

                altair_home_size=GetEnvironmentVariable("ALTAIR_TOP",altair_home,10240);

                if (altair_home_size > 0 && arch_size > 0){

                        strcpy(dllname,altair_home);

                        if ( dllname[altair_home_size-1]!='\\') strcat(dllname,"\\");
                        strcat(dllname,"hwsolvers\\MultiscaleDesigner\\");
                        strcat(dllname,arch);
                        strcat(dllname,"\\plugins\\radioss\\");
                        strcat(dllname,libname);
                        mds_userlibhandle = LoadLibrary(TEXT(dllname));
                }
        }



        if (!mds_userlibhandle){
                /* Fail to load library */
                *mds_avail=0;
                *mds_ver=-1;
                return;
        }   


        if (mds_userlibhandle){

                /* Routine _LECM */
                sprintf(rname,"MDS_READ");
                st_mds_lecm=(void*)GetProcAddress(mds_userlibhandle,rname);
                if (!st_mds_lecm) err=err+1;

                /* System - Library ID Version */
                sprintf(rname,"userlib_id");
                mds_userlib_id=(void*)GetProcAddress(mds_userlibhandle,rname);
                if(mds_userlib_id) {
                        mds_userlib_id( mds_ver);
                }else{
                        err = err+1; 
                }

                /* System - Windows Callback Routine */
                sprintf(rname,"set_callback");
                set_mds_callback=(void*)GetProcAddress(mds_userlibhandle,rname);

                if(set_mds_callback) {
                        mds_init_callback();
                }else{
                        err = err+1; 
                }

                if (err==0){  *mds_avail = 1;}
                else{
                        *mds_avail = 0;
                }

        }else{
                /*   printf("load unsuccessfull\n"); */
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
void mds_userlib_init_ (int * iresp,
                int * mds_avail, 
                int * mds_ver,
                char* mds_path,
                int * mds_path_len  )
{
        char *rad_userlib_libpath,*altair_home;
        char *arch="linux64";
        char libname[4096];
        char load_libname[15000];
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

        /* Second find in RAD_MDS_LIBPATH 
           ----------------------------------- */
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

                /* Routine ST_MDS_LECM */
                sprintf(rname,"mds_read_");
                st_mds_lecm=dlsym(mds_userlibhandle,rname);
                if (!st_mds_lecm) err=err+1;


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
void _FCALL ST_READ_MDS( my_real * uparam , int * maxuparam, int * nuparam,
                int * nuvar, int * ifunc,int * maxfunc,int * nfunc, 
                my_real * stifint, my_real * young, my_real * nu,my_real * rho0,
                char * files, char * label, int * ndepvar  )
{
        if (st_mds_lecm){
                (*st_mds_lecm)( uparam,maxuparam,nuparam,
                                nuvar,ifunc,maxfunc,nfunc,
                                stifint,young,nu,rho0,
                                files,label,ndepvar   );
        }
}

/* LINUX */
void st_read_mds_(  my_real * uparam , int * maxuparam, int * nuparam,
                int * nuvar, int * ifunc,int * maxfunc,int * nfunc, 
                my_real * stifint, my_real * young, my_real * nu,my_real * rho0,
                char * files, char * label, int * ndepvar )
{
        if (st_mds_lecm){
                (*st_mds_lecm)(uparam,maxuparam,nuparam,
                                nuvar,ifunc,maxfunc,nfunc,
                                stifint,young,nu,rho0,
                                files,label, ndepvar );
                printf("nuparam:%i\n", *nuparam);
                printf("ndepv:%i\n",*ndepvar);
        }
}


void mds_userlib_name_set(char * libname){ 
        int len_libname=strlen(libname);
        mds_userlib_name=malloc(sizeof(char)*(len_libname+1)) ;
        strcpy(mds_userlib_name, libname);
} 



void  _FCALL mds_userlib_name_get_(char * libname,int * length){
        *length = strlen(mds_userlib_name);
        strcpy(libname,mds_userlib_name);
}


