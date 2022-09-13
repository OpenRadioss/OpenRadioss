Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
Copyright>    
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>    
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>    
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>    
Copyright>    
Copyright>        Commercial Alternative: Altair Radioss Software 
Copyright>    
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
Chd|====================================================================
Chd|  R2R_FORK                      source/coupling/rad2rad/r2r_fork.F
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CREATE_CHILD                  source/coupling/rad2rad/r2r_fork.F
Chd|        MY_FORK                       source/system/rad_sys_call.c  
Chd|        MY_WAITPID                    source/system/rad_sys_call.c  
Chd|        R2R_PRELEC_NAME               source/coupling/rad2rad/r2r_prelec_name.F
Chd|        READ_FLAG_ALE                 source/coupling/rad2rad/r2r_fork.F
Chd|        WIN_WAITPID                   source/coupling/rad2rad/r2r_fork.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE R2R_FORK(CHRUN,FILNAM,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE R2R_MOD 
      USE INOUTFILE_MOD
      USE QA_OUT_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "scr15_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER FILNAM*100,CHRUN*4,ROOT_SUB*80
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)               
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISTAT,KDOM,IPID_RET,IERROR,WAIT,IPID_L,STAT,DOM_SWITCH
      LOGICAL(4) WAITA       
#if defined(COMP_ARMFLANG) || defined(COMP_LLVM)
      INTEGER  GETPID
#endif
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=4096) :: TMP_NAME
C-----------------------------------------------

        KDOM = 0
        DOM_SWITCH = 0
        DOM_NAME = ''
        ROOTNAM0 = ''
        ROOTNAM0 = ROOTNAM(1:ROOTLEN)
        ALLOCATE (ISUBDOM(8,NSUBDOM),STAT=stat)

2000    CONTINUE
        
C----------------------------------------------------------------------
C----------------------------- WINDOWS --------------------------------
C----------------------------------------------------------------------         
#if CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
        IF (IPID/=0) CALL CREATE_CHILD(IPID_RET,FLG_SWALE)
C----------------------------------------------------------------------         
C-------------PGI -> LINUX / SUN -> SOLARISX64 / IBM -> AIX64----------
C----------------------------------------------------------------------                 
#elif CPP_mach == CPP_linux964 || CPP_mach == CPP_sun25 || CPP_mach == CPP_pwr4
        IPID = FORK()
#if defined(_OPENMP)
        IF (IPID==0) CALL OMP_SET_NUM_THREADS(NTHREAD_R2R)
#endif         
C----------------------------------------------------------------------         
C-------------------INTEL -> LINUX/LINUXIA64/MACOSX--------------------
C----------------------------------------------------------------------         
#else
#if defined (COMP_GFORTRAN) ||  defined(COMP_ARMFLANG) || defined(COMP_LLVM)
        CALL MY_FORK(IPID)
        IPID_RET = GETPID()
#else
        CALL PXFFORK(IPID,IERROR)
        CALL PXFGETPID(IPID_RET,IERROR)
#endif
#if defined(_OPENMP)
        IF (IPID==0) CALL OMP_SET_NUM_THREADS(NTHREAD_R2R)
#endif        
#endif
C----------------------------------------------------------------------         
C----------------------------------------------------------------------    
        KDOM = KDOM + 1
        IPID_L = IPID
C
        IF (IPID/=0) THEN     
C----------------------------------------------------------------------
C----------------------------- WINDOWS --------------------------------
C----------------------------------------------------------------------         
#if CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
            CALL WIN_WAITPID(IPID_RET)
C----------------------------------------------------------------------
C-------------PGI -> LINUX / SUN -> SOLARISX64 / IBM -> AIX64----------
C----------------------------------------------------------------------         
#elif CPP_mach == CPP_linux964 || CPP_mach == CPP_sun25 || CPP_mach == CPP_pwr4
            IERROR = WAIT(ISTAT)
C----------------------------------------------------------------------     
C-------------------INTEL -> LINUX/LINUXIA64/MACOSX--------------------
C----------------------------------------------------------------------                     
#else
#if defined(COMP_GFORTRAN) ||  defined(COMP_ARMFLANG) || defined(COMP_LLVM)
            CALL MY_WAITPID(IPID,ISTAT,0,IPID_RET)
#else
            CALL PXFWAITPID(IPID,ISTAT,0,IPID_RET,IERROR)
#endif

#endif
            CALL R2R_PRELEC_NAME(KDOM,LSUBMODEL)
            IDDOM = 0
            IDDOM_L = IDDOM
            CALL READ_FLAG_ALE(CHRUN)
            IF (FLG_FSI==1) THEN
              IF (FLG_SWALE<1) THEN
                FLG_SWALE = FLG_SWALE + 1
                DOM_SWITCH = KDOM
                KDOM = KDOM - 1
#if CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
                CLOSE(IOUT)
#endif
                GOTO 2000
              ELSE
                CALL ANCMSG(MSGID=1103,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO)
                IERR=IERR+1    
              ENDIF
            ELSEIF (KDOM<NSUBDOM) THEN
              GOTO 2000
            ENDIF
            IF (FLG_SWALE==1) THEN
               IPID_L = 0
               IDDOM_L = 1
            ENDIF     
C---------------------------------------------------------------------- 
C----------------------------------------------------------------------
        ELSE
            CALL R2R_PRELEC_NAME(KDOM,LSUBMODEL)
            IDDOM = KDOM
            IDDOM_L = IDDOM
            IF (FLG_SWALE==1) THEN
               IPID_L = 1
               IDDOM_L = 0
            ENDIF   
        ENDIF
C
        IF (IPID_L/=0) THEN    
            WRITE(ISTDO,'(A)') '-----------------------------------' 
            WRITE(ISTDO,'(A)')' .. TREATMENT OF THE FULL DOMAIN'
            WRITE(ISTDO,'(A)') '-----------------------------------'
            WRITE(ISTDO,'(A)') ''          
        ELSE
            ROOTNAM = DOM_NAME
            ROOTLEN = LEN_TRIM(ROOTNAM)         
c-----------Initialisations for the starter processes of the subdomains----
            FILNAM  =ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'.out'
            R2R_FILNAM = FILNAM
            TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//R2R_FILNAM(1:LEN_TRIM(R2R_FILNAM))    
            LEN_TMP_NAME = OUTFILE_NAME_LEN+LEN_TRIM(R2R_FILNAM)
            OPEN(UNIT=IOUT,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .         ACCESS='SEQUENTIAL',
     .         FORM='FORMATTED',STATUS='UNKNOWN')
C-----------Repointing of temporary files for error messages
            RES_MES = 123458
            RES_TMP = 123459
            OPEN (UNIT=RES_MES,STATUS='SCRATCH',FORM='FORMATTED')
c    
            WRITE(ISTDO,'(A)') ''      
            WRITE(ISTDO,'(A)') '-----------------------------------'     
            WRITE(ISTDO,'(A)')' .. TREATMENT OF SUBDOMAIN '
     .       //ROOTNAM(1:ROOTLEN)
            WRITE(ISTDO,'(A)') '-----------------------------------'
            WRITE(ISTDO,'(A)') ''
C-----------printout only for main domain
            doqa = 0
C
        ENDIF

C--------------------------------------------------------------------C                          
      RETURN
      END

#if CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
Chd|====================================================================
Chd|  WIN_WAITPID                   source/coupling/rad2rad/r2r_fork.F
Chd|-- called by -----------
Chd|        R2R_FORK                      source/coupling/rad2rad/r2r_fork.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE WIN_WAITPID(IPID_RET)
C----------------------------------------------- 
      USE DFWIN
      USE DFLIB    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
      INTEGER IPID_RET          
C-----------------------------------------------
      LOGICAL(4) WAITA
C-----------------------------------------------

      WAITA = WAITFORSINGLEOBJECT(IPID_RET,INFINITE)
      
      END

Chd|====================================================================
Chd|  CREATE_CHILD                  source/coupling/rad2rad/r2r_fork.F
Chd|-- called by -----------
Chd|        R2R_FORK                      source/coupling/rad2rad/r2r_fork.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        RAD2RAD_CREATEPROCESS         source/system/machine_c.c     
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CREATE_CHILD(ID,FLG_SWALE)
      USE MESSAGE_MOD
C----------------------------------------------- 
      USE DFWIN
      USE DFLIB    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr15_c.inc"
#include      "execinp.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ID,FLG_SWALE          
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------          
      LOGICAL(4) SUCCESS
      INTEGER PROCESS_ERROR,STATUS,LEN,I          
      INTEGER SIZEOFSTARTUPINFO,SIZESECURITYATTRIBUTES
      CHARACTER*2048 COMMAND,LAUNCH
      type(T_STARTUPINFO) si
      type(T_PROCESS_INFORMATION) tpi
C-----------------------------------------------

      SUCCESS = SETENVQQ("R2R_ENV_IPID=5")
      IF (FLG_SWALE>0) SUCCESS = SETENVQQ("R2R_ENV_SWALE=5")

      CALL GET_COMMAND(COMMAND,LEN,STATUS)
      
C-------Incompatibility of "<" remplaced by "-i" ----------------      
      IF (LEN_TRIM(INPUT)==0) THEN
        CALL ANCMSG(MSGID=840,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO)
        IERR=IERR+1      
      ENDIF


      PROCESS_ERROR=0
      CALL RAD2RAD_CREATEPROCESS(COMMAND,LEN,ID,PROCESS_ERROR)


      IF (PROCESS_ERROR==1) THEN          
         CALL MY_EXIT(2)                 
      ENDIF

      END
#endif

Chd|====================================================================
Chd|  READ_FLAG_ALE                 source/coupling/rad2rad/r2r_fork.F
Chd|-- called by -----------
Chd|        R2R_FORK                      source/coupling/rad2rad/r2r_fork.F
Chd|-- calls ---------------
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|====================================================================
      SUBROUTINE READ_FLAG_ALE(CHRUN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE R2R_MOD   
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER CHRUN*4
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER DOMLEN,REF,ERR
      CHARACTER NAM*150   
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=4096) :: TMP_NAME
C-----------------------------------------------

      REF = 991982

      NAM=DOM_NAME(ISUBDOM(8,1):ISUBDOM(8,1)+
     .               ISUBDOM(7,1)-1)//'_'//CHRUN//'.domdec'
      DOMLEN = ISUBDOM(7,1)+12        
            
      TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//NAM(1:LEN_TRIM(NAM))    
      LEN_TMP_NAME = OUTFILE_NAME_LEN+LEN_TRIM(NAM)
      OPEN(UNIT=REF,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .      ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='UNKNOWN')

      READ(REF,1303,IOSTAT=ERR) FLG_FSI

      CLOSE(UNIT=REF,STATUS='KEEP')  

C--------------------------------------------------------------C
      RETURN
   
1303  FORMAT( 1X,I9)

C--------------------------------------------------------------C
      RETURN
      END  
