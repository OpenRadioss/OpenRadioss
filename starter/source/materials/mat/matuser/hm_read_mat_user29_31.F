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
Chd|  HM_READ_MAT29_31              source/materials/mat/matuser/hm_read_mat_user29_31.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING_INDEX           source/devtools/hm_reader/hm_get_string_index.F
Chd|        ST_USERLIB_USERMAT            source/user_interface/dyn_userlib.c
Chd|        USER_OUTPUT                   source/user_interface/user_output.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT29_31(ILAW,IUSER_KEY,
     .                            USERL_AVAIL,
     .                            UPARAM,MAXUPARAM,NUPARAM,
     .                            NUVAR ,IFUNC,MAXFUNC,NFUNC,
     .                            PARMAT,
     .                            LSUBMODEL,
     .                            PM )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Read user material 29 to 31 - generic routine
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     USERL_AVAIL   Flag if userlibrary was load
C     MATNUM        Number of Material 29, 30, 31
C     IS_AVAILABLE  Bool / Result of HM_interface
C     LSUBMODEL     SUBMODEL Structure.
C------------------------------------------------------------------
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr15_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ILAW
      INTEGER USERL_AVAIL
      INTEGER MAXUPARAM
      INTEGER NUPARAM 
      INTEGER NUVAR
      INTEGER NFUNC,MAXFUNC
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      INTEGER, DIMENSION(MAXFUNC)     :: IFUNC
      my_real, DIMENSION(100)         :: PARMAT
      my_real, DIMENSION(MAXUPARAM)   :: UPARAM
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)   :: PM
      CHARACTER IUSER_KEY*ncharline
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER(LEN=4096) :: SCR_FILE_NAME
      CHARACTER(LEN=ncharline) :: RLINE
      CHARACTER (LEN=4) :: CLAW
      LOGICAL :: IS_AVAILABLE
      INTEGER NLINES,J
      INTEGER SCR_FILE_NAME_LEN
      my_real RHO0,RHOR
!
      CHARACTER OPTION*256
      INTEGER SIZE
C-----------------------------------------------
      IS_AVAILABLE = .FALSE.
!
      IF (USERL_AVAIL == 0)THEN
      ! ERROR to be printed & exit
        OPTION='/MAT/'//IUSER_KEY
        SIZE=LEN_TRIM(OPTION)
        CALL ANCMSG(MSGID=1130,
     .              MSGTYPE=MSGERROR,C1=OPTION(1:SIZE),ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF

     
      CALL HM_GET_INTV ('Number_of_datalines' ,NLINES ,IS_AVAILABLE, LSUBMODEL)

      IF(NLINES > 1)THEN

        ! Rho & Rho0_r are read out of user laws
        J=1
        CALL HM_GET_STRING_INDEX('arraydatalines', RLINE, 1, ncharline, IS_AVAILABLE)
        READ(RLINE,ERR=999,FMT=FMT_2F)RHO0,RHOR

        PM(1)=RHOR
        PM(89)=RHO0

        ! Create tempo file  
        WRITE(CLAW,'(I4.4)')ILAW
        SCR_FILE_NAME='SI'//ROOTNAM(1:ROOTLEN)//'_'//CLAW//'.scr'
        SCR_FILE_NAME_LEN=LEN_TRIM(SCR_FILE_NAME)
        OPEN(UNIT=30,FILE=TRIM(SCR_FILE_NAME),FORM='FORMATTED',RECL=ncharline)

        ! Read & Dump in scratch file
        DO J=2,NLINES   
         CALL HM_GET_STRING_INDEX('arraydatalines', RLINE, J, ncharline, IS_AVAILABLE)
         WRITE(30,FMT='(A)')TRIM(RLINE)
        ENDDO
        CLOSE(UNIT=30)
  
        CALL ST_USERLIB_USERMAT(ILAW,ROOTNAM,ROOTLEN,
     .                          UPARAM,MAXUPARAM,NUPARAM,
     .                          NUVAR ,IFUNC,MAXFUNC,NFUNC   ,PARMAT)

        CALL USER_OUTPUT(IOUT,ILAW,ROOTNAM,ROOTLEN,1)
      ELSE
         ! Error MESSAGE_Mess
      ENDIF

      RETURN
 999  CALL ANCMSG(MSGID=55,
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            C1=KEY0(KCUR),
     .            C2=KLINE,
     .            C3=LINE)
      CALL ARRET(2)
      END
