Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  HM_READ_FAIL_USER             source/materials/fail/failuser/hm_read_fail_user.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING_INDEX           source/devtools/hm_reader/hm_get_string_index.F
Chd|        ST_USERLIB_LECR               source/user_interface/dyn_userlib.c
Chd|        USER_OUTPUT                   source/user_interface/user_output.F
Chd|        FAIL_PARAM_MOD                ../common_source/modules/mat_elem/fail_param_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_USER(FAIL   ,IRUPT,IUSER_KEY,USERL_AVAIL,
     .                             LSUBMODEL,UNITAB_SUB,MAT_ID)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Read user material failure - generic routine
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     USERL_AVAIL   Flag if userlibrary was load
C     IS_AVAILABLE  Bool / Result of HM_interface
C     LSUBMODEL     SUBMODEL Structure.
C------------------------------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
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
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRUPT,MAT_ID
      INTEGER USERL_AVAIL
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      my_real, DIMENSION(4)           :: UNITAB_SUB
      CHARACTER IUSER_KEY*ncharline
      TYPE (FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NUPARAM,NUVAR,NFUNC
      INTEGER, PARAMETER :: MAXFUNC  = 100
      INTEGER, PARAMETER :: MAXPARAM = 1000
      INTEGER, DIMENSION(MAXFUNC)  :: IFUNC
      my_real, DIMENSION(MAXPARAM) :: UPARAM
      CHARACTER(LEN=4096) :: SCR_FILE_NAME
      CHARACTER(LEN=ncharline) :: RLINE
      CHARACTER (LEN=4) :: CRUP
      CHARACTER(LEN=nchartitle) :: TITLE
      LOGICAL :: IS_AVAILABLE
      INTEGER NLINES,J
      INTEGER SCR_FILE_NAME_LEN
!
      CHARACTER OPTION*256
      INTEGER SIZE
C-----------------------------------------------
      IF (USERL_AVAIL == 0)THEN
      ! ERROR to be printed & exit
        OPTION='/FAIL/'//IUSER_KEY
        SIZE=LEN_TRIM(OPTION)
        CALL ANCMSG(MSGID=1130,
     .              MSGTYPE=MSGERROR,C1=OPTION(1:SIZE),ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF

      CALL HM_GET_INTV ('Number_of_datalines' ,NLINES ,IS_AVAILABLE, LSUBMODEL)

      IF(NLINES > 0)THEN 

        ! Create tempo file  
        WRITE(CRUP,'(I4.4)')IRUPT
        SCR_FILE_NAME='SI'//ROOTNAM(1:ROOTLEN)//'_'//CRUP//'.scr'
        SCR_FILE_NAME_LEN=LEN_TRIM(SCR_FILE_NAME)
        OPEN(UNIT=30,FILE=TRIM(SCR_FILE_NAME),FORM='FORMATTED',RECL=ncharline)

        ! Read & Dump in scratch file
        DO J=1,NLINES   
         CALL HM_GET_STRING_INDEX('arraydatalines', RLINE, J, ncharline, IS_AVAILABLE)
         WRITE(30,FMT='(A)')TRIM(RLINE)
        ENDDO
        CLOSE(UNIT=30)
!
        CALL ST_USERLIB_LECR(IRUPT,ROOTNAM,ROOTLEN,UPARAM,MAXPARAM,NUPARAM,
     .                       NUVAR ,IFUNC,MAXFUNC,NFUNC,UNITAB_SUB,
     .                       MAT_ID)
c
        FAIL%KEYWORD = 'USER FAILURE MODEL' 
        FAIL%IRUPT   = IRUPT 
        FAIL%FAIL_ID = 0 
        FAIL%NUPARAM = NUPARAM
        FAIL%NIPARAM = 0
        FAIL%NUVAR   = NUVAR
        FAIL%NFUNC   = NFUNC
        FAIL%NTABLE  = 0
        FAIL%NMOD    = 0
c            
        ALLOCATE (FAIL%UPARAM(FAIL%NUPARAM))
        ALLOCATE (FAIL%IPARAM(FAIL%NIPARAM))
        ALLOCATE (FAIL%IFUNC (FAIL%NFUNC))
        ALLOCATE (FAIL%TABLE (FAIL%NTABLE))
c
        FAIL%UPARAM(1:NUPARAM) = UPARAM(1:NUPARAM)
        FAIL%IFUNC (1:NFUNC)   = IFUNC(1:NFUNC)
c
        CALL USER_OUTPUT(IOUT,IRUPT,ROOTNAM,ROOTLEN,1)
      ELSE
         ! Error MESSAGE_Mess
      ENDIF
!---------
      RETURN
      END
