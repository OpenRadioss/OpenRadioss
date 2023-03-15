Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  HM_READ_WINDOW_USER           source/tools/userwi/hm_read_window_user.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING_INDEX           source/devtools/hm_reader/hm_get_string_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        SPMD_USERWIS_SPMD             source/user_interface/userwis_ini_spmd.F
Chd|        ST_USERLIB_USERWIS            source/user_interface/dyn_userlib.c
Chd|        ST_USERLIB_USERWIS_INI        source/user_interface/dyn_userlib.c
Chd|        USERWIS_FRONT                 source/user_interface/userwis_ini_spmd.F
Chd|        USER_OUTPUT                   source/user_interface/user_output.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_WINDOW_USER(USER_WINDOWS, LSUBMODEL,ITAB,
     *                               X, V, VR, MS, IN)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Read user global windows - generic routine
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
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE USER_WINDOWS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "my_allocate.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr15_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "userlib.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(USER_WINDOWS_),INTENT(INOUT)      :: USER_WINDOWS
      TYPE(SUBMODEL_DATA),INTENT(IN)          :: LSUBMODEL(*)
      INTEGER, DIMENSION(NUMNOD),INTENT(IN)   :: ITAB
      my_real, DIMENSION(3,NUMNOD),INTENT(IN) :: X, V, VR
      my_real, DIMENSION(NUMNOD),INTENT(IN)   :: MS,IN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER(LEN=4096)       :: SCR_FILE_NAME
      CHARACTER(LEN=ncharline)  :: RLINE
      CHARACTER (LEN=4)         :: CWIN
      CHARACTER(LEN=nchartitle) :: TITLE
      CHARACTER(LEN=ncharkey)   :: KEY
      LOGICAL                   :: IS_AVAILABLE
      INTEGER ,DIMENSION(100)   :: IUPARAM
      INTEGER NLINES,I,J,STAT,USERWI_ID
      INTEGER SCR_FILE_NAME_LEN
      INTEGER NUVAR,NUVARI
!
      CHARACTER OPTION*256
      INTEGER SIZE
C-----------------------------------------------
      IS_AVAILABLE = .FALSE.
!
      IF (USERL_AVAIL == 0)THEN
      ! ERROR to be printed & exit
        OPTION='/USERWI'
        SIZE=LEN_TRIM(OPTION)
        CALL ANCMSG(MSGID=1130,
     .              MSGTYPE=MSGERROR,C1=OPTION(1:SIZE),ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF

      CALL HM_OPTION_START('/USERWI')
!
      CALL HM_OPTION_READ_KEY(LSUBMODEL,
     *                        OPTION_ID   = USERWI_ID)

      USER_WINDOWS%USER_WINDOWS_ID = USERWI_ID

      CALL HM_GET_INTV ('Number_of_datalines' ,NLINES ,IS_AVAILABLE, LSUBMODEL)

      ! Create tempo file  
      WRITE(CWIN,'(I4.4)') 1
      SCR_FILE_NAME='SI'//ROOTNAM(1:ROOTLEN)//'_'//CWIN//'.scr'
      SCR_FILE_NAME_LEN=LEN_TRIM(SCR_FILE_NAME)
      OPEN(UNIT=30,FILE=TRIM(SCR_FILE_NAME),FORM='FORMATTED',RECL=ncharline)
      WRITE(30,'(A)') '/USERWI'
!
      IF (NLINES > 0) THEN 

        ! Read & Dump in scratch file
        DO J=1,NLINES   
         CALL HM_GET_STRING_INDEX('arraydatalines', RLINE, J, ncharline, IS_AVAILABLE)
         WRITE(30,FMT='(A)')TRIM(RLINE)
        ENDDO

      ENDIF ! IF (NLINES > 0)
!
      CLOSE(UNIT=30)
      
C-----  memory (user buffer length estimation)
      IUPARAM = 0
      USER_WINDOWS%NUVAR   = 0
      USER_WINDOWS%NUVARI  = 0
!
      CALL ST_USERLIB_USERWIS_INI(ROOTNAM,ROOTLEN,
     .             IUPARAM ,NUMNOD  ,ITAB,
     .             X       ,V       ,VR       ,MS      ,IN      ,
     .             NUVAR   ,NUVARI  )
      CALL USER_OUTPUT(IOUT,1,ROOTNAM,ROOTLEN,1)
!
      USER_WINDOWS%NUVAR   = NUVAR
      USER_WINDOWS%NUVARI  = NUVARI

      USER_WINDOWS%S_USER  = NUVAR
      USER_WINDOWS%S_IUSER = NUVARI+100

      MY_ALLOCATE(USER_WINDOWS%IUSER,USER_WINDOWS%S_IUSER)
      MY_ALLOCATE(USER_WINDOWS%USER,USER_WINDOWS%S_USER)

      USER_WINDOWS%USER(1:USER_WINDOWS%S_USER) = ZERO
      USER_WINDOWS%IUSER(1:USER_WINDOWS%S_IUSER) = 0
      
      USER_WINDOWS%IUSER(NUVARI+1:NUVARI+100)=IUPARAM(1:100)


C-----  Scratch file initialisation
      WRITE(CWIN,'(I4.4)') 1
      SCR_FILE_NAME='SI'//ROOTNAM(1:ROOTLEN)//'_'//CWIN//'.scr'
      SCR_FILE_NAME_LEN=LEN_TRIM(SCR_FILE_NAME)
      OPEN(UNIT=30,FILE=TRIM(SCR_FILE_NAME),FORM='FORMATTED',RECL=ncharline)
      WRITE(30,'(A)') '/USERWI'

        ! Read & Dump in scratch file
      DO J=1,NLINES   
         CALL HM_GET_STRING_INDEX('arraydatalines', RLINE, J, ncharline, IS_AVAILABLE)
         WRITE(30,FMT='(A)')TRIM(RLINE)
      ENDDO
      CLOSE(UNIT=30)
!
      CALL ST_USERLIB_USERWIS(ROOTNAM, ROOTLEN, NUMNOD, ITAB,
     .                        X, V, VR, MS, IN,
     .                        USER_WINDOWS%NUVAR, USER_WINDOWS%NUVARI, 
     .                        USER_WINDOWS%USER,  USER_WINDOWS%IUSER )
     
      CALL USER_OUTPUT(IOUT,1,ROOTNAM,ROOTLEN,1)
!
!---------
      RETURN
      END
