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
Chd|  HM_READ_PROP_USER             source/properties/user_spring_solid/hm_read_prop_user.F
Chd|-- called by -----------
Chd|        HM_READ_PROP_GENERIC          source/properties/hm_read_prop_generic.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING_INDEX           source/devtools/hm_reader/hm_get_string_index.F
Chd|        ST_USERLIB_LECGUSER           source/user_interface/dyn_userlib.c
Chd|        USER_OUTPUT                   source/user_interface/user_output.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP_USER(IOUT ,NUVAR ,PARGEO   ,UNITAB,IGTYP,
     .                             IG   ,TITLE ,LSUBMODEL,IUNIT ,ISKN ,
     .                             KEY  ,PROP_TAG)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Read spring user property - generic routine
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
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE ELBUFTAG_MOD
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
#include      "userlib.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: IOUT,IG,IUNIT
      INTEGER ,INTENT(INOUT) :: IGTYP
      INTEGER ,INTENT(IN) :: ISKN(LISKN,*)
      CHARACTER(LEN=ncharkey) :: KEY
      CHARACTER(LEN=nchartitle) :: TITLE
      INTEGER NUVAR(2)
      my_real
     .        PARGEO(100)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      TYPE (SUBMODEL_DATA) ,DIMENSION(NSUBMOD) :: LSUBMODEL
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real, DIMENSION(4) :: UNITAB_SUB
      LOGICAL :: IS_AVAILABLE
      INTEGER J,NLINES
      CHARACTER(LEN=ncharline) :: RLINE
      CHARACTER (LEN=4) :: CSPRI
      CHARACTER(LEN=4096) :: SCR_FILE_NAME
      INTEGER SCR_FILE_NAME_LEN
      CHARACTER IUSER_KEY*ncharline
!
      CHARACTER OPTION*256
      INTEGER SIZE
C-----------------------------------------------
!
      IF(KEY(1:5)     == 'USER1' .OR. KEY(1:6) == 'TYPE29')THEN
        IGTYP=29
      ELSEIF(KEY(1:5) == 'USER2' .OR. KEY(1:6) == 'TYPE30')THEN
        IGTYP=30
      ELSEIF(KEY(1:5) == 'USER3' .OR. KEY(1:6) == 'TYPE31')THEN
        IGTYP=31
      ENDIF
c--------------------------------------------------
      IS_AVAILABLE = .FALSE.
c--------------------------------------------------
      IUSER_KEY = KEY(1:LEN_TRIM(KEY))
      IF (USERL_AVAIL == 0)THEN
      ! ERROR to be printed & exit
        OPTION='/PROP/'//IUSER_KEY
        SIZE=LEN_TRIM(OPTION)
        CALL ANCMSG(MSGID=1130,
     .              MSGTYPE=MSGERROR,C1=OPTION(1:SIZE),ANMODE=ANINFO)
        CALL ARRET(2)
!
!        CALL ANCMSG(MSGID=2042,
!     .              ANMODE=ANINFO,
!     .              MSGTYPE=MSGERROR,
!     .              I1=IG,
!     .              I2=IGTYP,
!     .              C1=TITLE)
      ENDIF
!------------
!     READING
!------------
      WRITE(IOUT,1000) IG
!
      CALL HM_GET_INTV ('Number_of_datalines' ,NLINES ,IS_AVAILABLE, LSUBMODEL)
!
!      IF (NLINES > 1) THEN
!
        WRITE(CSPRI,'(I4.4)')IGTYP
        SCR_FILE_NAME='SI'//ROOTNAM(1:ROOTLEN)//'_'//CSPRI//'.scr'
        SCR_FILE_NAME_LEN=LEN_TRIM(SCR_FILE_NAME)
        OPEN(UNIT=30,FILE=TRIM(SCR_FILE_NAME),FORM='FORMATTED',RECL=ncharline)
!
        ! Read & Dump in scratch file
!        DO J=2,NLINES
        DO J=1,NLINES
          CALL HM_GET_STRING_INDEX('arraydatalines', RLINE, J, ncharline, IS_AVAILABLE)
          WRITE(30,FMT='(A)')TRIM(RLINE)
        ENDDO
        CLOSE(UNIT=30)
!
        UNITAB_SUB(1)=UNITAB%UNIT_ID(IUNIT)
        UNITAB_SUB(2)=UNITAB%FAC_M(IUNIT)
        UNITAB_SUB(3)=UNITAB%FAC_L(IUNIT)
        UNITAB_SUB(4)=UNITAB%FAC_T(IUNIT)
!
        CALL ST_USERLIB_LECGUSER(IGTYP,ROOTNAM,ROOTLEN,NUVAR ,PARGEO,UNITAB_SUB,ISKN,IG,TITLE)
        CALL USER_OUTPUT(IOUT,IGTYP,ROOTNAM,ROOTLEN,1)
!
!      ELSE
!   error TBD
!      ENDIF ! IF(NLINES > 0)THEN
!---------
        PROP_TAG(IGTYP)%G_EINT = 1
        PROP_TAG(IGTYP)%G_FOR  = 3
        PROP_TAG(IGTYP)%G_MOM = 5
        PROP_TAG(IGTYP)%G_SKEW = 6
        PROP_TAG(IGTYP)%G_MASS = 1
        PROP_TAG(IGTYP)%G_V_REPCVT  = 3 ! -- VITESSES REPERE CONVECTEE (V_REPCVT)
        PROP_TAG(IGTYP)%G_VR_REPCVT = 3  ! -- VITESSES REPERE CONVECTEE (VR_REPCVT)
 !
        PROP_TAG(IGTYP)%G_NUVAR = MAX(PROP_TAG(IGTYP)%G_NUVAR,NUVAR(1))
 !
        !---------
      RETURN
!---
 1000 FORMAT(
     & 5X,'USER PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10)
!---
      END
