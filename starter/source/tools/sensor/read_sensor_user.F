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
Chd|  READ_SENSOR_USER              source/tools/sensor/read_sensor_user.F
Chd|-- called by -----------
Chd|        HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING_INDEX           source/devtools/hm_reader/hm_get_string_index.F
Chd|        SENSOR_USER_ALLOC             source/tools/sensor/sensor_user_alloc.F
Chd|        ST_USERLIB_LECSEN             source/user_interface/dyn_userlib.c
Chd|        USER_OUTPUT                   source/user_interface/user_output.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE READ_SENSOR_USER(LSENSOR  ,SENSOR    ,HM_NSENSOR,ISEN    ,TITLE  ,
     .                            UNITAB   ,LSUBMODEL ,KEY       ,UNIT_ID ,SENS_ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr_usens_c.inc"
#include      "sysunit.inc"
#include      "scr15_c.inc"
#include      "userlib.inc"
!
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: HM_NSENSOR,UNIT_ID ,SENS_ID,ISEN
      CHARACTER(LEN=ncharkey) :: KEY
      CHARACTER(LEN=nchartitle) :: TITLE
      INTEGER ,DIMENSION(NSENSI,NSENSOR)  ,INTENT(INOUT) :: LSENSOR
      my_real ,DIMENSION(NSENSR,NSENSOR)  ,INTENT(INOUT) :: SENSOR
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      TYPE (SUBMODEL_DATA) ,DIMENSION(NSUBMOD) :: LSUBMODEL
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,TYP
      my_real :: TDEL
      LOGICAL :: IS_AVAILABLE
      LOGICAL :: ALREADY_DONE
      INTEGER NLINES
      CHARACTER(LEN=ncharline) :: RLINE
      CHARACTER (LEN=4) :: CSENS
      CHARACTER(LEN=4096) :: SCR_FILE_NAME
      INTEGER SCR_FILE_NAME_LEN
!
      CHARACTER IUSER_KEY*ncharline
      CHARACTER OPTION*256
      INTEGER SIZE
C=======================================================================
!
!     User libraries use the SET_U_SENS_xxx routines - it uses KSENS_CUR parameter.
      KSENS_CUR = ISEN

      IF(KEY(1:5)     == 'USER1')THEN
        TYP=29
      ELSEIF(KEY(1:5) == 'USER2')THEN
        TYP=30
      ELSEIF(KEY(1:5) == 'USER3')THEN
        TYP=31
      ELSE
        CALL ANCMSG(MSGID=43,
     .              ANMODE=ANINFO,
     .              MSGTYPE=MSGERROR,
     .              C2=KEY(1:LEN_TRIM(KEY)),
     .              I1=SENS_ID,
     .              C1=TITLE)
      ENDIF
c--------------------------------------------------
      IS_AVAILABLE = .FALSE.
      ALREADY_DONE = .FALSE.
c--------------------------------------------------
      IUSER_KEY = KEY(1:LEN_TRIM(KEY))
      IF (USERL_AVAIL == 0)THEN
      ! ERROR to be printed & exit
        OPTION='/SENSOR/'//IUSER_KEY
        SIZE=LEN_TRIM(OPTION)
        CALL ANCMSG(MSGID=1130,
     .              MSGTYPE=MSGERROR,C1=OPTION(1:SIZE),ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
!------------
!     READING
!------------
      CALL HM_GET_INTV ('Number_of_datalines' ,NLINES ,IS_AVAILABLE, LSUBMODEL)
!
      IF (NLINES > 1) THEN
!
        LSENSOR(1,ISEN)= SENS_ID
        LSENSOR(2,ISEN)= TYP
        LSENSOR(3,ISEN)= 0
!
        IF ((TYP>28.AND.TYP<38).AND. .NOT.(ALREADY_DONE) ) CALL SENSOR_USER_ALLOC(ALREADY_DONE)
!
c---------------------------------------
        IF (TYP>=29.AND.TYP<=31) THEN
C--------------------------------
C         USER SENSOR
C--------------------
          IF (USERL_AVAIL==1 .AND. (TYP==29.OR.TYP==30.OR.TYP==31)) THEN
!
            WRITE(CSENS,'(I4.4)')TYP
            SCR_FILE_NAME='SI'//ROOTNAM(1:ROOTLEN)//'_'//CSENS//'.scr'
            SCR_FILE_NAME_LEN=LEN_TRIM(SCR_FILE_NAME)
            OPEN(UNIT=30,FILE=TRIM(SCR_FILE_NAME),FORM='FORMATTED',RECL=ncharline)
!
            J=1
            CALL HM_GET_STRING_INDEX('arraydatalines', RLINE, J, ncharline, IS_AVAILABLE)
            READ(RLINE,ERR=999,FMT=FMT_F)TDEL
C           global activation time
            SENSOR(1,ISEN) = TDEL
!
           ! Read & Dump in scratch file
            DO J=2,NLINES
              CALL HM_GET_STRING_INDEX('arraydatalines', RLINE, J, ncharline, IS_AVAILABLE)
              WRITE(30,FMT='(A)')TRIM(RLINE)
            ENDDO
            CLOSE(UNIT=30)
          ENDIF ! IF (TYP>=29.AND.TYP<=31)
!
          WRITE (IOUT,'(A,I10)') ' SENSOR ID = ', SENS_ID
          IF(TYP == 29)THEN
            WRITE (IOUT,'(A,/,A,I10)')
     .      ' TYPE 29 SENSOR : USER1'
          WRITE (IOUT,'(A,1PG20.13)')
     .    ' TIME DELAY BEFORE ACTIVATION . . . . .',TDEL
            SENSOR (2,ISEN)=1E30
            SENSOR (2+LSENBUF,ISEN)=1E30
            IF (USERL_AVAIL==1)THEN
              CALL ST_USERLIB_LECSEN(TYP,ROOTNAM,ROOTLEN)
              CALL USER_OUTPUT(IOUT,TYP,ROOTNAM,ROOTLEN,1)
            ENDIF
          ELSEIF(TYP == 30)THEN
            WRITE (IOUT,'(A,/,A,I10)')
     .      ' TYPE 30 SENSOR : USER2'
          WRITE (IOUT,'(A,1PG20.13)')
     .    ' TIME DELAY BEFORE ACTIVATION . . . . .',TDEL
            SENSOR (2,ISEN)=INFINITY
            SENSOR (2+LSENBUF,ISEN)=INFINITY
            IF (USERL_AVAIL==1)THEN
              CALL ST_USERLIB_LECSEN(TYP,ROOTNAM,ROOTLEN)
              CALL USER_OUTPUT(IOUT,TYP,ROOTNAM,ROOTLEN,1)
            ENDIF
          ELSEIF(TYP == 31)THEN
            WRITE (IOUT,'(A,/,A,I10)')
     .      ' TYPE 31 SENSOR : USER3'
          WRITE (IOUT,'(A,1PG20.13)')
     .    ' TIME DELAY BEFORE ACTIVATION . . . . .',TDEL
            SENSOR (2,ISEN)=1E30
            SENSOR (2+LSENBUF,ISEN)=1E30
            IF (USERL_AVAIL==1)THEN
              CALL ST_USERLIB_LECSEN(TYP,ROOTNAM,ROOTLEN)
              CALL USER_OUTPUT(IOUT,TYP,ROOTNAM,ROOTLEN,1)
            ENDIF
          ENDIF
        ELSE
            CALL ANCMSG(MSGID=43,
     .                  ANMODE=ANINFO,
     .                  MSGTYPE=MSGERROR,
     .                  C2=KEY(1:LEN_TRIM(KEY)),
     .                  I1=SENS_ID,
     .                  C1=TITLE)
        ENDIF ! IF (TYP>=29.AND.TYP<=31)
!
      ELSE
!   error TBD
      ENDIF ! IF(NLINES > 0)THEN
C-----------
      RETURN
 999  CALL ANCMSG(MSGID=55,
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            C1=KEY0(KCUR),
     .            C2=KLINE,
     .            C3=LINE)
      CALL ARRET(2)
C-----------
      RETURN
      END

