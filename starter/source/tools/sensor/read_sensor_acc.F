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
Chd|  READ_SENSOR_ACC               source/tools/sensor/read_sensor_acc.F
Chd|-- called by -----------
Chd|        HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE READ_SENSOR_ACC(SENSOR_PTR ,SENS_ID  ,TITR      ,
     .           LACCELM  ,UNITAB   ,LSUBMODEL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE SENSOR_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: SENS_ID
      INTEGER ,DIMENSION(3,*) ,INTENT(IN) :: LACCELM
      CHARACTER TITR*nchartitle
      TYPE (SENSOR_STR_) :: SENSOR_PTR
      TYPE (SUBMODEL_DATA) ,DIMENSION(NSUBMOD) :: LSUBMODEL
      TYPE (UNIT_TYPE_)    ,INTENT(IN) ::UNITAB 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,LEN,NACC,NPARIS,NPARRS,NVAR,SENS_TYPE
      my_real :: TDEL
      INTEGER ,DIMENSION(6) :: ACC_ID,IACC,IDIR
      my_real ,DIMENSION(6) :: ACC,TMIN
      CHARACTER DIR(6)*ncharkey,DIRACC*ncharkey
      LOGICAL :: IS_AVAILABLE
C--------------------------------
C     ACCELEROMETER BASED SENSOR
C=======================================================================
      IS_AVAILABLE = .FALSE.
      SENS_TYPE = 1
c--------------------------------------------------
      CALL HM_GET_FLOATV('Tdelay' ,TDEL      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV  ('NACCEL1',NACC      ,IS_AVAILABLE,LSUBMODEL)
c  acc1
      CALL HM_GET_INTV  ('IACC1'  ,ACC_ID(1) ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_STRING('DIR1'   ,DIR(1)    ,ncharfield,IS_AVAILABLE)
      CALL HM_GET_FLOATV('Tomin1' ,ACC(1)    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Tmin1'  ,TMIN(1)   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c  acc2
      CALL HM_GET_INTV  ('IACC2'  ,ACC_ID(2) ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_STRING('DIR2'   ,DIR(2)    ,ncharfield,IS_AVAILABLE)
      CALL HM_GET_FLOATV('Tomin2' ,ACC(2)    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Tmin2'  ,TMIN(2)   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c  acc3
      CALL HM_GET_INTV  ('IACC3'  ,ACC_ID(3) ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_STRING('DIR3'   ,DIR(3)    ,ncharfield,IS_AVAILABLE)
      CALL HM_GET_FLOATV('Tomin3' ,ACC(3)    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Tmin3'  ,TMIN(3)   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c  acc4
      CALL HM_GET_INTV  ('IACC4'  ,ACC_ID(4) ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_STRING('DIR4'   ,DIR(4)    ,ncharfield,IS_AVAILABLE)
      CALL HM_GET_FLOATV('Tomin4' ,ACC(4)    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Tmin4'  ,TMIN(4)   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c  acc5
      CALL HM_GET_INTV  ('IACC5'  ,ACC_ID(5) ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_STRING('DIR5'   ,DIR(5)    ,ncharfield,IS_AVAILABLE)
      CALL HM_GET_FLOATV('Tomin5' ,ACC(5)    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Tmin5'  ,TMIN(5)   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c  acc6
      CALL HM_GET_INTV  ('IACC6'  ,ACC_ID(6) ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_STRING('DIR6'   ,DIR(6)    ,ncharfield,IS_AVAILABLE)
      CALL HM_GET_FLOATV('Tomin6' ,ACC(6)    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Tmin6'  ,TMIN(6)   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c--------------------------------------------------
c     Check input data
c--------------------------------------------------
      IF (NACC > 6) THEN
        CALL ANCMSG(MSGID=44,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .              I1=SENS_ID, C1=TITR, I2=NACC)
        NACC = MIN(NACC, 6)
      ENDIF
c
      IACC(:) = 0
      DO I = 1,NACC
        DO J = 1,NACCELM
          IF (ACC_ID(I) == LACCELM(2,J)) THEN
            IACC(I) = J
            EXIT
          ENDIF
        END DO
        IF (IACC(I) == 0) THEN
          CALL ANCMSG(MSGID=45, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_2,
     .                I1=SENS_ID, C1=TITR, I2=ACC_ID(I))
        END IF
      END DO
c
      DO I = 1,NACC
       IDIR(I) = 0
       LEN  = LEN_TRIM(DIR(I))
       DIRACC = DIR(I)(1:LEN)
       IF (DIRACC(1:1) == 'X' .OR. DIRACC(1:1) == 'x') IDIR(I) = IDIR(I)+1
       IF (DIRACC(1:1) == 'Y' .OR. DIRACC(1:1) == 'y') IDIR(I) = IDIR(I)+2
       IF (DIRACC(1:1) == 'Z' .OR. DIRACC(1:1) == 'z') IDIR(I) = IDIR(I)+4
      END DO
c--------------------------------------------------
c
      SENSOR_PTR%TYPE    = SENS_TYPE
      SENSOR_PTR%SENS_ID = SENS_ID
      SENSOR_PTR%STATUS  = 0            ! status = deactivated
      SENSOR_PTR%TSTART  = INFINITY
      SENSOR_PTR%TCRIT   = INFINITY
      SENSOR_PTR%TMIN    = ZERO         ! TMIN global
      SENSOR_PTR%TDELAY  = TDEL         ! time delay before activation
      SENSOR_PTR%VALUE   = ZERO

      NPARIS = NACC * 2 + 1
      NPARRS = NACC * 3
      NVAR   = 0 
c
      SENSOR_PTR%NPARI  = NPARIS
      SENSOR_PTR%NPARR  = NPARRS
      SENSOR_PTR%NVAR   = NVAR
c
      ALLOCATE (SENSOR_PTR%IPARAM(NPARIS))
      ALLOCATE (SENSOR_PTR%RPARAM(NPARRS))
      ALLOCATE (SENSOR_PTR%VAR(NVAR))
      SENSOR_PTR%VAR(:) = ZERO
c
      SENSOR_PTR%IPARAM(1) = NACC
      J = 1
      DO I = 1,NACC
        SENSOR_PTR%IPARAM(J+1) = IACC(I)
        SENSOR_PTR%IPARAM(J+2) = IDIR(I)
        J = J+2
      END DO
      J = 0
      DO I = 1,NACC
        SENSOR_PTR%RPARAM(J+1) = ACC(I)
        SENSOR_PTR%RPARAM(J+2) = TMIN(I)
        SENSOR_PTR%RPARAM(J+3) = INFINITY       
        J = J+3
      END DO    
c------------------------------------------------------------
      WRITE(IOUT, 1000) SENS_ID,TDEL
      WRITE(IOUT, 2000) NACC
      DO I = 1,NACC
        WRITE(IOUT, 3000) ACC_ID(I),IDIR(I),ACC(I),TMIN(I)
      END DO
c------------------------------------------------------------
 1000 FORMAT(
     . 5X,'    SENSOR TYPE 1: ACCELEROMETER           '/,
     . 5X,'    -----------------------------          '/,
     . 5X,'SENSOR ID. . . . . . . . . . . . . . . . .=',I10/
     . 5X,'TIME DELAY BEFORE ACTIVATION . . . . . . .=',E12.4)
 2000 FORMAT(
     . 5X,'NUMBER OF ACCELEROMETERS . . . . . . . . .=',I10)
 3000 FORMAT(
     . 5X,'   ACCELEROMETER ID. . . . . . . . . . . .=',I10/
     . 5X,'   DIRECTION . . . . . . . . . . . . . . .=',I10/
     . 5X,'   MINIMUM ACCELERATION FOR ACTIVATION . .=',E12.4/
     . 5X,'   MINIMUM ACC. DURATION FOR ACTIVATION  .=',E12.4/)
c-----------
      RETURN
      END
