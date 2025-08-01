Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    read_sensor_gauge          ../starter/source/tools/sensor/read_sensor_gauge.F
!||--- called by ------------------------------------------------------
!||    hm_read_sensors            ../starter/source/tools/sensor/hm_read_sensors.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                     ../starter/source/output/message/message.F
!||    hm_get_float_array_index   ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_floatv              ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_int_array_index     ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod         ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod                ../starter/share/message_module/message_mod.F
!||    submodel_mod               ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE READ_SENSOR_GAUGE(SENSOR_PTR ,SENS_ID  ,TITR  ,
     .           LGAUGE   ,UNITAB   ,LSUBMODEL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE SENSOR_MOD
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: SENS_ID
      INTEGER ,DIMENSION(3,*)    ,INTENT(IN)  :: LGAUGE
      CHARACTER(LEN=NCHARTITLE)::TITR
      TYPE (SENSOR_STR_) :: SENSOR_PTR
      TYPE (SUBMODEL_DATA) ,DIMENSION(NSUBMOD) :: LSUBMODEL
      TYPE (UNIT_TYPE_)    ,INTENT(IN) ::UNITAB 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NGAU,NPARIS,NPARRS,NVAR,SENS_TYPE
      my_real :: TDEL
      INTEGER ,DIMENSION(6) :: IGAU,G_ID
      my_real ,DIMENSION(6) :: PRES,TMIN
      LOGICAL :: IS_AVAILABLE
C--------------------------------
C     SENSOR : PRESSURE GAUGE LIST
C=======================================================================
      IS_AVAILABLE = .FALSE.
      SENS_TYPE = 10
c--------------------------------------------------
card1
      CALL HM_GET_FLOATV('Tdelay' ,TDEL  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card2
      CALL HM_GET_INTV  ('NIP'    ,NGAU  ,IS_AVAILABLE,LSUBMODEL)
card3...
      DO I=1,MIN(NGAU,6)
        CALL HM_GET_INT_ARRAY_INDEX  ('GaugeArrayIds',G_ID(I),I,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_FLOAT_ARRAY_INDEX('Fporp' ,PRES(I),I,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX('Fport' ,TMIN(I),I,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDDO
c--------------------------------------------------
      IF (NGAU > 6) THEN
         CALL ANCMSG(MSGID=833, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=SENS_ID,C1=TITR)
      ENDIF
c
      DO I=1,NGAU
        IGAU(I) = 0
        DO J = 1,NBGAUGE
          IF (G_ID(I) == LGAUGE(2,J)) THEN
            IGAU(I) = J
            EXIT
          ENDIF
        ENDDO
        IF (IGAU(I) == 0) THEN
          CALL ANCMSG(MSGID=834, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_2,
     .                I1=SENS_ID, C1=TITR)
          EXIT
        END IF
      ENDDO
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

      NPARIS = NGAU + 1
      NPARRS = NGAU * 3
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
      SENSOR_PTR%IPARAM(1) = NGAU
      DO I = 1,NGAU
        SENSOR_PTR%IPARAM(I+1) = IGAU(I)
      END DO
      J = 0
      DO I = 1,NGAU
        SENSOR_PTR%RPARAM(J+1) = PRES(I)
        SENSOR_PTR%RPARAM(J+2) = TMIN(I)
        SENSOR_PTR%RPARAM(J+3) = INFINITY       
        J = J+3
      END DO    
c------------------------------------------------------------
      WRITE(IOUT, 1000) SENS_ID,TDEL
      WRITE(IOUT, 2000) NGAU
      DO I = 1,NGAU
        WRITE(IOUT, 3000) G_ID(I),PRES(I),TMIN(I)
      END DO
c------------------------------------------------------------
 1000 FORMAT(
     . 5X,'    SENSOR TYPE 10: PRESSURE GAUGE        '/,
     . 5X,'    ------------------------------        '/,
     . 5X,'SENSOR ID. . . . . . . . . . . . . . . . .=',I10/
     . 5X,'TIME DELAY BEFORE ACTIVATION . . . . . . .=',E12.4)
 2000 FORMAT(
     . 5X,'NUMBER OF GAUGES . . . . . . . . . . . . .=',I10)
 3000 FORMAT(
     . 5X,'   GAUGE ID .  . . . . . . . . . . . . . .=',I10/
     . 5X,'   MIN PRESSURE FOR ACTIVATION . . . . . .=',E12.4/
     . 5X,'   MINIMUM PRES. DURATION FOR ACTIVATION .=',E12.4/)
c-----------
      RETURN
      END
