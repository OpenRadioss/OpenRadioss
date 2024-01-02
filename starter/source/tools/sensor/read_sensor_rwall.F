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
Chd|  READ_SENSOR_RWALL             source/tools/sensor/read_sensor_rwall.F
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
      SUBROUTINE READ_SENSOR_RWALL(SENSOR_PTR ,SENS_ID  ,TITR   ,
     .           UNITAB   ,LSUBMODEL )
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
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: SENS_ID
      CHARACTER TITR*nchartitle
      TYPE (SENSOR_STR_) :: SENSOR_PTR
      TYPE (SUBMODEL_DATA) ,DIMENSION(NSUBMOD) :: LSUBMODEL
      TYPE (UNIT_TYPE_)    ,INTENT(IN) :: UNITAB 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: RWALL_ID,IDIR,NPARIS,NPARRS,NVAR,SENS_TYPE
      my_real :: TDEL,TMIN,FMIN,FMAX
      CHARACTER DIR*ncharkey
      LOGICAL :: IS_AVAILABLE
C--------------------------------
C     SENSOR BASED ON RIGID WALL FORCE
C=======================================================================
      IS_AVAILABLE = .FALSE.
      SENS_TYPE    = 7
c--------------------------------------------------
card1
      CALL HM_GET_FLOATV('Tdelay'   ,TDEL     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card2
      CALL HM_GET_INTV  ('RWALL_ID' ,RWALL_ID ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_STRING('DIR '     ,DIR      ,ncharfield,IS_AVAILABLE)
      CALL HM_GET_FLOATV('Fmin'     ,FMIN     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Fmax'     ,FMAX     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Tmin'     ,TMIN     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c--------------------------------------------------
c     Check input data
c--------------------------------------------------
      IDIR = 0
      IF (DIR(1:1) == 'X'  .OR. DIR(1:1) == 'x')  IDIR=1         
      IF (DIR(1:1) == 'Y'  .OR. DIR(1:1) == 'Y')  IDIR=2        
      IF (DIR(1:1) == 'Z'  .OR. DIR(1:1) == 'Z')  IDIR=4        
      IF (DIR(1:2) == 'NF' .OR. DIR(1:2) == 'nf') IDIR=5        
      IF (DIR(1:2) == 'FN' .OR. DIR(1:2) == 'fn') IDIR=5   
      IF (DIR(1:2) == 'TF' .OR. DIR(1:2) == 'tf') IDIR=6    
      IF (DIR(1:2) == 'FT' .OR. DIR(1:2) == 'ft') IDIR=6   
      IF (IDIR == 0 .AND. LEN_TRIM(DIR) /= 0 ) THEN
        CALL ANCMSG(MSGID=1594, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND,
     .              I1=SENS_ID, C1=TITR, C2=DIR)
      END IF                                       
c--------------------------------------------------
c
      SENSOR_PTR%TYPE    = SENS_TYPE
      SENSOR_PTR%SENS_ID = SENS_ID
      SENSOR_PTR%STATUS  = 0            ! status = deactivated
      SENSOR_PTR%TSTART  = INFINITY
      SENSOR_PTR%TCRIT   = INFINITY
      SENSOR_PTR%TMIN    = TMIN
      SENSOR_PTR%TDELAY  = TDEL         ! time delay before activation
      SENSOR_PTR%VALUE   = ZERO

      NPARIS = 2
      NPARRS = 2
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

      SENSOR_PTR%IPARAM(1) = RWALL_ID
      SENSOR_PTR%IPARAM(2) = IDIR

      SENSOR_PTR%RPARAM(1) = FMIN
      SENSOR_PTR%RPARAM(2) = FMAX
c------------------------------------------------------------
      WRITE (IOUT, 1000) SENS_ID,TDEL
      WRITE (IOUT, 2000) RWALL_ID,FMIN,FMAX,TMIN,DIR(1:LEN_TRIM(DIR))
c------------------------------------------------------------
 1000 FORMAT(
     . 5X,'    SENSOR TYPE 7: RIGID WALL FORCE        '/,
     . 5X,'    -------------------------------        '/,
     . 5X,'SENSOR ID. . . . . . . . . . . . . . . . .=',I10/
     . 5X,'TIME DELAY BEFORE ACTIVATION . . . . . . .=',E12.4)
 2000 FORMAT(
     . 5X,'   RIGID WALL ID . . . . . . . . . . . . .=',I10/
     . 5X,'   FORCE MIN . . . . . . . . . . . . . . .=',E12.4/
     . 5X,'   FORCE MAX . . . . . . . . . . . . . . .=',E12.4/
     . 5X,'   MIN DURATION LIMIT. . . . . . . . . . .=',E12.4/
     . 5X,'   FORCE DIRECTION . . . . . . . . . . . .=',2X,A/)
c-----------
      RETURN
      END
