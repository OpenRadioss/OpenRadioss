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
Chd|  READ_SENSOR_TEMP              source/tools/sensor/read_sensor_temp.F
Chd|-- called by -----------
Chd|        HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE READ_SENSOR_TEMP(
     .           SENSOR_PTR,SENS_ID  ,TITR    ,
     .           IGRNOD    ,LSENSOR ,SENSOR   ,UNITAB   ,LSUBMODEL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
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
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "scr_usens_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: SENS_ID
      INTEGER ,DIMENSION(NSENSI)   ,INTENT(OUT) :: LSENSOR
      my_real ,DIMENSION(NSENSR)   ,INTENT(OUT) :: SENSOR
      CHARACTER TITR*nchartitle
      TYPE (SENSOR_STR_) :: SENSOR_PTR
      TYPE (SUBMODEL_DATA) ,DIMENSION(NSUBMOD) :: LSUBMODEL
      TYPE (SUBSET_)       ,DIMENSION(NSUBS)   :: SUBSET
      TYPE (GROUP_)        ,DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (UNIT_TYPE_)    ,INTENT(IN) :: UNITAB 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,TFLAG,SENS_TYPE,GRNOD_ID,IGRN,NPARIS,NPARRS,NVAR
      my_real :: TDEL,TEMP
      LOGICAL :: IS_AVAILABLE
C--------------------------------
C     SENSOR BASED ON PART ENERGY
C=======================================================================
      IS_AVAILABLE = .FALSE.
      SENS_TYPE    = 17
      TFLAG        = 0    ! const energy option
c--------------------------------------------------
card1
      CALL HM_GET_FLOATV('Tdelay'     ,TDEL     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card2
      CALL HM_GET_FLOATV('Temp'       ,TEMP     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV  ('Tflag'      ,TFLAG    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('Grnod_Id'   ,GRNOD_ID ,IS_AVAILABLE,LSUBMODEL)
c--------------------------------------------------
c     Check Grnod_ID
c--------------------------------------------------
      IGRN = 0 
      DO I=1,NGRNOD
        IF (IGRNOD(I)%ID == GRNOD_ID) THEN
          IGRN = I
          EXIT
        ENDIF
      ENDDO
c---------------------------
c     sensor data structure
c--------------------------------------------------
      NPARIS = 3   ! int  value parameters
      NPARRS = 1   ! real value parameters
      NVAR   = 3   ! internal variables - stored temperature
c
      ALLOCATE (SENSOR_PTR%IPARAM(NPARIS))
      ALLOCATE (SENSOR_PTR%RPARAM(NPARRS))
      ALLOCATE (SENSOR_PTR%VAR(NVAR))
      SENSOR_PTR%VAR(:) = ZERO

      SENSOR_PTR%NVAR    = NVAR
      SENSOR_PTR%NPARI   = NPARIS
      SENSOR_PTR%NPARR   = NPARRS
      SENSOR_PTR%TYPE    = SENS_TYPE
      SENSOR_PTR%SENS_ID = SENS_ID
      SENSOR_PTR%STATUS  = 0          ! status = deactivated
      SENSOR_PTR%TSTART  = INFINITY   ! start time
      SENSOR_PTR%TCRIT   = INFINITY   ! global activation time
      SENSOR_PTR%TDELAY  = TDEL       ! time delay before activation
      SENSOR_PTR%VALUE   = ZERO
c
      SENSOR_PTR%IPARAM(1) = IGRN     ! GRNOD index
      SENSOR_PTR%IPARAM(2) = TFLAG    ! temperature selector
      SENSOR_PTR%IPARAM(3) = IGRNOD(IGRN)%NENTITY    ! number of node
c
      SENSOR_PTR%RPARAM(1) = TEMP 
c---------------------------
c     old sensor data tables
c---------------------------
      LSENSOR(1) = SENS_ID
      LSENSOR(2) = SENS_TYPE
c
      SENSOR(1)  = TDEL        ! time delay
      SENSOR(2)  = INFINITY    ! start time
      SENSOR(3)  = INFINITY    ! stop time
      SENSOR(4)  = INFINITY    ! global activation time
      SENSOR(6)  = TEMP
c------------------------------------------------------------
      WRITE (IOUT ,1000) SENS_ID,TDEL
      WRITE (IOUT ,3000) TEMP,TFLAG,GRNOD_ID
c------------------------------------------------------------
 1000 FORMAT(
     . 5X,'    SENSOR : TEMPERATURE IN A NODE GROUP    '/,
     . 5X,'    ------------------------------------    '/,
     . 5X,'SENSOR ID. . . . . . . . . . . . . . . . . =',I10/
     . 5X,'TIME DELAY BEFORE ACTIVATION . . . . . . . =',E12.4)
 3000 FORMAT(
     . 5X,'  TEMPERATURE. . . . . . . . . . . . . . . =',E12.4/
     . 5X,'  TEMPERATURE FLAG . . . . . . . . . . . . =',I3  /
     . 5X,'        = 1 : MAX TEMP . . . . . . . . . . ='/
     . 5X,'        = 2 : MIN TEMP . . . . . . . . . . ='/
     . 5X,'        = 3 : AVG TEMP . . . . . . . . . . ='/
     . 5X,'  GRNOD ID . . . . . . . . . . . . . . . . =',I10)
c-----------
      RETURN
      END
