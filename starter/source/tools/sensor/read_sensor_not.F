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
Chd|  READ_SENSOR_NOT               source/tools/sensor/read_sensor_not.F
Chd|-- called by -----------
Chd|        HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE READ_SENSOR_NOT(SENSOR_PTR ,SENS_ID  ,UNITAB   ,LSUBMODEL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE UNITAB_MOD
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
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: SENS_ID
      TYPE (SENSOR_STR_) :: SENSOR_PTR
      TYPE (SUBMODEL_DATA) ,DIMENSION(NSUBMOD) :: LSUBMODEL
      TYPE (UNIT_TYPE_)    ,INTENT(IN) ::UNITAB 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: ID1,NPARIS,NPARRS,NVAR,SENS_TYPE
      my_real :: TDEL
      LOGICAL :: IS_AVAILABLE
C--------------------------------
C     SENSOR "NOT", ACTIVATED WHEN SENSOR1 IS NOT ACTIVE
C=======================================================================
      IS_AVAILABLE = .FALSE.
      SENS_TYPE = 8
c--------------------------------------------------
card1
      CALL HM_GET_FLOATV('Tdelay'  ,TDEL ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card2
      CALL HM_GET_INTV  ('Sensor1' ,ID1  ,IS_AVAILABLE,LSUBMODEL)
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

      NPARIS = 1
      NPARRS = 0
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

      SENSOR_PTR%IPARAM(1) = ID1
c------------------------------------------------------------
      WRITE(IOUT, 1000) SENS_ID,TDEL
      WRITE(IOUT, 2000) ID1
c------------------------------------------------------------
 1000 FORMAT(
     . 5X,'    SENSOR TYPE 8: SENSOR NOT              '/,
     . 5X,'    ------------------------               '/,
     . 5X,'SENSOR ID. . . . . . . . . . . . . . . . .=',I10/
     . 5X,'TIME DELAY BEFORE ACTIVATION . . . . . . .=',E12.4)
 2000 FORMAT(
     . 5X,'   SENSOR 1 ID . . . . . . . . . . . . . .=',I10/)
c-----------
      RETURN
      END
