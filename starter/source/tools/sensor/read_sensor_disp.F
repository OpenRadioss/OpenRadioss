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
Chd|  READ_SENSOR_DISP              source/tools/sensor/read_sensor_disp.F
Chd|-- called by -----------
Chd|        HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|-- calls ---------------
Chd|        ANODSET                       source/output/analyse/analyse_node.c
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE READ_SENSOR_DISP(SENSOR_TAB ,SENS_ID  ,
     .           LSENSOR ,SENSOR  ,ITABM1  ,UNITAB   ,LSUBMODEL )
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
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
      INTEGER ,DIMENSION(NUMNOD)  ,INTENT(IN)  :: ITABM1
      INTEGER ,DIMENSION(NSENSI)  ,INTENT(OUT) :: LSENSOR
      my_real ,DIMENSION(NSENSR)  ,INTENT(OUT) :: SENSOR
      TYPE (SENSOR_STR_) :: SENSOR_TAB
      TYPE (SUBMODEL_DATA) ,DIMENSION(NSUBMOD) :: LSUBMODEL
      TYPE (UNIT_TYPE_)    ,INTENT(IN) ::UNITAB 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NOD1,NOD2,NS,SENS_TYPE,P
      my_real :: TDEL,DMIN,DMAX
      CHARACTER MESS*40
      TYPE (SENSOR_STR_) :: SENSOR_PTR
      LOGICAL :: IS_AVAILABLE
      DATA MESS/'SENSOR DEFINITION                       '/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C--------------------------------
C     SENSOR BASED ON RELATIVE DISPLACEMENT OF TWO NODES
C=======================================================================
      IS_AVAILABLE = .FALSE.
c--------------------------------------------------
card1
      CALL HM_GET_FLOATV('Tdelay' ,TDEL  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card2
      CALL HM_GET_INTV  ('N1'     ,NOD1  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('N2'     ,NOD2  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('Dmin'   ,DMIN  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Dmax'   ,DMAX  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c--------------------------------------------------
c     Check input data
c--------------------------------------------------
      SENS_TYPE  = 2
      LSENSOR(1) = SENS_ID
      LSENSOR(2) = SENS_TYPE
      LSENSOR(4) = USR2SYS(NOD1,ITABM1,MESS,NS)
      LSENSOR(5) = USR2SYS(NOD2,ITABM1,MESS,NS)
      CALL ANODSET(LSENSOR(4), CHECK_USED)
      CALL ANODSET(LSENSOR(5), CHECK_USED)
      DO P = 1, NSPMD
        CALL IFRONTPLUS(LSENSOR(4),P)
        CALL IFRONTPLUS(LSENSOR(5),P)
      ENDDO
      SENSOR(1)  = TDEL
      SENSOR(2)  = INFINITY   ! activation time
      SENSOR(3) = DMIN
      SENSOR(4) = DMAX
c------------------------------------------------------------
      WRITE(IOUT, 1000) SENS_ID,TDEL
      WRITE(IOUT, 2000) NOD1,NOD2,DMIN,DMAX
c------------------------------------------------------------
 1000 FORMAT(
     . 5X,'    SENSOR TYPE 2: RELATIVE NODE DISTANCE  '/,
     . 5X,'    -------------------------------------  '/,
     . 5X,'SENSOR ID. . . . . . . . . . . . . . . . .=',I10/
     . 5X,'TIME DELAY BEFORE ACTIVATION . . . . . . .=',E12.4)
 2000 FORMAT(
     . 5X,'   NODE 1 ID . . . . . . . . . . . . . . .=',I10/
     . 5X,'   NODE 2 ID . . . . . . . . . . . . . . .=',I10/
     . 5X,'   MINIMAL DISPLACEMENT. . . . . . . . . .=',E12.4/
     . 5X,'   MAXIMAL DISPLACEMENT. . . . . . . . . .=',E12.4/)
c-----------
      RETURN
      END
