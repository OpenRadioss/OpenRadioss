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
Chd|  READ_SENSOR_ENERGY            source/tools/sensor/read_sensor_energy.F
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
      SUBROUTINE READ_SENSOR_ENERGY(SENSOR_PTR,SENS_ID   ,TITR     ,
     .           IPART    ,SUBSET   ,UNITAB   ,LSUBMODEL )
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
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: SENS_ID
      INTEGER ,DIMENSION(LIPART1,*),INTENT(IN)  :: IPART
      CHARACTER TITR*nchartitle
      TYPE (SENSOR_STR_) :: SENSOR_PTR
      TYPE (SUBMODEL_DATA) ,DIMENSION(NSUBMOD) :: LSUBMODEL
      TYPE (SUBSET_)       ,DIMENSION(NSUBS)   :: SUBSET
      TYPE (UNIT_TYPE_)    ,INTENT(IN) :: UNITAB 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: J,IFLAG,SENS_TYPE,PART_ID,SUB_ID,PARTN,SUBN,NPARIS,NPARRS,NVAR
      my_real :: TDEL,TMIN,IEMIN,IEMAX,KEMIN,KEMAX,IETOL,IETIME,KETOL,KETIME
      LOGICAL :: IS_AVAILABLE
C--------------------------------
C     SENSOR BASED ON PART ENERGY
C=======================================================================
      IS_AVAILABLE = .FALSE.
      SENS_TYPE    = 14
      IFLAG        = 0    ! const energy option
c--------------------------------------------------
card1
      CALL HM_GET_FLOATV('Tdelay'     ,TDEL    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card2
      CALL HM_GET_INTV  ('Part_Id'    ,PART_ID ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('Subset_ID'  ,SUB_ID  ,IS_AVAILABLE,LSUBMODEL)
card3
      CALL HM_GET_FLOATV('IEmin'      ,IEMIN   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('IEmax'      ,IEMAX   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('KEmin'      ,KEMIN   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('KEmax'      ,KEMAX   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Tmin'       ,TMIN    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card4
      CALL HM_GET_FLOATV('IEtol'      ,IETOL   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('IEtime'     ,IETIME  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('KEtol'      ,KETOL   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('KEtime'     ,KETIME  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c--------------------------------------------------
c     Defaults
c--------------------------------------------------
      IF (IETOL > ZERO .or. KETOL > ZERO .or. IETIME > ZERO .or. KETIME > ZERO) THEN
        IFLAG = 1  ! constant energy option active
      END IF
      IF (IEMAX  == ZERO) IEMAX  = INFINITY
      IF (KEMAX  == ZERO) KEMAX  = INFINITY
      IF (IETIME == ZERO) IETIME = INFINITY
      IF (KETIME == ZERO) KETIME = INFINITY
      IF (IETOL  == ZERO) IETOL  = 0.01
      IF (KETOL  == ZERO) KETOL  = 0.01
c--------------------------------------------------
c     Check Part_ID, Sub_ID
c--------------------------------------------------
      PARTN = 0 
      SUBN  = 0 
      IS_AVAILABLE = .FALSE.
      DO J=1,NPART
        IF (IPART(4,J) == PART_ID) THEN
          IS_AVAILABLE = .TRUE.
          PARTN = J
          EXIT
        ENDIF
      ENDDO
      IF (.NOT. IS_AVAILABLE) THEN
        PART_ID = 0
      ELSE ! ignore subset_id
        SUB_ID = 0
      ENDIF
c
      IF (.NOT. IS_AVAILABLE) THEN
        DO J=1,NSUBS-1
          IF (SUBSET(J)%ID == SUB_ID) THEN
            IS_AVAILABLE = .TRUE.
            SUBN  = J 
            EXIT
          ENDIF
        ENDDO
        IF (.NOT. IS_AVAILABLE) SUB_ID = 0
      ENDIF ! IF (.NOT. IS_AVAILABLE)
c
      IF (.NOT. IS_AVAILABLE) THEN
        ! we take global subset including all parts
        SUB_ID = SUBSET(NSUBS)%ID
        SUBN  = NSUBS 
      ENDIF ! IF (.NOT. IS_AVAILABLE)
c------------------------------------------------------------
      WRITE (IOUT, 1000) SENS_ID,TDEL
      IF (PART_ID > 0) THEN
        WRITE (IOUT, 2100) PART_ID
      ELSE IF (SUB_ID > 0) THEN
        WRITE (IOUT, 2200) SUB_ID
      ELSE
        WRITE (IOUT, 2300)
      END IF
      WRITE (IOUT ,3000) IEMIN,IEMAX,KEMIN,KEMAX,
     .                   IETOL,KETOL,TMIN,IETIME,KETIME
c--------------------------------------------------
      IF (IETIME == INFINITY) THEN
        IETIME = ZERO
        IETOL  = ZERO
      END IF
      IF (KETIME == INFINITY) THEN
        KETIME = ZERO
        KETOL  = ZERO
      END IF
c---------------------------
c     sensor data structure
c--------------------------------------------------
      NPARIS = 3
      NPARRS = 8
      NVAR   = 10
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
      SENSOR_PTR%STATUS  = 0      ! status = deactivated
      SENSOR_PTR%TSTART  = INFINITY
      SENSOR_PTR%TCRIT   = INFINITY
      SENSOR_PTR%TMIN    = TMIN
      SENSOR_PTR%TDELAY  = TDEL           ! time delay before activation
      SENSOR_PTR%VALUE   = ZERO
c
      SENSOR_PTR%IPARAM(1) = PARTN   ! PART number
      SENSOR_PTR%IPARAM(2) = SUBN    ! SUBSET number
      SENSOR_PTR%IPARAM(3) = IFLAG   ! const energy option
c
      SENSOR_PTR%RPARAM(1) = IEMIN 
      SENSOR_PTR%RPARAM(2) = IEMAX 
      SENSOR_PTR%RPARAM(3) = KEMIN 
      SENSOR_PTR%RPARAM(4) = KEMAX 
      SENSOR_PTR%RPARAM(5) = IETOL 
      SENSOR_PTR%RPARAM(6) = KETOL 
      SENSOR_PTR%RPARAM(7) = IETIME
      SENSOR_PTR%RPARAM(8) = KETIME
c------------------------------------------------------------
 1000 FORMAT(
     . 5X,'    SENSOR TYPE 14: PART ENERGY             '/,
     . 5X,'    ---------------------------             '/,
     . 5X,'SENSOR ID. . . . . . . . . . . . . . . . . =',I10/
     . 5X,'TIME DELAY BEFORE ACTIVATION . . . . . . . =',E12.4)
 2100 FORMAT(
     . 5X,'  PART ID. . . . . . . . . . . . . . . . . =',I10)
 2200 FORMAT(
     . 5X,'  SUBSET ID. . . . . . . . . . . . . . . . =',I10)
 2300 FORMAT(
     . 5X,'GLOBAL SUBSET ENERGY (ALL PARTS) . . . . .')
 3000 FORMAT(
     . 5X,'  MINIMUM PART/SUBSET INTERNAL ENERGY. . . =',E12.4/
     . 5X,'  MAXIMUM PART/SUBSET INTERNAL ENERGY. . . =',E12.4/
     . 5X,'  MINIMUM PART/SUBSET KINETIC ENERGY . . . =',E12.4/
     . 5X,'  MAXIMUM PART/SUBSET KINETIC ENERGY . . . =',E12.4/
     . 5X,'  TOLERANCE OF CONSTANT INTERNAL ENERGY. . =',E12.4/
     . 5X,'  TOLERANCE OF CONSTANT KINETIC ENERGY . . =',E12.4/
     . 5X,'  DURATION LIMIT OF MAX/MIN ENERGY . . . . =',E12.4/
     . 5X,'  DURATION LIMIT OF CONSTANT INT ENERGY. . =',E12.4/
     . 5X,'  DURATION LIMIT OF CONSTANT KIN ENERGY. . =',E12.4//)
c-----------
      RETURN
      END
