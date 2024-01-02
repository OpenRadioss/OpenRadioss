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
Chd|  SENSOR_ENERGY_TOTAL           source/tools/sensor/sensor_energy_total.F
Chd|-- called by -----------
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_ENERGY_TOTAL(SENSORS ,ENINT  ,ENCIN  )  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real ,INTENT(IN) :: ENINT
      my_real ,INTENT(IN) :: ENCIN
      TYPE (SENSORS_) ,INTENT(INOUT) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER TYP,ISENS,ISELECT
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     this routine saves the total internal and kinematic system energy for 
C     /SENSOR/ENERGY
C=======================================================================
      IF (ISPMD==0) THEN
        DO ISENS = 1,SENSORS%NSENSOR
          TYP = SENSORS%SENSOR_TAB(ISENS)%TYPE
          IF (TYP == 14) THEN
            ISELECT = SENSORS%SENSOR_TAB(ISENS)%IPARAM(4)
            IF (ISELECT == 2) THEN  ! total system energy option is activated
              SENSORS%SENSOR_TAB(ISENS)%VAR(9)  = ENINT
              SENSORS%SENSOR_TAB(ISENS)%VAR(10) = ENCIN
            ENDIF
          ENDIF
        END DO  ! ISENS
      ENDIF
c-----------
      RETURN
      END
