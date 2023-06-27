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
Chd|  STOP_SENSOR                   source/tools/sensor/stop_sensor.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OUTPUT_MOD                    ../common_source/modules/output/output_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        STATE_MOD                     ../common_source/modules/state_mod.F
Chd|====================================================================
      SUBROUTINE STOP_SENSOR(SENSORS   ,H3D_DATA  ,DYNAIN_DATA,OUTPUT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
      USE MESSAGE_MOD
      USE H3D_MOD
      USE SENSOR_MOD
      USE ANIM_MOD
      USE STATE_MOD
      USE OUTPUT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "scr16_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SENSORS_)       ,INTENT(IN)    :: SENSORS
      TYPE (H3D_DATABASE)   ,INTENT(INOUT) :: H3D_DATA
      TYPE (DYNAIN_DATABASE),INTENT(INOUT) :: DYNAIN_DATA
      TYPE(OUTPUT_) ,INTENT(INOUT) :: OUTPUT !< output structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,ISENS
C=======================================================================
C     /STOP/LSENSOR : set output timer flags after sensor activation
C-----------------------------------------------
      IF (SENSORS%NSTOP > 0 .and. ISPMD == 0) THEN
        DO I=1,SENSORS%NSTOP
          ISENS = SENSORS%STOP(I)
          IF (SENSORS%SENSOR_TAB(ISENS)%STATUS == 1) THEN
            IF (NSTH   > 0) OUTPUT%TH%THIS  = TT
            IF (NSANIM > 0) TANIM = TT
            IF (NSSTAT > 0) TSTAT = TT
            IF (NSOUTP > 0) TOUTP = TT
            IF (NSH3D  > 0) H3D_DATA%TH3D = TT
            IF (NSDYNANIN > 0) DYNAIN_DATA%TDYNAIN = TT
            IF (NSABF > 0) THEN   !IF ABF FILE
              DO J=1,10 
                IF (ABFILE(J) >= 0) THEN
                   TABFIS(J) = TT
                   TABFWR(J) = TT
                ENDIF
              ENDDO
            ENDIF
            EXIT
          ENDIF
        ENDDO
      ENDIF
c-----------
      RETURN
      END
