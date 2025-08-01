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
!||    stop_sensor   ../engine/source/tools/sensor/stop_sensor.F
!||--- called by ------------------------------------------------------
!||    resol         ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    ancmsg        ../engine/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    anim_mod      ../common_source/modules/output/anim_mod.F
!||    h3d_mod       ../engine/share/modules/h3d_mod.F
!||    imp_dyna      ../engine/share/modules/impbufdef_mod.F
!||    message_mod   ../engine/share/message_module/message_mod.F
!||    output_mod    ../common_source/modules/output/output_mod.F90
!||    sensor_mod    ../common_source/modules/sensor_mod.F90
!||    state_mod     ../common_source/modules/state_mod.F
!||====================================================================
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
      TYPE (SENSORS_)       ,INTENT(INOUT)    :: SENSORS
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
      IF (SENSORS%NSTOP > 0 ) THEN
        DO I=1,SENSORS%NSTOP
          ISENS = SENSORS%STOP(I)         
          IF (SENSORS%SENSOR_TAB(ISENS)%STATUS == 1) THEN
            IF(ISPMD == 0)THEN
               CALL ANCMSG(MSGID=234,ANMODE=ANINFO,
     .                  I1 = SENSORS%SENSOR_TAB(ISENS)%SENS_ID)
            ENDIF

            MSTOP=1        ! Stops the computation at the end of cycle
            MREST = 1

            IF (SENSORS%STOP_NSTH   > 0) OUTPUT%TH%THIS  = TT
            IF (SENSORS%STOP_NSANIM > 0) TANIM = TT
            IF (SENSORS%STOP_NSSTAT > 0) TSTAT = TT
            IF (SENSORS%STOP_NSOUTP > 0) TOUTP = TT
            IF (SENSORS%STOP_NSH3D  > 0) H3D_DATA%TH3D = TT
            IF (NSDYNANIN > 0) DYNAIN_DATA%TDYNAIN = TT
            IF (SENSORS%STOP_NSABF > 0) THEN   !IF ABF FILE
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
