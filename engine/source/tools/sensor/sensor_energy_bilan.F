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
!||    sensor_energy_bilan   ../engine/source/tools/sensor/sensor_energy_bilan.F
!||--- called by ------------------------------------------------------
!||    c3bilan               ../engine/source/elements/sh3n/coque3n/c3bilan.F
!||    cbilan                ../engine/source/elements/shell/coque/cbilan.F
!||    ige3dbilan            ../engine/source/elements/ige3d/ige3dbilan.F
!||    pbilan                ../engine/source/elements/beam/pbilan.F
!||    qbilan                ../engine/source/elements/solid_2d/quad/qbilan.F
!||    r3bilan               ../engine/source/elements/spring/r3bilan.F
!||    r5bilan               ../engine/source/elements/spring/r5bilan.F
!||    rbilan                ../engine/source/elements/spring/rbilan.F
!||    s10bilan              ../engine/source/elements/solid/solide10/s10bilan.F
!||    s16bilan              ../engine/source/elements/thickshell/solide16/s16bilan.F
!||    s20bilan              ../engine/source/elements/solid/solide20/s20bilan.F
!||    s4bilan               ../engine/source/elements/solid/solide4/s4bilan.F
!||    s6cbilan              ../engine/source/elements/thickshell/solide6c/s6cbilan.F
!||    s8bilan               ../engine/source/elements/solid/solide8/s8bilan.F
!||    sbilan                ../engine/source/elements/solid/solide/sbilan.F
!||    sr8bilan              ../engine/source/elements/solid/solide8/sr8bilan.F
!||    srbilan               ../engine/source/elements/solid/solide/srbilan.F
!||    tbilan                ../engine/source/elements/truss/tbilan.F
!||--- calls      -----------------------------------------------------
!||    sum_6_float           ../engine/source/system/parit.F
!||--- uses       -----------------------------------------------------
!||    sensor_mod            ../common_source/modules/sensor_mod.F90
!||====================================================================
      SUBROUTINE SENSOR_ENERGY_BILAN(JFT,JLT,EI,EK,OFF,IPART,ITASK,SENSORS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(in) :: JFT,JLT
      INTEGER, INTENT(in) :: ITASK
      INTEGER, DIMENSION(*), INTENT(in) ::  IPART
C     REAL
      my_real, DIMENSION(*), INTENT(in) :: EI,EK
      my_real, DIMENSION(*), INTENT(in) :: OFF
      type (sensors_),INTENT(INOUT) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,II,J,IC,NEL,IJK
      INTEGER :: K,IPART_SENS,LOCAL_INDEX
      my_real, DIMENSION(MVSIZ,2) :: FSTPARIT   
      INTEGER :: NGR_SENSOR
C-----------------------------------------------
      NGR_SENSOR=SENSORS%NGR_SENSOR(ITASK+1)
      IF(IPARIT>0) THEN
         IF(SENSORS%SENS_GROUP(NGR_SENSOR)%NUM_PART>0) THEN
            IF(SENSORS%SENS_GROUP(NGR_SENSOR)%NUM_PART==1) THEN
                DO I=JFT,JLT
                    IF( OFF(I)/=ZERO ) THEN
                        FSTPARIT(I,1) = EI(I)
                        FSTPARIT(I,2) = EK(I)
                    ELSE
                        FSTPARIT(I,1) = ZERO
                        FSTPARIT(I,2) = ZERO
                    ENDIF
                ENDDO
                NEL = JLT-JFT+1
                K = SENSORS%SENS_GROUP(NGR_SENSOR)%PART(1,3)    !   id of the sensor
                IJK = ITASK+1
                CALL SUM_6_FLOAT(1,NEL,FSTPARIT(1,1),SENSORS%SENSOR_STRUCT(K)%FBSAV6_SENS(1,1,IJK),2)
                CALL SUM_6_FLOAT(1,NEL,FSTPARIT(1,2),SENSORS%SENSOR_STRUCT(K)%FBSAV6_SENS(2,1,IJK),2)
            ELSEIF(SENSORS%SENS_GROUP(NGR_SENSOR)%NUM_PART>1) THEN

               DO J=1,SENSORS%SENS_GROUP(NGR_SENSOR)%NUM_PART
                    IPART_SENS = SENSORS%SENS_GROUP(NGR_SENSOR)%PART(J,1)   !   id of the part linked to the sensor
                    LOCAL_INDEX = 0     
                    DO I=JFT,JLT
                        MX=IPART(I)
                        IF( (IPART_SENS==MX).AND.(OFF(I)/=ZERO) ) THEN
                            LOCAL_INDEX = LOCAL_INDEX + 1
                            FSTPARIT(LOCAL_INDEX,1) = EI(I)
                            FSTPARIT(LOCAL_INDEX,2) = EK(I)   
                        ENDIF
                    ENDDO

                    K = SENSORS%SENS_GROUP(NGR_SENSOR)%PART(J,3)   !   id of the sensor
                    IJK = ITASK+1
                    CALL SUM_6_FLOAT(1,LOCAL_INDEX,FSTPARIT(1,1),SENSORS%SENSOR_STRUCT(K)%FBSAV6_SENS(1,1,IJK),2)
                    CALL SUM_6_FLOAT(1,LOCAL_INDEX,FSTPARIT(1,2),SENSORS%SENSOR_STRUCT(K)%FBSAV6_SENS(2,1,IJK),2)
                ENDDO
            ENDIF
         ENDIF
      ENDIF          
C---
      RETURN
      END SUBROUTINE SENSOR_ENERGY_BILAN
