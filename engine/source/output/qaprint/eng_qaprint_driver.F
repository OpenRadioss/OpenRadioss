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
      !||====================================================================
      !||    eng_qaprint_driver                 ../engine/source/output/qaprint/eng_qaprint_driver.F
      !||--- called by ------------------------------------------------------
      !||    sortie_main                        ../engine/source/output/sortie_main.F
      !||--- calls      -----------------------------------------------------
      !||    eng_qaprint_animinput              ../engine/source/output/qaprint/eng_qaprint_animinput.F
      !||    eng_qaprint_dtinput                ../engine/source/output/qaprint/eng_qaprint_dtinput.F
      !||    eng_qaprint_generalcontrolsinput   ../engine/source/output/qaprint/eng_qaprint_generalcontrolsinput.F
      !||--- uses       -----------------------------------------------------
      !||    dt_mod                             ../engine/source/modules/dt_mod.F
      !||    output_mod                         ../common_source/modules/output/output_mod.F90
      !||    qa_out_mod                         ../common_source/qa/qa_out_mod.F
      !||    sensor_mod                         ../engine/share/modules/sensor_mod.F
      !||====================================================================
      SUBROUTINE ENG_QAPRINT_DRIVER(IRUNN_BIS ,SENSORS,DT,OUTPUT,IPARIT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE SENSOR_MOD
      USE DT_MOD
      USE OUTPUT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRUNN_BIS
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
      TYPE (DT_)  , INTENT(INOUT) :: DT
      TYPE(OUTPUT_), INTENT(INOUT) :: OUTPUT !< output structure
      INTEGER,INTENT(IN)::  IPARIT
C=======================================================================

      ! doqa is set in QA_OUT_MOD depending on DO_QA env variable setting
      ! we skip the specific QAPRINT treatment if doqa is != 1
      IF ( doqa /= 1  ) RETURN

      IF(NCYCLE == 0) THEN ! ECRITURE QAPRINT AU CYCLE 0 ENGINE
        CALL ENG_QAPRINT_GENERALCONTROLSINPUT(IRUNN_BIS,OUTPUT,IPARIT,SENSORS)

        CALL ENG_QAPRINT_DTINPUT(DT)

        CALL ENG_QAPRINT_ANIMINPUT(SENSORS)
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END
