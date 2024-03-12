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
Chd|  TH_TIME_OUTPUT                source/output/th/th_time_output.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        OUTPUT_MOD                    ../common_source/modules/output/output_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE TH_TIME_OUTPUT(ITHOUT,SENSORS,OUTPUT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SENSOR_MOD
      USE OUTPUT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER         ,INTENT(INOUT) :: ITHOUT
      TYPE (SENSORS_) ,INTENT(IN)    :: SENSORS
      TYPE(OUTPUT_) ,INTENT(INOUT) :: OUTPUT !< output structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, J, ISENS, SENS_ID 
      my_real :: THIS_L, TS, TABFIS_L(10)
C---------------------------------------------------------------------------
C   ITHOUT : flag to check if TH file will be written for CAREA output
C--------------------------------------------------------------------------

C ******  IF /STOP/LSENSOR *****

       THIS_L = OUTPUT%TH%THIS
       TABFIS_L(1:10) =TABFIS(1:10)
       IF (SENSORS%NSTOP > 0) THEN
          DO I=1,SENSORS%NSTOP
             ISENS = SENSORS%STOP(I)
             TS = EP20
             IF (SENSORS%SENSOR_TAB(ISENS)%STATUS == 1) TS = TT
             IF (TT >= TS) THEN
                IF(SENSORS%STOP_NSTH/=0)THEN  !IF THIST FILE
                   THIS_L= TT
                ENDIF
                IF(SENSORS%STOP_NSABF/=0)THEN  !IF ABF FILE
                   DO J=1,10 
                     IF(ABFILE(J)/=0) THEN
                        TABFIS_L(J) = TT
                     ENDIF
                   ENDDO
                ENDIF

            ENDIF
          ENDDO
       ENDIF
C

         IF(TT>=THIS_L.
     .   OR.TT>=OUTPUT%TH%THIS1(1).OR.TT>=OUTPUT%TH%THIS1(2).
     .   OR.TT>=OUTPUT%TH%THIS1(3).OR.TT>=OUTPUT%TH%THIS1(4).OR.TT>=OUTPUT%TH%THIS1(5).
     .   OR.TT>=OUTPUT%TH%THIS1(6).OR.TT>=OUTPUT%TH%THIS1(7).OR.TT>=OUTPUT%TH%THIS1(8).
     .   OR.TT>=OUTPUT%TH%THIS1(9).
     .   OR.TT>=TABFIS_L(1).OR.TT>=TABFIS_L(2).
     .   OR.TT>=TABFIS_L(3).OR.TT>=TABFIS_L(4).OR.TT>=TABFIS_L(5).
     .   OR.TT>=TABFIS_L(6).OR.TT>=TABFIS_L(7).OR.TT>=TABFIS_L(8).
     .   OR.TT>=TABFIS_L(9).OR.TT>=TABFIS_L(10)) ITHOUT=1


      RETURN
      END SUBROUTINE TH_TIME_OUTPUT


