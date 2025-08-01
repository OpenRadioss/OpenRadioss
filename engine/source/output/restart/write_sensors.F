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
!||    write_sensors          ../engine/source/output/restart/write_sensors.F
!||--- called by ------------------------------------------------------
!||    wrrestp                ../engine/source/output/restart/wrrestp.F
!||--- calls      -----------------------------------------------------
!||    fretitl                ../engine/source/input/freform.F
!||    write_db               ../common_source/tools/input_output/write_db.F
!||    write_dpdb             ../common_source/tools/input_output/write_db.F
!||    write_i_c              ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||    sensor_mod             ../common_source/modules/sensor_mod.F90
!||====================================================================
      SUBROUTINE WRITE_SENSORS(SENSORS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SENSOR_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "scr07_c.inc"
#include "scr17_c.inc"
#include "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SENSORS_) ,INTENT(IN) ,TARGET :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ISEN,LEN,IAD,NFIX,NPARI,NPARR,NVAR,TYP
      INTEGER, DIMENSION(LTITR) :: ITITLE
      my_real, DIMENSION(:), ALLOCATABLE :: RBUF
      CHARACTER(LEN = NCHARTITLE) :: TITLE
      TYPE (SENSOR_STR_) ,POINTER :: SENSOR
C=======================================================================
      NFIX = 11
c
      IF (SENSORS%NSENSOR > 0) THEN
        DO ISEN = 1,SENSORS%NSENSOR
          SENSOR => SENSORS%SENSOR_TAB(ISEN)
          TYP   = SENSOR%TYPE 
          NPARI = SENSOR%NPARI
          NPARR = SENSOR%NPARR
          NVAR  = SENSOR%NVAR
          TITLE = SENSOR%TITLE
          
          LEN = NFIX + NPARI + NPARR + NVAR
          ALLOCATE (RBUF(LEN) )
c
          IAD = 0
          RBUF(IAD+1) = SENSOR%TYPE    
          RBUF(IAD+2) = SENSOR%SENS_ID 
          RBUF(IAD+3) = SENSOR%STATUS  
          RBUF(IAD+4) = SENSOR%TCRIT 
          RBUF(IAD+5) = SENSOR%TMIN  
          RBUF(IAD+6) = SENSOR%TDELAY
          RBUF(IAD+7) = SENSOR%TSTART
          RBUF(IAD+8) = SENSOR%VALUE 
          RBUF(IAD+9) = SENSOR%NPARI 
          RBUF(IAD+10)= SENSOR%NPARR 
          RBUF(IAD+11)= SENSOR%NVAR  
          IAD = IAD + NFIX
c
          IF (NPARI > 0) THEN
            DO I = 1,NPARI
              RBUF(IAD+I) = SENSOR%IPARAM(I)
            END DO
            IAD = IAD + NPARI
          END IF
          IF (NPARR > 0) THEN
            DO I = 1,NPARR
              RBUF(IAD+I) = SENSOR%RPARAM(I)
            END DO
            IAD = IAD + NPARR
          END IF        
          IF (NVAR > 0) THEN
            DO I = 1,NVAR
              RBUF(IAD+I) = SENSOR%VAR(I)
            END DO
            IAD = IAD + NVAR
          END IF
c
          CALL WRITE_DB (RBUF,LEN)
          DEALLOCATE (RBUF)
c
c         write sensor title
          CALL FRETITL(TITLE,ITITLE,LTITR)
          CALL WRITE_I_C(ITITLE,LTITR)
c
          IF (TYP==29.OR.TYP==30.OR.TYP==31)THEN
            CALL WRITE_I_C(SENSOR%INTEGER_USERPARAM,NSENPARI)
            CALL WRITE_I_C(SENSOR%INTEGER_USERBUF,ISENBUF)

            CALL WRITE_DB (SENSOR%FLOAT_USERPARAM,NSENPARR)
            CALL WRITE_DB (SENSOR%FLOAT_USERBUF,LSENBUF)

          ENDIF
          IF(TYP == 40 ) THEN
            CALL WRITE_I_C(SENSOR%PYTHON_FUNCTION_ID,1)
          ENDIF
        END DO
c
c--------------------------------------
c       Write Logical sensor index array
c--------------------------------------

        CALL WRITE_I_C(SENSORS%LOGICAL_SENSOR_COUNT,1)
        CALL WRITE_I_C(SENSORS%LOGICAL_SENSORS_LIST,SENSORS%LOGICAL_SENSOR_COUNT)
c
c--------------------------------------
c       Write spmd/PON exchange arrays
c--------------------------------------

        CALL WRITE_DPDB(SENSORS%FSAV,12*6*SENSORS%SFSAV)
        CALL WRITE_I_C(SENSORS%TABSENSOR,SENSORS%STABSEN)
c--------------------------------------
c       Write sensor lists in engine output options (/STOP/LSENSOR, /STAT/LSENSOR...)
c       in case of chkpoint restart
        IF (WMCHECK == 1) THEN
           CALL WRITE_I_C(SENSORS%STOP ,SENSORS%NSTOP) ! Must be read at any chkpt restart
        ENDIF

        IF (ISPMD == 0 .and. WMCHECK == 1) THEN
          CALL WRITE_I_C(SENSORS%STAT ,SENSORS%NSTAT)
          CALL WRITE_I_C(SENSORS%OUTP ,SENSORS%NOUTP)
          CALL WRITE_I_C(SENSORS%ANIM ,SENSORS%NANIM)
        END IF
c
      END IF
c-----------
      RETURN
      END
