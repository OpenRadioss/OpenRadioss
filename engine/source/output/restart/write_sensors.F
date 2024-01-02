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
Chd|  WRITE_SENSORS                 source/output/restart/write_sensors.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        FRETITL                       source/input/freform.F        
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_DPDB                    source/output/tools/write_db.F
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE WRITE_SENSORS(SENSORS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "scr07_c.inc"
#include "scr17_c.inc"
#include "task_c.inc"
#include "nchar_c.inc"
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
      CHARACTER(LEN = nchartitle) :: TITLE
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
        END DO
c
c--------------------------------------
c       Write Logical sensor index array
c--------------------------------------

        CALL WRITE_I_C(LOGICAL_SENSOR_COUNT,1)
        CALL WRITE_I_C(LOGICAL_SENSORS_LIST,LOGICAL_SENSOR_COUNT)
c
c--------------------------------------
c       Write spmd/PON exchange arrays
c--------------------------------------

        CALL WRITE_DPDB(SENSORS%FSAV,12*6*SENSORS%SFSAV)
        CALL WRITE_I_C(SENSORS%TABSENSOR,SENSORS%STABSEN)
c--------------------------------------
c       Write sensor lists in engine output options (/STOP/LSENSOR, /STAT/LSENSOR...)
c       in case of chkpoint restart
        IF (ISPMD == 0 .and. WMCHECK == 1) THEN
          CALL WRITE_I_C(SENSORS%STOP ,SENSORS%NSTOP)
          CALL WRITE_I_C(SENSORS%STAT ,SENSORS%NSTAT)
          CALL WRITE_I_C(SENSORS%OUTP ,SENSORS%NOUTP)
          CALL WRITE_I_C(SENSORS%ANIM ,SENSORS%NANIM)
        END IF
c
      END IF
c-----------
      RETURN
      END
