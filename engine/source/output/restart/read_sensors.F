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
      !||    read_sensors           ../engine/source/output/restart/read_sensors.F
      !||--- called by ------------------------------------------------------
      !||    rdresb                 ../engine/source/output/restart/rdresb.F
      !||--- calls      -----------------------------------------------------
      !||    copy_python_function   ../common_source/modules/python_mod.F90
      !||    fretitl2               ../engine/source/input/freform.F
      !||    nvar                   ../engine/source/input/nvar.F
      !||    read_db                ../common_source/tools/input_output/read_db.F
      !||    read_dpdb              ../common_source/tools/input_output/read_db.F
      !||    read_i_c               ../common_source/tools/input_output/write_routtines.c
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../engine/share/message_module/message_mod.F
      !||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
      !||    python_funct_mod       ../common_source/modules/python_mod.F90
      !||    sensor_mod             ../common_source/modules/sensor_mod.F90
      !||====================================================================
      SUBROUTINE READ_SENSORS(SENSORS,PYTHON)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PYTHON_FUNCT_MOD
      USE SENSOR_MOD
      USE MESSAGE_MOD
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
      TYPE (SENSORS_) ,INTENT(INOUT) :: SENSORS
      TYPE(PYTHON_), INTENT(IN) :: PYTHON
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ISEN,LEN,IAD,NFIX,NPARI,NPARR,NVAR,TYP,NSENSOR,IERR
      INTEGER, DIMENSION(LTITR) :: ITITLE
      my_real, DIMENSION(:), ALLOCATABLE :: HEAD,RBUF
      CHARACTER(LEN = NCHARTITLE) :: TITLE
      INTEGER :: PYID
C=======================================================================
      NSENSOR = SENSORS%NSENSOR
      ALLOCATE(SENSORS%SENSOR_TAB(NSENSOR))
      
      IF (NSENSOR > 0) THEN
        NFIX = 11
        ALLOCATE (HEAD(NFIX) )
c
        DO ISEN=1,NSENSOR
          CALL READ_DB (HEAD,NFIX)
c
          SENSORS%SENSOR_TAB(ISEN)%TYPE    = NINT(HEAD(1))
          SENSORS%SENSOR_TAB(ISEN)%SENS_ID = NINT(HEAD(2))
          SENSORS%SENSOR_TAB(ISEN)%STATUS  = NINT(HEAD(3))
          SENSORS%SENSOR_TAB(ISEN)%TCRIT   = HEAD(4)
          SENSORS%SENSOR_TAB(ISEN)%TMIN    = HEAD(5)
          SENSORS%SENSOR_TAB(ISEN)%TDELAY  = HEAD(6)
          SENSORS%SENSOR_TAB(ISEN)%TSTART  = HEAD(7) 
          SENSORS%SENSOR_TAB(ISEN)%VALUE   = HEAD(8)
          SENSORS%SENSOR_TAB(ISEN)%NPARI   = NINT(HEAD(9))
          SENSORS%SENSOR_TAB(ISEN)%NPARR   = NINT(HEAD(10))
          SENSORS%SENSOR_TAB(ISEN)%NVAR    = NINT(HEAD(11))
c
          NPARI = SENSORS%SENSOR_TAB(ISEN)%NPARI
          NPARR = SENSORS%SENSOR_TAB(ISEN)%NPARR
          NVAR  = SENSORS%SENSOR_TAB(ISEN)%NVAR
          
          ALLOCATE(SENSORS%SENSOR_TAB(ISEN)%IPARAM(NPARI))
          ALLOCATE(SENSORS%SENSOR_TAB(ISEN)%RPARAM(NPARR))
          ALLOCATE(SENSORS%SENSOR_TAB(ISEN)%VAR(NVAR))
c
          LEN   = NPARI + NPARR + NVAR
          ALLOCATE (RBUF(LEN) )
          CALL READ_DB (RBUF,LEN)
c
          IAD = 0
          IF (NPARI > 0) THEN
            DO I = 1,NPARI
              SENSORS%SENSOR_TAB(ISEN)%IPARAM(I) = NINT(RBUF(IAD+I))
            END DO
            IAD = IAD + NPARI
          END IF
          IF (NPARR > 0) THEN
            DO I = 1,NPARR
              SENSORS%SENSOR_TAB(ISEN)%RPARAM(I) = RBUF(IAD+I)
            END DO
            IAD = IAD + NPARR
          END IF
          IF (NVAR > 0) THEN
            DO I = 1,NVAR
              SENSORS%SENSOR_TAB(ISEN)%VAR(I) = RBUF(IAD+I)
            END DO
            IAD = IAD + NVAR
          END IF
c           
          DEALLOCATE (RBUF)
c
c         read sensor title
          CALL READ_I_C(ITITLE,LTITR)
          CALL FRETITL2(TITLE,ITITLE,LTITR)
          SENSORS%SENSOR_TAB(ISEN)%TITLE = TITLE
c
          TYP = SENSORS%SENSOR_TAB(ISEN)%TYPE
          IF (TYP == 29 .OR. TYP == 30 .OR. TYP == 31)THEN

              ALLOCATE(SENSORS%SENSOR_TAB(ISEN)%INTEGER_USERPARAM(NSENPARI))
              ALLOCATE(SENSORS%SENSOR_TAB(ISEN)%FLOAT_USERPARAM(NSENPARR))

              ALLOCATE(SENSORS%SENSOR_TAB(ISEN)%INTEGER_USERBUF(ISENBUF))
              ALLOCATE(SENSORS%SENSOR_TAB(ISEN)%FLOAT_USERBUF(LSENBUF))

              CALL READ_I_C(SENSORS%SENSOR_TAB(ISEN)%INTEGER_USERPARAM,NSENPARI)
              CALL READ_I_C(SENSORS%SENSOR_TAB(ISEN)%INTEGER_USERBUF,ISENBUF)

              CALL READ_DB (SENSORS%SENSOR_TAB(ISEN)%FLOAT_USERPARAM,NSENPARR)
              CALL READ_DB (SENSORS%SENSOR_TAB(ISEN)%FLOAT_USERBUF,LSENBUF)

          ENDIF
          IF( TYP == 40) THEN ! python senso
             CALL READ_I_C(SENSORS%SENSOR_TAB(ISEN)%PYTHON_FUNCTION_ID,1)
              PYID = SENSORS%SENSOR_TAB(ISEN)%PYTHON_FUNCTION_ID
             CALL COPY_PYTHON_FUNCTION(PYTHON%FUNCTS(PYID),SENSORS%SENSOR_TAB(ISEN)%PYTHON_FUNCTION)
          ENDIF

        END DO
c
c--------------------------------------
c       Read Logical sensor index array
c
        CALL READ_I_C(SENSORS%LOGICAL_SENSOR_COUNT,1)

        ALLOCATE(SENSORS%LOGICAL_SENSORS_LIST(SENSORS%LOGICAL_SENSOR_COUNT))
        CALL READ_I_C(SENSORS%LOGICAL_SENSORS_LIST,SENSORS%LOGICAL_SENSOR_COUNT)
c
c--------------------------------------
c       Read spmd/PON exchange arrays
c
        ALLOCATE (SENSORS%FSAV(12,6,SENSORS%SFSAV)  ,STAT=IERR)
        ALLOCATE (SENSORS%TABSENSOR(SENSORS%STABSEN),STAT=IERR)

        CALL READ_DPDB(SENSORS%FSAV,12*6*SENSORS%SFSAV)

        CALL READ_I_C(SENSORS%TABSENSOR,SENSORS%STABSEN)

c--------------------------------------
        IF (MCHECK /= 0 ) THEN
           CALL READ_I_C(SENSORS%STOP ,SENSORS%NSTOP)   ! Must be read at any chkpt restart
        ENDIF
        IF (MCHECK /= 0 .and. ISPMD == 0) THEN
          CALL READ_I_C(SENSORS%STAT ,SENSORS%NSTAT)
          CALL READ_I_C(SENSORS%OUTP ,SENSORS%NOUTP)
          CALL READ_I_C(SENSORS%ANIM ,SENSORS%NANIM)
        END IF 
c
        DEALLOCATE (HEAD)
c
      END IF ! NSENSOR > 0
c--------------------------------------
      RETURN
      END
