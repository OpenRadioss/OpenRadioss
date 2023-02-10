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
Chd|  WRITE_SENSOR_TAB              source/tools/sensor/write_sensor_tab.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|====================================================================
      SUBROUTINE WRITE_SENSOR_TAB(SENSOR_TAB,NSENSOR,NODLOCAL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "scr17_c.inc"
#include "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN)    :: NSENSOR
      INTEGER , DIMENSION(*)  :: NODLOCAL
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR), TARGET :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ISEN,LEN,IAD,NFIX,NPARI,NPARR,NVAR,N1,N2,TYP
      INTEGER, DIMENSION(LTITR) :: ITITLE
      INTEGER, DIMENSION(:), ALLOCATABLE :: LTEMP
      my_real, DIMENSION(:), ALLOCATABLE :: RBUF
      CHARACTER(LEN = nchartitle) :: TITLE
      TYPE (SENSOR_STR_) ,POINTER :: SENSOR
C=======================================================================
      NFIX = 11
c
      DO ISEN=1,NSENSOR
        SENSOR => SENSOR_TAB(ISEN)
        TYP   = SENSOR%TYPE 
        NPARI = SENSOR%NPARI
        NPARR = SENSOR%NPARR
        NVAR  = SENSOR%NVAR
        TITLE = SENSOR_TAB(ISEN)%TITLE
        ALLOCATE (LTEMP(NPARI))
        LTEMP(1:NPARI) = SENSOR%IPARAM(1:NPARI)
c       update node system N° after renumbering for sensors using nodes
        IF (TYP == 2) THEN   ! /sens/disp
          N1 = NODLOCAL(SENSOR%IPARAM(1))
          N2 = NODLOCAL(SENSOR%IPARAM(2))
          LTEMP(1) = N1
          LTEMP(2) = N2
        ELSE IF (TYP == 9) THEN  ! /sens/vel
          N1 = NODLOCAL(SENSOR%IPARAM(1))
          LTEMP(1) = N1
        ELSE IF (TYP == 13) THEN  ! /sens/work
          N2 = SENSOR%IPARAM(2)
          N1 = NODLOCAL(SENSOR%IPARAM(1))
          IF (N2 > 0) THEN
            N2 = NODLOCAL(SENSOR%IPARAM(2))
          END IF
          LTEMP(1) = N1
          LTEMP(2) = N2
        ELSE IF (TYP == 15) THEN  ! /sens/dist_surf
          LTEMP(1) = NODLOCAL(SENSOR%IPARAM(1))
          IF (SENSOR%IPARAM(2) == 0) THEN
            LTEMP(3) = NODLOCAL(SENSOR%IPARAM(3))
            LTEMP(4) = NODLOCAL(SENSOR%IPARAM(4))
            LTEMP(5) = NODLOCAL(SENSOR%IPARAM(5))
          END IF
        END IF        
c        
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
            RBUF(IAD+I) = LTEMP(I)
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
        DEALLOCATE (LTEMP)
c
c       write sensor title
        CALL FRETITL(TITLE,ITITLE,LTITR)
        CALL WRITE_I_C(ITITLE,LTITR)
c
        IF (TYP==29.OR.TYP==30.OR.TYP==31)THEN
          CALL WRITE_I_C(SENSOR%INTEGER_USERPARAM,NSENPARI)
          CALL WRITE_I_C(SENSOR%INTEGER_USERBUF,ISENBUF)

          CALL WRITE_DB (SENSOR%FLOAT_USERPARAM,NSENPARR)
          CALL WRITE_DB (SENSOR%FLOAT_USERBUF,LSENBUF)

        ENDIF
c
      END DO
C Write Logical sensor index array
      CALL WRITE_I_C(LOGICAL_SENSOR_COUNT,1)
      CALL WRITE_I_C(LOGICAL_SENSORS_LIST,LOGICAL_SENSOR_COUNT)

c-----------
      RETURN
      END
