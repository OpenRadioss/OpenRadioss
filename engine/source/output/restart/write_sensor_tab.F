Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  WRITE_SENSOR_TAB              source/output/restart/write_sensor_tab.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE WRITE_SENSOR_TAB(SENSOR_TAB,NSENSOR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN)    :: NSENSOR
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR), TARGET :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ISEN,LEN,IAD,NFIX,NPARI,NPARR,NVAR
      my_real, DIMENSION(:), ALLOCATABLE :: RBUF
      TYPE (SENSOR_STR_) ,POINTER :: SENSOR
c=======================================================================
      NFIX = 11
c
      DO ISEN=1,NSENSOR
        SENSOR => SENSOR_TAB(ISEN)
        NPARI = SENSOR%NPARI
        NPARR = SENSOR%NPARR
        NVAR  = SENSOR%NVAR
        
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
      END DO
c-----------
      RETURN
      END
