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
Chd|  READ_SENSOR_TAB               source/output/restart/read_sensor_tab.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE READ_SENSOR_TAB(SENSOR_TAB,NSENSOR)
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
      INTEGER , INTENT(IN) :: NSENSOR
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR), TARGET :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ISEN,LEN,IAD,NFIX,NPARI,NPARR,NVAR,TYP
      my_real, DIMENSION(:), ALLOCATABLE :: HEAD,RBUF
      TYPE (SENSOR_STR_) ,POINTER :: SENSOR
C=======================================================================
      NFIX = 11
      ALLOCATE (HEAD(NFIX) )
c
      DO ISEN=1,NSENSOR
        SENSOR => SENSOR_TAB(ISEN)
        CALL READ_DB (HEAD,NFIX)
c
        SENSOR%TYPE    = NINT(HEAD(1))
        SENSOR%SENS_ID = NINT(HEAD(2))
        SENSOR%STATUS  = NINT(HEAD(3))
        SENSOR%TCRIT   = HEAD(4)
        SENSOR%TMIN    = HEAD(5)
        SENSOR%TDELAY  = HEAD(6)
        SENSOR%TSTART  = HEAD(7) 
        SENSOR%VALUE   = HEAD(8)
        SENSOR%NPARI   = NINT(HEAD(9))
        SENSOR%NPARR   = NINT(HEAD(10))
        SENSOR%NVAR    = NINT(HEAD(11))
c
        NPARI = SENSOR%NPARI
        NPARR = SENSOR%NPARR
        NVAR  = SENSOR%NVAR
        
        ALLOCATE(SENSOR%IPARAM(NPARI))
        ALLOCATE(SENSOR%RPARAM(NPARR))
        ALLOCATE(SENSOR%VAR(NVAR))
c
        LEN   = NPARI + NPARR + NVAR
        ALLOCATE (RBUF(LEN) )
        CALL READ_DB (RBUF,LEN)
c
        IAD = 0
        IF (NPARI > 0) THEN
          DO I = 1,NPARI
            SENSOR%IPARAM(I) = NINT(RBUF(IAD+I))
          END DO
          IAD = IAD + NPARI
        END IF
        IF (NPARR > 0) THEN
          DO I = 1,NPARR
            SENSOR%RPARAM(I) = RBUF(IAD+I)
          END DO
          IAD = IAD + NPARR
        END IF
        IF (NVAR > 0) THEN
          DO I = 1,NVAR
            SENSOR%VAR(I) = RBUF(IAD+I)
          END DO
          IAD = IAD + NVAR
        END IF
c         
        DEALLOCATE (RBUF)

        TYP = SENSOR%TYPE
        IF (TYP == 29 .OR. TYP == 30 .OR. TYP == 31)THEN

            ALLOCATE(SENSOR%INTEGER_USERPARAM(NSENPARI))
            ALLOCATE(SENSOR%FLOAT_USERPARAM(NSENPARR))

            ALLOCATE(SENSOR%INTEGER_USERBUF(ISENBUF))
            ALLOCATE(SENSOR%FLOAT_USERBUF(LSENBUF))

            CALL READ_I_C(SENSOR%INTEGER_USERPARAM,NSENPARI)
            CALL READ_I_C(SENSOR%INTEGER_USERBUF,ISENBUF)

            CALL READ_DB (SENSOR%FLOAT_USERPARAM,NSENPARR)
            CALL READ_DB (SENSOR%FLOAT_USERBUF,LSENBUF)

        ENDIF


      END DO
c
      DEALLOCATE (HEAD)
c-----------
      RETURN
      END
