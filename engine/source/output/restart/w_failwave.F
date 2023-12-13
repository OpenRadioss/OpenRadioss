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
Chd|  W_FAILWAVE                    source/output/restart/w_failwave.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|====================================================================
      SUBROUTINE W_FAILWAVE(FAILWAVE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAILWAVE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (FAILWAVE_STR_)    :: FAILWAVE 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IAD,IWAVE,NDDL,SIZE,NNOD,LEN
      INTEGER, DIMENSION(4)  :: HEAD
      INTEGER, DIMENSION(:), ALLOCATABLE :: RBUF
C=======================================================================
      IWAVE = FAILWAVE%WAVE_MOD
      NDDL  = FAILWAVE%NDDL
      SIZE  = FAILWAVE%SIZE
      NNOD  = FAILWAVE%NNOD
      HEAD(1) = IWAVE
      HEAD(2) = NDDL 
      HEAD(3) = SIZE 
      HEAD(4) = NNOD
      CALL WRITE_I_C(HEAD,4)
c
      IF (IWAVE > 0) THEN
        LEN = NNOD*NDDL*SIZE
        ALLOCATE( RBUF(LEN) )
        IAD = 0
        DO K = 1,SIZE
          DO J = 1,NNOD
            DO I = 1,NDDL
              IAD = IAD+1
              RBUF(IAD) = FAILWAVE%FWAVE_NOD(I,J,K)
            END DO 
          END DO 
        END DO 
c
        CALL WRITE_I_C(FAILWAVE%INDX  , NNOD)
        CALL WRITE_I_C(FAILWAVE%IDXI  , NUMNOD)
        CALL WRITE_I_C(FAILWAVE%MAXLEV, NNOD)
        CALL WRITE_I_C(RBUF, LEN)
c
        DEALLOCATE( RBUF )
      ENDIF
c--------------------------------
      RETURN
      END
