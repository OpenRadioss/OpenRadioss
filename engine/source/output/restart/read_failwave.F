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
Chd|  READ_FAILWAVE                 source/output/restart/read_failwave.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        READ_I_C                      ../common_source/tools/input_output/write_routtines.c
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|====================================================================
      SUBROUTINE READ_FAILWAVE(FAILWAVE)
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
      TYPE (FAILWAVE_STR_) :: FAILWAVE 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IAD,IWAVE,LEN,SIZE,NDDL,NNOD
      INTEGER, DIMENSION(4)  :: HEAD
      INTEGER, DIMENSION(:), ALLOCATABLE :: RBUF
C=======================================================================
c     read failwave data
c
      CALL READ_I_C(HEAD, 4)
      IWAVE = HEAD(1) 
      NDDL  = HEAD(2) 
      SIZE  = HEAD(3) 
      NNOD  = HEAD(4) 
      FAILWAVE%WAVE_MOD = IWAVE
      FAILWAVE%NDDL     = NDDL
      FAILWAVE%SIZE     = SIZE
      FAILWAVE%NNOD     = NNOD
c
      IF (IWAVE > 0) THEN
        LEN = NDDL*NNOD*SIZE
        ALLOCATE (RBUF(LEN))
        ALLOCATE (FAILWAVE%INDX(NNOD))
        ALLOCATE (FAILWAVE%IDXI(NUMNOD))
        ALLOCATE (FAILWAVE%MAXLEV(NNOD))
        ALLOCATE (FAILWAVE%FWAVE_NOD(NDDL,NNOD,SIZE))
        ALLOCATE (FAILWAVE%FWAVE_NOD_STACK(NDDL,NNOD,SIZE))
        ALLOCATE (FAILWAVE%MAXLEV_STACK(NNOD))
        FAILWAVE%FWAVE_NOD_STACK(:,:,:)  = 0
        FAILWAVE%MAXLEV_STACK(:) = 0
c
        CALL READ_I_C(FAILWAVE%INDX, NNOD)
c
        CALL READ_I_C(FAILWAVE%IDXI, NUMNOD)
c
        CALL READ_I_C(FAILWAVE%MAXLEV, NNOD)
c
        CALL READ_I_C(RBUF, LEN)
c
        IAD = 0
        DO K = 1,SIZE
          DO J = 1,NNOD
            DO I = 1,NDDL
              IAD = IAD+1
              FAILWAVE%FWAVE_NOD(I,J,K) = RBUF(IAD)
            END DO 
          END DO 
        END DO 
c
        DEALLOCATE( RBUF )
      ELSE
        ALLOCATE (FAILWAVE%FWAVE_NOD(0,0,0))
        ALLOCATE (FAILWAVE%FWAVE_NOD_STACK(0,0,0))
        ALLOCATE (FAILWAVE%INDX(0))
        ALLOCATE (FAILWAVE%IDXI(0))
        ALLOCATE (FAILWAVE%MAXLEV(0))
        ALLOCATE (FAILWAVE%MAXLEV_STACK(0))
      ENDIF
c-----------
      RETURN
      END
