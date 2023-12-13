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
Chd|  W_FAILWAVE                    source/restart/ddsplit/w_failwave.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|====================================================================
      SUBROUTINE W_FAILWAVE(FAILWAVE,NODGLOB,NUMNOD,NUMNOD_L,LEN_AM,ITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAILWAVE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN)    :: NUMNOD,NUMNOD_L
      INTEGER , INTENT(INOUT) :: LEN_AM
      INTEGER , DIMENSION(NUMNOD_L) , INTENT(IN) :: NODGLOB
      TYPE (FAILWAVE_STR_)    :: FAILWAVE 
      INTEGER ITAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IWAVE,NNOD,NDDL,SIZE,LEN,NNOD_L,IAD,NG,NL,NN
      INTEGER, DIMENSION(NUMNOD_L) :: INDX_LOC,IDXI_LOC
      INTEGER, DIMENSION(4)  :: HEAD
      INTEGER, DIMENSION(:), ALLOCATABLE :: RBUF
C=======================================================================
      IWAVE = FAILWAVE%WAVE_MOD
      NNOD  = FAILWAVE%NNOD
      NDDL  = FAILWAVE%NDDL
      SIZE  = FAILWAVE%SIZE
c
      IF (IWAVE == 0) THEN
        HEAD(:) = ZERO
        CALL WRITE_I_C(HEAD,4)
        LEN_AM = LEN_AM + 4
c
      ELSE
c
        NNOD_L = 0
        IDXI_LOC(:) = 0
        DO I = 1,NUMNOD_L
          NG = NODGLOB(I)
          NN = FAILWAVE%IDXI(NG)
          IF (NN > 0) THEN
            NNOD_L = NNOD_L + 1
            INDX_LOC(NNOD_L) = I
            IDXI_LOC(I) = NNOD_L
          ENDIF
        ENDDO
c        
        HEAD(1) = IWAVE
        HEAD(2) = NDDL 
        HEAD(3) = SIZE 
        HEAD(4) = NNOD_L
c
        CALL WRITE_I_C(HEAD,4)
        LEN_AM = LEN_AM + 4
c
        CALL WRITE_I_C(INDX_LOC,NNOD_L)
        LEN_AM = LEN_AM + NNOD_L
c
        CALL WRITE_I_C(IDXI_LOC,NUMNOD_L)
        LEN_AM = LEN_AM + NUMNOD_L
c
        LEN = NNOD_L*NDDL*SIZE + NNOD_L
        ALLOCATE( RBUF(LEN) )
        RBUF(:) = 0
        CALL WRITE_I_C(RBUF,LEN)
        LEN_AM = LEN_AM + LEN
c---
        DEALLOCATE( RBUF )
      ENDIF
c--------------------------------
      RETURN
      END
