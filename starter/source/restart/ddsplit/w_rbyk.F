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
Chd|  W_RBYK                        source/restart/ddsplit/w_rbyk.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_RBYK(NPBY,LPBY,NODLOCAL,PROC,NSLARB_L,LEN_IA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "lagmult.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  PROC, LEN_IA, NSLARB_L, NODLOCAL(*),
     .         NPBY(NNPBY,*), LPBY(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSL, K, KK, N, NN, NSL_L, J, M, ILP
      INTEGER, DIMENSION(:), ALLOCATABLE :: LPBY_L
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: NPBY_L
C
! -------------------------
!     allocate 1d array
      ALLOCATE( LPBY_L(NSLARB_L) )
!     allocate 2d array
      ALLOCATE( NPBY_L(NNPBY,NRBYKIN) )
! -------------------------
      ILP = 0
      K = 0
      DO N = 1, NRBYKIN
        DO J = 3, NNPBY
          NPBY_L(J,N) = NPBY(J,N)
        ENDDO
        NPBY_L(11,N)=ILP
C
        NSL=NPBY(2,N)
        NSL_L = 0
        DO KK = 1, NSL
          NN = LPBY(K+KK)
          IF(NLOCAL(NN,PROC+1)==1)THEN
            NSL_L = NSL_L + 1
            LPBY_L(NSL_L+ILP) = NODLOCAL(NN)
          ENDIF
        ENDDO
        NPBY_L(2,N) = NSL_L
        M = NPBY(1,N)
C Skip useless rigid bodies
        IF(NSL_L==0.AND.NLOCAL(M,PROC+1)==0)THEN
          NPBY_L(1,N) = -M
        ELSE
          NPBY_L(1,N) = NODLOCAL(M)
        ENDIF
        K = K + NSL
        ILP = ILP + NSL_L
      ENDDO
C
      CALL WRITE_I_C(NPBY_L,NNPBY*NRBYKIN)

C write RBY LAG
      IF(NRBYLAG > 0 .AND. PROC == 0) THEN
        K = SLPBY
        DO N = NRBYKIN+1, NRBYKIN+NRBYLAG
          M = NPBY(1,N)
          IF(M /= 0)NPBY(1,N)=NODLOCAL(M)
          NSL=NPBY(2,N)
          DO KK = 1, NSL
            NN = LPBY(K+KK)
            IF(NN /= 0)LPBY(K+KK) = NODLOCAL(NN)
          END DO
          K = K + 3*NSL
        END DO
        CALL WRITE_I_C(NPBY(1,NRBYKIN+1),NNPBY*NRBYLAG)
      END IF

      CALL WRITE_I_C(LPBY_L,NSLARB_L)
      LEN_IA = LEN_IA + NNPBY*NRBYKIN + NSLARB_L

      IF(NRBYLAG > 0 .AND. PROC == 0) THEN
        CALL WRITE_I_C(LPBY(SLPBY+1),SLPBYL)
        LEN_IA = LEN_IA + NNPBY*NRBYLAG + SLPBYL
      END IF
C
      DEALLOCATE( LPBY_L )
      DEALLOCATE( NPBY_L )
      RETURN
      END
