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
C
Chd|====================================================================
Chd|  W_LLINK                       source/restart/ddsplit/w_llink.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_LLINK(
     .   NNLINK,LLLINK,NODLOCAL,PROC,NLLINK_L,LEN_IA)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC,NLLINK_L,LEN_IA,
     .        NNLINK(10,*), LLLINK(*), NODLOCAL(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, KL, NL, N, NLL
      INTEGER, DIMENSION(:), ALLOCATABLE :: LLLTMP
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: NNLTMP
C
! ----------------------
!     allocate 1d array
      ALLOCATE( LLLTMP(NLLINK_L) )
!     allocate 2d array
      ALLOCATE( NNLTMP(10,NLINK) )
! ----------------------
      K  = 0
      KL = 0
      DO I = 1, NLINK
        NL = NNLINK(1,I)
        DO J = 1, 10
          NNLTMP(J,I) = NNLINK(J,I)
        ENDDO
        NLL = 0
        DO J = 1, NL
          N = LLLINK(K+J)
          IF (NLOCAL(N,PROC+1)==1)THEN
            NLL = NLL + 1
            LLLTMP(KL+NLL) = NODLOCAL(N)
          ENDIF
        ENDDO
        KL = KL + NLL
        K  = K  + NL
        NNLTMP(1,I) = NLL
      ENDDO
C
      CALL WRITE_I_C(NNLTMP,10*NLINK)
      CALL WRITE_I_C(LLLTMP,NLLINK_L)
      LEN_IA = LEN_IA + NLLINK_L + 10*NLINK
C
! ----------------------
!     deallocate 1d array
      DEALLOCATE( LLLTMP )
!     deallocate 2d array
      DEALLOCATE( NNLTMP )
! ----------------------
      RETURN
      END
