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
Chd|  W_IXALOC                      source/restart/ddsplit/w_ixaloc.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE W_IXALOC(IXA,NNOD,CEP,P,
     +                   NUMEL,NUMEL_L,NODLOCAL,LEN_IA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNOD, NUMEL, NUMEL_L, P, LEN_IA,
     .        IXA(NNOD,*), CEP(*), NODLOCAL(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, EL, NN
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IXTMP
C
! --------------------------
!     allocate 2d array
      ALLOCATE( IXTMP(NNOD,NUMEL_L) )
! --------------------------

      EL = 0
      DO I = 1, NUMEL
        IF(CEP(I)==P) THEN
          EL = EL + 1
          DO N = 1, NNOD
            NN = IXA(N,I)
            IF(NN/=0) THEN
              IXTMP(N,EL) = NODLOCAL(NN)
            ELSE
              IXTMP(N,EL) = 0
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
      CALL WRITE_I_C(IXTMP,NUMEL_L*NNOD)
      LEN_IA = LEN_IA + NUMEL_L*NNOD
C
! --------------------------
!     deallocate 2d array
      DEALLOCATE( IXTMP )
! --------------------------
      RETURN
      END
