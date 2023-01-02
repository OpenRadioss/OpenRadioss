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
Chd|  W_IPARG                       source/restart/ddsplit/w_iparg.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE W_IPARG(IPARG,PROC,NGROUP_L,LEN_IA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC, NGROUP_L, LEN_IA, IPARG(NPARG,*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, NG_L, NFT_LOC, ITY, ITY_OLD, LB_L, J, LBUFELI,
     . SHIFT_XFE
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IPARG_L
C
! ---------------------
!     allocate 2d array
      ALLOCATE( IPARG_L(NPARG,NGROUP_L) )
! ---------------------
      NG_L = 0
      NFT_LOC = 0
      ITY_OLD = 0
      LB_L = 1
      DO NG = 1, NGROUP
        IF(IPARG(32,NG)==PROC) THEN
          NG_L = NG_L + 1
          DO J = 1, NPARG
            IPARG_L(J,NG_L) = IPARG(J,NG)
          ENDDO
          ITY = IPARG(5,NG)
          IF(ITY/=ITY_OLD) THEN
            NFT_LOC = 0
            ITY_OLD = ITY
          ENDIF
          IPARG_L(3,NG_L) = NFT_LOC
          IPARG_L(4,NG_L) = LB_L
          IF(IPARG(54,NG) > 0)THEN  ! if xfem
            SHIFT_XFE = IPARG(67,NG) - IPARG(4,NG)
            IPARG_L(67,NG_L) = IPARG_L(4,NG_L) + SHIFT_XFE
          ENDIF
          NFT_LOC = NFT_LOC + IPARG(2,NG)
          IF(NG<NGROUP) THEN
            LBUFELI = IPARG(4,NG+1) - IPARG(4,NG)
          ELSE
            LBUFELI = LBUFEL + 1 - IPARG(4,NG)
          ENDIF
          LB_L = LB_L + LBUFELI
        ENDIF
      ENDDO
C
      CALL WRITE_I_C(IPARG_L,NPARG*NGROUP_L)
      LEN_IA = LEN_IA + NPARG*NGROUP_L
C
! ---------------------
!     deallocate 2d array
      DEALLOCATE( IPARG_L )
! ---------------------
      RETURN
      END
