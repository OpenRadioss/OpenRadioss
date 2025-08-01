Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    c_iloadp   ../starter/source/restart/ddsplit/c_iloadp.F
!||--- called by ------------------------------------------------------
!||    ddsplit    ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||    nlocal     ../starter/source/spmd/node/ddtools.F
!||====================================================================
      SUBROUTINE C_ILOADP(ILOADP,LLOADP,LLLOADP_L,PROC)
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
      INTEGER PROC, LLLOADP_L,
     .        ILOADP(SIZLOADP,*), LLOADP(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IAD, NL, N, ILP
C
      ILP = 0
      DO I = 1, NLOADP
        NL = ILOADP(1,I)
        IAD= ILOADP(4,I)
        DO J = 1, NL
          N = ABS(LLOADP(IAD+J-1))
          IF (N == 0)THEN
            ILP = ILP + 1
          ELSEIF (NLOCAL(N,PROC+1)==1)THEN
            ILP = ILP + 1
          ENDIF
        ENDDO
      ENDDO
C
      LLLOADP_L = ILP
C
      RETURN
      END
