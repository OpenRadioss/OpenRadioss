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
!||    w_lacc      ../starter/source/restart/ddsplit/w_lacc.F
!||--- called by ------------------------------------------------------
!||    ddsplit     ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||====================================================================
      SUBROUTINE W_LACC(LACCELM,NC,NODLOCAL,LEN_IA,CEL,CEP,PROC)
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
      INTEGER NC, LEN_IA, PROC,
     .        LACCELM(3,*), NODLOCAL(*),CEL(*),CEP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, LACC(3,NC),P
C
      DO I = 1, NC
       N = LACCELM(1,I)
       LACC(1,I) = NODLOCAL(N)
C probleme de confusion entre les accelerometres et les gauges (meme
C structure) LACC(1,I)=0 marqueur de gauge
C  LACC(1,I)=0 ou negatif  marqueur de gauge
C  LACC(1,I)=NODLOCAL ou 2*NUMNOD (global) marqueur d'accelero
C
       if (LACC(1,I)==0)LACC(1,I)=2*NUMNOD
       LACC(2,I) = LACCELM(2,I)
       LACC(3,I) = LACCELM(3,I)
      ENDDO
      CALL WRITE_I_C(LACC,3*NC)
      LEN_IA = LEN_IA + 3*NC
C
      RETURN
      END
