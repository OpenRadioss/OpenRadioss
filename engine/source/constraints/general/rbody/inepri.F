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
!||    inepri    ../engine/source/constraints/general/rbody/inepri.F
!||--- called by ------------------------------------------------------
!||    r2r_rby   ../engine/source/coupling/rad2rad/r2r_init.F
!||    rbyact    ../engine/source/constraints/general/rbody/rbyact.F
!||--- calls      -----------------------------------------------------
!||    valpr     ../engine/source/constraints/general/rbody/valpr.F
!||====================================================================
      SUBROUTINE INEPRI(XI,BM)
C------------------------------------
C     CALCUL INERTIES ET REPERE PRINCIPAL
C------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real XI(3), BM(3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real XT(3,3), VP(3,3)
C-----------------------------------------------
      XT(1,1)=BM(1,1)
      XT(2,1)=BM(1,2)
      XT(3,1)=BM(2,2)
      XT(1,2)=BM(1,3)
      XT(2,2)=BM(2,3)
      XT(3,2)=BM(3,3)
      XT(1,3)=BM(1,3)
      XT(2,3)=BM(2,3)
      XT(3,3)=BM(3,3)
      DO I=1,3
        DO J=1,3
          VP(I,J)=0.
        ENDDO
      ENDDO
      CALL VALPR(XT,VP,3,0)
      XI(1)=XT(1,1)
      XI(2)=XT(3,1)
      XI(3)=XT(3,2)
      DO I=1,3
        DO J=1,3
          BM(I,J)=VP(I,J)
        ENDDO
      ENDDO
      RETURN
      END
