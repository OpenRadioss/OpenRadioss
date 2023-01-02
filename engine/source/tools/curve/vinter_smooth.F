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
Chd|  VINTER_SMOOTH                 source/tools/curve/vinter_smooth.F
Chd|-- called by -----------
Chd|        FIXTEMP                       source/constraints/thermic/fixtemp.F
Chd|        FIXVEL                        source/constraints/general/impvel/fixvel.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VINTER_SMOOTH(TF,IAD,IPOS ,ILEN,NEL0,X,DYDX,Y)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
      INTEGER ILEN(*),IPOS(*),IAD(*),NEL0
      my_real X(*),DYDX(*),Y(*),TF(2,*)
      INTEGER I,J1,J,ICONT,J2,J_FIRST,J_LAST
      my_real DYDX1,DYDX2,DYDX3,X_FIRST,X_LAST
C-----------------------------------------------
      J = 0
      ICONT = 1
      DO WHILE (ICONT == 1)
! 
       J = J+1
       ICONT = 0
       DO I=1,NEL0
         J1 = IPOS(I)+IAD(I)+1
         IF (J <= ILEN(I)-1 .AND. X(I) > TF(1,J1)) THEN
           IPOS(I) = IPOS(I) + 1
           ICONT = 1
         ELSEIF (IPOS(I) >= 1 .AND. X(I) < TF(1,J1-1)) THEN
           IPOS(I) = IPOS(I) - 1
           ICONT = 1
         ENDIF
       ENDDO
!
      ENDDO
!
!  smooth interpolation
C
       DO I=1,NEL0
C
        J_FIRST = IPOS(I)+IAD(I)
        J_LAST  = J_FIRST + 1
        X_FIRST = TF(1,J_FIRST)
        X_LAST  = TF(1,J_LAST)
C
        IF (X(I) <= X_FIRST) THEN
          Y(I)  = TF(2,J_FIRST)
        ELSEIF (X(I) >= X_LAST) THEN
          Y(I)  = TF(2,J_LAST)
        ELSE
! within interval
          J1   =IPOS(I)+IAD(I)
          J2   = J1+1
          DYDX(I)=(X(I)-TF(1,J1))/(TF(1,J2)-TF(1,J1))
!
          DYDX1 = DYDX(I)
          DYDX2 = DYDX1*DYDX1
          DYDX3 = DYDX1*DYDX2
!
          Y(I)  = TF(2,J1) + (TF(2,J2)-TF(2,J1))*DYDX3*
     .                       (10. - 15.*DYDX1 + 6.*DYDX2)
!
!!        Y(I)  = TF(2,J1) + (TF(2,J2)-TF(2,J1))*DYDX(I)**3*
!!     .                     (10. - 15.*DYDX(I) + 6.*DYDX(I)**2)
        ENDIF ! IF (X <= X_FIRST)
      ENDDO
!---
      RETURN
      END
