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
Chd|  ACCEL1                        source/tools/accele/accel1.F  
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ACCEL1(A,FF,A2,A1,A0,AS,VS,SKEW)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     . A(3),A0(3,2),A1(3,2),A2(3,2),AS(3),VS(3),SKEW(LSKEW),FF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,IFIRST
      my_real
     . PI1,PI8,PI38,SPI8,SPI38,C0,C1,C2,C3,C4,C5,C6,C7,C8,C9,
     . X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,D,DD,D2,DP,E,G,F
C
      AS(1) = A(1)*SKEW(1)
     .      + A(2)*SKEW(2)
     .      + A(3)*SKEW(3)
      AS(2) = A(1)*SKEW(4)
     .      + A(2)*SKEW(5)
     .      + A(3)*SKEW(6)
      AS(3) = A(1)*SKEW(7)
     .      + A(2)*SKEW(8)
     .      + A(3)*SKEW(9)
      VS(1) = VS(1)+AS(1)*DT12
      VS(2) = VS(2)+AS(2)*DT12
      VS(3) = VS(3)+AS(3)*DT12
      IF(FF==ZERO)RETURN
C
      F = MIN(FF,ZEP4/DT2)
C-----------------------------------------------
C INITIALIALISATION DES COEFFICIENTS DU FILTRE
C-----------------------------------------------
      PI1   = TWO*ATAN2(ONE,ZERO)
      PI8  = PI1*ONE_OVER_8
      PI38 = THREE*PI8
      SPI8  = SIN(PI8)
      SPI38 = SIN(PI38)
C
      D  = TAN(PI1*F*DT2)

      DD = D*D
      D2 = TWO*D
      DP = ONE + DD
      E  = D2*SPI8
      G  = E + DP
      G  = ONE/G
C
      C0 = DD * G
      C1 = TWO* C0
      C2 = C0
      C3 = TWO * G - C1
      C4 = (E - DP) * G
C
      E  = D2*SPI38
      G  = E + DP
      G  = ONE/G
C
      C5 = DD * G
      C6 = TWO * C5
      C7 = C5
      C8 = TWO * G - C6
      C9 = (E - DP) * G
C-----------------------------------------------
C FILTRAGE
C-----------------------------------------------
      DO J=1,3
        X1 = A0(J,2)
        X2 = A0(J,1)
        X3 = A(J)
        Y1 = A1(J,2)
        Y2 = A1(J,1)
        Y3 = C0 * X3 + C1 * X2 + C2 * X1
     .     + C3 * Y2 + C4 * Y1
        Z1 = A2(J,2)
        Z2 = A2(J,1)
        Z3 = C5 * Y3 + C6 * Y2 + C7 * Y1
     .     + C8 * Z2 + C9 * Z1
C
        A0(J,2) = X2
        A0(J,1) = X3
        A1(J,2) = Y2
        A1(J,1) = Y3
        A2(J,2) = Z2
        A2(J,1) = Z3
      ENDDO
C
      AS(1) = A2(1,1)*SKEW(1)
     .      + A2(2,1)*SKEW(2)
     .      + A2(3,1)*SKEW(3)
      AS(2) = A2(1,1)*SKEW(4)
     .      + A2(2,1)*SKEW(5)
     .      + A2(3,1)*SKEW(6)
      AS(3) = A2(1,1)*SKEW(7)
     .      + A2(2,1)*SKEW(8)
     .      + A2(3,1)*SKEW(9)
C
      RETURN
      END
