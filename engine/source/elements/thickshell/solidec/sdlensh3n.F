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
Chd|  SDLENSH3N                     source/elements/thickshell/solidec/sdlensh3n.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SDLENSH3N(
     1   VOLN,    LLSH3N,  AREA, X1,      X2,
     2   X3,      X4,      X5,      X6,
     3   Y1,      Y2,      Y3,      Y4,
     4   Y5,      Y6,      Z1,      Z2,
     5   Z3,      Z4,      Z5,      Z6,
     6   NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real, DIMENSION(MVSIZ) , INTENT(OUT)  :: AREA 
      my_real
     .   VOLN(*),LLSH3N(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N
      my_real
     .        E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .        E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .        E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .        X31(MVSIZ), Y31(MVSIZ), Z31(MVSIZ),
     .        X32(MVSIZ), Y32(MVSIZ), Z32(MVSIZ),
     .        X21(MVSIZ), Y21(MVSIZ), Z21(MVSIZ), 
     .        X2L(MVSIZ), X3L(MVSIZ), Y3L(MVSIZ),
     .        XN(3,MVSIZ) , YN(3,MVSIZ) , ZN(3,MVSIZ) 
      my_real
     .   AL1,AL2,AL3,ALMAX,SUM
C=======================================================================
       DO I=1,NEL
          XN(1,I) = HALF*(X1(I)+X4(I))
          YN(1,I) = HALF*(Y1(I)+Y4(I))
          ZN(1,I) = HALF*(Z1(I)+Z4(I))
          XN(2,I) = HALF*(X2(I)+X5(I))
          YN(2,I) = HALF*(Y2(I)+Y5(I))
          ZN(2,I) = HALF*(Z2(I)+Z5(I))
          XN(3,I) = HALF*(X3(I)+X6(I))
          YN(3,I) = HALF*(Y3(I)+Y6(I))
          ZN(3,I) = HALF*(Z3(I)+Z6(I))
       ENDDO 
      DO I=1,NEL
        X21(I)=XN(2,I)-XN(1,I)
        Y21(I)=YN(2,I)-YN(1,I)
        Z21(I)=ZN(2,I)-ZN(1,I)
        X31(I)=XN(3,I)-XN(1,I)
        Y31(I)=YN(3,I)-YN(1,I)
        Z31(I)=ZN(3,I)-ZN(1,I)
        X32(I)=XN(3,I)-XN(2,I)
        Y32(I)=YN(3,I)-YN(2,I)
        Z32(I)=ZN(3,I)-ZN(2,I)
      ENDDO
C
      DO I=1,NEL
        E1X(I)= X21(I)
        E1Y(I)= Y21(I)
        E1Z(I)= Z21(I)
        X2L(I) = SQRT(E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I))
        E1X(I)=E1X(I)/X2L(I)
        E1Y(I)=E1Y(I)/X2L(I)
        E1Z(I)=E1Z(I)/X2L(I)
      ENDDO
C
      DO I=1,NEL
        E3X(I)=Y31(I)*Z32(I)-Z31(I)*Y32(I)
        E3Y(I)=Z31(I)*X32(I)-X31(I)*Z32(I)
        E3Z(I)=X31(I)*Y32(I)-Y31(I)*X32(I)
        SUM = SQRT(E3X(I)*E3X(I)+E3Y(I)*E3Y(I)+E3Z(I)*E3Z(I))
        E3X(I)=E3X(I)/SUM
        E3Y(I)=E3Y(I)/SUM
        E3Z(I)=E3Z(I)/SUM
        AREA(I) = HALF * SUM
      ENDDO
C
      DO I=1,NEL
        E2X(I)=E3Y(I)*E1Z(I)-E3Z(I)*E1Y(I)
        E2Y(I)=E3Z(I)*E1X(I)-E3X(I)*E1Z(I)
        E2Z(I)=E3X(I)*E1Y(I)-E3Y(I)*E1X(I)
        SUM = SQRT(E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I))
        E2X(I)=E2X(I)/SUM
        E2Y(I)=E2Y(I)/SUM
        E2Z(I)=E2Z(I)/SUM
        Y3L(I)=E2X(I)*X31(I)+E2Y(I)*Y31(I)+E2Z(I)*Z31(I)
        X3L(I)=E1X(I)*X31(I)+E1Y(I)*Y31(I)+E1Z(I)*Z31(I)
      ENDDO
      DO I=1,NEL
        AL1 = X2L(I) * X2L(I)      
        AL2 = (X3L(I)-X2L(I)) * (X3L(I)-X2L(I)) + Y3L(I) * Y3L(I)
        AL3 = X3L(I) * X3L(I) + Y3L(I) * Y3L(I)
        ALMAX = MAX(AL1,AL2,AL3)
        LLSH3N(I)= TWO*AREA(I) / SQRT(ALMAX)
      ENDDO
C
      RETURN
      END
