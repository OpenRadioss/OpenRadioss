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
Chd|  SDLENSH14                       source/elements/thickshell/solide8c/sdlensh14.F
Chd|====================================================================
      SUBROUTINE SDLENSH14(
     1   VOLN,    LLSH, AREA, X1,      X2,
     2   X3,      X4,      X5,      X6,
     3   X7,      X8,      Y1,      Y2,
     4   Y3,      Y4,      Y5,      Y6,
     5   Y7,      Y8,      Z1,      Z2,
     6   Z3,      Z4,      Z5,      Z6,
     7   Z7,      Z8,      ICS ,   NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,ICS
      my_real, DIMENSION(MVSIZ) , INTENT(OUT)  :: AREA,LLSH 
      my_real, DIMENSION(MVSIZ) , INTENT(IN)  ::  
     .   VOLN,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,  
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N
      my_real
     .   RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),
     .   XN(MVSIZ,4) , YN(MVSIZ,4) , ZN(MVSIZ,4) 
      my_real
     .   AL1,AL2,E3X,E3Y,E3Z,DET
C=======================================================================
        SELECT CASE(ICS)
          CASE (1)
            DO I=1,NEL
              XN(I,1) = HALF*(X1(I)+X4(I))
              YN(I,1) = HALF*(Y1(I)+Y4(I))
              ZN(I,1) = HALF*(Z1(I)+Z4(I))
              XN(I,2) = HALF*(X2(I)+X3(I))
              YN(I,2) = HALF*(Y2(I)+Y3(I))
              ZN(I,2) = HALF*(Z2(I)+Z3(I))
              XN(I,3) = HALF*(X6(I)+X7(I))
              YN(I,3) = HALF*(Y6(I)+Y7(I))
              ZN(I,3) = HALF*(Z6(I)+Z7(I))
              XN(I,4) = HALF*(X5(I)+X8(I))
              YN(I,4) = HALF*(Y5(I)+Y8(I))
              ZN(I,4) = HALF*(Z5(I)+Z8(I))
            ENDDO 
          CASE (10)
            DO I=1,NEL
              XN(I,1) = HALF*(X1(I)+X5(I))
              YN(I,1) = HALF*(Y1(I)+Y5(I))
              ZN(I,1) = HALF*(Z1(I)+Z5(I))
              XN(I,2) = HALF*(X2(I)+X6(I))
              YN(I,2) = HALF*(Y2(I)+Y6(I))
              ZN(I,2) = HALF*(Z2(I)+Z6(I))
              XN(I,3) = HALF*(X3(I)+X7(I))
              YN(I,3) = HALF*(Y3(I)+Y7(I))
              ZN(I,3) = HALF*(Z3(I)+Z7(I))
              XN(I,4) = HALF*(X4(I)+X8(I))
              YN(I,4) = HALF*(Y4(I)+Y8(I))
              ZN(I,4) = HALF*(Z4(I)+Z8(I))
            ENDDO 
          CASE (100)
            DO I=1,NEL
              XN(I,1) = HALF*(X1(I)+X2(I))
              YN(I,1) = HALF*(Y1(I)+Y2(I))
              ZN(I,1) = HALF*(Z1(I)+Z2(I))
              XN(I,2) = HALF*(X5(I)+X6(I))
              YN(I,2) = HALF*(Y5(I)+Y6(I))
              ZN(I,2) = HALF*(Z5(I)+Z6(I))
              XN(I,3) = HALF*(X8(I)+X7(I))
              YN(I,3) = HALF*(Y8(I)+Y7(I))
              ZN(I,3) = HALF*(Z8(I)+Z7(I))
              XN(I,4) = HALF*(X4(I)+X3(I))
              YN(I,4) = HALF*(Y4(I)+Y3(I))
              ZN(I,4) = HALF*(Z4(I)+Z3(I))
            ENDDO 
        END SELECT
C------g1,g2 :
        DO I=1,NEL
          RX(I)=XN(I,2)+XN(I,3)-XN(I,1)-XN(I,4)
          RY(I)=YN(I,2)+YN(I,3)-YN(I,1)-YN(I,4)
          RZ(I)=ZN(I,2)+ZN(I,3)-ZN(I,1)-ZN(I,4)
          SX(I)=XN(I,3)+XN(I,4)-XN(I,1)-XN(I,2)
          SY(I)=YN(I,3)+YN(I,4)-YN(I,1)-YN(I,2)
          SZ(I)=ZN(I,3)+ZN(I,4)-ZN(I,1)-ZN(I,2)
        ENDDO 
        DO I=1,NEL
          E3X = RY(I) * SZ(I) - RZ(I) * SY(I) 
          E3Y = RZ(I) * SX(I) - RX(I) * SZ(I) 
          E3Z = RX(I) * SY(I) - RY(I) * SX(I) 
          DET = SQRT(E3X*E3X + E3Y*E3Y + E3Z*E3Z)
          AREA(I)=FOURTH*DET
          AL1 = RX(I)*RX(I) + RY(I)*RY(I)+ RZ(I)*RZ(I)
          AL2 = SX(I)*SX(I) + SY(I)*SY(I)+ SZ(I)*SZ(I)
          LLSH(I) = FOURTH*MAX(AL1,AL2)
        ENDDO 
C
      RETURN
      END
