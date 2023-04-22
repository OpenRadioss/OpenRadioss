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
Chd|  S8SBDEFO3                     source/elements/solid/solide8s/s8sbdefo3.F
Chd|-- called by -----------
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8SBDEFO3(
     1   ULX1,    ULX2,    ULX3,    ULX4,
     2   ULX5,    ULX6,    ULX7,    ULX8,
     3   ULY1,    ULY2,    ULY3,    ULY4,
     4   ULY5,    ULY6,    ULY7,    ULY8,
     5   ULZ1,    ULZ2,    ULZ3,    ULZ4,
     6   ULZ5,    ULZ6,    ULZ7,    ULZ8,
     7   X1,      X2,      X3,      X4,
     8   X5,      X6,      X7,      X8,
     9   Y1,      Y2,      Y3,      Y4,
     A   Y5,      Y6,      Y7,      Y8,
     B   Z1,      Z2,      Z3,      Z4,
     C   Z5,      Z6,      Z7,      Z8,
     D   INVJ,    KSI,     ETA,     ZETA,
     E   A11,     A12,     A13,     A21,
     F   A22,     A23,     A31,     A32,
     G   A33,     DN_R,    DN_S,    DN_T,
     H   BB,      DXX,     DXY,     DXZ,
     I   DYX,     DYY,     DYZ,     DZX,
     J   DZY,     DZZ,     D4,      D5,
     K   D6,      WXX,     WYY,     WZZ,
     L   VOLO,    OFF,     EINT,    OFFS,
     M   DSV,     ICP,     FAC,     SDV,
     N   I_SH,    IDEGE,   NEL,     ISMSTR)
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
#include      "com08_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER ICP,I_SH,IDEGE(*)
C     REAL
      my_real
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*), D4(*), D5(*), D6(*),
     .   WXX(*), WYY(*), WZZ(*), OFFS(*),DSV(*),
     .   VOLO(*),OFF(*),EINT(*),FAC(*),SDV(*),
     .   DN_X(MVSIZ,8),DN_Y(MVSIZ,8),DN_Z(MVSIZ,8),
     .   DN_R(8), DN_S(8), DN_T(8), INVJ(9,MVSIZ),
     .   ULX1(MVSIZ), ULX2(MVSIZ), ULX3(MVSIZ), ULX4(MVSIZ),
     .   ULX5(MVSIZ), ULX6(MVSIZ), ULX7(MVSIZ), ULX8(MVSIZ),
     .   ULY1(MVSIZ), ULY2(MVSIZ), ULY3(MVSIZ), ULY4(MVSIZ),
     .   ULY5(MVSIZ), ULY6(MVSIZ), ULY7(MVSIZ), ULY8(MVSIZ),
     .   ULZ1(MVSIZ), ULZ2(MVSIZ), ULZ3(MVSIZ), ULZ4(MVSIZ),
     .   ULZ5(MVSIZ), ULZ6(MVSIZ), ULZ7(MVSIZ), ULZ8(MVSIZ),
     .   X1(*),X2(*),X3(*),X4(*),X5(*),X6(*),X7(*),X8(*),
     .   Y1(*),Y2(*),Y3(*),Y4(*),Y5(*),Y6(*),Y7(*),Y8(*),
     .   Z1(*),Z2(*),Z3(*),Z4(*),Z5(*),Z6(*),Z7(*),Z8(*),
     .   A11(MVSIZ), A12(MVSIZ), A13(MVSIZ),
     .   A21(MVSIZ), A22(MVSIZ), A23(MVSIZ),
     .   A31(MVSIZ), A32(MVSIZ), A33(MVSIZ),
     .   BB(6,24,MVSIZ),
     .   KSI,ETA,ZETA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ,L,M,LM
C     REAL
      my_real
     .   DT1D2,DVC(MVSIZ),TOL,DT1D,UDT1,
     .   B(6,24),UM,UP,C(6,6),
     .   A1A1,A1A2,A1A3,A2A1,A2A2,A2A3,
     .   A1B1,A1B2,A1B3,A2B1,A2B2,A2B3,
     .   B1A1,B1A2,B1A3,B2A1,B2A2,B2A3,
     .   B1B1,B1B2,B1B3,B2B1,B2B2,B2B3,
     .   C1A1,C1A2,C1A3,C2A1,C2A2,C2A3,
     .   C1B1,C1B2,C1B3,C2B1,C2B2,C2B3,
     .   CA11,CA21,CA31,CA12,CA22,CA32,CA13,CA23,CA33,
     .   D11,D22,D33,D12,D13,D23
      DOUBLE PRECISION
     .   DVP,DV,DV1
C
      TOL = ONE-EM20
C-----------------------------------------------

      DO I=1,NEL
        DO J=1,8
          JJ = 3*(J-1)
          B(1,JJ+1) =  A11(I)*DN_T(J)
          B(1,JJ+2) =  A12(I)*DN_T(J)
          B(1,JJ+3) =  A13(I)*DN_T(J)
          B(2,JJ+1) =  A21(I)*DN_R(J)
          B(2,JJ+2) =  A22(I)*DN_R(J)
          B(2,JJ+3) =  A23(I)*DN_R(J)
          B(3,JJ+1) =  A31(I)*DN_S(J)
          B(3,JJ+2) =  A32(I)*DN_S(J)
          B(3,JJ+3) =  A33(I)*DN_S(J)
        ENDDO

! sortir cette partie car independante du PG
        A1A1 = (X2(I)+X3(I)-X1(I)-X4(I))*FOURTH
        A1A2 = (Y2(I)+Y3(I)-Y1(I)-Y4(I))*FOURTH
        A1A3 = (Z2(I)+Z3(I)-Z1(I)-Z4(I))*FOURTH
        A2A1 = (X4(I)+X3(I)-X1(I)-X2(I))*FOURTH
        A2A2 = (Y4(I)+Y3(I)-Y1(I)-Y2(I))*FOURTH
        A2A3 = (Z4(I)+Z3(I)-Z1(I)-Z2(I))*FOURTH
        A1B1 = (X6(I)+X7(I)-X5(I)-X8(I))*FOURTH
        A1B2 = (Y6(I)+Y7(I)-Y5(I)-Y8(I))*FOURTH
        A1B3 = (Z6(I)+Z7(I)-Z5(I)-Z8(I))*FOURTH
        A2B1 = (X8(I)+X7(I)-X5(I)-X6(I))*FOURTH
        A2B2 = (Y8(I)+Y7(I)-Y5(I)-Y6(I))*FOURTH
        A2B3 = (Z8(I)+Z7(I)-Z5(I)-Z6(I))*FOURTH
!
        UM = (ONE-ZETA)/EIGHT
        UP = (ONE+ZETA)/EIGHT
        B(4,1) = -UM*(A1A1+A2A1)
        B(4,2) = -UM*(A1A2+A2A2)
        B(4,3) = -UM*(A1A3+A2A3)
        B(4,4) =  UM*(A2A1-A1A1)
        B(4,5) =  UM*(A2A2-A1A2)
        B(4,6) =  UM*(A2A3-A1A3)
        B(4,7) =  UM*(A2A1+A1A1)
        B(4,8) =  UM*(A2A2+A1A2)
        B(4,9) =  UM*(A2A3+A1A3)
        B(4,10)=  UM*(A1A1-A2A1)
        B(4,11)=  UM*(A1A2-A2A2)
        B(4,12)=  UM*(A1A3-A2A3)
        B(4,13) =-UP*(A1B1+A2B1)
        B(4,14) =-UP*(A1B2+A2B2)
        B(4,15) =-UP*(A1B3+A2B3)
        B(4,16) = UP*(A2B1-A1B1)
        B(4,17) = UP*(A2B2-A1B2)
        B(4,18) = UP*(A2B3-A1B3)
        B(4,19) = UP*(A2B1+A1B1)
        B(4,20) = UP*(A2B2+A1B2)
        B(4,21) = UP*(A2B3+A1B3)
        B(4,22) = UP*(A1B1-A2B1)
        B(4,23) = UP*(A1B2-A2B2)
        B(4,24) = UP*(A1B3-A2B3)

! sortir cette partie car independante du PG
        B1A1 = (X2(I)+X6(I)-X1(I)-X5(I))*FOURTH
        B1A2 = (Y2(I)+Y6(I)-Y1(I)-Y5(I))*FOURTH
        B1A3 = (Z2(I)+Z6(I)-Z1(I)-Z5(I))*FOURTH
        B2A1 = (X5(I)+X6(I)-X1(I)-X2(I))*FOURTH
        B2A2 = (Y5(I)+Y6(I)-Y1(I)-Y2(I))*FOURTH
        B2A3 = (Z5(I)+Z6(I)-Z1(I)-Z2(I))*FOURTH
        B1B1 = (X3(I)+X7(I)-X4(I)-X8(I))*FOURTH
        B1B2 = (Y3(I)+Y7(I)-Y4(I)-Y8(I))*FOURTH
        B1B3 = (Z3(I)+Z7(I)-Z4(I)-Z8(I))*FOURTH
        B2B1 = (X7(I)+X8(I)-X3(I)-X4(I))*FOURTH
        B2B2 = (Y7(I)+Y8(I)-Y3(I)-Y4(I))*FOURTH
        B2B3 = (Z7(I)+Z8(I)-Z3(I)-Z4(I))*FOURTH
!
        UM = (ONE-ETA)/EIGHT
        UP = (ONE+ETA)/EIGHT
        B(5,1) = -UM*(B1A1+B2A1)
        B(5,2) = -UM*(B1A2+B2A2)
        B(5,3) = -UM*(B1A3+B2A3)
        B(5,4) =  UM*(B2A1-B1A1)
        B(5,5) =  UM*(B2A2-B1A2)
        B(5,6) =  UM*(B2A3-B1A3)
        B(5,7) =  UP*(B2B1-B1B1)
        B(5,8) =  UP*(B2B2-B1B2)
        B(5,9) =  UP*(B2B3-B1B3)
        B(5,10)= -UP*(B1B1+B2B1)
        B(5,11)= -UP*(B1B2+B2B2)
        B(5,12)= -UP*(B1B3+B2B3)
        B(5,13)=  UM*(B1A1-B2A1)
        B(5,14)=  UM*(B1A2-B2A2)
        B(5,15)=  UM*(B1A3-B2A3)
        B(5,16)=  UM*(B1A1+B2A1)
        B(5,17)=  UM*(B1A2+B2A2)
        B(5,18)=  UM*(B1A3+B2A3)
        B(5,19)=  UP*(B1B1+B2B1)
        B(5,20)=  UP*(B1B2+B2B2)
        B(5,21)=  UP*(B1B3+B2B3)
        B(5,22)=  UP*(B1B1-B2B1)
        B(5,23)=  UP*(B1B2-B2B2)
        B(5,24)=  UP*(B1B3-B2B3)

! sortir cette partie car independante du PG
        C1A1 = (X4(I)+X8(I)-X1(I)-X5(I))*FOURTH
        C1A2 = (Y4(I)+Y8(I)-Y1(I)-Y5(I))*FOURTH
        C1A3 = (Z4(I)+Z8(I)-Z1(I)-Z5(I))*FOURTH
        C2A1 = (X5(I)+X8(I)-X1(I)-X4(I))*FOURTH
        C2A2 = (Y5(I)+Y8(I)-Y1(I)-Y4(I))*FOURTH
        C2A3 = (Z5(I)+Z8(I)-Z1(I)-Z4(I))*FOURTH
        C1B1 = (X3(I)+X7(I)-X2(I)-X6(I))*FOURTH
        C1B2 = (Y3(I)+Y7(I)-Y2(I)-Y6(I))*FOURTH
        C1B3 = (Z3(I)+Z7(I)-Z2(I)-Z6(I))*FOURTH
        C2B1 = (X6(I)+X7(I)-X2(I)-X3(I))*FOURTH
        C2B2 = (Y6(I)+Y7(I)-Y2(I)-Y3(I))*FOURTH
        C2B3 = (Z6(I)+Z7(I)-Z2(I)-Z3(I))*FOURTH
!
        UM = (ONE-KSI)/EIGHT
        UP = (ONE+KSI)/EIGHT
        B(6,1) = -UM*(C1A1+C2A1)
        B(6,2) = -UM*(C1A2+C2A2)
        B(6,3) = -UM*(C1A3+C2A3)
        B(6,4) = -UP*(C1B1+C2B1)
        B(6,5) = -UP*(C1B2+C2B2)
        B(6,6) = -UP*(C1B3+C2B3)
        B(6,7) =  UP*(C2B1-C1B1)
        B(6,8) =  UP*(C2B2-C1B2)
        B(6,9) =  UP*(C2B3-C1B3)
        B(6,10)=  UM*(C2A1-C1A1)
        B(6,11)=  UM*(C2A2-C1A2)
        B(6,12)=  UM*(C2A3-C1A3)
        B(6,13)=  UM*(C1A1-C2A1)
        B(6,14)=  UM*(C1A2-C2A2)
        B(6,15)=  UM*(C1A3-C2A3)
        B(6,16)=  UP*(C1B1-C2B1)
        B(6,17)=  UP*(C1B2-C2B2)
        B(6,18)=  UP*(C1B3-C2B3)
        B(6,19)=  UP*(C1B1+C2B1)
        B(6,20)=  UP*(C1B2+C2B2)
        B(6,21)=  UP*(C1B3+C2B3)
        B(6,22)=  UM*(C1A1+C2A1)
        B(6,23)=  UM*(C1A2+C2A2)
        B(6,24)=  UM*(C1A3+C2A3)
!      ENDDO

!      DO I=1,NEL
        CA11 = INVJ(3,I)
        CA21 = INVJ(1,I)
        CA31 = INVJ(2,I)
        CA12 = INVJ(6,I)
        CA22 = INVJ(4,I)
        CA32 = INVJ(5,I)
        CA13 = INVJ(9,I)
        CA23 = INVJ(7,I)
        CA33 = INVJ(8,I)

        C(1,1) = CA11*CA11
        C(2,1) = CA12*CA12
        C(3,1) = CA13*CA13
        C(4,1) = TWO*CA11*CA12
        C(5,1) = TWO*CA11*CA13
        C(6,1) = TWO*CA12*CA13
        C(1,2) = CA21*CA21
        C(2,2) = CA22*CA22
        C(3,2) = CA23*CA23
        C(4,2) = TWO*CA21*CA22
        C(5,2) = TWO*CA21*CA23
        C(6,2) = TWO*CA22*CA23
        C(1,3) = CA31*CA31
        C(2,3) = CA32*CA32
        C(3,3) = CA33*CA33
        C(4,3) = TWO*CA31*CA32
        C(5,3) = TWO*CA31*CA33
        C(6,3) = TWO*CA32*CA33
        C(1,4) = CA11*CA21
        C(2,4) = CA12*CA22
        C(3,4) = CA13*CA23
        C(4,4) = CA12*CA21+CA11*CA22
        C(5,4) = CA13*CA21+CA11*CA23
        C(6,4) = CA12*CA23+CA13*CA22
        C(1,5) = CA11*CA31
        C(2,5) = CA12*CA32
        C(3,5) = CA13*CA33
        C(4,5) = CA12*CA31+CA11*CA32
        C(5,5) = CA13*CA31+CA11*CA33
        C(6,5) = CA12*CA33+CA13*CA32
        C(1,6) = CA21*CA31
        C(2,6) = CA22*CA32
        C(3,6) = CA23*CA33
        C(4,6) = CA22*CA31+CA21*CA32
        C(5,6) = CA23*CA31+CA21*CA33
        C(6,6) = CA23*CA32+CA22*CA33

        D11   =B(1,1)*ULX1(I)+B(1,2)*ULY1(I)+B(1,3)*ULZ1(I)
     .        +B(1,4)*ULX2(I)+B(1,5)*ULY2(I)+B(1,6)*ULZ2(I)
     .        +B(1,7)*ULX3(I)+B(1,8)*ULY3(I)+B(1,9)*ULZ3(I)
     .        +B(1,10)*ULX4(I)+B(1,11)*ULY4(I)+B(1,12)*ULZ4(I)
     .        +B(1,13)*ULX5(I)+B(1,14)*ULY5(I)+B(1,15)*ULZ5(I)
     .        +B(1,16)*ULX6(I)+B(1,17)*ULY6(I)+B(1,18)*ULZ6(I)
     .        +B(1,19)*ULX7(I)+B(1,20)*ULY7(I)+B(1,21)*ULZ7(I)
     .        +B(1,22)*ULX8(I)+B(1,23)*ULY8(I)+B(1,24)*ULZ8(I)
        D22   =B(2,1)*ULX1(I)+B(2,2)*ULY1(I)+B(2,3)*ULZ1(I)
     .        +B(2,4)*ULX2(I)+B(2,5)*ULY2(I)+B(2,6)*ULZ2(I)
     .        +B(2,7)*ULX3(I)+B(2,8)*ULY3(I)+B(2,9)*ULZ3(I)
     .        +B(2,10)*ULX4(I)+B(2,11)*ULY4(I)+B(2,12)*ULZ4(I)
     .        +B(2,13)*ULX5(I)+B(2,14)*ULY5(I)+B(2,15)*ULZ5(I)
     .        +B(2,16)*ULX6(I)+B(2,17)*ULY6(I)+B(2,18)*ULZ6(I)
     .        +B(2,19)*ULX7(I)+B(2,20)*ULY7(I)+B(2,21)*ULZ7(I)
     .        +B(2,22)*ULX8(I)+B(2,23)*ULY8(I)+B(2,24)*ULZ8(I)
        D33   =B(3,1)*ULX1(I)+B(3,2)*ULY1(I)+B(3,3)*ULZ1(I)
     .        +B(3,4)*ULX2(I)+B(3,5)*ULY2(I)+B(3,6)*ULZ2(I)
     .        +B(3,7)*ULX3(I)+B(3,8)*ULY3(I)+B(3,9)*ULZ3(I)
     .        +B(3,10)*ULX4(I)+B(3,11)*ULY4(I)+B(3,12)*ULZ4(I)
     .        +B(3,13)*ULX5(I)+B(3,14)*ULY5(I)+B(3,15)*ULZ5(I)
     .        +B(3,16)*ULX6(I)+B(3,17)*ULY6(I)+B(3,18)*ULZ6(I)
     .        +B(3,19)*ULX7(I)+B(3,20)*ULY7(I)+B(3,21)*ULZ7(I)
     .        +B(3,22)*ULX8(I)+B(3,23)*ULY8(I)+B(3,24)*ULZ8(I)
        D12   =B(4,1)*ULX1(I)+B(4,2)*ULY1(I)+B(4,3)*ULZ1(I)
     .        +B(4,4)*ULX2(I)+B(4,5)*ULY2(I)+B(4,6)*ULZ2(I)
     .        +B(4,7)*ULX3(I)+B(4,8)*ULY3(I)+B(4,9)*ULZ3(I)
     .        +B(4,10)*ULX4(I)+B(4,11)*ULY4(I)+B(4,12)*ULZ4(I)
     .        +B(4,13)*ULX5(I)+B(4,14)*ULY5(I)+B(4,15)*ULZ5(I)
     .        +B(4,16)*ULX6(I)+B(4,17)*ULY6(I)+B(4,18)*ULZ6(I)
     .        +B(4,19)*ULX7(I)+B(4,20)*ULY7(I)+B(4,21)*ULZ7(I)
     .        +B(4,22)*ULX8(I)+B(4,23)*ULY8(I)+B(4,24)*ULZ8(I)
        D13   =B(5,1)*ULX1(I)+B(5,2)*ULY1(I)+B(5,3)*ULZ1(I)
     .        +B(5,4)*ULX2(I)+B(5,5)*ULY2(I)+B(5,6)*ULZ2(I)
     .        +B(5,7)*ULX3(I)+B(5,8)*ULY3(I)+B(5,9)*ULZ3(I)
     .        +B(5,10)*ULX4(I)+B(5,11)*ULY4(I)+B(5,12)*ULZ4(I)
     .        +B(5,13)*ULX5(I)+B(5,14)*ULY5(I)+B(5,15)*ULZ5(I)
     .        +B(5,16)*ULX6(I)+B(5,17)*ULY6(I)+B(5,18)*ULZ6(I)
     .        +B(5,19)*ULX7(I)+B(5,20)*ULY7(I)+B(5,21)*ULZ7(I)
     .        +B(5,22)*ULX8(I)+B(5,23)*ULY8(I)+B(5,24)*ULZ8(I)
        D23   =B(6,1)*ULX1(I)+B(6,2)*ULY1(I)+B(6,3)*ULZ1(I)
     .        +B(6,4)*ULX2(I)+B(6,5)*ULY2(I)+B(6,6)*ULZ2(I)
     .        +B(6,7)*ULX3(I)+B(6,8)*ULY3(I)+B(6,9)*ULZ3(I)
     .        +B(6,10)*ULX4(I)+B(6,11)*ULY4(I)+B(6,12)*ULZ4(I)
     .        +B(6,13)*ULX5(I)+B(6,14)*ULY5(I)+B(6,15)*ULZ5(I)
     .        +B(6,16)*ULX6(I)+B(6,17)*ULY6(I)+B(6,18)*ULZ6(I)
     .        +B(6,19)*ULX7(I)+B(6,20)*ULY7(I)+B(6,21)*ULZ7(I)
     .        +B(6,22)*ULX8(I)+B(6,23)*ULY8(I)+B(6,24)*ULZ8(I)

        DXX(I) =C(1,1)*D11+C(1,2)*D22+C(1,3)*D33
     .         +C(1,4)*D12+C(1,5)*D13+C(1,6)*D23
        DYY(I) =C(2,1)*D11+C(2,2)*D22+C(2,3)*D33
     .         +C(2,4)*D12+C(2,5)*D13+C(2,6)*D23
        DZZ(I) =C(3,1)*D11+C(3,2)*D22+C(3,3)*D33
     .         +C(3,4)*D12+C(3,5)*D13+C(3,6)*D23
        D4(I)  =C(4,1)*D11+C(4,2)*D22+C(4,3)*D33
     .         +C(4,4)*D12+C(4,5)*D13+C(4,6)*D23
        D6(I)  =C(5,1)*D11+C(5,2)*D22+C(5,3)*D33
     .         +C(5,4)*D12+C(5,5)*D13+C(5,6)*D23
        D5(I)  =C(6,1)*D11+C(6,2)*D22+C(6,3)*D33
     .         +C(6,4)*D12+C(6,5)*D13+C(6,6)*D23
        DO L=1,6
          DO M=1,24
            BB(L,M,I) = ZERO
            DO LM=1,6
              BB(L,M,I) = BB(L,M,I)+C(L,LM)*B(LM,M)
            ENDDO
          ENDDO
        ENDDO
        DVC(I) =ZERO
      ENDDO
C-------------Volume traitement------
      IF (ICP==2) THEN
        DO I=1,NEL
          DVC(I)=(DSV(I)-DVC(I))*FAC(I)*DT1
        ENDDO
      ELSEIF (ICP==1) THEN
        DO I=1,NEL
          DVC(I)=(DSV(I)-DVC(I))*DT1
        ENDDO
      ENDIF
      IF ((ICP>0.AND.ISMSTR/=10.AND.ISMSTR/=12).OR.I_SH>1) THEN
        DO I=1,NEL
          DV =DVC(I)*OFF(I)
          IF(IDEGE(I)>10) DV = ZERO
          SDV(I) =DV
          IF (DV>TOL) THEN
            DV =ZERO
          ENDIF
          IF(OFFS(I)==TWO.OR.ISMSTR==11) CYCLE
          DV1 = ONE- DV
          VOLO(I) = VOLO(I)*DV1
          EINT(I) = EINT(I)/DV1
c        ENDIF
        ENDDO
      ELSE
        DO I=1,NEL
          SDV(I) =ZERO
        ENDDO
      ENDIF
C
      DT1D2=HALF*DT1
      IF (ISCAU>0)DT1D2=DT1
      DT1D=TWO*DT1D2
      UDT1 = ONE
      IF (DT1 > ZERO) UDT1 = ONE/DT1
      DO I=1,NEL
        DXX(I)= DXX(I)*UDT1
        DYY(I)= DYY(I)*UDT1
        DZZ(I)= DZZ(I)*UDT1
        D4(I) = D4(I)*UDT1
        D5(I) = D5(I)*UDT1
        D6(I) = D6(I)*UDT1
      ENDDO
C
      RETURN
      END
