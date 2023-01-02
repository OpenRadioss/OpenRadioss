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
Chd|  EPXLE3                        source/elements/solid/solide/epxle3.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE EPXLE3(VOL,VEUL,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   PX5, PX6, PX7, PX8,
     .   PY5, PY6, PY7, PY8,
     .   PZ5, PZ6, PZ7, PZ8,
     .   PX1H1, PX1H2, PX1H3,
     .   PX2H1, PX2H2, PX2H3,
     .   PX3H1, PX3H2, PX3H3,
     .   PX4H1, PX4H2, PX4H3,
     .   DET,DELTAX,
     .   N1X, N2X, N3X, N4X, N5X, N6X,
     .   N1Y, N2Y, N3Y, N4Y, N5Y, N6Y,
     .   N1Z, N2Z, N3Z, N4Z, N5Z, N6Z)
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
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "fluid_scr_c.inc"
#include      "alefvm.inc"
#include      "alesolver.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   VOL(*), VEUL(LVEUL,*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),  
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   PX5(*), PX6(*), PX7(*), PX8(*),  
     .   PY5(*), PY6(*), PY7(*), PY8(*),  
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),  
     .   PX1H1(*), PX1H2(*), PX1H3(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*),
     .   DET(*),DELTAX(*)
      my_real
     .   N1X(*), N2X(*), N3X(*), N4X(*), N5X(*), N6X(*),
     .   N1Y(*), N2Y(*), N3Y(*), N4Y(*), N5Y(*), N6Y(*),
     .   N1Z(*), N2Z(*), N3Z(*), N4Z(*), N5Z(*), N6Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II
      my_real
     .   HX,HY,HZ,
     .   PMX1(MVSIZ), PMX2(MVSIZ), PMX3(MVSIZ), PMX4(MVSIZ),  
     .   PMY1(MVSIZ), PMY2(MVSIZ), PMY3(MVSIZ), PMY4(MVSIZ),  
     .   PMZ1(MVSIZ), PMZ2(MVSIZ), PMZ3(MVSIZ), PMZ4(MVSIZ) 
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(INTEG8==0)THEN
       DO  I=LFT,LLT
        II=I+NFT
        PX1(I)=VEUL(1,II)
        PX2(I)=VEUL(2,II)
        PX3(I)=VEUL(3,II)
        PX4(I)=VEUL(4,II)

        PY1(I)=VEUL(5,II)
        PY2(I)=VEUL(6,II)
        PY3(I)=VEUL(7,II)
        PY4(I)=VEUL(8,II)

        PZ1(I)=VEUL(9,II)
        PZ2(I)=VEUL(10,II)
        PZ3(I)=VEUL(11,II)
        PZ4(I)=VEUL(12,II)
       ENDDO
      ELSE
       DO  I=LFT,LLT
        II=I+NFT
        PX1(I)=VEUL(1,II)
        PX2(I)=VEUL(2,II)
        PX3(I)=VEUL(3,II)
        PX4(I)=VEUL(4,II)
        PX5(I)=VEUL(5,II)
        PX6(I)=VEUL(6,II)
        PX7(I)=VEUL(7,II)
        PX8(I)=VEUL(8,II)

        PY1(I)=VEUL(9,II)
        PY2(I)=VEUL(10,II)
        PY3(I)=VEUL(11,II)
        PY4(I)=VEUL(12,II)
        PY5(I)=VEUL(32,II)
        PY6(I)=VEUL(33,II)
        PY7(I)=VEUL(34,II)
        PY8(I)=VEUL(35,II)

        PZ1(I)=VEUL(36,II)
        PZ2(I)=VEUL(37,II)
        PZ3(I)=VEUL(38,II)
        PZ4(I)=VEUL(39,II)
        PZ5(I)=VEUL(40,II)
        PZ6(I)=VEUL(41,II)
        PZ7(I)=VEUL(42,II)
        PZ8(I)=VEUL(43,II)
       ENDDO
      ENDIF

      DO I=LFT,LLT
       II=I+NFT
       DELTAX(I)=VEUL(13,II)
       DET(I)=VOL(I)
      ENDDO
      IF(JHBE/=0)THEN
       IF(INTEG8==0)THEN
        DO I=LFT,LLT
         PMX1(I)=PX1(I)
         PMY1(I)=PY1(I)
         PMZ1(I)=PZ1(I)
         PMX2(I)=PX2(I)
         PMY2(I)=PY2(I)
         PMZ2(I)=PZ2(I)
         PMX3(I)=PX3(I)
         PMY3(I)=PY3(I)
         PMZ3(I)=PZ3(I)
         PMX4(I)=PX4(I)
         PMY4(I)=PY4(I)
         PMZ4(I)=PZ4(I)
        ENDDO
       ELSE 
        DO I=LFT,LLT
         PMX1(I)=HALF*(PX1(I)-PX7(I))
         PMY1(I)=HALF*(PY1(I)-PY7(I))
         PMZ1(I)=HALF*(PZ1(I)-PZ7(I))
         PMX2(I)=HALF*(PX2(I)-PX8(I))
         PMY2(I)=HALF*(PY2(I)-PY8(I))
         PMZ2(I)=HALF*(PZ2(I)-PZ8(I))
         PMX3(I)=HALF*(PX3(I)-PX5(I))
         PMY3(I)=HALF*(PY3(I)-PY5(I))
         PMZ3(I)=HALF*(PZ3(I)-PZ5(I))
         PMX4(I)=HALF*(PX4(I)-PX6(I))
         PMY4(I)=HALF*(PY4(I)-PY6(I))
         PMZ4(I)=HALF*(PZ4(I)-PZ6(I))	 
        ENDDO
       ENDIF
        DO I=LFT,LLT
         !  1 -1 1 -1 1 -1 1 -1
         HX=(X1(I)-X2(I)+X3(I)-X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY=(Y1(I)-Y2(I)+Y3(I)-Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ=(Z1(I)-Z2(I)+Z3(I)-Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
         PX1H1(I)=PMX1(I)*HX+ PMY1(I)*HY+PMZ1(I)*HZ
         PX2H1(I)=PMX2(I)*HX+ PMY2(I)*HY+PMZ2(I)*HZ
         PX3H1(I)=PMX3(I)*HX+ PMY3(I)*HY+PMZ3(I)*HZ
         PX4H1(I)=PMX4(I)*HX+ PMY4(I)*HY+PMZ4(I)*HZ
        ENDDO
        !  1 1 -1 -1 -1 -1 1 1
        DO I=LFT,LLT
         HX=(X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I))
         HY=(Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I))
         HZ=(Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I))
         PX1H2(I)=PMX1(I)*HX+ PMY1(I)*HY+PMZ1(I)*HZ
         PX2H2(I)=PMX2(I)*HX+ PMY2(I)*HY+PMZ2(I)*HZ
         PX3H2(I)=PMX3(I)*HX+ PMY3(I)*HY+PMZ3(I)*HZ
         PX4H2(I)=PMX4(I)*HX+ PMY4(I)*HY+PMZ4(I)*HZ
        ENDDO
        !  1 -1 -1 1 -1 1 1 -1
        DO I=LFT,LLT
         HX=(X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I))
         HY=(Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I))
         HZ=(Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I))
         PX1H3(I)=PMX1(I)*HX+ PMY1(I)*HY+PMZ1(I)*HZ
         PX2H3(I)=PMX2(I)*HX+ PMY2(I)*HY+PMZ2(I)*HZ
         PX3H3(I)=PMX3(I)*HX+ PMY3(I)*HY+PMZ3(I)*HZ
         PX4H3(I)=PMX4(I)*HX+ PMY4(I)*HY+PMZ4(I)*HZ
        ENDDO
      ENDIF

      IF(ICAA/=0 .OR. IALEFVM/=0 .OR. ISFINT>=2)THEN
        DO I=LFT,LLT
          II=I+NFT
          N1X(I)=VEUL(14,II)
          N2X(I)=VEUL(15,II)
          N3X(I)=VEUL(16,II)
          N4X(I)=VEUL(17,II)
          N5X(I)=VEUL(18,II)
          N6X(I)=VEUL(19,II)
          N1Y(I)=VEUL(20,II)
          N2Y(I)=VEUL(21,II)
          N3Y(I)=VEUL(22,II)
          N4Y(I)=VEUL(23,II)
          N5Y(I)=VEUL(24,II)
          N6Y(I)=VEUL(25,II)
          N1Z(I)=VEUL(26,II)
          N2Z(I)=VEUL(27,II)
          N3Z(I)=VEUL(28,II)
          N4Z(I)=VEUL(29,II)
          N5Z(I)=VEUL(30,II)
          N6Z(I)=VEUL(31,II)
        ENDDO
      ENDIF

      RETURN

      END
