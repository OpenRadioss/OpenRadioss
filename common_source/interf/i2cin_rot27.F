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
Chd|  I2CIN_ROT27                   interf/i2cin_rot27.F          
Chd|-- called by -----------
Chd|        I2_DTN_27_CIN                 starter/source/interfaces/inter3d1/i2_dtn_27.F
Chd|        I2FOR27P_CIN                  engine/source/interfaces/interf/i2for27p_cin.F
Chd|        I2FOR27_CIN                   engine/source/interfaces/interf/i2for27_cin.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2CIN_ROT27(STBRK,RS,RM,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,
     .                       X4,Y4,Z4,DPARA,DWDU,E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,
     .                       NIR,BETAX,BETAY)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER NIR
      my_real
     .   STBRK,RS(3),RM(3),DPARA(7),X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,DWDU,
     .   E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,BETAX,BETAY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real 
     .   R(3),
     .   X12,X22,X32,X42,Y12,Y22,Y32,Y42,Z12,Z22,Z32,Z42,
     .   XX,YY,ZZ,XXX,YYY,ZZZ,XY,YZ,ZX,XY2,YZ2,ZX2,
     .   A1,A2,A3,B1,B2,B3,C1,C2,C3,DET,BB1,BB2,BB3,CC1,CC2,CC3,
     .   MLOC(3,3),MLOC2(3,3),MGLOB(3,3),PASS(3,3),TPASS(3,3),
     .   VECT_SX,VECT_SY,VECT_SZ,SURF,RATIO,L12_2,L23_2,L34_2,L41_2
C======================================================================= 
C
      R(1)=RS(1)-RM(1)
      R(2)=RS(2)-RM(2) 
      R(3)=RS(3)-RM(3)  
C
      X12=X1*X1
      X22=X2*X2
      X32=X3*X3
      X42=X4*X4 
      Y12=Y1*Y1
      Y22=Y2*Y2
      Y32=Y3*Y3
      Y42=Y4*Y4 
      Z12=Z1*Z1 
      Z22=Z2*Z2
      Z32=Z3*Z3 
      Z42=Z4*Z4 
      XX=X12 + X22 + X32 + X42 
      YY=Y12 + Y22 + Y32 + Y42 
      ZZ=Z12 + Z22 + Z32 + Z42 
      XY=X1*Y1 + X2*Y2 + X3*Y3 + X4*Y4 
      YZ=Y1*Z1 + Y2*Z2 + Y3*Z3 + Y4*Z4 
      ZX=Z1*X1 + Z2*X2 + Z3*X3 + Z4*X4
      ZZZ=XX+YY
      XXX=YY+ZZ
      YYY=ZZ+XX 
      XY2=XY*XY
      YZ2=YZ*YZ
      ZX2=ZX*ZX
      DET= XXX*YYY*ZZZ - XXX*YZ2 - YYY*ZX2 - ZZZ*XY2 
     .             - TWO*XY*YZ*ZX
C
C --- VECT_SURF = 0.5*(X1^X2 + X2^X3 + X3^X4 +X4^X1)
C
      IF (NIR == 4) THEN
        VECT_SX = Y1*Z2+Y2*Z3+Y3*Z4+Y4*Z1-Z1*Y2-Z2*Y3-Z3*Y4-Z4*Y1
        VECT_SY = Z1*X2+Z2*X3+Z3*X4+Z4*X1-X1*Z2-X2*Z3-X3*Z4-X4*Z1
        VECT_SZ = X1*Y2+X2*Y3+X3*Y4+X4*Y1-Y1*X2-Y2*X3-Y3*X4-Y4*X1
      ELSE
        VECT_SX = Y1*Z2+Y2*Z3+Y3*Z1-Z1*Y2-Z2*Y3-Z3*Y1
        VECT_SY = Z1*X2+Z2*X3+Z3*X1-X1*Z2-X2*Z3-X3*Z1
        VECT_SZ = X1*Y2+X2*Y3+X3*Y1-Y1*X2-Y2*X3-Y3*X1
      ENDIF
C
      SURF = SQRT(VECT_SX*VECT_SX+VECT_SY*VECT_SY+VECT_SZ*VECT_SZ)
C
      L12_2 = (X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2
      L23_2 = (X3-X2)**2+(Y3-Y2)**2+(Z3-Z2)**2
      L34_2 = (X4-X3)**2+(Y4-Y3)**2+(Z4-Z3)**2
      L41_2 = (X1-X4)**2+(Y1-Y4)**2+(Z1-Z4)**2
C
C --- RATIO = h / Lmax = 0.5*S/Lmax2
C--> in case of triangle RATIO=h/l and in case of rectangle RATIO = 2*h/L
      RATIO = SURF / MAX(L12_2,L23_2,L34_2,L41_2)
C
C      IF (ABS(DET) < 1e-8) print *,"ATTENTION D < 1e-8",DET,RATIO,XX,YY
C
      IF (RATIO > 5e-3) THEN
C-- standard situation 
        DET = ONE/DET
        B1=ZZZ*YYY-YZ2
        B2=XXX*ZZZ-ZX2
        B3=YYY*XXX-XY2
        C3=ZZZ*XY+YZ*ZX
        C1=XXX*YZ+ZX*XY
        C2=YYY*ZX+XY*YZ
        BETAX = ONE
        BETAY = ONE
      ELSEIF (YY < XX) THEN
C-- nodes are nearly aligned on local X axis - DET = 0 - switch to 1D formulation
        DET = ONE
        B1=ZERO
        B2=ONE/XX
        B3=ONE/XX
        C3=ZERO
        C1=ZERO
        C2=ZERO
        BETAX = ZERO
        BETAY = ONE
      ELSE
C-- nodes are nearly aligned on local Y axis - DET = 0 - switch to 1D formulation
        DET = ONE
        B1=ONE/YY
        B2=ZERO
        B3=ONE/YY
        C3=ZERO
        C1=ZERO
        C2=ZERO
        BETAX = ONE
        BETAY = ZERO
      ENDIF
C
      BB1=B1*B1
      BB2=B2*B2
      BB3=B3*B3
      CC1=C1*C1
      CC2=C2*C2
      CC3=C3*C3
C
      DWDU=DET*SQRT(MAX(BB1*(YY+ZZ)+CC3*(ZZ+XX)+CC2*(XX+YY),
     .                  BB2*(ZZ+XX)+CC1*(XX+YY)+CC3*(YY+ZZ),
     .                  BB3*(XX+YY)+CC2*(YY+ZZ)+CC1*(ZZ+XX)))
C
      STBRK=SQRT((R(1)*R(1)+R(2)*R(2)+R(3)*R(3)))*DWDU
C
C     Matrix M-1 must be stored in global skew for i2vit3
C
      MLOC(1,1)=B1
      MLOC(1,2)=C3
      MLOC(1,3)=C2
      MLOC(2,1)=C3
      MLOC(2,2)=B2
      MLOC(2,3)=C1
      MLOC(3,1)=C2
      MLOC(3,2)=C1
      MLOC(3,3)=B3
C
      PASS(1,1) = E1X
      PASS(1,2) = E2X
      PASS(1,3) = E3X
      PASS(2,1) = E1Y
      PASS(2,2) = E2Y
      PASS(2,3) = E3Y
      PASS(3,1) = E1Z
      PASS(3,2) = E2Z
      PASS(3,3) = E3Z
C
      TPASS(1,1) = E1X
      TPASS(1,2) = E1Y
      TPASS(1,3) = E1Z
      TPASS(2,1) = E2X
      TPASS(2,2) = E2Y
      TPASS(2,3) = E2Z
      TPASS(3,1) = E3X
      TPASS(3,2) = E3Y
      TPASS(3,3) = E3Z 
C
      MLOC2(1:3,1:3) = MATMUL(MLOC(1:3,1:3),TPASS(1:3,1:3))
      MGLOB(1:3,1:3) = MATMUL(PASS(1:3,1:3),MLOC2(1:3,1:3))
C
      DPARA(1)=DET
      DPARA(2)=MGLOB(1,1)
      DPARA(3)=MGLOB(2,2)
      DPARA(4)=MGLOB(3,3)
      DPARA(5)=MGLOB(2,3)
      DPARA(6)=MGLOB(1,3)
      DPARA(7)=MGLOB(1,2)      
C
C-----------
      RETURN
      END
