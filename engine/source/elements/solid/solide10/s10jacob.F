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
Chd|  S10JACOB                      source/elements/solid/solide10/s10jacob.F
Chd|-- called by -----------
Chd|        S10DERI3                      source/elements/solid/solide10/s10deri3.F
Chd|        S10DERIT3                     source/elements/solid/solide10/s10deri3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10JACOB(
     1   ALPH,    BETA,    W,       X1B,
     2   X2B,     X3B,     X4A,     X5B,
     3   X6B,     X7B,     X8B,     X9B,
     4   X10B,    X8A,     X9A,     X10A,
     5   Y1B,     Y2B,     Y3B,     Y4A,
     6   Y5B,     Y6B,     Y7B,     Y8B,
     7   Y9B,     Y10B,    Y8A,     Y9A,
     8   Y10A,    Z1B,     Z2B,     Z3B,
     9   Z4A,     Z5B,     Z6B,     Z7B,
     A   Z8B,     Z9B,     Z10B,    Z8A,
     B   Z9A,     Z10A,    PX1,     PX2,
     C   PX3,     PX4,     PX5,     PX6,
     D   PX7,     PX8,     PX9,     PX10,
     E   PY1,     PY2,     PY3,     PY4,
     F   PY5,     PY6,     PY7,     PY8,
     G   PY9,     PY10,    PZ1,     PZ2,
     H   PZ3,     PZ4,     PZ5,     PZ6,
     I   PZ7,     PZ8,     PZ9,     PZ10,
     J   NX1,     NX2,     NX3,     NX4,
     K   NX5,     NX6,     NX7,     NX8,
     L   NX9,     NX10,    VOL,     VOLDP,
     M   NEL,     OFFG)
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
C     REAL
       DOUBLE PRECISION
     .   X1B(MVSIZ),X2B(MVSIZ),X3B(MVSIZ),X4A(MVSIZ),X5B(MVSIZ),
     .   X6B(MVSIZ),X7B(MVSIZ),X8B(MVSIZ),X9B(MVSIZ),X10B(MVSIZ),
     .   X8A(MVSIZ),X9A(MVSIZ),X10A(MVSIZ),
     .   Y1B(MVSIZ),Y2B(MVSIZ),Y3B(MVSIZ),Y4A(MVSIZ),Y5B(MVSIZ),
     .   Y6B(MVSIZ),Y7B(MVSIZ),Y8B(MVSIZ),Y9B(MVSIZ),Y10B(MVSIZ),
     .   Y8A(MVSIZ),Y9A(MVSIZ),Y10A(MVSIZ),
     .   Z1B(MVSIZ),Z2B(MVSIZ),Z3B(MVSIZ),Z4A(MVSIZ),Z5B(MVSIZ),
     .   Z6B(MVSIZ),Z7B(MVSIZ),Z8B(MVSIZ),Z9B(MVSIZ),Z10B(MVSIZ),
     .   Z8A(MVSIZ),Z9A(MVSIZ),Z10A(MVSIZ),VOLDP(*)

       INTEGER, INTENT(IN) :: NEL ! number of element in the current group
       my_real, DIMENSION(NEL), INTENT(IN) :: OFFG ! off array : 0 if the element is deleted

       my_real
     .   PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),PX5(MVSIZ),
     .   PX6(MVSIZ),PX7(MVSIZ),PX8(MVSIZ),PX9(MVSIZ),PX10(MVSIZ),
     .   PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),PY5(MVSIZ),
     .   PY6(MVSIZ),PY7(MVSIZ),PY8(MVSIZ),PY9(MVSIZ),PY10(MVSIZ),
     .   PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),PZ5(MVSIZ),
     .   PZ6(MVSIZ),PZ7(MVSIZ),PZ8(MVSIZ),PZ9(MVSIZ),PZ10(MVSIZ),
     .   NX1(MVSIZ),NX2(MVSIZ),NX3(MVSIZ),NX4(MVSIZ),NX5(MVSIZ),
     .   NX6(MVSIZ),NX7(MVSIZ),NX8(MVSIZ),NX9(MVSIZ),NX10(MVSIZ),
     .   VOL(MVSIZ),ALPH,BETA,W
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      DOUBLE PRECISION 
c      my_real
     .   D,A4MB4,A4,B4,BB,AB,B2,A4_0,B4_0,
     .   DXDR,DXDS,DXDT,DYDR,DYDS,DYDT,DZDR,DZDS,DZDT
     
      DOUBLE PRECISION 
     .   AA,A4M1,B4M1
      
c      my_real
      DOUBLE PRECISION 
     .   DRDX, DSDX, DTDX,
     .   DRDY, DSDY, DTDY,
     .   DRDZ, DSDZ, DTDZ,
     .   DET,WUNSIX
C-----------------------------------------------
c      AA = (TWO*ALPH - ONE)*ALPH
c      BB = (TWO*BETA - ONE)*BETA
c      B2 = FOUR*BETA*BETA
c      AB = FOUR*ALPH*BETA      
cC-----------------------------------------------
c      DO I=1,NEL
c        NX1(I) = BB
c        NX2(I) = BB
c        NX3(I) = BB
c        NX4(I) = AA
c        NX5(I) = B2
c        NX6(I) = B2
c        NX7(I) = B2
c        NX8(I) = AB
c        NX9(I) = AB
c        NX10(I)= AB
c      ENDDO
C
       A4_0 = FOUR * ALPH
       B4_0 = FOUR * BETA
       WUNSIX = W*ONE_OVER_6
#include   "nofusion.inc"
      DO I=1,NEL
       AA = X5B(I) + X6B(I) + X7B(I)
     .    - X4A(I) - X8B(I) - X9B(I) - X10B(I)
       DXDR = X1B(I) +X8A(I) - X6B(I) + AA
       DXDS = X2B(I) +X9A(I) - X7B(I) + AA
       DXDT = X3B(I) +X10A(I)- X5B(I) + AA
C
       AA = Y5B(I) + Y6B(I) + Y7B(I)
     .    - Y4A(I) - Y8B(I) - Y9B(I) - Y10B(I)
       DYDR = Y1B(I) +Y8A(I) - Y6B(I) + AA
       DYDS = Y2B(I) +Y9A(I) - Y7B(I) + AA
       DYDT = Y3B(I) +Y10A(I)- Y5B(I) + AA
C
       AA = Z5B(I) + Z6B(I) + Z7B(I)
     .    - Z4A(I) - Z8B(I) - Z9B(I) - Z10B(I)
       DZDR = Z1B(I) +Z8A(I) - Z6B(I) + AA
       DZDS = Z2B(I) +Z9A(I) - Z7B(I) + AA
       DZDT = Z3B(I) +Z10A(I)- Z5B(I) + AA
C
       DRDX=DYDS*DZDT-DZDS*DYDT
       DSDX=DYDT*DZDR-DZDT*DYDR
       DTDX=DYDR*DZDS-DZDR*DYDS
C
       DRDY=DZDS*DXDT-DXDS*DZDT
       DSDY=DZDT*DXDR-DXDT*DZDR
       DTDY=DZDR*DXDS-DXDR*DZDS
C
       DRDZ=DXDS*DYDT-DYDS*DXDT
       DSDZ=DXDT*DYDR-DYDT*DXDR
       DTDZ=DXDR*DYDS-DYDR*DXDS
C
       DET = DXDR * DRDX + DYDR * DRDY + DZDR * DRDZ
       ! check if the element is deleted : if it's true, need to force det to 1
       IF(OFFG(I)==ZERO) DET = ONE
       D = ONE/MAX(EM30,DET)
C
c       A4 = FOUR * ALPH
c       B4 = FOUR * BETA
       A4M1  = D *(A4_0 - ONE)
       B4M1  =  D *(B4_0 - ONE)
!
       B4 = D * B4_0
       A4 = D * A4_0
       A4MB4  = A4 - B4
       VOLDP(I) = WUNSIX * DET 
       VOL(I) = VOLDP(I) 
C
       PX1(I) = B4M1 * DRDX
       PY1(I) = B4M1 * DRDY
       PZ1(I) = B4M1 * DRDZ
C
       PX2(I) = B4M1 * DSDX
       PY2(I) = B4M1 * DSDY
       PZ2(I) = B4M1 * DSDZ
C
       PX3(I) = B4M1 * DTDX
       PY3(I) = B4M1 * DTDY
       PZ3(I) = B4M1 * DTDZ
C
       PX4(I) =-A4M1 * (DRDX+DSDX+DTDX)
       PY4(I) =-A4M1 * (DRDY+DSDY+DTDY)
       PZ4(I) =-A4M1 * (DRDZ+DSDZ+DTDZ)
C
       PX5(I) = B4 * (DRDX+DSDX)
       PY5(I) = B4 * (DRDY+DSDY)
       PZ5(I) = B4 * (DRDZ+DSDZ)
C
       PX6(I) = B4 * (DSDX+DTDX)
       PY6(I) = B4 * (DSDY+DTDY)
       PZ6(I) = B4 * (DSDZ+DTDZ)
C
       PX7(I) = B4 * (DTDX+DRDX)
       PY7(I) = B4 * (DTDY+DRDY)
       PZ7(I) = B4 * (DTDZ+DRDZ)
C
       PX8(I) = A4MB4  * DRDX - PX6(I)
       PY8(I) = A4MB4  * DRDY - PY6(I)
       PZ8(I) = A4MB4  * DRDZ - PZ6(I)
C
       PX9(I) = A4MB4  * DSDX - PX7(I)
       PY9(I) = A4MB4  * DSDY - PY7(I)
       PZ9(I) = A4MB4  * DSDZ - PZ7(I)
C
       PX10(I)= A4MB4  * DTDX - PX5(I)
       PY10(I)= A4MB4  * DTDY - PY5(I)
       PZ10(I)= A4MB4  * DTDZ - PZ5(I)
C
      ENDDO
C
      RETURN
      END
