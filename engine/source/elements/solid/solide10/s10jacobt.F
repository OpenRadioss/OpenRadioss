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
Chd|  S10JACOBT                     source/elements/solid/solide10/s10jacobt.F
Chd|-- called by -----------
Chd|        S10DERITO3                    source/elements/solid/solide10/s10derito3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10JACOBT(
     1   ALPH,    BETA,    W,       PX1,
     2   PX2,     PX3,     PX4,     PX5,
     3   PX6,     PX7,     PX8,     PX9,
     4   PX10,    PY1,     PY2,     PY3,
     5   PY4,     PY5,     PY6,     PY7,
     6   PY8,     PY9,     PY10,    PZ1,
     7   PZ2,     PZ3,     PZ4,     PZ5,
     8   PZ6,     PZ7,     PZ8,     PZ9,
     9   PZ10,    NX1,     NX2,     NX3,
     A   NX4,     NX5,     NX6,     NX7,
     B   NX8,     NX9,     NX10,    VOL,
     C   JAC_I,   NEL)
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
       my_real
     .   PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),PX5(MVSIZ),
     .   PX6(MVSIZ),PX7(MVSIZ),PX8(MVSIZ),PX9(MVSIZ),PX10(MVSIZ),
     .   PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),PY5(MVSIZ),
     .   PY6(MVSIZ),PY7(MVSIZ),PY8(MVSIZ),PY9(MVSIZ),PY10(MVSIZ),
     .   PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),PZ5(MVSIZ),
     .   PZ6(MVSIZ),PZ7(MVSIZ),PZ8(MVSIZ),PZ9(MVSIZ),PZ10(MVSIZ),
     .   NX1(MVSIZ),NX2(MVSIZ),NX3(MVSIZ),NX4(MVSIZ),NX5(MVSIZ),
     .   NX6(MVSIZ),NX7(MVSIZ),NX8(MVSIZ),NX9(MVSIZ),NX10(MVSIZ),
     .   VOL(MVSIZ),ALPH,BETA,W,JAC_I(10,MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   D,A4MB4,A4,B4,BB,AB,B2,
     .   DXDR,DXDS,DXDT,DYDR,DYDS,DYDT,DZDR,DZDS,DZDT
     
      DOUBLE PRECISION 
     .   AA,A4M1,B4M1
      
      my_real
     .   DRDX, DSDX, DTDX,
     .   DRDY, DSDY, DTDY,
     .   DRDZ, DSDZ, DTDZ
C-----------------------------------------------
c      AA = (TWO*ALPH - ONE)*ALPH
c      BB = (TWO*BETA - ONE)*BETA
c      B2 = FOUR*BETA*BETA
c      AB = FOUR*ALPH*BETA      
C-----------------------------------------------
C
      A4 = FOUR * ALPH
      B4 = FOUR * BETA
      A4M1  = A4 - ONE
      B4M1  = B4 - ONE
      A4MB4  = A4 - B4
      DO I=1,NEL
       DRDX=JAC_I(1,I)
       DSDX=JAC_I(4,I)
       DTDX=JAC_I(7,I)
C
       DRDY=JAC_I(2,I)
       DSDY=JAC_I(5,I)
       DTDY=JAC_I(8,I)
C
       DRDZ=JAC_I(3,I)
       DSDZ=JAC_I(6,I)
       DTDZ=JAC_I(9,I)
C
       VOL(I) = JAC_I(10,I)
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
